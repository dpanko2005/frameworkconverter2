unit ReadMTA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO;

function Read(mtaFilePath: string): TArray<TMTARecord>;
procedure PopulateMTARecord(var MTARec: TMTARecord; descr: string;
  constituentSWMMName: string; constituentFWName: string;
  constituentType: string; unitsFactor: double; nodeID: string;
  swmmFilePath: string; scratchFilePath: string; convFactor: double);

implementation

function Read(mtaFilePath: string): TArray<TMTARecord>;
var
  FileContentsList: TStringList;
  MTADataArr: TArray<TMTARecord>;
  TempTokens: TStringList;
  TempStrList: TStringList;
  lineNumber: integer;
  intTokenLoc: integer;
  strLine: string;
  strToken: string;
  // strNodeName: string;
  // tempInt: integer;
  i: integer;
  j: integer;
  modelRunScenarioID: string;
  SWMMNodeID: string;
  swmmFilePath: string;
  scratchFilePath: string;
  flowConvFactor: double;
  pollConvFactor: double;
  numPolls: integer;
  swmmConstituentName: string;
  fwConstituentName: string;
begin
  FileContentsList := TStringList.Create;
  TempStrList := TStringList.Create;

  modelRunScenarioID := '';
  SWMMNodeID := '';
  swmmFilePath := '';
  scratchFilePath := '';
  flowConvFactor := 1.0;
  intTokenLoc := 0;
  try

    { First check if the file exists. }
    if FileExists(mtaFilePath) then
    begin
      { If it exists, load the data into the stringlist. }
      FileContentsList.LoadFromFile(mtaFilePath);

      // Define a string list object, and point our variable at it
      TempTokens := TStringList.Create;

      // Add supported MTA token names to the list of tokens to search for
      TempTokens.Delimiter := ' '; // Each list item will be blank separated
      TempTokens.QuoteChar := '|'; // And each item will be quoted with |'s
      TempTokens.DelimitedText :=
        '|ModelRunScenarioID| |SWMMNodeID| |SWMMOutputFilePath| |scratchFilePath| |FlowConv| |NumPolls| |FrameworkPollutants|';

      lineNumber := 0;
      while lineNumber < FileContentsList.Count - 1 do
      begin
        strLine := LowerCase(FileContentsList[lineNumber]);
        for i := 0 to TempTokens.Count - 1 do
        begin
          strToken := LowerCase(TempTokens[i]);
          intTokenLoc := Pos(strToken, strLine);

          // check inputfile line to see if token present
          if intTokenLoc > 0 then
            break;
        end;

        // if token found read in node names
        if intTokenLoc > 0 then
        begin
          Repeat
            inc(lineNumber);
            strLine := FileContentsList[lineNumber];
            intTokenLoc := Pos('##', strLine);
          until intTokenLoc < 1; // comments skipped

          // Now read values until next token or end of file
          if i = 0 then // token is description
            modelRunScenarioID := FileContentsList[lineNumber];
          if i = 1 then // token is swmm node id
            SWMMNodeID := FileContentsList[lineNumber];
          if i = 2 then // token is swmm input or output file path
            swmmFilePath := FileContentsList[lineNumber];
          if i = 3 then // token is framework scratch file path
            scratchFilePath := FileContentsList[lineNumber];
          if i = 4 then // token is FlowConv
            flowConvFactor := StrToFloat(FileContentsList[lineNumber]);
          if i = 5 then // token is NumPolls
          begin
            numPolls := StrToInt(FileContentsList[lineNumber]);
            SetLength(MTADataArr, numPolls + 1);
            PopulateMTARecord(MTADataArr[0], modelRunScenarioID, 'Flow', 'FLOW',
              'Flow', 1.0, SWMMNodeID, swmmFilePath, scratchFilePath,
              flowConvFactor);
          end;
          if i = 6 then // token is FrameworkPollutants
          begin
            j := 1;
            for i := 0 to numPolls - 1 do
            begin
              strLine := FileContentsList[lineNumber];
              strLine := StringReplace(strLine, '''', '', [rfReplaceAll]);
              SWMMIO.Split('/', strLine, TempStrList);
              pollConvFactor := StrToFloat(TempStrList[1]);

              SWMMIO.Split('=', TempStrList[0], TempStrList);
              fwConstituentName := Trim(TempStrList[0]);
              swmmConstituentName := Trim(TempStrList[1]);

              PopulateMTARecord(MTADataArr[j], modelRunScenarioID,
                swmmConstituentName, fwConstituentName, 'CONCEN', 1.0,
                SWMMNodeID, swmmFilePath, scratchFilePath, pollConvFactor);
              inc(j);
              intTokenLoc := Pos('##', strLine);
              if (intTokenLoc > 0) then
              begin
                result := MTADataArr;
                FileContentsList.Free;
                Exit;
              end;
              inc(lineNumber);
            end;
          end;
        end;
        inc(lineNumber);
      end;
    end
    else
    begin
      { Otherwise, raise an exception. }
      raise Exception.Create('File does not exist.');
      Exit
    end;
  finally
    FileContentsList.Free;
  end;
  result := MTADataArr;
end;

procedure PopulateMTARecord(var MTARec: TMTARecord; descr: string;
  constituentSWMMName: string; constituentFWName: string;
  constituentType: string; unitsFactor: double; nodeID: string;
  swmmFilePath: string; scratchFilePath: string; convFactor: double);
begin
  MTARec.tsUnitsFactor := unitsFactor;
  MTARec.constituentSWMMName := constituentSWMMName;
  MTARec.constituentFWName := constituentFWName;
  MTARec.convFactor := convFactor;
  MTARec.tsNodeName := nodeID;
  MTARec.tsType := constituentType;
  MTARec.modelRunScenarioID := descr;
  MTARec.swmmFilePath := swmmFilePath;
  MTARec.scratchFilePath := scratchFilePath;
end;

end.
