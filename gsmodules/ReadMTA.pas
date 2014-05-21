{ ------------------------------------------------------------------- }
{ Unit:    ReadMTA.pas                                                }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit for reading the Converter                        }
{ metadata control file (.mta)                                        }
{ ------------------------------------------------------------------- }

unit ReadMTA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO;

var
  errorsList: TStringList;

///	<summary>
///	  Function for reading SWMM 5 converter control file and outputing a
///	  structured record of the contents of the converter control file
///	</summary>
///	<param name="mtaFilePath">
///	  full file path of the SWMM 5 converter control file to be read in
///	</param>
///	<returns>
///	  Structured record created to hold the contents of the SWMM 5 converter
///	  control file read in
///	</returns>
function Read(mtaFilePath: string): TArray<TMTARecord>;

///	<summary>
///	  Helper function for populating the structured data structure (TMTARecord)
///	  that is used to store the contents of a SWMM 5 converter control file
///	</summary>
///	<param name="MTARec">
///	  TMTARecord to be populated with the rest of the inputs to this function
///	</param>
///	<param name="descr">
///	  Description of the SWMM 5 converter control file
///	</param>
///	<param name="constituentSWMMName">
///	  SWMM name of the consitituent for which record is being created
///	</param>
///	<param name="constituentFWName">
///	  Framework name of the consitituent for which record is being created
///	</param>
///	<param name="constituentType">
///	  Type of constituent (either FLOW or CONCEN)
///	</param>
///	<param name="unitsFactor">
///	  Units factor for the constituent in SWMM
///	</param>
///	<param name="nodeID">
///	  ID of SWMM node associated with the record being created
///	</param>
///	<param name="swmmFilePath">
///	  Filepath to the SWMM file assosciated with the record being created
///	</param>
///	<param name="scratchFilePath">
///	  Filepath to the framework contrl scratch file path associated with the
///	  record being created
///	</param>
///	<param name="convFactor">
///	  Conversion factor for the constituent being created
///	</param>
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
  errorsList := TStringList.Create;
  FileContentsList := TStringList.Create;
  TempStrList := TStringList.Create;

  modelRunScenarioID := '';
  SWMMNodeID := '';
  swmmFilePath := '';
  scratchFilePath := '';
  flowConvFactor := 1.0;
  intTokenLoc := 0;
  numPolls:= 0;
  try

    { First check if the file exists. }
    if FileExists(mtaFilePath) then
    begin
      { If it exists, load the data into the stringlist. }
      FileContentsList.LoadFromFile(mtaFilePath);

      // Define a string list object, and point our variable at it
      TempTokens := TStringList.Create;

      // Add supported MTA token names to the list of tokens to search for
      TempTokens.Delimiter := ' '; // Each list item will be comma separated
      TempTokens.QuoteChar := '|'; // And each item will be quoted with |'s
      TempTokens.DelimitedText :=
        '|ModelRunScenarioID| |SWMMNodeID| |SWMMFilePath| |scratchFilePath| |FlowConv| |NumPolls| |FrameworkPollutants|';

      lineNumber := 0;
      // loop through all the lines in the file and check for the tokens above
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
            modelRunScenarioID := trim(FileContentsList[lineNumber]);
          if i = 1 then // token is swmm node id
            SWMMNodeID := trim(FileContentsList[lineNumber]);
          if i = 2 then // token is swmm input or output file path
          begin
            swmmFilePath := trim(FileContentsList[lineNumber]);
            swmmFilePath := StringReplace(swmmFilePath, '''', '', [rfReplaceAll]);
          end;
          if i = 3 then // token is framework scratch file path
          begin
            scratchFilePath := trim(FileContentsList[lineNumber]);
            scratchFilePath := StringReplace(scratchFilePath, '''', '', [rfReplaceAll]);
          end;
          if i = 4 then // token is FlowConv
            flowConvFactor := StrToFloat(trim(FileContentsList[lineNumber]));
          if i = 5 then // token is NumPolls
          begin
            numPolls := StrToInt(trim(FileContentsList[lineNumber]));
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
              strLine := trim(FileContentsList[lineNumber]);
              strLine := StringReplace(strLine, '''', '', [rfReplaceAll]);
              SWMMIO.Split('/', strLine, TempStrList);
              pollConvFactor := StrToFloat(trim(TempStrList[1]));

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
      raise Exception.Create
        ('SWMM Converter Control File does not exist and hence cannot be read.');
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
  //populate the SWMM converter control record with the appropriate values
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

