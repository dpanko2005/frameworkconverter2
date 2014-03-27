{ ------------------------------------------------------------------- }
{ Unit:    SWMMIO.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit containing various utility functions, global variables }
{ and constants, primarily used for interacting with SWMM5 input / output }
{ files }
{ ------------------------------------------------------------------- }

unit SWMMIO;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons;

type
  TMTARecord = record // converted framework timeseries data structure
    tsName: string;
    tsNodeName: string;
    tsType: string; // FLOW or CONCEN
    tsUnitsFactor: Double; // default 1.0
    constituentSWMMName: string;
    constituentFWName: string;
    convFactor: Double; // default 1.0
    convertedTS: TStringList;
    convertedTSFilePath: string;
    ModelRunScenarioID: string;
    SWMMFilePath: string;
    scratchFilePath: string;
    mtaFilePath: string;
  end;

const
  // includes NsubcatchResults,SUBCATCH_RAINFALL,SUBCATCH_SNOWDEPTH,SUBCATCH_LOSSES,SUBCATCH_RUNOFF,SUBCATCH_GW_FLOW,SUBCATCH_GW_ELEV;
  NUMSUBCATCHVARS: integer = 7;
  MIN_WQ_FLOW: Double = 0.001; // minmun water quality flow from swmm
  NODE_INFLOW: integer = 4;
  NODE_QUAL: integer = 6;
  MAX_NODE_RESULTS = 7;
  MAX_SUBCATCH_RESULTS = 7;
  MAX_LINK_RESULTS = 6;
  MAX_SYS_RESULTS = 14;
  opModes: array [0 .. 1] of string = ('SWMM_TO_FW', 'SWMM_FROM_FW');
  appTypes: array [0 .. 1] of string = ('SWMM_CONSOLE', 'SWMM_GUI');
  constituentNames: array [0 .. 7] of string = ('FLOW', 'TSS', 'TP', 'DP',
    'DZn', 'TZN', 'DCU', 'TCU');
  // NnodeResults,NODE_DEPTH,NODE_HEAD,NODE_VOLUME,NODE_LATFLOW,NODE_INFLO,NODE_OVERFLOW;
  NUMNODEVARS: integer = 7;

  // NlinkResults,LINK_FLOW,LINK_DEPTH,LINK_VELOCITY,LINK_FROUDE,LINK_CAPACITY;
  NUMLINKVARS: integer = 6;

var
  workingDir: string; // exe folder
  SWMMFileStreamPosition: long;
  operatingMode: string; // SWMM_TO_FW or  SWMM_FROM_FW'
  appType: string; // SWMM_CONSOLE or SWMM_GUI
  // stores existing swmm TS and Inflow block names in swmm inputfile
  TSList, InflowsList: TStringList;
  PollList, NodeNameList: TStringList;
  frameCtrlFilePath, mtaFilePath: string;
  errorsList: TStringList;

  // stores file stream seek position after node and poll names are read
function readInFrameworkTSFile(filePath: string; var Conv: TArray<TMTARecord>)
  : TArray<TMTARecord>;
function getSWMMNodeIDsFromTxtInput(SWMMFilePath: string): TArray<TStringList>;
function getSWMMNodeIDsFromBinary(SWMMFilePath: string): TArray<TStringList>;
procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);
procedure saveTextFileToDisc(FileContentsList: TStringList; filePath: string;
  shdOverwrite: boolean = false);

implementation

function readInFrameworkTSFile(filePath: string; var Conv: TArray<TMTARecord>)
  : TArray<TMTARecord>;
var
  FileContentsList: TStringList;
  strLine: string;
  lineNumber: integer;
  tempStrList: TStrings;
  tempTimeVal: Double;
  tempDateTimeStr, tempTimeStr: string;
  tempValueStr: string;
  i: integer;
  j: integer;
begin
  FileContentsList := TStringList.Create;
  tempStrList := TStringList.Create;
  errorsList := TStringList.Create;

  { First check if the file exists. }
  if Not(FileExists(filePath)) then
  begin
    errorsList.Add('Framework time series data file not found at:' + filePath);
    exit;
  end;
  try
    for i := Low(Conv) to High(Conv) do
    begin
      Conv[i].convertedTS := TStringList.Create;
    end;

    FileContentsList.LoadFromFile(filePath);
    lineNumber := 0;
    while lineNumber < FileContentsList.Count - 1 do
    begin
      strLine := FileContentsList[lineNumber];
      // ignore comment lines
      if (Pos('#', strLine) < 1) and (Length(strLine) > 1) then
      begin
        tempStrList.Clear();
        ExtractStrings([','], [], PChar(strLine), tempStrList);
        tempTimeVal := strToFloat(tempStrList[3]);
        tempTimeStr := floatToStr(int(tempTimeVal)) + ':' +
          FormatFloat('00', frac(tempTimeVal) * 60);
        tempDateTimeStr := Format('%.2d/%.2d/%s	%5s',
          [strToInt(tempStrList[1]), strToInt(tempStrList[2]),
          trim(tempStrList[0]), tempTimeStr]);

        for i := Low(Conv) to High(Conv) do
        begin
          j := i + 4;
          if (j < tempStrList.Count) then
          begin
            tempValueStr := tempStrList[j];
            Conv[i].convertedTS.Add(tempDateTimeStr + '	' + tempValueStr);
          end;
        end;
      end;
      inc(lineNumber);
    end;
  finally
    FileContentsList.Free;
    tempStrList.Free;
  end;
  result := Conv;
end;

function getSWMMNodeIDsFromTxtInput(SWMMFilePath: string): TArray<TStringList>;
var
  FileContentsList: TStringList;
  rsltLists: TArray<TStringList>;
  SwmmTokens: TStringList;
  lineNumber, intTokenLoc, tempInt, i: integer;
  strLine, strToken, strObjectID: string;
begin
  intTokenLoc := 0;
  FileContentsList := TStringList.Create;
  SetLength(rsltLists, 4);
  for i := Low(rsltLists) to High(rsltLists) do
    rsltLists[i] := TStringList.Create();

  // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
  SwmmTokens := TStringList.Create;

  // Add supported SWMM Node types to the list of tokens to parse
  SwmmTokens.Delimiter := ' '; // Each list item will be blank separated
  SwmmTokens.QuoteChar := '|'; // And each item will be quoted with |'s
  SwmmTokens.DelimitedText :=
    '|[DIVIDERS]| |[JUNCTIONS]| |[OUTFALLS]| |[STORAGE]| |[POLLUTANTS]| |[TIMESERIES]| |[INFLOWS]|';

  if FileExists(SWMMFilePath) then
  begin
    { If it exists, load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMFilePath);
    lineNumber := 0;
    while lineNumber < FileContentsList.Count - 1 do
    begin
      strLine := LowerCase(FileContentsList[lineNumber]);
      for i := 0 to SwmmTokens.Count - 1 do
      begin
        strToken := LowerCase(SwmmTokens[i]);
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
          intTokenLoc := Pos('[', strLine);
          if intTokenLoc > 0 then
          begin
            dec(lineNumber);
            break;
          end;
          // ignore comment lines
          if (Pos(';;', strLine) < 1) and (Length(strLine) > 1) then
          begin
            // extract node name
            tempInt := Pos(' ', strLine);
            if tempInt > 0 then
            begin
              strObjectID := Copy(strLine, 1, tempInt - 1);
              // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
              if i = 4 then
              // if we are in the [POLLUTANTS] block save names to pollutants list
              begin
                rsltLists[1].Add(strObjectID);
              end
              else if i = 5 then
              // if we are in the [TIMESERIES] block save names to TIMESERIES list
              begin
                rsltLists[2].Add(strObjectID);
              end
              else if i = 6 then
              // if we are in the [INFLOWS] block save names to INFLOWS list
              begin
                rsltLists[3].Add(strLine);
              end
              else
                // everything else is a node so save names to nodes list
                rsltLists[0].Add(strObjectID);
            end;
          end;
        until intTokenLoc > 0;
      end;
      inc(lineNumber);
    end;
    result := rsltLists;
  end
  else
  begin
    errorsList.Add('SWMM input file not found at:' + SWMMFilePath);
    exit;
  end;
end;

function getSWMMNodeIDsFromBinary(SWMMFilePath: string): TArray<TStringList>;
var
  Stream: TFileStream;
  Reader: TBinaryReader;
  // magicNum, flowUnits, SWMMVersion: integer;
  numSubCatchs, numLinks, numPolls,numNodes: integer;
  idx: long;
  numCharsInID: integer;
  tempID: string;
  tempIDCharArr: TArray<Char>;
  nodeIDList, pollutantIDList: TStringList;
begin
  Stream := TFileStream.Create(SWMMFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  try
    Reader := TBinaryReader.Create(Stream);
    try
      // --- get number of objects reported on
      // numSubCatchs := 0;
      //numNodes := 0;
      // numLinks := 0;
      numPolls := 0;

      Reader.ReadInteger; // Magic number
      Reader.ReadInteger; // SWMM Version number
      Reader.ReadInteger; // Flow units
      numSubCatchs := Reader.ReadInteger; // # subcatchments
      numNodes := Reader.ReadInteger; // # nodes
      numLinks := Reader.ReadInteger; // # links
      Reader.ReadInteger; // # pollutants

      // Read all subcatchment IDs and discard, skipping this section is not straight forward since catchment
      // name lengths vary
      for idx := 0 to numSubCatchs - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
      end;

      // Read all node IDs and save for use later
      for idx := 0 to numNodes - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
        if Length(tempIDCharArr) > 0 then
        begin
          SetString(tempID, PChar(@tempIDCharArr[0]), Length(tempIDCharArr));
          nodeIDList.Add(tempID);
        end
      end;

      // Read all link IDs and discard, skipping this section is not straight forward since catchment
      // name lengths vary
      for idx := 0 to numLinks - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
      end;

      // Read all pollutant IDs and save for use later
      for idx := 0 to numPolls - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
        if Length(tempIDCharArr) > 0 then
        begin
          SetString(tempID, PChar(@tempIDCharArr[0]), Length(tempIDCharArr));
          pollutantIDList.Add(tempID);
        end
        else
      end;

      // save stream position for later when we extract node results to avoid having to start over
      SWMMFileStreamPosition := Stream.Position;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
  SetLength(result, 2);
  result[0] := nodeIDList;
  result[1] := pollutantIDList;
end;

procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  Strings.Delimiter := Delimiter;
  Strings.DelimitedText := '"' + StringReplace(Input, Delimiter,
    '"' + Delimiter + '"', [rfReplaceAll]) + '"';
end;

procedure saveTextFileToDisc(FileContentsList: TStringList; filePath: string;
  shdOverwrite: boolean = false);
var
  dirName: string;
begin
  // Save a new swmm file back to disc
  if (FileContentsList <> nil) then
  begin
    // check if directory exists
    dirName := ExtractFilePath(filePath);
    if (DirectoryExists(dirName) = false) then
    begin
      if CreateDir(dirName) then
        // do nothing ShowMessage('New directory added OK')
      else
      begin
        raise Exception.Create
          ('Fatal Error: Unable to create directory for saving file - error : '
          + IntToStr(GetLastError));
        exit;
      end;
    end;

    { First check if the file exists. }
    if (not shdOverwrite) and (FileExists(filePath)) then
      { If it exists, raise an exception. }
      raise Exception.Create
        ('Fatal Error: File already exists. Attemp to overwrite failed.')
    else
      FileContentsList.SaveToFile(filePath);
  end;
end;

end.
