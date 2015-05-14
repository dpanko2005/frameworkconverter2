{*------------------------------------------------------------------------------
  Delphi Pascal unit containing various utility functions, global variables
  and constants, primarily used for interacting with SWMM5 input / output
  files

  @Unit    SWMMIO.pas
  @Project WERF Framework - SWMM Converter
  @Version 2.0
  @Date    2/28/2014
  @Author  Gesoyntec Consultants Inc (D. Pankani)
------------------------------------------------------------------------------- }
unit SWMMIO;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  ConverterErrors, StrUtils, DateUtils;



type
{*------------------------------------------------------------------------------
  Data structure for holding converted framwork timeseries
------------------------------------------------------------------------------- }
  TMTARecord = record // converted framework timeseries data structure
  {*------------------------------------------------------------------------------
    timeseries name
  ------------------------------------------------------------------------------- }
    tsName: string;
  {*------------------------------------------------------------------------------
    timeseries node name
  ------------------------------------------------------------------------------- }
    tsNodeName: string;
  {*------------------------------------------------------------------------------
    timeseries type
  ------------------------------------------------------------------------------- }
    tsType: string; // FLOW or CONCEN
  {*------------------------------------------------------------------------------
    timeseries units factor
  ------------------------------------------------------------------------------- }
    tsUnitsFactor: Double; // default 1.0
  {*------------------------------------------------------------------------------
    SWMM constituent name
  ------------------------------------------------------------------------------- }
    constituentSWMMName: string;
  {*------------------------------------------------------------------------------
    framework constituent name
  ------------------------------------------------------------------------------- }
    constituentFWName: string;
  {*------------------------------------------------------------------------------
    conversion factor
  ------------------------------------------------------------------------------- }
    convFactor: Double; // default 1.0
  {*------------------------------------------------------------------------------
    converted timeseries
  ------------------------------------------------------------------------------- }
    convertedTS: TStringList;
  {*------------------------------------------------------------------------------
    converted timeseries filepath
  ------------------------------------------------------------------------------- }
    convertedTSFilePath: string;
  {*------------------------------------------------------------------------------
    current scenario id
  ------------------------------------------------------------------------------- }
    ModelRunScenarioID: string;
  {*------------------------------------------------------------------------------
    SWMM filepath
  ------------------------------------------------------------------------------- }
    SWMMFilePath: string;
  {*------------------------------------------------------------------------------
    temporary filepath
  ------------------------------------------------------------------------------- }
    scratchFilePath: string;
{*------------------------------------------------------------------------------
  mta file path
-------------------------------------------------------------------------------}
   mtaFilePath: string;

  end;

const
  // includes NsubcatchResults,SUBCATCH_RAINFALL,SUBCATCH_SNOWDEPTH,SUBCATCH_LOSSES,SUBCATCH_RUNOFF,SUBCATCH_GW_FLOW,SUBCATCH_GW_ELEV;
  {*------------------------------------------------------------------------------
    number of subcatchment variables
  ------------------------------------------------------------------------------- }
  NUMSUBCATCHVARS: integer = 7;

  {*------------------------------------------------------------------------------
    minmun water quality flow from swmm
  ------------------------------------------------------------------------------- }
  MIN_WQ_FLOW: Double = 0.00001;
  {*------------------------------------------------------------------------------
    SWMM input file block tokens
  ------------------------------------------------------------------------------- }
  SWMMINPUTTOKENS: array [0 .. 6] of string = ('[DIVIDERS]', '[JUNCTIONS]',
    '[OUTFALLS]', '[STORAGE]', '[POLLUTANTS]', '[TIMESERIES]', '[INFLOWS]');

  // SWMM Node results types from enum NodeResultType in SWMM v5.022 enums.h
  // not all being used. Let here for future
  {*------------------------------------------------------------------------------
    Node depth
  ------------------------------------------------------------------------------- }
  NODE_DEPTH: integer = 0; // not used - water depth above invert
  {*------------------------------------------------------------------------------
    Node head
  ------------------------------------------------------------------------------- }
  NODE_HEAD: integer = 1; // not used - hydraulic head
  {*------------------------------------------------------------------------------
    Node volume
  ------------------------------------------------------------------------------- }
  NODE_VOLUME: integer = 2; // not used - volume stored & ponded
  {*------------------------------------------------------------------------------
    Node lateral flow
  ------------------------------------------------------------------------------- }
  NODE_LATFLOW: integer = 3; // not used - lateral inflow rate
  {*------------------------------------------------------------------------------
    Node inflow
  ------------------------------------------------------------------------------- }
  NODE_INFLOW: integer = 4; // *used - total inflow rate
  {*------------------------------------------------------------------------------
    Node overflow
  ------------------------------------------------------------------------------- }
  NODE_OVERFLOW: integer = 5; // not used - overflow rate
  {*------------------------------------------------------------------------------
    Node quality
  ------------------------------------------------------------------------------- }
  NODE_QUAL: integer = 6; // not used - concentration of each pollutant
  {*------------------------------------------------------------------------------
    Maximun node results
  ------------------------------------------------------------------------------- }
  MAX_NODE_RESULTS = 7;
  {*------------------------------------------------------------------------------
    Maximun subcatchment results
  ------------------------------------------------------------------------------- }
  MAX_SUBCATCH_RESULTS = 7;
  {*------------------------------------------------------------------------------
    Maximun link results
  ------------------------------------------------------------------------------- }
  MAX_LINK_RESULTS = 6;
  {*------------------------------------------------------------------------------
    Maximun system results
  ------------------------------------------------------------------------------- }
  MAX_SYS_RESULTS = 14;
  {*------------------------------------------------------------------------------
    Operating modes
  ------------------------------------------------------------------------------- }
  opModes: array [0 .. 1] of string = ('SWMM_TO_FW', 'SWMM_FROM_FW');
  {*------------------------------------------------------------------------------
    Applicaiton moodes
  ------------------------------------------------------------------------------- }
  appTypes: array [0 .. 1] of string = ('SWMM_CONSOLE', 'SWMM_GUI');
  {*------------------------------------------------------------------------------
    Constituent names
  ------------------------------------------------------------------------------- }
  constituentNames: array [0 .. 22] of string = ('Q', 'FC', 'TSS1', 'TSS2',
    'TSS3', 'TSS4', 'TSS5', 'TSS', 'TP', 'TDS', 'POO4', 'NO3', 'LDOM', 'RDOM',
    'LPOM', 'RPOM', 'BOD1', 'ALGAE', 'DO', 'TIC', 'ALK', 'Gen 1', 'NH4');
  // NnodeResults,NODE_DEPTH,NODE_HEAD,NODE_VOLUME,NODE_LATFLOW,NODE_INFLO,NODE_OVERFLOW;
  {*------------------------------------------------------------------------------
    Number of node variables
  ------------------------------------------------------------------------------- }
  NUMNODEVARS: integer = 7;

  // NlinkResults,LINK_FLOW,LINK_DEPTH,LINK_VELOCITY,LINK_FROUDE,LINK_CAPACITY;
  {*------------------------------------------------------------------------------
    Number of node link variables
  ------------------------------------------------------------------------------- }
  NUMLINKVARS: integer = 6;

  // input/output file names
  // provides FW timespan and hold file paths for batching
  {*------------------------------------------------------------------------------
    Groupnames file name
  ------------------------------------------------------------------------------- }
  fileNameGroupNames = 'groupnames.txt';

  {*------------------------------------------------------------------------------
    Parameter names file name
  ------------------------------------------------------------------------------- }
  fileNameParamsList = 'params.txt'; // number and list of constituents

  {*------------------------------------------------------------------------------
    Parameter names matching (framework / SWMM) file name
  ------------------------------------------------------------------------------- }
  fileNameParamsMatch = 'parametermap.txt';
  // mapping of fw constituents to swmm

  {*------------------------------------------------------------------------------
    Temporary file name
  ------------------------------------------------------------------------------- }
  fileNameScratch = 'scratch'; // fw times series file

  {*------------------------------------------------------------------------------
    Framework control file path
  ------------------------------------------------------------------------------- }
  fileNameFWControlFile = 'swmmconvertstrings.txt';

  // fw times series control metatadata file
  {*------------------------------------------------------------------------------
    Messages file name
  ------------------------------------------------------------------------------- }
  fileNameMessages = 'messages.txt';
  // communicates successes and errors to framework

var
  {*------------------------------------------------------------------------------
    Working directory
  ------------------------------------------------------------------------------- }
  workingDir: string; // exe folder
  {*------------------------------------------------------------------------------
    Filestream position
  ------------------------------------------------------------------------------- }
  SWMMFileStreamPosition: long;
  {*------------------------------------------------------------------------------
    App operating mode SWMM_TO_FW or  SWMM_FROM_FW'
  ------------------------------------------------------------------------------- }
  operatingMode: string;
  {*------------------------------------------------------------------------------
    App type SWMM_CONSOLE or SWMM_GUI
  ------------------------------------------------------------------------------- }
  appType: string;

  {*------------------------------------------------------------------------------
    Stores lists of timeseries
  ------------------------------------------------------------------------------- }
  TSList: TStringList;

  {*------------------------------------------------------------------------------
    Stores inflow timeseries lists
  ------------------------------------------------------------------------------- }
  InflowsList: TStringList;
  {*------------------------------------------------------------------------------
    Stores pollutant list
  ------------------------------------------------------------------------------- }
  PollList: TStringList;
  {*------------------------------------------------------------------------------
    Stores node names
  ------------------------------------------------------------------------------- }
  NodeNameList: TStringList;
  {*------------------------------------------------------------------------------
    Framework control file path
  ------------------------------------------------------------------------------- }
  frameCtrlFilePath: string;
  {*------------------------------------------------------------------------------
    mta file path
  ------------------------------------------------------------------------------- }
  mtaFilePath: string;


{*------------------------------------------------------------------------------
  Extracts SWMM node, inflow, timeseries, and pollutant block names or ids
  ids from the contents of a SWMM input file

  @param swmmFileContentsList SWMM input file previous read into stringlist
  @return Array of string lists of SWMM various names/IDs
------------------------------------------------------------------------------- }
function getSWMMNodeIDsFromTxtInput(swmmFileContentsList: TStringList)
  : TArray<TStringList>;

{*------------------------------------------------------------------------------
  Extracts SWMM node, inflow, timeseries, and pollutant block names or ids
  ids from the contents of a SWMM input file

  @param SWMMFilePath Binary SWMM output file path
  @return Array of string lists of SWMM various names/IDs
------------------------------------------------------------------------------- }
function getSWMMNodeIDsFromBinary(SWMMFilePath: string): TArray<TStringList>;

{*------------------------------------------------------------------------------
  Utility function to split a delimitted string into its token parts

  @param Delimiter the delimiter for spliting the string
  @param Input input delimitted string to be split
  @param result list of string tokens
------------------------------------------------------------------------------- }
procedure Split(const Delimiter: Char; Input: string; const Strings: TStrings);

{*------------------------------------------------------------------------------
  Utility function to save a text file to disc

  @param FileContentsList the contents to be saved to disc
  @param filePath location to save to on disc
  @param shdOverwrite flag dictating where file will be overwritten if it
  already exists
------------------------------------------------------------------------------- }
procedure saveTextFileToDisc(FileContentsList: TStringList; filePath: string;
  shdOverwrite: boolean = false);

{*------------------------------------------------------------------------------
  Utility function to read very long text files while avoiding memomry issues

  @param filePath path to long text file to be read
  @return TStringList stringlist containing contents of text file read in
------------------------------------------------------------------------------- }
function readLongTxtFile(filePath: string): TStringList;

implementation

uses FWIO;

function getSWMMNodeIDsFromTxtInput(swmmFileContentsList: TStringList)
  : TArray<TStringList>;
var
  rsltLists: TArray<TStringList>;
  SwmmTokens: TStringList;
  lineNumber, intTokenLoc, tempInt, i: integer;
  strLine, strToken, strObjectID, strStartDate, strEndDate: string;
begin
  intTokenLoc := 0;
  SetLength(rsltLists, 5);
  for i := Low(rsltLists) to High(rsltLists) do
    rsltLists[i] := TStringList.Create;

  lineNumber := 0;
  while lineNumber < swmmFileContentsList.Count - 1 do
  begin
    strLine := LowerCase(swmmFileContentsList[lineNumber]);

    // save simulation and end dates start date - converts to lower case so search for lower case version
    tempInt := Pos('start_date', strLine);
    if (tempInt = 1) then
      strStartDate := Trim(ReplaceStr(strLine, 'start_date', '') )
    else
    begin
      tempInt := Pos('end_date', strLine);
    if (tempInt = 1) then
      strEndDate := Trim(ReplaceStr(strLine, 'end_date', ''));
    end;

    for i := 0 to High(SWMMINPUTTOKENS) do
    begin
      strToken := LowerCase(SWMMINPUTTOKENS[i]);
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
        strLine := swmmFileContentsList[lineNumber];
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
  rsltLists[4].Add(strStartDate);
  rsltLists[4].Add(strEndDate);
  result := rsltLists;
end;

function getSWMMNodeIDsFromBinary(SWMMFilePath: string): TArray<TStringList>;
var
  Stream: TFileStream;
  Reader: TBinaryReader;
  numberOfPeriods, outputStartPos: integer;
  numSubCatchs, numLinks, numPolls, numNodes: integer;
  reportStartDate, reportTimeInterv: Double;
  days: TDateTime;
  myYear, myMonth, myDay: Word;
  myHour, myMin, mySec, myMilli: Word;
  idx: long;
  numCharsInID: integer;
  tempID: string;
  tempIDCharArr: TArray<Char>;
  nodeIDList, pollutantIDList, miscList: TStringList;
  startDateList, endDateList: TStringList;
begin

  Stream := TFileStream.Create(SWMMFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  miscList := TStringList.Create();
  endDateList := TStringList.Create();
  startDateList := TStringList.Create();

  try
    Reader := TBinaryReader.Create(Stream);
    try

      // First get number of periods from the end of the file
      Stream.Seek(-4 * sizeof(integer), soEnd);

      // the byte position where the Computed Results section of the file begins (4-byte integer)
      outputStartPos := Reader.ReadInteger;

      // number of periods
      numberOfPeriods := Reader.ReadInteger;;

      Stream.Seek(0, soBeginning);

      Reader.ReadInteger; // Magic number
      Reader.ReadInteger; // SWMM Version number

      // Flow units - a code number for the flow units that are in effect where 0 = CFS, 1 = GPM, 2 = MGD, 3 = CMS, 4 = LPS, and 5 = LPD
      miscList.Add(IntToStr(Reader.ReadInteger)); // Flow units
      numSubCatchs := Reader.ReadInteger; // # subcatchments
      numNodes := Reader.ReadInteger; // # nodes
      numLinks := Reader.ReadInteger; // # links
      numPolls := Reader.ReadInteger; // # pollutants

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

      // read pollutant units code
      miscList.Add(IntToStr(Reader.ReadInteger));

      Stream.Seek(outputStartPos - (sizeof(Double) + sizeof(integer)),
        soBeginning);

      // Get Start date and reporting timestep
      reportStartDate := Reader.ReadDouble;
      reportTimeInterv := Reader.ReadInteger;

      // compute timeseries start date
      days := reportStartDate;
      DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      startDateList.Add(IntToStr(myYear));
      startDateList.Add(IntToStr(myMonth));
      startDateList.Add(IntToStr(myDay));
      startDateList.Add(IntToStr(myHour));

      // compute timeseries end date
      days := reportStartDate + (reportTimeInterv * numberOfPeriods / 86400.0);
      DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      endDateList.Add(IntToStr(myYear));
      endDateList.Add(IntToStr(myMonth));
      endDateList.Add(IntToStr(myDay));
      endDateList.Add(IntToStr(myHour));

    finally
      Reader.free;
    end;
  finally
    Stream.free;
  end;
  SetLength(result, 5);
  result[0] := nodeIDList;
  result[1] := pollutantIDList;
  result[2] := startDateList;
  result[3] := endDateList;
  // miscellaneous variables list position 0-flow unit code, 1-pollutant conc unit code, 2-reporting timestep in seconds
  result[4] := miscList;
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
  if ((FileContentsList <> nil) and (FileContentsList.Count > 0)) then
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
        Exit;
      end;
    end;

    { First check if the file exists. }
    if (not shdOverwrite) and (FileExists(filePath)) then
      { If it exists, raise an exception. }
      raise Exception.Create
        ('Fatal Error: File already exists. Attempt to overwrite failed.')
    else
      FileContentsList.SaveToFile(filePath);
  end;
end;

function readLongTxtFile(filePath: string): TStringList;
var
  t: TextFile;
  s: TStringList;
  x: String;
begin
  s := TStringList.Create;
  AssignFile(t, filePath);
  Reset(t);
  while not eof(t) do
  begin
    Readln(t, x);
    s.Add(x);
  end;
  CloseFile(t);
  result := s;
end;

end.
