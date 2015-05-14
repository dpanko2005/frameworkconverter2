{*------------------------------------------------------------------------------
  Delphi Pascal unit containing various utility functions, and records,
  primarily used for reading and writting input/output files for the framework

  @unit    FWIO.pas
  @project WERF Framework - SWMM Converter
  @version 2.0
  @date    2/28/2014
  @Author  Gesoyntec Consultants Inc (D. Pankani)
------------------------------------------------------------------------------- }
unit FWIO;

interface

uses
  Classes, DateUtils, SysUtils, StrUtils, Variants, SWMMIO, Windows,
  ConverterErrors;


type
{*------------------------------------------------------------------------------
  Data structure for holding groupname input variables read in from
  groupnames.txt

-------------------------------------------------------------------------------}
  GroupNames = record
    // converted framework groupnames.txt contents data structure
{*------------------------------------------------------------------------------
  groupnames.txt contents start date
-------------------------------------------------------------------------------}
    startDate: TDateTime;
{*------------------------------------------------------------------------------
  groupnames.txt contents end date
-------------------------------------------------------------------------------}
    endDate: TDateTime;
{*------------------------------------------------------------------------------
  list of swmm output file paths to be processed
-------------------------------------------------------------------------------}
    fileNames: TStringList;
  end;


type
{*------------------------------------------------------------------------------
  Data structure for holding control input variables read in from text input
  files.
-------------------------------------------------------------------------------}
  FWCtrlMetadataRecord = record
    // converted framework timeseries data structure
    //sourceFilePath, scratchFilePath, scratchControlFilePath, tsNodeName,
    //  description: string;
{*------------------------------------------------------------------------------
  File path of the framework control file
-------------------------------------------------------------------------------}
      sourceFilePath: string;
{*------------------------------------------------------------------------------
  scratch file path
-------------------------------------------------------------------------------}
      scratchFilePath: string;
{*------------------------------------------------------------------------------
  scratch control file path
-------------------------------------------------------------------------------}
      scratchControlFilePath: string;
{*------------------------------------------------------------------------------
  desired timeseries node name
-------------------------------------------------------------------------------}
      tsNodeName: string;
{*------------------------------------------------------------------------------
  framework control file description entry
-------------------------------------------------------------------------------}
      description: string;
    //flowConvFactor, endHourFrac, startHourFrac, userStartHourFrac,
    //  userEndHourFrac: double;


{*------------------------------------------------------------------------------
  framework flow conversion factor
-------------------------------------------------------------------------------}
      flowConvFactor: double;
{*------------------------------------------------------------------------------
  framework timespan end hour fraction
-------------------------------------------------------------------------------}
      endHourFrac: double;
{*------------------------------------------------------------------------------
  framework timespan start hour fraction
-------------------------------------------------------------------------------}
      startHourFrac: double;
{*------------------------------------------------------------------------------
  framework timespan user start hour fraction
-------------------------------------------------------------------------------}
      userStartHourFrac: double;
{*------------------------------------------------------------------------------
  framework timespan user end hour fraction
-------------------------------------------------------------------------------}
      userEndHourFrac: double;

    // swmm time series timespan variables
{*------------------------------------------------------------------------------
  swmm time series timespan - start date
-------------------------------------------------------------------------------}
    startDate: TDateTime;
{*------------------------------------------------------------------------------
  swmm time series timespan - end date
-------------------------------------------------------------------------------}
    endDate: TDateTime;

{*------------------------------------------------------------------------------
  swmm time series timespan - start year
-------------------------------------------------------------------------------}
    startYear: integer;
{*------------------------------------------------------------------------------
  swmm time series timespan - start month
-------------------------------------------------------------------------------}
    startMonth: integer;
{*------------------------------------------------------------------------------
  swmm time series timespan - start day
-------------------------------------------------------------------------------}
    startDay: integer;
{*------------------------------------------------------------------------------
  swmm time series timespan - end year
-------------------------------------------------------------------------------}
    endYear : integer;
{*------------------------------------------------------------------------------
  swmm time series timespan - end month
-------------------------------------------------------------------------------}
    endMonth: integer;
{*------------------------------------------------------------------------------
  swmm time series timespan - end day
-------------------------------------------------------------------------------}
    endDay: integer;

{*------------------------------------------------------------------------------
  user specified timespan - start year
-------------------------------------------------------------------------------}
    userStartYear: integer;
{*------------------------------------------------------------------------------
  user specified timespan - start month
-------------------------------------------------------------------------------}
    userStartMonth: integer;
{*------------------------------------------------------------------------------
  user specified timespan - start day
-------------------------------------------------------------------------------}
    userStartDay: integer;
{*------------------------------------------------------------------------------
  user specified timespan - end day
-------------------------------------------------------------------------------}
    userEndDay: integer;
{*------------------------------------------------------------------------------
  user specified timespan - end month
-------------------------------------------------------------------------------}
    userEndMonth: integer;
{*------------------------------------------------------------------------------
  user specified timespan - end year
-------------------------------------------------------------------------------}
    userEndYear: integer;
{*------------------------------------------------------------------------------
  SWMM simulation reporting timestep
-------------------------------------------------------------------------------}
    swmmReportTimestepSecs: integer;
{*------------------------------------------------------------------------------
  SWMM simulation timestep
-------------------------------------------------------------------------------}
    numberOfTimesteps: long;
{*------------------------------------------------------------------------------
  Framework timeseries
-------------------------------------------------------------------------------}
    fwTimeSeries: TStringList;
{*------------------------------------------------------------------------------
  SWMM timeseries file paths
-------------------------------------------------------------------------------}
    swmmTSFilePaths: TStringList;
{*------------------------------------------------------------------------------
  holds converted swmm timeseries
-------------------------------------------------------------------------------}
    swmmTimeSeries: TArray<TStringList>;
{*------------------------------------------------------------------------------
  holds fw TS names in order of scratchfile
-------------------------------------------------------------------------------}
    fwTimeSeriesNames: TStringList
    end;


  type
{*------------------------------------------------------------------------------
  Data structure for holding a converted framework timeseries.
-------------------------------------------------------------------------------}
    ParameterMapRecord = record
{*------------------------------------------------------------------------------
  Number of entries in the record.
-------------------------------------------------------------------------------}
      numberOfEntries: integer;
{*------------------------------------------------------------------------------
  List of framework parameter names, each one has a corresponding swmm parameter
  name
-------------------------------------------------------------------------------}
      fwNames: TStringList;
{*------------------------------------------------------------------------------
  List of SWMM parameter names, each one has a corresponding framework parameter
  name
-------------------------------------------------------------------------------}
      swmmNames: TStringList;
{*------------------------------------------------------------------------------
  Framework to swmm conversion factors
-------------------------------------------------------------------------------}
      convFactors: TArray<double>;
{*------------------------------------------------------------------------------
  Factors for future features not currently used
-------------------------------------------------------------------------------}
      futureFactors: TArray<double>; // not used in current version
    end;

  {*------------------------------------------------------------------------------
    Function to write a framework control scratch file to disc as text file.
    Also writes the framework timeseries reference in the file to disc

    @param Data structure for holding contents of framework control file. Has
    properties that map to the various options in the framework scratch file
    @param desiredExt Desired extention to check the file path for
    @return TRUE if write operation is successful, FALSE otherwise
  ------------------------------------------------------------------------------- }
  function writeFWControlMetadataFile(FWCtrlRecord
    : FWCtrlMetadataRecord): Boolean;

  {*------------------------------------------------------------------------------
    Reads framework group names from group names file on disc

    @return GroupNames record if successful and nothing otherwise
  ------------------------------------------------------------------------------- }
  function readGroupNames(): GroupNames;

  {*------------------------------------------------------------------------------
    Reads framework control file from disc

    @return FWCtrlMetadataRecord record if successful and nothing otherwise
  ------------------------------------------------------------------------------- }
  function readFWControlMetadataFile(): FWCtrlMetadataRecord;

  {*------------------------------------------------------------------------------
    Reads framework parmeter file from disc

    @return ParameterMapRecord record if successful and nothing otherwise
  ------------------------------------------------------------------------------- }
  function readFWParameterMapFile(): ParameterMapRecord;

  {*------------------------------------------------------------------------------
    Reads framework timeseries file for subsequent conversion into other forms

    @param pMapData ParameterMapRecord that maps framework constituents to SWMM
    constituents
    @param fwCtrlFileData record holding Framework control file inputs
    @return FWCtrlMetadataRecord record if successful and nothing otherwise
  ------------------------------------------------------------------------------- }
  function readInFrameworkTSFile(pMapData: ParameterMapRecord;
    fwCtrlFileData: FWCtrlMetadataRecord): FWCtrlMetadataRecord;

  {*------------------------------------------------------------------------------
    Reads in SWMM input file

    @param swmmInputFilePath path to SWMM input file on disc
    @return Stringlist containing lines of text from SWMM input file
  ------------------------------------------------------------------------------- }
  function readSWMMInputFile(swmmInputFilePath: String): TStringList;

implementation

function writeFWControlMetadataFile(FWCtrlRecord: FWCtrlMetadataRecord)
  : Boolean;
var
  FileContentsList: TStringList;
  fwControlFilePath: string;
begin
  FileContentsList := TStringList.Create;
  try
    begin
      // if no description was provided, use the default descrition
      if (FWCtrlRecord.description = '') then
        FWCtrlRecord.description := 'Converted from SWMM 5';

      // add structured data record contents as an ordered set of entries
      FileContentsList.Add('''' + FWCtrlRecord.sourceFilePath + '''');
      FileContentsList.Add('''' + FWCtrlRecord.tsNodeName + '''');
      FileContentsList.Add(Format('''%d'',''%d'',''%d''',
        [FWCtrlRecord.userStartYear, FWCtrlRecord.userStartMonth,
        FWCtrlRecord.userStartDay]));
      FileContentsList.Add(Format('''%d'',''%d'',''%d''',
        [FWCtrlRecord.userEndYear, FWCtrlRecord.userEndMonth,
        FWCtrlRecord.userEndDay]));
      // number of custom strings following this line - currently 0 for SWMM converter
      FileContentsList.Add('0');
      FileContentsList.Add(IntToStr(FWCtrlRecord.userStartYear));
      FileContentsList.Add(IntToStr(FWCtrlRecord.userStartMonth));
      FileContentsList.Add(IntToStr(FWCtrlRecord.userStartDay));
      FileContentsList.Add(FloatToStr(FWCtrlRecord.userStartHourFrac));
      FileContentsList.Add(IntToStr(FWCtrlRecord.userEndYear));
      FileContentsList.Add(IntToStr(FWCtrlRecord.userEndMonth));
      FileContentsList.Add(IntToStr(FWCtrlRecord.userEndDay));
      FileContentsList.Add(FloatToStr(FWCtrlRecord.userEndHourFrac));
      FileContentsList.Add(IntToStr(FWCtrlRecord.numberOfTimesteps));

      // save the framework timeseries in the record
      If (Assigned(FWCtrlRecord.fwTimeSeries)) then
      begin
        saveTextFileToDisc(FWCtrlRecord.fwTimeSeries,
          FWCtrlRecord.scratchFilePath, true);
      end;

      // save the framework control metadata scratchfile
      saveTextFileToDisc(FileContentsList,
        FWCtrlRecord.scratchControlFilePath, true);
    end;
  finally
    FileContentsList.Free();
  end;
  result := true;
end;

function readFWParameterMapFile(): ParameterMapRecord;
var
  Rslt: ParameterMapRecord;
  FileContentsList, TempTokens: TStringList;
  I: integer;
begin

  FileContentsList := TStringList.Create;
  Rslt.fwNames := TStringList.Create;
  Rslt.swmmNames := TStringList.Create;

  try
    { load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMIO.workingDir +
      SWMMIO.fileNameParamsMatch);

    // 0. read number of constituents
    Rslt.numberOfEntries := StrToInt(FileContentsList[0]);
    SetLength(Rslt.convFactors, Rslt.numberOfEntries);
    SetLength(Rslt.futureFactors, Rslt.numberOfEntries);

    TempTokens := TStringList.Create;
    TempTokens.Delimiter := ','; // Each list item will be comma separated
    TempTokens.QuoteChar := ''''; // And each item will be quoted with '

    // 1. read and convert mappings and save into record
    for I := 1 to FileContentsList.Count - 1 do
    begin
      TempTokens.DelimitedText := FileContentsList[I];
      if (TempTokens.DelimitedText <> '') then
      begin
        Rslt.fwNames.Add(TempTokens[0]);
        Rslt.convFactors[I - 1] := StrToFloat(TempTokens[1]);
        Rslt.futureFactors[I - 1] := StrToFloat(TempTokens[2]);
        Rslt.swmmNames.Add(TempTokens[3]);
      end;
    end;

    result := Rslt;
  finally
    FileContentsList.Free;
  end;
end;

function readGroupNames(): GroupNames;
var
  Rslt: GroupNames;
  FileContentsList, TempDateTokens: TStringList;
begin
  FileContentsList := TStringList.Create;
  try
    { load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMIO.workingDir +
      SWMMIO.fileNameGroupNames);

    TempDateTokens := TStringList.Create;

    // Add supported MTA token names to the list of tokens to search for
    TempDateTokens.Delimiter := ','; // Each list item will be comma separated
    TempDateTokens.QuoteChar := ''''; // And each item will be quoted with '

    // convert start date tokens into date object
    TempDateTokens.DelimitedText := FileContentsList[0];
    Rslt.startDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2]
      + '/' + TempDateTokens[0]);

    // convert end date tokens into date object
    TempDateTokens.DelimitedText := FileContentsList[1];
    Rslt.endDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2] +
      '/' + TempDateTokens[0]);

    result := Rslt;
  finally
    FileContentsList.Free;
  end;
end;

function readSWMMInputFile(swmmInputFilePath: String): TStringList;
var
  FileContentsList: TStringList;
begin

  FileContentsList := TStringList.Create;
  { load the data into the stringlist. }
  FileContentsList.LoadFromFile(swmmInputFilePath);
  result := FileContentsList;
end;

function readFWControlMetadataFile(): FWCtrlMetadataRecord;
var
  Rslt: FWCtrlMetadataRecord;
  FileContentsList, SWMMFileContentsList, TempDateTokens: TStringList;
begin

  FileContentsList := TStringList.Create;
  SWMMFileContentsList := TStringList.Create;
  try
    { load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMIO.workingDir +
      SWMMIO.fileNameFWControlFile);

    // 0. read file path from control file
    Rslt.sourceFilePath := AnsiDequotedStr(FileContentsList[0], '''');

    // SWMMFileContentsList.LoadFromFile(Rslt.sourceFilePath);
    // store scratchfile path for convenience
    Rslt.scratchFilePath := SWMMIO.workingDir + SWMMIO.fileNameScratch;
    // store scratch control file path for convenience
    Rslt.scratchControlFilePath := SWMMIO.workingDir + SWMMIO.fileNameScratch;

    // 1. read node name from control file
    Rslt.tsNodeName := AnsiDequotedStr(FileContentsList[1], '''');

    TempDateTokens := TStringList.Create;
    TempDateTokens.Delimiter := ','; // Each list item will be comma separated
    TempDateTokens.QuoteChar := ''''; // And each item will be quoted with '

    // 2. convert start date tokens into date object
    TempDateTokens.DelimitedText := FileContentsList[2];
    Rslt.startDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2]
      + '/' + TempDateTokens[0]);

    // 3. convert end date tokens into date object
    TempDateTokens.DelimitedText := FileContentsList[3];
    Rslt.endDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2] +
      '/' + TempDateTokens[0]);

    result := Rslt;
  finally
    FileContentsList.Free;
  end;
end;

function readInFrameworkTSFile(pMapData: ParameterMapRecord;
  fwCtrlFileData: FWCtrlMetadataRecord): FWCtrlMetadataRecord;
var
  FileContentsList, TempTokens: TStringList;
  strLine: string;
  lineNumber: integer;
  // tempStrList: TStrings;
  tempTimeVal: double;
  tempDateTimeStr, tempTimeStr: string;
  tempValueStr: string;
  I: integer;
  j: integer;
begin
  FileContentsList := TStringList.Create;
  fwCtrlFileData.fwTimeSeriesNames := TStringList.Create;

  { First check if the file exists. }
  if Not(FileExists(fwCtrlFileData.scratchFilePath)) then
  begin
    errorsList.Add(Errs[9] + fwCtrlFileData.scratchFilePath);
    exit;
  end;
  try
    begin
      // includes flow so do not subtract 1
      SetLength(fwCtrlFileData.swmmTimeSeries, pMapData.numberOfEntries);
      for I := 0 to pMapData.numberOfEntries do
      begin
        fwCtrlFileData.swmmTimeSeries[I] := TStringList.Create;
      end;

      FileContentsList.LoadFromFile(fwCtrlFileData.scratchFilePath);
      lineNumber := 0;
      TempTokens := TStringList.Create;
      TempTokens.Delimiter := ','; // Each list item will be comma separated
      TempTokens.QuoteChar := ''''; // And each item will be quoted with '

      while lineNumber < FileContentsList.Count do
      begin
        strLine := FileContentsList[lineNumber];

        // extract names of polls in the scratch file to convert to swmm names later in other routines
        if (Pos('##', strLine) > 0) and (Length(strLine) > 1) then
        begin
          // split line, extract FLOW and other constituent names
          TempTokens.DelimitedText := strLine;

          // FLOW starts at index 4 in scratch file
          for I := 5 to TempTokens.Count - 1 do
            fwCtrlFileData.fwTimeSeriesNames.Add(TempTokens[I]);
        end;

        // ignore comment lines
        if (Pos('#', strLine) < 1) and (Length(strLine) > 1) then
        begin
          TempTokens.DelimitedText := strLine;
          tempTimeVal := StrToFloat(TempTokens[3]);
          tempTimeStr := FloatToStr(int(tempTimeVal)) + ':' +
            FormatFloat('00', frac(tempTimeVal) * 60);
          tempDateTimeStr := Format('%.2d/%.2d/%s	%5s',
            [StrToInt(TempTokens[1]), StrToInt(TempTokens[2]),
            trim(TempTokens[0]), tempTimeStr]);

          for I := 0 to pMapData.numberOfEntries - 1 do
          begin
            j := I + 4;
            if (j < TempTokens.Count) then
            begin
              tempValueStr := TempTokens[j];
              fwCtrlFileData.swmmTimeSeries[I]
                .Add(tempDateTimeStr + '	' + tempValueStr);
            end;
          end;
        end;
        inc(lineNumber);
      end;
    end;
  finally
    FileContentsList.Free;
    TempTokens.Free;
  end;
  result := fwCtrlFileData;
end;

end.
