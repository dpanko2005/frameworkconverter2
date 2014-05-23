unit FWIO;

interface

uses
  Classes, DateUtils, SysUtils, StrUtils, Variants, SWMMIO, Windows,
  ConverterErrors;

type
  GroupNames = record
    // converted framework groupnames.txt contents data structure
    startDate, endDate: TDateTime;
    fileNames: TStringList;
  end;

type
  // converted framework timeseries data structure
  FWCtrlMetadataRecord = record
    sourceFilePath, scratchFilePath, scratchControlFilePath, tsNodeName,
      description: string;
    flowConvFactor, endHourFrac, startHourFrac, userStartHourFrac,
      userEndHourFrac: double;
    // swmm time series timespan variables
    startDate, endDate: TDateTime;
    startYear, startMonth, startDay: integer;
    endYear, endMonth, endDay: integer;
    // user specified timespan variables
    userStartYear, userStartMonth, userStartDay: integer;
    userEndYear, userEndMonth, userEndDay: integer;
    swmmReportTimestepSecs: integer;
    numberOfTimesteps: long;
    fwTimeSeries, swmmTSFilePaths: TStringList;
    swmmTimeSeries: TArray<TStringList>; // holds converted swmm TS
    fwTimeSeriesNames: TStringList // holds fw TS names in order of scratchfile
    end;

  type
    // converted framework timeseries data structure
    ParameterMapRecord = record
      numberOfEntries: integer;
      fwNames, swmmNames: TStringList;
      convFactors: TArray<double>;
      futureFactors: TArray<double>; // not used in current version
    end;

    /// <summary>
    /// Function for reading contents of framework control scratch file. Outputs
    /// a structured representation of the contents of the framework control
    /// scratch file
    /// </summary>
    /// <param name="fwCtrlFilePath">
    /// full path to the framework control scratch file to be read
    /// </param>
    /// <returns>
    /// Structured record data structure with contents of the framework control
    /// file
    /// </returns>
    // function Read(fwCtrlFilePath: string): FWCtrlMetadataRecord;

    /// <summary>
    /// Function to write a framework control scratch file to disc as text file. Also writes the framework
    /// timeseries reference in the file to disc
    /// </summary>
    /// <param name="FWCtrlRecord">
    /// Data structure for holding contents of framework control file. Has
    /// properties that map to the various options in the framework scratch file
    /// </param>
    /// <returns>
    /// True if write operation is successful
    /// </returns>
    /// <remarks>
    /// None
    /// </remarks>
  function writeFWControlMetadataFile(FWCtrlRecord
    : FWCtrlMetadataRecord): Boolean;
  function readGroupNames(): GroupNames;
  function readFWControlMetadataFile(): FWCtrlMetadataRecord;
  function readFWParameterMapFile(): ParameterMapRecord;
  function readInFrameworkTSFile(pMapData: ParameterMapRecord;
    fwCtrlFileData: FWCtrlMetadataRecord): FWCtrlMetadataRecord;
  function readSWMMInputFile(swmmInputFilePath:String): TStringList;

implementation

{ function readFile(filePath: string): TStringList;
  var
  tempList: TStringList;
  begin
  tempList := TStringList.Create;
  tempList.LoadFromFile(filePath);
  result := tempList;
  end; }

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
      // FileContentsList.Add('''' + FWCtrlRecord.scratchFilePath + '''');
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
      // FileContentsList.Add(FloatToStr(FWCtrlRecord.convFactor));
      // FileContentsList.Add(IntToStr(FWCtrlRecord.numPolls) + '''');
      // FileContentsList.Add(FWCtrlRecord.description);
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
      { fwControlFilePath := ExtractFilePath(FWCtrlRecord.scratchFilePath) +
        'swmmconvertstring.txt'; }
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

function readSWMMInputFile(swmmInputFilePath:String): TStringList;
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
    // Rslt.SWMMFileContentsList := SWMMFileContentsList;
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
  // tempStrList := TStringList.Create;
  // errorsList := TStringList.Create;
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
          // strLine := StringReplace(strLine, '##', '', [rfReplaceAll]);
          // ExtractStrings([','], [], PChar(strLine), tempStrList);
          // FLOW starts at index 4 in scratch file
          for I := 5 to TempTokens.Count - 1 do
            fwCtrlFileData.fwTimeSeriesNames.Add(TempTokens[I]);
        end;

        // ignore comment lines
        if (Pos('#', strLine) < 1) and (Length(strLine) > 1) then
        begin
          // tempStrList.Clear();
          // ExtractStrings([','], [], PChar(strLine), tempStrList);
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
