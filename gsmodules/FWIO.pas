unit FWIO;

interface

uses
  Classes, DateUtils, SysUtils, Variants, SWMMIO, Windows,
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
    startYear, startMonth, startDay, numPolls: integer;
    endYear, endMonth, endDay: integer;
    // user specified timespan variables
    userStartYear, userStartMonth, userStartDay: integer;
    userEndYear, userEndMonth, userEndDay: integer;
    swmmReportTimestepSecs: integer;
    numberOfTimesteps: long;
    fwTimeSeries: TStringList;
  end;

type
  // converted framework timeseries data structure
  ParameterMapRecord = record
    numberOfEntries: integer;
    fwNames, swmmNames, swmmTSFilePaths: TStringList;
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
function Read(fwCtrlFilePath: string): FWCtrlMetadataRecord;

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
function Write(FWCtrlRecord: FWCtrlMetadataRecord): Boolean;
function readGroupNames(): GroupNames;
function readFWControlMetadataFile(): FWCtrlMetadataRecord;
function readFWParameterMapFile(): ParameterMapRecord;

implementation

function Read(fwCtrlFilePath: string): FWCtrlMetadataRecord;
var
  FileContentsList: TStringList;
  Rslt: FWCtrlMetadataRecord;
begin
  errorsList := TStringList.Create;
  FileContentsList := TStringList.Create;
  try
    begin
      // First check if the file exists.
      if FileExists(fwCtrlFilePath) then
      begin
        // If it exists, load the data into the stringlist.
        FileContentsList.LoadFromFile(fwCtrlFilePath);

        // if file is in the proper format ie has at least 13 lines then read it in
        if (FileContentsList.Count > 13) then
        begin
          Rslt.scratchControlFilePath := SWMMIO.workingDir +
            SWMMIO.fileNameFWControlFile;
          Rslt.scratchFilePath := FileContentsList[0];
          Rslt.tsNodeName := FileContentsList[1];
          Rslt.flowConvFactor := StrToFloat(FileContentsList[2]);
          Rslt.numPolls := StrToInt(FileContentsList[3]);
          Rslt.description := FileContentsList[4];
          Rslt.startYear := StrToInt(FileContentsList[5]);
          Rslt.startMonth := StrToInt(FileContentsList[6]);
          Rslt.startDay := StrToInt(FileContentsList[7]);
          Rslt.startHourFrac := StrToFloat(FileContentsList[8]);
          Rslt.endYear := StrToInt(FileContentsList[9]);
          Rslt.endMonth := StrToInt(FileContentsList[10]);
          Rslt.endDay := StrToInt(FileContentsList[11]);
          Rslt.endHourFrac := StrToFloat(FileContentsList[12]);
          Rslt.numberOfTimesteps := StrToInt(FileContentsList[13]);
        end
        else
          errorsList.Add
            ('Not enough entries in Framework Metadata Scrach File. 14 lines expected but found '
            + IntToStr(FileContentsList.Count));
      end
      else
        errorsList.Add('Framework Metadata Scratch File not found at:' +
          fwCtrlFilePath);
    end;
  finally
    FileContentsList.Free;
  end;
  result := Rslt;
end;

function Write(FWCtrlRecord: FWCtrlMetadataRecord): Boolean;
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
      FileContentsList.Add('''' + FWCtrlRecord.scratchFilePath + '''');
      FileContentsList.Add(FWCtrlRecord.tsNodeName);
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
  Rslt.swmmTSFilePaths := TStringList.Create;

  try
    { load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMIO.workingDir +
      SWMMIO.fileNameParamsMatch);

    // 0. read number of constituents
    Rslt.numberOfEntries := StrToInt(FileContentsList[0]);
    SetLength(Rslt.convFactors, Rslt.numberOfEntries);
    SetLength(Rslt.futureFactors, Rslt.numberOfEntries);

    TempTokens := TStringList.Create;
    TempTokens.Delimiter := ','; // Each list item will be blank separated
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
    TempDateTokens.Delimiter := ','; // Each list item will be blank separated
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

function readFWControlMetadataFile(): FWCtrlMetadataRecord;
var
  Rslt: FWCtrlMetadataRecord;
  FileContentsList, TempDateTokens: TStringList;
begin

  FileContentsList := TStringList.Create;
  try
    { load the data into the stringlist. }
    FileContentsList.LoadFromFile(SWMMIO.workingDir +
      SWMMIO.fileNameFWControlFile);

    // 0. read file path from control file
    Rslt.sourceFilePath := AnsiDequotedStr(FileContentsList[0], '''');
    // store scratchfile path for convenience
    Rslt.scratchFilePath := SWMMIO.workingDir + SWMMIO.fileNameScratch;
    // store scratch control file path for convenience
    Rslt.scratchControlFilePath := SWMMIO.workingDir + SWMMIO.fileNameScratch;

    // 1. read node name from control file
    Rslt.tsNodeName := FileContentsList[1];

    TempDateTokens := TStringList.Create;
    TempDateTokens.Delimiter := ','; // Each list item will be blank separated
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

end.
