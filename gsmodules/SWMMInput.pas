{ ------------------------------------------------------------------- }
{ Unit:    SWMMOutput.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit that imports time series from a swmm result }
{ binary file and formats it for use in the the Framework. }
{ ------------------------------------------------------------------- }

unit SWMMInput;

interface

uses
  Windows, Messages, SysUtils, DateUtils, Variants, Classes, StrUtils, SWMMIO, ComCtrls, Vcl.Forms, FWIO, ConverterErrors;

// var
// errorsList: TStringList;

/// <summary>
/// Command line version of function that takes timeseries from SWMM into the
/// framework
/// </summary>
/// <param name="MTAFilePath">
/// Path to SWMM 5 control file with user options for the conversions
/// </param>
/// <returns>
/// Returns 1 if the operation was successful
/// </returns>
function consoleImportFromSWMMToFW(MTAFilePath: string): Integer;

/// <summary>
/// Function to read computed results for a single node at a specific time
/// period or time step
/// </summary>
/// <param name="period">
/// Time period or time step for which node results will be read
/// </param>
/// <param name="nodeIndex">
/// Array index of node for which node results will be read
/// </param>
/// <param name="numNodeResults">
/// Total number of node results to be read
/// </param>
/// <param name="numSubCatchs">
/// Total number of subcatchments for which results are saved
/// </param>
/// <param name="numSubCatchRslts">
/// Total number of types of subcatchment results saved by the simulation
/// </param>
/// <param name="outputStartPos">
/// Binary file position to begin reading from
/// </param>
/// <param name="bytesPerPeriod">
/// Number of bytes save to binary file for each time period or time step of
/// the simulation
/// </param>
/// <param name="Reader">
/// File stream reader
/// </param>
/// <returns>
/// Array of node results for the desired time period or time step
/// </returns>
function output_readNodeResults(period: Integer; nodeIndex: Integer;
  numNodeResults: Integer; numSubCatchs: Integer; numSubCatchRslts: Integer;
  outputStartPos: Integer; bytesPerPeriod: Integer; Reader: TBinaryReader)
  : TArray<single>;

/// <summary>
/// Function to read SWMM node results from SWMM 5 binary results file for
/// flow and all framework pollutants. Framework pollutants without matching
/// SWMM pollutants are filled in with 0s
/// </summary>
/// <param name="SWMMFilePath">
/// Path to binary SWMM 5 output file
/// </param>
/// <param name="nodeName">
/// SWMM5 node name for which results will be extracted
/// </param>
/// <param name="selectedConstituentRecs">
/// Data strucutre of constituents and user selected options
/// </param>
/// <returns>
/// Framework record / data structure to hold extracted result and timeseiries
/// </returns>
{ function getSWMMNodeResults(SWMMFilePath: string; nodeName: string;
  selectedConstituentRecs: ParameterMapRecord): FWCtrlScratchRecord; }
function getSWMMNodeResults(SWMMFilePath: string; nodeName: string;
  selectedConstituentRecs: ParameterMapRecord; FWCtrlRec: FWCtrlMetadataRecord)
  : FWCtrlMetadataRecord;

/// <summary>
/// Wrapper function that processes conversions from SWMM to the framework
/// </summary>
/// <param name="SWMMFilePath">
/// Path to binary SWMM 5 output file
/// </param>
/// <param name="fwTSFileToCreatePath">
/// Path to the location where the resulting framework timeseries will be
/// created
/// </param>
/// <param name="nodeName">
/// Name of node for which results will be extracted
/// </param>
/// <param name="selectedConstituentRecs">
/// Data strucutre of constituents and user selected options
/// </param>
/// <returns>
/// Framework record / data structure to hold extracted result and timeseiries
/// </returns>
function importFromSWMMToFW(SWMMFilePath: string; fwTSFileToCreatePath: string;
  nodeName: string; selectedConstituentRecs: ParameterMapRecord;
  FWCtrlRec: FWCtrlMetadataRecord): FWCtrlMetadataRecord;

implementation

function consoleImportFromSWMMToFW(MTAFilePath: string): Integer;
var
  // mtaData: TArray<TMTARecord>;
  fwCtrlFileData: FWCtrlMetadataRecord;
  pMapData: ParameterMapRecord;
  tempStr: string;
  i: Integer;
begin
  // read in MTA file which contains user inputs
  // mtaData := ReadMTA.Read(MTAFilePath);

  // read in FWControlMetadata file which contains inputs
  fwCtrlFileData := FWIO.readFWControlMetadataFile();

  // read in parametermap.txt which contains mappings between fw and swmm constituents and conv factors
  pMapData := readFWParameterMapFile();

  // check to see if any error occured while attempting to read the parameterMapFile
  if (ConverterErrors.errorsList.Count > 0) then
  begin
    ConverterErrors.displayErrors();
    Writeln('Operation failed.');
    result := 0;
    Exit;
  end
  else
  begin
    // Assert(Assigned(fwCtrlFileData));
    Writeln('SWMM output binary file to read from: ' +
      fwCtrlFileData.sourceFilePath + sLineBreak);
    Writeln('Target SWMM Node: ' + fwCtrlFileData.tsNodeName);
    Writeln('Generated framework scratch file output filepath: ' +
      fwCtrlFileData.scratchFilePath);
    // flow is in position 0 so highest array index is count of pollutants
    Writeln('Total number of pollutants:' + IntToStr(pMapData.numberOfEntries));

    for i := 0 + 1 to pMapData.numberOfEntries - 1 do
    begin
      Writeln(Format
        ('Pollutant %d: framework name: %s SWMM name: %s conversion factor: %12.5f',
        [i, pMapData.fwNames[i], pMapData.swmmNames[i], pMapData.convFactors[i]]
        ) + sLineBreak);
    end;

    Writeln('Now extracting SWMM timeseries. Please wait...');
    importFromSWMMToFW(fwCtrlFileData.sourceFilePath,
      fwCtrlFileData.scratchFilePath, fwCtrlFileData.tsNodeName, pMapData,
      fwCtrlFileData);

    // check to see if any errors occured while attempting to read node results
    if (ConverterErrors.errorsList.Count > 0) then
    begin
      displayErrors();
      reportErrorsToFW();
      Writeln('Operation failed.');
      Exit;
    end;

    Writeln('Operation completed successfully.');
  end;
  reportErrorsToFW();
  ConverterErrors.errorsList.Free;
  result := 1;
end;

function importFromSWMMToFW(SWMMFilePath: string; fwTSFileToCreatePath: string;
  nodeName: string; selectedConstituentRecs: ParameterMapRecord;
  FWCtrlRec: FWCtrlMetadataRecord): FWCtrlMetadataRecord;
var
  swmmNodeResults: FWCtrlMetadataRecord;
begin
  // errorsList := TStringList.Create();

  swmmNodeResults := getSWMMNodeResults(SWMMFilePath, nodeName,
    selectedConstituentRecs, FWCtrlRec);

  // check to see if any errors occured while attempting to read node results
  if (ConverterErrors.errorsList.Count > 0) then
    Exit;

  swmmNodeResults.scratchFilePath := fwTSFileToCreatePath;
  swmmNodeResults.sourceFilePath := SWMMFilePath;
  swmmNodeResults.scratchControlFilePath := SWMMIO.workingDir +
    SWMMIO.fileNameFWControlFile;

  FWIO.writeFWControlMetadataFile(swmmNodeResults);
  // check to see if any error occured while attempting to write scratch file
  if (ConverterErrors.errorsList.Count > 0) then
    Exit;

  result := swmmNodeResults;
end;

function getSWMMNodeResults(SWMMFilePath: string; nodeName: string;
  selectedConstituentRecs: ParameterMapRecord; FWCtrlRec: FWCtrlMetadataRecord)
  : FWCtrlMetadataRecord;
var
  rslt: FWCtrlMetadataRecord;
  Stream: TFileStream;
  Reader: TBinaryReader;
  Value: Integer;
  numberOfPeriods, outputStartPos, magicNum, flowUnits: Integer;
  SWMMVersion: Integer;
  numNodes, numSubCatchs, numLinks, numPolls: Integer;
  idx, currentBytePos: long;
  pollIdx, numCharsInID: Integer;
  tsResultEntryStr, tempID, tempPollHeader: string;
  tempIDCharArr: TArray<Char>;
  fwTS, nodeIDList, pollutantIDList, targetSWMPollutants: TStringList;
  pollUnits: TArray<Integer>;
  numLinkProperities, numNodeProperties, tempInt, reportTimeInterv: Integer;
  tempDouble, reportStartDate, tempReal8: Double;
  tempReal4: single;
  days: TDateTime;
  totalNumOfMatchedFRWPollutants, startPeriod, endPeriod,
    numberOfPeriodsToRead: Integer;
  formattedTSDate, formattedTSTime: string;
  myYear, myMonth, myDay: Word;
  myHour, myMin, mySec, myMilli: Word;
  nodeResultsForPeriod: TArray<single>;
  numNodeResults, numSubcatchResults: Integer;
  numlinkResults, bytesPerPeriod, targetNodeIndex: Integer;
  k, j: Integer;
  targetPollutantSWMMOrder, targetPollutantFRWOrder: TArray<Integer>;
begin
  // Assert(Assigned(selectedConstituentRecs));
  if Not(FileExists(SWMMFilePath)) then
  begin
    ConverterErrors.errorsList.Add('SWMM results file not found at: ' +
      SWMMFilePath);
    Exit;
  end;
  if (checkFileExt(SWMMFilePath, '.out') = -1) then
    Exit;

  Stream := TFileStream.Create(SWMMFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  fwTS := TStringList.Create();
  targetSWMPollutants := TStringList.Create();
  SetLength(pollUnits, selectedConstituentRecs.numberOfEntries);

  // save headers for output FW TS file#SWMM Trial Run Under pre-BMP conditions
  //fwTS.Add(Format('#NodeID:%s', [nodeName]));
  //fwTS.Add('#');
  //fwTS.Add('#');
  //tempPollHeader := '# Yr,MM,DD, hours,     FLOW,';
  tempPollHeader := 'Q';

  // for j := 1 to selectedConstituentRecs.numberOfEntries - 1 do
  for j := 0 to selectedConstituentRecs.numberOfEntries - 1 do
  begin
    if (selectedConstituentRecs.swmmNames[j] <> '') then
      targetSWMPollutants.Add(selectedConstituentRecs.swmmNames[j]);
  end;

  try
    Reader := TBinaryReader.Create(Stream);
    try
      // First get number of periods from the end of the file
      Stream.Seek(-4 * sizeof(Integer), soEnd);

      // the byte position where the Computed Results section of the file begins (4-byte integer)
      outputStartPos := Reader.ReadInteger;

      // number of periods
      numberOfPeriods := Reader.ReadInteger;;

      Stream.Seek(0, soBeginning);

      magicNum := Reader.ReadInteger; // Magic number
      SWMMVersion := Reader.ReadInteger; // Version number
      flowUnits := Reader.ReadInteger; // Flow units
      numSubCatchs := Reader.ReadInteger; // # subcatchments
      numNodes := Reader.ReadInteger; // # nodes
      numLinks := Reader.ReadInteger; // # links
      numPolls := Reader.ReadInteger; // # pollutants

      numNodeResults := MAX_NODE_RESULTS - 1 + numPolls;
      numSubcatchResults := MAX_SUBCATCH_RESULTS - 1 + numPolls;
      numlinkResults := MAX_LINK_RESULTS - 1 + numPolls;

      bytesPerPeriod := sizeof(Double) + numSubCatchs * numSubcatchResults *
        sizeof(single) + numNodes * numNodeResults * sizeof(single) + numLinks *
        numlinkResults * sizeof(single) + MAX_SYS_RESULTS * sizeof(single);

      // used in scheme for matching only selected fw pollutants
      // SetLength(targetPollutantSWMMOrder, targetSWMPollutants.Count);
      SetLength(targetPollutantSWMMOrder, numPolls);
      SetLength(targetPollutantFRWOrder, targetSWMPollutants.Count);

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

      k := 0;
      for j := 0 to numPolls - 1 do
      begin
        // search through fw names till match is found and save fw index or -1 if no match
        for idx := 0 to targetSWMPollutants.Count - 1 do
        begin
          if (AnsiCompareText(targetSWMPollutants[idx], pollutantIDList[j]) = 0)
          then
          begin
            targetPollutantSWMMOrder[j] := idx;
            targetPollutantFRWOrder[k] := idx;
            // tempPollHeader := tempPollHeader +
            // Format('%9s,', [targetSWMPollutants[idx]]);
            tempPollHeader := tempPollHeader +
              //Format('%9s,', [selectedConstituentRecs.fwNames[idx]]);
              Format(',%s', [selectedConstituentRecs.fwNames[idx]]);
            inc(k);
            break;
          end
          else
            targetPollutantSWMMOrder[j] := -1;
        end;
      end;

      // totalNumOfMatchedFRWPollutants := k+1;
      totalNumOfMatchedFRWPollutants := k;
      //fwTS.Add('#' + tempPollHeader);
      fwTS.Add(tempPollHeader);

      // skip to section in file we reached when we read in node names
      Stream.Seek(SWMMFileStreamPosition, soBeginning);
      SWMMFileStreamPosition := Stream.Position;

      // --- save codes of pollutant concentration units
      for idx := 0 to numPolls - 1 do
      begin
        pollUnits[0] := Reader.ReadInteger;
      end;

      // --- skip subcatchment area and associated codes
      tempInt := Reader.ReadInteger; // number of subcatchment properties
      tempInt := Reader.ReadInteger; // code number for subcatchment area
      if (numSubCatchs > 0) then
      begin
        currentBytePos := Stream.Position;
        Stream.Seek((numSubCatchs) * sizeof(single), currentBytePos);
      end;

      // --- skip through node type, invert, & max. depth
      numNodeProperties := Reader.ReadInteger; // 3 - number of node properties
      Reader.ReadInteger; // INPUT_TYPE_CODE
      Reader.ReadInteger; // INPUT_INVERT
      Reader.ReadInteger; // INPUT_MAX_DEPTH;
      // Type code is int rest are real4 so read type code seperately
      if (numNodes > 0) then
      begin
        currentBytePos := Stream.Position;
        Stream.Seek((numNodes) * sizeof(Integer) + (numNodes) *
          (numNodeProperties - 1) * sizeof(single), currentBytePos);
      end;

      // --- skip link type, offsets, max. depth, & length
      numLinkProperities := Reader.ReadInteger; // 3 - number of link properties
      Reader.ReadInteger; // INPUT_TYPE_CODE code number
      Reader.ReadInteger; // // INPUT_OFFSET
      Reader.ReadInteger; // // INPUT_OFFSET
      Reader.ReadInteger; // INPUT_MAX_DEPTH
      Reader.ReadInteger; // INPUT_LENGTH

      // Type code is int rest are real4 so read type code seperately
      if (numLinks > 0) then
      begin
        currentBytePos := Stream.Position;
        Stream.Seek((numLinks) * sizeof(Integer) + (numLinks) *
          (numLinkProperities - 1) * sizeof(single), currentBytePos);
      end;

      Stream.Seek(outputStartPos - (sizeof(Double) + sizeof(Integer)),
        soBeginning);

      // Get Start date and reporting timestep
      reportStartDate := Reader.ReadDouble;
      reportTimeInterv := Reader.ReadInteger;
      rslt.swmmReportTimestepSecs := reportTimeInterv;

      // calculate numberOfPeriods in user specified timespan
      numberOfPeriodsToRead := Round((FWCtrlRec.endDate - FWCtrlRec.startDate) *
        86400 / reportTimeInterv);
      // calculate period in user specified timespan to start reading from
      startPeriod := Round((FWCtrlRec.startDate - reportStartDate) * 86400 /
        reportTimeInterv);
      endPeriod := startPeriod + numberOfPeriodsToRead;
      if ((startPeriod < 0) or (numberOfPeriodsToRead > numberOfPeriods)) then
      begin
        errorsList.Add(ConverterErrors.Errs[6]);
        displayErrors();
        reportErrorsToFW();
        Exit;
      end;
      // skip to begining of user specified timespan
      Stream.Seek(outputStartPos + numberOfPeriods * bytesPerPeriod,
        soBeginning);

      // get node results for all user specified timespan
      targetNodeIndex := nodeIDList.IndexOf(nodeName);
      if (targetNodeIndex < 0) then
      begin
        errorsList.Add(Errs[8]);
        Exit;
      end;
      for idx := startPeriod to endPeriod do
      begin
        application.processmessages;
        days := reportStartDate + (reportTimeInterv * idx / 86400.0);

        DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
          mySec, myMilli);

        formattedTSDate := Format(' %s,%s,%s',
          [IntToStr(myYear), IntToStr(myMonth), IntToStr(myDay)]);
        formattedTSTime := Format(' %7.5f',
          [myHour + myMin / 60.0 + mySec / 3600.0]);

        nodeResultsForPeriod := output_readNodeResults(idx, targetNodeIndex,
          numNodeResults, numSubCatchs, numSubcatchResults, outputStartPos,
          bytesPerPeriod, Reader);

        // add formatted flow entry
        tsResultEntryStr :=
          (Format('%s,%s,%12.6e', [formattedTSDate, formattedTSTime,
          nodeResultsForPeriod[NODE_INFLOW] *
          selectedConstituentRecs.convFactors[0]]));
        {if (appType = appTypes[0]) then
          Writeln('Exporting data for: ' + formattedTSDate + ' ' +
            formattedTSTime); }

        // write results for all framework pollutants fill in zeros for fw polls that were not selected by the user
        // DO NOT DELETE may revert to this in future for pollIdx := 1 to High(constituentNames) do // pollIdx = 1 if flow

        // alternative below outputs results for matched fw pollutants only
        // for pollIdx := 0 to totalNumOfMatchedFRWPollutants - 1 do    //outputs results for matched fw pollutants only
        // loop through all swmm pollutants and save results for those with matching fw polls
        for pollIdx := 0 to numPolls - 1 do
        // outputs results for matched fw pollutants only
        begin
          if (targetPollutantSWMMOrder[pollIdx] > -1) then // there was a match
          begin
            if ((nodeResultsForPeriod[NODE_INFLOW] < MIN_WQ_FLOW)) then
              tsResultEntryStr := Format('%s,%12.6e', [tsResultEntryStr, 0.0])
            else
            begin
              { tsResultEntryStr := Format('%s,%12.5f',
                [tsResultEntryStr, nodeResultsForPeriod[NODE_QUAL +
                targetPollutantSWMMOrder[pollIdx]] *
                selectedConstituentRecs.convFactors[targetPollutantSWMMOrder
                [pollIdx]]]); }

              // Note: since flow is included as a pollutant in parametermap.txt but not in swmm the first position
              // of targetPollutantSWMMOrder[pollIdx] shd be occupied by the m
              tsResultEntryStr :=
                Format('%s,%12.6e', [tsResultEntryStr,
                nodeResultsForPeriod[NODE_QUAL + pollIdx] *
                selectedConstituentRecs.convFactors[targetPollutantSWMMOrder
                [pollIdx]]]);
            end;
          end;
        end;
        fwTS.Add(tsResultEntryStr);
      end;

      rslt.scratchFilePath := FWCtrlRec.scratchFilePath;
      rslt.tsNodeName := FWCtrlRec.tsNodeName;
      rslt.flowConvFactor := selectedConstituentRecs.convFactors[0];
      // rslt.numPolls := selectedConstituentRecs.numberOfEntries;
      rslt.description := 'Converted from SWMM 5';
      // selectedConstituentRecs[0].ModelRunScenarioID;

      // compute timeseries start date
      days := reportStartDate;
      DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      rslt.startYear := myYear;
      rslt.startMonth := myMonth;
      rslt.startDay := myDay;
      rslt.startHourFrac := myHour;

      // compute timeseries end date
      days := reportStartDate + (reportTimeInterv * numberOfPeriods / 86400.0);
      DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      rslt.endYear := myYear;
      rslt.endMonth := myMonth;
      rslt.endDay := myDay;
      rslt.endHourFrac := myHour;

      // save user specified timeseries start date variables
      DecodeDateTime(FWCtrlRec.startDate, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      rslt.userStartYear := myYear;
      rslt.userStartMonth := myMonth;
      rslt.userStartDay := myDay;
      rslt.userStartHourFrac := myHour;

      // save user specified timeseries start date variables
      DecodeDateTime(FWCtrlRec.endDate, myYear, myMonth, myDay, myHour, myMin,
        mySec, myMilli);
      rslt.userEndYear := myYear;
      rslt.userEndMonth := myMonth;
      rslt.userEndDay := myDay;
      rslt.userEndHourFrac := myHour;

      rslt.numberOfTimesteps := numberOfPeriods;
      rslt.fwTimeSeries := fwTS;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
  result := rslt;
end;

function output_readNodeResults(period: Integer; nodeIndex: Integer;
  numNodeResults: Integer; numSubCatchs: Integer; numSubCatchRslts: Integer;
  outputStartPos: Integer; bytesPerPeriod: Integer; Reader: TBinaryReader)
  : TArray<single>;

var
  bytePos: Integer;
  rslt: TArray<single>;
  idx: Integer;
begin
  SetLength(rslt, numNodeResults);
  bytePos := outputStartPos + (period - 1) * bytesPerPeriod;
  bytePos := bytePos + sizeof(Double) + numSubCatchs * numSubCatchRslts *
    sizeof(single);
  bytePos := bytePos + nodeIndex * numNodeResults * sizeof(single);
  Reader.BaseStream.Seek(bytePos, soBeginning);
  for idx := 0 to numNodeResults do
  begin
    rslt[idx] := Reader.ReadSingle;
  end;
  result := rslt;
end;

end.
