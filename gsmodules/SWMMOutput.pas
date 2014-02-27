unit SWMMOutput;

interface

uses
  Windows, Messages, SysUtils, DateUtils, Variants, Classes, StrUtils, SWMMIO;

function output_readNodeResults(period: integer; nodeIndex: integer;
  numNodeResults: integer; numSubCatchs: integer; numSubCatchRslts: integer;
  outputStartPos: integer; bytesPerPeriod: integer; Reader: TBinaryReader)
  : TArray<single>;
function getSWMMNodeResults(SWMMFilePath: string; nodeName: string;
  targetSWMPollutants: TStringList; selectedConstituentRecs: TArray<TMTARecord>)
  : TStringList;
function importFromSWMMToFW(SWMMFilePath: string; fwTSFileToCreatePath: string;
  nodeName: string; targetSWMPollutants: TStringList;
  selectedConstituentRecs: TArray<TMTARecord>): TStringList;

implementation

function importFromSWMMToFW(SWMMFilePath: string; fwTSFileToCreatePath: string;
  nodeName: string; targetSWMPollutants: TStringList;
  selectedConstituentRecs: TArray<TMTARecord>): TStringList;
var
  swmmNodeResults: TStringList;
begin
  swmmNodeResults := getSWMMNodeResults(SWMMFilePath, nodeName,
    targetSWMPollutants, selectedConstituentRecs);
  SWMMIO.saveTextFileToDisc(swmmNodeResults, fwTSFileToCreatePath);
  result := swmmNodeResults;
end;

function getSWMMNodeResults(SWMMFilePath: string; nodeName: string;
  targetSWMPollutants: TStringList; selectedConstituentRecs: TArray<TMTARecord>)
  : TStringList;

var
  Stream: TFileStream;
  Reader: TBinaryReader;
  Value: integer;
  numberOfPeriods, outputStartPos, magicNum, flowUnits: integer;
  SWMMVersion: integer;
  numNodes, numSubCatchs, numLinks, numPolls: integer;
  idx,currentBytePos: long;
  pollIdx, numCharsInID: integer;
  tsResultEntryStr, tempID: string;
  tempIDCharArr: TArray<Char>;
  rslt, nodeIDList, pollutantIDList: TStringList;
  pollUnits: TArray<integer>;
  numLinkProperities, numNodeProperties, tempInt: integer;
  tempDouble, reportStartDate, reportTimeInterv, tempReal8: Double;
  tempReal4: single;
  days: TDateTime;
  totalNumOfMatchedFRWPollutants: integer;
  formattedTSDate, formattedTSTime: string;
  myYear, myMonth, myDay: Word;
  myHour, myMin, mySec, myMilli: Word;
  nodeResultsForPeriod: TArray<single>;
  numNodeResults, numSubcatchResults: integer;
  numlinkResults, bytesPerPeriod, targetNodeIndex: integer;
  k, j: integer;
  targetPollutantSWMMOrder, targetPollutantFRWOrder: TArray<integer>;
begin
  Assert(Assigned(selectedConstituentRecs));
  Stream := TFileStream.Create(SWMMFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  rslt := TStringList.Create();
  SetLength(pollUnits, High(selectedConstituentRecs));

  // save headers for output FW TS file#SWMM Trial Run Under pre-BMP conditions
  rslt.add(Format('#NodeID:%s', [nodeName]));
  rslt.add('#');
  rslt.add('#');
  rslt.add('# Year,MM,DD,   hours,     flow,     TSSf,      TPf,     TCuf');

  SetLength(targetPollutantSWMMOrder, targetSWMPollutants.Count);
  SetLength(targetPollutantFRWOrder, targetSWMPollutants.Count);

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

      // Read all subcatchment IDs and discard, skipping this section is not straight forward since catchment
      // name lengths vary
      for idx := 0 to numSubCatchs - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
        // if Length(tempIDCharArr) > 0 then
        // SetString(tempID, PChar(@tempIDCharArr[0]), Length(tempIDCharArr));
      end;

      // Read all node IDs and save for use later
      for idx := 0 to numNodes - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
        if Length(tempIDCharArr) > 0 then
        begin
          SetString(tempID, PChar(@tempIDCharArr[0]), Length(tempIDCharArr));
          nodeIDList.add(tempID);
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
          pollutantIDList.add(tempID);
        end
        else
      end;

      // Match swmm pollutants to framework pollutants and determine pollutant order - order by framework pollutants
      k := 0;
      for idx := 0 to targetSWMPollutants.Count - 1 do
      begin
        for j := 0 to numPolls do
        begin
          if (AnsiCompareText(targetSWMPollutants[idx], pollutantIDList[j]) = 0)
          then
          begin
            targetPollutantSWMMOrder[idx] := j;
            targetPollutantFRWOrder[k] := idx;
            inc(k);
            break;
          end;
        end;
      end;
      totalNumOfMatchedFRWPollutants := k;

      // skip to section in file we reached when we read in node names
      Stream.Seek(SWMMFileStreamPosition, soBeginning);
      SWMMFileStreamPosition := Stream.Position;

      // --- save codes of pollutant concentration units
      for idx := 0 to numPolls do
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
        Stream.Seek((numNodes) * sizeof(integer) + (numNodes) *
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
        Stream.Seek((numLinks) * sizeof(integer) + (numLinks) *
          (numLinkProperities - 1) * sizeof(single), currentBytePos);
      end;

      Stream.Seek(outputStartPos - (sizeof(Double) + sizeof(integer)),
        soBeginning);

      // Get Start date and reporting timestep
      reportStartDate := Reader.ReadDouble;

      // StartDateTime  = getDateTime(reportStartDate)
      reportTimeInterv := Reader.ReadInteger;

      // get node results for all time periods
      // ============================================
      // targetNodeIndex := nodeNameList.IndexOf(nodeName);
      targetNodeIndex := nodeIDList.IndexOf(nodeName);
      for idx := 1 to numberOfPeriods do
      begin
        days := reportStartDate + (reportTimeInterv * idx / 86400.0);
        if (idx = 1) then
        begin
          DecodeDateTime(days, myYear, myMonth, myDay, myHour, myMin,
            mySec, myMilli);
        end;
        formattedTSDate := Format(' %s,%s,%s',
          [IntToStr(myYear), IntToStr(myMonth), IntToStr(myDay)]);
        formattedTSTime := Format(' %7.5f',
          [myHour + myMin / 60.0 + mySec / 3600.0]);

        nodeResultsForPeriod := output_readNodeResults(idx, targetNodeIndex,
          numNodeResults, numSubCatchs, numSubcatchResults, outputStartPos,
          bytesPerPeriod, Reader);

        // add formatted flow entry
        tsResultEntryStr :=
          (Format('%s,%s,%9.3f', [formattedTSDate, formattedTSTime,
          nodeResultsForPeriod[NODE_INFLOW] * selectedConstituentRecs[0]
          .convFactor]));

        for pollIdx := 0 to totalNumOfMatchedFRWPollutants - 1 do
        begin
          if (nodeResultsForPeriod[NODE_INFLOW] < MIN_WQ_FLOW) then
            tsResultEntryStr := Format('%s,%9.3f', [tsResultEntryStr, 0.0])
          else
          begin
            tsResultEntryStr := Format('%s,%9.3f',
              [tsResultEntryStr, nodeResultsForPeriod[NODE_QUAL +
              targetPollutantSWMMOrder[pollIdx]] * selectedConstituentRecs
              [pollIdx].convFactor]);
          end;
        end;
        rslt.add(tsResultEntryStr);
      end;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
  result := rslt;
end;

function output_readNodeResults(period: integer; nodeIndex: integer;
  numNodeResults: integer; numSubCatchs: integer; numSubCatchRslts: integer;
  outputStartPos: integer; bytesPerPeriod: integer; Reader: TBinaryReader)
  : TArray<single>;

// Purpose: reads computed results for a node at a specific time period.
//
var
  bytePos: integer;
  rslt: TArray<single>;
  idx: integer;
begin
  SetLength(rslt, numNodeResults);
  bytePos := outputStartPos + (period - 1) * bytesPerPeriod;
  bytePos := bytePos + sizeof(Double) + numSubCatchs * numSubCatchRslts *
    sizeof(single); // (5.0.014 - LR)
  bytePos := bytePos + nodeIndex * numNodeResults * sizeof(single);
  Reader.BaseStream.Seek(bytePos, soBeginning);
  for idx := 0 to numNodeResults do
  begin
    rslt[idx] := Reader.ReadSingle;
  end;
  result := rslt;
end;

end.
