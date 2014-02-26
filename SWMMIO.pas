unit SWMMIO;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils,
  Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons;

type
  TConvertedFWTS = record // converted framework timeseries data structure
    tsName: string;
    tsNodeName: string;
    tsType: string; // FLOW or CONCEN
    tsUnitsFactor: string; // default 1.0
    constituentName: string;
    convFactor: Double; // default 1.0
    convertedTS: TStringList;
    convertedTSFilePath: string;
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

  // NnodeResults,NODE_DEPTH,NODE_HEAD,NODE_VOLUME,NODE_LATFLOW,NODE_INFLO,NODE_OVERFLOW;
  NUMNODEVARS: integer = 7;

  // NlinkResults,LINK_FLOW,LINK_DEPTH,LINK_VELOCITY,LINK_FROUDE,LINK_CAPACITY;
  NUMLINKVARS: integer = 6;

var
  SWMMFileStreamPosition: long;
  // stores file stream seek position after node and poll names are read

function getSWMMNodeNames(swmmFilePath: string): TArray<TStringList>;
function getSWMMNodeResults(swmmFilePath: string; nodeName: string;
  selectedPollutantRecs: TArray<TConvertedFWTS>): TStringList;
function output_readNodeResults(period: integer; nodeIndex: integer;
  numNodeResults: integer; numSubCatchs: integer; numSubCatchRslts: integer;
  outputStartPos: integer; bytesPerPeriod: integer; Reader: TBinaryReader)
  : TArray<single>;

implementation

function getSWMMNodeNames(swmmFilePath: string): TArray<TStringList>;
var
  Stream: TFileStream;
  Reader: TBinaryReader;
  Value: integer;

  // numberOfPeriods: integer;
  // OutputStartPos: integer;
  // bytePos: integer;
  magicNum: integer;
  flowUnits: integer;
  SWMMVersion: integer;
  // byteOffset: integer;
  numNodes: integer;
  numSubCatchs: integer;
  numLinks: integer;
  numPolls: integer;
  idx: long;
  numCharsInID: integer;
  tempID: string;
  tempIDCharArr: TArray<Char>;
  nodeIDList: TStringList;
  pollutantIDList: TStringList;
begin
  Stream := TFileStream.Create(swmmFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  try
    Reader := TBinaryReader.Create(Stream);
    try
      // Value := Reader.ReadInteger;
      // --- get number of objects reported on
      numSubCatchs := 0;
      numNodes := 0;
      numLinks := 0;
      numPolls := 0;

      { // First get number of periods from the end of the file
        Stream.Seek(-1, soEnd);
        OutputStartPos := Reader.ReadInteger;
        // the byte position where the Computed Results section of the file begins (4-byte integer)
        numberOfPeriods := Reader.ReadInteger;; // number of periods
        // metaDataFound.numread=numberOfPeriods;
        Stream.Seek(-1, soBeginning); }

      magicNum := Reader.ReadInteger; // Magic number
      SWMMVersion := Reader.ReadInteger; // Version number
      flowUnits := Reader.ReadInteger; // Flow units
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
          nodeIDList.Add(tempID);
        end
      end;

      // Read all link IDs and discard, skipping this section is not straight forward since catchment
      // name lengths vary
      for idx := 0 to numLinks - 1 do
      begin
        numCharsInID := Reader.ReadInteger;
        tempIDCharArr := Reader.ReadChars(numCharsInID);
        { if Length(tempIDCharArr) > 0 then
          begin
          SetString(tempID, PChar(@tempIDCharArr[0]), Length(tempIDCharArr))
          end
          else }
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

function getSWMMNodeResults(swmmFilePath: string; nodeName: string;
  selectedPollutantRecs: TArray<TConvertedFWTS>): TStringList;
var
  Stream: TFileStream;
  Reader: TBinaryReader;
  Value: integer;

  numberOfPeriods: integer;
  outputStartPos: integer;
  // bytePos: integer;
  magicNum: integer;
  flowUnits: integer;
  SWMMVersion: integer;
  // byteOffset: integer;
  numNodes: integer;
  numSubCatchs: integer;
  numLinks: integer;
  numPolls: integer;
  idx: long;
  pollIdx: integer;
  numCharsInID: integer;
  tempID: string;
  tempIDCharArr: TArray<Char>;
  nodeIDList: TStringList;
  pollutantIDList: TStringList;
  pollUnits: TArray<integer>;
  currentBytePos: long;
  numLinkProperities: integer;
  numNodeProperties: integer;
  tempInt: integer;
  tempDouble: Double;
  tempReal8: Double;
  tempReal4: single;
  reportStartDate: Double;
  reportTimeInterv: Double;
  days: TDateTime;
  totalNumOfMatchedFRWPollutants: integer;
  formattedTSDate: string;
  formattedTSTime: string;
  nodeResultsForPeriod: TArray<single>;
  numNodeResults: integer;
  numSubcatchResults: integer;
  numlinkResults: integer;
begin
  Stream := TFileStream.Create(swmmFilePath, fmOpenRead or fmShareDenyWrite);
  nodeIDList := TStringList.Create();
  pollutantIDList := TStringList.Create();
  SetLength(pollUnits, High(selectedPollutantRecs));
  totalNumOfMatchedFRWPollutants := High(selectedPollutantRecs);
  try
    Reader := TBinaryReader.Create(Stream);
    try
      // Value := Reader.ReadInteger;
      // --- get number of objects reported on                                   //(5.0.014 - LR)
      numSubCatchs := 0;
      numNodes := 0;
      numLinks := 0;
      numPolls := 0;

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

      numNodeResults = MAX_NODE_RESULTS - 1 + numPolls;
      numSubcatchResults = MAX_SUBCATCH_RESULTS - 1 + numPolls;
      numlinkResults = MAX_LINK_RESULTS - 1 + numPolls;
      { // Read all subcatchment IDs and discard, skipping this section is not straight forward since catchment
        // name lengths vary
        for idx := 0 to numSubcatchs - 1 do
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
        end; }

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
      numCharsInID := Reader.ReadInteger; // INPUT_TYPE_CODE
      numCharsInID := Reader.ReadInteger; // INPUT_INVERT
      numCharsInID := Reader.ReadInteger; // INPUT_MAX_DEPTH;
      // Type code is int rest are real4 so read type code seperately
      if (numNodes > 0) then
      begin
        currentBytePos := Stream.Position;
        Stream.Seek((numNodes) * sizeof(integer) + (numNodes) *
          (numNodeProperties - 1) * sizeof(single), currentBytePos);
      end;
      { for idx := 0 to numNodes do
        begin
        numCharsInID := Reader.ReadInteger; // node type
        tempDouble := Reader.ReadDouble; // node invertElevation
        tempDouble := Reader.ReadDouble; // node fullDepth
        end; }

      // --- skip link type, offsets, max. depth, & length
      numLinkProperities := Reader.ReadInteger; // 3 - number of link properties
      tempInt := Reader.ReadInteger; // INPUT_TYPE_CODE code number
      tempInt := Reader.ReadInteger; // // INPUT_OFFSET
      tempInt := Reader.ReadInteger; // // INPUT_OFFSET
      tempInt := Reader.ReadInteger; // INPUT_MAX_DEPTH
      tempInt := Reader.ReadInteger; // INPUT_LENGTH

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

      // get node results for all time periods ============================================
      for idx := 1 to numberOfPeriods do
      begin
        days = reportStartDate + (reportTimeInterv * idx / 86400.0);
        if (idx = 1) then
        begin
          // formattedTSDate
          // formattedTSTime
          { datetime_decodeDate(days, &yr, &mo, &dy);
            metaDataFound.fyear=yr;
            metaDataFound.fmonth=mo;
            metaDataFound.fday=dy;
            datetime_decodeTime(days, &hr, &min, &sec);
            metaDataFound.fhour = hr*3600 + min*60 + sec; }
        end;
        { datetime_dateToStr2(days, theDate, dateTimeFormat);
          datetime_timeToStr2(days, theTime);
          memset(nodeResultsForPeriod,0,numNodeResults*sizeof(REAL4)); }
        nodeResultsForPeriod = output_readNodeResults(idx, targetNodeIndex,
          numNodeResults, numSubCatchs, numSubcatchResults, outputStartPos,
          bytesPerPeriod, fout);
        if (idx <> 1) then
        begin

          result.Add('\n');
        end;
        // add formatted flow entry
        result.Add(Format(' %11s,%8s,%9.3f', formattedTSDate, formattedTSTime,
          nodeResultsForPeriod[NODE_INFLOW] * selectedPollutantRecs[0]
          .convFactor));

        for pollIdx := 0 to totalNumOfMatchedFRWPollutants - 1 do
        begin
          if (nodeResultsForPeriod[NODE_INFLOW] < MIN_WQ_FLOW) then
          begin
            result.Add(Format(',%9.3f', 0.0 f));
          end
          else
          begin
            result.Add(Format(',%9.3f',
              nodeResultsForPeriod[NODE_QUAL + targetPollutantSWMMOrder[p]] *
              selectedPollutantRecs[pollIdx].convFactor));
          end;
        end;
      end;

      // save stream position for later when we extract node results to avoid having to start over
      // SWMMFileStreamPosition := Stream.Position;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
  // SetLength(result, 2);
  result := nodeIDList;
  // result[1] := pollutantIDList;
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
    rslt[idx] := Reader.Read(sizeof(single), soBeginning);
  end;
  result := rslt;
end;

end.
