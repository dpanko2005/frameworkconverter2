unit SWMMInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, StrUtils, SWMMIO, ReadMTA,
  FWControlScratchFile,
  ComCtrls;

var
  swmmIDsListArr: TArray<TStringList>;

function consoleExportFromFWToSWMM(MTAFilePath: string): Integer;
procedure finalizeExport(var Conv: array of TMTARecord; filePathDir: string);
function checkForDuplicateTS(tsBlockInsertPosition: Integer;
  TSList: TStringList; NewFileContentsList: TStringList;
  tsName: string): Integer;
function updateSWMMInputFile(var Conv: array of TMTARecord;
  origSWMMInputFilePath: string; newSWMMInputFilePath: string): string;

implementation

function consoleExportFromFWToSWMM(MTAFilePath: string): Integer;
var
  mtaData: TArray<TMTARecord>;
  tempStr, swmmFilePath: string;
  i: Integer;

  workingDirPath: string;
begin
  mtaData := ReadMTA.Read(MTAFilePath);

  if (ReadMTA.errorsList.Count > 0) then
  begin
    for tempStr in ReadMTA.errorsList do
      Writeln(tempStr);

    Writeln('Operation failed.');
    result := 0;
    Exit;
  end
  else
  begin
    Assert(Assigned(mtaData));
    swmmFilePath := mtaData[0].swmmFilePath;
    Writeln('SWMM output input file to insert timeseries into: ' + swmmFilePath
      + sLineBreak);
    Writeln('Target SWMM Node: ' + mtaData[0].tsNodeName);
    Writeln('Source framework scratch file to import timeseries from: ' +
      mtaData[0].scratchFilePath);
    // flow is in position 0 so highest array index is count of pollutants
    Writeln('Total number of pollutants:' + IntToStr(High(mtaData)));

    for i := Low(mtaData) + 1 to High(mtaData) do
    begin
      Writeln(Format
        ('Pollutant %d: framework name: %s SWMM name: %s conversion factor: %9.3f',
        [i, mtaData[i].constituentFWName, mtaData[i].constituentSWMMName,
        mtaData[i].convFactor]) + sLineBreak);
    end;

    Writeln('Now extracting SWMM timeseries. Please wait...');
    workingDirPath := ExtractFileDir(swmmFilePath);
    mtaData := SWMMIO.readInFrameworkTSFile(mtaData[0].scratchFilePath,
      mtaData);
    SWMMInput.finalizeExport(mtaData, workingDirPath);
    updateSWMMInputFile(mtaData, swmmFilePath, swmmFilePath);
    if (SWMMIO.errorsList.Count > 0) then
    begin
      for i := 0 + 1 to SWMMIO.errorsList.Count - 1 do
      begin
        Writeln(SWMMIO.errorsList[i]);
      end;
      Exit;
    end;
    Writeln('Operation completed successfully.');
  end;
  result := 1;
end;

procedure finalizeExport(var Conv: array of TMTARecord; filePathDir: string);
var
  filePath: string;
  pathPrefix: string;
  pathSuffix: string;
  i: Integer;
begin
  pathPrefix := filePathDir + '\TS\FrameworkTS_';
  pathSuffix := FormatDateTime('yyyymmddhhnnss', Now) + '.dat';

  for i := Low(Conv) to High(Conv) do
  begin
    if ((Conv[i].constituentSWMMName <> '') and (Conv[i].convFactor <> 0)) then
    begin
      filePath := pathPrefix + Conv[i].constituentSWMMName + pathSuffix;
      Conv[i].convertedTSFilePath := filePath;
      SWMMIO.saveTextFileToDisc(Conv[i].convertedTS, filePath);
    end;
  end;
end;

function checkForDuplicateTS(tsBlockInsertPosition: Integer;
  TSList: TStringList; NewFileContentsList: TStringList;
  tsName: string): Integer;
var
  i: Integer;
begin
  // check our cached list of TS names for a hit
  for i := 0 to TSList.Count - 1 do
  begin

    //if (TSList.IndexOf(tsName) > -1) then
    if (Pos(tsName,TSList[i]) > 0) then
    begin
      // duplicate TS exists in the swmmfile so return its line number so we can overwrite with replacement
      while ((Pos(';;', NewFileContentsList[tsBlockInsertPosition]) > 0) and
        (tsBlockInsertPosition < NewFileContentsList.Count)) do
      begin
        inc(tsBlockInsertPosition);
      end;

      while ((Pos(tsName, NewFileContentsList[tsBlockInsertPosition]) = 0) and
        (tsBlockInsertPosition < NewFileContentsList.Count)) do
      begin
        inc(tsBlockInsertPosition);
      end;

      if (Pos(tsName, NewFileContentsList[tsBlockInsertPosition]) > 0) then
      begin
        result := tsBlockInsertPosition;
        Exit;
      end
      else
        result := 0;
    end;
  end;
  result := 0;
end;

function updateSWMMInputFile(var Conv: array of TMTARecord;
  origSWMMInputFilePath: string; newSWMMInputFilePath: string): string;
var
  NewFileContentsList: TStringList;
  tempInt: Integer;
  tsBlockInsertPosition: Integer;
  tempRec: TMTARecord;
  pathSuffix: string;
  tsName: string;
  duplicateLineNumber: Integer;
begin
  NewFileContentsList := TStringList.Create;

  // take an inventory of the contents of the swmm file to avoid duplicates later
  // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
  swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(origSWMMInputFilePath);
  if (SWMMIO.errorsList.Count > 0) then
  begin
    Exit;
  end;
  SWMMIO.TSList := swmmIDsListArr[2];
  SWMMIO.InflowsList := swmmIDsListArr[3];
  SWMMIO.NodeNameList := swmmIDsListArr[0];
  SWMMIO.PollList := swmmIDsListArr[1];
  try

    { First check if the swmm file we will be updating exists - note that update version save to new path. }
    if FileExists(origSWMMInputFilePath) then
    begin
      { If it exists, load the data into the stringlist. }
      NewFileContentsList.LoadFromFile(origSWMMInputFilePath);

      // Look for insertion points for TIMESERIES and INFLOW blocks
      tsBlockInsertPosition := NewFileContentsList.IndexOf('[REPORT]');
      if (tsBlockInsertPosition < 0) then
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[CURVES]')
      else if (tsBlockInsertPosition < 0) then
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[WASHOFF]')
      else if (tsBlockInsertPosition < 0) then
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[TAGS]')
      else if (tsBlockInsertPosition < 0) then
      begin
        raise Exception.Create
          ('Check SWMM input file format. Unable to write timeseries to SWMM input file');
        Exit;
      end;

      // 1. Write TimeSeries Block
      // check TS list that was passed in to see if input file already contains TS to avoid duplicates
      if (SWMMIO.TSList.Count > 0) then
      begin
        // timeseries section already exists in swmm input file so simply add to it - check for duplicate names
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[TIMESERIES]');
        while (Pos(';;', NewFileContentsList[tsBlockInsertPosition + 1]) < 1) do
        begin
          inc(tsBlockInsertPosition);
        end;
        // see checkForDuplicateTS fxn which checks for duplicate TS names in swmm file below;
      end
      else
      begin
        tsBlockInsertPosition := tsBlockInsertPosition - 1;
        // timeseries section does not already exist in swmm input file so write times series block and add to TS to it
        NewFileContentsList.Insert(tsBlockInsertPosition, ' ');
        NewFileContentsList.Insert(tsBlockInsertPosition + 1, '[TIMESERIES]');
        NewFileContentsList.Insert(tsBlockInsertPosition + 2,
          ';;Name          	Type      	Path');
        NewFileContentsList.Insert(tsBlockInsertPosition + 3,
          ';;-------------- ---------- ---------- ----------');
        tsBlockInsertPosition := tsBlockInsertPosition + 4;
      end;

      tempInt := 0;
      for tempRec in Conv do
      begin
        tsName := tempRec.constituentSWMMName + 'TS';

        if (tempRec.convertedTSFilePath <> '') then
        begin
          duplicateLineNumber := checkForDuplicateTS(tsBlockInsertPosition,
            SWMMIO.TSList, NewFileContentsList, tsName);
          if (duplicateLineNumber <> 0) then
          begin
            NewFileContentsList[duplicateLineNumber] := tsName +
              '      FILE      "' + tempRec.convertedTSFilePath + '"';
          end
          else
            NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
              tsName + '      FILE      "' + tempRec.convertedTSFilePath + '"');
        end;
        inc(tempInt);
      end;
      tsBlockInsertPosition := tsBlockInsertPosition + tempInt;

      // 2. Write Inflow Block
      // check TS list that was passed in to see if input file already contains TS
      if ((SWMMIO.TSList.Count > 0) and
        (NewFileContentsList.IndexOf('[INFLOWS]') > -1)) then
      begin
        // Inflow section already exists in swmm input file so simply add to it - check for duplicate names
        tsBlockInsertPosition := NewFileContentsList.IndexOf('[INFLOWS]') + 1;
        while (Pos(';;', NewFileContentsList[tsBlockInsertPosition]) > 0) do
        begin
          inc(tsBlockInsertPosition);
        end;
        // TODO check for duplicates names in inflow block
      end
      else
      begin
        // Inflow section does not already exist in swmm input file so write times series block and add to TS to it
        NewFileContentsList.Insert(tsBlockInsertPosition, '');
        NewFileContentsList.Insert(tsBlockInsertPosition + 1, '[INFLOWS]');
        NewFileContentsList.Insert(tsBlockInsertPosition + 2,
          ';;                                                 Param    Units    Scale    Baseline Baseline');
        NewFileContentsList.Insert(tsBlockInsertPosition + 3,
          ';;Node           Parameter        Time Series      Type     Factor   Factor   Value    Pattern');
        NewFileContentsList.Insert(tsBlockInsertPosition + 4,
          ';;-------------- ---------------- ---------------- -------- -------- -------- -------- --------');
        tsBlockInsertPosition := tsBlockInsertPosition + 5;
      end;

      tempInt := 0;
      for tempRec in Conv do
      begin

        { if (tempRec.convertedTSFilePath <> '') then
          begin
          NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
          tempRec.tsNodeName + '        ' + tempRec.constituentSWMMName +
          '        ' + tsName + '        ' + tempRec.tsType + '        ' +
          FloatToStr(tempRec.tsUnitsFactor) + '        ' +
          FloatToStr(tempRec.convFactor));
          end;
          inc(tempInt); }

        if (tempRec.convertedTSFilePath <> '') then
        begin
          duplicateLineNumber := checkForDuplicateTS(tsBlockInsertPosition,
            SWMMIO.InflowsList, NewFileContentsList,
            tempRec.tsNodeName + '        ' + tempRec.constituentSWMMName);
          if (duplicateLineNumber <> 0) then
          begin
            NewFileContentsList[duplicateLineNumber] := tempRec.tsNodeName +
              '        ' + tempRec.constituentSWMMName + '        ' + tsName +
              '        ' + tempRec.tsType + '        ' +
              FloatToStr(tempRec.tsUnitsFactor) + '        ' +
              FloatToStr(tempRec.convFactor);
          end
          else
            NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
              tempRec.tsNodeName + '        ' + tempRec.constituentSWMMName +
              '        ' + tsName + '        ' + tempRec.tsType + '        ' +
              FloatToStr(tempRec.tsUnitsFactor) + '        ' +
              FloatToStr(tempRec.convFactor));
        end;
        inc(tempInt);
      end;
      SWMMIO.saveTextFileToDisc(NewFileContentsList,
        newSWMMInputFilePath, true);
    end
    else
      { Otherwise, raise an exception. }
      raise Exception.Create('File does not exist.');
  finally
    result := '';
    NewFileContentsList.Free;
  end;
  result := newSWMMInputFilePath;
end;

end.
