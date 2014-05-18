{ ------------------------------------------------------------------- }
{ Unit:    SWMMInput.pas                                              }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that exports time series from the framework      }
{ to a SWMM input file. TS are saved to external files and reference  }
{ in SWMM input file                                                  }
{ ------------------------------------------------------------------- }

unit SWMMInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, StrUtils, SWMMIO,ConverterErrors, ReadMTA,
  FWControlScratchFile,
  ComCtrls;

var
  swmmIDsListArr: TArray<TStringList>;

///	<summary>
///	  Command line version of function that takes timeseries from the framework
///	  into SWMM 5
///	</summary>
///	<param name="MTAFilePath">
///	  The SWMM 5 converter control file path that contains SWMM specific
///	  information needed for the operation
///	</param>
///	<returns>
///	  Returns 1 if operation was successful; 0 if operation had to be abandoned
///	  due to an error or missing data
///	</returns>
function consoleExportFromFWToSWMM(MTAFilePath: string): Integer;

///	<summary>
///	  Function that finalizes the export from the framework to SWMM by writing
///	  time series to disc that are formatted for use in SWMM 5
///	</summary>
///	<param name="Conv">
///	  Conversion factor
///	</param>
///	<param name="filePathDir">
///	  Directory where times series formatted for SWMM 5 will be saved
///	</param>
procedure finalizeExport(var ConvertedFWTSArr: array of TMTARecord; filePathDir: string);

///	<summary>
///	  Inspects portions of the SWMM 5 input file to see if entries pertaining
///	  to the framwork time series that is being exported already exist in the
///	  SWMM input file
///	</summary>
///	<param name="tsBlockInsertPosition">
///	  searches the contents of the SWMM 5 input file from this point forward
///	</param>
///	<param name="TSList">
///	  Saved list of SWMM 5 time series names to check for against SWMM 5 input
///	  file for duplicates
///	</param>
///	<param name="NewFileContentsList">
///	  Contents of the SWMM 5 input file receiving the export from the framework
///	</param>
///	<param name="tsName">
///	  Name of the current time series for which the duplicate check is being
///	  executed
///	</param>
///	<returns>
///	  Returns 0 if no duplicates and position of duplicate otherwise
///	</returns>
function checkForDuplicateTS(tsBlockInsertPosition: Integer;
  TSList: TStringList; NewFileContentsList: TStringList;
  tsName: string): Integer;

///	<summary>
///	  Updates a SWMM 5 input file by writting TIMESERIES and INFLOWS block ///
///	  entries that associated exported framework timeseries with the ///
///	  appropriate SWMM node and point SWMM to the external exported time series
///	  /// files
///	</summary>
///	<param name="ConvertedFWTSArr">
///	  array of converted framework time series
///	</param>
///	<param name="origSWMMInputFilePath">
///	  path to the original SWMM 5 input file to be edited and saved as a new
///	  file
///	</param>
///	<param name="newSWMMInputFilePath">
///	  path to new file to be created from modified original SWMM 5 input file
///	  to be edited
///	</param>
///  ///	<returns>
///	  Returns path to modified SWMM 5 input file saved to a new location
///	</returns>
function updateSWMMInputFile(var ConvertedFWTSArr: array of TMTARecord;
  origSWMMInputFilePath: string; newSWMMInputFilePath: string): string;

implementation

function consoleExportFromFWToSWMM(MTAFilePath: string): Integer;
var
  mtaData: TArray<TMTARecord>;
  tempStr, swmmFilePath,workingDirPath: string;
  i: Integer;
begin
//read in SWMM converter control file
  mtaData := ReadMTA.Read(MTAFilePath);

  //if there were errors write errors to the console  and exit
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
    //if there were no errors process export
    Assert(Assigned(mtaData));
    swmmFilePath := mtaData[0].swmmFilePath;
    Writeln('SWMM output input file to insert timeseries into: ' + swmmFilePath
      + sLineBreak);
    Writeln('Target SWMM Node: ' + mtaData[0].tsNodeName);
    Writeln('Source framework scratch file to import timeseries from: ' +
      mtaData[0].scratchFilePath);

    // flow is in position 0 so highest array index is count of pollutants
    Writeln('Total number of pollutants:' + IntToStr(High(mtaData)));

    //print all included pollutants to the console
    for i := Low(mtaData) + 1 to High(mtaData) do
    begin
      Writeln(Format
        ('Pollutant %d: framework name: %s SWMM name: %s conversion factor: %9.3f',
        [i, mtaData[i].constituentFWName, mtaData[i].constituentSWMMName,
        mtaData[i].convFactor]) + sLineBreak);
    end;

    //begin extracting and formatting the framework time series for SWMM 5
    Writeln('Now extracting SWMM timeseries. Please wait...');
    workingDirPath := ExtractFileDir(swmmFilePath);
    mtaData := SWMMIO.readInFrameworkTSFile(mtaData[0].scratchFilePath,
      mtaData);

    //finalize the export by writting the extracted framework time series to disc in SWMM 5 format
    SWMMInput.finalizeExport(mtaData, workingDirPath);

    //update the SWMM 5 input file by writing entries that point to the location of the exported time series files
    updateSWMMInputFile(mtaData, swmmFilePath, swmmFilePath);

    //if any errors occur print to the console and exit
    if (ConverterErrors.errorsList.Count > 0) then
    begin
      for i := 0 + 1 to ConverterErrors.errorsList.Count - 1 do
      begin
        Writeln(ConverterErrors.errorsList[i]);
      end;
      result := 0;
      Exit;
    end;
    Writeln('Operation completed successfully.');
  end;
  //if no errors occured return success
  result := 1;
end;

procedure finalizeExport(var ConvertedFWTSArr: array of TMTARecord; filePathDir: string);
var
  filePath: string;
  pathPrefix: string;
  pathSuffix: string;
  i: Integer;
begin
  //export framework time series are placed in TS direction with the following naming convention
  pathPrefix := filePathDir + '\TS\FrameworkTS_';

  //name includes a time stamp so subsequent exports are not overwritten in advertently
  pathSuffix := FormatDateTime('yyyymmddhhnnss', Now) + '.dat';

  //look through aray of converted framework time series and write them to disc in format usable in SWMM
  for i := Low(ConvertedFWTSArr) to High(ConvertedFWTSArr) do
  begin
    if ((ConvertedFWTSArr[i].constituentSWMMName <> '') and (ConvertedFWTSArr[i].convFactor <> 0)) then
    begin
    //following above name convention for file names and save to TS subdirectory
      filePath := pathPrefix + ConvertedFWTSArr[i].constituentSWMMName + pathSuffix;
      ConvertedFWTSArr[i].convertedTSFilePath := filePath;

      //delegate to SWMMIO to do the actual save operation
      SWMMIO.saveTextFileToDisc(ConvertedFWTSArr[i].convertedTS, filePath);
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
  // TS names were previously saved when we read the file
  for i := 0 to TSList.Count - 1 do
  begin

  // if the current TS name is found in the list of saved TS names  then ran check
  // duplicate TS exists in the swmmfile so return its line number so we can overwrite with replacement
    if (Pos(AnsiUpperCase(tsName), AnsiUpperCase(TSList[i])) > 0) then
    begin
      //skip all comments in the SWMM 5 input file
      while ((Pos(';;', NewFileContentsList[tsBlockInsertPosition]) > 0) and
        (tsBlockInsertPosition < NewFileContentsList.Count)) do
      begin
        inc(tsBlockInsertPosition);
      end;

      //search for the line containing the duplicate TS
      while ((Pos(AnsiUpperCase(tsName), UpperCase(NewFileContentsList[tsBlockInsertPosition])) = 0) and
        (tsBlockInsertPosition < NewFileContentsList.Count)) do
      begin
        inc(tsBlockInsertPosition);
      end;

      //if found return the position of the line that contains it so calling routine can insert the new
      //TS entry as a replacement at the same location
      if (Pos(AnsiUpperCase(tsName), AnsiUpperCase(NewFileContentsList[tsBlockInsertPosition])) > 0) then
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

function updateSWMMInputFile(var ConvertedFWTSArr: array of TMTARecord;
  origSWMMInputFilePath: string; newSWMMInputFilePath: string): string;
var
  NewFileContentsList: TStringList;
  tempInt: Integer;
  tsBlockInsertPosition: Integer;
  tempRec: TMTARecord;
  //pathSuffix: string;
  tsName: string;
  duplicateLineNumber: Integer;
begin
  NewFileContentsList := TStringList.Create;

  // take an inventory of the contents of the swmm file to avoid duplicates later
  // 0-NodeIDs list, 1-Pollutants list, 2-Timeseries list, 3-Inflows list
  swmmIDsListArr := SWMMIO.getSWMMNodeIDsFromTxtInput(origSWMMInputFilePath);

  //check to see if any errors occured while attempting to read in SWMM node IDs
  if (ConverterErrors.errorsList.Count > 0) then
  begin
    Exit;
  end;
  SWMMIO.TSList := swmmIDsListArr[2];
  SWMMIO.InflowsList := swmmIDsListArr[3];
  SWMMIO.NodeNameList := swmmIDsListArr[0];
  SWMMIO.PollList := swmmIDsListArr[1];
  try

   // First check if the swmm file we will be updating exists - note that updated version will be saved to new path.
    if FileExists(origSWMMInputFilePath) then
    begin
      // If it exists, load the data into a stringlist.
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

      // Write TimeSeries Block
      // check TS list that was passed in to see if input file already contains TS to avoid duplicates
      if (SWMMIO.TSList.Count > 0) then
      begin
        // timeseries section already exists in swmm input file so simply add to it while checking for duplicate names
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
        // timeseries section does not already exist in swmm input file so write times series block and add TS entries to it
        NewFileContentsList.Insert(tsBlockInsertPosition, ' ');
        NewFileContentsList.Insert(tsBlockInsertPosition + 1, '[TIMESERIES]');
        NewFileContentsList.Insert(tsBlockInsertPosition + 2,
          ';;Name          	Type      	Path');
        NewFileContentsList.Insert(tsBlockInsertPosition + 3,
          ';;-------------- ---------- ---------- ----------');
        tsBlockInsertPosition := tsBlockInsertPosition + 4;
      end;

      tempInt := 0;
      //check for duplicate TIMESERIES block entries in the SWMM file
      for tempRec in ConvertedFWTSArr do
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
          inc(tempInt);
        end;

      end;
      tsBlockInsertPosition := tsBlockInsertPosition + tempInt;

      // Write Inflow Block
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
        // see checkForDuplicateTS fxn which checks for duplicate TS names in swmm file below;
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
      //check for duplicate INFLOW block entries in the SWMM file
      for tempRec in ConvertedFWTSArr do
      begin
        if (tempRec.convertedTSFilePath <> '') then
        begin
          duplicateLineNumber := checkForDuplicateTS(tsBlockInsertPosition,
            SWMMIO.InflowsList, NewFileContentsList,
            tempRec.tsNodeName + '        ' + tempRec.constituentSWMMName);
          if (duplicateLineNumber <> 0) then
          begin
            NewFileContentsList[duplicateLineNumber] := tempRec.tsNodeName +
              '        ' + tempRec.constituentSWMMName + '        ' +
              tempRec.constituentSWMMName + 'TS' + '        ' + tempRec.tsType +
              '        ' + FloatToStr(tempRec.tsUnitsFactor) + '        ' +
              FloatToStr(tempRec.convFactor);
          end
          else
            NewFileContentsList.Insert(tsBlockInsertPosition + tempInt,
              tempRec.tsNodeName + '        ' + tempRec.constituentSWMMName +
              '        ' + tempRec.constituentSWMMName + 'TS' + '        ' +
              tempRec.tsType + '        ' + FloatToStr(tempRec.tsUnitsFactor) +
              '        ' + FloatToStr(tempRec.convFactor));
          inc(tempInt);
        end;

      end;
      SWMMIO.saveTextFileToDisc(NewFileContentsList,
        newSWMMInputFilePath, true);
    end
    else
      // Otherwise, raise an exception.
      raise Exception.Create('File does not exist.');
  finally
    result := '';
    NewFileContentsList.Free;
  end;
  result := newSWMMInputFilePath;
end;

end.
