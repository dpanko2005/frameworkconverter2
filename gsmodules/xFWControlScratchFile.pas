{ ------------------------------------------------------------------- }
{ Unit:    FWControlScratchFile.pas }
{ Project: WERF Framework - SWMM Converter }
{ Version: 2.0 }
{ Date:    2/28/2014 }
{ Author:  Gesoyntec (D. Pankani) }
{ }
{ Delphi Pascal unit that mangages the reading and writting of the }
{ Framework Control Scratch File (swmmconvertstring.txt) }
{ ------------------------------------------------------------------- }

unit xFWControlScratchFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO;

type

  // converted framework timeseries data structure
  FWScratchRecord = record
    scratchFilePath, scratchControlFilePath, tsNodeName, description: string;
    convFactor, startHourFrac: double;
    numPolls, startYear, startMonth, startDay: integer;
    endYear, endMonth, endDay: integer;
    endHourFrac: double;
    numberOfTimesteps: long;
    fwTimeSeries: TStringList;
    swmmReportTimestepSecs:integer;
  end;

var
  errorsList: TStringList;

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
function Read(fwCtrlFilePath: string): FWScratchRecord;

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
function Write(FWCtrlRecord: FWScratchRecord): Boolean;

implementation

function Read(fwCtrlFilePath: string): FWScratchRecord;
var
  FileContentsList: TStringList;
  Rslt: FWScratchRecord;
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
          Rslt.convFactor := StrToFloat(FileContentsList[2]);
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

function Write(FWCtrlRecord: FWScratchRecord): Boolean;
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
      FileContentsList.Add(FWCtrlRecord.scratchFilePath + '''');
      FileContentsList.Add(FWCtrlRecord.tsNodeName);
      FileContentsList.Add('''' + Format('''%s'',''%s'',''%s''',
        [FWCtrlRecord.startYear, FWCtrlRecord.startMonth,
        FWCtrlRecord.startDay]));
      FileContentsList.Add('''' + Format('''%s'',''%s'',''%s''',
        [FWCtrlRecord.endYear, FWCtrlRecord.endMonth,
        FWCtrlRecord.endDay]));
      // FileContentsList.Add(FloatToStr(FWCtrlRecord.convFactor));
      // FileContentsList.Add(IntToStr(FWCtrlRecord.numPolls) + '''');
      FileContentsList.Add(FWCtrlRecord.description);
      FileContentsList.Add(IntToStr(FWCtrlRecord.startYear));
      FileContentsList.Add(IntToStr(FWCtrlRecord.startMonth));
      FileContentsList.Add(IntToStr(FWCtrlRecord.startDay));
      FileContentsList.Add(FloatToStr(FWCtrlRecord.startHourFrac));
      FileContentsList.Add(IntToStr(FWCtrlRecord.endYear));
      FileContentsList.Add(IntToStr(FWCtrlRecord.endMonth));
      FileContentsList.Add(IntToStr(FWCtrlRecord.endDay));
      FileContentsList.Add(FloatToStr(FWCtrlRecord.endHourFrac));
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

end.
