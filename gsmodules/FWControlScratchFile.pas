unit FWControlScratchFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StrUtils, Dialogs, jpeg, ExtCtrls, ComCtrls, StdCtrls, Buttons, SWMMIO;

type
  FWCtrlScratchRecord = record // converted framework timeseries data structure
    scratchFilePath: string;
    tsNodeName: string;
    convFactor: double;
    numPolls: integer;
    description: string;
    startYear: integer;
    startMonth: integer;
    startDay: integer;
    startHourFrac: double;
    endYear: integer;
    endMonth: integer;
    endDay: integer;
    endHourFrac: double;
    numberOfTimesteps: long;
    fwTimeSeries: TStringList;
  end;

var
  errorsList: TStringList;

function Read(fwCtrlFilePath: string): FWCtrlScratchRecord;
function Write(FWCtrlRecord: FWCtrlScratchRecord): Boolean;

implementation

function Read(fwCtrlFilePath: string): FWCtrlScratchRecord;
var
  FileContentsList: TStringList;
  Rslt: FWCtrlScratchRecord;
begin
  errorsList := TStringList.Create;
  FileContentsList := TStringList.Create;
  try
    begin
      { First check if the file exists. }
      if FileExists(fwCtrlFilePath) then
      begin
        { If it exists, load the data into the stringlist. }
        FileContentsList.LoadFromFile(fwCtrlFilePath);
        if (FileContentsList.Count > 13) then
        begin
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

function Write(FWCtrlRecord: FWCtrlScratchRecord): Boolean;
var
  FileContentsList: TStringList;
  descr: string;
  fwControlFilePath: string;
begin
  try
    begin
      if (FWCtrlRecord.description = '') then
        FWCtrlRecord.description := 'Converted from SWMM 5';

      FileContentsList := TStringList.Create;
      FileContentsList.Add(FWCtrlRecord.scratchFilePath);
      FileContentsList.Add(FWCtrlRecord.tsNodeName);
      FileContentsList.Add(FloatToStr(FWCtrlRecord.convFactor));
      FileContentsList.Add(IntToStr(FWCtrlRecord.numPolls));
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
      fwControlFilePath := ExtractFilePath(FWCtrlRecord.scratchFilePath) +
        'swmmconvertstring.txt';
      saveTextFileToDisc(FileContentsList, fwControlFilePath, true);
    end;
  finally
    FileContentsList.Free();
  end;
  result := true;
end;

end.
