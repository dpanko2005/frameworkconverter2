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
  end;

var
  errorsList: TStringList;

function Read(fwCtrlFilePath: string): TArray<FWCtrlScratchRecord>;

implementation

function Read(fwCtrlFilePath: string): TArray<FWCtrlScratchRecord>;
var
  FileContentsList: TStringList;
  Rslt: FWCtrlScratchRecord;
begin
  errorsList := TStringList.Create;
  FileContentsList := TStringList.Create;
  try

    { First check if the file exists. }
    if FileExists(fwCtrlFilePath) then
    begin
      { If it exists, load the data into the stringlist. }
      FileContentsList.LoadFromFile(fwCtrlFilePath);
      if (FileContentsList.Count > 13) then
      begin
        Rslt.scratchFilePath := FileContentsList[0];
        Rslt.tsNodeName := FileContentsList[1];
        Rslt.convFactor := FileContentsList[2];
        Rslt.numPolls := FileContentsList[3];
        Rslt.description := FileContentsList[4];
        Rslt.startYear := FileContentsList[5];
        Rslt.startMonth := FileContentsList[6];
        Rslt.startDay := FileContentsList[7];
        Rslt.startHourFrac := FileContentsList[8];
        Rslt.endYear := FileContentsList[9];
        Rslt.endMonth := FileContentsList[10];
        Rslt.endDay := FileContentsList[11];
        Rslt.endHourFrac := FileContentsList[12];
        Rslt.numberOfTimesteps := FileContentsList[13];
      end
      else
        errorsList.Add
          ('Not enough entries in Framework Metadata Scrach File. 14 lines expected but found '
          + FileContentsList.Count);
    end
    else
      errorsList.Add('Framework Metadata Scratch File not found at:' +
        fwCtrlFilePath);
  finally
    FileContentsList.Free;
  end;
  result := Rslt;
end;

end.
