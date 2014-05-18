unit FWIO;

interface

uses
  Classes, DateUtils, SysUtils, SWMMIO;

type
  GroupNames = record
    // converted framework groupnames.txt contents data structure
    startDate, endDate: TDateTime;
    fileNames: TStringList;
  end;

function readGroupNames(): GroupNames;

implementation

function readGroupNames(): GroupNames;
var
  rslt: GroupNames;
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
    rslt.startDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2]
      + '/' + TempDateTokens[0]);

    // convert end date tokens into date object
    TempDateTokens.DelimitedText := FileContentsList[1];
    rslt.endDate := StrToDateTime(TempDateTokens[1] + '/' + TempDateTokens[2] +
      '/' + TempDateTokens[0]);

    result := rslt;
  finally
    FileContentsList.Free;
  end;
end;

end.
