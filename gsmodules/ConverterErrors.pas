unit ConverterErrors;

interface

uses
   Classes, SysUtils;

const
  Errs: array [0 .. 5] of string = ('F001 - Input file does not exist',
    'S001 - An unknown error occured when reading the SWMM file',
    'S002 - An unknown error occured when saving the new SWMM file',
    'S003 - Unable to read node IDs in the SWMM ouput file',
    'S004 - Unable to read pollutant IDs in the SWMM output file',
    'S005 - Unable to read the start/end dates of the simulation in the SWMM output file');

var
  errorsList: TStringList;

function checkInputFiles(): integer;
function checkIfFileExists(fileNameOrPath: string): integer;
procedure reportErrorsToFW();

implementation
     uses
     SWMMIO;

function checkInputFiles(): integer;
begin
  // for both import and export check whether groupNames.txt exists
  // 0. check if groupnames.txt exists
  result := checkIfFileExists(SWMMIO.fileNameGroupNames);

  if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
  // SWMM_TO_FW importing from swmm binary file
  begin
    // no further checks needed for this version
  end
  else // SWMM_FROM_FW we are exporting to swmm so using SWMM input file
  begin

  end;
end;

function checkIfFileExists(fileNameOrPath: string): integer;
begin
  if (FileExists(fileNameOrPath) or FileExists(SWMMIO.workingDir +
    fileNameOrPath)) then
    result := 1
  else
  begin
    result := -1;
    errorsList.Add('''' + Errs[0] + ': ' + fileNameOrPath + '''');
  end;
end;

procedure reportErrorsToFW();
begin
  if (errorsList.Count < 1) then
    errorsList.Add('''ALL OK''');
  SWMMIO.saveTextFileToDisc(errorsList, SWMMIO.workingDir +
    SWMMIO.fileNameMessages, true);
end;

end.
