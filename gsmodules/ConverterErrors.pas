{*------------------------------------------------------------------------------
  Delphi Pascal unit containing various utility functions related to error
  message handling, display and serialization

  @unit:    FWIO.pas
  @project: WERF Framework - SWMM Converter
  @version: 2.0
  @date:    2/28/2014
  @author:  Gesoyntec (D. Pankani)
------------------------------------------------------------------------------- }

unit ConverterErrors;

interface

uses
  Classes, SysUtils;

const
{*------------------------------------------------------------------------------
  List of pre-defined error messages with associated codes
------------------------------------------------------------------------------- }
  Errs: array [0 .. 9] of string = ('F001 - Input file does not exist',
    'S001 - An unknown error occurred when reading the SWMM file',
    'S002 - An unknown error occurred when saving the new SWMM file',
    'S003 - Unable to read node IDs in the SWMM output file',
    'S004 - Unable to read pollutant IDs in the SWMM output file',
    'S005 - Unable to read the start/end dates of the simulation in the SWMM output file',
    'S006 - User specified time span begins earlier than available SWMM data or User specified time span ends later than available SWMM data',
    'F002 - The file provided is either the wrong file or has the wrong extension',
    'S006 - Matching node not found in SWMM for node:',
    'F003 - Framework time series data file not found at:');

var
{*------------------------------------------------------------------------------
  List of errors that have occured in the course of execution
------------------------------------------------------------------------------- }
  errorsList: TStringList;

{*------------------------------------------------------------------------------
  For both import and export mode checks to see if groupnames.txt exists.

  @return TRUE if succesfull, FALSE otherwise
-------------------------------------------------------------------------------}
function checkInputFiles(): integer;

{*------------------------------------------------------------------------------
  Given a file path, checks to see if a file exists at that file path.

  @param fileNameOrPath Location on file system to check for file
  @return 1 if files exists, -1 otherwise
-------------------------------------------------------------------------------}
function checkIfFileExists(fileNameOrPath: string): integer;

{*------------------------------------------------------------------------------
  Given a file path and a file extention, checks the file path to see if its
  extention matches the desired extention.

  @param filePath Location on file system to check for file
  @param desiredExt Desired extention to check the file path for
  @return 1 if file path extention matches desired extention, -1 otherwise
-------------------------------------------------------------------------------}
function checkFileExt(filePath: string; desiredExt: string): integer;

{*------------------------------------------------------------------------------
  Saves errors accumulated from various routines in the course of execution of
  the converter to a text file on disc.

  @return none
-------------------------------------------------------------------------------}
procedure reportErrorsToFW();

{*------------------------------------------------------------------------------
  Displays errors accumulated from various routines in the course of execution of
  the converter on the console.

  @return none
-------------------------------------------------------------------------------}
procedure displayErrors();

implementation

uses
  SWMMIO;

function checkFileExt(filePath: string; desiredExt: string): integer;
var
  tempStr: string;
begin
  tempStr := ExtractFileExt(filePath);
  if (desiredExt <> tempStr) then
  begin
    result := -1;
    errorsList.Add(Errs[7] + ': ' + filePath);
    Exit;
  end;
  result := 1;
end;

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

procedure displayErrors();
var
  tempStr: string;
begin
  if ((errorsList.Count > 0) and (SWMMIO.appType = appTypes[0])) then
  begin
    for tempStr in errorsList do
      Writeln(tempStr);
  end
end;

end.
