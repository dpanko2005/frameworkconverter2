program FrameWorkSwmmDrivers;
{$IFDEF SWMM_CONSOLE}
{$APPTYPE CONSOLE}
{$R *.res}
{$ELSE}
{$R *.dres}
{$ENDIF}

{$R *.dres}

uses
  SysUtils,
  Classes,
  Vcl.Forms,
  SWMMDrivers in 'SWMMDrivers.pas' {Form1},
  SWMMIO in 'gsmodules\SWMMIO.pas',
  ReadMTA in 'gsmodules\ReadMTA.pas',
  WriteMTA in 'gsmodules\WriteMTA.pas',
  SWMMOutput in 'gsmodules\SWMMOutput.pas',
  SWMMInput in 'gsmodules\SWMMInput.pas',
  MTATemplateString in 'gsmodules\MTATemplateString.pas',
  ImportHelpDialogFrm in 'ImportHelpDialogFrm.pas' {ImportHelpDialog},
  ExportHelpDlgFrm in 'ExportHelpDlgFrm.pas' {ExportHelpDialogFrm},
  GSControlGrid in 'gsmodules\GSControlGrid.pas',
  ConverterErrors in 'gsmodules\ConverterErrors.pas',
  FWIO in 'gsmodules\FWIO.pas';

{$R *.res}

begin

  SWMMIO.workingDir := ExtractFilePath(Application.ExeName);
  //create list for holding errors
  ConverterErrors.errorsList := TStringList.Create();
  // decide whether going from SWMM to FW or FW to SWMM
{$IFDEF SWMM_TO_FW}
  SWMMIO.operatingMode := SWMMIO.opModes[0]; // SWMM_TO_FW
{$ELSE}
  SWMMIO.operatingMode := SWMMIO.opModes[1]; // SWMM_FROM_FW
{$ENDIF}
  { if (ParamCount > 2) then
    SWMMIO.frameCtrlFilePath := ParamStr(2);
    if (ParamCount > 1) then
    SWMMIO.mtaFilePath := ParamStr(1);
  }
{$IFDEF SWMM_CONSOLE}
  SWMMIO.appType := appTypes[0]; // set the application type to SWMM_CONSOLE
  try

    Writeln('SWMM Converter Version 2.0');
    Writeln('Opening SWMM Converter Control File:' + SWMMIO.workingDir +
      SWMMIO.fileNameFWControlFile);

    // process import or export request
    if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
    begin
      { if (ParamCount < 1) then
        begin
        Writeln('Error: Please pass in SWMM Converter Metadata Control file (*.mta)');
        Writeln('Usage: pathToConverter\SWMMOutput.exe path_to_mta_file.mta');
        exit;
        end;
        SWMMIO.mtaFilePath := ParamStr(1); }
      SWMMInput.consoleImportFromSWMMToFW(SWMMIO.fileNameFWControlFile)
      // importing
    end
    else
    begin
      { if (ParamCount < 1) then
        begin
        Writeln('Error: Please pass in SWMM Converter Metadata Control file (*.mta)');
        Writeln('Usage: pathToConverter\SWMMInput.exe path_to_mta_file.mta');
        exit;
        end;
        SWMMIO.mtaFilePath := ParamStr(1);
        Writeln('SWMM Converter Version 1.0.20140228');
        Writeln('Opening SWMM Converter Control File:' + SWMMIO.mtaFilePath); }
      SWMMOutput.consoleExportFromFWToSWMM(SWMMIO.fileNameFWControlFile)
      // exporting
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

{$ELSE}
  SWMMIO.appType := appTypes[1]; // set the application type to SWMM_GUI
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  // Application.CreateForm(TUserInputVerificationFrm,SWMMUserInputVerificationFrm );
  Application.Run;

{$ENDIF}

end.
