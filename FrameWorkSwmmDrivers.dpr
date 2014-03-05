program FrameWorkSwmmDrivers;
{$IFDEF SWMM_CONSOLE}
{$APPTYPE CONSOLE}
{$R *.res}
{$ELSE}
{$R *.dres}
{$ENDIF}

uses
  System.SysUtils,
  Vcl.Forms,
  SWMMDrivers in 'SWMMDrivers.pas' {Form1} ,
  UserInputConfirmationDlg in 'UserInputConfirmationDlg.pas' {Form2} ,
  OperationStatusDlgFrm in 'OperationStatusDlgFrm.pas' {OperationStatusDlg} ,
  SWMMIO in 'SWMMIO.pas',
  ReadMTA in 'gsmodules\ReadMTA.pas',
  WriteMTA in 'gsmodules\WriteMTA.pas',
  SWMMInput in 'gsmodules\SWMMInput.pas',
  SWMMOutput in 'gsmodules\SWMMOutput.pas',
  FWControlScratchFile in 'gsmodules\FWControlScratchFile.pas';

{$R *.res}

begin

  // decide whether going from SWMM to FW or FW to SWMM
{$IFDEF SWMM_TO_FW}
  SWMMIO.operatingMode := SWMMIO.opModes[0]; // SWMM_TO_FW
{$ELSE}
  SWMMIO.operatingMode := SWMMIO.opModes[1]; // SWMM_FROM_FW
{$ENDIF}
  if (ParamCount > 2) then
    SWMMIO.frameCtrlFilePath := ParamStr(2);
  if (ParamCount > 1) then
    SWMMIO.mtaFilePath := ParamStr(1);

{$IFDEF SWMM_CONSOLE}
  SWMMIO.appType := appTypes[0]; // set the application type to SWMM_CONSOLE
  try
    if (ParamCount < 2) then
    begin
      Writeln('Error: Please pass in control file');
      Writeln('Usage: swmmConverterExe path_to_mta_file.mta path_to_frameWorkMetaData_file.txt');
      exit;
    end;
    Writeln('SWMM Converter Version 1.0.20140228');
    Writeln('Framework metadata scratch file:' + ParamStr(2));
    Writeln('Opening SWMM control file:' + ParamStr(1));
    // process import or export request
    if (SWMMIO.operatingMode = SWMMIO.opModes[0]) then
      SWMMOutput.consoleImportFromSWMMToFW(SWMMIO.mtaFilePath)
    else
      SWMMInput.consoleExportFromFWToSWMM(SWMMIO.mtaFilePath)
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

{$ELSE}
  SWMMIO.appType := appTypes[1]; // set the application type to SWMM_GUI
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOperationStatusDlg, OperationStatusDlg);
  // Application.CreateForm(TUserInputVerificationFrm,SWMMUserInputVerificationFrm );
  Application.Run;

{$ENDIF}

end.
