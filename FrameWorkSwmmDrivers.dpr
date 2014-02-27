program FrameWorkSwmmDrivers;

{$R *.dres}

uses
  Vcl.Forms,
  SWMMDrivers in 'SWMMDrivers.pas' {Form1},
  UserInputConfirmationDlg in 'UserInputConfirmationDlg.pas' {Form2},
  OperationStatusDlgFrm in 'OperationStatusDlgFrm.pas' {OperationStatusDlg},
  SWMMIO in 'SWMMIO.pas',
  ReadMTA in 'gsmodules\ReadMTA.pas',
  WriteMTA in 'gsmodules\WriteMTA.pas',
  SWMMInput in 'gsmodules\SWMMInput.pas',
  SWMMOutput in 'gsmodules\SWMMOutput.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOperationStatusDlg, OperationStatusDlg);
  //Application.CreateForm(TUserInputVerificationFrm,SWMMUserInputVerificationFrm );
  Application.Run;
  Form1.Hide();
end.
