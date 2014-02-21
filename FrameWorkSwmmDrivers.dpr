program FrameWorkSwmmDrivers;

uses
  Vcl.Forms,
  SWMMDrivers in 'SWMMDrivers.pas' {Form1},
  UserInputConfirmationDlg in 'UserInputConfirmationDlg.pas' {Form2},
  OperationStatusDlgFrm in 'OperationStatusDlgFrm.pas' {OperationStatusDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TOperationStatusDlg, OperationStatusDlg);
  //Application.CreateForm(TUserInputVerificationFrm,SWMMUserInputVerificationFrm );
  Application.Run;
end.
