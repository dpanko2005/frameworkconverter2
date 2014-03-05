unit UserInputConfirmationDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtDlgs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Imaging.GIFImg;

type
  TProgressCallback = procedure(InProgressOverall: TProgressBar) of Object;

type
  TUserInputVerificationFrm = class(TForm)
    Label1: TLabel;
    txtSwmmFilePath: TLabel;
    txtSWMMNodeID: TLabel;
    lblNumberOfConstituents: TLabel;
    StringGrid1: TStringGrid;
    Label9: TLabel;
    txtErrors: TLabel;
    Label2: TLabel;
    btnRun: TButton;
    btnCancel: TButton;
    procedure btnRunClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

  end;

procedure ShowProgress(InCallback: TProgressCallback);

var
  SWMMUserInputVerificationFrm: TUserInputVerificationFrm;

implementation

{$R *.dfm}

procedure TUserInputVerificationFrm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TUserInputVerificationFrm.btnRunClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure ShowProgress(InCallback: TProgressCallback);
var
  LWindowList: TTaskWindowList;
  LSaveFocusState: TFocusState;
  LProgressForm: TUserInputVerificationFrm;
begin
  LProgressForm := TUserInputVerificationFrm.Create(NIL);
  try
    LSaveFocusState := SaveFocusState;
    Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
    Application.ModalStarted;
    LWindowList := DisableTaskWindows(0);
    Screen.FocusedForm := LProgressForm;
    SendMessage(LProgressForm.Handle, CM_ACTIVATE, 0, 0);
    LProgressForm.Show;
    //InCallback(LProgressForm.prgExectionProgress);
    EnableTaskWindows(LWindowList);
    RestoreFocusState(LSaveFocusState);
  finally
    Application.ModalFinished;
    FreeAndNil(SWMMUserInputVerificationFrm);
  end;
end;

end.
