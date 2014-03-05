unit OperationStatusDlgFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TOperationStatusDlg = class(TForm)
    lblHeader: TLabel;
    Label2: TLabel;
    lblTSFilesCreated: TLabel;
    lblSWMMFileLabel: TLabel;
    lblSWMMFilePath: TLabel;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OperationStatusDlg: TOperationStatusDlg;

implementation

{$R *.dfm}

procedure TOperationStatusDlg.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TOperationStatusDlg.FormShow(Sender: TObject);
begin
  OperationStatusDlg.color := clwhite;
end;

end.
