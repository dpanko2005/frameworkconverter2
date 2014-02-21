unit OperationStatusDlgFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TOperationStatusDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblTSFilesCreated: TLabel;
    Label3: TLabel;
    lblSWMMFilePath: TLabel;
    btnClose: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OperationStatusDlg: TOperationStatusDlg;

implementation

{$R *.dfm}

end.
