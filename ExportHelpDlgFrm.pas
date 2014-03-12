unit ExportHelpDlgFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TExportHelpDialogFrm = class(TForm)
    lblOperatingMode: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportHelpDialogFrm: TExportHelpDialogFrm;

implementation

{$R *.dfm}

procedure TExportHelpDialogFrm.Button1Click(Sender: TObject);
begin
Self.Close;
end;

end.
