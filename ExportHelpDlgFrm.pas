{*------------------------------------------------------------------------------
  Delphi Pascal unit containing code for a dialog that displays help related to
  the export of timeseries

  @Unit    ExportHelpDlgFrm.pas
  @Project WERF Framework - SWMM Converter
  @Version 2.0
  @Date    2/28/2014
  @Author  Gesoyntec Consultants Inc (D. Pankani)
------------------------------------------------------------------------------- }
unit ExportHelpDlgFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;


type
  {*------------------------------------------------------------------------------
    Help dialog with brief help text related to exporting
  ------------------------------------------------------------------------------- }
  TExportHelpDialogFrm = class(TForm)
  {*------------------------------------------------------------------------------
    Variable title text that changes to denote import or export help
  ------------------------------------------------------------------------------- }
    lblOperatingMode: TLabel;
  {*------------------------------------------------------------------------------
    Generic lines of help text
  ------------------------------------------------------------------------------- }
    Label1: TLabel;
  {*------------------------------------------------------------------------------
    Generic lines of help text
  ------------------------------------------------------------------------------- }
    Label2: TLabel;
  {*------------------------------------------------------------------------------
    Generic lines of help text
  ------------------------------------------------------------------------------- }
    Label3: TLabel;
  {*------------------------------------------------------------------------------
    Generic lines of help text
  ------------------------------------------------------------------------------- }
    Label4: TLabel;
  {*------------------------------------------------------------------------------
    Generic lines of help text
  ------------------------------------------------------------------------------- }
    Label5: TLabel;
  {*------------------------------------------------------------------------------
    Button that dismisses help text
  ------------------------------------------------------------------------------- }
    Button1: TButton;

  {*------------------------------------------------------------------------------
    Handler for button that dismisses this dialog

    @param Sender source of click
  ------------------------------------------------------------------------------- }
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  {*------------------------------------------------------------------------------
    Handler for help dialog form
  ------------------------------------------------------------------------------- }
  ExportHelpDialogFrm: TExportHelpDialogFrm;

implementation

{$R *.dfm}

procedure TExportHelpDialogFrm.Button1Click(Sender: TObject);
begin
Self.Close;
end;

end.
