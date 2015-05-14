
{*------------------------------------------------------------------------------
  Delphi Pascal unit containing code for dialog that displays help

  @unit:    ImportHelpDialogFrm.pas
  @project: WERF Framework - SWMM Converter
  @version: 2.0
  @date:    2/28/2014
  @author:  Gesoyntec (D. Pankani)
------------------------------------------------------------------------------- }
unit ImportHelpDialogFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

  {*------------------------------------------------------------------------------
    Help dialog with brief help text related to importing
  ------------------------------------------------------------------------------- }
type
  {*------------------------------------------------------------------------------
    Help dialog with brief help text related to importing
  ------------------------------------------------------------------------------- }
  TImportHelpDialog = class(TForm)
  {*------------------------------------------------------------------------------
    Button for dismissing dialog
  ------------------------------------------------------------------------------- }
    Button1: TButton;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    lblOperatingMode: TLabel;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    Label1: TLabel;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    Label2: TLabel;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    Label3: TLabel;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    Label4: TLabel;

  {*------------------------------------------------------------------------------
    Label with help text and instructions
  ------------------------------------------------------------------------------- }
    Label5: TLabel;

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
    Handle for help dialog with brief help text related to importing
  ------------------------------------------------------------------------------- }
  ImportHelpDialog: TImportHelpDialog;

implementation

{$R *.dfm}

procedure TImportHelpDialog.Button1Click(Sender: TObject);
begin
Self.Close;
end;

end.
