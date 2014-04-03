{ ------------------------------------------------------------------- }
{ Unit:    ImportHelpDialogFrm.pas                                    }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that the ImportHelpDialogFrm dialog              }
{ ------------------------------------------------------------------- }
unit ImportHelpDialogFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TImportHelpDialog = class(TForm)
    Button1: TButton;
    lblOperatingMode: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;

    ///	<summary>
    ///	  Handler for button that dismisses this dialog
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner
    ///	</param>
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImportHelpDialog: TImportHelpDialog;

implementation

{$R *.dfm}

procedure TImportHelpDialog.Button1Click(Sender: TObject);
begin
Self.Close;
end;

end.
