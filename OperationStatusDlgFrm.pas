{ ------------------------------------------------------------------- }
{ Unit:    OperationStatusDlgFrm.pas                                  }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that dispalys a dialog that show the status of   }
{ the import or export operation                                      }
{ ------------------------------------------------------------------- }

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

    ///	<summary>
    ///	  Handler for dismissing the dialog
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner
    ///	</param>
    procedure btnCloseClick(Sender: TObject);

    ///	<summary>
    ///	  Handler for showing the dialog
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner
    ///	</param>
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
