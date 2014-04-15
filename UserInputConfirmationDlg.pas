{ ------------------------------------------------------------------- }
{ Unit:    UserInputConfirmationDlg.pas                               }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that dispalys a dialog that show the a summary   }
{ of the imputs entered by the user                                   }
{ ------------------------------------------------------------------- }

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

    ///	<summary>
    ///	  Handler for moving forward and dismissing user verification dialog to
    ///	  be used when the user is sure that all the input has been properly
    ///	  entered
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner
    ///	</param>
    procedure btnRunClick(Sender: TObject);

    ///	<summary>
    ///	  Handler for canceling and dismissing user verification dialog to be
    ///	  used if input summary shows unintended input
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner
    ///	</param>
    procedure btnCancelClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

  end;

//procedure ShowProgress(InCallback: TProgressCallback);

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


end.
