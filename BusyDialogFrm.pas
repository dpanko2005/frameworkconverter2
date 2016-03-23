{ ------------------------------------------------------------------- }
{ Unit:    BusyDialogFrm.pas                                          }
{ Project: WERF Framework - SWMM Converter                            }
{ Version: 2.0                                                        }
{ Date:    2/28/2014                                                  }
{ Author:  Gesoyntec (D. Pankani)                                     }
{                                                                     }
{ Delphi Pascal unit that for the busy dialog                         }
{ ------------------------------------------------------------------- }

unit BusyDialogFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.GIFImg, Vcl.ExtCtrls,
  SWMMIO, Vcl.StdCtrls;

type
  TBusyFrm = class(TForm)
    Label1: TLabel;
    Image1: TImage;

    ///	<summary>
    ///	  Show handler for busy dialog. Used to simply change dialog color
    ///	</summary>
    ///	<param name="Sender">
    ///	  Owner (this dialog)
    ///	</param>
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BusyFrm: TBusyFrm;

implementation

{$R *.dfm}

procedure TBusyFrm.FormShow(Sender: TObject);
begin
  BusyFrm.color := clwhite;
end;

end.
