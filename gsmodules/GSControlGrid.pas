// adapted from http://delphi.xcjc.net/viewthread.php?tid=44064

unit GSControlGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, stdctrls;

type
  TGSControlGrid = class(TStringGrid)
  private
    fComboBox: TCombobox;
    Procedure WMCommand(var msg: TWMCommand); message WM_COMMAND;
    procedure DblClick; override;
    procedure Click; override;
    procedure RelocateComboBox;
    procedure HideCombobox;

  protected
    procedure KeyPress(var Key: Char); override;

  public
    Constructor Create(AOWner: TComponent); override;
    Destructor Destroy; override;

  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Geosyntec', [TGSControlGrid]);
end;

procedure TGSControlGrid.WMCommand(var msg: TWMCommand);
begin
  If EditorMode and (msg.Ctl = fComboBox.Handle) Then
    inherited
  Else If msg.Ctl= 0 Then
    msg.result := SendMessage(msg.Ctl, CN_COMMAND, TMessage(msg).wparam,
      TMessage(msg).lparam);
end;

procedure TGSControlGrid.KeyPress(var Key: Char);
begin
  if Key = #13 then
    RelocateComboBox
  else
    HideCombobox;

end;

procedure TGSControlGrid.DblClick;
begin
  inherited;
  RelocateComboBox;
end;

procedure TGSControlGrid.Click;
begin
  inherited;
  HideCombobox;
end;

procedure TGSControlGrid.RelocateComboBox;
begin
  fComboBox.boundsrect := CellRect(Selection.Left, Selection.Top);
  fComboBox.Visible := TRUE;
  fComboBox.setfocus;
end;

procedure TGSControlGrid.HideCombobox;
begin
  fComboBox.Visible := false;
end;

Constructor TGSControlGrid.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  fComboBox := TCombobox.Create(self);
  fComboBox.Parent := self;
  fComboBox.Visible := false;
  Options := Options - [goRangeSelect];
end;

Destructor TGSControlGrid.Destroy;
begin
  fComboBox.Destroy;
  inherited Destroy;
end;

end.
