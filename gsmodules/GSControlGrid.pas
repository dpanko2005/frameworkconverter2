// adapted from http://delphi.xcjc.net/viewthread.php?tid=44064
{*------------------------------------------------------------------------------
  Delphi Pascal unit containing custom stringgrid that allows a combobox to be
  created in a cell

  @unit:    GSControlGrid.pas
  @project: WERF Framework - SWMM Converter
  @version: 2.0
  @date:    2/28/2014
  @author:  Gesoyntec (D. Pankani)
------------------------------------------------------------------------------- }
unit GSControlGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, stdctrls;

{*------------------------------------------------------------------------------
  Custom class derived from TStringGrid used in the GUI to create comboboxes
  within TStringGrids

-------------------------------------------------------------------------------}
type
{*------------------------------------------------------------------------------
  Custom class derived from TStringGrid used in the GUI to create comboboxes
  within TStringGrids
-------------------------------------------------------------------------------}
  TGSControlGrid = class(TStringGrid)
  private
{*------------------------------------------------------------------------------
  Combobox to be drawn in string grid
-------------------------------------------------------------------------------}
    fComboBox: TCombobox;
{*------------------------------------------------------------------------------
  Handles messages sent to control
  @param msg message sent to the control
-------------------------------------------------------------------------------}
    Procedure WMCommand(var msg: TWMCommand); message WM_COMMAND;

  {*------------------------------------------------------------------------------
    Double-click handler override

  ------------------------------------------------------------------------------- }
    procedure DblClick; override;

  {*------------------------------------------------------------------------------
    Click handler override

  ------------------------------------------------------------------------------- }
    procedure Click; override;

  {*------------------------------------------------------------------------------
    Draws combobox at location specified by selection

  ------------------------------------------------------------------------------- }
    procedure RelocateComboBox;

  {*------------------------------------------------------------------------------
    Hides the combobox

  ------------------------------------------------------------------------------- }
    procedure HideCombobox;


  protected
  {*------------------------------------------------------------------------------
    Keypress handler override

    @param Key key that was pressed
  ------------------------------------------------------------------------------- }
    procedure KeyPress(var Key: Char); override;

  public
  {*------------------------------------------------------------------------------
    Class constructor

    @param AOWner class owner
  ------------------------------------------------------------------------------- }
    Constructor Create(AOWner: TComponent); override;

  {*------------------------------------------------------------------------------
    Class destructor
  ------------------------------------------------------------------------------- }
    Destructor Destroy; override;

  published
  end;

{*------------------------------------------------------------------------------
  Procedure to register the component
-------------------------------------------------------------------------------}
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
