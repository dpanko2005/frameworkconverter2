object UserInputVerificationFrm: TUserInputVerificationFrm
  Left = 0
  Top = 0
  Caption = 'Input Verification'
  ClientHeight = 422
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 40
    Width = 109
    Height = 13
    Caption = 'SWMM Input File Path:'
  end
  object txtSwmmFilePath: TLabel
    Left = 160
    Top = 40
    Width = 82
    Height = 13
    Caption = 'txtSwmmFilePath'
  end
  object txtSWMMNodeID: TLabel
    Left = 152
    Top = 64
    Width = 82
    Height = 13
    Caption = 'txtSWMMNodeID'
  end
  object lblNumberOfConstituents: TLabel
    Left = 32
    Top = 83
    Width = 100
    Height = 13
    Caption = 'Constituents (x of 8)'
  end
  object Label9: TLabel
    Left = 32
    Top = 64
    Width = 64
    Height = 13
    Caption = 'SWMM Node:'
  end
  object txtErrors: TLabel
    Left = 32
    Top = 21
    Width = 3
    Height = 13
    Caption = ' '
  end
  object Label2: TLabel
    Left = 32
    Top = 341
    Width = 364
    Height = 26
    Caption = 
      '*Note: TSS - Total Suspended Solids, TP - Total Phosphorus, DP -' +
      ' Dissolved Phosphorus, TZn - Total Zinc'
    WordWrap = True
  end
  object StringGrid1: TStringGrid
    Left = 32
    Top = 102
    Width = 368
    Height = 233
    Color = clWhite
    ColCount = 3
    Ctl3D = False
    FixedColor = 16776176
    FixedCols = 0
    RowCount = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 0
    ColWidths = (
      138
      105
      117)
  end
  object btnRun: TButton
    Left = 322
    Top = 390
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = btnRunClick
  end
  object btnCancel: TButton
    Left = 241
    Top = 390
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
