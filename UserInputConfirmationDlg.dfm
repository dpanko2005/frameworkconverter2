object UserInputVerificationFrm: TUserInputVerificationFrm
  Left = 0
  Top = 0
  Caption = 'Input Verification'
  ClientHeight = 540
  ClientWidth = 554
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
  object Label4: TLabel
    Left = 32
    Top = 96
    Width = 100
    Height = 13
    Caption = 'Constituents (x of 8)'
  end
  object Label5: TLabel
    Left = 25
    Top = 424
    Width = 107
    Height = 13
    Caption = 'Timeseries Start Date:'
  end
  object Label6: TLabel
    Left = 25
    Top = 448
    Width = 101
    Height = 13
    Caption = 'Timeseries End Date:'
  end
  object Label7: TLabel
    Left = 23
    Top = 467
    Width = 98
    Height = 13
    Caption = 'Timeseries File Path:'
  end
  object Label8: TLabel
    Left = 25
    Top = 486
    Width = 33
    Height = 13
    Caption = 'Errors:'
  end
  object Label9: TLabel
    Left = 32
    Top = 64
    Width = 64
    Height = 13
    Caption = 'SWMM Node:'
  end
  object txtTSStartDate: TLabel
    Left = 152
    Top = 424
    Width = 73
    Height = 13
    Caption = 'txtTSStartDate'
  end
  object txtTSEndDate: TLabel
    Left = 152
    Top = 448
    Width = 67
    Height = 13
    Caption = 'txtTSEndDate'
  end
  object txtTSFilePath: TLabel
    Left = 152
    Top = 464
    Width = 64
    Height = 13
    Caption = 'txtTSFilePath'
  end
  object txtErrors: TLabel
    Left = 136
    Top = 488
    Width = 43
    Height = 13
    Caption = 'txtErrors'
  end
  object Label2: TLabel
    Left = 32
    Top = 375
    Width = 364
    Height = 26
    Caption = 
      '*Note: TSS - Total Suspended Solids, TP - Total Phosphorus, DP -' +
      ' Dissolved Phosphorus, TZn - Total Zinc'
    WordWrap = True
  end
  object StringGrid1: TStringGrid
    Left = 32
    Top = 136
    Width = 361
    Height = 233
    Color = clWhite
    ColCount = 3
    Ctl3D = False
    FixedColor = 16776176
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 0
    ColWidths = (
      138
      112
      117)
  end
  object btnRun: TButton
    Left = 344
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = btnRunClick
  end
  object btnCancel: TButton
    Left = 263
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
