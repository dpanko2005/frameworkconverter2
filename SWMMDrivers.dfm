object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Framework SWMM Converter'
  ClientHeight = 748
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object txtSwmmFilePath: TLabel
    Left = 19
    Top = 77
    Width = 3
    Height = 13
    Color = clInfoBk
    ParentColor = False
  end
  object Label2: TLabel
    Left = 0
    Top = 631
    Width = 445
    Height = 13
    Caption = 
      '*TSS - Total Susp. Solids, TP - Total Phosphorus, DP - Dissolved' +
      ' Phosphorus, TZn - Total Zinc'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 40
    Top = 52
    Width = 122
    Height = 19
    Caption = 'Select SWMM File'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 32
    Top = 663
    Width = 220
    Height = 19
    Caption = 'OptionalTimeseries Description'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 32
    Top = 367
    Width = 226
    Height = 19
    Caption = 'Provide Unit Conversion Factors'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 19
    Top = 52
    Width = 15
    Height = 19
    Caption = '1:'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label7: TLabel
    Left = 11
    Top = 663
    Width = 15
    Height = 19
    Caption = '4:'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label8: TLabel
    Left = 11
    Top = 367
    Width = 15
    Height = 19
    Caption = '3:'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 19
    Top = 127
    Width = 15
    Height = 19
    Caption = '2:'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label9: TLabel
    Left = 19
    Top = 224
    Width = 64
    Height = 13
    Caption = 'Framework'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblOperatingMode: TLabel
    Left = 8
    Top = 8
    Width = 123
    Height = 19
    Alignment = taCenter
    Caption = 'operatingMode'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object lblHelp: TLabel
    Left = 447
    Top = 8
    Width = 21
    Height = 13
    Caption = 'Help'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHotLight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblHelpClick
  end
  object Label10: TLabel
    Left = 40
    Top = 199
    Width = 217
    Height = 19
    Caption = 'Match Framework Constituents'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label11: TLabel
    Left = 19
    Top = 199
    Width = 15
    Height = 19
    Caption = '3:'
    Color = clRed
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label12: TLabel
    Left = 40
    Top = 127
    Width = 135
    Height = 19
    Caption = 'Select SWMM Node'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label13: TLabel
    Left = 301
    Top = 221
    Width = 112
    Height = 13
    Caption = 'Linked Constituents'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label14: TLabel
    Left = 113
    Top = 224
    Width = 38
    Height = 13
    Caption = 'SWMM'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object btnSelectSWMMFile: TButton
    Left = 168
    Top = 49
    Width = 204
    Height = 25
    Caption = 'Browse...'
    TabOrder = 0
    OnClick = btnSelectSWMMFileClick
  end
  object cbxSwmmNode: TComboBox
    Left = 16
    Top = 152
    Width = 356
    Height = 21
    TabOrder = 1
    Text = 'Select SWMM Node'
    OnChange = cbxSwmmNodeChange
  end
  object sgdUserInputGrid: TStringGrid
    Left = 19
    Top = 392
    Width = 422
    Height = 233
    Color = clBtnFace
    ColCount = 3
    Ctl3D = False
    FixedColor = 16776176
    FixedCols = 0
    RowCount = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 2
    ColWidths = (
      136
      165
      136)
  end
  object btnCancel: TButton
    Left = 174
    Top = 715
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object btnHelp: TButton
    Left = 270
    Top = 715
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object btnNext: TButton
    Left = 366
    Top = 715
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 5
    OnClick = btnNextClick
  end
  object RadioGroup1: TRadioGroup
    Left = 316
    Top = 80
    Width = 133
    Height = 66
    Caption = 'Desired Operation'
    TabOrder = 6
    Visible = False
  end
  object txtScenarioDescr: TEdit
    Left = 8
    Top = 688
    Width = 352
    Height = 21
    TabOrder = 7
  end
  object lbxFWConstituents: TListBox
    Left = 19
    Top = 240
    Width = 70
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 8
  end
  object lbxSWMMConstituents: TListBox
    Left = 113
    Top = 240
    Width = 70
    Height = 97
    ItemHeight = 13
    TabOrder = 9
  end
  object btnLinkConstituents: TButton
    Left = 205
    Top = 257
    Width = 75
    Height = 25
    Caption = 'Link >>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = btnLinkConstituentsClick
  end
  object lbxLinkedConstituents: TListBox
    Left = 301
    Top = 240
    Width = 140
    Height = 97
    ItemHeight = 13
    TabOrder = 11
  end
  object unlinkConstituents: TButton
    Left = 205
    Top = 288
    Width = 75
    Height = 25
    Caption = '<< Unlink'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    OnClick = unlinkConstituentsClick
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Top = 712
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 48
    Top = 712
  end
end
