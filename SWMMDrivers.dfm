object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Framework SWMM Converter'
  ClientHeight = 620
  ClientWidth = 493
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
    Top = 125
    Width = 286
    Height = 13
    Caption = 'File Name'
    Color = clInfoBk
    ParentColor = False
  end
  object Label2: TLabel
    Left = 8
    Top = 511
    Width = 364
    Height = 26
    Caption = 
      '*Note: TSS - Total Suspended Solids, TP - Total Phosphorus, DP -' +
      ' Dissolved Phosphorus, TZn - Total Zinc'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 40
    Top = 100
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
    Left = 40
    Top = 175
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
  object Label5: TLabel
    Left = 40
    Top = 247
    Width = 269
    Height = 19
    Caption = 'Match Framework Constituents / Units'
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
    Top = 100
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
    Left = 19
    Top = 175
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
  object Label8: TLabel
    Left = 19
    Top = 247
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
  object Image1: TImage
    Left = 320
    Top = 56
    Width = 169
    Height = 210
  end
  object btnSelectSWMMFile: TButton
    Left = 168
    Top = 94
    Width = 146
    Height = 25
    Caption = 'Browse...'
    TabOrder = 0
    OnClick = btnSelectSWMMFileClick
  end
  object cbxSwmmNode: TComboBox
    Left = 16
    Top = 200
    Width = 217
    Height = 21
    TabOrder = 1
    Text = 'Select SWMM Node'
  end
  object sgdUserInputGrid: TStringGrid
    Left = 8
    Top = 272
    Width = 441
    Height = 233
    Color = clBtnFace
    ColCount = 3
    Ctl3D = False
    FixedColor = 16776176
    FixedCols = 0
    RowCount = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 2
    ColWidths = (
      136
      165
      136)
  end
  object cbxFlow: TComboBox
    Left = 147
    Top = 298
    Width = 159
    Height = 21
    TabOrder = 3
    Text = 'Select SWMM Pollutant'
    Visible = False
  end
  object cbxDCu: TComboBox
    Left = 147
    Top = 447
    Width = 159
    Height = 21
    TabOrder = 4
    Text = 'Select SWMM Pollutant'
  end
  object cbxTZn: TComboBox
    Left = 148
    Top = 422
    Width = 159
    Height = 21
    TabOrder = 5
    Text = 'Select SWMM Pollutant'
  end
  object cbxDZn: TComboBox
    Left = 148
    Top = 397
    Width = 159
    Height = 21
    TabOrder = 6
    Text = 'Select SWMM Pollutant'
  end
  object cbxDP: TComboBox
    Left = 148
    Top = 372
    Width = 159
    Height = 21
    TabOrder = 7
    Text = 'Select SWMM Pollutant'
  end
  object cbxTP: TComboBox
    Left = 146
    Top = 347
    Width = 159
    Height = 21
    TabOrder = 8
    Text = 'Select SWMM Pollutant'
  end
  object cbxTSS: TComboBox
    Left = 147
    Top = 322
    Width = 159
    Height = 21
    TabOrder = 9
    Text = 'Select SWMM Pollutant'
  end
  object btnCancel: TButton
    Left = 182
    Top = 584
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 10
    OnClick = btnCancelClick
  end
  object btnHelp: TButton
    Left = 278
    Top = 584
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 11
  end
  object btnNext: TButton
    Left = 374
    Top = 584
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 12
    OnClick = btnNextClick
  end
  object cbxTCu: TComboBox
    Left = 148
    Top = 472
    Width = 159
    Height = 21
    TabOrder = 13
    Text = 'Select SWMM Pollutant'
  end
  object RadioGroup1: TRadioGroup
    Left = 29
    Top = 22
    Width = 133
    Height = 66
    Caption = 'Desired Operation'
    TabOrder = 14
    OnClick = RadioGroup1Click
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 40
    Top = 544
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 128
    Top = 552
  end
end
