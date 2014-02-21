object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Framework SWMM Converter'
  ClientHeight = 719
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
    Left = 16
    Top = 61
    Width = 46
    Height = 13
    Caption = 'File Name'
    Color = clInfoBk
    ParentColor = False
  end
  object Label2: TLabel
    Left = 8
    Top = 463
    Width = 364
    Height = 26
    Caption = 
      '*Note: TSS - Total Suspended Solids, TP - Total Phosphorus, DP -' +
      ' Dissolved Phosphorus, TZn - Total Zinc'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 40
    Top = 36
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
    Top = 119
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
    Top = 199
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
    Top = 36
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
    Top = 119
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
  object Image1: TImage
    Left = 320
    Top = 8
    Width = 169
    Height = 210
  end
  object btnSelectSWMMFile: TButton
    Left = 200
    Top = 56
    Width = 153
    Height = 25
    Caption = 'Browse...'
    TabOrder = 0
    OnClick = btnSelectSWMMFileClick
  end
  object cbxSwmmNode: TComboBox
    Left = 16
    Top = 144
    Width = 217
    Height = 21
    TabOrder = 1
    Text = 'Select SWMM Node'
  end
  object Memo1: TMemo
    Left = 8
    Top = 567
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 224
    Width = 441
    Height = 233
    Color = clBtnFace
    ColCount = 3
    Ctl3D = False
    FixedColor = 16776176
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    ParentCtl3D = False
    ScrollBars = ssNone
    TabOrder = 3
    ColWidths = (
      136
      165
      136)
  end
  object cbxFlow: TComboBox
    Left = 147
    Top = 250
    Width = 159
    Height = 21
    TabOrder = 4
    Text = 'Select SWMM Pollutant'
    Visible = False
  end
  object cbxDCu: TComboBox
    Left = 147
    Top = 399
    Width = 159
    Height = 21
    TabOrder = 5
    Text = 'Select SWMM Pollutant'
  end
  object cbxTZn: TComboBox
    Left = 148
    Top = 374
    Width = 159
    Height = 21
    TabOrder = 6
    Text = 'Select SWMM Pollutant'
  end
  object cbxDZn: TComboBox
    Left = 148
    Top = 349
    Width = 159
    Height = 21
    TabOrder = 7
    Text = 'Select SWMM Pollutant'
  end
  object cbxDP: TComboBox
    Left = 148
    Top = 324
    Width = 159
    Height = 21
    TabOrder = 8
    Text = 'Select SWMM Pollutant'
  end
  object cbxTP: TComboBox
    Left = 146
    Top = 299
    Width = 159
    Height = 21
    TabOrder = 9
    Text = 'Select SWMM Pollutant'
  end
  object cbxTSS: TComboBox
    Left = 147
    Top = 274
    Width = 159
    Height = 21
    TabOrder = 10
    Text = 'Select SWMM Pollutant'
  end
  object btnCancel: TButton
    Left = 182
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 11
  end
  object btnHelp: TButton
    Left = 278
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 12
  end
  object btnNext: TButton
    Left = 374
    Top = 536
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 13
    OnClick = btnNextClick
  end
  object cbxTCu: TComboBox
    Left = 148
    Top = 424
    Width = 159
    Height = 21
    TabOrder = 14
    Text = 'Select SWMM Pollutant'
  end
  object RadioGroup1: TRadioGroup
    Left = 235
    Top = 567
    Width = 133
    Height = 66
    Caption = 'Desired Operation'
    TabOrder = 15
    OnClick = RadioGroup1Click
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 200
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 256
    Top = 8
  end
end
