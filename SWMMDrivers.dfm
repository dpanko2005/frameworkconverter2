object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Framework SWMM Converter'
  ClientHeight = 512
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
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
  object Label5: TLabel
    Left = 32
    Top = 287
    Width = 178
    Height = 19
    Caption = 'Select SWMM Constituent'
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
  object Label8: TLabel
    Left = 11
    Top = 287
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
    Top = 119
    Width = 142
    Height = 19
    Caption = 'Select SWMM Nodes'
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
  object Label15: TLabel
    Left = 19
    Top = 317
    Width = 96
    Height = 26
    Caption = 'Available SWMM Constituents'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object lblSelectedFWConstituents: TLabel
    Left = 226
    Top = 321
    Width = 116
    Height = 26
    Caption = 'Selected For Import to Framework'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label9: TLabel
    Left = 19
    Top = 141
    Width = 96
    Height = 26
    Caption = 'Available SWMM Constituents'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Label13: TLabel
    Left = 226
    Top = 145
    Width = 116
    Height = 26
    Caption = 'Selected For Import to Framework'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object lblTSStartEndDate: TLabel
    Left = 19
    Top = 93
    Width = 251
    Height = 13
    Caption = 'Simulation Period: From mm/dd/yyyy to mm/dd/yyyy'
    Color = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
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
  object btnCancel: TButton
    Left = 168
    Top = 467
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnHelp: TButton
    Left = 264
    Top = 467
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnRun: TButton
    Left = 360
    Top = 467
    Width = 75
    Height = 25
    Caption = 'Run'
    Enabled = False
    TabOrder = 3
    OnClick = btnRunClick
  end
  object lbxAvailSWMMConstituents: TListBox
    Left = 19
    Top = 353
    Width = 88
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 4
  end
  object lbxSelectedSWMMConstituents: TListBox
    Left = 226
    Top = 353
    Width = 88
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 5
  end
  object btnConstituentInclude: TButton
    Left = 129
    Top = 361
    Width = 75
    Height = 25
    Caption = 'Include >>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = btnConstituentIncludeClick
  end
  object btnConstituentExclude: TButton
    Left = 129
    Top = 392
    Width = 75
    Height = 25
    Caption = '<< Exclude'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = btnConstituentExcludeClick
  end
  object lbxAvailSWMMNodes: TListBox
    Left = 19
    Top = 177
    Width = 88
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
  object lbxSelectedSWMMNodes: TListBox
    Left = 226
    Top = 177
    Width = 88
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 9
  end
  object btnNodeInclude: TButton
    Left = 129
    Top = 185
    Width = 75
    Height = 25
    Caption = 'Include >>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = btnNodeIncludeClick
  end
  object btnNodeExclude: TButton
    Left = 129
    Top = 216
    Width = 75
    Height = 25
    Caption = '<< Exclude'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = btnNodeExcludeClick
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 10
    Top = 456
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 90
    Top = 472
  end
end
