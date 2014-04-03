object OperationStatusDlg: TOperationStatusDlg
  Left = 0
  Top = 0
  Caption = 'OperationStatusDlg'
  ClientHeight = 348
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
  object lblHeader: TLabel
    Left = 128
    Top = 8
    Width = 298
    Height = 25
    Alignment = taCenter
    Caption = 'The Operation Was Successful!!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 151
    Height = 13
    Caption = 'Timeseries File(s) Created:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object lblTSFilesCreated: TLabel
    Left = 8
    Top = 83
    Width = 82
    Height = 13
    Caption = 'lblTSFilesCreated'
    WordWrap = True
  end
  object lblSWMMFileLabel: TLabel
    Left = 8
    Top = 200
    Width = 118
    Height = 13
    Caption = 'SWMM File Saved As:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object lblSWMMFilePath: TLabel
    Left = 8
    Top = 219
    Width = 74
    Height = 13
    Caption = 'SWMM file path'
  end
  object btnClose: TButton
    Left = 471
    Top = 315
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 0
    OnClick = btnCloseClick
  end
end
