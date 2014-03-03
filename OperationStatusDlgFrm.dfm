object OperationStatusDlg: TOperationStatusDlg
  Left = 0
  Top = 0
  Caption = 'OperationStatusDlg'
  ClientHeight = 290
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
    Left = 0
    Top = 0
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
    Left = 0
    Top = 40
    Width = 128
    Height = 13
    Caption = 'Timeseries File(s) Created:'
  end
  object lblTSFilesCreated: TLabel
    Left = 8
    Top = 59
    Width = 82
    Height = 13
    Caption = 'lblTSFilesCreated'
  end
  object Label3: TLabel
    Left = 8
    Top = 128
    Width = 103
    Height = 13
    Caption = 'SWMM File Saved As:'
  end
  object lblSWMMFilePath: TLabel
    Left = 8
    Top = 147
    Width = 74
    Height = 13
    Caption = 'SWMM file path'
  end
  object btnClose: TButton
    Left = 471
    Top = 257
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnCloseClick
  end
end
