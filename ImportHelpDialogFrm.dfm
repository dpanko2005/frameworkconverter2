object ImportHelpDialog: TImportHelpDialog
  Left = 0
  Top = 0
  Caption = 'Help'
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
  object lblOperatingMode: TLabel
    Left = 104
    Top = 8
    Width = 317
    Height = 19
    Alignment = taCenter
    Caption = 'Importing from SWMM into Framework'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 24
    Top = 40
    Width = 298
    Height = 13
    Caption = '1. Browse to and select a SWMM5 results output file (.out file)'
  end
  object Label2: TLabel
    Left = 24
    Top = 72
    Width = 521
    Height = 26
    Caption = 
      '2. The list of nodes in the SWMM5 file will be extracted and mad' +
      'e available for selection in the node selection drop down. Selec' +
      't the node you want to import from.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 24
    Top = 119
    Width = 518
    Height = 26
    Caption = 
      '3. Now proceed to match pollutant names from the SWMM5 file to t' +
      'he pollutant names from the framework. Un-matched pollutants wil' +
      'l not be included in the import '
    WordWrap = True
  end
  object Label4: TLabel
    Left = 24
    Top = 168
    Width = 509
    Height = 26
    Caption = 
      '4. When satisfied with your inputs push Next button to proceed t' +
      'o the user input verification dialog which shows a summary of th' +
      'e inputs entered'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 24
    Top = 213
    Width = 497
    Height = 26
    Caption = 
      '5. Hit the Run button on the user input verification dialog to p' +
      'rocess the import or the Cancel button to return to the main for' +
      'm'
    WordWrap = True
  end
  object Button1: TButton
    Left = 471
    Top = 257
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
end
