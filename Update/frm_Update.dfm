object frmUpdate: TfrmUpdate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21319#32423
  ClientHeight = 380
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblHint: TLabel
    Left = 0
    Top = 273
    Width = 537
    Height = 13
    Align = alTop
    Caption = #27491#22312#19979#36733'...'
    ExplicitWidth = 60
  end
  object btnOK: TButton
    Left = 232
    Top = 336
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 0
  end
  object mmoUpdateExplain: TMemo
    Left = 0
    Top = 0
    Width = 537
    Height = 153
    Align = alTop
    TabOrder = 1
  end
  object sgdUpdateFiles: TStringGrid
    Left = 0
    Top = 153
    Width = 537
    Height = 120
    Align = alTop
    FixedCols = 0
    TabOrder = 2
    ExplicitLeft = 88
    ExplicitTop = 192
    ExplicitWidth = 320
  end
  object pb: TProgressBar
    Left = 0
    Top = 286
    Width = 537
    Height = 17
    Align = alTop
    TabOrder = 3
    ExplicitLeft = 88
    ExplicitTop = 304
    ExplicitWidth = 150
  end
end
