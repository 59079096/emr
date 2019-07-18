object frmScriptIDE: TfrmScriptIDE
  Left = 0
  Top = 0
  Caption = #33050#26412#32534#35793#22120
  ClientHeight = 460
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 740
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnSave: TButton
      Left = 106
      Top = 9
      Width = 75
      Height = 25
      Caption = #20445#23384#33050#26412
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnCompile: TButton
      Left = 9
      Top = 9
      Width = 75
      Height = 25
      Caption = #32534#35793
      TabOrder = 1
      OnClick = btnCompileClick
    end
  end
end
