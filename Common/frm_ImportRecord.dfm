object frmImportRecord: TfrmImportRecord
  Left = 0
  Top = 0
  Caption = 'frmImportRecord'
  ClientHeight = 434
  ClientWidth = 760
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
    Width = 760
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnImportAll: TButton
      Left = 48
      Top = 8
      Width = 75
      Height = 25
      Caption = #23548#20837#20840#37096
      TabOrder = 0
      OnClick = btnImportAllClick
    end
    object btnImportSelect: TButton
      Left = 152
      Top = 8
      Width = 97
      Height = 25
      Caption = #23548#20837#36873#20013#20869#23481
      TabOrder = 1
      OnClick = btnImportSelectClick
    end
  end
end
