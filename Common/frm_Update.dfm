object frmUpdate: TfrmUpdate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21319#32423
  ClientHeight = 355
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object sgdFiles: TStringGrid
    Left = 0
    Top = 0
    Width = 537
    Height = 209
    Align = alTop
    ColCount = 4
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect]
    TabOrder = 0
  end
  object mmo: TMemo
    Left = 0
    Top = 209
    Width = 537
    Height = 89
    Align = alTop
    ReadOnly = True
    TabOrder = 1
  end
  object btnDownLoad: TButton
    Left = 224
    Top = 315
    Width = 75
    Height = 25
    Caption = #21319#32423
    TabOrder = 2
    OnClick = btnDownLoadClick
  end
  object chkBackup: TCheckBox
    Left = 101
    Top = 319
    Width = 116
    Height = 17
    Caption = #21319#32423#21069#22791#20221#21407#25991#20214
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
