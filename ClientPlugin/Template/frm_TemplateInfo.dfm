object frmTemplateInfo: TfrmTemplateInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #27169#26495#20449#24687
  ClientHeight = 118
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 32
    Top = 24
    Width = 24
    Height = 13
    Caption = #21517#31216
  end
  object edtTName: TEdit
    Left = 72
    Top = 21
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object btnSave: TButton
    Left = 136
    Top = 64
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 1
    OnClick = btnSaveClick
  end
end
