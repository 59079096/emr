object frmDomainItem: TfrmDomainItem
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20540#22495#36873#39033
  ClientHeight = 149
  ClientWidth = 361
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
    Left = 22
    Top = 24
    Width = 24
    Height = 13
    Caption = #21517#31216
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 22
    Top = 64
    Width = 24
    Height = 13
    Caption = #32534#30721
  end
  object lbl3: TLabel
    Left = 190
    Top = 24
    Width = 24
    Height = 13
    Caption = #25340#38899
  end
  object edtName: TEdit
    Left = 52
    Top = 21
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtCode: TEdit
    Left = 52
    Top = 61
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edtPY: TEdit
    Left = 220
    Top = 21
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object btnSave: TButton
    Left = 139
    Top = 104
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 3
    OnClick = btnSaveClick
  end
end
