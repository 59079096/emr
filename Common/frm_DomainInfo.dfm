object frmDomainInfo: TfrmDomainInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20540#22495
  ClientHeight = 147
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 24
    Top = 16
    Width = 24
    Height = 13
    Caption = #32534#30721
  end
  object lbl2: TLabel
    Left = 24
    Top = 56
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
  object edtName: TEdit
    Left = 54
    Top = 53
    Width = 371
    Height = 21
    TabOrder = 0
  end
  object edtCode: TEdit
    Left = 54
    Top = 13
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object btnSave: TButton
    Left = 184
    Top = 104
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 2
    OnClick = btnSaveClick
  end
end
