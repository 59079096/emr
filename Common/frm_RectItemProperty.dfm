object frmRectItemProperty: TfrmRectItemProperty
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'RectlItem'#23646#24615
  ClientHeight = 98
  ClientWidth = 314
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
  object pnl1: TPanel
    Left = 0
    Top = 57
    Width = 314
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -8
    ExplicitTop = 80
    object btnSave: TButton
      Left = 117
      Top = 6
      Width = 75
      Height = 25
      Caption = #20445#23384
      TabOrder = 0
      OnClick = btnSaveClick
    end
  end
  object pnlSize: TPanel
    Left = 0
    Top = 0
    Width = 314
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lbl1: TLabel
      Left = 35
      Top = 19
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 154
      Top = 19
      Width = 12
      Height = 13
      Caption = #39640
    end
    object edtWidth: TEdit
      Left = 53
      Top = 16
      Width = 80
      Height = 21
      TabOrder = 0
    end
    object edtHeight: TEdit
      Left = 173
      Top = 16
      Width = 80
      Height = 21
      TabOrder = 1
    end
  end
end
