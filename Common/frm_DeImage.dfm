object frmDeImage: TfrmDeImage
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeImage'#23646#24615
  ClientHeight = 232
  ClientWidth = 310
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
    Top = 0
    Width = 310
    Height = 68
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 347
    object lbl1: TLabel
      Left = 12
      Top = 13
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 94
      Top = 13
      Width = 12
      Height = 13
      Caption = #39640
    end
    object lbl7: TLabel
      Left = 93
      Top = 46
      Width = 204
      Height = 13
      Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
    end
    object edtWidth: TEdit
      Left = 29
      Top = 10
      Width = 54
      Height = 21
      TabOrder = 0
    end
    object edtHeight: TEdit
      Left = 112
      Top = 10
      Width = 54
      Height = 21
      TabOrder = 1
    end
    object btnAddProp: TButton
      Left = 12
      Top = 37
      Width = 75
      Height = 25
      Caption = #26032#22686#23646#24615
      TabOrder = 2
      OnClick = btnAddPropClick
    end
    object chkDeleteAllow: TCheckBox
      Left = 191
      Top = 12
      Width = 80
      Height = 17
      Caption = #20801#35768#21024#38500
      TabOrder = 3
    end
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 68
    Width = 310
    Height = 118
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 1
    ExplicitWidth = 347
  end
  object btnSave: TButton
    Left = 112
    Top = 199
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 2
    OnClick = btnSaveClick
  end
end
