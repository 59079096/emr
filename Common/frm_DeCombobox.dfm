object frmDeCombobox: TfrmDeCombobox
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeCombobox'#23646#24615
  ClientHeight = 448
  ClientWidth = 322
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
    Width = 322
    Height = 131
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 728
    object lbl1: TLabel
      Left = 131
      Top = 13
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 213
      Top = 13
      Width = 12
      Height = 13
      Caption = #39640
    end
    object lbl9: TLabel
      Left = 23
      Top = 48
      Width = 24
      Height = 13
      Caption = #25991#26412
    end
    object lbl3: TLabel
      Left = 23
      Top = 80
      Width = 24
      Height = 13
      Caption = #36793#26694
    end
    object lbl7: TLabel
      Left = 10
      Top = 112
      Width = 204
      Height = 13
      Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
    end
    object chkAutoSize: TCheckBox
      Left = 23
      Top = 12
      Width = 97
      Height = 17
      Caption = #33258#21160#35745#31639#23485#39640
      TabOrder = 0
      OnClick = chkAutoSizeClick
    end
    object edtWidth: TEdit
      Left = 148
      Top = 10
      Width = 54
      Height = 21
      TabOrder = 1
    end
    object edtHeight: TEdit
      Left = 231
      Top = 10
      Width = 54
      Height = 21
      TabOrder = 2
    end
    object edtText: TEdit
      Left = 53
      Top = 45
      Width = 232
      Height = 21
      TabOrder = 3
    end
    object chkBorderTop: TCheckBox
      Left = 64
      Top = 79
      Width = 39
      Height = 17
      Caption = #19978
      TabOrder = 4
    end
    object chkBorderBottom: TCheckBox
      Left = 122
      Top = 79
      Width = 37
      Height = 17
      Caption = #19979
      TabOrder = 5
    end
    object chkBorderLeft: TCheckBox
      Left = 186
      Top = 79
      Width = 40
      Height = 17
      Caption = #24038
      TabOrder = 6
    end
    object chkBorderRight: TCheckBox
      Left = 250
      Top = 79
      Width = 32
      Height = 17
      Caption = #21491
      TabOrder = 7
    end
    object btnAddProp: TButton
      Left = 220
      Top = 105
      Width = 75
      Height = 25
      Caption = #26032#22686#23646#24615
      TabOrder = 8
      OnClick = btnAddPropClick
    end
  end
  object pnlCombobox: TPanel
    Left = 0
    Top = 249
    Width = 322
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 418
    ExplicitWidth = 728
    object chkSaveItem: TCheckBox
      Left = 10
      Top = 6
      Width = 192
      Height = 17
      Caption = #20445#23384#36873#39033#65288#20445#23384#26102#20002#24323#31354#36873#39033#65289
      TabOrder = 0
      OnClick = chkSaveItemClick
    end
    object btnAddItem: TButton
      Left = 220
      Top = 4
      Width = 75
      Height = 25
      Caption = #26032#22686#36873#39033
      TabOrder = 1
      OnClick = btnAddItemClick
    end
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 131
    Width = 322
    Height = 118
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 2
    ExplicitWidth = 728
  end
  object sgdItem: TStringGrid
    Left = 0
    Top = 281
    Width = 322
    Height = 118
    Align = alTop
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 3
    ExplicitLeft = -8
    ExplicitTop = 376
    ExplicitWidth = 728
  end
  object btnSave: TButton
    Left = 122
    Top = 411
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 4
    OnClick = btnSaveClick
  end
end
