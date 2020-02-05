object frmDeRadioGroup: TfrmDeRadioGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeRadioGroup'#23646#24615
  ClientHeight = 409
  ClientWidth = 339
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
    Width = 339
    Height = 100
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 347
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
    object lbl7: TLabel
      Left = 91
      Top = 82
      Width = 204
      Height = 13
      Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
    end
    object lbl3: TLabel
      Left = 23
      Top = 44
      Width = 73
      Height = 13
      Caption = 'RadioItem'#26679#24335
    end
    object chkAutoSize: TCheckBox
      Left = 23
      Top = 11
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
    object btnAddProp: TButton
      Left = 10
      Top = 72
      Width = 75
      Height = 25
      Caption = #26032#22686#23646#24615
      TabOrder = 3
      OnClick = btnAddPropClick
    end
    object cbbStyle: TComboBox
      Left = 102
      Top = 40
      Width = 88
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'RadioButton'
      Items.Strings = (
        'RadioButton'
        'CheckBox')
    end
    object chkMulSelect: TCheckBox
      Left = 199
      Top = 42
      Width = 53
      Height = 17
      Caption = #22810#36873
      TabOrder = 5
      OnClick = chkAutoSizeClick
    end
    object chkDeleteAllow: TCheckBox
      Left = 251
      Top = 42
      Width = 80
      Height = 17
      Caption = #20801#35768#21024#38500
      TabOrder = 6
    end
  end
  object pnlCombobox: TPanel
    Left = 0
    Top = 218
    Width = 339
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 1
    ExplicitWidth = 347
    object btnAddItem: TButton
      Left = 10
      Top = 3
      Width = 75
      Height = 25
      Caption = #26032#22686#39033
      TabOrder = 0
      OnClick = btnAddItemClick
    end
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 100
    Width = 339
    Height = 118
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 2
    ExplicitWidth = 347
  end
  object sgdItem: TStringGrid
    Left = 0
    Top = 250
    Width = 339
    Height = 118
    Align = alTop
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 3
    ExplicitWidth = 347
  end
  object btnSave: TButton
    Left = 126
    Top = 376
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 4
    OnClick = btnSaveClick
  end
end
