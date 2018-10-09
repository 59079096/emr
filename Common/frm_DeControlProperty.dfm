object frmDeControlProperty: TfrmDeControlProperty
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'ControlItem'#23646#24615
  ClientHeight = 653
  ClientWidth = 582
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
  object pnlSize: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 314
    object lbl1: TLabel
      Left = 35
      Top = 46
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 148
      Top = 46
      Width = 12
      Height = 13
      Caption = #39640
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
      Left = 53
      Top = 43
      Width = 80
      Height = 21
      TabOrder = 1
    end
    object edtHeight: TEdit
      Left = 173
      Top = 43
      Width = 80
      Height = 21
      TabOrder = 2
    end
  end
  object pnlBorder: TPanel
    Left = 0
    Top = 189
    Width = 582
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 314
    object lbl3: TLabel
      Left = 23
      Top = 3
      Width = 24
      Height = 13
      Caption = #36793#26694
    end
    object chkBorderTop: TCheckBox
      Left = 35
      Top = 23
      Width = 39
      Height = 17
      Caption = #19978
      TabOrder = 0
    end
    object chkBorderLeft: TCheckBox
      Left = 157
      Top = 23
      Width = 40
      Height = 17
      Caption = #24038
      TabOrder = 1
    end
    object chkBorderRight: TCheckBox
      Left = 221
      Top = 23
      Width = 32
      Height = 17
      Caption = #21491
      TabOrder = 2
    end
    object chkBorderBottom: TCheckBox
      Left = 93
      Top = 23
      Width = 37
      Height = 17
      Caption = #19979
      TabOrder = 3
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 612
    Width = 582
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 314
    object btnOk: TButton
      Left = 221
      Top = 6
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
  object pnlCombobox: TPanel
    Left = 0
    Top = 239
    Width = 582
    Height = 166
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 3
    ExplicitWidth = 314
    object lbl5: TLabel
      Left = 27
      Top = 128
      Width = 12
      Height = 13
      Caption = #20540
    end
    object lbl7: TLabel
      Left = 281
      Top = 128
      Width = 232
      Height = 13
      Caption = 'DeCombobox'#23646#24615#65288#31532#19968#21015#26080#20540#26102#19981#20250#23384#20648#65289
    end
    object edtValue: TEdit
      Left = 45
      Top = 125
      Width = 128
      Height = 21
      TabOrder = 0
    end
    object btnAdd: TButton
      Left = 179
      Top = 123
      Width = 25
      Height = 25
      Caption = #22686
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 239
      Top = 123
      Width = 25
      Height = 25
      Caption = #21024
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnSave: TButton
      Left = 209
      Top = 123
      Width = 25
      Height = 25
      Caption = #25913
      TabOrder = 3
      OnClick = btnSaveClick
    end
    object btnComboxAddProperty: TButton
      Left = 511
      Top = 123
      Width = 53
      Height = 25
      Caption = #28155'  '#21152
      TabOrder = 4
      OnClick = btnComboxAddPropertyClick
    end
    object pnl2: TPanel
      Left = 20
      Top = 0
      Width = 542
      Height = 117
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 5
      ExplicitWidth = 274
      object lstCombobox: TListBox
        Left = 0
        Top = 0
        Width = 253
        Height = 117
        Align = alLeft
        ItemHeight = 13
        TabOrder = 0
        OnClick = lstComboboxClick
      end
      object sgdCombobox: TStringGrid
        Left = 263
        Top = 0
        Width = 279
        Height = 117
        Align = alRight
        ColCount = 2
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
        TabOrder = 1
        ExplicitLeft = 261
      end
    end
  end
  object pnlDateTime: TPanel
    Left = 0
    Top = 571
    Width = 582
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitWidth = 314
    object lbl4: TLabel
      Left = 27
      Top = 13
      Width = 24
      Height = 13
      Caption = #26684#24335
    end
    object cbbDTFormat: TComboBox
      Left = 60
      Top = 10
      Width = 145
      Height = 21
      ItemIndex = 1
      TabOrder = 0
      Text = 'YYYY-MM-DD'
      Items.Strings = (
        'YYYY-MM-DD HH:mm:SS'
        'YYYY-MM-DD'
        'HH:mm:SS'
        'YYYY'#24180'MM'#26376'DD'#26085
        'HH'#26102'mm'#20998)
    end
  end
  object pnlRadioGroup: TPanel
    Left = 0
    Top = 405
    Width = 582
    Height = 166
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 5
    ExplicitWidth = 314
    object lbl6: TLabel
      Left = 27
      Top = 128
      Width = 12
      Height = 13
      Caption = #20540
    end
    object edtRadioValue: TEdit
      Left = 45
      Top = 125
      Width = 128
      Height = 21
      TabOrder = 0
    end
    object btnAddRadioItem: TButton
      Left = 179
      Top = 123
      Width = 25
      Height = 25
      Caption = #22686
      TabOrder = 1
      OnClick = btnAddRadioItemClick
    end
    object btnDeleteRadioItem: TButton
      Left = 239
      Top = 123
      Width = 25
      Height = 25
      Caption = #21024
      TabOrder = 2
      OnClick = btnDeleteRadioItemClick
    end
    object btnModRadioItem: TButton
      Left = 209
      Top = 123
      Width = 25
      Height = 25
      Caption = #25913
      TabOrder = 3
      OnClick = btnModRadioItemClick
    end
    object lstRadioItem: TListBox
      Left = 20
      Top = 0
      Width = 542
      Height = 119
      Align = alTop
      ItemHeight = 13
      TabOrder = 4
      OnClick = lstRadioItemClick
      ExplicitWidth = 274
    end
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 73
    Width = 582
    Height = 116
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnl2'
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 6
    ExplicitWidth = 314
    object lbl8: TLabel
      Left = 20
      Top = 92
      Width = 199
      Height = 13
      Caption = 'DeEdit'#23646#24615#65288#31532#19968#21015#26080#20540#26102#19981#20250#23384#20648#65289
    end
    object sgdEdit: TStringGrid
      Left = 20
      Top = 0
      Width = 542
      Height = 86
      Align = alTop
      ColCount = 2
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      TabOrder = 0
      ExplicitWidth = 274
    end
    object btnEditAddProperty: TButton
      Left = 218
      Top = 92
      Width = 53
      Height = 25
      Caption = #28155'  '#21152
      TabOrder = 1
      OnClick = btnEditAddPropertyClick
    end
  end
end
