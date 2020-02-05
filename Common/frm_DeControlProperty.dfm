object frmDeControlProperty: TfrmDeControlProperty
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'ControlItem'#23646#24615
  ClientHeight = 348
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
  object pnlSize: TPanel
    Left = 0
    Top = 0
    Width = 314
    Height = 100
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
    object lbl9: TLabel
      Left = 23
      Top = 76
      Width = 24
      Height = 13
      Caption = #25991#26412
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
    object edtText: TEdit
      Left = 53
      Top = 72
      Width = 200
      Height = 21
      TabOrder = 3
    end
  end
  object pnlBorder: TPanel
    Left = 0
    Top = 216
    Width = 314
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
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
    Top = 307
    Width = 314
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 473
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
  object pnlDateTime: TPanel
    Left = 0
    Top = 266
    Width = 314
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 432
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
  object pnlEdit: TPanel
    Left = 0
    Top = 100
    Width = 314
    Height = 116
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnl2'
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 4
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
      Width = 274
      Height = 86
      Align = alTop
      ColCount = 2
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      TabOrder = 0
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
