object frmDeDateTime: TfrmDeDateTime
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeDateTime'#23646#24615
  ClientHeight = 338
  ClientWidth = 307
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
    Width = 307
    Height = 175
    Align = alTop
    TabOrder = 0
    DesignSize = (
      307
      175)
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
      Top = 97
      Width = 12
      Height = 13
      Caption = #20540
    end
    object lbl3: TLabel
      Left = 23
      Top = 41
      Width = 24
      Height = 13
      Caption = #36793#26694
    end
    object lbl7: TLabel
      Left = 93
      Top = 156
      Width = 204
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
      ExplicitTop = 112
    end
    object lbl4: TLabel
      Left = 23
      Top = 69
      Width = 24
      Height = 13
      Caption = #26684#24335
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
    object edtText: TEdit
      Left = 41
      Top = 94
      Width = 241
      Height = 21
      ReadOnly = True
      TabOrder = 3
    end
    object chkBorderTop: TCheckBox
      Left = 64
      Top = 40
      Width = 39
      Height = 17
      Caption = #19978
      TabOrder = 4
    end
    object chkBorderBottom: TCheckBox
      Left = 122
      Top = 40
      Width = 37
      Height = 17
      Caption = #19979
      TabOrder = 5
    end
    object chkBorderLeft: TCheckBox
      Left = 186
      Top = 40
      Width = 40
      Height = 17
      Caption = #24038
      TabOrder = 6
    end
    object chkBorderRight: TCheckBox
      Left = 250
      Top = 40
      Width = 32
      Height = 17
      Caption = #21491
      TabOrder = 7
    end
    object btnAddProp: TButton
      Left = 12
      Top = 147
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #26032#22686#23646#24615
      TabOrder = 8
      OnClick = btnAddPropClick
    end
    object chkPrintOnlyText: TCheckBox
      Left = 107
      Top = 121
      Width = 176
      Height = 17
      Caption = #25171#21360#26102#20165#25171#21360#25991#26412#19981#25171#21360#36793#26694
      TabOrder = 9
    end
    object cbbDTFormat: TComboBox
      Left = 53
      Top = 66
      Width = 229
      Height = 21
      ItemIndex = 1
      TabOrder = 10
      Text = 'YYYY-MM-DD'
      Items.Strings = (
        'YYYY-MM-DD HH:mm:SS'
        'YYYY-MM-DD'
        'HH:mm:SS'
        'YYYY'#24180'MM'#26376'DD'#26085
        'HH'#26102'mm'#20998)
    end
    object chkDeleteAllow: TCheckBox
      Left = 23
      Top = 121
      Width = 80
      Height = 17
      Caption = #20801#35768#21024#38500
      TabOrder = 11
    end
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 175
    Width = 307
    Height = 118
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 1
  end
  object btnSave: TButton
    Left = 110
    Top = 303
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 2
    OnClick = btnSaveClick
  end
end
