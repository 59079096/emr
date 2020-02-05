object frmDeFloatBarCode: TfrmDeFloatBarCode
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #28014#21160#26465#30721
  ClientHeight = 281
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    312
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object sgdProperty: TStringGrid
    Left = 0
    Top = 118
    Width = 312
    Height = 118
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
    ExplicitTop = 66
    ExplicitWidth = 357
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 0
    Width = 312
    Height = 118
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 1
    DesignSize = (
      312
      118)
    object lbl1: TLabel
      Left = 131
      Top = 10
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 218
      Top = 10
      Width = 12
      Height = 13
      Caption = #39640
    end
    object lbl7: TLabel
      Left = 102
      Top = 100
      Width = 204
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
      ExplicitTop = 48
    end
    object lbl9: TLabel
      Left = 19
      Top = 37
      Width = 12
      Height = 13
      Caption = #20540
    end
    object lbl3: TLabel
      Left = 206
      Top = 38
      Width = 24
      Height = 13
      Caption = #32447#23485
    end
    object edtWidth: TEdit
      Left = 149
      Top = 7
      Width = 54
      Height = 21
      TabOrder = 0
    end
    object edtHeight: TEdit
      Left = 236
      Top = 7
      Width = 54
      Height = 21
      TabOrder = 1
    end
    object chkDeleteAllow: TCheckBox
      Left = 19
      Top = 64
      Width = 80
      Height = 17
      Caption = #20801#35768#21024#38500
      TabOrder = 2
    end
    object btnAddProperty: TButton
      Left = 19
      Top = 90
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #26032#22686#23646#24615
      TabOrder = 3
      OnClick = btnAddPropertyClick
      ExplicitTop = 38
    end
    object edtText: TEdit
      Left = 37
      Top = 34
      Width = 154
      Height = 21
      TabOrder = 4
    end
    object edtPenWidth: TEdit
      Left = 236
      Top = 34
      Width = 54
      Height = 21
      TabOrder = 5
    end
    object chkAutoSize: TCheckBox
      Left = 19
      Top = 8
      Width = 97
      Height = 17
      Caption = #33258#21160#35745#31639#23485#39640
      TabOrder = 6
      OnClick = chkAutoSizeClick
    end
    object chkShowText: TCheckBox
      Left = 105
      Top = 64
      Width = 80
      Height = 17
      Caption = #26174#31034#26465#30721#20540
      TabOrder = 7
    end
  end
  object btnSave: TButton
    Left = 116
    Top = 248
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #20445#23384
    TabOrder = 2
    OnClick = btnSaveClick
    ExplicitTop = 253
  end
end
