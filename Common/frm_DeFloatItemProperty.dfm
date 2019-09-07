object frmDeFloatItemProperty: TfrmDeFloatItemProperty
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #28014#21160#23545#35937#23646#24615#65288#31532#19968#21015#26080#20540#26102#19981#20250#23384#20648#65289
  ClientHeight = 180
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object sgdProperty: TStringGrid
    Left = 0
    Top = 41
    Width = 357
    Height = 94
    Align = alTop
    ColCount = 2
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
  end
  object btnAddProperty: TButton
    Left = 104
    Top = 146
    Width = 53
    Height = 25
    Caption = #28155'  '#21152
    TabOrder = 1
    OnClick = btnAddPropertyClick
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 0
    Width = 357
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Right = 20
    TabOrder = 2
    ExplicitWidth = 396
    object lbl1: TLabel
      Left = 19
      Top = 14
      Width = 12
      Height = 13
      Caption = #23485
    end
    object lbl2: TLabel
      Left = 132
      Top = 14
      Width = 12
      Height = 13
      Caption = #39640
    end
    object edtWidth: TEdit
      Left = 37
      Top = 11
      Width = 80
      Height = 21
      TabOrder = 0
    end
    object edtHeight: TEdit
      Left = 150
      Top = 11
      Width = 80
      Height = 21
      TabOrder = 1
    end
  end
  object btnSave: TButton
    Left = 181
    Top = 146
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 3
    OnClick = btnSaveClick
  end
end
