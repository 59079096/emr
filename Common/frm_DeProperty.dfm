object frmDeProperty: TfrmDeProperty
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #23646#24615
  ClientHeight = 312
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl8: TLabel
    Left = 8
    Top = 247
    Width = 241
    Height = 13
    Caption = #34892#20013#31532#19968#21015#20026#31354#21017#23646#24615#26080#25928#65292#30830#23450#21518#19981#22788#29702
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnAdd: TButton
    Left = 75
    Top = 272
    Width = 65
    Height = 25
    Caption = #28155'  '#21152
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnSave: TButton
    Left = 245
    Top = 272
    Width = 65
    Height = 25
    Caption = #30830'  '#23450
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 0
    Width = 344
    Height = 233
    Align = alTop
    ColCount = 2
    DefaultColWidth = 80
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
    TabOrder = 2
  end
  object btnDel: TButton
    Left = 151
    Top = 272
    Width = 65
    Height = 25
    Caption = #21024'  '#38500
    TabOrder = 3
    OnClick = btnDelClick
  end
end
