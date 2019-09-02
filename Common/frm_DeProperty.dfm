object frmDeProperty: TfrmDeProperty
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #23646#24615
  ClientHeight = 345
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
    Top = 280
    Width = 144
    Height = 13
    Caption = #34892#20013#31532#19968#21015#20026#31354#21017#23646#24615#26080#25928
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnAdd: TButton
    Left = 75
    Top = 307
    Width = 65
    Height = 25
    Caption = #28155'  '#21152
    TabOrder = 0
    OnClick = btnAddClick
  end
  object btnSave: TButton
    Left = 245
    Top = 307
    Width = 65
    Height = 25
    Caption = #30830'  '#23450
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 36
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
    Top = 307
    Width = 65
    Height = 25
    Caption = #21024'  '#38500
    TabOrder = 3
    OnClick = btnDelClick
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object chkCanEdit: TCheckBox
      Left = 13
      Top = 8
      Width = 91
      Height = 17
      Caption = #20801#35768#20462#25913#20869#23481
      TabOrder = 0
    end
    object chkCanCopy: TCheckBox
      Left = 119
      Top = 8
      Width = 66
      Height = 17
      Caption = #20801#35768#22797#21046
      TabOrder = 1
    end
  end
end
