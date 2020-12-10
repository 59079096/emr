object frmDeProperty: TfrmDeProperty
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #23646#24615
  ClientHeight = 275
  ClientWidth = 305
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
  object lbl7: TLabel
    Left = 88
    Top = 218
    Width = 204
    Height = 13
    Caption = #23646#24615#65288#20445#23384#26102#20002#24323#31532#19968#21015#20026#31354#30340#23646#24615#65289
  end
  object btnSave: TButton
    Left = 110
    Top = 242
    Width = 75
    Height = 25
    Caption = #20445#23384
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object sgdProperty: TStringGrid
    Left = 0
    Top = 59
    Width = 305
    Height = 149
    Align = alTop
    ColCount = 2
    DefaultColWidth = 80
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
    TabOrder = 1
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 305
    Height = 59
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object chkCanEdit: TCheckBox
      Left = 13
      Top = 8
      Width = 91
      Height = 17
      Caption = #20801#35768#20462#25913#20869#23481
      TabOrder = 0
    end
    object chkCanCopy: TCheckBox
      Left = 13
      Top = 32
      Width = 66
      Height = 17
      Caption = #20801#35768#22797#21046
      TabOrder = 1
    end
    object chkDeleteAllow: TCheckBox
      Left = 110
      Top = 32
      Width = 80
      Height = 17
      Caption = #20801#35768#21024#38500
      TabOrder = 2
    end
    object chkAllocOnly: TCheckBox
      Left = 110
      Top = 8
      Width = 172
      Height = 17
      Caption = #20462#25913#26102#25972#20307#36171#20540#19981#20801#35768#37096#20998#20462#25913
      TabOrder = 3
    end
  end
  object btnAdd: TButton
    Left = 5
    Top = 213
    Width = 75
    Height = 25
    Caption = #26032#22686#23646#24615
    TabOrder = 3
    OnClick = btnAddClick
  end
end
