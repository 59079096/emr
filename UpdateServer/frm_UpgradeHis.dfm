object frmUpgradeHis: TfrmUpgradeHis
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21382#27425#21319#32423
  ClientHeight = 420
  ClientWidth = 772
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnl2: TPanel
    Left = 0
    Top = 273
    Width = 772
    Height = 147
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 274
    ExplicitWidth = 600
    object lbl1: TLabel
      Left = 16
      Top = 6
      Width = 36
      Height = 13
      Caption = #35828'    '#26126
    end
    object lblVersion: TLabel
      Left = 16
      Top = 113
      Width = 36
      Height = 13
      Caption = #29256#26412#21495
    end
    object btnOK: TButton
      Left = 256
      Top = 108
      Width = 145
      Height = 25
      Caption = #20445#23384#29256#26412#21495#21644#35828#26126
      TabOrder = 0
      OnClick = btnOKClick
    end
    object edtVersion: TEdit
      Left = 58
      Top = 110
      Width = 121
      Height = 21
      MaxLength = 10
      TabOrder = 1
    end
    object mmo: TMemo
      Left = 58
      Top = 6
      Width = 703
      Height = 96
      MaxLength = 1024
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 772
    Height = 273
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 184
    ExplicitTop = 120
    ExplicitWidth = 185
    ExplicitHeight = 41
    object sgdList: TStringGrid
      Left = 0
      Top = 0
      Width = 313
      Height = 273
      Align = alLeft
      ColCount = 3
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect]
      PopupMenu = pmList
      TabOrder = 0
      OnClick = sgdListClick
    end
    object sgdFiles: TStringGrid
      Left = 313
      Top = 0
      Width = 459
      Height = 273
      Align = alClient
      ColCount = 4
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect]
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitWidth = 615
      ExplicitHeight = 289
    end
  end
  object pmList: TPopupMenu
    Left = 48
    Top = 72
    object mniAdd: TMenuItem
      Caption = #28155#21152
    end
    object mniDelete: TMenuItem
      Caption = #21024#38500
    end
  end
end
