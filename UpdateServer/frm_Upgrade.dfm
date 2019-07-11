object frmUpgrade: TfrmUpgrade
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #26032#24314#21319#32423
  ClientHeight = 420
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnl2: TPanel
    Left = 0
    Top = 273
    Width = 600
    Height = 147
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 274
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
      Left = 264
      Top = 108
      Width = 75
      Height = 25
      Caption = #25552'  '#20132
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
      Width = 527
      Height = 96
      MaxLength = 1024
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object sgdFiles: TStringGrid
    Left = 0
    Top = 0
    Width = 600
    Height = 273
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing]
    PopupMenu = pmFile
    TabOrder = 1
    ExplicitWidth = 615
    ExplicitHeight = 289
  end
  object pmFile: TPopupMenu
    Left = 48
    Top = 72
    object mniAdd: TMenuItem
      Caption = #28155#21152
      OnClick = mniAddClick
    end
    object mniDelete: TMenuItem
      Caption = #21024#38500
      OnClick = mniDeleteClick
    end
  end
end
