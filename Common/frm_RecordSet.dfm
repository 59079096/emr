object frmRecordSet: TfrmRecordSet
  Left = 0
  Top = 0
  Caption = #30149#21382#38598#21512
  ClientHeight = 608
  ClientWidth = 1264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TCFSplitter
    Left = 250
    Top = 0
    Width = 6
    Height = 608
    Cursor = crHSplit
    BorderVisible = False
    Alpha = 255
    Align = alLeft
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 608
    Align = alLeft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object sgdRecord: TCFGrid
      Left = 0
      Top = 0
      Width = 250
      Height = 408
      BorderVisible = False
      Alpha = 255
      Align = alClient
      Text = 'sgdRecord'
      DefaultColWidth = 70
      RowHeight = 20
      RowCount = 1
      ColCount = 5
      Options = [cgoColSizing]
      ReadOnly = False
    end
    object pnl2: TPanel
      Left = 0
      Top = 408
      Width = 250
      Height = 200
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object lbl1: TLabel
        Left = 11
        Top = 92
        Width = 48
        Height = 13
        Caption = #36215#22987#39029#30721
      end
      object lbl2: TLabel
        Left = 11
        Top = 121
        Width = 48
        Height = 13
        Caption = #39029#30721#26684#24335
      end
      object btnShow: TButton
        Left = 78
        Top = 157
        Width = 75
        Height = 25
        Caption = #26174#31034
        TabOrder = 0
        OnClick = btnShowClick
      end
      object edtPageNo: TEdit
        Left = 68
        Top = 89
        Width = 40
        Height = 21
        TabOrder = 1
        Text = '1'
      end
      object chkPageBlankTip: TCheckBox
        Left = 11
        Top = 37
        Width = 182
        Height = 17
        Caption = #21478#36215#39029#26102#19978#19968#39029#28155#21152#32467#26463#35821#21477
        TabOrder = 2
      end
      object edtPageBlankTip: TEdit
        Left = 32
        Top = 58
        Width = 212
        Height = 21
        TabOrder = 3
        Text = '--------'#26412#39029#20197#19979#20869#23481#31354#30333'--------'
      end
      object edtPageNoFmt: TEdit
        Left = 68
        Top = 118
        Width = 85
        Height = 21
        Hint = #31532#19968#20010'%d'#34920#31034#31532#20960#39029#65292#31532#20108#20010'%d'#34920#31034#20849#20960#39029
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object chkShowTrace: TCheckBox
        Left = 11
        Top = 10
        Width = 78
        Height = 17
        Caption = #26174#31034#30165#36857
        TabOrder = 5
      end
    end
  end
end
