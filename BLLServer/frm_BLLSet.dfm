object frmBLLSet: TfrmBLLSet
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #19994#21153#33050#26412#37197#32622
  ClientHeight = 629
  ClientWidth = 974
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 337
    Top = 0
    Height = 629
    ExplicitLeft = 424
    ExplicitTop = 80
    ExplicitHeight = 100
  end
  object sgdbll: TStringGrid
    Left = 0
    Top = 0
    Width = 337
    Height = 629
    Align = alLeft
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect]
    PopupMenu = pmBLL
    TabOrder = 0
    OnSelectCell = sgdbllSelectCell
    ColWidths = (
      29
      48
      154
      35
      44)
  end
  object pnlScript: TPanel
    Left = 340
    Top = 0
    Width = 634
    Height = 629
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object spl2: TSplitter
      Left = 0
      Top = 551
      Width = 634
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 165
      ExplicitWidth = 237
    end
    object pnl2: TPanel
      Left = 0
      Top = 0
      Width = 634
      Height = 165
      Align = alTop
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 0
      object lbl1: TLabel
        Left = 14
        Top = 10
        Width = 35
        Height = 13
        Caption = #19994#21153'ID'
      end
      object lbl2: TLabel
        Left = 25
        Top = 37
        Width = 24
        Height = 13
        Caption = #21517#31216
      end
      object lbl3: TLabel
        Left = 134
        Top = 10
        Width = 36
        Height = 13
        Caption = #25968#25454#24211
      end
      object lbl4: TLabel
        Left = 30
        Top = 61
        Width = 19
        Height = 13
        Caption = 'SQL'
      end
      object lbl5: TLabel
        Left = 14
        Top = 144
        Width = 336
        Height = 13
        Caption = #35831#22312#19979#38754#36755#20837#19994#21153#33050#26412#65292#23454#38469#24212#29992#20013#33050#26412#19981#20026#31354#26102#20248#20808#25191#34892#33050#26412
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lbl6: TLabel
        Left = 302
        Top = 10
        Width = 24
        Height = 13
        Caption = #29256#26412
      end
      object edtBLLID: TEdit
        Left = 55
        Top = 7
        Width = 64
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object edtName: TEdit
        Left = 55
        Top = 34
        Width = 560
        Height = 21
        MaxLength = 50
        TabOrder = 1
      end
      object cbbDB: TComboBox
        Left = 176
        Top = 7
        Width = 110
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = #40664#35748
        Items.Strings = (
          #40664#35748)
      end
      object edtVer: TEdit
        Left = 332
        Top = 7
        Width = 64
        Height = 21
        TabOrder = 3
        Text = '0'
      end
      object mmoSQL: TMemo
        Left = 55
        Top = 61
        Width = 560
        Height = 79
        ScrollBars = ssBoth
        TabOrder = 4
      end
      object btnSave: TButton
        Left = 414
        Top = 3
        Width = 75
        Height = 25
        Caption = #20445#23384#20449#24687
        TabOrder = 5
      end
    end
    object pnlMessage: TPanel
      Left = 0
      Top = 554
      Width = 634
      Height = 75
      Align = alBottom
      BevelKind = bkFlat
      BevelOuter = bvNone
      Caption = 'pnlMessage'
      TabOrder = 1
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 630
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblMsg: TLabel
          Left = 5
          Top = 4
          Width = 47
          Height = 13
          Caption = 'Messages'
        end
      end
      object lstMessage: TListBox
        Left = 0
        Top = 24
        Width = 630
        Height = 47
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = lstMessageDblClick
      end
    end
  end
  object pmBLL: TPopupMenu
    Left = 96
    Top = 144
    object mniNewBLL: TMenuItem
      Caption = #26032#24314
      OnClick = mniNewBLLClick
    end
    object mniDelBLL: TMenuItem
      Caption = #21024#38500
    end
  end
end
