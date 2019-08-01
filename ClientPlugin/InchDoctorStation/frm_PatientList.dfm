object frmPatientList: TfrmPatientList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #24739#32773#21015#34920
  ClientHeight = 547
  ClientWidth = 805
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
  object sgdPatient: TStringGrid
    Left = 0
    Top = 24
    Width = 805
    Height = 523
    Hint = '11111'
    Align = alClient
    BorderStyle = bsNone
    ColCount = 12
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnDblClick = sgdPatientDblClick
    OnMouseMove = sgdPatientMouseMove
    ColWidths = (
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64)
    RowHeights = (
      24
      24)
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 805
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object lbl4: TLabel
      Left = 5
      Top = 5
      Width = 24
      Height = 13
      Caption = #26597#25214
    end
    object lblSeachInfo: TLabel
      Left = 180
      Top = 5
      Width = 24
      Height = 13
      Caption = #40664#35748
    end
    object edtSearch: TCFPopupEdit
      Left = 34
      Top = 1
      Width = 140
      Height = 22
      BorderVisible = True
      Alpha = 255
      Text = ''
      ButtonStyle = cbsLookUp
      ReadOnly = False
      PopupControl = pnlSearch
    end
  end
  object pnlSearch: TPanel
    Left = 13
    Top = 82
    Width = 235
    Height = 175
    BevelKind = bkFlat
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    Visible = False
    object lbl1: TLabel
      Left = 8
      Top = 12
      Width = 24
      Height = 13
      Caption = #31185#23460
    end
    object lbl2: TLabel
      Left = 8
      Top = 47
      Width = 24
      Height = 13
      Caption = #29366#24577
    end
    object lbl3: TLabel
      Left = 8
      Top = 116
      Width = 24
      Height = 13
      Caption = #26465#20214
    end
    object lbl5: TLabel
      Left = 8
      Top = 81
      Width = 24
      Height = 13
      Caption = #26102#38388
    end
    object gdtDepts: TCFGridEdit
      Left = 40
      Top = 9
      Width = 186
      Height = 22
      BorderVisible = True
      Alpha = 255
      Text = ''
      ButtonStyle = cbsLookUp
      ReadOnly = False
      DropDownCount = 7
      Fields = <
        item
          Title = #31185#23460#21517#31216
          FieldName = 'name'
          DataType = ftString
          Width = 100
        end
        item
          Title = #31185#23460'ID'
          FieldName = 'id'
          DataType = ftInteger
          Width = 32
        end>
      KeyField = 'id'
      ValueField = 'name'
    end
    object edtSeachValue: TCFEdit
      Left = 40
      Top = 113
      Width = 186
      Height = 22
      Cursor = crIBeam
      BorderVisible = True
      Alpha = 255
      CanSelect = True
      HelpText = #22995#21517#12289#20303#38498#21495#12289#24202#21495#20854#20013#20043#19968
    end
    object btnSearch: TCFButton
      Left = 80
      Top = 142
      Width = 75
      Height = 25
      BorderVisible = True
      Alpha = 255
      Text = #30830'  '#23450
      Caption = #30830'  '#23450
      OnClick = btnSearchClick
    end
    object cbbState: TCFCombobox
      Left = 40
      Top = 43
      Width = 186
      Height = 22
      Cursor = crIBeam
      BorderVisible = True
      Alpha = 255
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Text = #22312#38498
      CanSelect = True
      ItemHeight = 16
      Items.Strings = (
        #22312#38498
        #20986#38498
        #22312#38498#12289#20986#38498)
      DropDownFont.Charset = DEFAULT_CHARSET
      DropDownFont.Color = clWindowText
      DropDownFont.Height = -11
      DropDownFont.Name = 'Tahoma'
      DropDownFont.Style = []
      ItemIndex = 0
      Style = csDropDownList
      ZoomSelected = False
    end
    object dtr1: TCFDateRang
      Left = 40
      Top = 77
      Width = 186
      Height = 22
      BorderVisible = True
      Alpha = 255
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      Text = 'dtr1'
      Format = 'YYYY-MM-DD'
      ReadOnly = False
      ButtonVisible = True
    end
  end
  object pnlOverView: TPanel
    Left = 13
    Top = 263
    Width = 593
    Height = 179
    BevelKind = bkFlat
    BevelOuter = bvNone
    ParentBackground = False
    PopupMenu = pm1
    TabOrder = 3
    object lbl7: TLabel
      Left = 176
      Top = 111
      Width = 159
      Height = 13
      Caption = #20170#26085#38656#35201#20070#20889'                         '#25110
    end
    object lbl8: TLabel
      Left = 16
      Top = 111
      Width = 159
      Height = 13
      Caption = #20170#26085#26377#25928'                '#65292'             '#65292
    end
    object lbl9: TLabel
      Left = 249
      Top = 111
      Width = 72
      Height = 13
      Caption = #26085#24120#30149#31243#35760#24405
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lbl10: TLabel
      Left = 337
      Top = 111
      Width = 48
      Height = 13
      Caption = #38454#27573#23567#32467
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lbl12: TLabel
      Left = 16
      Top = 135
      Width = 195
      Height = 13
      Caption = #26126#26085#25163#26415#20294#26410#21457#29616#26415#21069#30149#31243'                 '
    end
    object lbl6: TLabel
      Left = 64
      Top = 111
      Width = 48
      Height = 13
      Caption = #21307#22065'12'#26465
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lbl13: TLabel
      Left = 121
      Top = 111
      Width = 42
      Height = 13
      Caption = #26032#24320'4'#26465
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lbl14: TLabel
      Left = 326
      Top = 14
      Width = 60
      Height = 13
      Caption = #26410#31614#21517#30149#21382
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbl11: TLabel
      Left = 161
      Top = 135
      Width = 48
      Height = 13
      Caption = #29616#22312#20070#20889
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
    object lblPatBedNo: TLabel
      Left = 8
      Top = 8
      Width = 83
      Height = 39
      Caption = '123'#24202
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPatName: TLabel
      Left = 8
      Top = 53
      Width = 128
      Height = 39
      Caption = #24739#32773#22995#21517
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btn1: TButton
      Left = 240
      Top = 130
      Width = 75
      Height = 25
      Caption = #26816#26597
      TabOrder = 0
    end
    object btn2: TButton
      Left = 321
      Top = 130
      Width = 75
      Height = 25
      Caption = #26816#26816
      TabOrder = 1
    end
  end
  object pm1: TPopupMenu
    Left = 528
    Top = 111
    object mniN1111: TMenuItem
      Caption = '111'
    end
    object mniN22221: TMenuItem
      Caption = '2222'
    end
    object mniN3331: TMenuItem
      Caption = '333'
    end
  end
end
