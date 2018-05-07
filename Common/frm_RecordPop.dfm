object frmRecordPop: TfrmRecordPop
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = 'frmRecordPop'
  ClientHeight = 300
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblHint: TLabel
    Left = 0
    Top = 287
    Width = 259
    Height = 13
    Align = alBottom
    ExplicitWidth = 3
  end
  object pgPop: TPageControl
    Left = 0
    Top = 0
    Width = 259
    Height = 287
    ActivePage = tsDateTime
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    StyleElements = [seFont, seClient]
    object tsDomain: TTabSheet
      Caption = 'tsDomain'
      object sgdDomain: TStringGrid
        Left = 0
        Top = 30
        Width = 251
        Height = 226
        Align = alClient
        ColCount = 4
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
        TabOrder = 0
        ColWidths = (
          134
          43
          29
          26)
      end
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 251
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lbl2: TLabel
          Left = 12
          Top = 5
          Width = 24
          Height = 13
          Caption = #26816#32034
        end
        object edtSpliter: TEdit
          Left = 45
          Top = 2
          Width = 100
          Height = 21
          TabOrder = 0
        end
        object btnDomainOk: TButton
          Left = 170
          Top = 0
          Width = 75
          Height = 25
          Caption = #30830#23450
          TabOrder = 1
          OnClick = btnDomainOkClick
        end
      end
    end
    object tsNumber: TTabSheet
      Caption = 'tsNumber'
      ImageIndex = 1
      object pnl2: TPanel
        Left = 0
        Top = 0
        Width = 251
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object edtvalue: TButtonedEdit
          Left = 6
          Top = 4
          Width = 89
          Height = 21
          Alignment = taRightJustify
          AutoSelect = False
          BevelInner = bvNone
          BevelOuter = bvNone
          LeftButton.HotImageIndex = 8
          LeftButton.ImageIndex = 7
          LeftButton.Visible = True
          TabOrder = 0
          OnKeyDown = edtvalueKeyDown
        end
        object cbbUnit: TComboBox
          Left = 104
          Top = 4
          Width = 58
          Height = 21
          TabOrder = 1
          OnCloseUp = cbbUnitCloseUp
          OnSelect = cbbUnitSelect
        end
      end
      object chkhideunit: TCheckBox
        Left = 10
        Top = 30
        Width = 66
        Height = 17
        Caption = #38544#34255#21333#20301
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = #23435#20307
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object btn1: TButton
        Tag = 1
        Left = 5
        Top = 84
        Width = 38
        Height = 25
        Caption = '1'
        TabOrder = 2
        OnClick = btn1Click
      end
      object btn3: TButton
        Tag = 3
        Left = 93
        Top = 84
        Width = 38
        Height = 25
        Caption = '3'
        TabOrder = 3
        OnClick = btn1Click
      end
      object btn4: TButton
        Tag = 4
        Left = 5
        Top = 115
        Width = 38
        Height = 25
        Caption = '4'
        TabOrder = 4
        OnClick = btn1Click
      end
      object btn5: TButton
        Tag = 5
        Left = 49
        Top = 115
        Width = 38
        Height = 25
        Caption = '5'
        TabOrder = 5
        OnClick = btn1Click
      end
      object btn6: TButton
        Tag = 6
        Left = 93
        Top = 115
        Width = 38
        Height = 25
        Caption = '6'
        TabOrder = 6
        OnClick = btn1Click
      end
      object btn7: TButton
        Tag = 7
        Left = 5
        Top = 146
        Width = 38
        Height = 25
        Caption = '7'
        TabOrder = 7
        OnClick = btn1Click
      end
      object btn8: TButton
        Tag = 8
        Left = 49
        Top = 146
        Width = 38
        Height = 25
        Caption = '8'
        TabOrder = 8
        OnClick = btn1Click
      end
      object btn9: TButton
        Tag = 9
        Left = 93
        Top = 146
        Width = 38
        Height = 25
        Caption = '9'
        TabOrder = 9
        OnClick = btn1Click
      end
      object btn0: TButton
        Left = 5
        Top = 177
        Width = 38
        Height = 25
        Caption = '0'
        TabOrder = 10
        OnClick = btn1Click
      end
      object btnDiv: TButton
        Left = 137
        Top = 177
        Width = 38
        Height = 25
        Caption = '/'
        TabOrder = 11
        OnClick = btnAddClick
      end
      object btn2: TButton
        Tag = 2
        Left = 49
        Top = 84
        Width = 38
        Height = 25
        Caption = '2'
        TabOrder = 12
        OnClick = btn1Click
      end
      object btnAdd: TButton
        Left = 137
        Top = 84
        Width = 38
        Height = 25
        Caption = '+'
        TabOrder = 13
        OnClick = btnAddClick
      end
      object btnDec: TButton
        Left = 137
        Top = 115
        Width = 38
        Height = 25
        Caption = '-'
        TabOrder = 14
        OnClick = btnAddClick
      end
      object btnMul: TButton
        Left = 137
        Top = 146
        Width = 38
        Height = 25
        Caption = '*'
        TabOrder = 15
        OnClick = btnAddClick
      end
      object btnCE: TButton
        Left = 5
        Top = 53
        Width = 38
        Height = 25
        Caption = 'CE'
        TabOrder = 16
        OnClick = btnCEClick
      end
      object btnC: TButton
        Left = 49
        Top = 53
        Width = 38
        Height = 25
        Caption = 'C'
        TabOrder = 17
        OnClick = btnCClick
      end
      object btnResult: TButton
        Left = 93
        Top = 177
        Width = 38
        Height = 25
        Caption = '='
        TabOrder = 18
        OnClick = btnResultClick
      end
      object btn41: TButton
        Tag = 41
        Left = 93
        Top = 247
        Width = 38
        Height = 25
        Caption = '41.'
        TabOrder = 19
      end
      object btnDot: TButton
        Left = 49
        Top = 177
        Width = 38
        Height = 25
        Caption = '.'
        TabOrder = 20
        OnClick = btnDotClick
      end
      object btn39: TButton
        Tag = 39
        Left = 5
        Top = 247
        Width = 38
        Height = 25
        Caption = '39.'
        TabOrder = 21
      end
      object btn40: TButton
        Tag = 40
        Left = 49
        Top = 247
        Width = 38
        Height = 25
        Caption = '40.'
        TabOrder = 22
      end
      object btn35: TButton
        Tag = 35
        Left = 5
        Top = 216
        Width = 38
        Height = 25
        Caption = '35.'
        TabOrder = 23
        OnClick = btn35Click
      end
      object btn36: TButton
        Tag = 36
        Left = 49
        Top = 216
        Width = 38
        Height = 25
        Caption = '36.'
        TabOrder = 24
      end
      object btn37: TButton
        Tag = 37
        Left = 93
        Top = 216
        Width = 38
        Height = 25
        Caption = '37.'
        TabOrder = 25
      end
      object btn38: TButton
        Tag = 38
        Left = 137
        Top = 216
        Width = 38
        Height = 25
        Caption = '38.'
        TabOrder = 26
      end
      object btnNumberOk: TButton
        Left = 93
        Top = 31
        Width = 82
        Height = 47
        Caption = #30830#23450
        TabOrder = 27
        OnClick = btnNumberOkClick
      end
      object btn42: TButton
        Tag = 42
        Left = 137
        Top = 247
        Width = 38
        Height = 25
        Caption = '42.'
        TabOrder = 28
      end
    end
    object tsMemo: TTabSheet
      Caption = 'tsMemo'
      ImageIndex = 2
      object pnl3: TPanel
        Left = 0
        Top = 0
        Width = 251
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnMemoOk: TButton
          Left = 90
          Top = 1
          Width = 82
          Height = 25
          Caption = #30830#23450
          TabOrder = 0
          OnClick = btnMemoOkClick
        end
      end
      object mmoMemo: TMemo
        Left = 0
        Top = 30
        Width = 251
        Height = 226
        Align = alClient
        TabOrder = 1
      end
    end
    object tsDateTime: TTabSheet
      Caption = 'tsDateTime'
      ImageIndex = 3
      object pnl4: TPanel
        Left = 0
        Top = 0
        Width = 251
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnDateTimeOk: TButton
          Left = 86
          Top = 1
          Width = 82
          Height = 25
          Caption = #30830#23450
          TabOrder = 0
          OnClick = btnDateTimeOkClick
        end
      end
      object pnlDate: TPanel
        Left = 0
        Top = 30
        Width = 251
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object dtpdate: TDateTimePicker
          Left = 11
          Top = 9
          Width = 116
          Height = 24
          Date = 40725.914665567130000000
          Time = 40725.914665567130000000
          DateFormat = dfLong
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object cbbdate: TComboBox
          Left = 135
          Top = 11
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'yyyy'#24180'M'#26376'd'#26085
          Items.Strings = (
            'yyyy'#24180'M'#26376'd'#26085
            'yyyy'#24180'MM'#26376'dd'#26085
            'yyyy-M-d'
            'yyyy-MM-dd'
            'yyyy"/"M"/"d'
            'yyyy"/"MM"/"dd'
            'yyyy\M\d'
            'yyyy\MM\dd')
        end
      end
      object pnlTime: TPanel
        Left = 0
        Top = 71
        Width = 251
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object dtptime: TDateTimePicker
          Left = 45
          Top = 8
          Width = 82
          Height = 24
          Date = 40725.915467256940000000
          Time = 40725.915467256940000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          Kind = dtkTime
          ParentFont = False
          TabOrder = 0
        end
        object cbbtime: TComboBox
          Left = 135
          Top = 9
          Width = 82
          Height = 21
          Style = csDropDownList
          ItemIndex = 5
          TabOrder = 1
          Text = 'h:mm'
          Items.Strings = (
            'h'#26102'm'#20998
            'h'#26102'm'#20998's'#31186
            'hh:mm:ss'
            'hh:mm'
            'h:m:s'
            'h:mm')
        end
      end
    end
  end
  object fdgxwtcrsr: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 188
    Top = 59
  end
end
