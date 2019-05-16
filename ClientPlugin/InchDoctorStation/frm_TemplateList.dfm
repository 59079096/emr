object frmTemplateList: TfrmTemplateList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = #36873#25321#27169#26495
  ClientHeight = 496
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 200
    Top = 41
    Width = 5
    Height = 364
    ExplicitLeft = 249
    ExplicitTop = -160
    ExplicitHeight = 646
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 579
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object tvTemplate: TTreeView
    Left = 0
    Top = 41
    Width = 200
    Height = 364
    Align = alLeft
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    ShowHint = False
    ShowLines = False
    TabOrder = 1
    OnChange = tvTemplateChange
  end
  object pnl2: TPanel
    Left = 0
    Top = 405
    Width = 579
    Height = 91
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lbl1: TLabel
      Left = 147
      Top = 21
      Width = 48
      Height = 13
      Caption = #30149#21382#21517#31216
    end
    object lbl2: TLabel
      Left = 147
      Top = 53
      Width = 48
      Height = 13
      Caption = #30149#21382#26102#38388
    end
    object edtRecordName: TEdit
      Left = 202
      Top = 18
      Width = 216
      Height = 21
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 342
      Top = 46
      Width = 77
      Height = 25
      Caption = #30830#23450
      TabOrder = 1
      OnClick = btnOKClick
    end
    object dtpRecDT: TCFDateTimePicker
      Left = 202
      Top = 48
      Width = 135
      Height = 22
      BorderVisible = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      DateTime = 43599.674595023150000000
      Format = 'YYYY-M-D HH:mm'
      ButtonVisible = True
    end
  end
  object sgdTempList: TStringGrid
    Left = 205
    Top = 41
    Width = 374
    Height = 364
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 3
  end
end
