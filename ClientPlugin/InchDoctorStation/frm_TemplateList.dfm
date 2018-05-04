object frmTemplateList: TfrmTemplateList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = #36873#25321#27169#26495
  ClientHeight = 496
  ClientWidth = 651
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
    Left = 249
    Top = 41
    Width = 5
    Height = 395
    ExplicitTop = -160
    ExplicitHeight = 646
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 651
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object tvTemplate: TTreeView
    Left = 0
    Top = 41
    Width = 249
    Height = 395
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
    Top = 436
    Width = 651
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object edtRecordName: TEdit
      Left = 128
      Top = 16
      Width = 193
      Height = 21
      TabOrder = 0
    end
    object btnOK: TButton
      Left = 336
      Top = 14
      Width = 75
      Height = 25
      Caption = #30830#23450
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
  object sgdTempList: TStringGrid
    Left = 254
    Top = 41
    Width = 397
    Height = 395
    Align = alClient
    ColCount = 4
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 3
  end
end
