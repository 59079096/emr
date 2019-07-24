object frmScriptIDE: TfrmScriptIDE
  Left = 0
  Top = 0
  Caption = #33050#26412#32534#35793#22120
  ClientHeight = 315
  ClientWidth = 465
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
  object spl2: TSplitter
    Left = 0
    Top = 237
    Width = 465
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 457
    ExplicitWidth = 634
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 41
    Align = alTop
    TabOrder = 0
    object lbl1: TLabel
      Left = 200
      Top = 16
      Width = 268
      Height = 13
      Caption = #27880#24847#65306'32'#20301#31243#24207#20445#23384#30340'bin'#21644'64'#31243#24207#20445#23384#30340'bin'#19981#36890#29992
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnSave: TButton
      Left = 106
      Top = 9
      Width = 75
      Height = 25
      Hint = #20445#23384#33050#26412'(CTRL + S)'
      Caption = #20445#23384#33050#26412
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnSaveClick
    end
    object btnCompile: TButton
      Left = 9
      Top = 9
      Width = 75
      Height = 25
      Hint = #32534#35793'(CTRL + F9)'
      ParentCustomHint = False
      Caption = #32534#35793
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnCompileClick
    end
  end
  object pnlMessage: TPanel
    Left = 0
    Top = 240
    Width = 465
    Height = 75
    Align = alBottom
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = 'pnlMessage'
    TabOrder = 1
    object pnl2: TPanel
      Left = 0
      Top = 0
      Width = 461
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
      Width = 461
      Height = 47
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnDblClick = lstMessageDblClick
    end
  end
end
