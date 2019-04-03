object frmEmr: TfrmEmr
  Left = 0
  Top = 0
  Caption = #30005#23376#30149#21382
  ClientHeight = 498
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lstPlugin: TCFListView
    Left = 0
    Top = 0
    Width = 209
    Height = 498
    Alpha = 255
    BorderVisible = True
    TabStop = False
    Align = alLeft
    GroupHeight = 20
    ItemHeight = 50
    AutoFreeObject = True
    OnDBlClick = lstPluginDBlClick
    ExplicitHeight = 413
  end
  object xpmnfst: TXPManifest
    Left = 296
    Top = 64
  end
  object appEvents: TApplicationEvents
    OnIdle = appEventsIdle
    Left = 368
    Top = 64
  end
end
