object frmInchDoctorStation: TfrmInchDoctorStation
  Left = 0
  Top = 0
  Caption = #20303#38498#21307#29983#31449
  ClientHeight = 527
  ClientWidth = 805
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBar: TPanel
    Left = 0
    Top = 495
    Width = 805
    Height = 32
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 515
  end
  object mmMain: TMainMenu
    Left = 24
    Top = 16
    object mniN1: TMenuItem
      Caption = #25554#20837
      object mniN2: TMenuItem
        Caption = #25968#25454#20803
        OnClick = mniN2Click
      end
    end
    object mniPat: TMenuItem
      Caption = #24739#32773
    end
  end
end
