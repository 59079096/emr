object frmInchDoctorStation: TfrmInchDoctorStation
  Left = 0
  Top = 0
  Caption = #20303#38498#21307#29983#31449
  ClientHeight = 547
  ClientWidth = 805
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 805
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnl1'
    TabOrder = 0
  end
  object pmPatient: TPopupMenu
    Left = 128
    Top = 104
  end
end
