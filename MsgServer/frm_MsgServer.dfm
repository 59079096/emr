object frmMsgServer: TfrmMsgServer
  Left = 0
  Top = 0
  Caption = #21363#26102#28040#24687#26381#21153#31471'['#20572#27490':'#31471#21475'12820]'
  ClientHeight = 423
  ClientWidth = 623
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgc: TPageControl
    Left = 0
    Top = 0
    Width = 623
    Height = 423
    ActivePage = ts2
    Align = alClient
    TabOrder = 0
    object tsState: TTabSheet
      Caption = #36816#34892#29366#24577
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object ts2: TTabSheet
      Caption = #30417#25511
      ImageIndex = 1
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 615
        Height = 41
        Align = alTop
        TabOrder = 0
        object chkLog: TCheckBox
          Left = 12
          Top = 11
          Width = 97
          Height = 17
          Caption = #35760#24405#26085#24535
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object btnClear: TButton
          Left = 115
          Top = 8
          Width = 75
          Height = 25
          Caption = #28165#31354
          TabOrder = 1
        end
      end
      object mmoLog: TMemo
        Left = 0
        Top = 41
        Width = 615
        Height = 354
        Align = alClient
        ImeName = #20013#25991'('#31616#20307') - '#25628#29399#25340#38899#36755#20837#27861
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object mmMain: TMainMenu
    AutoHotkeys = maManual
    Left = 48
    Top = 96
    object mniN1: TMenuItem
      Caption = #25511#21046
      object mniStart: TMenuItem
        Caption = #21551#21160
        OnClick = mniStartClick
      end
      object mniStop: TMenuItem
        Caption = #20572#27490
        OnClick = mniStopClick
      end
    end
  end
end
