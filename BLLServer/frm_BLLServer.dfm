object frmBLLServer: TfrmBLLServer
  Left = 0
  Top = 0
  Caption = 'EMR'#26381#21153#31471
  ClientHeight = 506
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgc: TPageControl
    Left = 0
    Top = 0
    Width = 696
    Height = 506
    ActivePage = ts2
    Align = alClient
    TabOrder = 0
    object tsState: TTabSheet
      Caption = #36816#34892#29366#24577
    end
    object ts2: TTabSheet
      Caption = #30417#25511
      ImageIndex = 1
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 688
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
          OnClick = btnClearClick
        end
        object btnSave: TButton
          Left = 208
          Top = 8
          Width = 75
          Height = 25
          Caption = #20445#23384
          TabOrder = 2
        end
      end
      object mmoMsg: TMemo
        Left = 0
        Top = 41
        Width = 688
        Height = 437
        Align = alClient
        ImeMode = imClose
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
  object mmMain: TMainMenu
    AutoHotkeys = maManual
    Left = 32
    Top = 80
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
    object mniN3: TMenuItem
      Caption = #37197#32622
      object mniConnect: TMenuItem
        Caption = #36830#25509
        OnClick = mniConnectClick
      end
      object mniBLLSet: TMenuItem
        Caption = #19994#21153
        OnClick = mniBLLSetClick
      end
    end
  end
end
