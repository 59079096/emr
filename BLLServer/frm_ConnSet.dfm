object frmConnSet: TfrmConnSet
  Left = 0
  Top = 0
  Caption = 'frmConnSet'
  ClientHeight = 227
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnSave: TButton
    Left = 88
    Top = 188
    Width = 72
    Height = 24
    Caption = #20445#23384
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 258
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object chkRemote: TCheckBox
      Left = 10
      Top = 7
      Width = 97
      Height = 17
      Caption = #20351#29992#20027#26381#21153#22120
      TabOrder = 0
      OnClick = chkRemoteClick
    end
  end
  object pgc: TPageControl
    Left = 0
    Top = 30
    Width = 258
    Height = 149
    ActivePage = tsDataBase
    Align = alTop
    Style = tsFlatButtons
    TabOrder = 2
    object tsDataBase: TTabSheet
      Caption = #25968#25454#24211
      object lbl2: TLabel
        Left = 5
        Top = 6
        Width = 58
        Height = 13
        Caption = #25968#25454#24211'IP'#65306
      end
      object lbl3: TLabel
        Left = 15
        Top = 38
        Width = 48
        Height = 13
        Caption = #25968#25454#24211#65306
      end
      object lbl4: TLabel
        Left = 15
        Top = 70
        Width = 48
        Height = 13
        Caption = #29992#25143#21517#65306
      end
      object lbl5: TLabel
        Left = 27
        Top = 102
        Width = 36
        Height = 13
        Caption = #23494#30721#65306
      end
      object edtDBServer: TEdit
        Left = 65
        Top = 3
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtDBName: TEdit
        Left = 65
        Top = 35
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object edtDBUserName: TEdit
        Left = 65
        Top = 67
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object edtDBPassword: TEdit
        Left = 65
        Top = 99
        Width = 121
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
    end
    object tsRemote: TTabSheet
      Caption = #20027#26381#21153#22120
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbl7: TLabel
        Left = 5
        Top = 6
        Width = 58
        Height = 13
        Caption = #26381#21153#22120'IP'#65306
      end
      object lbl8: TLabel
        Left = 27
        Top = 39
        Width = 36
        Height = 13
        Caption = #31471#21475#65306
      end
      object edtRemoteServer: TEdit
        Left = 65
        Top = 3
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtRemotePort: TEdit
        Left = 65
        Top = 35
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object btnVerity: TButton
        Left = 199
        Top = 33
        Width = 45
        Height = 25
        Caption = #39564#35777
        TabOrder = 2
        OnClick = btnVerityClick
      end
    end
  end
end
