object frmDataElementDomain: TfrmDataElementDomain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #25968#25454#20803#20540#22495#36873#39033
  ClientHeight = 437
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnl3: TPanel
    Left = 0
    Top = 0
    Width = 347
    Height = 25
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object lblDE: TLabel
      Left = 6
      Top = 5
      Width = 272
      Height = 13
      AutoSize = False
      Caption = #20540#22495#36873#39033'('#28857#20987#21047#26032')'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = lblDEClick
    end
  end
  object sgdCV: TStringGrid
    Left = 0
    Top = 25
    Width = 347
    Height = 412
    Align = alClient
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    PopupMenu = pmCV
    TabOrder = 1
    ColWidths = (
      125
      39
      45
      39
      64)
  end
  object pmCV: TPopupMenu
    OnPopup = pmCVPopup
    Left = 24
    Top = 64
    object mniNewItem: TMenuItem
      Caption = #28155#21152
      OnClick = mniNewItemClick
    end
    object mniEditItem: TMenuItem
      Caption = #20462#25913
      OnClick = mniEditItemClick
    end
    object mniDeleteItem: TMenuItem
      Caption = #21024#38500
      OnClick = mniDeleteItemClick
    end
    object mniN10: TMenuItem
      Caption = '-'
    end
    object mniEditItemLink: TMenuItem
      Caption = #32534#36753#25193#23637#20869#23481
      OnClick = mniEditItemLinkClick
    end
    object mniDeleteItemLink: TMenuItem
      Caption = #21024#38500#25193#23637#20869#23481
      OnClick = mniDeleteItemLinkClick
    end
  end
end
