object frmDomain: TfrmDomain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20540#22495
  ClientHeight = 447
  ClientWidth = 799
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 481
    Top = 0
    Height = 447
    ExplicitLeft = 400
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  object sgdDomain: TStringGrid
    Left = 0
    Top = 0
    Width = 481
    Height = 447
    Align = alLeft
    ColCount = 3
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    PopupMenu = pm
    TabOrder = 0
    OnClick = sgdDomainClick
    ColWidths = (
      46
      77
      315)
  end
  object pm: TPopupMenu
    Left = 40
    Top = 144
    object mniNew: TMenuItem
      Caption = #28155#21152
      OnClick = mniNewClick
    end
    object mniEdit: TMenuItem
      Caption = #20462#25913
      OnClick = mniEditClick
    end
    object mniDelete: TMenuItem
      Caption = #21024#38500
      OnClick = mniDeleteClick
    end
  end
end
