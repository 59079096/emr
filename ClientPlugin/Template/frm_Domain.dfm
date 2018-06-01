object frmDomain: TfrmDomain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20540#22495
  ClientHeight = 447
  ClientWidth = 657
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
  object sgdDomain: TStringGrid
    Left = 0
    Top = 0
    Width = 657
    Height = 447
    Align = alClient
    ColCount = 3
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    PopupMenu = pm
    TabOrder = 0
    ColWidths = (
      46
      77
      504)
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
