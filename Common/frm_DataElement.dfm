object frmDataElement: TfrmDataElement
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #25554#20837#25968#25454#20803
  ClientHeight = 388
  ClientWidth = 361
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
  object pnl2: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblDeHint: TLabel
      Left = 168
      Top = 10
      Width = 188
      Height = 13
      AutoSize = False
      Caption = '<- '#36755#20837#21517#31216#25110#31616#25340#22238#36710#24320#22987#26816#32034
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edtPY: TEdit
      Left = 6
      Top = 7
      Width = 159
      Height = 21
      TabOrder = 0
      OnKeyDown = edtPYKeyDown
    end
  end
  object sgdDE: TStringGrid
    Left = 0
    Top = 34
    Width = 361
    Height = 354
    Align = alClient
    ColCount = 6
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    PopupMenu = pmde
    TabOrder = 1
    OnDblClick = sgdDEDblClick
    ExplicitHeight = 349
    ColWidths = (
      35
      142
      67
      36
      28
      23)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object pmde: TPopupMenu
    Left = 145
    Top = 176
    object mniInsertAsDE: TMenuItem
      Caption = #25554#20837#65288#25968#25454#20803#65289
      OnClick = mniInsertAsDEClick
    end
    object mniInsertAsDG: TMenuItem
      Caption = #25554#20837#65288#25968#25454#32452#65289
    end
    object mniInsertAsEdit: TMenuItem
      Caption = #25554#20837#65288'Edit'#65289
    end
    object mniInsertAsCombobox: TMenuItem
      Caption = #25554#20837#65288'Combobox'#65289
    end
    object mniN4: TMenuItem
      Caption = '-'
    end
    object mniRefresh: TMenuItem
      Caption = #21047#26032
    end
  end
end
