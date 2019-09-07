object frmDataElement: TfrmDataElement
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #25554#20837#25968#25454#20803
  ClientHeight = 402
  ClientWidth = 369
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
  object pgcDE: TPageControl
    Left = 0
    Top = 0
    Width = 369
    Height = 402
    ActivePage = tsDE
    Align = alClient
    TabOrder = 0
    object tsDE: TTabSheet
      Caption = #21015#34920
      object pnl2: TPanel
        Left = 0
        Top = 0
        Width = 361
        Height = 34
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblDeHint: TLabel
          Left = 145
          Top = 11
          Width = 188
          Height = 13
          AutoSize = False
          Caption = '<- '#21517#31216#25110#31616#30721#22238#36710#26816#32034
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = lblDeHintClick
        end
        object edtPY: TEdit
          Left = 0
          Top = 7
          Width = 140
          Height = 21
          TabOrder = 0
          OnKeyDown = edtPYKeyDown
        end
      end
      object sgdDE: TStringGrid
        Left = 0
        Top = 34
        Width = 361
        Height = 340
        Align = alClient
        BorderStyle = bsNone
        ColCount = 6
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        PopupMenu = pmDE
        TabOrder = 1
        OnClick = sgdDEClick
        OnDblClick = sgdDEDblClick
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
    end
    object tsList: TTabSheet
      Caption = #20998#32452
      ImageIndex = 1
      object tv1: TTreeView
        Left = 0
        Top = 0
        Width = 361
        Height = 374
        Align = alClient
        BorderStyle = bsNone
        Indent = 19
        TabOrder = 0
        Items.NodeData = {
          0303000000260000000000000000000000FFFFFFFFFFFFFFFF00000000000000
          00050000000104A3600580E14F6F60220000000000000000000000FFFFFFFFFF
          FFFFFF0000000000000000000000000102D3590D542200000000000000000000
          00FFFFFFFFFFFFFFFF000000000000000000000000010227602B522200000000
          00000000000000FFFFFFFFFFFFFFFF0000000000000000000000000102745E84
          9F260000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
          000104FA511F75E5651F67240000000000000000000000FFFFFFFFFFFFFFFF00
          00000000000000000000000103FA511F753057260000000000000000000000FF
          FFFFFFFFFFFFFF000000000000000004000000010465516296E14F6F60260000
          000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000010465
          516296F665F4952A0000000000000000000000FFFFFFFFFFFFFFFF0000000000
          000000000000000106535F4D52D179A45B490044002600000000000000000000
          00FFFFFFFFFFFFFFFF0000000000000000000000000104535F4D52D179A45B26
          0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000001
          04FA516296F665F495260000000000000000000000FFFFFFFFFFFFFFFF000000
          0000000000010000000104C5758653E14F6F602A0000000000000000000000FF
          FFFFFFFFFFFFFF0000000000000000000000000106C57586531B52FA5EF665F4
          95}
      end
    end
  end
  object pmDE: TPopupMenu
    OnPopup = pmDEPopup
    Left = 40
    Top = 88
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
    object mniN6: TMenuItem
      Caption = '-'
    end
    object mniDomain: TMenuItem
      Caption = #20540#22495#31649#29702
      OnClick = mniDomainClick
    end
    object mniN5: TMenuItem
      Caption = '-'
    end
    object mniInsertAsDeItem: TMenuItem
      Caption = #25554#20837#65288#25968#25454#20803#65289
      OnClick = mniInsertAsDeItemClick
    end
    object mniInsertAsDeGroup: TMenuItem
      Caption = #25554#20837#65288#25968#25454#32452#65289
      OnClick = mniInsertAsDeGroupClick
    end
    object mniInsertAsEdit: TMenuItem
      Caption = #25554#20837#65288'Edit'#65289
      OnClick = mniInsertAsEditClick
    end
    object mniInsertAsCombobox: TMenuItem
      Caption = #25554#20837#65288'Combobox'#65289
      OnClick = mniInsertAsComboboxClick
    end
    object mniInsertAsDateTime: TMenuItem
      Caption = #25554#20837#65288'DateTime'#65289
      OnClick = mniInsertAsDateTimeClick
    end
    object mniInsertAsRadioGroup: TMenuItem
      Caption = #25554#20837#65288'RadioGroup'#65289
      OnClick = mniInsertAsRadioGroupClick
    end
    object mniInsertAsCheckBox: TMenuItem
      Caption = #25554#20837#65288'CheckBox'#65289
      OnClick = mniInsertAsCheckBoxClick
    end
    object mniInsertAsFloatBarCode: TMenuItem
      Caption = #25554#20837#65288#28014#21160#19968#32500#30721#65289
      OnClick = mniInsertAsFloatBarCodeClick
    end
    object mniN4: TMenuItem
      Caption = '-'
    end
    object mniRefresh: TMenuItem
      Caption = #21047#26032
      OnClick = mniRefreshClick
    end
  end
end
