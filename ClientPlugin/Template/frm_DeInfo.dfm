object frmDeInfo: TfrmDeInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #25968#25454#20803#32500#25252
  ClientHeight = 640
  ClientWidth = 800
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
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 127
    Align = alTop
    TabOrder = 0
    object lbl1: TLabel
      Left = 182
      Top = 16
      Width = 24
      Height = 13
      Caption = #32534#30721
    end
    object lbl2: TLabel
      Left = 16
      Top = 16
      Width = 24
      Height = 13
      Caption = #21517#31216
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lbl3: TLabel
      Left = 16
      Top = 47
      Width = 24
      Height = 13
      Caption = #25340#38899
    end
    object lbl4: TLabel
      Left = 328
      Top = 16
      Width = 24
      Height = 13
      Caption = #23450#20041
    end
    object lbl5: TLabel
      Left = 128
      Top = 47
      Width = 24
      Height = 13
      Caption = #31867#22411
    end
    object lbl6: TLabel
      Left = 223
      Top = 47
      Width = 24
      Height = 13
      Caption = #26684#24335
    end
    object lbl7: TLabel
      Left = 328
      Top = 47
      Width = 24
      Height = 13
      Caption = #31867#21035
    end
    object lbl8: TLabel
      Left = 493
      Top = 47
      Width = 24
      Height = 13
      Caption = #21333#20301
    end
    object lbl9: TLabel
      Left = 659
      Top = 47
      Width = 24
      Height = 13
      Caption = #20540#22495
    end
    object lbl10: TLabel
      Left = 16
      Top = 109
      Width = 72
      Height = 13
      Caption = #19994#21153#25511#21046#33050#26412
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbbFrmtp: TComboBox
      Left = 358
      Top = 44
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = #25991#26412
      Items.Strings = (
        #25991#26412
        #21333#36873
        #22810#36873
        #25968#20540
        #26085#26399
        #26102#38388
        #26085#26399#26102#38388)
    end
    object edtCode: TEdit
      Left = 212
      Top = 13
      Width = 97
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 1
    end
    object edtDefine: TEdit
      Left = 358
      Top = 13
      Width = 424
      Height = 21
      TabOrder = 2
    end
    object edtDomainID: TEdit
      Left = 689
      Top = 44
      Width = 93
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      Text = '0'
    end
    object edtFormat: TEdit
      Left = 253
      Top = 44
      Width = 56
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 4
    end
    object edtName: TEdit
      Left = 46
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 5
    end
    object edtPY: TEdit
      Left = 46
      Top = 44
      Width = 67
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 6
    end
    object edtType: TEdit
      Left = 158
      Top = 44
      Width = 52
      Height = 21
      CharCase = ecUpperCase
      TabOrder = 7
    end
    object edtUnit: TEdit
      Left = 523
      Top = 44
      Width = 121
      Height = 21
      TabOrder = 8
    end
    object btnSave: TButton
      Left = 298
      Top = 85
      Width = 75
      Height = 25
      Caption = #20445#23384#20449#24687
      TabOrder = 9
      OnClick = btnSaveClick
    end
    object btnSaveClose: TButton
      Left = 393
      Top = 85
      Width = 119
      Height = 25
      Caption = #20445#23384#20449#24687#24182#20851#38381
      TabOrder = 10
      OnClick = btnSaveCloseClick
    end
  end
end
