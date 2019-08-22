object frmRecordOverView: TfrmRecordOverView
  Left = 0
  Top = 0
  Caption = 'frmRecordOverView'
  ClientHeight = 389
  ClientWidth = 723
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sgdRecordView: TCFGridTree
    Left = 0
    Top = 0
    Width = 723
    Height = 329
    BorderVisible = True
    Alpha = 255
    Align = alTop
    Text = 'sgdRecordView'
    DefaultColWidth = 70
    RowHeight = 20
    RowCount = 0
    ColCount = 6
    Options = [cgoRowSizing, cgoColSizing, cgoIndicator, cgoRowSelect, cgoShowSelect]
    ReadOnly = False
  end
end
