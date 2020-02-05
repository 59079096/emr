{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DeTableProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HCView, HCEmrElementItem, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.Buttons;

type
  TfrmDeTableProperty = class(TForm)
    pgTable: TPageControl;
    tsTable: TTabSheet;
    tsRow: TTabSheet;
    tsCell: TTabSheet;
    edtCellHPadding: TEdit;
    edtCellVPadding: TEdit;
    edtBorderWidth: TEdit;
    chkBorderVisible: TCheckBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    pnl1: TPanel;
    btnOk: TButton;
    lbl6: TLabel;
    edtRowHeight: TEdit;
    lbl7: TLabel;
    cbbCellAlignVert: TComboBox;
    btnBorderBackColor: TButton;
    lbl8: TLabel;
    btnComboxAddProperty: TButton;
    sgdTable: TStringGrid;
    lbl16: TLabel;
    lbl9: TLabel;
    edtFixRowFirst: TEdit;
    lbl10: TLabel;
    edtFixRowLast: TEdit;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    edtFixColFirst: TEdit;
    lbl14: TLabel;
    edtFixColLast: TEdit;
    lbl15: TLabel;
    lbl17: TLabel;
    btnCellLeftBorder: TSpeedButton;
    btnCellTopBorder: TSpeedButton;
    btnCellBottomBorder: TSpeedButton;
    btnCellRightBorder: TSpeedButton;
    btnCellLTRBBorder: TSpeedButton;
    btnCellRTLBBorder: TSpeedButton;
    chkDeleteAllow: TCheckBox;
    lbl3: TLabel;
    lbl18: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtCellHPaddingChange(Sender: TObject);
    procedure btnBorderBackColorClick(Sender: TObject);
    procedure btnComboxAddPropertyClick(Sender: TObject);
  private
    { Private declarations }
    FReFormat: Boolean;
    FHCView: THCView;
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCRichData, HCTableCell, frm_TableBorderBackColor;

{$R *.dfm}

{ TfrmTableProperty }

procedure TfrmDeTableProperty.btnBorderBackColorClick(Sender: TObject);
var
  vFrmBorderBackColor: TfrmBorderBackColor;
begin
  vFrmBorderBackColor := TfrmBorderBackColor.Create(Self);
  try
    vFrmBorderBackColor.SetView(FHCView);
  finally
    FreeAndNil(vFrmBorderBackColor);
  end;
end;

procedure TfrmDeTableProperty.btnComboxAddPropertyClick(Sender: TObject);
begin
  sgdTable.RowCount := sgdTable.RowCount + 1;
end;

procedure TfrmDeTableProperty.btnOkClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeTableProperty.edtCellHPaddingChange(Sender: TObject);
begin
  FReFormat := True;
end;

procedure TfrmDeTableProperty.FormShow(Sender: TObject);
begin
  pgTable.ActivePageIndex := 0;
  FReFormat := False;
end;

procedure TfrmDeTableProperty.SetHCView(const AHCView: THCView);
var
  vR, vC, viValue{, vRowAlignIndex}: Integer;
  vData: THCRichData;
  vAlignVert: THCAlignVert;
  vTable: TDeTable;
  i: Integer;
begin
  FHCView := AHCView;
  vData := FHCView.ActiveSection.ActiveData;
  vTable := vData.GetActiveItem as TDeTable;

  // 表格
  edtCellHPadding.Text := FormatFloat('0.##', vTable.CellHPaddingMM);
  edtCellVPadding.Text := FormatFloat('0.##', vTable.CellVPaddingMM);
  chkBorderVisible.Checked := vTable.BorderVisible;
  edtBorderWidth.Text := FormatFloat('0.##', vTable.BorderWidthPt);

  edtFixRowFirst.Text := IntToStr(vTable.FixRow + 1);
  edtFixRowLast.Text := IntToStr(vTable.FixRow + 1 + vTable.FixRowCount);
  edtFixColFirst.Text := IntToStr(vTable.FixCol + 1);
  edtFixColLast.Text := IntToStr(vTable.FixCol + 1 + vTable.FixColCount);

  // 行
  if vTable.SelectCellRang.StartRow >= 0 then
  begin
    tsRow.Caption := '行(' + IntToStr(vTable.SelectCellRang.StartRow + 1) + ')';
    if vTable.SelectCellRang.EndRow > 0 then
      tsRow.Caption := tsRow.Caption + ' - (' + IntToStr(vTable.SelectCellRang.EndRow + 1) + ')';

    edtRowHeight.Text := IntToStr(vTable.Rows[vTable.SelectCellRang.StartRow].Height);  // 行高
  end
  else
    tsRow.TabVisible := False;

  {vAlignVert := FTableItem.GetEditCell.AlignVert;
  cbbRowAlignVert.ItemIndex := Ord(vAlignVert) + 1;
  for i := 0 to FTableItem.Rows[FTableItem.SelectCellRang.StartRow].ColCount - 1 do
  begin
    if vAlignVert <> FTableItem.Cells[FTableItem.SelectCellRang.StartRow, i].AlignVert then  // 有不同
    begin
      cbbRowAlignVert.ItemIndex := 0;  // 自定义
      Break;
    end;
  end;
  vRowAlignIndex := cbbRowAlignVert.ItemIndex;}

  // 单元格
  if (vTable.SelectCellRang.StartRow >= 0) and (vTable.SelectCellRang.StartCol >= 0) then
  begin
    if vTable.SelectCellRang.EndRow >= 0 then  // 多选
    begin
      vAlignVert := vTable.Cells[vTable.SelectCellRang.StartRow,
        vTable.SelectCellRang.StartCol].AlignVert;

      tsCell.Caption := '单元格(' + IntToStr(vTable.SelectCellRang.StartRow + 1) + ','
        + IntToStr(vTable.SelectCellRang.StartCol + 1) + ') - ('
        + IntToStr(vTable.SelectCellRang.EndRow + 1) + ','
        + IntToStr(vTable.SelectCellRang.EndCol + 1) + ')';
    end
    else
    begin
      vAlignVert := vTable.GetEditCell.AlignVert;

      tsCell.Caption := '单元格(' + IntToStr(vTable.SelectCellRang.StartRow + 1) + ','
        + IntToStr(vTable.SelectCellRang.StartCol + 1) + ')';
    end;

    cbbCellAlignVert.ItemIndex := Ord(vAlignVert);
  end
  else
    tsCell.TabVisible := False;

  if vTable.Propertys.Count > 0 then
    sgdTable.RowCount := vTable.Propertys.Count + 1
  else
    sgdTable.RowCount := 2;

  chkDeleteAllow.Checked := vTable.DeleteAllow;

  sgdTable.FixedRows := 1;
  sgdTable.ColWidths[0] := 100;
  sgdTable.ColWidths[1] := 200;
  sgdTable.Cells[0, 0] := '键';
  sgdTable.Cells[1, 0] := '值';

  if vTable.Propertys.Count = 0 then
  begin
    sgdTable.Cells[0, 1] := '';
    sgdTable.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to vTable.Propertys.Count do
    begin
      sgdTable.Cells[0, i] := vTable.Propertys.Names[i - 1];
      sgdTable.Cells[1, i] := vTable.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  //
  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    FHCView.BeginUpdate;
    try
      // 表格
      vTable.CellHPaddingMM := StrToFloatDef(edtCellHPadding.Text, 0.2);
      vTable.CellVPaddingMM := StrToFloatDef(edtCellVPadding.Text, 0);
      vTable.BorderWidthPt := StrToFloatDef(edtBorderWidth.Text, 0.5);
      vTable.BorderVisible := chkBorderVisible.Checked;

      vTable.FixRow := StrToIntDef(edtFixRowFirst.Text, 0) - 1;
      vTable.FixRowCount := StrToIntDef(edtFixRowLast.Text, 0) - vTable.FixRow;
      vTable.FixCol := StrToIntDef(edtFixColFirst.Text, 0) - 1;
      vTable.FixColCount := StrToIntDef(edtFixColLast.Text, 0) - vTable.FixCol;

      // 行
      if (vTable.SelectCellRang.StartRow >= 0) and (TryStrToInt(edtRowHeight.Text, viValue)) then
      begin
        if vTable.SelectCellRang.EndRow > 0 then  // 有选中多行
        begin
          for vR := vTable.SelectCellRang.StartRow to vTable.SelectCellRang.EndRow do
            vTable.Rows[vR].Height := viValue;  // 行高
        end
        else  // 只选中一行
          vTable.Rows[vTable.SelectCellRang.StartRow].Height := viValue;  // 行高
      end;

      // 单元格
      if (vTable.SelectCellRang.StartRow >= 0) and (vTable.SelectCellRang.StartCol >= 0) then
      begin
        if vTable.SelectCellRang.EndCol > 0 then  // 有选中多个单元格
        begin
          for vR := vTable.SelectCellRang.StartRow to vTable.SelectCellRang.EndRow do
          begin
            for vC := vTable.SelectCellRang.StartCol to vTable.SelectCellRang.EndCol do
              vTable.Cells[vR, vC].AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);
          end;
        end
        else
          vTable.GetEditCell.AlignVert := THCAlignVert(cbbCellAlignVert.ItemIndex);
      end;

      vTable.DeleteAllow := chkDeleteAllow.Checked;

      vTable.Propertys.Clear;
      for i := 1 to sgdTable.RowCount - 1 do
      begin
        if sgdTable.Cells[0, i].Trim <> '' then
          vTable.Propertys.Add(sgdTable.Cells[0, i] + '=' + sgdTable.Cells[1, i]);
      end;

      if FReFormat then
        FHCView.ActiveSection.ReFormatActiveItem;
    finally
      FHCView.EndUpdate;
    end;
  end;
end;

end.
