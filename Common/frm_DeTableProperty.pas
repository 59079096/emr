unit frm_DeTableProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HCView, EmrElementItem, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids;

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
    cbbRowAlignVert: TComboBox;
    lbl3: TLabel;
    lbl6: TLabel;
    edtRowHeight: TEdit;
    lbl7: TLabel;
    cbbCellAlignVert: TComboBox;
    btnBorderBackColor: TButton;
    lbl8: TLabel;
    btnComboxAddProperty: TButton;
    sgdTable: TStringGrid;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtCellHPaddingChange(Sender: TObject);
    procedure btnBorderBackColorClick(Sender: TObject);
    procedure btnComboxAddPropertyClick(Sender: TObject);
  private
    { Private declarations }
    FReFormt: Boolean;
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
    vFrmBorderBackColor.SetHCView(FHCView);
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
  FReFormt := True;
end;

procedure TfrmDeTableProperty.FormShow(Sender: TObject);
begin
  pgTable.ActivePageIndex := 0;
  FReFormt := False;
end;

procedure TfrmDeTableProperty.SetHCView(const AHCView: THCView);
var
  vR, vC, viValue{, vRowAlignIndex}: Integer;
  vData: THCRichData;
  vAlignVert: TAlignVert;
  vTable: TDeTable;
  i: Integer;
begin
  FHCView := AHCView;
  vData := FHCView.ActiveSection.ActiveData;
  vTable := vData.GetCurItem as TDeTable;

  // 表格
  edtCellHPadding.Text := IntToStr(vTable.CellHPadding);
  edtCellVPadding.Text := IntToStr(vTable.CellVPadding);
  chkBorderVisible.Checked := vTable.BorderVisible;
  edtBorderWidth.Text := IntToStr(vTable.BorderWidth);

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

  sgdTable.RowCount := vTable.Propertys.Count;
  if sgdTable.RowCount = 0 then
  begin
    sgdTable.Cells[0, 0] := '';
    sgdTable.Cells[1, 0] := '';
  end
  else
  begin
    for i := 0 to vTable.Propertys.Count - 1 do
    begin
      sgdTable.Cells[0, i] := vTable.Propertys.Names[i];
      sgdTable.Cells[1, i] := vTable.Propertys.ValueFromIndex[i];
    end;
  end;

  //
  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    FHCView.BeginUpdate;
    try
      // 表格
      vTable.CellHPadding := StrToIntDef(edtCellHPadding.Text, 5);
      vTable.CellVPadding := StrToIntDef(edtCellVPadding.Text, 0);
      vTable.BorderWidth := StrToIntDef(edtBorderWidth.Text, 1);
      vTable.BorderVisible := chkBorderVisible.Checked;

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
              vTable.Cells[vR, vC].AlignVert := TAlignVert(cbbCellAlignVert.ItemIndex);
          end;
        end
        else
          vTable.GetEditCell.AlignVert := TAlignVert(cbbCellAlignVert.ItemIndex);
      end;

      vTable.Propertys.Clear;
      for i := 0 to sgdTable.RowCount - 1 do
        vTable.Propertys.Add(sgdTable.Cells[0, i] + '=' + sgdTable.Cells[1, i]);

      if FReFormt then
        FHCView.ActiveSection.ReFormatActiveItem;
    finally
      FHCView.EndUpdate;
    end;
  end;
end;

end.
