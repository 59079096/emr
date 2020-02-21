unit frm_DeRadioGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon, HCRadioGroup;

type
  TfrmDeRadioGroup = class(TForm)
    pnl1: TPanel;
    chkAutoSize: TCheckBox;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    pnlCombobox: TPanel;
    sgdProperty: TStringGrid;
    lbl7: TLabel;
    sgdItem: TStringGrid;
    btnSave: TButton;
    btnAddProp: TButton;
    btnAddItem: TButton;
    cbbStyle: TComboBox;
    lbl3: TLabel;
    chkMulSelect: TCheckBox;
    chkDeleteAllow: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const ARadioGroup: TDeRadioGroup);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeRadioGroup.btnAddItemClick(Sender: TObject);
begin
  sgdItem.RowCount := sgdItem.RowCount + 1;
end;

procedure TfrmDeRadioGroup.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeRadioGroup.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeRadioGroup.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeRadioGroup.SetHCView(const AHCView: THCView;
  const ARadioGroup: TDeRadioGroup);
var
  i: Integer;
begin
  if ARadioGroup[TDeProp.Name] <> '' then
    Self.Caption := ARadioGroup[TDeProp.Name];

  chkAutoSize.Checked := ARadioGroup.AutoSize;
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;

  edtWidth.Text := IntToStr(ARadioGroup.Width);
  edtHeight.Text := IntToStr(ARadioGroup.Height);

  if ARadioGroup.RadioStyle = THCRadioStyle.Radio then
    cbbStyle.ItemIndex := 0
  else
    cbbStyle.ItemIndex := 1;

  chkMulSelect.Checked := ARadioGroup.MultSelect;
  chkDeleteAllow.Checked := ARadioGroup.DeleteAllow;

  if ARadioGroup.Propertys.Count > 0 then
    sgdProperty.RowCount := ARadioGroup.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if ARadioGroup.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to ARadioGroup.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := ARadioGroup.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := ARadioGroup.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  if ARadioGroup.Items.Count > 0 then
    sgdItem.RowCount := ARadioGroup.Items.Count + 1
  else
    sgdItem.RowCount := 2;

  sgdItem.FixedRows := 1;
  sgdItem.ColWidths[0] := 100;
  sgdItem.ColWidths[1] := 200;
  sgdItem.Cells[0, 0] := '文本';
  sgdItem.Cells[1, 0] := '附加值';

  for i := 1 to ARadioGroup.Items.Count do
  begin
    sgdItem.Cells[0, i] := ARadioGroup.Items[i - 1].Text;
    sgdItem.Cells[1, i] := ARadioGroup.Items[i - 1].TextValue;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    ARadioGroup.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      ARadioGroup.Width := StrToIntDef(edtWidth.Text, ARadioGroup.Width);
      ARadioGroup.Height := StrToIntDef(edtHeight.Text, ARadioGroup.Height);
    end;

    if cbbStyle.ItemIndex = 0 then
      ARadioGroup.RadioStyle := THCRadioStyle.Radio
    else
      ARadioGroup.RadioStyle := THCRadioStyle.CheckBox;

    ARadioGroup.MultSelect := chkMulSelect.Checked;
    ARadioGroup.DeleteAllow := chkDeleteAllow.Checked;

    ARadioGroup.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
        ARadioGroup.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
    end;

    ARadioGroup.Items.Clear;
    for i := 1 to sgdItem.RowCount do
    begin
      if sgdItem.Cells[0, i] <> '' then
        ARadioGroup.AddItem(sgdItem.Cells[0, i], sgdItem.Cells[1, i]);
    end;

    AHCView.BeginUpdate;
    try
      AHCView.ActiveSection.ReFormatActiveItem;
    finally
      AHCView.EndUpdate;
    end;
  end;
end;

end.
