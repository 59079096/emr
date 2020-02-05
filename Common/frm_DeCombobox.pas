unit frm_DeCombobox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeCombobox = class(TForm)
    pnl1: TPanel;
    chkAutoSize: TCheckBox;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    lbl9: TLabel;
    edtText: TEdit;
    lbl3: TLabel;
    chkBorderTop: TCheckBox;
    chkBorderBottom: TCheckBox;
    chkBorderLeft: TCheckBox;
    chkBorderRight: TCheckBox;
    pnlCombobox: TPanel;
    sgdProperty: TStringGrid;
    lbl7: TLabel;
    sgdItem: TStringGrid;
    btnSave: TButton;
    chkSaveItem: TCheckBox;
    btnAddProp: TButton;
    btnAddItem: TButton;
    chkPrintOnlyText: TCheckBox;
    chkDeleteAllow: TCheckBox;
    procedure chkSaveItemClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const ACombobox: TDeCombobox);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeCombobox.btnAddItemClick(Sender: TObject);
begin
  sgdItem.RowCount := sgdItem.RowCount + 1;
end;

procedure TfrmDeCombobox.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeCombobox.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeCombobox.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeCombobox.chkSaveItemClick(Sender: TObject);
begin
  sgdItem.Enabled := chkSaveItem.Checked;
  btnAddItem.Visible := chkSaveItem.Checked;
end;

procedure TfrmDeCombobox.SetHCView(const AHCView: THCView;
  const ACombobox: TDeCombobox);
var
  i: Integer;
begin
  if ACombobox[TDeProp.Name] <> '' then
    Self.Caption := ACombobox[TDeProp.Name];

  chkAutoSize.Checked := ACombobox.AutoSize;
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;

  edtWidth.Text := IntToStr(ACombobox.Width);
  edtHeight.Text := IntToStr(ACombobox.Height);
  edtText.Text := ACombobox.Text;
  chkPrintOnlyText.Checked := ACombobox.PrintOnlyText;
  chkDeleteAllow.Checked := ACombobox.DeleteAllow;
  chkBorderLeft.Checked := cbsLeft in ACombobox.BorderSides;
  chkBorderTop.Checked := cbsTop in ACombobox.BorderSides;
  chkBorderRight.Checked := cbsRight in ACombobox.BorderSides;
  chkBorderBottom.Checked := cbsBottom in ACombobox.BorderSides;

  if ACombobox.Propertys.Count > 0 then
    sgdProperty.RowCount := ACombobox.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if ACombobox.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to ACombobox.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := ACombobox.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := ACombobox.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  chkSaveItem.Checked := ACombobox.SaveItem;
  btnAddItem.Visible := chkSaveItem.Checked;
  sgdItem.Enabled := chkSaveItem.Checked;
  sgdItem.RowCount := ACombobox.Items.Count;
  for i := 0 to ACombobox.Items.Count - 1 do
    sgdItem.Cells[0, i] := ACombobox.Items[i];

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    ACombobox.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      ACombobox.Width := StrToIntDef(edtWidth.Text, ACombobox.Width);
      ACombobox.Height := StrToIntDef(edtHeight.Text, ACombobox.Height);
    end;

    ACombobox.Text := edtText.Text;

    if chkBorderLeft.Checked then
      ACombobox.BorderSides := ACombobox.BorderSides + [cbsLeft]
    else
      ACombobox.BorderSides := ACombobox.BorderSides - [cbsLeft];

    if chkBorderTop.Checked then
      ACombobox.BorderSides := ACombobox.BorderSides + [cbsTop]
    else
      ACombobox.BorderSides := ACombobox.BorderSides - [cbsTop];

    if chkBorderRight.Checked then
      ACombobox.BorderSides := ACombobox.BorderSides + [cbsRight]
    else
      ACombobox.BorderSides := ACombobox.BorderSides - [cbsRight];

    if chkBorderBottom.Checked then
      ACombobox.BorderSides := ACombobox.BorderSides + [cbsBottom]
    else
      ACombobox.BorderSides := ACombobox.BorderSides - [cbsBottom];

    ACombobox.PrintOnlyText := chkPrintOnlyText.Checked;
    ACombobox.DeleteAllow := chkDeleteAllow.Checked;

    ACombobox.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if sgdProperty.Cells[0, i].Trim <> '' then
        ACombobox.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
    end;

    ACombobox.SaveItem := chkSaveItem.Checked;
    ACombobox.Items.Clear;
    if ACombobox.SaveItem then
    begin
      for i := 0 to sgdItem.RowCount - 1 do
      begin
        if sgdItem.Cells[0, i] <> '' then
          ACombobox.Items.Add(sgdItem.Cells[0, i]);
      end;
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
