unit frm_DeEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeEdit = class(TForm)
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
    sgdProperty: TStringGrid;
    lbl7: TLabel;
    btnSave: TButton;
    btnAddProp: TButton;
    chkPrintOnlyText: TCheckBox;
    chkDeleteAllow: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const AEdit: TDeEdit);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeEdit.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeEdit.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeEdit.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeEdit.SetHCView(const AHCView: THCView; const AEdit: TDeEdit);
var
  i: Integer;
begin
  if AEdit[TDeProp.Name] <> '' then
    Self.Caption := AEdit[TDeProp.Name];

  chkAutoSize.Checked := AEdit.AutoSize;
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;

  edtWidth.Text := IntToStr(AEdit.Width);
  edtHeight.Text := IntToStr(AEdit.Height);
  edtText.Text := AEdit.Text;
  chkPrintOnlyText.Checked := AEdit.PrintOnlyText;
  chkDeleteAllow.Checked := AEdit.DeleteAllow;
  chkBorderLeft.Checked := cbsLeft in AEdit.BorderSides;
  chkBorderTop.Checked := cbsTop in AEdit.BorderSides;
  chkBorderRight.Checked := cbsRight in AEdit.BorderSides;
  chkBorderBottom.Checked := cbsBottom in AEdit.BorderSides;

  if AEdit.Propertys.Count > 0 then
    sgdProperty.RowCount := AEdit.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if AEdit.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to AEdit.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := AEdit.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := AEdit.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AEdit.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      AEdit.Width := StrToIntDef(edtWidth.Text, AEdit.Width);
      AEdit.Height := StrToIntDef(edtHeight.Text, AEdit.Height);
    end;

    AEdit.Text := edtText.Text;

    if chkBorderLeft.Checked then
      AEdit.BorderSides := AEdit.BorderSides + [cbsLeft]
    else
      AEdit.BorderSides := AEdit.BorderSides - [cbsLeft];

    if chkBorderTop.Checked then
      AEdit.BorderSides := AEdit.BorderSides + [cbsTop]
    else
      AEdit.BorderSides := AEdit.BorderSides - [cbsTop];

    if chkBorderRight.Checked then
      AEdit.BorderSides := AEdit.BorderSides + [cbsRight]
    else
      AEdit.BorderSides := AEdit.BorderSides - [cbsRight];

    if chkBorderBottom.Checked then
      AEdit.BorderSides := AEdit.BorderSides + [cbsBottom]
    else
      AEdit.BorderSides := AEdit.BorderSides - [cbsBottom];

    AEdit.PrintOnlyText := chkPrintOnlyText.Checked;
    AEdit.DeleteAllow := chkDeleteAllow.Checked;

    AEdit.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if sgdProperty.Cells[0, i].Trim <> '' then
        AEdit.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
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
