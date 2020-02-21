unit frm_DeDateTime;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeDateTime = class(TForm)
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
    lbl4: TLabel;
    cbbDTFormat: TComboBox;
    chkDeleteAllow: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const ADateTime: TDeDateTimePicker);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeDateTime.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeDateTime.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeDateTime.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeDateTime.SetHCView(const AHCView: THCView; const ADateTime: TDeDateTimePicker);
var
  i: Integer;
begin
  if ADateTime[TDeProp.Name] <> '' then
    Self.Caption := ADateTime[TDeProp.Name];

  chkAutoSize.Checked := ADateTime.AutoSize;
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;

  edtWidth.Text := IntToStr(ADateTime.Width);
  edtHeight.Text := IntToStr(ADateTime.Height);

  edtText.Text := ADateTime.Text;
  chkPrintOnlyText.Checked := ADateTime.PrintOnlyText;
  chkDeleteAllow.Checked := ADateTime.DeleteAllow;
  cbbDTFormat.Text := ADateTime.Format;

  chkBorderLeft.Checked := cbsLeft in ADateTime.BorderSides;
  chkBorderTop.Checked := cbsTop in ADateTime.BorderSides;
  chkBorderRight.Checked := cbsRight in ADateTime.BorderSides;
  chkBorderBottom.Checked := cbsBottom in ADateTime.BorderSides;

  if ADateTime.Propertys.Count > 0 then
    sgdProperty.RowCount := ADateTime.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if ADateTime.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to ADateTime.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := ADateTime.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := ADateTime.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    ADateTime.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      ADateTime.Width := StrToIntDef(edtWidth.Text, ADateTime.Width);
      ADateTime.Height := StrToIntDef(edtHeight.Text, ADateTime.Height);
    end;

    if chkBorderLeft.Checked then
      ADateTime.BorderSides := ADateTime.BorderSides + [cbsLeft]
    else
      ADateTime.BorderSides := ADateTime.BorderSides - [cbsLeft];

    if chkBorderTop.Checked then
      ADateTime.BorderSides := ADateTime.BorderSides + [cbsTop]
    else
      ADateTime.BorderSides := ADateTime.BorderSides - [cbsTop];

    if chkBorderRight.Checked then
      ADateTime.BorderSides := ADateTime.BorderSides + [cbsRight]
    else
      ADateTime.BorderSides := ADateTime.BorderSides - [cbsRight];

    if chkBorderBottom.Checked then
      ADateTime.BorderSides := ADateTime.BorderSides + [cbsBottom]
    else
      ADateTime.BorderSides := ADateTime.BorderSides - [cbsBottom];

    ADateTime.PrintOnlyText := chkPrintOnlyText.Checked;
    ADateTime.DeleteAllow := chkDeleteAllow.Checked;
    ADateTime.Format := cbbDTFormat.Text;

    ADateTime.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
        ADateTime.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
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
