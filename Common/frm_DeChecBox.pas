unit frm_DeChecBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeCheckBox = class(TForm)
    pnl1: TPanel;
    chkAutoSize: TCheckBox;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    lbl9: TLabel;
    edtText: TEdit;
    sgdProperty: TStringGrid;
    lbl7: TLabel;
    btnSave: TButton;
    btnAddProp: TButton;
    chkDeleteAllow: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const ACheckBox: TDeCheckBox);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeCheckBox.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeCheckBox.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeCheckBox.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeCheckBox.SetHCView(const AHCView: THCView; const ACheckBox: TDeCheckBox);
var
  i: Integer;
begin
  if ACheckBox[TDeProp.Name] <> '' then
    Self.Caption := ACheckBox[TDeProp.Name];

  chkAutoSize.Checked := ACheckBox.AutoSize;
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;

  edtWidth.Text := IntToStr(ACheckBox.Width);
  edtHeight.Text := IntToStr(ACheckBox.Height);
  edtText.Text := ACheckBox.Text;
  chkDeleteAllow.Checked := ACheckBox.DeleteAllow;

  if ACheckBox.Propertys.Count > 0 then
    sgdProperty.RowCount := ACheckBox.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if ACheckBox.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to ACheckBox.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := ACheckBox.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := ACheckBox.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    ACheckBox.DeleteAllow := chkDeleteAllow.Checked;
    ACheckBox.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then  // 自定义大小
    begin
      ACheckBox.Width := StrToIntDef(edtWidth.Text, ACheckBox.Width);
      ACheckBox.Height := StrToIntDef(edtHeight.Text, ACheckBox.Height);
    end;

    ACheckBox.Text := edtText.Text;

    ACheckBox.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if sgdProperty.Cells[0, i].Trim <> '' then
        ACheckBox.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
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
