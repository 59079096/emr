unit frm_DeFloatBarCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  Grids, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeFloatBarCode = class(TForm)
    sgdProperty: TStringGrid;
    pnlEdit: TPanel;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    btnSave: TButton;
    chkDeleteAllow: TCheckBox;
    lbl7: TLabel;
    btnAddProperty: TButton;
    lbl9: TLabel;
    edtText: TEdit;
    edtPenWidth: TEdit;
    lbl3: TLabel;
    chkAutoSize: TCheckBox;
    chkShowText: TCheckBox;
    chkLock: TCheckBox;
    procedure btnAddPropertyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkAutoSizeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const AFloatBarCode: TDeFloatBarCodeItem);
  end;

implementation

uses
  HCCustomFloatItem;

{$R *.dfm}

{ TfrmDeFloatItemProperty }

procedure TfrmDeFloatBarCode.btnAddPropertyClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeFloatBarCode.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeFloatBarCode.chkAutoSizeClick(Sender: TObject);
begin
  edtWidth.Enabled := not chkAutoSize.Checked;
  edtHeight.Enabled := not chkAutoSize.Checked;
end;

procedure TfrmDeFloatBarCode.SetHCView(const AHCView: THCView; const AFloatBarCode: TDeFloatBarCodeItem);
var
  i: Integer;
begin
  chkAutoSize.Checked := AFloatBarCode.AutoSize;
  edtWidth.Text := IntToStr(AFloatBarCode.Width);
  edtHeight.Text := IntToStr(AFloatBarCode.Height);
  edtText.Text := AFloatBarCode.Text;
  edtPenWidth.Text := IntToStr(AFloatBarCode.PenWidth);
  chkDeleteAllow.Checked := AFloatBarCode.DeleteAllow;
  chkShowText.Checked := AFloatBarCode.ShowText;
  chkLock.Checked := AFloatBarCode.Lock;

  if AFloatBarCode.Propertys.Count > 0 then
    sgdProperty.RowCount := AFloatBarCode.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '¼ü';
  sgdProperty.Cells[1, 0] := 'Öµ';

  if AFloatBarCode.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to AFloatBarCode.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := AFloatBarCode.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := AFloatBarCode.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AFloatBarCode.AutoSize := chkAutoSize.Checked;
    if not chkAutoSize.Checked then
    begin
      AFloatBarCode.Width := StrToIntDef(edtWidth.Text, AFloatBarCode.Width);
      AFloatBarCode.Height := StrToIntDef(edtHeight.Text, AFloatBarCode.Height);
    end;

    AFloatBarCode.Text := edtText.Text;
    AFloatBarCode.PenWidth := StrToIntDef(edtPenWidth.Text, AFloatBarCode.PenWidth);
    AFloatBarCode.DeleteAllow := chkDeleteAllow.Checked;
    AFloatBarCode.ShowText := chkShowText.Checked;
    AFloatBarCode.Lock := chkLock.Checked;

    AFloatBarCode.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
      begin
        AFloatBarCode.Propertys.Add(HCDeleteBreak(sgdProperty.Cells[0, i])
          + '=' + HCDeleteBreak(sgdProperty.Cells[1, i]));
      end;
    end;
  end;
end;

end.
