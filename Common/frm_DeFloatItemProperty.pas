unit frm_DeFloatItemProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, HCView;

type
  TfrmDeFloatItemProperty = class(TForm)
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
    procedure btnAddPropertyClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

implementation

uses
  HCCustomFloatItem, HCEmrElementItem;

{$R *.dfm}

{ TfrmDeFloatItemProperty }

procedure TfrmDeFloatItemProperty.btnAddPropertyClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeFloatItemProperty.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeFloatItemProperty.SetHCView(const AHCView: THCView);
var
  vFloatItem: THCCustomFloatItem;
  vFloatBarCode: TDeFloatBarCodeItem;
  i: Integer;
begin
  vFloatItem := AHCView.ActiveSection.ActiveData.GetActiveFloatItem;
  edtWidth.Text := IntToStr(vFloatItem.Width);
  edtHeight.Text := IntToStr(vFloatItem.Height);

  if vFloatItem is TDeFloatBarCodeItem then
  begin
    vFloatBarCode := vFloatItem as TDeFloatBarCodeItem;
    chkDeleteAllow.Checked := vFloatBarCode.DeleteAllow;

    if vFloatBarCode.Propertys.Count > 0 then
      sgdProperty.RowCount := vFloatBarCode.Propertys.Count + 1
    else
      sgdProperty.RowCount := 2;

    sgdProperty.FixedRows := 1;
    sgdProperty.ColWidths[0] := 100;
    sgdProperty.ColWidths[1] := 200;
    sgdProperty.Cells[0, 0] := '¼ü';
    sgdProperty.Cells[1, 0] := 'Öµ';

    if vFloatBarCode.Propertys.Count = 0 then
    begin
      sgdProperty.Cells[0, 1] := '';
      sgdProperty.Cells[1, 1] := '';
    end
    else
    begin
      for i := 1 to vFloatBarCode.Propertys.Count do
      begin
        sgdProperty.Cells[0, i] := vFloatBarCode.Propertys.Names[i - 1];
        sgdProperty.Cells[1, i] := vFloatBarCode.Propertys.ValueFromIndex[i - 1];
      end;
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vFloatItem.Width := StrToIntDef(edtWidth.Text, vFloatItem.Width);
    vFloatItem.Height := StrToIntDef(edtHeight.Text, vFloatItem.Height);

    if vFloatBarCode <> nil then
    begin
      vFloatBarCode.DeleteAllow := chkDeleteAllow.Checked;

      vFloatBarCode.Propertys.Clear;
      for i := 1 to sgdProperty.RowCount - 1 do
      begin
        if sgdProperty.Cells[0, i].Trim <> '' then
          vFloatBarCode.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
      end;
    end;
  end;
end;

end.
