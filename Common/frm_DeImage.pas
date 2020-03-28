unit frm_DeImage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, HCView, HCEmrElementItem, HCCommon;

type
  TfrmDeImage = class(TForm)
    pnl1: TPanel;
    lbl1: TLabel;
    edtWidth: TEdit;
    lbl2: TLabel;
    edtHeight: TEdit;
    sgdProperty: TStringGrid;
    lbl7: TLabel;
    btnSave: TButton;
    btnAddProp: TButton;
    chkDeleteAllow: TCheckBox;
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView; const AImage: TDeImageItem);
  end;

implementation

{$R *.dfm}

{ TfrmDeCombobox }

procedure TfrmDeImage.btnAddPropClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeImage.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeImage.SetHCView(const AHCView: THCView; const AImage: TDeImageItem);
var
  i: Integer;
begin
  edtWidth.Text := IntToStr(AImage.Width);
  edtHeight.Text := IntToStr(AImage.Height);

  chkDeleteAllow.Checked := AImage.DeleteAllow;

  if AImage.Propertys.Count > 0 then
    sgdProperty.RowCount := AImage.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '¼ü';
  sgdProperty.Cells[1, 0] := 'Öµ';

  if AImage.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to AImage.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := AImage.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := AImage.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    AImage.Width := StrToIntDef(edtWidth.Text, AImage.Width);
    AImage.Height := StrToIntDef(edtHeight.Text, AImage.Height);

    AImage.DeleteAllow := chkDeleteAllow.Checked;

    AImage.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
      begin
        AImage.Propertys.Add(HCDeleteBreak(sgdProperty.Cells[0, i])
          + '=' + HCDeleteBreak(sgdProperty.Cells[1, i]));
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
