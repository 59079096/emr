unit frm_DeProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HCView, Vcl.StdCtrls, Vcl.Grids;

type
  TfrmDeProperty = class(TForm)
    lbl8: TLabel;
    btnAdd: TButton;
    btnSave: TButton;
    sgdProperty: TStringGrid;
    btnDel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetHCView(const AHCView: THCView);
  end;

var
  frmDeProperty: TfrmDeProperty;

implementation

uses
  EmrElementItem, emr_Common;

{$R *.dfm}

{ TfrmDeProperty }

procedure TfrmDeProperty.btnAddClick(Sender: TObject);
var
  i: Integer;
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
  for i := 0 to sgdProperty.ColCount - 1 do
    sgdProperty.Cells[i, sgdProperty.RowCount - 1] := '';

  if sgdProperty.RowCount > 1 then
    sgdProperty.FixedRows := 1;
end;

procedure TfrmDeProperty.btnDelClick(Sender: TObject);
begin
  DeleteGridRow(sgdProperty);
end;

procedure TfrmDeProperty.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeProperty.FormCreate(Sender: TObject);
begin
  sgdProperty.RowCount := 1;
  sgdProperty.ColWidths[1] := 240;
  sgdProperty.Cells[0, 0] := '¼ü';
  sgdProperty.Cells[1, 0] := 'Öµ';
end;

procedure TfrmDeProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vDeItem: TDeItem;
begin
  vDeItem := AHCView.ActiveSectionTopLevelData.GetCurItem as TDeItem;
  sgdProperty.RowCount := vDeItem.Propertys.Count + 1;
  if sgdProperty.RowCount > 1 then
    sgdProperty.FixedRows := 1
  else
    sgdProperty.Options := sgdProperty.Options - [goEditing];

  for i := 0 to vDeItem.Propertys.Count - 1 do
  begin
    sgdProperty.Cells[0, i + 1] := vDeItem.Propertys.KeyNames[i];
    sgdProperty.Cells[1, i + 1] := vDeItem.Propertys.ValueFromIndex[i];
  end;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vDeItem.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
        vDeItem.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
    end;
  end;
end;

end.
