{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DeProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, HCView, Vcl.StdCtrls, Vcl.Grids,
  Vcl.ExtCtrls;

type
  TfrmDeProperty = class(TForm)
    btnSave: TButton;
    sgdProperty: TStringGrid;
    pnl1: TPanel;
    chkCanEdit: TCheckBox;
    chkCanCopy: TCheckBox;
    chkDeleteAllow: TCheckBox;
    lbl7: TLabel;
    btnAdd: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
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
  HCEmrElementItem, emr_Common;

{$R *.dfm}

{ TfrmDeProperty }

procedure TfrmDeProperty.btnAddClick(Sender: TObject);
begin
  sgdProperty.RowCount := sgdProperty.RowCount + 1;
end;

procedure TfrmDeProperty.btnSaveClick(Sender: TObject);
begin
  Self.ModalResult := mrOk;
end;

procedure TfrmDeProperty.FormCreate(Sender: TObject);
begin
  sgdProperty.RowCount := 1;
  sgdProperty.ColWidths[1] := 240;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';
end;

procedure TfrmDeProperty.SetHCView(const AHCView: THCView);
var
  i: Integer;
  vDeItem: TDeItem;
begin
  vDeItem := AHCView.ActiveSectionTopLevelData.GetActiveItem as TDeItem;

  if vDeItem.Propertys.Count > 0 then
    sgdProperty.RowCount := vDeItem.Propertys.Count + 1
  else
    sgdProperty.RowCount := 2;

  sgdProperty.FixedRows := 1;
  sgdProperty.ColWidths[0] := 100;
  sgdProperty.ColWidths[1] := 200;
  sgdProperty.Cells[0, 0] := '键';
  sgdProperty.Cells[1, 0] := '值';

  if vDeItem.Propertys.Count = 0 then
  begin
    sgdProperty.Cells[0, 1] := '';
    sgdProperty.Cells[1, 1] := '';
  end
  else
  begin
    for i := 1 to vDeItem.Propertys.Count do
    begin
      sgdProperty.Cells[0, i] := vDeItem.Propertys.Names[i - 1];
      sgdProperty.Cells[1, i] := vDeItem.Propertys.ValueFromIndex[i - 1];
    end;
  end;

  chkCanEdit.Checked := not vDeItem.EditProtect;
  chkCanCopy.Checked := not vDeItem.CopyProtect;
  chkDeleteAllow.Checked := vDeItem.DeleteAllow;

  Self.ShowModal;
  if Self.ModalResult = mrOk then
  begin
    vDeItem.Propertys.Clear;
    for i := 1 to sgdProperty.RowCount - 1 do
    begin
      if Trim(sgdProperty.Cells[0, i]) <> '' then
        vDeItem.Propertys.Add(sgdProperty.Cells[0, i] + '=' + sgdProperty.Cells[1, i]);
    end;

    vDeItem.EditProtect := not chkCanEdit.Checked;
    vDeItem.CopyProtect := not chkCanCopy.Checked;
    vDeItem.DeleteAllow := chkDeleteAllow.Checked;
  end;
end;

end.
