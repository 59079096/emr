{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_DataElement;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus;

type
  TInsertAsDEEvent = procedure(const AIndex, AName: string) of object;
  TfrmDataElement = class(TForm)
    pnl2: TPanel;
    lblDeHint: TLabel;
    edtPY: TEdit;
    sgdDE: TStringGrid;
    pmde: TPopupMenu;
    mniInsertAsDE: TMenuItem;
    mniInsertAsDG: TMenuItem;
    mniInsertAsEdit: TMenuItem;
    mniInsertAsCombobox: TMenuItem;
    mniN4: TMenuItem;
    mniRefresh: TMenuItem;
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniInsertAsDEClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
  private
    { Private declarations }
    FOnInsertAsDE: TInsertAsDEEvent;
    procedure ShowDataElement;
    procedure DoInsertAsDE(const AIndex, AName: string);
  public
    { Public declarations }
    property OnInsertAsDE: TInsertAsDEEvent read FOnInsertAsDE write FOnInsertAsDE;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, FireDAC.Comp.Client, Data.DB;

{$R *.dfm}

procedure TfrmDataElement.DoInsertAsDE(const AIndex, AName: string);
begin
  if Assigned(FOnInsertAsDE) then
    FOnInsertAsDE(AIndex, AName);
end;

procedure TfrmDataElement.edtPYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function IsPY(const AChar: Char): Boolean;
  begin
    Result := AChar in ['a'..'z', 'A'..'Z'];
  end;

begin
  if Key = VK_RETURN then
  begin
    ClientCache.DataElementDT.FilterOptions := [foCaseInsensitive{不区分大小写, foNoPartialCompare不支持通配符(*)所表示的部分匹配}];
    if edtPY.Text = '' then
      ClientCache.DataElementDT.Filtered := False
    else
    begin
      ClientCache.DataElementDT.Filtered := False;
      if IsPY(edtPY.Text[1]) then
        ClientCache.DataElementDT.Filter := 'py like ''%' + edtPY.Text + '%'''
      else
        ClientCache.DataElementDT.Filter := 'dename like ''%' + edtPY.Text + '%''';
      ClientCache.DataElementDT.Filtered := True;
    end;

    ShowDataElement;
  end;
end;

procedure TfrmDataElement.FormShow(Sender: TObject);
begin
  sgdDE.RowCount := 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

  ShowDataElement;
end;

procedure TfrmDataElement.mniInsertAsDEClick(Sender: TObject);
begin
  if sgdDE.Row >= 0 then
    DoInsertAsDE(sgdDE.Cells[0, sgdDE.Row], sgdDE.Cells[1, sgdDE.Row]);
end;

procedure TfrmDataElement.sgdDEDblClick(Sender: TObject);
begin
  mniInsertAsDEClick(Sender);
end;

procedure TfrmDataElement.ShowDataElement;
var
  vRow: Integer;
begin
  vRow := 1;
  sgdDE.RowCount := ClientCache.DataElementDT.RecordCount + 1;

  with ClientCache.DataElementDT do
  begin
    First;
    while not Eof do
    begin
      sgdDE.Cells[0, vRow] := FieldByName('deid').AsString;;
      sgdDE.Cells[1, vRow] := FieldByName('dename').AsString;
      sgdDE.Cells[2, vRow] := FieldByName('decode').AsString;
      sgdDE.Cells[3, vRow] := FieldByName('py').AsString;
      sgdDE.Cells[4, vRow] := FieldByName('frmtp').AsString;
      sgdDE.Cells[5, vRow] := FieldByName('domainid').AsString;
      Inc(vRow);

      Next;
    end;
  end;

  if sgdDE.RowCount > 1 then
    sgdDE.FixedRows := 1;
end;

end.
