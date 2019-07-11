unit frm_UpgradeHis;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TfrmUpgradeHis = class(TForm)
    pmList: TPopupMenu;
    mniAdd: TMenuItem;
    mniDelete: TMenuItem;
    pnl2: TPanel;
    lbl1: TLabel;
    lblVersion: TLabel;
    btnOK: TButton;
    edtVersion: TEdit;
    mmo: TMemo;
    pnl1: TPanel;
    sgdList: TStringGrid;
    sgdFiles: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgdListClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowUpdateHistory;
  public
    { Public declarations }
  end;

var
  frmUpgradeHis: TfrmUpgradeHis;

implementation

uses
  frm_DM, FireDAC.Comp.Client, Xml.XMLDoc, Xml.XMLIntf, UPCommon;

{$R *.dfm}

procedure TfrmUpgradeHis.btnOKClick(Sender: TObject);
begin
  if sgdList.Row > 0 then
  begin
    if dm.ExecSql(Format('UPDATE UpdateInfo SET version = ''%s'', memo = ''%s'' WHERE verno = %s',
      [edtVersion.Text, mmo.Lines.Text, sgdList.Cells[0, sgdList.Row]])) then
    begin
      sgdList.Cells[1, sgdList.Row] := edtVersion.Text;
      sgdList.Cells[2, sgdList.Row] := mmo.Lines.Text;
      ShowMessage('修改成功！');
    end;
  end;
end;

procedure TfrmUpgradeHis.FormCreate(Sender: TObject);
begin
  sgdList.ColCount := 4;
  sgdList.ColWidths[0] := 80;
  sgdList.ColWidths[1] := 100;
  sgdList.ColWidths[2] := 100;
  sgdList.ColWidths[3] := 0;
  //
  sgdList.Cells[0, 0] := '版本序号';
  sgdList.Cells[1, 0] := '版本号';
  sgdList.Cells[2, 0] := '说明';

  //
  sgdFiles.ColCount := 5;
  sgdFiles.ColWidths[0] := 80;
  sgdFiles.ColWidths[1] := 100;
  sgdFiles.ColWidths[2] := 100;
  sgdFiles.ColWidths[3] := 100;
  sgdFiles.ColWidths[4] := 0;
  //
  sgdFiles.Cells[0, 0] := '版本';
  sgdFiles.Cells[1, 0] := '文件名';
  sgdFiles.Cells[2, 0] := '客户端路径';
  sgdFiles.Cells[3, 0] := '大小';
end;

procedure TfrmUpgradeHis.FormShow(Sender: TObject);
var
  vRow: Integer;
  vQuery: TFDQuery;
begin
  vQuery := dm.OpenSql('SELECT verno, version, memo, files FROM UpdateInfo ORDER BY verno DESC');
  try
    sgdList.RowCount := vQuery.RecordCount + 1;
    if sgdList.RowCount > 1 then
      sgdList.FixedRows := 1;

    vRow := 1;
    with vQuery do
    begin
      First;
      while not Eof do
      begin
        sgdList.Cells[0, vRow] := FieldByName('verno').AsString;
        sgdList.Cells[1, vRow] := FieldByName('version').AsString;
        sgdList.Cells[2, vRow] := FieldByName('memo').AsString;
        sgdList.Cells[3, vRow] := FieldByName('files').AsString;

        Inc(vRow);
        Next;
      end;
    end;
  finally
    FreeAndNil(vQuery);
  end;

  ShowUpdateHistory;
end;

procedure TfrmUpgradeHis.sgdListClick(Sender: TObject);
begin
  ShowUpdateHistory;
end;

procedure TfrmUpgradeHis.ShowUpdateHistory;
var
  vXmlDoc: IXMLDocument;
  vFileNode: IXMLNode;
  i: Integer;
begin
  if sgdList.Row > 0 then
  begin
    edtVersion.Text := sgdList.Cells[1, sgdList.Row];
    mmo.Lines.Text := sgdList.Cells[2, sgdList.Row];

    vXmlDoc := TXMLDocument.Create(nil);
    vXmlDoc.LoadFromXML(sgdList.Cells[3, sgdList.Row]);
    vFileNode := vXmlDoc.DocumentElement.ChildNodes[0];

    sgdFiles.RowCount := vFileNode.ChildNodes.Count + 1;
    if sgdFiles.RowCount > 1 then
      sgdFiles.FixedRows := 1;

    for i := 0 to vFileNode.ChildNodes.Count - 1 do
    begin
      sgdFiles.Cells[1, i + 1] := vFileNode.Text;  // name
      sgdFiles.Cells[0, i + 1] := vFileNode.Attributes['version'];
      sgdFiles.Cells[2, i + 1] := vFileNode.Attributes['path'];
      sgdFiles.Cells[3, i + 1] := BytesToStr(vFileNode.Attributes['size']);
    end;
  end
  else
  begin
    edtVersion.Clear;
    mmo.Clear;
    sgdFiles.RowCount := 1;
  end;
end;

end.
