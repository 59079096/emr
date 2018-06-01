{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Domain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.Menus;

type
  TfrmDomain = class(TForm)
    sgdDomain: TStringGrid;
    pm: TPopupMenu;
    mniNew: TMenuItem;
    mniEdit: TMenuItem;
    mniDelete: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetAllDomain;
  public
    { Public declarations }
  end;

implementation

uses
  emr_Common, emr_BLLConst, emr_BLLServerProxy, FireDAC.Comp.Client, frm_DomainOper,
  TemplateCommon;

{$R *.dfm}

procedure TfrmDomain.FormShow(Sender: TObject);
begin
  sgdDomain.RowCount := 1;
  sgdDomain.Cells[0, 0] := 'ID';
  sgdDomain.Cells[1, 0] := '编码';
  sgdDomain.Cells[2, 0] := '名称';

  GetAllDomain;
end;

procedure TfrmDomain.GetAllDomain;
begin
  HintFormShow('正在获取所有值域...', procedure(const AUpdateHint: TUpdateHint)
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETDOMAIN;  // 获取值域
        ABLLServerReady.BackDataSet := True;
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        i: Integer;
      begin
        if ABLLServer.MethodRunOk then
        begin
          if AMemTable <> nil then
          begin
            sgdDomain.RowCount := AMemTable.RecordCount + 1;

            if sgdDomain.RowCount > 1 then
              sgdDomain.FixedRows := 1;

            i := 1;

            AMemTable.First;
            while not AMemTable.Eof do
            begin
              sgdDomain.Cells[0, i] := AMemTable.FieldByName('DID').AsString;
              sgdDomain.Cells[1, i] := AMemTable.FieldByName('DCode').AsString;
              sgdDomain.Cells[2, i] := AMemTable.FieldByName('DName').AsString;

              Inc(i);
              AMemTable.Next;
            end;
          end;
        end
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  end);
end;

procedure TfrmDomain.mniDeleteClick(Sender: TObject);
var
  vRow, vTopRow, vDomainID: Integer;
  vDeleteOk: Boolean;
begin
  if sgdDomain.Row >= 0 then
  begin
    if MessageDlg('确定要删除值域【' + sgdDomain.Cells[1, sgdDomain.Row] + '】以及其对应的选项及选项关联的内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      SaveStringGridRow(vRow, vTopRow, sgdDomain);

      vDeleteOk := True;
      vDomainID := StrToInt(sgdDomain.Cells[0, sgdDomain.Row]);

      // 取所有的选项，遍历删除关联内容
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_GETDOMAINITEM;  // 获取值域选项
          ABLLServerReady.ExecParam.I['domainid'] := vDomainID;
          ABLLServerReady.BackDataSet := True;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
          begin
            ShowMessage(ABLLServer.MethodError);
            vDeleteOk := False;
          end;

          if AMemTable <> nil then
          begin
            with AMemTable do
            begin
              First;
              while not Eof do
              begin
                if not DeleteDomainItemContent(AMemTable.FieldByName('ID').AsInteger) then  // 删除值域选项关联内容
                begin
                  ShowMessage(CommonLastError);
                  vDeleteOk := False;
                  Break;
                end;

                Next;
              end;
            end;
          end;
        end);

      if not vDeleteOk then Exit;

      // 删除值域对应的所有选项
      if not DeleteDomainAllItem(vDomainID) then
      begin
        ShowMessage(CommonLastError);
        vDeleteOk := False;
      end;

      if not vDeleteOk then Exit;

      // 删除值域
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_DELETEDOMAIN;  // 删除值域
          ABLLServerReady.ExecParam.I['DID'] := vDomainID;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if not ABLLServer.MethodRunOk then
            ShowMessage(ABLLServer.MethodError)
          else
          begin
            ShowMessage('删除值域成功！');

            GetAllDomain;
            RestoreStringGridRow(vRow, vTopRow, sgdDomain);
          end;
        end);
    end;
  end;
end;

procedure TfrmDomain.mniEditClick(Sender: TObject);
var
  vFrmDomainOper: TfrmDomainOper;
  vTopRow, vRow: Integer;
begin
  if sgdDomain.Row > 0 then
  begin
    SaveStringGridRow(vRow, vTopRow, sgdDomain);

    vFrmDomainOper := TfrmDomainOper.Create(Self);
    try
      vFrmDomainOper.DID := StrToInt(sgdDomain.Cells[0, sgdDomain.Row]);
      vFrmDomainOper.edtCode.Text := sgdDomain.Cells[1, sgdDomain.Row];
      vFrmDomainOper.edtName.Text := sgdDomain.Cells[2, sgdDomain.Row];
      vFrmDomainOper.ShowModal;
      if vFrmDomainOper.ModalResult = mrOk then
        GetAllDomain;

      RestoreStringGridRow(vRow, vTopRow, sgdDomain);
    finally
      FreeAndNil(vFrmDomainOper);
    end;
  end;
end;

procedure TfrmDomain.mniNewClick(Sender: TObject);
var
  vFrmDomainOper: TfrmDomainOper;
begin
  vFrmDomainOper := TfrmDomainOper.Create(Self);
  try
    vFrmDomainOper.DID := 0;
    vFrmDomainOper.ShowModal;
    if vFrmDomainOper.ModalResult = mrOk then
      GetAllDomain;
  finally
    FreeAndNil(vFrmDomainOper);
  end;
end;

end.

