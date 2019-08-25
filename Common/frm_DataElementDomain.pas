unit frm_DataElementDomain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Menus;

type
  TfrmDataElementDomain = class(TForm)
    pnl3: TPanel;
    lblDE: TLabel;
    sgdCV: TStringGrid;
    pmCV: TPopupMenu;
    mniNewItem: TMenuItem;
    mniEditItem: TMenuItem;
    mniDeleteItem: TMenuItem;
    mniN10: TMenuItem;
    mniEditItemLink: TMenuItem;
    mniDeleteItemLink: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure pmCVPopup(Sender: TObject);
    procedure lblDEClick(Sender: TObject);
    procedure mniNewItemClick(Sender: TObject);
    procedure mniEditItemClick(Sender: TObject);
    procedure mniDeleteItemClick(Sender: TObject);
    procedure mniEditItemLinkClick(Sender: TObject);
    procedure mniDeleteItemLinkClick(Sender: TObject);
  private
    { Private declarations }
    FDomainID: Integer;
    FDeName: string;
    procedure GetDomainItem;
    procedure SetDomainID(const Value: Integer);
    procedure SetDeName(const Value: string);
  public
    { Public declarations }
    property DomainID: Integer read FDomainID write SetDomainID;
    property DeName: string read FDeName write SetDeName;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, FireDAC.Comp.Client, Data.DB, frm_DomainItem, frm_ItemContent;

{$R *.dfm}

procedure TfrmDataElementDomain.FormShow(Sender: TObject);
begin
  sgdCV.RowCount := 1;
//  sgdCV.ColWidths[0] := 120;
//  sgdCV.ColWidths[1] := 40;
//  sgdCV.ColWidths[2] := 25;
//  sgdCV.ColWidths[3] := 35;
//  sgdCV.ColWidths[4] := 35;
  sgdCV.Cells[0, 0] := '值';
  sgdCV.Cells[1, 0] := '编码';
  sgdCV.Cells[2, 0] := '拼音';
  sgdCV.Cells[3, 0] := 'id';
  sgdCV.Cells[4, 0] := '扩展';
end;

procedure TfrmDataElementDomain.GetDomainItem;
var
  vTopRow, vRow: Integer;
begin
  if FDomainID > 0 then
  begin
    SaveStringGridRow(vRow, vTopRow, sgdCV);

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETDOMAINITEM;  // 获取值域选项
        ABLLServerReady.ExecParam.I['domainid'] := FDomainID;
        ABLLServerReady.BackDataSet := True;
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        i: Integer;
      begin
        if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;

        if AMemTable <> nil then
        begin
          sgdCV.RowCount := AMemTable.RecordCount + 1;

          i := 1;

          with AMemTable do
          begin
            First;
            while not Eof do
            begin
              sgdCV.Cells[0, i] := FieldByName('devalue').AsString;
              sgdCV.Cells[1, i] := FieldByName('code').AsString;
              sgdCV.Cells[2, i] := FieldByName('py').AsString;
              sgdCV.Cells[3, i] := FieldByName('id').AsString;
              if FieldByName('content').DataType = ftBlob then
              begin
                if (FieldByName('content') as TBlobField).BlobSize > 0 then
                  sgdCV.Cells[4, i] := '...'
                else
                  sgdCV.Cells[4, i] := '';
              end
              else
                sgdCV.Cells[4, i] := '';

              Next;
              Inc(i);
            end;
          end;

          if AMemTable.RecordCount > 0 then
            sgdCV.FixedRows := 1;

          RestoreStringGridRow(vRow, vTopRow, sgdCV);
        end;
      end);
  end
  else
    sgdCV.RowCount := 0;
end;

procedure TfrmDataElementDomain.lblDEClick(Sender: TObject);
begin
  GetDomainItem;
end;

procedure TfrmDataElementDomain.mniDeleteItemClick(Sender: TObject);
var
  vDeleteOk: Boolean;
begin
  if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】和该选项对应的扩展内容吗？',
    mtWarning, [mbYes, mbNo], 0) = mrYes
  then
  begin
    vDeleteOk := True;

    // 删除扩展内容
    TBLLInvoke.DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row]),
      procedure (const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if not ABLLServer.MethodRunOk then
        begin
          vDeleteOk := False;
          ShowMessage(ABLLServer.MethodError);
        end
        else
          ShowMessage('删除选项扩展内容成功！');
      end);

    if not vDeleteOk then Exit;

    // 删除选项
    TBLLInvoke.DeleteDomainItem(StrToInt(sgdCV.Cells[3, sgdCV.Row]),
      procedure (const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then
        begin
          GetDomainItem;
          ShowMessage('删除选项成功！');
        end
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  end;
end;

procedure TfrmDataElementDomain.mniDeleteItemLinkClick(Sender: TObject);
begin
  if sgdCV.Row >= 0 then
  begin
    if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】的扩展内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      TBLLInvoke.DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row]),
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then
            ShowMessage('删除值域选项扩展内容成功！')
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end;
  end;
end;

procedure TfrmDataElementDomain.mniEditItemClick(Sender: TObject);
var
  vFrmDomainItem: TfrmDomainItem;
begin
  vFrmDomainItem := TfrmDomainItem.Create(nil);
  try
    vFrmDomainItem.DomainID := FDomainID;
    vFrmDomainItem.ItemID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
    vFrmDomainItem.PopupParent := Self;
    vFrmDomainItem.ShowModal;
    if vFrmDomainItem.ModalResult = mrOk then
      GetDomainItem;
  finally
    FreeAndNil(vFrmDomainItem);
  end;
end;

procedure TfrmDataElementDomain.mniEditItemLinkClick(Sender: TObject);
var
  vFrmItemContent: TfrmItemContent;
begin
  if sgdCV.Row < 1 then Exit;
  if sgdCV.Cells[3, sgdCV.Row] = '' then Exit;

  vFrmItemContent := TfrmItemContent.Create(nil);
  try
    vFrmItemContent.DomainItemID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
    vFrmItemContent.PopupParent := Self;
    vFrmItemContent.ShowModal;
  finally
    FreeAndNil(vFrmItemContent);
  end;
end;

procedure TfrmDataElementDomain.mniNewItemClick(Sender: TObject);
var
  vFrmDomainItem: TfrmDomainItem;
begin
  vFrmDomainItem := TfrmDomainItem.Create(nil);
  try
    vFrmDomainItem.DomainID := FDomainID;
    vFrmDomainItem.ItemID := 0;
    vFrmDomainItem.PopupParent := Self;
    vFrmDomainItem.ShowModal;
    if vFrmDomainItem.ModalResult = mrOk then
      GetDomainItem;
  finally
    FreeAndNil(vFrmDomainItem);
  end;
end;

procedure TfrmDataElementDomain.pmCVPopup(Sender: TObject);
begin
  mniNewItem.Visible := FDomainID > 0;
  mniEditItem.Visible := sgdCV.Row > 0;
  mniDeleteItem.Visible := sgdCV.Row > 0;
  mniEditItemLink.Visible := sgdCV.Row > 0;
  mniDeleteItemLink.Visible := sgdCV.Row > 0;
end;

procedure TfrmDataElementDomain.SetDeName(const Value: string);
begin
  if FDeName <> Value then
  begin
    FDeName := Value;
    lblDE.Caption := '[' + FDeName + '] 选项如下(点击刷新)';
  end;
end;

procedure TfrmDataElementDomain.SetDomainID(const Value: Integer);
begin
  if FDomainID <> Value then
  begin
    FDomainID := Value;
    GetDomainItem;
  end;
end;

end.
