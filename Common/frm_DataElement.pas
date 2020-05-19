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
  Vcl.Menus, Vcl.ComCtrls;

type
  TfrmDataElement = class(TForm)
    pnl2: TPanel;
    lblDeHint: TLabel;
    edtPY: TEdit;
    sgdDE: TStringGrid;
    pmDE: TPopupMenu;
    mniNew: TMenuItem;
    mniEdit: TMenuItem;
    mniDelete: TMenuItem;
    mniN6: TMenuItem;
    mniDomain: TMenuItem;
    mniN5: TMenuItem;
    mniInsertAsDeItem: TMenuItem;
    mniInsertAsDeGroup: TMenuItem;
    mniInsertAsEdit: TMenuItem;
    mniInsertAsCombobox: TMenuItem;
    mniInsertAsDateTime: TMenuItem;
    mniInsertAsRadioGroup: TMenuItem;
    mniInsertAsCheckBox: TMenuItem;
    mniN4: TMenuItem;
    mniRefresh: TMenuItem;
    pgcDE: TPageControl;
    tsDE: TTabSheet;
    tsList: TTabSheet;
    tv1: TTreeView;
    mniInsertAsFloatBarCode: TMenuItem;
    mniInsertAsImage: TMenuItem;
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
    procedure mniInsertAsDeItemClick(Sender: TObject);
    procedure mniInsertAsDeGroupClick(Sender: TObject);
    procedure mniInsertAsEditClick(Sender: TObject);
    procedure mniInsertAsComboboxClick(Sender: TObject);
    procedure mniInsertAsDateTimeClick(Sender: TObject);
    procedure mniInsertAsRadioGroupClick(Sender: TObject);
    procedure mniInsertAsCheckBoxClick(Sender: TObject);
    procedure lblDeHintClick(Sender: TObject);
    procedure mniRefreshClick(Sender: TObject);
    procedure pmDEPopup(Sender: TObject);
    procedure sgdDEClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniDomainClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniInsertAsFloatBarCodeClick(Sender: TObject);
    procedure mniInsertAsImageClick(Sender: TObject);
  private
    { Private declarations }
    FSelectRow: Integer;
    FDomainID: Integer;  // 当前查看的值域ID
    FOnSelectChange, FOnInsertAsDeItem, FOnInsertAsDeGroup, FOnInsertAsDeEdit,
      FOnInsertAsDeCombobox, FOnInsertAsDeDateTime, FOnInsertAsDeRadioGroup,
      FOnInsertAsDeCheckBox, FOnInsertAsDeFloatBarCode, FOnInsertAsDeImage: TNotifyEvent;
    procedure ShowDataElement;
    //procedure DoInsertAsDE(const AIndex, AName: string);
  public
    { Public declarations }
    function GetDomainID: Integer;
    function GetDeName: string;
    function GetDeIndex: string;
    property OnInsertAsDeItem: TNotifyEvent read FOnInsertAsDeItem write FOnInsertAsDeItem;
    property OnInsertAsDeGroup: TNotifyEvent read FOnInsertAsDeGroup write FOnInsertAsDeGroup;
    property OnInsertAsDeEdit: TNotifyEvent read FOnInsertAsDeEdit write FOnInsertAsDeEdit;
    property OnInsertAsDeCombobox: TNotifyEvent read FOnInsertAsDeCombobox write FOnInsertAsDeCombobox;
    property OnInsertAsDeDateTime: TNotifyEvent read FOnInsertAsDeDateTime write FOnInsertAsDeDateTime;
    property OnInsertAsDeRadioGroup: TNotifyEvent read FOnInsertAsDeRadioGroup write FOnInsertAsDeRadioGroup;
    property OnInsertAsDeCheckBox: TNotifyEvent read FOnInsertAsDeCheckBox write FOnInsertAsDeCheckBox;
    property OnInsertAsDeFloatBarCode: TNotifyEvent read FOnInsertAsDeFloatBarCode write FOnInsertAsDeFloatBarCode;
    property OnInsertAsDeImage: TNotifyEvent read FOnInsertAsDeImage write FOnInsertAsDeImage;

    property OnSelectChange: TNotifyEvent read FOnSelectChange write FOnSelectChange;
  end;

implementation

uses
  emr_Common, emr_BLLInvoke, FireDAC.Comp.Client, Data.DB, HCEmrElementItem,
  frm_DeInfo, frm_Domain;

{$R *.dfm}

{procedure TfrmDataElement.DoInsertAsDE(const AIndex, AName: string);
begin
  if Assigned(FOnInsertAsDE) then
    FOnInsertAsDE(AIndex, AName);
end;}

procedure TfrmDataElement.edtPYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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
  FSelectRow := -1;

  sgdDE.RowCount := 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

  edtPY.Clear;
  sgdDE.RowCount := 1;

  pgcDE.ActivePageIndex := 0;
  ShowDataElement;  // 数据元列表

  //值域
  FDomainID := 0;
end;

function TfrmDataElement.GetDeIndex: string;
begin
  if FSelectRow >= 0 then
    Result := sgdDE.Cells[0, FSelectRow]
  else
    Result := '';
end;

function TfrmDataElement.GetDeName: string;
begin
  if FSelectRow > 0 then
    Result := sgdDE.Cells[1, FSelectRow]
  else
    Result := '';
end;

function TfrmDataElement.GetDomainID: Integer;
begin
  if FSelectRow > 0 then
    Result := StrToInt(sgdDE.Cells[5, FSelectRow])
  else
    Result := 0;
end;

procedure TfrmDataElement.lblDeHintClick(Sender: TObject);
begin
  mniRefreshClick(Sender);
end;

procedure TfrmDataElement.mniDeleteClick(Sender: TObject);
begin
  if sgdDE.Row >= 0 then
  begin
    if MessageDlg('确定要删除数据元【' + sgdDE.Cells[1, sgdDE.Row] + '】吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if StrToInt(sgdDE.Cells[5, sgdDE.Row]) <> 0 then
      begin
        if MessageDlg('如果' + sgdDE.Cells[1, sgdDE.Row] + '对应的值域【' + sgdDE.Cells[5, sgdDE.Row] + '】不再使用，请注意及时删除，继续删除数据元？',
          mtWarning, [mbYes, mbNo], 0) <> mrYes
        then
          Exit;
      end;

      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_DELETEDE;  // 删除数据元
          ABLLServerReady.ExecParam.I['DeID'] := StrToInt(sgdDE.Cells[0, sgdDE.Row]);
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if not ABLLServer.MethodRunOk then
            ShowMessage(ABLLServer.MethodError)
          else
          begin
            ShowMessage('删除成功！');

            mniRefreshClick(Sender);
          end;
        end);
    end;
  end;
end;

procedure TfrmDataElement.mniDomainClick(Sender: TObject);
var
  vFrmDomain: TfrmDomain;
begin
  vFrmDomain := TfrmDomain.Create(nil);
  try
    vFrmDomain.PopupParent := Self;
    vFrmDomain.ShowModal;
  finally
    FreeAndNil(vFrmDomain);
  end;
end;

procedure TfrmDataElement.mniEditClick(Sender: TObject);
var
  vFrmDeInfo: TfrmDeInfo;
begin
  if sgdDE.Row > 0 then
  begin
    vFrmDeInfo := TfrmDeInfo.Create(nil);
    try
      vFrmDeInfo.DeID := StrToInt(sgdDE.Cells[0, sgdDE.Row]);
      vFrmDeInfo.PopupParent := Self;
      vFrmDeInfo.ShowModal;
      if vFrmDeInfo.ModalResult = mrOk then
        mniRefreshClick(Sender);
    finally
      FreeAndNil(vFrmDeInfo);
    end;
  end;
end;

procedure TfrmDataElement.mniInsertAsCheckBoxClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeCheckBox) then
    FOnInsertAsDeCheckBox(Self);
end;

procedure TfrmDataElement.mniInsertAsComboboxClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeCombobox) then
    FOnInsertAsDeCombobox(Self);
end;

procedure TfrmDataElement.mniInsertAsDateTimeClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeDateTime) then
    FOnInsertAsDeDateTime(Self);
end;

procedure TfrmDataElement.mniInsertAsDeItemClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeItem) then
    FOnInsertAsDeItem(Self);
end;

procedure TfrmDataElement.mniInsertAsDeGroupClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeGroup) then
    FOnInsertAsDeGroup(Self);
end;

procedure TfrmDataElement.mniInsertAsEditClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeEdit) then
    FOnInsertAsDeEdit(Self);
end;

procedure TfrmDataElement.mniInsertAsRadioGroupClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeRadioGroup) then
    FOnInsertAsDeRadioGroup(Self);
end;

procedure TfrmDataElement.mniInsertAsFloatBarCodeClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeFloatBarCode) then
    FOnInsertAsDeFloatBarCode(Self);
end;

procedure TfrmDataElement.mniInsertAsImageClick(Sender: TObject);
begin
  if sgdDE.Row < 0 then Exit;

  if Assigned(FOnInsertAsDeImage) then
    FOnInsertAsDeImage(Self);
end;

procedure TfrmDataElement.mniNewClick(Sender: TObject);
var
  vFrmDeInfo: TfrmDeInfo;
begin
  vFrmDeInfo := TfrmDeInfo.Create(nil);
  try
    vFrmDeInfo.DeID := 0;
    vFrmDeInfo.PopupParent := Self;
    vFrmDeInfo.ShowModal;
    if vFrmDeInfo.ModalResult = mrOk then
      mniRefreshClick(Sender);
  finally
    FreeAndNil(vFrmDeInfo);
  end;
end;

procedure TfrmDataElement.mniRefreshClick(Sender: TObject);
begin
  edtPY.Clear();

  HintFormShow('正在刷新数据元...', procedure(const AUpdateHint: TUpdateHint)
  var
    vTopRow, vRow: Integer;
  begin
    //FOnFunctionNotify(PluginID, FUN_REFRESHCLIENTCACHE, nil);  // 重新获取客户端缓存
    ClientCache.GetDataElementTable;
    SaveStringGridRow(vRow, vTopRow, sgdDE);
    ShowDataElement;  // 刷新数据元信息
    RestoreStringGridRow(vRow, vTopRow, sgdDE);
  end);
end;

procedure TfrmDataElement.pmDEPopup(Sender: TObject);
begin
  mniEdit.Enabled := sgdDE.Row > 0;
  mniDelete.Enabled := sgdDE.Row > 0;
  mniInsertAsDeItem.Visible := Assigned(FOnInsertAsDeItem) and (sgdDE.Row > 0);
  mniInsertAsDeGroup.Visible := Assigned(FOnInsertAsDeGroup) and (sgdDE.Row > 0);
  mniInsertAsEdit.Visible := Assigned(FOnInsertAsDeEdit) and (sgdDE.Row > 0);
  mniInsertAsCombobox.Visible := Assigned(FOnInsertAsDeCombobox) and (sgdDE.Row > 0);
  mniInsertAsDateTime.Visible := Assigned(FOnInsertAsDeDateTime) and (sgdDE.Row > 0);
  mniInsertAsRadioGroup.Visible := Assigned(FOnInsertAsDeRadioGroup) and (sgdDE.Row > 0);
  mniInsertAsCheckBox.Visible := Assigned(FOnInsertAsDeCheckBox) and (sgdDE.Row > 0);
  mniInsertAsImage.Visible := Assigned(FOnInsertAsDeImage) and (sgdDE.Row > 0);
  mniInsertAsFloatBarCode.Visible := Assigned(FOnInsertAsDeFloatBarCode) and (sgdDE.Row > 0);
end;

procedure TfrmDataElement.sgdDEClick(Sender: TObject);
begin
  if FSelectRow <> sgdDE.Row then
  begin
    FSelectRow := sgdDE.Row;

    if Assigned(FOnSelectChange) then
      FOnSelectChange(Sender);
  end;
end;

procedure TfrmDataElement.sgdDEDblClick(Sender: TObject);
begin
  mniInsertAsDeItemClick(Sender);
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

  sgdDEClick(sgdDE);
end;

end.
