{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Template;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FunctionIntf, FunctionImp,
  Vcl.ComCtrls, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus, Data.DB,
  Vcl.StdCtrls, Vcl.Grids, emr_Common, frm_RecordEdit, FireDAC.Comp.Client;

type
  TfrmTemplate = class(TForm)
    spl1: TSplitter;
    pgRecordEdit: TPageControl;
    tsHelp: TTabSheet;
    tvTemplate: TTreeView;
    il: TImageList;
    pm: TPopupMenu;
    mniNewTemp: TMenuItem;
    mniDeleteTemp: TMenuItem;
    pmpg: TPopupMenu;
    mniN1: TMenuItem;
    pnl1: TPanel;
    sgdDE: TStringGrid;
    spl2: TSplitter;
    sgdCV: TStringGrid;
    spl3: TSplitter;
    pmde: TPopupMenu;
    mniInsertAsDG: TMenuItem;
    pnl2: TPanel;
    edtPY: TEdit;
    mniViewItem: TMenuItem;
    mniInset: TMenuItem;
    pmM: TPopupMenu;
    mniEditItemLink: TMenuItem;
    mniDeleteItemLink: TMenuItem;
    pnl3: TPanel;
    lblDE: TLabel;
    mniN5: TMenuItem;
    mniInsertAsDE: TMenuItem;
    mniN2: TMenuItem;
    lblDeHint: TLabel;
    mniN6: TMenuItem;
    mniEdit: TMenuItem;
    mniNew: TMenuItem;
    mniDelete: TMenuItem;
    mniRefresh: TMenuItem;
    mniNewItem: TMenuItem;
    mniEditItem: TMenuItem;
    mniDeleteItem: TMenuItem;
    mniN10: TMenuItem;
    mniN3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvTemplateDblClick(Sender: TObject);
    procedure tvTemplateExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure mniNewTempClick(Sender: TObject);
    procedure pmPopup(Sender: TObject);
    procedure mniDeleteTempClick(Sender: TObject);
    procedure pgRecordEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mniN1Click(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
    procedure mniInsertAsDGClick(Sender: TObject);
    procedure mniViewItemClick(Sender: TObject);
    procedure pmdePopup(Sender: TObject);
    procedure mniInsetClick(Sender: TObject);
    procedure mniEditItemLinkClick(Sender: TObject);
    procedure mniInsertAsDEClick(Sender: TObject);
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniN2Click(Sender: TObject);
    procedure tvTemplateCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure mniEditClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniRefreshClick(Sender: TObject);
    procedure mniNewItemClick(Sender: TObject);
    procedure mniEditItemClick(Sender: TObject);
    procedure pmMPopup(Sender: TObject);
    procedure mniDeleteItemClick(Sender: TObject);
    procedure mniN3Click(Sender: TObject);
    procedure mniDeleteItemLinkClick(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FDETable: TFDMemTable;
    FDomainID: Integer;  // 当前查看的值域ID
    FOnFunctionNotify: TFunctionNotifyEvent;
    procedure ClearTemplateDeSet;
    procedure GetTemplateDeSet;
    procedure GetDataElement;
    procedure LoadeDETable;
    function GetRecordEditPageIndex(const ATempID: Integer): Integer;
    function GetActiveRecordEdit: TfrmRecordEdit;
    procedure GetDomainItem(const ADomainID: Integer);
    //
    procedure CloseRecordEditPage(const APageIndex: Integer;
      const ASaveChange: Boolean = True);
    procedure DoSaveTempContent(Sender: TObject);
    procedure DoRecordChangedSwitch(Sender: TObject);
  public
    { Public declarations }
  end;

  procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
  procedure PluginCloseTemplateForm;

var
  frmTemplate: TfrmTemplate;
  PluginID: string;

implementation

uses
  PluginConst, FunctionConst, emr_BLLConst, emr_BLLServerProxy, emr_MsgPack,
  emr_Entry, emr_PluginObject, EmrElementItem, EmrGroupItem, HCCommon, TemplateCommon,
  EmrView, frm_ItemContent, frm_TemplateInfo, frm_DeInfo, frm_DomainItem, frm_Domain;

{$R *.dfm}

procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
begin
  if frmTemplate = nil then
    frmTemplate := TfrmTemplate.Create(nil);

  frmTemplate.FOnFunctionNotify := AIFun.OnNotifyEvent;
  frmTemplate.Show;
end;

procedure PluginCloseTemplateForm;
begin
  if frmTemplate <> nil then
    FreeAndNil(frmTemplate);
end;

procedure TfrmTemplate.ClearTemplateDeSet;

  {procedure ClearTemplateGroupNode(const ANode: TTreeNode);
  var
    i: Integer;
  begin
    for i := 0 to ANode.Count - 1 do
      ClearTemplateGroupNode(ANode.Item[i]);

    if TObject(ANode.Data) is TTemplateGroupInfo then
      TTemplateGroupInfo(ANode.Data).Free
    else
      TTemplateInfo(ANode.Data).Free;
  end;}

var
  i: Integer;
  vNode: TTreeNode;
begin
  for i := 0 to tvTemplate.Items.Count - 1 do
  begin
    //ClearTemplateGroupNode(tvTemplate.Items[i]);
    vNode := tvTemplate.Items[i];
    if vNode <> nil then
    begin
      if TreeNodeIsTemplate(vNode) then
        TTemplateInfo(vNode.Data).Free
      else
        TDeSetInfo(vNode.Data).Free;
    end;
  end;

  tvTemplate.Items.Clear;
end;

procedure TfrmTemplate.CloseRecordEditPage(const APageIndex: Integer;
  const ASaveChange: Boolean = True);
var
  i: Integer;
  vPage: TTabSheet;
  vFrmRecordEdit: TfrmRecordEdit;
begin
  if APageIndex >= 0 then
  begin
    vPage := pgRecordEdit.Pages[APageIndex];

    for i := 0 to vPage.ControlCount - 1 do
    begin
      if vPage.Controls[i] is TfrmRecordEdit then
      begin
        if ASaveChange then  // 需要检测变动
        begin
          vFrmRecordEdit := (vPage.Controls[i] as TfrmRecordEdit);
          if vFrmRecordEdit.EmrView.IsChanged then  // 有变动
          begin
            if MessageDlg('是否保存模板 ' + TTemplateInfo(vFrmRecordEdit.ObjectData).NameEx + ' ？',
              mtWarning, [mbYes, mbNo], 0) = mrYes
            then
            begin
              DoSaveTempContent(vFrmRecordEdit);
            end;
          end;
        end;

        vPage.Controls[i].Free;
        Break;
      end;
    end;

    vPage.Free;

    if APageIndex > 0 then
      pgRecordEdit.ActivePageIndex := APageIndex - 1;
  end;
end;

procedure TfrmTemplate.DoRecordChangedSwitch(Sender: TObject);
var
  vText: string;
begin
  if (Sender is TfrmRecordEdit) then
  begin
    if (Sender as TfrmRecordEdit).Parent is TTabSheet then
    begin
      if (Sender as TfrmRecordEdit).EmrView.IsChanged then
        vText := TTemplateInfo((Sender as TfrmRecordEdit).ObjectData).NameEx + '*'
      else
        vText := TTemplateInfo((Sender as TfrmRecordEdit).ObjectData).NameEx;

      ((Sender as TfrmRecordEdit).Parent as TTabSheet).Caption := vText;
    end;
  end;
end;

procedure TfrmTemplate.DoSaveTempContent(Sender: TObject);
var
  vSM: TMemoryStream;
  vTempID: Integer;
begin
  vSM := TMemoryStream.Create;
  try
    (Sender as TfrmRecordEdit).EmrView.SaveToStream(vSM);

    vTempID := TTemplateInfo((Sender as TfrmRecordEdit).ObjectData).ID;

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // 获取患者
      begin
        ABLLServerReady.Cmd := BLL_SAVETEMPLATECONTENT;  // 获取模板分组列表
        ABLLServerReady.ExecParam.I['tid'] := TTemplateInfo((Sender as TfrmRecordEdit).ObjectData).ID;
        ABLLServerReady.ExecParam.ForcePathObject('content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
          ShowMessage('保存成功！')
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmTemplate.edtPYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function IsPY(const AChar: Char): Boolean;
  begin
    Result := AChar in ['a'..'z', 'A'..'Z'];
  end;

begin
  if Key = VK_RETURN then
  begin
    FDETable.FilterOptions := [foCaseInsensitive{不区分大小写, foNoPartialCompare不支持通配符(*)所表示的部分匹配}];
    if edtPY.Text = '' then
      FDETable.Filtered := False
    else
    begin
      FDETable.Filtered := False;
      if IsPY(edtPY.Text[1]) then
        FDETable.Filter := 'py like ''%' + edtPY.Text + '%'''
      else
        FDETable.Filter := 'dename like ''%' + edtPY.Text + '%''';
      FDETable.Filtered := True;
    end;

    LoadeDETable;
  end;
end;

procedure TfrmTemplate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
end;

procedure TfrmTemplate.FormCreate(Sender: TObject);
begin
  FDomainID := 0;
  PluginID := PLUGIN_TEMPLATE;
  GClientParam := TClientParam.Create;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
  FUserInfo := TUserInfo.Create;
  FDETable := TFDMemTable.Create(nil);
end;

procedure TfrmTemplate.FormDestroy(Sender: TObject);
var
  i, j: Integer;
begin
  ClearTemplateDeSet;

  for i := 0 to pgRecordEdit.PageCount - 1 do
  begin
    for j := 0 to pgRecordEdit.Pages[i].ControlCount - 1 do
    begin
      if pgRecordEdit.Pages[i].Controls[j] is TfrmRecordEdit then
      begin
        pgRecordEdit.Pages[i].Controls[j].Free;
        Break;
      end;
    end;
  end;

  FreeAndNil(FUserInfo);
  FreeAndNil(GClientParam);
  FreeAndNil(FDETable);
end;

procedure TfrmTemplate.FormShow(Sender: TObject);
var
  vServerInfo: IPluginServerInfo;
  vUserInfo: IPluginUserInfo;
begin
  sgdDE.RowCount := 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

  sgdCV.RowCount := 1;
  sgdCV.Cells[0, 0] := '值';
  sgdCV.Cells[1, 0] := '编码';
  sgdCV.Cells[2, 0] := '拼音';
  sgdCV.Cells[3, 0] := 'id';

  // 业务服务端连接参数
  vServerInfo := TPluginServerInfo.Create;
  FOnFunctionNotify(PluginID, FUN_BLLSERVERINFO, vServerInfo);
  GClientParam.BLLServerIP := vServerInfo.Host;
  GClientParam.BLLServerPort := vServerInfo.Port;
  FOnFunctionNotify(PluginID, FUN_MSGSERVERINFO, vServerInfo);
  GClientParam.MsgServerIP := vServerInfo.Host;
  GClientParam.MsgServerPort := vServerInfo.Port;
  // 当前登录用户ID
  vUserInfo := TPluginUserInfo.Create;
  FOnFunctionNotify(PluginID, FUN_USERINFO, vUserInfo);  // 获取主程序登录用户名
  FUserInfo.ID := vUserInfo.UserID;
  //
  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // 隐藏主窗体

  GetTemplateDeSet;  // 获取模板数据集信息
  GetDataElement;  // 获取数据元信息
end;

function TfrmTemplate.GetActiveRecordEdit: TfrmRecordEdit;
var
  vPage: TTabSheet;
  i: Integer;
begin
  Result := nil;

  vPage := pgRecordEdit.ActivePage;
  for i := 0 to vPage.ControlCount - 1 do
  begin
    if vPage.Controls[i] is TfrmRecordEdit then
    begin
      Result := (vPage.Controls[i] as TfrmRecordEdit);
      Break;
    end;
  end;
end;

procedure TfrmTemplate.GetDataElement;
begin
  edtPY.Clear;
  if FDETable.Active then
    FDETable.EmptyDataSet;
  sgdDE.RowCount := 1;
  sgdCV.RowCount := 1;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENT;  // 获取数据元列表
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
        FDETable.CloneCursor(AMemTable);
    end);

  LoadeDETable;
end;

procedure TfrmTemplate.GetDomainItem(const ADomainID: Integer);
var
  vTopRow, vRow: Integer;
begin
  if ADomainID > 0 then
  begin
    SaveStringGridRow(vRow, vTopRow, sgdCV);

    HintFormShow('正在获取选项...', procedure(const AUpdateHint: TUpdateHint)
    begin
      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)
        begin
          ABLLServerReady.Cmd := BLL_GETDOMAINITEM;  // 获取值域选项
          ABLLServerReady.ExecParam.I['domainid'] := ADomainID;
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

                Next;
                Inc(i);
              end;
            end;

            if AMemTable.RecordCount > 0 then
              sgdCV.FixedRows := 1;

            RestoreStringGridRow(vRow, vTopRow, sgdCV);
          end;
        end);
    end);
  end;
end;

function TfrmTemplate.GetRecordEditPageIndex(const ATempID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to pgRecordEdit.PageCount - 1 do
  begin
    if pgRecordEdit.Pages[i].Tag = ATempID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmTemplate.GetTemplateDeSet;
begin
  ClearTemplateDeSet;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    begin
      ABLLServerReady.Cmd := BLL_GETDATAELEMENTSETALL;  // 获取数据集(全目录)信息
      ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)

      function GetParentNode(const APID: Integer): TTreeNode;
      var
        i: Integer;
      begin
        Result := nil;
        for i := 0 to tvTemplate.Items.Count - 1 do
        begin
          if tvTemplate.Items[i].Data <> nil then
          begin
            if TDeSetInfo(tvTemplate.Items[i].Data).ID = APID then
            begin
              Result := tvTemplate.Items[i];
              Break;
            end;
          end;
        end;
      end;

    var
      vNode: TTreeNode;
      vDeSetInfo: TDeSetInfo;
    begin
      if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      if AMemTable <> nil then
      begin
        tvTemplate.Items.BeginUpdate;
        try
          with AMemTable do
          begin
            First;
            while not Eof do
            begin
              vDeSetInfo := TDeSetInfo.Create;
              vDeSetInfo.ID := FieldByName('id').AsInteger;
              vDeSetInfo.PID := FieldByName('pid').AsInteger;
              vDeSetInfo.GroupClass := FieldByName('Class').AsInteger;
              vDeSetInfo.GroupType := FieldByName('Type').AsInteger;
              vDeSetInfo.GroupName := FieldByName('Name').AsString;

              if vDeSetInfo.PID <> 0 then
              begin
                vNode := tvTemplate.Items.AddChildObject(GetParentNode(vDeSetInfo.PID),
                  vDeSetInfo.GroupName, vDeSetInfo)
              end
              else
                vNode := tvTemplate.Items.AddObject(nil, vDeSetInfo.GroupName, vDeSetInfo);

              vNode.HasChildren := True;

              Next;
            end;
          end;
        finally
          tvTemplate.Items.EndUpdate;
        end;
      end;
    end);
end;

procedure TfrmTemplate.LoadeDETable;
var
  i: Integer;
begin
  i := 1;
  sgdDE.RowCount := FDETable.RecordCount + 1;

  with FDETable do
  begin
    First;
    while not Eof do
    begin
      sgdDE.Cells[0, i] := FieldByName('deid').AsString;
      sgdDE.Cells[1, i] := FieldByName('dename').AsString;
      sgdDE.Cells[2, i] := FieldByName('decode').AsString;
      sgdDE.Cells[3, i] := FieldByName('py').AsString;
      sgdDE.Cells[4, i] := FieldByName('frmtp').AsString;
      sgdDE.Cells[5, i] := FieldByName('domainid').AsString;

      Next;
      Inc(i);
    end;
  end;

  if sgdDE.RowCount > 1 then
    sgdDE.FixedRows := 1;
end;

procedure TfrmTemplate.mniDeleteTempClick(Sender: TObject);
var
  vTempID: Integer;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    if MessageDlg('确定要删除模板 ' + TTemplateInfo(tvTemplate.Selected.Data).NameEx + ' ？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      vTempID := TTemplateInfo(tvTemplate.Selected.Data).ID;

      BLLServerExec(
        procedure(const ABLLServerReady: TBLLServerProxy)  // 获取模板分组子分组和模板
        begin
          ABLLServerReady.Cmd := BLL_DELETETEMPLATE;  // 新建模板
          ABLLServerReady.ExecParam.I['tid'] := vTempID;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  // 删除成功
          begin
            tvTemplate.Items.Delete(tvTemplate.Selected);  // 删除节点
            vTempID := GetRecordEditPageIndex(vTempID);
            if vTempID >= 0 then
              CloseRecordEditPage(vTempID, False);
          end
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end;
  end;
end;

procedure TfrmTemplate.mniInsetClick(Sender: TObject);
var
  vNode: TTreeNode;
  vRecordEdit: TfrmRecordEdit;
  vSM: TMemoryStream;
  vEmrView: TEmrView;
  vGroupClass: Integer;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vRecordEdit := GetActiveRecordEdit;

    if vRecordEdit <> nil then
    begin
      vNode := tvTemplate.Selected;

      vSM := TMemoryStream.Create;
      try
        GetTemplateContent(TTemplateInfo(vNode.Data).ID, vSM);

        while vNode.Parent <> nil do
          vNode := vNode.Parent;

        vGroupClass := TDeSetInfo(vNode.Data).GroupClass;
        case vGroupClass of
          TDeSetInfo.CLASS_DATA:  // 正文
            begin
              vSM.Position := 0;
              vRecordEdit.EmrView.InsertStream(vSM);
            end;

          TDeSetInfo.CLASS_HEADER,  // 页眉、页脚
          TDeSetInfo.CLASS_FOOTER:
            begin
              vEmrView := TEmrView.Create(nil);
              try
                vEmrView.LoadFromStream(vSM);
                vSM.Clear;
                vEmrView.Sections[0].Header.SaveToStream(vSM);
                vSM.Position := 0;
                if vGroupClass = TDeSetInfo.CLASS_HEADER then
                  vRecordEdit.EmrView.ActiveSection.Header.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt)
                else
                  vRecordEdit.EmrView.ActiveSection.Footer.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt);

                vRecordEdit.EmrView.IsChanged := True;
                vRecordEdit.EmrView.UpdateBuffer;
              finally
                FreeAndNil(vEmrView);
              end;
            end;
        end;
      finally
        FreeAndNil(vSM);
      end;
    end;
  end;
end;

procedure TfrmTemplate.mniRefreshClick(Sender: TObject);
begin
  HintFormShow('正在刷新数据元...', procedure(const AUpdateHint: TUpdateHint)
  var
    vTopRow, vRow: Integer;
  begin
    SaveStringGridRow(vRow, vTopRow, sgdDE);
    GetDataElement;  // 刷新数据元信息
    RestoreStringGridRow(vRow, vTopRow, sgdDE);
  end);
end;

procedure TfrmTemplate.mniN1Click(Sender: TObject);
begin
  CloseRecordEditPage(pgRecordEdit.ActivePageIndex);
end;

procedure TfrmTemplate.mniN2Click(Sender: TObject);
var
  vFrmTemplateInfo: TfrmTemplateInfo;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vFrmTemplateInfo := TfrmTemplateInfo.Create(nil);
    try
      vFrmTemplateInfo.TempID := TTemplateInfo(tvTemplate.Selected.Data).ID;
      vFrmTemplateInfo.ShowModal;

      TTemplateInfo(tvTemplate.Selected.Data).NameEx := vFrmTemplateInfo.TempName;
      tvTemplate.Selected.Text := vFrmTemplateInfo.TempName;
    finally
      FreeAndNil(vFrmTemplateInfo);
    end;
  end;
end;

procedure TfrmTemplate.mniN3Click(Sender: TObject);
var
  vFrmDomain: TfrmDomain;
begin
  vFrmDomain := TfrmDomain.Create(nil);
  try
    vFrmDomain.ShowModal;
  finally
    FreeAndNil(vFrmDomain);
  end;
end;

procedure TfrmTemplate.mniInsertAsDGClick(Sender: TObject);
var
  vDeGroup: TDeGroup;
  vFrmRecordEdit: TfrmRecordEdit;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecordEdit := GetActiveRecordEdit;

  if vFrmRecordEdit <> nil then
  begin
    vDeGroup := TDeGroup.Create(vFrmRecordEdit.EmrView.ActiveSection.ActiveData.GetTopLevelData);
    try
      vDeGroup.Propertys.Add(TDeProp.Index + '=' + sgdDE.Cells[0, sgdDE.Row]);
      vDeGroup.Propertys.Add(TDeProp.Name + '=' + sgdDE.Cells[1, sgdDE.Row]);
      //vDeGroup.Propertys.Add(TDeProp.Code + '=' + sgdDE.Cells[2, sgdDE.Row]);
      vFrmRecordEdit.EmrView.InsertDeGroup(vDeGroup);
    finally
      vDeGroup.Free;
    end;
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniEditItemLinkClick(Sender: TObject);
var
  vFrmItemContent: TfrmItemContent;
begin
  if sgdCV.Row < 1 then Exit;
  if sgdCV.Cells[3, sgdCV.Row] = '' then Exit;

  vFrmItemContent := TfrmItemContent.Create(nil);
  try
    vFrmItemContent.DETable.CloneCursor(FDETable);
    vFrmItemContent.DomainItemID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
    vFrmItemContent.ShowModal;
  finally
    FreeAndNil(vFrmItemContent);
  end;
end;

procedure TfrmTemplate.mniEditClick(Sender: TObject);
var
  vFrmDeList: TfrmDeInfo;
begin
  if sgdDE.Row > 0 then
  begin
    vFrmDeList := TfrmDeInfo.Create(nil);
    try
      vFrmDeList.DeID := StrToInt(sgdDE.Cells[0, sgdDE.Row]);
      vFrmDeList.ShowModal;
      if vFrmDeList.ModalResult = mrOk then
        mniRefreshClick(Sender);
    finally
      FreeAndNil(vFrmDeList);
    end;
  end;
end;

procedure TfrmTemplate.mniEditItemClick(Sender: TObject);
var
  vFrmDomainItem: TfrmDomainItem;
begin
  if (sgdCV.Row > 0) and (FDomainID > 0) then
  begin
    vFrmDomainItem := TfrmDomainItem.Create(nil);
    try
      vFrmDomainItem.DomainID := FDomainID;
      vFrmDomainItem.ItemID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
      vFrmDomainItem.ShowModal;
      if vFrmDomainItem.ModalResult = mrOk then
        GetDomainItem(FDomainID);
    finally
      FreeAndNil(vFrmDomainItem);
    end;
  end;
end;

procedure TfrmTemplate.mniNewClick(Sender: TObject);
var
  vFrmDeList: TfrmDeInfo;
begin
  vFrmDeList := TfrmDeInfo.Create(nil);
  try
    vFrmDeList.DeID := 0;
    vFrmDeList.ShowModal;
    if vFrmDeList.ModalResult = mrOk then
      mniRefreshClick(Sender);
  finally
    FreeAndNil(vFrmDeList);
  end;
end;

procedure TfrmTemplate.mniNewItemClick(Sender: TObject);
var
  vFrmDomainItem: TfrmDomainItem;
begin
  if FDomainID > 0 then
  begin
    vFrmDomainItem := TfrmDomainItem.Create(nil);
    try
      vFrmDomainItem.DomainID := FDomainID;
      vFrmDomainItem.ItemID := 0;
      vFrmDomainItem.ShowModal;
      if vFrmDomainItem.ModalResult = mrOk then
        GetDomainItem(FDomainID);
    finally
      FreeAndNil(vFrmDomainItem);
    end;
  end;
end;

procedure TfrmTemplate.mniDeleteClick(Sender: TObject);
begin
  if sgdDE.Row >= 0 then
  begin
    if MessageDlg('确定要删除数据元【' + sgdDE.Cells[1, sgdDE.Row] + '】吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if StrToInt(sgdDE.Cells[5, sgdDE.Row]) <> 0 then
      begin
      if MessageDlg('如果' + sgdDE.Cells[1, sgdDE.Row] + '对应的值域【' + sgdDE.Cells[5, sgdDE.Row] + '】不再使用，请注意及时删除！',
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

procedure TfrmTemplate.mniDeleteItemClick(Sender: TObject);
var
  vDeleteOk: Boolean;
begin
  if sgdCV.Row >= 0 then
  begin
    if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】及其关联的内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      vDeleteOk := True;

      // 删除关联
      vDeleteOk := DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row]));
      if vDeleteOk then
        ShowMessage('删除选项关联内容成功！')
      else
        ShowMessage(CommonLastError);

      if not vDeleteOk then Exit;

      // 删除选项
      vDeleteOk := DeleteDomainItem(StrToInt(sgdCV.Cells[3, sgdCV.Row]));
      if vDeleteOk then
      begin
        ShowMessage('删除选项成功！');
        GetDomainItem(FDomainID);
      end
      else
        ShowMessage(CommonLastError);
    end;
  end;
end;

procedure TfrmTemplate.mniDeleteItemLinkClick(Sender: TObject);
begin
  if sgdCV.Row >= 0 then
  begin
    if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】关联的内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row])) then
        ShowMessage('删除值域选项关联内容成功！')
      else
        ShowMessage(CommonLastError);
    end;
  end;
end;

procedure TfrmTemplate.mniInsertAsDEClick(Sender: TObject);
var
  vDeItem: TDeItem;
  vFrmRecordEdit: TfrmRecordEdit;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecordEdit := GetActiveRecordEdit;

  if vFrmRecordEdit <> nil then
  begin
    vDeItem := vFrmRecordEdit.EmrView.NewDeItem(sgdDE.Cells[1, sgdDE.Row]);
    vDeItem[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];
    vDeItem[TDeProp.Name] := sgdDE.Cells[1, sgdDE.Row];

    if not vFrmRecordEdit.EmrView.Focused then  // 先给焦点，便于处理光标处域
      vFrmRecordEdit.EmrView.SetFocus;

    vFrmRecordEdit.EmrView.InsertDeItem(vDeItem);
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniViewItemClick(Sender: TObject);
begin
  if sgdDE.Row > 0 then
  begin
    if sgdDE.Cells[5, sgdDE.Row] <> '' then
      FDomainID := StrToInt(sgdDE.Cells[5, sgdDE.Row])
    else
      FDomainID := 0;

    lblDE.Caption := sgdDE.Cells[1, sgdDE.Row];

    GetDomainItem(FDomainID);
  end;
end;

procedure TfrmTemplate.mniNewTempClick(Sender: TObject);
var
  vTName: string;
begin
  vTName := InputBox('新建', '模板名称', '');
  if vTName.Trim = '' then Exit;

  // 菜单可显示时已经确认选择在组节点上了
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)  // 获取模板分组子分组和模板
    begin
      ABLLServerReady.Cmd := BLL_NEWTEMPLATE;  // 新建模板

      if TreeNodeIsTemplate(tvTemplate.Selected) then
        ABLLServerReady.ExecParam.I['desid'] := TTemplateInfo(tvTemplate.Selected.Data).DesID
      else
        ABLLServerReady.ExecParam.I['desid'] := TDeSetInfo(tvTemplate.Selected.Data).ID;
      ABLLServerReady.ExecParam.I['owner'] := 1;
      ABLLServerReady.ExecParam.I['ownerid'] := 0;
      ABLLServerReady.ExecParam.ForcePathObject('tname').AsString := vTName;
      //
      ABLLServerReady.AddBackField('tempid');
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
    var
      vTempInfo: TTemplateInfo;
    begin
      if ABLLServer.MethodRunOk then  // 新建模板执行成功
      begin
        vTempInfo := TTemplateInfo.Create;
        vTempInfo.ID := ABLLServer.BackField('tempid').AsInteger;
        vTempInfo.Owner := 1;
        vTempInfo.OwnerID := 0;
        vTempInfo.NameEx := vTName;
        tvTemplate.Selected := tvTemplate.Items.AddChildObject(tvTemplate.Selected, vTempInfo.NameEx, vTempInfo);
      end
      else
        ShowMessage(ABLLServer.MethodError);
    end);
end;

procedure TfrmTemplate.pgRecordEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vTabIndex: Integer;
  vPt: TPoint;
begin
  if (Y < 20) and (Button = TMouseButton.mbRight) then  // 默认的 pgRecordEdit.TabHeight 可通过获取操作系统参数得到更精确的
  begin
    vTabIndex := pgRecordEdit.IndexOfTabAt(X, Y);

    if pgRecordEdit.Pages[vTabIndex].Tag = 0 then Exit; // 帮助

    if (vTabIndex >= 0) and (vTabIndex = pgRecordEdit.ActivePageIndex) then
    begin
      vPt := pgRecordEdit.ClientToScreen(Point(X, Y));
      pmpg.Popup(vPt.X, vPt.Y);
    end;
  end;
end;

procedure TfrmTemplate.pmdePopup(Sender: TObject);
begin
  mniViewItem.Enabled := (sgdDE.Row > 0)
    and ((sgdDE.Cells[4, sgdDE.Row] = TDeFrmtp.Radio)
         or (sgdDE.Cells[4, sgdDE.Row] = TDeFrmtp.Multiselect));
  mniEdit.Enabled := sgdDE.Row > 0;
  mniDelete.Enabled := sgdDE.Row > 0;
  mniInsertAsDE.Enabled := sgdDE.Row > 0;
  mniInsertAsDG.Enabled := sgdDE.Row > 0;
end;

procedure TfrmTemplate.pmMPopup(Sender: TObject);
begin
  mniNewItem.Enabled := FDomainID > 0;
  mniEditItem.Visible := sgdCV.Row > 0;
  mniDeleteItem.Visible := sgdCV.Row > 0;
  mniEditItemLink.Visible := sgdCV.Row > 0;
  mniDeleteItemLink.Visible := sgdCV.Row > 0;
end;

procedure TfrmTemplate.pmPopup(Sender: TObject);
begin
  mniNewTemp.Enabled := not TreeNodeIsTemplate(tvTemplate.Selected);
  mniDeleteTemp.Enabled := not mniNewTemp.Enabled;
  mniInset.Enabled := not mniNewTemp.Enabled;
end;

procedure TfrmTemplate.sgdDEDblClick(Sender: TObject);
begin
  mniInsertAsDEClick(Sender);
end;

procedure TfrmTemplate.tvTemplateCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if (not TreeNodeIsTemplate(Node)) and (TDeSetInfo(Node.Data).ID = 74) then  // 测试用
  begin
    tvTemplate.Canvas.Font.Style := [fsBold];
    tvTemplate.Canvas.Font.Color := clRed;
  end;
end;

procedure TfrmTemplate.tvTemplateDblClick(Sender: TObject);
var
  vPageIndex, vTempID: Integer;
  vPage: TTabSheet;
  vRecordEdit: TfrmRecordEdit;
  vSM: TMemoryStream;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vTempID := TTemplateInfo(tvTemplate.Selected.Data).ID;
    vPageIndex := GetRecordEditPageIndex(vTempID);
    if vPageIndex >= 0 then
    begin
      pgRecordEdit.ActivePageIndex := vPageIndex;
      Exit;
    end;

    vSM := TMemoryStream.Create;
    try
      GetTemplateContent(vTempID, vSM);

      vRecordEdit := TfrmRecordEdit.Create(nil);  // 创建编辑器
      vRecordEdit.ObjectData := tvTemplate.Selected.Data;
      if vSM.Size > 0 then
        vRecordEdit.EmrView.LoadFromStream(vSM);
    finally
      vSM.Free;
    end;

    if vRecordEdit <> nil then
    begin
      vPage := TTabSheet.Create(pgRecordEdit);
      vPage.Caption := tvTemplate.Selected.Text;
      vPage.Tag := vTempID;
      vPage.PageControl := pgRecordEdit;

      vRecordEdit.OnSave := DoSaveTempContent;
      vRecordEdit.OnChangedSwitch := DoRecordChangedSwitch;
      vRecordEdit.Parent := vPage;
      vRecordEdit.Align := alClient;
      vRecordEdit.Show;

      pgRecordEdit.ActivePage := vPage;

      vRecordEdit.EmrView.SetFocus;
    end;
  end;
end;

procedure TfrmTemplate.tvTemplateExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.Count = 0 then
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETTEMPLATELIST;  // 获取模板分组子分组和模板
        ABLLServerReady.ExecParam.I['desID'] := TDeSetInfo(Node.Data).ID;
        ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        vTempInfo: TTemplateInfo;
      begin
        if not ABLLServer.MethodRunOk then  // 服务端方法返回执行不成功
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;

        if AMemTable <> nil then
        begin
          tvTemplate.Items.BeginUpdate;
          try
            if AMemTable.RecordCount > 0 then
            begin
              with AMemTable do
              begin
                First;
                while not Eof do
                begin
                  vTempInfo := TTemplateInfo.Create;
                  vTempInfo.ID := FieldByName('id').AsInteger;
                  vTempInfo.DesID := FieldByName('desid').AsInteger;
                  vTempInfo.Owner := FieldByName('Owner').AsInteger;
                  vTempInfo.OwnerID := FieldByName('OwnerID').AsInteger;
                  vTempInfo.NameEx := FieldByName('tname').AsString;
                  tvTemplate.Items.AddChildObject(Node, vTempInfo.NameEx, vTempInfo);

                  Next;
                end;
              end;
            end
            else
              Node.HasChildren := False;
          finally
            tvTemplate.Items.EndUpdate;
          end;
        end;
      end);
  end;
end;

end.
