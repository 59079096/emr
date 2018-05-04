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
    lbl1: TLabel;
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
    mniN3: TMenuItem;
    mniN4: TMenuItem;
    pnl3: TPanel;
    lblDE: TLabel;
    mniN5: TMenuItem;
    mniInsetAsDE: TMenuItem;
    mniN2: TMenuItem;
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
    procedure mniN3Click(Sender: TObject);
    procedure mniInsetAsDEClick(Sender: TObject);
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniN2Click(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FDETable: TFDMemTable;
    FOnFunctionNotify: TFunctionNotifyEvent;
    procedure ClearTemplateDeSet;
    procedure GetTemplateDeSet;
    procedure GetDataElement;
    procedure LoadeDETable;
    function GetRecordEditPageIndex(const ATempID: Integer): Integer;
    function GetActiveRecordEdit: TfrmRecordEdit;
    //
    procedure CloseRecordEditPage(const APageIndex: Integer;
      const ASaveChange: Boolean = True);
    procedure DoSaveTempContent(Sender: TObject);
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
  emr_Entry, emr_PluginObject, EmrElementItem, EmrGroupItem, HCCommon,
  EmrView, frm_Item, frm_TemplateInfo;

{$R *.dfm}

procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
begin
  if frmTemplate = nil then
    frmTemplate := TfrmTemplate.Create(Application);

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
  PluginID := PLUGIN_TEMPLATE;
  GClientParam := TClientParam.Create;
  SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
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
  vServerInfo: IServerInfo;
  vUserInfo: IUserInfo;
begin
  // 业务服务端连接参数
  vServerInfo := TServerInfo.Create;
  FOnFunctionNotify(PluginID, FUN_BLLSERVERINFO, vServerInfo);
  GClientParam.BLLServerIP := vServerInfo.Host;
  GClientParam.BLLServerPort := vServerInfo.Port;
  FOnFunctionNotify(PluginID, FUN_MSGSERVERINFO, vServerInfo);
  GClientParam.MsgServerIP := vServerInfo.Host;
  GClientParam.MsgServerPort := vServerInfo.Port;
  // 当前登录用户ID
  vUserInfo := TUserInfoIntf.Create;
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
  if FDETable.Active then
    FDETable.EmptyDataSet;
  sgdDE.RowCount := 0;
  sgdCV.RowCount := 0;

  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)  // 获取患者
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
  sgdDE.RowCount := FDETable.RecordCount + 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

  i := 1;

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

procedure TfrmTemplate.mniInsertAsDGClick(Sender: TObject);
var
  vDeGroup: TDeGroup;
  vFrmRecordEdit: TfrmRecordEdit;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecordEdit := GetActiveRecordEdit;

  if vFrmRecordEdit <> nil then
  begin
    vDeGroup := TDeGroup.Create;
    vDeGroup.Propertys.Add(TDeProp.Index + '=' + sgdDE.Cells[0, sgdDE.Row]);
    vDeGroup.Propertys.Add(TDeProp.Name + '=' + sgdDE.Cells[1, sgdDE.Row]);
    //vDeGroup.Propertys.Add(TDeProp.Code + '=' + sgdDE.Cells[2, sgdDE.Row]);
    vFrmRecordEdit.EmrView.InsertDeGroup(vDeGroup);
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniN3Click(Sender: TObject);
var
  vFrmItem: TfrmItem;
  i, j: Integer;
begin
  if sgdCV.Row < 1 then Exit;
  if sgdCV.Cells[3, sgdCV.Row] = '' then Exit;

  vFrmItem := TfrmItem.Create(nil);
  try
    vFrmItem.sgdDE.RowCount := sgdDE.RowCount;
    vFrmItem.sgdDE.FixedRows := sgdDE.FixedRows;
    for i := 0 to sgdDE.RowCount - 1 do
    begin
      for j := 0 to sgdDE.ColCount - 1 do
        vFrmItem.sgdDE.Cells[j, i] := sgdDE.Cells[j, i];
    end;

    vFrmItem.DomainID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
    vFrmItem.ShowModal;
  finally
    FreeAndNil(vFrmItem);
  end;
end;

procedure TfrmTemplate.mniInsetAsDEClick(Sender: TObject);
var
  vDeItem: TEmrTextItem;
  vFrmRecordEdit: TfrmRecordEdit;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecordEdit := GetActiveRecordEdit;

  if vFrmRecordEdit <> nil then
  begin
    vDeItem := TEmrTextItem.CreateByText(sgdDE.Cells[1, sgdDE.Row]);
    vDeItem[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];
    vDeItem[TDeProp.Name] := sgdDE.Cells[1, sgdDE.Row];
    //vDeItem[TDeProp.Code] := sgdDE.Cells[2, sgdDE.Row];

    vFrmRecordEdit.EmrView.InsertDeItem(vDeItem);
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniViewItemClick(Sender: TObject);
begin
  sgdCV.RowCount := 0;

  if sgdDE.Row < 0 then Exit;

  if sgdDE.Cells[5, sgdDE.Row] <> '' then
  begin
    lblDE.Caption := sgdDE.Cells[1, sgdDE.Row];

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETDATAELEMENTDOMAIN;  // 获取模板分组子分组和模板
        ABLLServerReady.ExecParam.S['domainid'] := sgdDE.Cells[5, sgdDE.Row];
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
          sgdCV.Cells[0, 0] := '值';
          sgdCV.Cells[1, 0] := '编码';
          sgdCV.Cells[2, 0] := '拼音';
          sgdCV.Cells[3, 0] := 'id';

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
        end;
      end);
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
  mniViewItem.Enabled := (sgdDE.Row >= 0) and (sgdDE.Cells[5, sgdDE.Row] <> '');
end;

procedure TfrmTemplate.pmPopup(Sender: TObject);
begin
  mniNewTemp.Enabled := not TreeNodeIsTemplate(tvTemplate.Selected);
  mniDeleteTemp.Enabled := not mniNewTemp.Enabled;
  mniInset.Enabled := not mniNewTemp.Enabled;
end;

procedure TfrmTemplate.sgdDEDblClick(Sender: TObject);
begin
  mniInsetAsDEClick(Sender);
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
      vRecordEdit.Parent := vPage;
      vRecordEdit.Align := alClient;
      vRecordEdit.Show;

      pgRecordEdit.ActivePage := vPage;
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
