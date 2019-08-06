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
  Vcl.StdCtrls, Vcl.Grids, emr_Common, frm_Record, FireDAC.Comp.Client;

type
  TfrmTemplate = class(TForm)
    spl1: TSplitter;
    pgTemplate: TPageControl;
    tsHelp: TTabSheet;
    tvTemplate: TTreeView;
    il: TImageList;
    pmTemplate: TPopupMenu;
    mniNewTemplate: TMenuItem;
    mniDeleteTemplate: TMenuItem;
    pmpg: TPopupMenu;
    mniCloseTemplate: TMenuItem;
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
    mniInsertTemplate: TMenuItem;
    pmCV: TPopupMenu;
    mniEditItemLink: TMenuItem;
    mniDeleteItemLink: TMenuItem;
    pnl3: TPanel;
    lblDE: TLabel;
    mniN5: TMenuItem;
    mniInsertAsDE: TMenuItem;
    mniTemplateProperty: TMenuItem;
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
    mniDomain: TMenuItem;
    mniInsertAsEdit: TMenuItem;
    mniInsertAsCombobox: TMenuItem;
    mniN4: TMenuItem;
    mniCloseAll: TMenuItem;
    mniInsertAsDateTime: TMenuItem;
    mniInsertAsRadioGroup: TMenuItem;
    mniInsertAsCheckBox: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvTemplateDblClick(Sender: TObject);
    procedure tvTemplateExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure mniNewTemplateClick(Sender: TObject);
    procedure pmTemplatePopup(Sender: TObject);
    procedure mniDeleteTemplateClick(Sender: TObject);
    procedure pgTemplateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mniCloseTemplateClick(Sender: TObject);
    procedure sgdDEDblClick(Sender: TObject);
    procedure mniInsertAsDGClick(Sender: TObject);
    procedure mniViewItemClick(Sender: TObject);
    procedure pmdePopup(Sender: TObject);
    procedure mniInsertTemplateClick(Sender: TObject);
    procedure mniEditItemLinkClick(Sender: TObject);
    procedure mniInsertAsDEClick(Sender: TObject);
    procedure edtPYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mniTemplatePropertyClick(Sender: TObject);
    procedure tvTemplateCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure mniEditClick(Sender: TObject);
    procedure mniNewClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniNewItemClick(Sender: TObject);
    procedure mniEditItemClick(Sender: TObject);
    procedure pmCVPopup(Sender: TObject);
    procedure mniDeleteItemClick(Sender: TObject);
    procedure mniDomainClick(Sender: TObject);
    procedure mniDeleteItemLinkClick(Sender: TObject);
    procedure mniInsertAsComboboxClick(Sender: TObject);
    procedure mniCloseAllClick(Sender: TObject);
    procedure lblDeHintClick(Sender: TObject);
    procedure lblDEClick(Sender: TObject);
    procedure mniRefreshClick(Sender: TObject);
    procedure mniInsertAsDateTimeClick(Sender: TObject);
    procedure mniInsertAsRadioGroupClick(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FDomainID: Integer;  // 当前查看的值域ID
    FOnFunctionNotify: TFunctionNotifyEvent;
    procedure ClearTemplateDeSet;
    procedure ShowTemplateDeSet;
    procedure ShowAllDataElement;
    procedure ShowDataElement;
    function GetRecordEditPageIndex(const ATempID: Integer): Integer;
    function GetActiveRecord: TfrmRecord;
    procedure GetDomainItem(const ADomainID: Integer);
    //
    procedure CloseTemplatePage(const APageIndex: Integer;
      const ASaveChange: Boolean = True);
    procedure DoSaveTempContent(Sender: TObject);
    procedure DoRecordChangedSwitch(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
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
  Vcl.Clipbrd, PluginConst, FunctionConst, emr_BLLServerProxy, emr_MsgPack,
  emr_Entry, HCEmrElementItem, HCEmrGroupItem, HCCommon, TemplateCommon, CFBalloonHint,
  HCEmrView, frm_ItemContent, frm_TemplateInfo, frm_DeInfo, frm_DomainItem, frm_Domain;

{$R *.dfm}

procedure PluginShowTemplateForm(AIFun: IFunBLLFormShow);
begin
  if not Assigned(frmTemplate) then
    Application.CreateForm(TfrmTemplate, frmTemplate);

  frmTemplate.FOnFunctionNotify := AIFun.OnNotifyEvent;
  frmTemplate.Show;
end;

procedure PluginCloseTemplateForm;
begin
  if Assigned(frmTemplate) then
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
        TDataSetInfo(vNode.Data).Free;
    end;
  end;

  tvTemplate.Items.Clear;
end;

procedure TfrmTemplate.CloseTemplatePage(const APageIndex: Integer;
  const ASaveChange: Boolean = True);
var
  i: Integer;
  vPage: TTabSheet;
  vFrmRecord: TfrmRecord;
begin
  if APageIndex >= 0 then
  begin
    vPage := pgTemplate.Pages[APageIndex];

    for i := 0 to vPage.ControlCount - 1 do
    begin
      if vPage.Controls[i] is TfrmRecord then
      begin
        if ASaveChange then  // 需要检测变动
        begin
          vFrmRecord := (vPage.Controls[i] as TfrmRecord);
          if vFrmRecord.EmrView.IsChanged then  // 有变动
          begin
            if MessageDlg('是否保存模板 ' + TTemplateInfo(vFrmRecord.ObjectData).NameEx + ' ？',
              mtWarning, [mbYes, mbNo], 0) = mrYes
            then
            begin
              DoSaveTempContent(vFrmRecord);
            end;
          end;
        end;

        vPage.Controls[i].Free;
        Break;
      end;
    end;

    vPage.Free;

    if APageIndex > 0 then
      pgTemplate.ActivePageIndex := APageIndex - 1;
  end;
end;

procedure TfrmTemplate.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TfrmTemplate.DoRecordChangedSwitch(Sender: TObject);
var
  vText: string;
begin
  if (Sender is TfrmRecord) then
  begin
    if (Sender as TfrmRecord).Parent is TTabSheet then
    begin
      if (Sender as TfrmRecord).EmrView.IsChanged then
        vText := TTemplateInfo((Sender as TfrmRecord).ObjectData).NameEx + '*'
      else
        vText := TTemplateInfo((Sender as TfrmRecord).ObjectData).NameEx;

      ((Sender as TfrmRecord).Parent as TTabSheet).Caption := vText;
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
    (Sender as TfrmRecord).EmrView.SaveToStream(vSM);

    vTempID := TTemplateInfo((Sender as TfrmRecord).ObjectData).ID;

    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)  // 获取患者
      begin
        ABLLServerReady.Cmd := BLL_SAVETEMPLATECONTENT;  // 获取模板分组列表
        ABLLServerReady.ExecParam.I['tid'] := vTempID;
        ABLLServerReady.ExecParam.ForcePathObject('content').LoadBinaryFromStream(vSM);
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      begin
        if ABLLServer.MethodRunOk then  // 服务端方法返回执行成功
        begin
          (Sender as TfrmRecord).EmrView.IsChanged := False;  // 保存后文档标识为非修改
          ShowMessage('保存成功！');
        end
        else
          ShowMessage(ABLLServer.MethodError);
      end);
  finally
    FreeAndNil(vSM);
  end;
end;

procedure TfrmTemplate.edtPYKeyDown(Sender: TObject; var Key: Word;
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

procedure TfrmTemplate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FOnFunctionNotify(PluginID, FUN_MAINFORMSHOW, nil);  // 显示主窗体
  FOnFunctionNotify(PluginID, FUN_BLLFORMDESTROY, nil);  // 释放业务窗体资源
end;

procedure TfrmTemplate.FormCreate(Sender: TObject);
begin
  FDomainID := 0;
  PluginID := PLUGIN_TEMPLATE;
  //SetWindowLong(Handle, GWL_EXSTYLE, (GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_APPWINDOW));
end;

procedure TfrmTemplate.FormDestroy(Sender: TObject);
var
  i, j: Integer;
begin
  ClearTemplateDeSet;

  for i := 0 to pgTemplate.PageCount - 1 do
  begin
    for j := 0 to pgTemplate.Pages[i].ControlCount - 1 do
    begin
      if pgTemplate.Pages[i].Controls[j] is TfrmRecord then
      begin
        pgTemplate.Pages[i].Controls[j].Free;
        Break;
      end;
    end;
  end;
end;

procedure TfrmTemplate.FormShow(Sender: TObject);
var
  vObjFun: IObjectFunction;
begin
  sgdDE.RowCount := 1;
  sgdDE.Cells[0, 0] := '序';
  sgdDE.Cells[1, 0] := '名称';
  sgdDE.Cells[2, 0] := '编码';
  sgdDE.Cells[3, 0] := '拼音';
  sgdDE.Cells[4, 0] := '类型';
  sgdDE.Cells[5, 0] := '值域';

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

  // 获取客户缓存对象
  vObjFun := TObjectFunction.Create;
  FOnFunctionNotify(PluginID, FUN_CLIENTCACHE, vObjFun);
  ClientCache := TClientCache(vObjFun.&Object);

  // 当前登录用户ID
  FOnFunctionNotify(PluginID, FUN_USERINFO, vObjFun);  // 获取主程序登录用户名
  FUserInfo := TUserInfo(vObjFun.&Object);
  //
  FOnFunctionNotify(PluginID, FUN_MAINFORMHIDE, nil);  // 隐藏主窗体

  ShowTemplateDeSet;  // 获取并显示模板数据集信息
  ShowAllDataElement;  // 显示数据元信息
end;

function TfrmTemplate.GetActiveRecord: TfrmRecord;
var
  vPage: TTabSheet;
  i: Integer;
begin
  Result := nil;

  vPage := pgTemplate.ActivePage;
  for i := 0 to vPage.ControlCount - 1 do
  begin
    if vPage.Controls[i] is TfrmRecord then
    begin
      Result := (vPage.Controls[i] as TfrmRecord);
      Break;
    end;
  end;
end;

procedure TfrmTemplate.ShowAllDataElement;
begin
  edtPY.Clear;
  sgdDE.RowCount := 1;
  sgdCV.RowCount := 1;
  ShowDataElement;
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
    end);
  end;
end;

function TfrmTemplate.GetRecordEditPageIndex(const ATempID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to pgTemplate.PageCount - 1 do
  begin
    if pgTemplate.Pages[i].Tag = ATempID then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TfrmTemplate.lblDEClick(Sender: TObject);
begin
  mniViewItemClick(Sender);
end;

procedure TfrmTemplate.lblDeHintClick(Sender: TObject);
begin
  mniRefreshClick(Sender);
end;

procedure TfrmTemplate.ShowTemplateDeSet;
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
            if TDataSetInfo(tvTemplate.Items[i].Data).ID = APID then
            begin
              Result := tvTemplate.Items[i];
              Break;
            end;
          end;
        end;
      end;

    var
      vNode: TTreeNode;
      vDataSetInfo: TDataSetInfo;
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
              vDataSetInfo := TDataSetInfo.Create;
              vDataSetInfo.ID := FieldByName('id').AsInteger;
              vDataSetInfo.PID := FieldByName('pid').AsInteger;
              vDataSetInfo.GroupClass := FieldByName('Class').AsInteger;
              vDataSetInfo.GroupType := FieldByName('Type').AsInteger;
              vDataSetInfo.GroupName := FieldByName('Name').AsString;

              if vDataSetInfo.PID <> 0 then
              begin
                vNode := tvTemplate.Items.AddChildObject(GetParentNode(vDataSetInfo.PID),
                  vDataSetInfo.GroupName, vDataSetInfo)
              end
              else
                vNode := tvTemplate.Items.AddObject(nil, vDataSetInfo.GroupName, vDataSetInfo);

              vNode.HasChildren := True;
              vNode.ImageIndex := -1;
              vNode.SelectedIndex := -1;

              Next;
            end;
          end;
        finally
          tvTemplate.Items.EndUpdate;
        end;
      end;
    end);
end;

procedure TfrmTemplate.ShowDataElement;
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

procedure TfrmTemplate.mniDeleteTemplateClick(Sender: TObject);
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
          ABLLServerReady.Cmd := BLL_DELETETEMPLATE;  // 删除模板
          ABLLServerReady.ExecParam.I['tid'] := vTempID;
        end,
        procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
        begin
          if ABLLServer.MethodRunOk then  // 删除成功
          begin
            tvTemplate.Items.Delete(tvTemplate.Selected);  // 删除节点
            vTempID := GetRecordEditPageIndex(vTempID);
            if vTempID >= 0 then
              CloseTemplatePage(vTempID, False);
          end
          else
            ShowMessage(ABLLServer.MethodError);
        end);
    end;
  end;
end;

procedure TfrmTemplate.mniInsertTemplateClick(Sender: TObject);
var
  vNode: TTreeNode;
  vFrmRecord: TfrmRecord;
  vSM: TMemoryStream;
  vEmrView: THCEmrView;
  vGroupClass: Integer;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vFrmRecord := GetActiveRecord;

    if Assigned(vFrmRecord) then
    begin
      vNode := tvTemplate.Selected;

      vSM := TMemoryStream.Create;
      try
        GetTemplateContent(TTemplateInfo(vNode.Data).ID, vSM);

        while vNode.Parent <> nil do
          vNode := vNode.Parent;

        vGroupClass := TDataSetInfo(vNode.Data).GroupClass;
        case vGroupClass of
          TDataSetInfo.CLASS_PAGE:  // 正文
            begin
              vSM.Position := 0;
              vFrmRecord.EmrView.InsertStream(vSM);
            end;

          TDataSetInfo.CLASS_HEADER,  // 页眉、页脚
          TDataSetInfo.CLASS_FOOTER:
            begin
              vEmrView := THCEmrView.Create(nil);
              try
                vEmrView.LoadFromStream(vSM);
                vSM.Clear;
                vEmrView.Sections[0].Header.SaveToStream(vSM);
                vSM.Position := 0;
                if vGroupClass = TDataSetInfo.CLASS_HEADER then
                  vFrmRecord.EmrView.ActiveSection.Header.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt)
                else
                  vFrmRecord.EmrView.ActiveSection.Footer.LoadFromStream(vSM, vEmrView.Style, HC_FileVersionInt);

                vFrmRecord.EmrView.IsChanged := True;
                vFrmRecord.EmrView.UpdateView;
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

procedure TfrmTemplate.mniCloseTemplateClick(Sender: TObject);
begin
  CloseTemplatePage(pgTemplate.ActivePageIndex);
end;

procedure TfrmTemplate.mniTemplatePropertyClick(Sender: TObject);
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

procedure TfrmTemplate.mniDomainClick(Sender: TObject);
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

procedure TfrmTemplate.mniCloseAllClick(Sender: TObject);
begin
  while pgTemplate.PageCount > 1 do
    CloseTemplatePage(pgTemplate.PageCount - 1);
end;

procedure TfrmTemplate.mniInsertAsDGClick(Sender: TObject);
var
  vDeGroup: TDeGroup;
  vFrmRecord: TfrmRecord;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecord := GetActiveRecord;

  if Assigned(vFrmRecord) then
  begin
    vDeGroup := TDeGroup.Create(vFrmRecord.EmrView.ActiveSectionTopLevelData);  // 只为记录属性
    try
      vDeGroup[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];
      vDeGroup[TDeProp.Name] := sgdDE.Cells[1, sgdDE.Row];

      if not vFrmRecord.EmrView.Focused then  // 先给焦点，便于处理光标处域
        vFrmRecord.EmrView.SetFocus;

      vFrmRecord.EmrView.InsertDeGroup(vDeGroup);
    finally
      vDeGroup.Free;
    end;
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniInsertAsRadioGroupClick(Sender: TObject);
var
  vRadioGroup: TDeRadioGroup;
  vFrmRecord: TfrmRecord;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecord := GetActiveRecord;

  if Assigned(vFrmRecord) then
  begin
    vRadioGroup := TDeRadioGroup.Create(vFrmRecord.EmrView.ActiveSectionTopLevelData);
    vRadioGroup[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];
    // 取数据元的选项，选项太多时提示是否都插入
    vRadioGroup.AddItem('选项1');
    vRadioGroup.AddItem('选项2');
    vRadioGroup.AddItem('选项3');

    if not vFrmRecord.EmrView.Focused then  // 先给焦点，便于处理光标处域
      vFrmRecord.EmrView.SetFocus;

    vFrmRecord.EmrView.InsertItem(vRadioGroup);
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
    vFrmItemContent.DomainItemID := StrToInt(sgdCV.Cells[3, sgdCV.Row]);
    vFrmItemContent.ShowModal;
  finally
    FreeAndNil(vFrmItemContent);
  end;
end;

procedure TfrmTemplate.mniEditClick(Sender: TObject);
var
  vFrmDeInfo: TfrmDeInfo;
begin
  if sgdDE.Row > 0 then
  begin
    vFrmDeInfo := TfrmDeInfo.Create(nil);
    try
      vFrmDeInfo.DeID := StrToInt(sgdDE.Cells[0, sgdDE.Row]);
      vFrmDeInfo.ShowModal;
      if vFrmDeInfo.ModalResult = mrOk then
        mniRefreshClick(Sender);
    finally
      FreeAndNil(vFrmDeInfo);
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
  vFrmDeInfo: TfrmDeInfo;
begin
  vFrmDeInfo := TfrmDeInfo.Create(nil);
  try
    vFrmDeInfo.DeID := 0;
    vFrmDeInfo.ShowModal;
    if vFrmDeInfo.ModalResult = mrOk then
      mniRefreshClick(Sender);
  finally
    FreeAndNil(vFrmDeInfo);
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

procedure TfrmTemplate.mniInsertAsComboboxClick(Sender: TObject);
var
  vDeCombobox: TDeCombobox;
  vFrmRecord: TfrmRecord;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecord := GetActiveRecord;

  if Assigned(vFrmRecord) then
  begin
    vDeCombobox := TDeCombobox.Create(vFrmRecord.EmrView.ActiveSectionTopLevelData,
      sgdDE.Cells[1, sgdDE.Row]);
    vDeCombobox.SaveItem := False;
    vDeCombobox[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];

    if not vFrmRecord.EmrView.Focused then  // 先给焦点，便于处理光标处域
      vFrmRecord.EmrView.SetFocus;

    vFrmRecord.EmrView.InsertItem(vDeCombobox);
  end
  else
    ShowMessage('未发现打开的模板！');
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

procedure TfrmTemplate.mniDeleteItemClick(Sender: TObject);
var
  vDeleteOk: Boolean;
begin
  if sgdCV.Row >= 0 then
  begin
    if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】和该选项对应的扩展内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      vDeleteOk := True;

      // 删除扩展内容
      vDeleteOk := DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row]));
      if vDeleteOk then
        ShowMessage('删除选项扩展内容成功！')
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
    if MessageDlg('确定要删除选项【' + sgdCV.Cells[0, sgdCV.Row] + '】的扩展内容吗？',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      if DeleteDomainItemContent(StrToInt(sgdCV.Cells[3, sgdCV.Row])) then
        ShowMessage('删除值域选项扩展内容成功！')
      else
        ShowMessage(CommonLastError);
    end;
  end;
end;

procedure TfrmTemplate.mniInsertAsDateTimeClick(Sender: TObject);
var
  vDateTime: TDeDateTimePicker;
  vFrmRecord: TfrmRecord;
begin
  if sgdDE.Row < 0 then Exit;

  vFrmRecord := GetActiveRecord;

  if Assigned(vFrmRecord) then
  begin
    vDateTime := TDeDateTimePicker.Create(vFrmRecord.EmrView.ActiveSectionTopLevelData, Now);
    vDateTime[TDeProp.Index] := sgdDE.Cells[0, sgdDE.Row];

    if not vFrmRecord.EmrView.Focused then  // 先给焦点，便于处理光标处域
      vFrmRecord.EmrView.SetFocus;

    vFrmRecord.EmrView.InsertItem(vDateTime);
  end
  else
    ShowMessage('未发现打开的模板！');
end;

procedure TfrmTemplate.mniInsertAsDEClick(Sender: TObject);
var
  vFrmRecord: TfrmRecord;
begin
  if sgdDE.Row < 0 then Exit;
  vFrmRecord := GetActiveRecord;
  if Assigned(vFrmRecord) then
  begin
    if not vFrmRecord.EmrView.Focused then  // 先给焦点，便于处理光标处域
      vFrmRecord.EmrView.SetFocus;

    vFrmRecord.InsertDeItem(sgdDE.Cells[0, sgdDE.Row], sgdDE.Cells[1, sgdDE.Row]);
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

    GetDomainItem(FDomainID);
    lblDE.Caption := sgdDE.Cells[1, sgdDE.Row] + '(共 ' + IntToStr(sgdCV.RowCount - 1) + ' 条选项)';
  end;
end;

procedure TfrmTemplate.mniNewTemplateClick(Sender: TObject);
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
        ABLLServerReady.ExecParam.I['desid'] := TDataSetInfo(tvTemplate.Selected.Data).ID;
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

procedure TfrmTemplate.mniRefreshClick(Sender: TObject);
begin
  HintFormShow('正在刷新数据元...', procedure(const AUpdateHint: TUpdateHint)
  var
    vTopRow, vRow: Integer;
  begin
    FOnFunctionNotify(PluginID, FUN_REFRESHCLIENTCACHE, nil);  // 重新获取客户端缓存
    SaveStringGridRow(vRow, vTopRow, sgdDE);
    ShowAllDataElement;  // 刷新数据元信息
    RestoreStringGridRow(vRow, vTopRow, sgdDE);
  end);
end;

procedure TfrmTemplate.pgTemplateMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vTabIndex: Integer;
  vPt: TPoint;
begin
  if Y < 20 then  // 默认的 pgRecordEdit.TabHeight 可通过获取操作系统参数得到更精确的
  begin
    vTabIndex := pgTemplate.IndexOfTabAt(X, Y);

    if pgTemplate.Pages[vTabIndex].Tag = 0 then Exit; // 帮助

    if (vTabIndex >= 0) and (vTabIndex = pgTemplate.ActivePageIndex) then
    begin
      if Button = TMouseButton.mbRight then
      begin
        vPt := pgTemplate.ClientToScreen(Point(X, Y));
        pmpg.Popup(vPt.X, vPt.Y);
      end
      else
      if ssDouble in Shift then
        CloseTemplatePage(pgTemplate.ActivePageIndex);
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

procedure TfrmTemplate.pmCVPopup(Sender: TObject);
begin
  mniNewItem.Enabled := FDomainID > 0;
  mniEditItem.Visible := sgdCV.Row > 0;
  mniDeleteItem.Visible := sgdCV.Row > 0;
  mniEditItemLink.Visible := sgdCV.Row > 0;
  mniDeleteItemLink.Visible := sgdCV.Row > 0;
end;

procedure TfrmTemplate.pmTemplatePopup(Sender: TObject);
begin
  mniNewTemplate.Enabled := not TreeNodeIsTemplate(tvTemplate.Selected);
  mniDeleteTemplate.Enabled := not mniNewTemplate.Enabled;
  mniInsertTemplate.Enabled := not mniNewTemplate.Enabled;
end;

procedure TfrmTemplate.sgdDEDblClick(Sender: TObject);
begin
  mniInsertAsDEClick(Sender);
end;

procedure TfrmTemplate.tvTemplateCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
//var
//  vRect: TRect;
begin
  //DefaultDraw := False;
  if (not TreeNodeIsTemplate(Node)) and (TDataSetInfo(Node.Data).ID = 74) then  // 测试用
  begin
    tvTemplate.Canvas.Font.Style := [fsBold];
    tvTemplate.Canvas.Font.Color := clRed;
  end;

//  vRect := Node.DisplayRect(False);
//  tvTemplate.Canvas.TextOut(vRect.Left + (Node.Level + 1) * 16, vRect.Top, Node.Text);
end;

procedure TfrmTemplate.tvTemplateDblClick(Sender: TObject);
var
  vPageIndex, vTempID: Integer;
  vPage: TTabSheet;
  vFrmRecord: TfrmRecord;
  vSM: TMemoryStream;
begin
  if TreeNodeIsTemplate(tvTemplate.Selected) then
  begin
    vTempID := TTemplateInfo(tvTemplate.Selected.Data).ID;
    vPageIndex := GetRecordEditPageIndex(vTempID);
    if vPageIndex >= 0 then
    begin
      pgTemplate.ActivePageIndex := vPageIndex;
      Exit;
    end;

    vSM := TMemoryStream.Create;
    try
      GetTemplateContent(vTempID, vSM);

      vFrmRecord := TfrmRecord.Create(nil);  // 创建编辑器
      vFrmRecord.EmrView.DesignMode := True;  // 设计模式
      vFrmRecord.ObjectData := tvTemplate.Selected.Data;
      if vSM.Size > 0 then
        vFrmRecord.EmrView.LoadFromStream(vSM);
    finally
      vSM.Free;
    end;

    if vFrmRecord <> nil then
    begin
      vPage := TTabSheet.Create(pgTemplate);
      vPage.Caption := tvTemplate.Selected.Text;
      vPage.Tag := vTempID;
      vPage.PageControl := pgTemplate;

      vFrmRecord.OnSave := DoSaveTempContent;
      vFrmRecord.OnChangedSwitch := DoRecordChangedSwitch;
      vFrmRecord.Parent := vPage;
      vFrmRecord.Align := alClient;
      vFrmRecord.Show;

      pgTemplate.ActivePage := vPage;

      vFrmRecord.EmrView.SetFocus;
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
        ABLLServerReady.ExecParam.I['desID'] := TDataSetInfo(Node.Data).ID;
        ABLLServerReady.BackDataSet := True;  // 告诉服务端要将查询数据集结果返回
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        vTempInfo: TTemplateInfo;
        vTpltNode: TTreeNode;
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
                  vTpltNode := tvTemplate.Items.AddChildObject(Node, vTempInfo.NameEx, vTempInfo);
                  vTpltNode.ImageIndex := 0;

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
