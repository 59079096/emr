{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_Emr;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FunctionIntf, PluginIntf,
  emr_Common, CFControl, CFListView, Vcl.StdCtrls,
  Vcl.XPMan, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.AppEvnts;

type
  TfrmEmr = class(TForm)
    lstPlugin: TCFListView;
    xpmnfst: TXPManifest;
    appEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstPluginDBlClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure appEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure appEventsMessage(var Msg: tagMSG; var Handled: Boolean);
  private
    { Private declarations }
    FPluginManager: IPluginManager;
    FUserInfo: TUserInfo;

    function Frame_CreateCacheTable(const ATableName, AFields: string; const ADelIfExists: Boolean = True): Boolean;
    /// <summary> 从服务端同步缓存表 </summary>
    procedure Frame_SyncCacheTable;

    /// <summary> 列出所有插件 </summary>
    procedure LoadPluginList;

    // 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
    procedure DoPluginFunction(const APluginID, AFunctionID: string; const AObjFun: IObjectFunction);
  public
    { Public declarations }
    function LoginPluginExecute: Boolean;
  end;

  /// <summary> 插件列表项目信息 </summary>
  TFunInfo = class(TObject)
    PlugInID: string;  // 对应的插件
    Fun: Pointer;  // 对应的功能
    //BuiltIn: Boolean;  // 内置插件
  end;

  /// <summary> 获取本地参数 </summary>
  procedure GetClientParam;

var
  frmEmr: TfrmEmr;

implementation

uses
  frm_DM, PluginImp, FunctionImp, FunctionConst, PluginConst, emr_BLLInvoke,
  emr_MsgPack, System.IniFiles, CFBalloonHint;

{$R *.dfm}

// 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
procedure PluginFunction(const APluginID, AFunID: string; const AObjFun: IObjectFunction);
begin
  frmEmr.DoPluginFunction(APluginID, AFunID, AObjFun);
end;

procedure GetClientParam;
var
  vIniFile: TIniFile;
begin
  vIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'emr.ini');
  try
    ClientCache.ClientParam.TimeOut := vIniFile.ReadInteger('Client', 'TimeOut', 3000);  // 3秒
    ClientCache.ClientParam.VersionID := vIniFile.ReadInteger('Client', PARAM_LOCAL_VERSIONID, 0);

    ClientCache.ClientParam.BLLServerIP := vIniFile.ReadString('BLLServer', PARAM_LOCAL_BLLHOST, '127.0.0.1');  // 业务服务端
    ClientCache.ClientParam.BLLServerPort := vIniFile.ReadInteger('BLLServer', PARAM_LOCAL_BLLPORT, 12830);  // 业务服务端端口

    ClientCache.ClientParam.MsgServerIP := vIniFile.ReadString('MsgServer', PARAM_LOCAL_MSGHOST, '127.0.0.1');  // 消息服务端
    ClientCache.ClientParam.MsgServerPort := vIniFile.ReadInteger('MsgServer', PARAM_LOCAL_MSGPORT, 12832);  // 消息服务端端口
  finally
    FreeAndNil(vIniFile);
  end;
end;

procedure TfrmEmr.appEventsIdle(Sender: TObject; var Done: Boolean);
//var
//  vIFun: ICustomFunction;
begin
//  vIFun := TCustomFunction.Create;
//  vIFun.ID := FUN_APPEVENTSIDLE;
//  FPluginManager.FunBroadcast(vIFun);
end;

procedure TfrmEmr.appEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  i: Integer;
  vIFun: IObjectFunction;
  vEventMessage: TEventMessage;
begin
  vIFun := TObjectFunction.Create;
  vIFun.ID := FUN_APPONMESSAGE;
  vEventMessage := TEventMessage.Create;
  try
    vEventMessage.Msg := Msg;
    vEventMessage.Handled := Handled;

    vIFun.&Object := vEventMessage;

    for i := FPluginManager.Count - 1 downto 0 do
    begin
      if IPlugin(FPluginManager.PluginList[i]).GetFunction(FUN_APPONMESSAGE) <> nil then
        IPlugin(FPluginManager.PluginList[i]).ExecFunction(vIFun);
    end;

    Handled := vEventMessage.Handled;
  finally
    vEventMessage.Free;
  end;
end;

procedure TfrmEmr.DoPluginFunction(const APluginID, AFunctionID: string;
  const AObjFun: IObjectFunction);
var
  vIPlugin: IPlugin;
  vIFun: ICustomFunction;
  vUserCert: TUserCert;
begin
  vIPlugin := FPluginManager.GetPluginByID(APluginID);  // 获取相应的插件
  if Assigned(vIPlugin) then  // 有效插件
  begin
    if AFunctionID = FUN_LOGINCERTIFCATE then  // 身份认证
    begin
      vUserCert := TUserCert((AObjFun as IObjectFunction).&object);
      TBLLInvoke.Certification(vUserCert);
      if vUserCert.State = cfsPass then
        FUserInfo.ID := vUserCert.ID;
    end
    else
    if AFunctionID = FUN_USERINFO then  // 获取当前用户信息
    begin
      if APluginID = PLUGIN_LOGIN then
        FUserInfo.ID := string((AObjFun as IObjectFunction).&object)
      else
        (AObjFun as IObjectFunction).&Object := FUserInfo;
    end
    else
    if AFunctionID = FUN_MAINFORMHIDE then  // 隐藏主窗体
    begin
      // 不使用Hide或Visible=False防止插件中创建窗体调用InitializeNewForm-
      // Screen.AddForm(Self)-Application.UpdateVisible;将主窗体任务栏按钮显示
      ShowWindow(Handle, SW_HIDE);
      //ShowWindow(Application.Handle, SW_HIDE);
    end
    else
    if AFunctionID = FUN_MAINFORMSHOW then  // 显示主窗体
    begin
      ShowWindow(Handle, SW_SHOW);
      //ShowWindow(Application.Handle, SW_SHOW);
    end
    else
    if AFunctionID = FUN_CLIENTCACHE then  // 获取客户端缓存对象
      (AObjFun as IObjectFunction).&Object := ClientCache
    else
    if AFunctionID = FUN_REFRESHCLIENTCACHE then  // 重新获取客户端缓存
      ClientCache.GetCacheData
    else
    if AFunctionID = FUN_LOCALDATAMODULE then  // 获取本地数据库操作DataModule
      (AObjFun as IObjectFunction).&Object := dm
    else  // 未知的直接回调给插件
    begin
      vIFun := TCustomFunction.Create;
      vIFun.ID := AFunctionID;
      vIPlugin.ExecFunction(vIFun);
    end;
  end
  else
    BalloonMessage('插件调用失败，无ID为"' + APluginID + '"的插件！');
end;

procedure TfrmEmr.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := MessageDlg('确定要关闭emr客户端？', mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TfrmEmr.FormCreate(Sender: TObject);
begin
  FUserInfo := TUserInfo.Create;
  FPluginManager := TPluginManager.Create;
  LoadPluginList;  // 获取所有插件信息
end;

procedure TfrmEmr.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUserInfo);  // 为什么先FPluginManager := nil;后FreeAndNil(FUserInfo);程序退出会出错呢？
end;

procedure TfrmEmr.FormShow(Sender: TObject);
begin
  Frame_SyncCacheTable;  // 加载缓存表
end;

function TfrmEmr.Frame_CreateCacheTable(const ATableName, AFields: string; const ADelIfExists: Boolean = True): Boolean;
begin
  Result := False;

  if ADelIfExists then
  begin
    // 本地已经有缓存表时需要先删除
    dm.qryTemp.Open(Format('SELECT COUNT(*) AS tbcount FROM sqlite_master where type=''table'' and name=''%s''',
      [ATableName]));
    if dm.qryTemp.FieldByName('tbcount').AsInteger = 1 then  // 本地已经有缓存表了
      dm.ExecSql('DROP TABLE ' + ATableName);  // 更新表数据之前先删除表及数据
  end;

  // 从服务端查询需要缓存的表字段类型
  BLLServerExec(
    procedure(const ABLLServerReady: TBLLServerProxy)
    var
      vReplaceParam: TMsgPack;
    begin
      ABLLServerReady.Cmd := BLL_EXECSQL;
      vReplaceParam := ABLLServerReady.ReplaceParam;
      vReplaceParam.S['Sql'] := 'SELECT ' + AFields + ' FROM ' + ATableName;
      ABLLServerReady.BackDataSet := True;
    end,
    procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)

      {$REGION ' GetCreateTableSql 生成创建本地表语句 '}
      function GetCreateTableSql: string;
      var
        i: Integer;
        vField: string;
      begin
        Result := '';
        for i := 0 to AMemTable.FieldDefs.Count - 1 do
        begin
          vField := AMemTable.Fields[i].FieldName;  // 字段名
          case AMemTable.Fields[i].DataType of  // 字段类型
            ftSmallint: vField := vField + ' smallint';

            ftInteger, ftAutoInc: vField := vField + ' int';

            ftCurrency: vField := vField + ' money';

            ftFloat: vField := vField + ' float';

            ftLargeint: vField := vField + ' bigint';

            ftBoolean: vField := vField + ' bit';

            ftDate: vField := vField + ' date';

            ftSingle: vField := vField + ' real';

            ftString:  vField := vField + ' varchar(' + (AMemTable.Fields[i].DataSize - 1).ToString + ')';

            ftWideString: vField := vField + ' nvarchar(' + (AMemTable.Fields[i].DataSize / 2 - 1).ToString + ')';
          else
            vField := vField + ' nvarchar(50)';
          end;
          if AMemTable.Fields[i].ReadOnly then  // 字段是主键
            vField := vField + ' primary key';
          if Result = '' then
            Result := vField
          else
            Result := Result + ', ' + vField;
        end;
        Result := 'CREATE TABLE ' + ATableName + ' (' + Result + ')';
      end;
      {$ENDREGION}

    begin
      if not ABLLServer.MethodRunOk then
      begin
        ShowMessage(ABLLServer.MethodError);
        Exit;
      end;

      dm.conn.ExecSQL(GetCreateTableSql);  // 创建本地表

      if AMemTable <> nil then  // 服务端表有数据
      begin
        dm.qryTemp.Open('SELECT * FROM ' + ATableName);  // 打开本地缓存表
        dm.qryTemp.CopyDataSet(AMemTable);  // 复制数据
      end;
    end);

  Result := True;
end;

procedure TfrmEmr.Frame_SyncCacheTable;
begin
  HintFormShow('正在更新缓存表...', procedure(const AUpdateHint: TUpdateHint)
  begin
    BLLServerExec(
      procedure(const ABLLServerReady: TBLLServerProxy)
      begin
        ABLLServerReady.Cmd := BLL_GETCLIENTCACHE;  // 获取服务端缓存表信息
        ABLLServerReady.BackDataSet := True;
      end,
      procedure(const ABLLServer: TBLLServerProxy; const AMemTable: TFDMemTable = nil)
      var
        vTableName: string;
        vHasCache: Boolean;
      begin
        if not ABLLServer.MethodRunOk then
        begin
          ShowMessage(ABLLServer.MethodError);
          Exit;
        end;

        if AMemTable <> nil then  // 有缓存表信息
        begin
          AMemTable.First;
          while not AMemTable.Eof do
          begin
            vTableName := AMemTable.FieldByName('tbName').AsString;  // 缓存表表名

            AUpdateHint('更新缓存表 ' + vTableName);

            if not AMemTable.FieldByName('Used').AsBoolean then  // 不使用的缓存表
            begin
              // 删除本地已经有的缓存表
              dm.qryTemp.Open(Format('SELECT COUNT(*) AS tbcount FROM sqlite_master where type=''table'' and name=''%s''',
                [vTableName]));
              if dm.qryTemp.FieldByName('tbcount').AsInteger = 1 then  // 本地已经有缓存表了
                dm.ExecSql('DROP TABLE ' + vTableName);  // 删除
              // 删除缓存表中的信息
              dm.ExecSql(Format('DELETE FROM clientcache WHERE id = %d',
                [AMemTable.FieldByName('id').AsInteger]));
            end
            else
            begin
              dm.qryTemp.Open(Format('SELECT id, tbName, dataVer FROM clientcache WHERE id = %d',
                [AMemTable.FieldByName('id').AsInteger]));  // 获取服务端每个缓存表在本地的信息

              vHasCache := dm.qryTemp.RecordCount > 0;  // 本地缓存过此表

              if dm.qryTemp.FieldByName('dataVer').AsInteger <> AMemTable.FieldByName('dataVer').AsInteger then  // 本地版本小于服务端或本地没有该缓存表
              begin
                if Frame_CreateCacheTable(vTableName, AMemTable.FieldByName('tbField').AsString) then  // 更新本地缓存表及数据成功后记录本地缓存表信息
                begin
                  if vHasCache then  // 本地缓存过此表
                  begin
                    dm.ExecSql(Format('UPDATE clientcache SET dataVer = %d WHERE id = %d',
                      [AMemTable.FieldByName('dataVer').AsInteger,
                       AMemTable.FieldByName('id').AsInteger]));
                  end
                  else  // 本地没有缓存过此表
                  begin
                    dm.ExecSql(Format('INSERT INTO clientcache (id, tbName, dataVer) VALUES (%d, ''%s'', %d)',
                      [AMemTable.FieldByName('id').AsInteger,
                       vTableName,
                       AMemTable.FieldByName('dataVer').AsInteger]));
                  end;
                end;
              end;
            end;

            AMemTable.Next;
          end;
        end;
      end);
  end);
end;

procedure TfrmEmr.LoadPluginList;
var
  i, j: Integer;
  vIPlugin: IPlugin;
  vIFun: IPluginFunction;
  vFunInfo: TFunInfo;
  vListViewItem: TListViewItem;
  vRunPath: string;
begin
  // 加载外置插件
  vRunPath := ExtractFilePath(ParamStr(0));
  lstPlugin.BeginUpdate;
  try
    lstPlugin.Clear;
    FPluginManager.LoadPlugins(vRunPath + 'plugin', '.cpi');

    for i := 0 to FPluginManager.Count - 1 do
    begin
      vIPlugin := IPlugin(FPluginManager.PluginList[i]);
      //HintForm.ShowHint(vIPlugin.Name + '，' + vIPlugin.Comment, i);
      //
      for j := 0 to vIPlugin.FunctionCount - 1 do
      begin
        vIFun := vIPlugin.GetFunction(j);
        if vIFun.ShowEntrance then
        begin
          vFunInfo := TFunInfo.Create;
          //vFunInfo.BuiltIn := False;
          vFunInfo.PlugInID := vIPlugin.ID;
          vFunInfo.Fun := Pointer(vIPlugin.GetFunction(j));
          vListViewItem := lstPlugin.AddItem(vIPlugin.Name, vIPlugin.Comment
            + '(' + vIPlugin.Version + ')', nil, vFunInfo);

          if FileExists(vRunPath + 'image\' + vIPlugin.ID + '.png') then
            vListViewItem.ImagePng.LoadFromFile(vRunPath + 'image\' + vIPlugin.ID + '.png');
        end;
      end;
    end;
  finally
    lstPlugin.EndUpdate;
  end;
end;

function TfrmEmr.LoginPluginExecute: Boolean;
var
  vIPlugin: IPlugin;
  vIFunBLLFormShow: IFunBLLFormShow;
begin
  Result := False;
  FUserInfo.ID := '';
  vIPlugin := FPluginManager.GetPluginByID(PLUGIN_LOGIN);
  if Assigned(vIPlugin) then  // 有登录插件
  begin
    vIFunBLLFormShow := TFunBLLFormShow.Create;
    vIFunBLLFormShow.AppHandle := Application.Handle;
    vIFunBLLFormShow.OnNotifyEvent := @PluginFunction;
    vIPlugin.ExecFunction(vIFunBLLFormShow);
    Result := FUserInfo.ID <> '';
  end
  else
    ShowMessage('未找到登录插件！');
end;

procedure TfrmEmr.lstPluginDBlClick(Sender: TObject);
var
  vIPlugin: IPlugin;
  vIFunSelect: IPluginFunction;
  vIFun: IFunBLLFormShow;
begin
  if lstPlugin.Selected = nil then Exit;

  vIPlugin := FPluginManager.GetPluginByID(TFunInfo(lstPlugin.Selected.ObjectEx).PlugInID);
  if Assigned(vIPlugin) then  // 有插件
  begin
    HintFormShow('正在加载... ' + vIPlugin.Name, procedure(const AUpdateHint: TUpdateHint)
    begin
      vIFunSelect := IPluginFunction(TFunInfo(lstPlugin.Selected.ObjectEx).Fun);  // 获取插件功能
      if vIFunSelect <> nil then
      begin
        if vIFunSelect.ID = FUN_BLLFORMSHOW then
        begin
          vIFun := TFunBLLFormShow.Create;
          vIFun.AppHandle := Application.Handle;
          //(vIFun as IDBLFormFunction).UserID := FUserID;
        end
        else
          raise Exception.Create('异常：不识别的功能ID[方法：lstEntPlugsDBlClick]！');

        AUpdateHint('正在执行... ' + vIPlugin.Name + '-' + vIFun.Name);
        vIFun.ShowEntrance := vIFunSelect.ShowEntrance;
        vIFun.OnNotifyEvent := @PluginFunction;
        vIPlugin.ExecFunction(vIFun);
      end;
    end);
  end;
end;

end.
