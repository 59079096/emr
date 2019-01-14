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
  emr_Common, CFControl, CFListView, Vcl.StdCtrls, CFButtonEdit, CFGridEdit,
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
  private
    { Private declarations }
    FPluginManager: IPluginManager;
    FUserInfo: TUserInfo;

    function Frame_CreateCacheTable(const ATableName, AFields: string; const ADelIfExists: Boolean = True): Boolean;
    procedure Frame_LoadAllCacheTable;

    /// <summary> 列出所有插件 </summary>
    procedure LoadPluginList;

    // 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
    procedure DoPluginNotify(const APluginID, AFunctionID: string; const APluginObject: IPluginObject);
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
  frm_DM, PluginImp, FunctionImp, FunctionConst, PluginConst, emr_PluginObject,
  emr_BLLServerProxy, emr_MsgPack;

{$R *.dfm}

// 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
procedure PluginNotify(const APluginID, AFunctionID: string;
  const APluginObject: IPluginObject);
begin
  frmEmr.DoPluginNotify(APluginID, AFunctionID, APluginObject);
end;

procedure GetClientParam;
begin
  ClientCache.ClientParam.TimeOut := 3000;  // 3秒
  ClientCache.ClientParam.BLLServerIP := dm.GetParamStr(PARAM_LOCAL_BLLHOST);  // 业务服务器
  ClientCache.ClientParam.BLLServerPort := dm.GetParamInt(PARAM_LOCAL_BLLPORT, 12830);  // 业务服务器端口
  if ClientCache.ClientParam.BLLServerIP = '' then
    ClientCache.ClientParam.BLLServerIP := '127.0.0.1';  // 115.28.145.107

  ClientCache.ClientParam.MsgServerIP := dm.GetParamStr(PARAM_LOCAL_MSGHOST);  // 消息服务端
  ClientCache.ClientParam.MsgServerPort := dm.GetParamInt(PARAM_LOCAL_MSGPORT, 12832);  // 消息服务器端口
  if ClientCache.ClientParam.MsgServerIP = '' then
    ClientCache.ClientParam.MsgServerIP := '127.0.0.1';

  ClientCache.ClientParam.UpdateServerIP := dm.GetParamStr(PARAM_LOCAL_UPDATEHOST);  // 升级服务器
  ClientCache.ClientParam.UpdateServerPort := dm.GetParamInt(PARAM_LOCAL_UPDATEPORT, 12834);  // 更新服务器端口
  if ClientCache.ClientParam.UpdateServerIP = '' then
    ClientCache.ClientParam.UpdateServerIP := '127.0.0.1';
end;

procedure TfrmEmr.appEventsIdle(Sender: TObject; var Done: Boolean);
//var
//  vIFun: ICustomFunction;
begin
//  vIFun := TCustomFunction.Create;
//  vIFun.ID := FUN_APPEVENTSIDLE;
//  FPluginManager.FunBroadcast(vIFun);
end;

procedure TfrmEmr.DoPluginNotify(const APluginID, AFunctionID: string;
  const APluginObject: IPluginObject);
var
  vIPlugin: IPlugin;
  vIFun: ICustomFunction;
begin
  vIPlugin := FPluginManager.GetPlugin(APluginID);  // 获取相应的插件
  if vIPlugin <> nil then  // 有效插件
  begin
    if AFunctionID = FUN_USERINFO then  // 获取当前用户信息
    begin
      if APluginID = PLUGIN_LOGIN then
        FUserInfo.ID := string((APluginObject as IPlugInObjectInfo).&object)
      else
        (APluginObject as IPlugInObjectInfo).&Object := FUserInfo;
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
      (APluginObject as IPlugInObjectInfo).&Object := ClientCache
    else
    if AFunctionID = FUN_REFRESHCLIENTCACHE then  // 重新获取客户端缓存
      ClientCache.GetCacheData
    else
    if AFunctionID = FUN_LOCALDATAMODULE then  // 获取本地数据库操作DataModule
      (APluginObject as IPlugInObjectInfo).&Object := dm
    else  // 未知的直接回调给插件
    begin
      vIFun := TCustomFunction.Create;
      vIFun.ID := AFunctionID;
      vIPlugin.ExecFunction(vIFun);
    end;
  end;
end;

procedure TfrmEmr.FormCreate(Sender: TObject);
begin
  FPluginManager := TPluginManager.Create;
  FUserInfo := TUserInfo.Create;
  LoadPluginList;  // 获取所有插件信息
end;

procedure TfrmEmr.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUserInfo);  // 为什么先FPluginManager := nil;后FreeAndNil(FUserInfo);程序退出会出错呢？
end;

procedure TfrmEmr.FormShow(Sender: TObject);
begin
  Frame_LoadAllCacheTable;  // 加载缓存表
end;

function TfrmEmr.Frame_CreateCacheTable(const ATableName, AFields: string; const ADelIfExists: Boolean): Boolean;
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

      dm.conn.ExecSQL(GetCreateTableSql);  // 创建表

      if AMemTable <> nil then  // 服务端表有数据
      begin
        dm.qryTemp.Open('SELECT * FROM ' + ATableName);  // 打开本地缓存表
        dm.qryTemp.CopyDataSet(AMemTable);  // 复制数据
      end;
    end);

  Result := True;
end;

procedure TfrmEmr.Frame_LoadAllCacheTable;
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

            dm.qryTemp.Open(Format('SELECT id, tbName, dataVer FROM clientcache WHERE id = %d',
              [AMemTable.FieldByName('id').AsInteger]));  // 获取服务端每个缓存表在本地的信息

            vHasCache := dm.qryTemp.RecordCount > 0;  // 本地缓存过此表

            if dm.qryTemp.FieldByName('dataVer').AsInteger < AMemTable.FieldByName('dataVer').AsInteger then  // 本地版本小于服务端或本地没有该缓存表
            begin
              if Frame_CreateCacheTable(vTableName, AMemTable.FieldByName('tbField').AsString) then  // 更新本地缓存表及数据
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
          vListViewItem := lstPlugin.AddItem(vIPlugin.Name, vIPlugin.Comment + '(' + vIPlugin.Version + ')',
            nil, vFunInfo);

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
  vIPlugin := FPluginManager.GetPlugin(PLUGIN_LOGIN);
  if Assigned(vIPlugin) then  // 有登录插件
  begin
    vIFunBLLFormShow := TFunBLLFormShow.Create;
    vIFunBLLFormShow.AppHandle := Application.Handle;
    vIFunBLLFormShow.OnNotifyEvent := @PluginNotify;
    vIPlugin.ExecFunction(vIFunBLLFormShow);
    Result := FUserInfo.ID <> '';
  end;
end;

procedure TfrmEmr.lstPluginDBlClick(Sender: TObject);
var
  vIPlugin: IPlugin;
  vIFunSelect: IPluginFunction;
  vIFun: IFunBLLFormShow;
begin
  if lstPlugin.Selected = nil then Exit;

  vIPlugin := FPluginManager.GetPlugin(TFunInfo(lstPlugin.Selected.ObjectEx).PlugInID);
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
        vIFun.OnNotifyEvent := @PluginNotify;
        vIPlugin.ExecFunction(vIFun);
      end;
    end);
  end;
end;

end.
