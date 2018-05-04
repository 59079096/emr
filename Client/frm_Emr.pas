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
  emr_Common, CFControl, CFListView, Vcl.StdCtrls, CFButtonEdit, CFGridEdit;

type
  TfrmEmr = class(TForm)
    lstPlugin: TCFListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lstPluginDBlClick(Sender: TObject);
  private
    { Private declarations }
    FPluginManager: IPluginManager;
    FUserInfo: TUserInfo;

    /// <summary> 列出所有插件 </summary>
    procedure LoadPluginList;

    // 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
    procedure DoPluginNotify(const APluginID, AFunctionID: ShortString; const APluginObject: IPluginObject);
  public
    { Public declarations }
    function LoginPluginExec: Boolean;
  end;

  /// <summary> 插件列表项目信息 </summary>
  TFunInfo = class(TObject)
    PlugInID: ShortString;  // 对应的插件
    Fun: Pointer;  // 对应的功能
    //BuiltIn: Boolean;  // 内置插件
  end;

  /// <summary> 获取本地参数 </summary>
  procedure GetClientParam;

var
  frmEmr: TfrmEmr;

implementation

uses
  frm_DM, PluginImp, FunctionImp, FunctionConst, PluginConst,emr_PluginObject;

{$R *.dfm}

// 插件回调事件 注意和PluginFunctionIntf中TFunctionNotifyEvent保持一致
procedure PluginNotify(const APluginID, AFunctionID: ShortString;
  const APluginObject: IPluginObject);
begin
  frmEmr.DoPluginNotify(APluginID, AFunctionID, APluginObject);
end;

procedure GetClientParam;
begin
  if GClientParam = nil then
  begin
    GClientParam := TClientParam.Create;
    GClientParam.TimeOut := 3000;  // 3秒
  end;

  GClientParam.BLLServerIP := dm.GetParamStr(PARAM_LOCAL_BLLHOST);  // 业务服务器
  GClientParam.BLLServerPort := dm.GetParamInt(PARAM_LOCAL_BLLPORT, 12830);  // 业务服务器端口
  if GClientParam.BLLServerIP = '' then
    GClientParam.BLLServerIP := '115.28.145.107';

  GClientParam.MsgServerIP := dm.GetParamStr(PARAM_LOCAL_MSGHOST);  // 消息服务端
  GClientParam.MsgServerPort := dm.GetParamInt(PARAM_LOCAL_MSGPORT, 12832);  // 消息服务器端口
  if GClientParam.MsgServerIP = '' then
    GClientParam.MsgServerIP := '115.28.145.107';

  GClientParam.UpdateServerIP := dm.GetParamStr(PARAM_LOCAL_UPDATEHOST);  // 升级服务器
  GClientParam.UpdateServerPort := dm.GetParamInt(PARAM_LOCAL_UPDATEPORT, 12834);  // 更新服务器端口
  if GClientParam.UpdateServerIP = '' then
    GClientParam.UpdateServerIP := '115.28.145.107';
end;

procedure TfrmEmr.DoPluginNotify(const APluginID, AFunctionID: ShortString;
  const APluginObject: IPluginObject);
var
  vIPlugin: IPlugin;
  vIFun: ICustomFunction;
  vIUser: IUserInfo;
begin
  vIPlugin := FPluginManager.GetPlugin(APluginID);  // 获取相应的插件
  if vIPlugin <> nil then  // 有效插件
  begin
    if AFunctionID = FUN_USERINFO then  // 获取当前用户信息
    begin
      if APluginID = PLUGIN_LOGIN then
      begin
        FUserInfo.ID := (APluginObject as IUserInfo).UserID;
      end
      else
        (APluginObject as IUserInfo).UserID := FUserInfo.ID;
    end
    else
    if AFunctionID = FUN_MAINFORMHIDE then  // 隐藏主窗体
    begin
      // 不使用Hide或Visible=False防止插件中创建窗体调用InitializeNewForm-
      // Screen.AddForm(Self)-Application.UpdateVisible;将主窗体任务栏按钮显示
      ShowWindow(Handle, SW_HIDE);
      ShowWindow(Application.Handle, SW_HIDE);
    end
    else
    if AFunctionID = FUN_MAINFORMSHOW then  // 显示主窗体
    begin
      ShowWindow(Handle, SW_SHOW);
      ShowWindow(Application.Handle, SW_SHOW);
    end
    else
    if AFunctionID = FUN_BLLSERVERINFO then  // 获取业务服务端连接参数
    begin
      (APluginObject as IServerInfo).Host := GClientParam.BLLServerIP;
      (APluginObject as IServerInfo).Port := GClientParam.BLLServerPort;
      (APluginObject as IServerInfo).TimeOut := GClientParam.TimeOut;
    end
    else
    if AFunctionID = FUN_BLLSERVERINFO then  // 获取消息服务端连接参数
    begin
      (APluginObject as IServerInfo).Host := GClientParam.MsgServerIP;
      (APluginObject as IServerInfo).Port := GClientParam.MsgServerPort;
      (APluginObject as IServerInfo).TimeOut := GClientParam.TimeOut;
    end
    else
    if AFunctionID = FUN_UPDATESERVERINFO then  // 获取升级服务端连接参数
    begin
      (APluginObject as IServerInfo).Host := GClientParam.UpdateServerIP;
      (APluginObject as IServerInfo).Port := GClientParam.UpdateServerPort;
      (APluginObject as IServerInfo).TimeOut := GClientParam.TimeOut;
    end
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

procedure TfrmEmr.LoadPluginList;
var
  i, j: Integer;
  vIPlugin: IPlugin;
  vIFun: IPluginFunction;
  vFunInfo: TFunInfo;
  vListViewItem: TListViewItem;
  vHandle: THandle;
begin
  // 加载外置插件
  lstPlugin.BeginUpdate;
  try
    lstPlugin.Clear;
    FPluginManager.LoadPlugins(ExtractFilePath(ParamStr(0)) + 'plugin', '.cpi');
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

          vHandle := LoadLibrary(PChar(vIPlugin.FileName));
          try
            if FindResource(vHandle, 'PLUGINLOGO', RT_RCDATA) > 0 then
              vListViewItem.ImagePng.LoadFromResourceName(vHandle, 'PLUGINLOGO');
          finally
            FreeLibrary(vHandle);
          end;
        end;
      end;
    end;
  finally
    lstPlugin.EndUpdate;
  end;
end;

function TfrmEmr.LoginPluginExec: Boolean;
var
  vIPlugin: IPlugin;
  vIFunBLLFormShow: IFunBLLFormShow;
begin
  Result := False;
  FUserInfo.ID := '';
  vIPlugin := FPluginManager.GetPlugin(PLUGIN_LOGIN);
  if vIPlugin <> nil then  // 有登录插件
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
  vFunID: string;
begin
  if lstPlugin.Selected = nil then Exit;

  vIPlugin := FPluginManager.GetPlugin(TFunInfo(lstPlugin.Selected.ObjectEx).PlugInID);
  if vIPlugin <> nil then  // 有插件
  begin
    HintFormShow('正在加载...' + vIPlugin.Name, procedure(const AUpdateHint: TUpdateHint)
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

        AUpdateHint('正在启动 ' + vIPlugin.Name);
        vIFun.ShowEntrance := vIFunSelect.ShowEntrance;
        vIFun.OnNotifyEvent := @PluginNotify;
        vIPlugin.ExecFunction(vIFun);
      end;
    end);
  end;
end;

end.
