unit frm_BLLServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls, diocp_tcp_server, emr_MsgPack,
  System.Generics.Collections, BLLClientContext, emr_Common, emr_BLLServerProxy,
  emr_DBL, PluginIntf, PluginImp, FunctionIntf, FunctionImp, FunctionConst;

type
  TfrmBLLServer = class(TForm)
    mmMain: TMainMenu;
    mniN1: TMenuItem;
    mniStart: TMenuItem;
    mniStop: TMenuItem;
    pgc: TPageControl;
    tsState: TTabSheet;
    ts2: TTabSheet;
    pnl1: TPanel;
    chkLog: TCheckBox;
    btnClear: TButton;
    btnSave: TButton;
    mmoMsg: TMemo;
    mniN2: TMenuItem;
    mniSet: TMenuItem;
    mniPlugin: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniSetClick(Sender: TObject);
    procedure mniStartClick(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    /// <summary> 主服务器 </summary>
    FRemoteServer: TRemoteServer;
    FTcpServer: TDiocpTcpServer;
    FLogLocker: TObject;
    FDBL: TDBL;
    // 业务处理相关对象
    FAgentLocker: TObject;
    FAgentQueue: TQueue<TBLLAgent>;
    FAgentQueueThread: THCThread;
    //
    FPluginManager: IPluginManager;  // 插件列表
    FPluginLocker: TObject;
    /// <summary> 重新设置服务器 </summary>
    procedure ReCreateServer;
    procedure RefreshUIState;
    //
    procedure LoadServerPlugin(Sender: TObject);
    procedure DoPluginReLoad(Sender: TObject);
    procedure DoPluginUnLoad(Sender: TObject);
    //
    procedure RemoteProcessAgent(var AAgent: TBLLAgent);
    procedure ProcessAgent(var AAgent: TBLLAgent);
    procedure ExecuteSBLMsgPack(const AMsgPack: TMsgPack);
    //
    procedure DoAgentQueueThreadExecute(Sender: TObject);
    procedure DoContextAction(const AStream: TStream; const AContext: TIocpClientContext);
    procedure OnContextConnected(AClientContext: TIocpClientContext);
    procedure DoLog(const ALog: string);
  public
    { Public declarations }
  end;

var
  frmBLLServer: TfrmBLLServer;

implementation

uses
  uFMMonitor, BLLServerParam, DiocpError, emr_DataBase, emr_BLLDataBase,
  frm_Set, utils_zipTools;

{$R *.dfm}

procedure TfrmBLLServer.btnClearClick(Sender: TObject);
begin
  mmoMsg.Clear;
end;

procedure TfrmBLLServer.DoLog(const ALog: string);
begin
  if chkLog.Checked then
  begin
    System.MonitorEnter(FLogLocker);
    try
      mmoMsg.Lines.Add(sLineBreak + '=============='
        + FormatDateTime('YYYY-MM-DD HH:mm:ss', Now)
        + '=============='
        + sLineBreak + ALog);
    finally
      System.MonitorExit(FLogLocker);
    end;
  end;
end;

procedure TfrmBLLServer.DoContextAction(const AStream: TStream; const AContext: TIocpClientContext);
var
  vBLLAgent: TBLLAgent;
begin
  System.MonitorEnter(FAgentLocker);
  try
    vBLLAgent := TBLLAgent.Create(AStream, AContext);
    FAgentQueue.Enqueue(vBLLAgent);  // 加入队列
  finally
    System.MonitorExit(FAgentLocker);
  end;
end;

procedure TfrmBLLServer.DoAgentQueueThreadExecute(Sender: TObject);
var
  vAgent: TBLLAgent;
begin
  if FAgentQueue.Count = 0 then Exit;

  System.MonitorEnter(FAgentLocker);
  try
    vAgent := FAgentQueue.Dequeue;  // 从对列取出代理

    if FRemoteServer <> nil then  // 有主服务端
      RemoteProcessAgent(vAgent)  // 交主服务端处理
    else  // 我就是主服务端
      ProcessAgent(vAgent);
  finally
    System.MonitorExit(FAgentLocker);
  end;
end;

procedure TfrmBLLServer.DoPluginReLoad(Sender: TObject);
var
  i, vPluginIndex: Integer;
  vParentMenuItem: TMenuItem;
begin
  if Sender is TMenuItem then  // 菜单触发
  begin
    vPluginIndex := (Sender as TMenuItem).Tag;  // 插件标识
    if vPluginIndex < FPluginManager.Count then  // 有效的标识
    begin
      System.MonitorEnter(FPluginLocker);
      try
        IPlugin(FPluginManager.PluginList[vPluginIndex]).Load;  // 重新加载
      finally
        System.MonitorExit(FPlugInLocker);
      end;

      (Sender as TMenuItem).Enabled := False;  // 成功后，重新加载菜单不可用

      // 修改卸载插件菜单为可用
      vParentMenuItem := (Sender as TMenuItem).Parent;
      for i := 0 to vParentMenuItem.Count - 1 do
      begin
        if (vParentMenuItem[i].Caption = '卸载') and (vParentMenuItem[i].Tag = vPluginIndex) then
        begin
          vParentMenuItem[i].Enabled := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmBLLServer.DoPluginUnLoad(Sender: TObject);
var
  i, vPluginIndex: Integer;
  vParentMenuItem: TMenuItem;
begin
  if Sender is TMenuItem then  // 菜单触发
  begin
    vPluginIndex := (Sender as TMenuItem).Tag;  // 插件标识
    if vPluginIndex < FPluginManager.Count then  // 有效的标识
    begin
      System.MonitorEnter(FPluginLocker);
      try
        IPlugin(FPluginManager.PluginList[vPluginIndex]).UnLoad;  // 卸载
      finally
        System.MonitorExit(FPluginLocker);
      end;

      (Sender as TMenuItem).Enabled := False;  // 卸载成功后，卸载菜单不可用

      // 修改重新加载插件菜单为可用
      vParentMenuItem := (Sender as TMenuItem).Parent;
      for i := 0 to vParentMenuItem.Count - 1 do
      begin
        if (vParentMenuItem[i].Caption = '加载') and (vParentMenuItem[i].Tag = vPluginIndex) then
        begin
          vParentMenuItem[i].Enabled := True;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmBLLServer.ExecuteSBLMsgPack(const AMsgPack: TMsgPack);
var
  i: Integer;
  vObjFun: IObjectFunction;
  vPlugin: IPlugin;
begin
  System.MonitorEnter(FPluginLocker);
  try
    if FPluginManager.Count = 0 then Exit;

    vObjFun := TObjectFunction.Create;
    vObjFun.ID := FUN_OBJECT_BLL;
    vObjFun.&Object := AMsgPack;

    for i := 0 to FPluginManager.Count - 1 do
    begin
      vPlugin := IPlugin(FPluginManager.PluginList[i]);
      if vPlugin.Enable then
      begin
        try
          vPlugin.ExecFunction(vObjFun);
        except
          on E: Exception do
          begin
            DoLog('插件' + vPlugin.Name + '，调用方法' + vObjFun.ID + '异常：' + E.Message);
          end;
        end;
      end;
    end;
  finally
    System.MonitorExit(FPluginLocker);
  end;
end;

procedure TfrmBLLServer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FTcpServer.Active then
  begin
    if MessageDlg('确定要停止并关闭emr业务服务端？关闭后客户端不能处理业务！',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      FTcpServer.SafeStop;
      CanClose := True;
    end
    else
      CanClose := False;
  end;
end;

procedure TfrmBLLServer.FormCreate(Sender: TObject);
var
  FHandle: Cardinal;
begin
  pgc.ActivePageIndex := 0;
  FRemoteServer := nil;
  FLogLocker := TObject.Create;
  FTcpServer := TDiocpTcpServer.Create(Self);
  FTcpServer.CreateDataMonitor;  // 创建运行监视器
  FTcpServer.WorkerCount := 3;
  FTcpServer.RegisterContextClass(TBLLClientContext);
  FTcpServer.OnContextConnected := OnContextConnected;
  TFMMonitor.CreateAsChild(tsState, FTcpServer);
  //
  FAgentQueue := TQueue<TBLLAgent>.Create;
  FAgentLocker := TObject.Create;
  FAgentQueueThread := THCThread.Create;
  FAgentQueueThread.OnExecute := DoAgentQueueThreadExecute;
  //
//  FBLLQueue := TQueue<TBLLAgent>.Create;
//  FBLLLocker := TObject.Create;
//  FBLLQueueThread := THCThread.Create;
//  FBLLQueueThread.OnExecute := DoBLLQueueThreadExecute;
  //
  FDBL := TDBL.Create;
  FDBL.OnExecuteLog := DoLog;
  //
  FPluginLocker := TObject.Create;
  FPluginManager := TPluginManager.Create;
  LoadServerPlugin(nil);  // 加载服务端业务插件
end;

procedure TfrmBLLServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTcpServer);
  FreeAndNil(BLLServerParams);
  FreeAndNil(FDBL);

  if not FAgentQueueThread.Suspended then
  begin
    FAgentQueueThread.Terminate;
    FAgentQueueThread.WaitFor;
  end;
  FreeAndNil(FAgentQueueThread);
  FreeAndNil(FAgentQueue);
  FreeAndNil(FAgentLocker);

  FPluginManager.UnLoadAllPlugin;
  FreeAndNil(FPluginLocker);


//  if not FBLLQueueThread.Suspended then
//  begin
//    FBLLQueueThread.Terminate;
//    FBLLQueueThread.WaitFor;
//  end;
//  FreeAndNil(FBLLQueueThread);
//  FreeAndNil(FBLLQueue);
//  FreeAndNil(FBLLLocker);
  FreeAndNil(FLogLocker);
end;

procedure TfrmBLLServer.LoadServerPlugin(Sender: TObject);
var
  i: Integer;
  vMenu, vSubMenu: TMenuItem;
begin
  System.MonitorEnter(FPluginLocker);
  try
    {to do: 停止插件处理线程 }
    mniPlugin.Clear;
    FPluginManager.UnLoadAllPlugin;

    if not DirectoryExists(ExtractFilePath(ParamStr(0)) + 'plugin') then Exit;

    FPluginManager.LoadPlugins(ExtractFilePath(ParamStr(0)) + 'plugin', '.spi');

    for i := 0 to FPluginManager.Count - 1 do  // 遍历插件，添加插件对应的菜单
    begin
      vMenu := TMenuItem.Create(mniPlugIn);
      vMenu.Caption := IPlugIn(FPluginManager.PluginList[i]).Name;

      vSubMenu := TMenuItem.Create(vMenu);
      vSubMenu.Caption := '卸载';
      vSubMenu.Tag := i;
      vSubMenu.OnClick := DoPluginUnLoad;
      vMenu.Add(vSubMenu);

      vSubMenu := TMenuItem.Create(vMenu);
      vSubMenu.Enabled := False;
      vSubMenu.Caption := '加载';
      vSubMenu.Tag := i;
      vSubMenu.OnClick := DoPluginReLoad;
      vMenu.Add(vSubMenu);

      mniPlugIn.Add(vMenu);
    end;

    // 增加重新扫描菜单
    if mniPlugIn.Count > 0 then
    begin
      vSubMenu := TMenuItem.Create(mniPlugin);
      vSubMenu.Enabled := True;
      vSubMenu.Caption := '-';
      mniPlugin.Add(vSubMenu);
    end;

    vSubMenu := TMenuItem.Create(mniPlugin);
    vSubMenu.Enabled := True;
    vSubMenu.Caption := '重新扫描';
    vSubMenu.OnClick := LoadServerPlugin;
    mniPlugin.Add(vSubMenu);
  finally
    System.MonitorExit(FPluginLocker);
  end;
end;

procedure TfrmBLLServer.mniSetClick(Sender: TObject);
var
  vfrmSet: TfrmSet;
begin
  vfrmSet := TfrmSet.Create(Self);
  try
    if FileExists(ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini') then
      vfrmSet.FileName := ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini'
    else
      vfrmSet.FileName := '';

    vfrmSet.ShowModal;
    if FTcpServer.Active then  // 服务已运行
      ReCreateServer;  // 根据新设置重置服务端
  finally
    FreeAndNil(vfrmSet);
  end;
end;

procedure TfrmBLLServer.mniStartClick(Sender: TObject);
begin
  try
    ReCreateServer;  // 设置服务器

    pgc.ActivePageIndex := 1;  // 切换到监控页，否则客户端连接后界面不能切换bug
    FTcpServer.Port := 12830;
    FTcpServer.Active := true;

    if FAgentQueueThread.Suspended then
      FAgentQueueThread.Suspended := False;

    RefreshUIState;
  except
    on E: Exception do
    begin
      Caption := '启动失败：' + E.Message;
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmBLLServer.mniStopClick(Sender: TObject);
begin
  FTcpServer.SafeStop;
  FAgentQueueThread.Suspended := True;
  System.MonitorEnter(FAgentLocker);
  try
    FAgentQueue.Clear;
  finally
    System.MonitorExit(FAgentLocker);
  end;

  RefreshUIState;
end;

procedure TfrmBLLServer.OnContextConnected(AClientContext: TIocpClientContext);
begin
  TBLLClientContext(AClientContext).OnContextAction := DoContextAction;
end;

procedure TfrmBLLServer.ProcessAgent(var AAgent: TBLLAgent);
var
  vMsgPack: TMsgPack;
  vStream: TMemoryStream;
  vProxyType: TProxyType;
begin
  vMsgPack := TMsgPack.Create;
  try
    vStream := TMemoryStream.Create;
    try
      AAgent.Stream.Position := 0;
      TZipTools.UnZipStream(AAgent.Stream, vStream);  // 解压缩
      vStream.Position := 0;
      vMsgPack.DecodeFromStream(vStream);  // 解包

      vProxyType := TProxyType(vMsgPack.ForcePathObject(BLL_EXECPARAM).I[BLL_PROXYTYPE]);  // 调用哪种服务代理
      case vProxyType of  // 分发相应的业务;
        cptDBL:  // 数据库中业务语句
          FDBL.ExecuteMsgPack(vMsgPack);

        cptSBL:  // 服务端支持的业务
          ExecuteSBLMsgPack(vMsgPack);
      end;

      vStream.Clear;
      vMsgPack.EncodeToStream(vStream);  // 打包
      vStream.Position := 0;
      TZipTools.ZipStream(vStream, AAgent.Stream);  // 压缩数据
      AAgent.Stream.Position := 0;
      TBLLClientContext(AAgent.Context).WriteObject(AAgent.Stream);  // 推送到客户端
    finally
      vStream.Free;
    end;
  finally
    FreeAndNil(vMsgPack);
    FreeAndNil(AAgent);
  end;
end;

procedure TfrmBLLServer.ReCreateServer;
begin
  if BLLServerParams = nil then
    BLLServerParams := TBLLServerParams.Create(ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini');

  if BLLServerParams.RemoteActive then  // 指定了外部主服务器
  begin
    if FRemoteServer = nil then  // 未创建过外部服务器
      FRemoteServer := TRemoteServer.CreateEx(BLLServerParams.RemoteBLLIP, BLLServerParams.RemoteBLLPort);

    if FDBL.DB.Connected then
      FDBL.DB.DisConnect;
  end
  else  // 本机作为主服务器
  begin
    if not FDBL.DB.Connected then
    begin
      FDBL.DB.DBType := dbSqlServer;
      FDBL.DB.Server := BLLServerParams.DataBaseServer;
      FDBL.DB.DBName := BLLServerParams.DataBaseName;
      FDBL.DB.Username := BLLServerParams.DataBaseUsername;
      FDBL.DB.Password := BLLServerParams.DataBasePassword;
      FDBL.DB.Connect;
    end;

    FreeAndNil(FRemoteServer);
  end;
end;

procedure TfrmBLLServer.RefreshUIState;
begin
  mniStart.Enabled := not FTcpServer.Active;
  if FTcpServer.Active then
    Caption := 'emr业务(BLL)服务端[运行]' + FTcpServer.DefaultListenAddress + ' 端口:' + IntToStr(FTcpServer.Port)
  else
    Caption := 'emr业务(BLL)服务端[停止]';
  mniStop.Enabled := FTcpServer.Active;
end;

procedure TfrmBLLServer.RemoteProcessAgent(var AAgent: TBLLAgent);
var
  vMsgPack: TMsgPack;
  vStream: TMemoryStream;
  vCMD: Integer;
  vDBLSrvProxy: TBLLServerProxy;
  vErrorInfo: string;
begin
  vMsgPack := TMsgPack.Create;
  try
    vStream := TMemoryStream.Create;
    try
      AAgent.Stream.Position := 0;
      TZipTools.UnZipStream(AAgent.Stream, vStream);  // 解压缩
      vStream.Position := 0;
      vMsgPack.DecodeFromStream(vStream);  // 从流中解包
      vCMD := vMsgPack.I[BLL_CMD];

      try
        vDBLSrvProxy := TBLLServerProxy.CreateEx(FRemoteServer.Host, FRemoteServer.Port);
        try
          vDBLSrvProxy.ReConnectServer;
          if vDBLSrvProxy.Active then  // 如果连接成功
          begin
            if not vDBLSrvProxy.DispatchPack(vMsgPack) then
            begin
              vErrorInfo := GetDiocpErrorMessage(vDBLSrvProxy.ErrCode);
              if vErrorInfo = '' then
                vErrorInfo := SysErrorMessage(GetLastError);
              vMsgPack.Clear;
              vMsgPack.ForcePathObject(BLL_CMD).AsInteger := vCMD;
              vMsgPack.ForcePathObject(BACKRESULT).AsBoolean := False;
              vMsgPack.ForcePathObject(BACKMSG).AsString := vErrorInfo;
              DoLog('调用外部服务错误：' + vCMD.ToString + '，' + vErrorInfo);
            end;
          end;
        finally
          FreeAndNil(vDBLSrvProxy);
        end;
      except  // 返回异常信息
        on E:Exception do
        begin
          vMsgPack.Clear;
          vMsgPack.ForcePathObject(BLL_CMD).AsInteger := vCMD;
          vMsgPack.ForcePathObject(BACKRESULT).AsBoolean := False;
          vMsgPack.ForcePathObject(BACKMSG).AsString := E.Message;
          DoLog('调用外部服务错误：' + vCMD.ToString + '，' + E.Message);
        end;
      end;

      // 准备方法调用后的数据结果
      vStream.Clear;
      vMsgPack.EncodeToStream(vStream);  // 打包到流
      vStream.Position := 0;
      TZipTools.ZipStream(vStream, AAgent.Stream);  // 压缩数据
      AAgent.Stream.Position := 0;
      TBLLClientContext(AAgent.Context).WriteObject(AAgent.Stream);  // 推送到客户端
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vMsgPack);
    FreeAndNil(AAgent);
  end;
end;

end.
