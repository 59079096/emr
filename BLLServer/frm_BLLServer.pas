{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit frm_BLLServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls, diocp_tcp_server, emr_MsgPack,
  System.Generics.Collections, BLLClientContext, emr_Common, emr_DBL;

type
  TfrmBLLServer = class(TForm)
    mm: TMainMenu;
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
    mniN3: TMenuItem;
    mniConnect: TMenuItem;
    mniBLLSet: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniStartClick(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mniBLLSetClick(Sender: TObject);
    procedure mniConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FAlias: string;
    /// <summary> 主服务器 </summary>
    FRemoteServer: TRemoteServer;
    FTcpServer: TDiocpTcpServer;
    FLogLocker: TObject;
    FDBL: TDBL;
    // 业务处理相关对象
    FAgentLocker: TObject;
    FAgentQueue: TQueue<TBLLAgent>;
    FAgentQueueThread: THCThread;
    /// <summary> 重新设置服务器 </summary>
    procedure ReCreateServer;
    procedure RefreshUIState;
    //
    procedure RemoteProcessAgent(var AAgent: TBLLAgent);
    procedure ProcessAgent(var AAgent: TBLLAgent);
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
  frm_ConnSet, frm_BLLSet, utils_zipTools, BLLCompiler, emr_BLLInvoke, System.IniFiles;

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

  System.MonitorEnter(FAgentLocker);  // 锁队列
  try
    vAgent := FAgentQueue.Dequeue;  // 从对列取出代理

    if FRemoteServer <> nil then  // 有主服务端
      RemoteProcessAgent(vAgent)  // 交主服务端处理
    else  // 我就是主服务端
      ProcessAgent(vAgent);
  finally
    System.MonitorExit(FAgentLocker);  // 开队列
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
  FreeAndNil(FLogLocker);
end;

procedure TfrmBLLServer.FormShow(Sender: TObject);
var
  vIniFile: TIniFile;
begin
  vIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini');
  try
    FAlias := vIniFile.ReadString('RemoteServer', 'Alias', 'emr业务(BLL)服务端');  // 别名

    if vIniFile.ReadBool('RemoteServer', 'AutoStart', False) then  // 启动后立即开启服务
      mniStartClick(Sender);
  finally
    FreeAndNil(vIniFile);
  end;

  RefreshUIState;
end;

procedure TfrmBLLServer.mniConnectClick(Sender: TObject);
var
  vfrmConnSet: TfrmConnSet;
begin
  vfrmConnSet := TfrmConnSet.Create(Self);
  try
    if FileExists(ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini') then
      vfrmConnSet.FileName := ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini'
    else
      vfrmConnSet.FileName := '';

    vfrmConnSet.ShowModal;
    if FTcpServer.Active then  // 服务已运行
      ReCreateServer;  // 根据新设置重置服务端
  finally
    FreeAndNil(vfrmConnSet);
  end;
end;

procedure TfrmBLLServer.mniBLLSetClick(Sender: TObject);
var
  vFrmBLLSet: TfrmBLLSet;
begin
  vFrmBLLSet := TfrmBLLSet.Create(nil);
  try
    vFrmBLLSet.ShowModal;
  finally
    FreeAndNil(vFrmBLLSet);
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
  vStream: TMemoryStream;
begin
  try
    vStream := TMemoryStream.Create;
    try
      AAgent.Stream.Position := 0;
      TZipTools.UnZipStream(AAgent.Stream, vStream);  // 解压缩
      vStream.Position := 0;
      FDBL.MsgPack.DecodeFromStream(vStream);  // 解包
      FDBL.ExecuteMsgPack;  // 执行
      vStream.Clear;
      FDBL.MsgPack.EncodeToStream(vStream);  // 打包
      vStream.Position := 0;
      TZipTools.ZipStream(vStream, AAgent.Stream);  // 压缩数据
      AAgent.Stream.Position := 0;
      TBLLClientContext(AAgent.Context).SendStream(AAgent.Stream);  // 推送到客户端
    finally
      vStream.Free;
    end;
  finally
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
    Caption := FAlias + '[运行]' + FTcpServer.DefaultListenAddress + ' 端口:' + IntToStr(FTcpServer.Port)
  else
    Caption := FAlias + '[停止]';

  mniStop.Enabled := FTcpServer.Active;
  mniBLLSet.Enabled := mniStop.Enabled;
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
      TBLLClientContext(AAgent.Context).SendStream(AAgent.Stream);  // 推送到客户端
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vMsgPack);
    FreeAndNil(AAgent);
  end;
end;

end.
