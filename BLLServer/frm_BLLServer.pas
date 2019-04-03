unit frm_BLLServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls, diocp_tcp_server;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniSetClick(Sender: TObject);
    procedure mniStartClick(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FTcpServer: TDiocpTcpServer;

    /// <summary> 重新设置服务器 </summary>
    procedure ReCreateServer;

    procedure RefreshUIState;
    procedure DoBLLServerMethodExecuteLog(const ALog: string);
  public
    { Public declarations }
  end;

var
  frmBLLServer: TfrmBLLServer;

implementation

uses
  uFMMonitor, BLLClientContext, BLLServerMethods, BLLServerParam,
  emr_DataBase, emr_BLLDataBase, frm_Set;

{$R *.dfm}

procedure TfrmBLLServer.btnClearClick(Sender: TObject);
begin
  mmoMsg.Clear;
end;

procedure TfrmBLLServer.DoBLLServerMethodExecuteLog(const ALog: string);
begin
  if chkLog.Checked then
    mmoMsg.Lines.Add(sLineBreak + '==============' + FormatDateTime('YYYY-MM-DD HH:mm:ss', Now) + '=============='
     + sLineBreak + ALog);
end;

procedure TfrmBLLServer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FTcpServer.Active then
  begin
    if MessageDlg('确定要停止并关闭hps业务服务端？关闭后客户端不能处理业务！',
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
begin
  pgc.ActivePageIndex := 0;
  FTcpServer := TDiocpTcpServer.Create(Self);
  FTcpServer.CreateDataMonitor;  // 创建运行监视器
  FTcpServer.WorkerCount := 3;
  FTcpServer.RegisterContextClass(TBLLClientContext);
  TFMMonitor.CreateAsChild(tsState, FTcpServer);
  //
  BLLServerMethod := TBLLServerMethod.Create;
  BLLServerMethod.OnExecuteLog := DoBLLServerMethodExecuteLog;
end;

procedure TfrmBLLServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTcpServer);
  FreeAndNil(BLLServerParams);
  FreeAndNil(BLLServerMethod);
  FreeAndNil(frameDB);
  FreeAndNil(frameBLLDB);
end;

procedure TfrmBLLServer.mniSetClick(Sender: TObject);
var
  vfrmSet: TfrmSet;
begin
  vfrmSet := TfrmSet.Create(Self);
  try
    vfrmSet.FileName := ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini';
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
  RefreshUIState;
end;

procedure TfrmBLLServer.ReCreateServer;
begin
  if BLLServerParams = nil then
    BLLServerParams := TBLLServerParams.Create(ExtractFilePath(ParamStr(0)) + 'emrBLLServer.ini');

  if BLLServerParams.RemoteActive then  // 指定了外部主服务器
  begin
    if GRemoteServer = nil then  // 创建过外部服务器
      GRemoteServer := TRemoteServer.CreateEx(BLLServerParams.RemoteBLLIP, BLLServerParams.RemoteBLLPort);
    if frameDB <> nil then  // 创建过框架数据库连接对象
      frameDB.DisConnect;
    if frameBLLDB <> nil then  // 创建过业务数据库连接管理对象
      frameBLLDB.DisConnect;
  end
  else  // 本机作为主服务器
  begin
    if frameDB = nil then  // 创建框架数据库连接对象
      frameDB := TDataBase.Create(Self);

    if frameBLLDB = nil then  // 创建框架业务数据库连接管理对象
      frameBLLDB := TBLLDataBase.Create;

    if not frameDB.Connected then
    begin
      frameDB.DBType := dbSqlServer;
      frameDB.Server := BLLServerParams.DataBaseServer;
      frameDB.DBName := BLLServerParams.DataBaseName;
      frameDB.Username := BLLServerParams.DataBaseUsername;
      frameDB.Password := BLLServerParams.DataBasePassword;
      frameDB.Connect;
    end;

    FreeAndNil(GRemoteServer);
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

end.
