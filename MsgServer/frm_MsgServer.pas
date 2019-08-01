unit frm_MsgServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Net.CrossSocket.Base, HCSocket, emr_DataBase;

type
  TfrmMsgServer = class(TForm)
    pgc: TPageControl;
    tsState: TTabSheet;
    ts2: TTabSheet;
    pnl1: TPanel;
    chkLog: TCheckBox;
    btnClear: TButton;
    mmoLog: TMemo;
    mmMain: TMainMenu;
    mniN1: TMenuItem;
    mniStart: TMenuItem;
    mniStop: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mniStartClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniStopClick(Sender: TObject);
  private
    { Private declarations }
    FMsgServer: IHCRSocket;
    FLogLocker: TObject;
    FDB: TDataBase;
    /// <summary> 重新设置服务器 </summary>
    procedure ReCreateServer;
    procedure RefreshUIState;

    procedure DoLog(const ALog: string);

    procedure DoServerConnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoServerDisconnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoServerReceiveData(const AConnection: ICrossConnection; const AStream: TBytesStream);
    procedure DoServerError(const AError: string);
  public
    { Public declarations }
  end;

var
  frmMsgServer: TfrmMsgServer;

implementation

{$R *.dfm}

procedure TfrmMsgServer.DoLog(const ALog: string);
begin
  if chkLog.Checked then
  begin
    System.MonitorEnter(FLogLocker);
    try
      mmoLog.Lines.Add(sLineBreak + '=============='
        + FormatDateTime('YYYY-MM-DD HH:mm:ss', Now)
        + '=============='
        + sLineBreak + ALog);
    finally
      System.MonitorExit(FLogLocker);
    end;
  end;
end;

procedure TfrmMsgServer.DoServerConnected(Sender: TObject;
  AConnection: ICrossConnection);
begin
  //sfLogger.logMessage('(ScreenServer)有Client连接成功');
end;

procedure TfrmMsgServer.DoServerDisconnected(Sender: TObject;
  AConnection: ICrossConnection);
begin
  //sfLogger.logMessage('(SC)断开');
end;

procedure TfrmMsgServer.DoServerError(const AError: string);
begin
  //sfLogger.logMessage('(SS)错误：' + AError);
end;

procedure TfrmMsgServer.DoServerReceiveData(const AConnection: ICrossConnection;
  const AStream: TBytesStream);
begin
  // AStream.ReadBuffer(vCMD, 1);
end;

procedure TfrmMsgServer.FormCreate(Sender: TObject);
begin
  FLogLocker := TObject.Create;

  FMsgServer := THCRServer.Create;
  FMsgServer.Port := 12820;
  FMsgServer.OnConnected := DoServerConnected;
  FMsgServer.OnDisconnected := DoServerDisconnected;
  FMsgServer.OnReceiveData := DoServerReceiveData;
  FMsgServer.OnError := DoServerError;

  FDB := TDataBase.Create(nil);
end;

procedure TfrmMsgServer.FormDestroy(Sender: TObject);
begin
  FMsgServer.Active := False;
  FreeAndNil(FDB);
  FreeAndNil(FLogLocker);
end;

procedure TfrmMsgServer.mniStartClick(Sender: TObject);
begin
  try
    ReCreateServer;  // 设置服务器

    pgc.ActivePageIndex := 1;  // 切换到监控页，否则客户端连接后界面不能切换bug
    FMsgServer.Active := true;

    //if FAgentQueueThread.Suspended then
    //  FAgentQueueThread.Suspended := False;

    RefreshUIState;
  except
    on E: Exception do
    begin
      Caption := '启动失败：' + E.Message;
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmMsgServer.mniStopClick(Sender: TObject);
begin
  FMsgServer.Active := False;
  RefreshUIState;
end;

procedure TfrmMsgServer.ReCreateServer;
begin
  if not FDB.Connected then
  begin
    FDB.DBType := dbSqlServer;
    //FDB.Server := BLLServerParams.DataBaseServer;
    //FDB.DBName := BLLServerParams.DataBaseName;
    //FDB.Username := BLLServerParams.DataBaseUsername;
    //FDB.Password := BLLServerParams.DataBasePassword;
    //FDB.Connect;
  end;
end;

procedure TfrmMsgServer.RefreshUIState;
begin
  mniStart.Enabled := not FMsgServer.Active;
  if FMsgServer.Active then
    Caption := 'emr消息服务端[运行]' + FMsgServer.Host + ' 端口:' + IntToStr(FMsgServer.Port)
  else
    Caption := 'emr消息服务端[停止]';

  mniStop.Enabled := FMsgServer.Active;
end;

end.
