unit frm_UpdateServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Generics.Collections, diocp_tcp_server, diocp_coder_tcpServer, UPMsgPack,
  UPClientContext, Vcl.Menus, Vcl.ComCtrls;

type
  TUpdateFile = class(TObject)
  public
    &Name, Path, Version: string;
    VerNo, Size: Cardinal;
  end;

  TUpdateFiles = class(TObjectList<TUpdateFile>)
  public
    procedure AppendFile(const AVerNo, ASize: Cardinal;
      const AName, APath, AVersion: string);
  end;

  TSendFile = class(TObject)
  public
    FileName: string;
    FileStream: TFileStream;
    constructor Create(const AFile: string);
    destructor Destroy; override;
  end;

  TfrmUpdateServer = class(TForm)
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
    btnUpgrade: TButton;
    btnUpdateLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mniStartClick(Sender: TObject);
    procedure btnUpgradeClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mniStopClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnUpdateLogClick(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TDiocpCoderTcpServer;
    FSendFiles: TObjectList<TSendFile>;
    function GetSendFileIndex(const AFile: string): Integer;
    procedure RefreshStateUI;
    procedure DoContextAction(const AStream: TStream; const AContext: TUPClientContext);
    procedure OnContextConnected(AClientContext: TIocpClientContext);
    procedure OnContextDisConnected(AClientContext: TIocpClientContext);
    //
    procedure CheckUpdateFiles(const AClientContext: TUPClientContext; const AVerNo: Integer);
    procedure SendUpgradeFile(const AClientContext: TUPClientContext; const APath: string;
      const APos: Cardinal);
  public
    { Public declarations }
  end;

var
  frmUpdateServer: TfrmUpdateServer;

implementation

uses
  utils_safeLogger, utils_zipTools, uFMMonitor, UPCommon, Xml.XMLIntf, Xml.XMLDoc,
  frm_DM, frm_Upgrade, FireDAC.Comp.Client, ActiveX, UPMsgCoder, diocp_task,
  frm_UpgradeHis, System.Math;

{$R *.dfm}

function GetOSVersion: string;
begin
  Result := TOSVersion.ToString;
  {$IFDEF CPUX64}
  Result := Result + ' - 64bit';
  {$ENDIF}
  {$IFDEF CPUX86}
  Result := Result + ' - 32bit';
  {$ENDIF}
end;

procedure TfrmUpdateServer.btnClearClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TfrmUpdateServer.btnUpdateLogClick(Sender: TObject);
begin
  frmUpgradeHis.Show;
end;

procedure TfrmUpdateServer.btnUpgradeClick(Sender: TObject);
var
  vFrmUpgrade: TfrmUpgrade;
begin
  vFrmUpgrade := TfrmUpgrade.Create(nil);
  try
    vFrmUpgrade.ShowModal;
  finally
    FreeAndNil(vFrmUpgrade);
  end;
end;

procedure TfrmUpdateServer.CheckUpdateFiles(
  const AClientContext: TUPClientContext; const AVerNo: Integer);
var
  vFileNode: IXMLNode;
  vFiles: TUpdateFiles;
  vVerNo: Cardinal;
  vVersion, vMemo: string;

  procedure _SendUpdateFiles;
  var
    vXmlDocNew: IXMLDocument;
    vStream: TMemoryStream;
    vMsgPack: TUPMsgPack;
    i: Integer;
  begin
    vXmlDocNew := TXMLDocument.Create(nil);
    vXmlDocNew.Active := True;
    vXmlDocNew.Version := '1.0';
    //vXmlDoc.Encoding := GetEncodingName(AEncoding);

    vXmlDocNew.DocumentElement := vXmlDocNew.CreateNode('HCUpdateFile');
    vXmlDocNew.DocumentElement.Attributes['upfver'] := '1';
    vXmlDocNew.DocumentElement.Attributes['verno'] := vVerNo;
    vXmlDocNew.DocumentElement.Attributes['version'] := vVersion;
    vXmlDocNew.DocumentElement.Attributes['memo'] := vMemo;

    for i := 0 to vFiles.Count - 1 do
    begin
      vFileNode := vXmlDocNew.DocumentElement.AddChild('file' + (i + 1).ToString);
      vFileNode.Text := vFiles[i].Name;
      vFileNode.Attributes['verno'] := vFiles[i].VerNo;
      vFileNode.Attributes['version'] := vFiles[i].Version;
      vFileNode.Attributes['path'] := vFiles[i].Path;
      vFileNode.Attributes['size'] := vFiles[i].Size;
    end;

    vMsgPack := TUPMsgPack.Create;
    try
      vMsgPack.I[MSG_CMD] := CMD_UPDATEFIELS;
      vMsgPack.I[HCUP_VERNO] := vVerNo;

      vStream := TMemoryStream.Create;
      try
        vXmlDocNew.SaveToStream(vStream);
        vStream.Position := 0;
        vMsgPack.ForcePathObject(HCUP_UPDATEFILES).LoadBinaryFromStream(vStream);
      finally
        FreeAndNil(vStream);
      end;

      AClientContext.SendMsgPack(vMsgPack);
    finally
      FreeAndNil(vMsgPack);
    end;
  end;

  procedure _SendNoUpdateFiles;
  var
    vMsgPack: TUPMsgPack;
  begin
    vMsgPack := TUPMsgPack.Create;
    try
      vMsgPack.I[MSG_CMD] := CMD_UPDATEFIELS;
      vMsgPack.I[HCUP_VERNO] := AVerNo;
      AClientContext.SendMsgPack(vMsgPack);
    finally
      FreeAndNil(vMsgPack);
    end;
  end;

var
  vXmlDoc: IXMLDocument;
  vQuery: TFDQuery;
  i: Integer;
begin
  vVerNo := 0;

  vFiles := TUpdateFiles.Create;
  try
    vQuery := dm.OpenSql(Format('SELECT verno, version, memo, files ' +
      'FROM UpdateInfo WHERE verno > %d ORDER BY verno DESC', [AVerNo]));

    try
      if vQuery.RecordCount = 0 then
        _SendNoUpdateFiles
      else
      //if vQuery.RecordCount > 0 then  // 取跨版本合并后的文件
      begin
        vQuery.First;
        // 记录最新版本的信息
        vVerNo := vQuery.FieldByName('verno').AsInteger;
        vVersion := vQuery.FieldByName('version').AsString;
        vMemo := vQuery.FieldByName('memo').AsString;

        vXmlDoc := TXMLDocument.Create(nil);
        CoInitialize(nil);
        try
          while not vQuery.Eof do
          begin
            vXmlDoc.LoadFromXML(vQuery.FieldByName('files').AsString);
            //vXmlDoc.DocumentElement.Attributes['upfver']
            for i := 0 to vXmlDoc.DocumentElement.ChildNodes.Count - 1 do
            begin
              vFileNode := vXmlDoc.DocumentElement.ChildNodes[i];
              vFiles.AppendFile(
                vQuery.FieldByName('verno').AsInteger,
                vFileNode.Attributes['size'],
                vFileNode.Text,
                vFileNode.Attributes['path'],
                vFileNode.Attributes['version']);
            end;

            vQuery.Next;
          end;

          _SendUpdateFiles;
        finally
          CoUninitialize;
        end;
      end;
    finally
      vQuery.Free;
    end;
  finally
    FreeAndNil(vFiles);
  end;
end;

procedure TfrmUpdateServer.DoContextAction(const AStream: TStream;
  const AContext: TUPClientContext);
var
  vMsgPack: TUPMsgPack;
  vStream: TMemoryStream;
begin
  vMsgPack := TUPMsgPack.Create;
  try
    vStream := TMemoryStream.Create;
    try
      TZipTools.UnZipStream(AStream, vStream);  // 解压缩
      vStream.Position := 0;
      vMsgPack.DecodeFromStream(vStream);  // 解包

      case vMsgPack.I[MSG_CMD] of  // 请求的消息指令码
        CMD_CHECKVERSION: CheckUpdateFiles(AContext, vMsgPack.I[HCUP_VERNO]);

        CMD_DOWNLOADFILE: SendUpgradeFile(AContext, vMsgPack.S[HCUP_FILEPATH], vMsgPack.I[HCUP_FILEPOS]);
      end;
    finally
      vStream.Free;
    end;
  finally
    FreeAndNil(vMsgPack);
  end;
end;

procedure TfrmUpdateServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FTcpServer.Active then
  begin
    if MessageDlg('确定要停止并关闭服务端？关闭后客户端不能处理业务！',
      mtWarning, [mbYes, mbNo], 0) = mrYes
    then
    begin
      mniStopClick(Sender);
      CanClose := True;
    end
    else
      CanClose := False;
  end;
end;

procedure TfrmUpdateServer.FormCreate(Sender: TObject);
begin
  sfLogger.setAppender(TStringsAppender.Create(mmoLog.Lines));
  sfLogger.AppendInMainThread := True;

  UpgradePath := ExtractFilePath(ParamStr(0)) + 'Client';
  if not DirectoryExists(UpgradePath) then
    CreateDir(UpgradePath);

  pgc.ActivePageIndex := 0;
  FTcpServer := TDiocpCoderTcpServer.Create(Self);
  FTcpServer.Port := 12840;
  FTcpServer.SetMaxSendingQueueSize(MAX_OBJECT_SIZE div MAX_BLOCK_SIZE + 1);

  FTcpServer.WorkerCount := 0;
  IocpTaskManager.SetWorkerCount(FTcpServer.WorkerCount);
  FTcpServer.UseObjectPool := True;  // 使用对象池
  FTcpServer.RegisterCoderClass(TUPMsgDecoder, TUPMsgEncoder);
  FTcpServer.RegisterContextClass(TUPClientContext);

  FTcpServer.OnContextConnected := OnContextConnected;
  FTcpServer.OnContextDisconnected := OnContextDisConnected;

  // 创建数据监控中心实例 FDataMoniter
  FTcpServer.CreateDataMonitor;
  TFMMonitor.CreateAsChild(tsState, FTcpServer);

  FSendFiles := TObjectList<TSendFile>.Create;
end;

procedure TfrmUpdateServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSendFiles);
end;

function TfrmUpdateServer.GetSendFileIndex(const AFile: string): Integer;
var
  i: Integer;
  vSendFile: TSendFile;
begin
  Result := -1;

  for i := 0 to FSendFiles.Count - 1 do
  begin
    if FSendFiles[i].FileName = AFile then
    begin
      Result := i;
      Exit;
    end;
  end;

  if Result < 0 then
  begin
    vSendFile := TSendFile.Create(AFile);
    Result := FSendFiles.Add(vSendFile);
  end;
end;

{ TUpdateFiles }

procedure TUpdateFiles.AppendFile(const AVerNo, ASize: Cardinal; const AName,
  APath, AVersion: string);
var
  i: Integer;
  vFile: TUpdateFile;
begin
  for i := 0 to Self.Count - 1 do
  begin
    if (Self[i].Name = AName) and (Self[i].Path = APath) then  // 相同文件
    begin
      if Self[i].VerNo < AVerNo then  // 版本号小于要添加的
      begin
        Self[i].VerNo := AVerNo;
        Self[i].Version := AVersion;
        Self[i].Size := ASize;
      end;

      Exit;
    end;
  end;

  // 没有相同文件
  vFile := TUpdateFile.Create;
  vFile.VerNo := AVerNo;
  vFile.Size := ASize;
  vFile.Name := AName;
  vFile.Path := APath;
  vFile.Version := AVersion;
  Self.Add(vFile);
end;

procedure TfrmUpdateServer.mniStartClick(Sender: TObject);
begin
  try
    mmoLog.Clear;
    FTcpServer.Listeners.ClearObjects;
    FTcpServer.Active := True;
    RefreshStateUI;
    sfLogger.logMessage(GetOSVersion);
    pgc.ActivePageIndex := 1;
  except
    on E: Exception do
    begin
      Caption := '启动失败：' + E.Message;
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmUpdateServer.mniStopClick(Sender: TObject);
begin
  FTcpServer.SafeStop;
  sfLogger.logMessage('升级文件服务端停止！');
  FSendFiles.Clear;
  RefreshStateUI;
end;

procedure TfrmUpdateServer.OnContextConnected(
  AClientContext: TIocpClientContext);
begin
  TUPClientContext(AClientContext).OnContextAction := DoContextAction;
  sfLogger.logMessage('有客户端连接！');
end;

procedure TfrmUpdateServer.OnContextDisConnected(
  AClientContext: TIocpClientContext);
begin
  sfLogger.logMessage('有客户端断开！');
end;

procedure TfrmUpdateServer.RefreshStateUI;
begin
  mniStart.Enabled := not FTcpServer.Active;
  mniStop.Enabled := not mniStart.Enabled;

  if mniStart.Enabled then
  begin
    Self.Caption := '升级文件服务端[停止]';
    btnUpgrade.Enabled := True;
  end
  else
  begin
    Self.Caption := Format('升级文件服务端[运行]:%d', [FTcpServer.Port]);
    btnUpgrade.Enabled := False;
  end;
end;

procedure TfrmUpdateServer.SendUpgradeFile(
  const AClientContext: TUPClientContext; const APath: string; const APos: Cardinal);
var
  vMsgPack: TUPMsgPack;
  vPath: string;
  vSendSize: Cardinal;
  vFileStream: TFileStream;
  vFileIndex: Integer;
begin
  vMsgPack := TUPMsgPack.Create;
  try
    vMsgPack.I[MSG_CMD] := CMD_DOWNLOADFILE;
    vMsgPack.S[HCUP_FILEPATH] := APath;

    vPath := UpgradePath + APath;

    if FileExists(vPath) then
    begin
      vFileIndex := GetSendFileIndex(vPath);
      if vFileIndex >= 0 then
      begin
        vFileStream := FSendFiles[vFileIndex].FileStream;

        vFileStream.Position := APos;
        if APos = 0 then
        begin
          sfLogger.logMessage('正在发送文件：' + vPath);
          vMsgPack.I[HCUP_FILESIZE] := vFileStream.Size;
        end;

        vSendSize := Min(MAX_BLOCK_SIZE, vFileStream.Size - vFileStream.Position);  // 本次传输多大
        vMsgPack.ForcePathObject(HCUP_FILE).LoadBinaryFromStream(vFileStream, vSendSize);
        vMsgPack.I[HCUP_FILEPOS] := APos + vSendSize;

        if vFileStream.Position = vFileStream.Size then  // 发送完成后不要占用文件，防止再升级时不能替换
          sfLogger.logMessage('发送完成：' + vPath);
      end
      else
      begin
        vMsgPack.I[HCUP_FILEPOS] := 0; // 没找到文件
        sfLogger.logMessage('缺失文件：' + vPath);
      end;
    end
    else  // 要下载的文件缺失
    begin
      vMsgPack.I[HCUP_FILEPOS] := 0;
      sfLogger.logMessage('缺失文件：' + vPath);
    end;

    TUPClientContext(AClientContext).SendMsgPack(vMsgPack);
  finally
    FreeAndNil(vMsgPack);
  end;
end;

{ TSendFile }

constructor TSendFile.Create(const AFile: string);
begin
  FileName := AFile;
  FileStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyRead);
end;

destructor TSendFile.Destroy;
begin
  FileName := '';
  FreeAndNil(FileStream);
  inherited Destroy;
end;

end.
