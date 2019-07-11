unit frm_Update;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UPClient, UPMsgPack,
  Xml.XMLDoc, Xml.XMLIntf, System.IniFiles, Vcl.StdCtrls, Vcl.Grids;

type
  TFileProcess = procedure(const APos, ASize: Cardinal) of object;

  TfrmUpdate = class(TForm)
    sgdFiles: TStringGrid;
    mmo: TMemo;
    btnDownLoad: TButton;
    chkBackup: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDownLoadClick(Sender: TObject);
  private
    { Private declarations }
    FClient: TUPClient;
    FIniFile: TIniFile;
    FVerNo,  // 本地版本序号
    FLastVerNo,  // 最新的版本序号
    FDownFileIndex
      : Integer;

    FDownPath: string;  // 客户端路径(不带最后的\)

    procedure UpdateDownLoadHint(const AIndex: Integer; const AHint: string);

    /// <summary> 显示升级信息(包括文件) </summary>
    procedure ShowUpdateInfo(const AMsgPack: TUPMsgPack);

    procedure FileProcess(const APos, ASize: Cardinal);
    /// <summary> 下载所有要升级的文件 </summary>
    procedure DownLoadAllUpdateFile;
    /// <summary> 下载某个要升级的文件并备份和替换 </summary>
    procedure DownLoadAndUpdateAFile(const APath, AName: string;
      const AFileProc: TFileProcess);
  public
    { Public declarations }
    function CheckUpdate: Boolean;
  end;

function HCUpdate(const ABackUp: Boolean = False): Boolean;

implementation

uses
  UPCommon, Winapi.ShellAPI;

function HCUpdate(const ABackUp: Boolean = False): Boolean;
var
  vFrmUpdate: TfrmUpdate;
begin
  Result := False;

  vFrmUpdate := TfrmUpdate.Create(nil);
  try
    vFrmUpdate.chkBackup.Checked := ABackUp;
    Result := vFrmUpdate.CheckUpdate;
  finally
    FreeAndNil(vFrmUpdate);
  end;
end;

{$R *.dfm}

procedure TfrmUpdate.btnDownLoadClick(Sender: TObject);
begin
  if btnDownLoad.Tag = 0 then  // 未下载完成
  begin
    btnDownLoad.Enabled := False;
    chkBackup.Enabled := False;

    TThread.CreateAnonymousThread(procedure()
      begin
        DownLoadAllUpdateFile;  // 下载所有要升级的文件并替换

        Self.BorderIcons := Self.BorderIcons - [biSystemMenu];
        FIniFile.WriteInteger('version', 'lastverno', FLastVerNo);  // 写入最新版本序号
        btnDownLoad.Caption := '完成';
        btnDownLoad.Tag := 1;
        btnDownLoad.Enabled := True;
      end).Start;
  end
  else  // 下载完成，启动updaterp.exe
  begin
    ShellExecute(Handle, 'open', PChar(FDownPath + '\updaterp.exe'),
      PChar(ParamStr(0)), nil, SW_SHOW);

    Self.ModalResult := mrOk;
  end;
end;

function TfrmUpdate.CheckUpdate: Boolean;
var
  vMsgPack: TUPMsgPack;
begin
  Result := False;

  FVerNo := FIniFile.ReadInteger('version', 'verno', 0);

  FClient.Host := FIniFile.ReadString('connect', 'host', '127.0.0.1');
  FClient.Port := FIniFile.ReadInteger('connect', 'port', 12840);
  FClient.ReConnectServer;  // iocp
  //FClient.Connect;  // idtcp
  if FClient.Connected then
  begin
    vMsgPack := TUPMsgPack.Create;
    try
      vMsgPack.I[MSG_CMD] := CMD_CHECKVERSION;
      vMsgPack.I[HCUP_VERNO] := FVerNo;
      FClient.PostMsgPack(vMsgPack);  // 请求新版本
      FClient.ReceiveMsgPack(vMsgPack);
      if vMsgPack.I[HCUP_VERNO] <> FVerNo then  // 有新版本
      begin
        FLastVerNo := vMsgPack.I[HCUP_VERNO];
        ShowUpdateInfo(vMsgPack);
        Result := True;
      end;
    finally
      FreeAndNil(vMsgPack);
    end;
  end;
end;

procedure TfrmUpdate.DownLoadAllUpdateFile;
var
  i: Integer;
begin
  for i := 1 to sgdFiles.RowCount - 1 do
    sgdFiles.Cells[4, i] := '等待...';

  for i := 1 to sgdFiles.RowCount - 1 do
  begin
    sgdFiles.Row := i;
    FDownFileIndex := i;
    UpdateDownLoadHint(FDownFileIndex, '正在下载...');
    DownLoadAndUpdateAFile(sgdFiles.Cells[3, i], sgdFiles.Cells[0, i], FileProcess);
  end;

  if not FileExists(FDownPath + '\updaterp.exe') then
  begin
    mmo.Lines.Add('正在下载替换文件 updaterp.exe');
    DownLoadAndUpdateAFile('\', 'updaterp.exe', nil);
    mmo.Lines.Add('升级下载完成！');
  end;
end;

procedure TfrmUpdate.DownLoadAndUpdateAFile(const APath, AName: string;
  const AFileProc: TFileProcess);
var
  vMsgPack: TUPMsgPack;
  vStream: TMemoryStream;
  vFile: string;
  vPos, vFileSize: Cardinal;
  vRect: TRect;
begin
  vPos := 0;
  vFileSize := 0;

  vMsgPack := TUPMsgPack.Create;
  try
    vStream := TMemoryStream.Create;
    try
      while True do
      begin
        vMsgPack.Clear;
        vMsgPack.I[MSG_CMD] := CMD_DOWNLOADFILE;
        vMsgPack.S[HCUP_FILEPATH] := APath + AName;
        vMsgPack.I[HCUP_FILEPOS] := vPos;
        FClient.PostMsgPack(vMsgPack);  // 请求文件
        //Sleep(1);
        FClient.ReceiveMsgPack(vMsgPack);

        vPos := vMsgPack.I[HCUP_FILEPOS];
        if vPos = 0 then
        begin
          mmo.Lines.Add('升级失败，服务端缺失文件：' + APath + AName);
          Exit;
        end;

        if vFileSize = 0 then
        begin
          vFileSize := vMsgPack.I[HCUP_FILESIZE];
          vStream.Size := vFileSize;
        end;

        vMsgPack.ForcePathObject(HCUP_FILE).SaveBinaryToStream(vStream);

        if Assigned(AFileProc) then
          AFileProc(vPos, vFileSize); // 更新界面显示

        if vPos = vFileSize then  // 下载完了
          Break;
      end;

      UpdateDownLoadHint(FDownFileIndex, '正在替换...');

      if not DirectoryExists(FDownPath + APath) then  // 本地没有此目录则创建
        ForceDirectories(FDownPath + APath);

      vFile := FDownPath + APath + AName;
      if vFile = ParamStr(0) then  // 要升级自己
      begin
        FIniFile.WriteString('file', 'rp', vFile);  // 记录需要处理占用的运行程序
        FIniFile.WriteString('file', 'rpbackup', FDownPath + '\backup' + APath);
        vFile := vFile + '.temp';  // 先重命名把文件存下来
      end
      else  // 不是升级自己
      if chkBackup.Checked then // 备份原文件
      begin
        UpdateDownLoadHint(FDownFileIndex, '正在备份...');

        if FileExists(vFile) then
        begin
          if not DirectoryExists(FDownPath + '\backup' + APath) then  // 本地没有备份目录则创建
            ForceDirectories(FDownPath + '\backup' + APath);

          MoveFile(PChar(vFile), PChar(FDownPath + '\backup' + APath + AName));  // 移动到备份中
        end;
      end;

      UpdateDownLoadHint(FDownFileIndex, '正在保存...');
      vStream.SaveToFile(vFile);  // 下载的新文件保存到对应位置
      UpdateDownLoadHint(FDownFileIndex, '完成！');
    finally
      FreeAndNil(vStream);
    end;
  finally
    FreeAndNil(vMsgPack);
  end;
end;

procedure TfrmUpdate.FileProcess(const APos, ASize: Cardinal);
begin
  UpdateDownLoadHint(FDownFileIndex, FormatFloat('#%', APos / ASize * 100));
end;

procedure TfrmUpdate.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + '-升级';

  FDownPath := ExtractFilePath(ParamStr(0));
  Delete(FDownPath, Length(FDownPath), 1);

  FIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'update.ini');

  if not FIniFile.SectionExists('version') then
  begin
    FIniFile.WriteInteger('version', 'verno', 0);
    FIniFile.WriteInteger('version', 'lastverno', 0);
  end;

  if not FIniFile.SectionExists('connect') then
  begin
    FIniFile.WriteString('connect', 'host', '127.0.0.1');
    FIniFile.WriteInteger('connect', 'port', 12840);
  end;

  FIniFile.WriteString('file', 'run', ParamStr(0));

  FClient := TUPClient.Create;

  FDownFileIndex := -1;
  sgdFiles.ColCount := 5;
  sgdFiles.ColWidths[0] := 80;
  sgdFiles.ColWidths[1] := 100;
  sgdFiles.ColWidths[2] := 100;
  sgdFiles.ColWidths[3] := 150;
  sgdFiles.ColWidths[4] := 80;
  //
  sgdFiles.Cells[0, 0] := '文件名';
  sgdFiles.Cells[1, 0] := '版本';
  sgdFiles.Cells[2, 0] := '大小';
  sgdFiles.Cells[3, 0] := '客户端路径';
  sgdFiles.Cells[4, 0] := '下载进度';
end;

procedure TfrmUpdate.FormDestroy(Sender: TObject);
begin
  FClient.Free;
  FreeAndNil(FIniFile);
end;

procedure TfrmUpdate.ShowUpdateInfo(const AMsgPack: TUPMsgPack);
var
  vXmlDoc: IXMLDocument;
  vFileNode: IXMLNode;
  vStream: TMemoryStream;
  i: Integer;
begin
  vXmlDoc := TXMLDocument.Create(nil);
  // 取出要升级的文件
  vStream := TMemoryStream.Create;
  try
    AMsgPack.ForcePathObject(HCUP_UPDATEFILES).SaveBinaryToStream(vStream);
    vStream.Position := 0;
    vXmlDoc.LoadFromStream(vStream);
    //vXmlDoc.SaveToFile('c:\a.xml');
    //vXmlDoc.DocumentElement.Attributes['upfver'] = '1';
    mmo.Clear;
    mmo.Lines.Add('版本序号：' + vXmlDoc.DocumentElement.Attributes['verno']);
    mmo.Lines.Add('版本号：' + vXmlDoc.DocumentElement.Attributes['version']);
    mmo.Lines.Add('更新说明：' + vXmlDoc.DocumentElement.Attributes['memo']);

    sgdFiles.RowCount := vXmlDoc.DocumentElement.ChildNodes.Count + 1;
    if sgdFiles.RowCount > 1 then
      sgdFiles.FixedRows := 1;

    for i := 0 to vXmlDoc.DocumentElement.ChildNodes.Count - 1 do
    begin
      vFileNode := vXmlDoc.DocumentElement.ChildNodes[i];
      sgdFiles.Cells[0, i + 1] := vFileNode.Text;
      sgdFiles.Cells[1, i + 1] := vFileNode.Attributes['version'];
      sgdFiles.Cells[2, i + 1] := BytesToStr(vFileNode.Attributes['size']);
      sgdFiles.Cells[3, i + 1] := vFileNode.Attributes['path'];
    end;

    Self.ShowModal;
  finally
    FreeAndNil(vStream);
  end;

  FVerNo := AMsgPack.I[HCUP_VERNO];
end;

procedure TfrmUpdate.UpdateDownLoadHint(const AIndex: Integer;
  const AHint: string);
var
  vRect: TRect;
begin
  sgdFiles.Cells[4, AIndex] := AHint;
  vRect := sgdFiles.CellRect(4, AIndex);
  InvalidateRect(sgdFiles.Handle, vRect, False);
  UpdateWindow(sgdFiles.Handle);
end;

end.
