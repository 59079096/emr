{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_UpDownLoadClient;

interface

uses
  System.Classes, SysUtils, IdTCPClient, IdGlobal, emr_MsgPack;

type
  TShowDataEvent = reference to procedure(const
    AReciveSize,  // 已经收到的大小
    AFileSize     // 文件总大小
      : Integer);

  TIdTCPClientError = procedure(const AError: string) of object;

  TUpDownLoadClient = class(TObject)  { TODO : 要改为继承fram_MsgClient中的TMsgClient，以统一消息类型的客户端连接 }
  private
    FReconnect: Boolean;
    FIdTcpClient: TIdTCPClient;
    FOnError: TIdTCPClientError;
    FCurError: string;
    procedure CheckConnect;

    function RecvRawBuffer(buf: Pointer; len: Cardinal): Integer;
    function SendRawBuffer(buf: Pointer; len: Cardinal): Integer;
    function SendStream(pvStream: TStream): Integer;
  protected
    function GetHost: string;
    procedure SetHost(const AValue: string);
    function GetPort: Word;
    procedure SetPort(const AValue: Word);
    function SendBuf(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function RecvBuf(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    procedure CloseSocket; stdcall;
  public
    /// <summary>
    ///   返回一个ICoderSocket接口
    /// </summary>
    /// <param name="ATcpClient"> 需要使用的IdTcp组件 </param>
    /// <param name="pvReconnect"> 如果组件未打开是否打开连接 </param>
    constructor Create(AReconnect: Boolean = true);
    destructor Destroy; override;

    function SendDataStream(const AStream: TMemoryStream): Cardinal;
    function RecviceDataStream(const AStream: TMemoryStream): Boolean;
    procedure Connect;
    function Connected: Boolean;

    /// <summary>
    /// 从服务端下载指定文件
    /// </summary>
    /// <param name="AFileName">服务端相对路径：文件路径 + 文件名</param>
    /// <param name="AFileStream">从服务器接受到的文件映射到的本地文件流</AFileStream>
    /// <param name="AShowDataEvent">接收到服务端数据时触发事件</param>
    /// <returns>True: 下载成功</returns>
    function DownLoadFile(const ARelativeFileName: string; const AFileStream: TFileStream;
      const AShowDataEvent: TShowDataEvent = nil): Boolean;

    /// <summary>
    /// 上传指定文件到服务端
    /// </summary>
    /// <param name="AFileName">服务端相对路径：文件路径 + 文件名</param>
    /// <param name="AFileStream">本地上传文件流</AFileStream>
    /// <param name="AShowDataEvent">上传数据时触发事件</param>
    /// <returns>True: 上传成功</returns>
    function UpLoadFile(const ARelativeFileName: string; const AFileStream: TFileStream;
      const AShowDataEvent: TShowDataEvent = nil): Boolean;

    //property TcpClient: TIdTCPClient read FTcpClient;
    property Host: string read GetHost write SetHost;
    property Port: Word read GetPort write SetPort;
    property CurError: string read FCurError write FCurError;
    property OnError: TIdTCPClientError read FOnError write FOnError;
  end;

implementation

uses
  emr_MsgConst, utils_byteTools, System.Math;

function VerifyData(const buf; len: Cardinal): Cardinal;
var
  i:Cardinal;
  p:PByte;
begin
  i := 0;
  Result := 0;
  p := PByte(@buf);
  while i < len do
  begin
    Result := Result + p^;
    Inc(p);
    Inc(i);
  end;
end;

constructor TUpDownLoadClient.Create(AReconnect: Boolean = true);
begin
  inherited Create;
  FIdTcpClient := TIdTCPClient.Create;
  FReconnect := AReconnect;
  FCurError := '';
end;

destructor TUpDownLoadClient.Destroy;
begin
  FIdTcpClient.Free;
  inherited Destroy;
end;

function TUpDownLoadClient.DownLoadFile(const ARelativeFileName: string;
  const AFileStream: TFileStream; const AShowDataEvent: TShowDataEvent): Boolean;
var
  vDataStream: TMemoryStream;
  vMsgPack: TMsgPack;
  vBytes: TBytes;
  vBlockSize: Cardinal;
begin
  Result := False;
  FCurError := '';
  if Connected then  // 连接成功
  begin
    vDataStream := TMemoryStream.Create;
    try
      vMsgPack := TMsgPack.Create;

      while True do
      begin
        vMsgPack.ForcePathObject(MSG_CMD).AsInteger := CMD_DOWNLOAD;
        vMsgPack.ForcePathObject(FROMCLIENTTYPE).AsInteger := Ord(TClientType.cctLSD);  // 大屏
        vMsgPack.ForcePathObject('file').AsString := ARelativeFileName;
        vMsgPack.I['start'] := AFileStream.Position;

        vDataStream.Clear;
        vMsgPack.EncodeToStream(vDataStream);  // 编码

        Self.SendDataStream(vDataStream);  // 数据发送到服务端

        vDataStream.Clear;
        Self.RecviceDataStream(vDataStream);  // 从服务端接收数据
        vDataStream.Position := 0;
        vMsgPack.DecodeFromStream(vDataStream);  // 解码

        if not vMsgPack.B[BACKRESULT] then
        begin
          FCurError := vMsgPack.S[BACKMSG];
          Exit;
        end;

        vBlockSize := vMsgPack.ForcePathObject('blockSize').AsInteger;  // 本次传来多大的数据
        if vBlockSize > 0 then  // 有接收数据
        begin
          vBytes := vMsgPack.ForcePathObject('data').AsBytes;
          AFileStream.Write(vBytes[0], Length(vBytes));  // 将接受的数据写入文件中

          if Assigned(AShowDataEvent) then  // 处理接受的数据
            AShowDataEvent(AFileStream.Size, vMsgPack.I['fileSize']);
        end
        else
        begin
          FCurError := '异常：下载文件 ' + ARelativeFileName + ' 时接收到大小为0的数据！'
            + vMsgPack.S[BACKMSG];
          Exit;
        end;

        if AFileStream.Size = vMsgPack.I['fileSize'] then  //文件下载完成
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      vDataStream.Free;
      vMsgPack.Free;
    end;
  end;
end;

function TUpDownLoadClient.GetHost: string;
begin
  Result := FIdTcpClient.Host;
end;

function TUpDownLoadClient.GetPort: Word;
begin
  Result := FIdTcpClient.Port;
end;

procedure TUpDownLoadClient.CheckConnect;
begin
  if (not FIdTcpClient.Connected) then
  begin
    try
      FIdTcpClient.Connect();
    except
      on E:Exception do
      begin
        raise Exception.Create(
          Format('与服务器[%s:%d]建立连接失败', [FIdTcpClient.Host, FIdTcpClient.Port]) + sLineBreak + e.Message);
      end;
    end;
  end;
end;

procedure TUpDownLoadClient.CloseSocket;
begin
  try
    FIdTcpClient.Disconnect;
  except
  end;
end;

procedure TUpDownLoadClient.Connect;
begin
  FIdTcpClient.Connect;
end;

function TUpDownLoadClient.Connected: Boolean;
begin
  Result := False;
  try  // 用try防止非正常断线获取连接状态时出错
    Result := FIdTCPClient.Connected;
  except
    on e: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(e.Message)
      else
        raise Exception.Create(e.Message);
    end;
  end;
end;

function TUpDownLoadClient.RecvBuf(buf:Pointer; len:Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    CheckConnect;
    try
      Result := RecvRawBuffer(buf, len);
    except
      CloseSocket;
      raise;
    end;
  end
  else
  begin
    Result := RecvRawBuffer(buf, len);
  end;
end;

function TUpDownLoadClient.RecviceDataStream(const AStream: TMemoryStream): Boolean;
var
  lvBytes:TBytes;
  lvReadL, lvTempL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
  lvVerifyValue, lvVerifyDataValue: Cardinal;
  lvPByte:PByte;
begin
  Result := False;

  (* 此处代码要和emr_StreamCoder中的保持一致 *)
  RecvBuf(@lvPACK_FLAG, SizeOf(lvPACK_FLAG));

  if lvPACK_FLAG <> PACK_FLAG then  // 错误的包数据
  begin
    FIdTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //veri value
  RecvBuf(@lvVerifyValue, SizeOf(lvVerifyValue));

  //headlen
  RecvBuf(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  if lvDataLen > MAX_OBJECT_SIZE then  // 文件头过大,错误的包数据
  begin
    FIdTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorData);
  end;

  SetLength(lvBytes,lvDataLen);
  lvPByte := PByte(@lvBytes[0]);
  lvReadL := 0;
  while lvReadL < lvDataLen do
  begin
    lvTempL := RecvBuf(lvPByte, lvDataLen - lvReadL);
    if lvTempL = -1 then
    begin
      RaiseLastOSError;
    end;
    Inc(lvPByte, lvTempL);
    lvReadL := lvReadL + lvTempL;
  end;

{$IFDEF POSIX}
  lvVerifyDataValue := verifyData(lvBytes[0], lvDataLen);
{$ELSE}
  lvVerifyDataValue := verifyData(lvBytes[0], lvDataLen);
{$ENDIF}

  if lvVerifyDataValue <> lvVerifyValue then
    raise Exception.Create(strRecvException_VerifyErr);

  AStream.Clear;
  AStream.Write(lvBytes[0], lvDataLen);
  Result := True;
end;

function TUpDownLoadClient.RecvRawBuffer(buf: Pointer; len: Cardinal): Integer;
var
  lvBuf: TIdBytes;
begin
  FIdTcpClient.Socket.ReadBytes(lvBuf, len);
  Result := Length(lvBuf);
  Move(lvBuf[0], buf^, Result);
  SetLength(lvBuf, 0);
end;

function TUpDownLoadClient.SendBuf(buf:Pointer; len:Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    CheckConnect;;
    try
      Result := SendRawBuffer(buf, len);
    except
      CloseSocket;
      raise;
    end;
  end
  else
  begin
    Result := SendRawBuffer(buf, len);
  end;
end;

function TUpDownLoadClient.SendRawBuffer(buf: Pointer; len: Cardinal): Integer;
var
  lvBytes:TIdBytes;
begin
  SetLength(lvBytes, len);
  Move(buf^, lvBytes[0], len);
  FIdTcpClient.Socket.Write(lvBytes);
  SetLength(lvBytes, 0);
  Result := len;
end;

function TUpDownLoadClient.SendStream(pvStream: TStream): Integer;
var
  lvBufBytes: array[0..BUF_BLOCK_SIZE - 1] of byte;
  l, j, r, lvTotal: Integer;
  P: PByte;
begin
  Result := 0;
  if pvStream = nil then Exit;
  if pvStream.Size = 0 then Exit;
  lvTotal :=0;

  pvStream.Position := 0;
  repeat
    //FillMemory(@lvBufBytes[0], SizeOf(lvBufBytes), 0);
    l := pvStream.Read(lvBufBytes[0], SizeOf(lvBufBytes));
    if (l > 0) then
    begin
      P := PByte(@lvBufBytes[0]);
      j := l;
      while j > 0 do
      begin
        r := SendBuf(P, j);
        if r = -1 then
          RaiseLastOSError;
        Inc(P, r);
        Dec(j, r);
      end;
      lvTotal := lvTotal + l;
    end
    else
      Break;
  until (l = 0);
  Result := lvTotal;
end;

function TUpDownLoadClient.SendDataStream(const AStream: TMemoryStream): Cardinal;
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
  lvStream: TMemoryStream;
  lvVerifyValue: Cardinal;
begin
  (* 此处代码要和emr_StreamCoder中的保持一致 *)
  lvPACK_FLAG := PACK_FLAG;

  lvStream := TMemoryStream.Create;
  try
    AStream.Position := 0;

    if AStream.Size > MAX_OBJECT_SIZE then
      raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);

    lvStream.Write(lvPACK_FLAG, SizeOf(lvPACK_FLAG));  // 包头

    lvDataLen := AStream.Size;

    // stream data
    SetLength(lvBuf, lvDataLen);
    AStream.Read(lvBuf[0], lvDataLen);
    //veri value
    lvVerifyValue := verifyData(lvBuf[0], lvDataLen);

    lvStream.Write(lvVerifyValue, SizeOf(lvVerifyValue));

    lvWriteIntValue := TByteTools.swap32(lvDataLen);

    // stream len
    lvStream.Write(lvWriteIntValue, SizeOf(lvWriteIntValue));

    // send pack
    lvStream.write(lvBuf[0], lvDataLen);

    Result := SendStream(lvStream);
  finally
    lvStream.Free;
  end;
end;

procedure TUpDownLoadClient.SetHost(const AValue: string);
begin
  FIdTcpClient.Host := AValue;
end;

procedure TUpDownLoadClient.SetPort(const AValue: Word);
begin
  FIdTcpClient.Port := AValue;
end;

function TUpDownLoadClient.UpLoadFile(const ARelativeFileName: string;
  const AFileStream: TFileStream;
  const AShowDataEvent: TShowDataEvent): Boolean;
var
  vDataStream: TMemoryStream;
  vMsgPack: TMsgPack;
  vBytes: TBytes;
  vBlockSize: Cardinal;
begin
  Result := False;
  FCurError := '';
  if Connected then  // 连接成功
  begin
    vDataStream := TMemoryStream.Create;
    try
      vMsgPack := TMsgPack.Create;

      AFileStream.Position := 0;
      while True do
      begin
        // 准备上传协议和文件数据
        vMsgPack.Clear;
        vMsgPack.ForcePathObject(MSG_CMD).AsInteger := CMD_UPLOAD;
        vMsgPack.ForcePathObject(FROMCLIENTTYPE).AsInteger := Ord(TClientType.cctLSD);  // 大屏
        vMsgPack.ForcePathObject('file').AsString := ARelativeFileName;
        vMsgPack.I['start'] := AFileStream.Position;
        vMsgPack.I['fileSize'] := AFileStream.Size;
        // 添加文件数据
        vBlockSize := Min(MAX_UPLOAD_SIZE, AFileStream.Size - AFileStream.Position);
        if vBlockSize = 0 then  // 文件剩余大小
        begin
          FCurError := '异常：文件上传时数据大小为0！';
          Exit;
        end
        else
          vMsgPack.ForcePathObject('data').LoadBinaryFromStream(AFileStream, vBlockSize);

        vDataStream.Clear;
        vMsgPack.EncodeToStream(vDataStream);  // 编码

        Self.SendDataStream(vDataStream);  // 数据发送到服务端

        vDataStream.Clear;
        Self.RecviceDataStream(vDataStream);  // 从服务端接收数据
        vDataStream.Position := 0;
        vMsgPack.DecodeFromStream(vDataStream);  // 解码

        if not vMsgPack.B[BACKRESULT] then
        begin
          FCurError := vMsgPack.S[BACKMSG];
          Exit;
        end;

        if Assigned(AShowDataEvent) then  // 处理接受的数据
          AShowDataEvent(AFileStream.Position, AFileStream.Size);

        if AFileStream.Position = AFileStream.Size then  // 上传完了
        begin
          Result := True;
          Break;
        end;
      end;
    finally
      vDataStream.Free;
      vMsgPack.Free;
    end;
  end;
end;

end.
