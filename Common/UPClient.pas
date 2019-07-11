unit UPClient;

interface

uses
  Classes, diocp_tcp_blockClient, UPMsgPack, UPMsgCoder;

type
  TOnReceiveProcess = procedure(const AProcess, ASize: Integer) of object;

  TUPClient = class(TObject)
  private
    FTcpClient: TDiocpBlockTcpClient;
    FDataStream: TMemoryStream;
    FOnReceiveProcess: TOnReceiveProcess;
    procedure CheckConnect;
    function SendStream(pvStream: TStream): Integer;
    procedure DoError(const AErrCode: Integer; const AParam: string);
  protected
    function GetHost: string;
    procedure SetHost(const AHost: string);
    function GetPort: Integer;
    procedure SetPort(const APort: Integer);
    function GetActive: Boolean;
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    function SendDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function RecvDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function SendDataStream: Integer;
    function RecvDataStream: Boolean;
  public
    constructor Create; virtual;
    constructor CreateEx(const AHost: string; const APort: Integer);
    destructor Destroy; override;
    function Connected: Boolean;
    procedure ReConnectServer;
    procedure PostMsgPack(const AMsgPack: TUPMsgPack);
    procedure ReceiveMsgPack(const AMsgPack: TUPMsgPack);
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property Active: Boolean read GetActive;
    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    property OnReceiveProcess: TOnReceiveProcess read FOnReceiveProcess write FOnReceiveProcess;
  end;

implementation

uses
  SysUtils, utils_zipTools, utils_byteTools, DiocpError;

{ TUPClient }

procedure TUPClient.CheckConnect;
begin
  if not FTcpClient.Active then
    FTcpClient.Connect;
end;

function TUPClient.Connected: Boolean;
begin
  Result := FTcpClient.Active;
end;

constructor TUPClient.Create;
begin
  inherited Create;
  FTcpClient := TDiocpBlockTcpClient.Create(nil);
  FTcpClient.ReadTimeOut := 60000;
  FTcpClient.OnError := DoError;
  FDataStream := TMemoryStream.Create;
end;

constructor TUPClient.CreateEx(const AHost: string; const APort: Integer);
begin
  Create;

  FTcpClient.Host := AHost;
  FTcpClient.Port := APort;
end;

destructor TUPClient.Destroy;
begin
  FTcpClient.Disconnect;
  FTcpClient.Free;
  FDataStream.Free;
  inherited Destroy;
end;

procedure TUPClient.DoError(const AErrCode: Integer; const AParam: string);
begin
end;

function TUPClient.GetActive: Boolean;
begin
  Result := FTcpClient.Active;
end;

function TUPClient.GetHost: string;
begin
  Result := FTcpClient.Host;
end;

function TUPClient.GetPort: Integer;
begin
  Result := FTcpClient.Port;
end;

function TUPClient.GetTimeOut: Integer;
begin
  Result := FTcpClient.ReadTimeOut;
end;

procedure TUPClient.PostMsgPack(const AMsgPack: TUPMsgPack);
begin
  CheckConnect;
  FDataStream.Clear;
  AMsgPack.EncodeToStream(FDataStream);
  TZipTools.ZipStream(FDataStream, FDataStream);
  SendDataStream;
end;

procedure TUPClient.ReceiveMsgPack(const AMsgPack: TUPMsgPack);
begin
  RecvDataStream;
  TZipTools.UnZipStream(FDataStream, FDataStream);
  FDataStream.Position := 0;
  AMsgPack.DecodeFromStream(FDataStream);
end;

procedure TUPClient.ReConnectServer;
begin
  CheckConnect;
end;

function TUPClient.RecvDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  FTcpClient.Recv(buf, len);
  Result := len;
end;

function TUPClient.RecvDataStream: Boolean;
var
  vBytes: TBytes;
  vReadLen, vTempLen: Integer;
  vPACK_FLAG: Word;
  vDataLen: Integer;
  vVerifyValue, vVerifyDataValue: Cardinal;
  vPByte: PByte;
begin
  RecvDataBuffer(@vPACK_FLAG, 2);

  if vPACK_FLAG <> PACK_FLAG then  // 错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //datalen
  RecvDataBuffer(@vDataLen, SizeOf(vDataLen));

  //veri value
  RecvDataBuffer(@vVerifyValue, SizeOf(vVerifyValue));
  //vDataLen := TByteTools.swap32(vReadLen);

  if vDataLen > MAX_OBJECT_SIZE then  // 文件头过大，错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorData);
  end;

  SetLength(vBytes,vDataLen);
  vPByte := PByte(@vBytes[0]);
  vReadLen := 0;
  while vReadLen < vDataLen do
  begin
    vTempLen := RecvDataBuffer(vPByte, vDataLen - vReadLen);
    if vTempLen = -1 then
    begin
      RaiseLastOSError;
    end;

    Inc(vPByte, vTempLen);
    vReadLen := vReadLen + vTempLen;

    if Assigned(FOnReceiveProcess) then
      FOnReceiveProcess(vReadLen, vDataLen);
  end;

{$IFDEF POSIX}
  vVerifyDataValue := verifyData(lvBytes[0], lvDataLen);
{$ELSE}
  vVerifyDataValue := verifyData(vBytes[0], vDataLen);
{$ENDIF}

  if vVerifyDataValue <> vVerifyValue then
    raise Exception.Create(strRecvException_VerifyErr);

  FDataStream.Clear;
  FDataStream.Write(vBytes[0], vDataLen);
  Result := True;
end;

function TUPClient.SendDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  Result := FTcpClient.SendBuffer(buf, len);
end;

function TUPClient.SendDataStream: Integer;
var
  lvPACK_FLAG: WORD;
  lvDataLen, lvWriteIntValue: Integer;
  lvBuf: TBytes;
  lvStream: TMemoryStream;
  lvVerifyValue: Cardinal;
begin
  lvPACK_FLAG := PACK_FLAG;

  lvStream := TMemoryStream.Create;
  try
    FDataStream.Position := 0;

    if FDataStream.Size > MAX_OBJECT_SIZE then
      raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);

    lvStream.Write(lvPACK_FLAG, 2);  // 包头

    lvDataLen := FDataStream.Size;    // stream data
    SetLength(lvBuf, lvDataLen);
    FDataStream.Read(lvBuf[0], lvDataLen);

    // stream len
    lvStream.Write(lvDataLen, SizeOf(lvDataLen));

    //veri value
    lvVerifyValue := verifyData(lvBuf[0], lvDataLen);
    lvStream.Write(lvVerifyValue, SizeOf(lvVerifyValue));

    // send pack
    lvStream.write(lvBuf[0], lvDataLen);

    Result := SendStream(lvStream);
  finally
    lvStream.Free;
  end;
end;

function TUPClient.SendStream(pvStream: TStream): Integer;
var
  lvBufBytes: array[0..MAX_BLOCK_SIZE - 1] of byte;
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
        r := SendDataBuffer(P, j);
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

procedure TUPClient.SetHost(const AHost: string);
begin
  FTcpClient.Host := AHost;
end;

procedure TUPClient.SetPort(const APort: Integer);
begin
  FTcpClient.Port := APort;
end;

procedure TUPClient.SetTimeOut(const Value: Integer);
begin
  FTcpClient.ReadTimeOut := Value;
end;

end.
