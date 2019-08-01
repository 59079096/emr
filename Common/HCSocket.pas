unit HCSocket;

interface

uses
  System.Classes, System.Generics.Collections, Net.CrossSocket, Net.CrossSocket.Base,
  utils_buffer;

type
  THCRDataEvent = procedure(const AConnection: ICrossConnection; const ADataStream: TBytesStream) of object;
  THCRErrorEvent = procedure(const AError: string) of object;

  IHCRSocket = interface(ICrossSocket)
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetGraceful: Boolean;
    procedure SetGraceful(const Value: Boolean);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetOnReceivedData: THCRDataEvent;
    procedure SetOnReceivedData(const Value: THCRDataEvent);
    function GetOnError: THCRErrorEvent;
    procedure SetOnError(const Value: THCRErrorEvent);
    procedure PostStream(var AStream: TBytesStream; const AConnectionIndex: Integer = 0); overload;
    procedure PostStream(var AStream: TBytesStream; const AConnection: ICrossConnection); overload;

    property Active: Boolean read GetActive write SetActive;
    property Graceful: Boolean read GetGraceful write SetGraceful;
    property Port: Integer read GetPort write SetPort;
    property Host: string read GetHost write SetHost;
    property OnReceiveData: THCRDataEvent read GetOnReceivedData write SetOnReceivedData;
    property OnError: THCRErrorEvent read GetOnError write SetOnError;
  end;

  THCRSocket = class(TCrossSocket, IHCRSocket)
  strict private
    FHost: string;
    FPort: Integer;
    FGraceful: Boolean;
    FOnReceivedData, FOnSentData: THCRDataEvent;
    FOnError: THCRErrorEvent;
    function DecodeRecvBuffer(const ABuffer: TBufferLink): TObject;
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure LogicConnected(AConnection: ICrossConnection); override;
    procedure LogicDisconnected(AConnection: ICrossConnection); override;
    procedure LogicReceived(AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer); override;

    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetGraceful: Boolean;
    procedure SetGraceful(const Value: Boolean);
    function GetOnReceivedData: THCRDataEvent;
    procedure SetOnReceivedData(const Value: THCRDataEvent);
    function GetOnError: THCRErrorEvent;
    procedure SetOnError(const Value: THCRErrorEvent);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure PostStream(var AStream: TBytesStream; const AConnectionIndex: Integer = 0); overload;
    procedure PostStream(var AStream: TBytesStream; const AConnection: ICrossConnection); overload;
    property Active: Boolean read GetActive write SetActive;
    property Graceful: Boolean read GetGraceful write SetGraceful;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property OnReceiveData: THCRDataEvent read GetOnReceivedData write SetOnReceivedData;
    property OnError: THCRErrorEvent read GetOnError write SetOnError;
  end;

  THCRServer = class(THCRSocket)
  private
    FActive: Boolean;
  protected
    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create; override;
  end;

  THCRClient = class(THCRSocket)
  private
    FActive: Boolean;
  protected
    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
  public
    constructor Create; override;
  end;

  function LocalIP: String;
  function IsLegalIP(const AIP: string): Boolean;
  function GetIPSection(const AIP: string): string;

implementation

uses
  System.SysUtils, Winapi.Winsock2;

function LocalIP: String;
type
  TaPInAddr = Array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: Array[0..63] of AnsiChar;
  i: Integer;
  GInitData: TWSAData;
begin
  WSAStartup($101, GInitData);
  try
    Result := '';
    GetHostName(Buffer, SizeOf(Buffer));
    phe := GetHostByName(buffer);
    if phe = nil then Exit;

    pPtr := PaPInAddr(phe^.h_addr_list);
    i := 0;
    while pPtr^[i] <> nil do
    begin
      Result := inet_ntoa(pptr^[i]^);
      Inc(i);
    end;
  finally
    WSACleanup;
  end;
end;

{ 检查IP地址是否合法 }
function IsLegalIP(const AIP: string): Boolean;

  { 获得子字符串在父字符串中出现的次数 }
  function GetSubStrCount(ASubStr, AParentStr: string): integer;
  begin
    Result := 0;
    while Pos(UpperCase(ASubStr), UpperCase(AParentStr)) <> 0 do
    begin
      AParentStr := Copy(AParentStr, Pos(ASubStr, AParentStr) + 1, Length(AParentStr)); //假设s2最大长度为9999个字符
      Result := Result + 1;
    end;
  end;

begin
  {if inet_addr(PAnsiChar(AIP))=INADDR_NONE then
    Result := False
  else}
  if (GetSubStrCount('.', AIP) = 3) and (Longword(inet_addr(PAnsiChar(AnsiString(AIP)))) <> INADDR_NONE) then
    Result := True
  else
    Result := False;
end;

function GetIPSection(const AIP: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := Length(AIP) downto 1 do
  begin
    if AIP[i] = '.' then
    begin
      Result := Copy(AIP, 1, i) + '1';
      Break;
    end;
  end;
end;

{ THCRSocket }

constructor THCRSocket.Create;
begin
  inherited Create(0);
  FHost := '0.0.0.0';
  FPort := 0;
  FGraceful := True;
end;

function THCRSocket.DecodeRecvBuffer(const ABuffer: TBufferLink): TObject;
var
  vDataSize, vLen: Cardinal;
  vDataProtoVer: Byte;
begin
  Result := nil;

  if ABuffer.validCount < SizeOf(Cardinal) then Exit;  // 缓存中的数据长度不够表示数据长度

  ABuffer.MarkReaderIndex;  // 记录读取位置

  ABuffer.ReadBuffer(@vDataSize, SizeOf(vDataSize));  // 数据长度
  if vDataSize > 0 then
  begin
    if ABuffer.ValidCount < vDataSize - SizeOf(vDataSize) then  // 不够完整的数据
    begin
      ABuffer.restoreReaderIndex;
      Exit;
    end;

    ABuffer.ReadBuffer(@vDataProtoVer, SizeOf(vDataProtoVer));  // 读数据协议
    vLen := vDataSize - SizeOf(vDataSize) - SizeOf(vDataProtoVer);

    Result := TBytesStream.Create;
    TBytesStream(Result).SetSize(vLen);
    ABuffer.ReadBuffer(TMemoryStream(Result).Memory, vLen);
    TBytesStream(Result).Position := 0;

    {if FVerifyData then // 校验
    begin
      vActVerifyValue := VerifyData(TMemoryStream(Result).Memory^, vDataLen);
      if vVerifyValue <> vActVerifyValue then
      begin
        sfLogger.logMessage(IntToHex(vVerifyValue) + '-' + IntToHex(vActVerifyValue));
        raise Exception.Create(strRecvException_VerifyErr);
      end;
    end;}

    //清理缓存<如果没有可用的内存块>清理
    if ABuffer.ValidCount = 0 then
      ABuffer.ClearBuffer
    else
      ABuffer.ClearHaveReadBuffer;
  end
  else  // 不够完整
  begin
    Result := nil;
    ABuffer.restoreReaderIndex;
  end;
end;

destructor THCRSocket.Destroy;
begin
  Active := False;
  Self.StopLoop;

  inherited Destroy;
end;

function THCRSocket.GetActive: Boolean;
begin
  Result := False;
end;

function THCRSocket.GetGraceful: Boolean;
begin
  Result := FGraceful;
end;

function THCRSocket.GetHost: string;
begin
  Result := FHost;
end;

function THCRSocket.GetOnError: THCRErrorEvent;
begin
  Result := FOnError;
end;

function THCRSocket.GetOnReceivedData: THCRDataEvent;
begin
  Result := FOnReceivedData;
end;

function THCRSocket.GetPort: Integer;
begin
  Result := FPort;
end;

procedure THCRSocket.LogicConnected(AConnection: ICrossConnection);
begin
  inherited LogicConnected(AConnection);
  AConnection.UserObject := TBufferLink.Create;
end;

procedure THCRSocket.LogicDisconnected(AConnection: ICrossConnection);
begin
  inherited LogicDisconnected(AConnection);

  Active := False;

  if Assigned(AConnection.UserObject) then
    TBufferLink(AConnection.UserObject).Free;
end;

procedure THCRSocket.LogicReceived(AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
var
  vRecvBuffer: TBufferLink;
  vDecodeObj: TObject;
begin
  inherited LogicReceived(AConnection, ABuf, ALen);
  vRecvBuffer := AConnection.UserObject as TBufferLink;
  vRecvBuffer.AddBuffer(ABuf, ALen);

  vDecodeObj := DecodeRecvBuffer(vRecvBuffer);
  if Integer(vDecodeObj) = -1 then  // 错误的包格式, 关闭连接
  begin
    Active := False;
    Exit;
  end
  else
  if vDecodeObj <> nil then
  begin
    try
      if Assigned(FOnReceivedData) then
        FOnReceivedData(AConnection, TBytesStream(vDecodeObj));
    finally
      TBytesStream(vDecodeObj).Free;
    end;
  end;
end;

procedure THCRSocket.PostStream(var AStream: TBytesStream; const AConnectionIndex: Integer = 0);
var
  vConns: TArray<ICrossConnection>;
begin
  if AStream.Size = 0 then Exit;

  vConns := LockConnections.Values.ToArray;
  try
    if Assigned(vConns) then
    begin
      if (AConnectionIndex >= 0) and (AConnectionIndex <= Length(vConns)) then
        PostStream(AStream, vConns[AConnectionIndex]);
    end;
  finally
    UnlockConnections;
  end;
end;

procedure THCRSocket.PostStream(var AStream: TBytesStream;
  const AConnection: ICrossConnection);
var
  vStream: TBytesStream;
begin
  if AStream.Size = 0 then Exit;

  vStream := AStream;

  AConnection.SendStream(AStream, procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      vStream.Free;
    end);
end;

procedure THCRSocket.SetActive(const Value: Boolean);
begin
end;

procedure THCRSocket.SetGraceful(const Value: Boolean);
begin
  FGraceful := Value;
end;

procedure THCRSocket.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure THCRSocket.SetOnError(const Value: THCRErrorEvent);
begin
  FOnError := Value;
end;

procedure THCRSocket.SetOnReceivedData(const Value: THCRDataEvent);
begin
  FOnReceivedData := Value;
end;

procedure THCRSocket.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

{ THCRServer }

constructor THCRServer.Create;
begin
  inherited Create;
  FActive := False;
end;

function THCRServer.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure THCRServer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    if FActive then
    begin
      Self.Listen('0.0.0.0', Port, procedure(AListen: ICrossListen; ASuccess: Boolean)
        begin
          FActive := ASuccess;
          if not FActive then
          begin
            if Assigned(OnError) then
              OnError('服务端启动失败！');
          end;
        end);
    end
    else
    begin
      if Graceful then
        DisconnectAll
      else
        CloseAllConnections;
    end;

    inherited SetActive(FActive);
  end;
end;

{ THCRClient }

constructor THCRClient.Create;
begin
  inherited Create;
  FActive := False;
end;

function THCRClient.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure THCRClient.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    if FActive then
    begin
      Self.Connect(Host, Port, procedure(AConnection: ICrossConnection; ASuccess: Boolean)
        begin
          FActive := ASuccess;
          if not FActive then
          begin
            if Assigned(OnError) then
              OnError('连接失败！');
          end;
        end);
    end
    else
    begin
      if Graceful then
        DisconnectAll
      else
        CloseAllConnections;
    end;

    inherited SetActive(FActive);
  end;
end;

{ TRecvData }

//constructor TRecvData.Create;
//begin
//  inherited Create;
//  Size := 0;
//  Offset := 0;
//  Stream := TBytesStream.Create;
//end;
//
//destructor TRecvData.Destroy;
//begin
//  Stream.Free;
//  inherited Destroy;
//end;

end.
