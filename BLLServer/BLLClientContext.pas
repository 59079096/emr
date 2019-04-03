unit BLLClientContext;

interface

uses
  SysUtils, Classes, Windows, Math, diocp_tcp_server, emr_MsgPack, StreamProtocol;

type
  TRemoteServer = class(TObject)
  private
    FHost: string;
    FPort: Integer;
  public
    constructor CreateEx(const AHost: string; const APort: Integer);
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
  end;

  TBLLAgent = class(TObject)
  private
    FStream: TMemoryStream;
    FContext: TIocpClientContext;
  public
    constructor Create(const AStream: TStream; const AContext: TIocpClientContext);
    destructor Destroy; override;
    property Stream: TMemoryStream read FStream;
    property Context: TIocpClientContext read FContext;
  end;

  TOnContextActionEvent = procedure(const AStream: TStream; const AContext: TIocpClientContext) of object;

  TBLLClientContext = class(TIocpClientContext)
  private
    FStreamObject: TDiocpStreamObject;
    FOnContextAction: TOnContextActionEvent;
  protected
    procedure DoCleanUp; override;
    procedure OnDisconnected; override;
    procedure OnConnected; override;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    /// <summary>
    ///   数据处理
    /// </summary>
    /// <param name="pvObject"> (TDiocpStreamObject) </param>
    procedure DoContextAction(const ARequestContent: TMemoryStream);
    procedure WriteObject(pvObject: TMemoryStream);

    property OnContextAction: TOnContextActionEvent read FOnContextAction write FOnContextAction;
  end;

implementation

constructor TBLLClientContext.Create;
begin
  inherited Create;
  FStreamObject := TDiocpStreamObject.Create();
end;

destructor TBLLClientContext.Destroy;
begin
  FreeAndNil(FStreamObject);
  inherited Destroy;
end;

procedure TBLLClientContext.DoCleanUp;
begin
  inherited DoCleanUp;
  FStreamObject.Clear;
end;

procedure TBLLClientContext.DoContextAction(const ARequestContent: TMemoryStream);
begin
  // 此方法被触发时已经由TIOCPCoderClientContext.DoExecuteRequest处理线程同步
  if Assigned(FOnContextAction) then
    FOnContextAction(ARequestContent, Self);
end;

procedure TBLLClientContext.OnConnected;
begin

end;

procedure TBLLClientContext.OnDisconnected;
begin

end;

procedure TBLLClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word);
var
  i, r:Integer;
  lvPtr:PByte;
begin
  try
    i:= 0;
    lvPtr := PByte(buf);
    while i < len do
    begin
      r := FStreamObject.InputBuffer(lvPtr^);
      if r = -2 then
      begin
        Self.RequestDisconnect(Format('超过最大尺寸(%d)', [FStreamObject.ContentLength]));
        FStreamObject.Clear;
        Break;
      end
      else
      if r = 1 then
      begin
        DoContextAction(FStreamObject.Content);  // 触发
        FStreamObject.Clear;
      end
      else
      if r = -1 then
      begin
        Self.RequestDisconnect('异常数据包(-1)');
        FStreamObject.Clear;
        Break;
      end;
      Inc(i);
      Inc(lvPtr);
    end;
  except
    on E:Exception do
    begin
      Self.RequestDisconnect('处理逻辑出现异常:' + e.Message);
    end;
  end;
end;

procedure TBLLClientContext.WriteObject(pvObject: TMemoryStream);
var
  vBlock: array[0..MAX_BLOCK_SIZE - 1] of Byte;
  vLen: Integer;
  vStreamObject: TDiocpStreamObject;
begin
  vStreamObject := TDiocpStreamObject.Create;
  try
    vStreamObject.WrapContent(pvObject);
    vStreamObject.ResetReadPosition;
    while True do
    begin
      vLen := vStreamObject.ReadBlock(@vBlock[0], MAX_BLOCK_SIZE);
      if vLen = 0 then
        Break;

      PostWSASendRequest(@vBlock[0], vLen);
    end;
  finally
    vStreamObject.Free;
  end;
end;

{ TRemoteServer }

constructor TRemoteServer.CreateEx(const AHost: string; const APort: Integer);
begin
  inherited Create;
  FHost := AHost;
  FPort := APort;
end;

{ TBLLDataStream }

constructor TBLLAgent.Create(const AStream: TStream;
  const AContext: TIocpClientContext);
begin
  FContext := AContext;
  FStream := TMemoryStream.Create;
  FStream.CopyFrom(AStream, 0);
end;

destructor TBLLAgent.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

end.
