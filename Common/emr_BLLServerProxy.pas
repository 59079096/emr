{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit emr_BLLServerProxy;

interface

uses
  Classes, diocp_tcp_blockClient, emr_MsgPack;

type
  TBLLServerProxy = class(TObject)
  private
    FReconnect: Boolean;
    FTcpClient: TDiocpBlockTcpClient;
    FDataStream: TMemoryStream;
    procedure CheckConnect;
    function SendStream(pvStream: TStream): Integer;

    /// <summary>
    /// 获取指定的参数
    /// </summary>
    function Param(const AParamName: string): TMsgPack;
  protected
    FMsgPack: TMsgPack;
    function GetHost: string;
    procedure SetHost(const AHost: string);
    function GetPort: Integer;
    procedure SetPort(const APort: Integer);
    function GetActive: Boolean;
    function GetCmd: Integer;
    procedure SetCmd(const Value: Integer);
    function GetBackDataSet: Boolean;
    procedure SetBackDataSet(const Value: Boolean);

    function GetBatch: Boolean;
    procedure SetBatch(const Value: Boolean);

    function GetOnErrorEvent: TOnErrorEvent;
    procedure SetOnErrorEvent(Value: TOnErrorEvent);
    function GetTimeOut: Integer;
    procedure SetTimeOut(const Value: Integer);
    //
    function SendDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function RecvDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
    function PeekDataBuffer(buf:Pointer; len:Cardinal): Cardinal; stdcall;
  public
    constructor Create; virtual;
    constructor CreateEx(const AHost: string; const APort: Integer;
      AReconnect: Boolean = True);
    destructor Destroy; override;

    function Connected: Boolean;
    procedure ReConnectServer;
    function SendDataStream: Integer;
    function RecvDataStream: Boolean;

    /// <summary>
    /// 向服务端传递客户端调用数据
    /// </summary>
    /// <param name="AMsgPack"></param>
    /// <returns>服务端处理此次调用是否成功(仅表示服务端响应客户端调用成功，不代表方法执行的结果)</returns>
    function DispatchPack(const AMsgPack: TMsgPack): Boolean; overload;
    function DispatchPack: Boolean; overload;

    /// <summary>
    /// 存放客户端调用服务端方法时Sql语句的字段参数
    /// </summary>
    function ExecParam: TMsgPack;

    /// <summary>
    /// 存放客户端调用服务端方法时Sql语句的替换参数
    /// </summary>
    /// <returns></returns>
    function ReplaceParam: TMsgPack;

    /// <summary>
    /// 向服务端调用的方法添加一个要返回的字段
    /// </summary>
    procedure AddBackField(const AFieldName: string);

    /// <summary>
    /// 获取服务端方法返回的指定字段数据
    /// </summary>
    function BackField(const AFieldName: string): TMsgPack;

    /// <summary>
    /// 客户端调用的服务端具体方法执行是否成功
    /// </summary>
    function MethodRunOk: Boolean;

    // 记录服务端响应方法错误时的信息(BACKMSG)或响应成功时方法执行结果错误时的信息(BLL_METHODMSG)
    function MethodError: string;

    // 记录服务端响应成功时方法执行结果数据集的个数
    function RecordCount: Integer;

    /// <summary> 将返回的业务数据集写入内存流 </summary>
    /// <param name="AStream">存放数据集</param>
    procedure GetBLLDataSet(const AStream: TMemoryStream);

    //property MsgPack: TMsgPack read FMsgPack;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    //property TcpClient: TDiocpBlockTcpClient read FTcpClient;
    property DataStream: TMemoryStream read FDataStream;

    /// <summary>
    /// 每次调用服务时重连服务器(调用完服务后断开节省资源)
    /// </summary>
    property Reconnect: Boolean read FReconnect write FReconnect;
    property Active: Boolean read GetActive;
    property Cmd: Integer read GetCmd write SetCmd;
    /// <summary> 服务端是否返回数据集 </summary>
    property BackDataSet: Boolean read GetBackDataSet write SetBackDataSet;

    /// <summary> 是否批量处理数据 </summary>
    property Batch: Boolean read GetBatch write SetBatch;

    property TimeOut: Integer read GetTimeOut write SetTimeOut;
    property OnError: TOnErrorEvent read GetOnErrorEvent write SetOnErrorEvent;
  end;

implementation

{ TBLLServerProxy }

uses
  SysUtils, utils_zipTools, utils_byteTools, emr_BLLConst,
  DiocpError;

procedure TBLLServerProxy.AddBackField(const AFieldName: string);
begin
  Param(BLL_BACKFIELD).Add(AFieldName);
end;

function TBLLServerProxy.BackField(const AFieldName: string): TMsgPack;
begin
  Result := Param(BLL_BACKFIELD).O[AFieldName];
end;

procedure TBLLServerProxy.CheckConnect;
begin
  if (not FTcpClient.Active) then
    FTcpClient.Connect;
end;

function TBLLServerProxy.Connected: Boolean;
begin
  Result := FTcpClient.Active;
end;

constructor TBLLServerProxy.Create;
begin
  inherited Create;
  FReconnect := True;
  FTcpClient := TDiocpBlockTcpClient.Create(nil);
  FTcpClient.ReadTimeOut := 1000 * 60;  // 设置超时等待1分钟
  FDataStream := TMemoryStream.Create;
  FMsgPack := TMsgPack.Create;
end;

constructor TBLLServerProxy.CreateEx(const AHost: string;
  const APort: Integer; AReconnect: Boolean = True);
begin
  Create;
  FTcpClient.Host := AHost;
  FTcpClient.Port := APort;
  FReconnect := AReconnect;
end;

destructor TBLLServerProxy.Destroy;
begin
  FTcpClient.Disconnect;
  FTcpClient.Free;
  FDataStream.Free;
  FMsgPack.Free;
  inherited Destroy;
end;

function TBLLServerProxy.ExecParam: TMsgPack;
begin
  Result := Param(BLL_EXECPARAM);
end;

function TBLLServerProxy.DispatchPack: Boolean;
begin
  Result := DispatchPack(FMsgPack);
end;

function TBLLServerProxy.DispatchPack(const AMsgPack: TMsgPack): Boolean;
var
  vCmd: Integer;
begin
  vCmd := Cmd;  // 提前记录，防止出错后丢失信息
  CheckConnect;
  // 初始化连接时用到的对象
  FDataStream.Clear;
  // 设置调用时各对象值
  AMsgPack.ForcePathObject(BLL_PROXYTYPE).AsInteger := Ord(cptDBL);  // 代理类型
  AMsgPack.ForcePathObject(BLL_VER).AsInteger := BLLVERSION;  // 业务版本
  //AMsgPack.ForcePathObject(BLL_DEVICE).AsInteger := Ord(TDeviceType.cdtMobile);  // 设备类型
  AMsgPack.EncodeToStream(FDataStream);  // 加密传输的调用数据
  TZipTools.ZipStream(FDataStream, FDataStream);  // 压缩传输的调用数据
  SendDataStream;  // 数据发送到服务端
  RecvDataStream;  // 获取服务端返回数据
  TZipTools.UnZipStream(FDataStream, FDataStream);  // 解压缩返回的数据
  FDataStream.Position := 0;
  AMsgPack.DecodeFromStream(FDataStream);  // 解密返回的数据
  Result := AMsgPack.Result;  // 服务端处理此次调用是否成功(仅表示服务端响应客户端调用成功，不代表方法执行的结果)
  if not Result then  // 服务端处理此次调用出错
  begin
    if AMsgPack.ForcePathObject(BACKMSG).AsString <> '' then  // 出错信息
    begin
      {raise Exception.Create('服务端异常：方法[' + GetBLLMethodName(vCmd) + ']'
        + AMsgPack.ForcePathObject(BACKMSG).AsString);}
      Self.FMsgPack.ForcePathObject(BLL_ERROR).AsString := '【服务端错误】'
        //+ sLineBreak + '方法：' + GetBLLMethodName(vCmd)
        + sLineBreak + AMsgPack.ForcePathObject(BACKMSG).AsString;
    end;
  end
  else  // 方法执行返回错误信息
  begin
    if AMsgPack.ForcePathObject(BLL_METHODMSG).AsString <> '' then
    begin
      Self.FMsgPack.ForcePathObject(BLL_ERROR).AsString := '【执行方法错误】'
        //+ sLineBreak + GetBLLMethodName(vCmd) + '失败，'
        + sLineBreak + AMsgPack.ForcePathObject(BLL_METHODMSG).AsString;
    end;
  end;

  if FReconnect then  // 调用完服务后断开，节省资源，以后可改为代理池
    FTcpClient.Disconnect;;
end;

function TBLLServerProxy.GetActive: Boolean;
begin
  Result := FTcpClient.Active;
end;

function TBLLServerProxy.GetBackDataSet: Boolean;
begin
  Result := Param(BLL_BACKDATASET).AsBoolean;
end;

function TBLLServerProxy.GetBatch: Boolean;
begin
  Result := Param(BLL_BATCH).AsBoolean;
end;

procedure TBLLServerProxy.GetBLLDataSet(const AStream: TMemoryStream);
begin
  if FMsgPack.O[BLL_DATASET] <> nil then
    FMsgPack.O[BLL_DATASET].SaveBinaryToStream(AStream)
  else
    AStream.Size := 0;
end;

function TBLLServerProxy.GetCmd: Integer;
begin
  Result := FMsgPack.ForcePathObject(BLL_CMD).AsInteger
end;

function TBLLServerProxy.GetHost: string;
begin
  Result := FTcpClient.Host;
end;

function TBLLServerProxy.GetOnErrorEvent: TOnErrorEvent;
begin
  Result := FTcpClient.OnError;
end;

function TBLLServerProxy.GetPort: Integer;
begin
  Result := FTcpClient.Port;
end;

function TBLLServerProxy.GetTimeOut: Integer;
begin
  Result := FTcpClient.ReadTimeOut;
end;

function TBLLServerProxy.MethodError: string;
begin
  Result := Param(BLL_ERROR).AsString;
end;

function TBLLServerProxy.MethodRunOk: Boolean;
begin
  Result := Param(BLL_METHODRESULT).AsBoolean;
end;

function TBLLServerProxy.Param(const AParamName: string): TMsgPack;
begin
  Result := FMsgPack.ForcePathObject(AParamName);
end;

procedure TBLLServerProxy.ReConnectServer;
begin
  CheckConnect;
end;

function TBLLServerProxy.PeekDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      FTcpClient.Recv(buf, len);
      Result := len;
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
  begin
    FTcpClient.Recv(buf, len);
    Result := len;
  end;
end;

function TBLLServerProxy.RecvDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      FTcpClient.Recv(buf, len);
      Result := len;
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
  begin
    FTcpClient.Recv(buf, len);
    Result := len;
  end;
end;

function TBLLServerProxy.RecvDataStream: Boolean;
var
  lvBytes:TBytes;
  lvReadL, lvTempL:Integer;
  lvPACK_FLAG:Word;
  lvDataLen: Integer;
  lvVerifyValue, lvVerifyDataValue:Cardinal;
  lvPByte:PByte;
begin
  RecvDataBuffer(@lvPACK_FLAG, 2);

  if lvPACK_FLAG <> PACK_FLAG then  // 错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorFlag);
  end;

  //veri value
  RecvDataBuffer(@lvVerifyValue, SizeOf(lvVerifyValue));

  //headlen
  RecvDataBuffer(@lvReadL, SizeOf(lvReadL));
  lvDataLen := TByteTools.swap32(lvReadL);

  if lvDataLen > MAX_OBJECT_SIZE then  // 文件头过大,错误的包数据
  begin
    FTcpClient.Disconnect;
    raise Exception.Create(strRecvException_ErrorData);
  end;

  SetLength(lvBytes,lvDataLen);
  lvPByte := PByte(@lvBytes[0]);
  lvReadL := 0;
  while lvReadL < lvDataLen do
  begin
    lvTempL := RecvDataBuffer(lvPByte, lvDataLen - lvReadL);
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

  FDataStream.Clear;
  FDataStream.Write(lvBytes[0], lvDataLen);
  Result := true;
end;

function TBLLServerProxy.ReplaceParam: TMsgPack;
begin
  Result := Param(BLL_REPLACEPARAM);
end;

function TBLLServerProxy.RecordCount: Integer;
begin
  Result := Param(BLL_RECORDCOUNT).AsInteger;
end;

function TBLLServerProxy.SendDataBuffer(buf: Pointer; len: Cardinal): Cardinal;
begin
  if FReconnect then
  begin
    if not FTcpClient.Active then
      FTcpClient.Connect;
    try
      Result := FTcpClient.SendBuffer(buf, len);
    except
      FTcpClient.Disconnect;
      raise;
    end;
  end
  else
  begin
    Result := FTcpClient.SendBuffer(buf, len);
  end;
end;

function TBLLServerProxy.SendDataStream: Integer;
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

    lvDataLen := FDataStream.Size;

    // stream data
    SetLength(lvBuf, lvDataLen);
    FDataStream.Read(lvBuf[0], lvDataLen);
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

function TBLLServerProxy.SendStream(pvStream: TStream): Integer;
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

procedure TBLLServerProxy.SetBackDataSet(const Value: Boolean);
begin
  Param(BLL_BACKDATASET).AsBoolean := Value;
end;

procedure TBLLServerProxy.SetBatch(const Value: Boolean);
begin
  Param(BLL_BATCH).AsBoolean := Value;
end;

procedure TBLLServerProxy.SetCmd(const Value: Integer);
begin
  FMsgPack.ForcePathObject(BLL_CMD).AsInteger := Value;
end;

procedure TBLLServerProxy.SetHost(const AHost: string);
begin
  FTcpClient.Host := AHost;
end;

procedure TBLLServerProxy.SetOnErrorEvent(Value: TOnErrorEvent);
begin
  FTcpClient.OnError := Value;
end;

procedure TBLLServerProxy.SetPort(const APort: Integer);
begin
  FTcpClient.Port := APort;
end;

procedure TBLLServerProxy.SetTimeOut(const Value: Integer);
begin
  FTcpClient.ReadTimeOut := Value;
end;

end.
