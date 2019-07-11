unit UPMsgCoder;

interface

uses
  System.Classes, diocp_coder_baseObject, utils_buffer;

type
  TUPMsgDecoder = class(TIOCPDecoder)
  public
    /// <summary> 解码收到的数据 </summary>
    function Decode(const AInBuffer: TBufferLink; AContext: TObject): TObject; override;
  end;

  TUPMsgEncoder = class(TIOCPEncoder)
  public
    /// <summary> 编码要发送的数据 </summary>
    procedure Encode(ADataObject: TObject; const AOutBuffer: TBufferLink); override;
    procedure EncodeToStream(const AInStream: TMemoryStream; const AOutStream: TMemoryStream);
  end;

implementation

uses
  System.SysUtils, UPMsgPack;

{ TUPMsgDecoder }

function TUPMsgDecoder.Decode(const AInBuffer: TBufferLink;
  AContext: TObject): TObject;
var
  vDataLen: Integer;
  vPackFlag: Word;
  vVerifyValue, vActVerifyValue: Cardinal;
begin
  Result := nil;

  //如果缓存中的数据长度不够包头长度，
  vDataLen := AInBuffer.validCount;   //pack_flag + head_len + buf_len
  if (vDataLen < SizeOf(Word) + SizeOf(Integer) + SizeOf(Integer)) then Exit;

  //记录读取位置
  AInBuffer.MarkReaderIndex;
  AInBuffer.ReadBuffer(@vPackFlag, 2);

  if vPackFlag <> PACK_FLAG then
  begin
    //错误的包数据
    Result := TObject(-1);
    Exit;
  end;

  AInBuffer.ReadBuffer(@vDataLen, SizeOf(vDataLen));  // 数据长度
  AInBuffer.ReadBuffer(@vVerifyValue, SizeOf(vVerifyValue));  // 校验值

  if vDataLen > 0 then
  begin
    if vDataLen > MAX_OBJECT_SIZE then  //文件头不能过大
    begin
      Result := TObject(-1);
      Exit;
    end;

    if AInBuffer.ValidCount < vDataLen then  // 返回buf的读取位置
    begin
      AInBuffer.restoreReaderIndex;
      Exit;
    end;

    Result := TMemoryStream.Create;
    TMemoryStream(Result).SetSize(vDataLen);
    AInBuffer.ReadBuffer(TMemoryStream(Result).Memory, vDataLen);
    TMemoryStream(Result).Position := 0;

    // 校验
    vActVerifyValue := VerifyData(TMemoryStream(Result).Memory^, vDataLen);
    if vVerifyValue <> vActVerifyValue then
      raise Exception.Create(strRecvException_VerifyErr);
  end
  else
    Result := nil;
end;

{ TUPMsgEncoder }

procedure TUPMsgEncoder.Encode(ADataObject: TObject; const AOutBuffer: TBufferLink);
var
  vPackFlag: Word;
  vDataLen: Integer;
  vBuffer: TBytes;
  vVerifyValue: Cardinal;
begin
  vPackFlag := PACK_FLAG;

  TStream(ADataObject).Position := 0;

  if TStream(ADataObject).Size > MAX_OBJECT_SIZE then
    raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);

  AOutBuffer.AddBuffer(@vPackFlag, 2);  // 包头

  vDataLen := TStream(ADataObject).Size;  // 数据大小
  //vDataLenSw := TByteTools.swap32(vDataLen);
  AOutBuffer.AddBuffer(@vDataLen, SizeOf(vDataLen));  // 数据长度

  // stream data
  SetLength(vBuffer, vDataLen);
  TStream(ADataObject).Read(vBuffer[0], vDataLen);

  // 校验值
  vVerifyValue := VerifyData(vBuffer[0], vDataLen);
  AOutBuffer.AddBuffer(@vVerifyValue, SizeOf(vVerifyValue));  // 写入校验值

  AOutBuffer.AddBuffer(@vBuffer[0], vDataLen);  // 数据
end;

procedure TUPMsgEncoder.EncodeToStream(const AInStream: TMemoryStream;
  const AOutStream: TMemoryStream);
var
  vPackFlag: Word;
  vDataLen: Integer;
  vVerifyValue: Cardinal;
begin
  if AInStream.Size > MAX_OBJECT_SIZE then  // 超过最大发送量
    raise Exception.CreateFmt(strSendException_TooBig, [MAX_OBJECT_SIZE]);

  vPackFlag := PACK_FLAG;
  AOutStream.Write(vPackFlag, SizeOf(vPackFlag));  // 写包头

  vDataLen := AInStream.Size;  // 数据大小
  AOutStream.Write(vDataLen, SizeOf(vDataLen));  // 写入数据大小的值

  // 校验值
  vVerifyValue := VerifyData(AInStream.Memory^, vDataLen);
  AOutStream.Write(vVerifyValue, SizeOf(vVerifyValue));  // 写入校验值

  AInStream.Position := 0;
  AOutStream.Write(AInStream.Memory^, AInStream.Size);  // 写入实际数据
end;

end.
