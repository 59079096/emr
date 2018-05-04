unit emr_MsgPack;

interface

uses
  Classes, SysUtils
  {$IFDEF UNICODE}, Generics.Collections{$ELSE}, Contnrs{$ENDIF}
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ,Variants;

const
  BACKRESULT = '__result.result';  // 任务执行是否成功(不代表任务具体结果)
  BACKMSG = '__result.msg';
  //BACKDATA = '__result.data';
  PACK_FLAG1 = $0D;
  PACK_FLAG2 = $10;
  PACK_FLAG = $D10;  // 包头 PACK_FLAG1 shl 16 + PACK_FLAG2
  //PACK_FLAG  + CRC_VALUE + STREAM_LEN + STREAM_DATA
  MAX_OBJECT_SIZE = 1024 * 1024 * 10;  //最大对象大小 10M , 大于10M 则会认为错误的包。
  BUF_BLOCK_SIZE = 1024 * 8;  // 发送数据时块大小
  MAX_BLOCK_SIZE = 1024;   // 1K
  MAX_UPLOAD_SIZE = 100 * 1024;  // 上传、下载文件时一次传输最大100KB

type
  {$IF RTLVersion<25}
    IntPtr=Integer;
  {$IFEND IntPtr}

  {$if CompilerVersion < 18} //before delphi 2007
    TBytes = array of Byte;
  {$ifend}

  TProxyType = (cptNull, cptDBL, cptMsg);  // 调用的服务代理类型

  TMsgPackType = (mptUnknown, mptNull, mptMap, mptArray, mptString, mptInteger,
    mptBoolean, mptDouble, mptSingle, mptDateTime, mptBinary);

  // reserved
  IMsgPack = interface
    ['{37D3E479-7A46-435A-914D-08FBDA75B50E}'] 
  end;

  // copy from qmsgPack
  TMsgPackValue = packed record
    ValueType:Byte;
    case Integer of
      0:(U8Val:Byte);
      1:(I8Val:Shortint);
      2:(U16Val:Word);
      3:(I16Val:Smallint);
      4:(U32Val:Cardinal);
      5:(I32Val:Integer);
      6:(U64Val:UInt64);
      7:(I64Val:Int64);
      //8:(F32Val:Single);
      9:(F64Val:Double);
      10: (FDTVal: TDateTime);
      11:(BArray:array[0..16] of Byte);
  end;

  TMsgPackSetting = class(TObject)
  private
    FCaseSensitive: Boolean;
  public
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TMsgPack = class(TObject)
  private
    FParent: TMsgPack;
    FLowerName: string;
    FName: string;
    FValue: TBytes;
    FDataType: TMsgPackType;
  {$IFDEF UNICODE}
    FChildren: TObjectList<TMsgPack>;
  {$ELSE}
    FChildren: TObjectList;
  {$ENDIF}
    procedure InnerAddToChildren(pvDataType: TMsgPackType; obj: TMsgPack);
    function InnerAdd(pvDataType: TMsgPackType): TMsgPack; overload;
    function InnerAdd:TMsgPack; overload;
    function GetCount: Integer;
    procedure InnerEncodeToStream(pvStream:TStream);
    procedure InnerParseFromStream(pvStream: TStream);
    procedure SetName(pvName:string);
  private
    function GetAsString: String;
    procedure SetAsString(pvValue:string);

    function GetAsInteger: Int64;
    procedure SetAsInteger(pvValue:Int64);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);

    procedure SetAsDouble(const Value: Double);
    function GetAsDouble: Double;

    procedure SetAsDateTime(const Value: TDateTime);
    function GetAsDateTime: TDateTime;

    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);

    procedure SetAsSingle(const Value: Single);
    function GetAsSingle: Single;

    procedure SetAsBytes(const Value: TBytes);
    function GetAsBytes: TBytes;

    procedure CheckObjectDataType(ANewType: TMsgPackType);

    function FindObj(pvName:string): TMsgPack;
    function IndexOf(pvName:string): Integer;
    function IndexOfCaseSensitive(pvName:string): Integer;
    function IndexOfIgnoreSensitive(pvLowerCaseName: string): Integer;
  private
    /// <summary>
    ///   find object index by a path 
    /// </summary>
    function InnerFindPathObject(pvPath: string; var vParent: TMsgPack; var
        vIndex: Integer): TMsgPack;

    function GetO(pvPath: String): TMsgPack;
    procedure SetO(pvPath: String; const Value: TMsgPack);

    function GetS(pvPath: String): string;
    procedure SetS(pvPath: String; const Value: string);

    function GetI(pvPath: String): Int64;
    procedure SetI(pvPath: String; const Value: Int64);

    function GetB(pvPath: String): Boolean;
    procedure SetB(pvPath: String; const Value: Boolean);

    function GetD(pvPath: String): Double;
    procedure SetD(pvPath: String; const Value: Double);

    function GetDT(pvPath: String): TDateTime;
    procedure SetDT(pvPath: String; const Value: TDateTime);

    function GetItems(AIndex: Integer): TMsgPack;

    function GetResult: Boolean;
    procedure SetResult(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Count: Integer read GetCount;

    procedure LoadBinaryFromStream(pvStream: TStream; pvLen: cardinal = 0);
    procedure SaveBinaryToStream(pvStream:TStream);

    procedure LoadBinaryFromFile(pvFileName:String);
    procedure SaveBinaryToFile(pvFileName:String);

    procedure EncodeToStream(pvStream:TStream);
    procedure EncodeToFile(pvFileName:string);

    procedure DecodeFromStream(pvStream:TStream);
    procedure DecodeFromFile(pvFileName:string);

    function EncodeToBytes: TBytes;
    procedure DecodeFromBytes(pvBytes:TBytes); 

    function Add(pvNameKey, pvValue: string): TMsgPack; overload;
    function Add(pvNameKey: string; pvValue: Int64): TMsgPack; overload;
    function Add(pvNameKey: string; pvValue: TBytes): TMsgPack; overload;
    function Add(pvNameKey: String): TMsgPack; overload;
    function Add: TMsgPack; overload;

    function AddArrayChild: TMsgPack;

    function ForcePathObject(pvPath: string): TMsgPack;

    /// <summary>
    ///  remove and free object
    ///    false : object is not found!
    /// </summary>
    function DeleteObject(pvPath:String):Boolean;
    property DataType: TMsgPackType read FDataType;
    property AsInteger: Int64 read getAsInteger write setAsInteger;
    property AsString: string read getAsString write setAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;

    property AsBytes: TBytes read GetAsBytes write SetAsBytes;

    property O[pvPath: String]: TMsgPack read GetO write SetO;
    property S[pvPath: String]: string read GetS write SetS;
    property I[pvPath: String]: Int64 read GetI write SetI;
    property B[pvPath: String]: Boolean read GetB write SetB;
    property D[pvPath: String]: Double read GetD write SetD;
    property DT[pvPath: String]: TDateTime read GetDT write SetDT;

    property Items[AIndex: Integer]: TMsgPack read GetItems; default;
    property NameLower: string read FLowerName;
    property NameEx: string read FName;

    /// <summary>
    /// 消息包执行是否成功(不代表执行结果)
    /// </summary>
    property Result: Boolean read GetResult write SetResult;
  end;

  function VerifyData(const buf; len: Cardinal): Cardinal;

resourcestring
  strRecvException_ErrorFlag = '错误的包头数据,断开与服务器的连接!';
  strRecvException_ErrorData = '错误的数据,断开与服务器的连接!';
  strRecvException_VerifyErr = '错误的数据包，校验失败!';
  strSendException_TooBig = '数据包太大,请在业务层分拆发送,最大数据包[%d]!';
  strSendException_NotEqual = '发送Buffer错误指定发送%d,实际发送:%d';

implementation

{$IF CompilerVersion >= 14.0}  // 2010以上版本
uses
  Data.SqlTimSt;
{$IFEND}

resourcestring
  SVariantConvertNotSupport = 'type to convert not support!。';
  SCannotAddChild = 'Can''t add child in this node!';

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

function swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

function swap32(const v): Cardinal;
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@result)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 3)^ := PByte(@v)^;
end;

function swap64(const v): Int64;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
end;

function swapDouble(const v): Double;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap64Ex(const v; out outVal);
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@outVal)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@outVal) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap32Ex(const v; out outVal);
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@outVal)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap16Ex(const v; out outVal);
begin
  // FF, EE : EE->1, FF->2
  PByte(@outVal)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(@v)^;
end;

// overload swap, result type is integer, because single maybe NaN
function swap(v:Single): Integer; overload;
begin
  swap32Ex(v, Result);
end;

// overload swap
function swap(v:word): Word; overload;
begin
  swap16Ex(v, Result);
end;

// overload swap
function swap(v:Cardinal):Cardinal; overload;
begin
  swap32Ex(v, Result);
end;

// swap , result type is Int64, because Double maybe NaN
function swap(v:Double): Int64; overload;
begin
  swap64Ex(v, Result);
end;

// copy from qstring
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): string;
const
  B2HConvert: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PChar;
  pb: PByte;
begin
  if SizeOf(Char) = 2 then
  begin
    SetLength(Result, l shl 1);
  end
  else
  begin
    SetLength(Result, l);
  end;
  pd := PChar(Result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;

function getFirst(var strPtr: PChar; splitChars: TSysCharSet): string;
var
  oPtr:PChar;
  l:Cardinal;
begin
  oPtr := strPtr;
  Result := '';
  while True do
  begin
    if (strPtr^ in splitChars) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
        break;
      end;
    end
    else
    if (strPtr^ = #0) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
      {$IFDEF UNICODE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l shl 1);
      {$ELSE}
        SetLength(Result, l);
        Move(oPtr^, PChar(Result)^, l);
      {$ENDIF}
      end;
      break;
    end;
    Inc(strPtr);
  end;
end;


function Utf8DecodeEx(pvValue:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF}; len:Cardinal):string;
{$IFDEF UNICODE}
var             
  lvBytes:TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  lvBytes := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, pvValue);
  SetLength(Result, Length(lvBytes) shr 1);
  Move(lvBytes[0], PChar(Result)^, Length(lvBytes));
{$ELSE}
  result:= UTF8Decode(pvValue);
{$ENDIF}
end;

function Utf8EncodeEx(pvValue:string):{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
{$IFDEF UNICODE}
var
  lvBytes:TBytes;
  len:Cardinal;
{$ENDIF}
begin
{$IFDEF UNICODE}
  len := length(pvValue) shl 1;
  SetLength(lvBytes, len);
  Move(PChar(pvValue)^, lvBytes[0], len);
  Result := TEncoding.Convert(TEncoding.Unicode, TEncoding.UTF8, lvBytes);
{$ELSE}
  result:= UTF8Encode(pvValue);
{$ENDIF}
end;


// copy from qmsgPack
procedure writeString(pvValue: string; pvStream: TStream);
var

  lvRawData:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l:Integer;
  lvValue:TMsgPackValue;
begin
  lvRawData := Utf8EncodeEx(pvValue);
  l:=Length(lvRawData);

  //
  //fixstr stores a byte array whose length is upto 31 bytes:
  //+--------+========+
  //|101XXXXX|  data  |
  //+--------+========+
  //
  //str 8 stores a byte array whose length is upto (2^8)-1 bytes:
  //+--------+--------+========+
  //|  0xd9  |YYYYYYYY|  data  |
  //+--------+--------+========+
  //
  //str 16 stores a byte array whose length is upto (2^16)-1 bytes:
  //+--------+--------+--------+========+
  //|  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
  //+--------+--------+--------+========+
  //
  //str 32 stores a byte array whose length is upto (2^32)-1 bytes:
  //+--------+--------+--------+--------+--------+========+
  //|  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
  //+--------+--------+--------+--------+--------+========+
  //
  //where
  //* XXXXX is a 5-bit unsigned integer which represents N
  //* YYYYYYYY is a 8-bit unsigned integer which represents N
  //* ZZZZZZZZ_ZZZZZZZZ is a 16-bit big-endian unsigned integer which represents N
  //* AAAAAAAA_AAAAAAAA_AAAAAAAA_AAAAAAAA is a 32-bit big-endian unsigned integer which represents N
  //* N is the length of data

  if L<=31 then
  begin
    lvValue.ValueType:=$A0+Byte(L);
    pvStream.WriteBuffer(lvValue.ValueType,1);
  end
  else
  if L<=255 then
  begin
    lvValue.ValueType:=$d9;
    lvValue.U8Val:=Byte(L);
    pvStream.WriteBuffer(lvValue,2);
  end
  else
  if L<=65535 then
  begin
    lvValue.ValueType:=$da;
    lvValue.U16Val:=((L shr 8) and $FF) or ((L shl 8) and $FF00);
    pvStream.Write(lvValue,3);
  end
  else
  begin
    lvValue.ValueType:=$db;
    lvValue.BArray[0]:=(L shr 24) and $FF;
    lvValue.BArray[1]:=(L shr 16) and $FF;
    lvValue.BArray[2]:=(L shr 8) and $FF;
    lvValue.BArray[3]:=L and $FF;
    pvStream.WriteBuffer(lvValue,5);
  end;

  pvStream.Write(PByte(lvRawData)^, l);
end;

procedure WriteBinary(p: PByte; l: Integer; pvStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  if l <= 255 then
  begin
    lvValue.ValueType := $C4;
    lvValue.U8Val := Byte(l);
    pvStream.WriteBuffer(lvValue, 2);
  end
  else
  if l <= 65535 then
  begin
    lvValue.ValueType := $C5;
    lvValue.BArray[0] := (l shr 8) and $FF;
    lvValue.BArray[1] := l and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $C6;
    lvValue.BArray[0] := (l shr 24) and $FF;
    lvValue.BArray[1] := (l shr 16) and $FF;
    lvValue.BArray[2] := (l shr 8) and $FF;
    lvValue.BArray[3] := l and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;
  pvStream.WriteBuffer(p^, l);
end;

// copy from qmsgPack
procedure WriteInt(const iVal: Int64; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  if iVal >= 0 then
    begin
      if iVal <= 127 then
      begin
        lvValue.U8Val:=Byte(iVal);
        AStream.WriteBuffer(lvValue.U8Val,1);
      end
      else
      if iVal <= 255 then//UInt8
      begin
        lvValue.ValueType := $cc;
        lvValue.U8Val := Byte(iVal);
        AStream.WriteBuffer(lvValue, 2);
      end
      else
      if iVal <= 65535 then
      begin
        lvValue.ValueType := $cd;
        lvValue.BArray[0] := (iVal shr 8);
        lvValue.BArray[1] := (iVal and $FF);
        AStream.WriteBuffer(lvValue,3);
      end
      else
      if iVal <= Cardinal($FFFFFFFF) then
      begin
        lvValue.ValueType:=$ce;
        lvValue.BArray[0] := (iVal shr 24) and $FF;
        lvValue.BArray[1] := (iVal shr 16) and $FF;
        lvValue.BArray[2] := (iVal shr 8) and $FF;
        lvValue.BArray[3] := iVal and $FF;
        AStream.WriteBuffer(lvValue, 5);
      end
      else
      begin
        lvValue.ValueType := $cf;
        lvValue.BArray[0] := (iVal shr 56) and $FF;
        lvValue.BArray[1] := (iVal shr 48) and $FF;
        lvValue.BArray[2] := (iVal shr 40) and $FF;
        lvValue.BArray[3] := (iVal shr 32) and $FF;
        lvValue.BArray[4] := (iVal shr 24) and $FF;
        lvValue.BArray[5] := (iVal shr 16) and $FF;
        lvValue.BArray[6] := (iVal shr 8) and $FF;
        lvValue.BArray[7] := iVal and $FF;
        AStream.WriteBuffer(lvValue, 9);
      end;
    end
  else//<0
  begin
    if iVal <= Low(Integer) then  //-2147483648  // 64 bit
    begin
      lvValue.ValueType := $d3;
      lvValue.BArray[0] := (iVal shr 56) and $FF;
      lvValue.BArray[1] := (iVal shr 48) and $FF;
      lvValue.BArray[2] := (iVal shr 40) and $FF;
      lvValue.BArray[3] := (iVal shr 32) and $FF;
      lvValue.BArray[4] := (iVal shr 24) and $FF;
      lvValue.BArray[5] := (iVal shr 16) and $FF;
      lvValue.BArray[6] := (iVal shr 8) and $FF;
      lvValue.BArray[7] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 9);
    end
    else
    if iVal <= Low(SmallInt) then     // -32768    // 32 bit
    begin
      lvValue.ValueType := $d2;
      lvValue.BArray[0] := (iVal shr 24) and $FF;
      lvValue.BArray[1] := (iVal shr 16) and $FF;
      lvValue.BArray[2] := (iVal shr 8) and $FF;
      lvValue.BArray[3] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 5);
    end
    else
    if iVal <= -128 then
    begin
      lvValue.ValueType := $d1;
      lvValue.BArray[0] := (iVal shr 8);
      lvValue.BArray[1] := (iVal and $FF);
      AStream.WriteBuffer(lvValue, 3);
    end
    else
    if iVal < -32 then
    begin
      lvValue.ValueType := $d0;
      lvValue.I8Val := iVal;
      AStream.WriteBuffer(lvValue, 2);
    end
    else
    begin
      lvValue.I8Val := iVal;
      AStream.Write(lvValue.I8Val, 1);
    end;
  end;//End <0
end;

procedure WriteDateTime(pvVal: TDateTime; AStream: TStream);
var
  lvValue: TMsgPackValue;
begin
  lvValue.FDTVal := swapDouble(pvVal);
  lvValue.ValueType := $D4;
  AStream.WriteBuffer(lvValue, 9);
end;

procedure WriteDouble(pvVal: Double; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  lvValue.F64Val := swapDouble(pvVal);
  lvValue.ValueType := $CB;
  AStream.WriteBuffer(lvValue, 9);
end;

procedure WriteSingle(pvVal: Single; AStream: TStream);
var
  lvValue:TMsgPackValue;
begin
  lvValue.I32Val := swap(pvVal);
  lvValue.ValueType := $CA;
  AStream.WriteBuffer(lvValue, 5);
end;

procedure WriteNull(pvStream:TStream);
var
  lvByte:Byte;
begin
  lvByte := $C0;
  pvStream.Write(lvByte, 1);
end;

procedure WriteBoolean(pvValue:Boolean; pvStream:TStream);
var
  lvByte:Byte;
begin
  if pvValue then
    lvByte := $C3
  else
    lvByte := $C2;
  pvStream.Write(lvByte, 1);
end;


/// <summary>
///  copy from qmsgpack
/// </summary>
procedure writeArray(obj:TMsgPack; pvStream:TStream);
var
  c, i:Integer;
  lvValue:TMsgPackValue;
  lvNode:TMsgPack;
begin
  C:=obj.Count;

  if C <= 15 then
  begin
    lvValue.ValueType := $90 + C;
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else
  if C <= 65535 then
  begin
    lvValue.ValueType := $DC;
    lvValue.BArray[0] := (C shr 8) and $FF;
    lvValue.BArray[1] := C and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $DD;
    lvValue.BArray[0] := (C shr 24) and $FF;
    lvValue.BArray[1] := (C shr 16) and $FF;
    lvValue.BArray[2] := (C shr 8) and $FF;
    lvValue.BArray[3] := C and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;

  for I := 0 to C-1 do
  begin
    lvNode:=TMsgPack(obj.FChildren[I]);
    lvNode.InnerEncodeToStream(pvStream);
  end;
end;

procedure writeMap(obj:TMsgPack; pvStream:TStream);
var
  c, i:Integer;
  lvValue:TMsgPackValue;
  lvNode:TMsgPack;
begin
  C := obj.Count;
  if C <= 15 then
  begin
    lvValue.ValueType := $80 + C;
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else
  if C <= 65535 then
  begin
    lvValue.ValueType := $de;
    lvValue.BArray[0] := (C shr 8) and $FF;
    lvValue.BArray[1] := C and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $df;
    lvValue.BArray[0] := (C shr 24) and $FF;
    lvValue.BArray[1] := (C shr 16) and $FF;
    lvValue.BArray[2] := (C shr 8) and $FF;
    lvValue.BArray[3] := C and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;
  for I := 0 to C-1 do
  begin
    lvNode := TMsgPack(obj.FChildren[I]);
    writeString(lvNode.FName, pvStream);
    lvNode.InnerEncodeToStream(pvStream);
  end;
end;

function EncodeDateTime(pvVal: TDateTime): string;
var
  AValue: TDateTime;
begin
  AValue := pvVal;
  if AValue - Trunc(AValue) = 0 then // Date
    Result := FormatDateTime('yyyy-MM-dd', AValue)
  else
  begin
    if Trunc(AValue) = 0 then
      Result := FormatDateTime('hh:nn:ss.zzz', AValue)
    else
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AValue);
  end;
end;


constructor TMsgPack.Create;
begin
  inherited Create;
  {$IFDEF UNICODE}
    FChildren := TObjectList<TMsgPack>.Create(true);
  {$ELSE}
    FChildren := TObjectList.Create(true);
  {$ENDIF}
end;

procedure TMsgPack.DecodeFromBytes(pvBytes: TBytes);
var
  lvStream:TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    lvStream.Write(pvBytes[0], Length(pvBytes));
    lvStream.Position := 0;
    DecodeFromStream(lvStream);
  finally
    lvStream.Free;
  end;
end;

procedure TMsgPack.DecodeFromFile(pvFileName: string);
var
  lvFileStream:TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    lvFileStream := TFileStream.Create(pvFileName, fmOpenRead);
    try
      DecodeFromStream(lvFileStream);
    finally
      lvFileStream.Free;
    end;
  end;
end;

procedure TMsgPack.DecodeFromStream(pvStream: TStream);
begin
  InnerParseFromStream(pvStream);
end;

function TMsgPack.DeleteObject(pvPath: String): Boolean;
var
  lvParent, lvObj:TMsgPack;
  j:Integer;
begin
  lvObj := InnerFindPathObject(pvPath, lvParent, j);
  Result := lvObj <> nil;
  if Result then
  begin
    lvParent.FChildren.Delete(j);
  end;
end;

destructor TMsgPack.Destroy;
begin
  FChildren.Clear;
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
end;

function TMsgPack.Add(pvNameKey, pvValue: string): TMsgPack;
begin
  Result := InnerAdd(mptMap);
  Result.setName(pvNameKey);
  Result.AsString := pvValue;
end;

function TMsgPack.Add(pvNameKey: string; pvValue: Int64): TMsgPack;
begin
  Result := InnerAdd(mptMap);
  Result.setName(pvNameKey);
  Result.AsInteger := pvValue;
end;

function TMsgPack.Add: TMsgPack;
begin
  Result := InnerAdd(mptMap);
end;

function TMsgPack.AddArrayChild: TMsgPack;
begin
  if FDataType <> mptArray then
  begin
    Clear;
    FDataType := mptArray;
  end;
  Result := InnerAdd;
end;

function TMsgPack.Add(pvNameKey: string; pvValue: TBytes): TMsgPack;
begin
  Result := InnerAdd(mptMap);
  Result.setName(pvNameKey);
  Result.FDataType := mptBinary;
  Result.FValue := pvValue;
end;

function TMsgPack.Add(pvNameKey:String): TMsgPack;
begin
  Result := InnerAdd(mptMap);
  Result.setName(pvNameKey);
end;

procedure TMsgPack.CheckObjectDataType(ANewType: TMsgPackType);
begin
  if (FDataType <> ANewType) then
  begin
    FDataType := ANewType;
  end;
end;

procedure TMsgPack.Clear;
begin
  FChildren.Clear;
  FDataType := mptNull;
  SetLength(FValue, 0);
end;

function TMsgPack.EncodeToBytes: TBytes;
var
  lvStream:TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    EncodeToStream(lvStream);
    lvStream.Position := 0;
    SetLength(Result, lvStream.size);
    lvStream.Read(Result[0], lvStream.Size);
  finally
    lvStream.Free;
  end;
end;

procedure TMsgPack.EncodeToFile(pvFileName: string);
var
  lvFileStream:TFileStream;
begin
  if FileExists(pvFileName) then
    lvFileStream := TFileStream.Create(pvFileName, fmOpenWrite)
  else
    lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    lvFileStream.Size := 0;
    EncodeToStream(lvFileStream);
  finally
    lvFileStream.Free;
  end;
end;

procedure TMsgPack.EncodeToStream(pvStream: TStream);
begin
  InnerEncodeToStream(pvStream);
end;

function TMsgPack.FindObj(pvName: string): TMsgPack;
var
  i:Integer;
begin
  i := indexOfCaseSensitive(pvName);
  if i <> -1 then
  begin
    Result := TMsgPack(FChildren[i]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TMsgPack.ForcePathObject(pvPath:string): TMsgPack;
var
  lvName: string;
  s: string;
  sPtr: PChar;
  lvTempObj, lvParent: TMsgPack;
  j: Integer;
begin
  Result := nil;
  s := pvPath;

  lvParent := Self;
  sPtr := PChar(s);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/','\']);
    if lvName = '' then
    begin
      Break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin           // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          Result := TMsgPack(lvParent.FChildren[j]);
        end
        else
        begin
          Result := lvParent.Add(lvName);
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          lvParent := lvParent.Add(lvName);
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then
      Break;
    Inc(sPtr);
  end;
end;

function TMsgPack.GetAsBoolean: Boolean;
begin
  if FDataType = mptBoolean then
    Result := PBoolean(FValue)^
  else
  if FDataType = mptString then
    Result := StrToBoolDef(AsString, False)
  else
  if FDataType = mptInteger then
    Result := (AsInteger <> 0)
  else
  if FDataType in [mptNull, mptUnknown] then
    Result := False
  else
    Result := False;
end;

function TMsgPack.GetAsBytes: TBytes;
begin
  Result := FValue;
end;

function TMsgPack.GetAsDateTime: TDateTime;
begin
  if FDataType in [mptDateTime, mptDouble] then
    Result := PDouble(FValue)^
  else
  if FDataType = mptSingle then
    Result := PSingle(FValue)^
  else
  if FDataType = mptString then
  begin
    Result := StrToDateTimeDef(GetAsString, 0);
  end
  else
  if FDataType in [mptInteger] then
    Result := AsInteger
  else
    Result := 0;
end;

function TMsgPack.GetAsDouble: Double;
begin
  if FDataType in [mptDouble, mptDateTime] then
    Result := PDouble(FValue)^
  else
  if FDataType = mptSingle then
    Result := PSingle(FValue)^
  else
  if FDataType = mptBoolean then
    Result := Integer(AsBoolean)
  else
  if FDataType = mptString then
    Result := StrToFloatDef(AsString, 0)
  else
  if FDataType = mptInteger then
    Result := AsInteger
  else
    Result := 0;
end;

function TMsgPack.getAsInteger: Int64;
begin
  case FDataType of
    mptInteger: Result:=PInt64(FValue)^;
  else
    Result := 0;
  end;
end;

function TMsgPack.GetAsSingle: Single;
begin
  if FDataType in [mptDouble, mptDateTime] then
    Result := PDouble(FValue)^
  else
  if FDataType = mptSingle then
    Result := PSingle(FValue)^
  else
  if FDataType = mptBoolean then
    Result := Integer(AsBoolean)
  else
  if FDataType = mptString then
    Result := StrToFloatDef(AsString, 0)
  else
  if FDataType = mptInteger then
    Result := AsInteger
  else
    Result := 0;
end;

function TMsgPack.getAsString: String;
var
  l:Cardinal;
begin
  Result := '';
  if FDataType = mptString then
  begin
    l := Length(FValue);
    if l = 0 then
    begin
      Result := '';
    end
    else
    if SizeOf(Char) = 2 then
    begin
      SetLength(Result, l shr 1);
      Move(FValue[0],PChar(Result)^, l);
    end
    else
    begin
      SetLength(Result, l);
      Move(FValue[0],PChar(Result)^, l);
    end;
  end
  else
  begin
    case FDataType of
      mptUnknown, mptNull:
        Result := '';
      mptInteger:
        Result := IntToStr(AsInteger);
      mptBoolean:
        Result := BoolToStr(AsBoolean, True);
      mptDouble:
        Result := FloatToStrF(AsDouble, ffGeneral, 15, 0);
      mptSingle:
        Result := FloatToStrF(AsSingle, ffGeneral, 7, 0);
      mptBinary:
        Result := BinToHex(@FValue[0], Length(FValue), False);
      mptDateTime:
        Result := EncodeDateTime(AsDateTime);
//      mptArray:
//        Result := EncodeArray;
//      mptMap:
//        Result := EncodeMap;
//      mptExtended:
//        Result := EncodeExtended;
    else
       Result := '';
    end;
  end;
  //showMessage(Result);
end;

/// <summary>
///   copy from qdac3
/// </summary>
function TMsgPack.GetAsVariant: Variant;
var
  I: Integer;
  procedure BytesAsVariant;
  var
    L: Integer;
    p:PByte;
  begin
    L := Length(FValue);
    Result := VarArrayCreate([0, L - 1], varByte);
    p:=VarArrayLock(Result);
    Move(FValue[0],p^,L);
    VarArrayUnlock(Result);
  end;

begin
  case FDataType of
    mptNull:
      Result := null;
    mptString:
      Result := AsString;
    mptInteger:
      Result := AsInteger;
    mptDouble:
      Result := AsDouble;
    mptSingle:
      Result := AsSingle;
    mptDateTime:
      Result := AsDateTime;
    mptBoolean:
      Result := AsBoolean;
    mptArray, mptMap:
      begin
        Result := VarArrayCreate([0, Count - 1], varVariant);
        for I := 0 to Count - 1 do
          Result[I] := TMsgPack(FChildren[I]).AsVariant;
      end;
    mptBinary:
      BytesAsVariant;
  else
    raise Exception.Create(SVariantConvertNotSupport);
  end;
end;

function TMsgPack.GetB(pvPath: String): Boolean;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    Result := False;
  end
  else
  begin
    Result := lvObj.AsBoolean;
  end;
end;

function TMsgPack.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TMsgPack.GetD(pvPath: String): Double;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    Result := 0;
  end
  else
  begin
    Result := lvObj.AsDouble;
  end;
end;

function TMsgPack.GetDT(pvPath: String): TDateTime;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    Result := 0;
  end
  else
  begin
    Result := lvObj.AsDateTime;
  end;
end;

function TMsgPack.GetI(pvPath: String): Int64;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    Result := 0;
  end
  else
  begin
    Result := lvObj.AsInteger;
  end;
end;

function TMsgPack.GetItems(AIndex: Integer): TMsgPack;
begin
  Result := TMsgPack(FChildren[AIndex]);
end;

function TMsgPack.GetO(pvPath: String): TMsgPack;
var
  lvParent:TMsgPack;
  j:Integer;
begin
  Result := InnerFindPathObject(pvPath, lvParent, j);
end;

function TMsgPack.GetResult: Boolean;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(BACKRESULT);
  if lvObj = nil then
    Result := False
  else
    Result := lvObj.AsBoolean;
end;

function TMsgPack.GetS(pvPath: String): string;
var
  lvObj:TMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    Result := '';
  end
  else
  begin
    Result := lvObj.AsString;
  end;
end;

function TMsgPack.IndexOf(pvName:string): Integer;
begin
  Result := indexOfIgnoreSensitive(LowerCase(pvName));
end;

function TMsgPack.IndexOfCaseSensitive(pvName:string): Integer;
var
  i, l: Integer;
  lvObj:TMsgPack;
begin
  Result := -1;
  l := Length(pvName);
  if l = 0 then exit;
  for i := 0 to FChildren.Count-1 do
  begin
    lvObj := TMsgPack(FChildren[i]);
    if Length(lvObj.FName) = l then
    begin
      if lvObj.FName = pvName then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

function TMsgPack.IndexOfIgnoreSensitive(pvLowerCaseName: string):
    Integer;
var
  i, l: Integer;
  lvObj:TMsgPack;
begin
  Result := -1;
  l := Length(pvLowerCaseName);
  if l = 0 then exit;
  for i := 0 to FChildren.Count-1 do
  begin
    lvObj := TMsgPack(FChildren[i]);
    if Length(lvObj.FLowerName) = l then
    begin
      if lvObj.FLowerName = pvLowerCaseName then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

function TMsgPack.InnerAdd(pvDataType: TMsgPackType): TMsgPack;
begin
  Result := TMsgPack.Create;
  Result.FDataType := mptUnknown;
  InnerAddToChildren(pvDataType, Result);
end;

function TMsgPack.InnerAdd: TMsgPack;
begin
  if Self.FDataType in [mptMap, mptArray] then
  begin
    Result := TMsgPack.Create;
    Result.FDataType := mptUnknown;
    Result.FParent := Self;
    FChildren.Add(Result);
  end
  else
  begin
    raise Exception.Create(SCannotAddChild);
  end;
end;

procedure TMsgPack.InnerAddToChildren(pvDataType: TMsgPackType; obj:
    TMsgPack);
begin
  checkObjectDataType(pvDataType);
  obj.FParent := Self;
  FChildren.Add(obj);
end;

procedure TMsgPack.InnerEncodeToStream(pvStream:TStream);
begin
  case FDataType of
    mptUnknown, mptNull: WriteNull(pvStream);
    mptMap: writeMap(Self, pvStream);
    mptArray: writeArray(Self, pvStream);
    mptString: writeString(Self.getAsString, pvStream);
    mptInteger: WriteInt(Self.getAsInteger, pvStream);
    mptBoolean: WriteBoolean(Self.GetAsBoolean, pvStream);
    mptDouble: WriteDouble(GetAsDouble, pvStream);
    mptDateTime: WriteDateTime(GetAsDateTime, pvStream);
    mptSingle: WriteSingle(GetAsSingle, pvStream);
    mptBinary: WriteBinary(PByte(@FValue[0]), Length(FValue), pvStream);
  end;
end;

function TMsgPack.InnerFindPathObject(pvPath: string; var vParent:
    TMsgPack; var vIndex: Integer): TMsgPack;
var
  lvName:string;
  s:string;
  sPtr:PChar;
  lvTempObj, lvParent:TMsgPack;
  j:Integer;
begin
  s := pvPath;
  
  Result := nil;
  
  lvParent := Self;
  sPtr := PChar(s);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/','\']);
    if lvName = '' then
    begin
      Break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin           // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          Result := TMsgPack(lvParent.FChildren[j]);
          vIndex := j;
          vParent := lvParent;
        end
        else
        begin
          Break;
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          Break;
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then Break;
    Inc(sPtr);
  end;
end;

procedure TMsgPack.InnerParseFromStream(pvStream: TStream);
var
  lvByte:Byte;
  lvBData: array[0..15] of Byte;
  lvSwapData: array[0..7] of Byte;
  lvAnsiStr:{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l, i:Cardinal;
  i64 :Int64;
  D64: Double;
  vDT: TDateTime;
  lvObj:TMsgPack;
begin
  pvStream.Read(lvByte, 1);
  if lvByte in [$00 .. $7F] then   //positive fixint	0xxxxxxx	0x00 - 0x7f
  begin
    //  +--------+
    //  |0XXXXXXX|
    //  +--------+
    setAsInteger(lvByte);
  end
  else
  if lvByte in [$80 .. $8F] then //fixmap	1000xxxx	0x80 - 0x8f
  begin
    FDataType := mptMap;
    SetLength(FValue, 0);
    FChildren.Clear;
    l := lvByte - $80;
    if l > 0 then  // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := InnerAdd(mptMap);

        // map key
        lvObj.InnerParseFromStream(pvStream);
        lvObj.setName(lvObj.getAsString);

          // value
        lvObj.InnerParseFromStream(pvStream);
      end;
    end;
  end
  else
  if lvByte in [$90 .. $9F] then //fixarray	1001xxxx	0x90 - 0x9f
  begin
    FDataType := mptArray;
    SetLength(FValue, 0);
    FChildren.Clear;

    l := lvByte - $90;
    if l > 0 then  // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := InnerAdd(mptArray);
        // value
        lvObj.InnerParseFromStream(pvStream);
      end;
    end;
  end
  else
  if lvByte in [$A0 .. $BF] then //fixstr	101xxxxx	0xa0 - 0xbf
  begin
    l := lvByte - $A0;   // str len
    if l > 0 then
    begin

      SetLength(lvAnsiStr, l);
      pvStream.Read(PByte(lvAnsiStr)^, l);
      setAsString(UTF8DecodeEx(lvAnsiStr, l));

//      SetLength(lvBytes, l + 1);
//      lvBytes[l] := 0;
//      pvStream.Read(lvBytes[0], l);
//      setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
    end
    else
    begin
      setAsString('');
    end;
  end
  else
  if lvByte in [$E0 .. $FF] then
  begin
    //  negative fixnum stores 5-bit negative integer
    //  +--------+
    //  |111YYYYY|
    //  +--------+
    setAsInteger(Shortint(lvByte));
  end
  else
  begin
    case lvByte of
      $C0: // null
        begin
          FDataType := mptNull;
          SetLength(FValue, 0);
        end;

      $C1: // (never used)
        raise Exception.Create('(never used) type $c1');

      $C2: // False
        begin
          SetAsBoolean(False);
        end;

      $C3: // True
        begin
          SetAsBoolean(True);
        end;

      $C4: // 短二进制，最长255字节
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 1);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;

      $C5: // 二进制，16位，最长65535B
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;

      $C6: // 二进制，32位，最长2^32-1
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 4);
          l := swap32(l);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;

      $C7, $C8, $C9:      //ext 8	11000111	0xc7, ext 16	11001000	0xc8, ext 32	11001001	0xc9
        begin
          raise Exception.Create('(ext8,ext16,ex32) type $c7,$c8,$c9');
        end;

      $CA: // float 32
        begin
          pvStream.Read(lvBData[0], 4);

          swap32Ex(lvBData[0], lvSwapData[0]);

          AsSingle := PSingle(@lvSwapData[0])^;
        end;

      $CB: // Float, Double 64
        begin
          //pvStream.ReadBuffer(D64, SizeOf(D64));

          pvStream.Read(lvSwapData[0], 8);
          // swap to int64, and lvBData is not valid double value (for IEEE)
          D64 := swapDouble(lvSwapData[0]);
          //
          AsDouble := PDouble(@D64)^;

         // AsFloat := swap(PDouble(@lvBData[0])^);
        end;

      $CC: // UInt8
        begin
          //      uint 8 stores a 8-bit unsigned integer
          //      +--------+--------+
          //      |  0xcc  |ZZZZZZZZ|
          //      +--------+--------+
          l := 0;
          pvStream.Read(l, 1);
          setAsInteger(l);
        end;

      $CD:
        begin
          //    uint 16 stores a 16-bit big-endian unsigned integer
          //    +--------+--------+--------+
          //    |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+
          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
          SetAsInteger(Word(l));
        end;

      $CE:
        begin
          //  uint 32 stores a 32-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          setAsInteger(Cardinal(l));
        end;

      $CF:
        begin
          //  uint 64 stores a 64-bit big-endian unsigned integer
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          //  |  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          setAsInteger(i64);
        end;

      $D0:   //int 8
        begin
          //      int 8 stores a 8-bit signed integer
          //      +--------+--------+
          //      |  0xd0  |ZZZZZZZZ|
          //      +--------+--------+

          l := 0;
          pvStream.Read(l, 1);
          SetAsInteger(ShortInt(l));
        end;
      $D1:
        begin
          //    int 16 stores a 16-bit big-endian signed integer
          //    +--------+--------+--------+
          //    |  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
          //    +--------+--------+--------+

          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
          SetAsInteger(SmallInt(l));
        end;

      $D2:
        begin
          //  int 32 stores a 32-bit big-endian signed integer
          //  +--------+--------+--------+--------+--------+
          //  |  0xd2  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          setAsInteger(Integer(l));
        end;
      $D3:
        begin
          //  int 64 stores a 64-bit big-endian signed integer
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          //  |  0xd3  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          //  +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          setAsInteger(Int64(i64));
        end;

      $D4:  // TDataTiem
        begin
          pvStream.Read(vDT, SizeOf(vDT));
          vDT := swapDouble(vDT);
          AsDateTime := vDT;
        end;

      $D9:   //str 8 , 255
        begin
          //  str 8 stores a byte array whose length is upto (2^8)-1 bytes:
          //  +--------+--------+========+
          //  |  0xd9  |YYYYYYYY|  data  |
          //  +--------+--------+========+
          l := 0;
          pvStream.Read(l, 1);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(UTF8DecodeEx(lvAnsiStr, l));
          end else
          begin
            setAsString('');
          end;
  //        SetLength(lvBytes, l + 1);
  //        lvBytes[l] := 0;
  //        pvStream.Read(lvBytes[0], l);
  //        setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;

      $DA:    // str 16
        begin
          //      str 16 stores a byte array whose length is upto (2^16)-1 bytes:
          //      +--------+--------+--------+========+
          //      |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
          //      +--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(UTF8DecodeEx(lvAnsiStr, l));
          end else
          begin
            setAsString('');
          end;

  //        SetLength(lvBytes, l + 1);
  //        lvBytes[l] := 0;
  //        pvStream.Read(lvBytes[0], l);
  //        setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;

      $DB:    // str 16
        begin
          //  str 32 stores a byte array whose length is upto (2^32)-1 bytes:
          //  +--------+--------+--------+--------+--------+========+
          //  |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
          //  +--------+--------+--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 4);
          l := swap32(l);
          if l > 0 then  // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(UTF8DecodeEx(lvAnsiStr, l));
          end else
          begin
            setAsString('');
          end;


  //        SetLength(lvBytes, l + 1);
  //        lvBytes[l] := 0;
  //        pvStream.Read(lvBytes[0], l);
  //        setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;

      $DC: // array 16
        begin
          //      +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //      |  0xdc  |YYYYYYYY|YYYYYYYY|    N objects    |
          //      +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptArray;
          SetLength(FValue, 0);
          FChildren.Clear;

          l := 0; // fill zero
          pvStream.Read(l, 2);

          l := swap16(l);
          if l > 0 then  // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptArray);
              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;

      $DD: // Array 32
        begin
        //  +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
        //  |  0xdd  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|    N objects    |
        //  +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptArray;
          SetLength(FValue, 0);
          FChildren.Clear;


          l := 0; // fill zero
          pvStream.Read(l, 4);

          l := swap32(l);
          if l > 0 then  // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptArray);
              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;

      $DE: // Object map 16
        begin
          //    +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //    |  0xde  |YYYYYYYY|YYYYYYYY|   N*2 objects   |
          //    +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptMap;
          SetLength(FValue, 0);
          FChildren.Clear;


          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);
          if l > 0 then  // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptMap);
              // map key
              lvObj.InnerParseFromStream(pvStream);
              lvObj.setName(lvObj.getAsString);

              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;

      $DF: //Object map 32
        begin
          //    +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          //    |  0xdf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|   N*2 objects   |
          //    +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptMap;
          SetLength(FValue, 0);
          FChildren.Clear;


          l := 0; // fill zero
          pvStream.Read(l, 4);

          l := swap32(l);
          if l > 0 then  // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptMap);

              // map key
              lvObj.InnerParseFromStream(pvStream);
              lvObj.setName(lvObj.getAsString);

              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;
    end;
  end;
end;

procedure TMsgPack.LoadBinaryFromFile(pvFileName:String);
var
  lvFileStream:TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    lvFileStream := TFileStream.Create(pvFileName, fmOpenRead);
    try
      LoadBinaryFromStream(lvFileStream);
    finally
      lvFileStream.Free;
    end;
  end;
end;

procedure TMsgPack.LoadBinaryFromStream(pvStream: TStream; pvLen:
    cardinal = 0);
begin
  FDataType := mptBinary;
  if pvLen = 0 then
  begin
    pvStream.Position := 0;
    SetLength(FValue, pvStream.Size);
    pvStream.Read(FValue[0], pvStream.Size);
  end
  else
  begin
    SetLength(FValue, pvLen);
    pvStream.ReadBuffer(FValue[0], pvLen);
  end;
end;

procedure TMsgPack.SaveBinaryToFile(pvFileName: String);
var
  lvFileStream:TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    if not DeleteFile(PChar(pvFileName)) then
      RaiseLastOSError;
  end;
  lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    lvFileStream.WriteBuffer(FValue[0], Length(FValue));
  finally
    lvFileStream.Free;
  end;
end;

procedure TMsgPack.SaveBinaryToStream(pvStream: TStream);
begin
  pvStream.WriteBuffer(FValue[0], Length(FValue));
end;

procedure TMsgPack.SetAsBoolean(const Value: Boolean);
begin
  FDataType := mptBoolean;
  SetLength(FValue, 1);
  PBoolean(@FValue[0])^ := Value;
end;

procedure TMsgPack.SetAsBytes(const Value: TBytes);
begin
  FDataType := mptBinary;
  FValue := Value;
end;

procedure TMsgPack.SetAsDateTime(const Value: TDateTime);
begin
  FDataType := mptDateTime;
  SetLength(FValue, SizeOf(TDateTime));
  PDouble(@FValue[0])^ := Value;
end;

procedure TMsgPack.SetAsDouble(const Value: Double);
begin
  FDataType := mptDouble;
  SetLength(FValue, SizeOf(Double));
  PDouble(@FValue[0])^ := Value;
end;

procedure TMsgPack.setAsInteger(pvValue: Int64);
begin
  FDataType := mptInteger;
  SetLength(FValue, SizeOf(Int64));
  PInt64(@FValue[0])^ := pvValue;
end;

procedure TMsgPack.SetAsSingle(const Value: Single);
begin
  FDataType := mptSingle;
  SetLength(FValue, SizeOf(Single));
  PSingle(FValue)^ := Value;
end;

procedure TMsgPack.setAsString(pvValue: string);
begin
  FDataType := mptString;
  if SizeOf(Char) = 2 then
  begin
    SetLength(FValue, length(pvValue) shl 1);
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end
  else
  begin
    SetLength(FValue, length(pvValue));
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end;
end;

/// <summary>
///   copy from qdac3
/// </summary>
procedure TMsgPack.SetAsVariant(const Value: Variant);
var
  I: Integer;
  AType: TVarType;
  procedure VarAsBytes;
  var
    L: Integer;
    p: PByte;
  begin
    FDataType := mptBinary;
    L := VarArrayHighBound(Value, 1) + 1;
    SetLength(FValue, L);
    p := VarArrayLock(Value);
    Move(p^, FValue[0], L);
    VarArrayUnlock(Value);
  end;
begin
  {$IF CompilerVersion > 14.0}  // 2010以上版本
  if VarIsSQLTimeStamp(Value) then
    AsDateTime := Value
  else
  {$IFEND}

  if VarIsArray(Value) then
  begin
    AType := VarType(Value);
    if (AType and varTypeMask) = varByte then
      VarAsBytes
    else
    begin
      checkObjectDataType(mptArray);
      FChildren.Clear;
      for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
        to VarArrayHighBound(Value, VarArrayDimCount(Value))
      do
        Add.AsVariant := Value[I];
    end;
  end
  else
  begin
    case VarType(Value) of
      varSmallInt, varInteger, varByte, varShortInt, varWord,
        varLongWord, varInt64:
        AsInteger := Value;
      varSingle, varDouble, varCurrency:
        AsDouble := Value;
      varDate:
        AsDateTime := Value;
      varOleStr, varString{$IFDEF UNICODE}, varUString{$ENDIF}:
        AsString := Value;
      varBoolean:
        AsBoolean := Value;
      varNull,varEmpty,varUnknown:
        begin
          FDataType:=mptNull;
          SetLength(FValue, 0);
        end;
      {$IF RtlVersion>=26}
      varUInt64:
        AsInteger := Value;
      {$IFEND}
    else
      // null
      ;//raise Exception.Create(SVariantConvertNotSupport);
    end;
  end;
end;

procedure TMsgPack.SetB(pvPath: String; const Value: Boolean);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsBoolean := Value;
end;

procedure TMsgPack.SetD(pvPath: String; const Value: Double);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsDouble := Value;
end;

procedure TMsgPack.SetDT(pvPath: String; const Value: TDateTime);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsDateTime := Value;
end;

procedure TMsgPack.SetI(pvPath: String; const Value: Int64);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsInteger := Value;
end;

procedure TMsgPack.setName(pvName: string);
begin
  FName := pvName;
  FLowerName := LowerCase(FName);
end;

procedure TMsgPack.SetO(pvPath: String; const Value: TMsgPack);
var
  lvName:String;
  s:String;
  sPtr:PChar;
  lvTempObj, lvParent:TMsgPack;
  j:Integer;
begin
  s := pvPath;

  lvParent := Self;
  sPtr := PChar(s);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/','\']);
    if lvName = '' then
    begin
      Break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin           // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          lvTempObj := TMsgPack(lvParent.FChildren[j]);
          lvParent.FChildren[j] := Value;
          lvTempObj.Free;  // free old
        end
        else
        begin
          Value.setName(lvName);
          lvParent.InnerAddToChildren(mptMap, Value);
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          lvParent := lvParent.Add(lvName);
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then Break;
    Inc(sPtr);
  end;
end;

procedure TMsgPack.SetResult(const Value: Boolean);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(BACKRESULT);
  lvObj.AsBoolean := Value;
end;

procedure TMsgPack.SetS(pvPath: String; const Value: string);
var
  lvObj:TMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsString := Value;
end;

end.
