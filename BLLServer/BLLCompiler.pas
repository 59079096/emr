{*******************************************************}
{                                                       }
{         基于HCView的电子病历程序  作者：荆通          }
{                                                       }
{ 此代码仅做学习交流使用，不可用于商业目的，由此引发的  }
{ 后果请使用者承担，加入QQ群 649023932 来获取更多的技术 }
{ 交流。                                                }
{                                                       }
{*******************************************************}

unit BLLCompiler;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs, FireDAC.Comp.Client, Data.DB, emr_MsgPack,
  emr_DataBase, HCCompiler, PaxRegister, PaxProgram, PaxRunner, IMPORT_Classes,
  IMPORT_SysUtils, IMPORT_Dialogs, IMPORT_Variants;

type
  TBLLObj = class(TObject)
  private
    FDebugInfo: TStringList;
    FRecordCount: Integer;
  public
    DB, BLLDB: TDataBase;
    BLLQuery: TFDQuery;
    constructor Create; virtual;
    destructor Destroy; override;
    class procedure SetProposal(const AInsertList, AItemList: TStrings);
    // 业务数据连接相关的操作

    /// <summary> 业务对应的数据库中执行查询语句 </summary>
    function SelectSQL(const ASql: string; const AConn: Byte = 1): Integer;

    /// <summary> 业务对应的数据库中执行操作语句 </summary>
    function ExecSQL(const ASql: string; const AConn: Byte = 1): Integer; overload;
    function ExecSQL(const AConn: Byte = 1): Integer; overload;

    procedure SetSQL(const ASql: string; const AConn: Byte = 1);

    procedure SetParamValue(const AParam: string; const AValue: Variant; const AConn: Byte = 1);
    procedure ParamLoadFromStream(const AParam: string; const AStream: TStream; const AConn: Byte = 1);

    function FieldAsString(const AField: string; const AConn: Byte = 1): string;
    function FieldAsInteger(const AField: string; const AConn: Byte = 1): Longint;
    function FieldAsBoolean(const AField: string; const AConn: Byte = 1): Boolean;
    function FieldAsDateTime(const AField: string; const AConn: Byte = 1): TDateTime;
    function FieldAsSingle(const AField: string; const AConn: Byte = 1): Single;
    function FieldAsFloat(const AField: string; const AConn: Byte = 1): Double;
    function FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;

    /// <summary> 业务对应的数据库连接开始一个事务 </summary>
    procedure StartTransaction(const AConn: Byte = 1);

    /// <summary> 业务对应的数据库连接提交事务操作 </summary>
    procedure Commit(const AConn: Byte = 1);

    /// <summary> 业务对应的数据库连接回滚事务 </summary>
    procedure Rollback(const AConn: Byte = 1);

    procedure AddDebugInfo(const AInfo: string);

    property DebugInfo: TStringList read FDebugInfo;
  end;

  TVarAddressEvent = procedure (const FullName: string; Global: Boolean; var Address: Pointer) of object;

  TBLLCompiler = class(THCCompiler)  // 如果要增加属性或方法，需要在Proposal、BLLRegImportClass及对应的MapTable中统一增加
  private
    FOnVarAddress: TVarAddressEvent;
  protected
    procedure DoMapTableNamespace(Sender: TPaxRunner; const FullName: string;
      Global: Boolean); override;
    procedure DoMapTableVarAddress(Sender: TPaxRunner; const FullName: string;
      Global: Boolean; var Address: Pointer); override;
    procedure DoMapTableProcAddress(Sender: TPaxRunner; const FullName: string;
      OverCount: Byte; Global: Boolean; var Address: Pointer); override;
    procedure DoMapTableClassRef(Sender: TPaxRunner; const FullName: string;
      Global: Boolean; var ClassRef: TClass); override;
  public
    /// <summary> 设置 TBLLCpl 相关的代码提示 </summary>
    class procedure Proposal(const AWord: string; const AInsertList, AItemList: TStrings);

    /// <summary> 注册 TBLLCpl 需要的类和要设置的字符串 </summary>
    procedure RegClassVariable(const AMsgPack, ABLLObj: Pointer);

    property OnVarAddress: TVarAddressEvent read FOnVarAddress write FOnVarAddress;
  end;

  function TBLLObj_ExecSQL0(Self: TBLLObj; const AConn: Byte = 1): Integer;
  function TBLLObj_ExecSQL1(Self: TBLLObj; const ASql: string; const AConn: Byte = 1): Integer;
  function TMsgPack_GetAsInteger(Self: TMsgPack): Int64;
  procedure TMsgPack_SetAsInteger(Self: TMsgPack; const AValue: Integer);
  function TMsgPack_GetAsString(Self: TMsgPack): string;
  procedure TMsgPack_SetAsString(Self: TMsgPack; const AValue: string);
  function TMsgPack_GetAsBoolean(Self: TMsgPack): Boolean;
  procedure TMsgPack_SetAsBoolean(Self: TMsgPack; const AValue: Boolean);
  function TMsgPack_GetAsDouble(Self: TMsgPack): Double;
  procedure TMsgPack_SetAsDouble(Self: TMsgPack; const AValue: Double);
  function TMsgPack_GetAsSingle(Self: TMsgPack): Single;
  procedure TMsgPack_SetAsSingle(Self: TMsgPack; const AValue: Single);
  function TMsgPack_GetAsDateTime(Self: TMsgPack): TDateTime;
  procedure TMsgPack_SetAsDateTime(Self: TMsgPack; const AValue: TDateTime);
  function TMsgPack_GetAsVariant(Self: TMsgPack): Variant;
  procedure TMsgPack_SetAsVariant(Self: TMsgPack; const AValue: Variant);

implementation

const
  ProposalCommColor = '$00A00000';

var
  MsgPackClassType, BLLObjClassType: Integer;

{ TBLLCompiler }

procedure TBLLCompiler.DoMapTableClassRef(Sender: TPaxRunner;
  const FullName: string; Global: Boolean; var ClassRef: TClass);
var
  vName: string;
begin
  vName := LowerCase(FullName);
  if vName = 'tbllobj' then
    ClassRef := TBLLObj
  else
  if vName = 'emr_msgpack.tmsgpack' then
    ClassRef := emr_MsgPack.TMsgPack
  else
  if vName = 'classes.tmemorystream' then
    ClassRef := System.Classes.TMemoryStream;
end;

procedure TBLLCompiler.DoMapTableNamespace(Sender: TPaxRunner;
  const FullName: string; Global: Boolean);
begin
  //vName := LowerCase(FullName)
  //if FullName = 'emr_MsgPack' then
end;

procedure TBLLCompiler.DoMapTableProcAddress(Sender: TPaxRunner;
  const FullName: string; OverCount: Byte; Global: Boolean;
  var Address: Pointer);
var
  vName: string;
begin
  vName := LowerCase(FullName);
  {$REGION 'emr_MsgPack.TMsgPack'}
  if vName = 'emr_msgpack.tmsgpack.path' then
    Address := @emr_MsgPack.TMsgPack.Path
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasinteger' then
    Address := @TMsgPack_GetAsInteger
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasinteger' then
    Address := @TMsgPack_SetAsInteger
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasstring' then
    Address := @TMsgPack_GetAsString
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasstring' then
    Address := @TMsgPack_SetAsString
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasboolean' then
    Address := @TMsgPack_GetAsBoolean
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasboolean' then
    Address := @TMsgPack_SetAsBoolean
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasdouble' then
    Address := @TMsgPack_GetAsDouble
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasdouble' then
    Address := @TMsgPack_SetAsDouble
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getassingle' then
    Address := @TMsgPack_GetAsSingle
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setassingle' then
    Address := @TMsgPack_SetAsSingle
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasdatetime' then
    Address := @TMsgPack_GetAsDateTime
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasdatetime' then
    Address := @TMsgPack_SetAsDateTime
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_getasvariant' then
    Address := @TMsgPack_GetAsVariant
  else
  if vName = 'emr_msgpack.tmsgpack.tmsgpack_setasvariant' then
    Address := @TMsgPack_SetAsVariant
  else
  if vName = 'emr_msgpack.tmsgpack.loadbinaryfromstream' then
    Address := @TMsgPack.LoadBinaryFromStream
  else
  if vName = 'emr_msgpack.tmsgpack.savebinarytostream' then
    Address := @TMsgPack.SaveBinaryToStream
  {$ENDREGION}
  else
  {$REGION 'TBLLObj'}
  if vName = 'tbllobj.selectsql' then
    Address := @TBLLObj.SelectSQL
  else
  if vName = 'tbllobj.execsql' then
  begin
    if OverCount = 0 then
      Address := @TBLLObj_ExecSQL0
    else
      Address := @TBLLObj_ExecSQL1;
  end
  else
  if vName = 'tbllobj.starttransaction' then
    Address := @TBLLObj.StartTransaction
  else
  if vName = 'tbllobj.commit' then
    Address := @TBLLObj.Commit
  else
  if vName = 'tbllobj.rollback' then
    Address := @TBLLObj.Rollback
  else
  if vName = 'tbllobj.setsql' then
    Address := @TBLLObj.SetSQL
  else
  if vName = 'tbllobj.setparamvalue' then
    Address := @TBLLObj.SetParamValue
  else
  if vName = 'tbllobj.paramloadfromstream' then
    Address := @TBLLObj.ParamLoadFromStream
  else
  if vName = 'tbllobj.fieldasstring' then
    Address := @TBLLObj.FieldAsString
  else
  if vName = 'tbllobj.fieldasinteger' then
    Address := @TBLLObj.FieldAsInteger
  else
  if vName = 'tbllobj.fieldasboolean' then
    Address := @TBLLObj.FieldAsBoolean
  else
  if vName = 'tbllobj.fieldasdatetime' then
    Address := @TBLLObj.FieldAsDateTime
  else
  if vName = 'tbllobj.fieldassingle' then
    Address := @TBLLObj.FieldAsSingle
  else
  if vName = 'tbllobj.fieldasfloat' then
    Address := @TBLLObj.FieldAsFloat
  else
  if vName = 'tbllobj.fieldasvariant' then
    Address := @TBLLObj.FieldAsVariant
  else
  if vName = 'tbllobj.adddebuginfo' then
    Address := @TBLLObj.AddDebugInfo
  {$ENDREGION}
  else
  if vName = 'classes.tmemorystream.create' then
    Address := @System.Classes.TMemoryStream.Create
  else
  if vName = 'sysutils.quotedstr' then
    Address := @System.SysUtils.QuotedStr
  else
  if vName = 'sysutils.format' then
    Address := @System.SysUtils.Format
  else
  if vName = 'sysutils.formatdatetime' then
    Address := @System.SysUtils.FormatDateTime
  else
  if vName = 'sysutils.inttostr' then
    Address := @System.SysUtils.IntToStr
  else
  if vName = 'dialogs.showmessage' then
    Address := @Vcl.Dialogs.ShowMessage;
end;

procedure TBLLCompiler.DoMapTableVarAddress(Sender: TPaxRunner;
  const FullName: string; Global: Boolean; var Address: Pointer);
begin
  if Assigned(FOnVarAddress) then
    FOnVarAddress(FullName, Global, Address);
end;

class procedure TBLLCompiler.Proposal(const AWord: string; const AInsertList,
  AItemList: TStrings);
begin
  if AWord = '.' then
  begin
    AInsertList.Add('MsgPack');
    AItemList.Add('var \column{}\style{+B}MsgPack\style{-B}: TMsgPack;  // 客户端传递的数据');
    AInsertList.Add('BLL');
    AItemList.Add('var \column{}\style{+B}BLL\style{-B}: TBLLObj;  // 业务处理对象');
  end
  else
  if AWord = 'BLL' then
    TBLLObj.SetProposal(AInsertList, AItemList)
  else
  if AWord = 'MSGPACK' then
    TMsgPack.SetProposal(AInsertList, AItemList);
end;

procedure TBLLCompiler.RegClassVariable(const AMsgPack, ABLLObj: Pointer);
begin
  Self.RegisterVariable(0, 'MsgPack', MsgPackClassType, AMsgPack);  // 注册MsgPack
  Self.RegisterVariable(0, 'BLL', BLLObjClassType, ABLLObj);  // 注册业务对象
end;

function TBLLObj_ExecSQL0(Self: TBLLObj; const AConn: Byte = 1): Integer;
begin
  Result := Self.ExecSQL(AConn);
end;

function TBLLObj_ExecSQL1(Self: TBLLObj; const ASql: string; const AConn: Byte = 1): Integer;
begin
  Result := Self.ExecSQL(ASql, AConn);
end;

function TMsgPack_GetAsInteger(Self: TMsgPack): Int64;
begin
  Result := Self.AsInteger;
end;

procedure TMsgPack_SetAsInteger(Self: TMsgPack; const AValue: Integer);
begin
  Self.AsInteger := AValue;
end;

function TMsgPack_GetAsString(Self: TMsgPack): string;
begin
  Result := Self.AsString;
end;

procedure TMsgPack_SetAsString(Self: TMsgPack; const AValue: string);
begin
  Self.AsString := AValue;
end;

function TMsgPack_GetAsBoolean(Self: TMsgPack): Boolean;
begin
  Result := Self.AsBoolean;
end;

procedure TMsgPack_SetAsBoolean(Self: TMsgPack; const AValue: Boolean);
begin
  Self.AsBoolean := AValue;
end;

function TMsgPack_GetAsDouble(Self: TMsgPack): Double;
begin
  Result := Self.AsDouble;
end;

procedure TMsgPack_SetAsDouble(Self: TMsgPack; const AValue: Double);
begin
  Self.AsDouble := AValue;
end;

function TMsgPack_GetAsSingle(Self: TMsgPack): Single;
begin
  Result := Self.AsSingle;
end;

procedure TMsgPack_SetAsSingle(Self: TMsgPack; const AValue: Single);
begin
  Self.AsSingle := AValue;
end;

function TMsgPack_GetAsDateTime(Self: TMsgPack): TDateTime;
begin
  Result := Self.AsDateTime;
end;

procedure TMsgPack_SetAsDateTime(Self: TMsgPack; const AValue: TDateTime);
begin
  Self.AsDateTime := AValue;
end;

function TMsgPack_GetAsVariant(Self: TMsgPack): Variant;
begin
  Result := Self.AsVariant;
end;

procedure TMsgPack_SetAsVariant(Self: TMsgPack; const AValue: Variant);
begin
  Self.AsVariant := AValue;
end;

{ TBLLObj }

constructor TBLLObj.Create;
begin
  FDebugInfo := TStringList.Create;
end;

destructor TBLLObj.Destroy;
begin
  FDebugInfo.Free;
  inherited Destroy;
end;

function TBLLObj.ExecSQL(const AConn: Byte = 1): Integer;
begin
  Result := 0;
  BLLQuery.ExecSQL;
  Result := BLLQuery.RowsAffected;
end;

function TBLLObj.FieldAsBoolean(const AField: string; const AConn: Byte = 1): Boolean;
begin
  Result := BLLQuery.FieldByName(AField).AsBoolean;
end;

function TBLLObj.FieldAsDateTime(const AField: string; const AConn: Byte = 1): TDateTime;
begin
  Result := BLLQuery.FieldByName(AField).AsDateTime;
end;

function TBLLObj.FieldAsFloat(const AField: string; const AConn: Byte = 1): Double;
begin
  Result := BLLQuery.FieldByName(AField).AsFloat;
end;

function TBLLObj.FieldAsInteger(const AField: string; const AConn: Byte = 1): Longint;
begin
  Result := BLLQuery.FieldByName(AField).AsInteger;
end;

function TBLLObj.FieldAsSingle(const AField: string; const AConn: Byte = 1): Single;
begin
  Result := BLLQuery.FieldByName(AField).AsSingle;
end;

function TBLLObj.FieldAsString(const AField: string; const AConn: Byte = 1): string;
begin
  Result := BLLQuery.FieldByName(AField).AsString;
end;

function TBLLObj.FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;
begin
  Result := BLLQuery.FieldByName(AField).AsVariant;
end;

procedure TBLLObj.ParamLoadFromStream(const AParam: string;
  const AStream: TStream; const AConn: Byte = 1);
begin
  BLLQuery.ParamByName(AParam).LoadFromStream(AStream, TFieldType.ftBlob);
end;

procedure TBLLObj.AddDebugInfo(const AInfo: string);
begin
  FDebugInfo.Add(AInfo);
end;

procedure TBLLObj.Commit(const AConn: Byte = 1);
begin
  BLLQuery.Connection.Commit;  // 提交操作
end;

function TBLLObj.ExecSQL(const ASql: string; const AConn: Byte = 1): Integer;
begin
  Result := 0;
  if AConn = 1 then
  begin
    BLLQuery.Close;
    BLLQuery.SQL.Text := ASql;
    BLLQuery.ExecSQL;
    Result := BLLQuery.RowsAffected;
  end;
end;

procedure TBLLObj.Rollback(const AConn: Byte = 1);
begin
  BLLQuery.Connection.Rollback;
end;

function TBLLObj.SelectSQL(const ASql: string; const AConn: Byte = 1): Integer;
begin
  Result := 0;
  if AConn = 1 then
  begin
    BLLQuery.Close;
    BLLQuery.SQL.Text := ASql;
    BLLQuery.Open;
    Result := BLLQuery.RecordCount;
  end;
end;

procedure TBLLObj.SetParamValue(const AParam: string; const AValue: Variant; const AConn: Byte = 1);
begin
  BLLQuery.ParamByName(AParam).Value := AValue;
end;

class procedure TBLLObj.SetProposal(const AInsertList, AItemList: TStrings);
begin
  AInsertList.Add('SelectSQL');
  AItemList.Add('function \column{}\style{+B}SelectSQL\style{-B}(const ASql: string; const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// 业务对应的数据库中执行查询语句');
  AInsertList.Add('ExecSQL');
  AItemList.Add('function \column{}\style{+B}ExecSQL\style{-B}(const ASql: string; const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// 业务对应的数据库中执行操作语句');
  AInsertList.Add('ExecSQL');
  AItemList.Add('function \column{}\style{+B}ExecSQL\style{-B}(const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// 执行已有的SQL语句');
  AInsertList.Add('StartTransaction');
  AItemList.Add('procedure \column{}\style{+B}StartTransaction\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// 业务对应的数据库连接开始一个事务');
  AInsertList.Add('Commit');
  AItemList.Add('procedure \column{}\style{+B}Commit\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// 业务对应的数据库连接提交事务操作');
  AInsertList.Add('Rollback');
  AItemList.Add('procedure \column{}\style{+B}Rollback\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// 业务对应的数据库连接回滚事务');
  AInsertList.Add('SetSQL');
  AItemList.Add('procedure \column{}\style{+B}SetSQL\style{-B}(const ASql: string);  \color{' + ProposalCommColor + '}// 业务对应的数据库连接回滚事务');
  AInsertList.Add('ParamLoadFromStream');
  AItemList.Add('procedure \column{}\style{+B}ParamLoadFromStream\style{-B}(const AParam: string; const AStream: TStream; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// 业务对应的数据库连接回滚事务');
  AInsertList.Add('SetParamValue');
  AItemList.Add('procedure \column{}\style{+B}SetParamValue\style{-B}(const AParam: string; const AValue: Variant; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// 设置参数的值');
  AInsertList.Add('FieldAsString');
  AItemList.Add('function \column{}\style{+B}FieldAsString\style{-B}(const AField: string; const AConn: Byte = 1): string;  \color{' + ProposalCommColor + '}// 字段值转为string');
  AInsertList.Add('FieldAsInteger');
  AItemList.Add('function \column{}\style{+B}FieldAsInteger\style{-B}(const AField: string; const AConn: Byte = 1): Longint;  \color{' + ProposalCommColor + '}// 字段值转为Longint');
  AInsertList.Add('FieldAsBoolean');
  AItemList.Add('function \column{}\style{+B}FieldAsBoolean\style{-B}(const AField: string; const AConn: Byte = 1): Boolean;  \color{' + ProposalCommColor + '}// 字段值转为Boolean');
  AInsertList.Add('FieldAsDateTime');
  AItemList.Add('function \column{}\style{+B}FieldAsDateTime\style{-B}(const AField: TDateTime; const AConn: Byte = 1): TDateTime;  \color{' + ProposalCommColor + '}// 字段值转为TDateTime');
  AInsertList.Add('FieldAsSingle');
  AItemList.Add('function \column{}\style{+B}FieldAsSingle\style{-B}(const AField: string; const AConn: Byte = 1): Single;  \color{' + ProposalCommColor + '}// 字段值转为Single');
  AInsertList.Add('FieldAsFloat');
  AItemList.Add('function \column{}\style{+B}FieldAsFloat\style{-B}(const AField: string; const AConn: Byte = 1): Double;  \color{' + ProposalCommColor + '}// 字段值转为Double');
  AInsertList.Add('FieldAsVariant');
  AItemList.Add('function \column{}\style{+B}FieldAsVariant\style{-B}(const AField: string; const AConn: Byte = 1): Variant;  \color{' + ProposalCommColor + '}// 字段值转为Variant');
  AInsertList.Add('AddDebugInfo');
  AItemList.Add('procedure \column{}\style{+B}AddDebugInfo\style{-B}(const AInfo: string);  \color{' + ProposalCommColor + '}// 添加调试信息');
end;

procedure TBLLObj.SetSQL(const ASql: string; const AConn: Byte = 1);
begin
  BLLQuery.SQL.Text := ASql;
end;

procedure TBLLObj.StartTransaction(const AConn: Byte = 1);
begin
  BLLQuery.Connection.StartTransaction;  // 开始一个事务
end;

procedure BLLRegImportClass;
var
  vH: Integer;
begin
  // emr_MsgPack
  vH := RegisterNamespace(0, 'emr_MsgPack');
  RegisterRTTIType(vH, TypeInfo(TMsgPackType));
  RegisterConstant(vH, 'BLL_CMD', BLL_CMD);
  RegisterConstant(vH, 'BLL_VER', BLL_VER);
  RegisterConstant(vH, 'BLL_METHODRESULT', BLL_METHODRESULT);
  RegisterConstant(vH, 'BLL_EXECPARAM', BLL_EXECPARAM);
  RegisterConstant(vH, 'BLL_BACKDATASET', BLL_BACKDATASET);
  RegisterConstant(vH, 'BLL_BACKFIELD', BLL_BACKFIELD);
  RegisterConstant(vH, 'BLL_RECORDCOUNT', BLL_RECORDCOUNT);
  // TMsgPack
  MsgPackClassType := RegisterClassType(vH, TMsgPack);
  RegisterHeader(MsgPackClassType, 'function Path(APath: string): TMsgPack;', @TMsgPack.Path);
  RegisterHeader(MsgPackClassType, 'procedure LoadBinaryFromStream(AStream: TStream; ALen: Cardinal = 0);', @TMsgPack.LoadBinaryFromStream);
  RegisterHeader(MsgPackClassType, 'procedure SaveBinaryToStream(AStream: TStream);', @TMsgPack.SaveBinaryToStream);

  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsInteger: Integer;', @TMsgPack_GetAsInteger);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsInteger(const Value: Integer);', @TMsgPack_SetAsInteger);
  RegisterProperty(MsgPackClassType, 'property AsInteger: Int64 read TMsgPack_GetAsInteger write TMsgPack_SetAsInteger;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsString: string;', @TMsgPack_GetAsString);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsString(const Value: string);', @TMsgPack_SetAsString);
  RegisterProperty(MsgPackClassType, 'property AsString: string read TMsgPack_GetAsString write TMsgPack_SetAsString;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsBoolean: Boolean;', @TMsgPack_GetAsBoolean);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsBoolean(const Value: Boolean);', @TMsgPack_SetAsBoolean);
  RegisterProperty(MsgPackClassType, 'property AsBoolean: Boolean read TMsgPack_GetAsBoolean write TMsgPack_SetAsBoolean;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsDouble: Double;', @TMsgPack_GetAsDouble);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsDouble(const Value: Double);', @TMsgPack_SetAsDouble);
  RegisterProperty(MsgPackClassType, 'property AsDouble: Double read TMsgPack_GetAsDouble write TMsgPack_SetAsDouble;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsSingle: Single;', @TMsgPack_GetAsSingle);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsSingle(const Value: Single);', @TMsgPack_SetAsSingle);
  RegisterProperty(MsgPackClassType, 'property AsSingle: Single read TMsgPack_GetAsSingle write TMsgPack_SetAsSingle;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsDateTime: TDateTime;', @TMsgPack_GetAsDateTime);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsDateTime(const Value: TDateTime);', @TMsgPack_SetAsDateTime);
  RegisterProperty(MsgPackClassType, 'property AsDateTime: TDateTime read TMsgPack_GetAsDateTime write TMsgPack_SetAsDateTime;');
  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetAsVariant: Variant;', @TMsgPack_GetAsVariant);
  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetAsVariant(const Value: Variant);', @TMsgPack_SetAsVariant);
  RegisterProperty(MsgPackClassType, 'property AsVariant: Variant read TMsgPack_GetAsVariant write TMsgPack_SetAsVariant;');

//  RegisterFakeHeader(MsgPackClassType, 'function TMsgPack_GetPathAsString(Self: TMsgPack; const APath): string;', @TMsgPack_GetPathAsString);
//  RegisterFakeHeader(MsgPackClassType, 'procedure TMsgPack_SetPathAsString(const APath, AValue: string);', @TMsgPack_SetPathAsString);
//  RegisterProperty(MsgPackClassType, 'property PathAsString: string read TMsgPack_GetPathAsString write TMsgPack_SetPathAsString;');

  // TBLLObj
  BLLObjClassType := RegisterClassType(0, TBLLObj);
  RegisterHeader(BLLObjClassType, 'function SelectSQL(const ASql: string; const AConn: Byte = 1): Integer;', @TBLLObj.SelectSQL);
  RegisterFakeHeader(BLLObjClassType, 'function ExecSQL(const AConn: Byte = 1): Integer; overload;', @TBLLObj_ExecSQL0);
  RegisterFakeHeader(BLLObjClassType, 'function ExecSQL(const ASql: string; const AConn: Byte = 1): Integer; overload;', @TBLLObj_ExecSQL1);
  RegisterHeader(BLLObjClassType, 'procedure StartTransaction(const AConn: Byte = 1);', @TBLLObj.StartTransaction);
  RegisterHeader(BLLObjClassType, 'procedure Commit(const AConn: Byte = 1);', @TBLLObj.Commit);
  RegisterHeader(BLLObjClassType, 'procedure Rollback(const AConn: Byte = 1);', @TBLLObj.Rollback);
  RegisterHeader(BLLObjClassType, 'procedure SetSQL(const ASql: string);', @TBLLObj.SetSQL);
  RegisterHeader(BLLObjClassType, 'procedure SetParamValue(const AParam: string; const AValue: Variant; const AConn: Byte = 1);', @TBLLObj.SetParamValue);
  RegisterHeader(BLLObjClassType, 'procedure ParamLoadFromStream(const AParam: string; const AStream: TStream; const AConn: Byte = 1);', @TBLLObj.ParamLoadFromStream);

  RegisterHeader(BLLObjClassType, 'function FieldAsString(const AField: string; const AConn: Byte = 1): string;', @TBLLObj.FieldAsString);
  RegisterHeader(BLLObjClassType, 'function FieldAsInteger(const AField: string; const AConn: Byte = 1): Longint;', @TBLLObj.FieldAsInteger);
  RegisterHeader(BLLObjClassType, 'function FieldAsBoolean(const AField: string; const AConn: Byte = 1): Boolean;', @TBLLObj.FieldAsBoolean);
  RegisterHeader(BLLObjClassType, 'function FieldAsDateTime(const AField: string; const AConn: Byte = 1): TDateTime;', @TBLLObj.FieldAsDateTime);
  RegisterHeader(BLLObjClassType, 'function FieldAsSingle(const AField: string; const AConn: Byte = 1): Single;', @TBLLObj.FieldAsSingle);
  RegisterHeader(BLLObjClassType, 'function FieldAsFloat(const AField: string; const AConn: Byte = 1): Double;', @TBLLObj.FieldAsFloat);
  RegisterHeader(BLLObjClassType, 'function FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;', @TBLLObj.FieldAsVariant);

  RegisterHeader(BLLObjClassType, 'procedure AddDebugInfo(const AInfo: string);', @TBLLObj.AddDebugInfo);
//  RegisterFakeHeader(BLLObjClassType, 'function TBLLObj_GetRecordCount: Integer;', @TBLLObj_GetRecordCount);
//  RegisterFakeHeader(BLLObjClassType, 'procedure TBLLObj_PutRecordCount(const Value: Integer);', @TBLLObj_PutRecordCount);
//  RegisterProperty(BLLObjClassType, 'property RecordCount: Integer read TBLLObj_GetRecordCount write TBLLObj_PutRecordCount;');
end;

procedure RegisterImportClass;
begin
  IMPORT_Classes.Register_Classes;
  IMPORT_SysUtils.Register_SysUtils;
  IMPORT_Dialogs.Register_Dialogs;
  IMPORT_Variants.Register_Variants;

  BLLRegImportClass;
end;

initialization
  RegisterImportClass;

end.
