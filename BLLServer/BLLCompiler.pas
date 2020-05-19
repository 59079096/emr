{*******************************************************}
{                                                       }
{         ����HCView�ĵ��Ӳ�������  ���ߣ���ͨ          }
{                                                       }
{ �˴������ѧϰ����ʹ�ã�����������ҵĿ�ģ��ɴ�������  }
{ �����ʹ���߳е�������QQȺ 649023932 ����ȡ����ļ��� }
{ ������                                                }
{                                                       }
{*******************************************************}

unit BLLCompiler;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs, FireDAC.Comp.Client, Data.DB, emr_MsgPack,
  emr_DataBase, HCCompiler, PAXCOMP_KERNEL, PaxRegister;

type
  TBLLObj = class(TObject)
  private
    FDebugInfo: TStringList;
    //FRecordCount: Integer;
  public
    DB, BLLDB: TDataBase;
    BLLQuery: TFDQuery;
    ErrorInfo: string;
    constructor Create; virtual;
    destructor Destroy; override;
    class procedure Appends(const AConverts: TCplConverts; var ATypeID: Integer);
    // ҵ������������صĲ���

    /// <summary> ���ݿ����� </summary>
    function DBType(const AConn: Byte = 1): TDBType;

    function CreateBllObj: TBLLObj;
    procedure FreeBLLObj(var ABLLObj: TBLLObj);
    function Eof: Boolean;
    procedure First;
    procedure Next;
    procedure Last;

    /// <summary> ҵ���Ӧ�����ݿ���ִ�в�ѯ��� </summary>
    function SelectSQL(const ASql: string; const AConn: Byte = 1): Integer;

    /// <summary> ҵ���Ӧ�����ݿ���ִ�в������ </summary>
    function ExecSQL(const ASql: string; const AConn: Byte = 1): Integer; overload;
    function ExecSQL(const AConn: Byte = 1): Integer; overload;

    procedure SetSQL(const ASql: string; const AConn: Byte = 1);
    function GetRecordCount(const AConn: Byte = 1): Integer;
    function GetParamCount(const AConn: Byte = 1): Integer;
    function GetParamName(const AIndex: Integer; const AConn: Byte = 1): string;
    procedure SetParamValue(const AParam: string; const AValue: Variant; const AConn: Byte = 1);
    procedure ParamLoadFromStream(const AParam: string; const AStream: TStream; const AConn: Byte = 1);
    function FieldAsString(const AField: string; const AConn: Byte = 1): string;
    function FieldAsInteger(const AField: string; const AConn: Byte = 1): Longint;
    function FieldAsBoolean(const AField: string; const AConn: Byte = 1): Boolean;
    function FieldAsDateTime(const AField: string; const AConn: Byte = 1): TDateTime;
    function FieldAsSingle(const AField: string; const AConn: Byte = 1): Single;
    function FieldAsFloat(const AField: string; const AConn: Byte = 1): Double;
    function FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;
    procedure FieldAsStream(const AField: string; const AStream: TStream; const AConn: Byte = 1);
    procedure SaveToStream(const AStream: TStream; const AConn: Byte = 1);

    /// <summary> ҵ���Ӧ�����ݿ����ӿ�ʼһ������ </summary>
    procedure StartTransaction(const AConn: Byte = 1);

    /// <summary> ҵ���Ӧ�����ݿ������ύ������� </summary>
    procedure Commit(const AConn: Byte = 1);

    /// <summary> ҵ���Ӧ�����ݿ����ӻع����� </summary>
    procedure Rollback(const AConn: Byte = 1);

    procedure DebugInfoClear;
    procedure DebugInfoAppend(const AInfo: string);

    property DebugInfo: TStringList read FDebugInfo;
  end;

  TBLLMsgPack = class(TMsgPack)
  public
    class procedure Appends(const AConverts: TCplConverts; var ATypeID: Integer);
  end;

  TBLLCompiler = class(THCCompiler)  // ���Ҫ�������Ի򷽷�����Ҫ��Proposal��BLLRegImportClass����Ӧ��MapTable��ͳһ����
  private
    TMsgPackTypeID, TBLLObjTypeID: Integer;
  public
    constructor CreateByScriptType(AOwner: TComponent; const AScriptType: TScriptType = stpPascal); override;

    /// <summary> ע�� TBLLCpl ��Ҫ�����Ҫ���õ��ַ��� </summary>
    procedure RegClassVariable(const AMsgPack, ABLLObj: Pointer);
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

uses
  FireDAC.Stan.Intf;

{ TBLLCompiler }

constructor TBLLCompiler.CreateByScriptType(AOwner: TComponent;
  const AScriptType: TScriptType);
begin
  inherited CreateByScriptType(AOwner, AScriptType);

  TBLLObj.Appends(FCompilerConverts, TBLLObjTypeID);
  TBLLMsgPack.Appends(FCompilerConverts, TMsgPackTypeID);
end;

procedure TBLLCompiler.RegClassVariable(const AMsgPack, ABLLObj: Pointer);
begin
  if FindRegisterVariable(TMsgPackTypeID, 'MsgPack') then Exit;

  Self.RegisterVariable(0, 'MsgPack', TMsgPackTypeID, AMsgPack);  // ע��MsgPack
  FCompilerVariables.New(TMsgPackTypeID, AMsgPack, 'MsgPack', '�ͻ��˴��ݵ����ݽṹ');

  Self.RegisterVariable(0, 'BLL', TBLLObjTypeID, ABLLObj);  // ע��ҵ�����
  FCompilerVariables.New(TBLLObjTypeID, ABLLObj, 'BLL', 'ҵ�������');
end;

{$REGION 'TMsgPack��Compiler�����ļ�ӷ���'}
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
{$ENDREGION}

{ TBLLObj }

constructor TBLLObj.Create;
begin
  FDebugInfo := TStringList.Create;
end;

function TBLLObj.CreateBllObj: TBLLObj;
begin
  Result := TBLLObj.Create;
  Result.DB := DB;
  Result.BLLDB := BLLDB;
  Result.BLLQuery := TFDQuery.Create(nil);
  Result.BLLQuery.Connection := BLLQuery.Connection;
  Result.ErrorInfo := '';
end;

destructor TBLLObj.Destroy;
begin
  FDebugInfo.Free;
  inherited Destroy;
end;

function TBLLObj.Eof: Boolean;
begin
  Result := BLLQuery.Eof;
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

procedure TBLLObj.FieldAsStream(const AField: string; const AStream: TStream;
  const AConn: Byte = 1);
begin
  (BLLQuery.FieldByName(AField) as TBlobField).SaveToStream(AStream);
end;

function TBLLObj.FieldAsString(const AField: string; const AConn: Byte = 1): string;
begin
  Result := BLLQuery.FieldByName(AField).AsString;
end;

function TBLLObj.FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;
begin
  Result := BLLQuery.FieldByName(AField).AsVariant;
end;

procedure TBLLObj.First;
begin
  BLLQuery.First;
end;

procedure TBLLObj.FreeBLLObj(var ABLLObj: TBLLObj);
begin
  ABLLObj.BLLQuery.Free;
  ABLLObj.Free;
end;

function TBLLObj.GetParamCount(const AConn: Byte = 1): Integer;
begin
  Result := BLLQuery.ParamCount;
end;

function TBLLObj.GetParamName(const AIndex: Integer; const AConn: Byte): string;
begin
  Result := BLLQuery.Params[AIndex].Name;
end;

function TBLLObj.GetRecordCount(const AConn: Byte): Integer;
begin
  Result := BLLQuery.RecordCount;
end;

procedure TBLLObj.Last;
begin
  BLLQuery.Last;
end;

procedure TBLLObj.Next;
begin
  BLLQuery.Next;
end;

procedure TBLLObj.ParamLoadFromStream(const AParam: string;
  const AStream: TStream; const AConn: Byte = 1);
begin
  BLLQuery.ParamByName(AParam).LoadFromStream(AStream, TFieldType.ftBlob);
end;

function TBLLObj.DBType(const AConn: Byte): TDBType;
begin
  Result := BLLDB.DBType;
end;

procedure TBLLObj.DebugInfoAppend(const AInfo: string);
begin
  FDebugInfo.Add(AInfo);
end;

procedure TBLLObj.DebugInfoClear;
begin
  FDebugInfo.Clear;
  ErrorInfo := '';
end;

class procedure TBLLObj.Appends(const AConverts: TCplConverts; var ATypeID: Integer);
var
  i: Integer;
begin
  RegisterRTTIType(0, TypeInfo(TDBType));

  ATypeID := RegisterClassType(0, TBLLObj);  // TBLLObj

  i := AConverts.New('function DBType(const AConn: Byte = 1): TDBType;',
    '\image{5} \column{} function \column{}\style{+B}DBType\style{-B}(const AConn: Byte = 1): TDBType;  \color{' + ProposalCommColor + '}// ���ݿ�����',
    'DBType', 'BLLCompiler', 'TBLLObj', @TBLLObj.DBType);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function SelectSQL(const ASql: string; const AConn: Byte = 1): Integer;',
    '\image{5} \column{} function \column{}\style{+B}SelectSQL\style{-B}(const ASql: string; const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// ҵ���Ӧ�����ݿ���ִ�в�ѯ���',
    'SelectSQL', 'BLLCompiler', 'TBLLObj', @TBLLObj.SelectSQL);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function ExecSQL(const AConn: Byte = 1): Integer; overload;',
    '\image{5} \column{} function \column{}\style{+B}ExecSQL\style{-B}(const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// ҵ���Ӧ�����ݿ���ִ�в������',
    'ExecSQL', 'BLLCompiler', 'TBLLObj', @TBLLObj_ExecSQL0, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function ExecSQL(const ASql: string; const AConn: Byte = 1): Integer; overload;',
    '\image{5} \column{} function \column{}\style{+B}ExecSQL\style{-B}(const ASql: string; const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// ִ�����е�SQL���',
    'ExecSQL', 'BLLCompiler', 'TBLLObj', @TBLLObj_ExecSQL1, True, 1);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure StartTransaction(const AConn: Byte = 1)',
    '\image{4} \column{} procedure \column{}\style{+B}StartTransaction\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ҵ���Ӧ�����ݿ����ӿ�ʼһ������',
    'StartTransaction', 'BLLCompiler', 'TBLLObj', @TBLLObj.StartTransaction);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure Commit(const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}Commit\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ҵ���Ӧ�����ݿ������ύ�������',
    'Commit', 'BLLCompiler', 'TBLLObj', @TBLLObj.Commit);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure Rollback(const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}Rollback\style{-B}(const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ҵ���Ӧ�����ݿ����ӻع�����',
    'Rollback', 'BLLCompiler', 'TBLLObj', @TBLLObj.Rollback);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure SetSQL(const ASql: string);',
    '\image{4} \column{} procedure \column{}\style{+B}SetSQL\style{-B}(const ASql: string);  \color{' + ProposalCommColor + '}// ����ҵ���Ӧ��SQL���',
    'SetSQL', 'BLLCompiler', 'TBLLObj', @TBLLObj.SetSQL);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function GetRecordCount(const AConn: Byte = 1): Integer;',
    '\image{5} \column{} function \column{}\style{+B}GetRecordCount\style{-B}(const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// ����ҵ���Ӧ��SQL���',
    'GetRecordCount', 'BLLCompiler', 'TBLLObj', @TBLLObj.GetRecordCount);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function GetParamCount(const AConn: Byte = 1): Integer;',
    '\image{5} \column{} function \column{}\style{+B}GetParamCount\style{-B}(const AConn: Byte = 1): Integer;  \color{' + ProposalCommColor + '}// ����ҵ���Ӧ��SQL���',
    'GetParamCount', 'BLLCompiler', 'TBLLObj', @TBLLObj.GetParamCount);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function GetParamName(const AIndex: Integer; const AConn: Byte = 1): string;',
    '\image{5} \column{} function \column{}\style{+B}GetParamName\style{-B}(const AIndex: Integer; const AConn: Byte = 1): string;  \color{' + ProposalCommColor + '}// ����ҵ���Ӧ��SQL���',
    'GetParamName', 'BLLCompiler', 'TBLLObj', @TBLLObj.GetParamName);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure SetParamValue(const AParam: string; const AValue: Variant; const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}SetParamValue\style{-B}(const AParam: string; const AValue: Variant; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ���ò�����ֵ',
    'SetParamValue', 'BLLCompiler', 'TBLLObj', @TBLLObj.SetParamValue);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure ParamLoadFromStream(const AParam: string; const AStream: TStream; const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}ParamLoadFromStream\style{-B}(const AParam: string; const AStream: TStream; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ��������ҵ�����ݼ�ָ���ֶε�ֵ',
    'ParamLoadFromStream', 'BLLCompiler', 'TBLLObj', @TBLLObj.ParamLoadFromStream);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsString(const AField: string; const AConn: Byte = 1): string;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsString\style{-B}(const AField: string; const AConn: Byte = 1): string;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪstring',
    'FieldAsString', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsString);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsInteger(const AField: string; const AConn: Byte = 1): Longint;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsInteger\style{-B}(const AField: string; const AConn: Byte = 1): Longint;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪLongint',
    'FieldAsInteger', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsInteger);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsBoolean(const AField: string; const AConn: Byte = 1): Boolean;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsBoolean\style{-B}(const AField: string; const AConn: Byte = 1): Boolean;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪBoolean',
    'FieldAsBoolean', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsBoolean);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsDateTime(const AField: string; const AConn: Byte = 1): TDateTime;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsDateTime\style{-B}(const AField: TDateTime; const AConn: Byte = 1): TDateTime;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪTDateTime',
    'FieldAsDateTime', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsDateTime);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsSingle(const AField: string; const AConn: Byte = 1): Single;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsSingle\style{-B}(const AField: string; const AConn: Byte = 1): Single;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪSingle',
    'FieldAsSingle', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsSingle);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsFloat(const AField: string; const AConn: Byte = 1): Double;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsFloat\style{-B}(const AField: string; const AConn: Byte = 1): Double;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪDouble',
    'FieldAsFloat', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsFloat);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function FieldAsVariant(const AField: string; const AConn: Byte = 1): Variant;',
    '\image{5} \column{} function \column{}\style{+B}FieldAsVariant\style{-B}(const AField: string; const AConn: Byte = 1): Variant;  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪVariant',
    'FieldAsVariant', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsVariant);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure FieldAsStream(const AField: string; const AStream: TStream; const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}FieldAsStream\style{-B}(const AField: string; const AStream: TStream; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// �ֶ�ֵתΪ��',
    'FieldAsStream', 'BLLCompiler', 'TBLLObj', @TBLLObj.FieldAsStream);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure SaveToStream(const AStream: TStream; const AConn: Byte = 1);',
    '\image{4} \column{} procedure \column{}\style{+B}SaveToStream\style{-B}(const AStream: TStream; const AConn: Byte = 1);  \color{' + ProposalCommColor + '}// ���ݼ�����Ϊ��',
    'SaveToStream', 'BLLCompiler', 'TBLLObj', @TBLLObj.SaveToStream);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure DebugInfoAppend(const AInfo: string);',
    '\image{4} \column{} procedure \column{}\style{+B}DebugInfoAppend\style{-B}(const AInfo: string);  \color{' + ProposalCommColor + '}// ��ӵ�����Ϣ',
    'DebugInfoAppend', 'BLLCompiler', 'TBLLObj', @TBLLObj.DebugInfoAppend);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function CreateBllObj: TBLLObj',
    '\image{5} \column{} function \column{}\style{+B}CreateBllObj\style{-B};  \color{' + ProposalCommColor + '}// �����µ�TBLLObjʵ��',
    'CreateBllObj', 'BLLCompiler', 'TBLLObj', @TBLLObj.CreateBllObj);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure FreeBLLObj(var ABLLObj: TBLLObj);',
    '\image{4} \column{} procedure \column{}\style{+B}FreeBLLObj\style{-B}(var ABLLObj: TBLLObj);  \color{' + ProposalCommColor + '}// �ͷ��µ�BLLObjʵ��',
    'FreeBLLObj', 'BLLCompiler', 'TBLLObj', @TBLLObj.FreeBLLObj);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('function Eof: Boolean;',
    '\image{5} \column{} function \column{}\style{+B}Eof\style{-B}  \color{' + ProposalCommColor + '}// �Ƿ�ָ�����һ��������',
    'Eof', 'BLLCompiler', 'TBLLObj', @TBLLObj.Eof);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure First;',
    '\image{4} \column{} procedure \column{}\style{+B}First\style{-B};  \color{' + ProposalCommColor + '}// ָ���һ������',
    'First', 'BLLCompiler', 'TBLLObj', @TBLLObj.First);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure Next;',
    '\image{4} \column{} procedure \column{}\style{+B}Next\style{-B};  \color{' + ProposalCommColor + '}// ָ����һ������',
    'Next', 'BLLCompiler', 'TBLLObj', @TBLLObj.Next);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure Last;',
    '\image{4} \column{} procedure \column{}\style{+B}Last\style{-B};  \color{' + ProposalCommColor + '}// ָ�����һ������',
    'Last', 'BLLCompiler', 'TBLLObj', @TBLLObj.Last);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);
end;

procedure TBLLObj.Commit(const AConn: Byte = 1);
begin
  BLLQuery.Connection.Commit;  // �ύ����
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

procedure TBLLObj.SaveToStream(const AStream: TStream; const AConn: Byte);
begin
  BLLQuery.SaveToStream(AStream, TFDStorageFormat.sfBinary);
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

procedure TBLLObj.SetSQL(const ASql: string; const AConn: Byte = 1);
begin
  BLLQuery.SQL.Text := ASql;
end;

procedure TBLLObj.StartTransaction(const AConn: Byte = 1);
begin
  BLLQuery.Connection.StartTransaction;  // ��ʼһ������
end;

{ TBLLMsgPack }

class procedure TBLLMsgPack.Appends(const AConverts: TCplConverts; var ATypeID: Integer);
var
  vH, i: Integer;
begin
  vH := RegisterNamespace(0, 'emr_MsgPack');  // emr_MsgPack
  ATypeID := RegisterClassType(vH, TMsgPack);  // TMsgPack

  //RegisterRTTIType(vH, TypeInfo(TMsgPackType));
  RegisterConstant(vH, 'BLL_CMD', BLL_CMD);
  RegisterConstant(vH, 'BLL_VER', BLL_VER);
  RegisterConstant(vH, 'BLL_METHODRESULT', BLL_METHODRESULT);
  RegisterConstant(vH, 'BLL_EXECPARAM', BLL_EXECPARAM);
  RegisterConstant(vH, 'BLL_BACKDATASET', BLL_BACKDATASET);
  RegisterConstant(vH, 'BLL_BACKFIELD', BLL_BACKFIELD);
  RegisterConstant(vH, 'BLL_RECORDCOUNT', BLL_RECORDCOUNT);
  RegisterConstant(vH, 'BLL_DATASET', BLL_DATASET);

  i := AConverts.New('function Path(APath: string): TMsgPack;',
    '\image{5} \column{} function \column{}\style{+B}Path\style{-B}(APath: string): TMsgPack;  \color{' + ProposalCommColor + '}  // ��ȡָ���ڵ�',
    'Path', 'emr_MsgPack', 'TMsgPack', @TMsgPack.Path);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure LoadBinaryFromStream(AStream: TStream; ALen: Cardinal = 0);',
    '\image{4} \column{} procedure \column{}\style{+B}LoadBinaryFromStream\style{-B}(AStream: TStream; ALen: Cardinal = 0);  \color{' + ProposalCommColor + '}  // ��������',
    'LoadBinaryFromStream', 'emr_MsgPack', 'TMsgPack', @TMsgPack.LoadBinaryFromStream);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure SaveBinaryToStream(AStream: TStream);',
    '\image{4} \column{} procedure \column{}\style{+B}SaveBinaryToStream\style{-B}(AStream: TStream);  \color{' + ProposalCommColor + '}  // ����Ϊ��',
    'SaveBinaryToStream', 'emr_MsgPack', 'TMsgPack', @TMsgPack.SaveBinaryToStream);
  RegisterHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  // AsInteger
  i := AConverts.New('function TMsgPack_GetAsInteger: Integer;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsInteger\style{-B}: Integer;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsInteger', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsInteger, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsInteger(const Value: Integer);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsInteger\style{-B}(const Value: Integer);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsInteger', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsInteger, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsInteger: Int64 read TMsgPack_GetAsInteger write TMsgPack_SetAsInteger;',
    '\image{3} \column{} property \column{}\style{+B}AsInteger\style{-B}: Int64;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪInteger',
    'AsInteger', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsString
  i := AConverts.New('function TMsgPack_GetAsString: string;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsString\style{-B}: string;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsString', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsString, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsString(const Value: string);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsString\style{-B}(const Value: string);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsString', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsString, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsString: string read TMsgPack_GetAsString write TMsgPack_SetAsString;',
    '\image{3} \column{} property \column{}\style{+B}AsString\style{-B}: string;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪstring',
    'AsString', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsBoolean
  i := AConverts.New('function TMsgPack_GetAsBoolean: Boolean;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsBoolean\style{-B}: Boolean;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsBoolean', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsBoolean, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsBoolean(const Value: Boolean);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsBoolean\style{-B}(const Value: Boolean);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsBoolean', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsBoolean, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsBoolean: Boolean read TMsgPack_GetAsBoolean write TMsgPack_SetAsBoolean;',
    '\image{3} \column{} property \column{}\style{+B}AsBoolean\style{-B}: Boolean;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪBoolean',
    'AsBoolean', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsDouble
  i := AConverts.New('function TMsgPack_GetAsDouble: Double;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsDouble\style{-B}: Double;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsDouble', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsDouble, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsDouble(const Value: Double);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsDouble\style{-B}(const Value: Double);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsDouble', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsDouble, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsDouble: Double read TMsgPack_GetAsDouble write TMsgPack_SetAsDouble;',
    '\image{3} \column{} property \column{}\style{+B}AsDouble\style{-B}: Double;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪDouble',
    'AsDouble', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsSingle
  i := AConverts.New('function TMsgPack_GetAsSingle: Single;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsSingle\style{-B}: Single;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsSingle', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsSingle, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsSingle(const Value: Single);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsSingle\style{-B}(const Value: Single);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsSingle', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsSingle, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsSingle: Single read TMsgPack_GetAsSingle write TMsgPack_SetAsSingle;',
    '\image{3} \column{} property \column{}\style{+B}AsSingle\style{-B}: Single;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪSingle',
    'AsSingle', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsDateTime
  i := AConverts.New('function TMsgPack_GetAsDateTime: TDateTime;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsDateTime\style{-B}: TDateTime;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsDateTime', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsDateTime, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsDateTime(const Value: TDateTime);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsDateTime\style{-B}(const Value: TDateTime);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsDateTime', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsDateTime, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsDateTime: TDateTime read TMsgPack_GetAsDateTime write TMsgPack_SetAsDateTime;',
    '\image{3} \column{} property \column{}\style{+B}AsDateTime\style{-B}: TDateTime;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪTDateTime',
    'AsDateTime', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);

  // AsVariant
  i := AConverts.New('function TMsgPack_GetAsVariant: Variant;',
    '\image{5} \column{} function \column{}\style{+B}TMsgPack_GetAsVariant\style{-B}: Variant;  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_GetAsVariant', 'emr_MsgPack', 'TMsgPack', @TMsgPack_GetAsVariant, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('procedure TMsgPack_SetAsVariant(const Value: Variant);',
    '\image{4} \column{} procedure \column{}\style{+B}TMsgPack_SetAsVariant\style{-B}(const Value: Variant);  \color{' + ProposalCommColor + '}  // ',
    'TMsgPack_SetAsVariant', 'emr_MsgPack', 'TMsgPack', @TMsgPack_SetAsVariant, True);
  RegisterFakeHeader(ATypeID, AConverts[i].FullName, AConverts[i].Address);

  i := AConverts.New('property AsVariant: Variant read TMsgPack_GetAsVariant write TMsgPack_SetAsVariant;',
    '\image{3} \column{} property \column{}\style{+B}AsVariant\style{-B}: Variant;  \color{' + ProposalCommColor + '}  // �ڵ�����תΪVariant',
    'AsVariant', 'emr_MsgPack', 'TMsgPack', nil);
  RegisterProperty(ATypeID, AConverts[i].FullName);
end;

end.
