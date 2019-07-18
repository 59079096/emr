////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_JavaScript.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
{$Q-}
{$B-}
{$R-}
unit PAXCOMP_JavaScript;
interface

{$IFNDEF LINUX}
{$IFDEF UNIX} // {just to compile PASCAL ONLY!}

function GetVariantValue(Address: Pointer; FinTypeId: Integer): Variant;
procedure PutVariantValue(Address: Pointer; FinTypeId: Integer; const value: Variant);

implementation
Uses
   Variants,
   PAXCOMP_SYS,
   PAXCOMP_CONSTANTS;

procedure _VariantFromClass(Dest: PVariant;
                            SourceAddress: Pointer); stdcall;
begin
  VarClear(dest^);
  with TVarData(dest^) do
  begin
    VType := varClass;
    VInteger := IntPax(SourceAddress^);
  end;
end;

procedure _ClassFromVariant(DestAddress: Pointer;
                            Source: PVariant); stdcall;
var
  V: Variant;
begin
  if TVarData(source^).VType = varClass then
  begin
    TObject(DestAddress^) := TObject(TVarData(source^).VInteger);
    (*if TObject(DestAddress^) is TJS_Reference then
    begin
      V := TJS_Reference(DestAddress^).GetValue();
      if TVarData(V).VType = varClass then
        TObject(DestAddress^) := TObject(TVarData(V).VInteger)
      else
       TObject(DestAddress^) := nil;
    end; *)
  end
  else
    TObject(DestAddress^) := nil;
end;

function GetVariantValue(Address: Pointer; FinTypeId: Integer): Variant;
begin
  case FinTypeId of
    typeBOOLEAN: result := Boolean(Address^);
    typeBYTE: result := Byte(Address^);
    typeWORD: result := Word(Address^);
    typeINTEGER: result := Integer(Address^);
    typeDOUBLE: result := Double(Address^);
    typePOINTER: result := Integer(Address^);
    typeENUM: result := Byte(Address^);
    typePROC: result := Integer(Address^);
{$IFNDEF PAXARM}
    typeANSICHAR: result := AnsiChar(Address^);
    typeANSISTRING: result := AnsiString(Address^);
    typeSHORTSTRING: result := ShortString(Address^);
    typeWIDESTRING: result := WideString(Address^);
{$ENDIF}
    typeSINGLE: result := Single(Address^);
    typeEXTENDED: result := Extended(Address^);
    typeCLASS:
    begin
      _VariantFromClass(@result, Address);
    end;
    typeCLASSREF: result := Integer(Address^);
    typeWIDECHAR: result := WideChar(Address^);
    typeVARIANT: result := Variant(Address^);
    typeDYNARRAY: result := Integer(Address^);
{$IFDEF VARIANTS}
    typeEVENT: result := Int64(Address^);
    typeINT64: result := Int64(Address^);
{$ELSE}
    typeINT64: result := Integer(Address^);
{$ENDIF}
    typeINTERFACE: result := Integer(Address^);
    typeCARDINAL: result := Cardinal(Address^);
    typeCURRENCY: result := Currency(Address^);
    typeSMALLINT: result := SmallInt(Address^);
    typeSHORTINT: result := ShortInt(Address^);
    typeWORDBOOL: result := WordBool(Address^);
    typeLONGBOOL: result := LongBool(Address^);
    typeBYTEBOOL: result := ByteBool(Address^);
    typeOLEVARIANT: result := OleVariant(Address^);
    typeUNICSTRING: result := UnicString(Address^);
  end;
end;

procedure PutVariantValue(Address: Pointer; FinTypeId: Integer; const value: Variant);
var
  X, Y: TObject;
begin
  case FinTypeId of
    typeBOOLEAN: Boolean(Address^) := value;
    typeBYTE: Byte(Address^) := value;
    typeWORD: Word(Address^) := value;
    typeINTEGER: Integer(Address^) := value;
    typeDOUBLE: Double(Address^) := value;
    typePOINTER: Integer(Address^) := value;
    typeENUM: Byte(Address^) := value;
    typePROC: Integer(Address^) := value;
{$IFNDEF PAXARM}
    typeSHORTSTRING: ShortString(Address^) := ShortString(value);
    typeANSICHAR: AnsiChar(Address^) := AnsiChar(Byte(value));
    typeANSISTRING: AnsiString(Address^) := AnsiString(value);
    typeWIDESTRING: WideString(Address^) := value;
{$ENDIF}
    typeSINGLE: Single(Address^) := value;
    typeEXTENDED: Extended(Address^) := value;
    typeCLASS:
    begin
      X := TObject(Address^);
      _ClassFromVariant(@Y, @value);
      if Y = nil then
      begin
        if X = nil then
          Exit;
        //if X is TJS_Object then
        //else
          TObject(Address^) := nil;
        Exit;
      end;
      //if (X is TJS_Object) and (Y is TGC_Object) then
      //  GC_Assign(PGC_Object(Address), TGC_Object(Y))
      //else
        TObject(Address^) := Y;
    end;
    typeCLASSREF: Integer(Address^) := value;
    typeWIDECHAR: WideChar(Address^) := WideChar(Word(value));
    typeVARIANT: Variant(Address^) := value;
    typeDYNARRAY: Integer(Address^) := value;
{$IFDEF VARIANTS}
    typeINT64: Int64(Address^) := value;
    typeEVENT: Int64(Address^) := value;
{$ELSE}
    typeINT64: Integer(Address^) := value;
{$ENDIF}
    typeINTERFACE: Integer(Address^) := value;
    typeCARDINAL: Cardinal(Address^) := value;
    typeCURRENCY: Currency(Address^) := value;
    typeSMALLINT: SmallInt(Address^) := value;
    typeSHORTINT: ShortInt(Address^) := value;
    typeWORDBOOL: WordBool(Address^) := value;
    typeLONGBOOL: LongBool(Address^) := value;
{$IFDEF FPC}
    typeBYTEBOOL:
    if value <> 0 then
      ByteBool(Address^) := true
    else
      ByteBool(Address^) := false;
{$ELSE}
    typeBYTEBOOL: ByteBool(Address^) := value;
{$ENDIF}
    typeOLEVARIANT: OleVariant(Address^) := value;
    typeUNICSTRING: UnicString(Address^) := value;
  end;
end;
end.
{$ENDIF} // ndef linux
{$ENDIF} // ndef unix

uses {$I uses.def}
  SysUtils,
  Classes,
  Math,
  RegExpr2,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_STDLIB,
  PAXCOMP_GC,
  PAXCOMP_BASERUNNER,
  PAXCOMP_BASESYMBOL_TABLE;

const
  Delta = 100;

  TYP_JS_OBJECT = 1;
  TYP_JS_DATE = 2;
  TYP_JS_ARRAY = 3;
  TYP_JS_BOOLEAN = 4;
  TYP_JS_NUMBER = 5;
  TYP_JS_STRING = 6;
  TYP_JS_FUNCTION = 7;
  TYP_JS_MATH = 8;
  TYP_JS_REGEXP = 9;
  TYP_JS_ERROR = 10;

  MaxPrimes = 2;
  Primes: array[1..MaxPrimes] of Integer = (103, 199);

var
  MaxArgs: Variant;
{$IFDEF PAX64}
  RetAdr_OFFSET: IntPax = 0;
  ParArr_OFFSET: IntPax = 0;
{$ENDIF}

type
  TJS_Object = class;

  THashFunction = function (S : PChar; TableSize: Integer) : longint;

  TJS_PropRec = class
  public
    Key: PChar;
    Value: Variant;
  end;

  THashTable = class
  private
    PrimeIndex: Integer;
    MaxCard: Integer;
    A: array of TJS_PropRec;
  public
    HashFunction: THashFunction;
    TableSize: Integer;
    LastIndex: Integer;
    Card: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(const S: PChar; var I: Integer): Boolean;
  end;

  TJS_PropList = class
  private
    Owner: TJS_Object;
    HashTable: THashTable;
    Arr: array of Variant;
    AvailArrIndex: array of boolean;
    CardArr: Integer;
    LastPropAddress: PVariant;
  public
    constructor Create(i_Owner: TJS_Object);
    destructor Destroy; override;
    procedure Clear;

    function IndexOfProperty(PropName: PChar; var I: Integer;
      var PositiveInt: Boolean): Boolean;

    procedure SetArrLength(N: Integer);
    function GetArrProperty(PropName: Integer): PVariant;
    procedure PutArrProperty(PropName: Integer; const Value: Variant);

    function GetProperty(PropName: PChar): PVariant;
    procedure PutProperty(PropName: PChar; const Value: Variant);
    function HasProperty(PropName: PChar): Boolean;
  end;

  TJS_ObjectBase = class(TGC_Object)
  public
    function GetGC: TGC; override;
  end;

  TJS_Object = class(TJS_ObjectBase)
  private
    L: TJS_PropList;
    fLength: Integer;
    fDefaultValue: Variant;
    aconstructor: TJS_Object;
    Tag: Integer;
    NextPropIndex: Integer;
  public
    typ: Integer;
    prototype: TJS_Object;
    prog: TBaseRunner;
    function GetConstructor: TJS_Object;
    function HasProperty(PropName: PChar): Boolean;
    procedure PutProperty(PropName: PChar; const Value: Variant);
    function GetProperty(PropName: PChar): Variant;
    function GetPropertyAsObject(PropName: PChar): TJS_Object;
    procedure PutArrProperty(PropName: Integer; const Value: Variant);
    function GetArrProperty(PropName: Integer): Variant;
    function GetVarProperty(const PropName: Variant): Variant;
    procedure PutVarProperty(const PropName: Variant; const Value: Variant);
    procedure AddToGC;
    constructor Create;
    destructor Destroy; override;
    function GetGC: TGC; override;
    function __toString: String; override;
    property Prop[PropertyName: PChar]: Variant read GetProperty
                                                write PutProperty; default;
  end;

  TJS_Reference = class(TJS_ObjectBase)
  public
    Address: Pointer;
    FinTypeId: Integer;
    constructor Create(AFinTypeId: Integer);
    function GetValue(): Variant;
    function GetValueAsObject(): TJS_Object;
    procedure PutValue(const value: Variant);
    function __toString: String; override;
  end;

  TJS_Date = class(TJS_Object)
  private
    DelphiDate: TDateTime;
    function GetValue: Variant;
    function UTCDelphiDate: TDateTime;
    function DelphiDateFromUTCDate(D: TDateTime): TDateTime;
  public
    property Value: Variant read GetValue;
    constructor Create(Year: PVariant = nil;
                       Month: PVariant = nil;
                       Day: PVariant = nil;
                       Hours: PVariant = nil;
                       Minutes: PVariant = nil;
                       Seconds: PVariant = nil;
                       Ms: PVariant = nil);
    function toGMTString: Variant; stdcall;
    function getTime: Variant; stdcall;
    function getFullYear: Variant; stdcall;
    function getUTCFullYear: Variant; stdcall;
    function getMonth: Variant; stdcall;
    function getUTCMonth: Variant; stdcall;
    function getDate: Variant; stdcall;
    function getUTCDate: Variant; stdcall;
    function getDay: Variant; stdcall;
    function getUTCDay: Variant; stdcall;
    function getHours: Variant; stdcall;
    function getUTCHours: Variant; stdcall;
    function getMinutes: Variant; stdcall;
    function getUTCMinutes: Variant; stdcall;
    function getSeconds: Variant; stdcall;
    function getUTCSeconds: Variant; stdcall;
    function getMilliseconds: Variant; stdcall;
    function getUTCMilliseconds: Variant; stdcall;
    function setTime(const P: Variant): Variant; stdcall;
    function setMilliseconds(const ms: Variant): Variant; stdcall;
    function setUTCMilliseconds(const ms: Variant): Variant; stdcall;
    function setSeconds(const sec, ms: Variant): Variant; stdcall;
    function setUTCSeconds(const sec, ms: Variant): Variant; stdcall;
    function setMinutes(const min, sec, ms: Variant): Variant; stdcall;
    function setUTCMinutes(const min, sec, ms: Variant): Variant; stdcall;
    function setHours(const hour, min, sec, ms: Variant): Variant; stdcall;

    function setUTCHours(const hour, min, sec, ms: Variant): Variant; stdcall;
    function setDate(const date: Variant): Variant; stdcall;
    function _toString: Variant; stdcall;
    function __toString: String; override;
  end;

  TJS_Array = class(TJS_Object)
  private
    function GetLength: Integer;
    procedure SetLength(value: Integer);
  public
    constructor Create(const V: array of Variant);
    destructor Destroy; override;
    function _toString: Variant; stdcall;
    function __toString: String; override;
    function _pop: Variant; stdcall;
    function _push(P0: PVariant;
                   P1: PVariant = nil;
                   P2: PVariant = nil;
                   P3: PVariant = nil;
                   P4: PVariant = nil;
                   P5: PVariant = nil;
                   P6: PVariant = nil;
                   P7: PVariant = nil;
                   P8: PVariant = nil;
                   P9: PVariant = nil): Variant; stdcall;
    property Length: Integer read GetLength write SetLength;
  end;

  TJS_Boolean = class(TJS_Object)
    constructor Create(P: PVariant = nil);
    function _toString: Variant; stdcall;
    function __toString: String; override;
  end;

  TJS_Number = class(TJS_Object)
    constructor Create(P: PVariant = nil);
    function _toString(): Variant; stdcall;
    function __toString: String; override;
  end;

  TJS_String = class(TJS_Object)
    constructor Create(P: PVariant = nil);
    function _toString: Variant; stdcall;
    function __toString: String; override;
    function _valueOf: Variant; stdcall;
    function _length: Variant; stdcall;
    function _charAt(const P: Variant): Variant; stdcall;
    function _charCodeAt(const P: Variant): Variant; stdcall;
    function _concat(P0: PVariant;
                     P1: PVariant = nil;
                     P2: PVariant = nil;
                     P3: PVariant = nil;
                     P4: PVariant = nil;
                     P5: PVariant = nil;
                     P6: PVariant = nil;
                     P7: PVariant = nil;
                     P8: PVariant = nil;
                     P9: PVariant = nil): Variant; stdcall;
    function _fromCharCode(P0: PVariant;
                     P1: PVariant = nil;
                     P2: PVariant = nil;
                     P3: PVariant = nil;
                     P4: PVariant = nil;
                     P5: PVariant = nil;
                     P6: PVariant = nil;
                     P7: PVariant = nil;
                     P8: PVariant = nil;
                     P9: PVariant = nil): Variant; stdcall;
    function _slice(const VStart, VEnd: Variant): Variant; stdcall;
    function _substr(const VStart, VLength: Variant): Variant; stdcall;
    function _substring(const VStart, VLength: Variant): Variant; stdcall;
    function _indexOf(const P: Variant): Variant; stdcall;
    function _lastIndexOf(const P: Variant): Variant; stdcall;
    function _anchor(const P: Variant): Variant; stdcall;
    function _link(const P: Variant): Variant; stdcall;
    function _big: Variant; stdcall;
    function _small: Variant; stdcall;
    function _blink: Variant; stdcall;
    function _bold: Variant; stdcall;
    function _italics: Variant; stdcall;
    function _strike: Variant; stdcall;
    function _sub: Variant; stdcall;
    function _sup: Variant; stdcall;
    function _fixed: Variant; stdcall;
    function _fontcolor(const P: Variant): Variant; stdcall;
    function _fontsize(const P: Variant): Variant; stdcall;
    function _toUpperCase: Variant; stdcall;
    function _toLowerCase: Variant; stdcall;
    function _replace(const SearchValue, ReplaceValue: Variant): Variant; stdcall;
  end;

  TJS_Function = class(TJS_Object)
  private
//    DataPtr: Pointer;
//    CodePtr: Pointer;
    CoolCall: Integer;
    DefaultNP: Integer;
  public
    InternalFuncAddr: Pointer;
    arguments: TJS_Array;
    InternalLength: Integer;
    __this: TObject;
{$IFDEF PAX64}
  private
    ParArr: Pointer;
    RetAdr: Pointer;
    procedure InternalCall2(NP: Integer);
  public
{$ENDIF}
    constructor InternalCreate(i_InternalFuncAddr: Pointer;
                               i_NP: Integer;
                               i_ProgPtr: Pointer);
    destructor Destroy; override;
    function InternalCall(NP: Integer): Variant; stdcall;
    function Invoke(const Params: array of Variant): Variant; stdcall;
    function _toString: Variant; stdcall;
    function __toString: String; override;
  end;

  TJS_Math = class(TJS_Object)
  public
    constructor Create;
    function _abs(const P: Variant): Variant; stdcall;
    function _acos(const P: Variant): Variant; stdcall;
    function _asin(const P: Variant): Variant; stdcall;
    function _atan(const P: Variant): Variant; stdcall;
    function _atan2(const X, Y: Variant): Variant; stdcall;
    function _ceil(const P: Variant): Variant; stdcall;
    function _cos(const P: Variant): Variant; stdcall;
    function _exp(const P: Variant): Variant; stdcall;
    function _floor(const P: Variant): Variant; stdcall;
    function _log(const P: Variant): Variant; stdcall;
    function _max(P1, P2, P3, P4, P5: PVariant): Variant; stdcall;
    function _min(P1, P2, P3, P4, P5: PVariant): Variant; stdcall;
    function _pow(const X, Y: Variant): Variant; stdcall;
    function _random: Variant; stdcall;
    function _round(const P: Variant): Variant; stdcall;
    function _sin(const P: Variant): Variant; stdcall;
    function _sqrt(const P: Variant): Variant; stdcall;
    function _tan(const P: Variant): Variant; stdcall;
  end;

  TJS_RegExp = class(TJS_Object)
  private
    fLastIndex: Integer;
{$IFNDEF PAXARM}
    fRegExpr: TRegExpr;
    fZERO_BASED_STRINGS: Boolean;
{$ENDIF}
    function GetMatch(I: Integer): String;
    function GetMatchLen(I: Integer): Integer;
    function GetMatchPos(I: Integer): Integer;
    function GetSource: Variant;
    procedure SetSource(const Value: Variant);
    function GetInput: Variant;
    procedure SetInput(const Value: Variant);
    function GetGlobal: Boolean;
    procedure SetGlobal(const Value: Boolean);
    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);
    function GetMultiLine: Boolean;
    procedure SetMultiLine(const Value: Boolean);
  public
    constructor Create(Source: PVariant = nil; Modifiers: PVariant = nil);
    destructor Destroy; override;
    function Test(const InputString: Variant): Boolean;
    procedure Compile;
    function MatchCount: Integer;
    function Exec(const InputString: Variant): TJS_Array;
    function Execute(const InputString: Variant): TJS_Array;
    function Replace(const Expression, ReplaceStr: Variant): String;
    function _toString: Variant; stdcall;
    function __toString: String; override;
  published
    property global: Boolean read GetGlobal write SetGlobal;
    property ignoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property multiLine: Boolean read GetMultiLine write SetMultiLine;
    property lastIndex: Integer read fLastIndex write fLastIndex;
    property source: Variant read GetSource write SetSource;
    property input: Variant read GetInput write SetInput;
  end;

  TJS_Error = class(TJS_Object)
    constructor Create(P: PVariant = nil);
    function _toString: Variant; stdcall;
    function __toString: String; override;
  end;

procedure Register_StdJavaScript(st: TBaseSymbolTable);
function IsDateObject(const V: Variant): Boolean;
function VariantToDateObject(const V: Variant): TJS_Date;
function JS_IsObject(const V: Variant): Boolean;
function JS_IsPointer(const V: Variant): Boolean;
function JS_IsRef(const V: Variant): Boolean;
function JS_IsString(const V: Variant): Boolean;
function JS_IsBoolean(const V: Variant): Boolean;
function JS_IsUndefined(const V: Variant): Boolean;
function JS_GetValue(const V: Variant): Variant;
procedure JS_PutValue(const V: Variant; const value: Variant);
function JS_ToPrimitive(const V: Variant): Variant;
function JS_ToString(const V: Variant): Variant;
function JS_ToBoolean(const V: Variant): Variant;
function JS_ToNumber(const V: Variant): Variant;
function JS_ToNumberE(const V: Variant): Extended;
function JS_ToInt32(const V: Variant): Variant;
function JS_IsSimpleNumber(const V: Variant): Boolean;
function JS_IsNumber(const V: Variant): Boolean;
function JS_RelationalComparison(const V1, V2: Variant): Variant;
                                //performs x < y comparison
function JS_EqualityComparison(const V1, V2: Variant): Boolean;

procedure _VariantFromClass(Dest: PVariant;
                            SourceAddress: Pointer); stdcall;
procedure _VariantFromPointer(Dest: PVariant;
                              SourceAddress: Pointer); stdcall;
procedure _ClassFromVariant(DestAddress: Pointer;
                            Source: PVariant); stdcall;
function GetVariantValue(Address: Pointer; FinTypeId: Integer): Variant;
procedure PutVariantValue(Address: Pointer; FinTypeId: Integer; const value: Variant);
procedure _JS_ToObject(P:TBaseRunner;
                       Address: Pointer;
                       FinTypeId: Integer;
                       result: PVariant); stdcall;
procedure _AssignProg(X: TJS_Object; P: TBaseRunner); stdcall;
procedure _JS_GetNextProp(VObject: PVariant;
                          Prop: PString;
                          result: PBoolean); stdcall;
procedure _ClearReferences(P: TBaseRunner); stdcall;
procedure _ClassClr(Address: Pointer); stdcall;
procedure _FuncObjFromVariant(source: PVariant; DestAddress: Pointer); stdcall;
procedure _JS_TypeOf(V: PVariant;
                     result: PString); stdcall;
procedure _PushContext(P: TBaseRunner; value: PVariant); stdcall;
procedure _PopContext(P: TBaseRunner); stdcall;
procedure _FindContext(P: TBaseRunner; PropName: PChar;
                       AltAddress: Pointer;
                       FinTypeId: Integer;
                       result: PVariant); stdcall;
procedure _FindFunc(P: TBaseRunner; PropName: PChar;
                    Alt, result: PVariant); stdcall;

{$IFNDEF PAXARM}
procedure _VariantFromPAnsiChar(source: PAnsiChar; dest: PVariant); stdcall;
procedure _VariantFromAnsiString(Dest: PVariant; Source: PAnsiString); stdcall;
procedure _VariantFromWideString(Dest: PVariant; Source: PWideString); stdcall;
procedure _VariantFromAnsiChar(source: AnsiChar; dest: PVariant); stdcall;
{$ENDIF}
procedure _VariantFromPWideChar(source: PWideChar; dest: PVariant); stdcall;
procedure _VariantFromInterface(const source: IDispatch; dest: PVariant); stdcall;
procedure _VariantFromShortString(Dest: PVariant; Source: PShortString); stdcall;
procedure _VariantFromUnicString(Dest: PVariant; Source: PUnicString); stdcall;
procedure _VariantFromWideChar(source: WideChar; dest: PVariant); stdcall;
procedure _VariantFromInt(source: Integer; dest: PVariant); stdcall;
procedure _VariantFromInt64(dest: PVariant; source: PInt64); stdcall;
procedure _VariantFromByte(source: Byte; dest: PVariant); stdcall;
procedure _VariantFromBool(source: Boolean; dest: PVariant); stdcall;
procedure _VariantFromWord(source: Word; dest: PVariant); stdcall;
procedure _VariantFromCardinal(source: Cardinal; dest: PVariant); stdcall;
procedure _VariantFromSmallInt(source: SmallInt; dest: PVariant); stdcall;
procedure _VariantFromShortInt(source: ShortInt; dest: PVariant); stdcall;
procedure _VariantFromDouble(dest: PVariant; source: PDouble); stdcall;
procedure _VariantFromCurrency(dest: PVariant; source: PCurrency); stdcall;
procedure _VariantFromSingle(dest: PVariant; source: PSingle); stdcall;
procedure _VariantFromExtended(dest: PVariant; source: PExtended); stdcall;
procedure _VariantAssign(dest, source: PVariant); stdcall;
procedure _VariantAddition(Language: Integer;
                           v1, v2, dest: PVariant); stdcall;
procedure _VariantSubtraction(Language: Integer;
                              v1, v2, dest: PVariant); stdcall;
procedure _VariantMultiplication(Language: Integer;
                                 v1, v2, dest: PVariant); stdcall;
procedure _VariantDivision(Language: Integer;
                           v1, v2, dest: PVariant); stdcall;
procedure _VariantIDivision(Language: Integer;
                            v1, v2, dest: PVariant); stdcall;
procedure _VariantModulo(Language: Integer;
                         v1, v2, dest: PVariant); stdcall;
procedure _VariantLeftShift(Language: Integer;
                            v1, v2, dest: PVariant); stdcall;
procedure _VariantRightShift(Language: Integer;
                             v1, v2, dest: PVariant); stdcall;
procedure _VariantAnd(Language: Integer;
                      v1, v2, dest: PVariant); stdcall;
procedure _VariantOr(Language: Integer;
                     v1, v2, dest: PVariant); stdcall;
procedure _VariantXor(Language: Integer;
                      v1, v2, dest: PVariant); stdcall;
procedure _VariantLessThan(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _VariantLessThanOrEqual(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _VariantGreaterThan(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _VariantGreaterThanOrEqual(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _VariantEquality(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _VariantNotEquality(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
procedure _ClassAssign(dest, source: PObject); stdcall;
procedure _alert(Prog: TBaseRunner;
                 P1: PVariant;
                 P2: PVariant = nil;
                 P3: PVariant = nil;
                 P4: PVariant = nil;
                 P5: PVariant = nil); stdcall;

var
  VarIntTypes: set of byte;

implementation

{$IFDEF PAX64}
procedure Push_And_Call(NP: Integer; Instance, Params, RetAdr: Pointer); forward;
procedure AssignRBX(P: Pointer); forward;
{$ENDIF}

const
  varEmpty    = $0000; { vt_empty        0 }
  varNull     = $0001; { vt_null         1 }
  varSmallint = $0002; { vt_i2           2 }
  varInteger  = $0003; { vt_i4           3 }
  varSingle   = $0004; { vt_r4           4 }
  varDouble   = $0005; { vt_r8           5 }
  varCurrency = $0006; { vt_cy           6 }
  varDate     = $0007; { vt_date         7 }
  varOleStr   = $0008; { vt_bstr         8 }
  varDispatch = $0009; { vt_dispatch     9 }
  varError    = $000A; { vt_error       10 }
  varBoolean  = $000B; { vt_bool        11 }
  varVariant  = $000C; { vt_variant     12 }
  varUnknown  = $000D; { vt_unknown     13 }
//varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
  varShortInt = $0010; { vt_i1          16 }
  varByte     = $0011; { vt_ui1         17 }
  varWord     = $0012; { vt_ui2         18 }
  varLongWord = $0013; { vt_ui4         19 }
  varInt64    = $0014; { vt_i8          20 }
//varWord64   = $0015; { vt_ui8         21 } {UNSUPPORTED as of v6.x code base}
{  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

  varStrArg   = $0048; { vt_clsid       72 }
  varString   = $0100; { Pascal string 256 } {not OLE compatible }
  varAny      = $0101; { Corba any     257 } {not OLE compatible }
  varUString  = $0102; { Unicode string 258 } {not OLE compatible}

//
  varUndefined = varEmpty;

var
  Undefined: Variant;

var
  EmptyFunction: TJS_Function;

procedure RaiseError(const Message: string; params: array of Const);
begin
  raise PaxCompilerException.Create(Format(Message, params));
end;

{$IFNDEF VARIANTS}

function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;

function IsNan(const AValue: Single): Boolean; overload;
begin
  Result := ((PLongWord(@AValue)^ and $7F800000)  = $7F800000) and
            ((PLongWord(@AValue)^ and $007FFFFF) <> $00000000);
end;

function IsNan(const AValue: Double): Boolean; overload;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function IsNan(const AValue: Extended): Boolean; overload;
type
  TExtented = packed record
    Mantissa: Int64;
    Exponent: Word;
  end;
  PExtended = ^TExtented;
begin
  Result := ((PExtended(@AValue)^.Exponent and $7FFF)  = $7FFF) and
            ((PExtended(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0);
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;
{$ENDIF}

function VarFromClass(Source: TJS_ObjectBase): Variant;
begin
  TVarData(result).VInteger := Integer(Source);
  TVarData(result).VType    := varClass;
end;

procedure Create_DateObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_Date := TJS_Date.Create;
  P.SetAddress(P.GetOffset(R.H_JS_Date), @P.JS_Date);

  X := P.JS_Date as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Date.Create;
  with X.prototype do
  begin
    aconstructor := X;
    prog := P;
    AddToGC;

    PutProperty('toString', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date._toString, 0, @P)));

    PutProperty('toGMTString', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.toGMTString, 0, @P)));

    PutProperty('getTime', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getTime, 0, @P)));

    PutProperty('getFullYear', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getFullYear, 0, @P)));

    PutProperty('getUTCFullYear', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCFullYear, 0, @P)));

    PutProperty('getMonth', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getMonth, 0, @P)));

    PutProperty('getUTCMonth', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCMonth, 0, @P)));

    PutProperty('getDate', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getDate, 0, @P)));

    PutProperty('getUTCDate', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCDate, 0, @P)));

    PutProperty('getDay', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getDay, 0, @P)));

    PutProperty('getUTCDay', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCDay, 0, @P)));

    PutProperty('getHours', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getHours, 0, @P)));

    PutProperty('getUTCHours', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCHours, 0, @P)));

    PutProperty('getMinutes', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getMinutes, 0, @P)));

    PutProperty('getUTCMinutes', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCMinutes, 0, @P)));

    PutProperty('getSeconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getSeconds, 0, @P)));

    PutProperty('getUTCSeconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCSeconds, 0, @P)));

    PutProperty('getMilliseconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getMilliseconds, 0, @P)));

    PutProperty('getUTCMilliseconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.getUTCMilliseconds, 0, @P)));

    PutProperty('setTime', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setTime, 1, @P)));

    PutProperty('setMilliseconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setMilliseconds, 1, @P)));

    PutProperty('setUTCMilliseconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setUTCMilliseconds, 1, @P)));

    PutProperty('setSeconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setSeconds, 2, @P)));

    PutProperty('setUTCSeconds', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setUTCSeconds, 2, @P)));

    PutProperty('setMinutes', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setMinutes, 3, @P)));

    PutProperty('setUTCMinutes', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setUTCSeconds, 3, @P)));

    PutProperty('setHours', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setHours, 4, @P)));

    PutProperty('setUTCHours', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Date.setUTCHours, 4, @P)));
  end;
end;

procedure Create_BooleanObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_Boolean := TJS_Boolean.Create;
  X := P.JS_Boolean as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Boolean), @P.JS_Boolean);
end;

procedure Create_ErrorObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_Error := TJS_Error.Create;
  X := P.JS_Error as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Error), @P.JS_Error);
end;

procedure Create_NumberObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_Number := TJS_Number.Create;
  X := P.JS_Number as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Number), @P.JS_Number);

  with X.prototype do
  begin
    aconstructor := X;
    prog := P;

    PutProperty('toString', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Function._toString, 0, @P)));
  end;
end;

procedure Create_StringObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
  F: TJS_Function;
begin
  P.JS_String := TJS_String.Create;
  X := P.JS_String as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_String), @P.JS_String);

  with X.prototype do
  begin
    aconstructor := X;
    prog := P;

    PutProperty('toString', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._toString, 0, @P)));
    PutProperty('valueOf', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._valueOf, 0, @P)));
    PutProperty('charAt', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._charAt, 1, @P)));
    PutProperty('charCodeAt', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._charCodeAt, 1, @P)));

    F := TJS_Function.InternalCreate(@TJS_String._concat, 1, @P);
    F.DefaultNP := 10;
    PutProperty('concat', VarFromClass(F));

    F := TJS_Function.InternalCreate(@TJS_String._fromCharCode, 1, @P);
    F.DefaultNP := 10;
    PutProperty('fromCharCode', VarFromClass(F));

    PutProperty('length', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._length, 0, @P)));
    PutProperty('indexOf', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._indexOf, 1, @P)));
    PutProperty('lastIndexOf', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._lastIndexOf, 1, @P)));
    PutProperty('slice', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._slice, 2, @P)));
    PutProperty('substr', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._substr, 2, @P)));
    PutProperty('substring', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._substring, 2, @P)));
    PutProperty('anchor', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._anchor, 1, @P)));
    PutProperty('link', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._link, 1, @P)));
    PutProperty('big', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._big, 0, @P)));
    PutProperty('small', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._small, 0, @P)));
    PutProperty('blink', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._blink, 0, @P)));
    PutProperty('bold', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._bold, 0, @P)));
    PutProperty('italics', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._italics, 0, @P)));
    PutProperty('strike', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._strike, 0, @P)));
    PutProperty('sub', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._sub, 0, @P)));
    PutProperty('sup', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._sup, 0, @P)));
    PutProperty('fixed', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._fixed, 0, @P)));
    PutProperty('fontcolor', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._fontcolor, 1, @P)));
    PutProperty('fontsize', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._fontsize, 1, @P)));
    PutProperty('toUpperCase', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._toUpperCase, 0, @P)));
    PutProperty('toLowerCase', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._toLowerCase, 0, @P)));
    PutProperty('replace', VarFromClass(
      TJS_Function.InternalCreate(@TJS_String._replace, 2, @P)));
  end;
end;

procedure Create_ArrayObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
  F: TJS_Function;
begin
  P.JS_Array := TJS_Array.Create([]);
  X := P.JS_Array as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Array), @P.JS_Array);

  with X.prototype do
  begin
    aconstructor := X;
    prog := P;

    PutProperty('pop', VarFromClass(
      TJS_Function.InternalCreate(@TJS_Array._pop, 0, @P)));

    F := TJS_Function.InternalCreate(@TJS_Array._push, 1, @P);
    F.DefaultNP := 10;
    PutProperty('push', VarFromClass(F));
  end;
end;

procedure Create_RegExpObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_RegExp := TJS_RegExp.Create;
  X := P.JS_RegExp as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_RegExp), @P.JS_RegExp);

  with X.prototype do
  begin
    aconstructor := X;
    prog := P;

    PutProperty('toString', VarFromClass(
      TJS_Function.InternalCreate(@TJS_RegExp._toString, 0, @P)));
  end;
end;

procedure Create_FunctionObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
begin
  P.JS_Function := TJS_Function.Create;
  X := P.JS_Function as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Function), @P.JS_Function);
end;

var
  JS_MATH_ABS: Integer = -1;
  JS_MATH_ACOS: Integer = -1;
  JS_MATH_ASIN: Integer = -1;
  JS_MATH_ATAN: Integer = -1;
  JS_MATH_ATAN2: Integer = -1;
  JS_MATH_CEIL: Integer = -1;
  JS_MATH_COS: Integer = -1;
  JS_MATH_EXP: Integer = -1;
  JS_MATH_FLOOR: Integer = -1;
  JS_MATH_LOG: Integer = -1;
  JS_MATH_MAX: Integer = -1;
  JS_MATH_MIN: Integer = -1;
  JS_MATH_POW: Integer = -1;
  JS_MATH_RANDOM: Integer = -1;
  JS_MATH_ROUND: Integer = -1;
  JS_MATH_SIN: Integer = -1;
  JS_MATH_SQRT: Integer = -1;
  JS_MATH_TAN: Integer = -1;

  JS_MATH_PI: Integer = -1;
  JS_MATH_E: Integer = -1;
  JS_MATH_LN10: Integer = -1;
  JS_MATH_LN2: Integer = -1;
  JS_MATH_LOG2E: Integer = -1;
  JS_MATH_LOG10E: Integer = -1;
  JS_MATH_SQRT1_2: Integer = -1;
  JS_MATH_SQRT2: Integer = -1;

procedure Create_MathObject(P: TBaseRunner; R: TJS_Record);
var
  X: TJS_Object;
  F: TJS_Function;
begin
  P.JS_Math := TJS_Math.Create;
  P.SetAddress(P.GetOffset(R.H_JS_Math), @P.JS_Math);

  X := P.JS_Math as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  with X do
  begin
    F := TJS_Function.InternalCreate(@TJS_Math._abs, 1, @P);
    F.CoolCall := 1;
    PutProperty('abs', VarFromClass(F)); //0
    JS_MATH_ABS := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._acos, 1, @P);
    F.CoolCall := 1;
    PutProperty('acos', VarFromClass(F)); //1
    JS_MATH_ACOS := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._asin, 1, @P);
    F.CoolCall := 1;
    PutProperty('asin', VarFromClass(F)); //2
    JS_MATH_ASIN := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._atan, 1, @P);
    F.CoolCall := 1;
    PutProperty('atan', VarFromClass(F)); //3
    JS_MATH_ATAN := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._atan2, 2, @P);
    F.CoolCall := 1;
    PutProperty('atan2', VarFromClass(F)); //4
    JS_MATH_ATAN2 := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._ceil, 1, @P);
    F.CoolCall := 1;
    PutProperty('ceil', VarFromClass(F)); //5
    JS_MATH_CEIL := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._cos, 1, @P);
    F.CoolCall := 1;
    PutProperty('cos', VarFromClass(F)); //6
    JS_MATH_COS := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._exp, 1, @P);
    F.CoolCall := 1;
    PutProperty('exp', VarFromClass(F)); //7
    JS_MATH_EXP := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._floor, 1, @P);
    F.CoolCall := 1;
    PutProperty('floor', VarFromClass(F)); //8
    JS_MATH_FLOOR := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._log, 1, @P);
    F.CoolCall := 1;
    PutProperty('log', VarFromClass(F)); //9
    JS_MATH_LOG := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._max, 5, @P);
    F.CoolCall := 1;
    PutProperty('max', VarFromClass(F)); //10
    JS_MATH_MAX := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._min, 5, @P);
    F.CoolCall := 1;
    PutProperty('min', VarFromClass(F)); //11
    JS_MATH_MIN := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._pow, 2, @P);
    F.CoolCall := 1;
    PutProperty('pow', VarFromClass(F)); //12
    JS_MATH_POW := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._random, 0, @P);
    F.CoolCall := 1;
    PutProperty('random', VarFromClass(F)); //13
    JS_MATH_RANDOM := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._round, 1, @P);
    F.CoolCall := 1;
    PutProperty('round', VarFromClass(F)); //14
    JS_MATH_ROUND := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._sin, 1, @P);
    F.CoolCall := 1;
    PutProperty('sin', VarFromClass(F)); //15
    JS_MATH_SIN := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._sqrt, 1, @P);
    F.CoolCall := 1;
    PutProperty('sqrt', VarFromClass(F)); //16
    JS_MATH_SQRT := L.HashTable.LastIndex;

    F := TJS_Function.InternalCreate(@TJS_Math._tan, 1, @P);
    F.CoolCall := 1;
    PutProperty('tan', VarFromClass(F)); //17
    JS_MATH_TAN := L.HashTable.LastIndex;

    PutProperty('PI', PI);                    //18
    JS_MATH_PI := L.HashTable.LastIndex;

    PutProperty('E', 2.7182818284590452354);  //19
    JS_MATH_E := L.HashTable.LastIndex;

    PutProperty('LN10', 2.302585092994046);   //20
    JS_MATH_LN10 := L.HashTable.LastIndex;

    PutProperty('LN2', 0.6931471805599453);   //21
    JS_MATH_LN2 := L.HashTable.LastIndex;

    PutProperty('LOG2E', 1.4426950408889634); //22
    JS_MATH_LOG2E := L.HashTable.LastIndex;

    PutProperty('LOG10E', 0.434294819032518); //23
    JS_MATH_LOG10E := L.HashTable.LastIndex;

    PutProperty('SQRT1_2', 0.7071067811865476); //24
    JS_MATH_SQRT1_2 := L.HashTable.LastIndex;

    PutProperty('SQRT2', 1.4142135623730951); //25
    JS_MATH_SQRT2 := L.HashTable.LastIndex;
  end;
end;

procedure Create_JSObjects(Prog: Pointer; R: TJS_Record);
var
  X: TJS_Object;
  P: TBaseRunner;
begin
  P := TBaseRunner(Prog);

  P.ProgTag := 1;

// global Object object
  P.JS_Object := TJS_Object.Create;
  X := P.JS_Object as TJS_Object;
  X.prog := P;
  X.aconstructor := X;
  X.Tag := 1;
  X.AddToGC;

  X.prototype := TJS_Object.Create;
  X.prototype.aconstructor := X;
  X.prototype.prog := P;
  X.prototype.AddToGC;

  P.SetAddress(P.GetOffset(R.H_JS_Object), @P.JS_Object);

  Create_BooleanObject(P, R);
  Create_StringObject(P, R);
  Create_NumberObject(P, R);
  Create_DateObject(P, R);
  Create_FunctionObject(P, R);
  Create_ArrayObject(P, R);
  Create_RegExpObject(P, R);
  Create_MathObject(P, R);
  Create_ErrorObject(P, R);

  P.RootGC.Mark;
//  P.ProgTag := 0;
end;

// VARIANT OPERATORS

procedure _FuncObjFromVariant(source: PVariant; DestAddress: Pointer); stdcall;
begin
  with TVarData(source^) do
  begin
    if VType <> varClass then
    begin
      if VType in [varEmpty, varDispatch] then
      begin
        TObject(DestAddress^) := EmptyFunction;
        Exit;
      end;

      RaiseError(errCannotConvertToFunctionObject, []);
    end;
    TObject(DestAddress^) := TObject(VInteger);
    if TObject(DestAddress^).ClassType <> TJS_Function then
      RaiseError(errCannotConvertToFunctionObject, []);
  end;
end;

procedure _VariantAddition(Language: Integer;
                           v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);

    if JS_IsString(w1) or JS_IsString(w2) then
    begin
      if not JS_IsString(w1) then
        w1 := JS_ToString(w1);
      if not JS_IsString(w2) then
        w2 := JS_ToString(w2);

      if JS_IsRef(dest^) then
        JS_PutValue(dest^, w1 + w2)
      else
        dest^ := w1 + w2;
    end
    else
    begin
      if JS_IsRef(dest^) then
        JS_PutValue(dest^, JS_ToNumber(w1) + JS_ToNumber(w2))
      else
        dest^ := JS_ToNumber(w1) + JS_ToNumber(w2);
    end;
  end
  else
  begin
    dest^ := v1^ + v2^;
  end;
end;

procedure _VariantSubtraction(Language: Integer;
                              v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_GetValue(v1^);
    w2 := JS_GetValue(v2^);

    if IsDateObject(w1) and IsDateObject(w2) then
    begin
      if JS_IsRef(dest^) then
        JS_PutValue(dest^, VariantToDateObject(w1).Value -
                           VariantToDateObject(w2).Value)
      else
        dest^ := VariantToDateObject(w1).Value -
                 VariantToDateObject(w2).Value;
      Exit;
    end
    else if IsDateObject(w1) then
    begin
      if JS_IsRef(dest^) then
        JS_PutValue(dest^, VariantToDateObject(w1).Value - JS_ToNumber(w2))
      else
        dest^ := VariantToDateObject(w1).Value - JS_ToNumber(w2);
      Exit;
    end
    else if IsDateObject(w2) then
    begin
      if JS_IsRef(dest^) then
        JS_PutValue(dest^, VariantToDateObject(w2).Value - JS_ToNumber(w1))
      else
        dest^ := VariantToDateObject(w2).Value - JS_ToNumber(w1);
      Exit;
    end;

    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_ToNumber(w1) - JS_ToNumber(w2))
    else
      dest^ := JS_ToNumber(w1) - JS_ToNumber(w2);
  end
  else
  begin
    dest^ := v1^ - v2^;
  end;
end;

procedure _VariantMultiplication(Language: Integer;
                                 v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);

    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_ToNumber(w1) * JS_ToNumber(w2))
    else
      dest^ := JS_ToNumber(w1) * JS_ToNumber(w2);
  end
  else
  begin
    dest^ := v1^ * v2^;
  end;
end;

procedure _VariantDivision(Language: Integer;
                           v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);

    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_ToNumber(w1) / JS_ToNumber(w2))
    else
      dest^ := JS_ToNumber(w1) - JS_ToNumber(w2);
  end
  else
  begin
    dest^ := v1^ / v2^;
  end;
end;

procedure _VariantIDivision(Language: Integer;
                            v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);

    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_ToInt32(w1) div JS_ToInt32(w2))
    else
      dest^ := JS_ToInt32(w1) div JS_ToInt32(w2);
  end
  else
    dest^ := v1^ div v2^;
end;

procedure _VariantModulo(Language: Integer;
                         v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);

    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_ToInt32(w1) mod JS_ToInt32(w2))
    else
      dest^ := JS_ToInt32(w1) mod JS_ToInt32(w2);
  end
  else
    dest^ := v1^ mod v2^;
end;

procedure _VariantLeftShift(Language: Integer;
                            v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);
    w1 := JS_ToInt32(w1);
    w2 := JS_ToInt32(w2);
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, w1 shl w2)
    else
      dest^ := w1 shl w2;
  end
  else
    dest^ := v1^ shl v2^;
end;

procedure _VariantRightShift(Language: Integer;
                             v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);
    w1 := JS_ToInt32(w1);
    w2 := JS_ToInt32(w2);
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, w1 shr w2)
    else
      dest^ := w1 shr w2;
  end
  else
    dest^ := v1^ shr v2^;
end;

procedure _VariantAnd(Language: Integer;
                      v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);
    w1 := JS_ToInt32(w1);
    w2 := JS_ToInt32(w2);
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, w1 and w2)
    else
      dest^ := w1 and w2;
  end
  else
    dest^ := v1^ and v2^;
end;

procedure _VariantOr(Language: Integer;
                     v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);
    w1 := JS_ToInt32(w1);
    w2 := JS_ToInt32(w2);
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, w1 or w2)
    else
      dest^ := w1 or w2;
  end
  else
    dest^ := v1^ or v2^;
end;

procedure _VariantXor(Language: Integer;
                      v1, v2, dest: PVariant); stdcall;
var
  w1, w2: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    w1 := JS_ToPrimitive(v1^);
    w2 := JS_ToPrimitive(v2^);
    w1 := JS_ToInt32(w1);
    w2 := JS_ToInt32(w2);
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, w1 xor w2)
    else
      dest^ := w1 xor w2;
  end
  else
    dest^ := v1^ xor v2^;
end;

procedure _VariantLessThan(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
begin
  if Language = JS_LANGUAGE then
  begin
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_RelationalComparison(v1^, v2^))
    else
      dest^ := JS_RelationalComparison(v1^, v2^);
  end
  else
    dest^ := Boolean(v1^ < v2^);
end;

procedure _VariantLessThanOrEqual(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
var
  temp: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    temp := JS_RelationalComparison(v2^, v1^);
    if JS_IsUndefined(temp) then
      temp := false
    else if JS_IsBoolean(temp) then
      temp := not temp
    else
      temp := true;
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, temp)
    else
      dest^ := temp;
  end
  else
    dest^ := v1^ <= v2^;
end;

procedure _VariantGreaterThan(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
var
  temp: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    temp := JS_RelationalComparison(v2^, v1^);
    if JS_IsUndefined(temp) then
      temp := false;
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, temp)
    else
      dest^ := temp;
  end
  else
    dest^ := v1^ > v2^;
end;

procedure _VariantGreaterThanOrEqual(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
var
  temp: Variant;
begin
  if Language = JS_LANGUAGE then
  begin
    temp := JS_RelationalComparison(v1^, v2^);
    if JS_IsUndefined(temp) then
      temp := false
    else if JS_IsBoolean(dest^) then
      temp := not temp
    else
      temp := true;
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, temp)
    else
      dest^ := temp;
  end
  else
    dest^ := v1^ >= v2^;
end;

procedure _VariantEquality(Language: Integer;
                           v1, v2: PVariant; dest: PBoolean); stdcall;
begin
  if Language = JS_LANGUAGE then
  begin
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, JS_EqualityComparison(v1^, v2^))
    else
      dest^ := JS_EqualityComparison(v1^, v2^);
  end
  else
    dest^ := v1^ = v2^;
end;

procedure _VariantNotEquality(Language: Integer;
                              v1, v2: PVariant; dest: PBoolean); stdcall;
begin
  if Language = JS_LANGUAGE then
  begin
    if JS_IsRef(dest^) then
      JS_PutValue(dest^, not JS_EqualityComparison(v1^, v2^))
    else
      dest^ := not JS_EqualityComparison(v1^, v2^);
  end
  else
    dest^ := v1^ <> v2^;
end;

function JS_PointerToVariant(P: Pointer): Variant;
begin
  with TVarData(result) do
  begin
    VType := varPointer;
    VInteger := Integer(P);
  end;
end;

function JS_VariantToPointer(const V: Variant): Pointer;
begin
  with TVarData(V) do
  begin
    result := Pointer(VInteger);
  end;
end;

function Empty(P: PVariant): Boolean;
begin
  result := (P = nil) or (VarType(P^) = varEmpty);
end;

function JS_IsObject(const V: Variant): Boolean;
begin
  result := VarType(V) = varClass;
end;

function JS_IsPointer(const V: Variant): Boolean;
begin
  result := VarType(V) = varPointer;
end;

function JS_IsString(const V: Variant): Boolean;
var
  T: Integer;
begin
  T := VarType(V);
  result := (T = varOleStr) or (T = varString) or (T = varUString);
end;

function JS_IsBoolean(const V: Variant): Boolean;
begin
  result := VarType(V) = varBoolean;
end;

function JS_IsUndefined(const V: Variant): Boolean;
begin
  result := VarType(V) = varEmpty;
end;

function JS_ToPrimitive(const V: Variant): Variant;
var
  X: TJS_ObjectBase;
begin
  if VarType(V) = varClass then
  begin
    X := TJS_ObjectBase(TVarData(V).VInteger);
    if X is TJS_Reference then
    begin
      result := (X as TJS_Reference).GetValue();
      if VarType(result) = varClass then
      begin
        X := TJS_ObjectBase(TVarData(result).VInteger);
        result := (X as TJS_Object).fDefaultValue;
      end;
    end
    else
      result := (X as TJS_Object).fDefaultValue;
  end
  else
    result := V;
end;

function JS_ToString(const V: Variant): Variant;
var
  X: TJS_ObjectBase;
begin
  case VarType(V) of
    varClass:
    begin
      X := TJS_ObjectBase(TVarData(V).VInteger);
      if X is TJS_Reference then
        X := (X as TJS_Reference).GetValueAsObject();

      result := VarToStr((X as TJS_Object).fDefaultValue);
    end;
  else
    result := VarToStr(V);
  end;
end;

function JS_ToNumber(const V: Variant): Variant;
var
  X: TJS_ObjectBase;
  Code1: Integer;
  D: Double;
begin
  case VarType(V) of
    varEmpty: result := NaN;
    varNull: result := 0;
    varBoolean:
      if V then
        result := 1
      else
        result := 0;
    varString, varOleStr, varUString:
    begin
      Val(V, D, Code1);
      result := D;
//      result := StrToFloatDef(StringReplace(V, '.', PAXCOMP_SYS.DecimalSeparator, []), NaN);
    end;
    varClass:
    begin
      X := TJS_ObjectBase(TVarData(V).VInteger);
      if X is TJS_Reference then
        X := (X as TJS_Reference).GetValueAsObject();

      result := JS_ToNumber((X as TJS_Object).fDefaultValue);
    end
    else
      result := V;
  end;
end;

function JS_ToNumberE(const V: Variant): Extended;
var
  X: TJS_ObjectBase;
  D: Double;
  Code1: Integer;
begin
  case VarType(V) of
    varEmpty: result := NaN;
    varNull: result := 0;
    varBoolean:
      if V then
        result := 1
      else
        result := 0;
    varString, varOleStr, varUString:
    begin
      Val(V, D, Code1);
      result := D;
//      result := StrToFloatDef(StringReplace(V, '.', PAXCOMP_SYS.DecimalSeparator, []), NaN);
    end;
    varClass:
    begin
      X := TJS_ObjectBase(TVarData(V).VInteger);
      if X is TJS_Reference then
        X := (X as TJS_Reference).GetValueAsObject();

      result := JS_ToNumber((X as TJS_Object).fDefaultValue);
    end
    else
      result := V;
  end;
end;

function JS_ToBoolean(const V: Variant): Variant;
begin
  result := V;
end;

function JS_ToInt32(const V: Variant): Variant;
var
  N: Variant;
  D: Double;
  I: Integer;
begin
  N := JS_ToNumber(V);
  case VarType(N) of
    varDouble: begin
      D := N;
      if IsNaN(D) or IsInfinite(D) then
        result := 0
      else
      begin
        D := N;
{$IFDEF VARIANTS}
        result := Round(D);
{$ELSE}
        I := Round(D);
        result := I;
{$ENDIF}
      end;
    end;
    varInteger, varByte: Result := N;
{$IFDEF VARIANTS}
    varInt64:
    begin
      D := N;
      I := Round(D);
      result := I;
    end;
{$ENDIF}
  end;
end;

function JS_IsSimpleNumber(const V: Variant): Boolean;
begin
  result := VarType(V) in [varSmallint,
                           varInteger,
                           varSingle,
                           varDouble,
                           varCurrency,
                           varShortInt,
                           varByte,
                           varWord,
                           varLongWord,
                           varInt64];
end;

function JS_IsNumber(const V: Variant): Boolean;
begin
  result := JS_IsSimpleNumber(V);
end;

function JS_RelationalComparison(const V1, V2: Variant): Variant;
                                //performs x < y comparison
var
  I, L: Integer;
  S1, S2: String;
  P1, P2, N1, N2: Variant;
begin
  P1 := JS_ToPrimitive(V1);
  P2 := JS_ToPrimitive(V2);

  if JS_IsString(P1) and JS_IsString(P2) then
  begin
    S1 := P1;
    S2 := P2;
    if Pos(S1, S2) > 0 then
      result := true
    else if Pos(S2, S1) > 0 then
      result := false
    else
    begin
      L := Length(S1);
      if Length(S2) < L then
        L := Length(S2);
      for I:=1 to L do
       if S1[I] <> S2[I] then
       begin
         if Ord(S1[I]) < Ord(S2[I]) then
           result := true
         else
           result := false;
         Exit;
       end;
      result := false;
    end;
  end
  else
  begin
    N1 := JS_ToNumber(P1);
    N2 := JS_ToNumber(P2);
    if IsNAN(N1) then
      Exit;
    if IsNAN(N2) then
      Exit;
    result := N1 < N2;
  end;
end;

function JS_EqualityComparison(const V1, V2: Variant): Boolean;
var
  T1, T2: Integer;
  W1, W2: Variant;
begin
  result := false;
  if (VarType(V1) = varClass) and (VarType(V2) = varEmpty) then
    Exit;
  if (VarType(V2) = varClass) and (VarType(V1) = varEmpty) then
    Exit;

  W1 := JS_ToPrimitive(V1);
  W2 := JS_ToPrimitive(V2);

  T1 := VarType(W1);
  T2 := VarType(W2);
  if T1 = T2 then begin
    if T1 = varUndefined then
    begin
      result := true;
      Exit;
    end;
    if T1 = varNull then
    begin
      result := true;
      Exit;
    end;
    if JS_IsNumber(W1) then
    begin
      if IsNaN(W1) or IsNaN(W2) then
      begin
        result := false;
        Exit;
      end;
      result := W1 = W2;
      Exit;
    end;

    result := W1 = W2;
  end
  else
  begin
    if (T1 = varNull) and (T2 = varUndefined) then
      result := true
    else if (T2 = varNull) and (T1 = varUndefined) then
     result := true
    else if JS_IsNumber(W1) and JS_IsString(W2) then
      result := JS_EqualityComparison(W1, JS_ToNumber(W2))
    else if JS_IsNumber(W2) and JS_IsString(W1) then
      result := JS_EqualityComparison(W2, JS_ToNumber(W1))
    else if JS_IsNumber(W1) and JS_IsBoolean(W2) then
      result := JS_EqualityComparison(W1, JS_ToNumber(W2))
    else if JS_IsNumber(W2) and JS_IsBoolean(W1) then
      result := JS_EqualityComparison(W2, JS_ToNumber(W1))
    else if JS_IsObject(W1) and (JS_IsNumber(W2) or JS_IsBoolean(W2) or JS_IsString(W2)) then
      result := JS_EqualityComparison(JS_ToPrimitive(W1), W2)
    else if JS_IsObject(W2) and (JS_IsNumber(W1) or JS_IsBoolean(W1) or JS_IsString(W1)) then
      result := JS_EqualityComparison(JS_ToPrimitive(W2), W1)
    else if JS_IsNumber(W1) and JS_IsNumber(W2) then
      result := W1 = W2
    else
      result := false;
  end;
end;

//-- THashTable ----------------------------------------------------------------

function HashPJW(S : PChar; TableSize: Integer) : longint;
{Note: this hash function is described in "Practical Algorithms For
       Programmers" by Andrew Binstock and John Rex, Addison Wesley}
const
  BitsInLongint = sizeof(longint) * 8;
  ThreeQuarters = (BitsInLongint * 3) div 4;
  OneEighth = BitsInLongint div 8;
  HighBits : longint =
             (not longint(0)) shl (BitsInLongint - OneEighth);
var
  Test : longint;
  c: Char;
begin
  Result := 0;

  repeat
    c := S^;
    if c = #0 then
      break;

    Result := (Result shl OneEighth) + ord(c);
    Test := Result and HighBits;
    if (Test <> 0) then
      Result := (Result xor (Test shr ThreeQuarters)) and
                not HighBits;
    Inc(S);
  until false;
  result := Result mod TableSize;
  if result < 0 then
    writeln(123);
end;
{--------}
function HashELF(const S : string; TableSize: Integer) : longint;
{Note: this hash function is described in "Practical Algorithms For
       Programmers" by Andrew Binstock and John Rex, Addison Wesley,
       with modifications in Dr Dobbs Journal, April 1996}
var
  G : longint;
  i : integer;
begin
  Result := 0;
  for i := SLow(S) to SHigh(S) do begin
    Result := (Result shl 4) + ord(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then
      Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
  result := Result mod TableSize;
end;
{--------}
function HashBKDR(const S : string; TableSize: Integer) : longint;
{Note: this hash function is described in "The C Programming Language"
       by Brian Kernighan and Donald Ritchie, Prentice Hall}
var
  i : integer;
begin
  Result := 0;
  for i := SLow(S) to SHigh(S) do begin
    Result := (Result * 31) + ord(S[i]);
  end;
  result := Result mod TableSize;
end;

constructor THashTable.Create;
begin
  inherited;
  PrimeIndex := 1;
  TableSize := Primes[PrimeIndex];
  SetLength(A, TableSize + 1);
  Card := 0;
  MaxCard := TableSize div 2;
  HashFunction := HashPJW;
end;

destructor THashTable.Destroy;
begin
  Clear;
  inherited;
end;

function THashTable.IndexOf(const S: PChar; var I: Integer): Boolean;
var
  N: Integer;
begin
  result := false;

  N := HashFunction(S, TableSize);
  I := N;
  while I < TableSize do
  begin
    if A[I] = nil then
    begin
      LastIndex := I;
      result := false;
      Exit;
    end
    else
    if StrComp(A[I].Key, S) = 0 then
    begin
      LastIndex := I;
      result := true;
      Exit;
    end;
    Inc(I);
  end;

  I := 0;
  while I < N do
  begin
    if A[I] = nil then
    begin
      LastIndex := I;
      result := false;
      Exit;
    end
    else
    if StrComp(A[I].Key, S) = 0 then
    begin
      LastIndex := I;
      result := true;
      Exit;
    end;
    Inc(I);
  end;
end;

procedure THashTable.Clear;
var
  I: Integer;
begin
  Card := 0;
  for I:=0 to TableSize - 1 do
    if A[I] <> nil then
    begin
      StrDispose(A[I].Key);
      FreeAndNil(A[I]);
    end;
end;

//-- TJS_PropList --------------------------------------------------------------

constructor TJS_PropList.Create(i_Owner: TJS_Object);
begin
  inherited Create;
  Owner := i_Owner;
  HashTable := THashTable.Create;
  CardArr := 0;
end;

destructor TJS_PropList.Destroy;
begin
  Clear;
  FreeAndNil(HashTable);
  inherited;
end;

procedure TJS_PropList.Clear;
begin
  HashTable.Clear;
end;

procedure TJS_PropList.SetArrLength(N: Integer);
begin
  CardArr := N;
  SetLength(Arr, CardArr);
  SetLength(AvailArrIndex, CardArr);
end;

function TJS_PropList.GetArrProperty(PropName: Integer): PVariant;
begin
  if PropName >= 0 then
  begin
    if owner.typ = TYP_JS_ARRAY then
    begin
      if CardArr <= PropName then
        SetArrLength(PropName + Delta);
      result := @ Arr[PropName];
    end
    else
    begin
      if CardArr <= PropName then
        SetArrLength(PropName + Delta);

      if AvailArrIndex[PropName] then
      begin
        result := @ Arr[PropName];
        Exit;
      end;

      if owner.prototype <> nil then
      begin

        if owner.prototype.L.CardArr <= PropName then
          owner.prototype.L.SetArrLength(PropName + Delta);
        if owner.prototype.L.AvailArrIndex[PropName] then
        begin
          result := @ owner.prototype.L.Arr[PropName];
          Exit;
        end;

      end;

      if TJS_Object(owner.prog.JS_Object).prototype.L.CardArr <= PropName then
        TJS_Object(owner.prog.JS_Object).prototype.L.SetArrLength(PropName + Delta);
      if TJS_Object(owner.prog.JS_Object).prototype.L.AvailArrIndex[PropName] then
      begin
        result := @ TJS_Object(owner.prog.JS_Object).prototype.L.Arr[PropName];
        Exit;
      end;

      result := @ Arr[PropName];
      AvailArrIndex[PropName] := true;
    end;
  end
  else
    result := GetProperty(PChar(IntToStr(PropName)));
end;

procedure TJS_PropList.PutArrProperty(PropName: Integer; const Value: Variant);
begin
  if PropName >= 0 then
  begin
    if CardArr <= PropName then
      SetArrLength(PropName + Delta);
    Arr[PropName] := Value;
    AvailArrIndex[PropName] := true;
  end
  else
    PutProperty(PChar(IntToStr(PropName)), Value);
end;

function TJS_PropList.IndexOfProperty(PropName: PChar; var I: Integer;
  var PositiveInt: Boolean): Boolean;
var
  b: Boolean;
begin
  if PositiveInt then
    b := true
  else
    b := IsPositiveInt(PropName);

  if b then
  begin
    PositiveInt := true;
    I := StrToInt(PropName);

    if CardArr <= I then
      SetArrLength(I + Delta);

    if Owner.Typ = TYP_JS_ARRAY then
      result := true
    else
      result := AvailArrIndex[I];
    Exit;
  end;

  PositiveInt := false;
  result := HashTable.IndexOf(PropName, I);
end;

procedure TJS_PropList.PutProperty(PropName: PChar;
                                   const Value: Variant);
var
  R: TJS_PropRec;
  I: Integer;
  PositiveInt: Boolean;
begin
  PositiveInt := false;

  if IndexOfProperty(PropName, I, PositiveInt) then
  begin
    if PositiveInt then
      Arr[I] := Value
    else
      HashTable.A[I].Value := Value;
  end
  else
  begin
    if PositiveInt then
      Arr[I] := Value
    else
    begin
      R := TJS_PropRec.Create;
      R.Key := StrAlloc(StrLen(PropName) + 1);
      StrCopy(R.Key, PropName);
      R.Value := Value;
      HashTable.A[I] := R;
      Inc(HashTable.Card);
    end;
  end;
end;

function TJS_PropList.GetProperty(PropName: PChar): PVariant;
var
  R: TJS_PropRec;
  I: Integer;
  PositiveInt: Boolean;
  X: TJS_Object;
begin

{
  JS_MATH_LN10 = 20;
  JS_MATH_LN2 = 21;
  JS_MATH_LOG2E = 22;
  JS_MATH_LOG10E = 23;
  JS_MATH_SQRT1_2 = 24;
  JS_MATH_SQRT2 = 25;
}

  case owner.typ of
    TYP_JS_MATH:
      case PropName[0] of
        'a':
          case PropName[1] of
           'b':
            if PropName[2] = 's' then
            if PropName[3] = #0  then // 'abs'
            begin
              result := @ HashTable.A[JS_MATH_ABS].Value;
              Exit;
            end;
            'c':
            if PropName[2] = 'o' then
            if PropName[3] = 's' then
            if PropName[4] = #0  then // 'acos'
            begin
              result := @ HashTable.A[JS_MATH_ACOS].Value;
              Exit;
            end;
            's':
            if PropName[2] = 'i' then
            if PropName[3] = 'n' then
            if PropName[4] = #0  then // 'asin'
            begin
              result := @ HashTable.A[JS_MATH_ASIN].Value;
              Exit;
            end;
            't': // PropName[1]
             if PropName[2] = 'a' then
             if PropName[3] = 'n' then
             begin
               if PropName[4] = #0 then // 'atan'
               begin
                 result := @ HashTable.A[JS_MATH_ATAN].Value;
                 Exit;
               end
               else if PropName[4] = '2' then
                    if PropName[5] = #0  then // 'atan2'
                    begin
                      result := @ HashTable.A[JS_MATH_ATAN2].Value;
                      Exit;
                    end;
             end;
          end;
        'c':
           case PropName[1] of
             'e':
              if PropName[2] = 'i' then
              if PropName[3] = 'l' then
              if PropName[4] = #0 then // 'ceil'
              begin
                result := @ HashTable.A[JS_MATH_CEIL].Value;
                Exit;
              end;
             'o':
              if PropName[2] = 's' then
              if PropName[3] = #0 then // 'cos'
               begin
                 result := @ HashTable.A[JS_MATH_COS].Value;
                 Exit;
               end;
           end;
        'e':
          if StrComp(PropName, 'exp') = 0 then
          begin
            result := @ HashTable.A[JS_MATH_EXP].Value;
            Exit;
          end;
        'f':
          if StrComp(PropName, 'floor') = 0 then
          begin
            result := @ HashTable.A[JS_MATH_FLOOR].Value;
            Exit;
          end;
        'l':
          if PropName[1] = 'o' then
          if PropName[2] = 'g' then
          if PropName[3] = #0 then // 'log'
          begin
            result := @ HashTable.A[JS_MATH_LOG].Value;
            Exit;
          end;
        'm':
           case PropName[1] of
             'a': if StrComp(PropName, 'max') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_MAX].Value;
                 Exit;
               end;
             'i': if StrComp(PropName, 'min') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_MIN].Value;
                 Exit;
               end;
           end;
        'p':
          if StrComp(PropName, 'pow') = 0 then
          begin
            result := @ HashTable.A[JS_MATH_POW].Value;
            Exit;
          end;
        'r':
           case PropName[1] of
             'a': if StrComp(PropName, 'random') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_RANDOM].Value;
                 Exit;
               end;
             'o': if StrComp(PropName, 'round') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_ROUND].Value;
                 Exit;
               end;
           end;
        's':
           case PropName[1] of
             'i': if StrComp(PropName, 'sin') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_SIN].Value;
                 Exit;
               end;
             'q': if StrComp(PropName, 'sqrt') = 0 then
               begin
                 result := @ HashTable.A[JS_MATH_SQRT].Value;
                 Exit;
               end;
           end;
        't':
          if StrComp(PropName, 'tan') = 0 then
          begin
            result := @ HashTable.A[JS_MATH_TAN].Value;
            Exit;
          end;
         'P':
          if StrComp(PropName, 'PI') = 0 then
          begin
            result := @ HashTable.A[JS_MATH_PI].Value;
            Exit;
          end;
         'E':
          if PropName[1] = #0 then
          begin
            result := @ HashTable.A[JS_MATH_E].Value;
            Exit;
          end;
      end;
    // end of math
  end;

  PositiveInt := false;

  if IndexOfProperty(PropName, I, PositiveInt) then
  begin
    if PositiveInt then
      result := @ Arr[I]
    else
     result := @ HashTable.A[I].Value;
  end
  else
  begin
    R := nil;

    // find property in prototype chain

    X := owner.prototype;
    while X <> nil do
    begin
      if X.L.IndexOfProperty(PropName, I, PositiveInt) then
      begin
        if PositiveInt then
        begin
          result := @ X.L.Arr[I];
          Exit;
        end;
        R := HashTable.A[I];
        break;
      end;
      X := X.prototype;
    end;

    if R = nil then
    begin
      case owner.typ of
        TYP_JS_BOOLEAN: X := TJS_Object(owner.prog.JS_Boolean).prototype;
        TYP_JS_STRING: X := TJS_Object(owner.prog.JS_String).prototype;
        TYP_JS_NUMBER: X := TJS_Object(owner.prog.JS_Number).prototype;
        TYP_JS_DATE: X := TJS_Object(owner.prog.JS_Date).prototype;
        TYP_JS_FUNCTION: X := TJS_Object(owner.prog.JS_Function).prototype;
        TYP_JS_ARRAY: X := TJS_Object(owner.prog.JS_Array).prototype;
        TYP_JS_REGEXP: X := TJS_Object(owner.prog.JS_RegExp).prototype;
        TYP_JS_ERROR: X := TJS_Object(owner.prog.JS_Error).prototype;
      else
        X := nil;
      end;

      if X <> nil then
        if X.L.IndexOfProperty(PropName, I, PositiveInt) then
        begin
          if PositiveInt then
          begin
            result := @ X.L.Arr[I];
            Exit;
          end;
          R := X.L.HashTable.A[I];
        end;

      if R = nil then
      begin
        X := TJS_Object(owner.prog.JS_Object).prototype;
        if X.L.IndexOfProperty(PropName, I, PositiveInt) then
        begin
          if PositiveInt then
          begin
            result := @ X.L.Arr[I];
            Exit;
          end;
          R := X.L.HashTable.A[I];
        end;
      end;
    end;

    if R = nil then
      result := @ Undefined
    else
      result := @ R.Value;
  end;
end;

function TJS_PropList.HasProperty(PropName: PChar): Boolean;
begin
  LastPropAddress := GetProperty(PropName);
  result := LastPropAddress <> (@ Undefined);
end;

// -- TJS_Reference ------------------------------------------------------------

constructor TJS_Reference.Create(AFinTypeId: Integer);
begin
  inherited Create;
  FinTypeId := AFinTypeId;
end;

function TJS_Reference.GetValue(): Variant;
begin
  result := GetVariantValue(Address, FinTypeId);
end;

function TJS_Reference.GetValueAsObject(): TJS_Object;
begin
  result := TJS_Object(TVarData(Address^).VInteger);
end;

procedure TJS_Reference.PutValue(const value: Variant);
begin
  PutVariantValue(Address, FinTypeId, value);
end;

function TJS_Reference.__toString: String;
begin
  result := '';
end;

function JS_IsRef(const V: Variant): Boolean;
var
  X: TJS_ObjectBase;
  VT: Word;
begin
  VT := TVarData(V).VType;
  if VT = varClass then
  begin
    X := TJS_ObjectBase(TVarData(V).VInteger);
    result := X is TJS_Reference;
  end
  else
    result := false;
end;

function JS_GetValue(const V: Variant): Variant;
begin
  if JS_IsRef(V) then
    result := TJS_Reference(TVarData(V).VInteger).GetValue()
  else
    result := V;
end;

procedure JS_PutValue(const V: Variant; const value: Variant);
begin
  if not JS_IsRef(V) then
    RaiseError(errReferenceError, []);
  TJS_Reference(TVarData(V).VInteger).PutValue(value);
end;

//-- TJS_ObjectBase ------------------------------------------------------------

function TJS_ObjectBase.GetGC: TGC;
begin
  result := nil;
  RaiseError(errInternalError, []);
end;

//-- TJS_Object ----------------------------------------------------------------

constructor TJS_Object.Create;
begin
  inherited;
  Typ := TYP_JS_OBJECT;
  L := TJS_PropList.Create(Self);
  prototype := nil;
  Tag := 0;
  prog := nil;
  aconstructor := nil;
  NextPropIndex := -1;
end;

destructor TJS_Object.Destroy;
begin
  FreeAndNil(L);
  inherited;
end;

function TJS_Object.__toString: String;
begin
  result := 'Object[]';
end;

function TJS_Object.GetConstructor: TJS_Object;
var
  X: TJS_Object;
begin

  if aconstructor <> nil then
  begin
    result := aconstructor;
    Exit;
  end;

  X := Self.prototype;
  while X <> nil do
  begin
    if X.aconstructor <> nil then
    begin
      result := X.aconstructor;
      Exit;
    end;
    X := X.prototype;
  end;
  result := nil;
end;

procedure TJS_Object.PutProperty(PropName: PChar; const Value: Variant);
begin
  L.PutProperty(PropName, Value);
end;

function TJS_Object.HasProperty(PropName: PChar): Boolean;
begin
  result := L.HasProperty(PropName);
end;

function TJS_Object.GetProperty(PropName: PChar): Variant;
var
  P: Pointer;
begin
  P := L.GetProperty(PropName);
  if TVarData(P^).VType = varClass then
  begin
    TVarData(result).VType := varClass;
    TVarData(result).VInteger := TVarData(P^).VInteger;
  end
  else
    result := Variant(P^);
end;

function TJS_Object.GetVarProperty(const PropName: Variant): Variant;
var
  S: String;
begin
  S := JS_ToString(PropName);
  result := GetProperty(PChar(S));
end;

procedure TJS_Object.PutVarProperty(const PropName: Variant; const Value: Variant);
var
  S: String;
begin
  S := JS_ToString(PropName);
  PutProperty(PChar(S), Value);
end;

function TJS_Object.GetPropertyAsObject(PropName: PChar): TJS_Object;
begin
  result := TObject(TVarData(L.GetProperty(PropName)^).VInteger) as TJS_Object;
  if result = nil then
    RaiseError(errPropertyNotFound, [String(PropName)]);
end;

procedure TJS_Object.PutArrProperty(PropName: Integer; const Value: Variant);
begin
  if PropName >= fLength then
    fLength := PropName + 1;
  L.PutArrProperty(PropName, Value);
end;

function TJS_Object.GetArrProperty(PropName: Integer): Variant;
begin
  if PropName >= fLength then
    fLength := PropName + 1;
  result := L.GetArrProperty(PropName)^;
end;

procedure TJS_Object.AddToGC;
begin
  if prog = nil then
    RaiseError(errInternalError, []);
  prog.RootGC.AddObject(Self);
end;

function TJS_Object.GetGC: TGC;
begin
  if prog = nil then
    RaiseError(errInternalError, []);
  result := TBaseRunner(prog).RootGC;
end;

//-- TJS_Date ------------------------------------------------------------------

function IsDateObject(const V: Variant): Boolean;
begin
  result := TVarData(V).VType = varClass;
  if result then
    result := TObject(TVarData(V).VInteger).ClassType = TJS_Date;
end;

function VariantToDateObject(const V: Variant): TJS_Date;
begin
  result := TJS_Date(TVarData(V).VInteger);
end;

function _Floor(X: Extended): Int64;
begin
  result := Trunc(X);
  if Frac(X) < 0 then
    Dec(result);
end;

function DelphiDateTimeToEcmaTime(const AValue: TDateTime): Double;
var
  T: TTimeStamp;
  D1970: TDateTime;
begin
  D1970 := EncodeDate(1970,1,1);

  T := DateTimeToTimeStamp(AValue);
  Result := (_Floor(AValue) - _Floor(D1970)) * MSecsPerDay + T.Time;
end;

function EcmaTimeToDelphiDateTime(const AValue: Variant): TDateTime;
var
  TimeStamp: TTimeStamp;
  D1970: TDateTime;
begin
  D1970 := EncodeDate(1970,1,1);

  TimeStamp := DateTimeToTimeStamp(D1970);

  TimeStamp.Time := _Floor(AValue) mod MSecsPerDay;
  TimeStamp.Date := TimeStamp.Date + _Floor(AValue) div MSecsPerDay;

  result := TimeStampToDateTime(TimeStamp);
end;

{$IFDEF PAXARM}
function GetGMTDifference: Double;
begin
  result := 0;
end;
{$ELSE}
{$IFDEF LINUX}
function GetGMTDifference: Double;
begin
  result := 0;
end;
{$ELSE}
  {$IFDEF MACOS32}
function GetGMTDifference: Double;
begin
  result := 0;
end;
  {$ELSE}
function GetGMTDifference: Double;
var
  TZ: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TZ);
  if TZ.Bias = 0 then
    Result := 0
  else if TZ.Bias < 0 then
  begin
    if TZ.Bias mod 60 = 0 then
      Result := Abs(TZ.Bias) div 60
    else
      Result := Abs(TZ.Bias) / 60;
  end
  else
  begin
    if TZ.Bias mod 60 = 0 then
      Result := - TZ.Bias div 60
    else
      Result := - TZ.Bias / 60;
  end;
end;
  {$ENDIF}
{$ENDIF}
{$ENDIF}


constructor TJS_Date.Create(Year: PVariant = nil;
                            Month: PVariant = nil;
                            Day: PVariant = nil;
                            Hours: PVariant = nil;
                            Minutes: PVariant = nil;
                            Seconds: PVariant = nil;
                            Ms: PVariant = nil);
begin
  inherited Create;

  Typ := TYP_JS_DATE;

  DelphiDate := 0;
  if Empty(Year) then
  begin
    DelphiDate := Now;
  end
  else if Empty(Month) then
  begin
    DelphiDate := EncodeDate(Year^, 1, 1);
  end
  else if Empty(Day) then
  begin
    DelphiDate := EncodeDate(Year^, Month^, 1);
  end
  else if Empty(Hours) then
  begin
    DelphiDate := EncodeDate(Year^, Month^, Day^);
  end
  else if Empty(Minutes) then
  begin
    DelphiDate := EncodeDate(Year^, Month^, Day^);
    setHours(Hours^, 0, 0 , 0);
  end
  else if Empty(Seconds) then
  begin
    DelphiDate := EncodeDate(Year^, Month^, Day^);
    setMinutes(Minutes^, 0 , 0);
  end
  else
  begin
    DelphiDate := EncodeDate(Year^, Month^, Day^);
    setSeconds(Seconds^, 0);
  end;
  fDefaultValue := DelphiDate;
end;

function TJS_Date.UTCDelphiDate: TDateTime;
var
  Diff: Integer;
begin
  Diff := Floor(GetGMTDifference);
  result := EcmaTimeToDelphiDateTime(GetValue - MSecsPerHour * Diff);
end;

function TJS_Date.DelphiDateFromUTCDate(D: TDateTime): TDateTime;
var
  T: Double;
  Diff: Integer;
begin
  T := DelphiDateTimeToEcmaTime(D);
  Diff := Floor(GetGMTDifference);
  result := EcmaTimeToDelphiDateTime(T + MSecsPerHour * Diff);
end;

function TJS_Date.GetValue: Variant;
begin
  result := DelphiDateTimeToEcmaTime(DelphiDate);
end;

function TJS_Date._toString: Variant; stdcall;
begin
  result := JS_ToString(DelphiDate);
end;

function TJS_Date.__toString: String;
begin
  result := _toString();
end;

function TJS_Date.toGMTString: Variant; stdcall;
begin
  result := JS_ToString(UTCDelphiDate);
end;

function TJS_Date.getTime: Variant; stdcall;
begin
  result := JS_ToNumber(GetValue);
end;

function TJS_Date.getFullYear: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DelphiDate, Year, Month, Day);
  result := Integer(Year);
end;

function TJS_Date.getUTCFullYear: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(UTCDelphiDate, Year, Month, Day);
  result := Integer(Year);
end;

function TJS_Date.getMonth: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DelphiDate, Year, Month, Day);
  result := Integer(Month);
end;

function TJS_Date.getUTCMonth: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(UTCDelphiDate, Year, Month, Day);
  result := Integer(Month);
end;

function TJS_Date.getDate: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(DelphiDate, Year, Month, Day);
  result := Integer(Day);
end;

function TJS_Date.getUTCDate: Variant; stdcall;
var
  Year, Month, Day: Word;
begin
  DecodeDate(UTCDelphiDate, Year, Month, Day);
  result := Integer(Day);
end;

function TJS_Date.getDay: Variant; stdcall;
begin
  result := DayOfWeek(DelphiDate) - 1;
end;

function TJS_Date.getUTCDay: Variant; stdcall;
begin
  result := DayOfWeek(UTCDelphiDate) - 1;
end;

function TJS_Date.getHours: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(DelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Hour);
end;

function TJS_Date.getUTCHours: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(UTCDelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Hour);
end;

function TJS_Date.getMinutes: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(DelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Min);
end;

function TJS_Date.getUTCMinutes: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(UTCDelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Min);
end;

function TJS_Date.getSeconds: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(DelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Sec);
end;

function TJS_Date.getUTCSeconds: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(UTCDelphiDate, Hour, Min, Sec, MSec);
  result := Integer(Sec);
end;

function TJS_Date.getMilliseconds: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(DelphiDate, Hour, Min, Sec, MSec);
  result := Integer(MSec);
end;

function TJS_Date.getUTCMilliseconds: Variant; stdcall;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(DelphiDate, Hour, Min, Sec, MSec);
  result := Integer(MSec);
end;

function TJS_Date.setTime(const P: Variant): Variant; stdcall;
begin
  result := JS_ToNumber(P);
  DelphiDate := EcmaTimeToDelphiDateTime(result);
end;

function TJS_Date.setMilliseconds(const ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(DelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  result := GetValue;
end;

function TJS_Date.setUTCMilliseconds(const ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(UTCDelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  DelphiDate := DelphiDateFromUTCDate(DelphiDate);
  result := GetValue;
end;

function TJS_Date.setSeconds(const sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(DelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  result := GetValue;
end;

function TJS_Date.setUTCSeconds(const sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(UTCDelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  DelphiDate := DelphiDateFromUTCDate(DelphiDate);
  result := GetValue;
end;

function TJS_Date.setMinutes(const min, sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(DelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(min) <> varEmpty then
    aMin := JS_ToInt32(min);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  result := GetValue;
end;

function TJS_Date.setUTCMinutes(const min, sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(UTCDelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(min) <> varEmpty then
    aMin := JS_ToInt32(min);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  DelphiDate := DelphiDateFromUTCDate(DelphiDate);
  result := GetValue;
end;

function TJS_Date.setHours(const hour, min, sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(DelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(hour) <> varEmpty then
    aHour := JS_ToInt32(hour);
  if VarType(min) <> varEmpty then
    aMin := JS_ToInt32(min);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  result := GetValue;
end;

function TJS_Date.setUTCHours(const hour, min, sec, ms: Variant): Variant; stdcall;
var
  aHour, aMin, aSec, aMsec: Word;
begin
  DecodeTime(UTCDelphiDate, aHour, aMin, aSec, aMsec);
  if VarType(hour) <> varEmpty then
    aHour := JS_ToInt32(hour);
  if VarType(min) <> varEmpty then
    aMin := JS_ToInt32(min);
  if VarType(sec) <> varEmpty then
    aSec := JS_ToInt32(sec);
  if VarType(ms) <> varEmpty then
    aMsec := JS_ToInt32(ms);
  DelphiDate := EncodeTime(aHour, aMin, aSec, aMsec);
  DelphiDate := DelphiDateFromUTCDate(DelphiDate);
  result := GetValue;
end;

function TJS_Date.setDate(const date: Variant): Variant; stdcall;
var
  aYear, aMonth, aDay: Word;
begin
  DecodeDate(DelphiDate, aYear, aMonth, aDay);
  result := GetValue;
end;

//-- TJS_Array -----------------------------------------------------------------

constructor TJS_Array.Create(const V: array of Variant);
var
  I, L: Integer;
begin
  inherited Create;
  Typ := TYP_JS_ARRAY;
  L := System.Length(V);
  if L = 0 then
    Length := 0
  else if L = 1 then
    Length := V[0]
  else
    for I := 0 to L - 1 do
      PutArrProperty(I, V[I]);
end;

destructor TJS_Array.Destroy;
begin
  inherited;
end;

function TJS_Array.GetLength: Integer;
begin
  result := fLength;
end;

procedure TJS_Array.SetLength(value: Integer);
begin
  L.SetArrLength(value);
  fLength := value;
end;

function TJS_Array._toString: Variant; stdcall;
var
  I: Integer;
  V: Variant;
begin
  result := '[';
  for I := 0 to fLength - 1 do
  begin
    V := GetArrProperty(I);
    result := result + JS_ToString(V);
    if I < fLength - 1 then
      result := result + ',';
  end;
  result := result + ']';
end;

function TJS_Array.__toString: String;
begin
  result := _toString();
end;

function TJS_Array._pop: Variant; stdcall;
begin
  result := GetArrProperty(fLength - 1);
  SetLength(fLength - 1);
end;

function TJS_Array._push(P0: PVariant;
                         P1: PVariant = nil;
                         P2: PVariant = nil;
                         P3: PVariant = nil;
                         P4: PVariant = nil;
                         P5: PVariant = nil;
                         P6: PVariant = nil;
                         P7: PVariant = nil;
                         P8: PVariant = nil;
                         P9: PVariant = nil): Variant; stdcall;
begin
  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P0^);
  if Empty(P1) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P1^);
  if Empty(P2) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P2^);
  if Empty(P3) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P3^);
  if Empty(P4) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P4^);
  if Empty(P5) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P5^);
  if Empty(P6) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P6^);
  if Empty(P7) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P7^);
  if Empty(P8) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P8^);
  if Empty(P9) then
  begin
    result := fLength;
    Exit;
  end;

  SetLength(fLength + 1);
  PutArrProperty(fLength - 1, P9^);

end;

//-- TJS_Error -----------------------------------------------------------------

constructor TJS_Error.Create(P: PVariant = nil);
begin
  inherited Create;

  Typ := TYP_JS_ERROR;

  if Empty(P) then
    fDefaultValue := ''
  else
    fDefaultValue := JS_ToString(P^);
end;

function TJS_Error._toString: Variant; stdcall;
begin
  result := fDefaultValue;
end;

function TJS_Error.__toString: String;
begin
  result := _toString();
end;

//-- TJS_Boolean ---------------------------------------------------------------

constructor TJS_Boolean.Create(P: PVariant = nil);
begin
  inherited Create;

  Typ := TYP_JS_BOOLEAN;

  if Empty(P) then
    fDefaultValue := false
  else
    fDefaultValue := JS_ToBoolean(P^);
end;

function TJS_Boolean._toString: Variant; stdcall;
begin
  if fDefaultValue then
    result := 'true'
  else
    result := 'false';
end;

function TJS_Boolean.__toString: String;
begin
  result := _toString();
end;

//-- TJS_Number ----------------------------------------------------------------

constructor TJS_Number.Create(P: PVariant = nil);
begin
  inherited Create;

  Typ := TYP_JS_NUMBER;

  if Empty(P) then
    fDefaultValue := Undefined
  else
    fDefaultValue := JS_ToNumber(P^);
end;

function TJS_Number._toString(): Variant; stdcall;
begin
  result := JS_ToString(fDefaultValue);
end;

function TJS_Number.__toString(): String;
begin
  result := _toString();
end;

//-- TJS_String ----------------------------------------------------------------

constructor TJS_String.Create(P: PVariant = nil);
begin
  inherited Create;

  Typ := TYP_JS_STRING;

  if Empty(P) then
    fDefaultValue := ''
  else
    fDefaultValue := JS_ToString(P^);
end;

function TJS_String._toString: Variant; stdcall;
begin
  result := fDefaultValue;
end;

function TJS_String.__toString: String;
begin
  result := _toString();
end;

function TJS_String._valueOf: Variant; stdcall;
begin
  result := fDefaultValue;
end;

function TJS_String._length: Variant; stdcall;
begin
  result := Length(fDefaultValue);
end;

function TJS_String._charAt(const P: Variant): Variant; stdcall;
var
  I: Integer;
begin
  result := '';
  I := JS_ToInt32(P);
  if I < 0 then
    Exit;
  if I >= Length(fDefaultValue) then
    Exit;
  result := fDefaultValue[I + 1];
end;

function TJS_String._charCodeAt(const P: Variant): Variant; stdcall;
var
  I: Integer;
begin
  result := -1;
  I := JS_ToInt32(P);
  if I < 0 then
    Exit;
  if I >= Length(fDefaultValue) then
    Exit;
  result := ord(String(fDefaultValue)[I + 1]);
end;

function TJS_String._concat(P0: PVariant;
                            P1: PVariant = nil;
                            P2: PVariant = nil;
                            P3: PVariant = nil;
                            P4: PVariant = nil;
                            P5: PVariant = nil;
                            P6: PVariant = nil;
                            P7: PVariant = nil;
                            P8: PVariant = nil;
                            P9: PVariant = nil): Variant; stdcall;
begin
  result := fDefaultValue;
  if Empty(P0) then
    Exit;
  result := result + JS_ToString(P0^);
  if Empty(P1) then
    Exit;
  result := result + JS_ToString(P1^);
  if Empty(P2) then
    Exit;
  result := result + JS_ToString(P2^);
  if Empty(P3) then
    Exit;
  result := result + JS_ToString(P3^);
  if Empty(P4) then
    Exit;
  result := result + JS_ToString(P4^);
  if Empty(P5) then
    Exit;
  result := result + JS_ToString(P5^);
  if Empty(P6) then
    Exit;
  result := result + JS_ToString(P6^);
  if Empty(P7) then
    Exit;
  result := result + JS_ToString(P7^);
  if Empty(P8) then
    Exit;
  result := result + JS_ToString(P8^);
  if Empty(P9) then
    Exit;
  result := result + JS_ToString(P9^);
end;

function TJS_String._fromCharCode(P0: PVariant;
                            P1: PVariant = nil;
                            P2: PVariant = nil;
                            P3: PVariant = nil;
                            P4: PVariant = nil;
                            P5: PVariant = nil;
                            P6: PVariant = nil;
                            P7: PVariant = nil;
                            P8: PVariant = nil;
                            P9: PVariant = nil): Variant; stdcall;
var
  B: Byte;
begin
  result := '';

  if Empty(P0) then
    Exit;
  B := JS_ToInt32(P0^);
  result := result + Chr(B);

  if Empty(P1) then
    Exit;
  B := JS_ToInt32(P1^);
  result := result + Chr(B);

  if Empty(P2) then
    Exit;
  B := JS_ToInt32(P2^);
  result := result + Chr(B);

  if Empty(P3) then
    Exit;
  B := JS_ToInt32(P3^);
  result := result + Chr(B);

  if Empty(P4) then
    Exit;
  B := JS_ToInt32(P4^);
  result := result + Chr(B);

  if Empty(P5) then
    Exit;
  B := JS_ToInt32(P5^);
  result := result + Chr(B);

  if Empty(P6) then
    Exit;
  B := JS_ToInt32(P6^);
  result := result + Chr(B);

  if Empty(P7) then
    Exit;
  B := JS_ToInt32(P7^);
  result := result + Chr(B);

  if Empty(P8) then
    Exit;
  B := JS_ToInt32(P8^);
  result := result + Chr(B);

  if Empty(P9) then
    Exit;
  B := JS_ToInt32(P9^);
  result := result + Chr(B);
end;

function TJS_String._slice(const VStart, VEnd: Variant): Variant; stdcall;
var
  S: String;
  IStart, IEnd, L: Integer;
begin
  S := fDefaultValue;

  L := Length(S);

  if Empty(@VStart) then
  begin
    IStart := 0;
    IEnd := L - 1;
  end
  else if Empty(@VEnd) then
  begin
    IStart := JS_ToInt32(VStart);
    if IStart < 0 then
      IStart := IStart + L;
    IEnd := L - 1;
  end
  else
  begin
    IStart := JS_ToInt32(VStart);
    IEnd := JS_ToInt32(VEnd);
    if IStart < 0 then
      IStart := IStart + L;
    if IEnd < 0 then
      IEnd := IEnd + L;
  end;

  L := IEnd - IStart + 1;

  if L > 0 then
    result := Copy(S, IStart, L);
end;

function TJS_String._substr(const VStart, VLength: Variant): Variant; stdcall;
var
  S: String;
  I, L: Integer;
begin
  S := fDefaultValue;

  I := 1;
  L := Length(S);
  if not Empty(@VStart) then
    I := JS_ToInt32(VStart);
  if not Empty(@VLength) then
    L := JS_ToInt32(VLength);

  result := Copy(S, I + 1, L);
end;

function TJS_String._substring(const VStart, VLength: Variant): Variant; stdcall;
var
  S: String;
  I, L: Integer;
begin
  S := fDefaultValue;

  I := 1;
  L := Length(S);
  if not Empty(@VStart) then
    I := JS_ToInt32(VStart);
  if not Empty(@VLength) then
    L := JS_ToInt32(VLength);

  result := Copy(S, I + 1, L);
end;

function TJS_String._indexOf(const P: Variant): Variant; stdcall;
var
  I: Integer;
  S, Q: String;
begin
  result := Integer(-1);
  S := fDefaultValue;
  Q := JS_ToString(P);
  I := Pos(Q, S);
  if I = 0 then
    Exit;
  result := I - 1;
end;

function TJS_String._lastIndexOf(const P: Variant): Variant; stdcall;
var
  I, L: Integer;
  S, Q: String;
begin
  result := Integer(-1);
  S := fDefaultValue;
  Q := JS_ToString(P);
  L := Length(Q);
  for I:=Length(S) - L downto 1 do
    if Copy(S, I, L) = Q then
    begin
      result := I - 1;
      Exit;
    end;
end;

function TJS_String._anchor(const P: Variant): Variant; stdcall;
begin
  result := '<A NAME="' + JS_ToString(P) + '">' + fDefaultValue + '</A>';
end;

function TJS_String._link(const P: Variant): Variant; stdcall;
begin
  result := '<A HREF="' + JS_ToString(P) + '">' + fDefaultValue + '</A>';
end;

function TJS_String._big: Variant; stdcall;
begin
  result := '<BIG>' + fDefaultValue  + '</BIG>';
end;

function TJS_String._small: Variant; stdcall;
begin
  result := '<SMALL>' + fDefaultValue  + '</SMALL>';
end;

function TJS_String._blink: Variant; stdcall;
begin
  result := '<BLINK>' + fDefaultValue  + '</BLINK>';
end;

function TJS_String._bold: Variant; stdcall;
begin
  result := '<BOLD>' + fDefaultValue  + '</BOLD>';
end;

function TJS_String._italics: Variant; stdcall;
begin
  result := '<I>' + fDefaultValue  + '</I>';
end;

function TJS_String._strike: Variant; stdcall;
begin
  result := '<STRIKE>' + fDefaultValue  + '</STRIKE>';
end;

function TJS_String._sub: Variant; stdcall;
begin
  result := '<SUB>' + fDefaultValue  + '</SUB>';
end;

function TJS_String._sup: Variant; stdcall;
begin
  result := '<SUP>' + fDefaultValue  + '</SUP>';
end;

function TJS_String._fixed: Variant; stdcall;
begin
  result := '<TT>' + fDefaultValue  + '</TT>';
end;

function TJS_String._fontcolor(const P: Variant): Variant; stdcall;
begin
  result := '<FONT COLOR="' + JS_ToString(P) + '">' + fDefaultValue + '</A>';
end;

function TJS_String._fontsize(const P: Variant): Variant; stdcall;
begin
  result := '<FONT SIZE="' + JS_ToString(P) + '">' + fDefaultValue + '</A>';
end;

function TJS_String._toUpperCase: Variant; stdcall;
begin
  result := UpperCase(fDefaultValue);
end;

function TJS_String._toLowerCase: Variant; stdcall;
begin
  result := LowerCase(fDefaultValue);
end;

function TJS_String._Replace(const SearchValue, ReplaceValue: Variant): Variant; stdcall;
var
  SearchStr, ReplaceStr: Variant;
  X: TJS_ObjectBase;
begin
  ReplaceStr := JS_ToString(ReplaceValue);
  if JS_IsObject(SearchValue) then
  begin
    X := TJS_ObjectBase(TVarData(SearchValue).VInteger);
    if X is TJS_Reference then
      X := (X as TJS_Reference).GetValueAsObject();

    if X is TJS_RegExp then
    begin
      result := TJS_RegExp(X).Replace(fDefaultValue, ReplaceStr);
    end
    else
    begin
      SearchStr := VarToStr((X as TJS_Object).fDefaultValue);
      result := StringReplace(fDefaultValue, SearchStr, ReplaceStr, [rfReplaceAll]);
    end;
  end
  else
  begin
    SearchStr := VarToStr(SearchValue);
    result := StringReplace(fDefaultValue, SearchStr, ReplaceStr, [rfReplaceAll]);
  end;
end;

//-- TJS_Function --------------------------------------------------------------

constructor TJS_Function.InternalCreate(i_InternalFuncAddr: Pointer;
                                        i_NP: Integer;
                                        i_ProgPtr: Pointer);
begin
  inherited Create;

  Typ := TYP_JS_FUNCTION;

  arguments := TJS_Array.Create([]);
  arguments.Length := MaxArgs;

  InternalFuncAddr := i_InternalFuncAddr;
  InternalLength := i_NP;

  if i_ProgPtr <> nil then
  begin
    Prog := TBaseRunner(i_ProgPtr^);
    if Prog.ProgTag = 1 then // adding global objects
      Prog.RootGC.AddObject(Self);
  end;

  __this := nil;
  CoolCall := 0;
  DefaultNP := 0;
end;

destructor TJS_Function.Destroy;
begin
  FreeAndNil(arguments);
  inherited;
end;

function TJS_Function._toString: Variant; stdcall;
begin
  result := 'Function[]';
end;

function TJS_Function.__toString: String;
begin
  result := _toString();
end;

{$IFDEF PAXARM}
function TJS_Function.Invoke(const Params: array of Variant): Variant; stdcall;
begin
end;
{$ELSE}
{$IFDEF PAX64}
function TJS_Function.Invoke(const Params: array of Variant): Variant; stdcall;
var
  I, NP: Integer;
  A: array of Pointer;
begin
  NP := Length(Params);
  SetLength(A, NP);
  for I := 0 to NP - 1 do
    A[I] := @Params[I];
  AssignRBX(InternalFuncAddr);
  Push_And_Call(NP, Self, Pointer(A), @result);
end;
{$ELSE}
function TJS_Function.Invoke(const Params: array of Variant): Variant; stdcall;
var
  NP: Integer;
  P, _Self, Res: Pointer;
begin
  NP := Length(Params);
  _Self := Self;
  Res := @result;
  P := @Params;

  Inc(Integer(P), (NP - 1) * VARIANT_SIZE);

  if NP > 0 then
  asm
    // push parameters

    mov edx, P
    mov ecx, NP
    @@loop:

    push edx
    sub edx, VARIANT_SIZE
    sub ecx, 1
    cmp ecx, 0
    jnz @@loop
  end;

  asm
    push NP
    push _Self
    push Res
    call InternalCall
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF PAX64}
procedure Push_And_Call(NP: Integer; Instance, Params, RetAdr: Pointer); assembler;
asm
  // Address = rbx
  // np = rcx
  // instance = rdx
  // params = r8
  // RetAddr = r9

  push rbp

  sub rsp, $100
  mov rbp, rsp

  cmp rcx, 0
  jnz @@Par1
  jmp @@Ret

@@Par1:
  cmp rcx, 1
  jnz @@Par2
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, [r10]
  call rbx
  jmp @@Ret

@@Par2:
  cmp rcx, 2
  jnz @@Par3_or_More
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, [r10]
  add r10, 8
  mov r9, [r10]
  call rbx
  jmp @@Ret

@@Par3_or_More:
  mov r15, rcx
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, [r10]
  add r10, 8
  mov r9, [r10]

  sub r15, 1
  mov r11, $20
@@loop:
  add r10, 8
  mov r14, [r10]
  mov [rsp + r11], r14

  add r11, 8
  sub r15, 1
  jz @@Call
  jmp @@loop

@@Call:
  call rbx

  @@Ret:
  mov rsp, rbp
  add rsp, $100
  pop rbp
  ret
end;

procedure Push_And_Call2(NP: Integer; Instance, Params, RetAdr: Pointer); assembler;
asm
  // Address = rbx
  // np = rcx
  // instance = rdx
  // params = r8
  // RetAddr = r9

  push rbp

  sub rsp, $100
  mov rbp, rsp

  cmp rcx, 0
  jnz @@Par1
  jmp @@Ret

@@Par1:
  cmp rcx, 1
  jnz @@Par2
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, r10
  call rbx
  jmp @@Ret

@@Par2:
  cmp rcx, 2
  jnz @@Par3_or_More
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, r10
  add r10, VARIANT_SIZE
  mov r9, r10
  call rbx
  jmp @@Ret

@@Par3_or_More:
  mov r15, rcx
  mov rcx, rdx
  mov rdx, r9
  mov r10, r8
  mov r8, r10
  add r10, VARIANT_SIZE
  mov r9, r10

  sub r15, 1
  mov r11, $20
@@loop:
  add r10, VARIANT_SIZE
  mov r14, r10
  mov [rsp + r11], r14

  add r11, 8
  sub r15, 1
  jz @@Call
  jmp @@loop

@@Call:
  call rbx

  @@Ret:
  mov rsp, rbp
  add rsp, $100
  pop rbp
  ret
end;

procedure AssignRBX(P: Pointer); assembler;
asm
  mov rbx, P
end;

procedure TJS_Function.InternalCall2(NP: Integer);
var
  P, SelfPtr: Pointer;
  I: Integer;
  A: array[0..IntMaxArgs] of Variant;
  temp: Pointer;
  Q: PVariant;
begin
  arguments.fLength := NP;

  temp := Pointer(Arguments.L.Arr);
  Pointer(Arguments.L.Arr) := @A;

  P := ParArr;
  for I:=0 to NP - 1 do
  begin
    Q := Pointer(P^);
    A[I] := Variant(Q^);
    Inc(IntPax(P), SizeOf(Pointer));
  end;

  P := InternalFuncAddr;
  if __this <> nil then
  begin
    SelfPtr := __this;
    __this := nil;
  end
  else
    SelfPtr := Self;
  AssignRBX(P);
  Push_And_Call2(NP, Self, @ A, RetAdr);
  Pointer(Arguments.L.Arr) := temp;
end;

function TJS_Function.InternalCall(NP: Integer): Variant; stdcall;
asm
  push rbp
  sub rsp, $100
  mov rbp, rsp

  mov [rbp + $110], rcx // instance
  mov [rbp + $118], rdx // ret addr
  mov [rbp + $120], r8  // number of params
  mov [rbp + $128], r9  // first param

  mov r10, rcx
  add r10, RetAdr_OFFSET
  mov [r10], rdx

  mov r10, rcx
  add r10, ParArr_OFFSET
  mov r11, rbp
  add r11, $128
  mov [r10], r11

  mov rdx, r8
  call TJS_Function.InternalCall2

  lea rsp, [rbp + $100]
  pop rbp
  ret
end;
{$ELSE}
{$IFDEF PAXARM}
function TJS_Function.InternalCall(NP: Integer): Variant; stdcall;
begin
end;
{$ELSE}
function TJS_Function.InternalCall(NP: Integer): Variant; stdcall;

var
  Params: Pointer;

procedure Nested;
var
  P, Q, ResPtr, arg_ptr, SelfPtr, temp: Pointer;
  I, NA: Integer;
  A: array[0..IntMaxArgs] of Variant;
begin
  arguments.fLength := NP;

  temp := Pointer(Arguments.L.Arr);
  Pointer(Arguments.L.Arr) := @A;

  for I:=0 to NP - 1 do
  begin
    Inc(Integer(Params), 4);
    Q := Pointer(Params^);
    A[I] := Variant(Q^);
  end;

// make call

  P := InternalFuncAddr;
  if __this <> nil then
  begin
    SelfPtr := __this;
    __this := nil;
  end
  else
    SelfPtr := Self;

  ResPtr := @Result;
  NA := InternalLength;

  if DefaultNP > 0 then
    NA := DefaultNP;

  arg_ptr := @A;
  Inc(Integer(arg_ptr), (NA - 1) * 16);

  if NA > 0 then
  asm
    // push parameters

    mov edx, arg_ptr
    mov ecx, NA
    @@loop:

    push edx
    sub edx, 16
    sub ecx, 1
    cmp ecx, 0
    jnz @@loop
  end;

  asm
    // push self ptr
    push SelfPtr

    // push result ptr
    push ResPtr

    call P
  end;
  Pointer(Arguments.L.Arr) := temp;
end;

var
  RetSize: Integer;
  P: Pointer;
begin
  if InternalFuncAddr = nil then
    Exit;

  case CoolCall of
    1:if NP = InternalLength then // project2.dpr
      begin
        P := InternalFuncAddr;
        asm
          mov esp, ebp;

          pop esi // old ebp
          pop edi // ret addr
          pop ecx // result ptr
          pop edx // self

          pop eax // np

          push edx
          push ecx

          call P

          mov ebp, esi;
          jmp edi;
        end;
      end; // CoolCall = 1
  end;

  asm
    mov Params, ebp;
  end;

  Inc(Integer(Params), 16);

  Nested;

  RetSize := 12 + NP * 4;

  asm
  // emulate ret RetSize
    mov ecx, RetSize

    mov esp, ebp
    pop ebp
    mov ebx, [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop
    pop edx
    jmp ebx
  end;
end;
{$ENDIF}
{$ENDIF}

//-- Math ----------------------------------------------------------------------

constructor TJS_Math.Create;
begin
  inherited;
  Typ := TYP_JS_MATH;
end;

function TJS_Math._abs(const P: Variant): Variant; stdcall;
var
  V: Variant;
begin
  V := JS_ToNumber(P);
  if IsNaN(V) then
    result := NaN
  else if V >= 0 then
    result := V
  else
    result := - V;
end;

function TJS_Math._acos(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else if E > 1 then
    result := NaN
  else if E < -1 then
    result := NaN
  else
    result := Math.ArcCos(E);
end;

function TJS_Math._asin(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else if E > 1 then
    result := NaN
  else if E < -1 then
    result := NaN
  else
    result := Math.ArcSin(E);
end;

function TJS_Math._atan(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else
    result := ArcTan(E);
end;

function TJS_Math._atan2(const X, Y: Variant): Variant; stdcall;
var
  VX, VY: Extended;
begin
  VX := JS_ToNumberE(X);
  VY := JS_ToNumberE(Y);
  if IsNaN(VX) then
    result := NaN
  else if IsNaN(VY) then
    result := NaN
  else
    result := Math.ArcTan2(VX, VY);
end;

function TJS_Math._ceil(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else
    result := Math.Ceil(E);
end;

function TJS_Math._cos(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else
    result := Cos(E);
end;

function TJS_Math._exp(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else
    result := Exp(E);
end;

function TJS_Math._floor(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);
  if IsNaN(E) then
    result := NaN
  else
    result := Math.Floor(E);
end;

function TJS_Math._log(const P: Variant): Variant; stdcall;
var
  E: Extended;
begin
  E := JS_ToNumberE(P);

  if IsNaN(E) then
    result := NaN
  else if E < 0 then
    result := NaN
  else if E = 0 then
    result := NegInfinity
  else
    result := ln(E);
end;

function TJS_Math._max(P1, P2, P3, P4, P5: PVariant): Variant; stdcall;
var
  V: Extended;
begin
  result := NegInfinity;

  if Empty(P1) then
    Exit;
  V := JS_ToNumber(P1^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;

  result := V;

  if Empty(P2) then
    Exit;
  V := JS_ToNumber(P2^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V > result then
    result := V;

  if Empty(P3) then
    Exit;
  V := JS_ToNumber(P3^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V > result then
    result := V;

  if Empty(P4) then
    Exit;
  V := JS_ToNumber(P4^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V > result then
    result := V;

  if Empty(P5) then
    Exit;
  V := JS_ToNumber(P5^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V > result then
    result := V;
end;

function TJS_Math._min(P1, P2, P3, P4, P5: PVariant): Variant; stdcall;
var
  V: Extended;
begin
  result := Infinity;

  if Empty(P1) then
    Exit;
  V := JS_ToNumber(P1^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;

  result := V;

  if Empty(P2) then
    Exit;
  V := JS_ToNumber(P2^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V < result then
    result := V;

  if Empty(P3) then
    Exit;
  V := JS_ToNumber(P3^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V < result then
    result := V;

  if Empty(P4) then
    Exit;
  V := JS_ToNumber(P4^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V < result then
    result := V;

  if Empty(P5) then
    Exit;
  V := JS_ToNumber(P5^);
  if IsNan(V) then
  begin
    result := NaN;
    Exit;
  end;
  if V < result then
    result := V;
end;

function TJS_Math._pow(const X, Y: Variant): Variant; stdcall;
var
  VX, VY: Extended;
begin
  VX := JS_ToNumberE(X);
  VY := JS_ToNumberE(Y);

  if IsNaN(VX) then
    result := NaN
  else if IsNaN(VY) then
    result := NaN
  else
    result := Math.Power(VX, VY);
end;

function TJS_Math._random: Variant; stdcall;
begin
  result := Random(10000)/10000;
end;

function TJS_Math._round(const P: Variant): Variant; stdcall;
var
  V: Extended;
begin
  V := JS_ToNumberE(P);
  if IsNaN(V) then
    result := NaN
  else
{$IFDEF VARIANTS}
    result := round(V);
{$ELSE}
    result := Integer(round(V));
{$ENDIF}
end;

function TJS_Math._sin(const P: Variant): Variant; stdcall;
var
  V: Extended;
begin
  V := JS_ToNumberE(P);
  if IsNaN(V) then
    result := NaN
  else
    result := Sin(V);
end;

function TJS_Math._sqrt(const P: Variant): Variant; stdcall;
var
  V: Extended;
begin
  V := JS_ToNumberE(P);
  if IsNaN(P) then
    result := NaN
  else
    result := Sqrt(V);
end;

function TJS_Math._tan(const P: Variant): Variant; stdcall;
var
  V: Extended;
begin
  V := JS_ToNumberE(P);
  if IsNaN(V) then
    result := NaN
  else
    result := Math.tan(V);
end;

// TJS_RegExp ------------------------------------------------------------------

{$IFDEF PAXARM}
constructor TJS_RegExp.Create(Source: PVariant = nil; Modifiers: PVariant = nil);
begin
  inherited Create;
  Typ := TYP_JS_REGEXP;
end;

destructor TJS_RegExp.Destroy;
begin
  inherited;
end;

function TJS_RegExp.GetMatch(I: Integer): String;
begin
  result := '';
  RIE;
end;

function TJS_RegExp.GetMatchLen(I: Integer): Integer;
begin
  result := 0;
  RIE;
end;

function TJS_RegExp.GetMatchPos(I: Integer): Integer;
begin
  result := 0;
  RIE;
end;

function TJS_RegExp.GetSource: Variant;
begin
  RIE;
end;

procedure TJS_RegExp.SetSource(const Value: Variant);
begin
  RIE;
end;

function TJS_RegExp.GetInput: Variant;
begin
  RIE;
end;

procedure TJS_RegExp.SetInput(const Value: Variant);
begin
  RIE;
end;

function TJS_RegExp.GetGlobal: Boolean;
begin
  result := false;
  RIE;
end;

procedure TJS_RegExp.SetGlobal(const Value: Boolean);
begin
  RIE;
end;

function TJS_RegExp.GetIgnoreCase: Boolean;
begin
  result := false;
  RIE;
end;

procedure TJS_RegExp.SetIgnoreCase(const Value: Boolean);
begin
  RIE;
end;

function TJS_RegExp.GetMultiLine: Boolean;
begin
  result := false;
  RIE;
end;

procedure TJS_RegExp.SetMultiLine(const Value: Boolean);
begin
  RIE;
end;

function TJS_RegExp.Test(const InputString: Variant): Boolean;
begin
  result := false;
  RIE;
end;

procedure TJS_RegExp.Compile;
begin
  RIE;
end;

function TJS_RegExp.Exec(const InputString: Variant): TJS_Array;
begin
  RIE;
  result := nil;
end;

function TJS_RegExp.Execute(const InputString: Variant): TJS_Array;
begin
  RIE;
  result := nil;
end;

function TJS_RegExp.MatchCount: Integer;
begin
  result := 0;
  RIE;
end;

function TJS_RegExp.Replace(const Expression, ReplaceStr: Variant): String;
begin
  RIE;
end;

function TJS_RegExp._toString: Variant;
begin
  result := '/' + Source + '/';
  if Global then
    result := result + 'g';
  if IgnoreCase then
    result := result + 'i';
  if MultiLine then
    result := result + 'm';
end;

function TJS_RegExp.__toString: String;
begin
  result := _toString();
end;

{$ELSE}
constructor TJS_RegExp.Create(Source: PVariant = nil; Modifiers: PVariant = nil);
begin
  inherited Create;
  Typ := TYP_JS_REGEXP;
  fRegExpr := TRegExpr.Create;
  if Source <> nil then
    SetSource(Source^);
  fLastIndex := 1;

  if Modifiers = nil then
    Exit;

  if Length(Modifiers^) = 0 then
  begin
    Global := false;
    IgnoreCase := false;
    MultiLine := false;
  end
  else
  begin
    Global := PosCh('g', UpperCase(Modifiers^)) > 0;
    IgnoreCase := PosCh('i', UpperCase(Modifiers^)) > 0;
    MultiLine := PosCh('m', UpperCase(Modifiers^)) > 0;
  end;
end;

destructor TJS_RegExp.Destroy;
begin
  FreeAndNil(fRegExpr);
  inherited;
end;

function TJS_RegExp.GetMatch(I: Integer): String;
begin
  result := fRegExpr.Match[I];
end;

function TJS_RegExp.GetMatchLen(I: Integer): Integer;
begin
  result := fRegExpr.MatchLen[I];
end;

function TJS_RegExp.GetMatchPos(I: Integer): Integer;
begin
  if fZERO_BASED_STRINGS then
    result := fRegExpr.MatchPos[I] - 1
  else
    result := fRegExpr.MatchPos[I];
end;

function TJS_RegExp.GetSource: Variant;
begin
  result := fRegExpr.Expression;
end;

procedure TJS_RegExp.SetSource(const Value: Variant);
begin
  fRegExpr.Expression := Value;
end;

function TJS_RegExp.GetInput: Variant;
begin
  result := fRegExpr.InputString;
end;

procedure TJS_RegExp.SetInput(const Value: Variant);
begin
  fRegExpr.InputString := Value;
end;

function TJS_RegExp.GetGlobal: Boolean;
begin
  result := fRegExpr.ModifierG;
end;

procedure TJS_RegExp.SetGlobal(const Value: Boolean);
begin
  fRegExpr.ModifierG := Value;
end;

function TJS_RegExp.GetIgnoreCase: Boolean;
begin
  result := fRegExpr.ModifierI;
end;

procedure TJS_RegExp.SetIgnoreCase(const Value: Boolean);
begin
  fRegExpr.ModifierI := Value;
end;

function TJS_RegExp.GetMultiLine: Boolean;
begin
  result := fRegExpr.ModifierM;
end;

procedure TJS_RegExp.SetMultiLine(const Value: Boolean);
begin
  fRegExpr.ModifierM := Value;
end;

function TJS_RegExp.Test(const InputString: Variant): Boolean;
begin
  result := fRegExpr.Exec(InputString);
end;

procedure TJS_RegExp.Compile;
begin
  fRegExpr.Compile;
end;

function TJS_RegExp.Exec(const InputString: Variant): TJS_Array;
var
  I, L: Integer;
  _InputString: String;
begin
  _InputString := InputString;

  fRegExpr.InputString := _InputString;
  L := Length(_InputString);
  if LastIndex >= L then
  begin
    LastIndex := 1;
    result := TJS_Array.Create([]);
    result.prog := prog;
    result.AddToGC;
    result.length := 0;

    result.PutProperty('lastIndex', LastIndex);
    result.PutProperty('inputString', InputString);
    Exit;
  end;

  if fRegExpr.ExecPos(LastIndex) then
  begin
    result := TJS_Array.Create([]);
    result.prog := prog;
    result.AddToGC;

    for I:=0 to fRegExpr.SubExprMatchCount do
       result.PutArrProperty(I, fRegExpr.Match[I]);

    if fZERO_BASED_STRINGS then
    begin
      with fRegExpr do
      if MatchLen[0] = 0 then
        LastIndex := MatchPos[0]
      else
        LastIndex := MatchPos[0] + MatchLen[0];

      result.PutProperty('index', fRegExpr.MatchPos[0] - 1);
      result.PutProperty('lastIndex', LastIndex - 1);
    end
    else
    begin
      with fRegExpr do
      if MatchLen[0] = 0 then
        LastIndex := MatchPos[0] + 1
      else
        LastIndex := MatchPos[0] + MatchLen[0] + 1;

      result.PutProperty('index', fRegExpr.MatchPos[0]);
      result.PutProperty('lastIndex', LastIndex);
    end;

    result.PutProperty('inputString', InputString);
  end
  else
  begin
    result := TJS_Array.Create([]);
    result.prog := prog;
    result.AddToGC;
    result.length := 0;
    result.PutProperty('lastIndex', LastIndex);
    result.PutProperty('lnputString', InputString);
  end;
end;

function TJS_RegExp.Execute(const InputString: Variant): TJS_Array;
var
  I: Integer;
  P: TIntegerList;
begin
  fRegExpr.InputString := InputString;
  P := TIntegerList.Create;
  try
    if fRegExpr.Exec(InputString) then
    begin
      repeat
        P.Add(fRegExpr.MatchPos[0]);
      until not fRegExpr.ExecNext;
    end;
    result := TJS_Array.Create([]);
    result.prog := prog;
    result.AddToGC;
    for I:=0 to P.Count - 1 do
      result.PutArrProperty(I, P[I]);
  finally
    FreeAndNil(P);
  end;
end;

function TJS_RegExp.MatchCount: Integer;
begin
  result := fRegExpr.SubExprMatchCount;
end;

function TJS_RegExp.Replace(const Expression, ReplaceStr: Variant): String;
begin
  result := fRegExpr.Replace(Expression, ReplaceStr);
end;

function TJS_RegExp._toString: Variant;
begin
  result := '/' + Source + '/';
  if Global then
    result := result + 'g';
  if IgnoreCase then
    result := result + 'i';
  if MultiLine then
    result := result + 'm';
end;

function TJS_RegExp.__toString: String;
begin
  result := _toString();
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure _alert(Prog: TBaseRunner;
                 P1: PVariant;
                 P2: PVariant = nil;
                 P3: PVariant = nil;
                 P4: PVariant = nil;
                 P5: PVariant = nil); stdcall;

function Show(P: PVariant): Boolean;
begin
  result := P <> nil;
  if result then
    result := VarType(P^) <> varEmpty;

  if result then
    ErrMessageBox(JS_ToString(P^));
end;

begin
  if Assigned(Prog.OnPrint) then
    Prog.OnPrint(Prog.Owner, JS_ToString(P1^))
  else
    ErrMessageBox(JS_ToString(P1^));

  if not Show(P2) then Exit;
  if not Show(P3) then Exit;
  if not Show(P4) then Exit;
  if not Show(P5) then Exit;
end;

procedure _WriteObject(const value: TObject);
var
  S: String;
begin
  if value = nil then
  begin
    write('undefined');
  end
  else if value is TJS_Object then
  begin
    S := value.ClassName;
    S := 'object ' + Copy(S, 5, Length(S) - 4);
    write('[' + S + ']');
  end
  else
  begin
    S := 'object ' + value.ClassName;
    write('[' + S + ']');
  end;
end;

{$IFDEF PAX64}
procedure _GetGenericPropertyEx(Prog: TBaseRunner;
                                var VObject: Variant;
                                PropName: PChar;
                                NP: Integer;
                                var Result: Variant;
                                Params: Pointer);
var
  b: Boolean;
  I, VT: Integer;
  S: String;
  E: Extended;
  X, Y: TJS_Object;
  result_addr: Pointer;
  Q: Pointer;
begin
  b := JS_IsObject(VObject);
  if not b then
  begin
    VT := VarType(VObject);
    case VT of
      varUString, varString, varOleStr:
      begin
        S := VObject;
        _JS_ToObject(Prog, @S, typeSTRING, @VObject);
        b := JS_IsObject(VObject);
      end;
      varSmallInt, varInteger, varByte, varShortInt,
      varWord, varLongWord:
      begin
        I := VObject;
        _JS_ToObject(Prog, @I, typeINTEGER, @VObject);
        b := JS_IsObject(VObject);
      end;
      varSingle, varDouble, varCurrency:
      begin
        E:= VObject;
        _JS_ToObject(Prog, @E, typeEXTENDED, @VObject);
        b := JS_IsObject(VObject);
      end;
    end;
  end;

  if b then
  begin
    case NP of
      0:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        result := X.L.GetProperty(PropName)^;

        if JS_IsObject(result) then
        begin
          Y := TJS_Object(TVarData(result).VInteger);
          if Y is TJS_Function then
          begin
            (Y as TJS_Function).__this := X;
          end;
        end;
        Exit;
      end;
      1:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        Y := X.GetPropertyAsObject(PropName);

        if Y is TJS_Function then
        begin
          (Y as TJS_Function).__this := X;
          result_addr := @result;
//          asm
//            jmp TJS_Function.InternalCall
//          end;
        end;

        Q := Pointer(Params^);

        VT := TVarData(Q^).VType;
        if VT = varString then
          result := Y.L.GetProperty(PChar(TVarData(Q^).VString))^
        else if VT in VarIntTypes then
          result := Y.L.GetArrProperty(TVarData(Q^).VInteger)^
        else
          result := Y.GetVarProperty(JS_ToString(Variant(Q^)));
      end;
      else
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        Y := X.GetPropertyAsObject(PropName);

        if Y is TJS_Function then
        begin
          (Y as TJS_Function).__this := X;
          result_addr := @result;
//          asm
//            jmp TJS_Function.InternalCall
//          end;
        end;

        result := Undefined;
      end;
    end; // case NP
  end
  else if Assigned(GetOlePropProc) then
  begin
    RaiseNotImpl;
  end
  else
    result := Undefined;
end;

procedure _GetGenericProperty(Prog: TBaseRunner;
                              var VObject: Variant;
                              PropName: PChar;
                              NP: Integer); assembler;
//  r10 = result
asm
  push rbp
  sub rsp, $40
  mov rbp, rsp

  mov [rsp + $20], r10 // result
  mov [rsp + $28], rax // address of params

  call _GetGenericPropertyEx

  mov rsp, rbp
  add rsp, $40
  pop rbp
  ret
end;
{$ELSE}
{$IFNDEF PAXARM}
procedure _GetGenericProperty(Prog: TBaseRunner;
                              var VObject: Variant;
                              PropName: PChar;
                              NP: Integer;
                              var Result: Variant); stdcall;
var
  X, Y: TJS_Object;
  P, Q: Pointer;
  VT, RetSize: Integer;
  b: Boolean;
  S: String;
  I: Integer;
  E: Extended;
  result_addr: Pointer;
begin
  asm
    mov P, ebp
  end;

  b := JS_IsObject(VObject);
  if not b then
  begin
    VT := VarType(VObject);
    case VT of
      varUString, varString, varOleStr:
      begin
        S := VObject;
        _JS_ToObject(Prog, @S, typeSTRING, @VObject);
        b := JS_IsObject(VObject);
      end;
      varSmallInt, varInteger, varByte, varShortInt,
      varWord, varLongWord:
      begin
        I := VObject;
        _JS_ToObject(Prog, @I, typeINTEGER, @VObject);
        b := JS_IsObject(VObject);
      end;
      varSingle, varDouble, varCurrency:
      begin
        E:= VObject;
        _JS_ToObject(Prog, @E, typeEXTENDED, @VObject);
        b := JS_IsObject(VObject);
      end;
    end;
  end;

  if b then
  begin
    case NP of
      0:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        result := X.L.GetProperty(PropName)^;

        if JS_IsObject(result) then
        begin
          Y := TJS_Object(TVarData(result).VInteger);
          if Y is TJS_Function then
          begin
            (Y as TJS_Function).__this := X;
          end;
        end;
        Exit;
      end;
      1:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        Y := X.GetPropertyAsObject(PropName);

        if Y is TJS_Function then
        begin
          (Y as TJS_Function).__this := X;
          result_addr := @result;
          asm
            mov eax, NP
            mov edx, Y
            mov ecx, result_addr

            mov esp, ebp
            pop ebp // restore old ebp

            pop ebx // pop ret addr
            mov [ebp - 512], ebx // save ret address

            pop ebx // pop 5 parametes
            pop ebx
            pop ebx
            pop ebx
            pop ebx

            push eax // np
            push edx // instance
            push ecx // result

            mov ebx, [ebp - 512]
            push ebx
            jmp TJS_Function.InternalCall
          end;
        end;

        Inc(Integer(P), 28);
        Q := Pointer(P^);

        VT := TVarData(Q^).VType;
        if VT = varString then
          result := Y.L.GetProperty(PChar(TVarData(Q^).VString))^
        else if VT in VarIntTypes then
          result := Y.L.GetArrProperty(TVarData(Q^).VInteger)^
        else
          result := Y.GetVarProperty(JS_ToString(Variant(Q^)));
        asm
          mov esp, ebp
          pop ebp
          ret 24
        end;
      end;
      else
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        Y := X.GetPropertyAsObject(PropName);

        if Y is TJS_Function then
        begin
          (Y as TJS_Function).__this := X;
          result_addr := @result;
          asm
            mov eax, NP
            mov edx, Y
            mov ecx, result_addr

            mov esp, ebp
            pop ebp // restore old ebp

            pop ebx // pop ret addr
            mov [ebp - 512], ebx // save ret address

            pop ebx // pop 5 parametes
            pop ebx
            pop ebx
            pop ebx
            pop ebx

            push eax // np
            push edx // instance
            push ecx // result

            mov ebx, [ebp - 512]
            push ebx
            jmp TJS_Function.InternalCall
          end;
        end;

        result := Undefined;
      end;
    end; // case NP
  end
  else if Assigned(GetOlePropProc) then
  begin
    Inc(Integer(P), 28);

    if NP > 0 then
    asm
      mov edx, P
      mov ecx, NP
      @@loop:

      mov eax, [edx]
      push eax
      add edx, 4

      sub ecx, 1
      cmp ecx, 0
      jnz @@loop
    end;

    GetOlePropProc(VObject,
                   PropName,
                   result,
                   NP);
  end
  else
    result := Undefined;


  RetSize := 20 + NP * 4;

  asm
  // emulate ret RetSize
    mov ecx, RetSize

    mov esp, ebp
    pop ebp
    mov ebx, [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop
    pop edx
    jmp ebx
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF PAX64}

procedure _PutGenericPropertyEx(const VObject: Variant;
                                PropName: PChar;
                                NP: Integer;
                                const Value: Variant;
                                Params: Pointer);
var
  X: TJS_Object;
  Q: Pointer;
  VT: Integer;
begin
  if JS_IsObject(VObject) then
  begin
    case NP of
      0:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        X.L.PutProperty(PropName, Value);
        Exit;
      end;
      1:
      begin
        Q := Pointer(Params^);
        X := TJS_Object(TVarData(VObject).VInteger);
        X := X.GetPropertyAsObject(PropName);
        VT := TVarData(Q^).VType;
        if VT = varString then
          X.L.PutProperty(PChar(TVarData(Q^).VString), Value)
        else if VT in VarIntTypes then
          X.L.PutArrProperty(TVarData(Q^).VInteger, Value)
        else
          X.PutVarProperty(JS_ToString(Variant(Q^)), Value);
      end;
    end; // case NP
  end
  else
    RaiseNotImpl;
end;

procedure _PutGenericProperty(const VObject: Variant;
                              PropName: PChar;
                              NP: Integer;
                              const Value: Variant); assembler;
asm
  push rbp
  sub rsp, $30
  mov rbp, rsp

  mov [rsp + $20], rax // address of params

  call _PutGenericPropertyEx

  mov rsp, rbp
  add rsp, $30
  pop rbp
  ret
end;
{$ELSE}
{$IFNDEF PAXARM}
procedure _PutGenericProperty(const VObject: Variant;
                              PropName: PChar;
                              NP: Integer;
                              const Value: Variant); stdcall;
var
  X: TJS_Object;
  P, Q: Pointer;
  RetSize, VT: Integer;
begin
  asm
    mov P, ebp
  end;

  if JS_IsObject(VObject) then
  begin
    case NP of
      0:
      begin
        X := TJS_Object(TVarData(VObject).VInteger);
        X.L.PutProperty(PropName, Value);
        Exit;
      end;
      1:
      begin
        Inc(Integer(P), 24);
        Q := Pointer(P^);
        X := TJS_Object(TVarData(VObject).VInteger);
        X := X.GetPropertyAsObject(PropName);
        VT := TVarData(Q^).VType;
        if VT = varString then
          X.L.PutProperty(PChar(TVarData(Q^).VString), Value)
        else if VT in VarIntTypes then
          X.L.PutArrProperty(TVarData(Q^).VInteger, Value)
        else
          X.PutVarProperty(JS_ToString(Variant(Q^)), Value);
        asm
          mov esp, ebp
          pop ebp
          ret 20
        end;
      end;
    end; // case NP
  end
  else
  begin
    Inc(Integer(P), 24);

    if NP > 0 then
    asm
      mov edx, P
      mov ecx, NP
      @@loop:

      mov eax, [edx]
      push eax
      add edx, 4

      sub ecx, 1
      cmp ecx, 0
      jnz @@loop
    end;

    PutOlePropProc(VObject,
                   PropName,
                   Value,
                    NP);
  end;

  RetSize := 16 + NP * 4;

  asm
  // emulate ret RetSize
    mov ecx, RetSize

    mov esp, ebp
    pop ebp
    mov ebx, [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop
    pop edx
    jmp ebx
  end;
end;
{$ENDIF}
{$ENDIF}
procedure _JS_TypeOf(V: PVariant;
                     result: PString); stdcall;
var
  JS_Object: TJS_Object;
begin
  if JS_IsString(V^) then
    result^ := 'string'
  else if JS_IsBoolean(V^) then
    result^ := 'boolean'
  else if JS_IsNumber(V^) then
    result^ := 'number'
  else if JS_IsObject(V^) then
  begin
    JS_Object := TJS_Object(TVarData(V^).VInteger);
    if JS_Object is TJS_Function then
      result^ := 'function'
    else
      result^ := 'object';
  end
  else
    result^ := 'undefined';
end;

procedure _JS_Void(var V: Variant;
                   var result: Variant); stdcall;
begin
  VarClear(result);
end;

procedure _JS_Delete(VObject: PVariant;
                     Prop: PString); stdcall;
begin
end;

procedure _JS_GetNextProp(VObject: PVariant;
                          Prop: PString;
                          result: PBoolean); stdcall;
var
  I: Integer;
  JS_Array: TJS_Array;
  JS_Object: TJS_Object;
  LA: Integer;
begin
  JS_Object := TJS_Object(TVarData(VObject^).VInteger);
  I := JS_Object.NextPropIndex;
  Inc(I);
  if JS_Object is TJS_Array then
  begin
    JS_Array := TJS_Array(JS_Object);
    if I > JS_Array.Length then
    begin
      JS_Object.NextPropIndex := -1;
      result^ := false;
    end
    else
    begin
      Prop^ := IntToStr(I);
      JS_Object.NextPropIndex := I;
      result^ := true;
    end;
  end
  else
  begin
    LA := System.Length(JS_Object.L.HashTable.A);
    with JS_Object.L.HashTable do
    repeat
      if I >= LA then
      begin
        JS_Object.NextPropIndex := -1;
        result^ := false;
        Exit;
      end;

      if A[I] = nil then
      begin
        Inc(I);
        continue;
      end
      else
      begin
        Prop^ := A[I].Key;
        JS_Object.NextPropIndex := I;
        result^ := true;
        Exit;
      end;
    until false;
  end;
end;

procedure _JS_ToObject(P:TBaseRunner; Address: Pointer; FinTypeId: Integer;
                       result: PVariant); stdcall;
var
  V: Variant;
  XS: TJS_String;
  XN: TJS_Number;
  XB: TJS_Boolean;
  I: Integer;
  VT: Word;
begin
  case FinTypeId of
    typeCLASS:
    begin
      result^ := VarFromClass(TJS_ObjectBase(Address^));
    end;
    typePOINTER:
    begin
      V := String(PChar(Address^));
      XS := TJS_String.Create(@ V);
      XS.prog := P;
      XS.prototype := P.JS_String as TJS_Object;
      result^ := VarFromClass(XS);
    end;
{$IFNDEF PAXARM}
    typeANSISTRING:
    begin
      V := String(Address^);
      XS := TJS_String.Create(@ V);
      XS.prog := P;
      XS.prototype := P.JS_String as TJS_Object;
      result^ := VarFromClass(XS);
    end;
    typeWIDESTRING:
    begin
      V := WideString(Address^);
      XS := TJS_String.Create(@ V);
      XS.prog := P;
      XS.prototype := P.JS_String as TJS_Object;
      result^ := VarFromClass(XS);
    end;
{$ENDIF}
    typeUNICSTRING:
    begin
      V := String(Address^);
      XS := TJS_String.Create(@ V);
      XS.prog := P;
      XS.prototype := P.JS_String as TJS_Object;
      result^ := VarFromClass(XS);
    end;
    typeINTEGER:
    begin
      V := Integer(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeBYTE:
    begin
      V := Byte(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeWORD:
    begin
      V := Word(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeCARDINAL:
    begin
{$IFDEF VARIANTS}
      V := Cardinal(Address^);
{$ELSE}
      V := Integer(Address^);
{$ENDIF}
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeINT64:
    begin
      I := Int64(Address^);
      V := I;
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeDOUBLE:
    begin
      V := Double(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeSINGLE:
    begin
      V := Single(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeCURRENCY:
    begin
      V := Currency(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeEXTENDED:
    begin
      V := Extended(Address^);
      XN := TJS_Number.Create(@ V);
      XN.prog := P;
      XN.prototype := P.JS_Number as TJS_Object;
      result^ := VarFromClass(XN);
    end;
    typeBOOLEAN:
    begin
      V := Boolean(Address^);
      XB := TJS_Boolean.Create(@ V);
      XB.prog := P;
      XB.prototype := P.JS_Boolean as TJS_Object;
      result^ := VarFromClass(XB);
    end;
    typeVARIANT:
    begin
      VT := TVarData(Address^).VType;
      case VT of
        varInteger, varSmallInt, varShortInt, varByte, varWord, varLongWord,
        varInt64,
        varSingle, varDouble, varCurrency, varDate:
        begin
          XN := TJS_Number.Create(PVariant(Address));
          XN.prog := P;
          XN.prototype := P.JS_Number as TJS_Object;
          result^ := VarFromClass(XN);
        end;
        varBoolean:
        begin
          XB := TJS_Boolean.Create(PVariant(Address));
          XB.prog := P;
          XB.prototype := P.JS_Boolean as TJS_Object;
          result^ := VarFromClass(XB);
        end;
        varString, varOleStr, varUString:
        begin
          XS := TJS_String.Create(PVariant(Address));
          XS.prog := P;
          XS.prototype := P.JS_String as TJS_Object;
          result^ := VarFromClass(XS);
        end;
        varClass:
        begin
          result^ := Variant(Address^);
        end;
        else
          RaiseError(errCannotConvertToJS_Object, []);
      end;
    end;
    else
      RaiseError(errCannotConvertToJS_Object, []);
  end;
end;

procedure _VariantClr(var V: Variant); stdcall;
//var
//  X: TJS_Object;
begin
{
  if JS_IsObject(V) then
  begin
    X := TJS_Object(TVarData(V).VInteger);
    if X <> nil then
    begin
      X.Free;
      TVarData(V).VInteger := 0;
    end;
  end;
}
  VarClear(V);
end;

procedure _PushContext(P: TBaseRunner; value: PVariant); stdcall;
var
  X: TObject;
begin
  X := TObject(TVarData(value^).VInteger);

  if X is TJS_Reference then
    X := TJS_Reference(X).GetValueAsObject();

  P.ContextList.Add(X);
end;

procedure _PopContext(P: TBaseRunner); stdcall;
begin
  P.ContextList.Delete(P.ContextList.Count - 1);
end;

procedure _FindContext(P: TBaseRunner; PropName: PChar;
                       AltAddress: Pointer;
                       FinTypeId: Integer;
                       result: PVariant); stdcall;
var
  I: Integer;
  X: TJS_Object;
{$IFDEF ARC}
  L: TList<TObject>;
{$ELSE}
  L: TList;
{$ENDIF}
  R: TJS_Reference;
begin
  L := P.ContextList;
  for I := L.Count - 1 downto 0 do
    if TObject(L[I]) is TJS_Object then
    begin
      X := TJS_Object(L[I]);
      if X.HasProperty(PropName) then
      begin
        R := TJS_Reference.Create(typeVARIANT);
        R.Address := X.L.LastPropAddress;
        P.RootGC.AddReference(R);

//      R.Base := X;
//      R.PropName := PropName;

        result^ := VarFromClass(R);
        Exit;
      end;
    end;
  R := TJS_Reference.Create(FinTypeId);
  R.Address := AltAddress;
  P.RootGC.AddReference(R);
  result^ := VarFromClass(R);
end;

procedure _FindFunc(P: TBaseRunner; PropName: PChar;
                    Alt, result: PVariant); stdcall;
var
  I: Integer;
  X: TJS_Object;
{$IFDEF ARC}
  L: TList<TObject>;
{$ELSE}
  L: TList;
{$ENDIF}
begin
  L := P.ContextList;
  for I := L.Count - 1 downto 0 do
  begin
    if TObject(L[I]) is TJS_Object then
    begin
      X := TJS_Object(L[I]);
      if X.HasProperty(PropName) then
      begin
        result^ := X.GetProperty(PropName);
        if TVarData(result^).VInteger <> 0 then
{$IFDEF PAX64}
          TJS_Function(TVarData(result^).VInt64).__this := X;
{$ELSE}
          TJS_Function(TVarData(result^).VInteger).__this := X;
{$ENDIF}
        Exit;
      end;
    end;
  end;
  result^ := Alt^;
end;

// overriden routines - begin

procedure _VarArrayPut1(var V: Variant; var value: Variant; const I1: Variant);
stdcall;
var
  X: TJS_Object;
  S: String;
begin
  if JS_IsObject(V) then
  begin
    X := TJS_Object(TVarData(V).VInteger);
    if VarType(I1) in VarIntTypes then
      X.PutArrProperty(I1, Value)
    else
    begin
      S := JS_ToString(I1);
      X.PutProperty(PChar(S), Value);
    end;
  end
  else
    V[I1] := value;
end;

procedure _VarArrayGet1(var V: Variant; var result: Variant; const I1: Variant);
stdcall;
var
  X: TJS_Object;
  S: String;
begin
  if JS_IsObject(V) then
  begin
    X := TJS_Object(TVarData(V).VInteger);
    if VarType(I1) in VarIntTypes then
      result := X.GetArrProperty(I1)
    else
    begin
      S := JS_ToString(I1);
      result := X.GetProperty(PChar(S));
    end;
  end
  else
    result := V[I1];
end;

procedure _VarArrayPut2(var V: Variant; var value: Variant; const I2, I1: Variant);
stdcall;
begin
  V[I1, I2] := value;
end;

procedure _VarArrayGet2(var V: Variant; var result: Variant; const I2, I1: Variant);
stdcall;
begin
  result := V[I1, I2];
end;

procedure _VarArrayPut3(var V: Variant; var value: Variant; const I3, I2, I1: Variant);
stdcall;
begin
  V[I1, I2, I3] := value;
end;

procedure _VarArrayGet3(var V: Variant; var result: Variant; const I3, I2, I1: Variant);
stdcall;
begin
  result := V[I1, I2, I3];
end;

procedure _VariantFromPWideChar(source: PWideChar; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, UnicString(Source))
  else
    dest^ := UnicString(Source);
end;

{$IFNDEF PAXARM}
procedure _VariantFromPAnsiChar(source: PAnsiChar; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, AnsiString(Source))
  else
    dest^ := AnsiString(Source);
end;

procedure _VariantFromAnsiString(Dest: PVariant; Source: PAnsiString); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    Dest^ := Source^;
end;

procedure _VariantFromWideString(Dest: PVariant; Source: PWideString); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    Dest^ := Source^;
end;

procedure _VariantFromAnsiChar(source: AnsiChar; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;
{$ENDIF}

procedure _VariantFromInterface(const source: IDispatch; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := Source;
end;

procedure _VariantFromShortString(Dest: PVariant; Source: PShortString); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, StringFromPShortString(Source))
  else
    Dest^ := StringFromPShortString(Source);
end;

procedure _VariantFromUnicString(Dest: PVariant; Source: PUnicString); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    Dest^ := Source^;
end;

procedure _VariantFromWideChar(source: WideChar; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromInt(source: Integer; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

{$IFDEF VARIANTS}
procedure _VariantFromInt64(dest: PVariant; source: PInt64); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    dest^ := source^;
end;
{$ELSE}
procedure _VariantFromInt64(dest: PVariant; source: PInt64); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Integer(Source^))
  else
    dest^ := Integer(source^);
end;
{$ENDIF}

procedure _VariantFromByte(source: Byte; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromBool(source: Boolean; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromWord(source: Word; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromCardinal(source: Cardinal; dest: PVariant); stdcall;
begin
{$IFDEF VARIANTS}
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
{$ELSE}
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Integer(Source))
  else
    dest^ := Integer(source);
{$ENDIF}
end;

procedure _VariantFromSmallInt(source: SmallInt; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromShortInt(source: ShortInt; dest: PVariant); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source)
  else
    dest^ := source;
end;

procedure _VariantFromDouble(dest: PVariant; source: PDouble); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    dest^ := source^;
end;

procedure _VariantFromCurrency(dest: PVariant; source: PCurrency); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    dest^ := source^;
end;

procedure _VariantFromSingle(dest: PVariant; source: PSingle); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    dest^ := source^;
end;

procedure _VariantFromExtended(dest: PVariant; source: PExtended); stdcall;
begin
  if JS_IsRef(dest^) then
    JS_PutValue(dest^, Source^)
  else
    dest^ := source^;
end;

procedure _VariantAssign(dest, source: PVariant); stdcall;
var
  IsRefSource, IsRefDest, IsObjectDest, IsObjectSource: Boolean;
  Y: TJS_Object;
  temp: Variant;
begin
  if VarIsNull(source^) then
  begin
    VarClear(dest^);
  end;

  IsRefSource := JS_IsRef(source^);
  IsRefDest := JS_IsRef(dest^);

  if IsRefSource and IsRefDest then
    JS_PutValue(dest^, JS_GetValue(Source^))
  else if IsRefDest then
    JS_PutValue(dest^, Source^)
  else if IsRefSource then
  begin
    temp := JS_GetValue(Source^);
    IsObjectDest := JS_IsObject(dest^);
    IsObjectSource := JS_IsObject(temp);
    if IsObjectDest and IsObjectSource then
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger),
                TJS_Object(TVarData(temp).VInteger))
    else if IsObjectDest then
    begin
      _ClassFromVariant(@Y, @temp);
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger), TGC_Object(Y));
      if TVarData(Dest^).VInteger <> 0 then
        TVarData(Dest^).VType := varClass;
    end
    else if IsObjectSource then
    begin
      Y := TJS_Object(TVarData(temp).VInteger);
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger), TGC_Object(Y));
      if TVarData(Dest^).VInteger <> 0 then
        TVarData(Dest^).VType := varClass;
    end
    else
      dest^ := temp;
  end
  else
  begin
    IsObjectDest := JS_IsObject(dest^);
    IsObjectSource := JS_IsObject(source^);
    if IsObjectDest and IsObjectSource then
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger),
                TJS_Object(TVarData(Source^).VInteger))
    else if IsObjectDest then
    begin
      _ClassFromVariant(@Y, @Source);
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger), TGC_Object(Y));
      if TVarData(Dest^).VInteger <> 0 then
        TVarData(Dest^).VType := varClass
    end
    else if IsObjectSource then
    begin
      Y := TJS_Object(TVarData(Source^).VInteger);
      GC_Assign(PGC_Object(@TVarData(Dest^).VInteger), Y);
      if TVarData(Dest^).VInteger <> 0 then
        TVarData(Dest^).VType := varClass;
    end
    else
      dest^ := source^;
  end;
end;

procedure _ClassAssign(dest, source: PObject); stdcall;
var
  IsGCSource, IsGCDest: Boolean;
begin
  if source^ = nil then
  begin
    if dest^ = nil then
      Exit;

    if dest^ is TGC_Object then
      GC_Assign(PGC_Object(dest), nil)
    else
    begin
      FreeAndNil(dest^);
    end;

    Exit;
  end;

  if dest^ = nil then
  begin
    if source^ = nil then
      Exit;
    if source^ is TGC_Object then
    begin
      TGC_Object(source^).AddRef;
      dest^ := source^;
    end
    else
      dest^ := source^;

    Exit;
  end;

  IsGCSource := source^ is TGC_Object;
  IsGCDest := dest^ is TGC_Object;

  if IsGCSource and IsGCDest then
    GC_Assign(PGC_Object(dest), TGC_Object(source))
  else
    dest^ := source^;
end;

// overriden routines - end

procedure _AssignProg(X: TJS_Object; P: TBaseRunner); stdcall;
begin
  X.prog := P;
  X.AddToGC;
end;

procedure _VariantFromClass(Dest: PVariant;
                            SourceAddress: Pointer); stdcall;
begin
  VarClear(dest^);
  with TVarData(dest^) do
  begin
    VType := varClass;
    VInteger := IntPax(SourceAddress^);
  end;
end;

procedure _ClassFromVariant(DestAddress: Pointer;
                            Source: PVariant); stdcall;
var
  V: Variant;
begin
  if TVarData(source^).VType = varClass then
  begin
    TObject(DestAddress^) := TObject(TVarData(source^).VInteger);
    if TObject(DestAddress^) is TJS_Reference then
    begin
      V := TJS_Reference(DestAddress^).GetValue();
      if TVarData(V).VType = varClass then
        TObject(DestAddress^) := TObject(TVarData(V).VInteger)
      else
       TObject(DestAddress^) := nil;
    end;
  end
  else
    TObject(DestAddress^) := nil;
end;


function GetVariantValue(Address: Pointer; FinTypeId: Integer): Variant;
begin
  case FinTypeId of
    typeBOOLEAN: result := Boolean(Address^);
    typeBYTE: result := Byte(Address^);
    typeWORD: result := Word(Address^);
    typeINTEGER: result := Integer(Address^);
    typeDOUBLE: result := Double(Address^);
    typePOINTER: result := Integer(Address^);
    typeENUM: result := Byte(Address^);
    typePROC: result := Integer(Address^);
{$IFNDEF PAXARM}
    typeANSICHAR: result := AnsiChar(Address^);
    typeANSISTRING: result := AnsiString(Address^);
    typeSHORTSTRING: result := ShortString(Address^);
    typeWIDESTRING: result := WideString(Address^);
{$ENDIF}
    typeSINGLE: result := Single(Address^);
    typeEXTENDED: result := Extended(Address^);
    typeCLASS:
    begin
      _VariantFromClass(@result, Address);
    end;
    typeCLASSREF: result := Integer(Address^);
    typeWIDECHAR: result := WideChar(Address^);
    typeVARIANT: result := Variant(Address^);
    typeDYNARRAY: result := Integer(Address^);
{$IFDEF VARIANTS}
    typeEVENT: result := Int64(Address^);
    typeINT64: result := Int64(Address^);
{$ELSE}
    typeINT64: result := Integer(Address^);
{$ENDIF}
    typeINTERFACE: result := Integer(Address^);
    typeCARDINAL: result := Cardinal(Address^);
    typeCURRENCY: result := Currency(Address^);
    typeSMALLINT: result := SmallInt(Address^);
    typeSHORTINT: result := ShortInt(Address^);
    typeWORDBOOL: result := WordBool(Address^);
    typeLONGBOOL: result := LongBool(Address^);
    typeBYTEBOOL: result := ByteBool(Address^);
    typeOLEVARIANT: result := OleVariant(Address^);
    typeUNICSTRING: result := UnicString(Address^);
  end;
end;

procedure PutVariantValue(Address: Pointer; FinTypeId: Integer; const value: Variant);
var
  X, Y: TObject;
begin
  case FinTypeId of
    typeBOOLEAN: Boolean(Address^) := value;
    typeBYTE: Byte(Address^) := value;
    typeWORD: Word(Address^) := value;
    typeINTEGER: Integer(Address^) := value;
    typeDOUBLE: Double(Address^) := value;
    typePOINTER: Integer(Address^) := value;
    typeENUM: Byte(Address^) := value;
    typePROC: Integer(Address^) := value;
{$IFNDEF PAXARM}
    typeSHORTSTRING: ShortString(Address^) := ShortString(value);
    typeANSICHAR: AnsiChar(Address^) := AnsiChar(Byte(value));
    typeANSISTRING: AnsiString(Address^) := AnsiString(value);
    typeWIDESTRING: WideString(Address^) := value;
{$ENDIF}
    typeSINGLE: Single(Address^) := value;
    typeEXTENDED: Extended(Address^) := value;
    typeCLASS:
    begin
      X := TObject(Address^);
      _ClassFromVariant(@Y, @value);
      if Y = nil then
      begin
        if X = nil then
          Exit;
        if X is TJS_Object then
        else
          TObject(Address^) := nil;
        Exit;
      end;
      if (X is TJS_Object) and (Y is TGC_Object) then
        GC_Assign(PGC_Object(Address), TGC_Object(Y))
      else
        TObject(Address^) := Y;
    end;
    typeCLASSREF: Integer(Address^) := value;
    typeWIDECHAR: WideChar(Address^) := WideChar(Word(value));
    typeVARIANT: Variant(Address^) := value;
    typeDYNARRAY: Integer(Address^) := value;
{$IFDEF VARIANTS}
    typeINT64: Int64(Address^) := value;
    typeEVENT: Int64(Address^) := value;
{$ELSE}
    typeINT64: Integer(Address^) := value;
{$ENDIF}
    typeINTERFACE: Integer(Address^) := value;
    typeCARDINAL: Cardinal(Address^) := value;
    typeCURRENCY: Currency(Address^) := value;
    typeSMALLINT: SmallInt(Address^) := value;
    typeSHORTINT: ShortInt(Address^) := value;
    typeWORDBOOL: WordBool(Address^) := value;
    typeLONGBOOL: LongBool(Address^) := value;
{$IFDEF FPC}
    typeBYTEBOOL:
    if value <> 0 then
      ByteBool(Address^) := true
    else
      ByteBool(Address^) := false;
{$ELSE}
    typeBYTEBOOL: ByteBool(Address^) := value;
{$ENDIF}
    typeOLEVARIANT: OleVariant(Address^) := value;
    typeUNICSTRING: UnicString(Address^) := value;
  end;
end;

procedure _VariantFromPointer(Dest: PVariant;
                              SourceAddress: Pointer); stdcall;
begin
  Dest^ := IntPax(SourceAddress^);
  TVarData(Dest^).VType := varPointer;
end;

procedure _ClearReferences(P: TBaseRunner); stdcall;
begin
  P.RootGC.ClearRef;
end;

procedure _ClassClr(Address: Pointer); stdcall;
begin
  if not (PObject(Address)^ is TGC_Object) then
  begin
    PObject(Address)^ := nil;
    Exit;
  end;

  GC_Assign(Address, nil);
end;

function _IsJSType(T: Integer; P: Pointer): Boolean;
var
  SymbolTable: TBaseSymbolTable;
begin
  result := (T = JS_ObjectClassId) or
            (T = JS_DateClassId) or
            (T = JS_ArrayClassId) or
            (T = JS_FunctionClassId) or
            (T = JS_MathClassId) or
            (T = JS_NumberClassId) or
            (T = JS_StringClassId) or
            (T = JS_ErrorClassId) or
            (T = JS_RegExpClassId) or
            (T = JS_BooleanClassId);
  if not result then
    if P <> nil then
    begin
      SymbolTable := TBaseSymbolTable(P);
      if SymbolTable[T].FinalTypeId <> typeCLASS then
        Exit;
      result := SymbolTable.Inherits(T, JS_ObjectClassId);
    end;
end;

const
  ByRef = true;

procedure Register_StdJavaScript(st: TBaseSymbolTable);
var
  H, G, H_Sub: Integer;
begin
  IsJSType := _IsJSType;

  with st do
  begin
{$IFNDEF PAXARM}
    RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_GetGenericProperty);
    JS_GetGenericPropertyId := LastSubId;
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_AssignProg);
    JS_AssignProgId := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    RegisterParameter(H_Sub, typeCLASS, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromClass);
    Id_VariantFromClass := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromPointer);
    Id_VariantFromPointer := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ClassFromVariant);
    Id_ClassFromVariant := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ClassAssign);
    Id_ClassAssign := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ClearReferences);
    JS_ClearReferencesId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ClassClr);
    Id_ClassClr := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned, ByRef);

    // overriden routines - begin

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantClr);
    Id_VariantClr := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromPAnsiChar);
    Id_VariantFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromAnsiString);
    Id_VariantFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWideString);
    Id_VariantFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromAnsiChar);
    Id_VariantFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromPWideChar);
    Id_VariantFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInterface);
    Id_VariantFromInterface := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromShortString);
    Id_VariantFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromUnicString);
    Id_VariantFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWideChar);
    Id_VariantFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInt);
    Id_VariantFromInt := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInt64);
    Id_VariantFromInt64 := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromByte);
    Id_VariantFromByte := LastSubId;
    RegisterParameter(H_Sub, typeBYTE, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromBool);
    Id_VariantFromBool := LastSubId;
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWord);
    Id_VariantFromWord := LastSubId;
    RegisterParameter(H_Sub, typeWORD, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromCardinal);
    Id_VariantFromCardinal := LastSubId;
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromSmallInt);
    Id_VariantFromSmallInt := LastSubId;
    RegisterParameter(H_Sub, typeSMALLINT, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromShortInt);
    Id_VariantFromShortInt := LastSubId;
    RegisterParameter(H_Sub, typeSHORTINT, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromDouble);
    Id_VariantFromDouble := LastSubId;
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromCurrency);
    Id_VariantFromCurrency := LastSubId;
    RegisterParameter(H_Sub, typeCURRENCY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromSingle);
    Id_VariantFromSingle := LastSubId;
    RegisterParameter(H_Sub, typeSINGLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromExtended);
    Id_VariantFromExtended := LastSubId;
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantAssign);
    Id_VariantAssign := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$IFNDEF PAXARM}
    RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_PutGenericProperty);
    JS_PutGenericPropertyId := LastSubId;
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayGet1);
    Id_VarArrayGet1 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayPut1);
    Id_VarArrayPut1 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayGet2);
    Id_VarArrayGet2 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayPut2);
    Id_VarArrayPut2 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayGet3);
    Id_VarArrayGet3 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VarArrayPut3);
    Id_VarArrayPut3 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    // overriden routines - end

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteObject);
    Id_WriteObject := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    H_WriteObject := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _FuncObjFromVariant);
    Id_FuncObjFromVariant := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _JS_ToObject);
    JS_ToObjectId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _JS_GetNextProp);
    JS_GetNextPropId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _JS_TypeOf);
    JS_TypeOfId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _JS_Void);
    JS_VoidId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _JS_Delete);
    JS_Delete := LastSubId;

    JS_TempNamespaceId := RegisterNamespace(0, StrJavaScriptTempNamespace);

    H := RegisterNamespace(0, StrJavaScriptNamespace);
    JS_JavaScriptNamespace := H;

    RegisterConstant(H, 'Undefined', Undefined);
    RegisterHeader(H,
      'procedure alert(const P1: Variant; const P2, P3, P4, P5: Variant = Undefined); stdcall;',
      @ _alert);
    Records[LastSubId].PushProgRequired := true;
    JS_AlertId := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_PushContext);
    Id_PushContext := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_PopContext);
    Id_PopContext := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_FindContext);
    Id_FindContext := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVARIANT, ccSTDCALL, @_FindFunc);
    JS_FindFuncId := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    RegisterConstant(H, 'null', typeVARIANT, Undefined);

// Object ----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Object);
    JS_ObjectClassId := G;
    Records[G].Name := 'Object';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G, 'constructor Create;, @TJS_Object.Create);',
      @TJS_Object.Create);

    RegisterHeader(G,
      'procedure PutProperty(PropName: PChar; const Value: Variant);',
      @TJS_Object.PutProperty);
    JS_PutPropertyId := LastSubId;

    RegisterHeader(G,
      'function GetProperty(PropName: PChar): Variant;',
      @TJS_Object.GetProperty);
    JS_GetPropertyId := LastSubId;

    RegisterHeader(G, 'property ___prop[PropertyName: PChar]: Variant read GetProperty write PutProperty; default;', nil);

    RegisterHeader(G,
      'function GetPropertyAsObject(PropName: PChar): TObject;',
      @ TJS_Object.GetPropertyAsObject);
    JS_GetPropertyAsObjectId := LastSubId;

    RegisterHeader(G,
      'procedure PutArrProperty(PropName: Integer; const Value: Variant);',
      @TJS_Object.PutArrProperty);
    JS_PutArrPropertyId := LastSubId;

    RegisterHeader(G,
      'function GetArrProperty(PropName: Integer): Variant;',
      @TJS_Object.GetArrProperty);
    JS_GetArrPropertyId := LastSubId;

    RegisterTypeField(G, 'prototype', JS_ObjectClassId, Integer(@TJS_Object(nil).prototype));
    RegisterTypeField(G, strInternalConstructor, JS_ObjectClassId, Integer(@TJS_Object(nil).aconstructor));
    RegisterTypeField(G, strProgram, typePOINTER, Integer(@TJS_Object(nil).prog));

    RegisterHeader(G,
      'function GetConstructor: Object;',
      @TJS_Object.GetConstructor);
    RegisterHeader(G,
      'property constructor: Object read GetConstructor write ' + strInternalConstructor + ';', nil);

//-- Date ----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Date);
    JS_DateClassId := G;
    Records[G].Name := 'Date';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const Year, Month, Date, Hours, Minutes, Seconds, Ms: Variant = Undefined);',
      @TJS_Date.Create);

// Array -----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Array);
    JS_ArrayClassId := G;

    Records[G].Name := 'Array';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const V: array of Variant);',
      @TJS_Array.Create);

    RegisterHeader(G, 'function GetLength: Integer;', @TJS_Array.GetLength);
    RegisterHeader(G, 'procedure SetLength(value: Integer);', @TJS_Array.SetLength);
    RegisterHeader(G, 'property length: Integer read GetLength write SetLength;', nil);

// Boolean ---------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Boolean);
    JS_BooleanClassId := G;
    Records[G].Name := 'Boolean';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const P: Variant = Undefined);',
      @TJS_Boolean.Create);

// Number ----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Number);
    JS_NumberClassId := G;

    Records[G].Name := 'Number';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const P: Variant = Undefined);',
      @TJS_Number.Create);

// String ----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_String);
    JS_StringClassId := G;

    Records[G].Name := 'String';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const P: Variant = Undefined);',
      @TJS_String.Create);

// Function --------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Function);
    JS_FunctionClassId := G;

    Records[G].Name := 'Function';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor ' + strInternalCreate + '(i_InternalFuncAddr: Pointer; i_NP: Integer; i_ProgPtr: Pointer);',
      @TJS_Function.InternalCreate);

    RegisterHeader(G,
      'function ' + strInternalCall + '(NP: Integer): Variant; stdcall;',
      @TJS_Function.InternalCall);

    JS_FunctionCallId := LastSubId;

    RegisterTypeField(G, 'arguments', JS_ArrayClassId, Integer(@TJS_Function(nil).arguments));
    RegisterTypeField(G, 'length', typeINTEGER, Integer(@TJS_Function(nil).InternalLength));
    RegisterTypeField(G, strInternalLength, typeINTEGER, Integer(@TJS_Function(nil).InternalLength));
    RegisterTypeField(G, strInternalFuncAddr, typePOINTER, Integer(@TJS_Function(nil).InternalFuncAddr));
    RegisterTypeField(G, str__this, typePOINTER, Integer(@TJS_Function(nil).__this));

{$IFDEF PAX64}
    ParArr_OFFSET := IntPax(@TJS_Function(nil).ParArr);
    RetAdr_OFFSET := IntPax(@TJS_Function(nil).RetAdr);
{$ENDIF}

//-- Math ----------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Math);
    JS_MathClassId := G;
    Records[G].Name := 'Math';
    Records[G].IsJavaScriptClass := true;

//-- RegExp --------------------------------------------------------------------

    G := RegisterClassType(H, TJS_RegExp);
    JS_RegExpClassId := G;
    Records[G].Name := 'RegExp';
    Records[G].IsJavaScriptClass := true;

    RegisterHeader(G, 'constructor Create(const Source: Variant = Undefined; const Modifiers: Variant = Undefined);',
      @TJS_RegExp.Create);
    RegisterHeader(G, 'destructor Destroy; override;',
      @TJS_RegExp.Destroy);
    RegisterHeader(G, 'function test(const InputString: Variant): Boolean;',
      @TJS_RegExp.Test);
    RegisterHeader(G, 'procedure compile;',
      @TJS_RegExp.Compile);
    RegisterHeader(G, 'function matchCount: Integer;',
      @TJS_RegExp.MatchCount);
    RegisterHeader(G, 'function exec(const InputString: Variant): Array;',
      @TJS_RegExp.Exec);
    RegisterHeader(G, 'function execute(const InputString: Variant): Array;',
      @TJS_RegExp.Execute);
    RegisterHeader(G, 'function replace(const Expression, ReplaceStr: Variant): String;',
      @TJS_RegExp.Replace);
    RegisterHeader(G, 'function __GetMatch(I: Integer): String;',
      @TJS_RegExp.GetMatch);
    RegisterHeader(G, 'function __GetMatchLen(I: Integer): Integer;',
      @TJS_RegExp.GetMatchLen);
    RegisterHeader(G, 'function __GetMatchPos(I: Integer): Integer;',
      @TJS_RegExp.GetMatchPos);
    RegisterHeader(G, 'property match[I: Integer]: Integer read __GetMatch;', nil);
    RegisterHeader(G, 'property matchPos[I: Integer]: Integer read __GetMatchPos;', nil);
    RegisterHeader(G, 'property matchLen[I: Integer]: Integer read __GetMatchLen;', nil);

// Error ---------------------------------------------------------------------

    G := RegisterClassType(H, TJS_Error);
    JS_ErrorClassId := G;
    Records[G].Name := 'Error';
    Records[G].IsJavaScriptClass := true;
    RegisterHeader(G,
      'constructor Create(const P: Variant = Undefined);',
      @TJS_Error.Create);
  end;
end;

initialization
  VarIntTypes := [varSmallint, varInteger,
                 varShortInt, varByte, varWord,
                 varLongWord];

  Randomize;
  MaxArgs := IntMaxArgs;
  CrtJSObjects := Create_JSObjects;
  EmptyFunction := TJS_Function.InternalCreate(nil, 0, nil);
finalization
  EmptyFunction.Free;
end.
