//////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_STDLIB.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
{$R-}
unit PAXCOMP_STDLIB;
interface
{$I-}
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,

{$IFDEF DRTTI}
  Rtti,
{$ENDIF}

  PaxInfos,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_CLASSLST,
  PAXCOMP_CLASSFACT,
{$IFDEF PAXARM}
{$ELSE}
  PAXCOMP_SEH,
{$ENDIF}
{$IFNDEF INTERPRETER_ONLY}
  PAXCOMP_PROGLIB,
{$ENDIF}

  PAXCOMP_MAP;
type
  TExtraImportTableList = class(TTypedList)
  private
    function GetRecord(I: Integer): TBaseSymbolTable;
  public
    function Add(st: TBaseSymbolTable): Integer;
    procedure Remove(st: TBaseSymbolTable);
    function Import(CurrentTable: TBaseSymbolTable;
                    const FullName: String;
                    UpCase: Boolean;
                    DoRaiseError: Boolean = true): Integer;
    property Records[I: Integer]: TBaseSymbolTable read GetRecord; default;
  end;
var
  RUNNER_OWNER_OFFSET: IntPax;

  AvailUnitList: TStringList;
  AvailUnitList1: TStringList;
  AvailTypeList: TStringList;

  H_PascalNamespace: Integer;
  H_BasicNamespace: Integer;
  H_PaxCompilerSEH: Integer;

  H_PaxCompilerFramework: Integer;

  H_Writeln: Integer;
  H_WriteBool: Integer;
  H_WriteByteBool: Integer;
  H_WriteWordBool: Integer;
  H_WriteLongBool: Integer;
  H_WriteInteger: Integer;
  H_WriteInt64: Integer;
  H_WriteByte: Integer;
  H_WriteWord: Integer;
  H_WriteCardinal: Integer;
  H_WriteSmallInt: Integer;
  H_WriteShortInt: Integer;
  H_WriteAnsiString: Integer;
  H_WriteShortString: Integer;
  H_WriteAnsiChar: Integer;
  H_WriteWideChar: Integer;
  H_WriteWideString: Integer;
  H_WriteUnicString: Integer;
  H_WriteDouble: Integer;
  H_WriteSingle: Integer;
  H_WriteCurrency: Integer;
  H_WriteExtended: Integer;
  H_WriteVariant: Integer;
  H_WriteObject: Integer;
  H_TObject: Integer;
  H_TClass: Integer;
  H_TInterfacedObject: Integer;

  H_PInteger: Integer;
  H_PSmallInt: Integer;
  H_PShortInt: Integer;
  H_PCardinal: Integer;
  H_PWord: Integer;
  H_PByte: Integer;
  H_PInt64: Integer;
  H_PSingle: Integer;
  H_PDouble: Integer;
  H_PExtended: Integer;
  H_PCurrency: Integer;
  H_PVariant: Integer;
  H_PPointer: Integer;
  H_PBoolean: Integer;
  H_PWideChar: Integer;
  H_PAnsiChar: Integer;
  H_PShortString: Integer;
  H_PAnsiString: Integer;
  H_PWideString: Integer;
  H_PUnicString: Integer;
  H_PString: Integer;

  H_PPInteger: Integer;
  H_PPSmallInt: Integer;
  H_PPShortInt: Integer;
  H_PPCardinal: Integer;
  H_PPWord: Integer;
  H_PPByte: Integer;
  H_PPInt64: Integer;
  H_PPSingle: Integer;
  H_PPDouble: Integer;
  H_PPExtended: Integer;
  H_PPCurrency: Integer;
  H_PPVariant: Integer;
  H_PPPointer: Integer;
  H_PPBoolean: Integer;
  H_PPWideChar: Integer;
  H_PPAnsiChar: Integer;
  H_PPShortString: Integer;
  H_PPAnsiString: Integer;
  H_PPWideString: Integer;
  H_PPUnicString: Integer;
  H_PPString: Integer;

  H_QueryInterface,
  H_AddRef,
  H_Release,

  H_TGUID: Integer;
  H_PGUID: Integer;
  H_IUnknown: Integer;
  H_IDispatch: Integer;

  Id_ImplicitInt: Integer = -1;

  Id_CallJNIMethod: Integer = -1;

  H_TValue: Integer = -1;
  Id_VarFromTValue: Integer = -1;
  Id_GetDRTTIProperty: Integer = -1;
  Id_GetDRTTIIntegerProperty: Integer = -1;
  Id_GetDRTTIStringProperty: Integer = -1;
  Id_GetDRTTIExtendedProperty: Integer = -1;
  Id_GetDRTTIVariantProperty: Integer = -1;
  Id_GetDRTTIInt64Property: Integer = -1;
  Id_SetDRTTIProperty: Integer = -1;

  H_TVarRec: Integer;
  H_TFileRec: Integer;
  H_TTextRec: Integer;
  H_Dynarray_TVarRec: Integer;

  H_Dynarray_Integer: Integer;
  H_Dynarray_Byte: Integer;
  H_Dynarray_Word: Integer;
  H_Dynarray_ShortInt: Integer;
  H_Dynarray_SmallInt: Integer;
  H_Dynarray_Cardinal: Integer;
  H_Dynarray_Int64: Integer;
  H_Dynarray_UInt64: Integer;
  H_Dynarray_AnsiChar: Integer;
  H_Dynarray_WideChar: Integer;
  H_Dynarray_AnsiString: Integer;
  H_Dynarray_WideString: Integer;
  H_Dynarray_UnicodeString: Integer;
  H_Dynarray_ShortString: Integer;
  H_Dynarray_Double: Integer;
  H_Dynarray_Single: Integer;
  H_Dynarray_Extended: Integer;
  H_Dynarray_Currency: Integer;
  H_Dynarray_Boolean: Integer;
  H_Dynarray_ByteBool: Integer;
  H_Dynarray_WordBool: Integer;
  H_Dynarray_LongBool: Integer;
  H_Dynarray_Variant: Integer;
  H_Dynarray_OleVariant: Integer;
  H_Dynarray_Pointer: Integer;

  H_Unassigned: Integer;

  H_TMethod: Integer;

  H_DYN_VAR: Integer;
  Id_CallVirt: Integer;
  Id_PutVirt: Integer;

  Id_CondHalt: Integer;

  Id_ToParentClass: Integer;
  Id_UpdateInstance: Integer;
  Id_DestroyInherited: Integer;

  ID_Prog: Integer;

  Id_GetAddressGetCallerEIP: Integer;
  Id_WriteBool: Integer;
  Id_WriteByteBool: Integer;
  Id_WriteWordBool: Integer;
  Id_WriteLongBool: Integer;
  Id_WriteAnsiChar: Integer;
  Id_WriteByte: Integer;
  Id_WriteWord: Integer;
  Id_WriteCardinal: Integer;
  Id_WriteSmallInt: Integer;
  Id_WriteShortInt: Integer;
  Id_WriteInt: Integer;
  Id_WriteInt64: Integer;
  Id_WriteDouble: Integer;
  Id_WriteSingle: Integer;
  Id_WriteCurrency: Integer;
  Id_WriteExtended: Integer;
  Id_WriteString: Integer;
  Id_WriteShortString: Integer;
  Id_WriteWideChar: Integer;
  Id_WriteWideString: Integer;
  Id_WriteUnicString: Integer;
  Id_WriteVariant: Integer;
  Id_WriteObject: Integer;

  Id_PrintEx: Integer;
  Id_Is: Integer;

  Id_DynArrayLength: Integer;
  Id_VariantArrayLength: Integer;
  Id_AnsiStringLength: Integer;

  Id_SetVariantLength: Integer;
  Id_SetVariantLength2: Integer;
  Id_SetVariantLength3: Integer;
  Id_SetStringLength: Integer;
  Id_SetWideStringLength: Integer;
  Id_SetUnicStringLength: Integer;
  Id_SetShortStringLength: Integer;

  Id_LockVArray: Integer;
  Id_UnlockVArray: Integer;

  Id_LoadProc: Integer;
  Id_LoadSeg: Integer;
  Id_LoadClassRef: Integer;

  Id_TypeInfo,
  Id_GetDynamicMethodAddress,
  Id_AddMessage,

  Id_AnsiStringFromPAnsiChar: Integer;
  Id_AnsiStringFromPWideChar: Integer;
  Id_WideStringFromPAnsiChar: Integer;
  Id_WideStringFromPWideChar: Integer;
  Id_UnicStringFromPWideChar: Integer;
  Id_AnsiStringFromAnsiChar: Integer;
  Id_WideStringFromAnsiChar: Integer;
  Id_UnicStringFromAnsiChar: Integer;
  Id_WideStringFromWideChar: Integer;
  Id_UnicStringFromWideChar: Integer;
  Id_AnsiStringFromWideChar: Integer;

  Id_UnicStringFromPAnsiChar: Integer;

  Id_AnsiStringAssign: Integer;
  Id_AnsiStringAddition: Integer;
  Id_ShortStringAddition: Integer;
  Id_WideStringAddition: Integer;
  Id_UnicStringAddition: Integer;
  Id_ShortStringAssign: Integer;
  Id_WideStringAssign: Integer;
  Id_UnicStringAssign: Integer;
  Id_AnsiStringClr: Integer;
  Id_WideStringClr: Integer;
  Id_UnicStringClr: Integer;
  Id_InterfaceClr: Integer;
  Id_ClassClr: Integer = 0;

  Id_UniqueAnsiString: Integer = 0;
  Id_UniqueUnicString: Integer = 0;

  Id_StringAddRef: Integer;
  Id_WideStringAddRef: Integer;
  Id_UnicStringAddRef: Integer;
  Id_VariantAddRef: Integer;
  Id_DynarrayAddRef: Integer;
  Id_InterfaceAddRef: Integer;

  Id_ShortStringFromAnsiString: Integer;
  Id_ShortStringFromWideString: Integer;
  Id_ShortStringFromPWideChar: Integer;
  Id_ShortStringFromUnicString: Integer;
  Id_AnsiStringFromShortString: Integer;
  Id_UnicStringFromWideString: Integer;
  Id_WideStringFromShortString: Integer;
  Id_WideStringFromUnicString: Integer;
  Id_UnicStringFromShortString: Integer;
  Id_AnsiStringFromWideString: Integer;
  Id_AnsiStringFromUnicString: Integer;
  Id_WideStringFromAnsiString: Integer;
  Id_UnicStringFromAnsiString: Integer;
  Id_StrInt: Integer;
  Id_StrDouble: Integer;
  Id_StrSingle: Integer;
  Id_StrExtended: Integer;
  Id_DecStringCounter: Integer;
  Id_IncStringCounter: Integer;
  Id_AnsiStringEquality: Integer;
  Id_AnsiStringNotEquality: Integer;
  Id_ShortStringEquality: Integer;
  Id_ShortStringNotEquality: Integer;
  Id_WideStringEquality: Integer;
  Id_UnicStringEquality: Integer;
  Id_WideStringNotEquality: Integer;
  Id_UnicStringNotEquality: Integer;
  Id_ShortstringHigh: Integer;
  Id_AnsiStringGreaterThan: Integer;
  Id_AnsiStringGreaterThanOrEqual: Integer;
  Id_AnsiStringLessThan: Integer;
  Id_AnsiStringLessThanOrEqual: Integer;
  Id_ShortStringGreaterThan: Integer;
  Id_ShortStringGreaterThanOrEqual: Integer;
  Id_ShortStringLessThan: Integer;
  Id_ShortStringLessThanOrEqual: Integer;
  Id_WideStringGreaterThan: Integer;
  Id_UnicStringGreaterThan: Integer;
  Id_WideStringGreaterThanOrEqual: Integer;
  Id_UnicStringGreaterThanOrEqual: Integer;
  Id_WideStringLessThan: Integer;
  Id_UnicStringLessThan: Integer;
  Id_WideStringLessThanOrEqual: Integer;
  Id_UnicStringLessThanOrEqual: Integer;
  Id_Int64Multiplication: Integer;
  Id_Int64Division: Integer;
  Id_Int64Modulo: Integer;
  Id_Int64LeftShift: Integer;
  Id_Int64RightShift: Integer;
  Id_Int64LessThan: Integer;
  Id_Int64LessThanOrEqual: Integer;
  Id_Int64GreaterThan: Integer;
  Id_Int64GreaterThanOrEqual: Integer;
  Id_Int64Equality: Integer;
  Id_Int64NotEquality: Integer;
  Id_Int64Abs: Integer;

  Id_UInt64LessThan: Integer;
  Id_UInt64LessThanOrEqual: Integer;
  Id_UInt64GreaterThan: Integer;
  Id_UInt64GreaterThanOrEqual: Integer;

  Id_VariantClr: Integer;
  Id_VariantAssign: Integer;
  Id_VariantFromPAnsiChar: Integer;
  Id_VariantFromPWideChar: Integer;
  Id_VariantFromInterface: Integer;

  Id_OleVariantAssign: Integer;

  Id_ClassAssign: Integer;

  Id_VariantFromClass: Integer; // JS only
  Id_VariantFromPointer: Integer; // JS only
  Id_ClassFromVariant: Integer; // JS only

  Id_InterfaceFromClass,
  Id_InterfaceCast,
  Id_InterfaceAssign,

  Id_VariantFromAnsiString: Integer;
  Id_VariantFromWideString: Integer;
  Id_VariantFromUnicString: Integer;
  Id_VariantFromShortString: Integer;
  Id_VariantFromAnsiChar: Integer;
  Id_VariantFromWideChar: Integer;
  Id_VariantFromInt: Integer;
  Id_VariantFromInt64: Integer;
  Id_VariantFromByte: Integer;
  Id_VariantFromBool: Integer;
  Id_VariantFromWord: Integer;
  Id_VariantFromCardinal: Integer;
  Id_VariantFromSmallInt: Integer;
  Id_VariantFromShortInt: Integer;
  Id_VariantFromDouble: Integer;
  Id_VariantFromCurrency: Integer;
  Id_VariantFromSingle: Integer;
  Id_VariantFromExtended: Integer;

  Id_AnsiStringFromInt: Integer; // JS only
  Id_AnsiStringFromDouble: Integer; // JS only
  Id_AnsiStringFromSingle: Integer; // JS only
  Id_AnsiStringFromExtended: Integer; // JS only
  Id_AnsiStringFromBoolean: Integer; // JS only

  Id_UnicStringFromInt: Integer; // JS only
  Id_UnicStringFromDouble: Integer; // JS only
  Id_UnicStringFromSingle: Integer; // JS only
  Id_UnicStringFromExtended: Integer; // JS only
  Id_UnicStringFromBoolean: Integer; // JS only

  Id_FuncObjFromVariant: Integer; // JS only

  Id_PushContext: Integer; // JS only
  Id_PopContext: Integer; // JS only
  Id_FindContext: Integer; // JS only

  Id_AnsiCharFromVariant: Integer;
  Id_WideCharFromVariant: Integer;
  Id_AnsiStringFromVariant: Integer;
  Id_WideStringFromVariant: Integer;
  Id_UnicStringFromVariant: Integer;
  Id_ShortStringFromVariant: Integer;
  Id_DoubleFromVariant: Integer;
  Id_CurrencyFromVariant: Integer;
  Id_SingleFromVariant: Integer;
  Id_ExtendedFromVariant: Integer;
  Id_IntFromVariant: Integer;
  Id_Int64FromVariant: Integer;
  Id_ByteFromVariant: Integer;
  Id_WordFromVariant: Integer;
  Id_CardinalFromVariant: Integer;
  Id_SmallIntFromVariant: Integer;
  Id_ShortIntFromVariant: Integer;
  Id_BoolFromVariant: Integer;
  Id_ByteBoolFromVariant: Integer;
  Id_WordBoolFromVariant: Integer;
  Id_LongBoolFromVariant: Integer;
  Id_VariantAddition: Integer;
  Id_VariantSubtraction: Integer;
  Id_VariantMultiplication: Integer;
  Id_VariantDivision: Integer;
  Id_VariantIDivision: Integer;
  Id_VariantModulo: Integer;
  Id_VariantLeftShift: Integer;
  Id_VariantRightShift: Integer;
  Id_VariantAnd: Integer;
  Id_VariantOr: Integer;
  Id_VariantXor: Integer;
  Id_VariantLessThan: Integer;
  Id_VariantLessThanOrEqual: Integer;
  Id_VariantGreaterThan: Integer;
  Id_VariantGreaterThanOrEqual: Integer;
  Id_VariantEquality: Integer;
  Id_VariantNotEquality: Integer;
  Id_StructEquality: Integer;
  Id_StructNotEquality: Integer;
  Id_VariantNegation: Integer;
  Id_VariantAbs: Integer;
  Id_VariantNot: Integer;
  Id_VarArrayGet1: Integer;
  Id_VarArrayPut1: Integer;
  Id_VarArrayGet2: Integer;
  Id_VarArrayPut2: Integer;
  Id_VarArrayGet3: Integer;
  Id_VarArrayPut3: Integer;
  Id_DynarrayClr: Integer;
  Id_DynarrayAssign: Integer;
  Id_CreateEmptyDynarray: Integer;
  Id_DynarraySetLength: Integer;
  Id_DynarraySetLength2: Integer;
  Id_DynarraySetLength3: Integer;
  Id_DynarrayHigh: Integer;

  Id_DoubleMultiplication: Integer;
  Id_DoubleDivision: Integer;
  Id_DoubleAddition: Integer;
  Id_DoubleSubtraction: Integer;
  Id_DoubleNegation: Integer;

  Id_OleVariantFromVariant: Integer;
  Id_OleVariantFromPAnsiChar: Integer;
  Id_OleVariantFromPWideChar: Integer;
  Id_OleVariantFromAnsiString: Integer;
  Id_OleVariantFromWideString: Integer;
  Id_OleVariantFromUnicString: Integer;
  Id_OleVariantFromShortString: Integer;
  Id_OleVariantFromAnsiChar: Integer;
  Id_OleVariantFromWideChar: Integer;
  Id_OleVariantFromInt: Integer;
  Id_OleVariantFromInt64: Integer;
  Id_OleVariantFromByte: Integer;
  Id_OleVariantFromBool: Integer;
  Id_OleVariantFromWord: Integer;
  Id_OleVariantFromCardinal: Integer;
  Id_OleVariantFromSmallInt: Integer;
  Id_OleVariantFromShortInt: Integer;
  Id_OleVariantFromDouble: Integer;
  Id_OleVariantFromCurrency: Integer;
  Id_OleVariantFromSingle: Integer;
  Id_OleVariantFromExtended: Integer;
  Id_OleVariantFromInterface: Integer;

  Id_GetComponent: Integer;

  Id_SetInclude: Integer;
  Id_SetIncludeInterval: Integer;
  Id_SetExclude: Integer;
  Id_SetUnion: Integer;
  Id_SetDifference: Integer;
  Id_SetIntersection: Integer;
  Id_SetSubset: Integer;
  Id_SetSuperset: Integer;
  Id_SetEquality: Integer;
  Id_SetInequality: Integer;
  Id_SetMembership: Integer;

  Id_ClassName: Integer;
  Id_OnCreateObject: Integer;
  Id_OnAfterObjectCreation: Integer;

  Id_OnCreateHostObject: Integer;
  Id_OnDestroyHostObject: Integer;

  Id_BeforeCallHost: Integer;
  Id_AfterCallHost: Integer;

  Id_GetAnsiStrProp: Integer;
  Id_SetAnsiStrProp: Integer;
  Id_GetWideStrProp: Integer;
  Id_SetWideStrProp: Integer;
  Id_GetUnicStrProp: Integer;
  Id_SetUnicStrProp: Integer;
  Id_GetOrdProp: Integer;
  Id_SetOrdProp: Integer;
  Id_GetInterfaceProp: Integer;
  Id_SetInterfaceProp: Integer;
  Id_GetSetProp: Integer;
  Id_SetSetProp: Integer;
  Id_GetFloatProp: Integer;
  Id_SetFloatProp: Integer;
  Id_GetVariantProp: Integer;
  Id_SetVariantProp: Integer;
  Id_GetInt64Prop: Integer;
  Id_SetInt64Prop: Integer;
  Id_GetEventProp: Integer;
  Id_SetEventProp: Integer;
  Id_SetEventProp2: Integer;
  Id_CreateMethod: Integer;

  Id_CreateObject: Integer;

  Id_TryOn: Integer;
  Id_TryOff: Integer;
  Id_Raise: Integer;
  Id_Exit: Integer;
  Id_Finally: Integer;
  Id_CondRaise: Integer;
  Id_BeginExceptBlock: Integer;
  Id_EndExceptBlock: Integer;
  Id_Pause: Integer;
  Id_Halt: Integer;

  Id_GetClassByIndex: Integer;

  Id_CheckPause: Integer;
  Id_InitSub: Integer;
  Id_EndSub: Integer;

  Id_IntOver: Integer;
  Id_BoundError: Integer;

  Id_AssignTVarRec: Integer;
  Id_TObject_Destroy: Integer;

  Id_ErrAbstract: Integer;
  Id_RecordAssign: Integer;

  Id_TObject_Free: Integer;
  Id_TObject_GetInterface: Integer;

  CURR_FMUL_ID: Integer;

  Id_TDateTime: integer = 0;
  H_TFW_Object: Integer = 0;
  H_TFW_Boolean: Integer = 0;
  H_TFW_ByteBool: Integer = 0;
  H_TFW_WordBool: Integer = 0;
  H_TFW_LongBool: Integer = 0;
  H_TFW_Byte: Integer = 0;
  H_TFW_SmallInt: Integer = 0;
  H_TFW_ShortInt: Integer = 0;
  H_TFW_Word: Integer = 0;
  H_TFW_Cardinal: Integer = 0;
  H_TFW_Double: Integer = 0;
  H_TFW_Single: Integer = 0;
  H_TFW_Extended: Integer = 0;
  H_TFW_Currency: Integer = 0;
  H_TFW_AnsiChar: Integer = 0;
  H_TFW_WideChar: Integer = 0;
  H_TFW_Integer: Integer = 0;
  H_TFW_Int64: Integer = 0;
  H_TFW_Variant: Integer = 0;
  H_TFW_DateTime: Integer = 0;
  H_TFW_AnsiString: Integer = 0;
  H_TFW_UnicString: Integer = 0;
  H_TFW_Array: Integer = 0;
  FWArrayOffset: Integer = 0;
  Id_FWArray_Create: Integer = 0;
  Id_FWArray_GetLength: Integer = 0;

  Id_ToFWObject: Integer = 0;
  Id_PaxSEHHandler: Integer = 0;

  Id_InitFWArray: Integer = 0;

{$IFDEF TRIAL}
var strShowTrial: array[0..10] of Char;
{$ENDIF}
  GlobalSymbolTable: TBaseSymbolTable;
  GlobalImportTable: TBaseSymbolTable;
  GlobalExtraImportTableList: TExtraImportTableList;
  List: TExtraImportTableList;

type
  TMyInterfacedObject = class(TInterfacedObject);
procedure TMyInterfacedObject_AddRef(Self: TObject); stdcall;
procedure TMyInterfacedObject_Release(Self: TObject); stdcall;

{$IFNDEF PAXARM}
procedure _SetAnsiStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: AnsiString); stdcall;
procedure _SetWideStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: WideString); stdcall;
procedure _WideStringFromPWideChar(source: PWideChar; var dest: WideString); stdcall;
{$ENDIF}
procedure _SetUnicStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: UnicString); stdcall;

procedure _DynarrayClr(var A: Pointer;
                       FinalTypeID, TypeID, ElSize,
                       FinalTypeID2, TypeID2, ElSize2: Integer); stdcall;
procedure _DynarrayAssign(var Source, Dest: Pointer;
                          FinalTypeID, TypeID, ElSize,
                          FinalTypeID2, TypeID2, ElSize2: Integer); stdcall;

procedure _DynarraySetLength(var A: Pointer; L: Integer;
                             ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;
procedure _DynarraySetLength2(var A: Pointer; L1, L2: Integer;
                              ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;
procedure _DynarraySetLength3(var A: Pointer; L1, L2, L3: Integer;
                              ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;

procedure _DynarrayClr1(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
procedure _DynarrayClr2(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
procedure _DynarrayClr3(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
function _DynarrayLength(P: Pointer): Integer;
procedure _ClearTVarRec(var Dest: TVarRec);
procedure _SetVariantLength(var V: Variant; VType: Integer; L: Integer); stdcall;
procedure _SetVariantLength2(var V: Variant; VType: Integer; L1, L2: Integer); stdcall;
procedure _SetVariantLength3(var V: Variant; VType: Integer; L1, L2, L3: Integer); stdcall;

procedure AddStdRoutines(st: TBaseSymbolTable);

type
  DynarrayPointer = array of Pointer;

var
  Import_TValue: procedure(Level: Integer; st: TBaseSymbolTable) = nil;

procedure _InterfaceFromClass(Dest: PIUnknown;
                              GUID: PGUID;
                              SourceAddress: Pointer); stdcall;
procedure _InterfaceCast(Dest: PIUnknown;
                         GUID: PGUID;
                         Source: PIUnknown); stdcall;
function GetPaxInterface(Self: TObject; const GUID: TGUID; var obj: Pointer): Boolean;
function UpdateSet(const S: TByteSet; L: Integer): TByteSet;

procedure _PrintEx(PP: Pointer;
                   Address: Pointer;
                   Kind: Integer;
                   FT: Integer;
                   L1, L2: Integer); stdcall;
procedure _AssignTVarRec(P: Pointer;
                         Address: Pointer;
                         var Dest: TVarRec;
                         TypeID: Integer); stdcall;
function _LoadProc(Runner: Pointer;
                   ProcHandle: Integer;
                   ProcName: PChar;
                   DllName: PChar;
                   OverCount: Integer): Pointer; stdcall;
procedure _Halt(Runner: Pointer; ExitCode: Integer); stdcall;
procedure _AddMessage(Runner: Pointer; msg_id: Integer; FullName: PChar); stdcall;
procedure _OnAfterObjectCreation(Runner: Pointer; Instance: PObject); stdcall;
procedure _TypeInfo(Prog: Pointer; FullTypeName: PChar; var result: PTypeInfo); stdcall;
{$IFNDEF PAXARM}
function _InitInstance(Self: TClass; Instance: Pointer): TObject;
{$ENDIF}
procedure _ToParentClass2(Instance: TObject); stdcall;
procedure _UpdateInstance2(Instance: TObject; C: TClass); stdcall;
procedure _SetInclude(S: PByteSet; value: Integer); stdcall;
procedure _SetIncludeInterval(S: PByteSet; B1, B2: Integer); stdcall;
procedure _ShortStringAssign(const source: ShortString; Ldest: Integer; dest: PShortString);
stdcall;
procedure _CreateEmptyDynarray(var A: Pointer); stdcall;
procedure _GetComponent(X: TComponent; I: Integer; var Y: TComponent); stdcall;

function GetPointerType(T: Integer): Integer;

procedure FindAvailTypes;

implementation

uses
{$IFDEF DRTTI}
  PAXCOMP_2010,
  PAXCOMP_2010Reg,
{$ENDIF}
  PAXCOMP_BASERUNNER,
  PAXCOMP_JavaScript,
  PAXCOMP_Basic,
  PAXCOMP_GC,
  PAXCOMP_FRAMEWORK,
  PAXCOMP_TYPEINFO,
  Math;

{$IFDEF TRIAL}
procedure ShowTrial;
var a: array[0..50] of ansichar;
    b: array[0..20] of ansichar;
begin
  a[00] := 'T';
  a[01] := 'h';
  a[02] := 'i';
  a[03] := 's';
  a[04] := ' ';
  a[05] := 'i';
  a[06] := 's';
  a[07] := ' ';
  a[08] := 'a';
  a[09] := 'n';
  a[10] := ' ';
  a[11] := 'e';
  a[12] := 'v';
  a[13] := 'a';
  a[14] := 'l';
  a[15] := 'u';
  a[16] := 'a';
  a[17] := 't';
  a[18] := 'i';
  a[19] := 'o';
  a[20] := 'n';
  a[21] := ' ';
  a[22] := 'c';
  a[23] := 'o';
  a[24] := 'p';
  a[25] := 'y';
  a[26] := ' ';
  a[27] := 'o';
  a[28] := 'f';
  a[29] := ' ';
  a[30] := 'p';
  a[31] := 'a';
  a[32] := 'x';
  a[33] := 'C';
  a[34] := 'o';
  a[35] := 'm';
  a[36] := 'p';
  a[37] := 'i';
  a[38] := 'l';
  a[39] := 'e';
  a[40] := 'r';
  a[41] := '.';
  a[42] :=  #0;

  b[00] := 'p';
  b[01] := 'a';
  b[02] := 'x';
  b[03] := 'C';
  b[04] := 'o';
  b[05] := 'm';
  b[06] := 'p';
  b[07] := 'i';
  b[08] := 'l';
  b[09] := 'e';
  b[10] := 'r';
  b[11] := #0;

{$IFDEF LINUX}
{$IFDEF FPC}
// I will give you a better call for FPC/LINUX //
{$ELSE}
  Application.MessageBox(PChar(S), 'paxCompiler', [smbOK]);
{$ENDIF}
{$ELSE}
{$IFDEF UNIX}
  Writeln('Error '+S);
{$ELSE}
  MessageBox(GetActiveWindow(), PChar(String(a)), PChar(String(b)),
  MB_ICONEXCLAMATION or MB_OK);
{$ENDIF}
{$ENDIF}

end;
{$ENDIF}

procedure _BeginSub;
begin
end;

procedure _Write;
begin
end;

procedure _Writeln;
begin
  writeln;
end;

procedure _WriteBool(value: Boolean);
begin
  write(value);
end;

procedure _WriteByteBool(value: ByteBool);
begin
  write(value);
end;

procedure _WriteWordBool(value: WordBool);
begin
  write(value);
end;

procedure _WriteLongBool(value: LongBool);
begin
  write(value);
end;

{$IFDEF VARIANTS}
procedure _WriteWideChar(value: WideChar);
begin
  write(value);
end;
{$ELSE}
procedure _WriteWideChar(value: WideChar);
var
  S: AnsiString;
begin
  S := value;
  write(S);
end;
{$ENDIF}

procedure _WriteByte(value: Byte; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteWord(value: Word; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteCardinal(value: Cardinal; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteSmallInt(value: SmallInt; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteShortInt(value: ShortInt; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteInt(value: Integer; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteInt64(value: Int64; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteDouble(value: Double; L1, L2: Integer);
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      write(value:L1:L2)
    else
      write(value:L1);
  end
  else
    write(value);
end;

procedure _WriteSingle(value: Single; L1, L2: Integer);
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      write(value:L1:L2)
    else
      write(value:L1);
  end
  else
    write(value);
end;

procedure _WriteCurrency(value: Currency; L1, L2: Integer);
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      write(value:L1:L2)
    else
      write(value:L1);
  end
  else
    write(value);
end;

procedure _WriteExtended(value: Extended; L1, L2: Integer);
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      write(value:L1:L2)
    else
      write(value:L1);
  end
  else
    write(value);
end;

{$IFNDEF PAXARM}

procedure _WriteString(const value: AnsiString; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;

procedure _WriteAnsiChar(value: AnsiChar);
begin
  write(value);
end;

procedure _WriteShortString(const value: ShortString; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;
{$ENDIF}

{$IFDEF VARIANTS}
{$IFNDEF PAXARM}
procedure _WriteWideString(const value: WideString; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;
{$ENDIF}
{$ELSE}
procedure _WriteWideString(const value: WideString; L: Integer);
var
  S: AnsiString;
begin
  S := value;
  if L = 0 then
    write(S)
  else
    write(S:L);
end;
{$ENDIF}

{$IFDEF VARIANTS}
procedure _WriteUnicString(const value: UnicString; L: Integer);
begin
  if L = 0 then
    write(value)
  else
    write(value:L);
end;
{$ELSE}
procedure _WriteUnicString(const value: UnicString; L: Integer);
var
  S: AnsiString;
begin
  S := value;
  if L = 0 then
    write(S)
  else
    write(S:L);
end;
{$ENDIF}

procedure _WriteVariant(const value: Variant; L1, L2: Integer);
var
{$IFDEF PAXARM}
  S: String;
{$ELSE}
  {$IFDEF FPC}
    S: String;
  {$ELSE}
    S: ShortString;
  {$ENDIF}
{$ENDIF}
begin
  if L1 > 0 then
  begin
{$IFDEF PAXARM}
    S := VarToStr(value);
{$ELSE}
{$IFDEF FPC}
    S := VarToStr(value);
{$ELSE}
    if L2 > 0 then
      STR(value:L1:L2, S)
    else
      STR(value:L1, S);
    write(S);
{$ENDIF}
{$ENDIF}
  end
  else
  begin
    if VarType(value) = varEmpty then
      write('undefined')
    else
      write(VarToStr(value));
  end;
end;

procedure _WriteObject(const value: TObject);
begin
  write('[' + value.ClassName + ']');
end;

{$IFDEF DRTTI}
function CheckField(t: TRTTIType; f: TRTTIField): Boolean;
begin
  result := false;
  if not CheckType(f.FieldType) then
    Exit;
  if f.Parent <> t then
    Exit;
  result := true;
end;
{$ENDIF}

type
  TStdPrintHandler = class
  public
    S: String;
    procedure DoPrintClassTypeField(Sender: TObject;
      const Infos: TPrintClassTypeFieldInfo);
    procedure DoPrintClassTypeProp(Sender: TObject;
      const Infos: TPrintClassTypePropInfo);
  end;

procedure TStdPrintHandler.DoPrintClassTypeField(Sender: TObject;
                     const Infos: TPrintClassTypeFieldInfo);
begin
  if Infos.FieldIndex = 0 then
  begin
    if Infos.Started then
      S := '('
    else
      S := S + '(';
  end;
  S := S + Infos.FieldName;
  S := S + ': ';
  S := S + ScalarValueToString(Infos.Address, Infos.TypeId);
  if Infos.FieldIndex = Infos.FieldCount - 1 then
  begin
    S := S + ')';
    if Infos.Finished then
      Exit;
  end
  else
    S := S + '; ';
end;

procedure TStdPrintHandler.DoPrintClassTypeProp(Sender: TObject;
                     const Infos: TPrintClassTypePropInfo);
begin
  if Infos.PropIndex = 0 then
  begin
    if Infos.Started then
      S := '('
    else
      S := S + '(';
  end;
  S := S + Infos.PropName;
  S := S + ': ';
  S := S + Infos.StrValue;
  if Infos.PropIndex = Infos.PropCount - 1 then
  begin
    S := S + ')';
    if Infos.Finished then
      Exit;
  end
  else
    S := S + '; ';
end;

procedure _PrintEx(PP: Pointer;
                   Address: Pointer;
                   Kind: Integer;
                   FT: Integer;
                   L1, L2: Integer); stdcall;
var
  S: String;
{$IFDEF PAXARM}
  SS: String;
  type ShortString = string;
  var
{$ELSE}
  SS: ShortString;
{$ENDIF}
{$IFDEF DRTTI}
  ClassTypeInfoContainer: TClassTypeInfoContainer;
  ClassTypeDataContainer: TClassTypeDataContainer;
  FieldInfos: TPrintClassTypeFieldInfo;
  PropInfos: TPrintClassTypePropInfo;
  f: TRTTIField;
  t: TRTTIType;
  K: Integer;
  ptd: PTypeData;
  I: Integer;
  pti: PTypeInfo;
  nProps: Integer;
  pProps: PPropList;
  ppi: PPropInfo;
{$ENDIF}
  X: TObject;
  P: TBaseRunner;
label
  ByRef;
begin
  P := TBaseRunner(PP);

  if Assigned(P) then
  if Assigned(P.OnPrintEx) then
  begin
    P.OnPrintEx(P.Owner, Address, Kind, FT, L1, L2);
    Exit;
  end;

try
  if Kind = KindCONST then
  begin
{$IFNDEF PAXARM}
    if FT = typeANSICHAR then
    begin
      S := String(AnsiChar(Byte(Address)));
    end
    else
{$ENDIF}
    if FT = typeWIDECHAR then
    begin
      S := String(WideChar(Word(Address)));
    end
    else if FT in UnsignedIntegerTypes then
    begin
      if L1 > 0 then
        STR(Cardinal(Address):L1, SS)
      else
        STR(Cardinal(Address), SS);
      S := String(SS);
    end
    else if FT in OrdinalTypes then
    begin
      if L1 > 0 then
        STR(Integer(Address):L1, SS)
      else
        STR(Integer(Address), SS);
      S := String(SS);
    end
{$IFNDEF PAXARM}
    else if FT = typePANSICHAR then
    begin
      S := String(StrPas(PAnsiChar(Address)));
    end
{$ENDIF}
    else if FT = typePWIDECHAR then
    begin
      S := String(UnicString(PWideChar(Address)));
    end
    else
      goto ByRef;
  end
  else // Kind = KindVAR
  begin

ByRef:

    case FT of
      typeBOOLEAN: if Boolean(Address^) then
                     S := 'true'
                   else
                     S := 'false';
      typeBYTE, typeENUM:
      begin
        if L1 > 0 then
          STR(Byte(Address^):L1, SS)
        else
          STR(Byte(Address^), SS);
        S := String(SS);
      end;
{$IFNDEF PAXARM}
      typeANSICHAR: S := String(AnsiChar(Address^));
      typeANSISTRING:
        begin
          S := String(AnsiString(Address^));
          while Length(S) < L1 do
            S := S + ' ';
        end;
{$ENDIF}
      typeUNICSTRING:
        begin
          S := String(UnicString(Address^));
          while Length(S) < L1 do
            S := S + ' ';
        end;
{$IFNDEF PAXARM}
      typePANSICHAR:
        S := String(StrPas(PAnsiChar(Address^)));
{$ENDIF}
      typePWIDECHAR:
        S := String(PWideChar(Address^));
      typeWORD:
      begin
        if L1 > 0 then
          STR(Word(Address^):L1, SS)
        else
          STR(Word(Address^), SS);
        S := String(SS);
      end;
      typeINTEGER:
      begin
        if L1 > 0 then
          STR(Integer(Address^):L1, SS)
        else
          STR(Integer(Address^), SS);
        S := String(SS);
      end;
      typeDOUBLE:
      begin
        if (L1 > 0) and (L2 > 0) then
          STR(Double(Address^):L1:L2, SS)
        else if L1 > 0 then
          STR(Double(Address^):L1, SS)
        else
          STR(Double(Address^), SS);
        S := String(SS);
      end;
      typePOINTER: S := String(Format('%x', [Cardinal(Address^)]));
      typeRECORD: S := '[record]';
      typeARRAY: S := '[array]';
      typePROC: S := '[proc]';
      typeSET: S := '[set]';
{$IFNDEF PAXARM}
      typeSHORTSTRING:
        begin
          S := String(ShortString(Address^));
          while Length(S) < L1 do
            S := S + ' ';
        end;
{$ENDIF}
      typeSINGLE:
      begin
        if (L1 > 0) and (L2 > 0) then
          STR(Single(Address^):L1:L2, SS)
        else if L1 > 0 then
          STR(Single(Address^):L1, SS)
        else
          STR(Single(Address^), SS);
        S := String(SS);
      end;
      typeEXTENDED:
      begin
        if (L1 > 0) and (L2 > 0) then
        begin
          STR(Extended(Address^):L1:L2, SS);
        end
        else if L1 > 0 then
        begin
          STR(Extended(Address^):L1, SS);
        end
        else
          SS := ShortString(FloatToStr(Extended(Address^)));
        S := String(SS);
      end;
      typeCLASS:
        if Integer(Address^) = 0 then
          S := 'nil'
        else if TObject(Address^).InheritsFrom(TFW_Object) then
        begin
          S := TFW_Object(Address^)._ToString;
        end
        else
        begin
          X := TObject(Address^);
{$IFDEF DRTTI}
          ClassTypeInfoContainer :=
            GetClassTypeInfoContainer(X);
          if ClassTypeInfoContainer <> nil then
          begin
            try
              ClassTypeDataContainer :=
                ClassTypeInfoContainer.TypeDataContainer as
                TClassTypeDataContainer;

              with ClassTypeDataContainer.FieldListContainer do
              for I := 0 to Count - 1 do
              begin
                Address := ShiftPointer(Pointer(X),
                  Records[I].Offset);

                if Records[I].FinalFieldTypeId = typeCLASS then
                begin
                  _PrintEx(P, Address, KindTYPE_FIELD,
                      Records[I].FinalFieldTypeId, 0, 0);
                end
                else
                begin
                  FieldInfos.Host := false;

                  FieldInfos.Started := false;
                  FieldInfos.Finished := false;
                  if I = 0 then
                    if Kind = KindVAR then
                      FieldInfos.Started := true;

                  FieldInfos.Owner := X;
                  FieldInfos.FieldIndex := I;
                  FieldInfos.FieldCount := Count;
                  FieldInfos.Address := Address;
                  FieldInfos.FieldName := StringFromPShortString(@Records[I].Name);
                  FieldInfos.TypeId := Records[I].FinalFieldTypeId;
                  FieldInfos.Visibility := GetVisibility(Records[I].Vis);
                  if I = Count - 1 then
                    FieldInfos.Finished := true;

                  if Assigned(P) then
                  if Assigned(P.OnPrintClassTypeField) then
                  begin
                    P.OnPrintClassTypeField(P.Owner, FieldInfos);
                  end;
                end;
              end;

              with ClassTypeDataContainer.AnotherFieldListContainer do
              for I := 0 to Count - 1 do
              begin
                Address := ShiftPointer(Pointer(X),
                  Records[I].Offset);

                if Records[I].FinalFieldTypeId = typeCLASS then
                begin
                  _PrintEx(P, Address, KindTYPE_FIELD,
                      Records[I].FinalFieldTypeId, 0, 0);
                end
                else
                begin
                  FieldInfos.Host := false;

                  FieldInfos.Started := false;
                  FieldInfos.Finished := false;
                  if I = 0 then
                    if Kind = KindVAR then
                      FieldInfos.Started := true;

                  FieldInfos.Owner := X;
                  FieldInfos.FieldIndex := I;
                  FieldInfos.FieldCount := Count;
                  FieldInfos.Address := Address;
                  FieldInfos.FieldName := StringFromPShortString(@Records[I].Name);
                  FieldInfos.TypeId := Records[I].FinalFieldTypeId;
                  FieldInfos.Visibility := GetVisibility(Records[I].Vis);
                  if I = Count - 1 then
                    FieldInfos.Finished := true;

                  if Assigned(P) then
                  if Assigned(P.OnPrintClassTypeField) then
                  begin
                    P.OnPrintClassTypeField(P.Owner, FieldInfos);
                  end;
                end;
              end;

              // published properties

              pti := X.ClassInfo;
              if pti <> nil then
              begin
                ptd := GetTypeData(pti);
                nProps := ptd^.PropCount;
                GetMem(pProps, SizeOf(PPropInfo) * nProps);
                try
                  GetPropInfos(pti, pProps);
                  for I:=0 to nProps - 1 do
                  begin
                {$ifdef fpc}
                    ppi := pProps^[I];
                {$else}
                    ppi := pProps[I];
                {$endif}
                    case ppi^.PropType^.Kind of
                      tkClass:
                        begin
                          IntPax(X) := GetOrdProp(X, ppi);
                          _PrintEx(P, @X, KindTYPE_FIELD,
                              typeCLASS, 0, 0);
                        end;
                      else
                        begin
                          PropInfos.Host := false;

                          PropInfos.Started := false;
                          PropInfos.Finished := false;
                          if I = 0 then
                            if Kind = KindVAR then
                              PropInfos.Started := true;

                          PropInfos.Owner := X;
                          PropInfos.PropIndex := I;
                          PropInfos.PropCount := nProps;
                          PropInfos.StrValue :=
                            VarToStr(GetPropValue(X, ppi));
                          PropInfos.PropName := StringFromPShortString(@ppi^.Name);
                          PropInfos.Visibility := mvPublished;
                          if I = nProps - 1 then
                            PropInfos.Finished := true;

                          if Assigned(P) then
                          if Assigned(P.OnPrintClassTypeProp) then
                          begin
                            P.OnPrintClassTypeProp(P.Owner, PropInfos);
                          end;
                        end;
                    end;
                  end;
                finally
                  FreeMem(pProps, SizeOf(PPropInfo) * nProps);
                end;
              end;
            finally
              FreeAndNil(ClassTypeInfoContainer);
            end;
          end
          else // this is an object of host-defined class.
          begin
            t := PaxContext.GetType(X.ClassType);

            K := 0;

            for f in t.GetFields do
             if CheckField(t, f) then
               Inc(K);

            I := 0;
            for f in t.GetFields do
              if CheckField(t, f) then
              begin
                Inc(I);
                Address := ShiftPointer(Pointer(X),
                    f.Offset);

                if f.FieldType is TRTTIRecordType then
                begin
                  _PrintEx(P, Address, KindTYPE_FIELD,
                      typeRECORD, 0, 0);
                end
                else if f.FieldType is TRTTIInstanceType then
                begin
                  _PrintEx(P, Address, KindTYPE_FIELD,
                      typeCLASS, 0, 0);
                end
                else
                begin
                  FieldInfos.Host := true;
                  FieldInfos.Started := false;
                  FieldInfos.Finished := false;
                  if I = 0 then
                    if Kind = KindVAR then
                      FieldInfos.Started := true;
                  FieldInfos.Owner := X;
                  FieldInfos.FieldIndex := I;
                  FieldInfos.FieldCount := K;
                  FieldInfos.Address := Address;
                  FieldInfos.FieldName := f.Name;
                  FieldInfos.FieldTypeName := f.FieldType.Name;
                  FieldInfos.TypeId := PtiToFinType(f.FieldType.Handle);
                  FieldInfos.Visibility := f.Visibility;
                  if I = K - 1 then
                    FieldInfos.Finished := true;
                  if Assigned(P) then
                  if Assigned(P.OnPrintClassTypeField) then
                  begin
                    P.OnPrintClassTypeField(P.Owner, FieldInfos);
                  end;
                end;
              end;
          end;

{$ELSE}
          S := '[Object: ' + X.ClassName + ']';
{$ENDIF}
        end;
      typeCLASSREF:
        if Integer(Address^) = 0 then
          S := 'nil'
        else
          S := '[Class ref: ' + TClass(Address^).ClassName + ']';
      typeWIDECHAR: S := WideChar(Address^);
{$IFNDEF PAXARM}
      typeWIDESTRING: S := WideString(Address^);
{$ENDIF}
      typeVARIANT, typeOLEVARIANT:
      begin
        if TVarData(Variant(Address^)).VType = varDispatch then
          S := '[dispatch]'
        else
          if (L1 > 0) and (L2 > 0) then
          begin
            {$IFDEF FPC}
            S := VarToStr(Variant(Address^));
            {$ELSE}
            STR(Variant(Address^):L1:L2, SS);
            S := String(SS);
            {$ENDIF}
          end
          else if L1 > 0 then
          begin
            {$IFDEF FPC}
            S := VarToStr(Variant(Address^));
            {$ELSE}
            STR(Variant(Address^):L1, SS);
            S := String(SS);
            {$ENDIF}
          end
          else
            S := VarToStr(Variant(Address^));
      end;
      typeDYNARRAY: S := '[dynarray]';
      typeINT64:
      begin
        if L1 > 0 then
          STR(Int64(Address^):L1, SS)
        else
          STR(Int64(Address^), SS);
        S := String(SS);
      end;
      typeUINT64:
      begin
        if L1 > 0 then
          STR(UInt64(Address^):L1, SS)
        else
          STR(UInt64(Address^), SS);
        S := String(SS);
      end;
      typeINTERFACE: S := '[interface]';
      typeCARDINAL:
      begin
        if L1 > 0 then
          STR(Cardinal(Address^):L1, SS)
        else
          STR(Cardinal(Address^), SS);
        S := String(SS);
      end;
      typeEVENT: S := '[event]';
      typeCURRENCY:
      begin
        if (L1 > 0) and (L2 > 0) then
          STR(Currency(Address^):L1:L2, SS)
        else if L1 > 0 then
          STR(Currency(Address^):L1, SS)
        else
          STR(Currency(Address^), SS);
        S := String(SS);
      end;
      typeSMALLINT:
      begin
        if L1 > 0 then
          STR(SmallInt(Address^):L1, SS)
        else
          STR(SmallInt(Address^), SS);
        S := String(SS);
      end;
      typeSHORTINT:
      begin
        if L1 > 0 then
          STR(ShortInt(Address^):L1, SS)
        else
          STR(ShortInt(Address^), SS);
        S := String(SS);
      end;
      typeWORDBOOL: if WordBool(Address^) then
                      S := 'true'
                    else
                      S := 'false';
      typeLONGBOOL: if LongBool(Address^) then
                      S := 'true'
                    else
                      S := 'false';
      typeBYTEBOOL: if ByteBool(Address^) then
                      S := 'true'
                    else
                      S := 'false';
    else
      S := '';
    end;
  end;

except
  on E: Exception do
  begin
    S := E.Message;
    raise;
  end;
end;

  if Assigned(P) then
  if Assigned(P.OnPrint) then
  begin
    P.OnPrint(P.Owner, S);
    Exit;
  end;

{$IFDEF CONSOLE}
  write(S);
  Exit;
{$ELSE}
{$IFDEF PAXARM}
{$IFDEF PAXARM_DEVICE}
  ShowMessage(S);
{$ELSE}
  ShowMessage(S);
{$ENDIF}
{$ELSE}
{$IFDEF LINUX}
  ShowMessage(S);
{$ELSE}
  {$IFDEF MACOS32}
  ShowMessage(S);
  {$ELSE}
  MessageBox(GetActiveWindow(), PChar(String(S)), PChar('paxCompiler'), MB_ICONEXCLAMATION or MB_OK);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure _GetMem(var P: Pointer; Size: Integer);
begin
  GetMem(P, Size);
end;

procedure _FreeMem(P: Pointer; Size: Integer);
begin
  FreeMem(P, Size);
end;

// -------- ORDINAL FUNCTIONS --------------------------------------------------

function _Odd(X: Integer): Boolean;
begin
  result := Odd(X);
end;

//--------- SET ROUTINES ------------------------------------------------------

type
  TSetBytes = array[1..SizeOf(TByteSet)] of Byte;

function UpdateSet(const S: TByteSet; L: Integer): TByteSet;
var
  I: Integer;
begin
  FillChar(result, SizeOf(result), 0);
  for I := 1 to L do
    TSetBytes(result)[I] := TSetBytes(S)[I];
end;

procedure _SetInclude(S: PByteSet; value: Integer); stdcall;
begin
  if (value < 0) or (value > 255) then
    raise Exception.Create(errInvalidSet);
  Include(S^, value);
end;

procedure _SetIncludeInterval(S: PByteSet; B1, B2: Integer); stdcall;
var
  value: Integer;
begin
  if (B1 < 0) or (B1 > 255) then
    raise Exception.Create(errInvalidSet);
  if (B2 < 0) or (B2 > 255) then
    raise Exception.Create(errInvalidSet);
  for value:=B1 to B2 do
    Include(S^, value);
end;

procedure _SetExclude(var S: TByteSet; value: Integer); stdcall;
begin
  if (value < 0) or (value > 255) then
    raise Exception.Create(errInvalidSet);
  Exclude(S, value);
end;

procedure _SetUnion(var S1: TByteSet; var S2: TByteSet;
                    var R: TByteSet;
                    SZ1, SZ2: Integer); stdcall;
var
  L: Integer;
  Res: TByteSet;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  Res := UpdateSet(S1, L) + UpdateSet(S2, L);
  Move(Res, R, L);
end;

procedure _SetDifference(var S1: TByteSet; var S2: TByteSet;
                         var R: TByteSet;
                         SZ1, SZ2: Integer); stdcall;
var
  L: Integer;
  Res: TByteSet;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  Res := UpdateSet(S1, L) - UpdateSet(S2, L);
  Move(Res, R, L);
end;

procedure _SetIntersection(var S1: TByteSet; var S2: TByteSet;
                           var R: TByteSet;
                           SZ1, SZ2: Integer); stdcall;
var
  L: Integer;
  Res: TByteSet;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  Res := UpdateSet(S1, L) * UpdateSet(S2, L);
  Move(Res, R, L);
end;

function _SetSubset(var S1: TByteSet; var S2: TByteSet;
                    SZ1, SZ2: Integer): Boolean; stdcall;
var
  L: Integer;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  result := UpdateSet(S1, L) <= UpdateSet(S2, L);
end;

function _SetSuperset(var S1: TByteSet; var S2: TByteSet;
                      SZ1, SZ2: Integer): Boolean; stdcall;
var
  L: Integer;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  result := UpdateSet(S1, L) >= UpdateSet(S2, L);
end;

function _SetEquality(const S1: TByteSet; const S2: TByteSet;
                      SZ1, SZ2: Integer): Boolean; stdcall;
var
  L, I: Integer;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  result := true;
  for I := 1 to L do
    if TSetBytes(S1)[I] <> TSetBytes(S2)[I] then
    begin
      result := false;
      Exit;
    end;
end;

function _SetInequality(const S1: TByteSet; const S2: TByteSet;
                      SZ1, SZ2: Integer): Boolean; stdcall;
var
  L, I: Integer;
begin
  if SZ2 < SZ1 then
    L := SZ2
  else
    L := SZ1;
  result := false;
  for I := 1 to L do
    if TSetBytes(S1)[I] <> TSetBytes(S2)[I] then
    begin
      result := true;
      Exit;
    end;
end;

function _SetMembership(value: Integer; var S: TByteSet): Boolean; stdcall;
begin
  result := value in S;
end;

//--------- AnsiString ROUTINES ----------------------------------------------------

type
  PStringRec = ^TStringRec;
  TStringRec = packed record
    RefCount: Longint;
    Length: Longint;
  end;

{$IFNDEF PAXARM}
{$IFDEF MACOS32}
procedure _DecStringCounter(var S: AnsiString);
var
  P: PStringRec;
  D: Pointer;
begin
  D := Pointer(S);
  if D <> nil then
  begin
    P := PStringRec(Integer(D) - sizeof(TStringRec));
    if P^.RefCount > 0 then
    begin
      Dec(P^.RefCount);
      if P^.refCount = 0 then
        FreeMem(P);
    end;
  end;
end;
{$ELSE}
procedure _DecStringCounter(var S: AnsiString);
var
  P: PStringRec;
  D: Pointer;
begin
  D := Pointer(S);
  if D <> nil then
  begin
    P := PStringRec(Integer(D) - sizeof(TStringRec));
    if P^.RefCount > 0 then
      if InterlockedDecrement(P^.refCount) = 0 then
        FreeMem(P);
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF PAXARM}
{$IFDEF MACOS32}
procedure _IncStringCounter(var S: AnsiString);
var
  P: PStringRec;
  D: Pointer;
begin
  D := Pointer(S);
  if D <> nil then
  begin
    P := PStringRec(Integer(D) - sizeof(TStringRec));
    Inc(P^.RefCount);
  end;
end;
{$ELSE}
procedure _IncStringCounter(var S: AnsiString);
var
  P: PStringRec;
  D: Pointer;
begin
  D := Pointer(S);
  if D <> nil then
  begin
    P := PStringRec(Integer(D) - sizeof(TStringRec));
    InterlockedIncrement(P^.refCount);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure _LoadClassRef(P: TBaseRunner;
                        C: TClass); stdcall;
begin
  P.GetRootProg.PassedClassRef := C;
end;

function _LoadProc(Runner: Pointer;
                   ProcHandle: Integer;
                   ProcName: PChar;
                   DllName: PChar;
                   OverCount: Integer): Pointer; stdcall;
var
  H: THandle;
  Address: Pointer;
  I: Integer;
  Ext: String;
  MR: TMapRec;
  AClassName: String;
  C: TClass;
  VarName: String;
  S, S1, S2: String;
  Offset: Integer;
  MapFieldRec: TMapFieldRec;
  P: TBaseRunner;
label
  ProcessDll;
begin
  P := TBaseRunner(Runner);
  Address := nil;
  result := nil;

  Ext := ExtractFileExt(DllName);
  if StrEql(Ext, '.' + PCU_FILE_EXT) then
  begin
    if Assigned(P.PausedPCU) then
      Address := P.PausedPCU.LoadAddressEx(
        DllName, ProcName, true, OverCount, MR, result)
    else
      Address := P.LoadAddressEx(
        DllName, ProcName, true, OverCount, MR, result);

    if Address = nil then
    begin
      if MR <> nil then
        if MR.SubDesc.DllName <> '' then
        begin
          DllName := PChar(MR.SubDesc.DllName);
          ProcName := PChar(MR.SubDesc.AliasName);
          goto ProcessDll;
        end;

      raise Exception.Create(Format(errProcNotFoundInPCU,
        [String(ProcName), String(DllName)]));
    end;

    P.SetAddress(ProcHandle, Address);

    if (PosCh('.', ProcName) > 0) and (result <> nil) then
    begin
      AClassName := ExtractOwner(DllName) + '.' + ExtractOwner(ProcName);
      Address := TBaseRunner(result).GetAddress(AClassName, MR);
      if Address <> nil then
        C := TClass(Address^)
      else
        Exit;
      if C = nil then
        Exit;
      Address := P.GetAddress(AClassName, MR);
      if Address <> nil then
        Pointer(Address^) := C;
    end;

    Exit;
  end
  else if (StrEql(Ext, '.' + PRM_FILE_EXT) or StrEql(Ext, '.' + PRR_FILE_EXT))
           and
          (not StrEql(ProcName, 'Self')) then
  begin
    VarName := ProcName;
    S := ExtractFullOwner(DllName);
    S1 := ExtractOwner(S) + '.' + PCU_FILE_EXT;
    S2 := ExtractFullName(S);

    Address := P.PausedPCU.LoadAddressEx(
      S1, S2, true, OverCount, MR, result);

    if MR <> nil then
    begin
      I := MR.SubDesc.ParamList.IndexOf(VarName);
      if I >= 0 then
      begin
        Offset := MR.SubDesc.ParamList[I].ParamOffset;
        Address := P.PausedPCU.GetParamAddress(Offset);
        if StrEql(Ext, '.' + PRR_FILE_EXT) then
          Address := Pointer(Address^);
        P.SetAddress(ProcHandle, Address);
      end
      else
        P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
    end
    else
      P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);

    Exit;
  end
  else if StrEql(Ext, '.' + LOC_FILE_EXT) or
          StrEql(Ext, '.' + LOR_FILE_EXT) then
  begin
    VarName := ProcName;
    S := ExtractFullOwner(DllName);
    S1 := ExtractOwner(S) + '.' + PCU_FILE_EXT;
    S2 := ExtractFullName(S);
    Address := P.PausedPCU.LoadAddressEx(
      S1, S2, true, OverCount, MR, result);
    if MR <> nil then
    begin
      I := MR.SubDesc.LocalVarList.IndexOf(VarName);
      if I >= 0 then
      begin
        Offset := MR.SubDesc.LocalVarList[I].LocalVarOffset;
        Address := P.PausedPCU.GetLocalAddress(Offset);
        if StrEql(Ext, '.' + LOR_FILE_EXT) then
          Address := Pointer(Address^);
        P.SetAddress(ProcHandle, Address);
      end
      else
        P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
    end
    else
      P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
    Exit;
  end
  else if StrEql(Ext, '.' + SLF_FILE_EXT) then
  begin
    VarName := ProcName;
    S := ExtractFullOwner(DllName);
    S1 := ExtractOwner(S) + '.' + PCU_FILE_EXT;
    S2 := ExtractFullName(S);
    Address := P.PausedPCU.LoadAddressEx(
      S1, S2, true, OverCount, MR, result);
    if MR <> nil then
    begin
      Offset := MR.SubDesc.SelfOffset;
      Address := P.PausedPCU.GetLocalAddress(Offset);
      P.SetAddress(ProcHandle, Address);
    end
    else
      P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);

    Exit;
  end
  else if StrEql(Ext, '.' + FLD_FILE_EXT) then
  begin
    VarName := ProcName;
    S := ExtractFullOwner(DllName);
    S1 := ExtractOwner(S) + '.' + PCU_FILE_EXT;
    S2 := ExtractFullName(S);
    Address := P.PausedPCU.LoadAddressEx(
      S1, S2, true, OverCount, MR, result);
    if MR <> nil then
    begin
      Offset := MR.SubDesc.SelfOffset;
      Address := P.PausedPCU.GetLocalAddress(Offset);
      if Address <> nil then
      begin
        MR := TBaseRunner(result).ScriptMapTable.LookupType(
          ExtractFullOwner(S));
        if MR <> nil then
        begin
          MapFieldRec := MR.FieldList.Lookup(VarName);
          if MapFieldRec <> nil then
          begin
            Address := ShiftPointer(Pointer(Address^), MapFieldRec.FieldOffset);
            P.SetAddress(ProcHandle, Address);
          end
          else
            P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
        end
        else
          P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
      end
      else
        P.RaiseError(errEntryPointNotFoundInPCU, [S2, S1]);
    end;
    Exit;
  end;

ProcessDll:

  if Assigned(P.OnLoadProc) then
  begin
    P.OnLoadProc(P.Owner, ProcName, DllName, Address);
    if Address <> nil then
    begin
      P.SetAddress(ProcHandle, Address);
      Exit;
    end;
  end;

  I := P.DllList.IndexOf(DllName);
  if I = - 1 then
  begin
    {$IFDEF LINUX}
    H := HMODULE(dynlibs.LoadLibrary(DLLName));
    Address := dynlibs.GetProcedureAddress(H, ProcName);
    {$ELSE}
    H := LoadLibrary(DllName);
    Address := GetProcAddress(H, ProcName);
    {$ENDIF}
    if Address <> nil then
      P.DllList.AddObject(DllName, TObject(H));
  end
  else
  begin
    H := Cardinal(P.DllList.Objects[I]);
   {$IFDEF LINUX}
    Address := dynlibs.GetProcedureAddress(H, ProcName);
    {$ELSE}
    Address := GetProcAddress(H, PChar(ProcName));
    {$ENDIF}
  end;

 if H = 0 then
   raise Exception.Create(Format(errDllNotFound, [String(DllName)]));

 if Address = nil then
   raise Exception.Create(Format(errProcNotFound,
     [String(ProcName), String(DllName)]));

 P.SetAddress(ProcHandle, Address);
end;

{$IFNDEF PAXARM}
procedure _AnsiStringFromPAnsiChar(source: PAnsiChar; var dest: AnsiString); stdcall;
begin
  dest := source;
end;

procedure _AnsiStringFromPWideChar(source: PWideChar;
                                   var dest: AnsiString); stdcall;
begin
  dest := AnsiString(WideString(source));
end;

procedure _AnsiStringFromAnsiChar(source: AnsiChar; var dest: AnsiString); stdcall;
begin
  dest := source;
end;

procedure _WideStringFromAnsiChar(source: AnsiChar; var dest: WideString); stdcall;
begin
  dest := WideString(source);
end;

procedure _UnicStringFromAnsiChar(source: AnsiChar; var dest: UnicString); stdcall;
begin
  dest := UnicString(source);
end;

procedure _WideStringFromShortString(var Dest: WideString; Source: ShortString);
stdcall;
begin
  Dest := WideString(Source);
end;

procedure _ShortStringFromAnsiString(var Dest: ShortString; L: Integer; var Source: AnsiString);
stdcall;
begin
  Dest := Copy(Source, 1, L);
end;

procedure _ShortStringFromWideString(var Dest: ShortString; L: Integer; var Source: WideString);
stdcall;
begin
  Dest := ShortString(Copy(Source, 1, L));
end;

procedure _ShortStringFromPWideChar(var Dest: ShortString; L: Integer; Source: PWideChar);
stdcall;
begin
  Dest := ShortString(Copy(WideString(Source), 1, L));
end;

procedure _ShortStringFromUnicString(var Dest: ShortString; L: Integer; var Source: UnicString);
stdcall;
begin
  Dest := ShortString(Copy(Source, 1, L));
end;

procedure _AnsiStringFromShortString(var Dest: AnsiString; Source: ShortString); stdcall;
begin
  Dest := Source;
end;

procedure _UnicStringFromShortString(var Dest: UnicString; Source: ShortString);
stdcall;
begin
  Dest := UnicString(Source);
end;

procedure _AnsiStringFromWideString(var Dest: AnsiString; var Source: WideString);
stdcall;
begin
  Dest := AnsiString(Source);
end;

procedure _AnsiStringFromUnicString(var Dest: AnsiString; var Source: UnicString);
stdcall;
begin
  Dest := AnsiString(Source);
end;

procedure _WideStringFromAnsiString(var Dest: WideString; var Source: AnsiString);
stdcall;
begin
  Dest := WideString(Source);
end;

procedure _UnicStringFromAnsiString(var Dest: UnicString; var Source: AnsiString);
stdcall;
begin
  Dest := UnicString(Source);
end;

procedure _AnsiStringAddition(var s1: AnsiString; var s2: AnsiString; var dest: AnsiString);
stdcall;
begin
  dest := s1 + s2;
end;

procedure _ShortStringAddition(var s1, s2, dest: ShortString);
stdcall;
begin
  dest := s1 + s2;
end;

procedure _AnsiStringEquality(var s1: AnsiString; var s2: AnsiString; var dest: Boolean);
stdcall;
begin
  dest := s1 = s2;
end;

procedure _AnsiStringNotEquality(var s1: AnsiString; var s2: AnsiString; var dest: Boolean);
stdcall;
begin
  dest := s1 <> s2;
end;

procedure _ShortStringEquality(const s1: ShortString; const s2: ShortString;
                               var dest: Boolean); stdcall;
begin
  dest := s1 = s2;
end;

procedure _ShortStringNotEquality(const s1: ShortString; const s2: ShortString;
                                  var dest: Boolean); stdcall;
begin
  dest := s1 <> s2;
end;

procedure _AnsiStringGreaterThan(var s1: AnsiString; var s2: AnsiString; var dest: Boolean);
stdcall;
begin
  dest := s1 > s2;
end;

procedure _AnsiStringGreaterThanOrEqual(var s1: AnsiString; var s2: AnsiString;
                                    var dest: Boolean); stdcall;
begin
  dest := s1 >= s2;
end;

procedure _AnsiStringLessThan(var s1: AnsiString; var s2: AnsiString; var dest: Boolean);
stdcall;
begin
  dest := s1 < s2;
end;

procedure _AnsiStringLessThanOrEqual(var s1: AnsiString; var s2: AnsiString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <= s2;
end;

procedure _ShortStringGreaterThan(const s1: ShortString; const s2: ShortString;
                                  var dest: Boolean);
stdcall;
begin
  dest := s1 > s2;
end;

procedure _ShortStringGreaterThanOrEqual(const s1: ShortString;
                               const s2: ShortString; var dest: Boolean); stdcall;
begin
  dest := s1 >= s2;
end;

procedure _ShortStringLessThan(const s1: ShortString; const s2: ShortString;
                               var dest: Boolean); stdcall;
begin
  dest := s1 < s2;
end;

procedure _ShortStringLessThanOrEqual(const s1: ShortString; const s2: ShortString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <= s2;
end;

procedure _SetShortStringLength(var S: ShortString; L: Integer); stdcall;
begin
  SetLength(S, L);
end;

procedure _SetWideStringLength(var S: WideString; L: Integer); stdcall;
begin
  SetLength(S, L);
end;

procedure _WideStringFromPAnsiChar(source: PAnsiChar; var dest: WideString); stdcall;
begin
  dest := WideString(AnsiString(source));
end;

procedure _UnicStringFromPAnsiChar(source: PAnsiChar; var dest: UnicString); stdcall;
begin
  dest := String(AnsiString(source));
end;

procedure _WideStringFromPWideChar(source: PWideChar; var dest: WideString); stdcall;
begin
  dest := WideString(source);
end;

procedure _WideStringFromWideChar(source: WideChar; var dest: WideString);
stdcall;
begin
  dest := source;
end;

procedure _AnsiStringFromWideChar(source: WideChar; var dest: AnsiString);
stdcall;
begin
  dest := AnsiString(source);
end;

procedure _AnsiStringAssign(var dest: AnsiString; var source: AnsiString); stdcall;
begin
  dest := source;
end;

procedure _UnicStringFromWideString(var Dest: UnicString; var Source: WideString); stdcall;
begin
  Dest := Source;
end;

procedure _WideStringFromUnicString(var Dest: WideString; var Source: UnicString);
stdcall;
begin
  Dest := Source;
end;

procedure _WideStringAssign(var dest: WideString; var source: WideString);
stdcall;
begin
  dest := source;
end;

procedure _WideStringAddition(var s1: WideString; var s2: WideString;
                              var dest: WideString); stdcall;
begin
  dest := s1 + s2;
end;

procedure _WideStringEquality(var s1: WideString; var s2: WideString;
                              var dest: Boolean); stdcall;
begin
  dest := s1 = s2;
end;

procedure _WideStringNotEquality(var s1: WideString; var s2: WideString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <> s2;
end;

function _Copy(const S: AnsiString; Index, Count:Integer): AnsiString;
begin
  result := Copy(S, Index, Count);
end;

procedure _Insert(Source: AnsiString; var S: AnsiString; Index: Integer);
begin
  Insert(Source, S, Index);
end;

function _PosString(const Substr: AnsiString; const S: AnsiString): Integer;
begin
  result := Pos(Substr, S);
end;

function _PosChar(c: AnsiChar; const S: AnsiString): Integer;
var
  I: Integer;
begin
  for I:=SLow(s) to SHigh(s) do
    if s[I] = c then
    begin
      result := I;
      Exit;
    end;
  result := 0;
end;

procedure _StrInt(var S: AnsiString; L1, L2: Integer; value: Integer); stdcall;
begin
  if L1 > 0 then
    Str(value:L1, S)
  else
    Str(value, S);
end;

procedure _StrDouble(var S: AnsiString; L2, L1: Integer; value: Double); stdcall;
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      Str(value:L1:L2, S)
    else
      Str(value:L1, S);
  end
  else
    Str(value, S);
end;

procedure _StrSingle(var S: AnsiString; L2, L1: Integer; value: Single); stdcall;
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      Str(value:L1:L2, S)
    else
      Str(value:L1, S);
  end
  else
    Str(value, S);
end;

procedure _StrExtended(var S: AnsiString; L2, L1: Integer; value: Extended); stdcall;
begin
  if L1 > 0 then
  begin
    if L2 > 0 then
      Str(value:L1:L2, S)
    else
      Str(value:L1, S);
  end
  else
    Str(value, S);
end;

procedure _VariantFromShortString(var Dest: Variant; var Source: ShortString);
stdcall;
begin
  Dest := Source;
end;

procedure _OleVariantFromShortString(var Dest: OleVariant; var Source: ShortString);
stdcall;
begin
  Dest := Source;
end;

procedure _VariantFromAnsiChar(source: AnsiChar; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromAnsiChar(source: AnsiChar; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _ShortStringFromVariant(var Dest: ShortString; L: Integer; var Source: Variant);
stdcall;
begin
  Dest := Copy(ShortString(source), 1, L);
end;

procedure _WideStringGreaterThan(var s1: WideString; var s2: WideString;
                                                    var dest: Boolean); stdcall;
begin
  dest := s1 > s2;
end;

procedure _WideStringGreaterThanOrEqual(var s1: WideString; var s2: WideString;
                                                    var dest: Boolean); stdcall;
begin
  dest := s1 >= s2;
end;

procedure _WideStringLessThan(var s1: WideString; var s2: WideString;
                              var dest: Boolean);stdcall;
begin
  dest := s1 < s2;
end;

procedure _WideStringLessThanOrEqual(var s1: WideString; var s2: WideString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <= s2;
end;

procedure _SetStringLength(var S: AnsiString; L: Integer); stdcall;
begin
  SetLength(S, L);
end;

procedure _AnsiStringClr(var S: AnsiString); stdcall;
begin
  S := '';
end;

procedure _WideStringClr(var S: WideString); stdcall;
begin
  S := '';
end;

procedure _UniqueAnsiString(var S: AnsiString); stdcall;
begin
  UniqueString(S);
end;

function _LengthString(const S: AnsiString): Integer;
begin
  result := Length(S);
end;

function _LengthWideString(const S: WideString): Integer;
begin
  result := Length(S);
end;

procedure _Delete(var S: AnsiString; Index, Count:Integer);
begin
  Delete(S, Index, Count);
end;

procedure _VariantFromPAnsiChar(source: PAnsiChar; var dest: Variant); stdcall;
begin
  dest := AnsiString(Source);
end;

procedure _OleVariantFromPAnsiChar(source: PAnsiChar; var dest: OleVariant); stdcall;
begin
  dest := AnsiString(Source);
end;

procedure _VariantFromAnsiString(var Dest: Variant; var Source: AnsiString); stdcall;
begin
  Dest := Source;
end;

procedure _OleVariantFromAnsiString(var Dest: OleVariant; var Source: AnsiString); stdcall;
begin
  Dest := Source;
end;

procedure _VariantFromWideString(var Dest: Variant; var Source: WideString);
stdcall;
begin
  Dest := Source;
end;

procedure _OleVariantFromWideString(var Dest: OleVariant; var Source: WideString);
stdcall;
begin
  Dest := Source;
end;

procedure _AnsiStringFromInt(var dest: AnsiString; var source: Integer); stdcall; //JS only
begin
  dest := AnsiString(IntToStr(source));
end;

procedure _AnsiStringFromDouble(var dest: AnsiString; var source: Double); stdcall; //JS only
begin
  dest := AnsiString(FloatToStr(source));
end;

procedure _AnsiStringFromSingle(var dest: AnsiString; var source: Single); stdcall; //JS only
begin
  dest := AnsiString(FloatToStr(source));
end;

procedure _AnsiStringFromExtended(var dest: AnsiString; var source: Extended); stdcall; //JS only
begin
  dest := AnsiString(FloatToStr(source));
end;

procedure _AnsiStringFromBoolean(var dest: AnsiString; var source: Boolean); stdcall; //JS only
begin
  if source then
    dest := 'true'
  else
    dest := 'false';
end;

procedure _AnsiCharFromVariant(var dest: AnsiChar; var source: Variant); stdcall;
begin
  dest := AnsiChar(TVarData(Source).VInteger);
end;

procedure _AnsiStringFromVariant(var dest: AnsiString; var source: Variant); stdcall;
begin
  dest := AnsiString(source);
end;

procedure _WideStringFromVariant(var dest: WideString; var source: Variant);
stdcall;
begin
  dest := source;
end;

//////////////////////////////////////// NOT PAXARM /////////////////
{$ENDIF} /////////////////////////////// NOT PAXARM /////////////////
//////////////////////////////////////// NOT PAXARM /////////////////

procedure _ShortStringAssign(const source: ShortString; Ldest: Integer; dest: PShortString); stdcall;
var
  I, L: Integer;
begin
{$IFDEF ARC}
  L := Source[0];
{$ELSE}
  L := Length(Source);
{$ENDIF}
  if L > Ldest then
    L := Ldest;
{$IFDEF ARC}
  dest^[0] := L;
{$ELSE}
  dest^[0] := AnsiChar(Chr(L));
{$ENDIF}
  for I := 1 to L do
    dest^[I] := Source[I];
end;

procedure _UnicStringFromPWideChar(source: PWideChar; var dest: UnicString); stdcall;
begin
  dest := UnicString(source);
end;

procedure _UnicStringFromWideChar(source: WideChar; var dest: UnicString);
stdcall;
begin
  dest := source;
end;

procedure _UnicStringAssign(var dest: UnicString; var source: UnicString);
stdcall;
begin
  dest := source;
end;

procedure _UnicStringAddition(var s1: UnicString; var s2: UnicString;
                              var dest: UnicString); stdcall;
begin
  dest := s1 + s2;
end;

procedure _UnicStringEquality(var s1: UnicString; var s2: UnicString;
                              var dest: Boolean); stdcall;
begin
  dest := s1 = s2;
end;

procedure _UnicStringNotEquality(var s1: UnicString; var s2: UnicString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <> s2;
end;

procedure _UnicStringGreaterThan(var s1: UnicString; var s2: UnicString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 > s2;
end;

procedure _UnicStringGreaterThanOrEqual(var s1: UnicString; var s2: UnicString;
                                                    var dest: Boolean); stdcall;
begin
  dest := s1 >= s2;
end;

procedure _UnicStringLessThan(var s1: UnicString; var s2: UnicString;
                              var dest: Boolean);stdcall;
begin
  dest := s1 < s2;
end;

procedure _UnicStringLessThanOrEqual(var s1: UnicString; var s2: UnicString;
                                 var dest: Boolean); stdcall;
begin
  dest := s1 <= s2;
end;

procedure _SetVariantLength(var V: Variant; VType: Integer; L: Integer); stdcall;
begin
  V := VarArrayCreate([0, L - 1], VType);
end;

procedure _SetVariantLength2(var V: Variant; VType: Integer; L1, L2: Integer); stdcall;
begin
  V := VarArrayCreate([0, L1 - 1, 0, L2 - 1], VType);
end;

procedure _SetVariantLength3(var V: Variant; VType: Integer; L1, L2, L3: Integer); stdcall;
begin
  V := VarArrayCreate([0, L1 - 1, 0, L2 - 1, 0, L3 - 1], VType);
end;

procedure _SetUnicStringLength(var S: UnicString; L: Integer); stdcall;
begin
  SetLength(S, L);
end;

procedure _UnicStringClr(var S: UnicString); stdcall;
begin
  S := '';
end;

procedure _InterfaceClr(var I: IUnknown); stdcall;
begin
  I := nil;
end;

procedure _UniqueUnicString(var S: UnicString); stdcall;
begin
{$IFDEF VARIANTS}
  UniqueString(S);
{$ENDIF}
end;

function _LengthShortString(const S: ShortString): Integer;
begin
  result := Length(S);
end;

function _LengthUnicString(const S: UnicString): Integer;
begin
  result := Length(S);
end;

procedure _ValInt(const S: String; var V: Integer; var Code: Integer);
begin
  Val(S, V, Code);
end;

procedure _ValDouble(const S: String; var V: Double; var Code: Integer);
begin
  Val(S, V, Code);
end;

procedure _ShortstringHigh(const P: Shortstring; var result: Integer); stdcall;
begin
  result := Length(P) - 1;
end;

// unic string routines

function _UnicLength(const S: UnicString): Integer;
begin
  result := Length(S);
end;

procedure _UnicDelete(var S: UnicString; Index, Count: Integer);
begin
  Delete(S, Index, Count);
end;

function _UnicCopy(const S: UnicString; Index, Count: Integer): UnicString;
begin
  result := Copy(S, Index, Count);
end;

procedure _UnicInsert(Source: UnicString; var S: UnicString; Index: Integer);
begin
  Insert(Source, S, Index);
end;

procedure _UnicValInt(const S: UnicString; var V: Integer; var Code: Integer);
begin
  Val(S, V, Code);
end;

procedure _UnicValDouble(const S: UnicString; var V: Double; var Code: Integer);
begin
  Val(S, V, Code);
end;

function _UnicPos(const Substr: UnicString; const S: UnicString): Integer;
begin
  result := Pos(Substr, S);
end;

// INT64 ROUTINES ///////////////////////////////////////

procedure _Int64Multiplication(var v1: Int64; var v2: Int64; var dest: Int64);
stdcall;
begin
  dest := v1 * v2;
end;

procedure _Int64Division(var v1: Int64; var v2: Int64; var dest: Int64);
stdcall;
begin
  dest := v1 div v2;
end;

procedure _Int64Modulo(var v1: Int64; var v2: Int64; var dest: Int64);
stdcall;
begin
  dest := v1 mod v2;
end;

procedure _Int64LeftShift(var v1: Int64; var v2: Int64; var dest: Int64);
stdcall;
begin
  dest := v1 shl v2;
end;

procedure _Int64RightShift(var v1: Int64; var v2: Int64; var dest: Int64);
stdcall;
begin
  dest := v1 shr v2;
end;

procedure _Int64LessThan(var v1: Int64; var v2: Int64; var dest: Boolean);
stdcall;
begin
  dest := v1 < v2;
end;

procedure _Int64LessThanOrEqual(var v1: Int64; var v2: Int64;
                                  var dest: Boolean); stdcall;
begin
  dest := v1 <= v2;
end;

procedure _Int64GreaterThan(var v1: Int64; var v2: Int64;
                            var dest: Boolean); stdcall;
begin
  dest := v1 > v2;
end;

procedure _Int64GreaterThanOrEqual(var v1: Int64; var v2: Int64;
                                     var dest: Boolean); stdcall;
begin
  dest := v1 >= v2;
end;

procedure _Int64Equality(var v1: Int64; var v2: Int64;
                           var dest: Boolean); stdcall;
begin
  dest := v1 = v2;
end;

procedure _Int64NotEquality(var v1: Int64; var v2: Int64;
                              var dest: Boolean); stdcall;
begin
  dest := v1 <> v2;
end;

procedure _Int64Abs(var v1: Int64; var dest: Int64); stdcall;
begin
  dest := Abs(v1);
end;

procedure _UInt64LessThan(var v1: UInt64; var v2: UInt64; var dest: Boolean);
stdcall;
begin
  dest := v1 < v2;
end;

procedure _UInt64LessThanOrEqual(var v1: UInt64; var v2: UInt64;
                                  var dest: Boolean); stdcall;
begin
  dest := v1 <= v2;
end;

procedure _UInt64GreaterThan(var v1: UInt64; var v2: UInt64;
                             var dest: Boolean); stdcall;
begin
  dest := v1 > v2;
end;

procedure _UInt64GreaterThanOrEqual(var v1: UInt64; var v2: UInt64;
                                    var dest: Boolean); stdcall;
begin
  dest := v1 >= v2;
end;

procedure _VariantAssign(var dest: Variant; var source: Variant); stdcall;
begin
  if VarIsNull(source) then
    VarClear(dest);
  dest := source;
end;

procedure _OleVariantAssign(var dest: OleVariant; var source: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromInterface(const source: IDispatch; var dest: Variant); stdcall;
begin
  dest := Source;
end;

procedure _OleVariantFromInterface(const source: IDispatch; var dest: OleVariant); stdcall;
begin
  dest := Source;
end;

procedure _VariantFromPWideChar(source: PWideChar; var dest: Variant); stdcall;
begin
  dest := UnicString(Source);
end;

procedure _OleVariantFromPWideChar(source: PWideChar; var dest: OleVariant); stdcall;
begin
  dest := UnicString(Source);
end;

procedure _OleVariantFromVariant(var Dest: OleVariant; var Source: Variant); stdcall;
begin
  Dest := Source;
end;

procedure _VariantFromUnicString(var Dest: Variant; var Source: UnicString);
stdcall;
begin
  Dest := Source;
end;

procedure _OleVariantFromUnicString(var Dest: OleVariant; var Source: UnicString);
stdcall;
begin
  Dest := Source;
end;

procedure _VariantFromWideChar(source: WideChar; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromWideChar(source: WideChar; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromInt(source: Integer; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromInt(source: Integer; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

{$IFDEF VARIANTS}
procedure _VariantFromInt64(var dest: Variant; var source: Int64); stdcall;
begin
  dest := source;
end;
procedure _OleVariantFromInt64(var dest: OleVariant; var source: Int64); stdcall;
begin
  dest := source;
end;

{$ELSE}
procedure _VariantFromInt64(var dest: Variant; var source: Int64); stdcall;
begin
  dest := Integer(source);
end;
procedure _OleVariantFromInt64(var dest: OleVariant; var source: Int64); stdcall;
begin
  dest := Integer(source);
end;
{$ENDIF}

procedure _VariantFromByte(source: Byte; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromByte(source: Byte; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromBool(source: Boolean; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromBool(source: Boolean; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromWord(source: Word; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromWord(source: Word; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromCardinal(source: Cardinal; var dest: Variant); stdcall;
begin
{$IFDEF VARIANTS}
  dest := source;
{$ELSE}
  dest := Integer(source);
{$ENDIF}
end;

{$IFDEF VARIANTS}
procedure _OleVariantFromCardinal(source: Cardinal; var dest: OleVariant); stdcall;
begin
  dest := source;
end;
{$ELSE}
procedure _OleVariantFromCardinal(source: Integer; var dest: OleVariant); stdcall;
begin
  dest := source;
end;
{$ENDIF}

procedure _VariantFromSmallInt(source: SmallInt; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromSmallInt(source: SmallInt; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromShortInt(source: ShortInt; var dest: Variant); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromShortInt(source: ShortInt; var dest: OleVariant); stdcall;
begin
  dest := source;
end;

procedure _VariantFromDouble(var dest: Variant; var source: Double); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromDouble(var dest: OleVariant; var source: Double); stdcall;
begin
  dest := source;
end;

procedure _VariantFromCurrency(var dest: Variant; var source: Currency); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromCurrency(var dest: OleVariant; var source: Currency); stdcall;
begin
  dest := source;
end;

procedure _VariantFromSingle( var dest: Variant; var source: Single); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromSingle(var dest: OleVariant; var source: Single); stdcall;
begin
  dest := source;
end;

procedure _VariantFromExtended(var dest: Variant; var source: Extended); stdcall;
begin
  dest := source;
end;

procedure _OleVariantFromExtended(var dest: OleVariant; var source: Extended); stdcall;
begin
  dest := source;
end;

procedure _UnicStringFromInt(var dest: UnicString; var source: Integer); stdcall; //JS only
begin
  dest := IntToStr(source);
end;

procedure _UnicStringFromDouble(var dest: UnicString; var source: Double); stdcall; //JS only
begin
  dest := FloatToStr(source);
end;

procedure _UnicStringFromSingle(var dest: UnicString; var source: Single); stdcall; //JS only
begin
  dest := FloatToStr(source);
end;

procedure _UnicStringFromExtended(var dest: UnicString; var source: Extended); stdcall; //JS only
begin
  dest := FloatToStr(source);
end;

procedure _UnicStringFromBoolean(var dest: UnicString; var source: Boolean); stdcall; //JS only
begin
  if source then
    dest := 'true'
  else
    dest := 'false';
end;

procedure _WideCharFromVariant(var dest: WideChar; var source: Variant); stdcall;
begin
  dest := WideChar(TVarData(Source).VInteger);
end;

procedure _UnicStringFromVariant(var dest: UnicString; var source: Variant);
stdcall;
begin
  dest := source;
end;

procedure _DoubleFromVariant(var dest: Double; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _CurrencyFromVariant(var dest: Currency; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _SingleFromVariant(var dest: Single; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _ExtendedFromVariant(var dest: Extended; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _IntFromVariant(var dest: Integer; var source: Variant); stdcall;
begin
  dest := source;
end;

{$IFDEF VARIANTS}
procedure _Int64FromVariant(var dest: Int64; var source: Variant); stdcall;
begin
  dest := source;
end;
{$ELSE}
procedure _Int64FromVariant(var dest: Int64; var source: Variant); stdcall;
begin
  dest := Integer(source);
end;
{$ENDIF}

procedure _ByteFromVariant(var dest: Byte; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _WordFromVariant(var dest: Word; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _CardinalFromVariant(var dest: Cardinal; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _SmallIntFromVariant(var dest: SmallInt; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _ShortIntFromVariant(var dest: ShortInt; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _BoolFromVariant(var dest: Boolean; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _ByteBoolFromVariant(var dest: ByteBool; var source: Variant); stdcall;
begin
{$IFDEF FPC}
  if source <> 0 then
    dest := true
  else
    dest := false;
{$ELSE}
  dest := source;
{$ENDIF}
end;

procedure _WordBoolFromVariant(var dest: WordBool; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _LongBoolFromVariant(var dest: LongBool; var source: Variant); stdcall;
begin
  dest := source;
end;

procedure _StructEquality(P1, P2: Pointer; SZ: Integer; var dest: Boolean); stdcall;
begin
  dest := CompareMem(P1, P2, SZ);
end;

procedure _StructNotEquality(P1, P2: Pointer; SZ: Integer; var dest: Boolean); stdcall;
begin
  dest := not CompareMem(P1, P2, SZ);
end;

procedure _VariantNot(var v1: Variant; var dest: Variant); stdcall;
begin
  dest := not v1;
end;

procedure _VariantNegation(var v1: Variant; var dest: Variant); stdcall;
begin
  dest := - v1;
end;

procedure _VariantAbs(var v1: Variant; var dest: Variant); stdcall;
begin
  if v1 >= 0 then
    dest := v1
  else
    dest := -v1;
end;

procedure _VarArrayPut1(var V: Variant; var value: Variant; const I1: Variant);
stdcall;
begin
  V[I1] := value;
end;

procedure _VarArrayGet1(var V: Variant; var result: Variant; const I1: Variant);
stdcall;
begin
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

procedure _DoubleMultiplication(Language: Integer;
  var v1: Double; var v2: Double; var dest: Double); stdcall;
begin
  dest := v1 * v2;
end;

procedure _DoubleDivision(Language: Integer;
  var v1: Double; var v2: Double; var dest: Double); stdcall;
begin
  dest := v1 / v2;
end;

procedure _DoubleAddition(Language: Integer;
  var v1: Double; var v2: Double; var dest: Double); stdcall;
begin
  dest := v1 + v2;
end;

procedure _DoubleSubtraction(Language: Integer;
  var v1: Double; var v2: Double; var dest: Double); stdcall;
begin
  dest := v1 - v2;
end;

procedure _DoubleNegation(var v1: Double; var dest: Double); stdcall;
begin
  dest := - v1;
end;

function GetPaxInterface(Self: TObject; const GUID: TGUID; var obj: Pointer): Boolean;
var
  PaxInfo: PPaxInfo;
  P: TBaseRunner;
  ClassRec: TClassRec;
  IntfList: TIntfList;
  I, SZ: Integer;
begin
  PaxInfo := GetPaxInfo(Self.ClassType);
  if PaxInfo = nil then
    raise Exception.Create(errInternalError);

  P := TBaseRunner(PaxInfo^.Prog);
  ClassRec := P.ClassList[PaxInfo^.ClassIndex];
  IntfList := ClassRec.IntfList;
  I := IntfList.IndexOf(GUID);
  if I = -1 then
    result := false
  else
  begin
    SZ := Self.InstanceSize - IntfList.Count * SizeOf(Pointer);
    Obj := ShiftPointer(Self, SZ + I * SizeOf(Pointer));
    result := true;
  end;
end;

{
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
// PIC:  EBX must be correct before calling QueryInterface
var
  Temp: IInterface;
begin
  if Source = nil then
    Dest := nil
  else
  begin
    Temp := nil;
    if Source.QueryInterface(IID, Temp) <> 0 then
      Error(reIntfCastError)
    else
      Dest := Temp;
  end;
end;
}

{$IFNDEF PAXARM}
{$IFNDEF PAX64}
function TObject_GetScriptInterface(Self: TObject; const IID: TGUID; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  InterfaceEntry := Self.GetInterfaceEntry(IID);
  if InterfaceEntry <> nil then
  begin
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(Obj) := ShiftPointer(Pointer(Self), InterfaceEntry^.IOffset);
      asm
        push edi
        push esi
        push ebx
      end;
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
      asm
        pop ebx
        pop esi
        pop edi
      end;
    end;
  end;
  Result := Pointer(Obj) <> nil;
end;
{$ENDIF}
{$ENDIF}

procedure _InterfaceFromClass(Dest: PIUnknown;
                              GUID: PGUID;
                              SourceAddress: Pointer); stdcall;
var
  Temp: IInterface;
begin
  if Pointer(SourceAddress^) = nil then
  begin
    Dest^ := nil;
    Exit;
  end;

  Temp := nil;

{$IFNDEF PAXARM}
{$IFNDEF PAX64}
  if IsPaxObject(TObject(SourceAddress^)) then
  begin
    if not TObject_GetScriptInterface(TObject(SourceAddress^), GUID^, Temp) then
      raise Exception.Create(errIncompatibleTypesNoArgs);
  end
  else
{$ENDIF}
{$ENDIF}
  begin
    if not TMyInterfacedObject(SourceAddress^).GetInterface(GUID^, Temp) then
      raise Exception.Create(errIncompatibleTypesNoArgs);
  end;

  Dest^ := Temp;
end;

procedure _InterfaceCast(Dest: PIUnknown;
                         GUID: PGUID;
                         Source: PIUnknown); stdcall;
var
  Temp: Pointer;
begin
  if Source^ = nil then
    Dest^ := nil
  else
  begin
    Temp := nil;
    if Source.QueryInterface(GUID^, Temp) <> 0 then
      raise Exception.Create(errIncompatibleTypesNoArgs);
    if Assigned(Dest^) then
       Dest^._Release;
    Pointer(Dest^) := Temp;
  end;
end;

procedure _InterfaceAssign(var Dest: IUnknown;
                           var Source: IUnknown); stdcall;
begin
  Dest := Source;
end;

// PASCAL ARITHMETIC ROUTINES ///////////////////////////////////////

function _ArcTan(const X: Extended): Extended;
begin
  result := ArcTan(X);
end;

function _Cos(const X: Extended): Extended;
begin
  result := Cos(X);
end;

function _Exp(X: Extended): Extended;
begin
  result := Exp(X);
end;

function _Frac(X: Extended): Extended;
begin
  result := Frac(X);
end;

function _Int(X: Extended): Extended;
begin
  result := Int(X);
end;

function _Ln(X: Extended): Extended;
begin
  result := Ln(X);
end;

function _Sin(X: Extended): Extended;
begin
  result := Sin(X);
end;

function _Sqr(X: Extended): Extended;
begin
  result := Sqr(X);
end;

function _Sqrt(X: Extended): Extended;
begin
  result := Sqrt(X);
end;

function _Trunc(X: Extended): Integer;
begin
  result := Trunc(X);
end;

function _Power(const Base, Exponent: Extended): Extended;
begin
  result := Power(Base, Exponent);
end;

// PASCAL MISCELLANEOUS ROUTINES ////////////////////////////////////

procedure _FillChar(var X; Count: Integer; Value: Byte);
begin
  FillChar(X, Count, Value);
end;

function _Random: Double;
begin
  result := Random;
end;

function _Random1(N: Integer): Integer;
begin
  result := Random(N);
end;

function _HiInt(N: Integer): Byte;
begin
  result := Hi(N);
end;

function _HiWord(N: Word): Byte;
begin
  result := Hi(N);
end;

function _LoInt(N: Integer): Byte;
begin
  result := Lo(N);
end;

function _LoWord(N: Word): Byte;
begin
  result := Lo(N);
end;

{$IFDEF FPC}
function _UpCase(Ch: AnsiChar): AnsiChar;
begin
  result := Upcase(Ch);
end;
{$ENDIF}

function GetClassByIndex(P: TBaseRunner; I: Integer): TClass;
begin
  result := P.ClassList[I].PClass;
end;

procedure _Is(PClass: TClass; Instance: TObject;
              var result: Boolean); stdcall;
begin
  result := Instance is PClass;
end;

procedure _ToParentClass2(Instance: TObject); stdcall;
var
  C: TClass;
begin
  C := Instance.ClassType;
  while IsPaxClass(C) do
    C := C.ClassParent;
  Move(C, Pointer(Instance)^, SizeOf(Pointer));
end;

procedure _UpdateInstance2(Instance: TObject; C: TClass); stdcall;
begin
  Move(C, Pointer(Instance)^, SizeOf(Pointer));
end;

{$IFDEF NO_PARENT_CLASS}
procedure _ToParentClass(P: TBaseRunner;
                         Instance: TObject); stdcall;
begin
end;
procedure _UpdateInstance(P: TBaseRunner;
                          Instance: TObject); stdcall;
begin
end;
{$ELSE}
procedure _ToParentClass(P: TBaseRunner;
                         Instance: TObject); stdcall;
var
  C: TClass;
begin
{$IFNDEF PAXARM_DEVICE}
{$IFNDEF FPC}
  if not (Instance is TCustomForm) then
    Exit;
{$ENDIF}
{$ENDIF}

  C := Instance.ClassType;
  P.SavedClass := C;

  while IsPaxClass(C) do
    C := C.ClassParent;

  if Instance is TComponent then
     TComponent(Instance).Tag := Integer(P.SavedClass);

  Move(C, Pointer(Instance)^, SizeOf(Pointer));
end;

procedure _UpdateInstance(P: TBaseRunner;
                          Instance: TObject); stdcall;
var
  C: TClass;
begin
{$IFNDEF PAXARM_DEVICE}
{$IFNDEF FPC}
  if not (Instance is TCustomForm) then
    Exit;
{$ENDIF}
{$ENDIF}

  C := P.SavedClass;
  Move(C, Pointer(Instance)^, SizeOf(Pointer));
  P.SavedClass := nil;
end;
{$ENDIF}

{$IFNDEF PAXARM}
{$IFDEF PAX64}
procedure _DestroyInherited(Instance: TObject);
begin
  RaiseNotImpl;
end;
{$ELSE}
procedure _DestroyInherited(Instance: TObject);
var
  C, Temp: TClass;
begin
  Temp := Instance.ClassType;
  C := Instance.ClassParent;
  Move(C, Pointer(Instance)^, 4);
  asm
    xor edx, edx
    mov eax, instance
    mov ecx, [eax]
{$IFDEF FPC}
    mov ecx, [ecx + $30]
{$ELSE}
    mov ecx, [ecx - $04]
{$ENDIF}
    call ecx
  end;
  Move(Temp, Pointer(Instance)^, 4);
end;
{$ENDIF}
{$ENDIF}

{
procedure _DestroyInherited(Instance: TObject);
var
  C: TClass;
begin
  C := Instance.ClassParent;
  asm
    xor edx, edx
    mov eax, instance
    mov ecx, C
    mov ecx, [ecx - $04]
    call ecx
  end;
end;
}

{$IFDEF PAXARM}
procedure _ClassName(P: Pointer; result: PString); stdcall;
begin
  if IsDelphiClass(P) then
    result^ := TClass(P).ClassName
  else
    result^ := TObject(P).ClassName;
end;
{$ELSE}
procedure _ClassName(P: Pointer; result: PShortString); stdcall;
var
  ST: String;
begin
  if IsDelphiClass(P) then
    ST := TClass(P).ClassName
  else
    ST := TObject(P).ClassName;
  result^ := ShortString(ST);
end;
{$ENDIF}

procedure _OnCreateObject(P: TBaseRunner; Instance: TObject); stdcall;
begin
  if Assigned(P.OnCreateObject) then
    P.OnCreateObject(P.Owner, Instance);
end;

procedure _BeforeCallHost(P: TBaseRunner; Id: Integer); stdcall;
begin
  if Assigned(P.OnBeforeCallHost) then
    P.OnBeforeCallHost(P.Owner, Id);
end;

procedure _AfterCallHost(P: TBaseRunner; Id: Integer); stdcall;
begin
  if Assigned(P.OnAfterCallHost) then
    P.OnAfterCallHost(P.Owner, Id);
end;

procedure _OnCreateHostObject(P: TBaseRunner; Instance: TObject); stdcall;
begin
  if Instance is TFW_Object then
    (Instance as TFW_Object).prog := P;

  if Assigned(P.OnCreateHostObject) then
    P.OnCreateHostObject(P.Owner, Instance);
end;

procedure _OnDestroyHostObject(P: TBaseRunner; Instance: TObject); stdcall;
begin
  if Assigned(P.OnDestroyHostObject) then
    P.OnDestroyHostObject(P.Owner, Instance);
end;

procedure _OnAfterObjectCreation(Runner: Pointer; Instance: PObject); stdcall;
var
  P: TBaseRunner;
begin
  P := TBaseRunner(Runner);
  Instance^.AfterConstruction;
  if Assigned(P.OnAfterObjectCreation) then
    P.OnAfterObjectCreation(P.Owner, Instance^);
end;

{$IFNDEF PAXARM}
{$IFDEF PAX64}
function _InitInstance(Self: TClass; Instance: Pointer): TObject;
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  with Self do
  begin
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
        with IntfTable.Entries[I] do
        begin
          if VTable <> nil then
            PPointer(@PByte(Instance)[IOffset])^ := VTable;
        end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  end;
  Result := Instance;
end;
{$ELSE}
function _InitInstance(Self: TClass; Instance: Pointer): TObject;
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  with Self do
  begin
    ClassPtr := Self;
    while ClassPtr <> nil do
    begin
      IntfTable := ClassPtr.GetInterfaceTable;
      if IntfTable <> nil then
        for I := 0 to IntfTable.EntryCount-1 do
        with IntfTable.Entries[I] do
        begin
          if VTable <> nil then
            PInteger(@PAnsiChar(Instance)[IOffset])^ := Integer(VTable);
        end;
      ClassPtr := ClassPtr.ClassParent;
    end;
    Result := Instance;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF PAXARM}
procedure _ErrAbstract(S: PWideChar); stdcall;
begin
  raise Exception.Create(Format(ErrAbstractMethodCall, [S]));
end;
{$ELSE}
procedure _ErrAbstract(S: PAnsiChar); stdcall;
begin
  raise Exception.Create(Format(ErrAbstractMethodCall, [S]));
end;
{$ENDIF}

procedure _Finally(P: TBaseRunner); stdcall;
begin
  Inc(P.GetRootProg.FinallyCount);
end;

procedure _BeginExceptBlock(P: TBaseRunner); stdcall;
begin
  P.ProcessingExceptBlock := true;
end;

procedure _EndExceptBlock(P: TBaseRunner); stdcall;
begin
  P.ProcessingExceptBlock := false;

  if Assigned(P.CurrException) then
{$IFDEF ARC}
    P.CurrException := nil;
{$ELSE}
    P.CurrException.Free;
{$ENDIF}
  P.CurrException := nil;
end;

// processing breakpoints

procedure _CheckPause(P: TBaseRunner);
var
  SourceLine, ModuleIndex: Integer;
  HasBreakpoint: Boolean;
begin
  SourceLine := P.GetSourceLine;
  ModuleIndex := P.GetModuleIndex;

  HasBreakpoint :=
    P.RunTimeModuleList.BreakpointList.IndexOf(ModuleIndex, SourceLine) >= 0;

  if HasBreakpoint then
    P.Pause
  else
  begin
    if P.RunMode = rmRUN then
    begin
    end
    else if P.RunMode = rmTRACE_INTO then
      P.Pause
    else if P.RunMode = rmNEXT_SOURCE_LINE then
      P.Pause
    else if P.RunMode = rmSTEP_OVER then
    begin
      if P.RootInitCallStackCount >= P.GetCallStackCount then
        P.Pause;
    end else if P.RunMode = rmRUN_TO_CURSOR then
    begin
      if P.RunTimeModuleList.TempBreakpoint.SourceLine = SourceLine then
        if P.RunTimeModuleList.TempBreakpoint.ModuleIndex = ModuleIndex then
          P.Pause;
    end;
  end;
end;

// processing halt

procedure _Halt(Runner: Pointer; ExitCode: Integer); stdcall;
var
  P: TBaseRunner;
begin
  P := TBaseRunner(Runner);
  P.RootExceptionIsAvailableForHostApplication := false;
  P.ExitCode := ExitCode;
  raise THaltException.Create(ExitCode);
end;

// processing CondHalt

procedure _CondHalt(P: TBaseRunner); stdcall;
begin
  if P.IsHalted then
  begin
    P.RootExceptionIsAvailableForHostApplication := false;
    raise THaltException.Create(P.ExitCode);
  end;
end;

// processing of published properties

{$IFNDEF PAXARM}
procedure _GetAnsiStrProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: AnsiString); stdcall;
begin
{$IFDEF UNIC}
  Result := GetAnsiStrProp(Instance, PropInfo);
{$ELSE}
  result := GetStrProp(Instance, PropInfo);
{$ENDIF}
end;

procedure _SetAnsiStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: AnsiString); stdcall;
begin
{$IFDEF UNIC}
  SetAnsiStrProp(Instance, PropInfo, Value);
{$ELSE}
  SetStrProp(Instance, PropInfo, Value);
{$ENDIF}
end;

procedure _GetWideStrProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: WideString); stdcall;
begin
{$IFDEF VARIANTS}
  result := GetWideStrProp(Instance, PropInfo);
{$ELSE}
  result := '';
{$ENDIF}
end;

procedure _SetWideStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: WideString); stdcall;
begin
{$IFDEF VARIANTS}
  SetWideStrProp(Instance, PropInfo, Value);
{$ENDIF}
end;
{$ENDIF}

procedure _GetUnicStrProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: UnicString); stdcall;
begin
{$IFDEF VARIANTS}
  {$IFDEF UNIC}
  {$IFDEF PAXARM}
    result := GetStrProp(Instance, PropInfo);
  {$ELSE}
    result := GetUnicodeStrProp(Instance, PropInfo);
  {$ENDIF}
  {$ELSE}
    result := GetWideStrProp(Instance, PropInfo);
  {$ENDIF}
{$ELSE}
  result := '';
{$ENDIF}
end;

procedure _SetUnicStrProp(PropInfo: PPropInfo; Instance: TObject;
                      const value: UnicString); stdcall;
begin
{$IFDEF VARIANTS}
  {$IFDEF UNIC}
  {$IFDEF PAXARM}
    SetStrProp(Instance, PropInfo, Value);
  {$ELSE}
    SetUnicodeStrProp(Instance, PropInfo, Value);
  {$ENDIF}
  {$ELSE}
    SetWideStrProp(Instance, PropInfo, Value);
  {$ENDIF}
{$ENDIF}
end;

procedure _GetOrdProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: IntPax); stdcall;
begin
  result := GetOrdProp(Instance, PropInfo);
end;

procedure _SetOrdProp(PropInfo: PPropInfo; Instance: TObject; value: Integer);
stdcall;
begin
  SetOrdProp(Instance, PropInfo, Value);
end;

{$IFDEF VARIANTS}
procedure _GetInterfaceProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: IInterface); stdcall;
begin
{$IFDEF FPC}
  {$IFDEF LINUX}
  {$ELSE}
    result := GetInterfaceProp(Instance, PropInfo);
  {$ENDIF}
{$ELSE}
  result := GetInterfaceProp(Instance, PropInfo);
{$ENDIF}
end;

procedure _SetInterfaceProp(PropInfo: PPropInfo; Instance: TObject; value: IInterface);
stdcall;
begin
{$IFDEF FPC}
  {$IFDEF LINUX}
  {$ELSE}
    SetInterfaceProp(Instance, PropInfo, Value);
  {$ENDIF}
{$ELSE}
  SetInterfaceProp(Instance, PropInfo, Value);
{$ENDIF}
end;
{$ELSE}
procedure _GetInterfaceProp(PropInfo: PPropInfo; Instance: TObject;
                      var result: IUnknown); stdcall;
begin
//  result := GetInterfaceProp(Instance, PropInfo);
end;

procedure _SetInterfaceProp(PropInfo: PPropInfo; Instance: TObject; value: IUnknown);
stdcall;
begin
//  SetInterfaceProp(Instance, PropInfo, Value);
end;
{$ENDIF}

procedure _GetSetProp(PropInfo: PPropInfo;  Instance: TObject;
                       var result: TByteSet); stdcall;
var
  I: Integer;
begin
  I := GetOrdProp(Instance, PropInfo);
  result := Int32ToByteSet(I);
end;

procedure _SetSetProp(PropInfo: PPropInfo; Instance: TObject;
                      var value: TByteSet); stdcall;
var
  I: Integer;
begin
  I := ByteSetToInt32(value);
  SetOrdProp(Instance, PropInfo, I);
end;

procedure _GetFloatProp(PropInfo: PPropInfo; Instance: TObject;
                        var result: Extended); stdcall;
begin
  result := GetFloatProp(Instance, PropInfo);
end;

procedure _SetFloatProp(PropInfo: PPropInfo; Instance: TObject;
                        var value: Extended); stdcall;
begin
  SetFloatProp(Instance, PropInfo, Value);
end;

procedure _GetVariantProp(PropInfo: PPropInfo; Instance: TObject;
                          var result: Variant); stdcall;
begin
  result := GetVariantProp(Instance, PropInfo);
end;

procedure _SetVariantProp(PropInfo: PPropInfo; Instance: TObject;
                           var value: Variant); stdcall;
begin
  SetVariantProp(Instance, PropInfo, Value);
end;

procedure _GetInt64Prop(PropInfo: PPropInfo; Instance: TObject;
                        var result: Int64); stdcall;
begin
  result := GetInt64Prop(Instance, PropInfo);
end;

procedure _SetInt64Prop(PropInfo: PPropInfo; Instance: TObject;
                        var value: Int64); stdcall;
begin
  SetInt64Prop(Instance, PropInfo, Value);
end;

procedure _GetEventProp(PropInfo: PPropInfo; Instance: TObject;
                        var N: TMethod); stdcall;
begin
  N := GetMethodProp(Instance, PropInfo);
end;

procedure _CreateMethod(Data, Code: Pointer; var M: TMethod); stdcall;
begin
  M.Code := Code;
  M.Data := Data;
end;

procedure _RecordAssign(dest, source: Pointer; Size: Integer); stdcall;
begin
  Move(source^, dest^, Size);
end;

//------------------ Dynamic array support routines ----------------------------

procedure FreeDynarrayTVarRec(const A: DynarrayTVarRec);
var
  I: Integer;
begin
  for I:=0 to System.Length(A) - 1 do
  begin
    case A[I].VType of
      vtInt64:
        Dispose(PInt64(A[I].VInt64));
      vtExtended:
        Dispose(PExtended(A[I].VExtended));
      vtVariant:
        Dispose(PVariant(A[I].VVariant));
{$IFNDEF PAXARM}
      vtString:
        Dispose(PShortString(A[I].VString));
      vtWideString:
        WideString(A[I].VWideString) := '';
{$ENDIF}
      {$IFDEF UNIC}
      vtUnicodeString:
        UnicString(A[I].VUnicodeString) := '';
      {$ENDIF}
    end;
  end;
end;

procedure _ClearTVarRec(var Dest: TVarRec);
begin
  case Dest.VType of
    vtInt64:
      if Assigned(Dest.VInt64) then
      begin
        Dispose(PInt64(Dest.VInt64));
      end;
    vtExtended:
      if Assigned(Dest.VExtended) then
      begin
        Dispose(PExtended(Dest.VExtended));
      end;
{$IFNDEF PAXARM}
    vtString:
      if Assigned(Dest.VString) then
      begin
        Dispose(PShortString(Dest.VString));
      end;
{$ENDIF}
    vtVariant:
      if Assigned(Dest.VVariant) then
      begin
        Dispose(PVariant(Dest.VVariant));
      end;
{$IFNDEF PAXARM}
    vtWideString:
      if Assigned(Dest.VWideString) then
      begin
        WideString(Dest.VWideString) := '';
      end;
{$ENDIF}
    {$IFDEF UNIC}
    vtUnicodeString:
      if Assigned(Dest.VUnicodeString) then
      begin
        UnicString(Dest.VUnicodeString) := '';
      end;
    {$ENDIF}
  end;
  FillChar(Dest, SizeOf(Dest), 0);
end;

procedure _AssignTVarRec(P: Pointer;
                         Address: Pointer;
                         var Dest: TVarRec;
                         TypeID: Integer);
stdcall;
{$IFNDEF PAXARM}
var
  WS: WideString;
{$ENDIF}
begin
  _ClearTVarRec(Dest);
  case TypeId of
    typeINTEGER:
    begin
      Dest.VType := vtInteger;
      Dest.VInteger := Integer(Address^);
    end;
    typeBYTE:
    begin
      Dest.VType := vtInteger;
      Dest.VInteger := Byte(Address^);
    end;
    typeWORD:
    begin
      Dest.VType := vtInteger;
      Dest.VInteger := Word(Address^);
    end;
    typeSHORTINT:
    begin
      Dest.VType := vtInteger;
      Dest.VInteger := ShortInt(Address^);
    end;
    typeSMALLINT:
    begin
      Dest.VType := vtInteger;
      Dest.VInteger := SmallInt(Address^);
    end;
    typeCARDINAL:
    begin
      Dest.VType := vtInt64;
      New(PInt64(Dest.VInt64));
      Dest.VInt64^ := Cardinal(Address^);
    end;
    typeINT64, typeUINT64:
    begin
      Dest.VType := vtInt64;
      New(PInt64(Dest.VInt64));
      Dest.VInt64^ := Int64(Address^);
    end;
    typeCURRENCY:
    begin
      Dest.VType := vtCurrency;
      New(PCurrency(Dest.VCurrency));
      Dest.VCurrency^ := Currency(Address^);
    end;
    typeBOOLEAN:
    begin
      Dest.VType := vtBoolean;
      Dest.VBoolean := Boolean(Address^);
    end;
{$IFNDEF PAXARM}
    typeANSICHAR:
    begin
      Dest.VType := vtChar;
      Dest.VChar := AnsiChar(Address^);
    end;
    typeSHORTSTRING:
    begin
      Dest.VType := vtString;
      New(PShortString(Dest.VString));
      PShortString(Dest.VString)^ := PShortString(Address)^;
    end;
    typeANSISTRING:
    begin
      Dest.VType := vtAnsiString;
      AnsiString(Dest.VAnsiString) := PAnsiString(Address)^;
    end;
    typeWIDESTRING:
    begin
{$IFDEF UNIC}
      Dest.VType := vtUnicodeString;
      UnicString(Dest.VUnicodeString) := PWideString(Address)^;
{$ELSE}
      Dest.VType := vtString;
      New(PShortString(Dest.VString));
      PShortString(Dest.VString)^ := PWideString(Address)^;
{$ENDIF}
    end;
{$ENDIF}
    typeDOUBLE, typeSINGLE, typeEXTENDED:
    begin
      Dest.VType := vtExtended;
      New(PExtended(Dest.VExtended));
      case TypeId of
        typeDOUBLE: PExtended(Dest.VExtended)^ := PDouble(Address)^;
        typeSINGLE: PExtended(Dest.VExtended)^ := PSingle(Address)^;
        typeEXTENDED: PExtended(Dest.VExtended)^ := PExtended(Address)^;
      end;
    end;
    typePOINTER:
    begin
      Dest.VType := vtPointer;
      Dest.VPointer := Pointer(Address^);
    end;
{$IFNDEF PAXARM}
    typePANSICHAR:
    begin
      Dest.VType := vtPChar;
      Dest.VPChar := Pointer(Address^);
    end;
{$ENDIF}
    typeCLASS:
    begin
      Dest.VType := vtObject;
      Dest.VObject := TObject(Address^);
    end;
    typeCLASSREF:
    begin
      Dest.VType := vtClass;
      Dest.VClass := TClass(Address^);
    end;
    typeWIDECHAR:
    begin
      Dest.VType := vtWideChar;
      Dest.VWideChar := WideChar(Address^);
    end;
    typePWIDECHAR:
    begin
{$IFDEF PAXARM}
      Dest.VType := vtUnicodeString;
      UnicString(Dest.VUnicodeString) := PWideChar(Pointer(Address^));
{$ELSE}
{$IFDEF UNIC}
      Dest.VType := vtUnicodeString;
      _WideStringFromPWideChar(PWideChar(Pointer(Address^)), WS);
      UnicString(Dest.VUnicodeString) := WS;
{$ELSE}
      Dest.VType := vtString;
      New(PShortString(Dest.VString));
      _WideStringFromPWideChar(PWideChar(Pointer(Address^)), WS);
      PShortString(Dest.VString)^ := AnsiString(WS);
{$ENDIF}
{$ENDIF}
    end;
    typeVARIANT:
    begin
      Dest.VType := vtVariant;
      New(PVariant(Dest.VVariant));
      PVariant(Dest.VVariant)^ := PVariant(Address)^;
    end;
    typeUNICSTRING:
    begin
{$IFDEF UNIC}
      Dest.VType := vtUnicodeString;
      UnicString(Dest.VUnicodeString) := PUnicString(Address)^;
{$ELSE}
      Dest.VType := vtString;
      New(PShortString(Dest.VString));
      PShortString(Dest.VString)^ := PUnicString(Address)^;
{$ENDIF}
    end;
  end;
end;


type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPUX64}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$ENDIF}
    RefCnt: LongInt;
    Length: IntPax;
  end;

procedure _CreateEmptyDynarray(var A: Pointer); stdcall;
var
  P: Pointer;
begin
  if A <> nil then
  begin
    P := ShiftPointer(A, - SizeOf(TDynArrayRec));
    FreeMem(P, SizeOf(TDynArrayRec));
  end;
  P := AllocMem(SizeOf(TDynArrayRec));
  PDynArrayRec(P)^.RefCnt := 1;
  A := ShiftPointer(P, SizeOf(TDynArrayRec));
end;

function _DynarrayRefCount(P: Pointer): Integer;
begin
  if P = nil then
    result := -1
  else
  begin
    P := ShiftPointer(P, - SizeOf(TDynArrayRec));
    Result := PDynArrayRec(P)^.RefCnt;
  end;
end;

function _DynarrayLength(P: Pointer): Integer;
var
  Q: Pointer;
begin
  if P = nil then
    result := 0
  else
  begin
    Q := ShiftPointer(P, - SizeOf(TDynArrayRec));
    Result := PDynArrayRec(Q)^.Length;
{$IFDEF FPC}
    Inc(result);
{$ENDIF}
  end;
end;

procedure _DynarraySetLength(var A: Pointer; L: Integer;
                             ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;
var
  P: Pointer;
begin
  case ElFinalTypeID of
{$IFNDEF PAXARM}
    typeANSICHAR: SetLength(DynarrayChar(A), L);
    typeANSISTRING: SetLength(DynarrayString(A), L);
    typeSHORTSTRING: SetLength(DynarrayShortString(A), L);
    typeWIDESTRING: SetLength(DynarrayWideString(A), L);
{$ENDIF}
    typeBOOLEAN: SetLength(DynarrayBoolean(A), L);
    typeBYTE: SetLength(DynarrayByte(A), L);
    typeWORD: SetLength(DynarrayWord(A), L);
    typeCARDINAL: SetLength(DynarrayCardinal(A), L);
    typeINTEGER: SetLength(DynarrayInteger(A), L);
    typeDOUBLE: SetLength(DynarrayDouble(A), L);
    typePOINTER: SetLength(DynarrayPointer(A), L);
    typeENUM: SetLength(DynarrayInteger(A), L);
    typePROC: SetLength(DynarrayPointer(A), L);
    typeSINGLE: SetLength(DynarraySingle(A), L);
    typeEXTENDED: SetLength(DynarrayExtended(A), L);
    typeCURRENCY: SetLength(DynarrayCurrency(A), L);
    typeCLASS: SetLength(DynarrayPointer(A), L);
    typeCLASSREF: SetLength(DynarrayPointer(A), L);
    typeWIDECHAR: SetLength(DynarrayWideChar(A), L);
    typeUNICSTRING: SetLength(DynarrayUnicString(A), L);
    typeVARIANT: SetLength(DynarrayVariant(A), L);
    typeDYNARRAY: SetLength(DynarrayPointer(A), L);
    else
    begin
      if ElTypeID = H_TVarRec then
      begin
        SetLength(DynarrayTVarRec(A), L);
        Exit;
      end;

{$IFDEF FPC}
      Dec(L);
{$ENDIF}

      if A <> nil then
      begin
        P := ShiftPointer(A, - SizeOf(TDynArrayRec));
        ReallocMem(P, L * ElSize + SizeOf(TDynArrayRec));
        PDynArrayRec(P)^.Length := L;
        A := ShiftPointer(P, SizeOf(TDynArrayRec));
        Exit;
      end;

      A := AllocMem(SizeOf(TDynArrayRec) + L * ElSize);
      PDynArrayRec(A)^.RefCnt := 1;
      PDynArrayRec(A)^.Length := L;
      A := ShiftPointer(A, SizeOf(TDynArrayRec));
    end;
  end;
end;

procedure _DynarraySetLength2(var A: Pointer; L1, L2: Integer;
                              ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;
var
  I: Integer;
begin
  case ElFinalTypeID of
{$IFNDEF PAXARM}
    typeANSICHAR: SetLength(DynarrayChar2(A), L1, L2);
    typeANSISTRING: SetLength(DynarrayString2(A), L1, L2);
    typeSHORTSTRING: SetLength(DynarrayShortString2(A), L1, L2);
    typeWIDESTRING: SetLength(DynarrayWideString2(A), L1, L2);
{$ENDIF}
    typeBOOLEAN: SetLength(DynarrayBoolean2(A), L1, L2);
    typeBYTE: SetLength(DynarrayByte2(A), L1, L2);
    typeWORD: SetLength(DynarrayWord2(A), L1, L2);
    typeCARDINAL: SetLength(DynarrayCardinal2(A), L1, L2);
    typeINTEGER: SetLength(DynarrayInteger2(A), L1, L2);
    typeDOUBLE: SetLength(DynarrayDouble2(A), L1, L2);
    typePOINTER: SetLength(DynarrayPointer2(A), L1, L2);
    typeENUM: SetLength(DynarrayInteger2(A), L1, L2);
    typePROC: SetLength(DynarrayPointer2(A), L1, L2);
    typeSINGLE: SetLength(DynarraySingle2(A), L1, L2);
    typeEXTENDED: SetLength(DynarrayExtended2(A), L1, L2);
    typeCURRENCY: SetLength(DynarrayCurrency2(A), L1, L2);
    typeCLASS: SetLength(DynarrayPointer2(A), L1, L2);
    typeCLASSREF: SetLength(DynarrayPointer2(A), L1, L2);
    typeWIDECHAR: SetLength(DynarrayWideChar2(A), L1, L2);
    typeUNICSTRING: SetLength(DynarrayUnicString2(A), L1, L2);
    typeVARIANT: SetLength(DynarrayVariant2(A), L1, L2);
    typeDYNARRAY: SetLength(DynarrayPointer2(A), L1, L2);
    else
    begin
      _DynarraySetLength(A, L1, typePOINTER, 0, 0);
      for I := 0 to L1 - 1 do
        _DynarraySetLength(DynarrayPointer(A)[I], L2, ElFinalTypeId, ElTypeId, ElSize);
    end;
  end;
end;

procedure _DynarraySetLength3(var A: Pointer; L1, L2, L3: Integer;
                              ElFinalTypeID, ElTypeID, ElSize: Integer); stdcall;
type
  DynarrayPointer2 = array of array of Pointer;
var
  I, J: Integer;
begin
  case ElFinalTypeID of
{$IFNDEF PAXARM}
    typeANSICHAR: SetLength(DynarrayChar3(A), L1, L2, L3);
    typeANSISTRING: SetLength(DynarrayString3(A), L1, L2, L3);
    typeSHORTSTRING: SetLength(DynarrayShortString3(A), L1, L2, L3);
    typeWIDESTRING: SetLength(DynarrayWideString3(A), L1, L2, L3);
{$ENDIF}
    typeBOOLEAN: SetLength(DynarrayBoolean3(A), L1, L2, L3);
    typeBYTE: SetLength(DynarrayByte3(A), L1, L2, L3);
    typeWORD: SetLength(DynarrayWord3(A), L1, L2, L3);
    typeCARDINAL: SetLength(DynarrayCardinal3(A), L1, L2, L3);
    typeINTEGER: SetLength(DynarrayInteger3(A), L1, L2, L3);
    typeDOUBLE: SetLength(DynarrayDouble3(A), L1, L2, L3);
    typePOINTER: SetLength(DynarrayPointer3(A), L1, L2, L3);
    typeENUM: SetLength(DynarrayInteger3(A), L1, L2, L3);
    typePROC: SetLength(DynarrayPointer3(A), L1, L2, L3);
    typeSINGLE: SetLength(DynarraySingle3(A), L1, L2, L3);
    typeEXTENDED: SetLength(DynarrayExtended3(A), L1, L2, L3);
    typeCURRENCY: SetLength(DynarrayCurrency3(A), L1, L2, L3);
    typeCLASS: SetLength(DynarrayPointer3(A), L1, L2, L3);
    typeCLASSREF: SetLength(DynarrayPointer3(A), L1, L2, L3);
    typeWIDECHAR: SetLength(DynarrayWideChar3(A), L1, L2, L3);
    typeUNICSTRING: SetLength(DynarrayUnicString3(A), L1, L2, L3);
    typeVARIANT: SetLength(DynarrayVariant3(A), L1, L2, L3);
    typeDYNARRAY: SetLength(DynarrayPointer3(A), L1, L2, L3);
  else
   begin
  _DynarraySetLength2(A, L1, L2, typePOINTER, 0, 0);
  for I := 0 to L1 - 1 do
    for J := 0 to L2 - 1 do
      _DynarraySetLength(DynarrayPointer2(A)[I][J], L2, ElFinalTypeId, ElTypeId, ElSize);
   end;
  end;
end;

procedure _DynarrayHigh(var P: Pointer; var result: Integer); stdcall;
begin
  result := _DynarrayLength(P) - 1;
end;

function _DynarrayIncRefCount(P: Pointer): Integer;
var
  Q: Pointer;
begin
  if P <> nil then
  begin
    Q := ShiftPointer(P, - SizeOf(TDynArrayRec));
    Inc(PDynArrayRec(Q)^.RefCnt, 1);
    result := _DynarrayRefCount(P);
  end
  else
    result := 0;
end;

function _DynarrayDecRefCount(P: Pointer): Integer;
var
  Q: Pointer;
begin
  if P <> nil then
  begin
    Q := ShiftPointer(P, - SizeOf(TDynArrayRec));
    Dec(PDynArrayRec(Q)^.RefCnt, 1);
    result := _DynarrayRefCount(P);
  end
  else
    result := 0;
end;

procedure _DynarrayClr(var A: Pointer;
                       FinalTypeID, TypeID, ElSize,
                       FinalTypeID2, TypeID2, ElSize2: Integer); stdcall;
var
  P: Pointer;
  I, K, L: Integer;
begin
  case FinalTypeID of
{$IFNDEF PAXARM}
    typeANSISTRING: DynarrayString(A) := nil;
    typeWIDESTRING: DynarrayWideString(A) := nil;
    typeANSICHAR: DynarrayChar(A) := nil;
    typeSHORTSTRING: DynarrayShortString(A) := nil;
{$ENDIF}
    typeUNICSTRING: DynarrayUnicString(A) := nil;
    typeVARIANT, typeOLEVARIANT: DynarrayVariant(A) := nil;

    typeBOOLEAN: DynarrayBoolean(A) := nil;
    typeBYTE: DynarrayByte(A) := nil;
    typeWORD: DynarrayWord(A) := nil;
    typeCARDINAL: DynarrayCardinal(A) := nil;
    typeINTEGER: DynarrayInteger(A) := nil;
    typeDOUBLE: DynarrayDouble(A) := nil;
    typePOINTER: DynarrayPointer(A) := nil;
    typeENUM: DynarrayInteger(A) := nil;
    typePROC: DynarrayPointer(A) := nil;
    typeSINGLE: DynarraySingle(A) := nil;
    typeEXTENDED: DynarrayExtended(A) := nil;
    typeCURRENCY: DynarrayCurrency(A) := nil;
    typeCLASS: DynarrayPointer(A) := nil;
    typeCLASSREF: DynarrayPointer(A) := nil;
    typeWIDECHAR: DynarrayWideChar(A) := nil;

    typeDYNARRAY:
    begin
      if A <> nil then
      begin
        for I := 0 to System.Length(DynarrayPointer(A)) - 1 do
          _DynarrayClr(DynarrayPointer(A)[I],
             FinalTypeID2, TypeID2, ElSize2, 0, 0, 0);
        DynarrayPointer(A) := nil;
      end;
    end
    else
    begin
      if A <> nil then
      begin
        if TypeID = H_TVarRec then
        begin
          K := _DynarrayRefCount(A);
          if K = 1 then
          begin
{$ifdef GE_DXETOKYO}   // XILINX, Tokyo mod
            FreeDynarrayTVarRec(DynarrayTVarRec(A));
{$else}
            FreeDynarrayTVarRec(A);
{$endif}
            DynarrayTVarRec(A) := nil;
            Exit;
          end;
        end;

        K := _DynarrayRefCount(A);
        if K > 1 then
        begin
          _DynarrayDecRefCount(A);
          Exit;
        end;

        L := _DynarrayLength(A);
        P := ShiftPointer(A, - SizeOf(TDynArrayRec));
        FreeMem(P, L * ElSize + SizeOf(TDynArrayRec));
        A := nil;
      end;
    end;
  end;
end;

procedure _DynarrayClr1(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
var
  P: Pointer;
  K, L: Integer;
begin
  case FinalTypeID of
{$IFNDEF PAXARM}
    typeANSISTRING: DynarrayString(A) := nil;
    typeWIDESTRING: DynarrayWideString(A) := nil;
    typeANSICHAR: DynarrayChar(A) := nil;
    typeSHORTSTRING: DynarrayShortString(A) := nil;
{$ENDIF}
    typeUNICSTRING: DynarrayUnicString(A) := nil;
    typeVARIANT, typeOLEVARIANT: DynarrayVariant(A) := nil;

    typeBOOLEAN: DynarrayBoolean(A) := nil;
    typeBYTE: DynarrayByte(A) := nil;
    typeWORD: DynarrayWord(A) := nil;
    typeCARDINAL: DynarrayCardinal(A) := nil;
    typeINTEGER: DynarrayInteger(A) := nil;
    typeDOUBLE: DynarrayDouble(A) := nil;
    typePOINTER: DynarrayPointer(A) := nil;
    typeENUM: DynarrayInteger(A) := nil;
    typePROC: DynarrayPointer(A) := nil;
    typeSINGLE: DynarraySingle(A) := nil;
    typeEXTENDED: DynarrayExtended(A) := nil;
    typeCURRENCY: DynarrayCurrency(A) := nil;
    typeCLASS: DynarrayPointer(A) := nil;
    typeCLASSREF: DynarrayPointer(A) := nil;
    typeWIDECHAR: DynarrayWideChar(A) := nil;

    else
    begin
      if A <> nil then
      begin
        if TypeID = H_TVarRec then
        begin
          K := _DynarrayRefCount(A);
          if K = 1 then
          begin
{$ifdef GE_DXETOKYO}   // XILINX, Tokyo mod
            FreeDynarrayTVarRec(DynarrayTVarRec(A));
{$else}
            FreeDynarrayTVarRec(A);
{$endif}
            DynarrayTVarRec(A) := nil;
            Exit;
          end;
        end;

        K := _DynarrayRefCount(A);
        if K > 1 then
        begin
          _DynarrayDecRefCount(A);
          Exit;
        end;

        L := _DynarrayLength(A);
        P := ShiftPointer(A, - SizeOf(TDynArrayRec));
        FreeMem(P, L * ElSize + SizeOf(TDynArrayRec));
        A := nil;
      end;
    end;
  end;
end;

procedure _DynarrayClr2(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
var
  I: Integer;
begin
  if A = nil then
    Exit;
  for I := 0 to System.Length(DynarrayPointer(A)) - 1 do
    _DynarrayClr1(DynarrayPointer(A)[I], FinalTypeID, TypeID, ElSize);
  DynarrayPointer(A) := nil;
end;

procedure _DynarrayClr3(var A: Pointer;
                        FinalTypeID, TypeID, ElSize: Integer); stdcall;
var
  I: Integer;
begin
  if A = nil then
    Exit;
  for I := 0 to System.Length(DynarrayPointer(A)) - 1 do
    _DynarrayClr2(DynarrayPointer(A)[I], FinalTypeID, TypeID, ElSize);
  DynarrayPointer(A) := nil;
end;

procedure _DynarrayAssign(var Source, Dest: Pointer;
                          FinalTypeID, TypeID, ElSize,
                          FinalTypeID2, TypeID2, ElSize2: Integer); stdcall;
var
  K: Integer;
begin
  if Source = nil then
  begin
    _DynArrayClr(Dest, FinalTypeId, TypeId, ElSize,
                 FinalTypeId2, TypeId2, ElSize2);
    Exit;
  end;
  _DynarrayIncRefCount(Source);
  if Dest <> nil then
  begin
    K := _DynarrayDecRefCount(Dest);
    if K = 0 then
      _DynArrayClr(Dest, FinalTypeId, TypeId, ElSize,
                   FinalTypeId2, TypeId2, ElSize2);
  end;
  Dest := Source;
end;

function _VariantLength(const V: Variant): Integer;
var
  VT: Word;
  I: Integer;
begin
  VT := VarType(V);
  if VT < varArray then
    result := Length(V)
  else
  begin
    result := 1;
    for I := 1 to VarArrayDimCount(V) do
      result := result * (VarArrayHighBound(V, I) + 1);
  end;
end;

procedure _LockVArray(var V: Variant; var Result: Pointer); stdcall;
begin
  result := VarArrayLock(V);
end;

procedure _UnlockVArray(var V: Variant); stdcall;
begin
  VarArrayUnlock(V);
end;

procedure _IntOver; stdcall;
begin
  raise EIntOverflow.Create(errIntegerOverflow);
end;

procedure _BoundError; stdcall;
begin
  raise ERangeError.Create(errRangeCheckError);
end;

procedure _StringAddRef(var S: Pointer); stdcall;
var
  P: PStringRec;
begin
  if S <> nil then
  begin
    P := Pointer(Integer(S) - Sizeof(TStringRec));
    Inc(P^.RefCount);
  end;
end;

procedure _DynarrayAddRef(P: Pointer); stdcall;
begin
  _DynarrayIncRefCount(P);
end;

procedure _InterfaceAddRef(var I: Pointer); stdcall;
begin
  IUnknown(I)._AddRef;
end;

procedure _VariantAddRef(var V: Variant); stdcall;
var
  VT: Integer;
begin
  VT := VarType(V);
{$IFNDEF PAXARM}
  if VT = varString then
    _StringAddRef(TVarData(V).VString)
  else if VT = varOleStr then
    _StringAddRef(Pointer(TVarData(V).VOleStr))
  else
{$ENDIF}
{$IFDEF UNIC}
  if VT = varUString then
    _StringAddRef(Pointer(TVarData(V).VUString))
  else
{$ENDIF}
  if VT = varDispatch then
    _InterfaceAddRef(TVarData(V).VDispatch);
end;

{$IFDEF PAXARM}
function TObject_ClassName(IsClass: Integer; P: Pointer): String; stdcall;
begin
  if IsClass = 1 then
    result := TClass(P).ClassName
  else
    result := TObject(P).ClassName;
end;
{$ELSE}
function TObject_ClassName(IsClass: Integer; P: Pointer): ShortString; stdcall;
var
  X: TObject;
  S: String;
begin
  if IsClass = 1 then
  begin
    S := TClass(P).ClassName;
    result := ShortString(S);
  end
  else
  begin
    X := TObject(P);
    S := X.ClassName;
    result := ShortString(S);
  end;
end;
{$ENDIF}

function _GetProgAddress: Pointer;
begin
  result := @ CurrProg;
end;

function GetRefCount(Self: TInterfacedObject): Integer;
begin
  result := Self.RefCount;
end;

function _Round(E: Extended): Int64;
begin
  result := Round(E);
end;

procedure _GetComponent(X: TComponent; I: Integer; var Y: TComponent); stdcall;
begin
  Y := X.Components[I];
end;

procedure Dummy;
begin
end;

// TExtraImportTableList -------------------------------------------------------

function TExtraImportTableList.GetRecord(I: Integer): TBaseSymbolTable;
begin
  result := TBaseSymbolTable(L[I]);
end;

function TExtraImportTableList.Import(CurrentTable: TBaseSymbolTable;
                                      const FullName: String;
                                      UpCase: Boolean;
                                      DoRaiseError: Boolean = true): Integer;
var
  I: Integer;
begin
  result := 0;
  for I := 0 to Count - 1 do
  begin
    result := CurrentTable.ImportFromTable(Records[I], FullName, UpCase, DoRaiseError);
    if result <> 0 then
      Exit;
  end;
end;

function TExtraImportTableList.Add(st: TBaseSymbolTable): Integer;
begin
  result := L.Add(st);
end;

procedure TExtraImportTableList.Remove(st: TBaseSymbolTable);
var
  I: Integer;
begin
  I := L.IndexOf(st);
  if I >= 0 then
    L.Delete(I);
  FreeAndNil(st);
end;

//------------------------------------------------------------------------------

procedure TMyInterfacedObject_AddRef(Self: TObject); stdcall;
begin
  TMyInterfacedObject(Self)._AddRef;
end;

procedure TMyInterfacedObject_Release(Self: TObject); stdcall;
begin
  TMyInterfacedObject(Self)._Release;
end;

function TObject_GetInterface(Self: TObject; const IID: TGUID; out Obj): Boolean;
begin
  result := Self.GetInterface(IID, Obj);
end;

procedure Set_ExitCode(P: TBaseRunner; value: Integer); stdcall;
begin
  P.ExitCode := value;
end;

function Get_ExitCode(P: TBaseRunner): Integer; stdcall;
begin
  result := P.ExitCode;
end;

procedure _GetDynamicMethodAddress(AClass: TClass; Id: integer;
                                   var result: Pointer); stdcall;
var
  Dmt: PDmtTable;
  DmtMethodList: PDmtMethodList;
  I: Integer;
  C: TClass;
begin
  Result := nil;

  C := AClass;
  repeat
    Dmt := GetDmtFromClass(C);
    if Assigned(Dmt) then
    begin
      DmtMethodList := @Dmt^.IndexList[Dmt.Count];
      for I := 0 to Dmt^.Count - 1 do
        if Dmt^.IndexList[I] = Id then
        begin
          Result := DmtMethodList[I];
          Exit;
        end;
    end;

    C := C.ClassParent;
    if C = nil then
      break;
  until false;
end;

procedure _CallVirt(Runner: TBaseRunner; ObjectName, PropName: PChar; A: array of Variant; var Result: Variant); stdcall;
begin
  if not Assigned(Runner.OnVirtualObjectMethodCall) then
     Runner.RaiseError(errVirtualObjectMethodCallEventNotAssigned, []);
  Runner.OnVirtualObjectMethodCall(Runner.Owner, String(ObjectName), String(PropName), A, result);
end;

procedure _PutVirt(Runner: TBaseRunner; ObjectName, PropName: PChar; A: array of Variant; const value: Variant); stdcall;
begin
  if not Assigned(Runner.OnVirtualObjectPutProperty) then
     Runner.RaiseError(errVirtualObjectPutPropertyEventNotAssigned, []);
  Runner.OnVirtualObjectPutProperty(Runner.Owner, String(ObjectName), String(PropName), A, value);
end;

procedure _AddMessage(Runner: Pointer; msg_id: Integer; FullName: PChar); stdcall;
var
  R: TMessageRec;
  I: Integer;
  P: TBaseRunner;
begin
  P := TBaseRunner(Runner);
  I := P.MessageList.IndexOf(FullName);
  if I >= 0 then
    Exit;

  R := P.MessageList.AddRecord;
  R.msg_id := msg_id;
  R.FullName := FullName;
end;

procedure _TypeInfo(Prog: Pointer; FullTypeName: PChar; var result: PTypeInfo); stdcall;
var
  R: TTypeInfoContainer;
{$ifdef DRTTI}
  t: TRTTIType;
  PackageInfo: PPackageTypeInfo;
  lib: PLibModule;
  lp: PByte;
  i: integer;
  aName: String;
  aUnit: String;
  procedure PeekData(var P: PByte; var Data; Len: Integer);
  begin
    Move(P^, Data, Len);
  end;
  procedure ReadData(var P: PByte; var Data; Len: Integer);
  begin
    PeekData(P, Data, Len);
    Inc(P, Len);
  end;
  function ReadU8(var P: PByte): Byte;
  begin
    ReadData(P, Result, SizeOf(Result));
  end;
  function ReadShortString(var P: PByte): string;
  var
    len: Integer;
  begin
    Result := UTF8ToString(PShortString(P)^);
    len := ReadU8(P);
    Inc(P, len);
  end;
{$endif}
var P: TBaseRunner;
begin
  P := TBaseRunner(Prog);
  R := P.ProgTypeInfoList.LookupFullName(FullTypeName);
  if R = nil then
  begin
{$ifdef DRTTI}
    t := PaxContext.FindType(FullTypeName);
    if t = nil then
      t := PaxContext.FindType(ExtractName(FullTypeName));
    aName := ExtractName(FullTypeName);
    lib  := LibModuleList;
    while lib <> nil do
    begin
      PackageInfo := lib^.TypeInfo;
      if PackageInfo <> nil then
      begin
        lp := Pointer(PackageInfo^.UnitNames);
        for i := 0 to PackageInfo^.UnitCount - 1 do
        begin
          aUnit := ReadShortString(lp);
          t := PaxContext.FindType(aUnit + '.' + aName);
          if t <> nil then
            break;
        end;
      end;
      if t <> nil then
        break;
      lib := lib.Next;
    end;
    if t = nil then
      result := nil
    else
      result := t.Handle;
{$else}
    result := nil;
{$endif}
  end
  else
    result := R.TypeInfoPtr;
end;

function GetPointerType(T: Integer): Integer;
begin
  if T = typeINTEGER then
    result := H_PInteger
  else if T = typeSMALLINT then
    result := H_PSmallInt
  else if T = typeSHORTINT then
    result := H_PShortInt
  else if T = typeCARDINAL then
    result := H_PCardinal
  else if T = typeWORD then
    result := H_PWord
  else if T = typeBYTE then
    result := H_PByte
  else if T = typeINT64 then
    result := H_PInt64
  else if T = typeSINGLE then
    result := H_PSingle
  else if T = typeDOUBLE then
    result := H_PDouble
  else if T = typeEXTENDED then
    result := H_PExtended
  else if T = typeCURRENCY then
    result := H_PCurrency
  else if T = typeVARIANT then
    result := H_PVariant
  else if T = typePOINTER then
    result := H_PPointer
  else if T = typeBOOLEAN then
    result := H_PBoolean
  else if T = typeWIDECHAR then
    result := H_PWideChar
{$IFNDEF PAXARM}
  else if T = typeANSICHAR then
    result := H_PAnsiChar
  else if T = typeSHORTSTRING then
    result := H_PShortString
  else if T = typeANSISTRING then
    result := H_PAnsiString
  else if T = typeWIDESTRING then
    result := H_PWideString
{$ENDIF}
  else if T = typeUNICSTRING then
    result := H_PUnicString

  else if T = H_PINTEGER then
    result := H_PPInteger
  else if T = H_PSMALLINT then
    result := H_PPSmallInt
  else if T = H_PSHORTINT then
    result := H_PPShortInt
  else if T = H_PCARDINAL then
    result := H_PPCardinal
  else if T = H_PWORD then
    result := H_PPWord
  else if T = H_PBYTE then
    result := H_PPByte
  else if T = H_PINT64 then
    result := H_PPInt64
  else if T = H_PSINGLE then
    result := H_PPSingle
  else if T = H_PDOUBLE then
    result := H_PPDouble
  else if T = H_PEXTENDED then
    result := H_PPExtended
  else if T = H_PCURRENCY then
    result := H_PPCurrency
  else if T = H_PVARIANT then
    result := H_PPVariant
  else if T = H_PPOINTER then
    result := H_PPPointer
  else if T = H_PBOOLEAN then
    result := H_PPBoolean
  else if T = typeWIDECHAR then
    result := H_PWideChar
{$IFNDEF PAXARM}
  else if T = H_PANSICHAR then
    result := H_PPAnsiChar
  else if T = H_PSHORTSTRING then
    result := H_PPShortString
  else if T = H_PANSISTRING then
    result := H_PPAnsiString
  else if T = H_PWIDESTRING then
    result := H_PPWideString
{$ENDIF}
  else if T = H_PUNICSTRING then
    result := H_PPUnicString
  else
    result := 0;
end;

var
  Unassigned: Variant;

procedure Register_TYPEINFO(H_Namespace: Integer; st: TBaseSymbolTable);
var
  H_Sub: Integer;
begin
  with st do
  begin
    H_Sub := RegisterRoutine(H_Namespace, 'TypeInfo', typeVOID, ccSTDCALL, nil);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _TypeInfo);
    Id_TypeInfo := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
  end;
end;

procedure FindAvailTypes;
begin
  if AvailTypeList.Count = 0 then
  begin
{$IFDEF DRTTI}
  CreateAvailTypes;
{$ENDIF}
  end;
end;

type
  TMyObject = class(TObject);

const
  ByRef = true;

procedure AddStdRoutines(st: TBaseSymbolTable);
var
  H_Namespace, H_Sub, H, T1, T2, T3, T4, T5: Integer;
  H_R0_7, H_TGUID_D4, H_TEntries, H_TPOINT: Integer;
begin
  with st do
  begin
    Reset;

    RegisterRoutine(0, strWrite, typeVOID,  ccREGISTER, @_Write);

    H_Sub := RegisterRoutine(0, strWriteln, typeVOID, ccREGISTER, @_Writeln);
    H_Writeln := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteBool);
    Id_WriteBool := LastSubId;
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned);
    H_WriteBool := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteByteBool);
    Id_WriteByteBool := LastSubId;
    RegisterParameter(H_Sub, typeBYTEBOOL, Unassigned);
    H_WriteByteBool := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteWordBool);
    Id_WriteWordBool := LastSubId;
    RegisterParameter(H_Sub, typeWORDBOOL, Unassigned);
    H_WriteWordBool := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteLongBool);
    Id_WriteLongBool := LastSubId;
    RegisterParameter(H_Sub, typeLONGBOOL, Unassigned);
    H_WriteLongBool := H_Sub;
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteAnsiChar);
    Id_WriteAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned);
    H_WriteAnsiChar := H_Sub;
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteByte);
    Id_WriteByte := LastSubId;
    RegisterParameter(H_Sub, typeBYTE, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteByte := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteWord);
    Id_WriteWord := LastSubId;
    RegisterParameter(H_Sub, typeWORD, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteWord := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteCardinal);
    Id_WriteCardinal := LastSubId;
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteCardinal := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteSmallInt);
    Id_WriteSmallInt := LastSubId;
    RegisterParameter(H_Sub, typeSMALLINT, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteSmallInt := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteShortInt);
    Id_WriteShortInt := LastSubId;
    RegisterParameter(H_Sub, typeSHORTINT, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteShortInt := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteInt);
    Id_WriteInt := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteInteger := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteInt64);
    Id_WriteInt64 := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteInt64 := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteDouble);
    Id_WriteDouble := LastSubId;
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteDouble := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteSingle);
    Id_WriteSingle := LastSubId;
    RegisterParameter(H_Sub, typeSINGLE, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteSingle := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteCurrency);
    Id_WriteCurrency := LastSubId;
    RegisterParameter(H_Sub, typeCURRENCY, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteCurrency := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteExtended);
    Id_WriteExtended := LastSubId;
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteExtended := H_Sub;
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteString);
    Id_WriteString := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned);
    Records[Card].IsConst := true;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteAnsiString := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteShortString);
    Id_WriteShortString := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteShortString := H_Sub;
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteWideChar);
    Id_WriteWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    H_WriteWideChar := H_Sub;
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteWideString);
    Id_WriteWideString := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteWideString := H_Sub;
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteVariant);
    Id_WriteVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteVariant := H_Sub;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteObject);
    Id_WriteObject := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    H_WriteObject := H_Sub;

//--------- SET ROUTINES -------------------------------------------------------

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetInclude);
    Id_SetInclude := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetIncludeInterval);
    Id_SetIncludeInterval := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID,  ccSTDCALL, @_SetExclude);
    Id_SetExclude := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL,  @_SetUnion);
    Id_SetUnion := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetDifference);
    Id_SetDifference := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetIntersection);
    Id_SetIntersection := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeBOOLEAN, ccSTDCALL, @_SetSubset);
    Id_SetSubset := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeBOOLEAN, ccSTDCALL, @_SetSuperset);
    Id_SetSuperset := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeBOOLEAN, ccSTDCALL, @_SetEquality);
    Id_SetEquality := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeBOOLEAN, ccSTDCALL, @_SetInequality);
    Id_SetInequality := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeBOOLEAN, ccSTDCALL, @_SetMembership);
    Id_SetMembership := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typePOINTER, ccSTDCALL, @ _LoadProc);
    Id_LoadProc := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

//--------- String ROUTINES ----------------------------------------------------

{$IFNDEF PAXARM}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _AnsiStringFromPAnsiChar);
    Id_AnsiStringFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromPAnsiChar);
    Id_WideStringFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromPWideChar);
    Id_WideStringFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromAnsiChar);
    Id_AnsiStringFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromAnsiChar);
    Id_WideStringFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _WideStringFromWideChar);
    Id_WideStringFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _AnsiStringFromWideChar);
    Id_AnsiStringFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringAssign);
    Id_AnsiStringAssign := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringAssign);
    Id_WideStringAssign := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringAddition);
    Id_AnsiStringAddition := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringAddition);
    Id_WideStringAddition := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringAssign);
    Id_ShortStringAssign := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringAddition);
    Id_ShortStringAddition := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringClr);
    Id_AnsiStringClr := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringClr);
    Id_WideStringClr := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_InterfaceClr);
    Id_InterfaceClr := LastSubId;
    RegisterParameter(H_Sub, typeINTERFACE, Unassigned, ByRef);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StringAddRef);
    Id_StringAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StringAddRef);
    Id_WideStringAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);
{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_DynarrayAddRef);
    Id_DynarrayAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_InterfaceAddRef);
    Id_InterfaceAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantAddRef);
    Id_VariantAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringFromAnsiString);
    Id_ShortStringFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringFromWideString);
    Id_ShortStringFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromShortString);
    Id_AnsiStringFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromShortString);
    Id_WideStringFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromWideString);
    Id_AnsiStringFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromAnsiString);
    Id_WideStringFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StrInt);
    Id_StrInt := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StrDouble);
    Id_StrDouble := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StrSingle);
    Id_StrSingle := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeSINGLE, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StrExtended);
    Id_StrExtended := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned);

    RegisterRoutine(0, '', typeVOID, ccREGISTER, @_DecStringCounter);
    Id_DecStringCounter := LastSubId;
{$IFDEF FPC}
    RegisterRoutine(0, '', typeVOID, ccREGISTER, @_IncStringCounter);
    Id_IncStringCounter := LastSubId;
{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringEquality);
    Id_AnsiStringEquality := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringNotEquality);
    Id_AnsiStringNotEquality := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringEquality);
    Id_ShortStringEquality := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringNotEquality);
    Id_ShortStringNotEquality := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringEquality);
    Id_WideStringEquality := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringNotEquality);
    Id_WideStringNotEquality := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeINTEGER, ccSTDCALL, @_ShortstringHigh);
    Id_ShortstringHigh := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _AnsiStringGreaterThan);
    Id_AnsiStringGreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringGreaterThanOrEqual);
    Id_AnsiStringGreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringLessThan);
    Id_AnsiStringLessThan := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringLessThanOrEqual);
    Id_AnsiStringLessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _ShortStringGreaterThan);
    Id_ShortStringGreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringGreaterThanOrEqual);
    Id_ShortStringGreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringLessThan);
    Id_ShortStringLessThan := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringLessThanOrEqual);
    Id_ShortStringLessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _WideStringGreaterThan);
    Id_WideStringGreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringGreaterThanOrEqual);
    Id_WideStringGreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringLessThan);
    Id_WideStringLessThan := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringLessThanOrEqual);
    Id_WideStringLessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetStringLength);
    Id_SetStringLength := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetWideStringLength);
    Id_SetWideStringLength := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetShortStringLength);
    Id_SetShortStringLength := LastSubId;
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetVariantLength);
    Id_SetVariantLength := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

//--------- INT64 ROUTINES -----------------------------------------------------

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64Multiplication);
    Id_Int64Multiplication := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64Division);
    Id_Int64Division := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64Modulo);
    Id_Int64Modulo := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64LeftShift);
    Id_Int64LeftShift := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64RightShift);
    Id_Int64RightShift := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64LessThan);
    Id_Int64LessThan := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64LessThanOrEqual);
    Id_Int64LessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64GreaterThan);
    Id_Int64GreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64GreaterThanOrEqual);
    Id_Int64GreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64Equality);
    Id_Int64Equality := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64NotEquality);
    Id_Int64NotEquality := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _Int64Abs);
    Id_Int64Abs := LastSubId;
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

// uint64

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UInt64LessThan);
    Id_UInt64LessThan := LastSubId;
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UInt64LessThanOrEqual);
    Id_UInt64LessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UInt64GreaterThan);
    Id_UInt64GreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UInt64GreaterThanOrEqual);
    Id_UInt64GreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUINT64, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

//--------- VARIANT ROUTINES ---------------------------------------------------

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromVariant);
    Id_OleVariantFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantAssign);
    Id_VariantAssign := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _OleVariantAssign);
    Id_OleVariantAssign := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromPAnsiChar);
    Id_VariantFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromPAnsiChar);
    Id_OleVariantFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInterface);
    Id_VariantFromInterface := LastSubId;
    RegisterParameter(H_Sub, typeINTERFACE, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromInterface);
    Id_OleVariantFromInterface := LastSubId;
    RegisterParameter(H_Sub, typeINTERFACE, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromAnsiString);
    Id_VariantFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromAnsiString);
    Id_OleVariantFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWideString);
    Id_VariantFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromWideString);
    Id_OleVariantFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromShortString);
    Id_VariantFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromShortString);
    Id_OleVariantFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromAnsiChar);
    Id_VariantFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromAnsiChar);
    Id_OleVariantFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWideChar);
    Id_VariantFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromWideChar);
    Id_OleVariantFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInt);
    Id_VariantFromInt := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromInt);
    Id_OleVariantFromInt := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromInt64);
    Id_VariantFromInt64 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromInt64);
    Id_OleVariantFromInt64 := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantFromByte);
    Id_VariantFromByte := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBYTE, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _OleVariantFromByte);
    Id_OleVariantFromByte := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBYTE, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromBool);
    Id_VariantFromBool := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromBool);
    Id_OleVariantFromBool := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromWord);
    Id_VariantFromWord := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWORD, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromWord);
    Id_OleVariantFromWord := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWORD, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromCardinal);
    Id_VariantFromCardinal := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromCardinal);
    Id_OleVariantFromCardinal := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromSmallInt);
    Id_VariantFromSmallInt := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSMALLINT, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromSmallInt);
    Id_OleVariantFromSmallInt := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSMALLINT, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromShortInt);
    Id_VariantFromShortInt := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTINT, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromShortInt);
    Id_OleVariantFromShortInt := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTINT, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromDouble);
    Id_VariantFromDouble := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromDouble);
    Id_OleVariantFromDouble := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromCurrency);
    Id_VariantFromCurrency := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCURRENCY, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromCurrency);
    Id_OleVariantFromCurrency := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCURRENCY, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromSingle);
    Id_VariantFromSingle := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSINGLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromSingle);
    Id_OleVariantFromSingle := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSINGLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromExtended);
    Id_VariantFromExtended := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromExtended);
    Id_OleVariantFromExtended := LastSubId;
    RegisterParameter(H_Sub, typeOLEVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);

{$IFDEF UNIC}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromInt);
    Id_UnicStringFromInt := LastSubId; // js only
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromDouble);
    Id_UnicStringFromDouble := LastSubId; // js only
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromSingle);
    Id_UnicStringFromSingle := LastSubId; // js only
    RegisterParameter(H_Sub, typeSINGLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromExtended);
    Id_UnicStringFromExtended := LastSubId; // js only
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromBoolean);
    Id_UnicStringFromBoolean := LastSubId; // js only
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);
{$ELSE}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromInt);
    Id_AnsiStringFromInt := LastSubId; // js only
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromDouble);
    Id_AnsiStringFromDouble := LastSubId; // js only
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromSingle);
    Id_AnsiStringFromSingle := LastSubId; // js only
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSINGLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromExtended);
    Id_AnsiStringFromExtended := LastSubId; // js only
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromBoolean);
    Id_AnsiStringFromBoolean := LastSubId; // js only
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);
{$ENDIF} // not unic

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideCharFromVariant);
    Id_WideCharFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiCharFromVariant);
    Id_AnsiCharFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromVariant);
    Id_AnsiStringFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromVariant);
    Id_WideStringFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringFromVariant);
    Id_ShortStringFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, ByRef);
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_DoubleFromVariant);
    Id_DoubleFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_CurrencyFromVariant);
    Id_CurrencyFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCURRENCY, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SingleFromVariant);
    Id_SingleFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ExtendedFromVariant);
    Id_ExtendedFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_IntFromVariant);
    Id_IntFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_Int64FromVariant);
    Id_Int64FromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINT64, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ByteFromVariant);
    Id_ByteFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBYTE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WordFromVariant);
    Id_WordFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWORD, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_CardinalFromVariant);
    Id_CardinalFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SmallIntFromVariant);
    Id_SmallIntFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSMALLINT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortIntFromVariant);
    Id_ShortIntFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeSHORTINT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_BoolFromVariant);
    Id_BoolFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ByteBoolFromVariant);
    Id_ByteBoolFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBYTEBOOL, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WordBoolFromVariant);
    Id_WordBoolFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeWORDBOOL, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_LongBoolFromVariant);
    Id_LongBoolFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeLONGBOOL, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantAddition);
    Id_VariantAddition := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantSubtraction);
    Id_VariantSubtraction := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantMultiplication);
    Id_VariantMultiplication := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantDivision);
    Id_VariantDivision := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantIDivision);
    Id_VariantIDivision := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantModulo);
    Id_VariantModulo := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantLeftShift);
    Id_VariantLeftShift := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantRightShift);
    Id_VariantRightShift := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantAnd);
    Id_VariantAnd := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantOr);
    Id_VariantOr := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantXor);
    Id_VariantXor := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantLessThan);
    Id_VariantLessThan := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantLessThanOrEqual);
    Id_VariantLessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantGreaterThan);
    Id_VariantGreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantGreaterThanOrEqual);
    Id_VariantGreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantEquality);
    Id_VariantEquality := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantNotEquality);
    Id_VariantNotEquality := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantNegation);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    Id_VariantNegation := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantAbs);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    Id_VariantAbs := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _VariantNot);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    Id_VariantNot := LastSubId;

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

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _InterfaceFromClass);
    Id_InterfaceFromClass := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _InterfaceCast);
    Id_InterfaceCast := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _InterfaceAssign);
    Id_InterfaceAssign := LastSubId;
    RegisterParameter(H_Sub, typeINTERFACE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTERFACE, Unassigned, ByRef);

{$IFDEF PAX64}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _DoubleMultiplication);
    Id_DoubleMultiplication := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _DoubleDivision);
    Id_DoubleDivision := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _DoubleAddition);
    Id_DoubleAddition := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _DoubleSubtraction);
    Id_DoubleSubtraction := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _DoubleNegation);
    Id_DoubleNegation := LastSubId;
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef);
{$ENDIF}

//------------------ Dynamic array support routines ----------------------------
    RegisterRoutine(0, 'SetLength', typeVOID, ccSTDCALL, nil);

    RegisterRoutine(0, '', typeVOID, ccREGISTER, @_CondHalt);
    Id_CondHalt := LastSubId;

    H_Sub := RegisterRoutine(0, '_toParentClass', typeVOID, ccSTDCALL, @_ToParentClass);
    Id_ToParentClass := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    RegisterParameter(H_Sub, typeCLASS, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UpdateInstance);
    Id_UpdateInstance := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    RegisterParameter(H_Sub, typeCLASS, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_BeginExceptBlock);
    Id_BeginExceptBlock := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    // reserved
//  RegisterRoutine(0, '', typeVOID, ccSTDCALL, @Dummy);
{$IFNDEF PAXARM}
    RegisterRoutine(0, '', typeVOID, ccREGISTER, @_DestroyInherited);
    Id_DestroyInherited := LastSubId;
{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL,
                             @ _DynarraySetLength);
    Id_DynarraySetLength := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL,
                             @ _CreateEmptyDynarray);
    Id_CreateEmptyDynarray := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL,
                             @ _DynarrayClr);
    Id_DynarrayClr := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL,
                             @ _DynarrayAssign);
    Id_DynarrayAssign := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeINTEGER, ccSTDCALL,
                             @_DynarrayHigh);
    Id_DynarrayHigh := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef);

//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
    RegisterRoutine(0, strGetTickCount, typeINTEGER, ccREGISTER, @GetTickCount);
{$ELSE}
    RegisterRoutine(0, '', typeINTEGER, ccREGISTER, nil);
{$ENDIF}

    RegisterRoutine(0, '', typePOINTER, ccREGISTER, @GetClassByIndex);
    Id_GetClassByIndex := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_PrintEx);
    Id_PrintEx := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned); //runner
    RegisterParameter(H_Sub, typePOINTER, Unassigned); //address
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //kind
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //ft
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //L1
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //L2

// processing IS

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_Is);
    Id_Is := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ClassName);
    Id_ClassName := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_BeforeCallHost);
    Id_BeforeCallHost := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AfterCallHost);
    Id_AfterCallHost := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OnCreateObject);
    Id_OnCreateObject := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OnCreateHostObject);
    Id_OnCreateHostObject := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OnDestroyHostObject);
    Id_OnDestroyHostObject := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OnAfterObjectCreation);
    Id_OnAfterObjectCreation := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

// processing of published properties
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetAnsiStrProp);
    Id_GetAnsiStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetAnsiStrProp);
    Id_SetAnsiStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetWideStrProp);
    Id_GetWideStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetWideStrProp);
    Id_SetWideStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetOrdProp);
    Id_GetOrdProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetOrdProp);
    Id_SetOrdProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetInterfaceProp);
    Id_GetInterfaceProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetInterfaceProp);
    Id_SetInterfaceProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetSetProp);
    Id_GetSetProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetSetProp);
    Id_SetSetProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetFloatProp);
    Id_GetFloatProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetFloatProp);
    Id_SetFloatProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetVariantProp);
    Id_GetVariantProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetVariantProp);
    Id_SetVariantProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetInt64Prop);
    Id_GetInt64Prop := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _SetInt64Prop);
    Id_SetInt64Prop := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _GetEventProp);
    Id_GetEventProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_SetEventProp);
    Id_SetEventProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_SetEventProp2);
    Id_SetEventProp2 := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _CreateMethod);
    Id_CreateMethod := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

// processing try-except-finally

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_TryOn);
    Id_TryOn := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_TryOff);
    Id_TryOff := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_Raise);
    Id_Raise := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_Exit);
    Id_Exit := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_Finally);
    Id_Finally := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_CondRaise);
    Id_CondRaise := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_EndExceptBlock);
    Id_EndExceptBlock := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

// processing pause

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_Pause);
    Id_Pause := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    RegisterRoutine(0, 'pause', typeVOID, ccSTDCALL, nil);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, Address_InitSub);
    Id_InitSub := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, Address_EndSub);
    Id_EndSub := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);

// processing halt

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_Halt);
    Id_Halt := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    RegisterRoutine(0, 'halt', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(0, 'abort', typeVOID, ccSTDCALL, nil);

    RegisterRoutine(0, 'print', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(0, 'println', typeVOID, ccSTDCALL, nil);

// processing breakpoints

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_CheckPause);
    Id_CheckPause := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);

// processing integer overflow

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_IntOver);
    Id_IntOver := LastSubId;

// processing bound error

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_BoundError);
    Id_BoundError := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_CreateObject);
    Id_CreateObject := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _ErrAbstract);
    Id_ErrAbstract := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _RecordAssign);
    Id_RecordAssign := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    RegisterRoutine(0, '_GetProgAddress', typePOINTER, ccSTDCALL, @ _GetProgAddress);

    CURR_FMUL_ID := RegisterConstant(0, '_10000.0', typeSINGLE, 10000.0);

/////////////////////////////////////////////////////////////////////
/// PASCAL NAMESPASCE ///////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

    H_Namespace := RegisterNamespace(0, StrPascalNamespace);
    H_PascalNamespace := H_Namespace;

    H_Sub := RegisterRoutine(H_Namespace, 'GetMem', typeVOID, ccREGISTER,
                            @_GetMem);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef, 'P');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Size');

    H_Sub := RegisterRoutine(H_Namespace, 'FreeMem', typeVOID, ccREGISTER,
                             @_FreeMem);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, false, 'P');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Size');

    H_Sub := RegisterRoutine(H_Namespace, 'AllocMem', typePOINTER, ccREGISTER,
                             @AllocMem);
    RegisterParameter(H_Sub, typeCARDINAL, Unassigned, false, 'Size');

// PASCAL ARITHMETIC ROUTINES ///////////////////////////////////////

    H_Sub := RegisterRoutine(H_Namespace, 'Abs', typeVOID, ccSTDCALL, nil);
    RegisterParameter(H_Sub, typeVOID, Unassigned, ByRef, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'ArcTan', typeEXTENDED, ccREGISTER,
                             @_ArcTan);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Cos', typeEXTENDED, ccREGISTER, @_Cos);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Exp', typeEXTENDED, ccREGISTER, @_Exp);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Frac', typeEXTENDED, ccREGISTER, @_Frac);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Int', typeEXTENDED, ccREGISTER, @_Int);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Ln', typeEXTENDED, ccREGISTER, @_Ln);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    RegisterConstant(H_Namespace, 'Pi', typeEXTENDED, Pi);

    H_Sub := RegisterRoutine(H_Namespace, 'Sin', typeEXTENDED, ccREGISTER, @_Sin);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Sqr', typeEXTENDED, ccREGISTER, @_Sqr);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Sqrt', typeEXTENDED, ccREGISTER, @_Sqrt);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Trunc', typeINTEGER, ccREGISTER, @_Trunc);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Power', typeEXTENDED, ccREGISTER, @_Power);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'Base');
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'Exponent');

/////////////////////////////////////////////////////////////////////

    RegisterRoutine(H_Namespace, 'New', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Dispose', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Inc', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Dec', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Pred', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Succ', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Ord', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Chr', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'Low', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'High', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(0, 'Assigned', typeVOID, ccSTDCALL, nil);

    H_Sub := RegisterRoutine(H_Namespace, 'Odd', typeBOOLEAN, ccREGISTER, @_Odd);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'X');

// PASCAL AnsiString ROUTINES ///////////////////////////////////////

    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @_DynarrayLength);
    Id_DynArrayLength := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, false, 'X');
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @_LengthString);
    Id_AnsiStringLength := LastSubId;
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'S');

    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @ _LengthShortString);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, false, 'S');

    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @_LengthWideString);
    RegisterParameter(H_Sub, typeWIDESTRING, Unassigned, false, 'S');
{$ENDIF}
    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @_LengthUnicString);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'S');

    H_Sub := RegisterRoutine(0, 'Length', typeINTEGER, ccREGISTER,
                             @_VariantLength);
    Id_VariantArrayLength := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'S');
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(H_Namespace, 'Delete', typeVOID, ccREGISTER,
                             @_Delete);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');

    H_Sub := RegisterRoutine(H_Namespace, 'Insert', typeVOID, ccREGISTER,
                             @_Insert);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'Substr');
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, ByRef, 'Dest');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');
{$ENDIF}
    RegisterRoutine(H_Namespace, 'Str', typeVOID, ccSTDCALL, nil);

    H_Sub := RegisterRoutine(H_Namespace, 'Val', typeVOID, ccREGISTER, @_ValInt);
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'V');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'Code');

    H_Sub := RegisterRoutine(H_Namespace, 'Val', typeVOID, ccREGISTER,
                             @_ValDouble);
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'X');
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef, 'V');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'Code');
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(H_Namespace, 'Copy', typeANSISTRING, ccREGISTER,
                             @_Copy);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');

    H_Sub := RegisterRoutine(H_Namespace, 'Pos', typeINTEGER, ccREGISTER,
                            @_PosString);
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'Substr');
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'Str');

    H_Sub := RegisterRoutine(H_Namespace, 'Pos', typeINTEGER, ccREGISTER, @_PosChar);
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned, false, 'Ch');
    RegisterParameter(H_Sub, typeANSISTRING, Unassigned, false, 'Str');
{$ENDIF}
/////////////////////////////////////////////////////////////////////
// PASCAL MISCELLANEOUS ROUTINES ////////////////////////////////////

    H_Sub := RegisterRoutine(H_Namespace, 'SizeOf', typeVOID, ccSTDCALL, nil);
    RegisterParameter(H_Sub, typeVOID, Unassigned, ByRef, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Move', typeVOID, ccREGISTER, @Move);
    RegisterParameter(H_Sub, typePVOID, Unassigned, false, 'Source');
    RegisterParameter(H_Sub, typePVOID, Unassigned, ByRef, 'Dest');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(H_Namespace, 'FillChar', typeVOID, ccREGISTER,
                             @_FillChar);
    RegisterParameter(H_Sub, typePVOID, Unassigned, ByRef, 'X');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned, false, 'Value');
{$ENDIF}
    H_Sub := RegisterRoutine(H_Namespace, 'FillChar', typeVOID, ccREGISTER,
                             @_FillChar);
    RegisterParameter(H_Sub, typePVOID, Unassigned, ByRef, 'X');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');
    RegisterParameter(H_Sub, typeBYTE, Unassigned, false, 'Value');

{$IFNDEF PAXARM}
{$IFDEF FPC}
    H_Sub := RegisterRoutine(H_Namespace, 'Upcase', typeANSICHAR, ccREGISTER,
                             @_Upcase);
{$ELSE}
    H_Sub := RegisterRoutine(H_Namespace, 'Upcase', typeANSICHAR, ccREGISTER,
                             @ Upcase);
{$ENDIF}
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned, false, 'C');
{$ENDIF}

    RegisterRoutine(H_Namespace, 'Randomize', typeVOID, ccREGISTER, @Randomize);

    H_Sub := RegisterRoutine(H_Namespace, 'Random', typeINTEGER, ccREGISTER,
                             @_Random1);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'X');

    RegisterRoutine(H_Namespace, 'Random', typeDOUBLE, ccREGISTER, @_Random);

    H_Sub := RegisterRoutine(H_Namespace, 'Hi', typeBYTE, ccREGISTER, @_HiInt);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Hi', typeBYTE, ccREGISTER, @_HiWord);
    RegisterParameter(H_Sub, typeWORD, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Lo', typeBYTE, ccREGISTER, @_LoInt);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Lo', typeBYTE, ccREGISTER, @_LoWord);
    RegisterParameter(H_Sub, typeWORD, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Round', typeINT64, ccREGISTER, @_Round);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    {$IFDEF TRIAL}

    strShowTrial[0] := '_';
    strShowTrial[1] := '_';
    strShowTrial[2] := '_';
    strShowTrial[3] := '9';
    strShowTrial[4] := '^';
    strShowTrial[5] := '*';
    strShowTrial[6] := #0;

    RegisterRoutine(0, strShowTrial, typeVOID, ccREGISTER, @ ShowTrial);

   {$ENDIF}

    H_TPOINT := RegisterRecordType(0, 'TPoint', 1);
    RegisterTypeField(H_TPOINT, 'X', typeINTEGER);
    RegisterTypeField(H_TPOINT, 'Y', typeINTEGER);

    H := RegisterRecordType(0, 'TRect', 1);
    RegisterTypeField(H, 'Left', typeINTEGER);
    RegisterTypeField(H, 'Top', typeINTEGER);
    RegisterTypeField(H, 'Right', typeINTEGER);
    RegisterTypeField(H, 'Bottom', typeINTEGER);

// PASCAL CLASSES ROUTINES /////////////////////////////////////////////////////

    RegisterTypeAlias(H_Namespace, 'Real', typeDOUBLE);
    Id_TDateTime := RegisterTypeAlias(H_Namespace, 'TDateTime', typeDOUBLE);
    RegisterTypeAlias(H_Namespace, 'Longint', typeINTEGER);
    RegisterTypeAlias(H_Namespace, 'THandle', typeCARDINAL);
    RegisterTypeAlias(H_Namespace, 'LongWord', typeCARDINAL);
    RegisterTypeAlias(H_Namespace, 'HRESULT', typeINTEGER);
    RegisterTypeAlias(H_Namespace, 'HMODULE', typeINTEGER);

    H_PInteger := RegisterPointerType(H_Namespace, 'PInteger', typeINTEGER);
    H_PSmallInt := RegisterPointerType(H_Namespace, 'PSmallInt', typeSMALLINT);
    H_PShortInt := RegisterPointerType(H_Namespace, 'PShortInt', typeSHORTINT);
    H_PCardinal := RegisterPointerType(H_Namespace, 'PCardinal', typeCARDINAL);
    H_PWord := RegisterPointerType(H_Namespace, 'PWord', typeWORD);
    H_PByte := RegisterPointerType(H_Namespace, 'PByte', typeBYTE);
    H_PInt64 := RegisterPointerType(H_Namespace, 'PInt64', typeINT64);
    H_PSingle := RegisterPointerType(H_Namespace, 'PSingle', typeSINGLE);
    H_PDouble := RegisterPointerType(H_Namespace, 'PDouble', typeDOUBLE);
    H_PExtended := RegisterPointerType(H_Namespace, 'PExtended', typeEXTENDED);
    H_PCurrency := RegisterPointerType(H_Namespace, 'PCurrency', typeCURRENCY);
    H_PVariant := RegisterPointerType(H_Namespace, 'PVariant', typeVARIANT);
    H_PPointer := RegisterPointerType(H_Namespace, 'PPointer', typePOINTER);
    H_PBoolean := RegisterPointerType(H_Namespace, 'PBoolean', typeBOOLEAN);
    H_PWideChar := typePWIDECHAR;
{$IFNDEF PAXARM}
    H_PAnsiChar := typePANSICHAR;
    H_PShortString := RegisterPointerType(0, 'PShortString', typeSHORTSTRING);
    H_PAnsiString := RegisterPointerType(0, 'PAnsiString', typeANSISTRING);
    H_PWideString := RegisterPointerType(0, 'PWideString', typeWIDESTRING);
{$ENDIF}
    H_PUnicString := RegisterPointerType(0, 'PUnicString', typeUNICSTRING);

{$IFDEF UNIC}
    H_PString := H_PUnicString;
{$ELSE}
    H_PString := H_PAnsiString;
{$ENDIF}

    RegisterTypeAlias(H_Namespace, 'PLongint', H_PINTEGER);
    RegisterTypeAlias(H_Namespace, 'PLongWord', H_PCARDINAL);
    RegisterTypeAlias(H_Namespace, 'PDate', H_PDOUBLE);
{$IFNDEF PAXARM}
    RegisterTypeAlias(H_Namespace, 'PAnsiChar', typePANSICHAR);
{$ENDIF}

    H_PPInteger := RegisterPointerType(H_Namespace, 'PPInteger', H_PINTEGER);
    H_PPSmallInt := RegisterPointerType(H_Namespace, 'PPSmallInt', H_PSMALLINT);
    H_PPShortInt := RegisterPointerType(H_Namespace, 'PPShortInt', H_PSHORTINT);
    H_PPCardinal := RegisterPointerType(H_Namespace, 'PPCardinal', H_PCARDINAL);
    H_PPWord := RegisterPointerType(H_Namespace, 'PPWord', H_PWORD);
    H_PPByte := RegisterPointerType(H_Namespace, 'PPByte', H_PBYTE);
    H_PPInt64 := RegisterPointerType(H_Namespace, 'PPInt64', H_PINT64);
    H_PPSingle := RegisterPointerType(H_Namespace, 'PPSingle', H_PSINGLE);
    H_PPDouble := RegisterPointerType(H_Namespace, 'PPDouble', H_PDOUBLE);
    H_PPExtended := RegisterPointerType(H_Namespace, 'PPExtended', H_PEXTENDED);
    H_PPCurrency := RegisterPointerType(H_Namespace, 'PPCurrency', H_PCURRENCY);
    H_PPVariant := RegisterPointerType(H_Namespace, 'PPVariant', H_PVARIANT);
    H_PPPointer := RegisterPointerType(H_Namespace, 'PPPointer', H_PPOINTER);
    H_PPBoolean := RegisterPointerType(H_Namespace, 'PPBoolean', H_PBOOLEAN);
    H_PPWideChar := RegisterPointerType(H_Namespace, 'PPWideChar', H_PWIDECHAR);
{$IFNDEF PAXARM}
    H_PPAnsiChar := RegisterPointerType(H_Namespace, 'PPAnsiChar', H_PANSICHAR);
    H_PPShortString := RegisterPointerType(0, 'PPShortString', H_PSHORTSTRING);
    H_PPAnsiString := RegisterPointerType(0, 'PPAnsiString', H_PANSISTRING);
    H_PPWideString := RegisterPointerType(0, 'PPWideString', H_PWIDESTRING);
{$ENDIF}
    H_PPUnicString := RegisterPointerType(0, 'PPUnicString', H_PUNICSTRING);

    H_R0_7 := RegisterSubrangeType(0, '%0-7', typeINTEGER, 0, 7);
    H_TGUID_D4 := RegisterArrayType(0, '%TGUID_D4', H_R0_7, typeBYTE, 1);

    H_TGUID := RegisterRecordType(0, 'TGUID', 1);
    RegisterTypeField(H_TGUID, 'D1', typeCARDINAL);
    RegisterTypeField(H_TGUID, 'D2', typeWORD);
    RegisterTypeField(H_TGUID, 'D3', typeWORD);

    RegisterTypeField(H_TGUID, 'D4', H_TGUID_D4);

    H_PGUID := RegisterPointerType(0, 'PGUID', H_TGUID);

    H_IUnknown := RegisterInterfaceType(0, 'IUnknown', IUnknown);
    RegisterHeader(H_IUnknown, 'function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;', nil, 1);
    H_QueryInterface := LastSubId;
    RegisterHeader(H_IUnknown, 'function _AddRef: Integer; stdcall;', nil, 2);
    H_AddRef := LastSubId;
    RegisterHeader(H_IUnknown, 'function _Release: Integer; stdcall;', nil, 3);
    H_Release := LastSubId;

    RegisterTypeAlias(0, 'IInterface', H_IUNKNOWN);

    H_IDispatch := RegisterInterfaceType(0, 'IDispatch', IDispatch);
    RegisterHeader(H_IDispatch,
     'function GetTypeInfoCount(out Count: Integer): HResult; stdcall;', nil, 4);
    RegisterHeader(H_IDispatch,
     'function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;', nil, 5);
    RegisterHeader(H_IDispatch,
      'function GetIDsOfNames(const IID: TGUID; Names: Pointer;' +
      'NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;', nil, 6);
    RegisterHeader(H_IDispatch,
      'function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;' +
      'Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;', nil, 7);

// TObject

    H_TObject := RegisterClassType(0, TObject);
    H_TClass := RegisterClassReferenceType(0, 'TClass', H_TObject);

    RegisterConstructor(H_TObject, 'Create', @TObject.Create);

    RegisterDestructor(H_TObject, 'Destroy', @TMyObject.Destroy);
    Id_TObject_Destroy := LastSubId;

    RegisterMethod(H_TObject, 'Free', typeVOID, ccREGISTER, @TObject.Free);
    Id_TObject_Free := LastSubId;

{$IFDEF PAXARM}
    RegisterMethod(H_TObject, 'ClassName', typeSTRING, ccSTDCALL, @TObject_ClassName);
    Id_TObject_ClassName := LastSubId;
{$ELSE}
    RegisterMethod(H_TObject, 'ClassName', typeSHORTSTRING, ccSTDCALL,
                   @TObject_ClassName);
    Id_TObject_ClassName := LastSubId;
{$ENDIF}

    RegisterMethod(H_TObject, 'ClassType', H_TClass, ccREGISTER,
                   @TObject.ClassType);
    RegisterMethod(H_TObject, 'ClassParent', H_TClass, ccREGISTER,
                   @TObject.ClassParent, true);
    RegisterMethod(H_TObject, 'InstanceSize', typeINTEGER, ccREGISTER,
                   @TObject.InstanceSize, true);

    RegisterHeader(H_TObject, 'function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; virtual;',
      @TObject.SafeCallException);
    RegisterHeader(H_TObject, 'procedure AfterConstruction; virtual;',
      @TObject.AfterConstruction);
    RegisterHeader(H_TObject, 'procedure BeforeDestruction; virtual;',
      @TObject.BeforeDestruction);
    RegisterHeader(H_TObject, 'procedure Dispatch(var Message); virtual;',
      @TObject.Dispatch);
    RegisterHeader(H_TObject, 'procedure DefaultHandler(var Message); virtual;',
      @TObject.DefaultHandler);
    RegisterHeader(H_TObject, 'class function NewInstance: TObject; virtual;',
      @TObject.NewInstance);
    RegisterHeader(H_TObject, 'procedure FreeInstance; virtual;',
      @TObject.FreeInstance);

   {$ifdef UNIC}
    RegisterHeader(H_TObject, 'function ToString: String; virtual;',
      @TObject.ToString);
    RegisterHeader(H_TObject, 'function Equals(Obj: TObject): Boolean; virtual;',
      @TObject.Equals);
    RegisterHeader(H_TObject, 'function GetHashCode: Integer; virtual;',
      @TObject.GetHashCode);
   {$endif}

{$IFDEF PAXARM}
    H_Sub := RegisterMethod(H_TObject, 'FieldAddress', typePOINTER, ccREGISTER,
                            @TObject.FieldAddress);
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'Name');

    H_Sub := RegisterMethod(H_TObject, 'InheritsFrom', typeBOOLEAN, ccREGISTER,
                            @TObject.InheritsFrom, true);
    RegisterParameter(H_Sub, H_TClass, Unassigned, false, 'AClass');

    H_Sub := RegisterMethod(H_TObject, 'MethodAddress', typePOINTER, ccREGISTER,
                            @TObject.MethodAddress, true);
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'Name');

    H_Sub := RegisterMethod(H_TObject, 'MethodName', typeSTRING, ccREGISTER,
                            @TObject.MethodName, true);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, false, 'Address');
{$ELSE}
    H_Sub := RegisterMethod(H_TObject, 'FieldAddress', typePOINTER, ccREGISTER,
                            @TObject.FieldAddress);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, false, 'Name');

    H_Sub := RegisterMethod(H_TObject, 'InheritsFrom', typeBOOLEAN, ccREGISTER,
                            @TObject.InheritsFrom, true);
    RegisterParameter(H_Sub, H_TClass, Unassigned, false, 'AClass');

    H_Sub := RegisterMethod(H_TObject, 'MethodAddress', typePOINTER, ccREGISTER,
                            @TObject.MethodAddress, true);
    RegisterParameter(H_Sub, typeSHORTSTRING, Unassigned, false, 'Name');

    H_Sub := RegisterMethod(H_TObject, 'MethodName', typeSHORTSTRING, ccREGISTER,
                            @TObject.MethodName, true);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, false, 'Address');
{$ENDIF}
    RegisterHeader(H_TObject, 'function GetInterface(const IID: TGUID; out Obj): Boolean;',
             @ TObject_GetInterface);
    Id_TObject_GetInterface := LastSubId;

// TInterfacedObject
    H_TInterfacedObject := RegisterClassType(0, TInterfacedObject);
    RegisterSupportedInterface(H_TInterfacedObject, 'IUnknown', IUnknown);
    RegisterConstructor(H_TInterfacedObject, 'Create', @TInterfacedObject.Create);
    RegisterHeader(H_TInterfacedObject, 'class function NewInstance: TObject; override;',
       @TInterfacedObject.NewInstance);

    RegisterHeader(H_TInterfacedObject, 'function __GetRefCount: Integer;', @GetRefCount);
    RegisterHeader(H_TInterfacedObject, 'property RefCount: Integer read __GetRefCount;', nil);

    RegisterHeader(H_TInterfacedObject, 'function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;',
       @TMyInterfacedObject.QueryInterface);
    RegisterHeader(H_TInterfacedObject, 'function _AddRef: Integer; stdcall;',
       @TMyInterfacedObject_AddRef);
    RegisterHeader(H_TInterfacedObject, 'function _Release: Integer; stdcall;',
       @TMyInterfacedObject_Release);

// TInterfacedClass
    RegisterClassReferenceType(0, 'TInterfacedClass', H_TInterfacedObject);

    RegisterConstant(0, 'vtInteger', typeINTEGER, vtInteger);
    RegisterConstant(0, 'vtBoolean', typeINTEGER, vtBoolean);
    RegisterConstant(0, 'vtChar', typeINTEGER, vtChar);
    RegisterConstant(0, 'vtExtended', typeINTEGER, vtExtended);
    RegisterConstant(0, 'vtString', typeINTEGER, vtString);
    RegisterConstant(0, 'vtPointer', typeINTEGER, vtPointer);
    RegisterConstant(0, 'vtPChar', typeINTEGER, vtPChar);
    RegisterConstant(0, 'vtObject', typeINTEGER, vtObject);
    RegisterConstant(0, 'vtClass', typeINTEGER, vtClass);
    RegisterConstant(0, 'vtWideChar', typeINTEGER, vtWideChar);
    RegisterConstant(0, 'vtPWideChar', typeINTEGER, vtPWideChar);
    RegisterConstant(0, 'vtAnsiString', typeINTEGER, vtAnsiString);
    RegisterConstant(0, 'vtCurrency', typeINTEGER, vtCurrency);
    RegisterConstant(0, 'vtVariant', typeINTEGER, vtVariant);
    RegisterConstant(0, 'vtInterface', typeINTEGER, vtInterface);
    RegisterConstant(0, 'vtWideString', typeINTEGER, vtWideString);
    RegisterConstant(0, 'vtInt64', typeINTEGER, vtInt64);

    H_TVarRec := RegisterRecordType(0, 'TVarRec', 1);
    RegisterTypeField(H_TVarRec, 'VInteger', typeINTEGER, 0);
    RegisterTypeField(H_TVarRec, 'VBoolean', typeBOOLEAN, 0);
{$IFNDEF PAXARM}
    RegisterTypeField(H_TVarRec, 'VChar', typeANSICHAR, 0);
{$ENDIF}
    RegisterTypeField(H_TVarRec, 'VExtended', H_PExtended, 0);
    RegisterTypeField(H_TVarRec, 'VString', H_PShortString, 0);
    RegisterTypeField(H_TVarRec, 'VPointer', typePOINTER, 0);
{$IFNDEF PAXARM}
    RegisterTypeField(H_TVarRec, 'VPChar', typePANSICHAR, 0);
{$ENDIF}
    RegisterTypeField(H_TVarRec, 'VObject', H_TObject, 0);
    RegisterTypeField(H_TVarRec, 'VClass', H_TClass, 0);
    RegisterTypeField(H_TVarRec, 'VWideChar', typeWIDECHAR, 0);
    RegisterTypeField(H_TVarRec, 'VAnsiString', H_PSTRING, 0);
    RegisterTypeField(H_TVarRec, 'VCurrency', typePOINTER, 0);
    RegisterTypeField(H_TVarRec, 'VVariant', H_PVARIANT, 0);
    RegisterTypeField(H_TVarRec, 'VInterface', typePOINTER, 0);
    RegisterTypeField(H_TVarRec, 'VWideString', typePOINTER, 0);
    RegisterTypeField(H_TVarRec, 'VInt64', typePOINTER, 0);

    RegisterTypeField(H_TVarRec, 'VType', typeINTEGER, 4);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _AssignTVarRec);
    Id_AssignTVarRec := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Dynarray_TVarRec := RegisterDynamicArrayType(0, 'DYNARRAY_TVarRec', H_TVarRec);

    H_Dynarray_Integer := RegisterDynamicArrayType(0, 'DYNARRAY_Integer', typeINTEGER);
    H_Dynarray_Byte := RegisterDynamicArrayType(0, 'DYNARRAY_Byte', typeBYTE);
    H_Dynarray_Word := RegisterDynamicArrayType(0, 'DYNARRAY_Word', typeWORD);
    H_Dynarray_ShortInt := RegisterDynamicArrayType(0, 'DYNARRAY_ShortInt', typeSHORTINT);
    H_Dynarray_SmallInt := RegisterDynamicArrayType(0, 'DYNARRAY_SmallInt', typeSMALLINT);
    H_Dynarray_Cardinal := RegisterDynamicArrayType(0, 'DYNARRAY_Cardinal', typeCARDINAL);
    H_Dynarray_Int64 := RegisterDynamicArrayType(0, 'DYNARRAY_Int64', typeINT64);
    H_Dynarray_UInt64 := RegisterDynamicArrayType(0, 'DYNARRAY_UInt64', typeUINT64);
{$IFNDEF PAXARM}
    H_Dynarray_AnsiChar := RegisterDynamicArrayType(0, 'DYNARRAY_AnsiChar', typeANSICHAR);
    H_Dynarray_WideChar := RegisterDynamicArrayType(0, 'DYNARRAY_WideChar', typeWIDECHAR);
    H_Dynarray_AnsiString := RegisterDynamicArrayType(0, 'DYNARRAY_AnsiString', typeANSISTRING);
    H_Dynarray_WideString := RegisterDynamicArrayType(0, 'DYNARRAY_WideString', typeWIDESTRING);
    H_Dynarray_ShortString := RegisterDynamicArrayType(0, 'DYNARRAY_ShortString', typeSHORTSTRING);
{$ENDIF}
    H_Dynarray_UnicodeString := RegisterDynamicArrayType(0, 'DYNARRAY_UnicodeString', typeUNICSTRING);
    H_Dynarray_Double := RegisterDynamicArrayType(0, 'DYNARRAY_Double', typeDOUBLE);
    H_Dynarray_Single := RegisterDynamicArrayType(0, 'DYNARRAY_Single', typeSINGLE);
    H_Dynarray_Extended := RegisterDynamicArrayType(0, 'DYNARRAY_Extended', typeEXTENDED);
    H_Dynarray_Currency := RegisterDynamicArrayType(0, 'DYNARRAY_Currency', typeCURRENCY);
    H_Dynarray_Boolean := RegisterDynamicArrayType(0, 'DYNARRAY_Boolean', typeBOOLEAN);
    H_Dynarray_ByteBool := RegisterDynamicArrayType(0, 'DYNARRAY_ByteBool', typeBYTEBOOL);
    H_Dynarray_WordBool := RegisterDynamicArrayType(0, 'DYNARRAY_WordBool', typeWORDBOOL);
    H_Dynarray_LongBool := RegisterDynamicArrayType(0, 'DYNARRAY_LongBool', typeLONGBOOL);
    H_Dynarray_Variant := RegisterDynamicArrayType(0, 'DYNARRAY_Variant', typeVARIANT);
    H_Dynarray_OleVariant := RegisterDynamicArrayType(0, 'DYNARRAY_OleVariant', typeOLEVARIANT);
    H_Dynarray_Pointer := RegisterDynamicArrayType(0, 'DYNARRAY_Pointer', typePOINTER);

    RegisterConstant(0, 'Null', typeVARIANT, Null);
    H_Unassigned := RegisterConstant(0, strUnassigned, typeVARIANT, Unassigned);

    RegisterConstant(0, 'MaxInt', typeINTEGER, MaxInt);
    RegisterConstant(0, 'MaxLongint', typeINTEGER, MaxInt);

    H_TMethod := RegisterRecordType(0, 'TMethod', 1);
    RegisterTypeField(H_TMethod, 'Code', typePOINTER, 0);
    RegisterTypeField(H_TMethod, 'Data', typePOINTER, 4);

//    RegisterClassType(0, TPersistent);
//    RegisterClassType(0, TComponent);


    RegisterTypeAlias(H_Namespace, 'TVarType', typeWORD);
{$IFNDEF PAXARM}
    RegisterPointerType(H_Namespace, 'PWideString', typeWIDESTRING);
    RegisterTypeAlias(0, 'DWORD', typeCARDINAL);
    RegisterTypeAlias(0, 'AnsiString', typeANSISTRING);
    RegisterTypeAlias(0, 'AnsiChar', typeANSICHAR);
{$ENDIF}
    RegisterConstant (H_Namespace, 'MaxLongInt', typeINTEGER, MaxLongInt);
    RegisterTypeAlias (H_Namespace, 'IInvokable', H_IUNKNOWN);
    H_Sub := RegisterRecordType (H_Namespace, 'TDispatchMessage', 8);
    RegisterTypeField(H_Sub, 'MsgID', typeWORD, 0);
{$IFNDEF PAXARM}
    RegisterPointerType (H_Namespace, 'PAnsiString', typeANSISTRING);
    H := RegisterTypeAlias (H_Namespace, 'UCS2Char', typeWIDECHAR);
    RegisterPointerType (H_Namespace, 'PUCS2Char', H);
    H := RegisterTypeAlias (H_Namespace, 'UCS4Char', typeINTEGER); // typeLONGWORD
    RegisterPointerType (H_Namespace, 'PUCS4Char', H);
    H_Sub := RegisterArrayType (H_Namespace, 'TUCS4CharArray',
      RegisterSubrangeType (0, '%TUCS4CharArray', typeINTEGER, 0, $effffff),
      H,
      8
    );
    RegisterPointerType (H_Namespace, 'PUCS4CharArray', H_Sub);
    RegisterDynamicArrayType (H_Namespace, 'UCS4String', H);
    H_Sub := RegisterTypeAlias (H_Namespace, 'UTF8String', typeANSISTRING);
    RegisterPointerType (H_Namespace, 'PUTF8String', H_Sub);
    H_Sub := RegisterArrayType (H_Namespace, 'IntegerArray',
      RegisterSubrangeType (0, '%IntegerArray', typeINTEGER, 0, $effffff),
      typeINTEGER,
      8
    );
    RegisterPointerType (H_Namespace, 'PIntegerArray', H_Sub);
    H_Sub := RegisterArrayType (H_Namespace, 'PointerArray',
      RegisterSubRangeType (0, '%PointerArray', typeINTEGER, 0, 512*1024*1024 - 2),
      typePOINTER,
      8
    );
    RegisterPointerType (H_Namespace, 'PPointerArray', H_Sub);
    RegisterDynamicArrayType (H_Namespace, 'TBoundArray', typeINTEGER);
    H_Sub := RegisterArrayType (H_Namespace, 'TPCharArray',
      RegisterSubRangeType (0, '%TPCharArray', typeINTEGER, 0, (MaxLongint div SizeOf(PChar))-1),
      typePANSICHAR,
      1
    );
    RegisterPointerType (H_Namespace, 'PPCharArray', H_Sub);
{$ENDIF}
    RegisterPointerType (H_Namespace, 'PSmallInt', typeSMALLINT);
    RegisterPointerType (H_Namespace, 'PShortInt', typeSHORTINT);
    H := RegisterPointerType (H_Namespace, 'PDispatch', H_IDispatch);
    RegisterPointerType (H_Namespace, 'PPDispatch', H);
    RegisterPointerType (H_Namespace, 'PError', typeINTEGER); //typeLONGWORD
    RegisterPointerType (H_Namespace, 'PWordBool', typeWORDBOOL);
    H := RegisterPointerType (H_Namespace, 'PUnknown', H_IUnknown);
    RegisterPointerType (H_Namespace, 'PPUnknown', H);
    RegisterPointerType (H_Namespace, 'POleVariant', typeOLEVARIANT);
    RegisterPointerType (H_Namespace, 'PDateTime', typeDOUBLE);
    H_Sub := RegisterRecordType (H_Namespace, 'TVarArrayBound', 1);
    RegisterTypeField (H_Sub, 'ElementCount', typeINTEGER);
    RegisterTypeField (H_Sub, 'LowBound', typeINTEGER);
    H_Sub := RegisterArrayType (H_Namespace, 'TVarArrayBoundArray',
      RegisterSubRangeType (H_Namespace, '%TVarArrayBoundArray', typeBYTE, H_Namespace, H_Namespace),
      H_Sub,
      8
    );
    RegisterPointerType (H_Namespace, 'PVarArrayBoundArray', H_Sub);
    H := RegisterArrayType (H_Namespace, 'TVarArrayCoorArray',
      RegisterSubRangeType (H_Namespace, '%TVarArrayCoorArray', typeBYTE, H_Namespace, H_Namespace),
      typeINTEGER,
      8
    );
    RegisterPointerType (H_Namespace, 'PVarArrayCoorArray', H);

    H := RegisterRecordType (H_Namespace, 'TVarArray', 1);
    RegisterTypeField (H, 'DimCount', typeWORD);
    RegisterTypeField (H, 'Flags', typeWORD);
    RegisterTypeField (H, 'ElementSize', typeINTEGER);
    RegisterTypeField (H, 'LockCount', typeINTEGER);
    RegisterTypeField (H, 'Data', typePOINTER);
    RegisterTypeField (H, 'Bounds', H_Sub);
    RegisterPointerType (0, 'PVarArray', H);
    H_Sub := RegisterRecordType(0, 'TVarData', 1);
    RegisterVariantRecordTypeField(H_Sub, 'VType: TVarType',           01);
    RegisterVariantRecordTypeField(H_Sub, 'Reserved1: Word',         0101);
    RegisterVariantRecordTypeField(H_Sub, 'Reserved2: Word',       010101);
    RegisterVariantRecordTypeField(H_Sub, 'Reserved3: Word',       010101);
    RegisterVariantRecordTypeField(H_Sub, 'VSmallInt: SmallInt', 01010101);
    RegisterVariantRecordTypeField(H_Sub, 'VInteger: Integer',   02010101);
    RegisterVariantRecordTypeField(H_Sub, 'VSingle: Single',     03010101);
    RegisterVariantRecordTypeField(H_Sub, 'VDouble: Double',     04010101);
    RegisterVariantRecordTypeField(H_Sub, 'VCurrency: Currency', 05010101);
    RegisterVariantRecordTypeField(H_Sub, 'VDate: TDateTime',    06010101);
    RegisterVariantRecordTypeField(H_Sub, 'VOleStr: PWideChar',  07010101);
    RegisterVariantRecordTypeField(H_Sub, 'VDispatch: Pointer',  08010101);
    RegisterVariantRecordTypeField(H_Sub, 'VError: HRESULT',     09010101);
    RegisterVariantRecordTypeField(H_Sub, 'VBoolean: WordBool',  10010101);
    RegisterVariantRecordTypeField(H_Sub, 'VUnknown: Pointer',   11010101);
    RegisterVariantRecordTypeField(H_Sub, 'VShortInt: ShortInt', 12010101);
    RegisterVariantRecordTypeField(H_Sub, 'VByte: Byte',         13010101);
    RegisterVariantRecordTypeField(H_Sub, 'VWord: Word',         14010101);
    RegisterVariantRecordTypeField(H_Sub, 'VLongWord: LongWord', 15010101);
    RegisterVariantRecordTypeField(H_Sub, 'VInt64: Int64',       16010101);
    RegisterVariantRecordTypeField(H_Sub, 'VString: Pointer',    17010101);
    RegisterVariantRecordTypeField(H_Sub, 'VAny: Pointer',       18010101);
    RegisterVariantRecordTypeField(H_Sub, 'VArray: Pointer',     19010101);
    RegisterVariantRecordTypeField(H_Sub, 'VPointer: Pointer',   20010101);
    RegisterPointerType (H_Namespace, 'PVarData', H_Sub);
    RegisterTypeAlias (H_Namespace, 'TVarOp', typeINTEGER);
    H_Sub := RegisterRecordType (H_Namespace, 'TCallDesc', 1);
    RegisterTypeField (H_Sub, 'CallType', typeBYTE);
    RegisterTypeField (H_Sub, 'ArgCount', typeBYTE);
    RegisterTypeField (H_Sub, 'NamedArgCount', typeBYTE);
    H := RegisterDynamicArrayType (0, '%ArgTypes',
      RegisterSubRangeType (0, '%%ArgTypes', typeINTEGER, 0, 255)
    );
    RegisterTypeField (H_Sub, 'ArgTypes', H);
    RegisterPointerType (H_Namespace, 'PCallDesc', H_Sub);

    H := RegisterRecordType (H_Namespace, 'TDispDesc', 1);
    RegisterTypeField (H, 'DispID', typeINTEGER);
    RegisterTypeField (H, 'ResType', typeBYTE);
    RegisterTypeField (H, 'CallDesc', H_Sub);
    RegisterPointerType (H_Namespace, 'PDispDesc', H);

    {
    TDynArrayTypeInfo
    PDynArrayTypeInfo
    }
    RegisterPointerType (H_Namespace, 'PVarRec', H_TVarRec);
    H := RegisterEnumType (H_Namespace, 'TTextLineBreakStyle', typeINTEGER);
    RegisterEnumValue (H, 'tlbsLF', H_Namespace);
    RegisterEnumValue (H, 'tlbsCRLF', 1);
    H := RegisterTypeAlias (H_Namespace, 'HRSRC', typeCARDINAL); // THandle
    RegisterTypeAlias (H_Namespace, 'TResourceHandle', H);
    H := RegisterTypeAlias (H_Namespace, 'HINST', typeCARDINAL); // THandle
    RegisterTypeAlias (H_Namespace, 'HGLOBAL', H);
    H := RegisterPointerType(H_Namespace, 'PCardinal', typeCARDINAL); // redefined
    H_Sub := RegisterRecordType (H_Namespace, 'TResStringRec', 1);
    RegisterTypeField (H_Sub, 'Module', H);
    RegisterTypeField (H_Sub, 'Identifier', typeINTEGER);
    RegisterPointerType (H_Namespace, 'PResStringRec', H_Sub);

    H := RegisterRoutine (H_Namespace, '%TThreadFunc', typeINTEGER, ccREGISTER, Nil);
    RegisterParameter (H, typePOINTER, Unassigned);
    H_Sub := RegisterProceduralType (H_Namespace, 'TThreadFunc', H);
    H := RegisterRoutine (H_Namespace, 'BeginThread', typeINTEGER, ccREGISTER, @BeginThread);
    RegisterParameter (H, typePointer, Unassigned); // SecurityAttributes: Pointer;
    RegisterParameter (H, typeCARDINAL, Unassigned); // StackSize: LongWord;
    RegisterParameter (H, H_Sub, Unassigned); //ThreadFunc: TThreadFunc;
    RegisterParameter (H, typePOINTER, Unassigned); //Parameter: Pointer;
    RegisterParameter (H, typeCARDINAL, Unassigned); //CreationFlags: LongWord;
    RegisterParameter (H, typeCARDINAL, Unassigned, ByRef); // var ThreadId: LongWord
    H := RegisterRoutine (H_Namespace, 'EndThread', typeVOID, ccREGISTER, @EndThread);
    RegisterParameter (H, typeINTEGER, Unassigned); // EndThread(ExitCode: Integer);

    {
    H_Sub := RegisterClass (H_Namespace, TAggregatedObject);
    H_Sub := RegisterClass (H_Namespace, TContainedObject);
    }

    RegisterConstant(H_Namespace, 'varEmpty', varEmpty);
    RegisterConstant(H_Namespace, 'varNull', varNull);
    RegisterConstant(H_Namespace, 'varSmallint', varSmallint);
    RegisterConstant(H_Namespace, 'varInteger', varInteger);
    RegisterConstant(H_Namespace, 'varSingle', varSingle);
    RegisterConstant(H_Namespace, 'varDouble', varDouble);
    RegisterConstant(H_Namespace, 'varCurrency', varCurrency);
    RegisterConstant(H_Namespace, 'varDate', varDate);
    RegisterConstant(H_Namespace, 'varOleStr', varOleStr);
    RegisterConstant(H_Namespace, 'varDispatch', varDispatch);
    RegisterConstant(H_Namespace, 'varError', varError);
    RegisterConstant(H_Namespace, 'varBoolean', varBoolean);
    RegisterConstant(H_Namespace, 'varVariant', varVariant);
    RegisterConstant(H_Namespace, 'varUnknown', varUnknown);
{$IFDEF VARIANTS}
    RegisterConstant(H_Namespace, 'varShortInt', varShortInt);
{$ENDIF}
    RegisterConstant(H_Namespace, 'varByte', varByte);
{$IFDEF VARIANTS}
    RegisterConstant(H_Namespace, 'varWord', varWord);
    RegisterConstant(H_Namespace, 'varLongWord', varLongWord);
    RegisterConstant(H_Namespace, 'varInt64', varInt64);
    RegisterConstant(H_Namespace, 'varUInt64', $0015);
{$ENDIF}
    RegisterConstant(H_Namespace, 'varStrArg', varStrArg);
    RegisterConstant(H_Namespace, 'varString', varString);
    RegisterConstant(H_Namespace, 'varAny', varAny);
    RegisterConstant(H_Namespace, 'varTypeMask', varTypeMask);
    RegisterConstant(H_Namespace, 'varArray', varArray);
    RegisterConstant(H_Namespace, 'varByRef', varByRef);
{$IFDEF UNIC}
    RegisterConstant(H_Namespace, 'varUString', varUString);
{$ENDIF}

    H := RegisterRecordType (H_Namespace, 'TInterfaceEntry', 1);
    RegisterTypeField(H, 'IID', H_TGUID);
    RegisterTypeField(H, 'VTable', typePOINTER);
    RegisterTypeField(H, 'IOffset', typeINTEGER);
    RegisterTypeField(H, 'ImplGetter', typeINTEGER);
    RegisterPointerType(H_Namespace, 'PInterfaceEntry', H);

    H_TEntries := RegisterArrayType(0, '',
      RegisterSubrangeType(0, '', typeINTEGER, 0, 9999), H, 1);

    H := RegisterRecordType (H_Namespace, 'TInterfaceTable', 1);
    RegisterTypeField(H, 'EntryCount', typeINTEGER);
    RegisterTypeField(H, 'Entries', H_TEntries);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccREGISTER, @_WriteUnicString);
    Id_WriteUnicString := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    H_WriteUnicString := H_Sub;

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromPAnsiChar);
    Id_UnicStringFromPAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromAnsiChar);
    Id_UnicStringFromAnsiChar := LastSubId;
    RegisterParameter(H_Sub, typeANSICHAR, Unassigned);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringFromUnicString);
    Id_ShortStringFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromShortString);
    Id_UnicStringFromShortString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AnsiStringFromUnicString);
    Id_AnsiStringFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromAnsiString);
    Id_UnicStringFromAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_WideStringFromUnicString);
    Id_WideStringFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromWideString);
    Id_UnicStringFromWideString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromPWideChar);
    Id_UnicStringFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UnicStringFromWideChar);
    Id_UnicStringFromWideChar := LastSubId;
    RegisterParameter(H_Sub, typeWIDECHAR, Unassigned);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);


    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromUnicString);
    Id_VariantFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringFromVariant);
    Id_UnicStringFromVariant := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromUnicString);
    Id_OleVariantFromUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringAddition);
    Id_UnicStringAddition := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringAssign);
    Id_UnicStringAssign := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringEquality);
    Id_UnicStringEquality := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringNotEquality);
    Id_UnicStringNotEquality := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _UnicStringGreaterThan);
    Id_UnicStringGreaterThan := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringGreaterThanOrEqual);
    Id_UnicStringGreaterThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringLessThan);
    Id_UnicStringLessThan := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringLessThanOrEqual);
    Id_UnicStringLessThanOrEqual := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeBOOLEAN, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnicStringClr);
    Id_UnicStringClr := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_StringAddRef);
    Id_UnicStringAddRef := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetUnicStringLength);
    Id_SetUnicStringLength := LastSubId;
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VariantFromPWideChar);
    Id_VariantFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_OleVariantFromPWideChar);
    Id_OleVariantFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetUnicStrProp);
    Id_GetUnicStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetUnicStrProp);
    Id_SetUnicStrProp := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _AnsiStringFromPWideChar);
    Id_AnsiStringFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ShortStringFromPWideChar);
    Id_ShortStringFromPWideChar := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}

{$IFDEF UNIC}
    // Length, Pos, Copy, Insert, Delete

    H_Sub := RegisterRoutine(H_Namespace, 'Length', typeINTEGER, ccREGISTER,
                             @_UnicLength);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'S');

    H_Sub := RegisterRoutine(H_Namespace, 'Delete', typeVOID, ccREGISTER,
                             @_UnicDelete);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');

    H_Sub := RegisterRoutine(H_Namespace, 'Insert', typeVOID, ccREGISTER,
                             @_UnicInsert);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'Substr');
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, ByRef, 'Dest');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');

    H_Sub := RegisterRoutine(H_Namespace, 'Val', typeVOID, ccREGISTER,
                             @_UnicValInt);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'V');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'Code');

    H_Sub := RegisterRoutine(H_Namespace, 'Val', typeVOID, ccREGISTER,
                             @_UnicValDouble);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'X');
    RegisterParameter(H_Sub, typeDOUBLE, Unassigned, ByRef, 'V');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, ByRef, 'Code');

    H_Sub := RegisterRoutine(H_Namespace, 'Copy', typeUNICSTRING, ccREGISTER,
                             @_UnicCopy);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'S');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Index');
    RegisterParameter(H_Sub, typeINTEGER, Unassigned, false, 'Count');

    H_Sub := RegisterRoutine(H_Namespace, 'Pos', typeINTEGER, ccREGISTER,
                            @_UnicPos);
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'Substr');
    RegisterParameter(H_Sub, typeUNICSTRING, Unassigned, false, 'Str');

{$ENDIF} //unic

    ID_Prog := RegisterClassType(0, TBaseRunner);
    RegisterHeader(ID_Prog, 'procedure __Set_ExitCode(value: Integer); stdcall;',
      @Set_ExitCode);
    RegisterHeader(ID_Prog, 'function __Get_ExitCode: Integer; stdcall;',
      @Get_ExitCode);
    RegisterHeader(ID_Prog, 'property ExitCode: Integer read __Get_ExitCode write __Set_ExitCode;', nil);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _StructEquality);
    Id_StructEquality := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _StructNotEquality);
    Id_StructNotEquality := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    Register_TYPEINFO(H_Namespace, st);

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, Address_LoadSeg);
    Id_LoadSeg := LastSubId;

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, @ _LoadClassRef);
    Id_LoadClassRef := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetVariantLength2);
    Id_SetVariantLength2 := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetVariantLength3);
    Id_SetVariantLength3 := LastSubId;
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_LockVArray);
    Id_LockVArray := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UnlockVArray);
    Id_UnlockVArray := LastSubId;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_DynarraySetLength2);
    Id_DynarraySetLength2 := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_DynarraySetLength3);
    Id_DynarraySetLength3 := LastSubId;
    RegisterParameter(H_Sub, typeDYNARRAY, Unassigned, ByRef);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);

    H := RegisterRecordType(0, 'TMsg', 1);
    RegisterTypeField(H, 'hwnd', typeCARDINAL);
    RegisterTypeField(H, 'message', typeCARDINAL);
    RegisterTypeField(H, 'wParam', typeINTEGER);
    RegisterTypeField(H, 'lParam', typeINTEGER);
    RegisterTypeField(H, 'time', typeCARDINAL);
    RegisterTypeField(H, 'pt', H_TPOINT);

    RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDynamicMethodAddress);
    Id_GetDynamicMethodAddress := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_AddMessage);
    Id_AddMessage := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    if Assigned(RegisterSEH)  then
      RegisterSEH(st);

    Register_StdJavaScript(st);
    Register_StdBasic(st);
    Register_Framework(st);

    if Assigned(Import_TValue) then
      Import_TValue(0, st);
{$IFNDEF PAXARM}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UniqueAnsiString);
    Id_UniqueAnsiString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
{$ENDIF}
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_UniqueUnicString);
    Id_UniqueUnicString := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetComponent);
    Id_GetComponent := LastSubId;
    RegisterParameter(H_Sub, typeCLASS, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeCLASS, Unassigned, ByRef);

    H_DYN_VAR := RegisterDynamicArrayType(0, '#DYN_VAR', typeVARIANT);

    H_SUB := RegisterRoutine(0, '#CallVirt', typeVOID, ccSTDCALL, @_CallVirt);
    Records[LastSubId].PushProgRequired := true;
    Id_CallVirt := LastSubId;

    RegisterParameter(H_Sub, typePCHAR, Unassigned, false, 'Obj');
    RegisterParameter(H_Sub, typePCHAR, Unassigned, false, 'Prop');
    RegisterParameter(H_Sub, H_DYN_VAR, Unassigned, false, 'A');
    Records[Card].IsOpenArray := true;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, ByRef, 'Res');

    H_SUB := RegisterRoutine(0, '#PutVirt', typeVOID, ccSTDCALL, @_PutVirt);
    Records[LastSubId].PushProgRequired := true;
    Id_PutVirt := LastSubId;

    RegisterParameter(H_Sub, typePCHAR, Unassigned, false, 'Obj');
    RegisterParameter(H_Sub, typePCHAR, Unassigned, false, 'Prop');
    RegisterParameter(H_Sub, H_DYN_VAR, Unassigned, false, 'A');
    Records[Card].IsOpenArray := true;
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'value');
    Records[Card].IsConst := true;

    H_SUB := RegisterRoutine(0, 'FreeAndNil', typeVOID, ccREGISTER, @ FreeAndNil);
    RegisterParameter(H_SUB, typePVOID, Unassigned, true, 'X');

    if SizeOf(Pointer) = SizeOf(Integer) then
      typeNATIVEINT := typeINTEGER
    else
      typeNATIVEINT := typeINT64;
{$IFNDEF PAXARM}
    T1 := RegisterArrayType(H, '',
      RegisterSubrangeType(0, '', typeINTEGER, 1, 32),
      typeBYTE,
      1);
    T2 := RegisterArrayType(H, '',
      RegisterSubrangeType(0, '', typeINTEGER, 0, 259),
      typeCHAR,
      1);
    T3 := RegisterArrayType(H, '',
      RegisterSubrangeType(0, '', typeINTEGER, 0, 127),
      typeANSICHAR,
      1);

    H := RegisterRecordType(H, 'TFileRec', 1);
    H_TFileRec := H;
    RegisterTypeField(H, 'Handle', typeNATIVEINT);
    RegisterTypeField(H, 'Mode', typeWORD);
    RegisterTypeField(H, 'Flags', typeWORD);
    RegisterVariantRecordTypeField(H, 'RecSize', typeCARDINAL, 1);
    RegisterVariantRecordTypeField(H, 'BufSize', typeCARDINAL, 2);
    RegisterVariantRecordTypeField(H, 'BufPos', typeCARDINAL, 2);
    RegisterVariantRecordTypeField(H, 'BufEnd', typeCARDINAL, 2);
    RegisterVariantRecordTypeField(H, 'BufPtr', typePANSICHAR, 2);
    RegisterVariantRecordTypeField(H, 'OpenFunc', typePOINTER, 2);
    RegisterVariantRecordTypeField(H, 'InOutFunc', typePOINTER, 2);
    RegisterVariantRecordTypeField(H, 'FlushFunc', typePOINTER, 2);
    RegisterVariantRecordTypeField(H, 'CloseFunc', typePOINTER, 2);
    RegisterVariantRecordTypeField(H, 'UserData', T1, 2);
    RegisterVariantRecordTypeField(H, 'Name', T2, 2);

    T4 := RegisterArrayType(H, '',
      RegisterSubrangeType(0, '', typeINTEGER, 0, 5),
      typeANSICHAR,
      1);
    T5 := RegisterArrayType(H, '',
      RegisterSubrangeType(0, '', typeINTEGER, 0, 2),
      typeANSICHAR,
      1);

    H := RegisterRecordType(H, 'TTextRec', 1);
    H_TTextRec := H;
    RegisterTypeField(H, 'Handle', typeNATIVEINT);
    RegisterTypeField(H, 'Mode', typeWORD);
    RegisterTypeField(H, 'Flags', typeWORD);
    RegisterTypeField(H, 'BufSize', typeCARDINAL);
    RegisterTypeField(H, 'BufPos', typeCARDINAL);
    RegisterTypeField(H, 'BufEnd', typeCARDINAL);
    RegisterTypeField(H, 'BufPtr', typePANSICHAR);
    RegisterTypeField(H, 'OpenFunc', typePOINTER);
    RegisterTypeField(H, 'InOutFunc', typePOINTER);
    RegisterTypeField(H, 'FlushFunc', typePOINTER);
    RegisterTypeField(H, 'CloseFunc', typePOINTER);
    RegisterTypeField(H, 'UserData', T1);
    RegisterTypeField(H, 'Name', T2);
    RegisterTypeField(H, 'Buffer', T3);
{$IFDEF UNIC}
    RegisterTypeField(H, 'CodePage', typeWORD);
    RegisterTypeField(H, 'MBCSLength', typeSHORTINT);
    RegisterTypeField(H, 'MBCSBufPos', typeBYTE);
    RegisterVariantRecordTypeField(H, 'MBCSBuffer', T4, 1);
    RegisterVariantRecordTypeField(H, 'UTF16Buffer', T5, 2);
{$ENDIF}
{$ENDIF} //ndef paxarm

    RegisterRoutine(0, '', typeVOID, ccREGISTER, @ GetAddressGetCallerEIP);
    Id_GetAddressGetCallerEIP := LastSubId;

  end;
end;

initialization

  RUNNER_OWNER_OFFSET := IntPax(@TBaseRunner(nil).Owner);

  if Assigned(AssignRunnerLibProc) then
    AssignRunnerLibProc;

  {$IFDEF DRTTI}
    Initialize_paxcomp_2010;
    InitializePAXCOMP_2010Reg;
  {$ENDIF}

  GlobalSymbolTable := TBaseSymbolTable.Create;
  AddStdRoutines(GlobalSymbolTable);
  StdCard := GlobalSymbolTable.Card;
  StdSize := GlobalSymbolTable.GetDataSize;

  GlobalImportTable := GlobalSymbolTable;
  GlobalExtraImportTableList := TExtraImportTableList.Create;

  AvailUnitList := TStringList.Create;
  AvailUnitList1:= TStringList.Create;
  AvailTypeList := TStringList.Create;

{$IFDEF DRTTI}
  AvailUnitList.Sorted := true;
  AvailUnitList.Duplicates := dupIgnore;
  AvailUnitList.CaseSensitive := false;

  AvailUnitList1.Sorted := true;
  AvailUnitList1.Duplicates := dupIgnore;
  AvailUnitList1.CaseSensitive := false;

  AvailTypeList.Sorted := true;
  AvailTypeList.Duplicates := dupIgnore;
  AvailTypeList.CaseSensitive := false;
{$ENDIF}

finalization
  AvailUnitList.Free;
  AvailUnitList1.Free;
  AvailTypeList.Free;

  GlobalSymbolTable.Free;

  GlobalExtraImportTableList.Free;

  {$IFDEF DRTTI}
    Finalize_paxcomp_2010;
  {$ENDIF}

end.

