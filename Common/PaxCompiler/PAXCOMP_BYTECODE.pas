/// /////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_BYTECODE.pas
// ========================================================================
/// /////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_BYTECODE;

interface

uses {$I uses.def}
  TypInfo,
  Classes,
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_VAROBJECT,
  PAXCOMP_MODULE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_MAP,
  PAXCOMP_TYPEINFO,
  PAXCOMP_RTI,
  PAXCOMP_CLASSLST,
  PAXCOMP_GENERIC,
  PAXCOMP_FORBID;

type
  TCode = class;

  TCodeRec = class
  public
    Op: Integer;
    Arg1: Integer;
    Arg2: Integer;
    Res: Integer;
    GenOp: Integer;
    Language: Integer;
    SavedSubId: Integer;
    ModuleNum: Integer;
    IsStatic: Boolean;
    Upcase: Boolean;
    SwappedArgs: Boolean;
    LinePos: Integer;
    CodeRecTag: Integer;
    PatternFieldId: Integer;
    SavedLevel: Integer;
    OwnerObject: TCode;

    BreakLabel: Integer;
    ContinueLabel: Integer;
    LoopLabel: Integer;

    IsInherited: Boolean;

    constructor Create(i_OP: Integer; Code: TCode);
    procedure SwapArguments;
    function Clone: TCodeRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

{$IFDEF ARC}

  TCodeRecList = class(TList<TCodeRec>);
{$ELSE}
  TCodeRecList = TList;
{$ENDIF}
  TCodeProc = procedure of object;

  TCode = class
  private
    ProcList: array of TCodeProc;

    generic_binary_operators: TIntegerList;
    generic_unary_operators: TIntegerList;
    map_list: TIntegerList;
    context_list: TIntegerStack;

    SignTypeCast: Boolean;

    CRT_JS_FUNC_OBJECT_SWITCH: Boolean;

    temp_var_list: TAssocIntegers;

    SkipCheckFinal: Boolean;
    NoOverloadSearch: Boolean;

    procedure CreateUsingList(J: Integer);
    function GetPAX64: Boolean;
    function GetSizeOfPointer: Integer;
    function CheckAssignment(Arg1, Arg2: Integer): Boolean;
    function LookupTempVarId(Level, FinTypeId: Integer): Integer;
    function InsertSubCall(I: Integer): Boolean;
    procedure DiscardImport;
    function IsExplicitOff: Boolean;
    function CallExpected(ResId: Integer): Boolean;
    function InheritedExpected(ResId: Integer): Boolean;

    function PrevRec(J: Integer): TCodeRec;
    function NextRec(J: Integer): TCodeRec;

    function PrevN(J: Integer): Integer;
    function PrevPrevRec(J: Integer): TCodeRec;
    procedure RemoveDeclaredVar(Id: Integer);

    procedure MoveLValue(N_ASS: Integer);
    procedure MoveRValue(N_ASS: Integer);

    function IsValidOP(Op: Integer): Boolean;

    function GetRecord(I: Integer): TCodeRec;
    procedure SetRecord(I: Integer; value: TCodeRec);

    function ExistsOrdinalRelationalOperator(T1, T2: Integer): Boolean;
    function MatchSetTypes(T1, T2: Integer): Boolean;

    function GetDeclaredVar(const VarName: String; SubId: Integer;
      Upcase: Boolean; CurrPos: Integer): Integer;

    function EvalFrameworkType(const S: String): Integer;
    function CreateConst(TypeId: Integer; const value: Variant;
      ValueType: Integer = 0): Integer;
    function NewTempVar(Level, TypeId: Integer): Integer;
    function NewField(const FName: String; TypeId, OwnerId: Integer): Integer;
    function CreateWideCharVar(Level: Integer): Integer;
    function CreateBooleanVar(Level: Integer): Integer;
    function CreateByteBoolVar(Level: Integer): Integer;
    function CreateWordBoolVar(Level: Integer): Integer;
    function CreateLongBoolVar(Level: Integer): Integer;
    function CreateIntegerVar(Level: Integer): Integer;
    function CreatePointerVar(Level: Integer): Integer;
    function CreateCardinalVar(Level: Integer): Integer;
    function CreateSmallIntVar(Level: Integer): Integer;
    function CreateShortIntVar(Level: Integer): Integer;
    function CreateByteVar(Level: Integer): Integer;
    function CreateWordVar(Level: Integer): Integer;
    function CreateDoubleVar(Level: Integer): Integer;
    function CreateCurrencyVar(Level: Integer): Integer;
    function CreateSingleVar(Level: Integer): Integer;
    function CreateExtendedVar(Level: Integer): Integer;
    function CreateInt64Var(Level: Integer): Integer;
    function CreateUInt64Var(Level: Integer): Integer;
{$IFNDEF PAXARM}
    function CreateAnsiCharVar(Level: Integer): Integer;
    function CreateStringVar(Level: Integer): Integer;
    function CreateWideStringVar(Level: Integer): Integer;
    function CreateShortStringVar(Level, TypeId: Integer): Integer;
{$ENDIF}
    function CreateInterfaceVar(Level: Integer): Integer;
    function CreateClassVar(Level: Integer): Integer;
    function CreateUnicStringVar(Level: Integer): Integer;
    function CreateRecordVar(Level, TypeId: Integer): Integer;
    function CreateDynarrayVar(Level, TypeId: Integer): Integer;
    function CreateVariantVar(Level: Integer): Integer;
    function CreateOleVariantVar(Level: Integer): Integer;

    function InsertImplicitConversion(I, Arg, RecordTypeId,
      ResultTypeId: Integer): TCodeRec;

    function InsertExplicitConversion(I, RecordTypeId, ResultTypeId: Integer)
      : TCodeRec;

    function InsertConversionToInterface(I: Integer; Arg: Integer;
      InterfaceTypeId: Integer): TCodeRec;
    function InsertConversionToClass(I: Integer; Arg: Integer;
      ClassTypeId: Integer): TCodeRec;
    function InsertConversionToFrameworkClass(I: Integer; Arg: Integer;
      ClassTypeId: Integer): TCodeRec;

    function InsertDestroyLocalVar(I: Integer; Id: Integer): TCodeRec;
    function InsertConversionToWideChar(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToBoolean(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToByteBool(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToWordBool(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToLongBool(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToInteger(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToCardinal(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToByte(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToWord(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToShortInt(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToSmallInt(I: Integer; Arg: Integer): TCodeRec;

    function InsertConversionToDouble(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToSingle(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToExtended(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToCurrency(I: Integer; Arg: Integer): TCodeRec;
{$IFNDEF PAXARM}
    function InsertConversionToAnsiChar(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToAnsiString(I: Integer; Arg: Integer;
      Lang: Integer = PASCAL_LANGUAGE): TCodeRec;
    function InsertConversionToWideString(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToShortString(I: Integer; Arg: Integer): TCodeRec;
{$ENDIF}
    function InsertConversionToUnicString(I: Integer; Arg: Integer;
      Lang: Integer = PASCAL_LANGUAGE): TCodeRec;
    function InsertConversionToVariant(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToOleVariant(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToInt64(I: Integer; Arg: Integer): TCodeRec;
    function InsertConversionToUInt64(I: Integer; Arg: Integer): TCodeRec;
    function FindImplicitConversion(RecordTypeId, ParamArg,
      ResultArg: Integer): Integer;
    function FindExplicitConversion(RecordTypeId, ParamArg,
      ResultArg: Integer): Integer;
    function InsertConversionToRecord(I, Arg, RecordTypeId, ValueId: Integer)
      : TCodeRec;
    function ExistsBinaryOperator(RecordTypeId: Integer;
      const OperatorName: String): Boolean;
    procedure InsertBinaryOperator(RecordTypeId: Integer;
      const OperatorName: String);
    procedure InsertUnaryOperator(RecordTypeId: Integer;
      const OperatorName: String);

    procedure CompressListOfOverloaded(applicable_list, pos: TIntegerList);
    procedure CheckSubrangeBounds(SubrangeTypeId: Integer;
      ValueRec: TSymbolRec);

    function ConvertSetLiteralToDynarrayLiteral(CurrLevel, DynarrayTypeId,
      SetId: Integer): Integer;
    procedure CreateSetObject(Id: Integer);
    function GetNextStmt(I: Integer): Integer;
    function GetNextRec(I: Integer): TCodeRec;

    procedure InsertDeclareTempVar; overload;
    procedure InsertDeclareTempVar(Id: Integer); overload;

    procedure OperAddAncestor;

    procedure OperCheckOverride(List: TIntegerList);
    procedure OperAddMessage;

    procedure OperOP_BEGIN_CLASS_TYPE;
    procedure OperInitSub;
    procedure OperAddress;
    procedure OperEval;
    procedure OperEvalInherited;
    procedure OperEvalConstructor;
    procedure OperImplements;
    procedure OperGoTrue;
    procedure OperGoFalse;
    procedure OperGoTrueBool;
    procedure OperGoFalseBool;

    procedure OperSizeOf;

    procedure OperAssignment;
    procedure OperAssignEnum;
    procedure OperAssignmentConst;
    procedure OperCheckSubrangeType;

    procedure OperDetermineType;
    procedure OperCreateShortStringType;

    procedure OperCreateMethod;
    procedure OperAddMethodIndex;

    procedure OperPushContext;
    procedure OperPopContext;

    procedure OperAbs;
    procedure OperInc;
    procedure OperDec;
    procedure OperPred;
    procedure OperSucc;
    procedure OperOrd;
    procedure OperChr;
    procedure OperStr;
    procedure OperSetLength;
    procedure OperLow;
    procedure OperHigh;
    procedure OperAssigned;

    procedure OperAddition;
    procedure OperSubtraction;
    procedure OperMultiplication;
    procedure OperDivision;
    procedure OperIDivision;
    procedure OperModulo;
    procedure OperLeftShift;
    procedure OperRightShift;
    procedure OperAnd;
    procedure OperOr;
    procedure OperXor;

    procedure OperNegation;
    procedure OperPositive;
    procedure OperNot;

    procedure OperLessThan;
    procedure OperLessThanOrEqual;
    procedure OperGreaterThan;
    procedure OperGreaterThanOrEqual;
    procedure OperEqual;
    procedure OperNotEqual;
    procedure OperRaise;
    procedure OperIs;
    procedure OperAs;
    procedure OperTypeInfo;
    procedure OperAddTypeInfo;

    procedure OperPush;
    procedure OperPushInstance;
    procedure OperAdjustInstance;
    procedure OperCall;
    procedure OperCallDefaultConstructor;
    procedure OperCheckSubCall;
    procedure OperVCall;

    procedure OperCheckFinal;

    procedure OperField;
    procedure OperElem;

    procedure OperItem;
    procedure OperRecordItem;
    procedure OperBeginInitConst;
    procedure OperEndInitConst;

    procedure OperPrint;
    procedure OperTerminal;

    procedure OperTypeCast;

    procedure OperSetInclude;
    procedure OperSetExclude;
    procedure OperSetMembership;

    procedure OperVarArrayIdx;
    procedure OperSetEventProp;

    procedure OperOleValue;
    procedure OperOleParam;

    procedure OperFindConstructor;

    procedure OperBeginCrtJsFuncObject;
    procedure OperEndCrtJsFuncObject;
    procedure OperGetNextJSProp;
    procedure OperJStypeof;
    procedure OperJSvoid;

    procedure OperGetEnumerator;
    procedure OperSetReadId;
    procedure OperSetWriteId;
    procedure OperDetermineProp;

    procedure OperToFWObject;
    procedure OperFrameworkOn;
    procedure OperFrameworkOff;
    procedure OperAssignLambdaTypes;

    procedure RemoveNOP;
    procedure RemovePause;
    procedure Remove(R: TCodeRec);
    function Insert(I: Integer; R: TCodeRec): Integer;

    procedure OperNop;

    procedure CreateError(const Message: string; params: array of Const);
    procedure CreateEvalError(const VarName: String;
      using_stack: TIntegerStack);

    procedure RaiseError(const Message: string; params: array of Const);
    function HasError: Boolean;
    function GetCard: Integer;

    function AddTypeInfo(TypeId, SourceTypeId: Integer): TTypeInfoContainer;
    procedure AdjustTypes;
    function LookupInExtraUnitList(const S: String): Integer;
    function GetCurrSourceLineNumber: Integer;

  public
    A: TCodeRecList;
    N: Integer;

    kernel: Pointer;
    using_list: TIntegerList;
    extra_using_list: TIntegerList;
    used_private_members: TIntegerList;

    constructor Create(i_kernel: Pointer);
    destructor Destroy; override;
    procedure RestoreFieldType(J: Integer);
    function IsLocalPos(pos: Integer): Boolean;
    procedure Reset;
    procedure RemoveInstruction(Op, Arg1, Arg2, Res: Integer);
    function RemoveLastEvalInstruction(const S: String;
      Upcase: Boolean = true): Integer;
    function Add(Op, Arg1, Arg2, Res: Integer; SavedLevel: Integer;
      Upcase: Boolean; Language: Integer; ModuleNum: Integer; LinePos: Integer)
      : TCodeRec;
    function Ins(Ip, Op, Arg1, Arg2, Res: Integer; SavedLevel: Integer;
      Upcase: Boolean; Language: Integer; ModuleNum: Integer; LinePos: Integer)
      : TCodeRec;
    procedure RemoveEvalOp;
    procedure ProcessImplements;
    procedure RemoveEvalOpForTypes;
    procedure UpdateDefaultConstructors;
    procedure CheckTypes;
    function GetSymbolRec(Id: Integer): TSymbolRec;
    function GetModule(I: Integer): TModule;
    function GetModuleNumber(J: Integer): Integer;
    function GetSourceLineNumber(I: Integer): Integer;
    function GetSourceLine(I: Integer): String;
    function GetIncludedFileName(I: Integer): String;
    function GetLinePos(I: Integer): Integer;
    procedure ProcessSizeOf;
    procedure ChangeOrderOfActualParams;
    procedure RemoveLoadProc;
    procedure InsertHostMonitoring;
    procedure Optimization;
    procedure Optimization2;

    procedure InsertDynamicTypeDestructors;
    function CompareConversions(Id, id1, id2: Integer): Integer;
    function ExistsImplicitNumericConversion(type_id_source,
      type_id_dest: Integer): Boolean;
    function ExistsImplicitConversion(id_source, id_dest: Integer): Boolean;
    procedure ReplaceId(old_id, new_id: Integer);
    procedure ReplaceIdEx(old_id, new_id, start_pos, end_pos: Integer;
      Local: Boolean; ReplaceDeclId: Boolean = true);
    procedure AssignShifts;
    procedure CreateMapping(result: TMapTable; Host: Boolean;
      HostMapTable, ScriptMapTable: TMapTable);
    procedure GenHostStructConst;
    function GetUpcase(J: Integer): Boolean;
    function GetLanguage(J: Integer): Integer;
    function GetCurrSubId(CurrN: Integer): Integer;
    function GetCurrSelfId(CurrN: Integer): Integer;
    function GetCurrNamespaceId(J: Integer): Integer;
    function GetCurrClassId(J: Integer): Integer;
    function FindRecord1(Op, Arg1: Integer): Integer;
    procedure LocateDummyName(var NN: Integer);
    procedure InsertFinalizators;
    procedure InsertTryFinally;
    procedure AdjustTryList;
    procedure SetLastCondRaise;
    function GetLevel(pos: Integer): Integer;
    function GetClassId(pos: Integer): Integer;
    function GetStructureId(pos: Integer): Integer;
    procedure DestroyExpressionTempVars(ResultId: Integer);
    procedure OperNotImpl;

    function GetPrintKeyword: String;
    function GetPrintlnKeyword: String;
    function GetStmt(I: Integer): Integer;
    function ParamHasBeenChanged(I, Id: Integer): Boolean;
    procedure CreateExecLines(MR: TModuleRec);
    procedure AddWarnings;
    procedure DeleteRecord(I: Integer);
    function GetCompletionVisibility(Id: Integer; NN: Integer)
      : TMemberVisibilitySet;
    procedure CheckExpansions;
    procedure RemoveUnusedLabels;
    procedure CheckOverride;
    function GetTrueSubId: Integer;
    procedure CreateMethodEntryLists;
    procedure CreateEvalList(L: TStringList);
    procedure CreateExportList(MapTable: TMapTable);
    procedure InsertCallHostEvents;

    property Card: Integer read GetCard;
    property Records[I: Integer]: TCodeRec read GetRecord
      write SetRecord; default;
    property PAX64: Boolean read GetPAX64;
    property SizeOfPointer: Integer read GetSizeOfPointer;
    property CurrSourceLineNumber: Integer read GetCurrSourceLineNumber;
  end;

implementation

uses
  PAXCOMP_KERNEL,
  PAXCOMP_BASERUNNER,
  PAXCOMP_STDLIB;

constructor TCode.Create(i_kernel: Pointer);
var
  I: Integer;
begin
  Self.kernel := i_kernel;
  A := TCodeRecList.Create;
  SetLength(ProcList, -OP_DUMMY);

  for I := 0 to System.Length(ProcList) - 1 do
    ProcList[I] := OperNop;

  ProcList[-OP_NOP] := OperNop;
  ProcList[-OP_SEPARATOR] := OperNop;
  ProcList[-OP_STMT] := OperNop;
  ProcList[-OP_SET_CODE_LINE] := OperNop;
  ProcList[-OP_BEGIN_MODULE] := OperNop;
  ProcList[-OP_END_MODULE] := OperNop;
  ProcList[-OP_EXTRA_BYTECODE] := OperNop;

  ProcList[-OP_BEGIN_NAMESPACE] := OperNop;
  ProcList[-OP_END_NAMESPACE] := OperNop;

  ProcList[-OP_BEGIN_TYPE] := OperNop;
  ProcList[-OP_END_TYPE] := OperNop;

  ProcList[-OP_BEGIN_CLASS_TYPE] := OperOP_BEGIN_CLASS_TYPE;
  ProcList[-OP_END_CLASS_TYPE] := OperNop;

  ProcList[-OP_BEGIN_HELPER_TYPE] := OperNop;
  ProcList[-OP_END_HELPER_TYPE] := OperNop;

  ProcList[-OP_BEGIN_CLASSREF_TYPE] := OperNop;
  ProcList[-OP_END_CLASSREF_TYPE] := OperNop;

  ProcList[-OP_BEGIN_INTERFACE_TYPE] := OperNop;
  ProcList[-OP_END_INTERFACE_TYPE] := OperNop;

  ProcList[-OP_BEGIN_RECORD_TYPE] := OperNop;
  ProcList[-OP_END_RECORD_TYPE] := OperNop;

  ProcList[-OP_BEGIN_ARRAY_TYPE] := OperNop;
  ProcList[-OP_END_ARRAY_TYPE] := OperNop;

  ProcList[-OP_BEGIN_DYNARRAY_TYPE] := OperNop;
  ProcList[-OP_END_DYNARRAY_TYPE] := OperNop;

  ProcList[-OP_BEGIN_SUBRANGE_TYPE] := OperNop;
  ProcList[-OP_END_SUBRANGE_TYPE] := OperNop;

  ProcList[-OP_BEGIN_ENUM_TYPE] := OperNop;
  ProcList[-OP_END_ENUM_TYPE] := OperNop;

  ProcList[-OP_BEGIN_SET_TYPE] := OperNop;
  ProcList[-OP_END_SET_TYPE] := OperNop;

  ProcList[-OP_BEGIN_POINTER_TYPE] := OperNop;
  ProcList[-OP_END_POINTER_TYPE] := OperNop;

  ProcList[-OP_BEGIN_PROC_TYPE] := OperNop;
  ProcList[-OP_END_PROC_TYPE] := OperNop;

  ProcList[-OP_BEGIN_ALIAS_TYPE] := OperNop;
  ProcList[-OP_END_ALIAS_TYPE] := OperNop;

{$IFNDEF PAXARM}
  ProcList[-OP_BEGIN_SHORTSTRING_TYPE] := OperNop;
  ProcList[-OP_END_SHORTSTRING_TYPE] := OperNop;
{$ENDIF}
  ProcList[-OP_BEGIN_CONST] := OperNop;
  ProcList[-OP_END_CONST] := OperNop;

  ProcList[-OP_BEGIN_VAR] := OperNop;
  ProcList[-OP_END_VAR] := OperNop;

  ProcList[-OP_END_INTERFACE_SECTION] := OperNop;
  ProcList[-OP_END_IMPORT] := OperNop;
  ProcList[-OP_BEGIN_INITIALIZATION] := OperNop;
  ProcList[-OP_END_INITIALIZATION] := OperNop;
  ProcList[-OP_BEGIN_FINALIZATION] := OperNop;
  ProcList[-OP_END_FINALIZATION] := OperNop;
  ProcList[-OP_BEGIN_USING] := OperNop;
  ProcList[-OP_END_USING] := OperNop;
  ProcList[-OP_BEGIN_BLOCK] := OperNop;
  ProcList[-OP_END_BLOCK] := OperNop;
  ProcList[-OP_EMIT_OFF] := OperNop;
  ProcList[-OP_EMIT_ON] := OperNop;
  ProcList[-OP_EVAL] := OperEval;
  ProcList[-OP_EVAL_INHERITED] := OperEvalInherited;
  ProcList[-OP_EVAL_CONSTRUCTOR] := OperEvalConstructor;
  ProcList[-OP_UPDATE_INSTANCE] := OperNop;
  ProcList[-OP_CLEAR_EDX] := OperNop;
  ProcList[-OP_IMPLEMENTS] := OperImplements;
  ProcList[-OP_LOAD_PROC] := OperNop;
  ProcList[-OP_CHECK_OVERRIDE] := OperNop;
  ProcList[-OP_BEGIN_WITH] := OperPushContext;
  ProcList[-OP_END_WITH] := OperPopContext;
  ProcList[-OP_BEGIN_INIT_CONST] := OperBeginInitConst;
  ProcList[-OP_END_INIT_CONST] := OperEndInitConst;
  ProcList[-OP_EXIT] := OperNop;
  ProcList[-OP_GO] := OperNop;
  ProcList[-OP_GO_TRUE] := OperGoTrue;
  ProcList[-OP_GO_FALSE] := OperGoFalse;
  ProcList[-OP_GO_TRUE_BOOL] := OperGoTrueBool;
  ProcList[-OP_GO_FALSE_BOOL] := OperGoFalseBool;
  ProcList[-OP_GO_DL] := OperNop;
  ProcList[-OP_SAVE_EDX] := OperNop;
  ProcList[-OP_RESTORE_EDX] := OperNop;
  ProcList[-OP_BEGIN_CALL] := OperNop;
  ProcList[-OP_CHECK_SUB_CALL] := OperCheckSubCall;
  ProcList[-OP_CHECK_FINAL] := OperCheckFinal;
  ProcList[-OP_PUSH_INSTANCE] := OperPushInstance;
  ProcList[-OP_ADJUST_INSTANCE] := OperAdjustInstance;
  ProcList[-OP_CALL] := OperCall;
  ProcList[-OP_VCALL] := OperVCall;
  ProcList[-OP_CALL_DEFAULT_CONSTRUCTOR] := OperCallDefaultConstructor;
  ProcList[-OP_TYPE_CAST] := OperTypeCast;
  ProcList[-OP_DESTROY_LOCAL_VAR] := OperNop;
  ProcList[-OP_PUSH] := OperPush;
  ProcList[-OP_PUSH_EBP] := OperNop;
  ProcList[-OP_POP] := OperNop;
  ProcList[-OP_PUSH_CONTEXT] := OperPushContext;
  ProcList[-OP_POP_CONTEXT] := OperPopContext;
  ProcList[-OP_LABEL] := OperNop;
  ProcList[-OP_BEGIN_SUB] := OperNop;
  ProcList[-OP_DECLARE_LOCAL_VAR] := OperNop;
  ProcList[-OP_DECLARE_TEMP_VAR] := OperNop;
  ProcList[-OP_INIT_SUB] := OperInitSub;
  ProcList[-OP_END_SUB] := OperNop;
  ProcList[-OP_FIN_SUB] := OperNop;
  ProcList[-OP_EPILOGUE_SUB] := OperNop;

  ProcList[-OP_BEGIN_GLOBAL_BLOCK] := OperNop;
  ProcList[-OP_EPILOGUE_GLOBAL_BLOCK] := OperNop;
  ProcList[-OP_EPILOGUE_GLOBAL_BLOCK2] := OperNop;
  ProcList[-OP_END_GLOBAL_BLOCK] := OperNop;

  ProcList[-OP_ASSIGN_TYPE] := OperNop;
  ProcList[-OP_DETERMINE_TYPE] := OperDetermineType;
  ProcList[-OP_ASSIGN_THE_SAME_TYPE] := OperNop;
  ProcList[-OP_ASSIGN_TYPE_ALIAS] := OperNop;
  ProcList[-OP_ASSIGN_LAMBDA_TYPES] := OperAssignLambdaTypes;
  ProcList[-OP_CREATE_POINTER_TYPE] := OperNop;
  ProcList[-OP_CREATE_CLASSREF_TYPE] := OperNop;
  ProcList[-OP_CREATE_DYNAMIC_ARRAY_TYPE] := OperNop;
  ProcList[-OP_CREATE_SHORTSTRING_TYPE] := OperCreateShortStringType;
  ProcList[-OP_ADDRESS] := OperAddress;
  ProcList[-OP_TERMINAL] := OperTerminal;
  ProcList[-OP_ADDRESS_PROG] := OperNop;
  ProcList[-OP_ASSIGN_PROG] := OperNop;
  ProcList[-OP_ASSIGN_INT_M] := OperNop;
  ProcList[-OP_LVALUE] := OperNop;
  ProcList[-OP_ASSIGN] := OperAssignment;
  ProcList[-OP_ASSIGN_CONST] := OperAssignmentConst;
  ProcList[-OP_ASSIGN_ENUM] := OperAssignEnum;
  ProcList[-OP_CHECK_SUBRANGE_TYPE] := OperCheckSubrangeType;
  ProcList[-OP_SIZEOF] := OperSizeOf;

  ProcList[-OP_TRY_ON] := OperNop;
  ProcList[-OP_TRY_OFF] := OperNop;
  ProcList[-OP_FINALLY] := OperNop;
  ProcList[-OP_RAISE] := OperRaise;
  ProcList[-OP_COND_RAISE] := OperNop;
  ProcList[-OP_BEGIN_EXCEPT_BLOCK] := OperNop;
  ProcList[-OP_END_EXCEPT_BLOCK] := OperNop;
  ProcList[-OP_EXCEPT] := OperNop;
  ProcList[-OP_EXCEPT_ON] := OperNop;
  ProcList[-OP_PAUSE] := OperNop;
  ProcList[-OP_CHECK_PAUSE] := OperNop;
  ProcList[-OP_CHECK_PAUSE_LIGHT] := OperNop;
  ProcList[-OP_HALT] := OperNop;

  ProcList[-OP_OVERFLOW_CHECK] := OperNop;

  ProcList[-OP_SET_INCLUDE] := OperSetInclude;
  ProcList[-OP_SET_INCLUDE_INTERVAL] := OperSetInclude;
  ProcList[-OP_SET_EXCLUDE] := OperSetExclude;
  ProcList[-OP_SET_MEMBERSHIP] := OperSetMembership;

  ProcList[-OP_ABS] := OperAbs;
  ProcList[-OP_INC] := OperInc;
  ProcList[-OP_DEC] := OperDec;
  ProcList[-OP_PRED] := OperPred;
  ProcList[-OP_SUCC] := OperSucc;
  ProcList[-OP_ORD] := OperOrd;
  ProcList[-OP_CHR] := OperChr;
  ProcList[-OP_LOW] := OperLow;
  ProcList[-OP_HIGH] := OperHigh;
  ProcList[-OP_STR] := OperStr;
  ProcList[-OP_SET_LENGTH] := OperSetLength;
  ProcList[-OP_ASSIGNED] := OperAssigned;

  ProcList[-OP_PLUS] := OperAddition;
  ProcList[-OP_MINUS] := OperSubtraction;
  ProcList[-OP_MULT] := OperMultiplication;
  ProcList[-OP_DIV] := OperDivision;
  ProcList[-OP_IDIV] := OperIDivision;
  ProcList[-OP_MOD] := OperModulo;
  ProcList[-OP_SHL] := OperLeftShift;
  ProcList[-OP_SHR] := OperRightShift;
  ProcList[-OP_AND] := OperAnd;
  ProcList[-OP_OR] := OperOr;
  ProcList[-OP_XOR] := OperXor;

  ProcList[-OP_NEG] := OperNegation;
  ProcList[-OP_POSITIVE] := OperPositive;
  ProcList[-OP_NOT] := OperNot;

  ProcList[-OP_LT] := OperLessThan;
  ProcList[-OP_LE] := OperLessThanOrEqual;
  ProcList[-OP_GT] := OperGreaterThan;
  ProcList[-OP_GE] := OperGreaterThanOrEqual;
  ProcList[-OP_EQ] := OperEqual;
  ProcList[-OP_NE] := OperNotEqual;
  ProcList[-OP_IS] := OperIs;
  ProcList[-OP_AS] := OperAs;
  ProcList[-OP_TYPEINFO] := OperTypeInfo;
  ProcList[-OP_ADD_TYPEINFO] := OperAddTypeInfo;

  ProcList[-OP_CLASSNAME] := OperNop;

  ProcList[-OP_GET_PROG] := OperNop;

  ProcList[-OP_ADD_MESSAGE] := OperAddMessage;

  ProcList[-OP_CREATE_METHOD] := OperCreateMethod;
  ProcList[-OP_ADD_METHOD_INDEX] := OperAddMethodIndex;

  ProcList[-OP_RET] := OperNop;
  ProcList[-OP_FIELD] := OperField;
  ProcList[-OP_ELEM] := OperElem;
  ProcList[-OP_ITEM] := OperItem;
  ProcList[-OP_RECORD_ITEM] := OperRecordItem;
  ProcList[-OP_PRINT] := OperPrint;
  ProcList[-OP_PRINT_EX] := OperNop;

  ProcList[-OP_DETERMINE_PROP] := OperDetermineProp;

  ProcList[-OP_SET_SET_PROP] := OperNop;
  ProcList[-OP_SET_ORD_PROP] := OperNop;
  ProcList[-OP_SET_INTERFACE_PROP] := OperNop;
{$IFNDEF PAXARM}
  ProcList[-OP_SET_ANSISTR_PROP] := OperNop;
  ProcList[-OP_SET_WIDESTR_PROP] := OperNop;
{$ENDIF}
  ProcList[-OP_SET_UNICSTR_PROP] := OperNop;
  ProcList[-OP_SET_FLOAT_PROP] := OperNop;
  ProcList[-OP_SET_VARIANT_PROP] := OperNop;
  ProcList[-OP_SET_INT64_PROP] := OperNop;
  ProcList[-OP_SET_EVENT_PROP] := OperSetEventProp;
  ProcList[-OP_SET_EVENT_PROP2] := OperNop;

  ProcList[-OP_VARARRAY_PUT] := OperNop;
  ProcList[-OP_VARARRAY_GET] := OperNop;
  ProcList[-OP_VARARRAY_IDX] := OperVarArrayIdx;

  ProcList[-OP_OLE_VALUE] := OperOleValue;
  ProcList[-OP_OLE_PARAM] := OperOleParam;
  ProcList[-OP_OLE_GET] := OperNop;
  ProcList[-OP_OLE_SET] := OperNop;

  ProcList[-OP_ONCREATE_OBJECT] := OperNop;
  ProcList[-OP_ON_AFTER_OBJECT_CREATION] := OperNop;
  ProcList[-OP_CREATE_OBJECT] := OperNop;
  ProcList[-OP_DESTROY_OBJECT] := OperNop;
  ProcList[-OP_GET_VMT_ADDRESS] := OperNop;
  ProcList[-OP_PUSH_INSTANCE] := OperNop;

  ProcList[-OP_ONCREATE_HOST_OBJECT] := OperNop;
  ProcList[-OP_ONDESTROY_HOST_OBJECT] := OperNop;

  ProcList[-OP_SAVE_REGS] := OperNop;
  ProcList[-OP_RESTORE_REGS] := OperNop;

  ProcList[-OP_ADD_ANCESTOR] := OperAddAncestor;

  ProcList[-OP_ERR_ABSTRACT] := OperNop;
  ProcList[-OP_UPDATE_DEFAULT_CONSTRUCTOR] := OperNop;
  ProcList[-OP_FIND_CONSTRUCTOR] := OperFindConstructor;

  ProcList[-OP_BEGIN_CRT_JS_FUNC_OBJECT] := OperBeginCrtJsFuncObject;
  ProcList[-OP_END_CRT_JS_FUNC_OBJECT] := OperEndCrtJsFuncObject;

  ProcList[-OP_ASSIGN_SHIFT] := OperNop;

  ProcList[-OP_GET_NEXTJSPROP] := OperGetNextJSProp;
  ProcList[-OP_JS_TYPEOF] := OperJStypeof;
  ProcList[-OP_JS_VOID] := OperJSvoid;

  ProcList[-OP_GET_ENUMERATOR] := OperGetEnumerator;

  ProcList[-OP_SET_READ_ID] := OperSetReadId;
  ProcList[-OP_SET_WRITE_ID] := OperSetWriteId;

  ProcList[-OP_TO_FW_OBJECT] := OperToFWObject;
  ProcList[-OP_FRAMEWORK_ON] := OperFrameworkOn;
  ProcList[-OP_FRAMEWORK_OFF] := OperFrameworkOff;

  CRT_JS_FUNC_OBJECT_SWITCH := false;

  generic_binary_operators := TIntegerList.Create;
  with generic_binary_operators do
  begin
    Add(OP_PLUS);
    Add(OP_MINUS);
    Add(OP_MULT);
    Add(OP_DIV);
    Add(OP_IDIV);
    Add(OP_MOD);
  end;

  generic_unary_operators := TIntegerList.Create;
  with generic_unary_operators do
  begin
    Add(OP_NEG);
  end;

  map_list := TIntegerList.Create;
  context_list := TIntegerStack.Create;
  using_list := TIntegerList.Create(true);
  temp_var_list := TAssocIntegers.Create;

  extra_using_list := TIntegerList.Create;

  used_private_members := TIntegerList.Create(true);
end;

destructor TCode.Destroy;
begin
  Reset;
  FreeAndNil(A);
  FreeAndNil(generic_binary_operators);
  FreeAndNil(generic_unary_operators);
  FreeAndNil(map_list);
  FreeAndNil(context_list);
  FreeAndNil(using_list);
  FreeAndNil(temp_var_list);
  FreeAndNil(extra_using_list);
  FreeAndNil(used_private_members);

  inherited;
end;

function TCode.GetPrintKeyword: String;
var
  I: Integer;
begin
  result := 'print';
  for I := N downto 1 do
    if Records[I].Op = OP_PRINT_KWD then
    begin
      result := GetSymbolRec(Records[I].Arg1).value;
      Exit;
    end;
end;

function TCode.GetPrintlnKeyword: String;
var
  I: Integer;
begin
  result := 'println';
  for I := N downto 1 do
    if Records[I].Op = OP_PRINTLN_KWD then
    begin
      result := GetSymbolRec(Records[I].Arg1).value;
      Exit;
    end;
end;

function TCode.GetLevel(pos: Integer): Integer;
var
  I, K: Integer;
begin
  K := Card;

  if pos > K then
    I := K
  else
    I := pos;

  result := Records[I].SavedLevel;
  if result >= 0 then
    Exit;

  repeat
    Inc(I);
    if I > K then
      Exit;
    result := Records[I].SavedLevel;
    if result >= 0 then
      Exit;
  until false;
end;

function TCode.GetClassId(pos: Integer): Integer;
var
  Op, K: Integer;
begin
  result := 0;
  K := Card;
  repeat
    Op := Records[pos].Op;

    if Op = OP_BEGIN_CLASS_TYPE then
      Exit;
    if Op = OP_BEGIN_MODULE then
      Exit;
    if Op = OP_END_MODULE then
      Exit;
    if Op = OP_END_CLASS_TYPE then
    begin
      result := Records[pos].Arg1;
      Exit;
    end;
    if Op = OP_END_SUB then
    begin
      result := GetSymbolRec(Records[pos].Arg1).Level;
      Exit;
    end;
    Inc(pos);
    if pos > K then
      Exit;
  until false;
end;

function TCode.GetStructureId(pos: Integer): Integer;
var
  Op, K: Integer;
begin
  result := 0;
  K := Card;
  repeat
    Op := Records[pos].Op;

    if Op = OP_BEGIN_RECORD_TYPE then
      Exit;
    if Op = OP_BEGIN_MODULE then
      Exit;
    if Op = OP_END_MODULE then
      Exit;
    if Op = OP_END_RECORD_TYPE then
    begin
      result := Records[pos].Arg1;
      Exit;
    end;
    if Op = OP_END_SUB then
    begin
      result := GetSymbolRec(Records[pos].Arg1).Level;
      Exit;
    end;
    Inc(pos);
    if pos > K then
      Exit;
  until false;
end;

function TCode.IsLocalPos(pos: Integer): Boolean;
var
  L: Integer;
begin
  L := GetLevel(pos);
  if L = 0 then
  begin
    result := false;
    Exit;
  end;
  result := GetSymbolRec(L).Kind in kindSUBS;
end;

procedure TCode.ReplaceId(old_id, new_id: Integer);
begin
  ReplaceIdEx(old_id, new_id, 1, Card, false);
end;

procedure TCode.ReplaceIdEx(old_id, new_id, start_pos, end_pos: Integer;
  Local: Boolean; ReplaceDeclId: Boolean = true);
var
  I: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  SR: TSymbolRec;
begin
  if old_id = new_id then
    Exit;

  if ReplaceDeclId then
    if old_id = TKernel(kernel).FindDeclId then
      TKernel(kernel).FindDeclId := new_id;

  SymbolTable := TKernel(kernel).SymbolTable;

  for I := start_pos to end_pos do
  begin
    R := Records[I];

    if Local then
      if R.Op = OP_END_MODULE then
        break;

    if R.Op <> OP_SEPARATOR then
    begin
      if R.Arg1 = old_id then
        R.Arg1 := new_id;
      if R.Arg2 = old_id then
        R.Arg2 := new_id;
      if R.Res = old_id then
        R.Res := new_id;
    end;
  end;

  if GetSymbolRec(new_id).Kind = KindTYPE then
    TKernel(kernel).TypeDefList.ReplaceId(old_id, new_id);

  if GetSymbolRec(new_id).Kind in kindSUBS then
    Exit;

  for I := SymbolTable.CompileCard to SymbolTable.Card do
  begin
    SR := SymbolTable[I];
    if SR.OwnerId = old_id then
      SR.OwnerId := new_id;
    if SR.PatternId = old_id then
      SR.PatternId := new_id;
  end;
end;

procedure TCode.OperNotImpl;
begin
  RaiseError(errInternalError, []);
end;

procedure TCode.OperNop;
begin
end;

function TCode.GetCard: Integer;
begin
  result := A.Count;
end;

function TCode.GetStmt(I: Integer): Integer;
begin
  result := I;
  while Records[result].Op <> OP_STMT do
  begin
    Dec(result);

    if Records[result].Op = OP_BEGIN_MODULE then
      break;

    if result = 1 then
      Exit;
  end;
end;

function TCode.GetNextRec(I: Integer): TCodeRec;
var
  K: Integer;
begin
  result := nil;
  K := Card;
  repeat
    Inc(I);
    if I > K then
      Exit;
    if Records[I].Op <> OP_SEPARATOR then
    begin
      result := Records[I];
      Exit;
    end;
  until false;
end;

function TCode.GetNextStmt(I: Integer): Integer;
var
  K: Integer;
begin
  result := I + 1;
  K := Card;
  while Records[result].Op <> OP_STMT do
    with Records[result] do
    begin
      Inc(result);
      if result = K then
        Exit;
      if Op = OP_EPILOGUE_SUB then
      begin
        Dec(result);
        Exit;
      end;
      if Op = OP_END_SUB then
      begin
        Dec(result);
        Exit;
      end;
      if Op = OP_END_MODULE then
      begin
        Dec(result);
        Exit;
      end;
    end;
end;

function TCode.GetModuleNumber(J: Integer): Integer;
var
  I: Integer;
begin
  if J > Card then
    I := Card
  else
    I := J;
  result := Records[I].ModuleNum;
end;

function TCode.GetUpcase(J: Integer): Boolean;
var
  I: Integer;
begin
  if J > Card then
    I := Card
  else
    I := J;
  result := Records[I].Upcase;
end;

function TCode.GetLanguage(J: Integer): Integer;
var
  I: Integer;
begin
  if J > Card then
    I := Card
  else
    I := J;
  result := Records[I].Language;
end;

function TCode.GetCurrClassId(J: Integer): Integer;
var
  I: Integer;
  R: TCodeRec;
begin
  result := 0;

  if J > Card then
    I := Card
  else
    I := J;

  repeat
    R := Records[I];
    if R.Op = OP_END_MODULE then
      Exit;
    if R.Op = OP_END_CLASS_TYPE then
      Exit;
    if R.Op = OP_BEGIN_CLASS_TYPE then
    begin
      result := R.Arg1;
      Exit;
    end;

    Dec(I);
    if I <= 0 then
      Exit;
  until false;
end;

function TCode.GetCurrNamespaceId(J: Integer): Integer;
var
  I: Integer;
  R: TCodeRec;
begin
  result := 0;

  if J > Card then
    I := Card
  else
    I := J;

  repeat
    R := Records[I];
    if R.Op = OP_END_MODULE then
      Exit;
    if R.Op = OP_END_NAMESPACE then
      Exit;
    if R.Op = OP_BEGIN_NAMESPACE then
    begin
      result := R.Arg1;
      Exit;
    end;

    Dec(I);
    if I <= 0 then
      Exit;
  until false;
end;

procedure TCode.Reset;
var
  I: Integer;
begin
  for I := 1 to Card do
{$IFDEF ARC}
    Records[I] := nil;
{$ELSE}
    Records[I].Free;
{$ENDIF}
  A.Clear;
  SignTypeCast := false;
  map_list.Clear;
  context_list.Clear;
  using_list.Clear;
  temp_var_list.Clear;
  extra_using_list.Clear;
  used_private_members.Clear;
end;

function TCode.Add(Op, Arg1, Arg2, Res: Integer; SavedLevel: Integer;
  Upcase: Boolean; Language: Integer; ModuleNum: Integer; LinePos: Integer)
  : TCodeRec;
begin
  result := TCodeRec.Create(Op, nil);
  result.OwnerObject := Self;
  result.Upcase := Upcase;
  result.Language := Language;
  result.ModuleNum := ModuleNum;

  result.SavedLevel := SavedLevel;
  result.Arg1 := Arg1;
  result.Arg2 := Arg2;
  result.Res := Res;
  result.LinePos := LinePos;
  A.Add(result);
end;

function TCode.Ins(Ip, Op, Arg1, Arg2, Res: Integer; SavedLevel: Integer;
  Upcase: Boolean; Language: Integer; ModuleNum: Integer; LinePos: Integer)
  : TCodeRec;
begin
  result := TCodeRec.Create(Op, nil);
  result.OwnerObject := Self;
  result.Upcase := Upcase;
  result.Language := Language;
  result.ModuleNum := ModuleNum;

  result.SavedLevel := SavedLevel;
  result.Arg1 := Arg1;
  result.Arg2 := Arg2;
  result.Res := Res;
  result.LinePos := LinePos;
  A.Insert(Ip, result);
end;

function TCode.GetSymbolRec(Id: Integer): TSymbolRec;
begin
  result := TKernel(kernel).SymbolTable[Id];
end;

procedure TCode.Remove(R: TCodeRec);
begin
  A.Remove(R);
end;

function TCode.Insert(I: Integer; R: TCodeRec): Integer;
// var
// J: Integer;
begin
  result := I;
  A.Insert(I - 1, R);
  {
    for J:=I to Card do
    if Records[J].Op = OP_SET_CODE_LINE then
    Inc(Records[J].Arg1);
  }
end;

{$O+}

function TCode.GetRecord(I: Integer): TCodeRec;
begin
  result := TCodeRec(A[I - 1]);
end;
{$O-}

procedure TCode.SetRecord(I: Integer; value: TCodeRec);
begin
  A[I - 1] := value;
end;

procedure TCode.DeleteRecord(I: Integer);
begin
{$IFDEF ARC}
  Records[I] := nil;
{$ELSE}
  Records[I].Free;
{$ENDIF}
  A.Delete(I - 1);
end;

function TCode.CreateConst(TypeId: Integer; const value: Variant;
  ValueType: Integer = 0): Integer;
var
  FT: Integer;
  S: String;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  FT := SymbolTable[TypeId].FinalTypeId;

  result := 0;
  case FT of
    typeBOOLEAN:
      result := SymbolTable.AddBooleanConst(value).Id;
    typeBYTEBOOL:
      if TVarData(value).VInteger <> 0 then
        result := SymbolTable.AddByteBoolConst(true).Id
      else
        result := SymbolTable.AddByteBoolConst(false).Id;
    typeWORDBOOL:
      if TVarData(value).VInteger <> 0 then
        result := SymbolTable.AddWordBoolConst(true).Id
      else
        result := SymbolTable.AddWordBoolConst(false).Id;
    typeLONGBOOL:
      if TVarData(value).VInteger <> 0 then
        result := SymbolTable.AddLongBoolConst(true).Id
      else
        result := SymbolTable.AddLongBoolConst(false).Id;
    typeINTEGER:
      result := SymbolTable.AddIntegerConst(value).Id;
    typePOINTER:
      result := SymbolTable.AddPointerConst(TypeId, Pointer(Integer(value))).Id;
    typeSMALLINT:
      result := SymbolTable.AddSmallIntConst(value).Id;
    typeSHORTINT:
      result := SymbolTable.AddShortIntConst(value).Id;
    typeBYTE:
      result := SymbolTable.AddByteConst(value).Id;
    typeWORD:
      result := SymbolTable.AddWordConst(value).Id;
    typeCARDINAL:
      result := SymbolTable.AddCardinalConst(value).Id;
    typeDOUBLE:
      result := SymbolTable.AddDoubleConst(value).Id;
    typeCURRENCY:
      result := SymbolTable.AddCurrencyConst(value).Id;
    typeSINGLE:
      result := SymbolTable.AddSingleConst(value).Id;
    typeEXTENDED:
      result := SymbolTable.AddExtendedConst(value).Id;
{$IFNDEF PAXARM}
    typeANSISTRING, typeWIDESTRING, typeSHORTSTRING:
      begin
        if VarType(value) in [varByte, varInteger] then
        begin
          S := Chr(Integer(value));
          result := SymbolTable.AddPAnsiCharConst(AnsiString(S)).Id;
        end
        else
          result := SymbolTable.AddPAnsiCharConst(AnsiString(value)).Id;
      end;
{$ENDIF}
    typeUNICSTRING:
      begin
        if VarType(value) in [varByte, varInteger] then
        begin
          S := Chr(Integer(value));
          result := SymbolTable.AddPWideCharConst(S).Id;
        end
        else
          result := SymbolTable.AddPWideCharConst(value).Id;
      end;
{$IFNDEF PAXARM}
    typeANSICHAR:
      result := SymbolTable.AddAnsiCharConst(AnsiChar(Chr(Integer(value)))).Id;
{$ENDIF}
    typeWIDECHAR:
      result := SymbolTable.AddWideCharConst(Integer(value)).Id;
{$IFDEF VARIANTS}
    typeINT64:
      result := SymbolTable.AddInt64Const(value).Id;
    typeUINT64:
      result := SymbolTable.AddUInt64Const(value).Id;
{$ELSE}
    typeINT64:
      result := SymbolTable.AddInt64Const(Integer(value)).Id;
    typeUINT64:
      result := SymbolTable.AddUInt64Const(Integer(value)).Id;
{$ENDIF}
    typeVARIANT:
      begin
        if ValueType = typeBOOLEAN then
        begin
          if Integer(value) = 0 then
            result := SymbolTable.AddVariantConst(false).Id
          else
            result := SymbolTable.AddVariantConst(true).Id
        end
        else
          result := SymbolTable.AddVariantConst(value).Id;
      end;
    typeOLEVARIANT:
      begin
        if ValueType = typeBOOLEAN then
        begin
          if Integer(value) = 0 then
            result := SymbolTable.AddOleVariantConst(false).Id
          else
            result := SymbolTable.AddOleVariantConst(true).Id
        end
        else
          result := SymbolTable.AddOleVariantConst(value).Id;
      end;
  else
    begin
      if FT = typeENUM then
        result := SymbolTable.AddEnumConst(TypeId, value).Id
      else if FT = typeSET then
        result := SymbolTable.AddSetConst(H_TByteSet, value).Id
      else if FT = typeCLASS then
      begin
        if Integer(value) = 0 then
          result := SymbolTable.NilId
        else
          result := SymbolTable.AddClassConst(TypeId,
            TObject(Integer(value))).Id;
      end
      else
      begin
        if FT in OrdinalTypes then
          result := CreateConst(FT, value)
        else
          RaiseError(errInternalError, []);
      end;
    end;
  end;
end;

function TCode.NewTempVar(Level, TypeId: Integer): Integer;
var
  R: TSymbolRec;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  R := TKernel(kernel).SymbolTable.AddRecord;
  R.Kind := KindVAR;
  R.Level := Level;
  R.TypeId := TypeId;
  result := R.Id;
end;

function TCode.NewField(const FName: String; TypeId, OwnerId: Integer): Integer;
begin
  result := NewTempVar(GetSymbolRec(OwnerId).Level, TypeId);
  GetSymbolRec(result).OwnerId := OwnerId;
  GetSymbolRec(result).Name := FName;
end;

{$IFNDEF PAXARM}

function TCode.CreateAnsiCharVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddAnsiCharVar(Level).Id;
end;
{$ENDIF}

function TCode.CreateWideCharVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddWideCharVar(Level).Id;
end;

function TCode.CreatePointerVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddPointerVar(Level).Id;
end;

function TCode.CreateIntegerVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddIntegerVar(Level).Id;
end;

function TCode.CreateCardinalVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddCardinalVar(Level).Id;
end;

function TCode.CreateSmallIntVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddSmallIntVar(Level).Id;
end;

function TCode.CreateShortIntVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddShortIntVar(Level).Id;
end;

function TCode.CreateByteVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddByteVar(Level).Id;
end;

function TCode.CreateWordVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddWordVar(Level).Id;
end;

function TCode.CreateBooleanVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddBooleanVar(Level).Id;
end;

function TCode.CreateByteBoolVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddByteBoolVar(Level).Id;
end;

function TCode.CreateWordBoolVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddWordBoolVar(Level).Id;
end;

function TCode.CreateLongBoolVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddLongBoolVar(Level).Id;
end;

function TCode.CreateDoubleVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddDoubleVar(Level).Id;
end;

function TCode.CreateCurrencyVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddCurrencyVar(Level).Id;
end;

function TCode.CreateSingleVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddSingleVar(Level).Id;
end;

function TCode.CreateExtendedVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddExtendedVar(Level).Id;
end;

function TCode.CreateInt64Var(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddInt64Var(Level).Id;
end;

function TCode.CreateUInt64Var(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddUInt64Var(Level).Id;
end;

{$IFNDEF PAXARM}

function TCode.CreateStringVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddStringVar(Level).Id;
end;

function TCode.CreateWideStringVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddWideStringVar(Level).Id;
end;

function TCode.CreateShortStringVar(Level, TypeId: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddShortStringVar(Level, TypeId).Id;
end;
{$ENDIF}

function TCode.CreateInterfaceVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddInterfaceVar(Level).Id;
end;

function TCode.CreateClassVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddClassVar(Level).Id;
end;

function TCode.CreateUnicStringVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddUnicStringVar(Level).Id;
end;

function TCode.CreateRecordVar(Level, TypeId: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddRecordVar(Level, TypeId).Id;
end;

function TCode.CreateDynarrayVar(Level, TypeId: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddDynarrayVar(Level, TypeId).Id;
end;

function TCode.CreateVariantVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddVariantVar(Level).Id;
end;

function TCode.CreateOleVariantVar(Level: Integer): Integer;
begin
  if CRT_JS_FUNC_OBJECT_SWITCH then
    Level := JS_TempNamespaceId;

  result := TKernel(kernel).SymbolTable.AddOleVariantVar(Level).Id;
end;

function TCode.InsertDestroyLocalVar(I: Integer; Id: Integer): TCodeRec;
begin
  result := TCodeRec.Create(OP_DESTROY_LOCAL_VAR, Self);
  result.Arg1 := Id;
  Insert(I, result);
end;

{$IFNDEF PAXARM}

function TCode.InsertConversionToAnsiChar(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_ANSICHAR_FROM_VARIANT
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeANSICHAR);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateAnsiCharVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;
{$ENDIF}

function TCode.InsertConversionToWideChar(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_WIDECHAR_FROM_VARIANT
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeWIDECHAR);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateWideCharVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToInteger(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_INT_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_INT_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeINTEGER);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateIntegerVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToCardinal(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_CARDINAL_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_CARDINAL_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeCARDINAL);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateCardinalVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToSmallInt(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_SMALLINT_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_SMALLINT_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeSMALLINT);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateSmallIntVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToShortInt(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_SHORTINT_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_SHORTINT_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeSHORTINT);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateShortIntVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToByte(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_BYTE_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_BYTE_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeBYTE);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateByteVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToWord(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_WORD_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_WORD_FROM_INT64
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeWORD);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateWordVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToInterface(I: Integer; Arg: Integer;
  InterfaceTypeId: Integer): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg2 := R.Arg1
  else if Arg = 2 then
    RC.Arg2 := R.Arg2
  else if Arg = 3 then
    RC.Arg2 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg2).FinalTypeId;
  if T = typeCLASS then
    RC.Op := OP_INTERFACE_FROM_CLASS
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg2).TerminalTypeId,
      GetSymbolRec(RC.Arg2).TerminalTypeId);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Res := NewTempVar(Level, InterfaceTypeId);
  RC.Arg1 := RC.Res;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := Level;
  RD.Arg2 := RC.Res;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);
  Inc(N);

  Insert(I, RC);
end;

function TCode.InsertConversionToClass(I: Integer; Arg: Integer;
  ClassTypeId: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_CLASS_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      ClassTypeId);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);
  RC.Arg2 := RC.Arg1;
  RC.Res := CreateClassVar(Level);
  GetSymbolRec(RC.Res).TypeId := ClassTypeId;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToFrameworkClass(I: Integer; Arg: Integer;
  ClassTypeId: Integer): TCodeRec;
var
  R, RC: TCodeRec;
begin
  R := Records[I];

  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  RC.Op := OP_TO_FW_OBJECT;

  RC.Arg2 := GetSymbolRec(RC.Arg1).TerminalTypeId;
  RC.Res := CreateClassVar(GetLevel(N));
  GetSymbolRec(RC.Res).TypeId := ClassTypeId;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToBoolean(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_BOOL_FROM_VARIANT
  else if T = typeBYTEBOOL then
    RC.Op := OP_BOOL_FROM_BYTEBOOL
  else if T = typeWORDBOOL then
    RC.Op := OP_BOOL_FROM_WORDBOOL
  else if T = typeLONGBOOL then
    RC.Op := OP_BOOL_FROM_LONGBOOL
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeBOOLEAN);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);
  RC.Arg2 := RC.Arg1;
  RC.Res := CreateBooleanVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToByteBool(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_BYTEBOOL_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeBYTEBOOL);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateByteBoolVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToWordBool(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_WORDBOOL_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeWORDBOOL);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateWordBoolVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToLongBool(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);
  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T in VariantTypes then
    RC.Op := OP_LONGBOOL_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeLONGBOOL);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateLongBoolVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToDouble(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T = typeINT64 then
    RC.Op := OP_INT64_TO_DOUBLE
  else if T = typeUINT64 then
    RC.Op := OP_UINT64_TO_DOUBLE
  else if T in IntegerTypes then
    RC.Op := OP_INT_TO_DOUBLE
  else if T = typeSINGLE then
    RC.Op := OP_SINGLE_TO_DOUBLE
  else if T = typeCURRENCY then
    RC.Op := OP_CURRENCY_TO_DOUBLE
  else if T = typeEXTENDED then
    RC.Op := OP_EXTENDED_TO_DOUBLE
  else if T in VariantTypes then
    RC.Op := OP_DOUBLE_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeDOUBLE);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateDoubleVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToCurrency(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg2 := R.Arg1
  else if Arg = 2 then
    RC.Arg2 := R.Arg2
  else if Arg = 3 then
    RC.Arg2 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg2).FinalTypeId;
  if T = typeINT64 then
    RC.Op := OP_CURRENCY_FROM_INT64
  else if T in IntegerTypes then
    RC.Op := OP_CURRENCY_FROM_INT
  else if T in RealTypes then
    RC.Op := OP_CURRENCY_FROM_REAL
  else if T in VariantTypes then
    RC.Op := OP_CURRENCY_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeCURRENCY);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Res := CreateCurrencyVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToSingle(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;

  if T = typeINT64 then
    RC.Op := OP_INT64_TO_SINGLE
  else if T = typeUINT64 then
    RC.Op := OP_UINT64_TO_SINGLE
  else if T in IntegerTypes then
    RC.Op := OP_INT_TO_SINGLE
  else if T = typeDOUBLE then
    RC.Op := OP_DOUBLE_TO_SINGLE
  else if T = typeCURRENCY then
    RC.Op := OP_CURRENCY_TO_SINGLE
  else if T = typeEXTENDED then
    RC.Op := OP_EXTENDED_TO_SINGLE
  else if T in VariantTypes then
    RC.Op := OP_SINGLE_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeSINGLE);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateSingleVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToExtended(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
  E: Extended;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T = typeINT64 then
    RC.Op := OP_INT64_TO_EXTENDED
  else if T = typeUINT64 then
    RC.Op := OP_UINT64_TO_EXTENDED
  else if T in IntegerTypes then
  begin
    if GetSymbolRec(RC.Arg1).Kind = KindCONST then
    begin
      RC.Op := OP_NOP;
      E := GetSymbolRec(RC.Arg1).value;
      RC.Res := TKernel(kernel).SymbolTable.AddExtendedConst(E).Id;
      if Arg = 1 then
        R.Arg1 := RC.Res
      else if Arg = 3 then
        R.Res := RC.Res
      else
        R.Arg2 := RC.Res;
      Insert(I, RC);
      Exit;
    end;

    RC.Op := OP_INT_TO_EXTENDED;
  end
  else if T = typeDOUBLE then
    RC.Op := OP_DOUBLE_TO_EXTENDED
  else if T = typeCURRENCY then
    RC.Op := OP_CURRENCY_TO_EXTENDED
  else if T = typeSINGLE then
    RC.Op := OP_SINGLE_TO_EXTENDED
  else if T in VariantTypes then
    RC.Op := OP_EXTENDED_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeEXTENDED);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateExtendedVar(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToInt64(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;

  if T = typeUINT64 then
  begin
    FreeAndNil(RC);
    Dec(N);
    Exit;
  end;

  if T = typeINTEGER then
    RC.Op := OP_INT_TO_INT64
  else if T = typeBYTE then
    RC.Op := OP_BYTE_TO_INT64
  else if T = typeWORD then
    RC.Op := OP_WORD_TO_INT64
  else if T = typeCARDINAL then
    RC.Op := OP_CARDINAL_TO_INT64
  else if T = typeSMALLINT then
    RC.Op := OP_SMALLINT_TO_INT64
  else if T = typeSHORTINT then
    RC.Op := OP_SHORTINT_TO_INT64
  else if T in VariantTypes then
    RC.Op := OP_INT64_FROM_VARIANT
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeINT64);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateInt64Var(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToUInt64(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  T: Integer;
begin
  R := Records[I];
  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
    RC.Arg1 := R.Arg1
  else if Arg = 2 then
    RC.Arg1 := R.Arg2
  else if Arg = 3 then
    RC.Arg1 := R.Res
  else
    RaiseError(errInternalError, []);

  T := GetSymbolRec(RC.Arg1).FinalTypeId;
  if T = typeINT64 then
  begin
    FreeAndNil(RC);
    Dec(N);
    Exit;
  end;

  if T = typeINTEGER then
    RC.Op := OP_INT_TO_UINT64
  else if T = typeBYTE then
    RC.Op := OP_BYTE_TO_UINT64
  else if T = typeWORD then
    RC.Op := OP_WORD_TO_UINT64
  else if T = typeCARDINAL then
    RC.Op := OP_CARDINAL_TO_UINT64
  else if T = typeSMALLINT then
    RC.Op := OP_SMALLINT_TO_UINT64
  else if T = typeSHORTINT then
    RC.Op := OP_SHORTINT_TO_UINT64
  else if T in VariantTypes then
    RC.Op := OP_UINT64_FROM_VARIANT
  else if T = typeINT64 then
    RC.Op := OP_ASSIGN_INT64
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, GetSymbolRec(RC.Arg1).TerminalTypeId,
      typeUINT64);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    Exit;
  end
  else
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC.Arg2 := RC.Arg1;
  RC.Res := CreateUInt64Var(Level);

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  Insert(I, RC);
end;

function TCode.FindImplicitConversion(RecordTypeId, ParamArg,
  ResultArg: Integer): Integer;
var
  L: TIntegerList;
  SymbolTable: TSymbolTable;
  I, SubId, ResultId, T: Integer;
begin
  result := 0;
  SymbolTable := TKernel(kernel).SymbolTable;
  L := SymbolTable.LookUpAll(gen_Implicit, RecordTypeId, GetUpcase(N));
  try
    if L.Count = 0 then
      Exit;

    T := GetSymbolRec(ParamArg).TerminalTypeId;
    if RecordTypeId = T then
    begin
      for I := 0 to L.Count - 1 do
      begin
        SubId := L[I];
        ResultId := SymbolTable.GetResultId(SubId);
        if ExistsImplicitConversion(ResultId, ResultArg) then
        begin
          result := SubId;
          Exit;
        end;
      end;
    end
    else
    begin
      T := GetSymbolRec(ResultArg).TerminalTypeId;
      if RecordTypeId = T then
      begin
        for I := 0 to L.Count - 1 do
        begin
          SubId := L[I];
          ResultId := SymbolTable.GetParamId(SubId, 0);
          if ExistsImplicitConversion(ResultId, ParamArg) then
          begin
            result := SubId;
            Exit;
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(L);
  end;
end;

function TCode.FindExplicitConversion(RecordTypeId, ParamArg,
  ResultArg: Integer): Integer;
var
  L: TIntegerList;
  SymbolTable: TSymbolTable;
  I, SubId, ResultId, T: Integer;
begin
  result := 0;
  SymbolTable := TKernel(kernel).SymbolTable;
  L := SymbolTable.LookUpAll(gen_Explicit, RecordTypeId, GetUpcase(N));
  try
    if L.Count = 0 then
      Exit;

    T := GetSymbolRec(ParamArg).TerminalTypeId;
    if RecordTypeId = T then
    begin
      for I := 0 to L.Count - 1 do
      begin
        SubId := L[I];
        ResultId := SymbolTable.GetResultId(SubId);
        if ExistsImplicitConversion(ResultId, ResultArg) then
        begin
          result := SubId;
          Exit;
        end;
      end;
    end
    else
    begin
      T := GetSymbolRec(ResultArg).TerminalTypeId;
      if RecordTypeId = T then
      begin
        for I := 0 to L.Count - 1 do
        begin
          SubId := L[I];
          ResultId := SymbolTable.GetParamId(SubId, 0);
          if ExistsImplicitConversion(ResultId, ParamArg) then
          begin
            result := SubId;
            Exit;
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(L);
  end;
end;

function TCode.InsertConversionToRecord(I, Arg, RecordTypeId, ValueId: Integer)
  : TCodeRec;
var
  R, RC: TCodeRec;
  Level, SubId: Integer;
  S: String;
begin
  R := Records[I];

  S := gen_Implicit;
  SubId := TKernel(kernel).SymbolTable.LookUp(S, RecordTypeId, GetUpcase(N));
  if SubId = 0 then
    RaiseError(errIncompatibleTypesNoArgs, []);

  Level := GetLevel(N);

  RC := TCodeRec.Create(OP_PUSH, Self);
  RC.Arg1 := ValueId;
  RC.Arg2 := 0;
  RC.Res := SubId;
  Insert(N, RC);
  Inc(N);

  RC := TCodeRec.Create(OP_CALL, Self);
  RC.Arg1 := SubId;
  RC.Arg2 := 1;
  RC.Res := CreateRecordVar(Level, RecordTypeId);
  Insert(N, RC);
  Inc(N);

  case Arg of
    1:
      R.Arg1 := RC.Res;
    2:
      R.Arg2 := RC.Res;
    3:
      R.Res := RC.Res;
  end;

  Dec(N, 3);

  result := RC;
end;

function TCode.ExistsBinaryOperator(RecordTypeId: Integer;
  const OperatorName: String): Boolean;
var
  R: TCodeRec;
  SubId: Integer;
begin
  result := false;
  R := Records[N];
  SubId := TKernel(kernel).SymbolTable.LookUp(OperatorName, RecordTypeId,
    GetUpcase(N));
  if SubId = 0 then
  begin
    if GetSymbolRec(R.Arg1).TerminalTypeId = GetSymbolRec(R.Arg2).TerminalTypeId
    then
      if (OperatorName = gen_Equal) or (OperatorName = gen_NotEqual) then
        result := true;
  end
  else
    result := true;
end;

procedure TCode.InsertBinaryOperator(RecordTypeId: Integer;
  const OperatorName: String);
var
  R, RC: TCodeRec;
  Level, SubId, Res: Integer;
begin
  R := Records[N];
  Res := R.Res;

  SubId := TKernel(kernel).SymbolTable.LookUp(OperatorName, RecordTypeId,
    GetUpcase(N));
  if SubId = 0 then
  begin
    if GetSymbolRec(R.Arg1).TerminalTypeId = GetSymbolRec(R.Arg2).TerminalTypeId
    then
    begin

      if OperatorName = gen_Equal then
      begin
        R.Op := OP_EQ_STRUCT;
        Dec(N);
        Exit;
      end;

      if OperatorName = gen_NotEqual then
      begin
        R.Op := OP_NE_STRUCT;
        Dec(N);
        Exit;
      end;

    end;

    RaiseError(errE2015, []);
    // Operator not applicable to this operand type
  end;

  Level := GetLevel(N);

  RC := TCodeRec.Create(OP_PUSH, Self);
  RC.Arg1 := R.Arg1;
  RC.Arg2 := 0;
  RC.Res := SubId;
  Insert(N, RC);
  Inc(N);

  RC := TCodeRec.Create(OP_PUSH, Self);
  RC.Arg1 := R.Arg2;
  RC.Arg2 := 1;
  RC.Res := SubId;
  Insert(N, RC);
  Inc(N);

  R.Op := OP_CALL;
  R.Arg1 := SubId;
  R.Arg2 := 2;
  R.Res := CreateRecordVar(Level, RecordTypeId);

  ReplaceId(Res, R.Res);

  Dec(N, 4);
end;

procedure TCode.InsertUnaryOperator(RecordTypeId: Integer;
  const OperatorName: String);
var
  R, RC: TCodeRec;
  Level, SubId, Res: Integer;
begin
  R := Records[N];
  Res := R.Res;

  SubId := TKernel(kernel).SymbolTable.LookUp(OperatorName, RecordTypeId,
    GetUpcase(N));
  if SubId = 0 then
    RaiseError(errE2015, []);
  // Operator not applicable to this operand type

  Level := GetLevel(N);

  RC := TCodeRec.Create(OP_PUSH, Self);
  RC.Arg1 := R.Arg1;
  RC.Arg2 := 0;
  RC.Res := SubId;
  Insert(N, RC);
  Inc(N);

  R.Op := OP_CALL;
  R.Arg1 := SubId;
  R.Arg2 := 1;
  R.Res := CreateRecordVar(Level, RecordTypeId);

  ReplaceId(Res, R.Res);

  Dec(N, 3);
end;

{$IFNDEF PAXARM}

function TCode.InsertConversionToAnsiString(I: Integer; Arg: Integer;
  Lang: Integer = PASCAL_LANGUAGE): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T: Integer;
  S: String;
begin
  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if T = typeWIDECHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_ANSISTRING_FROM_PANSICHAR;
    end
    else
      RC.Op := OP_ANSISTRING_FROM_WIDECHAR;
  end
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_ANSISTRING_FROM_PANSICHAR;
    end
    else
      RC.Op := OP_ANSISTRING_FROM_ANSICHAR;
  end
  else if T = typeWIDESTRING then
    RC.Op := OP_ANSISTRING_FROM_WIDESTRING
  else if T = typeSHORTSTRING then
    RC.Op := OP_ANSISTRING_FROM_SHORTSTRING
  else if T = typeUNICSTRING then
    RC.Op := OP_ANSISTRING_FROM_UNICSTRING
  else if T in VariantTypes then
    RC.Op := OP_ANSISTRING_FROM_VARIANT
  else if SymbolTable[Id].HasPAnsiCharType then
    RC.Op := OP_ANSISTRING_FROM_PANSICHAR
  else if SymbolTable[Id].HasPWideCharType then
    RC.Op := OP_ANSISTRING_FROM_PWIDECHAR
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeANSISTRING);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin
    if Lang = JS_LANGUAGE then
    begin
      if T in IntegerTypes then
        RC.Op := OP_ANSISTRING_FROM_INT
      else if T = typeDOUBLE then
        RC.Op := OP_ANSISTRING_FROM_DOUBLE
      else if T = typeSINGLE then
        RC.Op := OP_ANSISTRING_FROM_SINGLE
      else if T = typeEXTENDED then
        RC.Op := OP_ANSISTRING_FROM_EXTENDED
      else if T = typeBOOLEAN then
        RC.Op := OP_ANSISTRING_FROM_BOOLEAN
      else
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        FreeAndNil(RC);
        result := nil;
        Exit;
      end;
    end
    else
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end;

  Level := GetLevel(N);

  RC.Arg1 := CreateStringVar(Level);
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := Level;
  RD.Arg2 := RC.Res;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);
  Inc(N);

  Insert(I, RC);
end;

function TCode.InsertConversionToShortString(I: Integer; Arg: Integer)
  : TCodeRec;
var
  R, RC: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T, L, TypeId: Integer;
  S: String;
begin
  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if SymbolTable[Id].HasPAnsiCharType then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_SHORTSTRING_FROM_PANSICHAR_LITERAL
    else
    begin
      CreateError(errInternalError, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
    end
    else
      RC.Op := OP_SHORTSTRING_FROM_ANSICHAR;
  end
  else if SymbolTable[Id].HasPWideCharType then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL
    else
    begin
      CreateError(errInternalError, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end
  else if T = typeANSISTRING then
  begin
    RC.Op := OP_SHORTSTRING_FROM_ANSISTRING;
  end
  else if T = typeWIDESTRING then
  begin
    RC.Op := OP_SHORTSTRING_FROM_WIDESTRING;
  end
  else if T = typeUNICSTRING then
  begin
    RC.Op := OP_SHORTSTRING_FROM_UNICSTRING;
  end
  else if T in VariantTypes then
  begin
    RC.Op := OP_SHORTSTRING_FROM_VARIANT;
  end
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeSHORTSTRING);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  Level := GetLevel(N);

  L := 255;
  TypeId := SymbolTable.RegisterShortStringType(0, '', L);

  RC.Arg1 := CreateShortStringVar(Level, TypeId);
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 2 then
    R.Arg2 := RC.Res
  else
    R.Res := RC.Res;

  Insert(I, RC);
end;

function TCode.InsertConversionToWideString(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T: Integer;
  S: String;
  TempVarId: Integer;
  Found: Boolean;
begin
  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if T = typeWIDECHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_WIDESTRING_FROM_WIDECHAR_LITERAL
    else
      RC.Op := OP_WIDESTRING_FROM_WIDECHAR;
  end
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_WIDESTRING_FROM_PANSICHAR_LITERAL;
    end
    else
      RC.Op := OP_WIDESTRING_FROM_ANSICHAR;
  end
  else if T = typeSHORTSTRING then
    RC.Op := OP_WIDESTRING_FROM_SHORTSTRING
  else if T = typeUNICSTRING then
    RC.Op := OP_WIDESTRING_FROM_UNICSTRING
  else if T = typeANSISTRING then
    RC.Op := OP_WIDESTRING_FROM_ANSISTRING
  else if T in VariantTypes then
    RC.Op := OP_WIDESTRING_FROM_VARIANT
  else if SymbolTable[Id].HasPAnsiCharType then
    RC.Op := OP_WIDESTRING_FROM_PANSICHAR_LITERAL
  else if SymbolTable[Id].HasPWideCharType then
    RC.Op := OP_WIDESTRING_FROM_PWIDECHAR_LITERAL
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeWIDESTRING);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  Level := GetLevel(N);

  TempVarId := LookupTempVarId(Level, typeWIDESTRING);

  if TempVarId = 0 then
  begin
    Found := false;
    TempVarId := CreateWideStringVar(Level);
    temp_var_list.Add(TempVarId, GetStmt(N));
  end
  else
    Found := true;

  RC.Arg1 := TempVarId;
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  if not Found then
  begin
    RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RD.Arg1 := Level;
    RD.Arg2 := RC.Res;
    RD.Res := 0;
    Insert(I, RD);
    Inc(I);
    Inc(N);
  end;

  Insert(I, RC);
end;
{$ENDIF}

function TCode.InsertImplicitConversion(I, Arg, RecordTypeId,
  ResultTypeId: Integer): TCodeRec;
var
  Id: Integer;
  R, RD: TCodeRec;
  TempId, TempResId, SubId, T2, OldN, OldCard: Integer;
  SymbolTable: TSymbolTable;
begin
  result := nil;

  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  if Arg = 1 then
    Id := R.Arg1
  else if Arg = 2 then
    Id := R.Arg2
  else
    Id := R.Res;

  OldN := N;
  OldCard := Card;

  TempId := NewTempVar(GetLevel(N), ResultTypeId);
  SubId := FindImplicitConversion(RecordTypeId, Id, TempId);

  if SubId = 0 then
  begin
    RaiseError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  T2 := SymbolTable.GetResultId(SubId);
  T2 := GetSymbolRec(T2).TypeId;

  TempResId := NewTempVar(GetLevel(N), T2);

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := GetLevel(N);
  RD.Arg2 := TempId;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := GetLevel(N);
  RD.Arg2 := TempResId;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_PUSH, Self);
  RD.Arg1 := Id;
  RD.Arg2 := 0;
  RD.Res := SubId;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_CALL, Self);
  RD.Arg1 := SubId;
  RD.Arg2 := 1;
  RD.Res := TempResId;
  Insert(I, RD);

  N := I;
  NoOverloadSearch := true;
  OperCall;
  Inc(N);

  RD := TCodeRec.Create(OP_ASSIGN, Self);
  RD.Arg1 := TempId;
  RD.Arg2 := TempResId;
  RD.Res := TempId;
  Insert(N, RD);

  result := RD;

  OperAssignment;

  if Arg = 1 then
    R.Arg1 := TempId
  else if Arg = 2 then
    R.Arg2 := TempId
  else if Arg = 3 then
    R.Res := TempId;

  N := OldN + (Card - OldCard);
end;

function TCode.InsertExplicitConversion(I, RecordTypeId, ResultTypeId: Integer)
  : TCodeRec;
var
  Id: Integer;
  R, RD: TCodeRec;
  TempId, TempResId, SubId, T2, OldN, OldCard: Integer;
  SymbolTable: TSymbolTable;
begin
  result := nil;

  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  Id := R.Arg2;

  OldN := N;
  OldCard := Card;

  TempId := NewTempVar(GetLevel(N), ResultTypeId);

  SubId := FindExplicitConversion(RecordTypeId, Id, TempId);
  if SubId = 0 then
    SubId := FindImplicitConversion(RecordTypeId, Id, TempId);

  if SubId = 0 then
  begin
    RaiseError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  T2 := SymbolTable.GetResultId(SubId);
  T2 := GetSymbolRec(T2).TypeId;

  TempResId := NewTempVar(GetLevel(N), T2);

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := GetLevel(N);
  RD.Arg2 := TempId;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RD.Arg1 := GetLevel(N);
  RD.Arg2 := TempResId;
  RD.Res := 0;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_PUSH, Self);
  RD.Arg1 := Id;
  RD.Arg2 := 0;
  RD.Res := SubId;
  Insert(I, RD);
  Inc(I);

  RD := TCodeRec.Create(OP_CALL, Self);
  RD.Arg1 := SubId;
  RD.Arg2 := 1;
  RD.Res := TempResId;
  Insert(I, RD);

  N := I;
  NoOverloadSearch := true;
  OperCall;
  Inc(N);

  RD := TCodeRec.Create(OP_ASSIGN, Self);
  RD.Arg1 := TempId;
  RD.Arg2 := TempResId;
  RD.Res := TempId;
  Insert(N, RD);

  result := RD;

  OperAssignment;

  R.Arg2 := TempId;

  N := OldN + (Card - OldCard);
end;

function TCode.InsertConversionToUnicString(I: Integer; Arg: Integer;
  Lang: Integer = PASCAL_LANGUAGE): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T, TempVarId: Integer;
{$IFNDEF PAXARM}
  S: String;
{$ENDIF}
  Found: Boolean;
begin
  R := Records[I];
  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if T = typeWIDECHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_UNICSTRING_FROM_WIDECHAR_LITERAL
    else
      RC.Op := OP_UNICSTRING_FROM_WIDECHAR;
  end
{$IFNDEF PAXARM}
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_UNICSTRING_FROM_PANSICHAR_LITERAL;
    end
    else
      RC.Op := OP_UNICSTRING_FROM_ANSICHAR;
  end
  else if T = typeWIDESTRING then
    RC.Op := OP_UNICSTRING_FROM_WIDESTRING
  else if T = typeSHORTSTRING then
    RC.Op := OP_UNICSTRING_FROM_SHORTSTRING
  else if T = typeANSISTRING then
    RC.Op := OP_UNICSTRING_FROM_ANSISTRING
  else if SymbolTable[Id].HasPAnsiCharType then
    RC.Op := OP_UNICSTRING_FROM_PANSICHAR_LITERAL
{$ENDIF}
  else if T in VariantTypes then
    RC.Op := OP_UNICSTRING_FROM_VARIANT
  else if SymbolTable[Id].HasPWideCharType then
    RC.Op := OP_UNICSTRING_FROM_PWIDECHAR_LITERAL
  else if T = typeRECORD then
  begin
    FreeAndNil(RC);
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeUNICSTRING);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin
    if Lang = JS_LANGUAGE then
    begin
      if T in IntegerTypes then
        RC.Op := OP_UNICSTRING_FROM_INT
      else if T = typeDOUBLE then
        RC.Op := OP_UNICSTRING_FROM_DOUBLE
      else if T = typeSINGLE then
        RC.Op := OP_UNICSTRING_FROM_SINGLE
      else if T = typeEXTENDED then
        RC.Op := OP_UNICSTRING_FROM_EXTENDED
      else if T = typeBOOLEAN then
        RC.Op := OP_UNICSTRING_FROM_BOOLEAN
      else
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        FreeAndNil(RC);
        result := nil;
        Exit;
      end;
    end
    else
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end;

  Level := GetLevel(N);

  TempVarId := LookupTempVarId(Level, typeUNICSTRING);

  if TempVarId = 0 then
  begin
    Found := false;
    TempVarId := CreateUnicStringVar(Level);
    temp_var_list.Add(TempVarId, GetStmt(N));
  end
  else
    Found := true;

  RC.Arg1 := TempVarId;
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  if not Found then
  begin
    RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RD.Arg1 := Level;
    RD.Arg2 := RC.Res;
    RD.Res := 0;
    Insert(I, RD);
    Inc(I);
    Inc(N);
  end;

  Insert(I, RC);
end;

function TCode.InsertConversionToVariant(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T: Integer;
{$IFNDEF PAXARM}
  S: String;
{$ENDIF}
  Lang, TempVarId: Integer;
  Found: Boolean;
begin
  R := Records[I];
  Lang := R.Language;

  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if T = typeWIDECHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_VARIANT_FROM_WIDECHAR_LITERAL
    else
      RC.Op := OP_VARIANT_FROM_WIDECHAR;
  end
{$IFNDEF PAXARM}
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_VARIANT_FROM_PANSICHAR_LITERAL;
    end
    else
      RC.Op := OP_VARIANT_FROM_ANSICHAR;
  end
{$ENDIF}
  else if T in IntegerTypes then
  begin
    if GetSymbolRec(Id).Kind = KindCONST then
    begin
      RC.Op := OP_NOP;
      RC.Res := TKernel(kernel).SymbolTable.AddVariantConst
        (GetSymbolRec(Id).value).Id;
      case Arg of
        1:
          R.Arg1 := RC.Res;
        2:
          R.Arg2 := RC.Res;
        3:
          R.Res := RC.Res;
      end;
      Insert(I, RC);
      Exit;
    end;
    case T of
      typeINTEGER:
        RC.Op := OP_VARIANT_FROM_INT;
      typeINT64:
        RC.Op := OP_VARIANT_FROM_INT64;
      typeBYTE:
        RC.Op := OP_VARIANT_FROM_BYTE;
      typeWORD:
        RC.Op := OP_VARIANT_FROM_WORD;
      typeCARDINAL:
        RC.Op := OP_VARIANT_FROM_CARDINAL;
      typeSMALLINT:
        RC.Op := OP_VARIANT_FROM_SMALLINT;
      typeSHORTINT:
        RC.Op := OP_VARIANT_FROM_SHORTINT;
    end;
  end
  else if T = typeBOOLEAN then
  begin
    if GetSymbolRec(Id).Kind = KindCONST then
    begin
      RC.Op := OP_NOP;
      RC.Res := TKernel(kernel).SymbolTable.AddVariantConst
        (Boolean(GetSymbolRec(Id).value)).Id;
      case Arg of
        1:
          R.Arg1 := RC.Res;
        2:
          R.Arg2 := RC.Res;
        3:
          R.Res := RC.Res;
      end;
      Insert(I, RC);
      Exit;
    end;
    RC.Op := OP_VARIANT_FROM_BOOL;
  end
  else if T = typeDOUBLE then
    RC.Op := OP_VARIANT_FROM_DOUBLE
  else if T = typeCURRENCY then
    RC.Op := OP_VARIANT_FROM_CURRENCY
  else if T = typeSINGLE then
    RC.Op := OP_VARIANT_FROM_SINGLE
  else if T = typeEXTENDED then
    RC.Op := OP_VARIANT_FROM_EXTENDED
  else if T = typeINTERFACE then
    RC.Op := OP_VARIANT_FROM_INTERFACE
{$IFNDEF PAXARM}
  else if T = typeSHORTSTRING then
    RC.Op := OP_VARIANT_FROM_SHORTSTRING
  else if T = typeWIDESTRING then
    RC.Op := OP_VARIANT_FROM_WIDESTRING
  else if T = typeANSISTRING then
    RC.Op := OP_VARIANT_FROM_ANSISTRING
  else if SymbolTable[Id].HasPAnsiCharType then
    RC.Op := OP_VARIANT_FROM_PANSICHAR_LITERAL
{$ENDIF}
  else if T = typeUNICSTRING then
    RC.Op := OP_VARIANT_FROM_UNICSTRING
  else if SymbolTable[Id].HasPWideCharType then
    RC.Op := OP_VARIANT_FROM_PWIDECHAR_LITERAL
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeVARIANT);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin

    if (Lang = JS_LANGUAGE) or (Lang = BASIC_LANGUAGE) then
    begin

      if T = typeCLASS then
        RC.Op := OP_VARIANT_FROM_CLASS
      else if T = typePOINTER then
        RC.Op := OP_VARIANT_FROM_POINTER
      else
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        FreeAndNil(RC);
        Exit;
      end;

    end
    else
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end;

  Level := GetLevel(N);

  TempVarId := 0; // LookupTempVarId(Level, typeVARIANT);

  if TempVarId = 0 then
  begin
    Found := false;
    TempVarId := CreateVariantVar(Level);
    temp_var_list.Add(TempVarId, GetStmt(N));
  end
  else
    Found := true;

  RC.Arg1 := TempVarId;
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  if not Found then
  begin
    RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RD.Arg1 := Level;
    RD.Arg2 := RC.Res;
    RD.Res := 0;
    Insert(I, RD);
    Inc(I);
    Inc(N);
  end;

  Insert(I, RC);
end;

function TCode.InsertConversionToOleVariant(I: Integer; Arg: Integer): TCodeRec;
var
  R, RC, RD: TCodeRec;
  Level: Integer;
  SymbolTable: TSymbolTable;
  Id, T: Integer;
{$IFNDEF PAXARM}
  S: String;
{$ENDIF}
  Lang, TempVarId: Integer;
  Found: Boolean;
begin
  R := Records[I];
  Lang := R.Language;

  SymbolTable := TKernel(kernel).SymbolTable;

  RC := TCodeRec.Create(0, Self);

  result := RC;

  if Arg = 1 then
  begin
    RC.Arg2 := R.Arg1;
    Id := R.Arg1;
  end
  else if Arg = 2 then
  begin
    RC.Arg2 := R.Arg2;
    Id := R.Arg2;
  end
  else if Arg = 3 then
  begin
    RC.Arg2 := R.Res;
    Id := R.Res;
  end
  else
  begin
    CreateError(errInternalError, []);
    FreeAndNil(RC);
    result := nil;
    Exit;
  end;

  T := SymbolTable[Id].FinalTypeId;

  if T = typeWIDECHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
      RC.Op := OP_OLEVARIANT_FROM_WIDECHAR_LITERAL
    else
      RC.Op := OP_OLEVARIANT_FROM_WIDECHAR;
  end
{$IFNDEF PAXARM}
  else if T = typeANSICHAR then
  begin
    if SymbolTable[Id].Kind = KindCONST then
    begin
      S := Chr(Integer(SymbolTable[Id].value));
      RC.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
        (AnsiString(S)).Id;
      RC.Op := OP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
    end
    else
      RC.Op := OP_OLEVARIANT_FROM_ANSICHAR;
  end
{$ENDIF}
  else if T in IntegerTypes then
  begin
    if GetSymbolRec(Id).Kind = KindCONST then
    begin
      RC.Op := OP_NOP;
      RC.Res := TKernel(kernel).SymbolTable.AddVariantConst
        (GetSymbolRec(Id).value).Id;
      case Arg of
        1:
          R.Arg1 := RC.Res;
        2:
          R.Arg2 := RC.Res;
        3:
          R.Res := RC.Res;
      end;
      Insert(I, RC);
      Exit;
    end;
    case T of
      typeINTEGER:
        RC.Op := OP_OLEVARIANT_FROM_INT;
      typeINT64:
        RC.Op := OP_OLEVARIANT_FROM_INT64;
      typeBYTE:
        RC.Op := OP_OLEVARIANT_FROM_BYTE;
      typeWORD:
        RC.Op := OP_OLEVARIANT_FROM_WORD;
      typeCARDINAL:
        RC.Op := OP_OLEVARIANT_FROM_CARDINAL;
      typeSMALLINT:
        RC.Op := OP_OLEVARIANT_FROM_SMALLINT;
      typeSHORTINT:
        RC.Op := OP_OLEVARIANT_FROM_SHORTINT;
    end;
  end
  else if T = typeBOOLEAN then
  begin
    if GetSymbolRec(Id).Kind = KindCONST then
    begin
      RC.Op := OP_NOP;
      RC.Res := TKernel(kernel).SymbolTable.AddVariantConst
        (Boolean(GetSymbolRec(Id).value)).Id;
      case Arg of
        1:
          R.Arg1 := RC.Res;
        2:
          R.Arg2 := RC.Res;
        3:
          R.Res := RC.Res;
      end;
      Insert(I, RC);
      Exit;
    end;
    RC.Op := OP_OLEVARIANT_FROM_BOOL;
  end
  else if T = typeDOUBLE then
    RC.Op := OP_OLEVARIANT_FROM_DOUBLE
  else if T = typeCURRENCY then
    RC.Op := OP_OLEVARIANT_FROM_CURRENCY
  else if T = typeSINGLE then
    RC.Op := OP_OLEVARIANT_FROM_SINGLE
  else if T = typeEXTENDED then
    RC.Op := OP_OLEVARIANT_FROM_EXTENDED
  else if T = typeINTERFACE then
    RC.Op := OP_OLEVARIANT_FROM_INTERFACE
{$IFNDEF PAXARM}
  else if T = typeSHORTSTRING then
    RC.Op := OP_OLEVARIANT_FROM_SHORTSTRING
  else if T = typeWIDESTRING then
    RC.Op := OP_OLEVARIANT_FROM_WIDESTRING
  else if T = typeANSISTRING then
    RC.Op := OP_OLEVARIANT_FROM_ANSISTRING
  else if SymbolTable[Id].HasPAnsiCharType then
    RC.Op := OP_OLEVARIANT_FROM_PANSICHAR_LITERAL
{$ENDIF}
  else if T = typeUNICSTRING then
    RC.Op := OP_OLEVARIANT_FROM_UNICSTRING
  else if T = typeVARIANT then
    RC.Op := OP_OLEVARIANT_FROM_VARIANT
  else if SymbolTable[Id].HasPWideCharType then
    RC.Op := OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL
  else if T = typeRECORD then
  begin
    InsertImplicitConversion(I, Arg, SymbolTable[Id].TerminalTypeId,
      typeOLEVARIANT);
    FreeAndNil(RC);
    Dec(N); // will be increased by 1
    result := nil;
    Exit;
  end
  else
  begin

    if Lang = JS_LANGUAGE then
    begin

      if T = typeCLASS then
        RC.Op := OP_VARIANT_FROM_CLASS
      else if T = typePOINTER then
        RC.Op := OP_VARIANT_FROM_POINTER
      else
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        FreeAndNil(RC);
        result := nil;
        Exit;
      end;

    end
    else
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      FreeAndNil(RC);
      result := nil;
      Exit;
    end;
  end;

  Level := GetLevel(N);

  TempVarId := LookupTempVarId(Level, typeOLEVARIANT);

  if TempVarId = 0 then
  begin
    Found := false;
    TempVarId := CreateOleVariantVar(Level);
    temp_var_list.Add(TempVarId, GetStmt(N));
  end
  else
    Found := true;

  RC.Arg1 := TempVarId;
  RC.Res := RC.Arg1;

  if Arg = 1 then
    R.Arg1 := RC.Res
  else if Arg = 3 then
    R.Res := RC.Res
  else
    R.Arg2 := RC.Res;

  if not Found then
  begin
    RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RD.Arg1 := Level;
    RD.Arg2 := RC.Res;
    RD.Res := 0;
    Insert(I, RD);
    Inc(I);
    Inc(N);
  end;

  Insert(I, RC);
end;

procedure TCode.CompressListOfOverloaded(applicable_list, pos: TIntegerList);
var
  p_id, q_id, n_p, n_q, I, J, actual_id, p_formal_id, q_formal_id, val: Integer;
  cannot_compress: Boolean;
  SymbolTable: TSymbolTable;
  sub_id, NP, FinalTypeId, TypeId: Integer;
  ok: Boolean;
label
  cont;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  NP := pos.Count;

  if NP = 1 then
  begin
    actual_id := Records[pos[0]].Arg1;
    TypeId := GetSymbolRec(actual_id).TerminalTypeId;
    FinalTypeId := GetSymbolRec(actual_id).FinalTypeId;
    for I := applicable_list.Count - 1 downto 0 do
    begin
      sub_id := applicable_list[I];
      if GetSymbolRec(sub_id).Count <> 1 then
        continue;

      p_formal_id := SymbolTable.GetParamId(sub_id, 0);
      if GetSymbolRec(p_formal_id).FinalTypeId = FinalTypeId then
      begin
        if FinalTypeId = typeENUM then
          continue;
        if FinalTypeId = typeCLASS then
        begin
          if (GetSymbolRec(actual_id).IsFWArrayVar) and
            (GetSymbolRec(p_formal_id).TerminalTypeId = H_TFW_Array) then
          begin
            ok := true;
          end
          else
            ok := GetSymbolRec(p_formal_id).TerminalTypeId = TypeId;
        end
        else if FinalTypeId in StandardTypes then
        begin
          ok := true;
        end
        else
          ok := GetSymbolRec(p_formal_id).TerminalTypeId = TypeId;

        if ok then
        begin
          applicable_list.Clear;
          applicable_list.Add(sub_id);
          Exit;
        end;
      end;
    end;
  end;

  for I := applicable_list.Count - 1 downto 0 do
  begin
    sub_id := applicable_list[I];
    if SymbolTable[sub_id].Count < NP then
      applicable_list.RemoveAt(I)
    else if SymbolTable[sub_id].Count > NP then
    begin
      p_id := SymbolTable.GetParamId(sub_id, NP);
      if not SymbolTable[p_id].Optional then
        applicable_list.RemoveAt(I);
    end;
  end;

  while applicable_list.Count >= 2 do
  begin
    p_id := applicable_list[0];

    cannot_compress := true;

    for I := 1 to applicable_list.Count - 1 do
    begin
      q_id := applicable_list[I];

      n_p := 0;
      n_q := 0;
      for J := 0 to pos.Count - 1 do
      begin
        actual_id := Records[pos[J]].Arg1;
        p_formal_id := SymbolTable.GetParamId(p_id, J);
        q_formal_id := SymbolTable.GetParamId(q_id, J);

        val := CompareConversions(actual_id, p_formal_id, q_formal_id);

        if val = 0 then
        begin
          if not ExistsImplicitConversion(actual_id, p_formal_id) then
          begin
            cannot_compress := false;
            applicable_list.DeleteValue(p_id);
            goto cont;
          end;
          if not ExistsImplicitConversion(actual_id, q_formal_id) then
          begin
            cannot_compress := false;
            applicable_list.DeleteValue(q_id);
            goto cont;
          end;
        end
        else if val > 0 then
        begin
          if val = 2 then
          begin
            cannot_compress := false;
            applicable_list.DeleteValue(q_id);
            goto cont;
          end;

          Inc(n_p);
        end
        else if val < 0 then
        begin
          if val = -2 then
          begin
            cannot_compress := false;
            applicable_list.DeleteValue(p_id);
            goto cont;
          end;

          Inc(n_q);
        end;
      end;

      if (n_p > 0) and (n_q = 0) then
      begin
        // p-member is better
        cannot_compress := false;
        applicable_list.DeleteValue(q_id);
        break;
      end
      else if (n_q > 0) and (n_p = 0) then
      begin
        // q-member is better
        cannot_compress := false;
        applicable_list.DeleteValue(p_id);
        break;
      end;
    end;

  cont:

    if cannot_compress then
      break;
  end;
end;

procedure TCode.CheckSubrangeBounds(SubrangeTypeId: Integer;
  ValueRec: TSymbolRec);
{$IFDEF VARIANTS}
var
  B1, B2: Int64;
{$ELSE}
var
  B1, B2: Integer;
{$ENDIF}
begin
  B1 := TKernel(kernel).SymbolTable.GetLowBoundRec(SubrangeTypeId).value;
  B2 := TKernel(kernel).SymbolTable.GetHighBoundRec(SubrangeTypeId).value;
  if (ValueRec.value < B1) or (ValueRec.value > B2) then
    CreateError(errConstantExpressionViolatesSubrangeBounds, []);
end;

procedure TCode.OperOP_BEGIN_CLASS_TYPE;
var
  ClassId, AncestorId: Integer;
begin
  ClassId := Records[N].Arg1;
  AncestorId := GetSymbolRec(ClassId).AncestorId;
  if GetSymbolRec(AncestorId).IsFinal then
  begin
    if GetLanguage(N) = JAVA_LANGUAGE then
      CreateError(errCannotInheritFromFinalClass,
        [GetSymbolRec(AncestorId).FullName])
    else
      CreateError(errCannotInheritFromSealedClass,
        [GetSymbolRec(AncestorId).FullName])
  end;
end;

procedure TCode.OperInitSub;
var
  I, J, K, NP, SubId, ParamId, T: Integer;
  SymbolTable: TSymbolTable;
  RC, RD, RK, R1, R2: TCodeRec;
  Changed: Boolean;
begin
  temp_var_list.Clear;

  SymbolTable := TKernel(kernel).SymbolTable;
  SubId := Records[N].Arg1;

  if GetSymbolRec(SubId).Kind = KindSUB then
    if GetSymbolRec(SubId).Name <> '' then
    begin
      I := SymbolTable.LookupParentMethod(SubId, GetUpcase(N), false);
      if I > 0 then
        if GetSymbolRec(I).IsFinal then
          CreateError(errOverridenMethodIsFinal, [GetSymbolRec(I).FullName]);
    end;

  NP := SymbolTable[SubId].Count;

  J := N;
  if Records[J + 1].Op = OP_NOP then
    Inc(J);
  if Records[J + 1].Op = OP_GO_DL then
    Inc(J);

  for I := 0 to NP - 1 do
  begin
    ParamId := SymbolTable.GetParamId(SubId, I);

    T := SymbolTable[ParamId].FinalTypeId;
    if T in [typeARRAY, typeRECORD] then
      continue;

    if T in (StringTypes + VariantTypes + [typeINTERFACE]) then
    begin
      if GetSymbolRec(ParamId).IsConst then
        continue;
      if GetSymbolRec(ParamId).ByRef then
        continue;
      {
        if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
        begin
        RC := TCodeRec.Create(OP_SAVE_EDX, Self);
        Inc(J);
        Insert(J, RC);
        end;

        RC := TCodeRec.Create(OP_ADDREF, Self);
        RC.Arg1 := ParamId;
        Inc(J);
        Insert(J, RC);

        if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
        begin
        RC := TCodeRec.Create(OP_RESTORE_EDX, Self);
        Inc(J);
        Insert(J, RC);
        end;

        dmp;
        continue;
      }
      RC := TCodeRec.Create(OP_ASSIGN, Self);
      case T of
{$IFNDEF PAXARM}
        typeANSISTRING:
          RC.Arg1 := CreateStringVar(SubId);
        typeWIDESTRING:
          RC.Arg1 := CreateWideStringVar(SubId);
        typeSHORTSTRING:
          RC.Arg1 := CreateShortStringVar(SubId, typeSHORTSTRING);
{$ENDIF}
        typeUNICSTRING:
          RC.Arg1 := CreateUnicStringVar(SubId);
        typeVARIANT:
          RC.Arg1 := CreateVariantVar(SubId);
        typeOLEVARIANT:
          RC.Arg1 := CreateOleVariantVar(SubId);
        typeINTERFACE:
          begin
            RC.Arg1 := CreateInterfaceVar(SubId);
            GetSymbolRec(RC.Arg1).TypeId := GetSymbolRec(ParamId).TypeId;
          end;
      end;
      RC.Arg2 := ParamId;
      RC.Res := RC.Arg1;

      RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
      RD.Arg1 := SubId;
      RD.Arg2 := RC.Res;
      RD.Res := ParamId; // substitute

      if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
      begin
        R1 := TCodeRec.Create(OP_SAVE_EDX, Self);
        R2 := TCodeRec.Create(OP_RESTORE_EDX, Self);
      end
      else
      begin
        R1 := TCodeRec.Create(OP_NOP, Self);
        R2 := TCodeRec.Create(OP_NOP, Self);
      end;

      Inc(J);
      Insert(J, RD);

      Inc(J);
      Insert(J, R1);

      Inc(J);
      Insert(J, RC);

      Inc(J);
      Insert(J, R2);

      ReplaceIdEx(ParamId, RC.Arg1, J + 1, Card, false, false);
    end
    else if TKernel(kernel).DEBUG_MODE then
    begin
      if GetLanguage(N) <> PASCAL_LANGUAGE then
        continue;

      if GetSymbolRec(ParamId).IsConst then
        continue;

      Changed := false;

      for K := J to Card do
      begin
        RK := Records[K];

        if RK.Op = OP_END_SUB then
          if RK.Arg1 = SubId then
            break;

        if RK.Op = OP_ASSIGN then
          if RK.Arg1 = ParamId then
            Changed := true;

        if Changed then
          if GetSymbolRec(ParamId).ByRef then
          begin
            RC := TCodeRec.Create(OP_PARAM_CHANGED, Self);
            RC.Arg1 := ParamId;

            Inc(J);
            Insert(K + 1, RC);

            break;
          end;
      end;

      if not Changed then
        continue;

      if GetSymbolRec(ParamId).ByRef then
        continue;

      RC := TCodeRec.Create(OP_ASSIGN, Self);
      RC.Arg1 := NewTempVar(SubId, 0);
      GetSymbolRec(RC.Arg1).Name := GetSymbolRec(ParamId).Name;
      RC.Arg2 := ParamId;
      RC.Res := RC.Arg1;

      RD := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
      RD.Arg1 := SubId;
      RD.Arg2 := RC.Res;
      RD.Res := ParamId; // substitute

      R1 := TCodeRec.Create(OP_SAVE_EDX, Self);
      R2 := TCodeRec.Create(OP_RESTORE_EDX, Self);

      Inc(J);
      Insert(J, RD);

      Inc(J);
      Insert(J, R1);

      Inc(J);
      Insert(J, RC);

      Inc(J);
      Insert(J, R2);

      ReplaceIdEx(ParamId, RC.Arg1, J + 1, Card, false);
    end;
  end;
end;

procedure TCode.OperTypeCast;
var
  R, RC: TCodeRec;
  K2, T1, T2, OldRes, NewRes: Integer;
  RecordTypeId, ResultTypeId: Integer;
begin
  R := Records[N];

  T1 := GetSymbolRec(R.Arg1).FinalTypeId;
  T2 := GetSymbolRec(R.Arg2).FinalTypeId;

  K2 := GetSymbolRec(R.Arg2).Kind;

  map_list.Add(R.Arg1);

  if GetSymbolRec(R.Arg1).FinalTypeId = typeRECORD then
  begin
    if GetSymbolRec(R.Arg1).TerminalTypeId <> GetSymbolRec(R.Arg2).TerminalTypeId
    then
    begin
      OldRes := R.Res;

      RecordTypeId := GetSymbolRec(R.Arg1).TerminalTypeId;
      ResultTypeId := GetSymbolRec(R.Arg1).TerminalTypeId;

      NewRes := InsertExplicitConversion(N, RecordTypeId, ResultTypeId).Res;

      R.Op := OP_NOP;
      if OldRes <> NewRes then
      begin
        ReplaceId(OldRes, NewRes);
        GetSymbolRec(OldRes).Kind := KindNONE;
      end;
    end
    else
    begin
      SignTypeCast := true;

      R.Op := OP_ASSIGN;
      R.Arg1 := R.Res;
      Dec(N);
    end;
    Exit;
  end;

  if GetSymbolRec(R.Arg2).FinalTypeId = typeRECORD then
  begin
    if GetSymbolRec(R.Arg1).TerminalTypeId <> GetSymbolRec(R.Arg2).TerminalTypeId
    then
    begin
      OldRes := R.Res;

      RecordTypeId := GetSymbolRec(R.Arg2).TerminalTypeId;
      ResultTypeId := GetSymbolRec(R.Arg1).TerminalTypeId;

      NewRes := InsertExplicitConversion(N, RecordTypeId, ResultTypeId).Res;

      R.Op := OP_NOP;
      if OldRes <> NewRes then
      begin
        ReplaceId(OldRes, NewRes);
        GetSymbolRec(OldRes).Kind := KindNONE;
      end;
    end
    else
    begin
      SignTypeCast := true;

      R.Op := OP_ASSIGN;
      R.Arg1 := R.Res;
      Dec(N);
    end;
    Exit;
  end;

  if (GetSymbolRec(T1).Size = GetSymbolRec(T2).Size) and
    (not(T1 in StringTypes)) and
{$IFNDEF PAXARM}
    (not GetSymbolRec(R.Arg1).HasPAnsiCharType) and
{$ENDIF}
    (not GetSymbolRec(R.Arg1).HasPWideCharType) then
  // variable type cast
  begin
    R.Op := OP_NOP;
    GetSymbolRec(R.Res).TypeId := R.Arg1;

    if K2 = KindCONST then
    begin
      R.Op := OP_NOP;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).value := GetSymbolRec(R.Arg2).value;

      if GetSymbolRec(R.Arg2).FinalTypeId = typeENUM then
        if R.Arg2 > R.Res then
          if GetSymbolRec(R.Arg2 + 1).Kind = KindCONST then
            GetSymbolRec(R.Res).value := GetSymbolRec(R.Arg2 + 1).value;

      Exit;
    end;

    if T1 in [typePOINTER, typePROC] then
    begin
      R.Op := OP_ASSIGN_INT_M;
      R.Arg1 := R.Res;
      Exit;
    end;

    if T1 in [typeCLASS, typeCLASSREF] then
      if (Records[N + 1].Arg1 = R.Res) then
      begin
        R.Op := OP_ASSIGN_INT_M;
        R.Arg1 := R.Res;
        Exit;
      end;

    if T1 in [typeCLASS, typeCLASSREF] then
      if (Records[N + 1].Arg2 = R.Res) then
      begin
        R.Op := OP_ASSIGN_INT_M;
        R.Arg1 := R.Res;
        Exit;
      end;

    GetSymbolRec(R.Res).UnionID := R.Arg2;
    GetSymbolRec(R.Res).ByRef := GetSymbolRec(R.Arg2).ByRef;
    GetSymbolRec(R.Res).Level := GetSymbolRec(R.Arg2).Level;
    if GetSymbolRec(R.Arg2).Name = '' then
      ReplaceIdEx(R.Arg2, R.Res, 1, N, false);
  end
  else if T2 in VariantTypes then
  begin
    case T1 of
{$IFNDEF PAXARM}
      typeANSICHAR:
        R.Op := OP_ANSICHAR_FROM_VARIANT;
      typeANSISTRING:
        R.Op := OP_ANSISTRING_FROM_VARIANT;
      typeWIDESTRING:
        R.Op := OP_WIDESTRING_FROM_VARIANT;
      typeSHORTSTRING:
        R.Op := OP_SHORTSTRING_FROM_VARIANT;
{$ENDIF}
      typeUNICSTRING:
        R.Op := OP_UNICSTRING_FROM_VARIANT;
      typeWIDECHAR:
        R.Op := OP_WIDECHAR_FROM_VARIANT;
      typeDOUBLE:
        R.Op := OP_DOUBLE_FROM_VARIANT;
      typeCURRENCY:
        R.Op := OP_CURRENCY_FROM_VARIANT;
      typeSINGLE:
        R.Op := OP_SINGLE_FROM_VARIANT;
      typeEXTENDED:
        R.Op := OP_EXTENDED_FROM_VARIANT;
      typeINT64:
        R.Op := OP_INT64_FROM_VARIANT;
      typeINTEGER:
        R.Op := OP_INT_FROM_VARIANT;
      typeBYTE:
        R.Op := OP_BYTE_FROM_VARIANT;
      typeWORD:
        R.Op := OP_WORD_FROM_VARIANT;
      typeCARDINAL:
        R.Op := OP_CARDINAL_FROM_VARIANT;
      typeBOOLEAN:
        R.Op := OP_BOOL_FROM_VARIANT;
      typeBYTEBOOL:
        R.Op := OP_BYTEBOOL_FROM_VARIANT;
      typeWORDBOOL:
        R.Op := OP_WORDBOOL_FROM_VARIANT;
      typeLONGBOOL:
        R.Op := OP_LONGBOOL_FROM_VARIANT;
      typeSMALLINT:
        R.Op := OP_SMALLINT_FROM_VARIANT;
      typeSHORTINT:
        R.Op := OP_SHORTINT_FROM_VARIANT;
    else
      RaiseError(errInvalidTypeCast, []);
    end;
    GetSymbolRec(R.Res).TypeId := T1;
  end
  else // value type cast
  begin
    GetSymbolRec(R.Res).TypeId := R.Arg1;

    if K2 = KindCONST then
    begin
      R.Op := OP_NOP;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).value := GetSymbolRec(R.Arg2).value;

      if GetSymbolRec(R.Arg2).FinalTypeId = typeENUM then
        if R.Arg2 > R.Res then
          if GetSymbolRec(R.Arg2 + 1).Kind = KindCONST then
            GetSymbolRec(R.Res).value := GetSymbolRec(R.Arg2 + 1).value;
{$IFNDEF PAXARM}
      if GetSymbolRec(R.Arg2).HasPAnsiCharType then
        GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg2).TypeId;
{$ENDIF}
      if GetSymbolRec(R.Arg2).HasPWideCharType then
        GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg2).TypeId;

      Exit;
    end;

    // R.Op := OP_ASSIGN_INT_M;
    // R.Arg1 := R.Res;

    SignTypeCast := true;

    R.Op := OP_ASSIGN;
    R.Arg1 := R.Res;

    if T1 in DynamicTypes then
    begin
      RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
      RC.Arg1 := GetLevel(N);
      RC.Arg2 := R.Res;
      RC.Res := 0;
      Insert(N, RC);
      Exit;
    end;

    Dec(N);
  end;
end;

procedure TCode.OperCallDefaultConstructor;
var
  R, RC: TCodeRec;
  SymbolTable: TSymbolTable;
  L, Id, FinTypeId, TypeId, SubId: Integer;
  V: Variant;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;
  Id := R.Arg1;
  FinTypeId := SymbolTable[Id].FinalTypeId;
  if FinTypeId = typeRECORD then
  begin
    TypeId := SymbolTable[Id].TerminalTypeId;
    if GetSymbolRec(TypeId).Host then
    begin
      R.Op := OP_NOP;
      Exit;
    end;

    SubId := SymbolTable.FindConstructorId(TypeId);
    if SubId = 0 then
    begin
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_CALL;
    R.Arg1 := SubId;
    R.Arg2 := 0;
    R.Res := 0;

    RC := TCodeRec.Create(OP_PUSH_INSTANCE, Self);
    RC.Arg1 := Id;
    RC.Arg2 := 0;
    RC.Res := SubId;
    Insert(N, RC);
    Dec(N);
  end
  else
  begin
    L := GetSymbolRec(Id).Level;
    if L = 0 then
    begin
      R.Op := OP_NOP;
      Exit;
    end;
    if not(GetSymbolRec(L).Kind in kindSUBS) then
    begin
      R.Op := OP_NOP;
      Exit;
    end;
    case FinTypeId of
      typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
        begin
          R.Arg2 := CreateConst(FinTypeId, false);
          R.Op := OP_ASSIGN;
          R.Res := R.Arg1;
          Dec(N);
        end;
      typeBYTE, typeINTEGER, typeSMALLINT, typeSHORTINT, typeWORD, typeCARDINAL,
        typeCLASS, typeCLASSREF, typeDOUBLE, typeSINGLE, typeCURRENCY,
        typeEXTENDED:
        begin
          R.Arg2 := CreateConst(FinTypeId, 0);
          R.Op := OP_ASSIGN;
          R.Res := R.Arg1;
          Dec(N);
        end;
{$IFDEF PAXARM}
      typeUNICSTRING:
{$ELSE}
      typeANSISTRING, typeWIDESTRING, typeSHORTSTRING
{$IFDEF UNIC}, typeUNICSTRING{$ENDIF}:
{$ENDIF}
        begin
          R.Arg2 := CreateConst(FinTypeId, '');
          R.Op := OP_ASSIGN;
          R.Res := R.Arg1;
          Dec(N);
        end;
      typeVARIANT, typeOLEVARIANT:
        begin
          R.Arg2 := CreateConst(FinTypeId, V);
          R.Op := OP_ASSIGN;
          R.Res := R.Arg1;
          Dec(N);
        end;
    else
      R.Op := OP_NOP;
    end;
  end;
end;

procedure TCode.OperCheckFinal;
begin
  if Records[N].Arg1 <> 0 then
    SkipCheckFinal := true
  else
    SkipCheckFinal := false;
end;

procedure TCode.OperCheckSubCall;
var
  SubId: Integer;
begin
  SubId := Records[N].Arg1;
  if GetSymbolRec(SubId).Kind = KindSUB then
    if GetSymbolRec(SubId).TypeId = typeVOID then
    begin
      Records[N].Op := OP_NOP;
    end
    else
      CreateError(errThisWayOfCallIsAllowedOnlyForProcedures, []);
end;

procedure TCode.OperPushInstance;
var
  SubId: Integer;
begin
  SubId := Records[N].Res;
  if SubId > 0 then
    if GetSymbolRec(SubId).CallMode = cmSTATIC then
      Records[N].Op := OP_NOP;
end;

procedure TCode.OperPush;
begin
end;

procedure TCode.OperAdjustInstance;
var
  I, Id: Integer;
begin
  I := N;
  Records[I].Op := OP_NOP;
  Id := Records[I].Arg1;
  while Records[I].Op <> OP_PUSH_INST do
    Dec(I);
  Records[I].Arg1 := Id;
end;

procedure TCode.OperCall;

  function CheckSub(var AnId: Integer): Boolean;
  var
    I, J, Id: Integer;
    S, S1, S2: String;
    SymbolTable: TSymbolTable;
  begin
    SymbolTable := TKernel(kernel).SymbolTable;
    S2 := GetSymbolRec(AnId).Name;
    result := false;
    Id := 0;
    J := N;
    while Records[J].Op <> OP_BEGIN_MODULE do
      Dec(J);
    for I := J to N do
      if Records[I].Op = OP_BEGIN_USING then
      begin
        S1 := GetSymbolRec(Records[I].Arg1).Name;
        if S1 = '' then
          continue;
        S := S1 + '.' + S2;
        Id := SymbolTable.LookupFullName(S, GetUpcase(N));
        if Id > 0 then
          if GetSymbolRec(Id).Kind in kindSUBS then
          begin
            result := true;
            break;
          end;
      end;
    if not result then
      Exit;
    Records[N].Arg1 := Id;
    for J := N downto GetStmt(N) do
      if (Records[J].Op = OP_PUSH) and (Records[J].Res = AnId) then
        Records[J].Res := Id;
    AnId := Id;
  end;

  function IsVarArrayIndex(Id, T1, J: Integer; P: TIntegerList): Boolean;
  var
    I, K, ArrayId, temp, temp_2: Integer;
    R, RC: TCodeRec;
    ParamIds: TIntegerList;
  begin
    result := false;
    ParamIds := TIntegerList.Create;
    ArrayId := 0;
    try
      for I := N downto 1 do
      begin
        R := Records[I];
        if R.Op = OP_BEGIN_MODULE then
          break;
        if R.Op = OP_VARARRAY_GET then
          if R.Res = Id then
          begin
            ArrayId := R.Arg1;
            for K := I downto 1 do
              if Records[K].Op = OP_PUSH_ADDRESS then
              begin
                ParamIds.Add(Records[K].Arg1);
                if ParamIds.Count = R.Arg2 then
                  break;
              end;

            result := true;
            break;
          end;
      end;

      if not result then
        Exit;

      temp := 0;
      case T1 of
        typeINTEGER:
          temp := InsertConversionToInteger(P[J], 1).Res;
        typeBYTE:
          temp := InsertConversionToByte(P[J], 1).Res;
        typeWORD:
          temp := InsertConversionToWord(P[J], 1).Res;
        typeCARDINAL:
          temp := InsertConversionToCardinal(P[J], 1).Res;
        typeSHORTINT:
          temp := InsertConversionToShortInt(P[J], 1).Res;
        typeSMALLINT:
          temp := InsertConversionToSmallInt(P[J], 1).Res;
        typeSINGLE:
          temp := InsertConversionToSingle(P[J], 1).Res;
        typeDOUBLE:
          temp := InsertConversionToDouble(P[J], 1).Res;
        typeEXTENDED:
          temp := InsertConversionToExtended(P[J], 1).Res;
        typeCURRENCY:
          temp := InsertConversionToCurrency(P[J], 1).Res;
        typeBOOLEAN:
          temp := InsertConversionToBoolean(P[J], 1).Res;
{$IFNDEF PAXARM}
        typeANSISTRING:
          temp := InsertConversionToAnsiString(P[J], 1).Res;
        typeSHORTSTRING:
          temp := InsertConversionToShortString(P[J], 1).Res;
        typeWIDESTRING:
          temp := InsertConversionToWideString(P[J], 1).Res;
        typeANSICHAR:
          temp := InsertConversionToAnsiChar(P[J], 1).Res;
{$ENDIF}
        typeUNICSTRING:
          temp := InsertConversionToUnicString(P[J], 1).Res;
        typeWIDECHAR:
          temp := InsertConversionToWideChar(P[J], 1).Res;
      else
        CreateError(errTypesOfActualAndFormalVarParametersMustBeIdentical, []);
      end;

      Inc(N);
      for I := J to P.Count - 1 do
        P[I] := P[I] + 1;

      I := N + 1;

      temp_2 := NewTempVar(GetLevel(N), typeVARIANT);

      RC := TCodeRec.Create(OP_ASSIGN, Self);
      RC.Arg1 := temp_2;
      RC.Arg2 := temp;
      RC.Res := temp_2;
      Insert(I, RC);
      Inc(I);

      RC := TCodeRec.Create(OP_PUSH_ADDRESS, Self);
      RC.Arg1 := ParamIds[0];
      RC.Arg2 := 0;
      RC.Res := 0;
      Insert(I, RC);
      Inc(I);

      RC := TCodeRec.Create(OP_VARARRAY_PUT, Self);
      RC.Arg1 := ArrayId;
      RC.Arg2 := 1;
      RC.Res := temp_2;
      Insert(I, RC);

    finally
      FreeAndNil(ParamIds);
    end;
  end;

  function PassedProc: Boolean;
  var
    I, ResId, SubId, PCount, ParamId, FinTypeId, TypeId, PatternSubId: Integer;
    SymbolTable: TSymbolTable;
  begin
    result := false;
    ResId := Records[N].Res;
    SymbolTable := TKernel(kernel).SymbolTable;
    if ResId = 0 then
      Exit;
    if Records[N - 1].Op <> OP_BEGIN_CALL then
      Exit;

    for I := N + 1 to Card do
      if Records[I].Op = OP_PUSH then
        if Records[I].Arg1 = ResId then
        begin
          SubId := Records[I].Res;
          PCount := Records[I].Arg2;
          ParamId := SymbolTable.GetParamId(SubId, PCount);
          FinTypeId := GetSymbolRec(ParamId).FinalTypeId;
          if FinTypeId = typePROC then
          begin
            TypeId := GetSymbolRec(ParamId).TerminalTypeId;
            PatternSubId := GetSymbolRec(TypeId).PatternId;
            if SymbolTable.EqualHeaders(Records[N].Arg1, PatternSubId) then
            begin
              result := true;
              Records[N - 1].Op := OP_ADDRESS;
              Records[N - 1].Res := CreatePointerVar(GetLevel(N));
              Records[N].Op := OP_NOP;
              Records[I].Arg1 := Records[N - 1].Res;
              Dec(N, 2);
            end;
          end;

          break;
        end;
  end;

var
  P: TIntegerList;

  procedure P_Add(J: Integer);
  var
    R, RJ: TCodeRec;
    I: Integer;
  begin
    RJ := Records[J];
    for I := 0 to P.Count - 1 do
    begin
      R := Records[P[I]];
      if (R.Arg2 = RJ.Arg2) and (R.Res = RJ.Res) then
      begin
        P[I] := J;
        Exit;
      end;
    end;

    P.Add(J);
  end;

var
  R, RR: TCodeRec;
  SubId, J: Integer;
  SymbolTable: TSymbolTable;
  Arg1, Arg2, T1, T2, K2: Integer;
  L: TCodeRecList;
  OverList: TIntegerList;
  IsOverloadedCall: Boolean;
  p_id: Integer;
  RC, RC_BEGIN_CALL: TCodeRec;
  SavedSubId: Integer;
  PosInstance: Integer;
  PosClassRef: Integer;
  S: String;
  J1, Card1, Card2: Integer;
  N_BeginCall: Integer;

  I, I1, Id, K: Integer;
  Id_VMT: Integer;
  CallRefId: Integer;
  ThisId, ThisRefId: Integer;
  b: Boolean;
  GenOp, SubId1, SubId2: Integer;
  ParamId, ArrLength: Integer;
  VarArgIds: TIntegerList;
  ArrTypeId, ElemTypeId, DynArrayId: Integer;
  ResId: Integer;
  RJ: TCodeRec;
  KZ: Integer;
  SubIdLevel: Integer;
{$IFDEF PAXARM}
  SS: String;
{$ELSE}
  SS: ShortString;
{$ENDIF}
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  PosInstance := 0;
  PosClassRef := 0;
  N_BeginCall := 0;
  IsOverloadedCall := false;

  R := Records[N];
  RR := Records[N];

  SubId := R.Arg1;

  SavedSubId := -1;
  if GetSymbolRec(SubId).Kind = KindCONST then
  begin
    RaiseError(errIdentifierExpectedNoArgs, []);
  end
  else if GetSymbolRec(SubId).Kind = KindVAR then
  begin

    if R.Language = JS_LANGUAGE then
    begin
      if GetSymbolRec(SubId).TypeId = typeVARIANT then
      begin
        for J := GetStmt(N) to N do
          if (Records[J].Op = OP_FIND_CONTEXT) and (Records[J].Res = SubId) then
          begin
            Records[J].Op := OP_FIND_JS_FUNC;
            break;
          end;

        SubId := NewTempVar(GetSymbolRec(R.Arg1).Level, JS_FunctionClassId);

        I := 0;
        ThisId := 0;
        for J := GetStmt(N) to N do
          if (Records[J].Op = OP_PUSH) and (Records[J].Res = R.Arg1) then
          begin
            if I = 0 then
              I := J;
            Records[J].Res := SubId;
          end
          else if (Records[J].Op = OP_CALL) and (Records[J].Res = R.Arg1) then
          begin
            K := J - 1;
            while Records[K].Op <> OP_PUSH_INST do
              Dec(K);

            ThisId := Records[K].Arg1;
          end;

        if I = 0 then
          I := N;

        b := false;
        I1 := N;
        for J := I downto GetStmt(N) do
          if (Records[J].Op = OP_CALL) and (Records[J].Arg1 = JS_GetPropertyId)
            and (Records[J].Res = Records[N].Arg1) then
          begin
            b := true;
            I1 := J;
            break;
          end;

        if b { and false } then
        begin
          Records[I1].Arg1 := JS_GetPropertyAsObjectId;
          Records[I1].Res := SubId;
          J := I1;
          repeat
            Dec(J);
            if Records[J].Res = JS_GetPropertyId then
              Records[J].Res := JS_GetPropertyAsObjectId;

          until (Records[J].Op = OP_BEGIN_CALL) and
            (Records[J].Arg1 = JS_GetPropertyId);
          Records[J].Arg1 := JS_GetPropertyAsObjectId;
        end
        else
        begin
          RC := TCodeRec.Create(OP_JS_FUNC_OBJ_FROM_VARIANT, Self);
          RC.Arg1 := R.Arg1;
          RC.Arg2 := 0;
          RC.Res := SubId;
          Insert(I, RC);
          Inc(N);
        end;

        if ThisId > 0 then
        begin
          ThisRefId := NewTempVar(GetSymbolRec(R.Res).Level, typePOINTER);
          GetSymbolRec(ThisRefId).OwnerId := SubId;
          GetSymbolRec(ThisRefId).Name := str__this;
          RC := TCodeRec.Create(OP_FIELD, Self);
          RC.Arg1 := SubId;
          RC.Arg2 := ThisRefId;
          RC.Res := ThisRefId;
          Insert(N, RC);
          Inc(N);

          RC := TCodeRec.Create(OP_ASSIGN, Self);
          RC.Arg1 := ThisRefId;
          RC.Arg2 := ThisId;
          RC.Res := ThisRefId;
          Insert(N, RC);
          Inc(N);

          R.Arg1 := SubId;

          {
            if Records[N].Op = OP_CALL then
            begin
            RC := TCodeRec.Create(OP_ADJUST_INSTANCE, Self);
            RC.Arg1 := ThisId;
            Insert(N + 1, RC);
            end;
          }
          Dec(N, 3);

          Exit;
        end;

        R.Arg1 := SubId;
      end;
    end;

    if GetSymbolRec(SubId).TypeId = JS_FunctionClassId then
    begin
      CallRefId := NewTempVar(GetLevel(N), typeVARIANT);
      GetSymbolRec(CallRefId).Name := strInternalCall;
      GetSymbolRec(CallRefId).OwnerId := Records[N].Arg1;

      P := TIntegerList.Create;

      for J := GetStmt(N) to N do
      begin
        if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
        begin
          P_Add(J);
          Records[J].Res := CallRefId;
          Inc(Records[J].Arg2, 1);
        end;
      end;
      Records[N].Arg1 := CallRefId;
      Inc(Records[N].Arg2);

      if P.Count > 0 then
        N := P[0];

      RC := TCodeRec.Create(OP_FIELD, Self);
      RC.Arg1 := SubId;
      RC.Arg2 := CallRefId;
      RC.Res := CallRefId;
      Insert(N, RC);
      Inc(N);

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := CreateConst(typeINTEGER, P.Count);
      RC.Arg2 := 0;
      RC.Res := CallRefId;
      Insert(N, RC);

      Dec(N, 2);

      FreeAndNil(P);
      Exit;
    end;

    if GetSymbolRec(SubId).FinalTypeId in [typePROC, typeEVENT] then
    begin
      SavedSubId := SubId;
      T1 := GetSymbolRec(SubId).TerminalTypeId;
      SubId := SymbolTable.GetPatternSubId(T1);
      R.Arg1 := SubId;
      for J := GetStmt(N) to N do
        if (Records[J].Op = OP_PUSH) and (Records[J].Res = SavedSubId) then
        begin
          Records[J].Res := SubId;
          Records[J].SavedSubId := SavedSubId;
        end;
    end
    else
    begin
      if Records[N - 1].Op = OP_OLE_GET then
      begin
        Records[N].Op := OP_NOP;
        Exit;
      end;

      if (GetSymbolRec(R.Arg1).FinalTypeId in [typeCLASS, typeINTERFACE]) and
        (not GetSymbolRec(R.Arg1).IsFWArrayVar) then
      begin
        I := 0;

        if GetSymbolRec(R.Arg1).FinalTypeId = typeINTERFACE then
          I := SymbolTable.LookupAnonymousMethod(GetSymbolRec(R.Arg1)
            .TerminalTypeId);

        if I = 0 then
          I := SymbolTable.FindDefaultPropertyId(GetSymbolRec(R.Arg1)
            .TerminalTypeId);

        if I <> 0 then
        begin
          S := GetSymbolRec(I).Name;
          Id := NewTempVar(GetLevel(N), GetSymbolRec(I).TypeId);

          GetSymbolRec(Id).OwnerId := R.Arg1;
          GetSymbolRec(Id).Name := GetSymbolRec(I).Name;

          RC := TCodeRec.Create(OP_FIELD, Self);
          RC.Arg1 := R.Arg1;
          RC.Arg2 := Id;
          RC.Res := Id;

          K := 0;
          for J := GetStmt(N) to N do
          begin
            if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
            begin
              if K = 0 then
                K := J;
              Records[J].Res := Id;
            end
          end;

          R.Arg1 := Id;

          if K > 0 then
            N := K;

          Insert(N, RC);
          Dec(N);
        end
        else
        begin
          S := GetSymbolRec(GetSymbolRec(R.Arg1).TerminalTypeId).Name;
          CreateError(errClassDoesNotHaveDefaultProperty, [S]);
        end;
        Exit;
      end;

      if Records[N - 1].Op = OP_CALL then
        if Records[N - 1].Res = Records[N].Arg1 then
        begin
          Records[N].Op := OP_NOP;
          Exit;
        end;

      if (GetSymbolRec(R.Arg1).FinalTypeId in [typeDYNARRAY, typeVARIANT]) or
        GetSymbolRec(R.Arg1).IsFWArrayVar then
      begin
        P := TIntegerList.Create;
        try
          for J := GetStmt(N) to N do
          begin
            if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
            begin
              P_Add(J);
              if R.Res = 0 then
                Records[J].Op := OP_NOP;
            end;
          end;

          if R.Res = 0 then
          begin
            R.Op := OP_NOP;
            Exit;
          end;

          if P.Count = 0 then
            RaiseError(errCannotApplyCall, [GetSymbolRec(SubId).Name]);

          ResId := SubId;

          for J := 0 to P.Count - 1 do
          begin
            I := P[J];
            Records[I].Op := OP_ELEM;
            Records[I].Arg2 := Records[I].Arg1;
            Records[I].Arg1 := ResId;
            Records[I].Res := CreateVariantVar(GetLevel(N));
            ResId := Records[I].Res;

            GetSymbolRec(ResId).OwnerId := Records[I].Arg1;
          end;

          Records[N].Op := OP_NOP;
          ReplaceId(Records[N].Res, ResId);

          Dec(N, P.Count + 1);

        finally
          FreeAndNil(P);
        end;
      end
      else if GetLanguage(N) <> JS_LANGUAGE then
      begin
        RaiseError(errCannotApplyCall, [GetSymbolRec(R.Arg1).Name]);
      end;

      Exit;
    end;
  end
  else if GetSymbolRec(SubId).Kind = KindTYPE then
    if not CheckSub(SubId) then
    begin

      if R.Arg2 <> 1 then
        CreateError(errTooManyActualParameters, []);
      p_id := 0;
      for J := N downto GetStmt(N) do
        if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
        begin
          p_id := Records[J].Arg1;
          Records[J].Op := OP_NOP;
          break;
        end;
      R.Arg2 := p_id;

      R.Op := OP_TYPE_CAST;
      Dec(N);

      Exit;
    end;

  P := TIntegerList.Create;

  try
    PosInstance := 0;
    PosClassRef := 0;

    for J := GetStmt(N) to N do
    begin
      RJ := Records[J];

      if (RJ.Op = OP_PUSH) and (RJ.Res = SubId) then
        P_Add(J);
      if (RJ.Op = OP_PUSH_INSTANCE) and (RJ.Res = SubId) then
      begin
        if GetSymbolRec(RJ.Res).CallMode = cmSTATIC then
          RJ.Op := OP_NOP
        else
          PosInstance := J;
      end;
      if (RJ.Op = OP_PUSH_CLASSREF) and (RJ.Res = SubId) then
      begin
        if GetSymbolRec(RJ.Res).CallMode = cmSTATIC then
          RJ.Op := OP_NOP
        else
          PosClassRef := J;
      end;
    end;

    // insert begin call /////////////

    RC := TCodeRec.Create(OP_BEGIN_CALL, Self);
    RC.Arg1 := SubId;
    RC.Arg2 := 0;
    RC.Res := 0;

    RC_BEGIN_CALL := RC;

    if GetSymbolRec(SubId).CallConv = ccSAFECALL then
      if GetSymbolRec(SubId).TypeId <> typeVOID then
        RC.Res := NewTempVar(GetLevel(N), typeINTEGER);

    if P.Count > 0 then
      N_BeginCall := Insert(P[0], RC)
    else
      N_BeginCall := Insert(N, RC);

    for J := P.Count - 1 downto 0 do
      P[J] := P[J] + 1;

    Inc(N);

    SubIdLevel := SymbolTable[SubId].Level;

    /// ///////////////////////////////
    if (SymbolTable[SubId].Name = '') or NoOverloadSearch then
    begin
      if SymbolTable[SubId].Kind = KindCONSTRUCTOR then
      begin
        OverList := SymbolTable.FindConstructorIds(GetSymbolRec(SubId).TypeId);
      end
      else
      begin
        OverList := TIntegerList.Create;
        OverList.Add(SubId);
      end;
    end
    else
    begin
      if SymbolTable[SubId].NSOwnerId > 0 then
      begin
        OverList := SymbolTable.LookUpSub(SymbolTable[SubId].Name,
          SymbolTable[SubId].NSOwnerId, GetUpcase(N));
      end
      else if PosInstance + PosClassRef = 0 then // global function call
        OverList := SymbolTable.LookUpSubs(SymbolTable[SubId].Name,
          SymbolTable[SubId].Level, using_list, GetUpcase(N))
      else
        OverList := SymbolTable.LookUpSub(SymbolTable[SubId].Name,
          SymbolTable[SubId].Level, GetUpcase(N));
    end;
    NoOverloadSearch := false;

    if OverList.Count > 1 then // overloaded call
    begin

      IsOverloadedCall := true;
      CompressListOfOverloaded(OverList, P);

      if OverList.Count = 0 then
        CreateError(errThereIsNoOverloaded, [SymbolTable[SubId].Name])
        // else if OverList.Count > 1 then
        // CreateError(errThereIsNoOverloaded, [SymbolTable[SubId].Name])
      else // ok
      begin
        SubId := -1;
        for J := 0 to OverList.Count - 1 do
          if GetSymbolRec(OverList[J]).Level = SubIdLevel then
          begin
            SubId := OverList[J];
            break;
          end;
        if SubId = -1 then
          SubId := OverList[OverList.Count - 1];

        R.Arg1 := SubId;
        for J := P.Count - 1 downto 0 do
          Records[P[J]].Res := SubId;

        RC_BEGIN_CALL.Arg1 := SubId;
      end;
    end
    else // OverList.Count = 1
      if SubId <> JS_FunctionCallId then
      begin
        IsOverloadedCall := false;
        if P.Count > SymbolTable[SubId].Count then
        begin
          if true then // GetLanguage(N) <> PASCAL_LANGUAGE then
          begin
            if SymbolTable[SubId].Count = 0 then
              CreateError(errTooManyActualParameters, [])
            else
            begin
              ParamId := SymbolTable.GetParamId(SubId,
                SymbolTable[SubId].Count - 1);
              if SymbolTable[ParamId].FinalTypeId <> typeDYNARRAY then
              begin
                CreateError(errTooManyActualParameters, []);
              end
              else
              begin
                if GetLanguage(N) = PASCAL_LANGUAGE then
                  CreateError(errTooManyActualParameters, []);

                RC_BEGIN_CALL.Op := OP_NOP;

                ArrLength := P.Count - SymbolTable[SubId].Count + 1;

                Dec(Records[N].Arg2, ArrLength - 1);

                VarArgIds := TIntegerList.Create;
                try
                  for I := P.Count - 1 downto 0 do
                  begin
                    Records[P[I]].Op := OP_NOP;
                    VarArgIds.Insert(0, Records[P[I]].Arg1);
                    if VarArgIds.Count = ArrLength then
                      break;
                  end;

                  ArrTypeId := GetSymbolRec(ParamId).TerminalTypeId;
                  ElemTypeId := GetSymbolRec(ArrTypeId).PatternId;
                  DynArrayId := NewTempVar(GetLevel(N), ArrTypeId);

                  RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
                  RC.Arg1 := GetLevel(N);
                  RC.Arg2 := DynArrayId;
                  RC.Res := 0;
                  Insert(N, RC);
                  Inc(N);

                  RC := TCodeRec.Create(OP_SET_LENGTH, Self);
                  RC.Arg1 := DynArrayId;
                  RC.Arg2 := CreateConst(typeINTEGER, ArrLength);
                  RC.Res := 0;
                  Insert(N, RC);
                  Inc(N);

                  for I := 0 to VarArgIds.Count - 1 do
                  begin
                    RC := TCodeRec.Create(OP_ELEM, Self);
                    RC.Arg1 := DynArrayId;
                    RC.Arg2 := CreateConst(typeINTEGER, I);
                    ParamId := NewTempVar(GetLevel(N), ElemTypeId);
                    RC.Res := ParamId;

                    Insert(N, RC);
                    Inc(N);

                    RC := TCodeRec.Create(OP_ASSIGN, Self);
                    RC.Arg1 := ParamId;
                    RC.Arg2 := VarArgIds[I];
                    RC.Res := ParamId;

                    Insert(N, RC);
                    Inc(N);
                  end;

                  RC := TCodeRec.Create(OP_PUSH, Self);
                  RC.Arg1 := DynArrayId;
                  RC.Arg2 := P.Count - ArrLength;
                  RC.Res := SubId;
                  Insert(N, RC);
                  Inc(N);

                  Dec(N, 4 + VarArgIds.Count * 2);

                finally
                  FreeAndNil(VarArgIds);
                  FreeAndNil(OverList);
                end;
                PosClassRef := 0;
                Exit;

              end;
            end;
          end
          else
            CreateError(errTooManyActualParameters, []);
        end
        else if P.Count < SymbolTable[SubId].Count then
        begin
          p_id := SymbolTable.GetParamId(SubId, P.Count);

          if not SymbolTable[p_id].Optional then
          begin
            if PassedProc then
            begin
              FreeAndNil(OverList);
              Exit;
            end
            else if SymbolTable[p_id].FinalTypeId
              in [typeDYNARRAY, typeOPENARRAY] then
            begin
              if GetLanguage(N) = PASCAL_LANGUAGE then
                CreateError(errNotEnoughActualParameters, []);

              RC_BEGIN_CALL.Op := OP_NOP;

              ArrTypeId := SymbolTable[p_id].TypeId;
              if GetSymbolRec(ArrTypeId).FinalTypeId = typeOPENARRAY then
                ArrTypeId := SymbolTable.RegisterDynamicArrayType(0, '',
                  GetSymbolRec(ArrTypeId).PatternId);

              DynArrayId := SymbolTable.AddDynarrayVar(GetLevel(N),
                ArrTypeId).Id;

              RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
              RC.Arg1 := GetLevel(N);
              RC.Arg2 := DynArrayId;
              RC.Res := 0;
              Insert(N, RC);
              Inc(N);

              RC := TCodeRec.Create(OP_CREATE_EMPTY_DYNARRAY, Self);
              RC.Arg1 := DynArrayId;
              RC.Arg2 := 0;
              RC.Res := 0;
              Insert(N, RC);
              Inc(N);

              RC := TCodeRec.Create(OP_PUSH, Self);
              RC.Arg1 := DynArrayId;
              RC.Arg2 := P.Count;
              RC.Res := SubId;
              Insert(N, RC);
              Inc(N);

              Dec(N, 3);
              PosClassRef := 0;

              FreeAndNil(OverList);
              Exit;
            end
            else
              CreateError(errNotEnoughActualParameters, []);
          end;
        end
        else if (P.Count = 1) and (SymbolTable[SubId].Count = 1) and
          (Records[P[0]].Arg1 <> SymbolTable.NilId) and
          (SymbolTable[SymbolTable.GetParamId(SubId, SymbolTable[SubId].Count -
          1)].FinalTypeId in [typeDYNARRAY, typeOPENARRAY]) and
          (not SymbolTable[Records[P[0]].Arg1].IsFWArrayVar) and
          (not(SymbolTable[Records[P[0]].Arg1].FinalTypeId in [typeDYNARRAY,
          typeSET, typeARRAY])) then
        begin
          ParamId := SymbolTable.GetParamId(SubId,
            SymbolTable[SubId].Count - 1);
          RC_BEGIN_CALL.Op := OP_NOP;

          ArrLength := P.Count - SymbolTable[SubId].Count + 1;

          Dec(Records[N].Arg2, ArrLength - 1);

          VarArgIds := TIntegerList.Create;
          try
            for I := P.Count - 1 downto 0 do
            begin
              Records[P[I]].Op := OP_NOP;
              VarArgIds.Insert(0, Records[P[I]].Arg1);
              if VarArgIds.Count = ArrLength then
                break;
            end;

            ArrTypeId := GetSymbolRec(ParamId).TerminalTypeId;
            ElemTypeId := GetSymbolRec(ArrTypeId).PatternId;
            if GetSymbolRec(ArrTypeId).FinalTypeId = typeOPENARRAY then
              ArrTypeId := SymbolTable.RegisterDynamicArrayType(0, '',
                ElemTypeId);
            DynArrayId := NewTempVar(GetLevel(N), ArrTypeId);

            RC := TCodeRec.Create(OP_SET_LENGTH, Self);
            RC.Arg1 := DynArrayId;
            RC.Arg2 := CreateConst(typeINTEGER, ArrLength);
            RC.Res := 0;
            Insert(N, RC);
            Inc(N);

            for I := 0 to VarArgIds.Count - 1 do
            begin
              RC := TCodeRec.Create(OP_ELEM, Self);
              RC.Arg1 := DynArrayId;
              RC.Arg2 := CreateConst(typeINTEGER, I);
              ParamId := NewTempVar(GetLevel(N), ElemTypeId);
              RC.Res := ParamId;

              Insert(N, RC);
              Inc(N);

              RC := TCodeRec.Create(OP_ASSIGN, Self);
              RC.Arg1 := ParamId;
              RC.Arg2 := VarArgIds[I];
              RC.Res := ParamId;

              Insert(N, RC);
              Inc(N);
            end;

            RC := TCodeRec.Create(OP_PUSH, Self);
            RC.Arg1 := DynArrayId;
            RC.Arg2 := P.Count - ArrLength;
            RC.Res := SubId;
            Insert(N, RC);
            Inc(N);

            RC := TCodeRec.Create(OP_DESTROY_LOCAL_VAR, Self);
            RC.Arg1 := DynArrayId;
            Insert(N + 1, RC);

            Dec(N, 3 + VarArgIds.Count * 2);
            PosClassRef := 0;

          finally
            FreeAndNil(VarArgIds);
            FreeAndNil(OverList);
          end;

          Exit;
        end;
      end;

    FreeAndNil(OverList);

    if HasError then
      Exit;

    {
      if SymbolTable[SymbolTable[SubId].Level].Kind = KindSUB then // internal sub call
      begin
      RC := TCodeRec.Create(OP_PUSH_EBP, Self);
      RC.Arg1 := 0;
      RC.Arg2 := 0;
      RC.Res := SubId;

      Insert(N, RC);
      Inc(N);
      end;
    }

    if P.Count < SymbolTable[SubId].Count then
    // insert optional parameter values
    begin
      for J := P.Count to SymbolTable[SubId].Count - 1 do
      begin
        p_id := SymbolTable.GetParamId(SubId, J);

        RC := TCodeRec.Create(OP_PUSH, Self);
        if SymbolTable[p_id].FinalTypeId = typeINTERFACE then
          RC.Arg1 := CreateConst(typePOINTER, SymbolTable[p_id].value)
        else
          RC.Arg1 := CreateConst(SymbolTable[p_id].TerminalTypeId,
            SymbolTable[p_id].value);
        RC.Arg2 := J;
        RC.Res := SubId;
        Insert(N, RC);

        Inc(N);
      end;
    end;

    // if subroutine has default parameter - recalculate p
    P.Clear;
    for J := GetStmt(N) to N do
      if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
        P_Add(J);

    if GetSymbolRec(SubId).CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL,
      ccMSFASTCALL] then
    begin
      L := TCodeRecList.Create;
      try
        for J := P.Count - 1 downto 0 do
        begin
          L.Add(Records[P[J]]);
          Remove(Records[P[J]]);
          Dec(N);
        end;

        for J := 0 to P.Count - 1 do
          Insert(N, TCodeRec(L[P.Count - J - 1]));
        Inc(N, P.Count);

        P.Clear();
        for J := GetStmt(N) to N do
          if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
            P_Add(J);
      finally
        FreeAndNil(L);
      end;
    end;

    if (SymbolTable[SubId].Kind <> KindCONSTRUCTOR) or
      (SymbolTable[R.Res].Name = '') then
    begin
      if (SymbolTable[SubId].Kind = KindCONSTRUCTOR) and (Records[N].Res = 0)
      then
        if not Records[N].IsInherited then
        begin
          T1 := SymbolTable[SubId].Level;
          if GetSymbolRec(T1).FinalTypeId = typeCLASS then
            CreateError(errWrongCall, []);
        end;

      if (SymbolTable[SubId].Kind = KindCONSTRUCTOR) and (PosClassRef > 0) then
      begin
        T1 := GetSymbolRec(Records[PosClassRef].Arg1).TerminalTypeId;
        T1 := GetSymbolRec(T1).PatternId;
        SymbolTable[R.Res].TypeId := T1;
      end
      else
        SymbolTable[R.Res].TypeId := SymbolTable[SubId].TypeId;
    end;

    R.Arg2 := P.Count;

    for J := P.Count - 1 downto 0 do
      if SubId = JS_FunctionCallId then
      begin
        R := Records[P[J]];
        if PAX64 then
          KZ := 0
        else
          KZ := P.Count - 1;
        if J = KZ then
        begin
          Records[N].Arg2 := 1; // only 1-st parameter is passed
          R.Op := OP_PUSH_INT_IMM;
        end
        else
        begin
          R.Op := OP_PUSH_ADDRESS;
          Arg2 := R.Arg1;
          T2 := GetSymbolRec(Arg2).FinalTypeId;
          if not(T2 in VariantTypes) then
          begin
            InsertConversionToVariant(P[J], 1);
            Inc(N);
          end;
        end;
      end
      else
      begin
        R := Records[P[J]];
        Arg1 := SymbolTable.GetParamId(SubId, R.Arg2);
        Arg2 := R.Arg1;

        T1 := GetSymbolRec(Arg1).FinalTypeId;
        T2 := GetSymbolRec(Arg2).FinalTypeId;
        K2 := GetSymbolRec(Arg2).Kind;

        if T2 = 0 then
          if GetLanguage(N) = JS_LANGUAGE then
          begin
            GetSymbolRec(Arg2).TypeId := typeVARIANT;
            T2 := typeVARIANT;
          end;

        if (K2 = KindTYPE) and (T2 = typeCLASS) then
        begin
          Inc(R.Arg1);
          Arg2 := R.Arg1;
          T2 := GetSymbolRec(Arg2).FinalTypeId;
          K2 := GetSymbolRec(Arg2).Kind;
        end;

        if GetSymbolRec(Arg1).ByRef and (not GetSymbolRec(Arg1).IsConst) then
        begin
          if (GetSymbolRec(Arg2).Name = '') and
            (not(T1 in [typeRECORD, typeARRAY])) then // 18.09.2009
          begin
            for K := N downto 1 do
            begin
              if Records[K].Op = OP_STMT then
                break

              else
              begin
                if Records[K].Res = Arg2 then
                begin
                  GenOp := Records[K].GenOp;
                  if Records[K].Op = OP_NOP then
                    GenOp := OP_NOP;

                  if (GenOp = OP_PLUS) or (GenOp = OP_MINUS) or
                    (GenOp = OP_MULT) or (GenOp = OP_DIV) or (GenOp = OP_IDIV)
                    or (GenOp = OP_MOD) or (GenOp = OP_SHL) or (GenOp = OP_SHR)
                    or (GenOp = OP_AND) or (GenOp = OP_OR) or (GenOp = OP_XOR)
                    or (GenOp = OP_NOT) or (GenOp = OP_IS) or (GenOp = OP_AS) or
                    (GenOp = OP_GT) or (GenOp = OP_GE) or (GenOp = OP_LT) or
                    (GenOp = OP_LE) or (GenOp = OP_EQ) or (GenOp = OP_NE) or
                    (GenOp = OP_CALL) then
                  begin
                    CreateError
                      (errTypesOfActualAndFormalVarParametersMustBeIdentical, []
                      );
                    break;
                  end;
                end;
              end;
            end;
          end
          else if GetSymbolRec(Arg2).Kind = KindCONST then
            CreateError(errConstantObjectCannotBePassedAsVarParameter, [])
          else if GetSymbolRec(Arg2).IsConst then
            CreateError(errConstantObjectCannotBePassedAsVarParameter, [])
          else if GetSymbolRec(Arg2).TypedConst then
            CreateError(errConstantObjectCannotBePassedAsVarParameter, []);

          if GetSymbolRec(Arg1).IsOut then
          begin
            InsertDestroyLocalVar(P[J], Arg2);
            Inc(N);
          end;

          R.Op := OP_PUSH_ADDRESS;

          if (T2 = typeINTERFACE) and (K2 = KindTYPE) and (T1 = typeINTERFACE)
          then
          begin
            R.Arg1 := GetSymbolRec(Arg2).TerminalTypeId + 1;
            continue;
          end
          else if (T2 = typeINTERFACE) and (K2 = KindTYPE) and
            (GetSymbolRec(Arg1).TerminalTypeId = H_TGUID) then
          begin
            R.Arg1 := GetSymbolRec(Arg2).TerminalTypeId + 1;
            continue;
          end;

          if T1 = typeOPENARRAY then
            T1 := typeDYNARRAY;

          if not((K2 = KindVAR) and ((T1 = T2) or GetSymbolRec(Arg1)
            .HasPVoidType)) then
          begin
            if T2 = typeVARIANT then
              if IsVarArrayIndex(Arg2, T1, J, P) then
                continue;

            CreateError
              (errTypesOfActualAndFormalVarParametersMustBeIdentical, []);
          end;

          if (T1 = typeDYNARRAY) and (T2 = typeDYNARRAY) then
          begin
            T1 := GetSymbolRec(Arg1).TerminalTypeId;
            T2 := GetSymbolRec(Arg2).TerminalTypeId;

            if GetSymbolRec(Arg1).IsOpenArray then
              R.Op := OP_PUSH_DYNARRAY;

            T1 := GetSymbolRec(T1).PatternId;
            T2 := GetSymbolRec(T2).PatternId;

            if (T1 = T2) or (T1 = 0) then
            begin
              // ok
            end
            else
              CreateError(errIncompatibleTypesNoArgs, []);

          end;
        end
        else if (T1 = typeEVENT) and (T2 = typeEVENT) then
        begin
          R.Op := OP_PUSH_EVENT;
        end
        else if (T1 = typeEVENT) and (T2 = typeRECORD) then
        begin
          R.Op := OP_PUSH_EVENT;
        end
        else if (T1 = typeEVENT) and (Arg2 = SymbolTable.NilId) then
        begin
          R.Arg1 := SymbolTable.EventNilId;
          R.Op := OP_PUSH_EVENT;
        end
        else if (T1 = typeRECORD) and (T2 = typeRECORD) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;
          if T1 <> T2 then
            if T1 <> H_TGUID then
              if T1 <> H_TVarRec then
              begin
                Dec(N);
                InsertConversionToRecord(P[J], 1,
                  GetSymbolRec(Arg1).TerminalTypeId, Arg2);
                Inc(N);
                Inc(N);
                R.Arg1 := Records[N].Res;
                Dec(N);

                Records[N_BeginCall].Op := OP_NOP;
                for I := 0 to P.Count - 1 do
                  Records[P[I]].Op := OP_PUSH;
                if PosInstance > 0 then
                  Records[PosInstance].Op := OP_PUSH_INSTANCE;
                if PosClassRef > 0 then
                  Records[PosInstance].Op := OP_PUSH_CLASSREF;

                Exit;
              end;

          if not StrEql(GetSymbolRec(T1).Name, GetSymbolRec(T2).Name) then
          begin
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
          // if GetSymbolRec(SubId).CallConv = ccREGISTER then
          // R.Op := OP_PUSH_ADDRESS
          // else
          R.Op := OP_PUSH_STRUCTURE;
        end
        else if GetSymbolRec(Arg1).HasPVoidType then
        begin
          R.Op := OP_PUSH_ADDRESS;
        end
        else if (T1 = typeARRAY) and (T2 = typeARRAY) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;
          if T1 <> T2 then
          begin
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
          R.Op := OP_PUSH_STRUCTURE;
        end
        else if (T1 = typeVARIANT) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_ADDRESS;
          if SymbolTable[SubId].Host then
            if SymbolTable[SubId].CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL]
            then
              if not SymbolTable[Arg1].IsConst then
                R.Op := OP_PUSH_STRUCTURE;
        end
        else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
        begin
          R.Op := OP_PUSH_ADDRESS;
          InsertConversionToVariant(P[J], 1);
          Inc(N);
          if SymbolTable[SubId].Host then
            if SymbolTable[SubId].CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL]
            then
              if not SymbolTable[Arg1].IsConst then
                R.Op := OP_PUSH_STRUCTURE;
        end
        else if (T1 = typeOLEVARIANT) and (T2 = typeOLEVARIANT) then
        begin
          R.Op := OP_PUSH_ADDRESS;
          if SymbolTable[SubId].Host then
            if SymbolTable[SubId].CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL]
            then
              if not SymbolTable[Arg1].IsConst then
                R.Op := OP_PUSH_STRUCTURE;
        end
        else if (T1 = typeOLEVARIANT) and (T2 <> typeOLEVARIANT) then
        begin
          R.Op := OP_PUSH_ADDRESS;
          InsertConversionToOleVariant(P[J], 1);
          Inc(N);
          if SymbolTable[SubId].Host then
            if SymbolTable[SubId].CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL]
            then
              if not SymbolTable[Arg1].IsConst then
                R.Op := OP_PUSH_STRUCTURE;
        end
        else if (T1 = typeBOOLEAN) and (T2 in BooleanTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_BYTE_IMM
          else
            R.Op := OP_PUSH_BYTE;
        end
        else if (T1 = typeBOOLEAN) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_BYTE;
          InsertConversionToBoolean(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeBYTEBOOL) and (T2 in BooleanTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_WORD_IMM
          else
            R.Op := OP_PUSH_WORD;
        end
        else if (T1 = typeBYTEBOOL) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_WORD;
          InsertConversionToByteBool(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWORDBOOL) and (T2 in BooleanTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_WORD_IMM
          else
            R.Op := OP_PUSH_WORD;
        end
        else if (T1 = typeWORDBOOL) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_WORD;
          InsertConversionToWordBool(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeLONGBOOL) and (T2 in BooleanTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
            R.Op := OP_PUSH_INT;
        end
        else if (T1 = typeLONGBOOL) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_INT;
          InsertConversionToLongBool(P[J], 1);
          Inc(N);
        end
{$IFNDEF PAXARM}
        else if (T1 = typeANSICHAR) and (T2 = typeANSICHAR) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_BYTE_IMM
          else
            R.Op := OP_PUSH_BYTE;
        end
        else if (T1 = typeANSICHAR) and (T2 = typeWIDECHAR) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_WORD_IMM
          else
            R.Op := OP_PUSH_WORD;
        end
{$ENDIF}
        else if (T1 = typeWIDECHAR) and (T2 in CharTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_WORD_IMM
          else
            R.Op := OP_PUSH_WORD;
        end
        else if (T1 = typeBYTE) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_BYTE_IMM
          else
            R.Op := OP_PUSH_BYTE;
        end
        else if (T1 = typeWORD) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_WORD_IMM
          else
            R.Op := OP_PUSH_WORD;
        end
        else if (T1 = typeCARDINAL) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_CARDINAL_IMM
          else
            R.Op := OP_PUSH_CARDINAL;
        end
        else if (T1 = typeCARDINAL) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_CARDINAL;
          InsertConversionToCardinal(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSMALLINT) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_SMALLINT_IMM
          else
            R.Op := OP_PUSH_SMALLINT;
        end
        else if (T1 = typeSMALLINT) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_SMALLINT;
          InsertConversionToSmallInt(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSHORTINT) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_SHORTINT_IMM
          else
            R.Op := OP_PUSH_SHORTINT;
        end
        else if (T1 = typeSHORTINT) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_SHORTINT;
          InsertConversionToShortInt(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeINTEGER) and (T2 in IntegerTypes) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
            R.Op := OP_PUSH_INT;
        end
        else if (T1 = typeINTEGER) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_INT;
          InsertConversionToInteger(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeBYTE) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_BYTE;
          InsertConversionToByte(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWORD) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_WORD;
          InsertConversionToWord(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeDOUBLE) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_DOUBLE;
          InsertConversionToDouble(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSINGLE) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_SINGLE;
          InsertConversionToSingle(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeEXTENDED) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_EXTENDED;
          InsertConversionToExtended(P[J], 1);
          Inc(N);
        end
{$IFNDEF PAXARM}
        else if (T1 = typeANSISTRING) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          InsertConversionToAnsiString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (T2 in VariantTypes) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (SymbolTable[Arg2].HasPWideCharType)
        then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
{$ENDIF}
        else if (T1 = typeCLASS) and (T2 = typeCLASS) then
        begin
          T1 := GetSymbolRec(Arg1).TypeId;
          T2 := GetSymbolRec(Arg2).TypeId;
          if not SymbolTable.Inherits(T2, T1) then
          begin
            if T1 <> typeCLASS then
              CreateError(errIncompatibleTypes,
                [GetSymbolRec(T1).Name, GetSymbolRec(T2).Name]);
          end;

          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
            R.Op := OP_PUSH_INT;
        end
        else if (T1 = typeCLASS) and (Arg2 = SymbolTable.NilId) then
        begin
          R.Op := OP_PUSH_INT_IMM;
        end
        else if (T1 = typeINTERFACE) and (T2 = typeINTERFACE) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
            R.Op := OP_PUSH_INT;
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;
          if not SymbolTable.Supports(T2, T1) then
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
        end
        else if (T1 = typeINTERFACE) and (T2 = typePOINTER) then
        begin
          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
        end
        else if (T1 = typeINTERFACE) and (Arg2 = SymbolTable.NilId) then
        begin
          R.Op := OP_PUSH_INT_IMM;
        end
        else if (T1 = typeINTERFACE) and (T2 = typeCLASS) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;
          if not SymbolTable.Supports(T2, T1) then
            CreateError(errIncompatibleTypesNoArgs, []);

          R.Op := OP_PUSH_INT;
          InsertConversionToInterface(P[J], 1,
            GetSymbolRec(Arg1).TerminalTypeId);
          Inc(N);
        end
        else if (T2 = typeINTERFACE) and (K2 = KindTYPE) and (T1 = typeINTERFACE)
        then
        begin
          R.Op := OP_PUSH_ADDRESS;
          R.Arg1 := GetSymbolRec(Arg2).TerminalTypeId + 1;
        end
        else if (T2 = typeINTERFACE) and (K2 = KindTYPE) and
          (GetSymbolRec(Arg1).TerminalTypeId = H_TGUID) then
        begin
          R.Op := OP_PUSH_ADDRESS;
          R.Arg1 := GetSymbolRec(Arg2).TerminalTypeId + 1;
        end
        else if (T1 = typeCLASSREF) and (T2 = typeCLASSREF) then
        begin
          T1 := GetSymbolRec(GetSymbolRec(Arg1).TerminalTypeId).PatternId;
          T2 := GetSymbolRec(GetSymbolRec(Arg2).TerminalTypeId).PatternId;
          if not SymbolTable.Inherits(T2, T1) then
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;

          if K2 = KindCONST then
            R.Op := OP_PUSH_INT_IMM
          else
            R.Op := OP_PUSH_INT;
        end
        else if (T1 = typeCLASSREF) and (Arg2 = SymbolTable.NilId) then
        begin
          R.Op := OP_PUSH_INT_IMM;
        end
        else if (T1 = typeCLASSREF) and (T2 = typeCLASS) then
        begin
          T1 := GetSymbolRec(GetSymbolRec(Arg1).TerminalTypeId).PatternId;
          T2 := Arg2;
          if (not SymbolTable.Inherits(T2, T1)) or (K2 <> KindTYPE) then
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;

          R.Op := OP_PUSH_INT;
          Inc(R.Arg1);
        end

        else if (T1 = typeOPENARRAY) and (T2 = typeOPENARRAY) then
        begin
          R.Op := OP_PUSH_OPENARRAY;

          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          T1 := GetSymbolRec(T1).PatternId;
          T2 := GetSymbolRec(T2).PatternId;

          if not((T1 = T2) or (T1 = 0)) then
            CreateError(errIncompatibleTypesNoArgs, []);
        end

        else if (T1 = typeOPENARRAY) and (T2 = typeDYNARRAY) then
        begin
          R.Op := OP_PUSH_DYNARRAY;

          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          T1 := GetSymbolRec(T1).PatternId;
          T2 := GetSymbolRec(T2).PatternId;

          if not((T1 = T2) or (T1 = 0)) then
            CreateError(errIncompatibleTypesNoArgs, []);
        end

        else if (T1 = typeOPENARRAY) and (T2 = typeARRAY) then
        begin
          R.Op := OP_PUSH_OPENARRAY;

          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          T1 := GetSymbolRec(T1).PatternId;
          SymbolTable.GetArrayTypeInfo(T2, K, T2);

          if not((T1 = T2) or (T1 = 0)) then
            CreateError(errIncompatibleTypesNoArgs, []);
        end

        else if (T1 = typeOPENARRAY) and (GetSymbolRec(Arg2).IsFWArrayVar) then
        begin
          R.Op := OP_PUSH_OPENARRAY;

          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          T1 := GetSymbolRec(T1).PatternId;
          T2 := GetSymbolRec(T2).PatternId;
          T2 := GetSymbolRec(T2).PatternId;

          if not((T1 = T2) or (T1 = 0)) then
            CreateError(errIncompatibleTypesNoArgs, []);
        end

        else if (T1 = typeDYNARRAY) and (T2 = typeDYNARRAY) then
        begin
          R.Op := OP_PUSH_DYNARRAY;

          T1 := GetSymbolRec(Arg1).TypeId;
          T2 := GetSymbolRec(Arg2).TypeId;

          T1 := GetSymbolRec(T1).PatternId;
          T2 := GetSymbolRec(T2).PatternId;

          if not((T1 = T2) or (T1 = 0)) then
            CreateError(errIncompatibleTypesNoArgs, []);
        end
        else if (T1 = typeDYNARRAY) and (Arg2 = SymbolTable.NilId) then
        begin
          R.Op := OP_PUSH_INT_IMM;
        end
        else if (T1 in [typeDYNARRAY, typeOPENARRAY]) and (T2 = typeSET) then
        begin
          if T1 = typeOPENARRAY then
          begin
            T1 := SymbolTable.RegisterDynamicArrayType(0, '',
              GetSymbolRec(GetSymbolRec(Arg1).TypeId).PatternId)
          end
          else
            T1 := GetSymbolRec(Arg1).TypeId;
          Card1 := Card;
          Arg2 := ConvertSetLiteralToDynarrayLiteral(GetSymbolRec(R.Arg1).Level,
            T1, R.Arg1);
          Card2 := Card;

          for J1 := P.Count - 1 downto 0 do
            P[J1] := P[J1] + (Card2 - Card1);

          if Arg2 = 0 then
          begin
            Arg2 := SymbolTable.AddDynarrayVar(GetLevel(N), T1).Id;
            R.Arg1 := Arg2;

            RC := TCodeRec.Create(OP_CREATE_EMPTY_DYNARRAY, Self);
            RC.Arg1 := R.Arg1;
            RC.Arg2 := 0;
            RC.Res := 0;

            Insert(P[J], RC);
            Inc(N);

            RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
            RC.Arg1 := GetLevel(N);
            RC.Arg2 := R.Arg1;
            RC.Res := 0;

            Insert(P[J], RC);
            Inc(N);

          end
          else
            R.Arg1 := Arg2;
          R.Op := OP_PUSH_DYNARRAY;
        end
        else if (T1 = typePOINTER) and
          (T2 in [typePOINTER, typeCLASS, typeCLASSREF]) then
        begin
          if GetSymbolRec(Arg1).HasPWideCharType then
          begin
            if K2 = KindCONST then
            begin
              if GetSymbolRec(Arg2).HasPWideCharType then
                R.Op := OP_PUSH_PWIDECHAR_IMM
{$IFNDEF PAXARM}
              else if GetSymbolRec(Arg2).HasPAnsiCharType then
              begin
                R.Op := OP_PUSH_WIDESTRING;
                InsertConversionToWideString(P[J], 1);
                Inc(N);
              end
{$ENDIF}
              else
                R.Op := OP_PUSH_INT_IMM;
            end
            else
              R.Op := OP_PUSH_INT;
          end
{$IFNDEF PAXARM}
          else if GetSymbolRec(Arg1).HasPAnsiCharType then
          begin
            if K2 = KindCONST then
            begin
              if GetSymbolRec(Arg2).HasPAnsiCharType then
                R.Op := OP_PUSH_PANSICHAR_IMM
              else if GetSymbolRec(Arg2).HasPWideCharType then
              begin
                R.Op := OP_PUSH_ANSISTRING;
                InsertConversionToAnsiString(P[J], 1);
                Inc(N);
              end
              else
                R.Op := OP_PUSH_INT_IMM;
            end
            else
              R.Op := OP_PUSH_INT;
          end
{$ENDIF}
          else
          begin
            if K2 = KindCONST then
              R.Op := OP_PUSH_INT_IMM
            else
              R.Op := OP_PUSH_INT;
          end;
        end
{$IFNDEF PAXARM}
        else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
        end
        else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
        end
        else if (T1 = typeANSISTRING) and (T2 = typePOINTER) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          if GetSymbolRec(Records[P[J]].Arg1).HasPAnsiCharType then
          begin
            InsertConversionToAnsiString(P[J], 1);
            Inc(N);
          end
          else if GetSymbolRec(Records[P[J]].Arg1).HasPWideCharType then
          begin
            InsertConversionToAnsiString(P[J], 1);
            Inc(N);
          end
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
        end
        else if (T1 = typeANSISTRING) and (T2 in CharTypes) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          InsertConversionToAnsiString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeANSISTRING) and (T2 = typeSHORTSTRING) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          InsertConversionToAnsiString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeANSISTRING) and (T2 = typeWIDESTRING) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          InsertConversionToAnsiString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSHORTSTRING) and (T2 = typePOINTER) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
          if (K2 = KindCONST) and GetSymbolRec(Records[P[J]].Arg1).HasPAnsiCharType
          then
          begin
            InsertConversionToShortString(P[J], 1);
            Inc(N);
          end
          else if (K2 = KindCONST) and GetSymbolRec(Records[P[J]].Arg1).HasPWideCharType
          then
          begin
            InsertConversionToShortString(P[J], 1);
            Inc(N);
          end
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
        end
        else if (T1 = typeSHORTSTRING) and (T2 in CharTypes) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
          InsertConversionToShortString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSHORTSTRING) and (T2 = typeANSISTRING) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
          InsertConversionToShortString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSHORTSTRING) and (T2 = typeWIDESTRING) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
          InsertConversionToShortString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
        end
        else if (T1 = typeWIDESTRING) and (T2 = typePOINTER) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          if (K2 = KindCONST) and GetSymbolRec(Records[P[J]].Arg1).HasPAnsiCharType
          then
          begin
            InsertConversionToWideString(P[J], 1);
            Inc(N);
          end
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
        end
        else if (T1 = typeWIDESTRING) and (T2 in CharTypes) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (T2 = typeANSISTRING) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (T2 = typeSHORTSTRING) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
{$ENDIF} // NOT PAXARM

        // unic string - begin

        else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
        begin
          R.Op := OP_PUSH_UNICSTRING;
        end
        else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
        begin
          R.Op := OP_PUSH_UNICSTRING;
          InsertConversionToUnicString(P[J], 1);
          Inc(N);
        end

        // unic string - end
{$IFNDEF PAXARM}
        else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
        begin
          R.Op := OP_PUSH_ANSISTRING;
          InsertConversionToAnsiString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
        begin
          R.Op := OP_PUSH_SHORTSTRING;
          InsertConversionToShortString(P[J], 1);
          Inc(N);
        end
        else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
        begin
          R.Op := OP_PUSH_WIDESTRING;
          InsertConversionToWideString(P[J], 1);
          Inc(N);
        end
{$ENDIF}
        else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
        begin
          R.Op := OP_PUSH_DOUBLE;
        end
        else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
        begin
          R.Op := OP_PUSH_DOUBLE;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeDOUBLE,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToDouble(P[J], 1);
            Inc(N);
          end;
        end
        else if (T1 = typeCURRENCY) and (T2 = typeCURRENCY) then
        begin
          R.Op := OP_PUSH_CURRENCY;
        end
        else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
        begin
          R.Op := OP_PUSH_CURRENCY;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeCURRENCY,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToCurrency(P[J], 1);
            Inc(N);
          end;
        end
        else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
        begin
          R.Op := OP_PUSH_SINGLE;
        end
        else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
        begin
          R.Op := OP_PUSH_SINGLE;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeSINGLE,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToSingle(P[J], 1);
            Inc(N);
          end;
        end
        else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
        begin
          R.Op := OP_PUSH_EXTENDED;
        end
        else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
        begin
          R.Op := OP_PUSH_EXTENDED;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeEXTENDED,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToExtended(P[J], 1);
            Inc(N);
          end;
        end
        else if (T1 in INT64Types) and (T2 in INT64Types) then
        begin
          R.Op := OP_PUSH_INT64;
        end
        else if (T1 = typeINT64) and (T2 in (NumberTypes + VariantTypes)) then
        begin
          R.Op := OP_PUSH_INT64;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeINT64,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToInt64(P[J], 1);
            Inc(N);
          end;
        end
        else if (T1 = typeUINT64) and (T2 in (NumberTypes + VariantTypes)) then
        begin
          R.Op := OP_PUSH_INT64;
          if K2 = KindCONST then
            Records[P[J]].Arg1 := CreateConst(typeUINT64,
              GetSymbolRec(Records[P[J]].Arg1).value)
          else
          begin
            InsertConversionToUInt64(P[J], 1);
            Inc(N);
          end;
        end
{$IFNDEF PAXARM}
        else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSISTRING)
        then
        begin
          R.Op := OP_PUSH_INT;
        end
        else if GetSymbolRec(Arg1).HasPAnsiCharType and
          SymbolTable.IsZerobasedAnsiCharArray(Arg2) then
        begin
          R.Op := OP_PUSH_ADDRESS;
        end
{$ENDIF}
        else if GetSymbolRec(Arg1).HasPWideCharType and
          SymbolTable.IsZerobasedWideCharArray(Arg2) then
        begin
          R.Op := OP_PUSH_ADDRESS;
        end
        else if (T1 = typeSET) and (T2 = typeSET) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          if not SymbolTable.CheckSetTypes(T1, T2) then
          begin
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;

          if SymbolTable.GetSizeOfSetType(T1) <= 4 then
            R.Op := OP_PUSH_SET
          else
            R.Op := OP_PUSH_ADDRESS;

        end
        else if (T1 = typePROC) and (T2 = typePROC) then
        begin
          R.Op := OP_PUSH_PTR;
        end
        else if (T1 = typePROC) and (T2 = typePOINTER) then
        begin
          R.Op := OP_PUSH_PTR;
        end
        else if (T1 = typePROC) and (K2 = KindSUB) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          SubId1 := TKernel(kernel).SymbolTable.GetPatternSubId(T1);
          SubId2 := Arg2;
          if not SymbolTable.EqualHeaders(SubId1, SubId2) then
            CreateError(errIncompatibleTypesNoArgs, []);
          R.Op := OP_PUSH_ADDRESS;
        end
        else if (T1 = typeENUM) and (T2 = typeENUM) then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          if T1 = T2 then
          begin
            R.Op := OP_PUSH_INT;
          end
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
        end
        else if (T2 = typeENUM) and (SubId = Id_WriteInt) then
        begin
          R.Op := OP_PUSH_INT;
        end
        else if T1 = typeRECORD then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;
          if T1 <> T2 then
            if T1 <> H_TGUID then
              if T1 <> H_TVarRec then
              begin
                Dec(N);
                InsertConversionToRecord(P[J], 1,
                  GetSymbolRec(Arg1).TerminalTypeId, Arg2);
                Inc(N);
                Inc(N);
                R.Arg1 := Records[N].Res;
                Dec(N);

                Records[N_BeginCall].Op := OP_NOP;
                for I := 0 to P.Count - 1 do
                  Records[P[I]].Op := OP_PUSH;
                if PosInstance > 0 then
                  Records[PosInstance].Op := OP_PUSH_INSTANCE;
                if PosClassRef > 0 then
                  Records[PosInstance].Op := OP_PUSH_CLASSREF;

                Exit;
              end;

          if not StrEql(GetSymbolRec(T1).Name, GetSymbolRec(T2).Name) then
          begin
            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
              GetSymbolRec(T2).Name]);
          end;
          if GetSymbolRec(SubId).CallConv = ccREGISTER then
            R.Op := OP_PUSH_ADDRESS
          else
            R.Op := OP_PUSH_STRUCTURE;
        end

        else if (SubId = Id_ImplicitInt) and (T1 = typeINTEGER) and
          (T2 = typeENUM) then
        begin
          R.Op := OP_PUSH_INT;
        end

        else
        begin
          if IsOverloadedCall then
          begin
            if P.Count = 1 then
            begin
              R := Records[P[0]];
              ParamId := R.Arg1;
              if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
                if StrEql('Length', SymbolTable[SubId].Name) then
                begin
                  Records[N_BeginCall].Op := OP_NOP;
                  Records[N_BeginCall].GenOp := OP_NOP;
                  R.Op := OP_HIGH;
                  R.Res := Records[N].Res;
                  Records[N].Op := OP_PLUS;
                  Records[N].Arg1 := R.Res;
                  Records[N].Arg2 := CreateConst(typeINTEGER, 1);
                  Records[N].Res := R.Res;
                  N := P[0] - 1;
                  Exit;
                end;
            end;

            CreateError(errThereIsNoOverloaded, [SymbolTable[SubId].Name]);
          end
          else
          begin
            T1 := GetSymbolRec(Arg1).TypeId;
            T2 := GetSymbolRec(Arg2).TypeId;

            if (SubId = JS_GetPropertyId) or (SubId = JS_PutPropertyId) then
            begin
              T2 := GetSymbolRec(Arg2).FinalTypeId;
              if K2 = KindCONST then
              begin
{$IFDEF UNIC}
                R.Op := OP_PUSH_PWIDECHAR_IMM;
{$ELSE}
                R.Op := OP_PUSH_PANSICHAR_IMM;
{$ENDIF}
                if T2 in IntegerTypes then
                begin
                  R.Op := OP_PUSH_INT_IMM;
                  if SubId = JS_GetPropertyId then
                  begin
                    Records[N_BeginCall].Arg1 := JS_GetArrPropertyId;
                    Records[N].Arg1 := JS_GetArrPropertyId;
                  end
                  else
                  begin
                    Records[N_BeginCall].Arg1 := JS_PutArrPropertyId;
                    Records[N].Arg1 := JS_PutArrPropertyId;
                  end;
                end
                else if T2 in BooleanTypes then
                begin
                  if GetSymbolRec(R.Arg1).value then
                    R.Arg1 := CreateConst(typeSTRING, 'true')
                  else
                    R.Arg1 := CreateConst(typeSTRING, 'false');
                end
                else if T2 in RealTypes then
                begin
                  STR(Extended(GetSymbolRec(R.Arg1).value), SS);
                  R.Arg1 := CreateConst(typeSTRING, String(SS));
                end
                else
                  CreateError(errIncompatibleTypesNoArgs, []);
              end
              else
              begin
                T2 := GetSymbolRec(Arg2).FinalTypeId;
                if T2 in IntegerTypes then
                begin
                  R.Op := OP_PUSH_INT;
                  if SubId = JS_GetPropertyId then
                  begin
                    Records[N_BeginCall].Arg1 := JS_GetArrPropertyId;
                    Records[N].Arg1 := JS_GetArrPropertyId;
                  end
                  else
                  begin
                    Records[N_BeginCall].Arg1 := JS_PutArrPropertyId;
                    Records[N].Arg1 := JS_PutArrPropertyId;
                  end;
                end
                else
                begin
{$IFDEF UNIC}
                  R.Op := OP_PUSH_UNICSTRING;
                  if T2 <> typeUNICSTRING then
                  begin
                    InsertConversionToUnicString(P[J], 1, JS_LANGUAGE);
                    Inc(N);
                  end;
{$ELSE}
                  R.Op := OP_PUSH_ANSISTRING;
                  if T2 <> typeANSISTRING then
                  begin
                    InsertConversionToAnsiString(P[J], 1, JS_LANGUAGE);
                    Inc(N);
                  end;
{$ENDIF}
                end;
              end;
            end
            else
            begin
              if GetLanguage(N) = JS_LANGUAGE then
              begin
                if (GetSymbolRec(T1).FinalTypeId = typeCLASS) and
                  (GetSymbolRec(T2).FinalTypeId = typeVARIANT) then
                begin
                  R.Op := OP_PUSH_INT;
                  InsertConversionToClass(P[J], 1, T1);
                  Inc(N);
                  continue;
                end;
              end;

              if GetSymbolRec(Arg1).HasFrameworkType then
              begin
                if Arg2 = TKernel(kernel).SymbolTable.NilId then
                begin
                  // ok
                end
                else if GetSymbolRec(Arg1).TerminalTypeId = GetSymbolRec(Arg2).TerminalTypeId
                then
                begin

                end
                else
                begin
                  InsertConversionToFrameworkClass(P[J], 1,
                    GetSymbolRec(Arg1).TerminalTypeId);
                  Inc(N);
                end;
                R.Op := OP_PUSH_INT;
                continue;
              end;

              if GetSymbolRec(Arg2).FinalTypeId = typeRECORD then
              begin
                InsertImplicitConversion(P[J], 1,
                  GetSymbolRec(Arg2).TerminalTypeId, T1);
                T1 := GetSymbolRec(Arg1).FinalTypeId;
                if T1 in OrdinalTypes then
                  R.Op := OP_PUSH_INT
                else
                  case T1 of
                    typeCLASS, typePOINTER, typeCLASSREF, typeINTERFACE:
                      R.Op := OP_PUSH_INT;
                    typeDYNARRAY:
                      R.Op := OP_PUSH_DYNARRAY;
                    typeSINGLE:
                      R.Op := OP_PUSH_SINGLE;
                    typeDOUBLE:
                      R.Op := OP_PUSH_DOUBLE;
                    typeEXTENDED:
                      R.Op := OP_PUSH_EXTENDED;
{$IFNDEF PAXARM}
                    typeANSISTRING:
                      R.Op := OP_PUSH_ANSISTRING;
                    typeWIDESTRING:
                      R.Op := OP_PUSH_WIDESTRING;
                    typeSHORTSTRING:
                      R.Op := OP_PUSH_SHORTSTRING;
{$ENDIF}
                    typeUNICSTRING:
                      R.Op := OP_PUSH_UNICSTRING;
                    typeVARIANT, typeOLEVARIANT:
                      begin
                        R.Op := OP_PUSH_ADDRESS;
                        if SymbolTable[SubId].Host then
                          if SymbolTable[SubId].CallConv
                            in [ccSTDCALL, ccCDECL, ccSAFECALL] then
                            if not SymbolTable[Arg1].IsConst then
                              R.Op := OP_PUSH_STRUCTURE;
                      end;
                  else
                    CreateError(errIncompatibleTypesNoArgs, [])
                  end;
                continue;
              end;

              if (GetSymbolRec(T1).Name = '') or (GetSymbolRec(T2).Name = '')
              then
                CreateError(errIncompatibleTypesNoArgs, [])
              else
                CreateError(errIncompatibleTypes,
                  [GetSymbolRec(T1).Name, GetSymbolRec(T2).Name]);
            end;
          end;
        end;
      end;

    if SymbolTable[SymbolTable[SubId].Level].Kind = KindSUB then
    // nested sub call
    begin
      RC := TCodeRec.Create(OP_POP, Self);
      RC.Arg1 := 0;
      RC.Arg2 := 0;
      RC.Res := 0;

      Insert(N + 1, RC);
      // Inc(N);
    end;
  finally
    if SavedSubId <> -1 then
    begin
      for J := GetStmt(N) to N do
        if (Records[J].Op = OP_PUSH) and (Records[J].Res = SubId) then
          Records[J].Res := SavedSubId
        else if (Records[J].Op = OP_BEGIN_CALL) and (Records[J].Arg1 = SubId)
        then
        begin
          Records[J].Arg1 := SavedSubId;
        end;

      RR.Arg1 := SavedSubId;

      if GetSymbolRec(SavedSubId).FinalTypeId = typeEVENT then
      begin

        RC := TCodeRec.Create(OP_PUSH_DATA, Self);
        RC.Arg1 := SavedSubId;
        RC.Arg2 := 0;
        RC.Res := SubId;

        Insert(N, RC);
        Inc(N);

      end;

    end;
    FreeAndNil(P);

    Id_VMT := 0;

    if (PosInstance = 0) and (PosClassRef <> 0) then
    begin
      T1 := GetSymbolRec(SubId).Level;
      if T1 > 0 then
        if GetSymbolRec(T1).Kind = KindTYPE then
          if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
            if GetSymbolRec(T1).FinalTypeId = typeRECORD then
            begin
              PosInstance := PosClassRef;
              PosClassRef := 0;
              Records[PosInstance].Arg1 := Records[N].Res;
            end;
    end;

    if PosInstance <> 0 then
    begin
      Records[PosInstance].Op := OP_NOP;

      RC := TCodeRec.Create(OP_PUSH_INST, Self);
      RC.Arg1 := Records[PosInstance].Arg1;
      RC.Arg2 := 0;
      RC.Res := SubId;
      RC.CodeRecTag := Records[PosInstance].CodeRecTag;

      Insert(N, RC);
      Inc(N);

      if not Records[N].IsStatic then
        if GetSymbolRec(SubId).IsVirtual then
        begin
          Id_VMT := NewTempVar(GetLevel(N), typePOINTER);

          RC := TCodeRec.Create(OP_GET_VMT_ADDRESS, Self);
          RC.Arg1 := Records[PosInstance].Arg1;
          RC.Arg2 := SubId;
          RC.Res := Id_VMT;

          Insert(N, RC);
          Inc(N);
        end;
    end;

    if PosClassRef <> 0 then
    begin
      Records[PosClassRef].Op := OP_NOP;

      T1 := GetSymbolRec(SubId).Level;
      if GetSymbolRec(T1).Kind = KindTYPE then
      begin
        if GetSymbolRec(T1).Host then
        begin
          RC := TCodeRec.Create(OP_PUSH_CLSREF, Self);

          if GetSymbolRec(Records[PosClassRef].Arg1).Kind = KindVAR then
            RC.Arg1 := Records[PosClassRef].Arg1
          else
            RC.Arg1 := T1 + 1;

          RC.Arg2 := 0;
          RC.Res := SubId;

          Insert(N, RC);
          Inc(N);

          if GetSymbolRec(Records[PosClassRef].Arg1).Kind = KindVAR then
            if GetSymbolRec(Records[PosClassRef].Arg1).Name <> '' then
              if GetSymbolRec(SubId).IsVirtual then
              begin

                Id_VMT := NewTempVar(GetLevel(N), typePOINTER);

                RC := TCodeRec.Create(OP_GET_VMT_ADDRESS, Self);
                RC.Arg1 := Records[PosClassRef].Arg1;
                RC.Arg2 := SubId;
                RC.Res := Id_VMT;

                Insert(N, RC);
                Inc(N);
              end;

        end
        else if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
        begin
          {
            J1 := NewTempVar(GetLevel(N), T1);

            RC := TCodeRec.Create(OP_CREATE_OBJECT, GetUpcase(N), GetLanguage(N),
            GetModuleNumber(N));
            RC.Arg1 := T1;
            RC.Arg2 := 0;
            RC.Res  := J1;

            Insert(N_BeginCall, RC);
            Inc(N);

            RC := TCodeRec.Create(OP_PUSH_INST, Self);
            RC.Arg1 := J1;
            RC.Arg2 := 0;
            RC.Res := SubId;
          }

          RC := TCodeRec.Create(OP_PUSH_CLSREF, Self);
          if GetSymbolRec(Records[PosClassRef].Arg1).Kind = KindVAR then
            RC.Arg1 := Records[PosClassRef].Arg1
          else
            RC.Arg1 := T1 + 1;
          RC.Arg2 := 0;
          RC.Res := SubId;

          Insert(N, RC);
          Inc(N);

          if GetSymbolRec(SubId).IsVirtual then
          begin
            Id_VMT := NewTempVar(GetLevel(N), typePOINTER);

            RC := TCodeRec.Create(OP_GET_VMT_ADDRESS, Self);
            RC.Arg1 := Records[PosClassRef].Arg1;
            RC.Arg2 := SubId;
            RC.Res := Id_VMT;

            Insert(N, RC);
            Inc(N);
          end;

        end
        else
        begin
          RC := TCodeRec.Create(OP_PUSH_CLSREF, Self);
          if GetSymbolRec(Records[PosClassRef].Arg1).Kind = KindVAR then
            RC.Arg1 := Records[PosClassRef].Arg1
          else
            RC.Arg1 := T1 + 1;
          RC.Arg2 := 0;
          RC.Res := SubId;

          Insert(N, RC);
          Inc(N);

          if GetSymbolRec(SubId)
            .IsVirtual { and (GetSymbolRec(SubId).Kind = KindSUB) } then
          begin
            Id_VMT := NewTempVar(GetLevel(N), typePOINTER);

            RC := TCodeRec.Create(OP_GET_VMT_ADDRESS, Self);
            RC.Arg1 := Records[PosClassRef].Arg1;
            RC.Arg2 := SubId;
            RC.Res := Id_VMT;

            Insert(N, RC);
            Inc(N);
          end;

        end;
      end
      else
        RaiseError(errInternalError, []);
    end;

    if Records[N].Res = 0 then
      if GetSymbolRec(Records[N].Arg1).FinalTypeId in RealTypes then
      begin
        Records[N].Res := NewTempVar(GetLevel(N), GetSymbolRec(Records[N].Arg1)
          .FinalTypeId);
      end;

    if Records[N].Res > 0 then
    begin
      if GetSymbolRec(Records[N].Res).TypeId = 0 then
        GetSymbolRec(Records[N].Res).TypeId := GetSymbolRec(SubId).TypeId;
      if GetSymbolRec(Records[N].Res).FinalTypeId
        in (DynamicTypes + [typeRECORD, typeARRAY]) then
      begin
        if GetSymbolRec(Records[N].Res).FinalTypeId = typeINTERFACE then
          if GetSymbolRec(Records[N].Res).Name = '' then
          begin
            RC := TCodeRec.Create(OP_INTERFACE_CLR, Self);
            RC.Arg1 := Records[N].Res;
            RC.Arg2 := 0;
            RC.Res := 0;
            Insert(N_BeginCall + 1, RC);
            Inc(N);
          end;

        RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
        RC.Arg1 := GetSymbolRec(Records[N].Res).Level;
        RC.Arg2 := Records[N].Res;
        RC.Res := 0;
        Insert(N, RC);
        Inc(N);
      end;
    end
    else
    begin
      Id := Records[N].Arg1;

      if GetSymbolRec(Id).Kind = KindCONSTRUCTOR then
      begin
        J := GetSymbolRec(Id).Level;
        if GetSymbolRec(J).FinalTypeId = typeRECORD then
          T1 := 0
        else
          T1 := GetSymbolRec(Id).FinalTypeId;
      end
      else
        T1 := GetSymbolRec(Id).FinalTypeId;

      if T1 in (DynamicTypes + [typeRECORD, typeARRAY]) then
      begin
        case T1 of
{$IFNDEF PAXARM}
          typeANSISTRING:
            Records[N].Res := CreateStringVar(GetLevel(N));
          typeWIDESTRING:
            Records[N].Res := CreateWideStringVar(GetLevel(N));
{$ENDIF}
          typeUNICSTRING:
            Records[N].Res := CreateUnicStringVar(GetLevel(N));
          typeVARIANT:
            Records[N].Res := CreateVariantVar(GetLevel(N));
          typeOLEVARIANT:
            Records[N].Res := CreateOleVariantVar(GetLevel(N));
          typeDYNARRAY:
            Records[N].Res := CreateDynarrayVar(GetLevel(N),
              GetSymbolRec(Records[N].Arg1).TypeId);
          typeINTERFACE:
            Records[N].Res := CreateInterfaceVar(GetLevel(N));
          typeRECORD, typeARRAY:
            Records[N].Res := NewTempVar(GetLevel(N),
              GetSymbolRec(Records[N].Arg1).TerminalTypeId);
        end;
        if GetSymbolRec(Records[N].Res).FinalTypeId = typeINTERFACE then
        begin
          RC := TCodeRec.Create(OP_INTERFACE_CLR, Self);
          RC.Arg1 := Records[N].Res;
          RC.Arg2 := 0;
          RC.Res := 0;
          Insert(N_BeginCall + 1, RC);
          Inc(N);
        end;
        RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
        RC.Arg1 := GetSymbolRec(Records[N].Res).Level;
        RC.Arg2 := Records[N].Res;
        RC.Res := 0;
        Insert(N, RC);
        Inc(N);
      end
      else if GetSymbolRec(Records[N].Arg1).FinalTypeId > typeVOID then
      begin // this is a function
        Records[N].Res := NewTempVar(GetLevel(N), GetSymbolRec(Records[N].Arg1)
          .FinalTypeId);
      end;
    end;

    if Id_VMT > 0 then
      Records[N].Arg1 := Id_VMT;
  end;
end;

procedure TCode.OperTerminal;
var
  TypeId: Integer;
begin
  TypeId := GetSymbolRec(Records[N].Arg1).TypeId;
  TypeId := GetSymbolRec(TypeId).PatternId;
  GetSymbolRec(Records[N].Res).TypeId := TypeId;
  GetSymbolRec(Records[N].Res).ByRef := true;
end;

procedure TCode.OperAddMethodIndex;
var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  I, SubId, IntfType: Integer;
begin
  R := Records[N];
  R.Op := OP_NOP;
  SymbolTable := TKernel(kernel).SymbolTable;
  SubId := R.Arg1;
  IntfType := SymbolTable[SubId].Level;
  I := SymbolTable.RestorePositiveIndex(IntfType);
  I := I + Abs(R.Arg2);
  SymbolTable[SubId].MethodIndex := I;
  SymbolTable[SubId].NegativeMethodIndex := -R.Arg2;
end;

procedure TCode.OperAddMessage;

  function SetIndex(K: Integer): Integer;
  var
    R: TMessageRec;
    V: Variant;
    SubId, I, J, Id, Level: Integer;
    L: TIntegerList;
    SymbolTable: TSymbolTable;
    SymbolRec: TSymbolRec;
  begin
    result := 0;

    SubId := Records[K].Arg1;
    if GetSymbolRec(Records[K].Arg2).Kind = KindVAR then
    begin
      Records[K].Res := CreateConst(typeSTRING, GetSymbolRec(SubId).FullName);
      Exit; // later, at run-time
    end
    else
    begin
      V := GetSymbolRec(Records[K].Arg2).value;
    end;

    try
      I := V;
    except
      RaiseError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;

    if I = -1000 then // it was not set up yet
    begin
      SymbolTable := TKernel(kernel).SymbolTable;
      Level := GetSymbolRec(SubId).Level;
      I := 0;
      Id := 0;

      if GetSymbolRec(SubId).CallMode = cmDYNAMIC then
      begin
        for J := Level + 1 to SubId do
        begin
          SymbolRec := SymbolTable[J];
          if SymbolRec.CallMode = cmDYNAMIC then
            if SymbolRec.Level = Level then
              Dec(I);
        end;
      end;

      if (GetSymbolRec(SubId).Name = '') and
        (GetSymbolRec(SubId).Kind = KindCONSTRUCTOR) then
        L := SymbolTable.LookupParentConstructors(SubId)
      else
        L := SymbolTable.LookupParentMethods(SubId, GetUpcase(N));

      try
        for J := 0 to L.Count - 1 do
        begin
          Id := L[J];
          SymbolRec := GetSymbolRec(Id);
          if SymbolRec.CallMode in [cmDYNAMIC, cmOVERRIDE] then
            if SymbolRec.DynamicMethodIndex <> 0 then
            begin
              I := SymbolRec.DynamicMethodIndex;
              break;
            end;
        end;

        if I = 0 then
        begin
          Records[K].Op := OP_NOP;
          Exit;
        end;

        if I = -1000 then
        begin
          for J := 1 to Card do
            if Records[J].Op = OP_ADD_MESSAGE then
              if Records[J].Arg1 = Id then
              begin
                I := SetIndex(J);
                break;
              end;
        end;

      finally
        FreeAndNil(L);
      end;
    end;

    GetSymbolRec(SubId).DynamicMethodIndex := I;
    R := TKernel(kernel).MessageList.AddRecord;
    R.msg_id := I;
    R.FullName := GetSymbolRec(SubId).FullName;
    Records[K].Op := OP_NOP;

    result := I;
  end;

begin
  SetIndex(N);
end;

procedure TCode.OperCreateMethod;
var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  TypeId, PatternFieldId, AddressId: Integer;
  S: String;
  RC: TCodeRec;
begin
  R := Records[N];

  if GetSymbolRec(R.Arg1).Kind <> KindVAR then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  SymbolTable := TKernel(kernel).SymbolTable;
  TypeId := GetSymbolRec(R.Arg1).TerminalTypeId;
  S := GetSymbolRec(R.Arg2).Name;
  PatternFieldId := SymbolTable.LookUp(S, TypeId, GetUpcase(N));

  if PatternFieldId = 0 then
  begin
    CreateError(errUndeclaredIdentifier, [S]);
    Exit;
  end;

  if GetSymbolRec(PatternFieldId).Kind <> KindSUB then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  AddressId := SymbolTable.AddPointerVar(GetLevel(N)).Id;

  GetSymbolRec(R.Arg2).Kind := KindNONE;
  R.Arg2 := AddressId;

  RC := TCodeRec.Create(OP_ADDRESS, Self);
  RC.Arg1 := PatternFieldId;
  RC.Arg2 := 0;
  RC.Res := AddressId;

  Insert(N, RC);
  Inc(N);
end;

procedure TCode.MoveRValue(N_ASS: Integer);
var
  I, J: Integer;
  L: TCodeRecList;
  RI: TCodeRec;
begin
  L := TCodeRecList.Create;
  try
    I := N_ASS - 1;
    while Records[I].Op <> OP_LVALUE do
    begin
      L.Insert(0, Records[I].Clone);
      Records[I].Op := OP_NOP;
      Dec(I);
    end;

    J := N;

    for I := 0 to L.Count - 1 do
    begin
      RI := TCodeRec(L[I]);
      Insert(J, RI);
      Inc(J);
    end;

  finally
    FreeAndNil(L);
  end;
end;

procedure TCode.MoveLValue(N_ASS: Integer);
var
  L: TCodeRecList;
  I: Integer;
  RI: TCodeRec;
begin
  L := TCodeRecList.Create;
  try
    for I := N to N_ASS - 1 do
    begin
      RI := Records[I];
      if RI.Op = OP_LVALUE then
        break;
      L.Add(RI.Clone);
      RI.Op := OP_NOP;
    end;

    for I := L.Count - 1 downto 0 do
    begin
      RI := TCodeRec(L[I]);
      Insert(N_ASS, RI);
    end;

  finally
    FreeAndNil(L);
  end;
end;

procedure TCode.InsertDeclareTempVar;
var
  R, RC: TCodeRec;
begin
  R := Records[N];
  if GetSymbolRec(R.Res).Name = '' then
  begin
    RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RC.Arg1 := GetLevel(N);
    RC.Arg2 := R.Res;
    RC.Res := 0;
    Insert(N, RC);
    Inc(N);
  end;
end;

procedure TCode.InsertDeclareTempVar(Id: Integer);
var
  RC: TCodeRec;
begin
  RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RC.Arg1 := GetLevel(N);
  RC.Arg2 := Id;
  RC.Res := 0;
  Insert(N, RC);
  Inc(N);
end;

function TCode.CallExpected(ResId: Integer): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := N + 1 to Card do
    with Records[I] do
    begin
      if Op = OP_STMT then
        break;
      if (Op = OP_CALL) and (Arg1 = ResId) then
      begin
        result := true;
        Exit;
      end;
    end;
end;

function TCode.InheritedExpected(ResId: Integer): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := N + 1 to Card do
    with Records[I] do
    begin
      if Op = OP_STMT then
        break;

      if (Op = OP_EVAL_INHERITED) and (Arg1 = ResId) then
      begin
        result := true;
        Exit;
      end;
    end;
end;

procedure TCode.OperVCall;
var
  VarArgIds: TIntegerList;
  Id_Object, Id_Prop, SubId, ResId, I, DynArrayId, ParamId, Cnt: Integer;
  RC: TCodeRec;
  S: String;
begin
  Id_Object := 0;
  Id_Prop := 0;
  SubId := Records[N].Arg1;
  ResId := Records[N].Res;
  Cnt := Records[N].Arg2;
  if ResId = 0 then
    ResId := NewTempVar(GetLevel(N), typeVARIANT)
  else
    GetSymbolRec(ResId).TypeId := typeVARIANT;
  VarArgIds := TIntegerList.Create;

  I := N;
  repeat
    Dec(I);
    if Records[I].Op = OP_BEGIN_VCALL then
      if Records[I].Res = SubId then
      begin
        S := GetSymbolRec(Records[I].Arg1).Name;
        Id_Object := CreateConst(typeSTRING, S);

        S := GetSymbolRec(Records[I].Arg2).Name;
        Id_Prop := CreateConst(typeSTRING, S);

        Records[I].Op := OP_NOP;
        Records[I].GenOp := OP_NOP;

        Records[N].Op := OP_NOP;
        Records[N].GenOp := OP_NOP;

        break;
      end;

    if Records[I].Op = OP_PUSH then
      if Records[I].Res = SubId then
      begin
        VarArgIds.Insert(0, Records[I].Arg1);

        Records[I].Op := OP_NOP;
        Records[I].GenOp := OP_NOP;
      end;
  until false;

  if Cnt = VarArgIds.Count then
    SubId := Id_CallVirt
  else
  begin
    SubId := Id_PutVirt;
    ResId := VarArgIds.Top;
    VarArgIds.RemoveAt(VarArgIds.Count - 1);
  end;

  Inc(N);

  try
    DynArrayId := NewTempVar(GetLevel(N), H_DYN_VAR);

    RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
    RC.Arg1 := GetLevel(N);
    RC.Arg2 := DynArrayId;
    RC.Res := 0;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_SET_LENGTH, Self);
    RC.Arg1 := DynArrayId;
    RC.Arg2 := CreateConst(typeINTEGER, VarArgIds.Count);
    RC.Res := 0;
    Insert(N, RC);
    Inc(N);

    for I := 0 to VarArgIds.Count - 1 do
    begin
      RC := TCodeRec.Create(OP_ELEM, Self);
      RC.Arg1 := DynArrayId;
      RC.Arg2 := CreateConst(typeINTEGER, I);
      ParamId := NewTempVar(GetLevel(N), typeVARIANT);
      RC.Res := ParamId;

      Insert(N, RC);
      Inc(N);

      RC := TCodeRec.Create(OP_ASSIGN, Self);
      RC.Arg1 := ParamId;
      RC.Arg2 := VarArgIds[I];
      RC.Res := ParamId;

      Insert(N, RC);
      Inc(N);
    end;

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := Id_Object;
    RC.Arg2 := 0;
    RC.Res := SubId;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := Id_Prop;
    RC.Arg2 := 1;
    RC.Res := SubId;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := DynArrayId;
    RC.Arg2 := 2;
    RC.Res := SubId;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := ResId;
    RC.Arg2 := 3;
    RC.Res := SubId;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_CALL, Self);
    RC.Arg1 := SubId;
    RC.Arg2 := 4;
    RC.Res := SubId;
    Insert(N, RC);
    Inc(N);

    Dec(N, 8 + VarArgIds.Count * 2);

  finally
    FreeAndNil(VarArgIds);
  end;
end;

procedure TCode.OperField;

var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  OldArg1: Integer;

  procedure CheckVis(PatternFieldId: Integer);

    function IsExtraByteCode: Boolean;
    var
      I, Op: Integer;
    begin
      result := false;
      for I := N downto 1 do
      begin
        Op := Records[I].Op;
        if Op = OP_EXTRA_BYTECODE then
        begin
          result := true;
          Exit;
        end;
      end;
    end;

  var
    Id, I1, I2, L, ClassId: Integer;
    Vis: TClassVisibility;
    OwnerTypeId, PatternTypeId, Lang, K: Integer;
  begin
    Vis := GetSymbolRec(PatternFieldId).Vis;
    if Vis in [cvPrivate, cvStrictPrivate] then
      used_private_members.Add(PatternFieldId);

    Id := OldArg1;

    if GetSymbolRec(Id).Name = '' then
      Exit;

    I1 := TKernel(kernel).Modules.IndexOfModuleById(Id);
    I2 := TKernel(kernel).Modules.IndexOfModuleById(PatternFieldId);

    Lang := GetLanguage(N);

    if I1 = I2 then // the same module
    begin
      if Lang = PASCAL_LANGUAGE then
      begin
        if Vis = cvStrictPrivate then
        begin
          PatternTypeId := GetSymbolRec(PatternFieldId).Level;
          case GetSymbolRec(PatternTypeId).FinalTypeId of
            typeCLASS:
              OwnerTypeId := GetClassId(N);
            typeRECORD:
              OwnerTypeId := GetStructureId(N);
          else
            Exit;
          end;

          if PatternTypeId <> OwnerTypeId then
            CreateError(errProtectionLevel,
              [GetSymbolRec(PatternFieldId).Name]);
        end;
        if I1 <= 0 then
          Exit;
        K := GetSymbolRec(Id).Kind;
        if K <> KindTYPE then
          Exit;
      end;
    end;

    if Vis = cvPrivate then
    begin
      if Lang in [BASIC_LANGUAGE, JAVA_LANGUAGE] then
      begin
        PatternTypeId := GetSymbolRec(PatternFieldId).Level;
        case GetSymbolRec(PatternTypeId).FinalTypeId of
          typeCLASS:
            OwnerTypeId := GetClassId(N);
          typeRECORD:
            OwnerTypeId := GetStructureId(N);
        else
          Exit;
        end;

        if PatternTypeId <> OwnerTypeId then
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
      end
      else
      begin
        if not IsExtraByteCode then
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
      end;
    end
    else if Vis in [cvProtected, cvStrictProtected] then
    begin
      if Vis = cvStrictProtected then
      begin
        PatternTypeId := GetSymbolRec(PatternFieldId).Level;
        case GetSymbolRec(PatternTypeId).FinalTypeId of
          typeCLASS:
            OwnerTypeId := GetClassId(N);
          typeRECORD:
            OwnerTypeId := GetStructureId(N);
        else
          Exit;
        end;

        if PatternTypeId <> OwnerTypeId then
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
      end;

      L := GetSymbolRec(R.Arg1).TerminalTypeId;
      if I1 = TKernel(kernel).Modules.IndexOfModuleById(L) then
        Exit;

      Id := GetLevel(N);

      if Id = 0 then
        if not IsExtraByteCode then
        begin
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
          Exit;
        end;

      L := GetSymbolRec(Id).Level;

      if L = 0 then
        if not IsExtraByteCode then
        begin
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
          Exit;
        end;

      while GetSymbolRec(L).Kind in kindSUBS do
      begin
        L := GetSymbolRec(L).Level;
        if L = 0 then
          RaiseError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
      end;

      if GetSymbolRec(L).Kind <> KindTYPE then
        if not IsExtraByteCode then
        begin
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
          Exit;
        end;

      if GetSymbolRec(L).FinalTypeId = typeCLASS then
      begin
        ClassId := GetSymbolRec(PatternFieldId).Level;
        if not SymbolTable.Inherits(L, ClassId) then
        begin
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
        end;
      end
      else
      begin
        if not IsExtraByteCode then
          CreateError(errProtectionLevel, [GetSymbolRec(PatternFieldId).Name]);
      end;
    end;
  end;

  procedure InsertCall;
  var
    SubId: Integer;
    RC: TCodeRec;
  begin
    SubId := R.Res;

    RC := TCodeRec.Create(OP_CALL, Self);
    RC.Arg1 := SubId;
    RC.Arg2 := 0;
    if GetSymbolRec(SubId).TypeId = typeVOID then
      RC.Res := 0
    else
      RC.Res := NewTempVar(GetLevel(N), GetSymbolRec(SubId).TypeId);

    Insert(N + 1, RC);
    ReplaceIdEx(SubId, RC.Res, N + 2, GetNextStmt(N + 2), true);
  end;

  procedure InsertCallEx(I, NParams: Integer);
  var
    SubId: Integer;
    RC: TCodeRec;
  begin
    SubId := R.Res;

    RC := TCodeRec.Create(OP_CALL, Self);
    RC.Arg1 := SubId;
    RC.Arg2 := NParams;
    if GetSymbolRec(SubId).TypeId = typeVOID then
      RC.Res := 0
    else
      RC.Res := NewTempVar(GetLevel(N), GetSymbolRec(SubId).TypeId);

    Insert(I + 1, RC);
    ReplaceIdEx(SubId, RC.Res, I + 2, Card, true);
  end;

  function ReplaceOP_ELEM(SubId: Integer; var K: Integer;
    var OldRes: Integer): Integer;
  var
    I, IndexId, ParCount: Integer;
    R: TCodeRec;
  begin
    result := N;
    K := -1;
    OldRes := SubId;

    ParCount := GetSymbolRec(SubId).Count;

    for I := N + 1 to Card do
    begin
      R := Records[I];

      if R.Op = OP_STMT then
        break;

      if (R.Op = OP_ELEM) and (R.Arg1 = OldRes) then
      begin
        result := I;
        OldRes := R.Res;
        IndexId := R.Arg2;
        Inc(K);

        GetSymbolRec(OldRes).Kind := 0;

        R.GenOp := OP_PUSH;
        R.Op := OP_PUSH;
        R.Arg1 := IndexId;
        R.Arg2 := K;
        R.Res := SubId;

        if ParCount - 1 = K then
          break;
      end;
    end;
  end;

  function InsertCreateEvent(PatternFieldId: Integer): Boolean;
  var
    J, J1, N1, ParamId: Integer;
    S1, S2: String;
  begin
    result := false;

    N1 := N + 1;

    J := N;
    repeat
      Inc(J);
      if J > Card then
        break;

      if Records[J].Op = OP_STMT then
        break;

      if Records[J].Op = OP_ASSIGN then
      begin
        N1 := J;
        break;
      end;
    until false;

    if Records[N1].Op = OP_ASSIGN then
    begin
      if Records[N1].Arg2 = Records[N].Res then
        if GetSymbolRec(Records[N1].Arg1).FinalTypeId = typeEVENT then
        begin
          S1 := GetSymbolRec(PatternFieldId).SignatureSimple;
          J1 := GetSymbolRec(Records[N1].Arg1).TerminalTypeId;
          J1 := GetSymbolRec(J1).PatternId;
          S2 := GetSymbolRec(J1).SignatureSimple;

          if not StrEql(S1, S2) then
            CreateError(errIncompatibleTypesNoArgs, []);

          Records[N].Op := OP_CREATE_EVENT;
          R.Arg2 := PatternFieldId;
          GetSymbolRec(R.Res).OwnerId := 0;
          GetSymbolRec(R.Res).TypeId := GetSymbolRec(Records[N1].Arg1).TypeId;
          GetSymbolRec(R.Res).Name := '';

          result := true;

          Exit;
        end;
    end;
    J := N;
    repeat
      Inc(J);
      if J > Card then
        break;

      if Records[J].Op = OP_STMT then
        break;

      if Records[J].Op = OP_PUSH then
        if Records[J].Arg1 = Records[N].Res then
          if GetSymbolRec(Records[J].Res).Kind in kindSUBS then
          begin
            if SymbolTable[Records[J].Res].Count = 0 then
              break;

            ParamId := SymbolTable.GetParamId(Records[J].Res, Records[J].Arg2);
            if GetSymbolRec(ParamId).FinalTypeId = typeEVENT then
            begin
              S1 := GetSymbolRec(PatternFieldId).SignatureSimple;
              J1 := GetSymbolRec(ParamId).TerminalTypeId;
              J1 := GetSymbolRec(J1).PatternId;
              S2 := GetSymbolRec(J1).SignatureSimple;

              if not StrEql(S1, S2) then
                CreateError(errIncompatibleTypesNoArgs, []);

              Records[N].Op := OP_CREATE_EVENT;
              R.Arg2 := PatternFieldId;
              GetSymbolRec(R.Res).OwnerId := 0;
              GetSymbolRec(R.Res).TypeId := GetSymbolRec(ParamId).TypeId;
              GetSymbolRec(R.Res).Name := '';

              result := true;

              Exit;
            end;

            break;
          end;
    until false;
  end;

  procedure ProcessVOBJECT(ResId: Integer);
  var
    I, J, Res, K, IndexId: Integer;
    RC: TCodeRec;
    has_assign: Boolean;
  begin
    K := 0;
    if CallExpected(ResId) then
    begin
      Records[N].Op := OP_BEGIN_VCALL;
      Records[N].GenOp := OP_BEGIN_VCALL;
      for I := N + 1 to Card do
      begin
        if Records[I].Op = OP_STMT then
          break;

        if Records[I].Op = OP_PUSH then
          if Records[I].Res = ResId then
          begin
            Inc(K);
          end;

        if Records[I].Op = OP_CALL then
          if Records[I].Arg1 = ResId then
          begin
            if GetLanguage(N) = BASIC_LANGUAGE then
            begin
              if Records[I + 1].Op = OP_LVALUE then
              begin
                Res := Records[I].Res;
                for J := I + 1 to Card do
                begin
                  if Records[J].Op = OP_ASSIGN then
                    if Records[J].Arg1 = Res then
                    begin
                      Records[I].Op := OP_NOP;
                      Records[I].GenOp := OP_NOP;

                      IndexId := Records[J].Arg2;

                      Records[J].Op := OP_PUSH;
                      Records[J].GenOp := OP_PUSH;
                      Records[J].Arg1 := IndexId;
                      Records[J].Arg2 := K;
                      Records[J].Res := ResId;

                      RC := TCodeRec.Create(OP_VCALL, Self);
                      RC.Arg1 := ResId;
                      RC.Arg2 := K;
                      RC.Res := 0;
                      Insert(J + 1, RC);
                      Exit;
                    end;
                end;
              end;
            end;

            Records[I].Op := OP_VCALL;
            Records[I].GenOp := OP_VCALL;
            Records[I].Arg2 := K;
            break;
          end;
      end;
    end
    else
    begin
      has_assign := false;
      Records[N].Op := OP_BEGIN_VCALL;
      Records[N].GenOp := OP_BEGIN_VCALL;
      Res := ResId;
      for I := N + 1 to Card do
      begin
        if Records[I].Op = OP_STMT then
          break;

        if Records[I].Op = OP_ELEM then
          if Records[I].Arg1 = Res then
          begin
            IndexId := Records[I].Arg2;

            Records[I].Op := OP_PUSH;
            Records[I].GenOp := OP_PUSH;
            Records[I].Arg1 := IndexId;
            Records[I].Arg2 := K;
            Res := Records[I].Res;
            Records[I].Res := ResId;
            Inc(K);
          end;
        if Records[I].Op = OP_ASSIGN then
          if Records[I].Arg1 = Res then
          begin
            has_assign := true;

            IndexId := Records[I].Arg2;

            Records[I].Op := OP_PUSH;
            Records[I].GenOp := OP_PUSH;
            Records[I].Arg1 := IndexId;
            Records[I].Arg2 := K;
            Records[I].Res := ResId;

            RC := TCodeRec.Create(OP_VCALL, Self);
            RC.Arg1 := ResId;
            RC.Arg2 := K;
            RC.Res := 0;
            Insert(I + 1, RC);
            break;
          end;
      end;
      if not has_assign then
      begin
        RC := TCodeRec.Create(OP_VCALL, Self);
        RC.Arg1 := ResId;
        RC.Arg2 := 0;
        RC.Res := ResId;
        Insert(N + 1, RC);
      end;
    end;
  end;

var
  Arg1, Arg2, T1, T2, K1: Integer;
  PatternFieldId: Integer;
  FinalOwnerId: Integer;
  Level: Integer;
  S: String;
  I, J, K, RValueId, OldRes: Integer;
  ok: Boolean;
  RC: TCodeRec;
  Id: Integer;
  Final_T1: Integer;
  NP: Integer;
  PropNameId: Integer;
  ValId, ResId: Integer;
  IsCallExpected, IsAssignmentExpected: Boolean;
  IsInheritedExpected: Boolean;
  J1, J2, J3, tempN: Integer;
  KK: Integer;
  ParamId: Integer;
  TerminalTypeArg2, PatternArg2, RV_Count: Integer;
  IsDRTTI: Boolean;
  HelperTypeId: Integer;
label
  labelPROP;
begin
  HelperTypeId := 0;
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  ResId := R.Res;
  T1 := GetSymbolRec(Arg1).TerminalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  OldArg1 := Arg1;

  if K1 = KindSUB then
  begin
    if GetSymbolRec(Arg1).Count = 0 then
    begin
      RC := TCodeRec.Create(OP_CALL, Self);
      RC.Arg1 := Arg1;
      RC.Arg2 := 0;
      RC.Res := NewTempVar(GetSymbolRec(ResId).Level,
        GetSymbolRec(Arg1).TypeId);
      Insert(N, RC);

      R.Arg1 := RC.Res;

      Dec(N);
    end
    else
      CreateError(errRecordRequired, []);

    Exit;
  end
  else if K1 in [KindVAR, KindCONST] then
  begin
    if K1 = KindCONST then
      GetSymbolRec(Arg1).MustBeAllocated := true;

    Final_T1 := GetSymbolRec(Arg1).FinalTypeId;

    if Final_T1 = typeVOBJECT then
    begin
      ProcessVOBJECT(ResId);
      Exit;
    end;

    if Final_T1 in VariantTypes then
    begin
      if TKernel(kernel).IsFramework then
      begin
        S := GetSymbolRec(Arg2).Name;
        PatternFieldId := SymbolTable.LookUp(S, H_TFW_Variant, GetUpcase(N));
        if (PatternFieldId > 0) or (S = DummyName) then
        begin
          RC := TCodeRec.Create(OP_TO_FW_OBJECT, Self);
          RC.Arg1 := R.Arg1;
          RC.Arg2 := GetSymbolRec(Arg1).TerminalTypeId;
          RC.Res := NewTempVar(GetLevel(N), typeVARIANT);
          Insert(N, RC);

          Dec(N);

          R.Arg1 := RC.Res;

          Exit;
        end;
      end;

      GetSymbolRec(R.Res).TypeId := Final_T1;
      GetSymbolRec(R.Res).OwnerId := 0;

      S := GetSymbolRec(Arg2).Name;
      PropNameId := CreateConst(typeSTRING, S);

      NP := 0;
      J := -1;
      K := -1;
      ValId := 0;
      ResId := R.Res;

      for I := N + 1 to Card do
      begin
        if Records[I].Op = OP_STMT then
          break;

        if Records[I].Op = OP_ELEM then
        begin
          if Records[I].Arg1 = ResId then
          begin
            K := I;

            Inc(NP);
            ResId := Records[I].Res;

            GetSymbolRec(ResId).Kind := KindNONE;
            GetSymbolRec(ResId).OwnerId := 0;

            Records[I].Op := OP_OLE_PARAM;
            Records[I].Arg1 := Records[I].Arg2;
            Records[I].Arg2 := 0;
            Records[I].Res := PropNameId;
          end;
        end
        else if Records[I].Op = OP_CALL then
        begin
          if (Records[I].Arg1 = ResId) and (Records[I].Arg2 > 0) then
          begin
            for J1 := 1 to I - 1 do
            begin
              if (Records[J1].Op = OP_PUSH) and
                (Records[J1].Res = Records[I].Arg1) then
              begin
                K := J1;
                Inc(NP);
                Records[J1].Op := OP_OLE_PARAM;
                Records[J1].Arg2 := 0;
                Records[J1].Res := PropNameId;
              end;
            end;

            ResId := Records[I].Res;

            GetSymbolRec(ResId).Kind := KindNONE;
            GetSymbolRec(ResId).OwnerId := 0;
            Records[I].Op := OP_NOP;
          end;
        end;

        if Records[I].Op = OP_ASSIGN then
          if Records[I].Arg1 = ResId then
          begin
            J := I;
            ValId := Records[I].Arg2;
            Records[I].Op := OP_OLE_VALUE;
            Records[I].Arg1 := ValId;
            Records[I].Arg2 := 0;
            Records[I].Res := PropNameId;
            break;
          end;
      end;

      if J <= 0 then // read prop
      begin
        if NP = 0 then
        begin
          R.Op := OP_OLE_GET;
          R.Arg2 := PropNameId;
        end
        else
        begin
          RC := TCodeRec.Create(OP_OLE_GET, Self);
          RC.Arg1 := R.Arg1;
          RC.Arg2 := PropNameId;
          RC.Res := R.Res;

          Insert(K + 1, RC);
          R.Op := OP_NOP;

          for I := K + 1 to Card do
          begin
            if Records[I].Op = OP_STMT then
              break;

            if Records[I].Arg1 = ResId then
            begin
              Records[I].Arg1 := R.Res;
              break;
            end
            else if Records[I].Arg2 = ResId then
            begin
              Records[I].Arg2 := R.Res;
              break;
            end;
          end;
        end;
      end
      else // write prop
      begin
        RemoveDeclaredVar(Arg2);

        RC := TCodeRec.Create(OP_OLE_SET, Self);
        RC.Arg1 := R.Arg1;
        RC.Arg2 := PropNameId;
        RC.Res := ValId;

        Insert(J + 1, RC);
        R.Op := OP_NOP;
      end;

      Exit;
    end;

    if not(Final_T1 in [typeRECORD, typeCLASS, typeCLASSREF, typeINTERFACE])
    then
    begin
      S := GetSymbolRec(Arg2).Name;
      SymbolTable.LookUpEx(HelperTypeId, S, T1, GetUpcase(N));
      if HelperTypeId > 0 then
        Final_T1 := typeRECORD;
    end;

    if not(Final_T1 in [typeRECORD, typeCLASS, typeCLASSREF, typeINTERFACE])
    then
    begin
      if Records[N].Language = JS_LANGUAGE then
      begin
        RC := TCodeRec.Create(OP_TO_JS_OBJECT, Self);
        RC.Arg1 := R.Arg1;
        RC.Arg2 := GetSymbolRec(Arg1).TerminalTypeId;
        RC.Res := NewTempVar(GetLevel(N), typeVARIANT);
        Insert(N, RC);

        R.Arg1 := RC.Res;

        Exit;
      end
      else
      begin
        if Final_T1 = typePOINTER then
{$IFNDEF PAXARM}
          if not GetSymbolRec(Arg1).HasPAnsiCharType then
{$ENDIF}
            if not GetSymbolRec(Arg1).HasPWideCharType then
            begin
              T1 := GetSymbolRec(Arg1).TerminalTypeId;
              T1 := GetSymbolRec(T1).PatternId;

              RC := TCodeRec.Create(OP_TERMINAL, Self);
              RC.Arg1 := R.Arg1;
              RC.Arg2 := 0;
              RC.Res := NewTempVar(GetLevel(N), T1);
              Insert(N, RC);
              R.Arg1 := RC.Res;
              GetSymbolRec(R.Res).OwnerId := RC.Res;

              Dec(N);

              Exit;
            end;

        dmp;

        RC := TCodeRec.Create(OP_TO_FW_OBJECT, Self);
        RC.Arg1 := R.Arg1;
        RC.Arg2 := GetSymbolRec(Arg1).TerminalTypeId;
        RC.Res := NewTempVar(GetLevel(N), typeVARIANT);
        Insert(N, RC);

        Dec(N);

        R.Arg1 := RC.Res;

        Exit;
      end;

      CreateError(errRecordRequired, []);
      Exit;
    end;

    if Final_T1 = typeCLASSREF then
      T1 := GetSymbolRec(T1).PatternId;

    S := GetSymbolRec(Arg2).Name;
    if Records[N].PatternFieldId <> 0 then
      PatternFieldId := Records[N].PatternFieldId
    else
      PatternFieldId := SymbolTable.LookUpEx(HelperTypeId, S, T1, GetUpcase(N),
        MaxInt, not IsFrameworkTypeId(T1));
    if PatternFieldId = 0 then
    begin
      if GetSymbolRec(T1).IsJavaScriptClass then
      begin
        R.Op := OP_ELEM;
        Arg2 := R.Arg2;
        R.Arg2 := CreateConst(typeSTRING, GetSymbolRec(R.Arg2).Name);
        R.Res := NewTempVar(GetSymbolRec(R.Arg1).Level, typeVARIANT);
        ReplaceIdEx(Arg2, R.Res, N + 1, Card, true);

        Dec(N);
        GetSymbolRec(Arg2).Kind := KindNONE;

        Exit;
      end;

      if Records[N].Language = JS_LANGUAGE then
        if S = StrProgram then
        begin
          // OP_FIELD  (PROG)
          // OP_ASSIGN_PROG

          GetSymbolRec(R.Res).Kind := KindNONE;
          GetSymbolRec(R.Arg2).Kind := KindNONE;
          Records[N].Op := OP_NOP;
          Records[N + 1].Op := OP_NOP;

          // OP_FIELD (CONSTRUCTOR)
          // OP_ASSIGN

          GetSymbolRec(Records[N + 2].Res).Kind := KindNONE;
          GetSymbolRec(Records[N + 2].Arg2).Kind := KindNONE;
          Records[N + 2].Op := OP_NOP;
          Records[N + 3].Op := OP_NOP;

          Exit;
        end;

      CreateError(errUndeclaredIdentifier, [S]);
      Exit;
    end;

    CheckVis(PatternFieldId);

    if GetSymbolRec(PatternFieldId).Kind = KindCONST then
    begin
      Records[N].Op := OP_NOP;
      Records[N].GenOp := OP_NOP;
      ReplaceId(Records[N].Res, PatternFieldId);
      Exit;
    end;

    GetSymbolRec(Arg2).TypeId := GetSymbolRec(PatternFieldId).TypeId;
    GetSymbolRec(Arg2).PatternId := PatternFieldId;

    if Final_T1 = typeCLASSREF then
    begin
      case GetSymbolRec(PatternFieldId).Kind of
        KindCONSTRUCTOR:
          begin
            IsCallExpected := CallExpected(ResId);

            GetSymbolRec(Arg2).Name := '';
            GetSymbolRec(Arg2).Kind := 0;

            ReplaceId(Arg2, PatternFieldId);
            R.Op := OP_PUSH_CLASSREF;

            if not IsCallExpected then
              InsertCall;
          end;
        KindPROP:
          begin
            goto labelPROP;
          end;
        KindSUB:
          begin
            if not GetSymbolRec(PatternFieldId).IsSharedMethod then
              if not StrEql('ClassName', GetSymbolRec(PatternFieldId).Name) then
                CreateError
                  (errThisFormOfMethodCanOnlyAllowedForClassMethod, []);

            IsCallExpected := CallExpected(ResId);

            GetSymbolRec(Arg2).Name := '';
            GetSymbolRec(Arg2).Kind := 0;

            ReplaceId(Arg2, PatternFieldId);
            R.Op := OP_PUSH_CLASSREF;

            if not IsCallExpected then
            begin
              if Self[N + 1].Op = OP_EVAL_INHERITED then
                if R.Res = Self[N + 1].Arg1 then
                  Exit;

              InsertCall;
            end;

            if not GetSymbolRec(Self[N].Arg1).Host then
              if StrEql(GetSymbolRec(Self[N + 1].Arg1).Name, 'ClassName') then
              begin
                Self[N].Op := OP_NOP;
                Self[N + 1].Op := OP_CLASSNAME;
                Self[N + 1].Arg1 := Self[N].Arg1;
              end;

          end;
      end;
      Exit;
    end
    else if (Final_T1 in [typeCLASS, typeINTERFACE]) or
      ((Final_T1 = typeRECORD) and (GetSymbolRec(PatternFieldId).Kind
      in [KindSUB, KindPROP])) then
    begin
      case GetSymbolRec(PatternFieldId).Kind of
        KindCONSTRUCTOR:
          begin
            GetSymbolRec(Arg2).Name := '';
            GetSymbolRec(Arg2).Kind := 0;

            ReplaceId(Arg2, PatternFieldId);
            R.Op := OP_PUSH_CLASSREF;
            R.Arg2 := 0;
          end;
        KindSUB:
          begin
            if InsertCreateEvent(PatternFieldId) then
              Exit;

            GetSymbolRec(Arg2).Name := '';
            GetSymbolRec(Arg2).Kind := 0;

            IsCallExpected := CallExpected(ResId);

            ReplaceIdEx(Arg2, PatternFieldId, N, GetNextStmt(N), true);
            R.Op := OP_PUSH_INSTANCE;
            R.Arg2 := 0;

            if not IsCallExpected then
            begin
              if Self[N + 1].Op = OP_EVAL_INHERITED then
                if R.Res = Self[N + 1].Arg1 then
                  Exit;

              InsertCall;
            end;
          end;
        KindDESTRUCTOR:
          begin
            tempN := N;

            GetSymbolRec(Arg2).Name := '';
            GetSymbolRec(Arg2).Kind := 0;

            IsCallExpected := CallExpected(ResId);
            IsInheritedExpected := InheritedExpected(ResId);

            ReplaceId(Arg2, PatternFieldId);
            R.Op := OP_PUSH_INSTANCE;
            R.Arg2 := 0;

            if not(IsCallExpected or IsInheritedExpected) then
              InsertCall;

            if IsInheritedExpected then
              Exit;

            Exit;

            RC := TCodeRec.Create(OP_DESTROY_OBJECT, Self);
            RC.Arg1 := R.Arg1;
            RC.Arg2 := 0;
            RC.Res := 0;

            while (Records[N].Op <> OP_CALL) do
              Inc(N);

            Insert(N + 1, RC);
            N := tempN;
          end;
        KindTYPE_FIELD:
          begin
            if GetSymbolRec(PatternFieldId).CompIndex >= 0 then
            begin
              R.Op := OP_GET_COMPONENT;
              R.Arg2 := GetSymbolRec(PatternFieldId).CompIndex;
              GetSymbolRec(R.Res).Kind := KindVAR;
              GetSymbolRec(R.Res).PatternId := 0;
              GetSymbolRec(R.Res).OwnerId := 0;
            end
            else
              GetSymbolRec(Arg2).ByRef := true;
          end;
        KindPROP:
          begin
          labelPROP:
            if (GetSymbolRec(PatternFieldId).IsPublished or
              GetSymbolRec(PatternFieldId).IsDRTTI) and
              GetSymbolRec(PatternFieldId).Host then

            begin
              IsDRTTI := GetSymbolRec(PatternFieldId).IsDRTTI;

              T2 := GetSymbolRec(PatternFieldId).Level;

              if Assigned(ForbiddenPropList) then
                if GetSymbolRec(T2).PClass <> nil then
                  if ForbiddenPropList.IsForbidden(GetSymbolRec(T2).PClass,
                    GetSymbolRec(PatternFieldId).Name) then
                  begin
                    CreateError(errPropertyIsForbidden,
                      [GetSymbolRec(PatternFieldId).Name,
                      GetSymbolRec(T2).Name]);
                  end;

              T2 := GetSymbolRec(PatternFieldId).FinalTypeId;
              R.Arg2 := PatternFieldId;
              GetSymbolRec(R.Res).OwnerId := 0;

              RV_Count := -1;

              for I := N to Card do
              begin
                if Records[I].Op = OP_STMT then
                  break;

                if Records[I].Op = OP_LVALUE then
                  RV_Count := 0
                else if RV_Count >= 0 then
                begin
                  if (Records[I].Op <> OP_SEPARATOR) and
                    (Records[I].Op <> OP_NOP) then
                    Inc(RV_Count);
                end;

                if Records[I].Arg1 = R.Res then
                  if Records[I].Op = OP_ASSIGN then
                  begin
                    // process put property

                    RValueId := Records[I].Arg2;

                    if GetSymbolRec(RValueId).Name = DummyName then
                    begin
                      CreateError(errCanceled, []);
                      Exit;
                    end;

                    if (GetSymbolRec(RValueId).TypeId = 0) or
                      (Records[I - 1].Op = OP_ADDRESS) or
                      (Records[I - 1].Op = OP_CALL) then
                    begin
                      if Records[I - 1].Op = OP_ADDRESS then
                      begin
                        if RV_Count > 1 then
                        begin
                          MoveLValue(I);
                          Exit;
                        end;
                      end;

                      if T2 = typeEVENT then
                        GetSymbolRec(RValueId).TypeId := T2
                      else
                      begin
                        if RV_Count > 1 then
                        begin
                          MoveLValue(I);
                          Exit;
                        end;
                      end;
                    end
                    else if (RV_Count > 1) and (T2 <> typeEVENT) then
                    begin
                      MoveRValue(I);
                      Dec(N);
                      Exit;
                    end;

                    R.Res := RValueId;

                    Records[I].Arg1 := R.Arg1;
                    Records[I].Arg2 := R.Arg2;
                    Records[I].Res := R.Res;

                    R.Op := OP_NOP;

                    R := Records[I];

                    if IsDRTTI then
                    begin
                      RC := TCodeRec.Create(OP_ASSIGN, Self);
                      RC.Arg1 := NewTempVar(GetLevel(N), H_TValue);
                      RC.Arg2 := R.Res;
                      RC.Res := RC.Arg1;
                      Insert(I, RC);

                      R.Res := RC.Res;

                      R.Op := OP_SET_DRTTI_PROP;

                      N := I - 1;
                      Exit;
                    end
                    else
                      case T2 of
{$IFNDEF PAXARM}
                        typeANSISTRING, typeSHORTSTRING:
                          begin
                            if GetSymbolRec(R.Res).Kind = KindCONST then
                            begin
                              if GetSymbolRec(R.Res).FinalTypeId in CharTypes
                              then
                              begin
                                S := Chr(Integer(GetSymbolRec(R.Res).value));
                                R.Res := SymbolTable.AddPAnsiCharConst
                                  (AnsiString(S)).Id;
                              end
                              else if GetSymbolRec(R.Res).FinalTypeId <> typeANSISTRING
                              then
                              begin
                                RC := InsertConversionToAnsiString(I, 3);
                                R.Res := RC.Res;
                                Inc(N);
                              end;
                            end
                            else if GetSymbolRec(R.Res).FinalTypeId <> typeANSISTRING
                            then
                            begin
                              RC := InsertConversionToAnsiString(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_ANSISTR_PROP;
                          end;
                        typeWIDESTRING:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeWIDESTRING
                            then
                            begin
                              RC := InsertConversionToWideString(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_WIDESTR_PROP;
                          end;
{$ENDIF}
                        typeUNICSTRING:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeUNICSTRING
                            then
                            begin
                              RC := InsertConversionToUnicString(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_UNICSTR_PROP;
                          end;
                        typeINTEGER, typeBYTE, typeWORD, typeCARDINAL,
                          typeSMALLINT, typeSHORTINT:
                          begin
                            if not(GetSymbolRec(R.Res).FinalTypeId
                              in IntegerTypes) then
                            begin
                              RC := InsertConversionToInteger(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_ORD_PROP;
                          end;
                        typeINTERFACE:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeINTERFACE
                            then
                              CreateError(errIncompatibleTypesNoArgs, []);
                            R.Op := OP_SET_INTERFACE_PROP;
                          end;
{$IFNDEF PAXARM}
                        typeANSICHAR:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeANSICHAR
                            then
                              CreateError(errIncompatibleTypesNoArgs, []);
                            R.Op := OP_SET_ORD_PROP;
                          end;
{$ENDIF}
                        typeWIDECHAR:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeWIDECHAR
                            then
                              CreateError(errIncompatibleTypesNoArgs, []);
                            R.Op := OP_SET_ORD_PROP;
                          end;
                        typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeBOOLEAN
                            then
                            begin
                              RC := InsertConversionToBoolean(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_ORD_PROP;
                          end;
                        typeENUM:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeENUM then
                              CreateError(errIncompatibleTypesNoArgs, []);
                            R.Op := OP_SET_ORD_PROP;
                          end;
                        typeSET:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeSET then
                            begin
                              ok := false;
                              for J := N to Card do
                              begin
                                if Records[J].Op = OP_STMT then
                                  break;

                                with Records[J] do
                                  if (Op = OP_SET_INCLUDE) or
                                    (Op = OP_SET_INCLUDE_INTERVAL) then
                                    if Arg1 = R.Res then
                                    begin
                                      ok := true;
                                    end;
                              end;

                              if ok then
                              begin
                                Records[I].Op := OP_SET_SET_PROP;
                                Records[I].Arg1 := R.Arg1;
                                Records[I].Arg2 := R.Arg2;
                                Records[I].Res := R.Res;

                                R.Op := OP_NOP;

                                Exit;
                              end;

                              CreateError(errIncompatibleTypesNoArgs, []);
                            end;
                            R.Op := OP_SET_SET_PROP;
                          end;
                        typeCLASS:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeCLASS then
                            begin
                              if R.Res <> SymbolTable.NilId then
                                CreateError(errIncompatibleTypesNoArgs, []);
                              R.Op := OP_SET_ORD_PROP;
                              Exit;
                            end;

                            if not CheckAssignment(PatternFieldId, R.Res) then
                              CreateError(errIncompatibleTypesNoArgs, []);

                            R.Op := OP_SET_ORD_PROP;
                          end;
                        typeEXTENDED, typeSINGLE, typeDOUBLE, typeCURRENCY:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeEXTENDED
                            then
                            begin
                              RC := InsertConversionToExtended(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_FLOAT_PROP;
                          end;
                        typeVARIANT:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeVARIANT
                            then
                            begin
                              RC := InsertConversionToVariant(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_VARIANT_PROP;
                          end;
                        typeOLEVARIANT:
                          begin
                            if GetSymbolRec(R.Res).FinalTypeId <> typeOLEVARIANT
                            then
                            begin
                              RC := InsertConversionToOleVariant(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_VARIANT_PROP;
                          end;
                        typeINT64, typeUINT64:
                          begin
                            if not(GetSymbolRec(R.Res).FinalTypeId in INT64Types)
                            then
                            begin
                              RC := InsertConversionToInt64(I, 3);
                              R.Res := RC.Res;
                              Inc(N);
                            end;
                            R.Op := OP_SET_INT64_PROP;
                          end;
                        typeEVENT, typeRECORD:
                          begin
                            R.Op := OP_SET_EVENT_PROP;
                            for J := GetStmt(N) to I do
                              if Records[J].Op = OP_FIELD then
                                if Records[J].Res = R.Res then
                                begin
                                  Records[J].Op := OP_NOP;
                                  T1 := GetSymbolRec(Records[J].Arg1)
                                    .TerminalTypeId;
                                  if T1 = 0 then
                                  begin
                                    RestoreFieldType(J);
                                    T1 := GetSymbolRec(Records[J].Arg1)
                                      .TerminalTypeId;
                                  end;

                                  break;
                                end;

                            if R.Res = SymbolTable.NilId then
                              Exit;

                            GetSymbolRec(R.Res).Kind := KindNONE;

                            S := GetSymbolRec(R.Res).Name;
                            if S = '' then
                              RaiseError(errIncompatibleTypesNoArgs, []);

                            PatternFieldId :=
                              SymbolTable.LookUp(S, T1, GetUpcase(N));
                            if PatternFieldId = 0 then
                            begin
                              if GetSymbolRec(R.Res).FinalTypeId = typeEVENT
                              then
                              begin
                                GetSymbolRec(R.Res).Kind := KindVAR;
                                R.Op := OP_SET_EVENT_PROP2;
                                Exit;
                              end;

                              // CreateError(errUndeclaredIdentifier, [S]);
                              Exit;
                            end;

                            GetSymbolRec(R.Res).PatternId := PatternFieldId;

                            TerminalTypeArg2 := GetSymbolRec(Arg2)
                              .TerminalTypeId;
                            PatternArg2 := GetSymbolRec(TerminalTypeArg2)
                              .PatternId;

                            if (GetSymbolRec(PatternFieldId).Kind
                              in [KindCONSTRUCTOR, KindDESTRUCTOR]) then
                              CreateError(errIncompatibleTypesNoArgs, []);

                            if (GetSymbolRec(PatternFieldId).Kind = KindSUB)
                            then
                              if (GetSymbolRec(PatternArg2).Kind = KindSUB) then
                              begin
                                if not SymbolTable.EqualHeaders(PatternFieldId,
                                  PatternArg2) then
                                  CreateError(errIncompatibleTypesNoArgs, []);
                              end;
                          end;
                      else
                        CreateError(errInternalError, []);
                      end;
                    Exit;
                  end;
              end; // for

              InsertDeclareTempVar(R.Res);

              if IsDRTTI then
              begin
                R.Op := OP_GET_DRTTI_PROP;
                if T2 = typeINT64 then
                  GetSymbolRec(R.Res).TypeId := typeINT64
                else if T2 = typeUINT64 then
                  GetSymbolRec(R.Res).TypeId := typeUINT64
                else if T2 in IntegerTypes then
                  GetSymbolRec(R.Res).TypeId := typeINTEGER
                else if T2 in StringTypes then
                  GetSymbolRec(R.Res).TypeId := typeSTRING
                else if T2 in RealTypes then
                  GetSymbolRec(R.Res).TypeId := typeEXTENDED
                else if T2 in VariantTypes then
                  GetSymbolRec(R.Res).TypeId := typeVARIANT
                else
                begin
                  GetSymbolRec(R.Res).TypeId := H_TValue;

                  Id := NewTempVar(GetLevel(N), GetSymbolRec(PatternFieldId)
                    .TerminalTypeId);
                  RC := TCodeRec.Create(OP_VAR_FROM_TVALUE, Self);
                  RC.Arg1 := Id;
                  RC.Arg2 := R.Res;
                  RC.Res := 0;

                  Insert(N + 1, RC);
                  ReplaceIdEx(R.Res, Id, N + 2, Card, false);

                end;
              end
              else
                case T2 of
{$IFNDEF PAXARM}
                  typeANSISTRING:
                    R.Op := OP_GET_ANSISTR_PROP;
                  typeSHORTSTRING:
                    begin
                      R.Op := OP_GET_ANSISTR_PROP;

                      Id := R.Res;
                      Level := SymbolTable[Id].Level;
                      R.Res := CreateStringVar(Level);

                      RC := TCodeRec.Create(OP_ASSIGN, Self);
                      RC.Arg1 := Id;
                      RC.Arg2 := R.Res;
                      RC.Res := Id;

                      Insert(N + 1, RC);
                    end;
                  typeWIDESTRING:
                    R.Op := OP_GET_WIDESTR_PROP;
                  typeANSICHAR:
                    R.Op := OP_GET_ORD_PROP;
{$ENDIF}
                  typeUNICSTRING:
                    R.Op := OP_GET_UNICSTR_PROP;
                  typeINTEGER, typeBYTE, typeWORD, typeCARDINAL, typeSMALLINT,
                    typeSHORTINT:
                    R.Op := OP_GET_ORD_PROP;
                  typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
                    R.Op := OP_GET_ORD_PROP;
                  typeENUM:
                    R.Op := OP_GET_ORD_PROP;
                  typeSET:
                    R.Op := OP_GET_SET_PROP;
                  typeINTERFACE:
                    R.Op := OP_GET_INTERFACE_PROP;
                  typeCLASS:
                    R.Op := OP_GET_ORD_PROP;
                  typeEXTENDED:
                    R.Op := OP_GET_FLOAT_PROP;
                  typeVARIANT:
                    R.Op := OP_GET_VARIANT_PROP;
                  typeOLEVARIANT:
                    R.Op := OP_GET_VARIANT_PROP;
                  typeINT64, typeUINT64:
                    R.Op := OP_GET_INT64_PROP;
                  typeSINGLE, typeDOUBLE, typeCURRENCY:
                    begin
                      R.Op := OP_GET_FLOAT_PROP;

                      Id := R.Res;
                      Level := SymbolTable[Id].Level;
                      R.Res := CreateExtendedVar(Level);

                      RC := TCodeRec.Create(OP_ASSIGN, Self);
                      RC.Arg1 := Id;
                      RC.Arg2 := R.Res;
                      RC.Res := Id;

                      Insert(N + 1, RC);
                    end;
                  typeEVENT:
                    begin
                      R.Op := OP_GET_EVENT_PROP;
                    end;
                else
                  CreateError(errInternalError, []);
                end;
            end
            else // not published property
            begin
              Id := SymbolTable[PatternFieldId].ReadId;

              if Id > 0 then
              begin
                I := GetSymbolRec(Id).Level;
                if GetSymbolRec(I).FinalTypeId = typeINTERFACE then
                  if GetSymbolRec(Arg1).FinalTypeId = typeCLASS then
                  begin
                    S := GetSymbolRec(Id).Name;
                    Id := SymbolTable.LookUp(S,
                      GetSymbolRec(Arg1).TerminalTypeId, GetUpcase(N));
                  end;
              end;

              if Id <> 0 then
                if SymbolTable[Id].Kind = KindSUB then
                begin

                  IsAssignmentExpected := false;
                  for I := N to Card do
                  begin

                    if Records[I].Op = OP_STMT then
                      break;

                    if Records[I].Arg1 = R.Res then
                      if Records[I].Op = OP_ASSIGN then
                      begin
                        IsAssignmentExpected := true;
                        break;
                      end;
                  end;

                  if SymbolTable[Id].Count = 0 then
                  begin

                    if not IsAssignmentExpected then
                    begin
                      J := R.Res;

                      if GetSymbolRec(Id).IsSharedMethod then
                        R.Op := OP_PUSH_CLASSREF
                      else
                        R.Op := OP_PUSH_INSTANCE;

                      R.Arg1 := R.Arg1;
                      R.Arg2 := 0;
                      R.Res := Id;

                      RC := TCodeRec.Create(OP_CALL, Self);
                      RC.Arg1 := Id;
                      RC.Arg2 := 0;
                      RC.Res := NewTempVar(GetLevel(N), SymbolTable[Id].TypeId);

                      Insert(N + 1, RC);

                      ReplaceIdEx(J, RC.Res, N + 1, Card, true);

                      SymbolTable[J].Kind := KindNONE;

                      Exit;
                    end;
                  end
                  else if (SymbolTable[Id].Count = 1) and // fake method
                    ((SymbolTable[Id].Level = 0) or
                    (GetSymbolRec(SymbolTable[Id].Level).Kind = KindNAMESPACE))
                  then
                  begin

                    if not IsAssignmentExpected then
                    begin
                      J := R.Res;

                      R.Op := OP_PUSH;
                      R.Arg1 := R.Arg1;
                      R.Arg2 := 0;
                      R.Res := Id;

                      RC := TCodeRec.Create(OP_CALL, Self);
                      RC.Arg1 := Id;
                      RC.Arg2 := 0;
                      RC.Res := NewTempVar(GetLevel(N), SymbolTable[Id].TypeId);

                      Insert(N + 1, RC);

                      ReplaceIdEx(J, RC.Res, N + 1, Card, true);

                      SymbolTable[J].Kind := KindNONE;

                      Exit;
                    end;
                  end;
                end;

              Id := R.Res;

              IsCallExpected := false;
              J2 := 0;

              for I := N to Card do
              begin
                if Records[I].Op = OP_STMT then
                  break;

                if Records[I].Op = OP_ELEM then
                  if Records[I].Arg1 = Id then
                    Id := Records[I].Res;

                if Records[I].Op = OP_CALL then
                  if Records[I].Arg1 = Id then
                  begin
                    Id := Records[I].Res;
                    IsCallExpected := true;

                    if GetLanguage(N) = PASCAL_LANGUAGE then
                    begin
                      // CreateError(errCannotApplyCall, [GetSymbolRec(PatternFieldId).Name]);
                    end;

                    J2 := I;
                  end;

                if Records[I].Arg1 = Id then
                  if Records[I].Op = OP_ASSIGN then
                  begin
                    // process put property

                    PatternFieldId := GetSymbolRec(PatternFieldId).WriteId;

                    if PatternFieldId > 0 then
                    begin
                      if GetSymbolRec(GetSymbolRec(PatternFieldId).Level)
                        .FinalTypeId = typeINTERFACE then
                        if GetSymbolRec(Arg1).FinalTypeId = typeCLASS then
                        begin
                          S := GetSymbolRec(PatternFieldId).Name;
                          PatternFieldId :=
                            SymbolTable.LookUp(S,
                            GetSymbolRec(Arg1).TerminalTypeId, GetUpcase(N));
                        end;
                    end;

                    if PatternFieldId = 0 then
                    begin
                      CreateError(errCannotAssignToReadOnlyProperty, []);
                      Exit;
                    end;

                    if GetSymbolRec(PatternFieldId).Kind = KindTYPE_FIELD then
                    begin
                      GetSymbolRec(Arg2).PatternId := PatternFieldId;
                      GetSymbolRec(Arg2).ByRef := true;
                    end
                    else if GetSymbolRec(PatternFieldId).Kind = KindSUB then
                    begin
                      GetSymbolRec(Arg2).Name := '';
                      GetSymbolRec(Arg2).Kind := 0;

                      ReplaceId(Arg2, PatternFieldId);
                      R.Op := OP_PUSH_INSTANCE;
                      R.Arg2 := 0;

                      if GetSymbolRec(PatternFieldId).Count > 1 then
                      begin
                        // if GetLanguage(n) = PASCAL_LANGUAGE then
                        begin
                          J1 := ReplaceOP_ELEM(PatternFieldId, K, OldRes);
                          if K = -1 then
                          begin
                            if IsCallExpected then
                            begin
                              for J3 := 1 to J2 do
                                if Records[J3].Op = OP_PUSH then
                                  if Records[J3].Res = PatternFieldId then
                                    Inc(K);

                              Records[J2].Op := OP_NOP;

                              // push value

                              Records[I].Op := OP_PUSH;
                              Records[I].Arg1 := Records[I].Arg2;
                              Records[I].Arg2 := K + 1;
                              Records[I].Res := PatternFieldId;

                              RC := TCodeRec.Create(OP_CALL, Self);
                              RC.Arg1 := PatternFieldId;
                              RC.Arg2 := K + 2;
                              RC.Res := 0;

                              Insert(I + 1, RC);

                              Exit;
                            end;

                            CreateError(errNotEnoughActualParameters, []);
                          end
                          else
                          begin
                            // push value

                            Records[I].Op := OP_PUSH;
                            Records[I].Arg1 := Records[I].Arg2;
                            Records[I].Arg2 := K + 1;
                            Records[I].Res := PatternFieldId;

                            RC := TCodeRec.Create(OP_CALL, Self);
                            RC.Arg1 := PatternFieldId;
                            RC.Arg2 := K + 2;
                            RC.Res := 0;

                            Insert(I + 1, RC);

                            if J1 <> N then
                            begin
                              RC := TCodeRec.Create(OP_RESTORE_REGS, Self);
                              Insert(I, RC);
                              RC := TCodeRec.Create(OP_SAVE_REGS, Self);
                              Insert(J1 + 1, RC);
                            end;
                          end;
                        end;
                        Exit;
                      end;

                      // push value

                      Records[I].Op := OP_PUSH;
                      Records[I].Arg1 := Records[I].Arg2;
                      Records[I].Arg2 := 0;
                      Records[I].Res := PatternFieldId;

                      RC := TCodeRec.Create(OP_CALL, Self);
                      RC.Arg1 := PatternFieldId;
                      RC.Arg2 := 1;
                      RC.Res := 0;

                      Insert(I + 1, RC);

                      if GetSymbolRec(PatternFieldId).Count = 1 then
                      begin
                        ParamId := SymbolTable.GetParamId(PatternFieldId, 0);
                        if GetSymbolRec(ParamId).FinalTypeId = typeEVENT then
                        begin
                          if Records[I - 1].Op = OP_FIELD then
                          begin
                            ParamId := SymbolTable.AddTMethodVar
                              (GetLevel(N)).Id;
                            Records[I - 1].Op := OP_CREATE_METHOD;
                            Records[I - 1].Res := ParamId;

                            Records[I].Arg1 := ParamId;
                          end;
                        end;
                      end;

                    end
                    else
                      CreateError(errInternalError, []);

                    Exit;
                  end;
              end; // for-loop

              PatternFieldId := GetSymbolRec(PatternFieldId).ReadId;
              if PatternFieldId = 0 then
              begin
                CreateError(errCannotReadWriteOnlyProperty, []);
                Exit;
              end;

              if GetSymbolRec(PatternFieldId).Kind = KindTYPE_FIELD then
              begin
                GetSymbolRec(Arg2).PatternId := PatternFieldId;
                GetSymbolRec(Arg2).ByRef := true;
              end
              else if GetSymbolRec(PatternFieldId).Kind = KindSUB then
              begin
                IsCallExpected := CallExpected(ResId);

                GetSymbolRec(Arg2).Name := '';
                GetSymbolRec(Arg2).Kind := 0;

                ReplaceId(Arg2, PatternFieldId);
                R.Op := OP_PUSH_INSTANCE;
                R.Arg2 := 0;

                KK := GetSymbolRec(PatternFieldId).Count;

                if KK > 0 then
                begin
                  // if GetLanguage(n) = PASCAL_LANGUAGE then
                  begin
                    J := ReplaceOP_ELEM(PatternFieldId, K, OldRes);
                    if K = -1 then
                    begin
                      if IsCallExpected then
                        Exit;
                      Exit;
                      CreateError(errNotEnoughActualParameters, []);
                    end
                    else
                    begin
                      InsertCallEx(J, K + 1);
                      ReplaceId(OldRes, Records[J + 1].Res);
                    end;
                  end;
                  Exit;
                end;

                if not IsCallExpected then
                  InsertCall;
              end
              else
                CreateError(errInternalError, []);
            end
          end; // KindPROP
      else
        CreateError(errInternalError, []);
      end;
      Exit;
    end;

    // typeRECORD

    FinalOwnerId := GetSymbolRec(Arg2).FinalOwnerId;
    Level := GetSymbolRec(FinalOwnerId).Level;
    if GetSymbolRec(Level).Kind in kindSUBS then
    begin
      if SymbolTable.GetResultId(Level) = FinalOwnerId then
      begin
        GetSymbolRec(FinalOwnerId).ByRef := true;
      end;
      {
        if GetSymbolRec(FinalOwnerId).Param then
        if not StrEql(GetSymbolRec(FinalOwnerId).Name, 'Self') then
        begin
        GetSymbolRec(FinalOwnerId).ByRef := true;
        end;
      }
    end;

    if GetSymbolRec(FinalOwnerId).Host or (GetSymbolRec(Arg1).Name = '') or
      GetSymbolRec(Arg2).HasByRefOwner or (FinalOwnerId > Arg2) or
      GetSymbolRec(FinalOwnerId).Param then
      GetSymbolRec(Arg2).ByRef := true
    else
      R.Op := OP_NOP;
  end
  else if K1 = KindTYPE then
  begin
    if GetSymbolRec(Arg1).TypeId = TypeALIAS then
      Arg1 := GetSymbolRec(Arg1).TerminalTypeId;

    T1 := Arg1;

    if not(GetSymbolRec(Arg1).FinalTypeId in [typeRECORD, typeCLASS]) then
    begin
      S := GetSymbolRec(Arg2).Name;
      SymbolTable.LookUpEx(HelperTypeId, S, T1, GetUpcase(N));
    end;

    if not(GetSymbolRec(Arg1).FinalTypeId in [typeRECORD, typeCLASS]) then
      if HelperTypeId = 0 then
      begin
        if GetSymbolRec(Arg1).FinalTypeId = typeENUM then
        begin
          S := GetSymbolRec(Arg2).Name;
          PatternFieldId := SymbolTable.LookUpEnumItem(S, T1, GetUpcase(N));
          if PatternFieldId = 0 then
            CreateError(errUndeclaredIdentifier, [S])
          else
          begin
            R.Op := OP_NOP;
            GetSymbolRec(Arg2).Kind := KindNONE;
            GetSymbolRec(Arg2).OwnerId := 0;
            ReplaceId(Arg2, PatternFieldId);
          end;
          Exit;
        end;

        CreateError(errRecordRequired, []);
        Exit;
      end;

    S := GetSymbolRec(Arg2).Name;
    if Records[N].PatternFieldId <> 0 then
      PatternFieldId := Records[N].PatternFieldId
    else
      PatternFieldId := SymbolTable.LookUpEx(HelperTypeId, S, T1, GetUpcase(N));
    if PatternFieldId = 0 then
      CreateError(errUndeclaredIdentifier, [S]);

    CheckVis(PatternFieldId);

    if GetSymbolRec(PatternFieldId).Kind in kindSUBS then
      if Records[N + 1].Op = OP_ADDRESS then
        if Records[N + 1].Arg1 = Records[N].Res then
        begin
          Records[N].Op := OP_NOP;
          Records[N + 1].Arg1 := PatternFieldId;
          Exit;
        end;

    GetSymbolRec(Arg2).TypeId := GetSymbolRec(PatternFieldId).TypeId;
    GetSymbolRec(Arg2).PatternId := PatternFieldId;

    case GetSymbolRec(PatternFieldId).Kind of
      KindCONSTRUCTOR:
        begin
          IsCallExpected := CallExpected(ResId);

          GetSymbolRec(Arg2).Name := '';
          GetSymbolRec(Arg2).Kind := 0;

          ReplaceId(Arg2, PatternFieldId);
          R.Op := OP_PUSH_CLASSREF;
          Inc(R.Arg1);
          R.Arg2 := 0;

          if not IsCallExpected then
            InsertCall;
        end;
      KindSUB:
        begin
          if not GetSymbolRec(PatternFieldId).IsSharedMethod then
            if not StrEql('ClassName', GetSymbolRec(PatternFieldId).Name) then
              CreateError(errThisFormOfMethodCanOnlyAllowedForClassMethod, []);

          if InsertCreateEvent(PatternFieldId) then
            Exit;

          IsCallExpected := CallExpected(ResId);

          GetSymbolRec(Arg2).Name := '';
          GetSymbolRec(Arg2).Kind := 0;

          ReplaceId(Arg2, PatternFieldId);
          R.Op := OP_PUSH_CLASSREF;
          Inc(R.Arg1);
          R.Arg2 := 0;

          if not IsCallExpected then
            InsertCall;

          if not GetSymbolRec(Self[N].Arg1).Host then
            if StrEql(GetSymbolRec(Self[N + 1].Arg1).Name, 'ClassName') then
            begin
              Self[N].Op := OP_NOP;
              Self[N + 1].Op := OP_CLASSNAME;
              Self[N + 1].Arg1 := Self[N].Arg1;
            end;

        end;
      KindTYPE_FIELD:
        begin
          GetSymbolRec(Arg2).ByRef := true;
        end;
      KindCONST:
        begin
          Records[N].Op := OP_NOP;
          Records[N].GenOp := OP_NOP;
          ReplaceId(Records[N].Res, PatternFieldId);
        end;
    else
      CreateError(errPropertyInaccessibleHere, [S]);
    end;
  end
  else if K1 = KindNAMESPACE then
  begin
    CreateUsingList(N);
    if using_list.IndexOf(Arg1) = -1 then
    begin
      S := GetSymbolRec(Arg1).Name;
      CreateError(errUndeclaredIdentifier, [S]);
      SymbolTable.AddUndeclaredIdent(S, TKernel(kernel).UndeclaredIdents,
        TKernel(kernel).Errors.Count - 1, true);
    end;

    S := GetSymbolRec(Arg2).Name;
    PatternFieldId := SymbolTable.LookUp(S, Arg1, GetUpcase(N));

    if PatternFieldId = 0 then
    begin
      if ((Arg1 = H_PascalNamespace) and (GetLanguage(N) = PASCAL_LANGUAGE)) or
        ((Arg1 = H_BasicNamespace) and (GetLanguage(N) = BASIC_LANGUAGE)) then
        PatternFieldId := SymbolTable.LookUp(S, 0, GetUpcase(N));
    end;

    if PatternFieldId = 0 then
    begin
      CreateError(errUndeclaredIdentifier, [S]);
      if TKernel(kernel).Canceled then
      begin
        Exit;
      end;
    end;

    GetSymbolRec(PatternFieldId).NSOwnerId := Arg1;

    ReplaceIdEx(R.Res, PatternFieldId, N + 1, Card, true);
    R.Op := OP_NOP;
  end
  else
  begin

    if Records[N].Language = JS_LANGUAGE then
    begin

      RC := TCodeRec.Create(OP_TO_JS_OBJECT, Self);
      RC.Arg1 := R.Arg1;
      RC.Arg2 := 0;
      RC.Res := NewTempVar(GetLevel(N), typeVARIANT);
      Insert(N, RC);

      R.Arg1 := RC.Res;

      Exit;
    end;

    CreateError(errRecordRequired, []);
  end;
end;

procedure TCode.AssignShifts;
var
  I, J, OwnerId, DestId, K, FinTypeId, FieldDescriptorId, OwnerShift, DestShift,
    DestSize, D: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  for I := Card downto 1 do
  begin
    R := Self[I];
    if R.Op = OP_ABSOLUTE then
    begin
      OwnerShift := GetSymbolRec(R.Arg2).Shift;
      if OwnerShift = 0 then
      begin
        N := I;
        CreateError(errIdentifierExpected, []);
      end;
      GetSymbolRec(R.Arg1).Shift := OwnerShift;
    end
    else if R.Op = OP_ASSIGN_SHIFT then
    begin
      OwnerId := R.Arg1;
      K := R.Arg2;
      DestId := R.Res;

      FinTypeId := SymbolTable[OwnerId].FinalTypeId;
      OwnerShift := SymbolTable[OwnerId].Shift;
      DestShift := SymbolTable[DestId].Shift;
      DestSize := SymbolTable[DestId].Size;
      case FinTypeId of
        typeARRAY:
          begin
            SymbolTable[DestId].Shift := OwnerShift + K * DestSize;
          end;
        typeRECORD:
          begin
            FieldDescriptorId := SymbolTable.GetFieldDescriptorId(OwnerId, K);
            SymbolTable[DestId].Shift := OwnerShift + SymbolTable
              [FieldDescriptorId].Shift;
          end;
      end;
      D := DestShift - SymbolTable[DestId].Shift;
      for J := DestId + 1 to SymbolTable.Card do
        if SymbolTable[J].OwnerId = DestId then
          SymbolTable[J].Shift := SymbolTable[J].Shift - D;

      R.Op := OP_NOP;
    end;
  end;
end;

procedure TCode.OperCheckOverride(List: TIntegerList);
var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  temp, I, SubId, L, BestId, J: Integer;
  temp_l: TIntegerList;
  SR, ST: TSymbolRec;
begin
  if TKernel(kernel).InterfaceOnly then
    Exit;

  R := Records[N];
  R.Op := OP_NOP;
  SubId := R.Arg1;

  BestId := 0;

  L := GetSymbolRec(SubId).Level;
  if L = 0 then
    Exit;
  if GetSymbolRec(L).Kind <> KindTYPE then
    Exit;

  SymbolTable := TKernel(kernel).SymbolTable;
  if (GetSymbolRec(SubId).Name = '') and
    (GetSymbolRec(SubId).Kind = KindCONSTRUCTOR) then
  begin
    temp_l := SymbolTable.LookupParentConstructors(SubId);
    try
      for I := 0 to temp_l.Count - 1 do
      begin
        if GetSymbolRec(temp_l[I]).CallMode in [cmNONE, cmSTATIC] then
        begin
          GetSymbolRec(SubId).CallMode := GetSymbolRec(temp_l[I]).CallMode;
          Exit;
        end;
      end;
    finally
      FreeAndNil(temp_l);
    end;
    Exit;
  end
  else
    temp := SymbolTable.LookupParentMethodBase(SubId, GetUpcase(N), BestId);
  if temp = 0 then
  begin
    if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
    begin
      // fix default constructor
      GetSymbolRec(SubId).CallMode := cmNONE;
    end
    else
    begin
      if BestId > 0 then
      begin
        if SymbolTable[BestId].CallMode in [cmNONE, cmSTATIC] then
          CreateError(errCannotOverrideStaticMethod, [])
        else
          CreateError(errDeclarationDiffersFromPreviousDeclaration,
            [GetSymbolRec(SubId).Name]);
      end
      else
        CreateError(errMethodDoesNotExistsInTheBaseClass,
          [GetSymbolRec(SubId).Name]);
    end;
  end
  else if SymbolTable[temp].CallMode in [cmNONE, cmSTATIC] then
  begin
    if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
    begin
      // fix default constructor
      GetSymbolRec(SubId).CallMode := cmNONE;
    end
    else
      CreateError(errCannotOverrideStaticMethod, []);
  end
  else
  begin
    for J := 0 to List.Count - 1 do
    begin
      I := List[J];
      if Records[I].Op = OP_CHECK_OVERRIDE then
        if Records[I].Arg1 = temp then
        begin
          N := I;
          OperCheckOverride(List);
          GetSymbolRec(SubId).CallMode := GetSymbolRec(temp).CallMode;
          break;
        end;
    end;
  end;
  SR := GetSymbolRec(SubId);
  ST := GetSymbolRec(temp);

  SR.MethodIndex := ST.MethodIndex;
  SR.DynamicMethodIndex := ST.DynamicMethodIndex;
end;

procedure TCode.OperBeginInitConst;

var
  SymbolTable: TSymbolTable;

  procedure CheckArray(Id: Integer);
  var
    ArrayTypeId, RangeTypeId, ElemTypeId, B1, B2, K, I: Integer;
    RI: TCodeRec;
  begin
    ArrayTypeId := SymbolTable[Id].TerminalTypeId;
    SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
    B1 := SymbolTable.GetLowBoundRec(RangeTypeId).value;
    B2 := SymbolTable.GetHighBoundRec(RangeTypeId).value;
    K := 0;
    I := N;
    repeat
      Inc(I);
      if I > Card then
        RaiseError(errInternalError, []);
      RI := Records[I];
      if RI.Op = OP_ITEM then
      begin
        if RI.Arg1 = Id then
          Inc(K);
      end
      else if RI.Op = OP_BEGIN_INIT_CONST then
        Exit
      else if RI.Op = OP_END_INIT_CONST then
      begin
        if RI.Arg1 = Id then
        begin
          if K <> B2 - B1 + 1 then
            RaiseError(errE2072, [K, B2 - B1 + 1]);
          Exit;
        end;
      end;
    until false;
  end;

var
  R, RI, RJ: TCodeRec;
  K, I, J, FinTypeId, OwnerId, ItemNumber, B1, B2, L, KK: Integer;
  ArrayTypeId, RangeTypeId, ElemTypeId, FieldDescriptorId: Integer;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;
  FinTypeId := SymbolTable[R.Arg1].FinalTypeId;

  if FinTypeId = typeARRAY then
    CheckArray(R.Arg1);

  if FinTypeId in [typeRECORD, typeARRAY] then
    Exit;

  if FinTypeId = 0 then // this is a nested initializer
  begin
    if Self[N + 1].Op = OP_ASSIGN then
    begin
      if GetSymbolRec(Self[N + 1].Arg2).HasPWideCharType then
      begin
        SymbolTable[R.Arg1].TypeId := typeUNICSTRING;
        Exit;
      end
{$IFNDEF PAXARM}
      else if GetSymbolRec(Self[N + 1].Arg2).HasPAnsiCharType then
      begin
        SymbolTable[R.Arg1].TypeId := typeANSISTRING;
        Exit;
      end;
{$ENDIF}
    end;

    K := 0;
    for I := N - 1 downto 1 do
    begin
      RI := Self[I];
      if RI.Op = OP_BEGIN_INIT_CONST then
      begin
        Inc(K);
        if K = 1 then
        begin
          OwnerId := RI.Arg1;
          FinTypeId := SymbolTable[OwnerId].FinalTypeId;

          ItemNumber := -1;

          for J := N + 1 to Card do
          begin
            RJ := Self[J];
            if RJ.Op = OP_ASSIGN_SHIFT then
              if RJ.Res = R.Arg1 then
              begin
                ItemNumber := RJ.Arg2;

                RJ.Arg1 := OwnerId;
                break;
              end;
          end;

          case FinTypeId of
            typeRECORD:
              begin
                if ItemNumber = -1 then
                  RaiseError(errInternalError, []);
                FieldDescriptorId := SymbolTable.GetFieldDescriptorId(OwnerId,
                  ItemNumber);
                SymbolTable[R.Arg1].TypeId :=
                  SymbolTable[FieldDescriptorId].TypeId;
              end;
            typeARRAY:
              begin
                ArrayTypeId := SymbolTable[OwnerId].TerminalTypeId;
                SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId,
                  ElemTypeId);
                SymbolTable[R.Arg1].TypeId := ElemTypeId;
                if GetSymbolRec(R.Arg1).FinalTypeId = typeARRAY then
                  CheckArray(R.Arg1);
                B1 := SymbolTable.GetLowBoundRec(RangeTypeId).value;
                B2 := SymbolTable.GetHighBoundRec(RangeTypeId).value;
                if ItemNumber > B2 - B1 then
                  CreateError(errRangeCheckError, []);

                if ItemNumber > 0 then
                  Exit;

                L := 0;
                KK := 0;

                if SymbolTable[ElemTypeId].FinalTypeId = typeARRAY then
                begin
                  for J := N to Card do
                  begin
                    RJ := Self[J];
                    if RJ.Op = OP_BEGIN_INIT_CONST then
                    begin
                      Inc(KK);
                      if KK = 1 then
                        Inc(L);
                    end
                    else if RJ.Op = OP_END_INIT_CONST then
                    begin
                      Dec(KK);
                      if KK < 0 then
                        break;
                    end;
                  end;

                  if L <> B2 - B1 + 1 then
                    CreateError(errE2072, [L, B2 - B1 + 1]);
                end;
              end
          else
            CreateError(errIncompatibleTypesNoArgs, []);
          end;

          Exit;
        end;
      end
      else if RI.Op = OP_END_INIT_CONST then
      begin
        Dec(K);
      end;
    end;
  end;
end;

procedure TCode.OperEndInitConst;
begin
end;

procedure TCode.OperItem;
var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  K, FieldDescriptorId, FinTypeId, ArrayTypeId, RangeTypeId, ElemTypeId, H1,
    H2: Integer;
  S: String;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;

  FinTypeId := SymbolTable[R.Arg1].FinalTypeId;

  case FinTypeId of
    typeRECORD:
      begin
        K := R.Arg2;
        FieldDescriptorId := SymbolTable.GetFieldDescriptorId(R.Arg1, K);
        S := SymbolTable[FieldDescriptorId].Name;
        SymbolTable[R.Res].Name := S;
        SymbolTable[R.Res].TypeId := FieldDescriptorId;
        SymbolTable[R.Res].OwnerId := R.Arg1;
        R.Arg2 := R.Res;
        R.Op := OP_FIELD;
        Dec(N);
      end;
    typeARRAY:
      begin
        K := R.Arg2;

        ArrayTypeId := SymbolTable[R.Arg1].TypeId;
        SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);

        H1 := SymbolTable.GetLowBoundRec(RangeTypeId).value;
        H2 := SymbolTable.GetHighBoundRec(RangeTypeId).value;

        if K + H1 > H2 then
          CreateError(errConstantExpressionViolatesSubrangeBounds, []);

        R.Arg2 := CreateConst(SymbolTable[RangeTypeId].TypeId, K + H1);
        SymbolTable[R.Res].TypeId := ElemTypeId;
        R.Op := OP_ELEM;
        Dec(N);
      end;
  else
    CreateError(errIncompatibleTypesNoArgs, []);
  end;
end;

procedure TCode.OperRecordItem;
var
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  FieldDescriptorId, FinTypeId: Integer;
  S: String;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;

  FinTypeId := SymbolTable[R.Arg1].FinalTypeId;

  case FinTypeId of
    typeRECORD:
      begin
        S := SymbolTable[R.Arg2].value;
        FieldDescriptorId := SymbolTable.GetFieldDescriptorIdByName(R.Arg1, S);

        if FieldDescriptorId = 0 then
          CreateError(errUndeclaredIdentifier, [S]);

        SymbolTable[R.Res].Name := S;
        SymbolTable[R.Res].TypeId := FieldDescriptorId;
        SymbolTable[R.Res].OwnerId := R.Arg1;
        R.Arg2 := R.Res;
        R.Op := OP_FIELD;
        Dec(N);
      end;
  else
    CreateError(errIncompatibleTypesNoArgs, []);
  end;

end;

procedure TCode.OperElem;
var
  Arg1, Arg2, Res, K1, K2, ArrayTypeId, TRange, TElem: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  FinalOwnerId: Integer;
  S: String;
  I, J, LastI, LastN: Integer;
  RI, RC: TCodeRec;
  Id: Integer;
  OldList: TCodeRecList;
  NewList: TCodeRecList;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  Res := R.Res;
  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  OldList := TCodeRecList.Create;
  NewList := TCodeRecList.Create;

  try

    if K1 = KindVAR then
    begin

      if GetSymbolRec(Arg1).IsFWArrayVar then
      begin
        GetSymbolRec(Res).ByRef := true;

        ArrayTypeId := GetSymbolRec(Arg1).TerminalTypeId;
        TElem := GetSymbolRec(ArrayTypeId).PatternId;
        TElem := GetSymbolRec(TElem).PatternId;

        GetSymbolRec(Res).TypeId := TElem;

        Exit;
      end;

      if GetSymbolRec(Arg1).FinalTypeId = typeDYNARRAY then
      begin
        GetSymbolRec(Res).ByRef := true;

        ArrayTypeId := GetSymbolRec(Arg1).TerminalTypeId;
        TElem := GetSymbolRec(ArrayTypeId).PatternId;

        GetSymbolRec(Res).TypeId := TElem;
        Exit;
      end;

      if GetSymbolRec(Arg1).FinalTypeId = typeOPENARRAY then
      begin
        GetSymbolRec(Res).ByRef := true;

        ArrayTypeId := GetSymbolRec(Arg1).TerminalTypeId;
        TElem := GetSymbolRec(ArrayTypeId).PatternId;

        GetSymbolRec(Res).TypeId := TElem;
        Exit;
      end;
{$IFNDEF PAXARM}
      if GetSymbolRec(Arg1).FinalTypeId = typeANSISTRING then
      begin
        GetSymbolRec(Res).ByRef := true;
        GetSymbolRec(Res).TypeId := typeANSICHAR;
        Exit;
      end;

      if GetSymbolRec(Arg1).FinalTypeId = typeSHORTSTRING then
      begin
        GetSymbolRec(Res).ByRef := true;
        GetSymbolRec(Res).TypeId := typeANSICHAR;
        Exit;
      end;

      if GetSymbolRec(Arg1).FinalTypeId = typeWIDESTRING then
      begin
        GetSymbolRec(Res).ByRef := true;
        GetSymbolRec(Res).TypeId := typeWIDECHAR;
        Exit;
      end;
{$ENDIF}
      if GetSymbolRec(Arg1).FinalTypeId = typeUNICSTRING then
      begin
        GetSymbolRec(Res).ByRef := true;
        GetSymbolRec(Res).TypeId := typeWIDECHAR;
        Exit;
      end;

      if GetSymbolRec(Arg1).FinalTypeId <> typeARRAY then
      begin
        if GetSymbolRec(Arg1).FinalTypeId in [typeCLASS, typeINTERFACE] then
        begin
          I := SymbolTable.FindDefaultPropertyId
            (GetSymbolRec(Arg1).TerminalTypeId);
          if I <> 0 then
          begin
            if R.Language = JS_LANGUAGE then
              if Records[N + 1].Op = OP_ELEM then
                if Records[N + 1].Arg1 = Res then
                begin
                  Id := NewTempVar(GetSymbolRec(Res).Level, 0);
                  RC := TCodeRec.Create(OP_ASSIGN, Self);
                  RC.Arg1 := Id;
                  RC.Arg2 := Res;
                  RC.Res := Id;

                  GetSymbolRec(Id).TypeId := GetSymbolRec(Arg1).TypeId;
                  GetSymbolRec(Res).TypeId := GetSymbolRec(Arg1).TypeId;

                  Records[N + 1].Arg1 := Id;
                  Insert(N + 1, RC);
                  Dec(N);

                  Exit;
                end;

            S := GetSymbolRec(I).Name;
            Id := NewTempVar(GetLevel(N), GetSymbolRec(I).TypeId);

            GetSymbolRec(Id).OwnerId := R.Arg1;
            GetSymbolRec(Id).Name := GetSymbolRec(I).Name;

            RC := TCodeRec.Create(OP_FIELD, Self);
            RC.Arg1 := R.Arg1;
            RC.Arg2 := Id;
            RC.Res := Id;

            R.Arg1 := Id;
            Insert(N, RC);

            Dec(N);

            Exit;
          end
          else
          begin
            S := GetSymbolRec(GetSymbolRec(Arg1).TerminalTypeId).Name;
            CreateError(errClassDoesNotHaveDefaultProperty, [S]);
            Exit;
          end;
        end
        else if GetSymbolRec(Arg1).FinalTypeId in VariantTypes then
        begin
          OldList.Add(Records[N].Clone);
          NewList.Add(Records[N]);

          J := 1;
          K1 := R.Res;
          Records[N].Op := OP_VARARRAY_IDX;
          GetSymbolRec(Records[N].Res).Kind := KindNONE;
          Records[N].Res := 0;

          LastI := N;

          I := N + 1;
          while I <= Card do
          begin
            if Records[I].Op = OP_ELEM then
            begin
              if Records[I].Arg1 = K1 then
              begin
                OldList.Add(Records[I].Clone);
                NewList.Add(Records[I]);

                K1 := Records[I].Res;
                Records[I].Op := OP_VARARRAY_IDX;
                Records[I].Arg1 := R.Arg1;
                GetSymbolRec(Records[I].Res).Kind := KindNONE;
                Records[I].Res := 0;

                LastI := I;

                Inc(J);
              end;
            end
            else if (Records[I].Op = OP_ASSIGN) and (Records[I].Arg1 = K1) then
            begin
              break;
            end;
            Inc(I);
          end;

          if I > Card then
            I := LastI + 1;

          if (Records[I].Op = OP_ASSIGN) and (Records[I].Arg1 = K1) then
          begin
            LastN := N;

            RI := Records[I];

            if GetSymbolRec(RI.Arg2).TypeId = 0 then
            begin
              MoveRValue(I);

              for I := 0 to OldList.Count - 1 do
              begin
                TCodeRec(NewList[I]).Op := TCodeRec(OldList[I]).Op;
                TCodeRec(NewList[I]).Arg1 := TCodeRec(OldList[I]).Arg1;
                TCodeRec(NewList[I]).Arg2 := TCodeRec(OldList[I]).Arg2;
                TCodeRec(NewList[I]).Res := TCodeRec(OldList[I]).Res;
              end;

              Dec(N);
              Exit;
            end
            else if GetSymbolRec(RI.Arg2).FinalTypeId <> typeVARIANT then
              InsertConversionToVariant(I, 2);

            RI.Op := OP_VARARRAY_PUT;
            RI.Arg1 := R.Arg1;
            RI.Res := RI.Arg2;
            RI.Arg2 := J;

            N := LastN;
          end
          else
          begin
            RC := TCodeRec.Create(OP_VARARRAY_GET, Self);
            RC.Arg1 := R.Arg1;
            RC.Arg2 := J;
            RC.Res := K1;
            Insert(I, RC);

            GetSymbolRec(K1).TypeId := typeVARIANT;
            GetSymbolRec(K1).Kind := KindVAR;
            GetSymbolRec(K1).OwnerId := 0;

            InsertDeclareTempVar(RC.Res);
          end;

          Dec(N);
        end
        else
          CreateError(errArrayTypeRequired, []);

        Exit;
      end;

      ArrayTypeId := SymbolTable[Arg1].TerminalTypeId;
      SymbolTable.GetArrayTypeInfo(ArrayTypeId, TRange, TElem);

      GetSymbolRec(Res).TypeId := TElem;

      if (K2 = KindCONST) and (not GetSymbolRec(Arg1).ByRef) then
      begin
        CheckSubrangeBounds(TRange, GetSymbolRec(Arg2));
        FinalOwnerId := GetSymbolRec(Res).FinalOwnerId;
        if GetSymbolRec(FinalOwnerId).Host or GetSymbolRec(FinalOwnerId).ByRef
        then
          GetSymbolRec(Res).ByRef := true
        else
        begin
          // save index value for TSymbolTable.SetShifts
          // GetSymbolRec(Res).Value := GetSymbolRec(Arg2).Value;
          // R.Op := OP_NOP;
          GetSymbolRec(Res).ByRef := true;
        end;
      end
      else
      begin
        GetSymbolRec(Res).ByRef := true;
      end;
    end
    else if K1 = KindCONST then
    begin
      if GetSymbolRec(Arg1).HasPWideCharType then
      begin
        GetSymbolRec(Res).TypeId := typeWIDECHAR;
        if K2 = KindCONST then
        begin
          GetSymbolRec(Res).Kind := KindCONST;
          S := GetSymbolRec(Arg1).value;
          I := GetSymbolRec(Arg2).value;
          GetSymbolRec(Res).value := Integer(S[I]);
          R.Op := OP_NOP;
        end
        else if K2 = KindVAR then
        begin
          GetSymbolRec(Res).ByRef := true;
          GetSymbolRec(Res).TypeId := typeWIDECHAR;
        end
        else
          CreateError(errIncompatibleTypesNoArgs, []);
      end
{$IFNDEF PAXARM}
      else if GetSymbolRec(Arg1).HasPAnsiCharType then
      begin
        GetSymbolRec(Res).TypeId := typeANSICHAR;
        if K2 = KindCONST then
        begin
          GetSymbolRec(Res).Kind := KindCONST;
          S := GetSymbolRec(Arg1).value;
          I := GetSymbolRec(Arg2).value;
          GetSymbolRec(Res).value := Integer(S[I]);
          R.Op := OP_NOP;
        end
        else if K2 = KindVAR then
        begin
          GetSymbolRec(Res).ByRef := true;
          GetSymbolRec(Res).TypeId := typeANSICHAR;
        end
        else
          CreateError(errIncompatibleTypesNoArgs, []);
      end
{$ENDIF}
      else
        CreateError(errArrayTypeRequired, []);
    end
    else if K1 = KindSUB then
    begin
      R.Op := OP_CALL;
      R.GenOp := OP_CALL;

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := R.Arg2;
      RC.Arg2 := 0;
      RC.Res := R.Arg1;

      R.Arg2 := 1;

      Insert(N, RC);
      Dec(N);

      for I := N downto 1 do
        if Records[I].Op = OP_STMT then
        begin
          Records[I].Op := OP_NOP;
          break;
        end;
    end
    else
      CreateError(errArrayTypeRequired, []);

  finally

    FreeAndNil(NewList);
    for I := 0 to OldList.Count - 1 do
{$IFDEF ARC}
      OldList[I] := nil;
{$ELSE}
      TCodeRec(OldList[I]).Free;
{$ENDIF}
    FreeAndNil(OldList);

  end;
end;

procedure TCode.OperPrint;
var
  R: TCodeRec;
  Arg1, Arg2, Res, T1, K1, SubId: Integer;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  Res := R.Res;

  if (Arg1 = 0) and (Arg2 = 0) and (Res = 0) then // writeln
  begin
    SubId := SymbolTable.LookUp(strWriteln, 0, false);
    R.Op := OP_CALL;
    R.Arg1 := SubId;
    R.Arg2 := 0;
    R.Res := 0;
    if SubId = 0 then
      RaiseError(errInternalError, []);
    Dec(N);
    Exit;
  end;

  T1 := GetSymbolRec(Arg1).FinalTypeId;

  if GetSymbolRec(Arg1).HasPWideCharType then
    T1 := typePWIDECHAR;

  K1 := GetSymbolRec(Arg1).Kind;
  if K1 in [KindVAR, KindCONST] then
  begin
    case T1 of
      typeBOOLEAN:
        begin
          SubId := Id_WriteBool;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
      typeBYTEBOOL:
        begin
          SubId := Id_WriteByteBool;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
      typeWORDBOOL:
        begin
          SubId := Id_WriteWordBool;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
      typeLONGBOOL:
        begin
          SubId := Id_WriteLongBool;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
      typeCLASS:
        begin
          SubId := Id_WriteObject;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
{$IFNDEF PAXARM}
      typeANSICHAR:
        begin
          SubId := Id_WriteAnsiChar;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
{$ENDIF}
      typeWIDECHAR:
        begin
          SubId := Id_WriteWideChar;

          if (Arg2 <> 0) or (Res <> 0) then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 1;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);
        end;
      typeBYTE:
        begin
          SubId := Id_WriteByte;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeWORD:
        begin
          SubId := Id_WriteWord;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeCARDINAL:
        begin
          SubId := Id_WriteCardinal;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeSMALLINT:
        begin
          SubId := Id_WriteSmallInt;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeSHORTINT:
        begin
          SubId := Id_WriteShortInt;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeINTEGER, typeENUM:
        begin
          SubId := Id_WriteInt;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeINT64, typeUINT64:
        begin
          SubId := Id_WriteInt64;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typePOINTER:
        begin
          if GetSymbolRec(Arg1).HasPWideCharType then
          begin
            SubId := Id_WriteUnicString;

            if Res <> 0 then
              CreateError(errIllegalTypeInWriteStatememt, []);

            R.Op := OP_CALL;
            R.Arg1 := SubId;
            R.Arg2 := 2;
            R.Res := 0;

            R := TCodeRec.Create(OP_PUSH, Self);
            R.Arg1 := Arg1;
            R.Arg2 := 0; // param number
            R.Res := SubId;

            Insert(N, R);

            R := TCodeRec.Create(OP_PUSH, Self);
            if Arg2 = 0 then
              R.Arg1 := CreateConst(typeINTEGER, 0)
            else
              R.Arg1 := Arg2;
            R.Arg2 := 1; // param number
            R.Res := SubId;

            Insert(N + 1, R);
          end
{$IFNDEF PAXARM}
          else if GetSymbolRec(Arg1).HasPAnsiCharType then
          begin
            SubId := Id_WriteString;

            if Res <> 0 then
              CreateError(errIllegalTypeInWriteStatememt, []);

            R.Op := OP_CALL;
            R.Arg1 := SubId;
            R.Arg2 := 2;
            R.Res := 0;

            R := TCodeRec.Create(OP_PUSH, Self);
            R.Arg1 := Arg1;
            R.Arg2 := 0; // param number
            R.Res := SubId;

            Insert(N, R);

            R := TCodeRec.Create(OP_PUSH, Self);
            if Arg2 = 0 then
              R.Arg1 := CreateConst(typeINTEGER, 0)
            else
              R.Arg1 := Arg2;
            R.Arg2 := 1; // param number
            R.Res := SubId;

            Insert(N + 1, R);
          end
{$ENDIF}
          else
            CreateError(errIllegalTypeInWriteStatememt, []);
        end;
{$IFNDEF PAXARM}
      typeANSISTRING:
        begin
          SubId := Id_WriteString;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeSHORTSTRING:
        begin
          SubId := Id_WriteShortString;

          if SubId = 0 then
            RaiseError(errInternalError, []);

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeWIDESTRING, typePWIDECHAR:
        begin
          SubId := Id_WriteWideString;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
{$ENDIF}
      typeUNICSTRING:
        begin
          SubId := Id_WriteUnicString;

          if Res <> 0 then
            CreateError(errIllegalTypeInWriteStatememt, []);

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 2;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;

          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;

          Insert(N + 1, R);
        end;
      typeDOUBLE:
        begin
          SubId := Id_WriteDouble;

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 3;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;
          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;
          Insert(N + 1, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Res = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Res;
          R.Arg2 := 2; // param number
          R.Res := SubId;
          Insert(N + 2, R);
        end;
      typeSINGLE:
        begin
          SubId := Id_WriteSingle;

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 3;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;
          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;
          Insert(N + 1, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Res = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Res;
          R.Arg2 := 2; // param number
          R.Res := SubId;
          Insert(N + 2, R);

        end;
      typeCURRENCY:
        begin
          SubId := Id_WriteCurrency;

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 3;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;
          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;
          Insert(N + 1, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Res = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Res;
          R.Arg2 := 2; // param number
          R.Res := SubId;
          Insert(N + 2, R);

        end;
      typeEXTENDED:
        begin
          SubId := Id_WriteExtended;

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 3;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;
          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;
          Insert(N + 1, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Res = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Res;
          R.Arg2 := 2; // param number
          R.Res := SubId;
          Insert(N + 2, R);

        end;
      typeVARIANT, typeOLEVARIANT:
        begin
          SubId := Id_WriteVariant;

          R.Op := OP_CALL;
          R.Arg1 := SubId;
          R.Arg2 := 3;
          R.Res := 0;

          R := TCodeRec.Create(OP_PUSH, Self);
          R.Arg1 := Arg1;
          R.Arg2 := 0; // param number
          R.Res := SubId;
          Insert(N, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Arg2 = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Arg2;
          R.Arg2 := 1; // param number
          R.Res := SubId;
          Insert(N + 1, R);

          R := TCodeRec.Create(OP_PUSH, Self);
          if Res = 0 then
            R.Arg1 := CreateConst(typeINTEGER, 0)
          else
            R.Arg1 := Res;
          R.Arg2 := 2; // param number
          R.Res := SubId;
          Insert(N + 2, R);
        end;
    else
      CreateError(errIllegalTypeInWriteStatememt, []);
    end;
  end
  else
    CreateError(errIllegalTypeInWriteStatememt, []);
end;

procedure TCode.OperDetermineType;
var
  I, Id, DestId, DestTypeId, SubId, ParamNumber: Integer;
begin
  Id := Records[N].Arg1;
  for I := N to Card do
    if Records[I].Op = OP_CALL then
    begin
      if (Records[I].Res = Id) and (Records[I].Arg1 <> Id_FWArray_Create) then
      begin
        DestId := Records[I].Arg1;
        DestTypeId := GetSymbolRec(DestId).TypeId;

        if DestTypeId = 0 then
        begin
          RaiseError(errIncompatibleTypesNoArgs, []);
        end;

        DestTypeId := GetSymbolRec(DestId).TerminalTypeId;

        if GetSymbolRec(DestTypeId).FinalTypeId = typeOPENARRAY then
        begin
          DestTypeId := TKernel(kernel).SymbolTable.RegisterDynamicArrayType(0,
            '', GetSymbolRec(DestTypeId).PatternId);
        end;

        GetSymbolRec(Id).TypeId := DestTypeId;
        Exit;
      end;
    end
    else if Records[I].Op = OP_ASSIGN then
    begin
      if Records[I].Arg2 = Id then
      begin
        DestId := Records[I].Arg1;
        DestTypeId := GetSymbolRec(DestId).TypeId;
        if DestTypeId = 0 then
        begin
          RaiseError(errIncompatibleTypesNoArgs, []);
        end;
        DestTypeId := GetSymbolRec(DestId).TerminalTypeId;

        if GetSymbolRec(DestTypeId).FinalTypeId = typeOPENARRAY then
        begin
          DestTypeId := TKernel(kernel).SymbolTable.RegisterDynamicArrayType(0,
            '', GetSymbolRec(DestTypeId).PatternId);
        end;

        GetSymbolRec(Id).TypeId := DestTypeId;
        Records[N].Op := OP_NOP;
        if Records[N + 1].Op = OP_PUSH_CLASSREF then
          if Records[N + 1].Arg1 = 0 then
            Records[N + 1].Arg1 := DestTypeId;

        Exit;
      end;
    end
    else if Records[I].Op = OP_PUSH then
    begin
      if Records[I].Arg1 = Id then
      begin
        SubId := Records[I].Res;
        ParamNumber := Records[I].Arg2;
        DestId := TKernel(kernel).SymbolTable.GetParamId(SubId, ParamNumber);
        DestTypeId := GetSymbolRec(DestId).TypeId;
        if DestTypeId = 0 then
        begin
          RaiseError(errIncompatibleTypesNoArgs, []);
        end;
        DestTypeId := GetSymbolRec(DestId).TerminalTypeId;

        if GetSymbolRec(DestTypeId).FinalTypeId = typeOPENARRAY then
        begin
          DestTypeId := TKernel(kernel).SymbolTable.RegisterDynamicArrayType(0,
            '', GetSymbolRec(DestTypeId).PatternId);
        end;

        GetSymbolRec(Id).TypeId := DestTypeId;
        Records[N].Op := OP_NOP;

        if Records[N + 1].Op = OP_PUSH_CLASSREF then
          if Records[N + 1].Arg1 = 0 then
            Records[N + 1].Arg1 := DestTypeId;

        Exit;
      end;
    end;

  RaiseError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperCreateShortStringType;
var
  K2, T2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  T2 := GetSymbolRec(R.Arg2).FinalTypeId;
  K2 := GetSymbolRec(R.Arg2).Kind;

  R.Op := OP_NOP;

  if not(T2 in IntegerTypes) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if K2 <> KindCONST then
  begin
    CreateError(errConstantExpressionExpected, []);
    Exit;
  end;

  GetSymbolRec(R.Arg1).Count := GetSymbolRec(R.Arg2).value;
end;

procedure TCode.OperCheckSubrangeType;
var
  SymbolTable: TSymbolTable;
  Arg1, Arg2, T1, T2: Integer;
  R: TCodeRec;
  B1, B2: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;

  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 <> T2 then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;

  B1 := SymbolTable[Arg1].value;
  B2 := SymbolTable[Arg2].value;
  if B1 > B2 then
    CreateError(errLowBoundExceedsHighBound, []);

  R.Op := OP_NOP;
end;

procedure TCode.OperAssignEnum;
var
  Arg1, Arg2, T1, T2, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;
  K2 := GetSymbolRec(Arg2).Kind;

  R.Op := OP_NOP;

  if K2 <> KindCONST then
    if T2 <> typeENUM then
    begin
      CreateError(errConstantExpressionExpected, []);
      Exit;
    end;

  if (T1 = typeENUM) and (T2 in (IntegerTypes + [typeENUM])) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    GetSymbolRec(Arg1).Kind := KindCONST;
  end
  else
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

function TCode.MatchSetTypes(T1, T2: Integer): Boolean;
begin
  T1 := GetSymbolRec(T1).PatternId;
  T2 := GetSymbolRec(T2).PatternId;

  if T1 = T2 then
  begin
    result := true;
    Exit;
  end;

  result := ExistsOrdinalRelationalOperator(T1, T2) or (T1 = typeVOID) or
  // empty set
    (T2 = typeVOID); // not empty set
end;

procedure TCode.CreateSetObject(Id: Integer);
var
  I, J, temp, K, B1, B2: Integer;
  SetObject: TSetObject;
  ByteSet: TByteSet;
begin
  ByteSet := [];

  for I := 1 to Card do
  begin
    if Records[I].Op = OP_STMT then
      break;

    if (Records[I].Op = OP_SET_INCLUDE) and (Records[I].Arg1 = Id) then
    begin
      temp := Records[I].Arg2;
      K := GetSymbolRec(temp).Kind;
      if K = KindCONST then
      begin
        if TVarData(GetSymbolRec(temp).value).VType in IntegerVariantTypes then
          ByteSet := ByteSet + [Integer(GetSymbolRec(temp).value)]
        else
        begin
          N := I;
          RaiseError(errIncompatibleTypesNoArgs, []);
        end;
      end
      else
        Exit;
    end
    else if (Records[I].Op = OP_SET_INCLUDE_INTERVAL) and (Records[I].Arg1 = Id)
    then
    begin
      temp := Records[I].Arg2;
      K := GetSymbolRec(temp).Kind;
      if K <> KindCONST then
        Exit;
      B1 := GetSymbolRec(temp).value;

      temp := Records[I].Res;
      K := GetSymbolRec(temp).Kind;
      if K <> KindCONST then
        Exit;
      B2 := GetSymbolRec(temp).value;

      for J := B1 to B2 do
        ByteSet := ByteSet + [J];
    end;
  end;

  if ByteSet <> [] then
  begin
    SetObject := TSetObject.Create(TKernel(kernel).SymbolTable, ByteSet,
      GetSymbolRec(Id).TypeId, TKernel(kernel).SymbolTable.GetTypeBase
      (GetSymbolRec(Id).TypeId));
    GetSymbolRec(Id).value := VarObjectToVariant(SetObject);
    GetSymbolRec(Id).Kind := KindCONST;
  end;
end;

procedure TCode.OperSizeOf;
var
  R: TCodeRec;
  T, SZ: Integer;
begin
  R := Records[N];
  if GetSymbolRec(R.Arg1).FinalTypeId = typePOINTER then
  begin
    if GetSymbolRec(R.Arg1).Kind = KindTYPE then
      T := typePOINTER
    else
      T := GetSymbolRec(GetSymbolRec(R.Arg1).TerminalTypeId).PatternId;
  end
  else
    T := GetSymbolRec(R.Arg1).TerminalTypeId;

  SZ := GetSymbolRec(T).Size;
  if SZ > 0 then
  begin
    GetSymbolRec(R.Res).value := SZ;
    GetSymbolRec(R.Res).Kind := KindCONST;
    R.Op := OP_NOP;
  end;
end;

procedure TCode.OperAssignmentConst;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T2 = typeSET then
    CreateSetObject(Arg2);

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  R.Op := OP_NOP;

  if K2 <> KindCONST then
  begin
    if TKernel(kernel).InterfaceOnly then
      Exit;

    // CreateError(errConstantExpressionExpected, []);
    // Exit;
  end;

  if (T1 in [0, typeENUM]) and (T2 = typeENUM) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := GetSymbolRec(Arg2).TypeId;
  end
  else if (T1 in [0, typeBOOLEAN]) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(Arg1).value := Boolean(GetSymbolRec(Arg2).value);
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeBOOLEAN;
  end
  else if (T1 in [0, typeBYTEBOOL]) and (T2 in BooleanTypes) then
  begin
{$IFDEF UNIX}
    GetSymbolRec(Arg1).value := Boolean(GetSymbolRec(Arg2).value);
{$ELSE}
    GetSymbolRec(Arg1).value := ByteBool(Byte(GetSymbolRec(Arg2).value));
{$ENDIF}
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeBYTEBOOL;
  end
  else if (T1 in [0, typeWORDBOOL]) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(Arg1).value := WordBool(GetSymbolRec(Arg2).value);
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeWORDBOOL;
  end
  else if (T1 in [0, typeLONGBOOL]) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(Arg1).value := LongBool(GetSymbolRec(Arg2).value);
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeLONGBOOL;
  end
{$IFNDEF PAXARM}
  else if (T1 in [0, typeANSICHAR]) and (T2 in CharTypes) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeANSICHAR;
  end
{$ENDIF}
  else if (T1 in [0, typeWIDECHAR]) and (T2 in CharTypes) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeWIDECHAR;
  end
  else if (T1 in [0, typeINTEGER]) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
    begin
      if Abs(GetSymbolRec(Arg1).value) > MaxInt then
        GetSymbolRec(Arg1).TypeId := typeINT64
      else
        GetSymbolRec(Arg1).TypeId := typeINTEGER;
    end;
    CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
  end
  else if (T1 in [0, typeBYTE]) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeBYTE;
    CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
  end
  else if (T1 in [0, typeWORD]) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeWORD;
    CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
  end
  else if (T1 in [0, typeDOUBLE]) and
    (T2 in (IntegerTypes + RealTypes + [typeCURRENCY] + VariantTypes)) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeDOUBLE;
  end
  else if (T1 in [0, typeCURRENCY]) and
    (T2 in (IntegerTypes + RealTypes + [typeCURRENCY] + VariantTypes)) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeCURRENCY;
  end
  else if (T1 in [0, typeSINGLE]) and
    (T2 in (IntegerTypes + RealTypes + [typeCURRENCY] + VariantTypes)) then
  begin
    GetSymbolRec(Arg1).value := Single(GetSymbolRec(Arg2).value);
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeSINGLE;
  end
  else if (T1 in [0, typeEXTENDED]) and
    (T2 in (IntegerTypes + RealTypes + [typeCURRENCY] + VariantTypes)) then
  begin
    GetSymbolRec(Arg1).value := Single(GetSymbolRec(Arg2).value);
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeEXTENDED;
  end

{$IFNDEF PAXARM}
  else if (T1 in [0, typeANSISTRING]) and (T2 = typeANSISTRING) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeANSISTRING;
  end
  else if (T1 in [0, typeANSISTRING, typeWIDESTRING, typeSHORTSTRING]) and
    GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
    begin
      if K1 = KindCONST then
        GetSymbolRec(Arg1).TypeId := typePANSICHAR
      else
        GetSymbolRec(Arg1).TypeId := typeANSISTRING;
    end;
  end
  else if (T1 in [0, typeANSISTRING, typeWIDESTRING, typeSHORTSTRING]) and
    (T2 = typeANSICHAR) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
    begin
      if K1 = KindCONST then
        GetSymbolRec(Arg1).TypeId := typePANSICHAR
      else
        GetSymbolRec(Arg1).TypeId := typeANSISTRING;
    end;
  end
{$ENDIF}
  // unic string

  else if (T1 in [0, typeUNICSTRING]) and (T2 = typeUNICSTRING) then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := typeUNICSTRING;
  end
  else if (T1 in ([0] + StringTypes)) and (T2 in CharTypes) then
  begin
    GetSymbolRec(Arg1).value := Chr(Integer(GetSymbolRec(Arg2).value));
    if T1 = 0 then
    begin
      if K1 = KindCONST then
{$IFDEF PAXARM}
        GetSymbolRec(Arg1).TypeId := typePWIDECHAR
{$ELSE}
        GetSymbolRec(Arg1).TypeId := typePANSICHAR
{$ENDIF}
      else
        GetSymbolRec(Arg1).TypeId := typeSTRING;
    end;
  end
{$IFNDEF PAXARM}
  else if (T1 in ([0] + StringTypes)) and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
    begin
      if K1 = KindCONST then
        GetSymbolRec(Arg1).TypeId := typePANSICHAR
      else
        GetSymbolRec(Arg1).TypeId := typeANSISTRING;
    end;
  end
{$ENDIF}
  else if (T1 in ([0] + StringTypes)) and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
    if T1 = 0 then
    begin
      if K1 = KindCONST then
        GetSymbolRec(Arg1).TypeId := typePWIDECHAR
      else
{$IFDEF UNIC}
        GetSymbolRec(Arg1).TypeId := typeUNICSTRING;
{$ELSE}
        GetSymbolRec(Arg1).TypeId := typeWIDESTRING;
{$ENDIF}
    end;
  end

  else if (T1 in [0, typeSET]) and (T2 = typeSET) then
  begin
    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := GetSymbolRec(Arg2).TypeId;

    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
  end
  else if (T1 in [0, typePOINTER]) and (T2 = typePOINTER) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;

    if T1 = 0 then
      GetSymbolRec(Arg1).TypeId := GetSymbolRec(Arg2).TypeId;

    GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
  end
  else if T1 = typeINTERFACE then
  begin
    if T2 = typeCLASS then
      R.Op := OP_INTERFACE_FROM_CLASS
    else if T2 = typeINTERFACE then
      R.Op := OP_ASSIGN_INTERFACE
    else if Arg2 = TKernel(kernel).SymbolTable.NilId then
    begin
      R.Op := OP_INTERFACE_CLR;
      GetSymbolRec(Arg1).value := 0;
    end;
  end
  else if T1 = typeCLASS then
  begin
    if Arg2 = TKernel(kernel).SymbolTable.NilId then
      GetSymbolRec(Arg1).value := 0;
  end
  else if T1 = typeCLASSREF then
  begin
    if Arg2 = TKernel(kernel).SymbolTable.NilId then
      GetSymbolRec(Arg1).value := 0;
  end
  else
  begin
    if GetSymbolRec(Arg1).Param then
      CreateError(errParameterCannotHaveDefaultValue, [])
    else
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;

      CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
        GetSymbolRec(T2).Name]);
    end;
  end;
end;

procedure TCode.OperOleValue;
var
  R, RC: TCodeRec;
  Arg1, T1: Integer;
  PropNameId: Integer;
  I: Integer;
begin
  R := Records[N];

  PropNameId := R.Res;

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  if not(T1 in VariantTypes) then
  begin
    RC := InsertConversionToVariant(N, 1);

    for I := N + 1 to Card do
    begin
      if Records[I].Op = OP_STMT then
        break;

      if Records[I].Op = OP_OLE_SET then
        if Records[I].Arg2 = PropNameId then
          Records[I].Res := RC.Res;
    end;

    Inc(N);
  end;
  R.Op := OP_NOP;
end;

procedure TCode.OperOleParam;
var
  R, RC, RI: TCodeRec;
  Arg1, T1: Integer;
  PropNameId: Integer;
  I: Integer;
  TempVarId, N1: Integer;
begin
  R := Records[N];

  PropNameId := R.Res;

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  if not(T1 in VariantTypes) then
  begin
    RC := InsertConversionToVariant(N, 1);

    TempVarId := RC.Arg1;
    N1 := 0;

    for I := N + 1 to Card do
    begin
      RI := Records[I];

      if RI.Op = OP_STMT then
        break;

      if RI.Op = OP_OLE_SET then
      begin
        if RI.Arg2 = PropNameId then
        begin
          RI.Res := RC.Res;
          break;
        end;
      end
      else if RI.Op = OP_OLE_GET then
        if RI.Arg2 = PropNameId then
        begin
          N1 := I;
          break;
        end;

    end;

    Inc(N);

    if N1 > 0 then
      if GetSymbolRec(Arg1).Kind = KindVAR then
        if not GetSymbolRec(Arg1).IsConst then
        begin
          RC := TCodeRec.Create(OP_ASSIGN, Self);
          RC.Arg1 := Arg1;
          RC.Arg2 := TempVarId;
          RC.Res := Arg1;
          Insert(N1 + 1, RC);
        end;
  end;
end;

procedure TCode.OperBeginCrtJsFuncObject;
begin
  CRT_JS_FUNC_OBJECT_SWITCH := true;
end;

procedure TCode.OperEndCrtJsFuncObject;
begin
  CRT_JS_FUNC_OBJECT_SWITCH := false;
end;

procedure TCode.OperGetNextJSProp;
begin
  GetSymbolRec(Records[N].Arg2).TypeId := typeSTRING;
  GetSymbolRec(Records[N].Res).TypeId := typeBOOLEAN;
end;

procedure TCode.OperJStypeof;
begin
  if GetSymbolRec(Records[N].Arg1).FinalTypeId <> typeVARIANT then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
  end;

  GetSymbolRec(Records[N].Arg1).TypeId := typeVARIANT;
  GetSymbolRec(Records[N].Res).TypeId := typeSTRING;
end;

procedure TCode.OperJSvoid;
begin
  if GetSymbolRec(Records[N].Arg1).FinalTypeId <> typeVARIANT then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
  end;

  GetSymbolRec(Records[N].Arg1).TypeId := typeVARIANT;
  GetSymbolRec(Records[N].Res).TypeId := typeVOID;
end;

procedure TCode.OperFindConstructor;
var
  R, RC, RCall: TCodeRec;
  TypeId, SubId, I, ObjectId, AddressId, FunctionConstructorId,
    InternalLengthId, NN, CallRefId: Integer;
  P: TIntegerList;
  S: String;
begin
  R := Records[N];

  TypeId := R.Arg1;
  if TKernel(kernel).SymbolTable[TypeId].TypeId = TypeALIAS then
    TypeId := TKernel(kernel).SymbolTable[TypeId].TerminalTypeId;

  if R.Language = JS_LANGUAGE then
  begin
    if TKernel(kernel).SymbolTable[TypeId].Kind = KindVAR then
    begin
      S := TKernel(kernel).SymbolTable[TypeId].Name;

      I := TKernel(kernel).SymbolTable.LookUpTypeEx(S, JS_JavaScriptNamespace,
        false, Types.Count);
      if I > 0 then
      begin
        R.Arg1 := I;
        TypeId := I;
      end;
      {
        if S = 'Object' then
        begin
        R.Arg1 := JS_ObjectClassId;
        TypeId := JS_ObjectClassId;
        end;
      }
    end;
  end;

  SubId := TKernel(kernel).SymbolTable.FindConstructorId(TypeId);

  if SubId = 0 then
  begin
    if R.Language = JS_LANGUAGE then
    begin
      R.Op := OP_FIELD;
      with TKernel(kernel) do
      begin
        SymbolTable[R.Res].Name := strInternalCall;
        SymbolTable[R.Res].OwnerId := R.Arg1;
      end;
      R.Arg2 := R.Res;

      ObjectId := R.Arg1;
      CallRefId := R.Res;

      P := TIntegerList.Create;
      I := N;
      repeat
        Inc(I);
        if (Records[I].Op = OP_PUSH) and (Records[I].Res = CallRefId) then
        begin
          P.Add(I);
          Inc(Records[I].Arg2, 1);
        end;
        if Records[I].Op = OP_CALL then
          if Records[I].Arg1 = R.Res then
          begin
            break;
          end;
      until I = Card;

      if I = Card then
        RaiseError(errConstructorNotFoundInClass,
          [TKernel(kernel).SymbolTable[TypeId].Name]);

      RCall := Records[I];

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := CreateConst(typeINTEGER, P.Count);
      RC.Arg2 := 0;
      RC.Res := CallRefId;
      Insert(N + 1, RC);

      FreeAndNil(P);

      NN := N - 1;

      FunctionConstructorId := NewTempVar(GetSymbolRec(R.Res).Level,
        JS_FunctionClassId);
      GetSymbolRec(FunctionConstructorId).OwnerId := JS_FunctionClassId;
      GetSymbolRec(FunctionConstructorId).Name := strInternalCreate;
      RC := TCodeRec.Create(OP_FIELD, Self);
      RC.Arg1 := JS_FunctionClassId;
      RC.Arg2 := FunctionConstructorId;
      RC.Res := FunctionConstructorId;
      Insert(N, RC);
      Inc(N);

      AddressId := NewTempVar(GetSymbolRec(R.Res).Level, typePOINTER);
      GetSymbolRec(AddressId).OwnerId := ObjectId;
      GetSymbolRec(AddressId).Name := strInternalFuncAddr;
      RC := TCodeRec.Create(OP_FIELD, Self);
      RC.Arg1 := ObjectId;
      RC.Arg2 := AddressId;
      RC.Res := AddressId;
      Insert(N, RC);
      Inc(N);

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := AddressId;
      RC.Arg2 := 0; // the first arg
      RC.Res := FunctionConstructorId;
      Insert(N, RC);
      Inc(N);

      InternalLengthId := NewTempVar(GetSymbolRec(R.Res).Level, typeINTEGER);
      GetSymbolRec(InternalLengthId).OwnerId := ObjectId;
      GetSymbolRec(InternalLengthId).Name := strInternalLength;
      RC := TCodeRec.Create(OP_FIELD, Self);
      RC.Arg1 := ObjectId;
      RC.Arg2 := InternalLengthId;
      RC.Res := InternalLengthId;
      Insert(N, RC);
      Inc(N);

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := InternalLengthId;
      RC.Arg2 := 1; // the second arg
      RC.Res := FunctionConstructorId;
      Insert(N, RC);
      Inc(N);

      AddressId := NewTempVar(GetSymbolRec(R.Res).Level, typePOINTER);
      RC := TCodeRec.Create(OP_ADDRESS_PROG, Self);
      RC.Arg1 := 0;
      RC.Arg2 := 0;
      RC.Res := AddressId;
      Insert(N, RC);
      Inc(N);

      RC := TCodeRec.Create(OP_PUSH, Self);
      RC.Arg1 := AddressId;
      RC.Arg2 := 2; // the third arg
      RC.Res := FunctionConstructorId;
      Insert(N, RC);
      Inc(N);

      ObjectId := NewTempVar(GetSymbolRec(R.Res).Level, JS_FunctionClassId);
      RC := TCodeRec.Create(OP_CALL, Self);
      RC.Arg1 := FunctionConstructorId;
      RC.Arg2 := 3;
      RC.Res := ObjectId;
      Insert(N, RC);
      Inc(N);

      R.Arg1 := ObjectId;
      ReplaceIdEx(RCall.Res, ObjectId, N, Card, true);
      RCall.Res := 0;

      N := NN;

      Exit;
    end
    else
      RaiseError(errConstructorNotFoundInClass,
        [TKernel(kernel).SymbolTable[TypeId].Name]);
  end;

  R.Op := OP_FIELD;
  with TKernel(kernel) do
  begin
    SymbolTable[R.Res].Name := SymbolTable[SubId].Name;
    SymbolTable[R.Res].OwnerId := R.Arg1;
  end;
  R.Arg2 := R.Res;
  Dec(N);
end;

function TCode.CheckAssignment(Arg1, Arg2: Integer): Boolean;
var
  T1, T2: Integer;
  SymbolTable: TSymbolTable;
begin
  result := true;

  SymbolTable := TKernel(kernel).SymbolTable;
  T1 := GetSymbolRec(Arg1).TerminalTypeId;
  T2 := GetSymbolRec(Arg2).TerminalTypeId;

  if T1 = T2 then
    Exit;

  if IsJSType(T1, SymbolTable) and IsJSType(T2, SymbolTable) and
    (GetSymbolRec(Arg1).Name <> '') and
    (GetSymbolRec(Arg1).Name <> strInternalConstructor) and
    (GetSymbolRec(Arg2).Name <> '') then
    Exit;

  if SymbolTable.Inherits(T2, T1) then
    Exit;

  result := false;
end;

procedure TCode.OperAssignment;

  function IsFuncResult(Id: Integer): Boolean;
  var
    I: Integer;
  begin
    result := false;
    I := N - 1;
    if I <= 0 then
      Exit;
    while I > 0 do
    begin
      if Records[I].Op = OP_SEPARATOR then
        Dec(I)
      else if Records[I].Op = OP_LVALUE then
        Dec(I)
      else if Records[I].Op = OP_CALL then
      begin
        result := Records[I].Res = Id;
        Exit;
      end
      else
        Exit;
    end;
  end;

  function IsConstElement(Id: Integer): Boolean;
  var
    RC: TCodeRec;
    I: Integer;
  begin
    result := false;
    RC := PrevRec(N);
    if RC <> nil then
    begin
      if RC.Op = OP_ELEM then
      begin
        if RC.Res = Id then
          if GetSymbolRec(RC.Arg1).IsConst then
            result := true;
      end
      else if RC.Op = OP_LVALUE then
      begin
        I := N;
        while Records[I] <> RC do
          Dec(I);
        RC := PrevRec(I);
        if RC.Op = OP_ELEM then
          if RC.Res = Id then
            if GetSymbolRec(RC.Arg1).IsConst then
              result := true;
      end;
    end;
  end;

var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R, RC: TCodeRec;
  SubId1, SubId2: Integer;
{$IFNDEF PAXARM}
  S: String;
{$ENDIF}
  SymbolTable: TSymbolTable;
  SignTypeCast1: Boolean;
  b: Boolean;
  OverList: TIntegerList;
  I: Integer;
label
  ok, err;
begin
  SignTypeCast1 := SignTypeCast;
  SignTypeCast := false;
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if R.Arg1 = TKernel(kernel).SymbolTable.ResultId then
  begin
    GetSymbolRec(Arg1).TypeId := GetSymbolRec(Arg2).TypeId;
    T1 := T2;
  end;

  if T2 = typeSET then
  begin
    if T1 <> typeDYNARRAY then
      CreateSetObject(Arg2);
  end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not SkipCheckFinal then
  begin
    if GetSymbolRec(Arg1).IsFinal then
      K1 := KindCONST
    else
    begin
      if GetSymbolRec(Arg1).PatternId > 0 then
        if GetSymbolRec(GetSymbolRec(Arg1).PatternId).IsFinal then
          K1 := KindCONST;
    end;
  end;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  if GetSymbolRec(Arg1).IsConst then
    CreateError(errLeftSideCannotBeAssignedTo, []);

  if IsConstElement(Arg1) then
    CreateError(errLeftSideCannotBeAssignedTo, []);

  if IsFuncResult(Arg1) then
    CreateError(errLeftSideCannotBeAssignedTo, []);

  if TKernel(kernel).InterfaceOnly then
  begin
    if (T1 = 0) or (T2 = 0) then
    begin
      R.Op := OP_NOP;
      Exit;
    end;
  end;

  if not((K1 = KindVAR) and (K2 in [KindVAR, KindCONST])) then
  begin
    if (T1 = typePROC) and (K2 = KindSUB) then
      goto ok;

    if (T1 = typeCLASSREF) and (K2 = KindTYPE) then
      goto ok;

    if T2 = typeINTERFACE then
      goto ok;

    if Arg2 = SymbolTable.NilId then
      goto ok;

    CreateError(errLeftSideCannotBeAssignedTo, []);
    Exit;
  end;

ok:

  if T1 = 0 then
  begin
    if (R.Language = JS_LANGUAGE) and (T2 = typeINTERFACE) then
    begin
      T1 := typeVARIANT; // ole
      GetSymbolRec(Arg1).TypeId := typeVARIANT;
    end
    else
    begin
      T1 := T2;
      GetSymbolRec(Arg1).TypeId := GetSymbolRec(Arg2).TypeId;
      if
{$IFNDEF PAXARM}
        GetSymbolRec(Arg2).HasPAnsiCharType or
{$ENDIF}
        GetSymbolRec(Arg2).HasPWideCharType then
      begin
        T1 := typeSTRING;
        GetSymbolRec(Arg1).TypeId := T1;
      end;
    end;
  end;

  if (T1 = typeTYPEPARAM) and (T2 = typeTYPEPARAM) then
  begin
    R.Op := OP_ERR_ABSTRACT;
  end
  else if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    R.Op := OP_ASSIGN_INT_M;
  end
  else if (T1 = typeINTEGER) and (T2 = typeINT64) then
  begin
    R.Op := OP_INT_FROM_INT64;
  end
  else if (T1 = typeINTEGER) and (T2 = typeUINT64) then
  begin
    R.Op := OP_INT_FROM_UINT64;
  end
  else if (T1 = typeBYTE) and (T2 = typeINT64) then
  begin
    R.Op := OP_BYTE_FROM_INT64;
  end
  else if (T1 = typeBYTE) and (T2 = typeUINT64) then
  begin
    R.Op := OP_BYTE_FROM_UINT64;
  end
  else if (T1 = typeWORD) and (T2 = typeINT64) then
  begin
    R.Op := OP_WORD_FROM_INT64;
  end
  else if (T1 = typeWORD) and (T2 = typeUINT64) then
  begin
    R.Op := OP_WORD_FROM_UINT64;
  end
  else if (T1 = typeCARDINAL) and (T2 = typeINT64) then
  begin
    R.Op := OP_CARDINAL_FROM_INT64;
  end
  else if (T1 = typeCARDINAL) and (T2 = typeUINT64) then
  begin
    R.Op := OP_CARDINAL_FROM_UINT64;
  end
  else if (T1 = typeSMALLINT) and (T2 = typeINT64) then
  begin
    R.Op := OP_SMALLINT_FROM_INT64;
  end
  else if (T1 = typeSMALLINT) and (T2 = typeUINT64) then
  begin
    R.Op := OP_SMALLINT_FROM_UINT64;
  end
  else if (T1 = typeSHORTINT) and (T2 = typeINT64) then
  begin
    R.Op := OP_SHORTINT_FROM_INT64;
  end
  else if (T1 = typeSHORTINT) and (T2 = typeUINT64) then
  begin
    R.Op := OP_SHORTINT_FROM_UINT64;
  end
  else if (T1 = typeBOOLEAN) and (T2 in BooleanTypes) then
  begin
    if K2 = KindVAR then
    begin
      R.Op := OP_ASSIGN_BYTE_M;
      if T2 <> typeBOOLEAN then
      begin
        InsertConversionToBoolean(N, 2);
        Inc(N);
      end;
    end
    else
    begin
      if T2 <> typeBOOLEAN then
        R.Arg2 := CreateConst(typeBOOLEAN, GetSymbolRec(R.Arg2).value);
      R.Op := OP_ASSIGN_BYTE_I;
    end;
  end
  else if (T1 = typeBYTEBOOL) and (T2 in BooleanTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_BYTE_M
    else
    begin
      if T2 <> typeBYTEBOOL then
        R.Arg2 := CreateConst(typeBYTEBOOL, GetSymbolRec(R.Arg2).value);
      R.Op := OP_ASSIGN_BYTE_I;
    end;
  end
  else if (T1 = typeWORDBOOL) and (T2 in BooleanTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_WORD_M
    else
    begin
      if T2 <> typeWORDBOOL then
        R.Arg2 := CreateConst(typeWORDBOOL, GetSymbolRec(R.Arg2).value);
      R.Op := OP_ASSIGN_WORD_I;
    end;
  end
  else if (T1 = typeLONGBOOL) and (T2 in BooleanTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
    begin
      if T2 <> typeLONGBOOL then
        R.Arg2 := CreateConst(typeLONGBOOL, GetSymbolRec(R.Arg2).value);
      R.Op := OP_ASSIGN_INT_I;
    end;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeANSICHAR) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_BYTE_M
    else
      R.Op := OP_ASSIGN_BYTE_I;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType and
    (K2 = KindCONST) then
  begin
    S := GetSymbolRec(Arg2).value;
    if Length(S) = 1 then
    begin
      GetSymbolRec(Arg2).TypeId := typeANSICHAR;
      GetSymbolRec(Arg2).value := Ord(S[1]);
      R.Op := OP_ASSIGN_BYTE_I;
    end
    else
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;
      CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
        GetSymbolRec(T2).Name]);
      Exit;
    end;
  end
  else if (T1 = typeWIDECHAR) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindCONST then
      R.Op := OP_ASSIGN_WORD_I
    else
      R.Op := OP_ASSIGN_WORD_M;
  end
{$ENDIF}
  else if (T1 = typeWIDECHAR) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_WORD_M
    else
      R.Op := OP_ASSIGN_WORD_I;
  end
  else if (T1 = typeBYTE) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_BYTE_M
    else
    begin
      R.Op := OP_ASSIGN_BYTE_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeWORD) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_WORD_M
    else
    begin
      R.Op := OP_ASSIGN_WORD_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeINTEGER) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
    begin
      R.Op := OP_ASSIGN_INT_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeCARDINAL) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_CARDINAL_M
    else
    begin
      R.Op := OP_ASSIGN_CARDINAL_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeSMALLINT) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_SMALLINT_M
    else
    begin
      R.Op := OP_ASSIGN_SMALLINT_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeSHORTINT) and (T2 in IntegerTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_SHORTINT_M
    else
    begin
      R.Op := OP_ASSIGN_SHORTINT_I;
      CheckSubrangeBounds(GetSymbolRec(Arg1).TypeId, GetSymbolRec(Arg2));
    end;
  end
  else if (T1 = typeINTEGER) and (T2 in VariantTypes) then
  begin
    R.Op := OP_INT_FROM_VARIANT;
  end
  else if (T1 = typeWORD) and (T2 in VariantTypes) then
  begin
    R.Op := OP_WORD_FROM_VARIANT;
  end
  else if (T1 = typeBYTE) and (T2 in VariantTypes) then
  begin
    R.Op := OP_BYTE_FROM_VARIANT;
  end
  else if (T1 = typeBOOLEAN) and (T2 in VariantTypes) then
  begin
    R.Op := OP_BOOL_FROM_VARIANT;
  end
  else if (T1 = typeBYTEBOOL) and (T2 in VariantTypes) then
  begin
    R.Op := OP_BYTEBOOL_FROM_VARIANT;
  end
  else if (T1 = typeWORDBOOL) and (T2 in VariantTypes) then
  begin
    R.Op := OP_WORDBOOL_FROM_VARIANT;
  end
  else if (T1 = typeLONGBOOL) and (T2 in VariantTypes) then
  begin
    R.Op := OP_LONGBOOL_FROM_VARIANT;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    R.Op := OP_ANSISTRING_FROM_PANSICHAR;
  end
  else if (T1 = typeANSISTRING) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    R.Op := OP_ANSISTRING_FROM_PWIDECHAR;
  end
  else if (T1 = typeANSISTRING) and SymbolTable.IsZerobasedAnsiCharArray(Arg2)
  then
  begin
    RC := TCodeRec.Create(OP_ADDRESS, Self);
    RC.Arg1 := Arg2;
    RC.Arg2 := 0;
    RC.Res := CreatePointerVar(GetLevel(N));
    Insert(N, RC);
    Inc(N);

    R.Op := OP_ANSISTRING_FROM_PANSICHAR;
    R.Arg2 := RC.Res;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and
    SymbolTable.IsZerobasedAnsiCharArray(Arg2) then
  begin
    RC := TCodeRec.Create(OP_ADDRESS, Self);
    RC.Arg1 := Arg2;
    RC.Arg2 := 0;
    RC.Res := CreatePointerVar(GetLevel(N));
    Insert(N, RC);
    Inc(N);

    R.Op := OP_ASSIGN_INT_M;
    R.Arg2 := RC.Res;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_ANSISTRING
    else
      CreateError(errInternalError, []);
  end
  else if (T1 = typeANSISTRING) and (T2 in VariantTypes) then
  begin
    R.Op := OP_ANSISTRING_FROM_VARIANT;
  end
  else if (T1 = typeANSISTRING) and (T2 in CharTypes) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ANSISTRING_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_ANSISTRING_FROM_PANSICHAR;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_ANSISTRING_FROM_PANSICHAR;
      end;
    end;
  end
  else if (T1 = typeSHORTSTRING) and (GetSymbolRec(Arg2).HasPAnsiCharType) then
  begin
    if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_SHORTSTRING_FROM_PANSICHAR_LITERAL
    else
      CreateError(errLeftSideCannotBeAssignedTo, []);
  end
  else if (T1 = typeSHORTSTRING) and (GetSymbolRec(Arg2).HasPWideCharType) then
  begin
    if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL
    else
      CreateError(errLeftSideCannotBeAssignedTo, []);
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_SHORTSTRING_FROM_ANSISTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 in VariantTypes) then
  begin
    R.Op := OP_SHORTSTRING_FROM_VARIANT;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_SHORTSTRING_FROM_WIDESTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_UNICSTRING_FROM_WIDESTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_SHORTSTRING_FROM_UNICSTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_ANSISTRING_FROM_SHORTSTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_WIDESTRING_FROM_SHORTSTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_WIDESTRING_FROM_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_UNICSTRING_FROM_SHORTSTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_ANSISTRING_FROM_WIDESTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_ANSISTRING_FROM_UNICSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_SHORTSTRING_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
      end;
    end;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_SHORTSTRING_FROM_WIDECHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePWIDECHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL;
      end;
    end;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_ASSIGN_SHORTSTRING
  end

  // wide strings

  else if (T1 = typeWIDESTRING) and (T2 in VariantTypes) then
  begin
    R.Op := OP_WIDESTRING_FROM_VARIANT;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_WIDESTRING
    else
      CreateError(errInternalError, []);
  end
  else if (T1 = typeWIDESTRING) and (GetSymbolRec(Arg2).HasPAnsiCharType) then
  begin
    if K2 = KindCONST then
      R.Op := OP_WIDESTRING_FROM_PANSICHAR_LITERAL
    else
      CreateError(errLeftSideCannotBeAssignedTo, []);
  end
  else if (T1 = typeWIDESTRING) and (GetSymbolRec(Arg2).HasPWideCharType) then
  begin
    R.Op := OP_WIDESTRING_FROM_PWIDECHAR_LITERAL;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_WIDESTRING_FROM_ANSISTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_UNICSTRING_FROM_ANSISTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_WIDESTRING_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_WIDESTRING_FROM_PANSICHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_WIDESTRING_FROM_PANSICHAR_LITERAL;
      end;
    end;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_WIDESTRING_FROM_WIDECHAR
    else
      R.Op := OP_WIDESTRING_FROM_WIDECHAR_LITERAL;
  end
  else if (T1 = typeWIDESTRING) and SymbolTable.IsZerobasedWideCharArray(Arg2)
  then
  begin
    RC := TCodeRec.Create(OP_ADDRESS, Self);
    RC.Arg1 := Arg2;
    RC.Arg2 := 0;
    RC.Res := CreatePointerVar(GetLevel(N));
    Insert(N, RC);
    Inc(N);

    R.Op := OP_WIDESTRING_FROM_WIDECHAR_LITERAL;
    R.Arg2 := RC.Res;
  end
{$ENDIF}
  else if GetSymbolRec(Arg1).HasPWideCharType and
    SymbolTable.IsZerobasedWideCharArray(Arg2) then
  begin
    RC := TCodeRec.Create(OP_ADDRESS, Self);
    RC.Arg1 := Arg2;
    RC.Arg2 := 0;
    RC.Res := CreatePointerVar(GetLevel(N));
    Insert(N, RC);
    Inc(N);

    R.Op := OP_ASSIGN_INT_M;
    R.Arg2 := RC.Res;
  end

  // unic string - begin

  else if (T1 = typeUNICSTRING) and (T2 in VariantTypes) then
  begin
    R.Op := OP_UNICSTRING_FROM_VARIANT;
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_UNICSTRING
    else
      CreateError(errInternalError, []);
  end
  else if (T1 = typeUNICSTRING) and (GetSymbolRec(Arg2).HasPWideCharType) then
  begin
    R.Op := OP_UNICSTRING_FROM_PWIDECHAR_LITERAL;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeUNICSTRING) and (GetSymbolRec(Arg2).HasPAnsiCharType) then
  begin
    // if K2 = KindCONST then
    R.Op := OP_UNICSTRING_FROM_PANSICHAR_LITERAL
    // else
    // CreateError(errLeftSideCannotBeAssignedTo, []);
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_UNICSTRING_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_UNICSTRING_FROM_PANSICHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_UNICSTRING_FROM_PANSICHAR_LITERAL;
      end;
    end;
  end
{$ENDIF}
  else if (T1 = typeUNICSTRING) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_UNICSTRING_FROM_WIDECHAR
    else
      R.Op := OP_UNICSTRING_FROM_WIDECHAR_LITERAL;
  end

  // unic string - end

  else if (T1 = typeDOUBLE) and (T2 in RealTypes) then
  begin
    R.Op := OP_ASSIGN_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 = typeCURRENCY) then
  begin
    R.Op := OP_CURRENCY_TO_DOUBLE;
    R.Arg1 := R.Arg2;
  end
  else if (T1 = typeDOUBLE) and (T2 in IntegerTypes) then
  begin
    R.Op := OP_ASSIGN_DOUBLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 = typeDOUBLE) and (T2 in VariantTypes) then
  begin
    R.Op := OP_DOUBLE_FROM_VARIANT;
  end
  else if (T1 = typeSINGLE) and (T2 in VariantTypes) then
  begin
    R.Op := OP_SINGLE_FROM_VARIANT;
  end
  else if (T1 = typeSINGLE) and (T2 in RealTypes) then
  begin
    R.Op := OP_ASSIGN_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 = typeCURRENCY) then
  begin
    R.Op := OP_CURRENCY_TO_SINGLE;
    R.Arg1 := R.Arg2;
  end
  else if (T1 = typeSINGLE) and (T2 in IntegerTypes) then
  begin
    R.Op := OP_ASSIGN_SINGLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 = typeEXTENDED) and (T2 in VariantTypes) then
  begin
    R.Op := OP_EXTENDED_FROM_VARIANT;
  end
  else if (T1 = typeEXTENDED) and (T2 = typeCURRENCY) then
  begin
    R.Op := OP_CURRENCY_TO_EXTENDED;
    R.Arg1 := R.Arg2;
  end
  else if (T1 = typeEXTENDED) and (T2 in RealTypes) then
  begin
    R.Op := OP_ASSIGN_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in IntegerTypes) then
  begin
    R.Op := OP_ASSIGN_EXTENDED;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 = typeINT64) and (T2 in (IntegerTypes + VariantTypes)) then
  begin
    R.Op := OP_ASSIGN_INT64;
    if K2 = KindCONST then
    begin
      if T2 <> typeINT64 then
        R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value);
    end
    else
    begin
      if T2 = typeINT64 then
      begin
        // ok
      end
      else
      begin
        InsertConversionToInt64(N, 2);
        Inc(N);
      end;
    end;
  end
  else if (T1 = typeUINT64) and (T2 in (IntegerTypes + VariantTypes)) then
  begin
    R.Op := OP_ASSIGN_UINT64;
    if K2 = KindCONST then
    begin
      if T2 <> typeUINT64 then
        R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value);
    end
    else
    begin
      if T2 = typeUINT64 then
      begin
        // ok
      end
      else
      begin
        InsertConversionToUInt64(N, 2);
        Inc(N);
      end;
    end;
  end
  /// ///////
  else if (T1 = typeVARIANT) and (T2 in VariantTypes) then
  begin
    R.Op := OP_ASSIGN_VARIANT;
  end
  else if (T1 = typeOLEVARIANT) and (T2 in VariantTypes) then
  begin
    R.Op := OP_ASSIGN_OLEVARIANT;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeVARIANT) and (GetSymbolRec(Arg2).HasPAnsiCharType) then
  begin
    R.Op := OP_VARIANT_FROM_PANSICHAR_LITERAL
  end
  else if (T1 = typeOLEVARIANT) and (GetSymbolRec(Arg2).HasPAnsiCharType) then
  begin
    R.Op := OP_OLEVARIANT_FROM_PANSICHAR_LITERAL
  end
  else if (T1 = typeVARIANT) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_VARIANT_FROM_ANSISTRING;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_OLEVARIANT_FROM_ANSISTRING;
  end
  else if (T1 = typeVARIANT) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_VARIANT_FROM_WIDESTRING;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_OLEVARIANT_FROM_WIDESTRING;
  end
  else if (T1 = typeVARIANT) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_VARIANT_FROM_SHORTSTRING;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_OLEVARIANT_FROM_SHORTSTRING;
  end
  else if (T1 = typeVARIANT) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_VARIANT_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_VARIANT_FROM_PANSICHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_VARIANT_FROM_PANSICHAR_LITERAL;
      end;
    end;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeANSICHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_OLEVARIANT_FROM_ANSICHAR
    else
    begin
      if GetSymbolRec(Arg2).HasName then
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        R.Arg2 := TKernel(kernel).SymbolTable.AddPAnsiCharConst
          (AnsiString(S)).Id;
        R.Op := OP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
      end
      else
      begin
        S := Chr(Integer(GetSymbolRec(Arg2).value));
        GetSymbolRec(Arg2).TypeId := typePANSICHAR;
        GetSymbolRec(Arg2).value := S;
        R.Op := OP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
      end;
    end;
  end
{$ENDIF}
  else if (T1 = typeVARIANT) and (GetSymbolRec(Arg2).HasPWideCharType) then
  begin
    R.Op := OP_VARIANT_FROM_PWIDECHAR_LITERAL
  end
  else if (T1 = typeOLEVARIANT) and (GetSymbolRec(Arg2).HasPWideCharType) then
  begin
    R.Op := OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL
  end
  else if (T1 = typeVARIANT) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_VARIANT_FROM_UNICSTRING;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_OLEVARIANT_FROM_UNICSTRING;
  end
  else if (T1 = typeVARIANT) and (T2 = typeINTERFACE) then
  begin
    R.Op := OP_VARIANT_FROM_INTERFACE;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeINTERFACE) then
  begin
    R.Op := OP_OLEVARIANT_FROM_INTERFACE;
  end
  else if (T1 = typeVARIANT) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_VARIANT_FROM_WIDECHAR
    else
      R.Op := OP_VARIANT_FROM_WIDECHAR_LITERAL;
  end
  else if (T1 = typeOLEVARIANT) and (T2 = typeWIDECHAR) then
  begin
    if K2 = KindVAR then
      R.Op := OP_OLEVARIANT_FROM_WIDECHAR
    else
      R.Op := OP_OLEVARIANT_FROM_WIDECHAR_LITERAL;
  end
  else if (T1 = typeVARIANT) and
    (T2 in (IntegerTypes + RealTypes + BooleanTypes + [typeCURRENCY])) then
  begin
    R.Op := OP_ASSIGN_VARIANT;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeVARIANT, GetSymbolRec(R.Arg2).value, T2)
    else
    begin
      InsertConversionToVariant(N, 2);
      Inc(N);
    end;
  end
  else if (T1 = typeOLEVARIANT) and
    (T2 in (IntegerTypes + RealTypes + BooleanTypes + [typeCURRENCY])) then
  begin
    R.Op := OP_ASSIGN_OLEVARIANT;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeOLEVARIANT, GetSymbolRec(R.Arg2).value, T2)
    else
    begin
      InsertConversionToOleVariant(N, 2);
      Inc(N);
    end;
  end
  else if IsJSType(GetSymbolRec(Arg1).TerminalTypeId, SymbolTable) and
    (T2 = typeVARIANT) then
  begin
    InsertConversionToClass(N, 2, GetSymbolRec(Arg1).TerminalTypeId);
    Inc(N);
    R.Op := OP_ASSIGN_CLASS;
  end
  else if GetSymbolRec(Arg1).HasFrameworkType then
  begin
    if Arg2 = TKernel(kernel).SymbolTable.NilId then
    begin
{$IFDEF PAXARM}
      R.Op := OP_CLASS_CLR;
      Exit;
{$ENDIF}
    end
    else if GetSymbolRec(Arg1).TerminalTypeId = GetSymbolRec(Arg2).TerminalTypeId
    then
    begin

    end
    else
    begin
      InsertConversionToFrameworkClass(N, 2, GetSymbolRec(Arg1).TerminalTypeId);
      Inc(N);
    end;
    R.Op := OP_ASSIGN_CLASS;
  end
  else if (T1 = typeCLASS) and (T2 = typeCLASS) then
  begin
    T1 := GetSymbolRec(Arg1).TerminalTypeId;
    T2 := GetSymbolRec(Arg2).TerminalTypeId;

    if IsJSType(T1, SymbolTable) and IsJSType(T2, SymbolTable) and
      (GetSymbolRec(Arg1).Name <> '') and
      (GetSymbolRec(Arg1).Name <> strInternalConstructor) and
      (GetSymbolRec(Arg2).Name <> '') then
    begin
      R.Op := OP_ASSIGN_CLASS;
      Exit;
    end;

    if not SymbolTable.Inherits(T2, T1) then
    begin
      // ok, let's check that we have not case
      // F := TForm.Create(.........
      // [TForm] := [TCustomForm]

      b := false;
      if SymbolTable.Inherits(T1, T2) then
        if Arg2 = PrevRec(N).Res then
          if PrevRec(N).Op = OP_CALL then
          begin
            if GetSymbolRec(PrevRec(N).Arg1).Kind in kindSUBS then
            begin
              if PrevPrevRec(N).Op = OP_PUSH_CLSREF then
              begin
                T2 := GetSymbolRec(PrevPrevRec(N).Arg1).TypeId;
                if GetSymbolRec(T2).FinalTypeId = typeCLASSREF then
                begin
                  T2 := GetSymbolRec(T2).PatternId;
                  b := SymbolTable.Inherits(T2, T1);
                end;
              end;
            end
            else if PrevPrevRec(N).Op = OP_GET_VMT_ADDRESS then
              b := true
          end;

      if not b then
      begin
        if GetLanguage(N) = JS_LANGUAGE then
        begin
          R.Arg1 := NewTempVar(TKernel(kernel).SymbolTable[Arg1].Level,
            TKernel(kernel).SymbolTable[Arg2].TypeId);
          ReplaceIdEx(Arg1, R.Arg1, N + 1, Card, false);
          TKernel(kernel).SymbolTable[R.Arg1].Name := TKernel(kernel)
            .SymbolTable[Arg1].Name;
          Dec(N);
          Exit;
        end;

        T1 := GetSymbolRec(Arg1).TypeId;
        T2 := GetSymbolRec(Arg2).TypeId;
        CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
          GetSymbolRec(T2).Name]);
      end;
    end;
{$IFDEF PAXARM}
    R.Op := OP_ASSIGN_CLASS;
{$ELSE}
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
      R.Op := OP_ASSIGN_INT_I;
{$ENDIF}
  end
  else if (T1 = typePOINTER) and (T2 = typeCLASS) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
      R.Op := OP_ASSIGN_INT_I;
  end
  else if (T1 = typePOINTER) and (T2 = typeCLASSREF) then
  begin
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
      R.Op := OP_ASSIGN_INT_I;
  end
  else if (T1 = typeCLASS) and (Arg2 = TKernel(kernel).SymbolTable.NilId) then
  begin
{$IFDEF PAXARM}
    R.Op := OP_CLASS_CLR;
{$ELSE}
    R.Op := OP_ASSIGN_INT_I;
{$ENDIF}
  end
  else if (T1 = typeCLASS) and
    (Arg2 = TKernel(kernel).SymbolTable.CurrExceptionObjectId) then
  begin
{$IFDEF PAXARM}
    R.Op := OP_ASSIGN_CLASS;
{$ELSE}
    R.Op := OP_ASSIGN_INT_M;
{$ENDIF}
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASS) then
  begin
    T1 := GetSymbolRec(Arg1).TerminalTypeId;
    T1 := GetSymbolRec(T1).PatternId;
    T2 := Arg2;
    if (not SymbolTable.Inherits(T2, T1)) or (K2 <> KindTYPE) then
    begin
      CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
        GetSymbolRec(T2).Name]);
    end;
    R.Op := OP_ASSIGN_INT_M;
    Inc(R.Arg2);
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASSREF) then
  begin
    T1 := GetSymbolRec(GetSymbolRec(Arg1).TerminalTypeId).PatternId;
    T2 := GetSymbolRec(GetSymbolRec(Arg2).TerminalTypeId).PatternId;
    if not SymbolTable.Inherits(T2, T1) then
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;
      CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
        GetSymbolRec(T2).Name]);
    end;
    if K2 = KindVAR then
      R.Op := OP_ASSIGN_INT_M
    else
      R.Op := OP_ASSIGN_INT_I;
  end
  else if (T1 = typeCLASSREF) and (Arg2 = TKernel(kernel).SymbolTable.NilId)
  then
  begin
    R.Op := OP_ASSIGN_INT_I;
  end
  else if (T1 = typePROC) and (Arg2 = TKernel(kernel).SymbolTable.NilId) then
  begin
    R.Op := OP_ASSIGN_INT_I;
  end
  else if (T1 = typeDYNARRAY) and (T2 = typeDYNARRAY) then
  begin
    R.Op := OP_DYNARRAY_ASSIGN;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if T1 <> T2 then
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if (T1 = typeDYNARRAY) and (Arg2 = TKernel(kernel).SymbolTable.NilId)
  then
  begin
    R.Op := OP_DYNARRAY_CLR;
  end
  else if (T1 = typeEVENT) and (T2 = typeEVENT) then
  begin
    T1 := GetSymbolRec(Arg1).TerminalTypeId;
    T2 := GetSymbolRec(Arg2).TerminalTypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if not StrEql(GetSymbolRec(T1).SignatureSimple,
      GetSymbolRec(T2).SignatureSimple) then
      CreateError(errIncompatibleTypesNoArgs, []);

    R.Op := OP_ASSIGN_EVENT;
  end
  else if (T1 = typeEVENT) and (Arg2 = TKernel(kernel).SymbolTable.NilId) then
  begin
    R.Op := OP_ASSIGN_EVENT;
    R.Arg2 := TKernel(kernel).SymbolTable.EventNilId;
  end
  else if T1 = typeCURRENCY then
  begin
    if T2 = typeINT64 then
      R.Op := OP_CURRENCY_FROM_INT64
    else if T2 = typeUINT64 then
      R.Op := OP_CURRENCY_FROM_UINT64
    else if T2 in IntegerTypes then
    begin
      if K2 = KindCONST then
      begin
        R.Op := OP_CURRENCY_FROM_INT64;
        R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value);
      end
      else
        R.Op := OP_CURRENCY_FROM_INT;
    end
    else if T2 = typeCURRENCY then
      R.Op := OP_ASSIGN_CURRENCY
    else if T2 in RealTypes then
    begin
      R.Op := OP_CURRENCY_FROM_REAL;
    end
    else if T2 in VariantTypes then
    begin
      R.Op := OP_CURRENCY_FROM_VARIANT;
    end
    else
      goto err;
  end

  else if T1 = typeINTERFACE then
  begin
    if T2 = typeCLASS then
    begin
      T1 := GetSymbolRec(Arg1).TerminalTypeId;
      T2 := GetSymbolRec(Arg2).TerminalTypeId;
      if not SymbolTable.Supports(T2, T1) then
        CreateError(errIncompatibleTypesNoArgs, []);

      R.Op := OP_INTERFACE_FROM_CLASS;
    end
    else if T2 = typeINTERFACE then
    begin
      T1 := GetSymbolRec(Arg1).TerminalTypeId;
      T2 := GetSymbolRec(Arg2).TerminalTypeId;
      if not SymbolTable.Supports(T2, T1) then
        CreateError(errIncompatibleTypesNoArgs, []);

      R.Op := OP_ASSIGN_INTERFACE;
    end
    else if Arg2 = SymbolTable.NilId then
      R.Op := OP_INTERFACE_CLR
    else
      goto err;
  end
  else if (T2 = typeINTERFACE) and (K2 = KindTYPE) then
  begin
    if GetSymbolRec(Arg1).TerminalTypeId = H_TGUID then
    begin
      R.Op := OP_ASSIGN_RECORD;
      R.Arg2 := GetSymbolRec(Arg2).TerminalTypeId + 1;
    end
    else
      goto err;
  end
  else
  begin
    if SignTypeCast1 then
    begin
      if (T1 = typeENUM) and (T2 in VariantTypes) then
      begin
        R.Op := OP_BYTE_FROM_VARIANT;
      end
      else
        R.Op := OP_ASSIGN_INT_M;
      Exit;
    end;

    if (T1 = typePOINTER) and (T2 = typePOINTER) then
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;

      T1 := GetSymbolRec(T1).PatternId;
      T2 := GetSymbolRec(T2).PatternId;

      if (T1 = T2) or (T1 = typeVOID) or
        (Arg2 = TKernel(kernel).SymbolTable.NilId) or
{$IFNDEF PAXARM}
        (GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeSHORTSTRING)) or
{$ENDIF}
        (PrevRec(N).Op = OP_TYPEINFO) or StrEql(GetSymbolRec(T1).Name,
        GetSymbolRec(T2).Name) then
      begin
        if (K1 = KindVAR) and (K2 = KindVAR) then
          R.Op := OP_ASSIGN_INT_M
{$IFNDEF PAXARM}
        else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
        then
          R.Op := OP_ASSIGN_PANSICHAR
{$ENDIF}
        else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
        then
          R.Op := OP_ASSIGN_PWIDECHAR
        else if (K1 = KindVAR) and (K2 = KindCONST) then
          R.Op := OP_ASSIGN_INT_I;
        Exit;
      end
{$IFNDEF PAXARM}
      else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPAnsiCharType
      then
      begin
        R.Op := OP_ASSIGN_PWIDECHAR;
        if K2 = KindCONST then
          GetSymbolRec(Arg2).TypeId := typePWIDECHAR;

        Exit;
      end
      else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPWideCharType
      then
      begin
        R.Op := OP_ASSIGN_PANSICHAR;
        if K2 = KindCONST then
          GetSymbolRec(Arg2).TypeId := typePANSICHAR;

        Exit;
      end;
{$ENDIF}
    end
    else if (T1 = typePROC) and (T2 = typePROC) then
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;

      if T1 = T2 then
        R.Op := OP_ASSIGN_INT_M
      else
        CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end
    else if (T1 = typePROC) and (T2 = typePOINTER) then
    begin
      R.Op := OP_ASSIGN_INT_M;
      Exit;
    end
    else if (T1 = typeSET) and (T2 = typeSET) then
    begin
      T1 := GetSymbolRec(Arg1).TerminalTypeId;
      T2 := GetSymbolRec(Arg2).TerminalTypeId;

      if SymbolTable.CheckSetTypes(T1, T2) then
      begin
        R.Op := OP_SET_ASSIGN;
        GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
        Exit;
      end;
    end
    else if (T1 = typeRECORD) and (T2 = typeRECORD) then
    begin
      R.Op := OP_ASSIGN_RECORD;
      GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
      Exit;
    end
    else if (T1 = typeARRAY) and (T2 = typeARRAY) then
    begin
      R.Op := OP_ASSIGN_ARRAY;
      GetSymbolRec(Arg1).value := GetSymbolRec(Arg2).value;
      Exit;
    end
    else if (GetSymbolRec(Arg1).TerminalTypeId = H_TVarRec) and
      (T2 in OrdinalTypes + StringTypes + RealTypes + [typeVARIANT,
      typeOLEVARIANT, typePOINTER, typeCLASS]) then
    begin
      if SymbolTable[R.Arg2].Kind = KindCONST then
        SymbolTable[R.Arg2].MustBeAllocated := true;
      R.Op := OP_ASSIGN_TVarRec;
      Exit;
    end
    else if T1 = typePROC then
    begin
      if K2 = KindSUB then
      begin
        T1 := GetSymbolRec(Arg1).TypeId;
        SubId1 := TKernel(kernel).SymbolTable.GetPatternSubId(T1);
        SubId2 := R.Arg2;

        if TKernel(kernel).SymbolTable.EqualHeaders(SubId1, SubId2) then
        begin
          R.Op := OP_ADDRESS;
          R.Arg1 := R.Arg2;
          R.Arg2 := 0;
          Exit;
        end
        else
        begin
          OverList := SymbolTable.LookUpSub(SymbolTable[SubId2].Name,
            SymbolTable[SubId2].Level, GetUpcase(N));
          try
            for I := 0 to OverList.Count - 1 do
            begin
              SubId2 := OverList[I];
              if TKernel(kernel).SymbolTable.EqualHeaders(SubId1, SubId2) then
              begin
                R.Arg2 := SubId2;
                R.Op := OP_ADDRESS;
                R.Arg1 := R.Arg2;
                R.Arg2 := 0;
                Exit;
              end;
            end;
          finally
            FreeAndNil(OverList);
          end;
        end;

      end
      else
        CreateError(errIncompatibleTypesNoArgs, []);
    end
    else if (T1 = typeDYNARRAY) and (T2 = typeSET) then
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      Arg2 := ConvertSetLiteralToDynarrayLiteral(GetSymbolRec(R.Arg1).Level,
        T1, R.Arg2);
      if Arg2 = 0 then
        CreateError(errIncompatibleTypesNoArgs, [])
      else
        R.Arg2 := Arg2;
      Dec(N);
      Exit;
    end;

  err:

    if (GetSymbolRec(Arg1).FinalTypeId = typeSET) and
      (GetSymbolRec(Arg2).FinalTypeId = typeSET) then
      CreateError(errIncompatibleTypesNoArgs, [])
    else
    begin
      T1 := GetSymbolRec(Arg1).TypeId;
      T2 := GetSymbolRec(Arg2).TypeId;
{$IFNDEF PAXARM}
      if (T1 = H_TGUID) and (T2 = typePANSICHAR) then
      begin
        R.Op := OP_NOP;
        GetSymbolRec(R.Arg1).value := GetSymbolRec(R.Arg2).value;
      end
      else
{$ENDIF}
        if (T1 = H_TGUID) and (T2 = typePWIDECHAR) then
        begin
          R.Op := OP_NOP;
          GetSymbolRec(R.Arg1).value := GetSymbolRec(R.Arg2).value;
        end
        else if GetSymbolRec(Arg1).FinalTypeId = typeRECORD then
        begin
          T1 := GetSymbolRec(Arg1).TerminalTypeId;
          T2 := GetSymbolRec(Arg2).TerminalTypeId;

          R.Op := OP_ASSIGN_RECORD;

          if T1 <> T2 then
            if T1 <> H_TVarRec then
            begin
              InsertConversionToRecord(N, 2, GetSymbolRec(R.Arg1)
                .TerminalTypeId, R.Arg2);
              Inc(N);
            end;

          Exit;
        end
        else
        begin
          if T2 = H_TValue then
          begin
            R.Op := OP_VAR_FROM_TVALUE;
          end
          else
          begin
            if GetSymbolRec(Arg2).FinalTypeId = typeRECORD then
            begin
              SubId2 := FindImplicitConversion
                (GetSymbolRec(Arg2).TerminalTypeId, Arg2, Arg1);
              if SubId2 > 0 then
              begin
                NoOverloadSearch := true;

                T2 := SymbolTable.GetResultId(SubId2);
                T2 := GetSymbolRec(T2).TypeId;

                RC := TCodeRec.Create(OP_PUSH, Self);
                RC.Arg1 := Arg2;
                RC.Arg2 := 0;
                RC.Res := SubId2;
                Insert(N, RC);
                Inc(N);

                RC := TCodeRec.Create(OP_CALL, Self);
                RC.Arg1 := SubId2;
                RC.Arg2 := 1;
                RC.Res := NewTempVar(GetLevel(N), T2);
                Insert(N, RC);
                Inc(N);

                R.Arg2 := RC.Res;

                Dec(N, 2);

                Exit;
              end;
            end;

            if Records[N].Language = JS_LANGUAGE then
            begin
              if TKernel(kernel).SymbolTable.IsResultId(Arg1) then
              begin
                R.Op := OP_ASSIGN_VARIANT;
                if K2 = KindCONST then
                  R.Arg2 := CreateConst(typeVARIANT,
                    GetSymbolRec(R.Arg2).value, T2)
                else
                begin
                  InsertConversionToVariant(N, 2);
                  Inc(N);
                end;
              end
              else
              begin
                R.Arg1 := NewTempVar(TKernel(kernel).SymbolTable[Arg1].Level,
                  TKernel(kernel).SymbolTable[Arg2].TypeId);
                ReplaceIdEx(Arg1, R.Arg1, N + 1, Card, false);
                TKernel(kernel).SymbolTable[R.Arg1].Name := TKernel(kernel)
                  .SymbolTable[Arg1].Name;
                Dec(N);
              end;
              Exit;
            end
            else if IsExplicitOff then
            begin
              R.Arg1 := NewTempVar(TKernel(kernel).SymbolTable[Arg1].Level,
                TKernel(kernel).SymbolTable[Arg2].TypeId);
              ReplaceIdEx(Arg1, R.Arg1, N + 1, Card, false);
              TKernel(kernel).SymbolTable[R.Arg1].Name := TKernel(kernel)
                .SymbolTable[Arg1].Name;
              Dec(N);
              Exit;
            end;

            CreateError(errIncompatibleTypes, [GetSymbolRec(T1).NameEx,
              GetSymbolRec(T2).NameEx]);
          end;
        end;
    end;
  end;
end;

function TCode.ConvertSetLiteralToDynarrayLiteral(CurrLevel, DynarrayTypeId,
  SetId: Integer): Integer;
var
  I, J: Integer;
  RC: TCodeRec;
  SubId: Integer;
  ElemTypeId: Integer;
  ElemSizeId: Integer;
  FinalElemTypeId: Integer;
  L: Integer;
  DynArrayId: Integer;

  Op, tempN, K1, K2: Integer;
begin
  J := -1;

  for I := 1 to N do
  begin
    if Records[I].Op = OP_SET_INCLUDE then
      if Records[I].Arg1 = SetId then
      begin
        J := I;
        break;
      end;
    if Records[I].Op = OP_SET_INCLUDE_INTERVAL then
      if Records[I].Arg1 = SetId then
      begin
        result := 0;
        Exit;
      end;
  end;

  if J = -1 then
  begin
    result := 0;
    Exit;
  end;

  L := 0;
  for I := 1 to N do
    if Records[I].Op = OP_SET_INCLUDE then
      if Records[I].Arg1 = SetId then
        Inc(L);

  SubId := Id_DynarraySetLength;

  RC := TCodeRec.Create(OP_BEGIN_CALL, Self);
  RC.Arg1 := SubId;
  RC.Arg2 := 0;
  RC.Res := 0;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  DynArrayId := NewTempVar(CurrLevel, DynarrayTypeId);
  GetSymbolRec(DynArrayId).Name := '@';
  ElemTypeId := GetSymbolRec(DynarrayTypeId).PatternId;

  RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
  RC.Arg1 := CurrLevel;
  RC.Arg2 := DynArrayId;
  RC.Res := 0;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  ElemSizeId := NewTempVar(CurrLevel, typeINTEGER);
  RC := TCodeRec.Create(OP_SIZEOF, Self);
  RC.Arg1 := ElemTypeId;
  RC.Arg2 := 0;
  RC.Res := ElemSizeId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  RC := TCodeRec.Create(OP_PUSH_INT, Self);
  RC.Arg1 := ElemSizeId;
  RC.Arg2 := 4;
  RC.Res := SubId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  RC := TCodeRec.Create(OP_PUSH_INT_IMM, Self);
  RC.Arg1 := CreateConst(typeINTEGER, ElemTypeId);
  RC.Arg2 := 3;
  RC.Res := SubId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  FinalElemTypeId := GetSymbolRec(ElemTypeId).FinalTypeId;

  RC := TCodeRec.Create(OP_PUSH_INT_IMM, Self);
  RC.Arg1 := CreateConst(typeINTEGER, FinalElemTypeId);
  RC.Arg2 := 2;
  RC.Res := SubId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  RC := TCodeRec.Create(OP_PUSH_INT_IMM, Self);
  RC.Arg1 := CreateConst(typeINTEGER, L);
  RC.Arg2 := 1;
  RC.Res := SubId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  RC := TCodeRec.Create(OP_PUSH_ADDRESS, Self);
  RC.Arg1 := DynArrayId;
  RC.Arg2 := 0;
  RC.Res := SubId;

  Insert(J, RC);
  Inc(J);
  Inc(N);

  RC := TCodeRec.Create(OP_CALL, Self);
  RC.Arg1 := SubId;
  RC.Arg2 := 5;
  RC.Res := 0;

  Insert(J, RC);
  // Inc(J);
  Inc(N);

  for I := N downto 1 do
  begin
    Op := Records[I].Op;
    if Op = OP_SET_INCLUDE then
    begin
      if Records[I].Arg1 = SetId then
      begin
        Dec(L);

        J := Records[I].Arg2;

        Records[I].Op := OP_ELEM;
        Records[I].Arg1 := DynArrayId;
        Records[I].Arg2 := CreateConst(typeINTEGER, L);
        Records[I].Res := NewTempVar(CurrLevel, ElemTypeId);

        RC := TCodeRec.Create(OP_ASSIGN, Self);
        RC.Arg1 := Records[I].Res;
        RC.Arg2 := J;
        RC.Res := Records[I].Res;
        Insert(I + 1, RC);
        Inc(N);
      end;
    end
    else if Op = OP_BEGIN_MODULE then
      break;
  end;

  result := DynArrayId;

  GetSymbolRec(result).Count := GetSymbolRec(SetId).Count;

  tempN := N;

  for I := N downto 1 do
  begin
    if Records[I].Op = OP_ELEM then
    begin
      if Records[I].Arg1 = DynArrayId then
      begin
        K1 := Card;
        N := I + 1;
        Op := Records[N].Op;
        ProcList[-Op];
        K2 := Card;
        Inc(tempN, K2 - K1);

        K1 := Card;
        N := I;
        Op := Records[N].Op;
        ProcList[-Op];
        K2 := Card;
        Inc(tempN, K2 - K1);
      end;
    end
    else if Records[I].Op = OP_BEGIN_MODULE then
      break;
  end;

  N := tempN;
end;

procedure TCode.OperSetInclude;
var
  Arg1, T1, K1, L: Integer;
  Arg2, T2: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  SetTypeId, BaseTypeId: Integer;
  SetTypeName: String;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  Arg2 := R.Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if K1 in [KindVAR, KindCONST] then
  begin
    if T1 = 0 then
    begin
      SetTypeName := SetTypeName + 'type_Set_' + IntToStr(SymbolTable.Card + 1);

      L := GetSymbolRec(Arg1).Level;

      SetTypeId := SymbolTable.RegisterSetType(L, SetTypeName,
        GetSymbolRec(Arg2).TerminalTypeId);

      AddTypeInfo(SetTypeId, SetTypeId);

      GetSymbolRec(Arg1).TypeId := SetTypeId;
      T1 := typeSET;
    end
    else
    begin
      SetTypeId := GetSymbolRec(Arg1).TypeId;
    end;

    if T1 <> typeSET then
    begin
      CreateError(errSetTypeRequired, []);
      Exit;
    end;

    if SetTypeId = typeSET then
      Exit;

    BaseTypeId := GetSymbolRec(SetTypeId).PatternId;

    if GetSymbolRec(BaseTypeId).FinalTypeId <> T2 then
    begin
      if not((GetSymbolRec(BaseTypeId).FinalTypeId in IntegerTypes) and
        (T2 in IntegerTypes)) then
      begin
        // T2 := GetSymbolRec(Arg2).TypeID;
        // CreateError(errIncompatibleTypes,
        // [GetSymbolRec(BaseTypeId).Name, GetSymbolRec(T2).Name]);
        // Exit;
      end;
    end;
  end
  else
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
  end;
end;

procedure TCode.OperSetExclude;
begin
end;

procedure TCode.OperInc;
var
  T1, T2, K1, K2, PatternId: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  T1 := GetSymbolRec(R.Arg1).FinalTypeId;
  K1 := GetSymbolRec(R.Arg1).Kind;

  if T1 = typeENUM then
    T1 := typeINTEGER;

  if T1 in [typeINT64, typeUINT64, typeVARIANT, typeOLEVARIANT] then
  begin
    R.Op := OP_PLUS;
    Dec(N);
  end
  else if T1 in OrdinalTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Arg1).value := GetSymbolRec(R.Arg1).value + 1;
      R.Op := OP_NOP;
    end
    else if K1 = KindVAR then
    begin
      PatternId := GetSymbolRec(R.Res).PatternId;
      if PatternId <> 0 then
        if GetSymbolRec(PatternId).Kind = KindPROP then
          CreateError(errYouCannotUseIncOnProperties, []);

      T2 := GetSymbolRec(R.Arg2).FinalTypeId;
      K2 := GetSymbolRec(R.Arg2).Kind;
      if not(T2 in IntegerTypes) then
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        Exit;
      end;
      if K2 = KindCONST then
        R.Op := OP_ADD_INT_MI
      else if K2 = KindVAR then
        R.Op := OP_ADD_INT_MM
      else
        CreateError(errIncompatibleTypesNoArgs, []);
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if T1 = typeRECORD then
  begin
    InsertUnaryOperator(GetSymbolRec(R.Arg1).TerminalTypeId, gen_Inc);
    Inc(N);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperDec;
var
  T1, T2, K1, K2, PatternId: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  T1 := GetSymbolRec(R.Arg1).FinalTypeId;
  K1 := GetSymbolRec(R.Arg1).Kind;

  if T1 = typeENUM then
    T1 := typeINTEGER;

  if T1 in [typeINT64, typeUINT64, typeVARIANT, typeOLEVARIANT] then
  begin
    R.Op := OP_MINUS;
    Dec(N);
  end
  else if T1 in OrdinalTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Arg1).value := GetSymbolRec(R.Arg1).value - 1;
      R.Op := OP_NOP;
    end
    else if K1 = KindVAR then
    begin
      PatternId := GetSymbolRec(R.Res).PatternId;
      if PatternId <> 0 then
        if GetSymbolRec(PatternId).Kind = KindPROP then
          CreateError(errYouCannotUseDecOnProperties, []);

      T2 := GetSymbolRec(R.Arg2).FinalTypeId;
      K2 := GetSymbolRec(R.Arg2).Kind;
      if not(T2 in IntegerTypes) then
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        Exit;
      end;
      if K2 = KindCONST then
        R.Op := OP_SUB_INT_MI
      else if K2 = KindVAR then
        R.Op := OP_SUB_INT_MM
      else
        CreateError(errIncompatibleTypesNoArgs, []);
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if T1 = typeRECORD then
  begin
    InsertUnaryOperator(GetSymbolRec(R.Arg1).TerminalTypeId, gen_Dec);
    Inc(N);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperSetLength;
var
  T1, T2, K1, L1, ArrayId, ElementTypeId, ElementFinalTypeId,
    ElementSizeId: Integer;
  R, RC: TCodeRec;
begin
  R := Records[N];

  T1 := GetSymbolRec(R.Arg1).FinalTypeId;
  T2 := GetSymbolRec(R.Arg2).FinalTypeId;
  K1 := GetSymbolRec(R.Arg1).Kind;

  if T1 = typeDYNARRAY then
  begin
    if not(K1 = KindVAR) then
      CreateError(errIncompatibleTypesNoArgs, []);
    if not(T2 in IntegerTypes) then
      CreateError(errIncompatibleTypesNoArgs, []);

    ArrayId := R.Arg1;
    L1 := R.Arg2;
    T1 := GetSymbolRec(R.Arg1).TerminalTypeId;
    ElementTypeId := GetSymbolRec(T1).PatternId;
    ElementFinalTypeId := GetSymbolRec(ElementTypeId).FinalTypeId;
    ElementSizeId := NewTempVar(0, typeINTEGER);

    R.Op := OP_CALL;
    R.Arg1 := Id_DynarraySetLength;
    R.Arg2 := 3;
    R.Res := 0;

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := ArrayId;
    RC.Arg2 := 0;
    RC.Res := R.Arg1;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := L1;
    RC.Arg2 := 1;
    RC.Res := R.Arg1;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := CreateConst(typeINTEGER, ElementFinalTypeId);
    RC.Arg2 := 2;
    RC.Res := R.Arg1;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := CreateConst(typeINTEGER, ElementTypeId);
    RC.Arg2 := 3;
    RC.Res := R.Arg1;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_SIZEOF, Self);
    RC.Arg1 := ElementTypeId;
    RC.Arg2 := 0;
    RC.Res := ElementSizeId;
    Insert(N, RC);
    Inc(N);

    RC := TCodeRec.Create(OP_PUSH, Self);
    RC.Arg1 := ElementSizeId;
    RC.Arg2 := 4;
    RC.Res := R.Arg1;
    Insert(N, RC);
    Inc(N);

    Dec(N);
  end
  else if T1 in StringTypes then
  begin
    // ok
  end
  else if T1 in VariantTypes then
  begin
    // ok
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperAbs;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
  I64: Int64;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if not(K1 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;

  if T1 in INT64Types then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
{$IFDEF VARIANTS}
        I64 := GetSymbolRec(Arg1).value;
        GetSymbolRec(R.Res).value := Abs(I64);
{$ELSE}
        I64 := Integer(GetSymbolRec(Arg1).value);
        GetSymbolRec(R.Res).value := Abs(Integer(I64));
{$ENDIF}
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_INT64;
        R.Arg1 := R.Res;
{$IFDEF VARIANTS}
        I64 := GetSymbolRec(Arg1).value;
        R.Arg2 := CreateConst(typeINT64, Abs(I64));
{$ELSE}
        I64 := Integer(GetSymbolRec(Arg1).value);
        R.Arg2 := CreateConst(typeINT64, Integer(Abs(I64)));
{$ENDIF}
      end;
    end
    else
      R.Op := OP_ABS_INT64;
  end
  else if T1 in IntegerTypes then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := Abs(Integer(GetSymbolRec(Arg1).value));
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_INT_I;
        R.Arg1 := R.Res;
        R.Arg2 := CreateConst(typeINTEGER,
          Abs(Integer(GetSymbolRec(Arg1).value)));
      end;
    end
    else
      R.Op := OP_ABS_INT;
  end
  else if T1 = typeDOUBLE then
  begin
    if K1 = KindCONST then
    begin

      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := Abs(Double(GetSymbolRec(Arg1).value));
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_DOUBLE;
        R.Arg1 := R.Res;
        R.Arg2 := CreateConst(typeDOUBLE,
          Abs(Double(GetSymbolRec(Arg1).value)));
      end;

    end
    else
      R.Op := OP_ABS_DOUBLE
  end
  else if T1 = typeSINGLE then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := Abs(Single(GetSymbolRec(Arg1).value));
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_SINGLE;
        R.Arg1 := R.Res;
        R.Arg2 := CreateConst(typeSINGLE,
          Abs(Single(GetSymbolRec(Arg1).value)));
      end;
    end
    else
      R.Op := OP_ABS_SINGLE
  end
  else if T1 = typeEXTENDED then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := Abs(Extended(GetSymbolRec(Arg1).value));
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_EXTENDED;
        R.Arg1 := R.Res;
        R.Arg2 := CreateConst(typeEXTENDED,
          Abs(Extended(GetSymbolRec(Arg1).value)));
      end;
    end
    else
      R.Op := OP_ABS_EXTENDED
  end
  else if T1 = typeCURRENCY then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := Abs(Currency(GetSymbolRec(Arg1).value));
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_CURRENCY;
        R.Arg1 := R.Res;
        R.Arg2 := CreateConst(typeCURRENCY,
          Abs(Currency(GetSymbolRec(Arg1).value)));
      end;
    end
    else
      R.Op := OP_ABS_CURRENCY;
  end
  else if T1 in VariantTypes then
  begin
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        if GetSymbolRec(Arg1).value >= 0 then
          GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value
        else
          GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_VARIANT;
        R.Arg1 := R.Res;
        if GetSymbolRec(Arg1).value >= 0 then
          R.Arg2 := CreateConst(typeVARIANT, GetSymbolRec(Arg1).value)
        else
          R.Arg2 := CreateConst(typeVARIANT, GetSymbolRec(Arg1).value);
      end;
    end
    else
      R.Op := OP_ABS_VARIANT
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperPred;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 = typeENUM then
    T1 := typeINTEGER;

  if T1 in OrdinalTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value - 1;
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_SUB_INT_MI;
        R.Arg2 := CreateConst(typeINTEGER, 1);
      end;
    end
    else if K1 = KindVAR then
    begin
      R.Op := OP_ADD_INT_MI;
      R.Arg2 := CreateConst(typeINTEGER, -1);
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperSucc;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 = typeENUM then
    T1 := typeINTEGER;

  if T1 in OrdinalTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value + 1;
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ADD_INT_MI;
        R.Arg2 := CreateConst(typeINTEGER, 1);
      end;
    end
    else if K1 = KindVAR then
    begin
      R.Op := OP_ADD_INT_MI;
      R.Arg2 := CreateConst(typeINTEGER, 1);
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperOrd;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 = typeENUM then
    T1 := typeINTEGER;

  if T1 in OrdinalTypes then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value;
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_INT_I;
        R.Arg1 := R.Res;
        R.Arg2 := Arg1;
      end;
    end
    else if K1 = KindVAR then
    begin
      R.Op := OP_ASSIGN_INT_M;
      R.Arg1 := R.Res;
      R.Arg2 := Arg1;
    end
    else
      CreateError(errInternalError, []);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperChr;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 in IntegerTypes then
  begin
    GetSymbolRec(R.Res).TypeId := typeCHAR;
    if K1 = KindCONST then
    begin
      if GetSymbolRec(R.Res).IsGlobalVar then
      begin
        GetSymbolRec(R.Res).Kind := KindCONST;
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value;
        R.Op := OP_NOP;
      end
      else
      begin
        R.Op := OP_ASSIGN_INT_I;
        R.Arg1 := R.Res;
        R.Arg2 := Arg1;
      end;
    end
    else if K1 = KindVAR then
    begin
      R.Op := OP_ASSIGN_INT_M;
      R.Arg1 := R.Res;
      R.Arg2 := Arg1;
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperAssigned;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if not K1 in [KindVAR, KindCONST] then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if T1 in [typePOINTER, typeCLASS, typeCLASSREF, typePROC, typeINTERFACE] then
  begin
    R.Op := OP_NE;
    R.Arg2 := TKernel(kernel).SymbolTable.NilId;
    Dec(N);
  end
  else if T1 = typeEVENT then
  begin
    R.Op := OP_NE;
    R.Arg2 := TKernel(kernel).SymbolTable.EventNilId;
    Dec(N);
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperLow;
var
  Arg1, Res, T1: Integer;
  R: TCodeRec;
  RangeTypeId, ElemTypeId: Integer;
  SymbolTable: TSymbolTable;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  Res := R.Res;

  if T1 in IntegerTypes then
  begin
    case T1 of
      typeINTEGER, typeBYTE, typeWORD, typeSMALLINT, typeSHORTINT:
        R.Res := CreateConst(typeINTEGER,
          SymbolTable.GetLowBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
      typeCARDINAL, typeINT64:
        R.Res := CreateConst(typeINT64,
          SymbolTable.GetLowBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    end;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeENUM then
  begin
    R.Res := CreateConst(typeINTEGER,
      SymbolTable.GetLowBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$IFNDEF PAXARM}
  else if T1 = typeANSICHAR then
  begin
    R.Res := CreateConst(typeANSICHAR,
      SymbolTable.GetLowBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$ENDIF}
  else if T1 = typeWIDECHAR then
  begin
    R.Res := CreateConst(typeWIDECHAR,
      SymbolTable.GetLowBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeBOOLEAN then
  begin
    R.Res := CreateConst(typeBOOLEAN, Low(Boolean));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeBYTEBOOL then
  begin
    R.Res := CreateConst(typeBYTEBOOL, Low(ByteBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeWORDBOOL then
  begin
    R.Res := CreateConst(typeWORDBOOL, Low(WordBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeLONGBOOL then
  begin
    R.Res := CreateConst(typeLONGBOOL, Low(LongBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeARRAY then
  begin
    SymbolTable.GetArrayTypeInfo(GetSymbolRec(Arg1).TerminalTypeId, RangeTypeId,
      ElemTypeId);
    R.Res := CreateConst(typeINTEGER,
      SymbolTable.GetLowBoundRec(RangeTypeId).value);
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 in [typeDYNARRAY, typeOPENARRAY] then
  begin
    R.Res := CreateConst(typeINTEGER, 0);
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$IFNDEF PAXARM}
  else if T1 = typeSHORTSTRING then
  begin
    R.Res := CreateConst(typeINTEGER, 0);
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$ENDIF}
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperHigh;
var
  Arg1, Res, T1: Integer;
  R: TCodeRec;
  RangeTypeId, ElemTypeId: Integer;
  SymbolTable: TSymbolTable;
begin
  R := Records[N];
  SymbolTable := TKernel(kernel).SymbolTable;

  Arg1 := R.Arg1;
  Res := R.Res;
  T1 := GetSymbolRec(Arg1).FinalTypeId;

  if T1 in IntegerTypes then
  begin
    case T1 of
      typeINTEGER, typeBYTE, typeWORD, typeSMALLINT, typeSHORTINT:
        R.Res := CreateConst(typeINTEGER,
          SymbolTable.GetHighBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
      typeCARDINAL, typeINT64:
        R.Res := CreateConst(typeINT64,
          SymbolTable.GetHighBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    end;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$IFNDEF PAXARM}
  else if T1 = typeANSICHAR then
  begin
    R.Res := CreateConst(typeANSICHAR,
      SymbolTable.GetHighBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
{$ENDIF}
  else if T1 = typeWIDECHAR then
  begin
    R.Res := CreateConst(typeWIDECHAR,
      SymbolTable.GetHighBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeENUM then
  begin
    R.Res := CreateConst(typeINTEGER,
      SymbolTable.GetHighBoundRec(GetSymbolRec(Arg1).TerminalTypeId).value);
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeBOOLEAN then
  begin
    R.Res := CreateConst(typeBOOLEAN, High(Boolean));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeBYTEBOOL then
  begin
    R.Res := CreateConst(typeBYTEBOOL, High(ByteBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeWORDBOOL then
  begin
    R.Res := CreateConst(typeWORDBOOL, High(WordBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeLONGBOOL then
  begin
    R.Res := CreateConst(typeLONGBOOL, High(LongBool));
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeARRAY then
  begin
    SymbolTable.GetArrayTypeInfo(GetSymbolRec(Arg1).TerminalTypeId, RangeTypeId,
      ElemTypeId);
    R.Res := CreateConst(typeINTEGER,
      SymbolTable.GetHighBoundRec(RangeTypeId).value);
    R.Op := OP_NOP;
    ReplaceIdEx(Res, R.Res, N, Card, true);
  end
  else if T1 = typeDYNARRAY then
  begin
    GetSymbolRec(Res).TypeId := typeINTEGER;

    if GetSymbolRec(Arg1).Kind = KindVAR then
      R.Op := OP_DYNARRAY_HIGH
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if T1 = typeOPENARRAY then
  begin
    GetSymbolRec(Res).TypeId := typeINTEGER;
    R.Arg2 := SymbolTable.GetOpenArrayHighId(R.Arg1);
    R.Arg1 := R.Res;

    R.Op := OP_ASSIGN;
    Dec(N);
  end
{$IFNDEF PAXARM}
  else if T1 = typeSHORTSTRING then
  begin
    GetSymbolRec(Res).TypeId := typeINTEGER;

    if GetSymbolRec(Arg1).Kind = KindVAR then
      R.Op := OP_SHORTSTRING_HIGH
    else if GetSymbolRec(Arg1).Kind = KindTYPE then
    begin
      if Arg1 = typeSHORTSTRING then
        R.Res := CreateConst(typeINTEGER, High(ShortString))
      else
        R.Res := CreateConst(typeINTEGER, GetSymbolRec(Arg1).Count);
      R.Op := OP_NOP;
      ReplaceIdEx(Res, R.Res, N, Card, true);
    end
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
{$ENDIF}
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperStr;
var
  R: TCodeRec;
  I, SubId, Id, T: Integer;
  L: TIntegerList;
  RC: TCodeRec;
begin
  R := Records[N];

  SubId := R.Arg1;

  L := TIntegerList.Create;

  try
    for I := 1 to N do
      if Records[I].Res = SubId then
        L.Add(I);

    if L.Count <> 4 then
    begin
      CreateError(errInternalError, []);
      Exit;
    end;

    Id := Records[L[0]].Arg1;
    T := GetSymbolRec(Id).FinalTypeId;

    if T in IntegerTypes then
    begin
      SubId := Id_StrInt;
      case GetSymbolRec(Id).Kind of
        KindCONST:
          Records[L[0]].Op := OP_PUSH_INT_IMM;
        KindVAR:
          Records[L[0]].Op := OP_PUSH_INT;
      else
        begin
          N := L[0];
          CreateError(errIncompatibleTypesNoArgs, []);
        end;
      end;
    end
    else if T = typeDOUBLE then
    begin
      SubId := Id_StrDouble;
      case GetSymbolRec(Id).Kind of
        KindCONST, KindVAR:
          Records[L[0]].Op := OP_PUSH_DOUBLE;
      else
        begin
          N := L[0];
          CreateError(errIncompatibleTypesNoArgs, []);
        end;
      end;
    end
    else if T = typeSINGLE then
    begin
      SubId := Id_StrSingle;
      case GetSymbolRec(Id).Kind of
        KindCONST, KindVAR:
          Records[L[0]].Op := OP_PUSH_SINGLE;
      else
        begin
          N := L[0];
          CreateError(errIncompatibleTypesNoArgs, []);
        end;
      end;
    end
    else if T = typeEXTENDED then
    begin
      SubId := Id_StrExtended;
      case GetSymbolRec(Id).Kind of
        KindCONST, KindVAR:
          Records[L[0]].Op := OP_PUSH_EXTENDED;
      else
        begin
          N := L[0];
          CreateError(errIncompatibleTypesNoArgs, []);
        end;
      end;
    end
    else
      SubId := 0;

    if SubId = 0 then
    begin
      N := L[0];
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;

    Id := Records[L[1]].Arg1;
    T := GetSymbolRec(Id).FinalTypeId;
    if not(T in IntegerTypes) then
    begin
      N := L[1];
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
    case GetSymbolRec(Id).Kind of
      KindCONST:
        Records[L[1]].Op := OP_PUSH_INT_IMM;
      KindVAR:
        Records[L[1]].Op := OP_PUSH_INT;
    else
      begin
        N := L[1];
        CreateError(errIncompatibleTypesNoArgs, []);
      end;
    end;

    Id := Records[L[2]].Arg1;
    T := GetSymbolRec(Id).FinalTypeId;
    if not(T in IntegerTypes) then
    begin
      N := L[2];
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
    case GetSymbolRec(Id).Kind of
      KindCONST:
        Records[L[2]].Op := OP_PUSH_INT_IMM;
      KindVAR:
        Records[L[2]].Op := OP_PUSH_INT;
    else
      begin
        N := L[2];
        CreateError(errIncompatibleTypesNoArgs, []);
      end;
    end;

    Id := Records[L[3]].Arg1;
    T := GetSymbolRec(Id).FinalTypeId;
    if not(T in StringTypes) then
    begin
      N := L[3];
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
    case GetSymbolRec(Id).Kind of
      KindVAR:
        Records[L[3]].Op := OP_PUSH_ADDRESS;
    else
      begin
        N := L[3];
        CreateError(errIncompatibleTypesNoArgs, []);
      end;
    end;

    R.Op := OP_CALL;
    R.Arg1 := SubId;
    R.Arg2 := 4;
    R.Res := 0;

    for I := 0 to L.Count - 1 do
    begin
      Records[L[I]].Arg2 := L.Count - I - 1;
      Records[L[I]].Res := SubId;
    end;

  finally
    RC := TCodeRec.Create(0, Self);
    RC.Op := OP_BEGIN_CALL;
    RC.Arg1 := SubId;
    Insert(L[0], RC);
    Inc(N);

    L.Free;
  end;
end;

procedure TCode.OperSetMembership;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T2 = typeSET then
    CreateSetObject(Arg2)
  else
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  T2 := GetSymbolRec(GetSymbolRec(Arg2).TypeId).PatternId;
  T2 := GetSymbolRec(T2).FinalTypeId;
  if not((ExistsOrdinalRelationalOperator(T1, T2) or (T2 = typeVOID))) then
  begin
    if (GetSymbolRec(T1).FinalTypeId = typeENUM) and
      (GetSymbolRec(T2).FinalTypeId = typeENUM) then
    begin
      // ok
    end
    else if GetSymbolRec(Arg2).TerminalTypeId = H_TByteSet then
    begin
      // ok
    end
    else
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;
  if (K1 = KindCONST) and (K2 = KindCONST) then
  begin
    GetSymbolRec(R.Res).value := Integer(GetSymbolRec(Arg1).value)
      in GetSymbolRec(Arg2).ValueAsByteSet;
    GetSymbolRec(R.Res).Kind := KindCONST;
    R.Op := OP_NOP;
  end;
end;

procedure TCode.OperSetEventProp;
var
  R: TCodeRec;
  CodeId: Integer;
begin
  R := Records[N];
  if R.Res = TKernel(kernel).SymbolTable.NilId then
    Exit;
  if R.Language = JS_LANGUAGE then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
  end;
  CodeId := GetSymbolRec(R.Res).PatternId;
  if CodeId = 0 then
    CreateError(errIncompatibleTypesNoArgs, []);
  if not GetSymbolRec(CodeId).IsMethod then
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperVarArrayIdx;
var
  Arg2, T2, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];
  Arg2 := R.Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T2 in IntegerTypes then
  begin
    // if R.Language = JS_LANGUAGE then
    begin
      R.Op := OP_PUSH_ADDRESS;
      R.Arg1 := R.Arg2;
      R.Arg2 := 0;
      R.Res := 0;
      if not(T2 in VariantTypes) then
      begin
        InsertConversionToVariant(N, 1);
        Inc(N);
      end;
      Exit;
    end;

    K2 := GetSymbolRec(Arg2).Kind;
    if K2 = KindCONST then
      R.Op := OP_PUSH_INT_IMM
    else
      R.Op := OP_PUSH_INT;
    R.Arg1 := R.Arg2;
    R.Arg2 := 0;
    R.Res := 0;
  end
  else
  begin
    if R.Language = JS_LANGUAGE then
    begin
      if
{$IFNDEF PAXARM}
        GetSymbolRec(Arg2).HasPAnsiCharType or
{$ENDIF}
        GetSymbolRec(Arg2).HasPWideCharType then
      begin
        if Records[N + 1].Op = OP_VARARRAY_PUT then
        begin
          Records[N].Op := OP_NOP;
          Records[N + 1].Op := OP_OLE_SET;
          Records[N + 1].Arg2 := Arg2;
          Exit;
        end
        else if Records[N + 1].Op = OP_VARARRAY_GET then
        begin
          Records[N].Op := OP_NOP;
          Records[N + 1].Op := OP_OLE_GET;
          Records[N + 1].Arg2 := Arg2;
          Exit;
        end;
      end;

      R.Op := OP_PUSH_ADDRESS;
      R.Arg1 := R.Arg2;
      R.Arg2 := 0;
      R.Res := 0;
      if not(T2 in VariantTypes) then
      begin
        InsertConversionToVariant(N, 1);
        Inc(N);
      end;
      Exit;
    end;

    CreateError(errIncompatibleTypesNoArgs, []);
  end;
end;

procedure TCode.OperAddition;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  AdjustTypes;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_ADD_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_INT64;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T2 = typeINT64) and (T1 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeUINT64) and (T2 = typeUINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeUINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_ADD_UINT64;
  end
  else if (T1 = typeUINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeUINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_UINT64;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToUInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T2 = typeUINT64) and (T1 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeUINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_UINT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeUINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToUInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := GetIntResultType(T1, T2);
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_ADD_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_ADD_INT_MI;
      // GetSymbolRec(R.Res).TypeID := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_ADD_INT_MI;
      R.SwapArguments;
      // GetSymbolRec(R.Res).TypeID := T2;
    end;
  end
  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_ADD_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
    begin
      if T2 <> typeCURRENCY then
        R.SwapArguments;
      R.Op := OP_ADD_CURRENCY;
    end
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_ADD_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_ADD_CURRENCY;
  end
  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_ADD_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_DOUBLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_ADD_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_SINGLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_ADD_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_EXTENDED;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end
    else
      R.Op := OP_ADD_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    if not MatchSetTypes(T1, T2) then
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;

    R.Op := OP_SET_UNION;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if T2 = typeVOID then
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId
    else
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;

    if (K1 = KindVAR) and (K2 = KindVAR) then
    begin
      // ok
    end
    else if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).ValueAsByteSet := GetSymbolRec(Arg1).ValueAsByteSet +
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      if T2 = typeVOID then // empty set
      begin
        R.Op := OP_NOP;
        ReplaceId(R.Res, R.Arg1);
      end;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      if T1 = typeVOID then // empty set
      begin
        R.Op := OP_NOP;
        ReplaceId(R.Res, R.Arg2);
      end;
      R.SwapArguments;
    end;
  end
  else if (T1 = typePOINTER) and (T2 = typePOINTER) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
{$IFNDEF PAXARM}
    if (GetSymbolRec(T1).PatternId = typeANSICHAR) and
      (GetSymbolRec(T2).PatternId = typeANSICHAR) then
    begin
      if (K1 = KindCONST) and (K2 = KindCONST) then
      begin
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
          GetSymbolRec(Arg2).value;
        GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
        GetSymbolRec(R.Res).Kind := KindCONST;
        R.Op := OP_NOP;
      end
      else
      begin
        CreateError(errIncompatibleTypesNoArgs, []);
        Exit;
      end;
    end
    else
{$ENDIF}
      if (GetSymbolRec(T1).PatternId = typeWIDECHAR) and
        (GetSymbolRec(T2).PatternId = typeWIDECHAR) then
      begin
        if (K1 = KindCONST) and (K2 = KindCONST) then
        begin
          GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
            GetSymbolRec(Arg2).value;
          GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
          GetSymbolRec(R.Res).Kind := KindCONST;
          R.Op := OP_NOP;
        end
        else
        begin
          CreateError(errIncompatibleTypesNoArgs, []);
          Exit;
        end;
      end
      else if (GetSymbolRec(T1).PatternId in CharTypes) and
        (GetSymbolRec(T2).PatternId in CharTypes) then
      begin
        if (K1 = KindCONST) and (K2 = KindCONST) then
        begin
          GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
            GetSymbolRec(Arg2).value;
          GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
          GetSymbolRec(R.Res).Kind := KindCONST;
          R.Op := OP_NOP;
        end
        else
        begin
          CreateError(errIncompatibleTypesNoArgs, []);
          Exit;
        end;
      end
      else
        CreateError(errIncompatibleTypesNoArgs, []);
  end
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSISTRING) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end

  else if (T1 = typeANSISTRING) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 = typeANSISTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end

  else if (T1 = typeANSISTRING) and (T2 = typeANSICHAR) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSICHAR) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSICHAR) and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) +
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).TypeId := typePANSICHAR;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeSHORTSTRING) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end

  else if (T1 = typeSHORTSTRING) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end

  else if (T1 = typeSHORTSTRING) and (T2 = typeANSICHAR) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSICHAR) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeSHORTSTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(1, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_ADD_ANSISTRING;
    GetSymbolRec(R.Res).TypeId := typeANSISTRING;
    InsertDeclareTempVar;
  end
{$ENDIF}
  // wide string

  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).TypeId := typePWIDECHAR;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
{$IFDEF UNIC}
    InsertConversionToUnicString(N, 1);
    Inc(N);
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
{$ELSE}
    InsertConversionToWideString(N, 1);
    Inc(N);
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
{$ENDIF}
    InsertDeclareTempVar;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).TypeId := typePWIDECHAR;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
{$IFDEF UNIC}
    InsertConversionToUnicString(N, 1);
    Inc(N);
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
{$ELSE}
    InsertConversionToWideString(N, 1);
    Inc(N);
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
{$ENDIF}
    InsertDeclareTempVar;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeWIDESTRING) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeWIDESTRING) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeWIDESTRING) and (T2 in CharTypes) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSICHAR) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeWIDESTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
    InsertDeclareTempVar;
  end
{$ENDIF}
  else if (T1 = typeWIDECHAR) and (T2 = typeWIDECHAR) then
  begin
{$IFDEF UNIC}
    InsertConversionToUnicString(N, 1);
    Inc(N);
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
{$ELSE}
    InsertConversionToWideString(N, 1);
    Inc(N);
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_ADD_WIDESTRING;
    GetSymbolRec(R.Res).TypeId := typeWIDESTRING;
{$ENDIF}
    InsertDeclareTempVar;
  end

  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeUNICSTRING) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
{$ENDIF}
  else if (T1 = typeUNICSTRING) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
{$IFNDEF PAXARM}
  else if (T1 = typeUNICSTRING) and (T2 = typeANSICHAR) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSICHAR) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeUNICSTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if (T1 = typeANSISTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
{$ENDIF}
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_ADD_UNICSTRING;
    GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
    InsertDeclareTempVar;
  end

  // variant

  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_ADD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
    InsertDeclareTempVar;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_ADD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
    InsertDeclareTempVar;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_ADD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
    InsertDeclareTempVar;
  end
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_ADD_ANSISTRING;
      GetSymbolRec(R.Res).TypeId := typeANSISTRING;
      InsertDeclareTempVar;
    end;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) +
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_ADD_ANSISTRING;
      GetSymbolRec(R.Res).TypeId := typeANSISTRING;
      InsertDeclareTempVar;
    end;
  end
{$ENDIF}
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Add) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Add);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Add) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Add);
    Inc(N);
  end
  else
  begin
    if R.Language = JS_LANGUAGE then
    begin
{$IFNDEF PAXARM}
      if T1 = typeANSISTRING then
      begin
        InsertConversionToAnsiString(N, 2, JS_LANGUAGE);
        Inc(N);
        R.Op := OP_ADD_ANSISTRING;
        GetSymbolRec(R.Res).TypeId := typeANSISTRING;
        InsertDeclareTempVar;
        Exit;
      end
      else if T2 = typeANSISTRING then
      begin
        InsertConversionToAnsiString(N, 1, JS_LANGUAGE);
        Inc(N);
        R.Op := OP_ADD_ANSISTRING;
        GetSymbolRec(R.Res).TypeId := typeANSISTRING;
        InsertDeclareTempVar;
        Exit;
      end
      else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 in IntegerTypes) then
      begin
        if (K1 = KindCONST) and (K2 = KindCONST) then
        begin
          GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
            IntToStr(Integer(GetSymbolRec(Arg2).value));
          GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
          GetSymbolRec(R.Res).Kind := KindCONST;
          R.Op := OP_NOP;
        end
        else
        begin
          InsertConversionToAnsiString(N, 1, JS_LANGUAGE);
          Inc(N);
          InsertConversionToAnsiString(N, 2, JS_LANGUAGE);
          Inc(N);
          R.Op := OP_ADD_ANSISTRING;
          GetSymbolRec(R.Res).TypeId := typeANSISTRING;
          InsertDeclareTempVar;
        end;
        Exit;
      end
      else if (T1 in IntegerTypes) and GetSymbolRec(Arg2).HasPAnsiCharType then
      begin
        if (K1 = KindCONST) and (K2 = KindCONST) then
        begin
          GetSymbolRec(R.Res).value :=
            IntToStr(Integer(GetSymbolRec(Arg1).value)) +
            GetSymbolRec(Arg2).value;
          GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;
          GetSymbolRec(R.Res).Kind := KindCONST;
          R.Op := OP_NOP;
        end
        else
        begin
          InsertConversionToAnsiString(N, 1, JS_LANGUAGE);
          Inc(N);
          InsertConversionToAnsiString(N, 2, JS_LANGUAGE);
          Inc(N);
          R.Op := OP_ADD_ANSISTRING;
          GetSymbolRec(R.Res).TypeId := typeANSISTRING;
          InsertDeclareTempVar;
        end;
        Exit;
      end
      else
{$ENDIF}
        if T1 = typeUNICSTRING then
        begin
          InsertConversionToUnicString(N, 2, JS_LANGUAGE);
          Inc(N);
          R.Op := OP_ADD_UNICSTRING;
          GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
          InsertDeclareTempVar;
          Exit;
        end
        else if T2 = typeUNICSTRING then
        begin
          InsertConversionToUnicString(N, 1, JS_LANGUAGE);
          Inc(N);
          R.Op := OP_ADD_UNICSTRING;
          GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
          InsertDeclareTempVar;
          Exit;
        end
        else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in IntegerTypes)
        then
        begin
          if (K1 = KindCONST) and (K2 = KindCONST) then
          begin
            GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
              IntToStr(Integer(GetSymbolRec(Arg2).value));
            GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId;
            GetSymbolRec(R.Res).Kind := KindCONST;
            R.Op := OP_NOP;
          end
          else
          begin
            InsertConversionToUnicString(N, 1, JS_LANGUAGE);
            Inc(N);
            InsertConversionToUnicString(N, 2, JS_LANGUAGE);
            Inc(N);
            R.Op := OP_ADD_UNICSTRING;
            GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
            InsertDeclareTempVar;
          end;
          Exit;
        end
        else if (T1 in IntegerTypes) and GetSymbolRec(Arg2).HasPWideCharType
        then
        begin
          if (K1 = KindCONST) and (K2 = KindCONST) then
          begin
            GetSymbolRec(R.Res).value :=
              IntToStr(Integer(GetSymbolRec(Arg1).value)) +
              GetSymbolRec(Arg2).value;
            GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;
            GetSymbolRec(R.Res).Kind := KindCONST;
            R.Op := OP_NOP;
          end
          else
          begin
            InsertConversionToUnicString(N, 1, JS_LANGUAGE);
            Inc(N);
            InsertConversionToUnicString(N, 2, JS_LANGUAGE);
            Inc(N);
            R.Op := OP_ADD_UNICSTRING;
            GetSymbolRec(R.Res).TypeId := typeUNICSTRING;
            InsertDeclareTempVar;
          end;
          Exit;
        end;

    end // JS_LANGUAGE
    else
    begin
      if (
{$IFNDEF PAXARM}
        GetSymbolRec(R.Arg1).HasPAnsiCharType or
{$ENDIF}
        GetSymbolRec(R.Arg1).HasPWideCharType) and (T2 in IntegerTypes) then
      begin
        GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
        if (K1 = KindCONST) and (K2 = KindCONST) then
        begin
          GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value +
            GetSymbolRec(Arg2).value;
          GetSymbolRec(R.Res).Kind := KindCONST;
          if T1 = T2 then
            GetSymbolRec(R.Res).TypeId := T1;
          R.Op := OP_NOP;
        end
        else if (K1 = KindVAR) and (K2 = KindVAR) then
          R.Op := OP_ADD_INT_MM
        else if (K1 = KindVAR) and (K2 = KindCONST) then
        begin
          R.Op := OP_ADD_INT_MI;
        end
        else if (K1 = KindCONST) and (K2 = KindVAR) then
        begin
          R.Op := OP_ADD_INT_MI;
          R.SwapArguments;
        end;
        Exit;
      end;
    end;

    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_ADD_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperNegation;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if T1 = typeINT64 then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_INT64;
  end
  else if T1 = typeUINT64 then
  begin
    GetSymbolRec(R.Res).TypeId := typeUINT64;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_UINT64;
  end
  else if T1 in IntegerTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetIntResultType(T1, T1);
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_INT;
  end
  else if T1 = typeDOUBLE then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_DOUBLE;
  end
  else if T1 = typeCURRENCY then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_CURRENCY;
  end
  else if T1 = typeSINGLE then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_SINGLE;
  end
  else if T1 = typeEXTENDED then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := -GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_NEG_EXTENDED;
  end
  else if T1 = typeVARIANT then
  begin
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
    R.Op := OP_NEG_VARIANT;
  end
  else if T1 = typeRECORD then
  begin
    InsertUnaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Negative);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      R.Op := OP_NEG_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    CreateError(errOperatorNotApplicableToThisOperandType,
      [GetSymbolRec(T1).Name]);
  end;
end;

procedure TCode.OperPositive;
var
  Arg1, Res, T1, K1: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Res := R.Res;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if T1 = typeINT64 then
  begin
    R.Op := OP_NOP;
    R.Res := Arg1;
    ReplaceId(Res, Arg1);
  end
  else if T1 in IntegerTypes then
  begin
    R.Op := OP_NOP;
    R.Res := Arg1;
    ReplaceId(Res, Arg1);
  end
  else if T1 in RealTypes then
  begin
    R.Op := OP_NOP;
    R.Res := Arg1;
    ReplaceId(Res, Arg1);
  end
  else if T1 = typeVARIANT then
  begin
    R.Op := OP_NOP;
    R.Res := Arg1;
    ReplaceId(Res, Arg1);
  end
  else if T1 = typeRECORD then
  begin
    InsertUnaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Positive);
    Inc(N);
  end
  else
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    CreateError(errOperatorNotApplicableToThisOperandType,
      [GetSymbolRec(T1).Name]);
  end;
end;

procedure TCode.OperNot;
var
  Arg1, T1, K1: Integer;
  R: TCodeRec;
  S: String;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  K1 := GetSymbolRec(Arg1).Kind;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if T1 in BooleanTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := not Boolean(GetSymbolRec(Arg1).value);
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if K1 = KindVAR then
    begin
      case T1 of
        typeBOOLEAN:
          R.Op := OP_NOT_BOOL;
        typeBYTEBOOL:
          R.Op := OP_NOT_BYTEBOOL;
        typeWORDBOOL:
          R.Op := OP_NOT_WORDBOOL;
        typeLONGBOOL:
          R.Op := OP_NOT_LONGBOOL;
      end;
    end;
  end
  else if T1 in IntegerTypes then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
    if K1 = KindCONST then
    begin
      GetSymbolRec(R.Res).value := not GetSymbolRec(Arg1).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if K1 = KindVAR then
    begin
      // ok
    end;
  end
  else if T1 = typeVARIANT then
  begin
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
    R.Op := OP_NOT_VARIANT;
  end
  else if T1 = typeRECORD then
  begin
    InsertUnaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalNot);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      R.Op := OP_NOT_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    S := GetSymbolRec(T1).Name;
    if (PosCh('_', S) > 0) or (PosCh('.', S) > 0) or (S = '') then
      CreateError(errOperatorNotApplicableToThisOperandTypeNoArgs, [])
    else
      CreateError(errOperatorNotApplicableToThisOperandType, [S]);
  end;
end;

procedure TCode.OperSubtraction;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  AdjustTypes;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_SUB_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := GetIntResultType(T1, T2);

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := T1;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_SUB_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else
      R.Op := OP_SUB_INT_MM;
  end
{$IFNDEF PAXARM}
  else if (GetSymbolRec(R.Arg1).HasPAnsiCharType or GetSymbolRec(R.Arg1)
    .HasPWideCharType) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := GetSymbolRec(R.Arg1).TypeId;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_SUB_INT_MI
    else
      R.Op := OP_SUB_INT_MM;
  end
{$ENDIF}
  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_SUB_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
    begin
      if T2 <> typeCURRENCY then
        R.SwapArguments;
      R.Op := OP_SUB_CURRENCY;
    end
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_SUB_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_SUB_CURRENCY;
  end
  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_SUB_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_DOUBLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_SUB_DOUBLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in (NumberTypes)) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_SINGLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in (NumberTypes)) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value -
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SUB_EXTENDED;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    if not MatchSetTypes(T1, T2) then
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;

    T2 := GetSymbolRec(T2).PatternId;

    if T2 = typeVOID then
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId
    else
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).ValueAsByteSet := GetSymbolRec(Arg1).ValueAsByteSet -
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_DIFFERENCE
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_SUB_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_SUB_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_SUB_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Subtract) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Subtract);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Subtract) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Subtract);
    Inc(N);
  end
  else
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    if R.Language = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertDeclareTempVar;

      Dec(N);

      Exit;
    end;

    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperMultiplication;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  AdjustTypes;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_MULT_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MULT_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MULT_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := GetIntResultType(T1, T2);

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;

      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_IMUL_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_IMUL_INT_MI;
      // GetSymbolRec(R.Res).TypeID := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_IMUL_INT_MI;
      R.SwapArguments;
      // GetSymbolRec(R.Res).TypeID := T2;
    end;
  end
  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_MUL_CURRENCY;
      if T2 <> typeCURRENCY then
      begin
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
        R.SwapArguments;
      end;
    end
    else
    begin
      if T2 <> typeCURRENCY then
        R.SwapArguments;
      R.Op := OP_MUL_CURRENCY;
    end
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_MUL_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_MUL_CURRENCY;
  end
  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
      R.Op := OP_MUL_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_MUL_DOUBLE;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_DOUBLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_SINGLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value *
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MUL_EXTENDED;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    if not MatchSetTypes(T1, T2) then
    begin
      CreateError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;

    T2 := GetSymbolRec(T2).PatternId;

    if T2 = typeVOID then
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg1).TypeId
    else
      GetSymbolRec(R.Res).TypeId := GetSymbolRec(Arg2).TypeId;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).ValueAsByteSet := GetSymbolRec(Arg1).ValueAsByteSet *
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_INTERSECTION
    else
      CreateError(errIncompatibleTypesNoArgs, []);
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_MULT_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_MULT_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_MULT_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Multiply) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Multiply);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Multiply) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Multiply);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_MULT_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.AdjustTypes;
var
  Arg1, Arg2, T, K1, K2, I, Op: Integer;
  R: TCodeRec;
  I64: Int64;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if (K1 <> KindCONST) or (K2 <> KindCONST) then
    Exit;

  I := N;
  repeat
    if (Records[I + 1].Op = OP_ASSIGN) and (Records[I + 1].Arg2 = R.Res) then
    begin
      T := GetSymbolRec(Records[I + 1].Arg1).FinalTypeId;
      if T = 0 then
        Exit;
      if not(T in (IntegerTypes + RealTypes)) then
        Exit;

      GetSymbolRec(Arg1).TypeId := T;
      GetSymbolRec(Arg2).TypeId := T;
      case T of
        typeINT64:
          begin
{$IFDEF VARIANTS}
            I64 := GetSymbolRec(Arg1).value;
            GetSymbolRec(Arg1).value := I64;
            I64 := GetSymbolRec(Arg2).value;
            GetSymbolRec(Arg2).value := I64;
{$ELSE}
            I64 := Integer(GetSymbolRec(Arg1).value);
            GetSymbolRec(Arg1).value := Integer(I64);
            I64 := Integer(GetSymbolRec(Arg2).value);
            GetSymbolRec(Arg2).value := Integer(I64);
{$ENDIF}
          end;
        typeSINGLE:
          begin
            GetSymbolRec(Arg1).value := Single(GetSymbolRec(Arg1).value);
            GetSymbolRec(Arg2).value := Single(GetSymbolRec(Arg2).value);
          end;
        typeDOUBLE:
          begin
            GetSymbolRec(Arg1).value := Double(GetSymbolRec(Arg1).value);
            GetSymbolRec(Arg2).value := Double(GetSymbolRec(Arg2).value);
          end;
        typeEXTENDED:
          begin
            GetSymbolRec(Arg1).value := Extended(GetSymbolRec(Arg1).value);
            GetSymbolRec(Arg2).value := Extended(GetSymbolRec(Arg2).value);
          end;
        typeCURRENCY:
          begin
            GetSymbolRec(Arg1).value := Currency(GetSymbolRec(Arg1).value);
            GetSymbolRec(Arg2).value := Currency(GetSymbolRec(Arg2).value);
          end;
      end;
      Exit;
    end;

    Op := Records[I].Op;

    if Op = OP_SEPARATOR then
    begin
      Inc(I);
      continue;
    end;

    if (Op = OP_PLUS) or (Op = OP_MINUS) or (Op = OP_MULT) or (Op = OP_DIV) or
      (Op = OP_IDIV) or (Op = OP_MOD) or (Op = OP_SHL) or (Op = OP_SHR) or
      (Op = OP_AND) or (Op = OP_OR) or (Op = OP_XOR) or (Op = OP_NOT) or
      (Op = OP_GT) or (Op = OP_GE) or (Op = OP_LT) or (Op = OP_LE) or
      (Op = OP_EQ) or (Op = OP_NE) then
      Inc(I)
    else
      Exit;

  until false;
end;

procedure TCode.OperDivision;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  AdjustTypes;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value /
          GetSymbolRec(Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (R.Language = JS_LANGUAGE) and (not GetSymbolRec(R.Res).FinalTypeId
      in IntegerTypes) then
    begin
      if K1 = KindCONST then
        R.Op := OP_IDIV_INT_IM
      else if K2 = KindCONST then
        R.Op := OP_IDIV_INT_MI
      else
        R.Op := OP_IDIV_INT_MM;
      GetSymbolRec(R.Res).TypeId := typeINTEGER;
      Exit;
    end;

    R.Op := OP_DIV_DOUBLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;

  end
  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value /
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_DIV_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_DIV_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    GetSymbolRec(R.Res).TypeId := typeCURRENCY;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value /
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_DIV_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_DIV_CURRENCY;
  end
  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Double(GetSymbolRec(Arg1).value) /
          Double(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Double(GetSymbolRec(Arg1).value) /
          Double(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeDOUBLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Double(GetSymbolRec(Arg1).value) /
          Double(GetSymbolRec(Arg2).value);
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_DOUBLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Single(GetSymbolRec(Arg1).value) /
          Single(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Single(GetSymbolRec(Arg1).value) /
          Single(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    GetSymbolRec(R.Res).TypeId := typeSINGLE;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Single(GetSymbolRec(Arg1).value) /
          Single(GetSymbolRec(Arg2).value);
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_SINGLE;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Extended(GetSymbolRec(Arg1).value) /
          Extended(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Extended(GetSymbolRec(Arg1).value) /
          Extended(GetSymbolRec(Arg2).value);

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    GetSymbolRec(R.Res).TypeId := typeEXTENDED;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := Extended(GetSymbolRec(Arg1).value) /
          Extended(GetSymbolRec(Arg2).value);
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_DIV_EXTENDED;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_DIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_DIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_DIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Divide) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Divide);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Divide) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Divide);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_DIV_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperIDivision;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  AdjustTypes;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_IDIV_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value div GetSymbolRec
          (Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_IDIV_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value div GetSymbolRec
          (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_IDIV_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value div GetSymbolRec
          (Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;

      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_IDIV_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, []);

      R.Op := OP_IDIV_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_IDIV_INT_IM;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_IDIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_IDIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_IDIV_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_IntDivide) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_IntDivide);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_IntDivide) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_IntDivide);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_IDIV_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperModulo;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_MOD_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value mod GetSymbolRec
          (Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MOD_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value mod GetSymbolRec
          (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_MOD_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, [])
      else
        GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value mod GetSymbolRec
          (Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;

      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_MOD_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      if GetSymbolRec(Arg2).value = 0 then
        CreateError(errDivisionByZero, []);

      R.Op := OP_MOD_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_MOD_INT_IM;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_MOD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_MOD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_MOD_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Modulus) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Modulus);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Modulus) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Modulus);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_MOD_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperLeftShift;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_SHL_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shl GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_SHL_INT64;
    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shl GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SHL_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shl GetSymbolRec
        (Arg2).value;

      GetSymbolRec(R.Res).Kind := KindCONST;

      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_SHL_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_SHL_INT_MI;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_SHL_INT_IM;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_SHL_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_SHL_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_SHL_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LeftShift) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LeftShift);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LeftShift) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_LeftShift);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_SHL_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperRightShift;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_SHR_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shr GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SHR_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shr GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_SHR_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value shr GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_SHR_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_SHR_INT_MI;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_SHR_INT_IM;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_SHR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_SHR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_SHR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_RightShift) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_RightShift);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_RightShift) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_RightShift);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_SHR_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperAnd;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_AND_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value and
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_AND_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value and
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_AND_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value and
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_AND_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_AND_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_AND_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 in BooleanTypes) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeBOOLEAN;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value and
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_AND_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_AND_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_AND_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_AND_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_AND_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_AND_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalAnd) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalAnd);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalAnd) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalAnd);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_AND_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperOr;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_OR_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value or
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_OR_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value or
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_OR_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value or
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_OR_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_OR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_OR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 in BooleanTypes) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeBOOLEAN;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value or
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_OR_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_OR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_OR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_OR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_OR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_OR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalOr) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalOr);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalOr) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalOr);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_OR_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperXor;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;
    R.Op := OP_XOR_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value xor GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_XOR_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINT64;

    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value xor GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_XOR_INT64;

    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in IntegerTypes) and (T2 in IntegerTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeINTEGER;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value xor GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_XOR_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_XOR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_XOR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 in BooleanTypes) and (T2 in BooleanTypes) then
  begin
    GetSymbolRec(R.Res).TypeId := typeBOOLEAN;
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value xor GetSymbolRec
        (Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      if T1 = T2 then
        GetSymbolRec(R.Res).TypeId := T1;
      R.Op := OP_NOP;
    end
    else if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_XOR_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      R.Op := OP_XOR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T1;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_XOR_INT_MI;
      GetSymbolRec(R.Res).TypeId := T2;
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_XOR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_XOR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_XOR_VARIANT;
    GetSymbolRec(R.Res).TypeId := typeVARIANT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalXor) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LogicalXor);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalXor) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_LogicalXor);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      GetSymbolRec(R.Res).TypeId := typeVARIANT;
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_XOR_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

function TCode.ExistsOrdinalRelationalOperator(T1, T2: Integer): Boolean;
begin
  result := (T1 in IntegerTypes) and (T2 in IntegerTypes);
  if not result then
    result := (T1 in CharTypes) and (T2 in CharTypes);
  if not result then
    result := (T1 in BooleanTypes) and (T2 in BooleanTypes);
end;

procedure TCode.OperLessThan;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LT_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeUINT64) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_UINT64;
  end
  else if (T1 = typeUINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_UINT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToUInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LT_UINT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeUINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToUInt64(N, 1);
      Inc(N);
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_LT_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_LT_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_GT_INT_MI;
    end;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_LT_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_LT_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_LT_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_LT_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LT_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LT_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LT_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LT_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_LT_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_LT_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_LT_VARIANT;
  end

  // string
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_LT_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_LT_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_LT_ANSISTRING;
  end

  // shortstring

  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_LT_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_LT_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_LT_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_LT_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_LT_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_LT_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_LT_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_LT_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_LT_UNICSTRING;
  end
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LT_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LT_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LT_ANSISTRING;
    end;
  end
{$ENDIF}
  //

  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LT_UNICSTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LT_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LT_UNICSTRING;
    end;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LessThan) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_LessThan);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LessThan) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_LessThan);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_LT_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperLessThanOrEqual;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LE_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeUINT64) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_UINT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_UINT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToUInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LE_UINT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeUINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToUInt64(N, 1);
      Inc(N);
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_LE_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_LE_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_GE_INT_MI;
    end;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_LE_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_LE_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_LE_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_LE_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LE_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LE_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_LE_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_LE_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    if not MatchSetTypes(T1, T2) then
      CreateError(errIncompatibleTypesNoArgs, [])
    else if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).ValueAsByteSet <=
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_SUBSET
    else
      CreateError(errIncompatibleTypesNoArgs, [])
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_LE_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_LE_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_LE_VARIANT;
  end

  // string
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_LE_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_LE_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_LE_ANSISTRING;
  end

  // shortstring

  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_LE_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_LE_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_LE_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_LE_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_LE_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_LE_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_LE_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_LE_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_LE_UNICSTRING;
  end
  //
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LE_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LE_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_LE_ANSISTRING;
    end;
  end
{$ENDIF}
  //
  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LE_UNICSTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <=
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LE_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_LE_UNICSTRING;
    end;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_LessThanOrEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId,
      gen_LessThanOrEqual);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_LessThanOrEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId,
      gen_LessThanOrEqual);
    Inc(N);
  end
  //
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_LE_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperGreaterThan;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GT_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeUINT64) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_UINT64;
  end
  else if (T1 = typeUINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_UINT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToUInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GT_UINT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeUINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToUInt64(N, 1);
      Inc(N);
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_GT_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_GT_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_LT_INT_MI;
    end;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_GT_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_GT_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_GT_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_GT_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GT_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GT_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GT_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GT_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_GT_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_GT_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_GT_VARIANT;
  end

  // string
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_GT_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_GT_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_GT_ANSISTRING;
  end

  // shortstring

  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_GT_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_GT_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_GT_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_GT_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_GT_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_GT_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_GT_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_GT_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_GT_UNICSTRING;
  end
  //
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GT_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GT_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GT_ANSISTRING;
    end;
  end
{$ENDIF}
  //
  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GT_UNICSTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GT_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) >
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GT_UNICSTRING;
    end;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_GreaterThan) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_GreaterThan);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_GreaterThan) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_GreaterThan);
    Inc(N);
  end
  //
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_GT_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperGreaterThanOrEqual;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 = typeINT64) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_INT64;
  end
  else if (T1 = typeINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GE_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end
  else if (T1 = typeUINT64) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_UINT64;
  end
  else if (T1 = typeUINT64) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_UINT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeUINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToUInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 = typeUINT64) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GE_UINT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeUINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToUInt64(N, 1);
      Inc(N);
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_GE_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_GE_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      R.Op := OP_LE_INT_MI;
    end;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_GE_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_GE_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_GE_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_GE_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GE_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GE_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_GE_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_GE_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    if not MatchSetTypes(T1, T2) then
      CreateError(errIncompatibleTypesNoArgs, [])
    else if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).ValueAsByteSet >=
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_SUPERSET
    else
      CreateError(errIncompatibleTypesNoArgs, [])
  end
  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_GE_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_GE_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_GE_VARIANT;
  end

  // string
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_GE_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_GE_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_GE_ANSISTRING;
  end

  // shortstring

  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_GE_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_GE_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_GE_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_GE_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_GE_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_GE_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_GE_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_GE_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_GE_UNICSTRING;
  end
  //
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GE_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GE_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_GE_ANSISTRING;
    end;
  end
{$ENDIF}
  //
  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GE_UNICSTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPWideCharType and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value >=
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GE_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and GetSymbolRec(Arg2).HasPWideCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) >=
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_GE_UNICSTRING;
    end;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_GreaterThanOrEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId,
      gen_GreaterThanOrEqual);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_GreaterThanOrEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId,
      gen_GreaterThanOrEqual);
    Inc(N);
  end
  //
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_GE_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

function TCode.InsertSubCall(I: Integer): Boolean;
var
  SubId, TypeId: Integer;
  R, RC: TCodeRec;
begin
  result := true;

  R := Records[N];

  SubId := 0;
  case I of
    1:
      SubId := R.Arg1;
    2:
      SubId := R.Arg2;
    3:
      SubId := R.Res;
  else
    RaiseError(errInternalError, []);
  end;

  if TKernel(kernel).SymbolTable.EventNilId = SubId then
  begin
    result := false;
    Exit;
  end;

  TypeId := GetSymbolRec(SubId).TerminalTypeId;
  TypeId := GetSymbolRec(TypeId).PatternId;

  if GetSymbolRec(TypeId).Count > 0 then
  begin
    result := false;
    Exit;
  end;

  TypeId := GetSymbolRec(TypeId).TypeId;

  RC := TCodeRec.Create(OP_CALL, Self);
  RC.Arg1 := SubId;
  RC.Arg2 := 0;
  RC.Res := NewTempVar(GetLevel(N), TypeId);

  Insert(N, RC);

  case I of
    1:
      R.Arg1 := RC.Res;
    2:
      R.Arg2 := RC.Res;
    3:
      R.Res := RC.Res;
  else
    RaiseError(errInternalError, []);
  end;

  Dec(N);
end;

procedure TCode.OperEqual;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 in INT64Types) and (T2 in INT64Types) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_INT64;
  end
  else if (T1 in INT64Types) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 in INT64Types) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_EQ_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in BooleanTypes) and (T2 in BooleanTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if (TVarData(GetSymbolRec(Arg1).value).VInteger <> 0) and
        (TVarData(GetSymbolRec(Arg2).value).VInteger <> 0) then
        GetSymbolRec(R.Res).value := true
      else if (TVarData(GetSymbolRec(Arg1).value).VInteger = 0) and
        (TVarData(GetSymbolRec(Arg2).value).VInteger = 0) then
        GetSymbolRec(R.Res).value := true
      else
        GetSymbolRec(R.Res).value := false;

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    if (K1 = KindVAR) and (K2 = KindVAR) then
    begin
      if T1 <> T2 then
      begin
        if T1 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T2 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 2);
          Inc(N);
        end;
      end;
      R.Op := OP_EQ_INT_MM;
    end
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      if T1 <> T2 then
      begin
        if T1 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T2 <> typeBOOLEAN then
        begin
          if TVarData(GetSymbolRec(Arg2).value).VInteger <> 0 then
            R.Arg2 := SymbolTable.TrueId
          else
            R.Arg2 := SymbolTable.FalseId
        end;
      end;
      R.Op := OP_EQ_INT_MI;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_EQ_INT_MI;
      R.SwapArguments;
      if T1 <> T2 then
      begin
        if T2 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T1 <> typeBOOLEAN then
        begin
          if TVarData(GetSymbolRec(Arg2).value).VInteger <> 0 then
            R.Arg2 := SymbolTable.TrueId
          else
            R.Arg2 := SymbolTable.FalseId
        end;
      end;
      R.Op := OP_EQ_INT_MI;
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_EQ_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_EQ_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_EQ_INT_MI;
      R.SwapArguments;
    end;
  end
  else if (T1 = typeCLASS) and (Arg2 = SymbolTable.NilId) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    R.Op := OP_EQ_INT_MI;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typeCLASS) then
  begin
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_EQ_INT_MI;
    R.SwapArguments;
  end
  else if (T1 = typeCLASS) and (T2 = typeCLASS) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_EQ_INT_MM;
  end
  else if (T1 = typeINTERFACE) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_EQ_INT_MI;
  end
  else if (T1 = typeINTERFACE) and (T2 = typeINTERFACE) then
  begin
    R.Op := OP_EQ_INT_MM;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typeINTERFACE) then
  begin
    R.Op := OP_EQ_INT_MI;
    R.SwapArguments;
  end
  else if (T1 in [typePOINTER, typeCLASS, typeCLASSREF, typePROC, typeINTERFACE]
    ) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_EQ_INT_MI;
  end
  else if (Arg1 = SymbolTable.NilId) and
    (T2 in [typePOINTER, typeCLASS, typeCLASSREF, typePROC, typeINTERFACE]) then
  begin
    R.Op := OP_EQ_INT_MI;
    R.SwapArguments;
  end
  else if (T1 = typePOINTER) and (T2 = typePOINTER) then
  begin
{$IFNDEF PAXARM}
    if (GetSymbolRec(Arg1).HasPAnsiCharType or GetSymbolRec(Arg1)
      .HasPWideCharType) and (GetSymbolRec(Arg2).HasPAnsiCharType or
      GetSymbolRec(Arg2).HasPWideCharType) then
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_EQ_WIDESTRING;
      Exit;
    end;
{$ENDIF}
    R.Op := OP_EQ_INT_MM;
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASSREF) then
  begin
    R.Op := OP_EQ_INT_MM;
  end
  else if (T1 = typeCLASS) and (T2 = typeCLASSREF) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    R.Op := OP_EQ_INT_MM;
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASS) then
  begin
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_EQ_INT_MM;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_EQ_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_EQ_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_EQ_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_EQ_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_EQ_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_EQ_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_EQ_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_EQ_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    if not MatchSetTypes(T1, T2) then
      CreateError(errIncompatibleTypesNoArgs, [])
    else if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .ValueAsByteSet = GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_EQUALITY
    else
      CreateError(errIncompatibleTypesNoArgs, [])
  end
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_EQ_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_EQ_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_EQ_ANSISTRING;
  end
  //
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_EQ_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_EQ_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value))
        = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_EQ_ANSISTRING;
    end;
  end
{$ENDIF}
  //
  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_EQ_UNICSTRING;
    end;
  end
  else if (GetSymbolRec(Arg1).HasPWideCharType
{$IFNDEF PAXARM}
    or GetSymbolRec(Arg1).HasPAnsiCharType
{$ENDIF}
    ) and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1)
        .value = Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_EQ_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and (GetSymbolRec(Arg2).HasPWideCharType
{$IFNDEF PAXARM}
    or GetSymbolRec(Arg2).HasPAnsiCharType
{$ENDIF}
    ) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value))
        = GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_EQ_UNICSTRING;
    end;
  end
{$IFNDEF PAXARM}
  //
  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_EQ_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_EQ_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_EQ_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_EQ_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_EQ_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_EQ_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_EQ_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_EQ_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_EQ_UNICSTRING;
  end

  // variant

  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_EQ_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_EQ_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_EQ_VARIANT;
  end
  else if (T1 = typeEVENT) and (T2 = typeEVENT) then
  begin
    R.Op := OP_EQ_EVENT;
  end
  else if (T1 = typeEVENT) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_EQ_EVENT;
    R.Arg2 := SymbolTable.EventNilId;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typeEVENT) then
  begin
    R.Op := OP_EQ_EVENT;
    R.Arg1 := SymbolTable.EventNilId;
  end
  else if (T1 = typeARRAY) and (T2 = typeARRAY) and
    (GetSymbolRec(Arg1).TypeId = GetSymbolRec(Arg2).TypeId) then
  begin
    R.Op := OP_EQ_STRUCT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_Equal) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_Equal);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_Equal) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_Equal);
    Inc(N);
  end
  else if (T1 = typePOINTER) and (T2 = typePOINTER) then
  begin
    R.Op := OP_EQ_INT_MM;
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_EQ_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperNotEqual;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if T1 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(1) then
      Exit;
  end;
  if T2 in [typePROC, typeEVENT] then
  begin
    if InsertSubCall(2) then
      Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  if T1 = typeOLEVARIANT then
    T1 := typeVARIANT;
  if T2 = typeOLEVARIANT then
    T2 := typeVARIANT;

  if T1 = typeSET then
    CreateSetObject(Arg1);
  if T2 = typeSET then
    CreateSetObject(Arg2);

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  if (T1 = typeENUM) and (T2 = typeENUM) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;

    T1 := GetSymbolRec(T1).PatternId;
    T2 := GetSymbolRec(T2).PatternId;

    if GetSymbolRec(Arg1).HasSubrangeEnumType then // enum subrange type
    begin
      T1 := GetSymbolRec(Arg1).TypeId + 1;
      T1 := GetSymbolRec(T1).TypeId;
      T1 := GetSymbolRec(T1).PatternId;
    end;

    if GetSymbolRec(Arg2).HasSubrangeEnumType then // enum subrange type
    begin
      T2 := GetSymbolRec(Arg1).TypeId + 1;
      T2 := GetSymbolRec(T2).TypeId;
      T2 := GetSymbolRec(T2).PatternId;
    end;
  end;

  if not(K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
    Exit;
  end;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 in INT64Types) and (T2 in INT64Types) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_INT64;
  end
  else if (T1 in INT64Types) and (T2 in IntegerTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_INT64;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeINT64, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToInt64(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in IntegerTypes) and (T2 in INT64Types) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_NE_INT64;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeINT64, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToInt64(N, 1);
      Inc(N);
    end;
  end

  else if (T1 in BooleanTypes) and (T2 in BooleanTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      if (TVarData(GetSymbolRec(Arg1).value).VInteger <> 0) and
        (TVarData(GetSymbolRec(Arg2).value).VInteger <> 0) then
        GetSymbolRec(R.Res).value := false
      else if (TVarData(GetSymbolRec(Arg1).value).VInteger = 0) and
        (TVarData(GetSymbolRec(Arg2).value).VInteger = 0) then
        GetSymbolRec(R.Res).value := false
      else
        GetSymbolRec(R.Res).value := false;

      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    if (K1 = KindVAR) and (K2 = KindVAR) then
    begin
      if T1 <> T2 then
      begin
        if T1 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T2 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 2);
          Inc(N);
        end;
      end;
      R.Op := OP_NE_INT_MM;
    end
    else if (K1 = KindVAR) and (K2 = KindCONST) then
    begin
      if T1 <> T2 then
      begin
        if T1 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T2 <> typeBOOLEAN then
        begin
          if TVarData(GetSymbolRec(Arg2).value).VInteger <> 0 then
            R.Arg2 := SymbolTable.TrueId
          else
            R.Arg2 := SymbolTable.FalseId
        end;
      end;
      R.Op := OP_NE_INT_MI;
    end
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.SwapArguments;
      if T1 <> T2 then
      begin
        if T2 <> typeBOOLEAN then
        begin
          InsertConversionToBoolean(N, 1);
          Inc(N);
        end;
        if T1 <> typeBOOLEAN then
        begin
          if TVarData(GetSymbolRec(Arg2).value).VInteger <> 0 then
            R.Arg2 := SymbolTable.TrueId
          else
            R.Arg2 := SymbolTable.FalseId
        end;
      end;
      R.Op := OP_NE_INT_MI;
    end;
  end

  else if ExistsOrdinalRelationalOperator(T1, T2) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    if (K1 = KindVAR) and (K2 = KindVAR) then
      R.Op := OP_NE_INT_MM
    else if (K1 = KindVAR) and (K2 = KindCONST) then
      R.Op := OP_NE_INT_MI
    else if (K1 = KindCONST) and (K2 = KindVAR) then
    begin
      R.Op := OP_NE_INT_MI;
      R.SwapArguments;
    end;
  end
  else if (T1 = typeCLASS) and (Arg2 = SymbolTable.NilId) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    R.Op := OP_NE_INT_MI;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typeCLASS) then
  begin
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_NE_INT_MI;
    R.SwapArguments;
  end
  else if (T1 = typeCLASS) and (T2 = typeCLASS) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_NE_INT_MM;
  end
  else if (T1 in [typePOINTER, typeCLASS, typeCLASSREF, typePROC, typeINTERFACE]
    ) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_NE_INT_MI;
  end
  else if (Arg1 = SymbolTable.NilId) and
    (T2 in [typePOINTER, typeCLASS, typeCLASSREF, typePROC, typeINTERFACE]) then
  begin
    R.Op := OP_NE_INT_MI;
    R.SwapArguments;
  end
  else if (T1 = typeINTERFACE) and (T2 = typeINTERFACE) then
  begin
    R.Op := OP_NE_INT_MM;
  end
  else if (T1 = typePOINTER) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_NE_INT_MI;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typePOINTER) then
  begin
    R.Op := OP_NE_INT_MI;
    R.SwapArguments;
  end
  else if (T1 = typePOINTER) and (T2 = typePOINTER) then
  begin
{$IFNDEF PAXARM}
    if (GetSymbolRec(Arg1).HasPAnsiCharType or GetSymbolRec(Arg1)
      .HasPWideCharType) and (GetSymbolRec(Arg2).HasPAnsiCharType or
      GetSymbolRec(Arg2).HasPWideCharType) then
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_NE_WIDESTRING;
      Exit;
    end;
{$ENDIF}
    R.Op := OP_NE_INT_MM;
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASSREF) then
  begin
    R.Op := OP_NE_INT_MM;
  end
  else if (T1 = typeCLASS) and (T2 = typeCLASSREF) then
  begin
    if K1 = KindTYPE then
      Inc(R.Arg1);
    R.Op := OP_NE_INT_MM;
  end
  else if (T1 = typeCLASSREF) and (T2 = typeCLASS) then
  begin
    if K2 = KindTYPE then
      Inc(R.Arg2);
    R.Op := OP_NE_INT_MM;
  end

  else if (T1 = typeCURRENCY) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K2 = KindCONST then
    begin
      R.Op := OP_NE_CURRENCY;
      if T2 <> typeCURRENCY then
        R.Arg2 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg2).value);
    end
    else
      R.Op := OP_NE_CURRENCY;
  end
  else if (T1 in NumberTypes) and (T2 = typeCURRENCY) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if K1 = KindCONST then
    begin
      R.Op := OP_NE_CURRENCY;
      if T1 <> typeCURRENCY then
        R.Arg1 := CreateConst(typeCURRENCY, GetSymbolRec(R.Arg1).value);
    end
    else
      R.Op := OP_NE_CURRENCY;
  end

  else if (T1 = typeDOUBLE) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_DOUBLE;
  end
  else if (T1 = typeDOUBLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_DOUBLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToDouble(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeDOUBLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_NE_DOUBLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeDOUBLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToDouble(N, 1);
      Inc(N);
    end;
  end

  else if (T1 = typeSINGLE) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_SINGLE;
  end
  else if (T1 = typeSINGLE) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_SINGLE;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToSingle(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeSINGLE) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_NE_SINGLE;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeSINGLE, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToSingle(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeEXTENDED) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_EXTENDED;
  end
  else if (T1 = typeEXTENDED) and (T2 in NumberTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;

    R.Op := OP_NE_EXTENDED;

    if K2 = KindCONST then
      R.Arg2 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg2).value)
    else
    begin
      InsertConversionToExtended(N, 2);
      Inc(N);
    end;
  end
  else if (T1 in NumberTypes) and (T2 = typeEXTENDED) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
      Exit;
    end;
    R.Op := OP_NE_EXTENDED;
    if K1 = KindCONST then
      R.Arg1 := CreateConst(typeEXTENDED, GetSymbolRec(R.Arg1).value)
    else
    begin
      InsertConversionToExtended(N, 1);
      Inc(N);
    end;
  end
  //
  else if (T1 = typeSET) and (T2 = typeSET) then
  begin
    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    if not MatchSetTypes(T1, T2) then
      CreateError(errIncompatibleTypesNoArgs, [])
    else if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).ValueAsByteSet <>
        GetSymbolRec(Arg2).ValueAsByteSet;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else if (K1 in [KindVAR, KindCONST]) and (K2 in [KindVAR, KindCONST]) then
      R.Op := OP_SET_INEQUALITY
    else
      CreateError(errIncompatibleTypesNoArgs, [])
  end
{$IFNDEF PAXARM}
  else if (T1 = typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    R.Op := OP_NE_ANSISTRING;
  end
  else if (T1 = typeANSISTRING) and (T2 <> typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 2);
    Inc(N);
    R.Op := OP_NE_ANSISTRING;
  end
  else if (T1 <> typeANSISTRING) and (T2 = typeANSISTRING) then
  begin
    InsertConversionToAnsiString(N, 1);
    Inc(N);
    R.Op := OP_NE_ANSISTRING;
  end

  else if (T1 = typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    R.Op := OP_NE_SHORTSTRING;
  end
  else if (T1 = typeSHORTSTRING) and (T2 <> typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 2);
    Inc(N);
    R.Op := OP_NE_SHORTSTRING;
  end
  else if (T1 <> typeSHORTSTRING) and (T2 = typeSHORTSTRING) then
  begin
    InsertConversionToShortString(N, 1);
    Inc(N);
    R.Op := OP_NE_SHORTSTRING;
  end

  // wide string

  else if (T1 = typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    R.Op := OP_NE_WIDESTRING;
  end
  else if (T1 = typeWIDESTRING) and (T2 <> typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 2);
    Inc(N);
    R.Op := OP_NE_WIDESTRING;
  end
  else if (T1 <> typeWIDESTRING) and (T2 = typeWIDESTRING) then
  begin
    InsertConversionToWideString(N, 1);
    Inc(N);
    R.Op := OP_NE_WIDESTRING;
  end
{$ENDIF}
  // unic string

  else if (T1 = typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    R.Op := OP_NE_UNICSTRING;
  end
  else if (T1 = typeUNICSTRING) and (T2 <> typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 2);
    Inc(N);
    R.Op := OP_NE_UNICSTRING;
  end
  else if (T1 <> typeUNICSTRING) and (T2 = typeUNICSTRING) then
  begin
    InsertConversionToUnicString(N, 1);
    Inc(N);
    R.Op := OP_NE_UNICSTRING;
  end

  // variant

  else if (T1 = typeVARIANT) and (T2 = typeVARIANT) then
  begin
    R.Op := OP_NE_VARIANT;
  end
  else if (T1 = typeVARIANT) and (T2 <> typeVARIANT) then
  begin
    InsertConversionToVariant(N, 2);
    Inc(N);
    R.Op := OP_NE_VARIANT;
  end
  else if (T1 <> typeVARIANT) and (T2 = typeVARIANT) then
  begin
    InsertConversionToVariant(N, 1);
    Inc(N);
    R.Op := OP_NE_VARIANT;
  end
  else if (T1 = typeEVENT) and (T2 = typeEVENT) then
  begin
    R.Op := OP_NE_EVENT;
  end
  else if (T1 = typeEVENT) and (Arg2 = SymbolTable.NilId) then
  begin
    R.Op := OP_NE_EVENT;
    R.Arg2 := SymbolTable.EventNilId;
  end
  else if (Arg1 = SymbolTable.NilId) and (T2 = typeEVENT) then
  begin
    R.Op := OP_NE_EVENT;
    R.Arg1 := SymbolTable.EventNilId;
  end
  //
{$IFNDEF PAXARM}
  else if GetSymbolRec(Arg1).HasPAnsiCharType and GetSymbolRec(Arg2).HasPAnsiCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_NE_ANSISTRING;
    end;
  end
  else if GetSymbolRec(Arg1).HasPAnsiCharType and (T2 = typeANSICHAR) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_NE_ANSISTRING;
    end;
  end
  else if (T1 = typeANSICHAR) and GetSymbolRec(Arg2).HasPAnsiCharType then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToAnsiString(N, 1);
      Inc(N);
      InsertConversionToAnsiString(N, 2);
      Inc(N);
      R.Op := OP_NE_ANSISTRING;
    end;
  end
  //
{$ENDIF}
  else if GetSymbolRec(Arg1).HasPWideCharType and GetSymbolRec(Arg2).HasPWideCharType
  then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_NE_UNICSTRING;
    end;
  end
  else if (GetSymbolRec(Arg1).HasPWideCharType
{$IFNDEF PAXARM}
    or GetSymbolRec(Arg1).HasPAnsiCharType
{$ENDIF}
    ) and (T2 in CharTypes) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := GetSymbolRec(Arg1).value <>
        Chr(Integer(GetSymbolRec(Arg2).value));
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_NE_UNICSTRING;
    end;
  end
  else if (T1 in CharTypes) and (GetSymbolRec(Arg2).HasPWideCharType
{$IFNDEF PAXARM}
    or GetSymbolRec(Arg2).HasPAnsiCharType
{$ENDIF}
    ) then
  begin
    if (K1 = KindCONST) and (K2 = KindCONST) then
    begin
      GetSymbolRec(R.Res).value := Chr(Integer(GetSymbolRec(Arg1).value)) <>
        GetSymbolRec(Arg2).value;
      GetSymbolRec(R.Res).Kind := KindCONST;
      R.Op := OP_NOP;
    end
    else
    begin
      InsertConversionToUnicString(N, 1);
      Inc(N);
      InsertConversionToUnicString(N, 2);
      Inc(N);
      R.Op := OP_NE_UNICSTRING;
    end;
  end
  else if (T1 = typePOINTER) and (T2 = typePOINTER) then
  begin
    R.Op := OP_NE_INT_MM;
  end
  //
  else if (T1 = typeARRAY) and (T2 = typeARRAY) and
    (GetSymbolRec(Arg1).TypeId = GetSymbolRec(Arg2).TypeId) then
  begin
    R.Op := OP_NE_STRUCT;
  end
  else if (T1 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg1).TerminalTypeId, gen_NotEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg1).TerminalTypeId, gen_NotEqual);
    Inc(N);
  end
  else if (T2 = typeRECORD) and ExistsBinaryOperator
    (GetSymbolRec(Arg2).TerminalTypeId, gen_NotEqual) then
  begin
    InsertBinaryOperator(GetSymbolRec(Arg2).TerminalTypeId, gen_NotEqual);
    Inc(N);
  end
  else
  begin
    if GetLanguage(N) = JS_LANGUAGE then
    begin
      InsertConversionToVariant(N, 1);
      Inc(N);
      InsertConversionToVariant(N, 2);
      Inc(N);
      R.Op := OP_NE_VARIANT;
      Exit;
    end;

    T1 := GetSymbolRec(Arg1).TypeId;
    T2 := GetSymbolRec(Arg2).TypeId;
    CreateError(errIncompatibleTypes, [GetSymbolRec(T1).Name,
      GetSymbolRec(T2).Name]);
  end;
end;

procedure TCode.OperRaise;
var
  R: TCodeRec;
begin
  R := Records[N];
  if R.Arg1 > 0 then
    if GetSymbolRec(R.Arg1).FinalTypeId <> typeCLASS then
      CreateError(errClassTypeExpected, []);
end;

procedure TCode.OperTypeInfo;
var
  R: TCodeRec;
  S: String;
begin
  R := Records[N];
  if GetSymbolRec(R.Arg1).Kind <> KindTYPE then
    CreateError(errIncompatibleTypesNoArgs, []);
  S := GetSymbolRec(R.Arg1).FullName;
  R.Arg2 := CreateConst(typeSTRING, S);
end;

procedure TCode.OperAddTypeInfo;
var
  R: TCodeRec;
  SourceTypeId: Integer;
begin
  R := Records[N];
  if GetSymbolRec(R.Arg1).Kind <> KindTYPE then
  begin
    CreateError(errIncompatibleTypesNoArgs, []);
  end;

  if GetSymbolRec(R.Arg1).Kind = KindTYPE then
    SourceTypeId := R.Arg1
  else
    SourceTypeId := GetSymbolRec(R.Arg1).TypeId;

  AddTypeInfo(GetSymbolRec(R.Arg1).TerminalTypeId, SourceTypeId);
end;

procedure TCode.OperIs;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  GetSymbolRec(R.Res).TypeId := typeBOOLEAN;

  if (T1 = typeCLASS) and (T2 = typeCLASS) and (K1 = KindVAR) and (K2 = KindTYPE)
  then
  begin
    // ok
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperAs;
var
  Arg1, Arg2, T1, T2, K1, K2: Integer;
  R: TCodeRec;
begin
  R := Records[N];

  Arg1 := R.Arg1;
  Arg2 := R.Arg2;
  T1 := GetSymbolRec(Arg1).FinalTypeId;
  T2 := GetSymbolRec(Arg2).FinalTypeId;

  if TKernel(kernel).InterfaceOnly then
    if (T1 = 0) or (T2 = 0) then
    begin
      DiscardImport;
      Exit;
    end;

  K1 := GetSymbolRec(Arg1).Kind;
  K2 := GetSymbolRec(Arg2).Kind;

  map_list.Add(Arg2);

  if (T1 = typeCLASS) and (T2 = typeCLASS) and (K1 = KindVAR) and (K2 = KindTYPE)
  then
  begin
    R.Op := OP_ASSIGN_INT_M;
    R.Arg1 := R.Res;
    R.Arg2 := Arg1;
    GetSymbolRec(R.Res).TypeId := Arg2;
  end
  else if (T1 = typeINTERFACE) and (T2 = typeINTERFACE) and (K1 = KindVAR) and
    (K2 = KindTYPE) then
  begin
    R.Op := OP_INTERFACE_CAST;
    GetSymbolRec(R.Res).TypeId := Arg2;
  end
  else if (T1 = typeCLASS) and (T2 = typeINTERFACE) and (K1 = KindVAR) and
    (K2 = KindTYPE) then
  begin
    R.Op := OP_INTERFACE_FROM_CLASS;
    R.Arg1 := R.Res;
    R.Arg2 := Arg1;
    GetSymbolRec(R.Res).TypeId := Arg2;
  end
  else
    CreateError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.OperGoTrue;
var
  Arg2, T2: Integer;
  V: Variant;
begin
  Arg2 := Records[N].Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;
  if not(T2 in BooleanTypes) then
  begin
    if Records[N].Language = JS_LANGUAGE then
    begin
      if GetSymbolRec(Arg2).Kind = KindCONST then
      begin
        if T2 in IntegerTypes then
        begin
          V := GetSymbolRec(Arg2).value;
          Records[N].Arg2 := CreateConst(typeBOOLEAN, V = 0);
          Exit;
        end
{$IFNDEF PAXARM}
        else if GetSymbolRec(Arg2).HasPAnsiCharType then
        begin
          V := GetSymbolRec(Arg2).value;
          Records[N].Arg2 := CreateConst(typeBOOLEAN, V = '');
          Exit;
        end;
{$ENDIF}
      end
      else
      begin
        InsertConversionToBoolean(N, 2);
        Inc(N);
        Exit;
      end;
    end
    else if T2 in VariantTypes then
    begin
      InsertConversionToBoolean(N, 2);
      Inc(N);
      Exit;
    end;

    CreateError(errTypeOfExpressionMustBe, ['BOOLEAN']);
  end;
end;

procedure TCode.OperGoFalse;
var
  Arg2, T2: Integer;
  V: Variant;
begin
  Arg2 := Records[N].Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;
  if not(T2 in BooleanTypes) then
  begin
    if Records[N].Language = JS_LANGUAGE then
    begin
      if GetSymbolRec(Arg2).Kind = KindCONST then
      begin
        if T2 in IntegerTypes then
        begin
          V := GetSymbolRec(Arg2).value;
          Records[N].Arg2 := CreateConst(typeBOOLEAN, V <> 0);
          Exit;
        end
{$IFNDEF PAXARM}
        else if GetSymbolRec(Arg2).HasPAnsiCharType then
        begin
          V := GetSymbolRec(Arg2).value;
          Records[N].Arg2 := CreateConst(typeBOOLEAN, V <> '');
          Exit;
        end
{$ENDIF}
        else if GetSymbolRec(Arg2).HasPWideCharType then
        begin
          V := GetSymbolRec(Arg2).value;
          Records[N].Arg2 := CreateConst(typeBOOLEAN, V <> '');
          Exit;
        end;
      end
      else
      begin
        InsertConversionToBoolean(N, 2);
        Inc(N);
        Exit;
      end;
    end
    else if T2 in VariantTypes then
    begin
      InsertConversionToBoolean(N, 2);
      Inc(N);
      Exit;
    end;

    CreateError(errTypeOfExpressionMustBe, ['BOOLEAN']);
  end;
end;

procedure TCode.OperGoTrueBool;
var
  Arg2, T2: Integer;
begin
  Arg2 := Records[N].Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;
  if T2 in (BooleanTypes + VariantTypes) then
  begin
    Records[N].Op := OP_GO_TRUE;
    Dec(N);
  end
  else
    Records[N].Op := OP_NOP;
end;

procedure TCode.OperGoFalseBool;
var
  Arg2, T2: Integer;
begin
  Arg2 := Records[N].Arg2;
  T2 := GetSymbolRec(Arg2).FinalTypeId;
  if T2 in (BooleanTypes + VariantTypes) then
  begin
    Records[N].Op := OP_GO_FALSE;
    Dec(N);
  end
  else
    Records[N].Op := OP_NOP;
end;

procedure TCode.RemoveEvalOpForTypes;

var
  using_stack: TIntegerStack;
  EndOfImport: Boolean;

  function CheckNamespace(Arg1: Integer): Boolean;
  var
    I, Id: Integer;
    S, Q: String;
  begin
    if not EndOfImport then
    begin
      result := true;
      Exit;
    end;

    result := using_stack.IndexOf(Arg1) >= 0;
    if result then
      Exit;

    S := UpperCase(GetSymbolRec(Arg1).FullName);
    for I := 0 to using_stack.Count - 1 do
    begin
      Id := using_stack[I];
      if Id = 0 then
        continue;
      Q := UpperCase(GetSymbolRec(Id).FullName);
      result := pos(S, Q) = SLow(S);
      if result then
        Exit;
    end;
  end;

var
  Id, Op, Arg1, Arg2, K, K1: Integer;
  sub_stack: TIntegerStack;
  SymbolTable: TSymbolTable;
  Upcase: Boolean;
  S: String;
  RN: TCodeRec;
  WithCount: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  using_stack := TIntegerStack.Create;
  sub_stack := TIntegerStack.Create;
  WithCount := 0;
  EndOfImport := false;

  try
    N := 0;
    while N < Card do
    begin
      Inc(N);

      RN := Records[N];
      Op := RN.Op;

      if Op = OP_BEGIN_USING then
        using_stack.Push(RN.Arg1)
      else if Op = OP_END_USING then
        using_stack.Pop
      else if Op = OP_BEGIN_WITH then
        Inc(WithCount)
      else if Op = OP_END_WITH then
        Dec(WithCount)
      else if Op = OP_END_IMPORT then
        EndOfImport := true
      else if Op = OP_BEGIN_MODULE then
        EndOfImport := false
        {
          else if Op = OP_BEGIN_MODULE then
          using_stack.Clear
        }
      else if Op = OP_END_MODULE then
        using_stack.Clear

      else if Op = OP_BEGIN_SUB then
        sub_stack.Push(RN.Arg1)
      else if Op = OP_END_SUB then
        sub_stack.Pop

      else if Op = OP_EVAL then
      begin
        if RN.Res <= Types.Count then
        begin
          RN.Op := OP_NOP;
          continue;
        end;

        S := GetSymbolRec(RN.Res).Name;

        if S = DummyName then
        begin
          RN.Op := OP_NOP;
          continue;
        end;

        if S = '' then
        begin
          RN.Op := OP_NOP;
          GetSymbolRec(RN.Res).Kind := KindNONE;
          continue;
        end;

        if WithCount > 0 then
          continue;

        Upcase := GetUpcase(N);

        // try to find in the sub list

        K := GetSymbolRec(RN.Res).Kind;
        GetSymbolRec(RN.Res).Kind := KindNONE;

        Id := SymbolTable.LookUps(S, sub_stack, Upcase);

        if Id > 0 then
          if GetSymbolRec(Id).Kind = KindTYPE then
          begin
            GetSymbolRec(RN.Res).Name := '';
            ReplaceId(RN.Res, Id);
            RN.Op := OP_NOP;
            continue;
          end;

        // try to find in the using list
        Id := SymbolTable.LookUps(S, using_stack, Upcase);
        if Id = 0 then
          Id := SymbolTable.LookUp(S, 0, Upcase);

        if Id > 0 then
          if GetSymbolRec(Id).Kind in [KindTYPE, KindNAMESPACE] then
          begin
            if GetSymbolRec(Id).FinalTypeId in [typeCLASS, typeRECORD] then
              continue;

            if Id > RN.Res then
              if GetLanguage(N) in [PASCAL_LANGUAGE] then
                if TKernel(kernel).Modules.IndexOfModuleById(RN.Res)
                  = TKernel(kernel).Modules.IndexOfModuleById(Id) then
                begin
                  if TKernel(kernel).Modules.IsDefinedInPCU(Id) then
                  begin
                    // ok
                  end
                  else if GetSymbolRec(Id).Host then
                  begin
                    // ok
                  end
                  else
                    CreateError(errUndeclaredIdentifier, [S]);
                end;

            GetSymbolRec(RN.Res).Name := '';
            ReplaceId(RN.Res, Id);
            RN.Op := OP_NOP;

            continue;
          end;

        GetSymbolRec(RN.Res).Kind := K;
      end
      else if Op = OP_FIELD then
      begin
        Arg1 := RN.Arg1;
        Arg2 := RN.Arg2;
        K1 := GetSymbolRec(Arg1).Kind;

        if K1 = KindNAMESPACE then
        begin
          if GetLanguage(N) = PASCAL_LANGUAGE then
            if not CheckNamespace(Arg1) then
            begin
              S := GetSymbolRec(Arg1).Name;
              CreateError(errUndeclaredIdentifier, [S]);
              TKernel(kernel).SymbolTable.AddUndeclaredIdent(S,
                TKernel(kernel).UndeclaredIdents, TKernel(kernel).Errors.Count
                - 1, true);
            end;

          Id := SymbolTable.LookUp(GetSymbolRec(Arg2).Name, Arg1, GetUpcase(N));
          if Id <> 0 then
            if GetSymbolRec(Id).Kind in [KindTYPE, KindNAMESPACE] then
            begin
              ReplaceId(Arg2, Id);
              GetSymbolRec(Arg2).Name := '';
              GetSymbolRec(Arg2).Kind := KindNONE;
              RN.Op := OP_NOP;

            end;
        end;
      end

      // set types
      else if Op = OP_ASSIGN_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
          continue;

        GetSymbolRec(RN.Arg1).TypeId := RN.Arg2;

        RN.Op := OP_NOP;
      end
      else if Op = OP_ASSIGN_THE_SAME_TYPE then
      begin
        GetSymbolRec(RN.Arg1).TypeId := GetSymbolRec(RN.Arg2).TypeId;
        RN.Op := OP_NOP;
      end
      else if Op = OP_ASSIGN_TYPE_ALIAS then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end

      else if Op = OP_CREATE_POINTER_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_CREATE_CLASSREF_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_BEGIN_HELPER_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;
        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        if SymbolTable.TypeHelpers.Keys.IndexOf(RN.Arg1) = -1 then
          SymbolTable.TypeHelpers.Add(RN.Arg1, RN.Arg2);
      end
      else if Op = OP_CALL then
      begin
        if GetSymbolRec(RN.Arg1).Kind = KindTYPE then
          if Records[N - 1].Op = OP_PUSH then
            if Records[N - 1].Arg1 = SymbolTable.NilId then
              if Records[N + 1].Op = OP_FIELD then
                if Records[N + 1].Arg1 = RN.Res then
                begin
                  RN.Op := OP_NOP;
                  RN.GenOp := OP_NOP;
                  Records[N - 1].Op := OP_NOP;
                  Records[N - 1].GenOp := OP_NOP;
                  Records[N + 1].Arg1 := RN.Arg1;
                  GetSymbolRec(Records[N + 1].Arg2).OwnerId := RN.Arg1;
                end;
      end
      else if Op = OP_CREATE_DYNAMIC_ARRAY_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_ADD_ANCESTOR then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          continue;
        end;

        GetSymbolRec(RN.Arg1).AncestorId := RN.Arg2;

        // RN.Op := OP_NOP;
      end
      else if Op = OP_ADD_INTERFACE then
      begin
        if RN.Arg2 = 0 then
          RN.Arg2 := SymbolTable.LookupAnonymousInterface(RN.Arg1);
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          if TKernel(kernel).InterfaceOnly then
            RN.Op := OP_NOP;
          continue;
        end;

        if GetSymbolRec(RN.Arg2).FinalTypeId <> typeINTERFACE then
          CreateError(errIncompatibleTypesNoArgs, []);

        SymbolTable.RegisterSupportedInterface(RN.Arg1, RN.Arg2);
        RN.Op := OP_NOP;
      end;
    end; // while

  finally
    FreeAndNil(using_stack);
    FreeAndNil(sub_stack);
  end;
end;

procedure TCode.ProcessImplements;

var
  PropId, IntfTypeId, ClassTypeId, LanguageId, CurrModule: Integer;
  Upcase: Boolean;
  SymbolTable: TSymbolTable;
  L: Integer;

  procedure Gen(Op, Arg1, Arg2, Res: Integer);
  begin
    Add(Op, Arg1, Arg2, Res, L, Upcase, LanguageId, CurrModule, 0);
  end;

  procedure AddMethod(IntfSubId: Integer);
  var
    I, SubId, LabelId, ResId, SelfId, ParamId, IntfParamId, IntfResTypeId,
      TempPropId, NameId, TempId, NP: Integer;
    IsFunction: Boolean;
  begin
    IntfResTypeId := SymbolTable[IntfSubId].TypeId;
    IsFunction := (IntfResTypeId <> 0) and (IntfResTypeId <> typeVOID);
    NP := GetSymbolRec(IntfSubId).Count;

    LabelId := SymbolTable.AddLabel.Id;

    SubId := NewTempVar(ClassTypeId, IntfResTypeId);

    L := SubId;

    GetSymbolRec(SubId).Name := GetSymbolRec(IntfSubId).Name;
    GetSymbolRec(SubId).Kind := KindSUB;
    GetSymbolRec(SubId).CallConv := GetSymbolRec(IntfSubId).CallConv;
    GetSymbolRec(SubId).Count := GetSymbolRec(IntfSubId).Count;

    ResId := NewTempVar(SubId, IntfResTypeId);
    if not IsFunction then
      GetSymbolRec(ResId).Kind := KindNONE;

    SelfId := NewTempVar(SubId, ClassTypeId);
    GetSymbolRec(SelfId).Param := true;

    for I := 0 to NP - 1 do
    begin
      IntfParamId := SymbolTable.GetParamId(IntfSubId, I);
      ParamId := NewTempVar(SubId, GetSymbolRec(IntfParamId).TypeId);
      GetSymbolRec(ParamId).Param := true;
      GetSymbolRec(ParamId).Name := GetSymbolRec(IntfParamId).Name;
      GetSymbolRec(ParamId).ByRef := GetSymbolRec(IntfParamId).ByRef;
      GetSymbolRec(ParamId).IsConst := GetSymbolRec(IntfParamId).IsConst;
      GetSymbolRec(ParamId).Optional := GetSymbolRec(IntfParamId).Optional;
      GetSymbolRec(ParamId).value := GetSymbolRec(IntfParamId).value;
    end;

    TempPropId := NewTempVar(SubId, GetSymbolRec(PropId).TypeId);
    GetSymbolRec(TempPropId).OwnerId := SelfId;
    GetSymbolRec(TempPropId).Name := GetSymbolRec(PropId).Name;

    NameId := NewTempVar(SubId, GetSymbolRec(SubId).TypeId);
    GetSymbolRec(NameId).OwnerId := TempPropId;
    GetSymbolRec(NameId).Name := GetSymbolRec(SubId).Name;

    Gen(OP_GO, LabelId, 0, 0);
    Gen(OP_BEGIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, SubId, 0, 0);
    Gen(OP_INIT_SUB, SubId, 0, 0);
    // reserved for prologue
    Gen(OP_NOP, 0, 0, 0);

    if IsFunction then
    begin
      TempId := NewTempVar(SubId, GetSymbolRec(ResId).TypeId);

      Gen(OP_LVALUE, ResId, 0, 0);
      Gen(OP_FIELD, SelfId, TempPropId, TempPropId);
      Gen(OP_FIELD, TempPropId, NameId, NameId);
      for I := 0 to NP - 1 do
        Gen(OP_PUSH, SymbolTable.GetParamId(SubId, I), I, NameId);
      Gen(OP_CALL, NameId, NP, TempId);
      Gen(OP_ASSIGN, ResId, TempId, ResId);
    end
    else
    begin
      Gen(OP_FIELD, SelfId, TempPropId, TempPropId);
      Gen(OP_FIELD, TempPropId, NameId, NameId);
      for I := 0 to NP - 1 do
        Gen(OP_PUSH, SymbolTable.GetParamId(SubId, I), I, NameId);
      Gen(OP_CALL, NameId, NP, 0);
    end;

    Gen(OP_EPILOGUE_SUB, SubId, 0, 0);
    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);

    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_END_SUB, SubId, 0, 0);
    Gen(OP_FIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, LabelId, 0, 0);
  end;

var
  I, J: Integer;
  SI: TSymbolRec;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  for I := 1 to Card do
    if Records[I].Op = OP_IMPLEMENTS then
    begin
      N := I;

      PropId := Records[N].Arg1;
      IntfTypeId := Records[N].Arg2;

      ClassTypeId := GetSymbolRec(PropId).Level;

      if GetSymbolRec(IntfTypeId).FinalTypeId <> typeINTERFACE then
        RaiseError(errInternalError, []);

      LanguageId := GetLanguage(N);
      Upcase := GetUpcase(N);
      CurrModule := GetModuleNumber(N);

      for J := IntfTypeId + 1 to SymbolTable.Card do
      begin
        SI := SymbolTable[J];
        if SI.Kind = KindSUB then
          if SI.Level = IntfTypeId then
          begin
            AddMethod(J);
          end;

        if SI = SymbolTable.SR0 then
          break;
      end;
    end;
end;

function TCode.IsExplicitOff: Boolean;
var
  I: Integer;
  R: TCodeRec;
begin
  result := false;
  for I := N downto 1 do
  begin
    R := Records[I];
    if R.Op = OP_BEGIN_MODULE then
      Exit;
    if R.Op = OP_OPTION_EXPLICIT then
      if R.Arg1 = 0 then
      begin
        result := true;
        Exit;
      end;
  end;
end;

function TCode.EvalFrameworkType(const S: String): Integer;
begin
  result := 0;
  if S = 'Object' then
    result := H_TFW_Object
  else if S = 'String' then
  begin
{$IFDEF UNIC}
    result := H_TFW_UnicString;
{$ELSE}
    result := H_TFW_AnsiString;
{$ENDIF}
  end
  else if S = 'Integer' then
    result := H_TFW_Integer
  else if S = 'Byte' then
    result := H_TFW_Byte
  else if S = 'Short' then
    result := H_TFW_ShortInt
  else if S = 'Long' then
    result := H_TFW_Int64
  else if S = 'Float' then
    result := H_TFW_Single
  else if S = 'Double' then
    result := H_TFW_Double
  else if S = 'Boolean' then
    result := H_TFW_Boolean
  else if S = 'Char' then
  begin
{$IFDEF UNIC}
    result := H_TFW_AnsiChar;
{$ELSE}
    result := H_TFW_WideChar;
{$ENDIF}
  end;
end;

procedure TCode.CreateEvalError(const VarName: String;
  using_stack: TIntegerStack);
var
  I, Id, L, TypeId, K1, K2: Integer;
  S, Scope, TypeName: String;
  SymbolTable: TSymbolTable;
  Upcase, ok: Boolean;
begin
  if GetLanguage(N) in [JAVA_LANGUAGE] then
  begin
    Id := EvalFrameworkType(VarName);
    if Id <> 0 then
    begin
      Records[N].Op := OP_NOP;
      ReplaceId(Records[N].Res, Id);
      Exit;
    end;
  end;

  SymbolTable := TKernel(kernel).SymbolTable;
  Upcase := GetUpcase(N);
  Id := Records[N].Res;
  Records[N].Op := OP_NOP;
  if Assigned(TKernel(kernel).OnUndeclaredIdentifier) then
  begin
    L := GetSymbolRec(Id).Level;
    if L = 0 then
      Scope := ''
    else
      Scope := GetSymbolRec(L).FullName;

    S := Scope;
    TypeName := '';

    K1 := TKernel(kernel).SymbolTable.Card;
    if TKernel(kernel).OnUndeclaredIdentifier(TKernel(kernel).Owner, VarName,
      Scope, TypeName) then
      if TypeName <> '' then
      begin
        K2 := TKernel(kernel).SymbolTable.Card;
        for I := K1 + 1 to K2 do
        begin

          if Upcase then
            ok := StrEql(GetSymbolRec(I).Name, VarName)
          else
            ok := GetSymbolRec(I).Name = VarName;

          if ok then
          begin
            ReplaceId(Id, I);
            Exit;
          end;
        end;

        if S <> Scope then
        begin
          if Scope = '' then
            L := 0
          else
          begin
            L := SymbolTable.LookupFullName(Scope, GetUpcase(N));
            if L = 0 then
              CreateError(errUndeclaredIdentifier, [Scope]);
          end;
        end;

        TypeId := SymbolTable.LookupFullName(TypeName, GetUpcase(N));
        if TypeId = 0 then
        begin
          CreateError(errUndeclaredIdentifier, [TypeName]);
          SymbolTable.AddUndeclaredIdent(TypeName,
            TKernel(kernel).UndeclaredIdents, TKernel(kernel).Errors.Count
            - 1, true);
        end
        else if GetSymbolRec(TypeId).Kind = KindTYPE then
        begin
          GetSymbolRec(Id).Kind := KindVAR;
          GetSymbolRec(Id).TypeId := TypeId;
          GetSymbolRec(Id).Level := L;
        end;

        Exit;
      end;
  end;

  I := N;
  S := GetSymbolRec(Records[I].Res).Name;
  while (Records[I + 1].Op = OP_FIELD) and
    (Records[I + 1].Arg1 = Records[I].Res) do
  begin
    S := S + '.' + GetSymbolRec(Records[I + 1].Res).Name;
    Id := SymbolTable.LookUps(S, using_stack, Upcase);
    if Id > 0 then
    begin
      Inc(I);
      ReplaceId(Records[I].Res, Id);
      while I > N do
      begin
        Records[I].Op := OP_NOP;
        if Records[I].Arg1 > 0 then
          if GetSymbolRec(Records[I].Arg1).Kind = KindNONE then
            GetSymbolRec(Records[I].Arg1).Name := '';

        Dec(I);
      end;

      Exit;
    end;

    Inc(I);
  end;

  if IsExplicitOff then
  begin
    GetSymbolRec(Id).Kind := KindVAR;
    GetSymbolRec(Id).Level := GetLevel(N);
    Exit;
  end;

  if TKernel(kernel).Errors.Count > 10 then
    RaiseError(errUndeclaredIdentifier, [VarName])
  else
  begin
    CreateError(errUndeclaredIdentifier, [VarName]);
    SymbolTable.AddUndeclaredIdent(VarName, TKernel(kernel).UndeclaredIdents,
      TKernel(kernel).Errors.Count - 1, true);
  end;
end;

procedure TCode.RemoveEvalOp;
var
  SubId, ParamId, I, J, Id, Op, Arg1, Arg2, K, K1, TypeId,
    PatternFieldId: Integer;
  using_stack, with_stack, sub_stack, block_stack: TIntegerStack;
  SymbolTable: TSymbolTable;
  Upcase: Boolean;
  S: String;
  Modules: TModuleList;
  I1, I2: Integer;
  B1, B2: Boolean;
  UpperBoundId: Integer;
  NS_ID: Integer;
  RC: TCodeRec;
  RJ, RN: TCodeRec;
  eval_helper_name: TStringList;
  eval_helper_id, eval_helper_PatternFieldId: TIntegerList;
  temp_id: Integer;
  IsExport: Boolean;
label
  Next, NextTime;
begin
  IsExport := false;

  SymbolTable := TKernel(kernel).SymbolTable;
  Modules := TKernel(kernel).Modules;

  using_stack := TIntegerStack.Create;
  block_stack := TIntegerStack.Create;
  with_stack := TIntegerStack.Create;
  sub_stack := TIntegerStack.Create;
  eval_helper_name := TStringList.Create;
  eval_helper_id := TIntegerList.Create;
  eval_helper_PatternFieldId := TIntegerList.Create;

  try
    N := 1;
    while N <= Card do
    begin

    NextTime:

      RN := Records[N];
      Op := RN.Op;

      if Op = OP_NOP then
        goto Next
      else if Op = OP_BEGIN_USING then
        using_stack.Push(RN.Arg1)
      else if Op = OP_END_USING then
        using_stack.Pop
        {
          else if Op = OP_BEGIN_MODULE then
          using_stack.Clear
        }
      else if Op = OP_BEGIN_EXPORT then
        IsExport := true
      else if Op = OP_END_MODULE then
      begin
        IsExport := false;
        using_stack.Clear;
      end

      else if Op = OP_BEGIN_BLOCK then
        block_stack.Push(RN.Arg1)
      else if Op = OP_END_BLOCK then
        block_stack.Pop

      else if Op = OP_BEGIN_WITH then
      begin
        // RN.Op := OP_NOP;
        eval_helper_name.Clear;
        eval_helper_id.Clear;
        eval_helper_PatternFieldId.Clear;
        with_stack.Push(RN.Arg1);
        if not SymbolTable[with_stack.Top].FinalTypeId in [typeRECORD, typeCLASS]
        then
        begin
          CreateError(errRecordRequired, []);
          break;
        end;
      end
      else if Op = OP_END_WITH then
      begin
        eval_helper_name.Clear;
        eval_helper_id.Clear;
        eval_helper_PatternFieldId.Clear;
        with_stack.Pop;
        // RN.Op := OP_NOP;
      end
      else if Op = OP_BEGIN_SUB then
      begin
        sub_stack.Push(RN.Arg1);

        K := GetSymbolRec(RN.Arg1).Level;
        if K > 0 then
          if GetSymbolRec(K).FinalTypeId = typeHELPER then
          begin
            TypeId := GetSymbolRec(K).PatternId;
            Id := SymbolTable.GetSelfId(RN.Arg1);
            if TypeId = 0 then
              RaiseError(errInternalError, []);
            GetSymbolRec(Id).TypeId := TypeId;
          end;
      end
      else if Op = OP_END_SUB then
        sub_stack.Pop

      else if Op = OP_EVAL_OUTER then
      begin
        I1 := GetCurrSelfId(N);
        I2 := SymbolTable.GetOuterThisId(RN.Arg1);

        if (I1 = 0) or (I2 = 0) then
        begin
          RN.Op := OP_NOP;
          Records[N + 1].Op := OP_NOP;
          Records[N + 2].Op := OP_NOP;
        end
        else
        begin

          if GetSymbolRec(I1).TerminalTypeId = GetSymbolRec(I2).TerminalTypeId
          then
          begin
            RN.Op := OP_NOP;
            ReplaceId(RN.Res, I1);
          end
          else
          begin
            RN.Op := OP_FIELD;
            RN.Arg1 := I1;
            RN.Arg2 := NewField(StrOuterThis, GetSymbolRec(I2).TypeId, I1);
            ReplaceId(RN.Res, RN.Arg2);
            RN.Res := RN.Arg2;
          end;
        end;
      end

      else if Op = OP_EVAL then
      begin
        if RN.Res <= Types.Count then
        begin
          RN.Op := OP_NOP;
          continue;
        end;

        if GetSymbolRec(RN.Res).Kind = KindTYPE then
        begin
          RN.Op := OP_NOP;
          continue;
        end;

        S := GetSymbolRec(RN.Res).Name;

        if GetLanguage(N) in [PASCAL_LANGUAGE, BASIC_LANGUAGE] then
        begin
          if StrEql(S, 'System') then
          begin
            S := StrPascalNamespace;
            GetSymbolRec(RN.Res).Name := S;

            if NextRec(N).Op = OP_FIELD then
              if NextRec(N).Arg1 = RN.Res then
              begin
                if StrEql(GetSymbolRec(NextRec(N).Arg2).Name, GetPrintKeyword)
                then
                begin
                  Id := NextRec(N).Res;
                  GetSymbolRec(RN.Res).Name := '';
                  GetSymbolRec(RN.Res).Kind := KindNONE;
                  RN.Op := OP_NOP;
                  NextRec(N).Op := OP_NOP;
                  for J := N + 1 to Card do
                  begin
                    if Records[J].Op = OP_PUSH then
                      if Records[J].Res = Id then
                      begin
                        Records[J].Op := OP_PRINT_EX;
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                      end;

                    if Records[J].Op = OP_CALL then
                      if Records[J].Arg1 = Id then
                      begin
                        Records[J].Op := OP_NOP;
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                        break;
                      end;

                  end;
                  continue;
                end
                else if StrEql(GetSymbolRec(NextRec(N).Arg2).Name,
                  GetPrintlnKeyword) then
                begin
                  Id := NextRec(N).Res;
                  GetSymbolRec(RN.Res).Name := '';
                  GetSymbolRec(RN.Res).Kind := KindNONE;
                  RN.Op := OP_NOP;
                  NextRec(N).Op := OP_NOP;
                  for J := N + 1 to Card do
                  begin
                    if Records[J].Op = OP_PUSH then
                      if Records[J].Res = Id then
                      begin
                        Records[J].Op := OP_PRINT_EX;
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                      end;

                    if Records[J].Op = OP_CALL then
                      if Records[J].Arg1 = Id then
                      begin
                        Records[J].Op := OP_PRINT_EX;
                        Records[J].Arg1 := CreateConst(typeSTRING, '');
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                        break;
                      end;
                  end;
                  continue;
                end
                else if StrEql(GetSymbolRec(NextRec(N).Arg2).Name, 'write') then
                begin
                  Id := NextRec(N).Res;
                  GetSymbolRec(RN.Res).Name := '';
                  GetSymbolRec(RN.Res).Kind := KindNONE;
                  RN.Op := OP_NOP;
                  NextRec(N).Op := OP_NOP;
                  for J := N + 1 to Card do
                  begin
                    if Records[J].Op = OP_PUSH then
                      if Records[J].Res = Id then
                      begin
                        Records[J].Op := OP_PRINT;
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                      end;

                    if Records[J].Op = OP_CALL then
                      if Records[J].Arg1 = Id then
                      begin
                        Records[J].Op := OP_NOP;
                        break;
                      end;

                  end;
                  continue;
                end
                else if StrEql(GetSymbolRec(NextRec(N).Arg2).Name, 'writeln')
                then
                begin
                  Id := NextRec(N).Res;
                  GetSymbolRec(RN.Res).Name := '';
                  GetSymbolRec(RN.Res).Kind := KindNONE;
                  RN.Op := OP_NOP;
                  NextRec(N).Op := OP_NOP;
                  for J := N + 1 to Card do
                  begin
                    if Records[J].Op = OP_PUSH then
                      if Records[J].Res = Id then
                      begin
                        Records[J].Op := OP_PRINT_EX;
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                      end;

                    if Records[J].Op = OP_CALL then
                      if Records[J].Arg1 = Id then
                      begin
                        Records[J].Op := OP_PRINT_EX;
                        Records[J].Arg1 := CreateConst(typeSTRING, '');
                        Records[J].Arg2 := 0;
                        Records[J].Res := 0;
                        break;
                      end;
                  end;
                  continue;
                end;
              end;
          end; // System
        end;

        UpperBoundId := RN.Res;

        if RN.Language = JS_LANGUAGE then
        begin

          if with_stack.Count > 0 then
          begin
            Inc(N);
            continue;
          end;

          if S = 'arguments' then
          begin
            Id := GetSymbolRec(RN.Res).Level;
            if Id > 0 then
              if GetSymbolRec(Id).Kind = KindSUB then
              begin
                RN.Op := OP_FIELD;
                RN.Arg1 := TKernel(kernel).SymbolTable.GetSelfId(Id);
                RN.Arg2 := RN.Res;
                GetSymbolRec(RN.Res).OwnerId := TKernel(kernel)
                  .SymbolTable.GetSelfId(Id);
                GetSymbolRec(RN.Res).Kind := KindVAR;

                goto Next;
              end;
          end;
        end;

        if S = '' then
        begin
          RN.Op := OP_NOP;
          GetSymbolRec(RN.Res).Kind := KindNONE;
          continue;
        end;

        Upcase := GetUpcase(N);

        Id := 0;

        if block_stack.Count > 0 then
        begin
          Id := SymbolTable.LookUps(S, block_stack, Upcase, UpperBoundId);
          if Id > 0 then
          begin
            GetSymbolRec(RN.Res).Name := '';
            ReplaceId(RN.Res, Id);
            RN.Op := OP_NOP;
            goto Next;
          end;
        end;

        // try to find in the with list

        if Id = 0 then
        begin
          RC := GetNextRec(N);
          if (RC.Op = OP_SET_READ_ID) or (RC.Op = OP_SET_WRITE_ID) then
          begin
            I := GetSymbolRec(RN.Res).Level;
            while GetSymbolRec(I).Kind <> KindTYPE do
              I := GetSymbolRec(I).Level;
            Id := SymbolTable.LookUp(S, I, Upcase);
          end;
        end;

        if Id = 0 then
        begin
          if GetUpcase(N) then
            J := eval_helper_name.IndexOf(UpperCase(S))
          else
            J := eval_helper_name.IndexOf(S);

          if J >= 0 then
          begin
            Id := eval_helper_id[J];
            PatternFieldId := eval_helper_PatternFieldId[J];

            if GetSymbolRec(PatternFieldId).Kind = KindTYPE then
            begin
              GetSymbolRec(RN.Res).Name := '';
              ReplaceId(RN.Res, PatternFieldId);
              RN.Op := OP_NOP;
              goto Next;
            end;

            if GetSymbolRec(PatternFieldId).Kind = KindVAR then
              if GetSymbolRec(PatternFieldId).IsFinal then
              begin
                GetSymbolRec(RN.Res).Name := '';
                ReplaceId(RN.Res, PatternFieldId);
                RN.Op := OP_NOP;
                goto Next;
              end;

            RN.Op := OP_FIELD;
            RN.PatternFieldId := PatternFieldId;
            RN.Arg1 := Id;
            RN.Arg2 := RN.Res;
            GetSymbolRec(RN.Res).OwnerId := Id;
            GetSymbolRec(RN.Res).Kind := KindVAR;
            if GetSymbolRec(RN.Res).TypeId = 0 then
              GetSymbolRec(RN.Res).TypeId :=
                GetSymbolRec(PatternFieldId).TypeId;

            goto Next;
          end;
        end;

        if Id = 0 then
          for J := with_stack.Count - 1 downto 0 do
          begin
            Id := with_stack[J];
            TypeId := SymbolTable[Id].TypeId;

            if TypeId < Types.Count then
            begin
              Inc(N);
              goto NextTime; // for such cases as WITH TMyClass.Create DO ......
            end;

            PatternFieldId := SymbolTable.LookUp(S, TypeId, Upcase);
            if PatternFieldId <> 0 then
            begin
              if StrEql(S, GetPrintKeyword) then
              begin
                if (GetSymbolRec(PatternFieldId).Kind = KindSUB) and
                  (GetSymbolRec(PatternFieldId).Count = 0) then
                begin
                  RN.Op := OP_NOP;
                  goto Next;
                end;

                // remove OP_PRINT_EX
                for K := N + 1 to Card do
                  if Records[K].Op = OP_PRINT_EX then
                  begin
                    Records[K].Op := OP_NOP;
                    break;
                  end;
              end;

              if GetUpcase(N) then
                S := UpperCase(S);
              if eval_helper_name.IndexOf(S) = -1 then
              begin
                eval_helper_name.Add(S);
                eval_helper_id.Add(Id);
                eval_helper_PatternFieldId.Add(PatternFieldId);
              end;

              if GetSymbolRec(PatternFieldId).Kind = KindTYPE then
              begin
                GetSymbolRec(RN.Res).Name := '';
                ReplaceId(RN.Res, PatternFieldId);
                RN.Op := OP_NOP;
                goto Next;
              end;

              if GetSymbolRec(PatternFieldId).Kind = KindVAR then
                if GetSymbolRec(PatternFieldId).IsFinal then
                begin
                  GetSymbolRec(RN.Res).Name := '';
                  ReplaceId(RN.Res, PatternFieldId);
                  RN.Op := OP_NOP;
                  goto Next;
                end;

              RN.Op := OP_FIELD;
              RN.PatternFieldId := PatternFieldId;
              RN.Arg1 := Id;
              RN.Arg2 := RN.Res;
              GetSymbolRec(RN.Res).OwnerId := Id;
              GetSymbolRec(RN.Res).Kind := KindVAR;
              if GetSymbolRec(RN.Res).TypeId = 0 then
                GetSymbolRec(RN.Res).TypeId :=
                  GetSymbolRec(PatternFieldId).TypeId;

              goto Next;
            end
            else
              Id := 0;
          end;

        // try to find in parameters of the sub list

        if Id = 0 then
          for J := sub_stack.Count - 1 downto 0 do
          begin
            SubId := sub_stack[J];
            for I := 0 to SymbolTable[SubId].Count - 1 do
            begin
              ParamId := SymbolTable.GetParamId(SubId, I);
              if Upcase then
              begin
                if StrEql(S, GetSymbolRec(ParamId).Name) then
                begin
                  Id := ParamId;
                  break;
                end;
              end
              else
              begin
                if S = GetSymbolRec(ParamId).Name then
                begin
                  Id := ParamId;
                  break;
                end;
              end;
            end;

            if Id > 0 then
            begin
              ReplaceId(RN.Res, Id);
              RN.Op := OP_NOP;
              goto Next;
            end;
          end;

        // try to find in the sub list

        for J := sub_stack.Count - 1 downto 0 do
        begin
          Id := GetDeclaredVar(S, sub_stack[J], Upcase, N);

          if Id = 0 then
          begin
            if Upcase then
            begin
              if StrEql(S, GetSymbolRec(SymbolTable.GetResultId(sub_stack[J]))
                .Name) then
                Id := SymbolTable.GetResultId(sub_stack[J]);
            end
            else
            begin
              if S = GetSymbolRec(SymbolTable.GetResultId(sub_stack[J])).Name
              then
                Id := SymbolTable.GetResultId(sub_stack[J]);
            end;
          end;

          if Id > 0 then
          begin
            ReplaceId(RN.Res, Id);
            RN.Op := OP_NOP;
            goto Next;
          end;
        end;

        if (Id = 0) and (sub_stack.Count > 0) then
        // try to find nested function
        begin
          GetSymbolRec(RN.Res).Name := '';
          Id := SymbolTable.LookUps(S, sub_stack, Upcase, UpperBoundId);
        end;

        // try to find in nested class
        if RN.Language in [BASIC_LANGUAGE, JAVA_LANGUAGE] then
          if Id = 0 then
            if GetCurrSelfId(N) > 0 then
            begin
              temp_id := RN.Res;

              NS_ID := GetSymbolRec(temp_id).Level;
              if NS_ID > 0 then
              begin
                repeat
                  if GetSymbolRec(NS_ID).Kind in kindSUBS then
                    NS_ID := GetSymbolRec(NS_ID).Level
                  else
                    break;
                  if NS_ID = 0 then
                    break;
                until false;

                if NS_ID > 0 then
                  if GetSymbolRec(NS_ID).Kind = KindTYPE then
                    if GetSymbolRec(NS_ID).FinalTypeId = typeCLASS then
                    begin
                      TypeId := NS_ID;

                      NS_ID := GetSymbolRec(NS_ID).Level;
                      if GetSymbolRec(NS_ID).Kind = KindTYPE then
                        if GetSymbolRec(NS_ID).FinalTypeId = typeCLASS then
                        begin
                          temp_id := SymbolTable.LookUp(S, NS_ID, Upcase);
                          if temp_id > 0 then
                          begin

                            if GetSymbolRec(temp_id).Kind = KindTYPE then
                            begin
                              GetSymbolRec(RN.Res).Name := '';
                              ReplaceId(RN.Res, temp_id);
                              RN.Op := OP_NOP;
                            end
                            else
                            begin
                              RC := TCodeRec.Create(OP_FIELD, Self);
                              RC.Arg1 := GetCurrSelfId(N);
                              RC.Res := NewField(StrOuterThis,
                                SymbolTable.GetOuterThisId(TypeId), RC.Arg1);
                              RC.Arg2 := RC.Res;

                              Insert(N, RC);

                              RN.Op := OP_FIELD;
                              RN.Arg1 := RC.Res;
                              RN.Arg2 := RN.Res;
                              GetSymbolRec(RN.Res).Name := S;
                              GetSymbolRec(RN.Res).OwnerId := RN.Arg1;
                              GetSymbolRec(RN.Res).Kind := KindVAR;
                              GetSymbolRec(RN.Res).TypeId :=
                                GetSymbolRec(temp_id).TypeId;
                            end;

                            goto Next;
                          end;
                        end;
                    end;
              end;
            end;

        NS_ID := -1;

        if (Id = 0) then
        // try to find in the using list
        begin
          GetSymbolRec(RN.Res).Name := '';

          Id := LookupInExtraUnitList(S);
          if Id = 0 then
            Id := SymbolTable.LookUpsEx(S, using_stack, NS_ID, Upcase);

          if Id = 0 then
            Id := SymbolTable.LookUp(S, 0, Upcase);
        end;

        if Id = 0 then
        begin
          if RN.Language = JS_LANGUAGE then
          begin
            Id := GetSymbolRec(RN.Res).Level;
            if Id > 0 then
            begin
              if GetSymbolRec(Id).Kind = KindSUB then
              begin
                RN.Op := OP_FIELD;
                RN.Arg1 := TKernel(kernel).SymbolTable.GetSelfId(Id);
                RN.Arg2 := RN.Res;
                GetSymbolRec(RN.Res).Name := S;
                GetSymbolRec(RN.Res).OwnerId := TKernel(kernel)
                  .SymbolTable.GetSelfId(Id);
                GetSymbolRec(RN.Res).Kind := KindVAR;

                goto Next;
              end
              else
              begin
                GetSymbolRec(RN.Res).Name := S;
                GetSymbolRec(RN.Res).Kind := KindVAR;
                GetSymbolRec(RN.Res).TypeId := typeVARIANT;
                RN.Op := OP_NOP;
                goto Next;
              end;
            end
            else
            begin
              GetSymbolRec(RN.Res).Name := S;
              GetSymbolRec(RN.Res).Kind := KindVAR;
              GetSymbolRec(RN.Res).TypeId := typeVARIANT;
              RN.Op := OP_NOP;
              goto Next;
            end;
          end;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            if Records[N + 1].Op = OP_ASSIGN_TYPE then
              if Records[N + 1].Arg2 = RN.Res then
                Records[N + 1].Op := OP_NOP;
            if Records[N + 2].Op = OP_ASSIGN_TYPE then
              if Records[N + 2].Arg2 = RN.Res then
                Records[N + 2].Op := OP_NOP;
            goto Next;
          end;

          GetSymbolRec(RN.Res).Name := S;
          if StrEql(S, 'ExitCode') then
          begin
            RC := TCodeRec.Create(Op, Self);
            RC.Op := OP_GET_PROG;
            RC.Arg1 := 0;
            RC.Arg2 := 0;
            RC.Res := NewTempVar(GetLevel(N), Id_Prog);
            Insert(N, RC);

            Inc(N);

            RN.Op := OP_FIELD;
            RN.Arg1 := RC.Res;
            RN.Arg2 := RN.Res;
            GetSymbolRec(RN.Res).OwnerId := RC.Res;
            GetSymbolRec(RN.Res).Kind := KindVAR;
          end
          else if StrEql(S, GetPrintKeyword) then
          begin
            RN.Op := OP_NOP;
          end
          else if StrEql(S, GetPrintlnKeyword) then
          begin
            RN.Op := OP_NOP;
          end
          else
          begin
            if GetSymbolRec(RN.Res).Kind = KindLABEL then
              RN.Op := OP_NOP
            else
            begin
              CreateEvalError(S, using_stack);
            end;
          end;
        end
        else
        begin
          I1 := Modules.IndexOfModuleById(RN.Res);
          I2 := Modules.IndexOfModuleById(Id);

          if (I1 >= 0) and (I2 >= 0) and (I1 <> I2) then
          begin
            with Modules[I2] do
              if StrEql(LanguageName, 'Pascal') then
                if not((Id >= S1) and (Id <= S2)) then
                begin
                  GetSymbolRec(RN.Res).Name := S;
                  CreateEvalError(S, using_stack);
                  goto Next;
                end;
          end;

          if (Id > RN.Res) and (GetSymbolRec(Id).Kind in [KindVAR, KindCONST])
            and (I1 = I2) then
          begin
            if RN.Language = JS_LANGUAGE then
            begin
              ReplaceId(RN.Res, Id);
              RN.Op := OP_NOP;
              goto Next;
            end;

            Id := SymbolTable.LookUpsExcept(S, using_stack, NS_ID, Upcase);
            if Id = 0 then
            begin
              GetSymbolRec(RN.Res).Name := S;
              CreateEvalError(S, using_stack);
              goto Next;
            end;
          end;

          B1 := (SymbolTable[Id].Kind = KindSUB) and
            (SymbolTable[Id].FinalTypeId <> typeVOID);

          B2 := false;
          for J := N + 1 to Card do
          begin
            RJ := Records[J];
            if RJ.Op = OP_END_MODULE then
              break;

            if (RJ.Op = OP_ASSIGN) and (RJ.Arg2 = RN.Res) then
              if GetSymbolRec(RJ.Arg1).FinalTypeId = typePROC then
              begin
                B2 := true;
                break;
              end;

            if (RJ.Op = OP_CALL) and (RJ.Arg1 = RN.Res) then
            begin
              B2 := true;
              break;
            end;
          end;

          if B1 and (not B2) and (RN.Language <> JS_LANGUAGE) and (not IsExport)
          then
          begin
            RN.Op := OP_CALL;
            RN.Arg1 := Id;
            RN.Arg2 := 0;

            if RN.Res > 0 then
            begin
              GetSymbolRec(RN.Res).Kind := KindVAR;
              GetSymbolRec(RN.Res).TypeId := GetSymbolRec(RN.Arg1).TypeId;
            end;
          end
          else
          begin
            ReplaceId(RN.Res, Id);
            RN.Op := OP_NOP;
          end;
        end;

      end
      else if Op = OP_FIELD then
      begin
        Arg1 := RN.Arg1;
        Arg2 := RN.Arg2;
        K1 := GetSymbolRec(Arg1).Kind;

        if K1 = KindNAMESPACE then
        begin
          Id := SymbolTable.LookUp(GetSymbolRec(Arg2).Name, Arg1, GetUpcase(N));
          if Id = 0 then
            if Arg1 = H_PascalNamespace then
              Id := SymbolTable.LookUp(GetSymbolRec(Arg2).Name, 0,
                GetUpcase(N));

          if Id <> 0 then
          begin
            GetSymbolRec(Id).NSOwnerId := Arg1;

            if (GetSymbolRec(Id).Kind in kindSUBS) and
              (GetSymbolRec(Id).Count = 0) and (not CallExpected(Arg2)) then
            begin
              RN.Op := OP_CALL;
              RN.Arg1 := Id;
              RN.Arg2 := 0;
              RN.Res := Arg2;
              GetSymbolRec(RN.Res).TypeId := GetSymbolRec(Id).TypeId;
            end
            else
            begin
              ReplaceId(Arg2, Id);
              GetSymbolRec(Arg2).Name := '';
              GetSymbolRec(Arg2).Kind := KindNONE;

              RN.Op := OP_NOP;
            end;
          end;
        end
        else if K1 = KindTYPE then
        begin
          Id := SymbolTable.LookUp(GetSymbolRec(Arg2).Name, Arg1, GetUpcase(N));
          if Id > 0 then
            if GetSymbolRec(Id).Kind = KindTYPE then
            begin
              ReplaceId(Arg2, Id);
              RN.Op := OP_NOP;
            end;
        end
        else
        begin
          TypeId := GetSymbolRec(Arg1).TerminalTypeId;
          Id := SymbolTable.LookUp(GetSymbolRec(Arg2).Name, TypeId,
            GetUpcase(N));
          if Id > 0 then
            if GetSymbolRec(Id).Kind in [KindTYPE_FIELD, KindPROP] then
            begin
              GetSymbolRec(Arg2).TypeId := GetSymbolRec(Id).TypeId;
            end;
        end;
      end

      // set types
      else if Op = OP_ASSIGN_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);

          SymbolTable.AddTypes(S, TKernel(kernel).UndeclaredTypes,
            TKernel(kernel).Errors.Count - 1, true);
        end;

        GetSymbolRec(RN.Arg1).TypeId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_ASSIGN_THE_SAME_TYPE then
      begin
        GetSymbolRec(RN.Arg1).TypeId := GetSymbolRec(RN.Arg2).TypeId;
        RN.Op := OP_NOP;
      end
      else if Op = OP_ASSIGN_TYPE_ALIAS then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_CREATE_POINTER_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_CREATE_CLASSREF_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_BEGIN_HELPER_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        if SymbolTable.TypeHelpers.Keys.IndexOf(RN.Arg1) = -1 then
          SymbolTable.TypeHelpers.Add(RN.Arg1, RN.Arg2);
      end
      else if Op = OP_CREATE_DYNAMIC_ARRAY_TYPE then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          S := GetSymbolRec(RN.Arg2).Name;

          if TKernel(kernel).InterfaceOnly then
          begin
            TKernel(kernel).UndeclaredIdentifiers.Add(S);
            RN.Op := OP_NOP;
            goto Next;
          end;

          CreateError(errUndeclaredType, [S]);
        end;

        GetSymbolRec(RN.Arg1).PatternId := RN.Arg2;
        RN.Op := OP_NOP;
      end
      else if Op = OP_ADD_ANCESTOR then
      begin
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
        begin
          CreateError(errClassTypeRequired, []);
        end;

        GetSymbolRec(RN.Arg1).AncestorId := RN.Arg2;
        // RN.Op := OP_NOP;
      end
      else if Op = OP_ADD_INTERFACE then
      begin
        if RN.Arg2 = 0 then
          RN.Arg2 := SymbolTable.LookupAnonymousInterface(RN.Arg1);
        if GetSymbolRec(RN.Arg2).Kind <> KindTYPE then
          goto Next;

        SymbolTable.RegisterSupportedInterface(RN.Arg1,
          GetSymbolRec(RN.Arg2).Name, IUnknown);
        RN.Op := OP_NOP;
      end
      else if Op = OP_ADDRESS then
      begin
        if not(GetSymbolRec(RN.Arg1).Kind in [KindVAR, KindSUB]) then
          goto Next;

        TypeId := TKernel(kernel).SymbolTable.AddPointerType
          (GetSymbolRec(RN.Arg1).TypeId).Id;
        GetSymbolRec(RN.Res).TypeId := TypeId;
      end
      else if Op = OP_TERMINAL then
      begin
        TypeId := GetSymbolRec(RN.Arg1).TypeId;
        TypeId := GetSymbolRec(TypeId).PatternId;
        GetSymbolRec(RN.Res).TypeId := TypeId;
        GetSymbolRec(RN.Res).ByRef := true;
      end
      else if Op = OP_SIZEOF then
      begin
        GetSymbolRec(RN.Res).TypeId := typeINTEGER;
      end
      else if Op = OP_FIND_CONSTRUCTOR then
      begin
        TypeId := RN.Arg1;
        if RN.Language = JS_LANGUAGE then
          if GetSymbolRec(TypeId).Kind = KindTYPE then
          begin
            Id := TKernel(kernel).SymbolTable.FindConstructorId(TypeId);
            if Id > 0 then
            begin
              TypeId := GetSymbolRec(Id).TerminalTypeId;
              if Records[N - 1].Op = OP_LVALUE then
              begin
                Id := Records[N - 1].Arg1;
                if GetSymbolRec(Id).TypeId = 0 then
                  GetSymbolRec(Id).TypeId := TypeId;
              end;

            end;
          end;
      end
      else if Op = OP_CALL then
      begin
        if RN.Res > 0 then
          if GetSymbolRec(RN.Arg1).TypeId > 0 then
            if GetSymbolRec(RN.Res).TypeId = 0 then
            begin
              GetSymbolRec(RN.Res).TypeId := GetSymbolRec(RN.Arg1).TypeId;
            end;
      end;

    Next:

      Inc(N);
    end; // while

  finally
    FreeAndNil(using_stack);
    FreeAndNil(block_stack);
    FreeAndNil(with_stack);
    FreeAndNil(sub_stack);
    FreeAndNil(eval_helper_name);
    FreeAndNil(eval_helper_id);
    FreeAndNil(eval_helper_PatternFieldId);

  end;
end;

procedure TCode.UpdateDefaultConstructors;
var
  R, RC: TCodeRec;
  SubId, ClassId, AncestorClassId, ConstructorId: Integer;
  SymbolTable: TSymbolTable;
  I, ParamId, ParamTypeId, Res: Integer;
  S: String;
  SR: TSymbolRec;
  ByRef, IsConst: Boolean;
  value: Variant;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  N := Card;
  while N > 1 do
  begin
    R := Records[N];
    if R.Op = OP_UPDATE_DEFAULT_CONSTRUCTOR then
    begin
      SubId := R.Arg1;
      Res := R.Res;

      if not GetSymbolRec(SubId).IsDefault then
        RaiseError(errInternalError, []);

      R.Op := OP_NOP;

      ClassId := GetSymbolRec(SubId).Level;

      AncestorClassId := GetSymbolRec(ClassId).AncestorId;
      if GetSymbolRec(AncestorClassId).FinalTypeId = typeINTERFACE then
      begin
        SymbolTable.RegisterSupportedInterface(ClassId, AncestorClassId);

        AncestorClassId := H_TInterfacedObject;
        GetSymbolRec(ClassId).AncestorId := H_TInterfacedObject;
      end;

      repeat
        ConstructorId := SymbolTable.FindConstructorId(AncestorClassId);

        if ConstructorId > 0 then
        begin
          if GetSymbolRec(ConstructorId).IsDefault then
            AncestorClassId := GetSymbolRec(AncestorClassId).AncestorId
          else // ok
            break;
        end
        else
          AncestorClassId := GetSymbolRec(AncestorClassId).AncestorId;

      until false;

      GetSymbolRec(SubId).Count := GetSymbolRec(ConstructorId).Count;

      for I := 0 to GetSymbolRec(SubId).Count - 1 do
      begin
        ParamId := SymbolTable.GetParamId(ConstructorId, I);
        SR := GetSymbolRec(ParamId);
        ParamTypeId := SR.TypeId;
        S := SR.Name;
        ByRef := SR.ByRef;
        value := SR.value;
        IsConst := SR.IsConst;

        ParamId := NewTempVar(SubId, ParamTypeId);
        SR := GetSymbolRec(ParamId);
        SR.Param := true;
        SR.Name := S;
        SR.ByRef := ByRef;
        SR.value := value;
        SR.IsConst := IsConst;

        RC := TCodeRec.Create(OP_PUSH, Self);
        RC.Arg1 := ParamId;
        RC.Arg2 := I;
        RC.Res := Res; // ImmConstructorId;

        Insert(N, RC);
        Inc(N);
      end;

    end;
    Dec(N);
  end;
end;

procedure TCode.OperAddress;
var
  TypeId: Integer;
begin
  if not(GetSymbolRec(Records[N].Arg1).Kind in [KindVAR, KindSUB]) then
    CreateError(errVariableRequired, []);

  if GetSymbolRec(Records[N].Res).TypeId = 0 then
  begin
    TypeId := TKernel(kernel).SymbolTable.AddPointerType
      (GetSymbolRec(Records[N].Arg1).TypeId).Id;
    GetSymbolRec(Records[N].Res).TypeId := TypeId;
  end;
end;

procedure TCode.OperEval;
var
  SubId, ParamId, J, K, Id, Op, TypeId, PatternFieldId: Integer;
  using_stack, with_stack, sub_stack, block_stack: TIntegerStack;
  SymbolTable: TSymbolTable;
  Upcase: Boolean;
  S: String;
  Modules: TModuleList;
  I1, I2: Integer;
  B1, B2: Boolean;
  I: Integer;
  RC: TCodeRec;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  Modules := TKernel(kernel).Modules;

  using_stack := TIntegerStack.Create;
  block_stack := TIntegerStack.Create;
  with_stack := TIntegerStack.Create;
  sub_stack := TIntegerStack.Create;

  try
    for I := 1 to N do
    begin
      Op := Records[I].Op;

      if Op = OP_BEGIN_USING then
        using_stack.Push(Records[I].Arg1)
      else if Op = OP_END_USING then
        using_stack.Pop
      else if Op = OP_BEGIN_MODULE then
        using_stack.Clear
      else if Op = OP_END_MODULE then
        using_stack.Clear

      else if Op = OP_BEGIN_BLOCK then
        block_stack.Push(Records[I].Arg1)
      else if Op = OP_END_BLOCK then
        block_stack.Pop

      else if Op = OP_BEGIN_WITH then
      begin
        with_stack.Push(Records[I].Arg1);
      end
      else if Op = OP_END_WITH then
      begin
        with_stack.Pop;
      end
      else if Op = OP_BEGIN_SUB then
        sub_stack.Push(Records[I].Arg1)
      else if Op = OP_END_SUB then
        sub_stack.Pop
    end;

    S := GetSymbolRec(Records[N].Res).Name;

    Upcase := GetUpcase(N);

    Id := 0;

    // try to find in the with list

    if Id = 0 then
      for J := with_stack.Count - 1 downto 0 do
      begin
        Id := with_stack[J];
        TypeId := SymbolTable[Id].TypeId;

        PatternFieldId := SymbolTable.LookUp(S, TypeId, Upcase);
        if PatternFieldId <> 0 then
        begin
          Records[N].Op := OP_FIELD;
          Records[N].PatternFieldId := PatternFieldId;
          Records[N].Arg1 := Id;
          Records[N].Arg2 := Records[N].Res;
          GetSymbolRec(Records[N].Res).OwnerId := Id;
          GetSymbolRec(Records[N].Res).Kind := KindVAR;
          if GetSymbolRec(Records[N].Res).TypeId = 0 then
            GetSymbolRec(Records[N].Res).TypeId :=
              GetSymbolRec(PatternFieldId).TypeId;
          Dec(N);
          Exit;
        end
        else
          Id := 0;
      end;

    // try to find in parameters of the sub list

    if Id = 0 then
      for J := sub_stack.Count - 1 downto 0 do
      begin
        SubId := sub_stack[J];
        for K := 0 to SymbolTable[SubId].Count - 1 do
        begin
          ParamId := SymbolTable.GetParamId(SubId, K);
          if Upcase then
          begin
            if StrEql(S, GetSymbolRec(ParamId).Name) then
            begin
              Id := ParamId;
              break;
            end;
          end
          else
          begin
            if S = GetSymbolRec(ParamId).Name then
            begin
              Id := ParamId;
              break;
            end;
          end;
        end;

        if Id > 0 then
        begin
          ReplaceId(Records[N].Res, Id);
          Records[N].Op := OP_NOP;
          Exit;
        end;
      end;

    // try to find in the sub list

    if Id = 0 then
      for J := sub_stack.Count - 1 downto 0 do
      begin
        Id := GetDeclaredVar(S, sub_stack[J], Upcase, N);

        if Id = 0 then
        begin
          if Upcase then
          begin
            if StrEql(S, GetSymbolRec(SymbolTable.GetResultId(sub_stack[J]))
              .Name) then
              Id := SymbolTable.GetResultId(sub_stack[J]);
          end
          else
          begin
            if S = GetSymbolRec(SymbolTable.GetResultId(sub_stack[J])).Name then
              Id := SymbolTable.GetResultId(sub_stack[J]);
          end;
        end;

        if Id > 0 then
        begin
          ReplaceId(Records[N].Res, Id);
          Records[N].Op := OP_NOP;
          Exit;
        end;
      end;

    if (Id = 0) and (sub_stack.Count > 0) then
    // try to find nested function
    begin
      GetSymbolRec(Records[N].Res).Name := '';
      Id := SymbolTable.LookUps(S, sub_stack, Upcase);
    end;

    if (Id = 0) then
    // try to find in the using list
    begin
      GetSymbolRec(Records[N].Res).Name := '';
      Id := SymbolTable.LookUps(S, using_stack, Upcase);
    end;

    if Id = 0 then
    begin
      GetSymbolRec(Records[N].Res).Name := S;

      if StrEql(S, 'ExitCode') then
      begin
        RC := TCodeRec.Create(OP_GET_PROG, Self);
        RC.Arg1 := 0;
        RC.Arg2 := 0;
        RC.Res := NewTempVar(GetLevel(N), Id_Prog);
        Insert(N, RC);

        Inc(N);

        Records[N].Op := OP_FIELD;
        Records[N].Arg1 := RC.Res;
        Records[N].Arg2 := Records[N].Res;
        GetSymbolRec(Records[N].Res).OwnerId := RC.Res;
        GetSymbolRec(Records[N].Res).Kind := KindVAR;
      end
      else
      begin
        if GetLanguage(N) = JS_LANGUAGE then
        begin
          Records[N].Op := OP_FIND_CONTEXT;
          Records[N].Arg1 := CreateConst(typeSTRING, S);
          Records[N].Arg2 := CreateVariantVar(GetLevel(N));
          GetSymbolRec(Records[N].Res).Kind := KindVAR;
          GetSymbolRec(Records[N].Res).TypeId := typeVARIANT;

          GetSymbolRec(Records[N].Res).Name := '@';

          RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
          RC.Arg1 := GetLevel(N);
          RC.Arg2 := Records[N].Res;
          RC.Res := 0;
          Insert(N, RC);

          Inc(N);

          Exit;
        end;

        CreateEvalError(S, using_stack);
      end;
    end
    else
    begin

      if GetSymbolRec(Id).Kind = KindVAR then
        if GetSymbolRec(Id).TypeId <> 0 then
          if GetLanguage(N) = JS_LANGUAGE then
          begin
            GetSymbolRec(Records[N].Res).Name := S;
            begin
              Records[N].Op := OP_FIND_CONTEXT;
              Records[N].Arg1 := CreateConst(typeSTRING, S);
              Records[N].Arg2 := Id;
              GetSymbolRec(Records[N].Res).Kind := KindVAR;
              GetSymbolRec(Records[N].Res).TypeId := typeVARIANT;
              GetSymbolRec(Records[N].Res).Name := '@';

              RC := TCodeRec.Create(OP_DECLARE_TEMP_VAR, Self);
              RC.Arg1 := GetLevel(N);
              RC.Arg2 := Records[N].Res;
              RC.Res := 0;
              Insert(N, RC);

              Inc(N);

              Exit;
            end;
          end;

      I1 := Modules.IndexOfModuleById(Records[N].Res);
      I2 := Modules.IndexOfModuleById(Id);

      if (I1 >= 0) and (I2 >= 0) and (I1 <> I2) then
      begin
        with Modules[I2] do
          if not((Id >= S1) and (Id <= S2)) then
          begin
            GetSymbolRec(Records[N].Res).Name := S;
            CreateError(errUndeclaredIdentifier, [S]);
            Exit;
          end;
      end;

      if (Id > Records[N].Res) and
        (GetSymbolRec(Id).Kind in [KindVAR, KindCONST]) and (I1 = I2) then
      begin
        GetSymbolRec(Records[N].Res).Name := S;
        CreateError(errUndeclaredIdentifier, [S]);
        Exit;
      end;

      B1 := (SymbolTable[Id].Kind = KindSUB) and (SymbolTable[Id].Count = 0) and
        (SymbolTable[Id].FinalTypeId <> typeVOID);

      B2 := false;
      for J := N + 1 to Card do
        if (Records[J].Op = OP_CALL) and (Records[J].Arg1 = Records[N].Res) then
        begin
          B2 := true;
          break;
        end;

      if B1 and (not B2) then
      begin
        Records[N].Op := OP_CALL;
        Records[N].Arg1 := Id;
        Records[N].Arg2 := 0;

        if Records[N].Res > 0 then
        begin
          GetSymbolRec(Records[N].Res).Kind := KindVAR;
          GetSymbolRec(Records[N].Res).TypeId :=
            GetSymbolRec(Records[N].Arg1).TypeId;
        end;
        Dec(N);
      end
      else
      begin
        ReplaceId(Records[N].Res, Id);
        Records[N].Op := OP_NOP;
      end;
    end;

  finally
    FreeAndNil(using_stack);
    FreeAndNil(block_stack);
    FreeAndNil(with_stack);
    FreeAndNil(sub_stack);
  end;
end;

procedure TCode.OperImplements;
begin
  Records[N].Op := OP_NOP;
end;

procedure TCode.OperPushContext;
begin
  context_list.Push(Records[N].Arg1);
end;

procedure TCode.OperPopContext;
begin
  context_list.Pop;
end;

procedure TCode.OperEvalConstructor;
var
  SubId, ClassId: Integer;
begin
  ClassId := Records[N].Arg1;
  if GetSymbolRec(ClassId).Kind <> KindTYPE then
    CreateError(errClassTypeExpected, []);
  if GetSymbolRec(ClassId).FinalTypeId <> typeCLASS then
    CreateError(errClassTypeExpected, []);
  SubId := TKernel(kernel).SymbolTable.FindConstructorIdEx(ClassId);
  ReplaceId(Records[N].Res, SubId);
  Records[N].Op := OP_NOP;

  Records[N].Op := OP_PUSH_CLASSREF;
  Records[N].Arg1 := ClassId + 1;
  Records[N].Arg2 := 0;
  Records[N].Res := SubId;
end;

procedure TCode.OperEvalInherited;
var
  Op, Res, I, SubId, AncestorClassId, T, L, CurrSubId: Integer;
  S: String;
  RC, RN: TCodeRec;
  ncase: Integer;
  ReadProp: Boolean;
begin
  S := GetSymbolRec(Records[N].Arg1).Name;

  CurrSubId := Records[N].Arg1;
  ncase := 1;
  if not(GetSymbolRec(CurrSubId).Kind in kindSUBS) then
  begin
    CurrSubId := GetSymbolRec(CurrSubId).Level;
    ncase := 2;
  end
  else
  begin
    I := N;
    while Records[I].Op <> OP_INIT_SUB do
      Dec(I);
    CurrSubId := Records[I].Arg1;
  end;

  L := GetSymbolRec(CurrSubId).Level;
  T := GetSymbolRec(L).FinalTypeId;
  if T <> typeCLASS then
  begin
    CreateError(errThisFormOfMethodCallOnlyAllowedInMethodsOfDerivedTypes, []);
    Exit;
  end;
  AncestorClassId := GetSymbolRec(L).AncestorId;

  SubId := 0;

  if S = '' then
  begin
    if GetSymbolRec(CurrSubId).Kind = KindCONSTRUCTOR then
    begin
      SubId := TKernel(kernel).SymbolTable.FindConstructorIdEx(AncestorClassId);
      GetSymbolRec(CurrSubId).Name := 'Create';
    end
    else if GetSymbolRec(CurrSubId).Kind = KindDESTRUCTOR then
    begin
      SubId := TKernel(kernel).SymbolTable.FindDestructorIdEx(AncestorClassId);
      GetSymbolRec(CurrSubId).Name := 'Destroy';
    end
    else
      RaiseError(errInternalError, []);
  end
  else
  begin
    SubId := TKernel(kernel).SymbolTable.LookUp(S, AncestorClassId,
      GetUpcase(N));
  end;

  if SubId = 0 then
  begin
    CreateError(errUndeclaredIdentifier, [S]);
    Exit;
  end;

  // if GetSymbolRec(SubId).Kind = KindDESTRUCTOR then
  // SubId := Id_DestroyInherited;

  if GetSymbolRec(SubId).Kind = KindPROP then
  begin
    ReadProp := false;
    for I := N downto GetStmt(N) do
      if Records[I].Op = OP_LVALUE then
        ReadProp := true;

    if ReadProp then
    begin
      SubId := TKernel(kernel).SymbolTable[SubId].ReadId;
      if SubId = 0 then
        RaiseError(errCannotReadWriteOnlyProperty, [S]);
    end
    else
    begin
      SubId := TKernel(kernel).SymbolTable[SubId].WriteId;
      if SubId = 0 then
        RaiseError(errCannotAssignToReadOnlyProperty, [S]);
    end;
  end;

  AncestorClassId := GetSymbolRec(SubId).Level;

  if GetSymbolRec(SubId).OverCount = 0 then
    NoOverloadSearch := true;

  if Records[N - 1].Op = OP_PUSH_CLASSREF then
  begin
    Records[N - 1].Op := OP_PUSH_INSTANCE;
    Records[N - 1].Res := SubId;
    Records[N - 1].CodeRecTag := TAG_DISCARD_VIRTUAL_CALL;
  end
  else if Records[N - 1].Op = OP_PUSH_INSTANCE then
  begin
    Records[N - 1].Res := SubId;
    Records[N - 1].CodeRecTag := TAG_DISCARD_VIRTUAL_CALL;
  end;

  Records[N].Op := OP_NOP;
  Res := Records[N].Res;
  GetSymbolRec(Res).Kind := KindNONE;

  I := N + 1;

  while I < Card do
  begin
    Op := Records[I].Op;

    if (Op = OP_PUSH) and (Records[I].Res = Res) then
      Records[I].Res := SubId
    else if (Op = OP_PUSH_INSTANCE) and (Records[I].Res = Res) then
      Records[I].Res := SubId
    else if (Op = OP_PUSH_CLASSREF) and (Records[I].Res = Res) then
      Records[I].Res := SubId;

    if Op = OP_CALL then
      if Records[I].Arg1 = Res then
      begin
        Records[I].IsStatic := true;
        Records[I].Arg1 := SubId;

        if ncase = 2 then
        begin
          if GetSymbolRec(SubId).IsSharedMethod then
          begin
            RC := TCodeRec.Create(OP_PUSH_CLASSREF, Self);
            RC.CodeRecTag := TAG_DISCARD_VIRTUAL_CALL;
            RC.Arg1 := AncestorClassId + 1;
            RC.Arg2 := 0;
            RC.Res := SubId;
            Insert(N, RC);
          end
          else if GetSymbolRec(SubId).Kind = KindTYPE_FIELD then
          begin
            Records[I].Op := OP_NOP;

            RN := Records[I];

            RN.Op := OP_FIELD;
            RN.Arg1 := GetCurrSelfId(N);
            RN.Arg2 := RN.Res;
            GetSymbolRec(RN.Res).Name := GetSymbolRec(SubId).Name;
            GetSymbolRec(RN.Res).TypeId := GetSymbolRec(SubId).TypeId;
          end
          else
          begin
            RC := TCodeRec.Create(OP_PUSH_INSTANCE, Self);
            RC.CodeRecTag := TAG_DISCARD_VIRTUAL_CALL;
            RC.Arg1 := TKernel(kernel).SymbolTable.GetSelfId(CurrSubId);
            RC.Arg2 := 0;
            RC.Res := SubId;
            Insert(N, RC);

            if ((GetSymbolRec(SubId).Kind = KindCONSTRUCTOR) and
              GetSymbolRec(SubId).Host) or
              ((GetSymbolRec(SubId).Kind = KindSUB) and
              (GetSymbolRec(SubId).IsSharedMethod)) then
            begin
              Inc(I);

              Records[I].Res := 0;

              RC := TCodeRec.Create(OP_UPDATE_INSTANCE, Self);
              RC.Arg1 := TKernel(kernel).SymbolTable.GetSelfId(CurrSubId);
              RC.Arg2 := 0;
              RC.Res := 0;
              Insert(I + 1, RC);
            end;
          end;

          Exit;
        end;

        if ((GetSymbolRec(SubId).Kind = KindCONSTRUCTOR) and GetSymbolRec(SubId)
          .Host) or ((GetSymbolRec(SubId).Kind = KindSUB) and
          (GetSymbolRec(SubId).IsSharedMethod)) then
        begin
          Records[I].Res := 0;

          RC := TCodeRec.Create(OP_UPDATE_INSTANCE, Self);
          RC.Arg1 := TKernel(kernel).SymbolTable.GetSelfId(CurrSubId);
          RC.Arg2 := 0;
          RC.Res := 0;
          Insert(I + 1, RC);
        end
        else if GetSymbolRec(SubId).Kind = KindDESTRUCTOR then
        begin
          RC := TCodeRec.Create(OP_CLEAR_EDX, Self);
          RC.Arg1 := 0;
          RC.Arg2 := 0;
          RC.Res := 0;
          Insert(I, RC);
        end;

        Exit;
      end;

    Inc(I);
  end;

  RN := Records[N];

  RN.Op := OP_FIELD;
  RN.Arg1 := GetCurrSelfId(N);
  RN.Arg2 := RN.Res;
  GetSymbolRec(RN.Res).TypeId := GetSymbolRec(SubId).TypeId;
  GetSymbolRec(RN.Res).PatternId := SubId;
  GetSymbolRec(RN.Res).ByRef := true;
end;

procedure TCode.CreateUsingList(J: Integer);
var
  R: TCodeRec;
begin
  using_list.Clear;
  while J > 1 do
  begin
    R := Records[J];
    if R.Op = OP_BEGIN_MODULE then
      Exit;
    if R.Op = OP_BEGIN_USING then
      if R.Arg1 > 0 then
        using_list.Add(R.Arg1);
    Dec(J);
  end;
end;

procedure TCode.CheckTypes;

  procedure ProcessAnotherModules;
  var
    Op: Integer;
  begin
    repeat
      Inc(N);
      Op := Records[N].Op;
    until Op = OP_END_MODULE;
    if N >= Card then
      Exit;
    repeat
      Inc(N);
      Op := Records[N].Op;
      if IsValidOP(Op) then
        ProcList[-Op];
    until N >= Card;
  end;

var
  Op, I, J, K: Integer;
  M: TModule;
  LoadOrder: TIntegerList;
  HasExtraByteCode: Boolean;
  HasInterfaceSection: Boolean;
begin
  LoadOrder := TKernel(kernel).Modules.LoadOrder;

  // process interface sections
  for I := 0 to LoadOrder.Count - 1 do
  begin
    J := LoadOrder[I];
    M := TKernel(kernel).Modules[J];

    M.Recalc;

    // if M.LanguageName = strJavaScriptLanguage then
    // continue;

    HasInterfaceSection := false;
    for K := M.P1 to M.P2 do
      if Records[K].Op = OP_END_INTERFACE_SECTION then
      begin
        HasInterfaceSection := true;
        break;
      end;

    if not HasInterfaceSection then
      continue;

    Op := 0;

    N := M.P1 - 1;
    repeat
      Inc(N);
      if N > Card then
        break;

      Op := Records[N].Op;

      if IsValidOP(Op) then
        ProcList[-Op];

      if HasError then
      begin
        if TKernel(kernel).SignCodeCompletion then
          ProcessAnotherModules;
        Exit;
      end;
    until Op = OP_END_INTERFACE_SECTION;
  end;

  // process implementation sections
  for I := 0 to LoadOrder.Count - 1 do
  begin
    J := LoadOrder[I];
    M := TKernel(kernel).Modules[J];

    M.Recalc;

    N := M.P2;
    if N = 0 then
      N := 1;

    CreateUsingList(N);

    Op := Records[N].Op;
    repeat
      Inc(N);
      if N > Card then
        break;

      Op := Records[N].Op;
      if IsValidOP(Op) then
        ProcList[-Op]
      else
      begin
        // RaiseError(errInternalError, []);
      end;

      if HasError then
      begin
        if TKernel(kernel).SignCodeCompletion then
          ProcessAnotherModules;
        Exit;
      end;
    until Op = OP_END_MODULE;
  end;

  HasExtraByteCode := false;

  for I := N + 1 to Card do
    if Records[I].Op = OP_EXTRA_BYTECODE then
    begin
      N := I;
      HasExtraByteCode := true;
      break;
    end;

  if not HasExtraByteCode then
    Exit;

  repeat
    Inc(N);
    if N > Card then
      break;

    Op := Records[N].Op;
    if IsValidOP(Op) then
      ProcList[-Op]
    else
    begin
      RaiseError(errInternalError, []);
    end;

    if HasError then
      Exit;
  until false;
end;

function TCode.IsValidOP(Op: Integer): Boolean;
begin
  Op := -Op;
  result := (Op >= 0) and (Op < System.Length(ProcList));
end;

procedure TCode.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

procedure TCode.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).RaiseError(Message, params);
end;

function TCode.HasError: Boolean;
begin
  result := TKernel(kernel).HasError;
end;

function TCode.GetModule(I: Integer): TModule;
begin
  if I <= 0 then
    result := nil
  else
    result := TKernel(kernel).Modules[Records[I].ModuleNum];
end;

function TCode.GetSourceLineNumber(I: Integer): Integer;
begin
  result := 0;
  if I <= 0 then
    Exit;
  while Records[I].Op <> OP_SEPARATOR do
  begin
    Dec(I);
    if I <= 0 then
      Exit;
  end;
  result := Records[I].Arg2;
end;

function TCode.GetCurrSourceLineNumber: Integer;
begin
  result := GetSourceLineNumber(N);
end;

function TCode.GetLinePos(I: Integer): Integer;
begin
  result := 0;
  if I <= 0 then
    Exit;
  while Records[I].Op <> OP_SEPARATOR do
  begin
    if Records[I].LinePos >= 0 then
    begin
      result := Records[I].LinePos;
      Exit;
    end;

    Dec(I);
    if I <= 0 then
      Exit;
  end;
end;

function TCode.GetSourceLine(I: Integer): String;
var
  Lines: TStringList;
  K: Integer;
begin
  result := '';
  if I <= 0 then
    Exit;
  while Records[I].Op <> OP_SEPARATOR do
  begin
    Dec(I);
    if I <= 0 then
      Exit;
  end;
  Lines := GetModule(I).Lines;
  K := Records[I].Arg2;
  if K < Lines.Count then
    result := Lines[K]
  else
    result := '';
end;

function TCode.GetIncludedFileName(I: Integer): String;
var
  M: TModule;
begin
  result := '';
  if I <= 0 then
    Exit;
  repeat
    Dec(I);
    if I <= 0 then
      Exit;
    if Records[I].Op = OP_BEGIN_MODULE then
      Exit;
    if Records[I].Op = OP_END_INCLUDED_FILE then
      Exit;
    if Records[I].Op = OP_BEGIN_INCLUDED_FILE then
    begin
      M := GetModule(I);
      I := Records[I].Arg1;
      if I < M.IncludedFiles.Count then
        result := M.IncludedFiles[I];
      Exit;
    end;
  until false;
end;

procedure TCode.ProcessSizeOf;

  function GetMemExpected: Boolean;
  var
    I, Op: Integer;
  begin
    result := false;
    for I := N + 1 to Card do
    begin
      Op := Records[I].GenOp;
      if Op = OP_STMT then
        break;
      if Op = OP_CALL then
        if StrEql(GetSymbolRec(Records[I].Arg1).Name, 'GetMem') then
        begin
          result := true;
          Exit;
        end;
    end;
  end;

var
  R: TCodeRec;
  ConstId: Integer;
  SZ, T: Integer;
  SymbolRec: TSymbolRec;
begin
  N := 1;
  while N < Card do
  begin
    R := Records[N];
    if R.Op = OP_SIZEOF then
    begin
      SymbolRec := GetSymbolRec(R.Arg1);

      if SymbolRec.Kind = KindCONST then
      begin
{$IFNDEF PAXARM}
        if SymbolRec.HasPAnsiCharType then
          SZ := System.Length(SymbolRec.value) + 1
        else
{$ENDIF}
          if SymbolRec.HasPWideCharType then
            SZ := System.Length(SymbolRec.value) * 2 + 1
          else
            SZ := SymbolRec.Size;
      end
      else
      begin
        if (GetSymbolRec(R.Arg1).FinalTypeId = typePOINTER) and GetMemExpected
        then
        begin
          T := GetSymbolRec(GetSymbolRec(R.Arg1).TerminalTypeId).PatternId;
          SZ := GetSymbolRec(T).Size;
        end
        else
          SZ := GetSymbolRec(R.Arg1).PtrSize;
      end;

      ConstId := CreateConst(typeINTEGER, SZ, 0);
      R.Op := OP_ASSIGN_INT_I;
      R.Arg1 := R.Res;
      R.Arg2 := ConstId;
    end;
    Inc(N);
  end;
end;

procedure TCode.ChangeOrderOfActualParams;
var
  L: TCodeRecList;
  need_relocate: Boolean;
  J, SubId, TrueSubId, ParamId: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
  K, N1, T: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  L := TCodeRecList.Create;

  try

    N := Card + 1;
    repeat
      Dec(N);
      if N <= 1 then
        break;

      R := Records[N];
      if R.Op = OP_CALL then
      begin
        SubId := R.Arg1;
        TrueSubId := SubId;
        if SymbolTable[SubId].Kind = KindVAR then
        begin
          T := GetSymbolRec(SubId).TerminalTypeId;
          if GetSymbolRec(T).FinalTypeId in [typePROC, typeEVENT] then
          begin
            TrueSubId := GetSymbolRec(T).PatternId;
          end
          else
            continue;
        end;

        if GetSymbolRec(TrueSubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
          if GetSymbolRec(TrueSubId).Count > 0 then
          begin
            L.Clear;
            need_relocate := false;

            K := 0;

            for J := N - 1 downto 1 do
            begin
              R := Records[J];
              if R.Op = OP_BEGIN_MODULE then
                break;

              if (K = 0) and (R.GenOp = OP_PUSH) and (R.Res = TrueSubId) then
              begin
                ParamId := SymbolTable.GetParamId(TrueSubId, R.Arg2);
                if GetSymbolRec(ParamId).Register > 0 then
                begin
                  if GetSymbolRec(ParamId).FinalTypeId
                    in [typeDYNARRAY, typeOPENARRAY] then
                    if GetSymbolRec(ParamId).Register = _ECX then
                      continue;

                  L.Add(R);
                  need_relocate := true;
                end
                else
                begin
                  need_relocate := true;
                end;
              end
              else if (R.Op = OP_CALL) and (R.Arg1 = SubId) then
              begin
                Inc(K);
              end
              else if (R.Op = OP_BEGIN_CALL) and (R.Arg1 = SubId) then
              begin
                if K = 0 then
                  break
                else
                  Dec(K);
              end;
            end;

            if L.Count = 0 then
              need_relocate := false;

            if need_relocate then
            begin
              N1 := N;
              if (Records[N1 - 1].Op = OP_PUSH_INST) or
                (Records[N1 - 1].Op = OP_PUSH_CLSREF) or
                (Records[N1 - 1].Op = OP_PUSH_DATA) then
                Dec(N1);
              for J := L.Count - 1 downto 0 do
              begin
                R := TCodeRec(L[J]);
                Insert(N1, R.Clone);
                R.Op := OP_NOP;
              end;
            end;
          end;
      end;
    until false;

  finally
    FreeAndNil(L);
  end;
end;

procedure TCode.Optimization;
var
  I, FT, FT1: Integer;
  ok: Boolean;
  GenOp: Integer;
  R, R1: TCodeRec;
begin
  for I := 1 to Card - 1 do
  begin
    R := Records[I];
    R1 := Records[I + 1];

    if (R.Op = OP_NOP) or (R1.Op = OP_NOP) then
      continue;

    ok := R1.GenOp = OP_ASSIGN;
    if ok then
      ok := R.Op <> OP_NOP;

    if R.Op = OP_INTERFACE_FROM_CLASS then
    begin
      if R1.Op = OP_PUSH_INT then
        ok := false;
      if R1.Op = OP_PUSH_INST then
        ok := false;

      if ok then
      begin
        R.Res := R1.Res;
        R1.Op := OP_NOP;
        R1.GenOp := OP_NOP;
      end;
    end;
    if R.Op = OP_INTERFACE_CAST then
    begin
      if R1.Op = OP_PUSH_INT then
        ok := false;
      if R1.Op = OP_PUSH_INST then
        ok := false;

      if ok then
      begin
        R.Res := R1.Res;
        R1.Op := OP_NOP;
        R1.GenOp := OP_NOP;
      end;
    end;

    if ok then
      if R.Res = R1.Arg2 then
      begin
        if generic_binary_operators.IndexOf(R.GenOp) >= 0 then
        begin
          FT := GetSymbolRec(R.Res).FinalTypeId;
          FT1 := GetSymbolRec(R1.Arg1).FinalTypeId;
          if FT = FT1 then
            if not(GetSymbolRec(R.Res).FinalTypeId in DynamicTypes) then
            begin
              R.Res := R1.Res;
              R1.Op := OP_NOP;
              R1.GenOp := OP_NOP;
            end;
        end
        else if generic_unary_operators.IndexOf(R.GenOp) >= 0 then
        begin
          if GetSymbolRec(R.Res).FinalTypeId = GetSymbolRec(R1.Arg1).FinalTypeId
          then
          begin
            R.Res := R1.Res;
            R1.Op := OP_NOP;
            R1.GenOp := OP_NOP;
          end;
        end
      end;

    if (R.Op = OP_SAVE_REGS) and (R1.Op = OP_RESTORE_REGS) then
    begin
      R.Op := OP_NOP;
      R1.Op := OP_NOP;
    end;

    GenOp := R.GenOp;

    if (GenOp = OP_PLUS) or (GenOp = OP_MINUS) or (GenOp = OP_MULT) or
      (GenOp = OP_DIV) then
    begin
      if (R.Arg1 = 0) or (GetSymbolRec(R.Arg1).FinalTypeId = typeCURRENCY) then
        if GetSymbolRec(R.Arg2).FinalTypeId = typeCURRENCY then
        begin
          if R1.Arg1 = R.Res then
          begin
            if GetSymbolRec(R1.Arg1).FinalTypeId = typeCURRENCY then
              if (R1.GenOp = OP_PLUS) or (R1.GenOp = OP_MINUS) or
                (R1.GenOp = OP_MULT) or (R1.GenOp = OP_DIV) then
              begin
                R.Res := 0;
                R1.Arg1 := 0;
              end;
          end
          else if R1.Arg2 = R.Res then
          begin
            if GetSymbolRec(R1.Arg2).FinalTypeId = typeCURRENCY then
              if (R1.GenOp = OP_PLUS) or (R1.GenOp = OP_MULT) then
              begin
                R.Res := 0;
                R1.Arg2 := 0;
                R1.SwapArguments;
              end;
          end;
        end;
    end;
  end;
  RemovePause;
  RemoveNOP;
end;

procedure TCode.Optimization2;
var
  I: Integer;
  ok: Boolean;
  R, R1: TCodeRec;
begin
  for I := 1 to Card - 1 do
  begin
    R := Records[I];
    R1 := Records[I + 1];

    if R1.Op = OP_ONCREATE_HOST_OBJECT then
      R1 := Records[I + 2];

    if (R.Op = OP_NOP) or (R1.Op = OP_NOP) then
      continue;

    ok := (R1.GenOp = OP_ASSIGN);
    if ok then
      ok := R.Op <> OP_NOP;

    if ok then
      if R.Res = R1.Arg2 then
      begin
        if generic_binary_operators.IndexOf(R.GenOp) >= 0 then
        begin
          R.Res := R1.Res;
          R1.Op := OP_NOP;
          R1.GenOp := OP_NOP;
        end
        else if generic_unary_operators.IndexOf(R.GenOp) >= 0 then
        begin
          R.Res := R1.Res;
          R1.Op := OP_NOP;
          R1.GenOp := OP_NOP;
        end
        else if R.Op = OP_CALL then
        begin
          if Records[I + 2].Op = OP_GO_TRUE then
            if Records[I + 2].Arg2 = R.Res then
              continue;

          if Records[I + 2].Op = OP_GO_FALSE then
            if Records[I + 2].Arg2 = R.Res then
              continue;

          if R.Res <> 0 then
          begin
            R.Res := R1.Res;
            R1.Op := OP_NOP;
            R1.GenOp := OP_NOP;
          end;
        end;
      end;
  end;
end;

procedure TCode.RemovePause;
var
  I, L: Integer;
begin
  I := 0;
  while I <= Card do
  begin
    Inc(I);
    if I >= Card - 4 then
      break;
    if Records[I].Op = OP_SEPARATOR then
      if Records[I + 1].Op = OP_SET_CODE_LINE then
        if Records[I + 2].Op = OP_CHECK_PAUSE then
          if Records[I + 3].Op = OP_LABEL then
            if Records[I + 4].Op = OP_SEPARATOR then
            begin
              Records[I + 1].Op := OP_NOP;
              Records[I + 2].Op := OP_NOP;
              Records[I + 2].Op := OP_NOP;
              L := Records[I + 3].Arg1;
              TKernel(kernel).SymbolTable[L].Kind := KindNONE;
              Inc(I, 3);
            end;
  end;
end;

procedure TCode.RemoveNOP;
var
  I, L, Op: Integer;
begin
  for I := Card downto 1 do
  begin
    Op := Records[I].Op;
    if (Op = OP_NOP) or (Op = OP_LVALUE) or (Op = OP_POSTFIX_EXPRESSION) or
      (Op = OP_PRINT_KWD) or (Op = OP_PRINTLN_KWD) or
      (Op = OP_OPTION_EXPLICIT) or
    // (Op = OP_STMT) or
      (Op = OP_MYCLASS) or (Op = OP_MYBASE) or (Op = OP_CHECK_FINAL) or

    // (Op = OP_DECLARE_TEMP_VAR) or
    // (Op = OP_DECLARE_LOCAL_VAR) or

      (Op = OP_END_IMPORT) or (Op = OP_EPILOGUE_GLOBAL_BLOCK2) then
      DeleteRecord(I)
    else if Op = OP_DECLARE_LOCAL_VAR then
    begin
      L := GetSymbolRec(Records[I].Arg2).Level;
      if L > 0 then
        if GetSymbolRec(L).Kind in kindSUBS then
          DeleteRecord(I);
    end
    else if Op = OP_ONCREATE_HOST_OBJECT then
    begin
      if GetSymbolRec(Records[I].Arg1).FinalTypeId <> typeCLASS then
        DeleteRecord(I);
    end;

    // (Op = OP_BEGIN_USING) or
    // (Op = OP_END_USING) or
    // (Op = OP_BEGIN_BLOCK) or
    // (Op = OP_END_BLOCK) or
    // (Op = OP_BEGIN_WITH) or
    // (Op = OP_END_WITH) or
    // (Op = OP_BEGIN_SUB) or
    // (Op = OP_DECLARE_TEMP_VAR) then
  end;
end;

function TCode.GetDeclaredVar(const VarName: String; SubId: Integer;
  Upcase: Boolean; CurrPos: Integer): Integer;
var
  I, Op: Integer;
  R: TCodeRec;
begin
  result := 0;
  for I := CurrPos + 1 downto 1 do
  begin
    R := Records[I];
    Op := R.Op;

    if Op = OP_BEGIN_MODULE then
      break;

    if R.Arg1 <> SubId then
      continue;

    if Op = OP_DECLARE_LOCAL_VAR then
    begin
      if GetSymbolRec(R.Arg2).Kind = KindNONE then
        continue;

      if Upcase then
      begin
        if StrEql(VarName, GetSymbolRec(R.Arg2).Name) then
        begin
          result := R.Arg2;
          Exit;
        end;
      end
      else
      begin
        if VarName = GetSymbolRec(R.Arg2).Name then
        begin
          result := R.Arg2;
          Exit;
        end;
      end;
    end
    else if Op = OP_BEGIN_SUB then
      Exit;
  end;
end;

function TCode.ExistsImplicitNumericConversion(type_id_source,
  type_id_dest: Integer): Boolean;
begin
  if type_id_source in IntegerTypes then
    if type_id_dest in (IntegerTypes + RealTypes) then
    begin
      result := true;
      Exit;
    end;

  if type_id_source in RealTypes then
    if type_id_dest in RealTypes then
    begin
      result := true;
      Exit;
    end;

  result := false;
end;

function TCode.ExistsImplicitConversion(id_source, id_dest: Integer): Boolean;
var
  symbol_table: TSymbolTable;
  T1, T2: Integer;
begin
  symbol_table := TKernel(kernel).SymbolTable;

  if symbol_table[id_source].TerminalTypeId = symbol_table[id_dest].TerminalTypeId
  then
  begin
    result := true;
    Exit;
  end;

  if symbol_table[id_dest].TerminalTypeId = typePVOID then
  begin
    result := true;
    Exit;
  end;

  T1 := symbol_table[id_source].FinalTypeId;
  T2 := symbol_table[id_dest].FinalTypeId;

  if (T2 = typeOPENARRAY) and (T1 = typeSET) then
  begin
    result := true;
    Exit;
  end;

  if T1 in VariantTypes then
    if T2 in (IntegerTypes + RealTypes + [typeCURRENCY] + BooleanTypes +
      StringTypes) then
    begin
      result := true;
      Exit;
    end;

  if T1 in (IntegerTypes + RealTypes + BooleanTypes + StringTypes) then
    if T2 in VariantTypes then
    begin
      result := true;
      Exit;
    end;

  if T1 in StringTypes then
    if T2 in StringTypes then
    begin
      result := true;
      Exit;
    end;

  if T1 in CharTypes then
    if T2 in (CharTypes + StringTypes) then
    begin
      result := true;
      Exit;
    end;

  if (T1 = typeINTERFACE) and (T2 = typeINTERFACE) then
  begin
    result := true;
    Exit;
  end;

  if (T1 = typeCLASS) and (T2 = typeCLASS) then
  begin
    result := true;
    Exit;
  end;

{$IFNDEF PAXARM}
  if symbol_table[id_source].HasPAnsiCharType and (T2 in StringTypes) then
  begin
    result := true;
    Exit;
  end;
{$ENDIF}
  if symbol_table[id_source].HasPWideCharType and (T2 in StringTypes) then
  begin
    result := true;
    Exit;
  end;

  if (id_source = symbol_table.NilId) and
    (T2 in [typeCLASS, typeCLASSREF, typeINTERFACE]) then
  begin
    result := true;
    Exit;
  end;

  result := ExistsImplicitNumericConversion(T1, T2);
end;

/// <summary>
/// Returns 1 or 2, if conversion between types of id and id1 is better than
/// conversion between types of id and id2.
/// Returns -1 or -2, if conversion between types of id and id2 is better than
/// conversion between types of id and id1.
/// Returns 0, if there is no better conversion.
/// </summary>
function TCode.CompareConversions(Id, id1, id2: Integer): Integer;
var
  symbol_table: TSymbolTable;
  S, T1, T2, K: Integer;
  B1, B2: Boolean;
begin
  symbol_table := TKernel(kernel).SymbolTable;
  S := symbol_table[Id].FinalTypeId;

  K := symbol_table[Id].Kind;
  if K = KindTYPE then
    if symbol_table[Id].FinalTypeId = typeCLASS then
    begin
      Inc(Id);
      S := symbol_table[Id].FinalTypeId;
    end;

  T1 := symbol_table[id1].FinalTypeId;
  T2 := symbol_table[id2].FinalTypeId;

  result := 0;

  if T1 = T2 then
  begin
    if T1 = typeENUM then
    begin
      T1 := symbol_table[id1].TerminalTypeId;
      T2 := symbol_table[id2].TerminalTypeId;
      S := symbol_table[Id].TerminalTypeId;

      if T1 = T2 then
        Exit;
      B1 := S = T1;
      B2 := S = T2;
      if B1 then
        Inc(result, 2);
      if B2 then
        Dec(result, 2);
      Exit;
    end
    else if T1 = typeCLASS then
    begin
      T1 := symbol_table[id1].TerminalTypeId;
      T2 := symbol_table[id2].TerminalTypeId;

      if T1 = T2 then
        Exit;

      S := symbol_table[Id].TerminalTypeId;

      B1 := symbol_table.Inherits(S, T1);
      B2 := symbol_table.Inherits(S, T2);

      if B1 and B2 then
      begin
        B1 := S = T1;
        B2 := S = T2;
        if B1 then
          Inc(result, 2);
        if B2 then
          Dec(result, 2);
      end
      else if B1 then
      begin
        result := 2;
      end
      else if B2 then
      begin
        result := -2;
      end;
      Exit;
    end
    else if T1 = typeINTERFACE then
    begin
      T1 := symbol_table[id1].TerminalTypeId;
      T2 := symbol_table[id2].TerminalTypeId;

      if T1 = T2 then
        Exit;

      S := symbol_table[Id].TerminalTypeId;

      B1 := symbol_table.Supports(S, T1);
      B2 := symbol_table.Supports(S, T2);

      if B1 and B2 then
      begin
        B1 := S = T1;
        B2 := S = T2;
        if B1 then
          Inc(result, 2);
        if B2 then
          Dec(result, 2);
      end
      else if B1 then
      begin
        result := 2;
      end
      else if B2 then
      begin
        result := -2;
      end;
    end;
    Exit;
  end;

  if S = T1 then
  begin
    result := 1;
    if not ExistsImplicitConversion(Id, id2) then
      Inc(result);
    Exit;
  end;

  if S = T2 then
  begin
    result := -1;
    if not ExistsImplicitConversion(Id, id1) then
      Dec(result);
    Exit;
  end;

  if ExistsImplicitConversion(Id, id1) then
  begin
    if ExistsImplicitConversion(Id, id2) then
      result := 0
    else
      result := 2;
    Exit;
  end;

  if ExistsImplicitConversion(Id, id2) then
  begin
    if ExistsImplicitConversion(Id, id1) then
      result := 0
    else
      result := -2;
    Exit;
  end;
  result := 0;
end;

procedure TCode.RemoveInstruction(Op, Arg1, Arg2, Res: Integer);
var
  I: Integer;
begin
  for I := Card downto 1 do
    if Records[I].Op = Op then
    begin
      if not((Arg1 = -1) or (Arg1 = Records[I].Arg1)) then
        continue;
      if not((Arg2 = -1) or (Arg2 = Records[I].Arg2)) then
        continue;
      if not((Res = -1) or (Res = Records[I].Res)) then
        continue;
      Records[I].Op := OP_NOP;
      Exit;
    end;
end;

function TCode.RemoveLastEvalInstruction(const S: String;
  Upcase: Boolean = true): Integer;
var
  I, Id, K: Integer;
  Q: String;
  b: Boolean;
begin
  result := 0;
  K := 0;
  for I := Card downto 1 do
  begin
    if Records[I].Op = OP_STMT then
    begin
      Inc(K);
      if K > 1 then
        break;
    end
    else if Records[I].Op = OP_EVAL then
    begin
      Id := Records[I].Res;
      Q := GetSymbolRec(Id).Name;
      if Upcase then
        b := StrEql(S, Q)
      else
        b := S = Q;
      if b then
      begin
        result := Id;
        Records[I].Op := OP_NOP;
        Exit;
      end;
    end;
  end;
end;

procedure TCode.CreateMapping(result: TMapTable; Host: Boolean;
  HostMapTable, ScriptMapTable: TMapTable);

var
  CurrN: Integer;

  procedure AddSubDesc(MapRec: TMapRec; SubId: Integer);
  var
    I, SelfId, ParamId, ResTypeId, ParamTypeId, L, ElemTypeId: Integer;
    R: TSymbolRec;
    SymbolTable: TSymbolTable;
    SubParamRec: TSubParamRec;
    SubLocalVarRec: TSubLocalVarRec;
    S: String;
    CR: TCodeRec;
  begin
    R := GetSymbolRec(SubId);
    if not(R.Kind in kindSUBS) then
      Exit;

    SymbolTable := TKernel(kernel).SymbolTable;

{$IFDEF PCU_EX}
    if not Host then
      for I := 1 to Card do
      begin
        CR := Records[I];

        if CR.Arg1 = SubId then
        begin
          if CR.Op = OP_INIT_SUB then
            MapRec.SubDesc.N1 := I
          else if CR.Op = OP_FIN_SUB then
            MapRec.SubDesc.N2 := I;
        end;
      end;
{$ENDIF}
    MapRec.SubDesc.Sid := SubId;
    MapRec.SubDesc.CallConv := R.CallConv;
    MapRec.SubDesc.RetSize := SymbolTable.GetSizeOfParams(SubId);
    MapRec.SubDesc.ResTypeId := R.FinalTypeId;
    ResTypeId := R.TypeId;
    MapRec.SubDesc.ResTypeName := GetSymbolRec(ResTypeId).Name;
    MapRec.Vis := R.Vis;
    MapRec.SubDesc.IsShared := R.IsSharedMethod;

    L := R.Level;
    if L = 0 then
      MapRec.SubDesc.IsMethod := false
    else if GetSymbolRec(L).Kind = KindTYPE then
      MapRec.SubDesc.IsMethod := true
    else
      MapRec.SubDesc.IsMethod := false;

    if MapRec.SubDesc.IsMethod then
    begin
      SelfId := SymbolTable.GetSelfId(SubId);
      MapRec.SubDesc.SelfOffset := GetSymbolRec(SelfId).Shift;
    end;

    if Records[CurrN].Op = OP_LOAD_PROC then
    begin
      S := GetSymbolRec(Records[CurrN].Res).value;
      if not StrEql(ExtractFileExt(S), '.' + PCU_FILE_EXT) then
      begin
        MapRec.SubDesc.DllName := S;
        MapRec.SubDesc.AliasName := GetSymbolRec(Records[CurrN].Arg2).value;
      end;
    end;

    for I := 0 to R.Count - 1 do
    begin
      ParamId := SymbolTable.GetParamId(SubId, I);
      SubParamRec := MapRec.SubDesc.ParamList.AddRecord;
      SubParamRec.FinTypeId := SymbolTable[ParamId].FinalTypeId;
      SubParamRec.ParamSize := SymbolTable[ParamId].FinSize;
      SubParamRec.ParamName := SymbolTable[ParamId].Name;
      SubParamRec.ParamOffset := SymbolTable[ParamId].Shift;
      ParamTypeId := SymbolTable[ParamId].TypeId;
      SubParamRec.ParamTypeName := SymbolTable[ParamTypeId].Name;
      if SymbolTable[ParamId].FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
        if SymbolTable[ParamId].IsOpenArray then
        begin
          ParamTypeId := SymbolTable[ParamId].TerminalTypeId;
          ElemTypeId := SymbolTable[ParamTypeId].PatternId;
          SubParamRec.ParamTypeName := 'array of ' + SymbolTable
            [ElemTypeId].Name;
        end;

      if SymbolTable[ParamId].ByRef then
        SubParamRec.ParamMod := PM_BYREF
      else if SymbolTable[ParamId].IsConst then
        SubParamRec.ParamMod := PM_CONST
      else
        SubParamRec.ParamMod := PM_BYVAL;

      if SymbolTable[ParamId].Optional then
        SubParamRec.OptValue :=
          VariantToString(SymbolTable[ParamId].FinalTypeId,
          SymbolTable[ParamId].value);
    end;

{$IFDEF PCU_EX}
    if not Host then
      for I := SubId + 1 to SymbolTable.Card do
        if SymbolTable.IsLocalOf(I, SubId) then
        begin
          SubLocalVarRec := MapRec.SubDesc.LocalVarList.AddRecord;
          SubLocalVarRec.LocalVarName := SymbolTable[I].Name;
          SubLocalVarRec.LocalVarOffset := SymbolTable[I].Shift;
          SubLocalVarRec.IsByRef := SymbolTable[I].ByRef;
          ParamTypeId := SymbolTable[I].TypeId;
          SubLocalVarRec.LocalVarTypeName := SymbolTable[ParamTypeId].Name;
        end
        else if GetSymbolRec(I).Kind = KindNAMESPACE then
          break;
{$ENDIF}
  end;

procedure TryMap(Id: Integer); forward;

  procedure AddDescendats(Id: Integer);
  var
    SymbolTable: TSymbolTable;
    I, J, T: Integer;
    List: TIntegerList;
    S: String;
  begin
    SymbolTable := TKernel(kernel).SymbolTable;
    S := SymbolTable[Id].Name;
    T := SymbolTable[Id].Level;
    List := SymbolTable.HashArray.GetList(S);

    for J := List.Count - 1 downto 0 do
    begin
      I := List[J];
      if I <> Id then
        with SymbolTable[I] do
          if Kind = KindSUB then
            if SymbolTable[Level].Kind = KindTYPE then
              if SymbolTable[Level].FinalTypeId = typeCLASS then
                if StrEql(Name, S) then
                  if SymbolTable.Inherits(Level, T) then
                  begin
                    SymbolTable.InCode[I] := true;
                    TryMap(I);
                  end;
    end;
  end;

  procedure AddIUnknownMethods(Id: Integer);
  var
    SymbolTable: TSymbolTable;
    I, J, L, KK: Integer;
    List: TIntegerList;
    S: String;
    MapRec: TMapRec;
    R: TSymbolRec;
  begin
    if Host then
      Exit;

    SymbolTable := TKernel(kernel).SymbolTable;

    for KK := 1 to 3 do
    begin
      S := '';
      case KK of
        1:
          S := 'QueryInterface';
        2:
          S := '_AddRef';
        3:
          S := '_Release';
      end;

      List := SymbolTable.HashArray.GetList(S);

      for J := List.Count - 1 downto 0 do
      begin
        I := List[J];
        if I <> Id then
          if SymbolTable[I].Kind = KindSUB then
          begin
            L := SymbolTable[I].Level;
            if SymbolTable[L].Kind = KindTYPE then
              if SymbolTable[L].FinalTypeId = typeCLASS then
                if StrEql(SymbolTable[I].Name, S) then
                begin
                  if SymbolTable.Inherits(Id, L) then
                  begin
                    R := SymbolTable[I];

                    SymbolTable.InCode[I] := true;
                    if R.Host then
                    begin
                      MapRec := HostMapTable.AddRec(R.FullName, R.Shift, -1,
                        KindSUB, false, 0, 0);
                    end
                    else
                      MapRec := ScriptMapTable.AddRec(R.FullName, R.value, -1,
                        KindSUB, false, 0, 0);

                    AddSubDesc(MapRec, Id);

                    break;
                  end;
                end;
          end;
      end;
    end;
  end;

  procedure TryMap(Id: Integer);
  var
    SymbolTable: TSymbolTable;
    Global: Boolean;
    R, R1: TSymbolRec;
    T: Integer;
    MapRec: TMapRec;
    Lst: TIntegerList;
    J: Integer;
  begin
    SymbolTable := TKernel(kernel).SymbolTable;

    if Id > SymbolTable.Card then
      Exit;

    if Id <= StdCard then
      Exit;

    R := SymbolTable[Id];

    if R.FullName = '' then
    begin
      if R.FinalTypeId = typeCLASS then
      begin
        T := R.TerminalTypeId;
        if GetSymbolRec(T).Name <> '' then
          TryMap(T);
      end;

      if R.FinalTypeId = typeCLASSREF then
      begin
        T := R.TerminalTypeId;
        T := GetSymbolRec(T).PatternId;
        if GetSymbolRec(T).Name <> '' then
          TryMap(T);
      end;

      if R.FinalTypeId = typeINTERFACE then
      begin
        T := R.TerminalTypeId;
        SymbolTable.InCode[T] := true;
      end;

      Exit;
    end;

    if ExtractName(R.FullName) = '' then
    begin
      if R.FinalTypeId = typeCLASS then
      begin
        T := R.TerminalTypeId;
        if GetSymbolRec(T).Name <> '' then
          TryMap(T);
      end;

      if R.FinalTypeId = typeINTERFACE then
      begin
        T := R.TerminalTypeId;
        SymbolTable.InCode[T] := true;
      end;

      Exit;
    end;

    if R.TypedConst and (Host = false) then
    begin
      // Exit;
    end;

    if Id <= TKernel(kernel).GT.Card then
      Global := true
    else
      Global := false;

    if result.LookUpEx(R.FullName, R.OverCount) <> nil then
      Exit;

    if R.Kind = KindNAMESPACE then
    begin
      if R.Host <> Host then
        Exit;

      result.AddRec(R.FullName, 0, -1, R.Kind, Global, 0, 0);
      Exit;
    end;

    if R.ClassIndex <> -1 then
    begin
      if R.Host <> Host then
        Exit;

      R1 := SymbolTable[Id + 1];
      MapRec := result.AddRec(R.FullName, R1.Shift, R.ClassIndex, KindTYPE,
        Global, 0, 0);

      if not Host then
        SymbolTable.AddScriptFields(Id, MapRec.FieldList);

      SymbolTable.InCode[Id] := true;

      if R.SupportedInterfaces <> nil then
        if R.SupportedInterfaces.Count > 0 then
          AddIUnknownMethods(Id);
      {
        if not R.Host then
        if GetSymbolRec(R.AncestorId).Host then
        if HostMapTable <> nil then
        if R.AncestorId <> H_TObject then
        begin
        R := GetSymbolRec(R.AncestorId);
        if HostMapTable.Lookup(R.FullName) <> nil then
        Exit;

        Id := R.Id;
        SymbolTable.InCode[Id] := true;
        R1 := GetSymbolRec(Id + 1);
        HostMapTable.AddRec(R.FullName, R1.Shift, R.ClassIndex, KindTYPE, Global, 0, 0);
        end;
      }
      Exit;
    end;

    if R.IsGlobalVarEx then
    begin
      if R.FinalTypeId = typeCLASS then
      begin
        T := R.TerminalTypeId;
        if GetSymbolRec(T).Name <> '' then
          TryMap(T);
      end;

      if R.FinalTypeId = typeINTERFACE then
      begin
        T := R.TerminalTypeId;
        SymbolTable.InCode[T] := true;
      end;

      if R.Host <> Host then
        Exit;

      MapRec := result.AddRec(R.FullName, R.Shift, -1, R.Kind, Global, 0, 0);
      MapRec.TypedConst := R.TypedConst;
      MapRec.FullTypeName := GetSymbolRec(R.TypeId).FullName;
    end
    else if R.Kind in kindSUBS then
    begin
      if not(R.IsSubPartOfEventType or R.IsSubPartOfProcType) then
        if not R.IsNestedSub then
          if R.Name <> '' then
          begin
            if R.Level > 0 then
            begin
              if GetSymbolRec(R.Level).FinalTypeId = typeINTERFACE then
                Exit;

              if GetSymbolRec(R.Level).FinalTypeId = typeCLASS then
                TryMap(R.Level);
            end;

            if R.Host <> Host then
            begin
              if not R.IsExternal then
                Exit;
              if Host then
                Exit;
            end;

            if R.IsExternal then
            begin
              MapRec := result.AddRec(R.FullName, 0, -1, R.Kind, Global,
                R.OverCount, R.CallMode);
              MapRec.IsExternal := true;
            end
            else if Host then
            begin
              MapRec := result.AddRec(R.FullName, R.Shift, -1, R.Kind, Global,
                R.OverCount, R.CallMode);
            end
            else
              MapRec := result.AddRec(R.FullName, R.value, -1, R.Kind, Global,
                R.OverCount, R.CallMode);

            AddSubDesc(MapRec, Id);

            if R.IsVirtual and Host then
            begin
              // AddDescendats(Id);
            end;

            if R.OverCount > 0 then
            begin
              Lst := SymbolTable.LookUpAll(SymbolTable[Id].Name,
                SymbolTable[Id].Level, true);
              try
                for J := 0 to Lst.Count - 1 do
                  if Lst[J] <> Id then
                  begin
                    SymbolTable.InCode[Lst[J]] := true;
                    TryMap(Lst[J]);
                  end;
              finally
                FreeAndNil(Lst);
              end;
            end;
          end
    end
    else if R.FinalTypeId = typeCLASS then
    begin
      T := R.TerminalTypeId;
      if GetSymbolRec(T).Name <> '' then
        TryMap(T);
    end
    else if R.FinalTypeId = typeINTERFACE then
    begin
      T := R.TerminalTypeId;
      SymbolTable.InCode[T] := true;
    end
    else if R.Level > 0 then
    begin
      if GetSymbolRec(R.Level).FinalTypeId = typeCLASS then
      begin
        T := GetSymbolRec(R.Level).TerminalTypeId;
        if GetSymbolRec(T).Name <> '' then
          TryMap(T);
      end
      else if R.FinalTypeId = typeEVENT then
      begin
        Id := R.PatternId;
        if Id > 0 then
        begin
          R := GetSymbolRec(Id);
          if R.Kind <> KindSUB then
            Exit;

          if R.Host then
            Exit;

          SymbolTable.InCode[Id] := true;
          if R.Host then
          begin
            MapRec := HostMapTable.AddRec(R.FullName, R.Shift, -1, KindSUB,
              false, 0, 0);
          end
          else
          begin
            if ScriptMapTable = nil then
              Exit;
            MapRec := ScriptMapTable.AddRec(R.FullName, R.value, -1, KindSUB,
              false, 0, 0);
          end;

          AddSubDesc(MapRec, Id);
        end;
      end;
    end;
  end;

var
  I, T: Integer;
  R: TCodeRec;
begin
  result.Clear;

  for I := 1 to Card do
  begin
    CurrN := I;
    R := Records[I];

    if R.Op = OP_SET_CODE_LINE then
      continue;
    if R.Op = OP_SEPARATOR then
      continue;

    TryMap(R.Arg1);
    TryMap(R.Arg2);
    TryMap(R.Res);

    if R.Op = OP_PUSH_CLSREF then
      TryMap(R.Arg1 - 1)
    else if R.Op = OP_IS then
    begin
      T := GetSymbolRec(R.Arg1).TypeId;
      TryMap(T);
    end;
  end;

  for I := 0 to map_list.Count - 1 do
    TryMap(map_list[I]);
end;

procedure TCode.CreateExportList(MapTable: TMapTable);
var
  I: Integer;
  S: String;
  MR: TMapRec;
begin
  for I := 1 to Card do
    if Records[I].Op = OP_EXPORTS then
    begin
      S := GetSymbolRec(Records[I].Arg1).FullName;
      MR := MapTable.LookUp(S);
      if MR = nil then
        RaiseError(errInternalError, []);
      S := ExtractName(S);
      with TKernel(kernel).ExportList.Add do
      begin
        Name := S;
        Offset := MR.Offset;
        Ordinal := TKernel(kernel).ExportList.Count - 1;
      end;
    end;
end;

procedure TCode.GenHostStructConst;

var
  ModuleNum: Integer;
  Language: Integer;
  Upcase: Boolean;
  SymbolTable: TSymbolTable;

  procedure Gen(Op, Arg1, Arg2, Res, Lev: Integer);
  begin
    Add(Op, Arg1, Arg2, Res, Lev, Upcase, Language, ModuleNum, -1);
  end;

var
  InCode: array of Boolean;

  procedure SetInCode;
  var
    I, K, Id: Integer;
  begin
    K := TKernel(kernel).SymbolTable.Card;

    SetLength(InCode, K + K);

    for I := 1 to Card do
    begin

      Id := Self[I].Arg1;
      if Id > 0 then
        if Id <= K then
          InCode[Id] := true;

      Id := Self[I].Arg2;
      if Id > 0 then
        if Id <= K then
          InCode[Id] := true;

      Id := Self[I].Res;
      if Id > 0 then
        if Id <= K then
          InCode[Id] := true;

    end;
  end;

  procedure Init(I: Integer; const V: Variant);
  var
    ArrObject: TArrObject;
    VarObject: TVarObject;
    SimpleObject: TSimpleObject;
    NameId: Integer;
    ItemId: Integer;
    ValId: Integer;
    value: Variant;
    ValueTypeId: Integer;
    J: Integer;
    FT: Integer;
  begin
    FT := SymbolTable[I].FinalTypeId;
    SymbolTable[I].Kind := KindVAR;
    SymbolTable[I].TypedConst := true;

    Gen(OP_BEGIN_INIT_CONST, I, 0, 0, 0);
    ArrObject := VariantToVarObject(V) as TArrObject;
    for J := 0 to ArrObject.Count - 1 do
    begin
      VarObject := ArrObject[J];
      if VarObject is TSimpleObject then
      begin
        SimpleObject := VarObject as TSimpleObject;
        ItemId := NewTempVar(0, 0);
        value := SimpleObject.value;
        ValueTypeId := 0;
        case VarType(value) of
          varByte, varInteger:
            ValueTypeId := typeINTEGER;
{$IFDEF VARIANTS}
          varInt64:
            ValueTypeId := typeINT64;
          varLongWord:
            ValueTypeId := typeCARDINAL;
{$ENDIF}
{$IFNDEF PAXARM}
          varString:
            ValueTypeId := typeANSISTRING;
{$ENDIF}
          varBoolean:
            ValueTypeId := typeBOOLEAN;
          varDouble:
            ValueTypeId := typeDOUBLE;
        else
          RaiseError(errInternalError, []);
        end;
        ValId := CreateConst(ValueTypeId, value);

        if FT = typeRECORD then
        begin
          NameId := CreateConst(typeSTRING, SimpleObject.Name);
          Gen(OP_RECORD_ITEM, I, NameId, ItemId, 0);
        end
        else
          Gen(OP_ITEM, I, J, ItemId, 0);

        Gen(OP_ASSIGN, ItemId, ValId, ItemId, 0);
      end
      else if VarObject is TArrObject then
      begin
        ItemId := NewTempVar(0, 0);
        Init(ItemId, VarObjectToVariant(VarObject as TArrObject));
        Gen(OP_ASSIGN_SHIFT, 0, J, ItemId, 0);
      end;
    end;
    Gen(OP_END_INIT_CONST, I, 0, 0, 0);
  end;

var
  I, FT: Integer;
  SymbolRec: TSymbolRec;
  V: Variant;
  K: Integer;
  K1, K2, KK: Integer;
begin
  ModuleNum := GetModuleNumber(Card);
  Language := GetLanguage(Card);
  Upcase := GetUpcase(Card);

  SetInCode;

  K := Card;
  while Records[K].Op <> OP_END_MODULE do
    Dec(K);

  SymbolTable := TKernel(kernel).SymbolTable;

  for KK := 1 to 2 do
  begin
    if KK = 1 then
    begin
      K1 := Types.Count;
      K2 := TKernel(kernel).GT.Card;
    end
    else
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end;

    for I := K1 to K2 do
    begin
      SymbolRec := SymbolTable[I];

      if SymbolRec.Kind = KindCONST then
      begin
        FT := SymbolRec.FinalTypeId;
        if FT in [typeRECORD, typeARRAY] then
        begin
          V := SymbolRec.value;
          if VarType(V) <> PAXCOMP_VAROBJECT.VarObject then
            continue;

          if not InCode[I] then
            continue;

          Init(I, V);
        end
        else if I <= SymbolTable.CompileCard then
        begin
{$IFNDEF PAXARM}
          if FT = typeSHORTSTRING then
          begin
            if not InCode[I] then
              continue;

            SymbolTable[I].Kind := KindVAR;
            SymbolTable[I].TypedConst := true;
            Gen(OP_BEGIN_INIT_CONST, I, 0, 0, 0);
            Gen(OP_ASSIGN, I, SymbolTable.AddPAnsiCharConst
              (AnsiString(SymbolTable[I].value)).Id, I, 0);
            Gen(OP_END_INIT_CONST, I, 0, 0, 0);
          end;
{$ENDIF}
        end;
      end;
    end;
  end;

  Records[K].Op := OP_NOP;
  Gen(OP_END_MODULE, ModuleNum, 0, 0, 0);
end;

function TCode.PrevN(J: Integer): Integer;
begin
  result := 0;

  J := J - 1;
  if Records[J].Op <> OP_SEPARATOR then
  begin
    result := J;
    Exit;
  end;

  while J > 1 do
  begin
    Dec(J);
    if Records[J].Op <> OP_SEPARATOR then
    begin
      result := J;
      Exit;
    end;
  end;
end;

function TCode.PrevPrevRec(J: Integer): TCodeRec;
begin
  result := PrevRec(PrevN(J));
end;

function TCode.PrevRec(J: Integer): TCodeRec;
begin
  result := nil;

  J := J - 1;
  if Records[J].Op <> OP_SEPARATOR then
  begin
    result := Records[J];
    Exit;
  end;

  while J > 1 do
  begin
    Dec(J);
    if Records[J].Op <> OP_SEPARATOR then
    begin
      result := Records[J];
      Exit;
    end;
  end;

end;

function TCode.NextRec(J: Integer): TCodeRec;
begin
  result := nil;

  J := J + 1;
  if Records[J].Op <> OP_SEPARATOR then
  begin
    result := Records[J];
    Exit;
  end;

  while J < Card do
  begin
    Inc(J);
    if Records[J].Op <> OP_SEPARATOR then
    begin
      result := Records[J];
      Exit;
    end;
  end;
end;

function TCode.GetCurrSubId(CurrN: Integer): Integer;
var
  I, K, Op: Integer;
  R: TCodeRec;
begin
  result := 0;
  K := 0;
  for I := CurrN downto 1 do
  begin
    R := Records[I];
    Op := R.Op;
    if Op = OP_INIT_SUB then
    begin
      if K = 0 then
      begin
        result := R.Arg1;
        Exit;
      end
      else
        Dec(K);
    end
    else if Op = OP_END_SUB then
      Inc(K)
    else if Op = OP_BEGIN_MODULE then
      Exit;
  end;
end;

function TCode.GetCurrSelfId(CurrN: Integer): Integer;
var
  SubId, L: Integer;
begin
  result := 0;
  SubId := GetCurrSubId(CurrN);
  if SubId = 0 then
    Exit;
  if GetSymbolRec(SubId).IsJSFunction then
  begin
    result := TKernel(kernel).SymbolTable.GetSelfId(SubId);
    Exit;
  end;

  L := SubId;
  repeat
    L := GetSymbolRec(L).Level;
    if L = 0 then
      Exit;
    if GetSymbolRec(L).Kind = KindTYPE then
    begin
      result := TKernel(kernel).SymbolTable.GetSelfId(SubId);
      Exit;
    end
    else if not(GetSymbolRec(L).Kind in kindSUBS) then // nested sub
      Exit;
  until false;
end;

procedure TCode.LocateDummyName(var NN: Integer);
var
  I, J, SubId: Integer;
  R, RJ: TCodeRec;
begin
  NN := -1;
  for I := Card downto 1 do
  begin
    R := Records[I];

    if R.GenOp = OP_PUSH then
    begin
      if GetSymbolRec(R.Arg1).Name = DummyName then
      begin
        SubId := R.Res;

        for J := I + 1 to Card do
        begin
          RJ := Records[J];
          if (RJ.Op = OP_CALL) or (RJ.Op = OP_STR) then
            if RJ.Arg1 = SubId then
            begin
              NN := J;
              Exit;
            end;
        end;

        for J := I - 1 downto 1 do
        begin
          RJ := Records[J];
          if RJ.Op = OP_BEGIN_CALL then
            if RJ.Arg1 = SubId then
            begin
              NN := J;
              Exit;
            end;
        end;
      end;
    end
    else if R.GenOp = OP_FIELD then
    begin
      if GetSymbolRec(R.Arg2).Name = DummyName then
      begin
        NN := I;
        Exit;
      end;
    end
    else if R.GenOp = OP_EVAL then
    begin
      if GetSymbolRec(R.Res).Name = DummyName then
      begin
        R.Op := OP_EVAL;
        NN := I;
        Exit;
      end;
    end
    else if (R.GenOp = OP_ABS) or (R.GenOp = OP_INC) or (R.GenOp = OP_DEC) or
      (R.GenOp = OP_PRED) or (R.GenOp = OP_SUCC) or (R.GenOp = OP_ORD) or
      (R.GenOp = OP_SIZEOF) or (R.GenOp = OP_CHR) or (R.GenOp = OP_LOW) or
      (R.GenOp = OP_HIGH) or (R.GenOp = OP_ASSIGNED) or (R.GenOp = OP_STR) or
      (R.GenOp = OP_PRINT) or (R.GenOp = OP_PRINT_EX) then
    begin
      if GetSymbolRec(R.Arg1).Name = DummyName then
      begin
        NN := I;
        Exit;
      end;
    end;
  end;
end;

function TCode.FindRecord1(Op, Arg1: Integer): Integer;
var
  I: Integer;
  R: TCodeRec;
begin
  result := 0;
  for I := 1 to Card do
  begin
    R := Records[I];
    if R.Op = Op then
      if R.Arg1 = Arg1 then
      begin
        result := I;
        Exit;
      end;
  end;
end;

procedure TCode.InsertDynamicTypeDestructors;
var
  I, IStart, J, T, TypeId, Id, Level, Op: Integer;
  R: TCodeRec;
  WasInsertion: Boolean;
  Upcase: Boolean;
  Language, ModuleNum: Integer;
begin
  dmp;
  for I := 1 to Card do
  begin
    R := Records[I];
    if R.Op = OP_DESTROY_LOCAL_VAR then
    begin
      if not GetSymbolRec(R.Arg1).IsExternal then
      begin
        T := GetSymbolRec(R.Arg1).FinalTypeId;
        case T of
          typeVARIANT, typeOLEVARIANT:
            R.Op := OP_VARIANT_CLR;
{$IFNDEF PAXARM}
          typeANSISTRING:
            R.Op := OP_ANSISTRING_CLR;
          typeWIDESTRING:
            R.Op := OP_WIDESTRING_CLR;
{$ENDIF}
          typeUNICSTRING:
            R.Op := OP_UNICSTRING_CLR;
          typeINTERFACE:
            R.Op := OP_INTERFACE_CLR;
          typeDYNARRAY:
            R.Op := OP_DYNARRAY_CLR;
          typeRECORD, typeARRAY:
            begin
              TypeId := GetSymbolRec(R.Arg1).TerminalTypeId;
              if TKernel(kernel).SymbolTable.HasDynamicFields(TypeId) then
                R.Op := OP_STRUCTURE_CLR
              else
                R.Op := OP_NOP;
            end;
          typeCLASS:
            begin
{$IFDEF PAXARM}
              R.Op := OP_CLASS_CLR;
{$ELSE}
              TypeId := GetSymbolRec(R.Arg1).TerminalTypeId;
              if IsJSType(TypeId, TKernel(kernel).SymbolTable) then
                R.Op := OP_CLASS_CLR
              else
                R.Op := OP_NOP;
{$ENDIF}
            end
        else
          R.Op := OP_NOP;
        end;
      end
      else
        R.Op := OP_NOP;
    end;
  end;

  IStart := 1;
  repeat
    WasInsertion := false;

    I := IStart - 1;
    while I < Card do
    begin
      Inc(I);

      R := Records[I];
      if (R.Op = OP_DECLARE_TEMP_VAR) or (R.Op = OP_OLE_GET) then
      begin
        if not GetSymbolRec(R.Arg2).IsExternal then
        begin
          if R.Op = OP_OLE_GET then
            Id := R.Res
          else
            Id := R.Arg2;
          Level := GetSymbolRec(Id).Level;
          Op := OP_NOP;

          T := GetSymbolRec(Id).FinalTypeId;
          case T of
            typeVARIANT, typeOLEVARIANT:
              Op := OP_VARIANT_CLR;
{$IFNDEF PAXARM}
            typeANSISTRING:
              Op := OP_ANSISTRING_CLR;
            typeWIDESTRING:
              Op := OP_WIDESTRING_CLR;
{$ENDIF}
            typeUNICSTRING:
              Op := OP_UNICSTRING_CLR;
            typeINTERFACE:
              Op := OP_INTERFACE_CLR;
{$IFDEF PAXARM}
            typeCLASS:
              Op := OP_CLASS_CLR;
{$ENDIF}
            typeDYNARRAY:
              Op := OP_DYNARRAY_CLR;
            typeRECORD, typeARRAY:
              begin
                TypeId := GetSymbolRec(Id).TerminalTypeId;
                if TKernel(kernel).SymbolTable.HasDynamicFields(TypeId) then
                  Op := OP_STRUCTURE_CLR
                else
                  Op := OP_NOP;
              end;
          end;

          if Op = OP_NOP then
          begin
            // break;
            Inc(I);
            continue;
          end;

          // find insertion point

          if (Level = 0) or (GetSymbolRec(Level).Kind in [KindNONE, KindVAR,
            KindNAMESPACE]) then
          begin
            J := I - 1;
            while J < Card do
            begin
              Inc(J);

              R := Records[J];
              if R.Op = OP_EPILOGUE_GLOBAL_BLOCK2 then
              begin
                if Records[J - 1].Op = OP_RET then
                  Dec(J);

                Upcase := R.Upcase;
                Language := R.Language;
                ModuleNum := R.ModuleNum;
                R := TCodeRec.Create(Op, Self);
                R.Upcase := Upcase;
                R.Language := Language;
                R.ModuleNum := ModuleNum;

                R.Arg1 := Id;
                R.Arg2 := 0;
                R.Res := 0;
                Insert(J, R);

                WasInsertion := true;
                break;
              end;
            end;
          end
          else
          begin
            J := I;
            while J < Card do
            begin
              Inc(J);

              R := Records[J];
              if R.Op = OP_END_SUB then
              begin
                if Level <> R.Arg1 then
                begin
                  if GetSymbolRec(R.Arg1).IsNestedSub then
                    continue;

                  RaiseError(errInternalError, []);
                end;

                while Records[J - 1].Op = OP_NOP do
                  Dec(J);
                if Records[J - 1].Op = OP_EPILOGUE_SUB then
                  Inc(J, 3);

                Upcase := R.Upcase;
                Language := R.Language;
                ModuleNum := R.ModuleNum;
                R := TCodeRec.Create(Op, Self);
                R.Upcase := Upcase;
                R.Language := Language;
                R.ModuleNum := ModuleNum;

                R.Arg1 := Id;
                R.Arg2 := 0;
                R.Res := 0;
                Insert(J, R);

                WasInsertion := true;

                break;
              end;
            end;
          end;
        end;
      end;
    end;

    if not WasInsertion then
      break;

    IStart := I + 1;

  until false;
end;

procedure TCode.InsertFinalizators;
var
  I, J, K, ClassId, FT, Op, TypeId, DestructorId, SelfId, FieldId: Integer;
  SymbolTable: TSymbolTable;
  RJ: TSymbolRec;
  RC: TCodeRec;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  I := Card;
  while I > 1 do
  begin
    Dec(I);
    if Self[I].Op = OP_BEGIN_CLASS_TYPE then
    begin
      ClassId := Self[I].Arg1;
      DestructorId := SymbolTable.FindDestructorId(ClassId);
      SelfId := SymbolTable.GetSelfId(DestructorId);

      K := 0;

      for J := I + 1 to Card do
      begin
        RC := Self[J];
        if RC.Op = OP_END_SUB then
          if RC.Arg1 = DestructorId then
          begin
            K := J - 1;
            while not((Self[K].Op = OP_END_WITH) and (Self[K].Arg1 = SelfId)) do
              Dec(K);
          end;
      end;

      if K = 0 then
        continue;
      // RaiseError(errInternalError, []);

      for J := ClassId + 1 to SymbolTable.Card do
      begin
        RJ := SymbolTable[J];
        if RJ.Kind = KindTYPE_FIELD then
          if RJ.Level = ClassId then
          begin
            FT := RJ.FinalTypeId;
            TypeId := RJ.TerminalTypeId;
            Op := OP_NOP;
            case FT of
              typeVARIANT, typeOLEVARIANT:
                Op := OP_VARIANT_CLR;
{$IFNDEF PAXARM}
              typeANSISTRING:
                Op := OP_ANSISTRING_CLR;
              typeWIDESTRING:
                Op := OP_WIDESTRING_CLR;
{$ENDIF}
              typeUNICSTRING:
                Op := OP_UNICSTRING_CLR;
              typeINTERFACE:
                Op := OP_INTERFACE_CLR;
              typeDYNARRAY:
                Op := OP_DYNARRAY_CLR;
              typeRECORD, typeARRAY:
                if SymbolTable.HasDynamicFields(TypeId) then
                  Op := OP_STRUCTURE_CLR;
              typeCLASS:
{$IFDEF PAXARM}
                Op := OP_CLASS_CLR;
{$ELSE}
                if SymbolTable.Inherits(TypeId, H_TFW_Array) then
                begin
                  Op := OP_CLASS_CLR;
                end;
{$ENDIF}
            end;

            if Op <> OP_NOP then
            begin
              FieldId := NewTempVar(DestructorId, TypeId);

              GetSymbolRec(FieldId).OwnerId := SelfId;
              GetSymbolRec(FieldId).PatternId := J;
              GetSymbolRec(FieldId).Name := RJ.Name;
              GetSymbolRec(FieldId).ByRef := true;

              RC := TCodeRec.Create(OP_FIELD, Self);
              RC.Upcase := GetUpcase(I);
              RC.Language := GetLanguage(I);
              RC.ModuleNum := GetModuleNumber(I);
              RC.Arg1 := SelfId;
              RC.Arg2 := FieldId;
              RC.Res := FieldId;

              Insert(K, RC);

              RC := TCodeRec.Create(Op, Self);
              RC.Upcase := GetUpcase(I);
              RC.Language := GetLanguage(I);
              RC.ModuleNum := GetModuleNumber(I);
              RC.Arg1 := FieldId;

              Insert(K + 1, RC);
            end;
          end;
      end;
    end;
  end;
end;

procedure TCode.InsertTryFinally;

  procedure TestNOP(J: Integer);
  begin
    if Records[J].Op <> OP_NOP then
      RaiseError(errInternalError, []);
  end;

var
  I, J, K, Op, SubId, I1, I2, I3, L1, L2, L3, L: Integer;
  HasDynVars, HasTryOn: Boolean;

  SymbolTable: TSymbolTable;
  T: Integer;
  LoopLabel, BreakLabel, ContinueLabel: Integer;
begin
  if not TKernel(kernel).SupportedSEH then
    Exit;

  SymbolTable := TKernel(kernel).SymbolTable;

  I := 0;
  while I < Card do
  begin
    Inc(I);
    Op := Records[I].Op;
    if Op = OP_EPILOGUE_SUB then
    begin
      SubId := Records[I].Arg1;
      T := GetSymbolRec(SubId).Level;
      if T > 0 then
        if GetSymbolRec(T).FinalTypeId = typeINTERFACE then
          continue;

      J := I + 1;
      HasDynVars := false;
      while not((Records[J].Op = OP_END_SUB) and (Records[J].Arg1 = SubId)) do
      begin
        if IsDynDestr(Records[J].Op) then
          HasDynVars := true;
        Inc(J);
      end;

      if not HasDynVars then
      begin
        K := J;
        HasTryOn := false;

        L := GetSymbolRec(SubId).Level;
        if L > 0 then
          if GetSymbolRec(SubId).Kind = KindSUB then
            if GetSymbolRec(L).Kind = KindTYPE then
            begin
              HasDynVars := true;
              HasTryOn := true;
            end;

        while not((Records[K].Op = OP_INIT_SUB) and
          (Records[K].Arg1 = SubId)) do
        begin
          if Records[K].Op = OP_EXIT then
          begin
            if HasTryOn then
            begin
              HasDynVars := true;
            end
            else
              Records[K].Op := OP_GO;
          end
          else if Records[K].Op = OP_RAISE then
            HasDynVars := true
          else if Records[K].Op = OP_TRY_OFF then
            HasTryOn := true;

          Dec(K);
          if K = 1 then
            break;
        end;
      end;

      if HasDynVars then
      begin
        Inc(TKernel(kernel).TryCount);

        I2 := I;
        I3 := J;
        J := I - 1;
        while not((Records[J].Op = OP_INIT_SUB) and
          (Records[J].Arg1 = SubId)) do
          Dec(J);
        I1 := J;

        L1 := SymbolTable.AddLabel.Id;
        L2 := SymbolTable.AddLabel.Id;
        L3 := SymbolTable.AddLabel.Id;

        // I1

        J := I1 + 1;
        TestNOP(J);
        Records[J].Op := OP_TRY_ON;
        Records[J].Arg1 := TKernel(kernel).TryCount;
        Records[J].Res := SubId;

        // I3, 2 nops are reserved

        J := I2;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_EXCEPT_SEH;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L1;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_FINALLY;
        Records[J].Arg1 := TKernel(kernel).TryCount;
        Records[J].Arg2 := L1;
        Records[J].Res := SubId;

        // I3, 4 nops are reserved

        J := I3 - 4;
        TestNOP(J);
        Records[J].Arg1 := L3;
        Records[J].Res := CreateBooleanVar(SubId);
        Records[J].Op := OP_COND_RAISE;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L2;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_TRY_OFF;
        Records[J].Arg1 := TKernel(kernel).TryCount;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L3;

      end
      else
      begin
        Records[I].Op := OP_NOP;
        continue;
      end;
    end
    // loop
    else if Records[I].Op = OP_EPILOGUE_LOOP then
    begin
      if Records[I].LoopLabel = 0 then
        continue;

      Inc(TKernel(kernel).TryCount);

      LoopLabel := Records[I].LoopLabel;
      BreakLabel := Records[I].BreakLabel;
      ContinueLabel := Records[I].ContinueLabel;

      SubId := Records[I].Arg1;

      J := I + 1;
      while not((Records[J].Op = OP_END_LOOP) and (Records[J].Arg1 = SubId)) do
        Inc(J);
      I3 := J;

      I2 := I;
      J := I - 1;
      while not((Records[J].Op = OP_BEGIN_LOOP) and
        (Records[J].Arg1 = SubId)) do
        Dec(J);
      I1 := J;

      L1 := SymbolTable.AddLabel.Id;
      L2 := SymbolTable.AddLabel.Id;
      L3 := SymbolTable.AddLabel.Id;

      // I1

      J := I1 + 1;
      TestNOP(J);
      Records[J].Op := OP_TRY_ON;
      Records[J].Arg1 := TKernel(kernel).TryCount;
      Records[J].Res := GetLevel(J);

      // I3, 2 nops are reserved

      J := I2;

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_EXCEPT_SEH;

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_LABEL;
      Records[J].Arg1 := L1;

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_FINALLY;
      Records[J].Arg1 := TKernel(kernel).TryCount;
      Records[J].Arg2 := L1;

      Records[J].LoopLabel := LoopLabel;
      Records[J].BreakLabel := BreakLabel;
      Records[J].ContinueLabel := ContinueLabel;

      // I3, 4 nops are reserved

      J := I3 - 4;
      TestNOP(J);
      Records[J].Op := OP_COND_RAISE;
      Records[J].Arg1 := L3;
      Records[J].Res := CreateBooleanVar(0);

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_LABEL;
      Records[J].Arg1 := L2;

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_TRY_OFF;
      Records[J].Arg1 := TKernel(kernel).TryCount;

      Inc(J);
      TestNOP(J);
      Records[J].Op := OP_LABEL;
      Records[J].Arg1 := L3;

    end // loop

    else if Records[I].Op = OP_EPILOGUE_GLOBAL_BLOCK then
    begin
      SubId := 0;
      J := I + 1;
      HasDynVars := false;
      while not((Records[J].Op = OP_END_GLOBAL_BLOCK) and
        (Records[J].Arg1 = SubId)) do
      begin
        if IsDynDestr(Records[J].Op) then
          HasDynVars := true;
        Inc(J);
      end;

      if HasDynVars then
      begin
        Inc(TKernel(kernel).TryCount);

        I2 := I;
        I3 := J;
        J := I - 1;
        while not((Records[J].Op = OP_BEGIN_GLOBAL_BLOCK) and
          (Records[J].Arg1 = SubId)) do
          Dec(J);
        I1 := J;

        L1 := SymbolTable.AddLabel.Id;
        L2 := SymbolTable.AddLabel.Id;
        L3 := SymbolTable.AddLabel.Id;

        // I1

        J := I1 + 1;
        TestNOP(J);
        Records[J].Op := OP_TRY_ON;
        Records[J].Arg1 := TKernel(kernel).TryCount;
        Records[J].Res := GetLevel(J);

        // I3, 2 nops are reserved

        J := I2;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_EXCEPT_SEH;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L1;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_FINALLY;
        Records[J].Arg1 := TKernel(kernel).TryCount;
        Records[J].Arg2 := L1;

        // I3, 4 nops are reserved

        J := I3 - 4;
        TestNOP(J);
        Records[J].Op := OP_COND_RAISE;
        Records[J].Arg1 := L3;
        Records[J].Res := CreateBooleanVar(0);

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L2;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_TRY_OFF;
        Records[J].Arg1 := TKernel(kernel).TryCount;

        Inc(J);
        TestNOP(J);
        Records[J].Op := OP_LABEL;
        Records[J].Arg1 := L3;
      end
      else
      begin
        Records[I].Op := OP_NOP;
        continue;
      end;
    end;
  end;
end;

procedure TCode.AdjustTryList;
var
  I, J, K, Op, OldBlockId, NewBlockId: Integer;
begin
  K := 0;
  for I := 1 to Card do
    if Self[I].Op = OP_TRY_ON then
    begin
      OldBlockId := Self[I].Arg1;
      NewBlockId := K;

      Inc(K);

      Self[I].Arg1 := NewBlockId;
      for J := I + 1 to Card do
      begin
        Op := Self[J].Op;
        if (Op = OP_FINALLY) or (Op = OP_EXCEPT) or (Op = OP_EXCEPT_ON) then
        begin
          if Self[J].Arg1 = OldBlockId then
            Self[J].Arg1 := NewBlockId;
        end
        else if Op = OP_TRY_OFF then
        begin
          if Self[J].Arg1 = OldBlockId then
          begin
            Self[J].Arg1 := NewBlockId;
            break;
          end;
        end;
      end;
    end;
end;

procedure TCode.DiscardImport;
var
  I, OpEnd: Integer;
  R: TCodeRec;
  Processed: Boolean;
  S: String;
begin
  OpEnd := 0;

  Processed := false;
  Records[N].Op := OP_NOP;

  for I := N downto 1 do
  begin
    R := Records[I];
    if R.Op = OP_BEGIN_CONST then
    begin
      R.Op := OP_NOP;
      Processed := true;
      OpEnd := OP_END_CONST;
      break;
    end
    else if R.Op = OP_BEGIN_VAR then
    begin
      R.Op := OP_NOP;
      Processed := true;
      OpEnd := OP_END_VAR;
      break;
    end
    else if R.Op = OP_BEGIN_TYPE then
    begin
      R.Op := OP_NOP;
      Processed := true;
      OpEnd := OP_END_TYPE;
      break;
    end
    else if R.Op = OP_BEGIN_SUB then
    begin
      R.Op := OP_NOP;
      S := GetSymbolRec(R.Arg1).Name;
      if PosCh('#', S) > 0 then
        continue; // belongs to a procedural type declaration

      Processed := true;
      OpEnd := OP_END_SUB;
      break;
    end
    else
      R.Op := OP_NOP;
  end;

  if Processed then
    for I := N to Card do
    begin
      R := Records[I];
      if R.Op = OpEnd then
      begin
        R.Op := OP_NOP;
        break;
      end
      else
        R.Op := OP_NOP;
    end;
end;

function TCode.AddTypeInfo(TypeId, SourceTypeId: Integer): TTypeInfoContainer;
var
  SymbolTable: TBaseSymbolTable;

  LanguageId: Integer;
  Upcase: Boolean;
  CurrModule: Integer;
  Lev: Integer;

  procedure Gen(Op, Arg1, Arg2, Res: Integer);
  begin
    Add(Op, Arg1, Arg2, Res, Lev, Upcase, LanguageId, CurrModule, 0);
  end;

  function CreatePropReader(TypeFieldId: Integer): Integer;
  var
    SubId, LabelId, ResId, SelfId, ResTypeId, TempPropId, PropTypeId,
      ClassTypeId: Integer;
  begin
    ClassTypeId := GetSymbolRec(TypeFieldId).Level;
    PropTypeId := GetSymbolRec(TypeFieldId).TypeId;
    ResTypeId := PropTypeId; // function
    LabelId := SymbolTable.AddLabel.Id;
    SubId := NewTempVar(ClassTypeId, ResTypeId);
    GetSymbolRec(SubId).Name := READ_PREFIX + GetSymbolRec(TypeFieldId).Name;
    GetSymbolRec(SubId).Kind := KindSUB;
    GetSymbolRec(SubId).CallConv := ccREGISTER;

    ResId := NewTempVar(SubId, ResTypeId);

    SelfId := NewTempVar(SubId, ClassTypeId);
    GetSymbolRec(SelfId).Param := true;

    TempPropId := NewTempVar(SubId, TypeFieldId);
    GetSymbolRec(TempPropId).OwnerId := SelfId;
    GetSymbolRec(TempPropId).Name := GetSymbolRec(TypeFieldId).Name;

    Gen(OP_EXTRA_BYTECODE, CurrModule, 0, 0);

    Gen(OP_GO, LabelId, 0, 0);
    Gen(OP_BEGIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, SubId, 0, 0);
    Gen(OP_INIT_SUB, SubId, 0, 0);
    // reserved for prologue
    Gen(OP_NOP, 0, 0, 0);

    Gen(OP_FIELD, SelfId, TempPropId, TempPropId);
    Gen(OP_ASSIGN, ResId, TempPropId, ResId);

    Gen(OP_EPILOGUE_SUB, SubId, 0, 0);
    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);

    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_END_SUB, SubId, 0, 0);
    Gen(OP_FIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, LabelId, 0, 0);

    result := SubId;
  end;

  function CreatePropWriter(TypeFieldId: Integer): Integer;
  var
    SubId, LabelId, ResId, SelfId, ParamId, ResTypeId, TempPropId, PropTypeId,
      ClassTypeId, NP: Integer;
  begin
    ClassTypeId := GetSymbolRec(TypeFieldId).Level;
    PropTypeId := GetSymbolRec(TypeFieldId).TypeId;
    NP := 1;
    ResTypeId := typeVOID; // procedure
    LabelId := SymbolTable.AddLabel.Id;
    SubId := NewTempVar(ClassTypeId, ResTypeId); // procedure
    GetSymbolRec(SubId).Name := WRITE_PREFIX + GetSymbolRec(TypeFieldId).Name;
    GetSymbolRec(SubId).Kind := KindSUB;
    GetSymbolRec(SubId).CallConv := ccREGISTER;
    GetSymbolRec(SubId).Count := NP;

    ResId := NewTempVar(SubId, ResTypeId);
    GetSymbolRec(ResId).Kind := KindNONE;

    SelfId := NewTempVar(SubId, ClassTypeId);
    GetSymbolRec(SelfId).Param := true;

    ParamId := NewTempVar(SubId, PropTypeId);
    GetSymbolRec(ParamId).Param := true;
    GetSymbolRec(ParamId).Name := 'value';
    GetSymbolRec(ParamId).IsConst := true;

    TempPropId := NewTempVar(SubId, TypeFieldId);
    GetSymbolRec(TempPropId).OwnerId := SelfId;
    GetSymbolRec(TempPropId).Name := GetSymbolRec(TypeFieldId).Name;

    Gen(OP_EXTRA_BYTECODE, CurrModule, 0, 0);

    Gen(OP_GO, LabelId, 0, 0);
    Gen(OP_BEGIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, SubId, 0, 0);
    Gen(OP_INIT_SUB, SubId, 0, 0);
    // reserved for prologue
    Gen(OP_NOP, 0, 0, 0);

    Gen(OP_FIELD, SelfId, TempPropId, TempPropId);
    Gen(OP_ASSIGN, TempPropId, ParamId, TempPropId);

    Gen(OP_EPILOGUE_SUB, SubId, 0, 0);
    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);

    // reserved for epilogue
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_END_SUB, SubId, 0, 0);
    Gen(OP_FIN_SUB, SubId, 0, 0);
    Gen(OP_LABEL, LabelId, 0, 0);

    result := SubId;
  end;

var
  TypeInfoList: TPaxTypeInfoList;
  S: String;
  FullName: String;
  FinTypeId: Integer;
  tk: TTypeKind;
  ClassTypeInfoContainer: TClassTypeInfoContainer;
  ClassTypeDataContainer: TClassTypeDataContainer;
  MethodTypeInfoContainer: TMethodTypeInfoContainer;
  MethodTypeDataContainer: TMethodTypeDataContainer;
  InterfaceTypeInfoContainer: TInterfaceTypeInfoContainer;
  InterfaceTypeDataContainer: TInterfaceTypeDataContainer;
  SetTypeDataContainer: TSetTypeDataContainer;
  EnumTypeDataContainer: TEnumTypeDataContainer;

  I, J, Id, PropId, Level, K, SubId, ResTypeId, ParamId, ParamTypeId,
    NP: Integer;
  RI: TSymbolRec;
  AncestorTypeId: Integer;
  FieldTypeId: Integer;
  PatternId: Integer;
  NS_ID: Integer;
  b: Boolean;
  pti: PTypeInfo;
  val, Min_Value, Max_Value: Cardinal;

  ArrayTypeDataContainer: TArrayTypeDataContainer;
  RecordTypeDataContainer: TRecordTypeDataContainer;
  ProceduralTypeDataContainer: TProceduralTypeDataContainer;
  RangeTypeId, ElemTypeId: Integer;
  SubDesc: TSubDesc;
  DGUID: packed record D1, D2: Double;
end;

label update;
begin
  result := nil;
  LanguageId := GetLanguage(N);
  Upcase := GetUpcase(N);
  CurrModule := GetModuleNumber(N);

  SymbolTable := TKernel(kernel).SymbolTable;
  TypeInfoList := TKernel(kernel).TypeInfoList;

  S := SymbolTable[TypeId].Name;
  FullName := SymbolTable[TypeId].FullName;

  if TypeId <> SourceTypeId then
  begin
    result := TAliasTypeInfoContainer.Create(SymbolTable[SourceTypeId].Name);
    result.FullName := SymbolTable[SourceTypeId].FullName;
    (result.TypeDataContainer as TAliasTypeDataContainer).FullSourceTypeName
      := FullName;
    TypeInfoList.Add(result);
  end;

  {
    result := TypeInfoList.LookupFullName(FullName);
    if result <> nil then
    begin
    Exit;
    end;
  }
  I := TypeInfoList.IndexOf(FullName);
  if I >= 0 then
    TypeInfoList.RemoveAt(I);

  FinTypeId := SymbolTable[TypeId].FinalTypeId;
  tk := FinTypeToTypeKind(FinTypeId);

  case tk of
    tkArray:
      begin
        SymbolTable.GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
        // if (GetSymbolRec(RangeTypeId).FinalTypeId <> typeTYPEPARAM) and
        // (GetSymbolRec(ElemTypeId).FinalTypeId <> typeTYPEPARAM) then
        begin
          AddTypeInfo(RangeTypeId, RangeTypeId);
          AddTypeInfo(ElemTypeId, ElemTypeId);

          result := TArrayTypeInfoContainer.Create(S);
          result.FullName := FullName;
          TypeInfoList.Add(result);

          ArrayTypeDataContainer :=
            result.TypeDataContainer as TArrayTypeDataContainer;
          ArrayTypeDataContainer.FullRangeTypeName :=
            GetSymbolRec(RangeTypeId).FullName;
          ArrayTypeDataContainer.FullElemTypeName :=
            GetSymbolRec(ElemTypeId).FullName;
          ArrayTypeDataContainer.FinRangeTypeId := GetSymbolRec(RangeTypeId)
            .FinalTypeId;
          ArrayTypeDataContainer.B1 := SymbolTable.GetLowBoundRec
            (RangeTypeId).value;
          ArrayTypeDataContainer.B2 := SymbolTable.GetHighBoundRec
            (RangeTypeId).value;
        end;
      end;
    tkDynarray:
      begin
        PatternId := GetSymbolRec(TypeId).PatternId;

        result := TDynArrayTypeInfoContainer.Create(SymbolTable[TypeId].Name);
        result.FullName := FullName;
        S := GetSymbolRec(PatternId).FullName;
        (result.TypeDataContainer as TDynArrayTypeDataContainer)
          .FullElementTypeName := S;
        TypeInfoList.Add(result);
      end;
    tkRecord:
      begin
        result := TRecordTypeInfoContainer.Create(S);
        TKernel(kernel).TypeDefList.CreateConainer(TypeId,
          result.GenericTypeContainer);

        result.FullName := FullName;
        TypeInfoList.Add(result);

        RecordTypeDataContainer :=
          result.TypeDataContainer as TRecordTypeDataContainer;
        RecordTypeDataContainer.IsPacked := SymbolTable[TypeId].IsPacked;
        for I := TypeId + 1 to SymbolTable.Card do
        begin
          RI := SymbolTable[I];
          if RI.Level = TypeId then
            if RI.Kind = KindTYPE_FIELD then
            begin
              FieldTypeId := RI.TerminalTypeId;
              with RecordTypeDataContainer.FieldListContainer.Add do
              begin
                Id := I;
                // Offset := <it does not matter>;
                // ClassIndex := <it does not matter>;
                PShortStringFromString(@Name, RI.Name);
                FullFieldTypeName := GetSymbolRec(FieldTypeId).FullName;
              end;
            end;
        end;
      end;
    tkEnumeration:
      begin
        result := TEnumTypeInfoContainer.Create(S);
        result.FullName := FullName;
        TypeInfoList.Add(result);

        EnumTypeDataContainer :=
          result.TypeDataContainer as TEnumTypeDataContainer;

        NS_ID := GetSymbolRec(TypeId).GetNamespaceId;
        if NS_ID > 0 then
          PShortStringFromString(@EnumTypeDataContainer.EnumUnitName,
            GetSymbolRec(NS_ID).Name)
        else
          PShortStringFromString(@EnumTypeDataContainer.EnumUnitName, '');

        if FinTypeId in BooleanTypes then
        begin
          EnumTypeDataContainer.TypeData.OrdType := otUByte;
          EnumTypeDataContainer.TypeData.MinValue := 0;
          EnumTypeDataContainer.TypeData.MaxValue := 1;
          SetLength(EnumTypeDataContainer.NameList, 2);
          PShortStringFromString(@EnumTypeDataContainer.NameList[0], 'false');
          PShortStringFromString(@EnumTypeDataContainer.NameList[1], 'true');
          SetLength(EnumTypeDataContainer.ValueList, 2);
          EnumTypeDataContainer.ValueList[0] := 0;
          EnumTypeDataContainer.ValueList[1] := 1;
          Exit;
        end;

        EnumTypeDataContainer.TypeData.OrdType := otSLong;

        SetLength(EnumTypeDataContainer.NameList, GetSymbolRec(TypeId).Count);
        SetLength(EnumTypeDataContainer.ValueList, GetSymbolRec(TypeId).Count);

        Min_Value := MaxInt;
        Max_Value := 0;

        K := 0;
        for I := TypeId + 1 to SymbolTable.Card do
          if SymbolTable[I].Kind = KindCONST then
            if SymbolTable[I].OwnerId = TypeId then
            begin
              PShortStringFromString(@EnumTypeDataContainer.NameList[K],
                SymbolTable[I].Name);
              val := SymbolTable[I].value;
              EnumTypeDataContainer.ValueList[K] := val;

              if val < Min_Value then
                Min_Value := val;
              if val > Max_Value then
                Max_Value := val;

              Inc(K);
              if K >= System.Length(EnumTypeDataContainer.NameList) then
                break;
            end;

        EnumTypeDataContainer.TypeData.MinValue := Min_Value;
        EnumTypeDataContainer.TypeData.MaxValue := Max_Value;
      end;
    tkSet:
      begin
        result := TSetTypeInfoContainer.Create(S);
        result.FullName := FullName;
        TypeInfoList.Add(result);

        PatternId := GetSymbolRec(TypeId).PatternId;

        if PatternId = 0 then
          RaiseError(errInternalError, []);

        SetTypeDataContainer :=
          result.TypeDataContainer as TSetTypeDataContainer;
        SetTypeDataContainer.FullCompName := GetSymbolRec(PatternId).FullName;
        AddTypeInfo(PatternId, PatternId);
      end;
    tkInteger, tkChar, tkWChar:
      begin
        result := TTypeInfoContainer.Create(FinTypeId);
        result.FullName := FullName;
        result.TypeInfo.Kind := tk;
        PShortStringFromString(@result.TypeInfo.Name, S);
        TypeInfoList.Add(result);

        pti := nil;

        case FinTypeId of
          typeSHORTINT:
            begin
              result.TypeDataContainer.TypeData.OrdType := otSByte;
              pti := TypeInfo(ShortInt);
            end;
{$IFNDEF PAXARM}
          typeANSICHAR,
{$ENDIF}
          typeBYTE:
            begin
              result.TypeDataContainer.TypeData.OrdType := otUByte;
              pti := TypeInfo(Byte);
            end;
          typeSMALLINT:
            begin
              result.TypeDataContainer.TypeData.OrdType := otSWord;
              pti := TypeInfo(SmallInt);
            end;
          typeWORD, typeWIDECHAR:
            begin
              result.TypeDataContainer.TypeData.OrdType := otUWord;
              pti := TypeInfo(Word);
            end;
          typeINTEGER:
            begin
              result.TypeDataContainer.TypeData.OrdType := otSLong;
              pti := TypeInfo(Integer);
            end;
          typeCARDINAL:
            begin
              result.TypeDataContainer.TypeData.OrdType := otULong;
              pti := TypeInfo(Cardinal);
            end;
        end;

        if pti <> nil then
        begin
          result.TypeDataContainer.TypeData.MinValue :=
            GetTypeData(pti).MinValue;
          result.TypeDataContainer.TypeData.MaxValue :=
            GetTypeData(pti).MaxValue;
        end;
      end;
    tkString:
      begin
        result := TTypeInfoContainer.Create(FinTypeId);
        result.FullName := FullName;
        result.TypeInfo.Kind := tk;
        PShortStringFromString(@result.TypeInfo.Name, S);
        TypeInfoList.Add(result);

{$IFDEF PAXARM}
        RIE;
{$ELSE}
        result.TypeDataContainer.TypeData.MaxLength :=
          SymbolTable[TypeId].Count;
{$ENDIF}
      end;
    tkInt64:
      begin
        result := TTypeInfoContainer.Create(FinTypeId);
        result.FullName := FullName;
        result.TypeInfo.Kind := tk;
        PShortStringFromString(@result.TypeInfo.Name, S);
        TypeInfoList.Add(result);

        pti := TypeInfo(Int64);
        result.TypeDataContainer.TypeData.MinInt64Value := GetTypeData(pti)
          ^.MinInt64Value;
        result.TypeDataContainer.TypeData.MaxInt64Value := GetTypeData(pti)
          ^.MaxInt64Value;
      end;
    tkFloat:
      begin
        result := TTypeInfoContainer.Create(FinTypeId);
        result.FullName := FullName;
        result.TypeInfo.Kind := tk;
        PShortStringFromString(@result.TypeInfo.Name, S);
        TypeInfoList.Add(result);

        case FinTypeId of
          typeSINGLE:
            result.TypeDataContainer.TypeData.FloatType := ftSingle;
          typeDOUBLE:
            result.TypeDataContainer.TypeData.FloatType := ftDouble;
          typeEXTENDED:
            result.TypeDataContainer.TypeData.FloatType := ftExtended;
          typeCURRENCY:
            result.TypeDataContainer.TypeData.FloatType := ftCurr;
        end;
      end;
    tkMethod:
      begin
        result := TMethodTypeInfoContainer.Create(S);
        result.FullName := FullName;
        TypeInfoList.Add(result);

        MethodTypeInfoContainer := TMethodTypeInfoContainer(result);
        SubId := GetSymbolRec(TypeId).PatternId;

        if GetSymbolRec(SubId).Kind <> KindSUB then
          RaiseError(errInternalError, []);

        MethodTypeDataContainer := TMethodTypeDataContainer
          (MethodTypeInfoContainer.TypeDataContainer);
        MethodTypeDataContainer.ResultTypeId := GetSymbolRec(SubId).TypeId;
        MethodTypeDataContainer.CallConv := GetSymbolRec(SubId).CallConv;
        MethodTypeDataContainer.OverCount := GetSymbolRec(SubId).OverCount;

        if GetSymbolRec(SubId).TypeId = typeVOID then
        begin
          if GetSymbolRec(SubId).IsSharedMethod then
            MethodTypeDataContainer.MethodKind := mkClassProcedure
          else
            MethodTypeDataContainer.MethodKind := mkProcedure;
        end
        else
        begin
          if GetSymbolRec(SubId).IsSharedMethod then
            MethodTypeDataContainer.MethodKind := mkClassFunction
          else
            MethodTypeDataContainer.MethodKind := mkFunction;
        end;

        NP := GetSymbolRec(SubId).Count;
        MethodTypeDataContainer.ParamCount := NP;

        ResTypeId := GetSymbolRec(SubId).TypeId;
        if ResTypeId = typeVOID then
          PShortStringFromString(@MethodTypeDataContainer.ResultType, '')
        else
          PShortStringFromString(@MethodTypeDataContainer.ResultType,
            GetSymbolRec(ResTypeId).Name);

        SetLength(MethodTypeDataContainer.ParamListContainer.ParamList, NP);

        for I := 0 to NP - 1 do
        begin
          ParamId := SymbolTable.GetParamId(SubId, I);
          ParamTypeId := GetSymbolRec(ParamId).TypeId;
          PShortStringFromString
            (@MethodTypeDataContainer.ParamListContainer.ParamList[I].ParamName,
            GetSymbolRec(ParamId).Name);
          PShortStringFromString
            (@MethodTypeDataContainer.ParamListContainer.ParamList[I].TypeName,
            GetSymbolRec(ParamTypeId).Name);
          MethodTypeDataContainer.ParamListContainer.ParamList[I].Flags := [];
          if GetSymbolRec(ParamId).ByRef then
            MethodTypeDataContainer.ParamListContainer.ParamList[I].Flags :=
              MethodTypeDataContainer.ParamListContainer.ParamList[I].Flags
              + [pfVar];
          if GetSymbolRec(ParamId).IsConst then
            MethodTypeDataContainer.ParamListContainer.ParamList[I].Flags :=
              MethodTypeDataContainer.ParamListContainer.ParamList[I].Flags +
              [pfConst];
        end;
      end;
    tkInterface:
      begin
        result := TInterfaceTypeInfoContainer.Create(S);
        result.FullName := FullName;
        TypeInfoList.Add(result);
        InterfaceTypeInfoContainer := TInterfaceTypeInfoContainer(result);

        Level := SymbolTable[TypeId].Level;
        if Level > 0 then
          PShortStringFromString
            (@InterfaceTypeInfoContainer.TypeDataContainer.TypeData.IntfUnit,
            SymbolTable[Level].Name);

        InterfaceTypeDataContainer :=
          InterfaceTypeInfoContainer.TypeDataContainer as
          TInterfaceTypeDataContainer;

        DGUID.D1 := SymbolTable[TypeId + 1].value;
        DGUID.D2 := SymbolTable[TypeId + 2].value;
        Move(DGUID, InterfaceTypeDataContainer.GUID, SizeOf(TGUID));

        InterfaceTypeDataContainer.TypeData.IntfFlags := [ifHasGuid];

        b := SymbolTable[TypeId].SupportedInterfaces <> nil;
        if b then
          b := SymbolTable[TypeId].SupportedInterfaces.Count > 0;

        if b then
        begin
          AncestorTypeId := SymbolTable[TypeId].SupportedInterfaces[0].Id;
          InterfaceTypeDataContainer.FullParentName :=
            SymbolTable[AncestorTypeId].FullName;
          InterfaceTypeDataContainer.TypeData.GUID := SymbolTable[TypeId]
            .SupportedInterfaces[0].GUID;
        end
        else
        begin
          if StrEql(SymbolTable[TypeId].Name, 'IUnknown') then
          begin
            InterfaceTypeDataContainer.FullParentName := '';
          end
          else
          begin
            InterfaceTypeDataContainer.FullParentName := 'IUnknown';
            InterfaceTypeDataContainer.TypeData.GUID := IUnknown;
          end;
        end;

        for I := TypeId + 1 to SymbolTable.Card do
        begin
          RI := SymbolTable[I];
          // if RI.IsPublished then
          if RI.Level = TypeId then
          begin
            if RI.Kind = KindPROP then
            begin
              PropId := I;

              Inc(InterfaceTypeDataContainer.PropDataContainer.PropData.
                PropCount);
              K := InterfaceTypeDataContainer.PropDataContainer.
                PropData.PropCount;
              SetLength(InterfaceTypeDataContainer.PropDataContainer.
                PropList, K);
              SetLength(InterfaceTypeDataContainer.PropDataContainer.
                PropTypeIds, K);
              PShortStringFromString
                (@InterfaceTypeDataContainer.PropDataContainer.PropList[K - 1]
                .Name, SymbolTable[PropId].Name);

              Id := SymbolTable[PropId].ReadId;

              if Id > 0 then
                S := SymbolTable[Id].FullName
              else
                S := '';
              InterfaceTypeDataContainer.PropDataContainer.ReadNames.Add(S);

              Id := SymbolTable[PropId].WriteId;

              if Id > 0 then
                S := SymbolTable[Id].FullName
              else
                S := '';
              InterfaceTypeDataContainer.PropDataContainer.WriteNames.Add(S);

              Id := SymbolTable[PropId].TypeId;
              if Id > 0 then
                S := SymbolTable[Id].FullName
              else
                S := '';
              InterfaceTypeDataContainer.PropDataContainer.PropTypeNames.Add(S);
            end // add interface property
            else if RI.Kind = KindSUB then
            begin
              PatternId := I;
              SubDesc := InterfaceTypeDataContainer.SubDescList.AddRecord;
              SubDesc.CallConv := GetSymbolRec(PatternId).CallConv;
              SubDesc.ResTypeName :=
                GetSymbolRec(GetSymbolRec(PatternId).TypeId).Name;
              SubDesc.ResTypeId := GetSymbolRec(PatternId).FinalTypeId;
              SubDesc.SubName := GetSymbolRec(PatternId).Name;
              NP := GetSymbolRec(PatternId).Count;
              for J := 0 to NP - 1 do
              begin
                ParamId := SymbolTable.GetParamId(PatternId, J);
                with SubDesc.ParamList.AddRecord do
                begin
                  ParamName := GetSymbolRec(ParamId).Name;
                  ParamTypeName :=
                    GetSymbolRec(GetSymbolRec(ParamId).TypeId).Name;

                  if SymbolTable[ParamId].FinalTypeId
                    in [typeDYNARRAY, typeOPENARRAY] then
                    if SymbolTable[ParamId].IsOpenArray then
                    begin
                      ParamTypeId := SymbolTable[ParamId].TerminalTypeId;
                      ElemTypeId := SymbolTable[ParamTypeId].PatternId;
                      ParamTypeName := 'array of ' + SymbolTable
                        [ElemTypeId].Name;
                    end;

                  if SymbolTable[ParamId].ByRef then
                    ParamMod := PM_BYREF
                  else if SymbolTable[ParamId].IsConst then
                    ParamMod := PM_CONST
                  else
                    ParamMod := PM_BYVAL;

                  if SymbolTable[ParamId].Optional then
                    OptValue :=
                      VariantToString(SymbolTable[ParamId].FinalTypeId,
                      SymbolTable[ParamId].value);
                end;
              end;
            end; // RI.Kind = kindSUB
          end;
        end;
      end;
    tkClass:
      begin
        if GetSymbolRec(TypeId).Host then
          Exit;

        result := TClassTypeInfoContainer.Create(S);
        TKernel(kernel).TypeDefList.CreateConainer(TypeId,
          result.GenericTypeContainer);

        result.FullName := FullName;
        TypeInfoList.Add(result);
        ClassTypeInfoContainer := TClassTypeInfoContainer(result);

        Level := SymbolTable[TypeId].Level;
        if Level > 0 then
          PShortStringFromString
            (@ClassTypeInfoContainer.TypeDataContainer.TypeData.UnitName,
            SymbolTable[Level].Name);
        ClassTypeDataContainer := ClassTypeInfoContainer.TypeDataContainer as
          TClassTypeDataContainer;

        if GetSymbolRec(TypeId).SupportedInterfaces <> nil then
          for I := 0 to GetSymbolRec(TypeId).SupportedInterfaces.Count - 1 do
          begin
            Id := GetSymbolRec(TypeId).SupportedInterfaces[I].Id;
            ClassTypeDataContainer.SupportedInterfaces.Add
              (GetSymbolRec(Id).Name);
          end;

        ClassTypeDataContainer.FieldTableCount := 0;
        ClassTypeDataContainer.FieldTableSize := SizeOf(Word);

{$IFDEF ARC}
        Inc(ClassTypeDataContainer.FieldTableSize, SizeOfPointer);
{$ELSE}
{$IFDEF WIN32}
        Inc(ClassTypeDataContainer.FieldTableSize, SizeOfPointer);
{$ELSE}
        Inc(ClassTypeDataContainer.FieldTableSize, SizeOf(Word));
{$ENDIF};

{$ENDIF}
        ClassTypeDataContainer.MethodTableCount := 0;
        ClassTypeDataContainer.MethodTableSize := SizeOf(TVmtMethodCount);

        AncestorTypeId := SymbolTable[TypeId].AncestorId;

        ClassTypeDataContainer.FullParentName :=
          SymbolTable[AncestorTypeId].FullName;

        for I := TypeId + 1 to SymbolTable.Card do
        begin
          RI := SymbolTable[I];
        update:
          if RI.IsPublished then
          begin
            if RI.Level = TypeId then
              if RI.Kind = KindPROP then
              begin
                PropId := I;

                Inc(ClassTypeInfoContainer.TypeDataContainer.TypeData.
                  PropCount);
                Inc(ClassTypeDataContainer.PropDataContainer.PropData.
                  PropCount);
                K := ClassTypeDataContainer.PropDataContainer.PropData.
                  PropCount;
                SetLength(ClassTypeDataContainer.PropDataContainer.PropList, K);
                SetLength(ClassTypeDataContainer.PropDataContainer.
                  PropTypeIds, K);
                PShortStringFromString
                  (@ClassTypeDataContainer.PropDataContainer.PropList[K - 1]
                  .Name, SymbolTable[PropId].Name);

                Id := SymbolTable[PropId].ReadId;

                if SymbolTable[Id].Kind = KindTYPE_FIELD then
                  Id := CreatePropReader(Id);

                if Id > 0 then
                  S := SymbolTable[Id].FullName
                else
                  S := '';
                ClassTypeDataContainer.PropDataContainer.ReadNames.Add(S);

                Id := SymbolTable[PropId].WriteId;

                if SymbolTable[Id].Kind = KindTYPE_FIELD then
                  Id := CreatePropWriter(Id);

                if Id > 0 then
                  S := SymbolTable[Id].FullName
                else
                  S := '';
                ClassTypeDataContainer.PropDataContainer.WriteNames.Add(S);

                Id := SymbolTable[PropId].TerminalTypeId;
                if Id > 0 then
                  S := SymbolTable[Id].FullName
                else
                  S := '';
                ClassTypeDataContainer.PropDataContainer.PropTypeNames.Add(S);
              end // add published property
              else if RI.Kind in kindSUBS then
              begin
                Inc(ClassTypeDataContainer.MethodTableCount);
{$IFDEF FPC}
                Inc(ClassTypeDataContainer.MethodTableSize, SizeOf(TVmtMethod));
{$ELSE}
                Inc(ClassTypeDataContainer.MethodTableSize, SizeOf(Word));
                // Size
                Inc(ClassTypeDataContainer.MethodTableSize, SizeOfPointer +
                  // Address
                  Length(RI.Name) + 1); // Name
{$ENDIF}
                MethodTypeInfoContainer :=
                  TMethodTypeInfoContainer.Create(RI.Name);
                MethodTypeInfoContainer.FullName := RI.FullName;
                TypeInfoList.Add(MethodTypeInfoContainer);

                SubId := I;

                MethodTypeDataContainer :=
                  TMethodTypeDataContainer
                  (MethodTypeInfoContainer.TypeDataContainer);
                MethodTypeDataContainer.MethodTableIndex :=
                  ClassTypeDataContainer.MethodTableCount - 1;

                MethodTypeDataContainer.OwnerTypeName :=
                  GetSymbolRec(TypeId).FullName;

                MethodTypeDataContainer.OverCount := GetSymbolRec(SubId)
                  .OverCount;

                if GetSymbolRec(SubId).TypeId = typeVOID then
                begin
                  if GetSymbolRec(SubId).IsSharedMethod then
                    MethodTypeDataContainer.MethodKind := mkClassProcedure
                  else
                    MethodTypeDataContainer.MethodKind := mkProcedure;
                end
                else
                begin
                  if GetSymbolRec(SubId).IsSharedMethod then
                    MethodTypeDataContainer.MethodKind := mkClassFunction
                  else
                    MethodTypeDataContainer.MethodKind := mkFunction;
                end;

                NP := GetSymbolRec(SubId).Count;
                MethodTypeDataContainer.ParamCount := NP;

                ResTypeId := GetSymbolRec(SubId).TypeId;
                if ResTypeId = typeVOID then
                  PShortStringFromString
                    (@MethodTypeDataContainer.ResultType, '')
                else
                  PShortStringFromString(@MethodTypeDataContainer.ResultType,
                    GetSymbolRec(ResTypeId).Name);

                SetLength(MethodTypeDataContainer.ParamListContainer.
                  ParamList, NP);

                for J := 0 to NP - 1 do
                begin
                  ParamId := SymbolTable.GetParamId(SubId, J);
                  ParamTypeId := GetSymbolRec(ParamId).TypeId;
                  PShortStringFromString
                    (@MethodTypeDataContainer.ParamListContainer.ParamList[J]
                    .ParamName, GetSymbolRec(ParamId).Name);
                  PShortStringFromString
                    (@MethodTypeDataContainer.ParamListContainer.ParamList[J]
                    .TypeName, GetSymbolRec(ParamTypeId).Name);
                  MethodTypeDataContainer.ParamListContainer.ParamList[J]
                    .Flags := [];
                  if GetSymbolRec(ParamId).ByRef then
                    MethodTypeDataContainer.ParamListContainer.ParamList[J]
                      .Flags := MethodTypeDataContainer.ParamListContainer.
                      ParamList[J].Flags + [pfVar];
                  if GetSymbolRec(ParamId).IsConst then
                    MethodTypeDataContainer.ParamListContainer.ParamList[J]
                      .Flags := MethodTypeDataContainer.ParamListContainer.
                      ParamList[J].Flags + [pfConst];
                end;
              end // add published method
              else if RI.Kind = KindTYPE_FIELD then
              begin
                if RI.FinalTypeId <> typeCLASS then
                begin
                  RI.Vis := cvPublic;
                  goto update;
                  // continue;
                end;
                Inc(ClassTypeDataContainer.FieldTableCount);

                Inc(ClassTypeDataContainer.FieldTableSize, SizeOf(Cardinal) +
                  // Offset
                  SizeOf(Word) + // ClassIndex
                  Length(RI.Name) + 1); // Name

                FieldTypeId := RI.TerminalTypeId;

                with ClassTypeDataContainer.FieldListContainer.Add do
                begin
                  Id := I;
                  // Offset := <later>;
                  ClassIndex := ClassTypeDataContainer.FieldTableCount - 1;
                  PShortStringFromString(@Name, RI.Name);
                  FullFieldTypeName := GetSymbolRec(FieldTypeId).FullName;
                end;
              end;
          end // published members
          else if RI.Vis in [cvPrivate, cvStrictPrivate, cvProtected,
            cvStrictProtected, cvPublic] then
          begin
            if RI.Level = TypeId then
            begin
              if RI.Kind = KindTYPE_FIELD then
              begin
                FieldTypeId := RI.TerminalTypeId;
                with ClassTypeDataContainer.AnotherFieldListContainer.Add do
                begin
                  Vis := RI.Vis; // PCU only
                  Id := I;
                  // Offset := <later in UpdateTypeInfos>;
                  // ClassIndex := <it does not matter>;
                  PShortStringFromString(@Name, RI.Name);
                  FullFieldTypeName := GetSymbolRec(FieldTypeId).FullName;
                end;
              end // another fields
              else if RI.Kind = KindPROP then
              begin
                // PCU only
                with ClassTypeDataContainer.AnotherPropList.Add do
                begin
                  PropId := I;
                  Vis := RI.Vis;
                  PropName := RI.Name;
                  if GetSymbolRec(PropId).Count > 0 then
                    for J := 0 to GetSymbolRec(PropId).Count - 1 do
                    begin
                      ParamId := SymbolTable.GetParamId(PropId, J);
                      ParamNames.Add(GetSymbolRec(ParamId).Name);
                      ParamTypes.Add
                        (GetSymbolRec(GetSymbolRec(ParamId).TypeId).Name);
                    end;
                  PropType := GetSymbolRec(RI.TypeId).Name;
                  ReadName := GetSymbolRec(RI.ReadId).Name;
                  WriteName := GetSymbolRec(RI.WriteId).Name;
                  IsDefault := RI.IsDefault;
                end;
              end;
            end;
          end; // another members
        end; // for-loop
      end // tkClass
  else
    begin
      case FinTypeId of
        typePOINTER:
          begin
            PatternId := GetSymbolRec(TypeId).PatternId;

            result := TPointerTypeInfoContainer.Create
              (SymbolTable[TypeId].Name);
            result.FullName := FullName;
            (result.TypeDataContainer as TPointerTypeDataContainer)
              .FullOriginTypeName := GetSymbolRec(PatternId).FullName;
            TypeInfoList.Add(result);
          end;
        typeCLASSREF:
          begin
            PatternId := GetSymbolRec(TypeId).PatternId;

            result := TClassRefTypeInfoContainer.Create
              (SymbolTable[TypeId].Name);
            result.FullName := FullName;
            S := GetSymbolRec(PatternId).FullName;
            (result.TypeDataContainer as TClassRefTypeDataContainer)
              .FullOriginTypeName := S;
            TypeInfoList.Add(result);
          end;
        typePROC:
          begin
            PatternId := GetSymbolRec(TypeId).PatternId;

            result := TProceduralTypeInfoContainer.Create
              (SymbolTable[TypeId].Name);
            result.FullName := FullName;
            ProceduralTypeDataContainer :=
              result.TypeDataContainer as TProceduralTypeDataContainer;
            with ProceduralTypeDataContainer do
            begin
              SubDesc.CallConv := GetSymbolRec(PatternId).CallConv;
              SubDesc.ResTypeName :=
                GetSymbolRec(GetSymbolRec(PatternId).TypeId).Name;
              SubDesc.ResTypeId := GetSymbolRec(PatternId).FinalTypeId;
              NP := GetSymbolRec(PatternId).Count;
              for J := 0 to NP - 1 do
              begin
                ParamId := SymbolTable.GetParamId(PatternId, J);
                with SubDesc.ParamList.AddRecord do
                begin
                  ParamName := GetSymbolRec(ParamId).Name;
                  ParamTypeName :=
                    GetSymbolRec(GetSymbolRec(ParamId).TypeId).Name;

                  if SymbolTable[ParamId].FinalTypeId
                    in [typeDYNARRAY, typeOPENARRAY] then
                    if SymbolTable[ParamId].IsOpenArray then
                    begin
                      ParamTypeId := SymbolTable[ParamId].TerminalTypeId;
                      ElemTypeId := SymbolTable[ParamTypeId].PatternId;
                      ParamTypeName := 'array of ' + SymbolTable
                        [ElemTypeId].Name;
                    end;

                  if SymbolTable[ParamId].ByRef then
                    ParamMod := PM_BYREF
                  else if SymbolTable[ParamId].IsConst then
                    ParamMod := PM_CONST
                  else
                    ParamMod := PM_BYVAL;

                  if SymbolTable[ParamId].Optional then
                    OptValue :=
                      VariantToString(SymbolTable[ParamId].FinalTypeId,
                      SymbolTable[ParamId].value);
                end;
              end;
            end;

            TypeInfoList.Add(result);
          end; // typePROC
      else
        begin
          result := TTypeInfoContainer.Create(FinTypeId);
          result.FullName := FullName;
          result.TypeInfo.Kind := tk;
          PShortStringFromString(@result.TypeInfo.Name, S);
          TypeInfoList.Add(result);
        end;
      end; // case
    end;
  end;
end;

procedure TCode.OperDetermineProp;
var
  Id, PropId, ReadId, WriteId, ClassId, TypeId: Integer;
  SymbolTable: TSymbolTable;
  S: String;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  PropId := Records[N].Arg1;
  S := GetSymbolRec(PropId).Name;

  ClassId := GetSymbolRec(PropId).Level;

  repeat
    ClassId := GetSymbolRec(ClassId).AncestorId;
    if ClassId = 0 then
      RaiseError(errInternalError, []);

    Id := SymbolTable.LookUp(S, ClassId, GetUpcase(N));
    if Id > 0 then
    begin
      TypeId := GetSymbolRec(Id).TypeId;
      ReadId := GetSymbolRec(Id).ReadId;
      WriteId := GetSymbolRec(Id).WriteId;
      if (ReadId > 0) or (WriteId > 0) then
      begin
        GetSymbolRec(PropId).ReadId := ReadId;
        GetSymbolRec(PropId).WriteId := WriteId;
        if GetSymbolRec(TypeId).Kind = KindTYPE then
        begin
          GetSymbolRec(PropId).TypeId := TypeId;
          Records[N].Op := OP_NOP;
        end
        else
          RaiseError(errInternalError, []);

        // GetSymbolRec(PropId).PatternId := Id;
        break;
      end;
    end;
  until false;
end;

procedure TCode.OperSetReadId;
var
  PropId, ReadId: Integer;
  SymbolTable: TSymbolTable;
  I, K, ParamId1, ParamId2, T1, T2: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  PropId := Records[N].Arg1;
  ReadId := Records[N].Arg2;
  T1 := SymbolTable[PropId].TypeId;
  T2 := SymbolTable[ReadId].TypeId;

  GetSymbolRec(PropId).ReadId := ReadId;

  if GetSymbolRec(ReadId).Kind = KindTYPE_FIELD then
  begin
    if T1 <> T2 then
      RaiseError(errIncompatibleTypesNoArgs, []);
    Records[N].Op := OP_NOP;
    Exit;
  end;

  if GetSymbolRec(ReadId).Kind <> KindSUB then
    RaiseError(errIncompatibleTypesNoArgs, []);

  K := GetSymbolRec(PropId).Count;
  if K <> GetSymbolRec(ReadId).Count then
    RaiseError(errIncompatibleTypesNoArgs, []);

  if T1 <> T2 then
    RaiseError(errIncompatibleTypesNoArgs, []);

  for I := 0 to K - 1 do
  begin
    ParamId1 := SymbolTable.GetParamId(PropId, I);
    ParamId2 := SymbolTable.GetParamId(ReadId, I);
    T1 := SymbolTable[ParamId1].TypeId;
    T2 := SymbolTable[ParamId2].TypeId;
    if T1 <> T2 then
      RaiseError(errIncompatibleTypesNoArgs, []);
  end;
  Records[N].Op := OP_NOP;
end;

procedure TCode.OperSetWriteId;
var
  PropId, WriteId: Integer;
  SymbolTable: TSymbolTable;
  I, K, ParamId1, ParamId2, T1, T2: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  PropId := Records[N].Arg1;
  WriteId := Records[N].Arg2;

  GetSymbolRec(PropId).WriteId := WriteId;

  if GetSymbolRec(WriteId).Kind = KindTYPE_FIELD then
  begin
    T1 := SymbolTable[PropId].TypeId;
    T2 := SymbolTable[WriteId].TypeId;
    if T1 <> T2 then
      RaiseError(errIncompatibleTypesNoArgs, []);
    Records[N].Op := OP_NOP;
    Exit;
  end;

  if GetSymbolRec(WriteId).Kind <> KindSUB then
    RaiseError(errIncompatibleTypesNoArgs, []);

  K := GetSymbolRec(PropId).Count;
  if K + 1 <> GetSymbolRec(WriteId).Count then
    RaiseError(errIncompatibleTypesNoArgs, []);

  T1 := typeVOID;
  T2 := SymbolTable[WriteId].TypeId;
  if T1 <> T2 then
    RaiseError(errIncompatibleTypesNoArgs, []);

  for I := 0 to K - 1 do
  begin
    ParamId1 := SymbolTable.GetParamId(PropId, I);
    ParamId2 := SymbolTable.GetParamId(WriteId, I);
    T1 := SymbolTable[ParamId1].TypeId;
    T2 := SymbolTable[ParamId2].TypeId;
    if T1 <> T2 then
      RaiseError(errIncompatibleTypesNoArgs, []);
  end;

  T1 := SymbolTable[PropId].TypeId;
  ParamId2 := SymbolTable.GetParamId(WriteId, K);
  T2 := SymbolTable[ParamId2].TypeId;
  if T1 <> T2 then
    RaiseError(errIncompatibleTypesNoArgs, []);

  Records[N].Op := OP_NOP;
end;

procedure TCode.OperFrameworkOn;
begin
  TKernel(kernel).IsFramework := true;
end;

procedure TCode.OperFrameworkOff;
begin
  TKernel(kernel).IsFramework := false;
end;

procedure TCode.OperToFWObject;
var
  FinTypeId, ClassId: Integer;
  R: TCodeRec;
  aSymb: TSymbolRec;
begin
  R := Records[N];
  aSymb := GetSymbolRec(R.Arg1);
  FinTypeId := aSymb.FinalTypeId;
  ClassId := 0;

  case FinTypeId of
    typeBOOLEAN:
      ClassId := H_TFW_Boolean;
    typeBYTEBOOL:
      ClassId := H_TFW_ByteBool;
    typeWORDBOOL:
      ClassId := H_TFW_WordBool;
    typeLONGBOOL:
      ClassId := H_TFW_LongBool;
    typeBYTE:
      ClassId := H_TFW_Byte;
    typeSMALLINT:
      ClassId := H_TFW_SmallInt;
    typeSHORTINT:
      ClassId := H_TFW_ShortInt;
    typeWORD:
      ClassId := H_TFW_Word;
    typeCARDINAL:
      ClassId := H_TFW_Cardinal;
    typeDOUBLE:
      begin
        if aSymb.TypeId = Id_TDateTime then
          ClassId := H_TFW_DateTime
        else
          ClassId := H_TFW_Double;
      end;
    typeSINGLE:
      ClassId := H_TFW_Single;
    typeEXTENDED:
      ClassId := H_TFW_Extended;
    typeCURRENCY:
      ClassId := H_TFW_Currency;
{$IFNDEF PAXARM}
    typeANSICHAR:
      ClassId := H_TFW_AnsiChar;
    typeANSISTRING:
      ClassId := H_TFW_AnsiString;
{$ENDIF}
    typeWIDECHAR:
      ClassId := H_TFW_WideChar;
    typeINTEGER:
      ClassId := H_TFW_Integer;
    typeINT64:
      ClassId := H_TFW_Int64;
    typeVARIANT:
      ClassId := H_TFW_Variant;
    typeUNICSTRING:
      ClassId := H_TFW_UnicString;
  else
    begin
{$IFNDEF PAXARM}
      if aSymb.HasPAnsiCharType then
        ClassId := H_TFW_AnsiString
      else
{$ENDIF}
        if aSymb.HasPWideCharType then
          ClassId := H_TFW_UnicString
        else
          RaiseError(errRecordRequired, []);
    end;
  end;

  if ClassId = 0 then
    RaiseError(errRecordRequired, []);

  GetSymbolRec(R.Res).TypeId := ClassId;
end;

procedure TCode.OperGetEnumerator;
var
  SymbolTable: TSymbolTable;
  I, break_label_id, continue_label_id, collection_id, enumerator_id,
    element_id, element_TypeId, bool_id, collection_TypeId,
    collection_FinTypeId, RangeTypeId, ElemTypeId, ForInCounter, B1, B2, T,
    temp, element_count, ElemSizeId, PointerTypeId, PointerVarId, N_MOVE_NEXT,
    N_CURRENT, N_LOCK_VARRAY, N_UNLOCK_VARRAY: Integer;
  S: String;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  continue_label_id := 0;
  break_label_id := 0;
  collection_id := Records[N].Arg1;
  enumerator_id := 0;
  element_id := 0;
  bool_id := 0;
  collection_FinTypeId := GetSymbolRec(collection_id).FinalTypeId;
  collection_TypeId := GetSymbolRec(collection_id).TerminalTypeId;
  element_TypeId := GetSymbolRec(element_id).TerminalTypeId;
  ForInCounter := Records[N].Arg2;
  N_MOVE_NEXT := 0;
  N_CURRENT := 0;
  N_LOCK_VARRAY := 0;
  N_UNLOCK_VARRAY := 0;
  for I := N - 1 to Card do
    if (Records[I].Op = OP_MOVE_NEXT) and (Records[I].Arg2 = ForInCounter) then
    begin
      N_MOVE_NEXT := I;
      bool_id := Records[I].Res;
      GetSymbolRec(bool_id).TypeId := typeBOOLEAN;
    end
    else if (Records[I].Op = OP_CURRENT) and (Records[I].Arg2 = ForInCounter)
    then
    begin
      enumerator_id := Records[I].Arg1;
      element_id := Records[I].Res;
      N_CURRENT := I;
    end
    else if (Records[I].Op = OP_LABEL) and (Records[I].Arg2 = ForInCounter) then
    begin
      continue_label_id := Records[I].Arg1;
    end
    else if (Records[I].Op = OP_LABEL) and (Records[I].Res = ForInCounter) then
    begin
      break_label_id := Records[I].Arg1;
    end
    else if (Records[I].Op = OP_LOCK_VARRAY) and (Records[I].Arg2 = ForInCounter)
    then
    begin
      N_LOCK_VARRAY := I;
      if not(collection_FinTypeId in [typeVARIANT, typeOLEVARIANT]) then
        Records[I].Op := OP_NOP;
    end
    else if (Records[I].Op = OP_UNLOCK_VARRAY) and
      (Records[I].Arg2 = ForInCounter) then
    begin
      N_UNLOCK_VARRAY := I;
      if not(collection_FinTypeId in [typeVARIANT, typeOLEVARIANT]) then
        Records[I].Op := OP_NOP;
      break;
    end;

  if (N_MOVE_NEXT = 0) or (N_CURRENT = 0) then
    RaiseError(errInternalError, []);

  if (break_label_id = 0) or (continue_label_id = 0) then
    RaiseError(errInternalError, []);

  if collection_FinTypeId = typeCLASS then
    if GetSymbolRec(collection_id).IsFWArrayVar then
      collection_FinTypeId := typeDYNARRAY;

  case collection_FinTypeId of
    typeCLASS:
      begin
        temp := NewField('GetEnumerator', 0, collection_id);

        Records[N].Op := OP_FIELD;
        Records[N].Arg1 := collection_id;
        Records[N].Arg2 := temp;
        Records[N].Res := temp;

        Records[N + 1].Op := OP_ASSIGN;
        Records[N + 1].GenOp := OP_ASSIGN;
        Records[N + 1].Arg1 := enumerator_id;
        Records[N + 1].Arg2 := temp;
        Records[N + 1].Res := enumerator_id;

        temp := NewField('MoveNext', typeBOOLEAN, enumerator_id);

        Records[N + 2].Op := OP_FIELD;
        Records[N + 2].Arg1 := enumerator_id;
        Records[N + 2].Arg2 := temp;
        Records[N + 2].Res := temp;

        Records[N + 3].Op := OP_GO_FALSE;
        Records[N + 3].Arg1 := break_label_id;
        Records[N + 3].Arg2 := temp;
        Records[N + 3].Res := 0;

        temp := NewField('Current', ElemTypeId, enumerator_id);

        Records[N_CURRENT].Op := OP_FIELD;
        Records[N_CURRENT].Arg1 := enumerator_id;
        Records[N_CURRENT].Arg2 := temp;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        temp := NewField('MoveNext', typeBOOLEAN, enumerator_id);

        Records[N_MOVE_NEXT].Op := OP_FIELD;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := temp;
        Records[N_MOVE_NEXT].Res := temp;

        Records[N_MOVE_NEXT + 1].Op := OP_ASSIGN;
        Records[N_MOVE_NEXT + 1].GenOp := OP_ASSIGN;
        Records[N_MOVE_NEXT + 1].Arg1 := bool_id;
        Records[N_MOVE_NEXT + 1].Arg2 := temp;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end;
    typeVARIANT, typeOLEVARIANT:
      begin
        if (N_LOCK_VARRAY = 0) or (N_UNLOCK_VARRAY = 0) then
          RaiseError(errInternalError, []);

        RangeTypeId := typeINTEGER;
        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        B1 := CreateConst(typeINTEGER, 0);
        B2 := CreateIntegerVar(GetLevel(N));

        ElemSizeId := CreateConst(typeINTEGER, SizeOf(Variant));

        temp := NewTempVar(GetLevel(N), typeVARIANT);
        PointerVarId := NewTempVar(GetLevel(N), H_PVARIANT);

        Records[N_LOCK_VARRAY].Res := PointerVarId;
        Records[N_UNLOCK_VARRAY].Res := PointerVarId;

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        Records[N + 1].Op := OP_PUSH;
        Records[N + 1].Arg1 := collection_id;
        Records[N + 1].Arg2 := 0;
        Records[N + 1].Res := Id_VariantArrayLength;

        Records[N + 2].Op := OP_CALL;
        Records[N + 2].Arg1 := Id_VariantArrayLength;
        Records[N + 2].Arg2 := 1;
        Records[N + 2].Res := B2;

        Records[N + 3].Op := OP_MINUS;
        Records[N + 3].Arg1 := B2;
        Records[N + 3].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N + 3].Res := B2;

        Records[N_CURRENT].Op := OP_TERMINAL;
        Records[N_CURRENT].Arg1 := PointerVarId;
        Records[N_CURRENT].Arg2 := 0;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Records[N_MOVE_NEXT + 2].Op := OP_ADD_INT_MM;
        Records[N_MOVE_NEXT + 2].Arg1 := PointerVarId;
        Records[N_MOVE_NEXT + 2].Arg2 := ElemSizeId;
        Records[N_MOVE_NEXT + 2].Res := PointerVarId;

        Dec(N);
      end;
    typeARRAY:
      begin
        SymbolTable.GetArrayTypeInfo(collection_TypeId, RangeTypeId,
          ElemTypeId);
        if GetSymbolRec(ElemTypeId).FinalTypeId = typeARRAY then
        begin
          // multi-dimensional array
          element_count := SymbolTable.GetHighBoundRec(RangeTypeId).value -
            SymbolTable.GetLowBoundRec(RangeTypeId).value + 1;

          while GetSymbolRec(ElemTypeId).FinalTypeId = typeARRAY do
          begin
            SymbolTable.GetArrayTypeInfo(ElemTypeId, RangeTypeId, ElemTypeId);
            element_count := element_count *
              (SymbolTable.GetHighBoundRec(RangeTypeId).value -
              SymbolTable.GetLowBoundRec(RangeTypeId).value + 1);
          end;

          ElemSizeId := CreateIntegerVar(GetLevel(N));

          S := GetSymbolRec(ElemTypeId).Name;
          S := '__P' + Copy(S, 2, Length(S) - 1);

          PointerTypeId := SymbolTable.RegisterPointerType(0, S, ElemTypeId);
          PointerVarId := NewTempVar(GetLevel(N), PointerTypeId);
          temp := NewTempVar(GetLevel(N), ElemTypeId);
          B1 := CreateConst(typeINTEGER, 1);
          B2 := CreateConst(typeINTEGER, element_count);

          Records[N].Op := OP_ASSIGN;
          Records[N].GenOp := OP_ASSIGN;
          Records[N].Arg1 := enumerator_id;
          Records[N].Arg2 := B1;
          Records[N].Res := enumerator_id;

          Records[N + 1].Op := OP_ADDRESS;
          Records[N + 1].Arg1 := collection_id;
          Records[N + 1].Arg2 := 0;
          Records[N + 1].Res := PointerVarId;

          Records[N + 2].Op := OP_SIZEOF;
          Records[N + 2].Arg1 := ElemTypeId;
          Records[N + 2].Arg2 := 0;
          Records[N + 2].Res := ElemSizeId;

          Records[N_CURRENT].Op := OP_TERMINAL;
          Records[N_CURRENT].Arg1 := PointerVarId;
          Records[N_CURRENT].Arg2 := 0;
          Records[N_CURRENT].Res := temp;

          Records[N_CURRENT + 1].Op := OP_ASSIGN;
          Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
          Records[N_CURRENT + 1].Arg1 := element_id;
          Records[N_CURRENT + 1].Arg2 := temp;
          Records[N_CURRENT + 1].Res := element_id;

          Records[N_MOVE_NEXT].Op := OP_INC;
          Records[N_MOVE_NEXT].Arg1 := enumerator_id;
          Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
          Records[N_MOVE_NEXT].Res := enumerator_id;

          Records[N_MOVE_NEXT + 1].Op := OP_LE;
          Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
          Records[N_MOVE_NEXT + 1].Arg2 := B2;
          Records[N_MOVE_NEXT + 1].Res := bool_id;

          Records[N_MOVE_NEXT + 2].Op := OP_ADD_INT_MM;
          Records[N_MOVE_NEXT + 2].Arg1 := PointerVarId;
          Records[N_MOVE_NEXT + 2].Arg2 := ElemSizeId;
          Records[N_MOVE_NEXT + 2].Res := PointerVarId;

          Dec(N);

          Exit;
        end;

        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        T := GetSymbolRec(RangeTypeId).FinalTypeId;
        B1 := CreateConst(T, SymbolTable.GetLowBoundRec(RangeTypeId).value);
        B2 := CreateConst(T, SymbolTable.GetHighBoundRec(RangeTypeId).value);
        temp := NewTempVar(GetLevel(N), element_TypeId);

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        Records[N_CURRENT].Op := OP_ELEM;
        Records[N_CURRENT].Arg1 := collection_id;
        Records[N_CURRENT].Arg2 := enumerator_id;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end;
    typeDYNARRAY:
      begin
        RangeTypeId := typeINTEGER;
        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        B1 := CreateConst(typeINTEGER, 0);
        B2 := CreateIntegerVar(GetLevel(N));
        temp := NewTempVar(GetLevel(N), element_TypeId);

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        if GetSymbolRec(collection_id).IsFWArrayVar then
          Records[N + 1].Op := OP_PUSH_INSTANCE
        else
          Records[N + 1].Op := OP_PUSH;

        Records[N + 1].Arg1 := collection_id;
        Records[N + 1].Arg2 := 0;

        if GetSymbolRec(collection_id).IsFWArrayVar then
          Records[N + 1].Res := Id_FWArray_GetLength
        else
          Records[N + 1].Res := Id_DynArrayLength;

        Records[N + 2].Op := OP_CALL;
        if GetSymbolRec(collection_id).IsFWArrayVar then
          Records[N + 2].Arg1 := Id_FWArray_GetLength
        else
          Records[N + 2].Arg1 := Id_DynArrayLength;
        Records[N + 2].Arg2 := 1;
        Records[N + 2].Res := B2;

        Records[N + 3].Op := OP_MINUS;
        Records[N + 3].Arg1 := B2;
        Records[N + 3].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N + 3].Res := B2;

        Records[N_CURRENT].Op := OP_ELEM;
        Records[N_CURRENT].Arg1 := collection_id;
        Records[N_CURRENT].Arg2 := enumerator_id;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end;
    typeOPENARRAY:
      begin
        RangeTypeId := typeINTEGER;
        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        B1 := CreateConst(typeINTEGER, 0);
        B2 := CreateIntegerVar(GetLevel(N));
        temp := NewTempVar(GetLevel(N), element_TypeId);

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        Records[N + 1].Op := OP_HIGH;
        Records[N + 1].Arg1 := collection_id;
        Records[N + 1].Arg2 := 0;
        Records[N + 1].Res := B2;

        Records[N_CURRENT].Op := OP_ELEM;
        Records[N_CURRENT].Arg1 := collection_id;
        Records[N_CURRENT].Arg2 := enumerator_id;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end;
{$IFNDEF PAXARM}
    typeANSISTRING, typeSHORTSTRING, typeWIDESTRING,
{$ENDIF}
    typeUNICSTRING:
      begin
        RangeTypeId := typeINTEGER;
        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        B1 := CreateConst(typeINTEGER, 1);
        B2 := CreateIntegerVar(GetLevel(N));
        temp := NewTempVar(GetLevel(N), element_TypeId);

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        Records[N + 1].Op := OP_PUSH;
        Records[N + 1].Arg1 := collection_id;
        Records[N + 1].Arg2 := 0;
        Records[N + 1].Res := Id_AnsiStringLength; // will seek overloaded

        Records[N + 2].Op := OP_CALL;
        Records[N + 2].Arg1 := Id_AnsiStringLength; // will seek overloaded
        Records[N + 2].Arg2 := 1;
        Records[N + 2].Res := B2;

        Records[N_CURRENT].Op := OP_ELEM;
        Records[N_CURRENT].Arg1 := collection_id;
        Records[N_CURRENT].Arg2 := enumerator_id;
        Records[N_CURRENT].Res := temp;

        Records[N_CURRENT + 1].Op := OP_ASSIGN;
        Records[N_CURRENT + 1].GenOp := OP_ASSIGN;
        Records[N_CURRENT + 1].Arg1 := element_id;
        Records[N_CURRENT + 1].Arg2 := temp;
        Records[N_CURRENT + 1].Res := element_id;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end;
    typeSET:
      begin
        if continue_label_id = 0 then
          RaiseError(errInternalError, []);

        RangeTypeId := typeINTEGER;
        GetSymbolRec(enumerator_id).TypeId := RangeTypeId;
        B1 := CreateConst(RangeTypeId, 0);
        B2 := NewTempVar(GetLevel(N), RangeTypeId);

        temp := NewTempVar(GetLevel(N), H_TByteSet);

        Records[N].Op := OP_ASSIGN;
        Records[N].GenOp := OP_ASSIGN;
        Records[N].Arg1 := enumerator_id;
        Records[N].Arg2 := B1;
        Records[N].Res := enumerator_id;

        Records[N + 1].Op := OP_SET_ASSIGN;
        Records[N + 1].GenOp := OP_SET_ASSIGN;
        Records[N + 1].Arg1 := temp;
        Records[N + 1].Arg2 := collection_id;
        Records[N + 1].Res := temp;

        Records[N + 2].Op := OP_SET_COUNTER_ASSIGN;
        Records[N + 2].GenOp := OP_ASSIGN;
        Records[N + 2].Arg1 := B2;
        Records[N + 2].Arg2 := collection_id;
        Records[N + 2].Res := B2;

        collection_id := temp;

        Records[N_CURRENT].Op := OP_ASSIGN_INT_M;
        Records[N_CURRENT].GenOp := OP_ASSIGN;
        Records[N_CURRENT].Arg1 := element_id;
        Records[N_CURRENT].Arg2 := enumerator_id;
        Records[N_CURRENT].Res := element_id;

        Records[N_CURRENT + 1].Op := OP_SET_MEMBERSHIP;
        Records[N_CURRENT + 1].Arg1 := enumerator_id;
        Records[N_CURRENT + 1].Arg2 := collection_id;
        Records[N_CURRENT + 1].Res := bool_id;

        Records[N_CURRENT + 2].Op := OP_GO_FALSE;
        Records[N_CURRENT + 2].Arg1 := continue_label_id;
        Records[N_CURRENT + 2].Arg2 := bool_id;
        Records[N_CURRENT + 2].Res := 0;

        Records[N_MOVE_NEXT].Op := OP_INC;
        Records[N_MOVE_NEXT].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT].Arg2 := CreateConst(typeINTEGER, 1);
        Records[N_MOVE_NEXT].Res := enumerator_id;

        Records[N_MOVE_NEXT + 1].Op := OP_LE;
        Records[N_MOVE_NEXT + 1].Arg1 := enumerator_id;
        Records[N_MOVE_NEXT + 1].Arg2 := B2;
        Records[N_MOVE_NEXT + 1].Res := bool_id;

        Dec(N);
      end
  else
    begin
      RaiseError(errTypeHasNotEnumerator,
        [GetSymbolRec(collection_TypeId).Name]);
    end;
  end;
end;

procedure TCode.OperAddAncestor;
var
  I, ClassId: Integer;
  R: TCodeRec;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  ClassId := Records[N].Arg1;

  if SymbolTable.HasAbstractAncestor(ClassId) then
  begin
    I := N;
    repeat
      Inc(I);
      if I >= Card then
        RaiseError(errInternalError, []);

      R := Records[I];
      if R.Op = OP_END_CLASS_TYPE then
        if R.Arg1 = ClassId then
          break;

      if R.Op = OP_BEGIN_SUB then
        if SymbolTable[R.Arg1].Level = ClassId then
          if SymbolTable[R.Arg1].Kind = KindSUB then
            SymbolTable[R.Arg1].CallMode := cmOVERRIDE;
    until false;
  end;
end;

procedure TCode.DestroyExpressionTempVars(ResultId: Integer);
var
  I, J, K, Id, T, TypeId: Integer;
  RC: TCodeRec;
begin
  K := Card;

  J := -1;
  for I := 1 to K do
    if Records[I].GenOp = OP_ASSIGN then
      if Records[I].Arg1 = ResultId then
      begin
        J := I + 1;
        break;
      end;

  if J = -1 then
    RaiseError(errInternalError, []);

  for I := 1 to K do
    if Records[I].Op = OP_DECLARE_TEMP_VAR then
    begin
      Id := Records[I].Arg2;
      RC := TCodeRec.Create(0, Self);
      RC.Upcase := GetUpcase(I);
      RC.Language := GetLanguage(I);
      RC.ModuleNum := GetModuleNumber(I);
      T := GetSymbolRec(Id).FinalTypeId;
      case T of
        typeVARIANT, typeOLEVARIANT:
          RC.Op := OP_VARIANT_CLR;
{$IFNDEF PAXARM}
        typeANSISTRING:
          RC.Op := OP_ANSISTRING_CLR;
        typeWIDESTRING:
          RC.Op := OP_WIDESTRING_CLR;
{$ENDIF}
        typeUNICSTRING:
          RC.Op := OP_UNICSTRING_CLR;
        typeINTERFACE:
          RC.Op := OP_INTERFACE_CLR;
{$IFDEF PAXARM}
        typeCLASS:
          RC.Op := OP_CLASS_CLR;
{$ENDIF}
        typeDYNARRAY:
          RC.Op := OP_DYNARRAY_CLR;
        typeRECORD, typeARRAY:
          begin
            TypeId := GetSymbolRec(Id).TerminalTypeId;
            if TKernel(kernel).SymbolTable.HasDynamicFields(TypeId) then
              RC.Op := OP_STRUCTURE_CLR
            else
              RC.Op := OP_NOP;
          end;
      else
        begin
          FreeAndNil(RC);
          continue;
        end;
      end;
      RC.Arg1 := Id;
      Insert(J, RC);
    end;
end;

procedure TCode.InsertHostMonitoring;
var
  I, J, SubId, Id, TypeId: Integer;
  R, RC: TCodeRec;
begin
  for I := Card downto 1 do
  begin
    R := Self[I];
    if R.Op = OP_CALL then
    begin
      SubId := R.Arg1;
      if GetSymbolRec(SubId).Kind = KindCONSTRUCTOR then
      begin
        for J := I - 1 downto 1 do
          if Self[J].Op = OP_PUSH_CLSREF then
          begin
            Id := Self[J].Arg1;
            if GetSymbolRec(Id).Kind = KindVAR then
            begin
              TypeId := GetSymbolRec(Id).TerminalTypeId;
              TypeId := GetSymbolRec(TypeId).PatternId;
            end
            else
              TypeId := Id;

            if GetSymbolRec(TypeId).Host then
              if R.Res > 0 then
              begin
                RC := TCodeRec.Create(OP_ONCREATE_HOST_OBJECT, Self);
                RC.Upcase := GetUpcase(I);
                RC.Language := GetLanguage(I);
                RC.ModuleNum := GetModuleNumber(I);
                RC.Arg1 := R.Res;
                Insert(I + 1, RC);
              end;

            break;
          end;
      end
      else if SubId = Id_TObject_Free then
      begin
        for J := I - 1 downto 1 do
          if Self[J].Op = OP_PUSH_INST then
          begin
            Id := Self[J].Arg1;
            TypeId := GetSymbolRec(Id).TerminalTypeId;

            if GetSymbolRec(TypeId).Host then
            begin
              RC := TCodeRec.Create(OP_ONDESTROY_HOST_OBJECT, Self);
              RC.Upcase := GetUpcase(I);
              RC.Language := GetLanguage(I);
              RC.ModuleNum := GetModuleNumber(I);
              RC.Arg1 := Id;
              Insert(J, RC);
            end;

            break;
          end;
      end;
    end;
  end;
end;

procedure TCode.SetLastCondRaise;
var
  I: Integer;
  b: Boolean;
  R: TCodeRec;
begin
  b := false;
  for I := Card downto 1 do
  begin
    R := Self[I];
    if R.Op = OP_END_SUB then
      b := true
    else if R.Op = OP_INIT_SUB then
      b := false
    else if R.Op = OP_END_GLOBAL_BLOCK then
      b := true
    else if R.Op = OP_BEGIN_GLOBAL_BLOCK then
      b := false
    else if R.Op = OP_COND_RAISE then
    begin
      if b then
        R.Arg2 := 1;
      b := false;
    end;
  end;
end;

function TCode.ParamHasBeenChanged(I, Id: Integer): Boolean;
var
  R: TCodeRec;
  SubId: Integer;
begin
  result := false;
  if I <= 0 then
    Exit;
  SubId := GetSymbolRec(Id).Level;
  if SubId <= 0 then
    Exit;
  if not(GetSymbolRec(SubId).Kind in kindSUBS) then
    Exit;
  repeat
    R := Records[I];
    if R.Op = OP_PARAM_CHANGED then
    begin
      if R.Arg1 = Id then
      begin
        result := true;
        Exit;
      end;
    end
    else if R.Op = OP_INIT_SUB then
      if R.Arg1 = SubId then
        Exit;
    Dec(I);
  until I = 0;
end;

procedure TCode.CreateExecLines(MR: TModuleRec);
var
  I, J, SourceLine: Integer;
begin
  for I := MR.P1 to MR.P3 do
    if Self[I].Op = OP_SEPARATOR then
    begin
      SourceLine := Self[I].Arg2;
      J := I;
      repeat
        Inc(J);
        if J > Card then
          break;
        if Self[J].Op = OP_SEPARATOR then
          break;
        if Self[J].Op = OP_SET_CODE_LINE then
        begin
          MR.ExecutableLines.Add(SourceLine);
          break;
        end;
      until false;
    end;
end;

procedure TCode.AddWarnings;
var
  I, J, Id, SubId, PosUsed, ParamId, OwnerId: Integer;
  R, RJ: TCodeRec;
  NeverUsed, NotInit: Boolean;
  S1, S2: String;
  SymbolTable: TSymbolTable;
  ok, b: Boolean;
begin

  ok := false;
  SymbolTable := TKernel(kernel).SymbolTable;
  for I := 1 to Card do
  begin
    R := Records[I];

    if R.Op = OP_WARNINGS_ON then
      ok := true
    else if R.Op = OP_WARNINGS_OFF then
      ok := false;

    if not ok then
      continue;

    if R.Op = OP_DECLARE_MEMBER then
    begin
      Id := R.Arg2;
      if GetSymbolRec(Id).Vis in [cvPrivate, cvStrictPrivate] then
        if used_private_members.IndexOf(Id) = -1 then
        begin
          b := false;
          for J := 1 to Card do
          begin
            RJ := Records[J];
            if (RJ.Op = OP_CALL) or (RJ.Op = OP_ADDRESS) then
            begin
              if RJ.Arg1 = Id then
              begin
                b := true;
                break;
              end;
            end
            else if RJ.Op = OP_CREATE_EVENT then
            begin
              if RJ.Arg2 = Id then
              begin
                b := true;
                break;
              end;
            end
            else if RJ.Op = OP_SET_EVENT_PROP then
            begin
              if GetSymbolRec(RJ.Res).PatternId = Id then
              begin
                b := true;
                break;
              end;
            end;
          end;
          if not b then
          begin
            N := I;
            S2 := GetSymbolRec(Id).Name;
            if IsValidName(S2) then
              TKernel(kernel).CreateWarning(wrnPrivateNeverUsed, [S2]);
          end;
        end;
    end
    else if (R.Op = OP_DECLARE_LOCAL_VAR) or (R.Op = OP_INIT_SUB) then
    begin
      NeverUsed := true;
      NotInit := true;
      SubId := R.Arg1;
      if R.Op = OP_INIT_SUB then
      begin
        Id := TKernel(kernel).SymbolTable.GetResultId(SubId);
        if GetSymbolRec(SubId).FinalTypeId = typeVOID then
          continue;
      end
      else
      begin
        Id := R.Arg2;
      end;

      if GetSymbolRec(Id).Param then
        continue;
      if GetSymbolRec(Id).Kind in [KindNONE, KindCONST] then
        continue;
      if not IsValidName(GetSymbolRec(Id).Name) then
        continue;

      PosUsed := 0;

      J := I;
      repeat
        Inc(J);
        RJ := Records[J];
        if RJ.Op = OP_NOP then
          continue;

        if (RJ.Op = OP_END_SUB) and (RJ.Arg1 = SubId) then
          break;
        if RJ.Op = OP_END_MODULE then
          break;
        if (RJ.Arg1 = Id) or (RJ.Arg2 = Id) or (RJ.Res = Id) then
          if RJ.Op <> OP_DESTROY_LOCAL_VAR then
          begin
            PosUsed := J;
            NeverUsed := false;
          end;

        if RJ.Arg1 > 0 then
        begin
          OwnerId := GetSymbolRec(RJ.Arg1).OwnerId;
          if OwnerId = Id then
          begin
            PosUsed := J;
            NeverUsed := false;
          end;
        end;

        if RJ.Arg2 > 0 then
        begin
          OwnerId := GetSymbolRec(RJ.Arg2).OwnerId;
          if OwnerId = Id then
          begin
            PosUsed := J;
            NeverUsed := false;
          end;
        end;

        if RJ.Res > 0 then
        begin
          OwnerId := GetSymbolRec(RJ.Res).OwnerId;
          if OwnerId = Id then
          begin
            PosUsed := J;
            NeverUsed := false;
          end;
        end;

        if RJ.GenOp = OP_ERR_ABSTRACT then
          if R.Op = OP_INIT_SUB then
          begin
            NotInit := false;
            PosUsed := J;
            NeverUsed := false;
          end;

        if (RJ.GenOp = OP_ASSIGN) or (RJ.GenOp = OP_ABSOLUTE) or
          (RJ.GenOp = OP_SET_LENGTH) or (RJ.GenOp = OP_SET_LENGTH_EX) then
          if RJ.Arg1 = Id then
            NotInit := false;
        if RJ.GenOp = OP_PUSH then
          if RJ.Arg1 = Id then
            if GetSymbolRec(RJ.Res).Kind in kindSUBS then
              if RJ.Res <> JS_FunctionCallId then
              begin
                ParamId := SymbolTable.GetParamId(RJ.Res, RJ.Arg2);
                if GetSymbolRec(ParamId).ByRef or GetSymbolRec(ParamId).ByRefEx
                then
                  NotInit := false;
              end;
      until J = Card;

      if NeverUsed then
      begin
        S1 := GetSymbolRec(Id).Name;
        if SubId = 0 then
        begin
          N := I;
          TKernel(kernel).CreateWarning(wrnNeverUsed, [S1]);
        end
        else
        begin
          S2 := GetSymbolRec(SubId).Name;

          N := I;
          if R.Op = OP_INIT_SUB then
          begin
            while (N > 0) do
            begin
              Dec(N);
              if Records[N].Op = OP_BEGIN_SUB then
                if Records[N].Arg1 = SubId then
                  break;
            end;

            if GetSymbolRec(SubId).FinalTypeId <> typeRECORD then
              if IsValidName(GetSymbolRec(SubId).Name) then
                TKernel(kernel).CreateWarning(wrnReturnValue, [S2]);
          end
          else
            TKernel(kernel).CreateWarning(wrnNeverUsedIn, [S1, S2]);
        end;
      end
      else if NotInit then
      begin
        N := I;
        if R.Op = OP_INIT_SUB then
        begin
          while (N > 0) do
          begin
            Dec(N);
            if Records[N].Op = OP_BEGIN_SUB then
              if Records[N].Arg1 = SubId then
                break;
          end;

          S2 := GetSymbolRec(SubId).Name;
          if GetSymbolRec(SubId).FinalTypeId <> typeRECORD then
            if IsValidName(S2) then
              TKernel(kernel).CreateWarning(wrnReturnValue, [S2]);
        end
        else
        begin
          if PosUsed > 0 then
            N := PosUsed
          else
            N := I;
          S1 := GetSymbolRec(Id).Name;
          if not(GetSymbolRec(Id).FinalTypeId in [typeRECORD, typeARRAY]) then
            TKernel(kernel).CreateWarning(wrnNotInit, [S1]);
        end;
      end;
    end;
  end;
end;

function TCode.GetCompletionVisibility(Id: Integer; NN: Integer)
  : TMemberVisibilitySet;
var
  TypeId, I1, I2, SubId, L, Lang: Integer;
begin
  Lang := GetLanguage(NN);
  result := [cvPublic, cvPublished];

  TypeId := GetSymbolRec(Id).TypeId;

  if TypeId = 0 then
    Exit;

  if not GetSymbolRec(TypeId).Host then
    if Lang = PASCAL_LANGUAGE then
    begin
      I1 := TKernel(kernel).Modules.IndexOfModuleById(Id);
      I2 := TKernel(kernel).Modules.IndexOfModuleById(TypeId);

      if I1 = I2 then // the same module
      begin
        result := [cvPrivate, cvProtected, cvPublic, cvPublished];
        Exit;
      end;
    end;

  // different modules

  SubId := GetCurrSubId(NN);
  if SubId = 0 then
    Exit;

  L := GetSymbolRec(SubId).Level;

  if L = 0 then
    Exit;

  if GetSymbolRec(L).Kind = KindNAMESPACE then
    Exit;

  if GetSymbolRec(L).Kind <> KindTYPE then
    Exit;

  if GetSymbolRec(L).FinalTypeId <> typeCLASS then
    Exit;

  if (L = TypeId) or GetSymbolRec(L).Inherits(TypeId) then
    result := [cvProtected, cvPublic, cvPublished]
  else
    result := [cvPublic, cvPublished];
end;

procedure TCode.RemoveDeclaredVar(Id: Integer);
var
  I: Integer;
  R: TCodeRec;
begin
  for I := N + 1 to Card do
  begin
    R := Records[I];
    if R.Op = OP_STMT then
      break;
    if R.Op = OP_DECLARE_LOCAL_VAR then
      if R.Arg2 = Id then
      begin
        R.Op := OP_NOP;
        R.GenOp := OP_NOP;
        Exit;
      end;
  end;
end;

function TCode.LookupTempVarId(Level, FinTypeId: Integer): Integer;

  function GetStmtEx(I: Integer; Id: Integer): Integer;
  var
    R: TCodeRec;
  begin
    result := I;

    repeat
      R := Records[result];

      if R.Res = Id then
      begin
        result := -100;
        Exit;
      end;

      if R.Op = OP_STMT then
      begin
        Exit;
      end
      else
      begin
        Dec(result);

        if R.Op = OP_BEGIN_MODULE then
          break;
        if R.Op = OP_INIT_SUB then
          break;

        if result = 1 then
          Exit;
      end;
    until false;
  end;

var
  I, Id, NN, NN2: Integer;
  R: TSymbolRec;
begin
  result := 0;
  if temp_var_list.Count = 0 then
    Exit;

  NN2 := -1;

  for I := temp_var_list.Count - 1 downto 0 do
  begin
    Id := temp_var_list.Keys[I];
    R := GetSymbolRec(Id);
    if R.Level = Level then
    begin
      if R.FinalTypeId = FinTypeId then
      begin
        if NN2 = -1 then
        begin
          NN2 := GetStmtEx(N, Id);
          if NN2 = -100 then
            Exit;
        end;

        NN := temp_var_list.Values[I];
        if NN2 = NN then
          Exit;

        result := Id;
        Exit;
      end;
    end
    else
      break;
  end;
end;

procedure TCode.RemoveLoadProc;
var
  L: TIntegerList;
  R: TCodeRec;

  procedure Proc(Id: Integer);
  begin
    L.Add(Id);
  end;

var
  I, K, Op: Integer;
  b: Boolean;
begin
  b := TKernel(kernel).BuildWithRuntimePackages;

  if b then
    Exit;

  L := TIntegerList.Create;

  try
    K := 0;
    for I := 1 to Card do
    begin
      R := Records[I];
      Op := R.Op;

      if Op = OP_LOAD_PROC then
        continue;
      if Op = OP_SET_CODE_LINE then
        continue;
      if Op = OP_SEPARATOR then
        continue;

      if b then
        if Op = OP_END_MODULE then
        begin
          K := I;
          break;
        end;

      Proc(R.Arg1);
      Proc(R.Arg2);
      Proc(R.Res);
    end;
    L.QuickSort;
    for I := K + 1 to Card do
    begin
      R := Records[I];
      if R.Op = OP_LOAD_PROC then
        if L.BinSearch(R.Arg1) = -1 then
          R.Op := OP_NOP;
    end;

  finally
    FreeAndNil(L);
  end;
end;

function TCode.LookupInExtraUnitList(const S: String): Integer;
var
  I: Integer;
  Upcase: Boolean;
begin
  result := 0;
  Upcase := GetUpcase(N);
  for I := extra_using_list.Count - 1 downto 0 do
  begin
    result := TKernel(kernel).SymbolTable.LookUp(S, extra_using_list[I],
      Upcase, MaxInt);
    if result > 0 then
      Exit;
  end;
end;

procedure TCode.CheckExpansions;
var
  I, J, K, FTD, FTE, TypeD, TypeE: Integer;
  TD: TTypeDefRec;
  TE: TTypeExpRec;
  TRD, TRE: TTypeRestrictionRec;
  S: String;
begin
  for K := 0 to TKernel(kernel).TypeDefList.Expansions.Count - 1 do
  begin
    I := TKernel(kernel).TypeDefList.Expansions.Keys[K];
    J := TKernel(kernel).TypeDefList.Expansions.Values[K];

    TD := TKernel(kernel).TypeDefList[J];
    TE := TKernel(kernel).TypeDefList.TypeExpList[I];

    for I := 0 to TD.ParamList.Count - 1 do
    begin
      TRD := TTypeRestrictionRec(TD.ParamList.Objects[I]);
      if TRD = nil then
        continue;
      FTD := GetSymbolRec(TRD.Id).FinalTypeId;
      if not(FTD in [typeCLASS, typeINTERFACE]) then
      begin
        N := TRD.N;
        S := GetSymbolRec(TRD.Id).Name;
        RaiseError(errTypeIsNotValidConstraint, [S]);
      end;
      TRE := TTypeRestrictionRec(TE.ParamList.Objects[I]);
      FTE := GetSymbolRec(TRE.Id).FinalTypeId;
      if FTD <> FTE then
      begin
        N := TRE.N;
        RaiseError(errIncompatibleTypes, [GetSymbolRec(TRD.Id).Name,
          GetSymbolRec(TRE.Id).Name]);
        Exit;
      end;
      if (FTD = typeCLASS) and (FTE = typeCLASS) then
      begin
        TypeD := GetSymbolRec(TRD.Id).TerminalTypeId;
        TypeE := GetSymbolRec(TRE.Id).TerminalTypeId;
        if TypeD = TypeE then
          Exit;
        if TKernel(kernel).SymbolTable.Inherits(TypeE, TypeD) then
          Exit;
        N := TRE.N;
        RaiseError(errIncompatibleTypes, [GetSymbolRec(TRD.Id).Name,
          GetSymbolRec(TRE.Id).Name]);
        Exit;
      end;
      if FTD = typeRECORD then
      begin
        if FTE <> typeRECORD then
        begin
          N := TRE.N;
          RaiseError(errIncompatibleTypesNoArgs, []);
        end;
        Exit;
      end;
    end;
  end;
end;

procedure TCode.RemoveUnusedLabels;
var
  I, Op: Integer;
  L: TIntegerList;
begin
  L := TIntegerList.Create;
  try
    for I := 1 to Card do
    begin
      Op := Records[I].Op;
      if (Op = OP_GO) or (Op = OP_GO_FALSE) or (Op = OP_GO_TRUE) or
        (Op = OP_GO_TRUE_BOOL) or (Op = OP_GO_FALSE_BOOL) or (Op = OP_GO_DL)
      then
        L.Add(Records[I].Arg1);

    end;
    for I := 1 to Card do
    begin
      Op := Records[I].Op;
      if Op = OP_LABEL then
        if L.IndexOf(Records[I].Arg1) = -1 then
        begin
          Records[I].Op := OP_NOP;
          Records[I].GenOp := OP_NOP;
        end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TCode.GetPAX64: Boolean;
begin
  result := TKernel(kernel).PAX64;
end;

function TCode.GetSizeOfPointer: Integer;
begin
  if PAX64 then
    result := 8
  else
    result := 4;
end;

procedure TCode.CheckOverride;
var
  I, J: Integer;
  List: TIntegerList;
begin
  List := TIntegerList.Create;
  try
    for I := 1 to Card do
      if Records[I].Op = OP_CHECK_OVERRIDE then
        List.Add(I);
    for J := 0 to List.Count - 1 do
    begin
      I := List[J];
      if Records[I].Op = OP_CHECK_OVERRIDE then
      begin
        N := I;
        OperCheckOverride(List);
      end;
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TCode.OperAssignLambdaTypes;

var
  SubId, ClassId: Integer;
  SymbolTable: TSymbolTable;

  function AssignTypes(T: Integer): Boolean;
  var
    I, SubId2, id1, id2, K: Integer;
  begin
    result := false;
    if GetSymbolRec(T).FinalTypeId <> typeINTERFACE then
      Exit;
    SubId2 := SymbolTable.LookupAnonymousMethod(T);
    if SubId2 = 0 then
      Exit;
    K := GetSymbolRec(SubId2).Count;
    if K <> GetSymbolRec(SubId).Count then
      Exit;
    id1 := SymbolTable.GetResultId(SubId);
    id2 := SymbolTable.GetResultId(SubId2);
    GetSymbolRec(id1).TypeId := GetSymbolRec(id2).TypeId;
    GetSymbolRec(SubId).TypeId := GetSymbolRec(id2).TypeId;
    for I := 0 to K - 1 do
    begin
      id1 := SymbolTable.GetParamId(SubId, I);
      id2 := SymbolTable.GetParamId(SubId2, I);
      GetSymbolRec(id1).TypeId := GetSymbolRec(id2).TypeId;
    end;
    SymbolTable.RegisterSupportedInterface(ClassId, T);
    result := true;
  end;

var
  I, ResId, ParamId: Integer;
  RI: TCodeRec;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  Records[N].Op := OP_NOP;

  SubId := Records[N].Arg1;
  I := N;
  repeat
    Inc(I);
    RI := Records[I];
    if RI.Op = OP_ASSIGN_LAMBDA_TYPES then
      if RI.Arg1 = SubId then
        break;
  until false;

  ClassId := Records[I].Arg2;
  ResId := Records[I].Res;

  Records[I].Op := OP_NOP;

  for I := I + 1 to Card do
  begin
    RI := Records[I];
    if RI.Op = OP_ASSIGN then
    begin
      if RI.Arg2 = ResId then
      begin
        if AssignTypes(GetSymbolRec(RI.Arg1).TerminalTypeId) then
          Exit
        else
          break;
      end;
    end
    else if RI.Op = OP_PUSH then
    begin
      if RI.Arg1 = ResId then
      begin
        ParamId := SymbolTable.GetParamId(RI.Res, RI.Arg2);
        if AssignTypes(GetSymbolRec(ParamId).TerminalTypeId) then
          Exit
        else
          break;
      end;
    end;
  end;
  RaiseError(errIncompatibleTypesNoArgs, []);
end;

procedure TCode.RestoreFieldType(J: Integer);
var
  OwnerId, Id, TypeId, PatternId, SubId, N1: Integer;
  S: String;
  SymbolTable: TSymbolTable;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  if Records[J - 1].Op = OP_FIELD then
  begin
    if Records[J - 1].Res = Records[J].Arg1 then
    begin
      OwnerId := Records[J - 1].Arg1;
      TypeId := SymbolTable[OwnerId].TypeId;

      if TypeId = 0 then
        RestoreFieldType(J - 1);

      TypeId := SymbolTable[OwnerId].TypeId;

      if TypeId = 0 then
        Exit;

      if SymbolTable[TypeId].Kind <> KindTYPE then
        Exit;

      Id := Records[J].Arg1;
      S := SymbolTable[Id].Name;
      PatternId := SymbolTable.LookUp(S, TypeId, GetUpcase(J));
      if PatternId > 0 then
        SymbolTable[Id].TypeId := SymbolTable[PatternId].TypeId;
    end;
  end
  else if Records[J - 1].Op = OP_CALL then
  begin
    if Records[J - 1].Res = Records[J].Arg1 then
    begin
      SubId := Records[J - 1].Arg1;
      TypeId := SymbolTable[SubId].TypeId;

      if TypeId = 0 then
      begin
        N1 := J - 1;
        repeat
          Dec(N1);
          if N1 = 0 then
            Exit;
          if Records[N1].Op = OP_FIELD then
            if Records[N1].Res = SubId then
              break;
        until false;

        RestoreFieldType(N1);
        TypeId := GetSymbolRec(Records[N1].Arg1).TypeId;
        S := SymbolTable[SubId].Name;
        PatternId := SymbolTable.LookUp(S, TypeId, GetUpcase(J));
        if PatternId > 0 then
          TypeId := SymbolTable[PatternId].TypeId;
      end;

      Id := Records[J].Arg1;
      SymbolTable[Id].TypeId := TypeId;
    end;
  end;
end;

function TCode.GetTrueSubId: Integer;
var
  T, K: Integer;
  R: TCodeRec;
begin
  R := Records[N];
  result := R.Arg1;
  if GetSymbolRec(result).Kind in kindSUBS then
    Exit;

  T := GetSymbolRec(R.Arg1).TerminalTypeId;
  result := GetSymbolRec(T).PatternId;

  if not(GetSymbolRec(result).Kind in kindSUBS) then
  begin
    K := N - 1;
    repeat
      if Self[K].Op = OP_GET_VMT_ADDRESS then
        if Self[K].Res = R.Arg1 then
        begin
          result := Self[K].Arg2;
          break;
        end;

      if Self[K].Op = OP_SEPARATOR then
        break;

      Dec(K);
    until false;
  end;
end;

procedure TCode.CreateMethodEntryLists;
var
  ClassList: TClassList;
  CR, CA: TClassRec;
  I, J, K, SubId, L, ClassIndex, V, CountGlobal, AncestorIndex, MethodIndex,
    ClassId, AncestorId: Integer;
  R: TCodeRec;
  L1, L2: TIntegerList;
  b: Boolean;
begin
  ClassList := TKernel(kernel).prog.ClassList;
  L1 := TIntegerList.Create;
  L2 := TIntegerList.Create;
  try
    for I := 1 to Card do
    begin
      R := Records[I];
      if R.Op = OP_BEGIN_CLASS_TYPE then
      begin
        ClassId := R.Arg1;
        AncestorId := GetSymbolRec(ClassId).AncestorId;
        if GetSymbolRec(AncestorId).Host then
          L1.Add(ClassId)
        else
          L2.Add(ClassId);
      end;
    end;

    while L2.Count > 0 do
    begin
      for I := L2.Count - 1 downto 0 do
      begin
        ClassId := L2[I];
        AncestorId := GetSymbolRec(ClassId).AncestorId;
        if L1.IndexOf(AncestorId) >= 0 then
        begin
          L1.Add(ClassId);
          L2.RemoveAt(I);
          break;
        end;
      end;
    end;

    for I := 0 to L1.Count - 1 do
    begin
      ClassId := L1[I];
      AncestorId := GetSymbolRec(ClassId).AncestorId;
      ClassIndex := GetSymbolRec(ClassId).ClassIndex;
      AncestorIndex := GetSymbolRec(AncestorId).ClassIndex;
      CR := ClassList[ClassIndex];
      if CR = nil then
        RaiseError(errInternalError, []);
      CA := ClassList[AncestorIndex];
      if CA = nil then
        RaiseError(errInternalError, []);

      b := GetSymbolRec(AncestorId).Host;

      K := 0;

      if not b then
      begin
        for J := 0 to System.Length(CA.ByteCodeMethodEntryList) - 1 do
        begin
          V := CA.ByteCodeMethodEntryList[J];
          if V = 0 then
          begin
            K := J;
            break;
          end;
          SetLength(CR.ByteCodeMethodEntryList, J + 1);
          CR.ByteCodeMethodEntryList[J] := V;
        end;
      end;

      for J := 1 to Card do
      begin
        R := Records[J];
        if R.Op = OP_INIT_SUB then
        begin
          SubId := R.Arg1;

          MethodIndex := GetSymbolRec(SubId).MethodIndex;

          if MethodIndex > 0 then
            CR.VirtualMethodEntryList[MethodIndex] := J;

          SetLength(CR.ByteCodeMethodEntryList, K + 1);
          CR.ByteCodeMethodEntryList[K] := J;
          Inc(K);
        end;
      end;
    end;

    CountGlobal := 0;
    for J := 1 to Card do
    begin
      R := Records[J];
      if R.Op = OP_INIT_SUB then
      begin
        SubId := R.Arg1;
        L := GetSymbolRec(SubId).Level;
        if L = 0 then
        begin
          SetLength(TKernel(kernel).prog.ByteCodeGlobalEntryList,
            CountGlobal + 1);
          TKernel(kernel).prog.ByteCodeGlobalEntryList[CountGlobal] := J;
          Inc(CountGlobal);
        end
        else if GetSymbolRec(L).FinalTypeId <> typeCLASS then
        begin
          SetLength(TKernel(kernel).prog.ByteCodeGlobalEntryList,
            CountGlobal + 1);
          TKernel(kernel).prog.ByteCodeGlobalEntryList[CountGlobal] := J;
          Inc(CountGlobal);
        end;
      end
      else if R.Op = OP_JUMP_SUB then
      begin
        SetLength(TKernel(kernel).prog.ByteCodeGlobalEntryList,
          CountGlobal + 1);
        TKernel(kernel).prog.ByteCodeGlobalEntryList[CountGlobal] := J;
        Inc(CountGlobal);
      end;
    end;

  finally
    FreeAndNil(L1);
    FreeAndNil(L2);
  end;
end;

procedure TCode.CreateEvalList(L: TStringList);
var
  I: Integer;
  R: TCodeRec;
  S: String;
begin
  L.Add('Create');
  for I := 1 to Card do
  begin
    R := Records[I];
    S := '';
    if R.Op = OP_EVAL then
      S := GetSymbolRec(R.Res).Name
    else if R.Op = OP_ASSIGN_TYPE then
      S := GetSymbolRec(R.Arg2).Name
    else if R.Op = OP_FIELD then
      S := GetSymbolRec(R.Res).Name
    else if (R.Op = OP_BEGIN_SUB) or (R.Op = OP_CALL) then
      S := GetSymbolRec(R.Arg1).Name;
    if S <> '' then
      if L.IndexOf(S) = -1 then
        L.Add(S);
  end;
end;

procedure TCode.InsertCallHostEvents;
var
  I: Integer;
  RI, RC: TCodeRec;
begin
  I := 0;
  repeat
    Inc(I);
    if I > Card then
      break;
    RI := Records[I];
    if RI.Op = OP_BEGIN_CALL then
    begin
      if GetSymbolRec(RI.Arg1).Kind in kindSUBS then
        if GetSymbolRec(RI.Arg1).Host then
        begin
          RC := TCodeRec.Create(OP_BEFORE_CALL_HOST, Self);
          RC.Arg1 := RI.Arg1;
          Insert(I, RC);
          Inc(I);
        end;
    end
    else if RI.Op = OP_CALL then
    begin
      if GetSymbolRec(RI.Arg1).Kind in kindSUBS then
        if GetSymbolRec(RI.Arg1).Host then
        begin
          RC := TCodeRec.Create(OP_AFTER_CALL_HOST, Self);
          RC.Arg1 := RI.Arg1;
          Insert(I + 1, RC);
          Inc(I);
        end;
    end;
  until false;
end;

// TCodeRec --------------------------------------------------------------------

constructor TCodeRec.Create(i_OP: Integer; Code: TCode);
begin
  OwnerObject := Code;

  Op := i_OP;
  GenOp := i_OP;

  Arg1 := 0;
  Arg2 := 0;
  Res := 0;

  LinePos := -1;
  SavedLevel := -1;

  if Code = nil then
    Exit;

  with Code do
  begin
    Upcase := GetUpcase(N);
    Language := GetLanguage(N);
    ModuleNum := GetModuleNumber(N);
    SavedLevel := GetLevel(N);
  end;
end;

procedure TCodeRec.SwapArguments;
var
  temp: Integer;
begin
  temp := Arg1;
  Arg1 := Arg2;
  Arg2 := temp;

  SwappedArgs := not SwappedArgs;
end;

function TCodeRec.Clone: TCodeRec;
begin
  result := TCodeRec.Create(Op, OwnerObject);
  result.Arg1 := Arg1;
  result.Arg2 := Arg2;
  result.Res := Res;
  result.Upcase := Upcase;
  result.Language := Language;
  result.IsStatic := IsStatic;
  result.SavedSubId := SavedSubId;
  result.SwappedArgs := SwappedArgs;
  result.LinePos := LinePos;
  result.CodeRecTag := CodeRecTag;
  result.SavedLevel := SavedLevel;
  result.IsInherited := IsInherited;
end;

procedure TCodeRec.SaveToStream(S: TStream);
begin
  S.Write(Op, SizeOf(Op));
  S.Write(Arg1, SizeOf(Arg1));
  S.Write(Arg2, SizeOf(Arg2));
  S.Write(Res, SizeOf(Res));
  S.Write(GenOp, SizeOf(GenOp));
  S.Write(IsStatic, SizeOf(IsStatic));
  S.Write(SwappedArgs, SizeOf(SwappedArgs));
  S.Write(SavedSubId, SizeOf(SavedSubId));
  S.Write(CodeRecTag, SizeOf(CodeRecTag));
  S.Write(SavedLevel, SizeOf(SavedLevel));
  S.Write(IsInherited, SizeOf(IsInherited));
end;

procedure TCodeRec.LoadFromStream(S: TStream);
begin
  S.Read(Op, SizeOf(Op));
  S.Read(Arg1, SizeOf(Arg1));
  S.Read(Arg2, SizeOf(Arg2));
  S.Read(Res, SizeOf(Res));
  S.Read(GenOp, SizeOf(GenOp));
  S.Read(IsStatic, SizeOf(IsStatic));
  S.Read(SwappedArgs, SizeOf(SwappedArgs));
  S.Read(SavedSubId, SizeOf(SavedSubId));
  S.Read(CodeRecTag, SizeOf(CodeRecTag));
  S.Read(SavedLevel, SizeOf(SavedLevel));
  S.Read(IsInherited, SizeOf(IsInherited));
end;

end.
