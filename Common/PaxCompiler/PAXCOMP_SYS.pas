////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_SYS.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}

{$O-}
{$Q-}
{$R-}

unit PAXCOMP_SYS;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PaxInfos,
  PAXCOMP_TYPES,
  PAXCOMP_CONSTANTS;

{$IFDEF VARIANTS}
type
  UInt64 = Int64;
  PBoolean = ^Boolean;
{$ELSE}
type
  UInt64 = Int64;
  IInterface = IUnknown;
  PBoolean = ^Boolean;
  PInteger = ^Integer;
  PWord = ^Word;
{$ENDIF}
  PIUnknown = ^IUnknown;

{$IFNDEF VARIANTS}
const
  varShortInt = $0010;
  varWord     = $0012;
  varLongWord = $0013;
{$ENDIF}

{$IFDEF UNIX}
const
   MEM_COMMIT = 0;
   PAGE_EXECUTE_READWRITE = 0;
{$ENDIF}

const
  FirstCompiledScriptVersion = 102;
var
  FirstLocalId: Integer = 1000000;

  StreamVersion: Integer = 137;
  CompiledScriptVersion: Integer = 128;

  StdCard: Integer = 0;
  StdSize: Integer = 0;
const
  MaxPublishedProps = 50; // importer only
  varUString  = $0102; { Unicode string 258 } {not OLE compatible}

  MinDouble   =  4.9406564584124654418e-324;
  MaxDouble   =  1.7976931348623157081e+308;

{$IFDEF VARIANTS}
  IntegerVariantTypes = [varByte, varSmallInt, varInteger, varWord, varLongWord, varInt64];
{$ELSE}
  IntegerVariantTypes = [varByte, varSmallInt, varInteger];
{$ENDIF}

  NaN         =  0.0 / 0.0;
  {$EXTERNALSYM NaN}
  {$HPPEMIT 'static const Extended NaN = 0.0 / 0.0 ; // UW 2013-08-13 emit}
  Infinity    =  1.0 / 0.0;
  {$EXTERNALSYM Infinity}
  {$HPPEMIT 'static const Extended Infinity = 1.0 / 0.0; // UW 2013-08-13 emit}
  NegInfinity = -1.0 / 0.0;
  {$EXTERNALSYM NegInfinity}
  {$HPPEMIT 'static const Extended NegInfinity = -1.0 / 0.0; // UW 2013-08-13 emit}

  SecsPerHour   = 60 * 60;
  SecsPerDay    = SecsPerHour * 24;
  MSecsPerDay   = SecsPerDay * 1000;
  MSecsPerHour  = SecsPerHour * 1000;

{$IFDEF MACOS32}
  varClass = varError;
  varPointer = $15;
{$ELSE}
  {$IFDEF LINUX}
    varClass = varError;
    varPointer = $15;
  {$ELSE}
    {$IFDEF PAXARM_DEVICE}
      varClass = varError;
      varPointer = varAny;
    {$ELSE}
      varClass = $0E;
      varPointer = varAny;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  H_SelfPtr: Integer = 32;

  H_ExceptionPtr_64 = 40;
  H_ByteCodePtr_64 = 48;
  H_Flag_64 = 56;
  H_SkipPop_64 = 64;
  FirstShiftValue_64 = 141;

  H_ExceptionPtr_32 = 36;
  H_ByteCodePtr_32 = 40;
  H_Flag_32 = 44;
  H_SkipPop_32 = 48;
  FirstShiftValue_32 = 105;
type
  TPortDir = (portNone, portPlatform, portDeprecated, portLibrary);

  TRecord4 = record
    x: longInt;
  end;

  TRecord8 = record
    x1, x2: longInt;
  end;

  TRecord12 = record
    x1, x2, x3: longInt;
  end;

  TArray4 = array[0..0] of longInt;
  TArray8 = array[0..1] of longInt;
  TArray12 = array[0..2] of longInt;

  TRange1 = 0..31;
  TSet1 = set of TRange1;
  TRange2 = 0..63;
  TSet2 = set of TRange2;
  TRange4 = 0..127;
  TSet4 = set of TRange4;
  TRange8 = 0..255;
  TSet8 = set of TRange8;

{$IFNDEF PAXARM}
  DynarrayChar = array of AnsiChar;
  DynarrayString = array of AnsiString;
  DynarrayWideString = array of WideString;
{$ENDIF}

  DynarrayInteger = array of Integer;
  DynarrayWord = array of Word;
  DynarrayCardinal = array of Cardinal;
  DynarrayBoolean = array of Boolean;
  DynarrayByte = array of Byte;
  DynarrayWideChar = array of WideChar;
  DynarrayShortString = array of ShortString;
  DynarrayUnicString = array of UnicString;
  DynarrayDouble = array of Double;
  DynarraySingle = array of Single;
  DynarrayExtended = array of Extended;
  DynarrayCurrency = array of Currency;
  DynarrayVariant = array of Variant;
  DynarrayTVarRec = array of TVarRec;

{$IFNDEF PAXARM}
  DynarrayChar2 = array of array of AnsiChar;
  DynarrayString2 = array of array of AnsiString;
  DynarrayWideString2 = array of array of WideString;
{$ENDIF}

  DynarrayInteger2 = array of array of Integer;
  DynarrayWord2 = array of array of Word;
  DynarrayCardinal2 = array of array of Cardinal;
  DynarrayBoolean2 = array of array of Boolean;
  DynarrayByte2 = array of array of Byte;
  DynarrayWideChar2 = array of array of WideChar;
  DynarrayShortString2 = array of array of ShortString;
  DynarrayUnicString2 = array of array of UnicString;
  DynarrayPointer2 = array of array of Pointer;
  DynarrayDouble2 = array of array of Double;
  DynarraySingle2 = array of array of Single;
  DynarrayExtended2 = array of array of Extended;
  DynarrayCurrency2 = array of array of Currency;
  DynarrayVariant2 = array of array of Variant;
  DynarrayTVarRec2 = array of array of TVarRec;

{$IFNDEF PAXARM}
  DynarrayChar3 = array of array of array of  AnsiChar;
  DynarrayString3 = array of array of array of  AnsiString;
  DynarrayWideString3 = array of array of array of  WideString;
{$ENDIF}

  DynarrayInteger3 = array of array of array of  Integer;
  DynarrayWord3 = array of array of array of  Word;
  DynarrayCardinal3 = array of array of array of  Cardinal;
  DynarrayBoolean3 = array of array of array of  Boolean;
  DynarrayByte3 = array of array of array of  Byte;
  DynarrayWideChar3 = array of array of array of  WideChar;
  DynarrayShortString3 = array of array of array of  ShortString;
  DynarrayUnicString3 = array of array of array of  UnicString;
  DynarrayPointer3 = array of array of array of  Pointer;
  DynarrayDouble3 = array of array of array of  Double;
  DynarraySingle3 = array of array of array of  Single;
  DynarrayExtended3 = array of array of array of  Extended;
  DynarrayCurrency3 = array of array of array of  Currency;
  DynarrayVariant3 = array of array of array of  Variant;
  DynarrayTVarRec3 = array of array of array of  TVarRec;

  PObject = ^TObject;

  TJS_Record = record
    H_JS_Object: Integer;
    H_JS_Boolean: Integer;
    H_JS_String: Integer;
    H_JS_Number: Integer;
    H_JS_Date: Integer;
    H_JS_Function: Integer;
    H_JS_Array: Integer;
    H_JS_RegExp: Integer;
    H_JS_Math: Integer;
    H_JS_Error: Integer;

    Id_JS_Object: Integer;
    Id_JS_Boolean: Integer;
    Id_JS_String: Integer;
    Id_JS_Number: Integer;
    Id_JS_Date: Integer;
    Id_JS_Function: Integer;
    Id_JS_Array: Integer;
    Id_JS_RegExp: Integer;
    Id_JS_Math: Integer;
    Id_JS_Error: Integer;
  end;
var
  H_ExceptionPtr: Integer = H_ExceptionPtr_32;
  H_ByteCodePtr: Integer = H_ByteCodePtr_32;
  H_Flag: Integer = H_Flag_32;
  H_SkipPop: Integer = H_SkipPop_32;
{$IFDEF PAXARM}
  FirstShiftValue: Integer = 102;
{$ELSE}
  FirstShiftValue: Integer = FirstShiftValue_32;
{$ENDIF}

const
  H_InitOnly = 4;
  H_BodyOnly = 8;

  kindNONE = 0;
  kindVAR = 1;
  kindCONST = 2;
  kindSUB = 3;
  kindPARAM = 4;
  kindTYPE = 5;
  kindTYPE_FIELD = 6;
  kindLABEL = 7;
  kindNAMESPACE = 8;
  kindCONSTRUCTOR = 9;
  kindDESTRUCTOR = 10;
  kindPROP = 11;
  kindEND_CLASS_HEADER = 12;

  KindSubs = [KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR];

  UnsignedIntegerTypes = [typeBYTE, typeWORD, typeCARDINAL];

  IntegerTypes = [typeBYTE, typeWORD, typeINTEGER, typeINT64, typeUINT64,
                  typeCARDINAL, typeSMALLINT, typeSHORTINT];
  Int64Types = [typeINT64, typeUINT64];
  VariantTypes = [typeVARIANT, typeOLEVARIANT];
{$IFDEF PAXARM}
  CharTypes = [typeWIDECHAR];
  StringTypes = [typeUNICSTRING];
  DynamicTypes = [typeUNICSTRING] +
                  VariantTypes +
{$IFDEF ARC}
                 [typeCLASS] +
{$ENDIF}
                 [typeDYNARRAY, typeINTERFACE];
{$ELSE}
  CharTypes = [typeANSICHAR, typeWIDECHAR];
  StringTypes = [typeANSISTRING, typeSHORTSTRING, typeWIDESTRING, typeUNICSTRING];
  DynamicTypes = [typeANSISTRING, typeWIDESTRING, typeUNICSTRING] + VariantTypes + [typeDYNARRAY, typeINTERFACE];
{$ENDIF}
  BooleanTypes = [typeBOOLEAN, typeWORDBOOL, typeLONGBOOL, typeBYTEBOOL];
  OrdinalTypes = IntegerTypes + CharTypes + BooleanTypes + [typeENUM];
  RealTypes = [typeSINGLE, typeDOUBLE, typeEXTENDED];
  NumberTypes = IntegerTypes + RealTypes + [typeCURRENCY];
  StandardTypes = OrdinalTypes + RealTypes + StringTypes + VariantTypes;

var
  OP_NOP,
  OP_SEPARATOR,
  OP_ADD_COMMENT,
  OP_STMT,
  OP_SET_CODE_LINE,

  OP_BEGIN_TEXT,
  OP_END_TEXT,

  OP_BEGIN_LOOP,
  OP_EPILOGUE_LOOP,
  OP_END_LOOP,

  OP_ADD_MESSAGE,

  OP_OPTION_EXPLICIT,
  OP_INIT_FWARRAY,

  OP_CHECK_FINAL,

  OP_BEGIN_NAMESPACE,
  OP_END_NAMESPACE,

  OP_BEGIN_TYPE,
  OP_END_TYPE,

  OP_BEGIN_CLASS_TYPE,
  OP_END_CLASS_TYPE,

  OP_BEGIN_CLASSREF_TYPE,
  OP_END_CLASSREF_TYPE,

  OP_BEGIN_HELPER_TYPE,
  OP_END_HELPER_TYPE,

  OP_BEGIN_INTERFACE_TYPE,
  OP_END_INTERFACE_TYPE,

  OP_BEGIN_RECORD_TYPE,
  OP_END_RECORD_TYPE,

  OP_BEGIN_ARRAY_TYPE,
  OP_END_ARRAY_TYPE,

  OP_BEGIN_DYNARRAY_TYPE,
  OP_END_DYNARRAY_TYPE,

  OP_BEGIN_SUBRANGE_TYPE,
  OP_END_SUBRANGE_TYPE,

  OP_BEGIN_ENUM_TYPE,
  OP_END_ENUM_TYPE,

  OP_BEGIN_SET_TYPE,
  OP_END_SET_TYPE,

  OP_BEGIN_POINTER_TYPE,
  OP_END_POINTER_TYPE,

  OP_BEGIN_PROC_TYPE,
  OP_END_PROC_TYPE,

{$IFNDEF PAXARM}
  OP_BEGIN_SHORTSTRING_TYPE,
  OP_END_SHORTSTRING_TYPE,
{$ENDIF}

  OP_BEGIN_ALIAS_TYPE,
  OP_END_ALIAS_TYPE,

  OP_BEGIN_CONST,
  OP_END_CONST,

  OP_BEGIN_VAR,
  OP_END_VAR,

  OP_GET_NEXTJSPROP, // js only
  OP_CLEAR_REFERENCES, // js only

  OP_BEGIN_LIBRARY,
  OP_BEGIN_EXPORT,
  OP_BEGIN_MODULE,
  OP_END_MODULE,
  OP_BEGIN_INCLUDED_FILE,
  OP_END_INCLUDED_FILE,
  OP_END_INTERFACE_SECTION,
  OP_END_IMPORT,
  OP_BEGIN_INITIALIZATION,
  OP_END_INITIALIZATION,
  OP_BEGIN_FINALIZATION,
  OP_END_FINALIZATION,

  OP_EXTRA_BYTECODE,

  OP_WARNINGS_ON,
  OP_WARNINGS_OFF,

  OP_FRAMEWORK_ON,
  OP_FRAMEWORK_OFF,

  OP_TRY_ON,
  OP_TRY_OFF,
  OP_FINALLY,
  OP_EXCEPT,
  OP_EXCEPT_SEH,
  OP_EXCEPT_ON,
  OP_RAISE,
  OP_COND_RAISE,
  OP_BEGIN_EXCEPT_BLOCK,
  OP_END_EXCEPT_BLOCK,

  OP_OVERFLOW_CHECK,

  OP_PAUSE,
  OP_CHECK_PAUSE,
  OP_CHECK_PAUSE_LIGHT,
  OP_HALT,

  OP_EMIT_OFF,
  OP_EMIT_ON,

  OP_BEGIN_USING,
  OP_END_USING,

  OP_BEGIN_BLOCK,
  OP_END_BLOCK,

  OP_EVAL,
  OP_EVAL_OUTER,

  OP_EVAL_INHERITED,
  OP_EVAL_CONSTRUCTOR,
  OP_UPDATE_INSTANCE,
  OP_ADJUST_INSTANCE,
  OP_CLEAR_EDX,
  OP_IMPLEMENTS,
  OP_MYCLASS,
  OP_MYBASE,

  OP_LOAD_PROC,

  OP_CHECK_OVERRIDE,

  OP_EXIT,
  OP_GO,
  OP_GO_1,
  OP_GO_2,
  OP_GO_3,
  OP_GO_TRUE,
  OP_GO_FALSE,
  OP_GO_TRUE_BOOL,
  OP_GO_FALSE_BOOL,
  OP_GO_DL,
  OP_CALL_INHERITED,
  OP_CALL,
  OP_BEGIN_CALL,
  OP_CALL_DEFAULT_CONSTRUCTOR,
  OP_CHECK_SUB_CALL,
  OP_BEGIN_VCALL,
  OP_VCALL,
  OP_PUSH,
  OP_PUSH_INSTANCE,
  OP_PUSH_CLASSREF,
  OP_PUSH_CONTEXT,
  OP_POP_CONTEXT,
  OP_FIND_CONTEXT,
  OP_FIND_JS_FUNC,
  OP_LABEL,

  OP_SAVE_EDX,
  OP_RESTORE_EDX,

  OP_TYPE_CAST,

  OP_DECLARE_MEMBER,

  OP_DECL_SUB,
  OP_BEGIN_SUB,
  OP_DECLARE_LOCAL_VAR,
  OP_DECLARE_TEMP_VAR,
  OP_DESTROY_LOCAL_VAR,
  OP_INIT_SUB,
  OP_JUMP_SUB,
  OP_END_SUB,
  OP_FIN_SUB,
  OP_EPILOGUE_SUB,

  OP_BEGIN_GLOBAL_BLOCK,
  OP_EPILOGUE_GLOBAL_BLOCK,
  OP_EPILOGUE_GLOBAL_BLOCK2,
  OP_END_GLOBAL_BLOCK,

  OP_ABSOLUTE,

  OP_ASSIGN_TYPE,
  OP_DETERMINE_TYPE,
  OP_ASSIGN_THE_SAME_TYPE,
  OP_ASSIGN_TYPE_ALIAS,
  OP_ASSIGN_LAMBDA_TYPES,

  OP_BEGIN_WITH,
  OP_END_WITH,

  OP_BEGIN_INIT_CONST,
  OP_END_INIT_CONST,

  OP_CREATE_POINTER_TYPE,
  OP_CREATE_CLASSREF_TYPE,
  OP_ADDRESS,
  OP_TERMINAL,
  OP_ADDRESS_PROG,
  OP_ASSIGN_PROG,

  OP_CREATE_DYNAMIC_ARRAY_TYPE,

  OP_CREATE_SHORTSTRING_TYPE,

  OP_SET_INCLUDE,
  OP_SET_INCLUDE_INTERVAL,
  OP_SET_EXCLUDE,

  OP_LVALUE,
  OP_POSTFIX_EXPRESSION,

  OP_ASSIGN,
  OP_ASSIGN_CONST,
  OP_ASSIGN_ENUM,
  OP_CHECK_SUBRANGE_TYPE,

  OP_INC,
  OP_DEC,
  OP_PRED,
  OP_SUCC,
  OP_ORD,
  OP_CHR,
  OP_STR,
  OP_LOW,
  OP_HIGH,

  OP_SET_LENGTH,

  OP_SET_LENGTH_EX,
  OP_PUSH_LENGTH,

  OP_DYNARRAY_ASSIGN,
  OP_DYNARRAY_CLR,
  OP_DYNARRAY_HIGH,
  OP_CREATE_EMPTY_DYNARRAY,

  OP_SHORTSTRING_HIGH,

  OP_EXPORTS,

  OP_PLUS,
  OP_MINUS,
  OP_MULT,
  OP_DIV,
  OP_IDIV,
  OP_MOD,
  OP_SHL,
  OP_SHR,

  OP_NEG,
  OP_POSITIVE,
  OP_ABS,

  OP_EQ,
  OP_NE,
  OP_LT,
  OP_LE,
  OP_GT,
  OP_GE,

  OP_CLASSNAME,

  OP_GET_PROG,

  OP_IS,
  OP_AS,
  OP_TYPEINFO,
  OP_ADD_TYPEINFO,
  OP_INSTANCE_OF,

  OP_AND,
  OP_OR,
  OP_XOR,
  OP_NOT,

  OP_RET,

  OP_FIELD,
  OP_ELEM,

  OP_ITEM,
  OP_RECORD_ITEM,

  OP_PRINT,
  OP_PRINT_EX,

  OP_PRINT_KWD,
  OP_PRINTLN_KWD,

{$IFNDEF PAXARM}
  OP_INIT_PANSICHAR_LITERAL,
{$ENDIF}
  OP_INIT_PWIDECHAR_LITERAL,
  OP_SIZEOF,

  OP_SET_READ_ID,
  OP_SET_WRITE_ID,

  OP_OLE_GET,
  OP_OLE_SET,
  OP_OLE_VALUE,
  OP_OLE_PARAM,

  OP_PARAM_CHANGED,

  OP_ONCREATE_OBJECT,
  OP_ON_AFTER_OBJECT_CREATION,
  OP_CREATE_OBJECT,
  OP_DESTROY_OBJECT,
  OP_GET_VMT_ADDRESS,
  OP_ADD_ANCESTOR,
  OP_ADD_INTERFACE,
  OP_ADD_METHOD_INDEX,
  OP_ASSIGNED,

  OP_ONCREATE_HOST_OBJECT,
  OP_ONDESTROY_HOST_OBJECT,

  OP_BEFORE_CALL_HOST,
  OP_AFTER_CALL_HOST,

  OP_SAVE_REGS,
  OP_RESTORE_REGS,

  OP_ERR_ABSTRACT,
  OP_UPDATE_DEFAULT_CONSTRUCTOR,
  OP_FIND_CONSTRUCTOR,

  OP_BEGIN_CRT_JS_FUNC_OBJECT,
  OP_END_CRT_JS_FUNC_OBJECT,

  OP_TO_JS_OBJECT,
  OP_JS_TYPEOF,
  OP_JS_VOID,
  OP_JS_DELETE,

  OP_TO_FW_OBJECT,

  // for-in statement
  OP_GET_ENUMERATOR,
  OP_MOVE_NEXT,
  OP_CURRENT,
  OP_LOCK_VARRAY,
  OP_UNLOCK_VARRAY,

  OP_DUMMY
  : Integer;

// detailed operators
var
  OP_VAR_FROM_TVALUE,

  OP_CURRENCY_FROM_INT64,
  OP_CURRENCY_FROM_UINT64,
  OP_CURRENCY_FROM_INT,
  OP_CURRENCY_FROM_REAL,

  OP_INT_TO_DOUBLE,
  OP_INT64_TO_DOUBLE,
  OP_UINT64_TO_DOUBLE,

  OP_INT_TO_SINGLE,
  OP_INT64_TO_SINGLE,
  OP_UINT64_TO_SINGLE,

  OP_INT_TO_EXTENDED,
  OP_INT64_TO_EXTENDED,
  OP_UINT64_TO_EXTENDED,

  OP_INT_TO_INT64,
  OP_BYTE_TO_INT64,
  OP_WORD_TO_INT64,
  OP_CARDINAL_TO_INT64,
  OP_SMALLINT_TO_INT64,
  OP_SHORTINT_TO_INT64,

  OP_INT_FROM_INT64,
  OP_BYTE_FROM_INT64,
  OP_WORD_FROM_INT64,
  OP_CARDINAL_FROM_INT64,
  OP_SMALLINT_FROM_INT64,
  OP_SHORTINT_FROM_INT64,

  OP_INT_TO_UINT64,
  OP_BYTE_TO_UINT64,
  OP_WORD_TO_UINT64,
  OP_CARDINAL_TO_UINT64,
  OP_SMALLINT_TO_UINT64,
  OP_SHORTINT_TO_UINT64,

  OP_INT_FROM_UINT64,
  OP_BYTE_FROM_UINT64,
  OP_WORD_FROM_UINT64,
  OP_CARDINAL_FROM_UINT64,
  OP_SMALLINT_FROM_UINT64,
  OP_SHORTINT_FROM_UINT64,

  OP_MULT_INT64,
  OP_IDIV_INT64,
  OP_MOD_INT64,
  OP_SHL_INT64,
  OP_SHR_INT64,

  OP_CURRENCY_TO_EXTENDED,
  OP_CURRENCY_TO_SINGLE,
  OP_CURRENCY_TO_DOUBLE,

  OP_DOUBLE_TO_SINGLE,
  OP_DOUBLE_TO_EXTENDED,

  OP_SINGLE_TO_DOUBLE,
  OP_SINGLE_TO_EXTENDED,

  OP_EXTENDED_TO_DOUBLE,
  OP_EXTENDED_TO_SINGLE,

  OP_ASSIGN_BYTE_I,
  OP_ASSIGN_BYTE_M,
  OP_ASSIGN_WORD_I,
  OP_ASSIGN_WORD_M,
  OP_ASSIGN_CARDINAL_I,
  OP_ASSIGN_CARDINAL_M,
  OP_ASSIGN_SMALLINT_I,
  OP_ASSIGN_SMALLINT_M,
  OP_ASSIGN_SHORTINT_I,
  OP_ASSIGN_SHORTINT_M,
  OP_ASSIGN_INT_I,
  OP_ASSIGN_INT_M,
  OP_ASSIGN_DOUBLE,
  OP_ASSIGN_CURRENCY,
  OP_ASSIGN_EVENT,
  OP_ASSIGN_SINGLE,
  OP_ASSIGN_EXTENDED,
{$IFNDEF PAXARM}
  OP_ASSIGN_PANSICHAR,
{$ENDIF}
  OP_ASSIGN_PWIDECHAR,
  OP_ASSIGN_INT64,
  OP_ASSIGN_UINT64,
  OP_ASSIGN_INTERFACE,

  OP_CREATE_EVENT,

{$IFNDEF PAXARM}
  OP_ASSIGN_ANSISTRING,
  OP_ASSIGN_SHORTSTRING,
  OP_ASSIGN_WIDESTRING,
{$ENDIF}
  OP_ASSIGN_UNICSTRING,
  OP_ASSIGN_VARIANT,
  OP_ASSIGN_OLEVARIANT,

  OP_ASSIGN_CLASS,

  OP_ASSIGN_SHIFT,

  OP_ASSIGN_TVarRec,

  OP_ASSIGN_RECORD,
  OP_ASSIGN_ARRAY,

{$IFNDEF PAXARM}
  OP_ANSISTRING_FROM_PANSICHAR,
  OP_ANSISTRING_FROM_PWIDECHAR,
  OP_ANSISTRING_FROM_ANSICHAR,
  OP_ADD_ANSISTRING,
  OP_ADD_SHORTSTRING,
  OP_ADD_WIDESTRING,
{$ENDIF}
  OP_ADD_UNICSTRING,

  OP_ADD_STRING,

{$IFNDEF PAXARM}
  OP_ANSISTRING_CLR,
  OP_WIDESTRING_CLR,
{$ENDIF}
  OP_UNICSTRING_CLR,
  OP_INTERFACE_CLR,
  OP_CLASS_CLR, // js only

  OP_EQ_STRUCT,
  OP_NE_STRUCT,

{$IFNDEF PAXARM}
  OP_EQ_ANSISTRING,
  OP_NE_ANSISTRING,

  OP_GT_ANSISTRING,
  OP_GE_ANSISTRING,
  OP_LT_ANSISTRING,
  OP_LE_ANSISTRING,

  OP_GT_SHORTSTRING,
  OP_GE_SHORTSTRING,
  OP_LT_SHORTSTRING,
  OP_LE_SHORTSTRING,

  OP_GT_WIDESTRING,
  OP_GE_WIDESTRING,
  OP_LT_WIDESTRING,
  OP_LE_WIDESTRING,
{$ENDIF}
  OP_GT_UNICSTRING,
  OP_GE_UNICSTRING,
  OP_LT_UNICSTRING,
  OP_LE_UNICSTRING,

{$IFNDEF PAXARM}
  OP_EQ_SHORTSTRING,
  OP_EQ_WIDESTRING,
{$ENDIF}
  OP_EQ_UNICSTRING,
{$IFNDEF PAXARM}
  OP_NE_SHORTSTRING,
  OP_NE_WIDESTRING,
{$ENDIF}
  OP_NE_UNICSTRING,

  OP_STRUCTURE_CLR,
  OP_STRUCTURE_ADDREF,
  OP_ADDREF,

{$IFNDEF PAXARM}
  OP_SHORTSTRING_FROM_PANSICHAR_LITERAL,
  OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL,
  OP_SHORTSTRING_FROM_ANSICHAR,
  OP_SHORTSTRING_FROM_WIDECHAR,
  OP_SHORTSTRING_FROM_ANSISTRING,
  OP_SHORTSTRING_FROM_WIDESTRING,
  OP_UNICSTRING_FROM_WIDESTRING,
  OP_SHORTSTRING_FROM_UNICSTRING,
  OP_ANSISTRING_FROM_SHORTSTRING,

  OP_WIDESTRING_FROM_PANSICHAR_LITERAL,
  OP_WIDESTRING_FROM_PWIDECHAR_LITERAL,
  OP_WIDESTRING_FROM_ANSICHAR,
  OP_WIDESTRING_FROM_WIDECHAR,
  OP_ANSISTRING_FROM_WIDECHAR,
  OP_WIDESTRING_FROM_WIDECHAR_LITERAL,
  OP_WIDESTRING_FROM_ANSISTRING,
  OP_UNICSTRING_FROM_ANSISTRING,
  OP_WIDESTRING_FROM_SHORTSTRING,
  OP_WIDESTRING_FROM_UNICSTRING,
  OP_UNICSTRING_FROM_SHORTSTRING,
  OP_ANSISTRING_FROM_WIDESTRING,
  OP_ANSISTRING_FROM_UNICSTRING,

  OP_UNICSTRING_FROM_PANSICHAR_LITERAL,
  OP_UNICSTRING_FROM_ANSICHAR,
{$ENDIF}
  OP_UNICSTRING_FROM_PWIDECHAR_LITERAL,
  OP_UNICSTRING_FROM_WIDECHAR,
  OP_UNICSTRING_FROM_WIDECHAR_LITERAL,

  OP_VARIANT_FROM_CLASS, // JS only
  OP_VARIANT_FROM_POINTER, // JS only
  OP_CLASS_FROM_VARIANT, // JS only

  OP_INTERFACE_FROM_CLASS,
  OP_INTERFACE_CAST,

{$IFNDEF PAXARM}
  OP_VARIANT_FROM_PANSICHAR_LITERAL,
  OP_VARIANT_FROM_ANSISTRING,
  OP_VARIANT_FROM_WIDESTRING,
  OP_VARIANT_FROM_SHORTSTRING,
  OP_VARIANT_FROM_ANSICHAR,
{$ENDIF}
  OP_VARIANT_FROM_PWIDECHAR_LITERAL,
  OP_VARIANT_FROM_UNICSTRING,
  OP_VARIANT_FROM_WIDECHAR,
  OP_VARIANT_FROM_WIDECHAR_LITERAL,
  OP_VARIANT_FROM_INT,
  OP_VARIANT_FROM_INT64,
  OP_VARIANT_FROM_BYTE,
  OP_VARIANT_FROM_BOOL,
  OP_VARIANT_FROM_WORD,
  OP_VARIANT_FROM_CARDINAL,
  OP_VARIANT_FROM_SMALLINT,
  OP_VARIANT_FROM_SHORTINT,
  OP_VARIANT_FROM_DOUBLE,
  OP_VARIANT_FROM_CURRENCY,
  OP_VARIANT_FROM_SINGLE,
  OP_VARIANT_FROM_EXTENDED,
  OP_VARIANT_FROM_INTERFACE,

  OP_OLEVARIANT_FROM_VARIANT,
{$IFNDEF PAXARM}
  OP_OLEVARIANT_FROM_PANSICHAR_LITERAL,
  OP_OLEVARIANT_FROM_ANSISTRING,
  OP_OLEVARIANT_FROM_WIDESTRING,
  OP_OLEVARIANT_FROM_SHORTSTRING,
  OP_OLEVARIANT_FROM_ANSICHAR,
{$ENDIF}
  OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL,
  OP_OLEVARIANT_FROM_UNICSTRING,
  OP_OLEVARIANT_FROM_WIDECHAR,
  OP_OLEVARIANT_FROM_WIDECHAR_LITERAL,
  OP_OLEVARIANT_FROM_INT,
  OP_OLEVARIANT_FROM_INT64,
  OP_OLEVARIANT_FROM_BYTE,
  OP_OLEVARIANT_FROM_BOOL,
  OP_OLEVARIANT_FROM_WORD,
  OP_OLEVARIANT_FROM_CARDINAL,
  OP_OLEVARIANT_FROM_SMALLINT,
  OP_OLEVARIANT_FROM_SHORTINT,
  OP_OLEVARIANT_FROM_DOUBLE,
  OP_OLEVARIANT_FROM_CURRENCY,
  OP_OLEVARIANT_FROM_SINGLE,
  OP_OLEVARIANT_FROM_EXTENDED,
  OP_OLEVARIANT_FROM_INTERFACE,
{$IFNDEF PAXARM}
  OP_ANSISTRING_FROM_INT, // JS only
  OP_ANSISTRING_FROM_DOUBLE, // JS only
  OP_ANSISTRING_FROM_SINGLE, // JS only
  OP_ANSISTRING_FROM_EXTENDED, // JS only
  OP_ANSISTRING_FROM_BOOLEAN, // JS only
{$ENDIF}
  OP_UNICSTRING_FROM_INT, // JS only
  OP_UNICSTRING_FROM_DOUBLE, // JS only
  OP_UNICSTRING_FROM_SINGLE, // JS only
  OP_UNICSTRING_FROM_EXTENDED, // JS only
  OP_UNICSTRING_FROM_BOOLEAN, // JS only

  OP_JS_FUNC_OBJ_FROM_VARIANT, // JS only

{$IFNDEF PAXARM}
  OP_ANSICHAR_FROM_VARIANT,
  OP_ANSISTRING_FROM_VARIANT,
  OP_WIDESTRING_FROM_VARIANT,
  OP_SHORTSTRING_FROM_VARIANT,
{$ENDIF}
  OP_WIDECHAR_FROM_VARIANT,
  OP_UNICSTRING_FROM_VARIANT,
  OP_DOUBLE_FROM_VARIANT,
  OP_CURRENCY_FROM_VARIANT,
  OP_SINGLE_FROM_VARIANT,
  OP_EXTENDED_FROM_VARIANT,
  OP_INT64_FROM_VARIANT,
  OP_UINT64_FROM_VARIANT,
  OP_INT_FROM_VARIANT,
  OP_BYTE_FROM_VARIANT,
  OP_WORD_FROM_VARIANT,
  OP_CARDINAL_FROM_VARIANT,
  OP_BOOL_FROM_VARIANT,
  OP_BYTEBOOL_FROM_VARIANT,
  OP_WORDBOOL_FROM_VARIANT,
  OP_LONGBOOL_FROM_VARIANT,
  OP_SMALLINT_FROM_VARIANT,
  OP_SHORTINT_FROM_VARIANT,
  OP_BOOL_FROM_BYTEBOOL,
  OP_BOOL_FROM_WORDBOOL,
  OP_BOOL_FROM_LONGBOOL,

  OP_NOT_BOOL,
  OP_NOT_BYTEBOOL,
  OP_NOT_WORDBOOL,
  OP_NOT_LONGBOOL,

  OP_NOT_VARIANT,
  OP_NEG_VARIANT,
  OP_ADD_VARIANT,
  OP_SUB_VARIANT,
  OP_MULT_VARIANT,
  OP_DIV_VARIANT,
  OP_IDIV_VARIANT,
  OP_MOD_VARIANT,
  OP_SHL_VARIANT,
  OP_SHR_VARIANT,
  OP_AND_VARIANT,
  OP_OR_VARIANT,
  OP_XOR_VARIANT,
  OP_LT_VARIANT,
  OP_LE_VARIANT,
  OP_GT_VARIANT,
  OP_GE_VARIANT,
  OP_EQ_VARIANT,
  OP_NE_VARIANT,

  OP_EQ_EVENT,
  OP_NE_EVENT,

  OP_VARIANT_CLR,
  OP_VARARRAY_GET,
  OP_VARARRAY_PUT,
  OP_VARARRAY_IDX,

  OP_ADD_INT_MI,
  OP_ADD_INT_MM,

  OP_SUB_INT_MI,
  OP_SUB_INT_MM,

  OP_IMUL_INT_MI,
  OP_IMUL_INT_MM,

  OP_IDIV_INT_MI,
  OP_IDIV_INT_MM,
  OP_IDIV_INT_IM,

  OP_MOD_INT_MI,
  OP_MOD_INT_MM,
  OP_MOD_INT_IM,

  OP_SHL_INT_MI,
  OP_SHL_INT_MM,
  OP_SHL_INT_IM,

  OP_SHR_INT_MI,
  OP_SHR_INT_MM,
  OP_SHR_INT_IM,

  OP_AND_INT_MI,
  OP_AND_INT_MM,

  OP_OR_INT_MI,
  OP_OR_INT_MM,

  OP_XOR_INT_MI,
  OP_XOR_INT_MM,

  OP_NEG_INT,
  OP_NEG_INT64,
  OP_NEG_UINT64,

  OP_ABS_INT,
  OP_ABS_INT64,
  OP_ABS_DOUBLE,
  OP_ABS_SINGLE,
  OP_ABS_EXTENDED,
  OP_ABS_CURRENCY,
  OP_ABS_VARIANT,

  OP_LT_INT_MI,
  OP_LT_INT_MM,

  OP_LE_INT_MI,
  OP_LE_INT_MM,

  OP_GT_INT_MI,
  OP_GT_INT_MM,

  OP_GE_INT_MI,
  OP_GE_INT_MM,

  OP_EQ_INT_MI,
  OP_EQ_INT_MM,

  OP_NE_INT_MI,
  OP_NE_INT_MM,

  OP_ADD_INT64,
  OP_SUB_INT64,
  OP_AND_INT64,
  OP_OR_INT64,
  OP_XOR_INT64,

  OP_ADD_UINT64,
  OP_SUB_UINT64,
  OP_AND_UINT64,
  OP_OR_UINT64,
  OP_XOR_UINT64,

  OP_LT_INT64,
  OP_LE_INT64,
  OP_GT_INT64,
  OP_GE_INT64,
  OP_EQ_INT64,
  OP_NE_INT64,

  OP_LT_UINT64,
  OP_LE_UINT64,
  OP_GT_UINT64,
  OP_GE_UINT64,

  OP_ADD_CURRENCY,
  OP_SUB_CURRENCY,
  OP_MUL_CURRENCY,
  OP_DIV_CURRENCY,

  OP_LT_CURRENCY,
  OP_LE_CURRENCY,
  OP_GT_CURRENCY,
  OP_GE_CURRENCY,
  OP_EQ_CURRENCY,
  OP_NE_CURRENCY,

  OP_ADD_DOUBLE,
  OP_SUB_DOUBLE,
  OP_MUL_DOUBLE,
  OP_DIV_DOUBLE,

  OP_NEG_DOUBLE,
  OP_NEG_CURRENCY,

  OP_LT_DOUBLE,
  OP_LE_DOUBLE,
  OP_GT_DOUBLE,
  OP_GE_DOUBLE,
  OP_EQ_DOUBLE,
  OP_NE_DOUBLE,

  OP_ADD_SINGLE,
  OP_SUB_SINGLE,
  OP_MUL_SINGLE,
  OP_DIV_SINGLE,

  OP_NEG_SINGLE,

  OP_LT_SINGLE,
  OP_LE_SINGLE,
  OP_GT_SINGLE,
  OP_GE_SINGLE,
  OP_EQ_SINGLE,
  OP_NE_SINGLE,

  OP_ADD_EXTENDED,
  OP_SUB_EXTENDED,
  OP_MUL_EXTENDED,
  OP_DIV_EXTENDED,

  OP_NEG_EXTENDED,

  OP_LT_EXTENDED,
  OP_LE_EXTENDED,
  OP_GT_EXTENDED,
  OP_GE_EXTENDED,
  OP_EQ_EXTENDED,
  OP_NE_EXTENDED,

  OP_PUSH_EBP,
  OP_POP,

  OP_PUSH_PROG,
  OP_PUSH_ADDRESS,
  OP_PUSH_STRUCTURE,
  OP_PUSH_SET,

  OP_PUSH_BYTE_IMM,
  OP_PUSH_BYTE,
  OP_PUSH_WORD_IMM,
  OP_PUSH_WORD,
  OP_PUSH_CARDINAL_IMM,
  OP_PUSH_CARDINAL,
  OP_PUSH_SMALLINT_IMM,
  OP_PUSH_SMALLINT,
  OP_PUSH_SHORTINT_IMM,
  OP_PUSH_SHORTINT,
  OP_PUSH_INT_IMM,
  OP_PUSH_INT,
  OP_PUSH_PTR,
  OP_PUSH_DOUBLE,
  OP_PUSH_CURRENCY,
  OP_PUSH_SINGLE,
  OP_PUSH_EXTENDED,
{$IFNDEF PAXARM}
  OP_PUSH_ANSISTRING,
  OP_PUSH_SHORTSTRING,
  OP_PUSH_WIDESTRING,
  OP_PUSH_PANSICHAR_IMM,
{$ENDIF}
  OP_PUSH_UNICSTRING,
  OP_PUSH_PWIDECHAR_IMM,
  OP_PUSH_INST,
  OP_PUSH_CLSREF,
  OP_PUSH_DYNARRAY,
  OP_PUSH_OPENARRAY,
  OP_PUSH_INT64,
  OP_PUSH_DATA,
  OP_PUSH_EVENT,

  OP_SET_ASSIGN,
  OP_SET_COUNTER_ASSIGN,
  OP_SET_UNION,
  OP_SET_DIFFERENCE,
  OP_SET_INTERSECTION,
  OP_SET_SUBSET,
  OP_SET_SUPERSET,
  OP_SET_EQUALITY,
  OP_SET_INEQUALITY,
  OP_SET_MEMBERSHIP,

  OP_GET_COMPONENT,

  OP_DETERMINE_PROP,

  OP_GET_DRTTI_PROP,
  OP_SET_DRTTI_PROP,
{$IFNDEF PAXARM}
  OP_GET_ANSISTR_PROP,
  OP_SET_ANSISTR_PROP,

  OP_GET_WIDESTR_PROP,
  OP_SET_WIDESTR_PROP,
{$ENDIF}
  OP_GET_UNICSTR_PROP,
  OP_SET_UNICSTR_PROP,

  OP_GET_ORD_PROP,
  OP_SET_ORD_PROP,

  OP_GET_INTERFACE_PROP,
  OP_SET_INTERFACE_PROP,

  OP_GET_SET_PROP,
  OP_SET_SET_PROP,

  OP_GET_FLOAT_PROP,
  OP_SET_FLOAT_PROP,

  OP_GET_VARIANT_PROP,
  OP_SET_VARIANT_PROP,

  OP_GET_INT64_PROP,
  OP_SET_INT64_PROP,

  OP_GET_EVENT_PROP,
  OP_SET_EVENT_PROP,
  OP_SET_EVENT_PROP2,
  OP_CREATE_METHOD

  : Integer;

  ASM_NOP,
  ASM_WAIT,
  ASM_CLC,
  ASM_PUSHFD,
  ASM_POPFD,
  ASM_XCHG,

  ASM_MOV,
  ASM_LEA,
  ASM_TEST,

  ASM_ADD,
  ASM_ADC,
  ASM_SBB,
  ASM_NEG,
  ASM_NOT,
  ASM_SUB,

  ASM_MUL,
  ASM_IMUL,
  ASM_DIV,
  ASM_IDIV,
  ASM_XOR,
  ASM_AND,
  ASM_OR,
  ASM_SHL,
  ASM_SHR,

  ASM_CDQ,

  ASM_CALL,
  ASM_RET,
  ASM_PUSH,
  ASM_POP,
  ASM_JMP,

  ASM_INC,
  ASM_DEC,

  ASM_JNO,
  ASM_JNC,
  ASM_JZ,
  ASM_JNZ,
  ASM_JBE,
  ASM_JNLE,

  ASM_FLD,
  ASM_FILD,
  ASM_FISTP,
  ASM_FSTP,
  ASM_FADD,
  ASM_FSUB,
  ASM_FMUL,
  ASM_FDIV,
  ASM_FCOMP,
  ASM_FCOMPP,
  ASM_FSTSV,
  ASM_SAHF,
  ASM_FCHS,
  ASM_FABS,

  ASM_SETL,
  ASM_SETLE,
  ASM_SETNLE,
  ASM_SETNL,

  ASM_SETB,
  ASM_SETBE,
  ASM_SETNBE,
  ASM_SETNB,
  ASM_SETZ,
  ASM_SETNZ,

  ASM_CMP,

  ASM_REP_MOVSB,
  ASM_REP_MOVSD,

  ASM_MOVSD,
  ASM_MOVSS,
  ASM_CVTSD2SS,
  ASM_CVTSS2SD

  : Integer;

type
  PExcFrame = ^TExcFrame;
  TExcFrame = record
    next: PExcFrame;
    desc: Pointer;
    hEBP: Integer;
    case Integer of
    0:  ( );
    1:  ( ConstructedObject: Pointer );
    2:  ( SelfOfMethod: Pointer );
  end;

const
  HOST_EXC_FRAME_SIZE = SizeOf(TExcFrame);

type
  TParserNotifyEvent = procedure(Sender: TObject) of object;
  TParserIdentEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer) of object;
  TParserIdentEventEx = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const Declaration: String) of object;
  TParserNamedValueEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const Value: Variant;
    const Declaration: String) of object;
  TParserTypedIdentEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const TypeName: String;
    const Declaration: String) of object;
  TParserVariantRecordFieldEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const TypeName: String; VarCount: Int64;
    const Declaration: String) of object;
  TParserNamedTypedValueEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const TypeName: String;
    const DefaultValue: String;
    const Declaration: String) of object;
  TParserDeclarationEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer; const Declaration: String) of object;
  TParserArrayTypeEvent = procedure(Sender: TObject;
    const IdentName: String; Id: Integer;
    Ranges: TStringList;
    const ElemTypeName: String) of object;

  TPauseNotifyEvent = procedure (Sender: TObject;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  THaltNotifyEvent = procedure (Sender: TObject; ExitCode: Integer;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  TErrNotifyEvent = procedure (Sender: TObject; E: Exception;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  TLoadProcEvent = procedure (Sender: TObject;
       const ProcName, DllName: String; var Address: Pointer) of object;

  TObjectNotifyEvent = procedure (Sender: TObject;
       Instance: TObject) of object;
  TIdNotifyEvent = procedure (Sender: TObject;
       Id: Integer) of object;
  TClassNotifyEvent = procedure (Sender: TObject;
       C: TClass) of object;

  TMapTableNamespaceEvent = procedure (Sender: TObject;
                                       const FullName: String;
                                       Global: Boolean) of object;
  TMapTableVarAddressEvent = procedure (Sender: TObject;
       const FullName: String; Global: Boolean; var Address: Pointer) of object;
  TMapTableProcAddressEvent = procedure (Sender: TObject;
       const FullName: String; OverCount: Byte;
       Global: Boolean; var Address: Pointer) of object;
  TMapTableClassRefEvent = procedure (Sender: TObject;
       const FullName: String; Global: Boolean; var ClassRef: TClass) of object;

  TPrintEvent = procedure (Sender: TObject;
                           const Text: String) of object;
  TPrintExEvent = procedure (Sender: TObject;
                             Address: Pointer;
                             Kind: Integer;
                             FT: Integer;
                             L1, L2: Integer) of object;

  TPrintClassTypeFieldEvent = procedure (Sender: TObject;
                                const Infos: TPrintClassTypeFieldInfo)
                                of object;
  TPrintClassTypePropEvent = procedure (Sender: TObject;
                                const Infos: TPrintClassTypePropInfo)
                                of object;

  TCustomExceptionHelperEvent = procedure (Sender: TObject;
                                      RaisedException, DestException: Exception)
                                      of object;

  TStreamEvent = procedure (Sender: TObject; Stream: TStream) of object;
  TProcNotifyEvent = procedure (Sender: TObject;
       const FullName: String; OverCount: Byte) of object;


  TVirtualObjectMethodCallEvent = procedure(Sender: TObject; const ObjectName,
      PropName: String; const Params: array of Variant; var result: Variant) of object;
  TVirtualObjectPutPropertyEvent = procedure(Sender: TObject; const ObjectName,
      PropName: String; const Params: array of Variant; const value: Variant) of object;

  PPaxExcFrame = ^TPaxExcFrame;
  TPaxExcFrame = record
    next: PPaxExcFrame;   //0
    desc: Pointer;        //4
    hEBP: Integer;        //8
    SelfOfMethod: Pointer;//12
    Prog: Pointer;        //16
    TryBlockNumber: Integer; //20
    Magic: Integer;          //24
    hESP: Integer;           //28
  end; // size of = 32

type
  TIsJSType = function (T: Integer; P: Pointer): Boolean;

function _IsJSType(T: Integer; P: Pointer): Boolean;
var
  IsJSType: TIsJSType = {$IFDEF FPC}@{$ENDIF}_IsJSType;

type
  TTryKind = (tryExcept, tryFinally);

  TSavePCUEvent = procedure (Sender: TObject; const UnitName: String; var result: TStream)
    of object;
  TLoadPCUEvent = procedure (Sender: TObject; const UnitName: String; var result: TStream)
    of object;
  TSavePCUFinishedEvent = procedure(Sender: TObject; const UnitName: String; var Stream : TStream) of object; // jason
  TLoadPCUFinishedEvent = procedure (Sender: TObject; const UnitName: String; var Stream : TStream) of object; // jason

  TExceptionClass = class of Exception;

  PPointer = ^Pointer;

  TClassVisibility = (cvNone, cvPrivate, cvProtected, cvPublic, cvPublished,
                      cvStrictPrivate, cvStrictProtected);
  TMemberVisibilitySet = set of TClassVisibility;

  TBytes = array[0..1000000000] of Byte;
  PBytes = ^TBytes;

  TIntegers = array[0..100000] of Integer;
  PIntegers = ^ TIntegers;
  TPointers = array[0..100000] of Pointer;
  PPointers = ^ TPointers;

  PByteSet = ^TByteSet;
  TByteSet = set of Byte;

  TTokenClass = (tcNone, tcSeparator, tcKeyword, tcIdentifier,
                 tcBooleanConst, tcCharConst, tcPCharConst, tcIntegerConst,
                 tcDoubleConst, tcNumCharConst, tcVariantConst,
                 tcSpecial, tcHtmlStringConst);

  TParamData = record
    Flags: TParamFlags;
    ParamName, TypeName: ShortString;
  end;
  PParamData = ^TParamData;

function MPtr(X: Integer): Integer;
function StrEql(const S1, S2: String): Boolean;
function ShiftPointer(P: Pointer; L: Integer): Pointer;
function AlignLeft(const S: String; L: Integer): String;
function ByteToHex(B: Byte): String;
function IsShortInt(I: Integer): Boolean;

function NativeAddress(P: Pointer): Boolean;

function IsEmpty(const V: Variant): Boolean;

function InterfaceRefCount(I: Pointer): Integer;
function StrRefCountPtr(S: Pointer): Pointer;
function StrRefCount(S: Pointer): Integer;
function StrSizePtr(S: Pointer): Pointer;
function StrSize(S: Pointer): Integer;

function InterfaceRTTIMethodCount(pti: PTypeInfo): Word;
function HasInterfaceRTTIMethod(pti: PTypeInfo): Boolean;

function ExtractNames(const S: String): TStringList;

type
  TIntegerDynArray = array of Integer;

procedure SaveIntDynarrayToStream(const A: TIntegerDynArray; P: TStream);
function LoadIntDynarrayFromStream(P: TStream): TIntegerDynArray;

function SaveStringListToStream(L: TStringList; P: TStream): Integer;
function LoadStringListFromStream(L: TStringList; P: TStream): Integer;

procedure SaveStringToStream(const S: String; P: TStream);
function LoadStringFromStream(P: TStream): String;

{$IFNDEF PAXARM}
procedure SaveWideStringToStream(const S: WideString; P: TStream);
function LoadWideStringFromStream(P: TStream): WideString;
{$ENDIF}

procedure SaveShortStringToStream(const S: ShortString; P: TStream);
function LoadShortStringFromStream(P: TStream): ShortString;

procedure SaveVariantToStream(const Value: Variant; S: TStream);
function LoadVariantFromStream(S: TStream): Variant;

procedure SaveIntegerToStream(Value: Integer; S: TStream);
function LoadIntegerFromStream(S: TStream): Integer;

function Int32ToByteSet(value: Integer): TByteSet;
function ByteSetToInt32(value: TByteSet): Integer;
function ByteSetToString(value: TByteSet;
                         FinTypeId: Integer;
                         EnumNames: TStringList = nil): String;

function Norm(const S: String; L: Integer): String;

function HashNumber(const S: String): Integer;
function VariantToDate(const V: Variant): TDateTime;
function VariantIsString(const V: Variant): Boolean;

function RemoveWhiteSpaces(const S: String): String;
function Space(K: Integer): String;
function PosCh(ch: Char; const S: String): Integer;
function LastPosCh(ch: Char; const S: String): Integer;
function CountCh(ch: Char; const S: String): Integer;
function ReplaceCh(Source, Dest: Char; const S: String): String;
function RemoveCh(Ch: Char; const S: String): String;
function RemoveChars(C: TByteSet; const S: String): String;
function RemoveLeftChars(C: TByteSet; const S: String): String;
function RemoveLeftChars1(C: TByteSet; const S: String): String;
function RemoveRightChars1(C: TByteSet; const S: String): String;
function RemoveRightChars(C: TByteSet; const S: String): String;
function RemoveBorderChars(C: TByteSet; const S: String): String;
function IsPositiveInt(S: PChar): Boolean;

function GuidsAreEqual(const G1, G2: TGUID): Boolean;
function ExtractName(const S: String): String;
function ExtractFullName(const S: String): String;
function ExtractClassName(const S: String): String;
function ExtractOwner(const S: String): String;
function ExtractFullOwner(const S: String): String;

function ChCount(const S: String; Ch: Char): Integer;
function IsPaxFrame: Boolean;

{$IFNDEF UNIX}
{$IFNDEF PAXARM}
function CLSIDFromString(psz: PWideString; out clsid: TGUID): HResult; stdcall;
{$ENDIF}
{$ENDIF}

var
  Types: TStdTypeList;
  Kinds: TStringList;
  Operators: TStringList;

  AsmOperators: TStringList;
  DynDestrList: TIntegerList;
  PushOperators: TIntegerList;

function IsDynDestr(OP: Integer): Boolean;
procedure ErrMessageBox(const S:  String);

function GetImplementorOfInterface(const I: IUnknown): TObject;
function IsValidName(const S: String): Boolean;
function IsDigit(C: Char): Boolean;
function IsAlpha(C: Char): Boolean;
function ByteInSet(B: Char; const S: TByteSet): Boolean;
function Subst(const S, X, Y: String): String;

type
  PClass = ^TClass;
  PSafeCallException = function  (Self: TObject; ExceptObject:
    TObject; ExceptAddr: Pointer): HResult;
  PAfterConstruction = procedure (Self: TObject);
  PBeforeDestruction = procedure (Self: TObject);
  PDispatch          = procedure (Self: TObject; var Message);
  PDefaultHandler    = procedure (Self: TObject; var Message);
  PNewInstance       = function  (Self: TClass) : TObject;
  PFreeInstance      = procedure (Self: TObject);
  PDestroy           = procedure (Self: TObject; OuterMost: ShortInt);
  PVmt = ^TVmt;

{$IFNDEF FPC}
  TVmt = packed record
    Buff: array[0..300] of Byte;
  end;
{$ENDIF}

  PDmtIndexList = ^TDmtIndexList;
  TDmtIndexList = array[0..High(Word)-1] of SmallInt;
  PDmtMethodList = ^TDmtMethodList;
  TDmtMethodList = array[0..High(Word)-1] of Pointer;
  PDmtTable = ^TDmtTable;
  TDmtTable = packed record
    Count: word;
    IndexList: TDmtIndexList;
    MethodList : TDmtMethodList;
  end;
const
  FPC_VIRTUAL_OFFSET = SizeOf(TVMT);

function GetVmtFromClass(AClass: TClass): PVmt;
function GetVmtFromObject(Instance: TObject): PVmt;
function GetClassFromVMT(Vmt: PVmt): TClass;
function GetDestructorAddress(AClass: TClass): Pointer;
function GetDmtFromClass(AClass: TClass): PDmtTable;
function GetDynamicMethodIndex(AClass: TClass; I: integer): integer;
function GetDynamicMethodIndexByAddress(AClass: TClass; Address: Pointer): integer;
procedure SetDynamicMethodIndex(AClass: TClass; I: Integer; value: SmallInt);
function GetDynamicMethodAddress(AClass: TClass; I: integer): Pointer;
procedure SetDynamicMethodAddress(AClass: TClass; I: Integer; value: Pointer);


const
  MaxVirtuals = 999;
type
  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxVirtuals] of pointer;

  PPaxInfo = ^TPaxInfo;
  TPaxInfo = packed record
    PaxSignature: TPaxSignature;
    Prog: Pointer;
    ClassIndex: Integer;
    OldWndProc: Pointer;
    ClassFactoryRec: Pointer;
  end;

  PPaxClassRec = ^TPaxClassRec;
  TPaxClassRec = packed record
    PaxInfo: TPaxInfo;
    VMT: TVMT;
    UserDefinedVirtuals: TPointerArray;
  end;

function GetRBPPtr: Pointer;
function IsDelphiClass(Address: Pointer): Boolean;
function GetPaxInfo(C: TClass): PPaxInfo;
function GetUnitName(C: TClass): String;
function IsPaxObject(X: TObject): Boolean;
function IsPaxClass(C: TClass): Boolean;
function GetHostParentClass(C: TClass): TClass;
function GetVArray(C: TClass): PPointerArray;

function GetIntResultType(T1, T2: Integer): Integer;

type
  PVmtMethod = ^TVmtMethod;
  PVmtMethodTable = ^TVmtMethodTable;

{$IFDEF FPC}
  TVmtMethodCount = Cardinal;

  TVmtMethod = packed record
    MethName: PShortString;
    MethAddr: Pointer;
  end;

  TVmtMethodTable = packed record
    Count: TVmtMethodCount;
    MethList: array[0..300] of TVmtMethod;
  end;

{$ELSE}
  {$IFDEF PAX64}
    TVmtMethod = packed record
      Size: Word;
      Address: Pointer;
      Name: ShortString;
  //  nameLen: Byte;
      { nameChars[nameLen]: _AnsiChr }
    end;
  {$ELSE}
    TVmtMethod = packed record
      Size: Word;
      Address: Pointer;
      Name: ShortString;
    end;
  {$ENDIF}

  TVmtMethodCount = Word;

{$IFDEF PAXARM}
const
  MaxListSize = 1024 * SizeOf(Pointer);
type
{$ENDIF}

  TVmtMethodTable = packed record
    Count: TVmtMethodCount;
    Methods: array[0..MaxInt div 16] of Byte;
    { Methods: array[1..Count] of TVmtMethod; }
  end;

{$ENDIF}

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
{$IFDEF FPC}
    Classes: array [0..8191] of TClass;
{$ELSE}
    Classes: array [0..8191] of ^TPersistentClass;
{$ENDIF}
  end;

  PVmtField = ^TVmtField;
  TVmtField = packed record
    Offset: Cardinal;    { Offset of field in the class data. }
    ClassIndex: Word;    { Index in the FieldClassTable. }
    Name: ShortString;
  end;

  PFieldTable = ^TVmtFieldTable;

  PVmtFieldTable = ^TVmtFieldTable;
  TVmtFieldTable = packed record
    Count: Word;
{$IFDEF ARC}
    FieldClassTable: PFieldClassTable;
{$ELSE}
    FieldClassTable: {$ifdef WIN32} PFieldClassTable {$else} Word {$endif};
{$ENDIF}
    Fields: packed array[0..MaxInt div 16] of Byte;
    { Fields: packed array [1..Count] of TVmtField; }
  end;

function GetMethodSize(PMethod: PVmtMethod): Cardinal;
function GetMethodTable(AClass: TClass): PVmtMethodTable;
function GetFieldSize(PField: PVmtField): Cardinal;
function GetFieldTable(AClass: TClass): PVmtFieldTable;
function GetFieldClassTableSize(FieldClassTable: PFieldClassTable): Integer;
function CreateFieldClassTable(InitCount: SmallInt): PFieldClassTable;
procedure DestroyFieldClassTable(FieldClassTable: PFieldClassTable);
function CreateInterfaceTable(EntryCount: Integer): PInterfaceTable;
function AddEntryToInterfaceTable(P: PInterfaceTable;
                                  var GUID: TGUID;
                                  Address: Pointer;
                                  Offset: Integer): PInterfaceEntry;
procedure DestroyInterfaceTable(P: PInterfaceTable);



{$IFNDEF VARIANTS}
function StringToGUID(const S: string): TGUID;
function GUIDToString(const GUID: TGUID): string;
{$ENDIF}

function LoadText(const FileName: String): String;

function BinSearch(List: TIntegerList; const Key: Integer): Integer;
function GetVisibility(value: TClassVisibility): TMemberVisibility;
function ScalarValueToString(Address: Pointer; T: Integer): String;

procedure RaiseNotImpl;
procedure RIE;
procedure DumpSEH2(I: Integer);
function GetPrevHandler(EstablisherFrame: PPaxExcFrame): Pointer;

function BoolToStr(B: Boolean): String;
function NormR(const S: String; K: Integer): String; overload;
function NormR(I, K: Integer): String; overload;
function NormR(B: Boolean; K: Integer): String; overload;
function NormL(const S: String; K: Integer): String; overload;
function NormL(I, K: Integer): String; overload;
function NormL(B: Boolean; K: Integer): String; overload;

function FindNextVirtualMethodAddress(C: TClass; PrevAddress: Pointer): Pointer;
function VirtualMethodIndex(AClass: TClass; A: Pointer): Integer;
function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
function GetVirtualMethodOffset(AClass: TClass; Address: Pointer): Integer;

function VmtSelfPtrSlot(C: PVMT): PPointer;
function VmtIntfTableSlot(C: PVMT): PPointer;
function VmtAutoTableSlot(C: PVMT): PPointer;
function VmtInitTableSlot(C: PVMT): PPointer;
function VmtTypeInfoSlot(C: PVMT): PPointer;
function VmtFieldTableSlot(C: PVMT): PPointer;
function VmtMethodTableSlot(C: PVMT): PPointer;
function VmtDynamicTableSlot(C: PVMT): PPointer;
function VmtClassNameSlot(C: PVMT): PPointer;
function VmtInstanceSizeSlot(C: PVMT): PPointer;
function VmtParentSlot(C: PVMT): PPointer;
{$IFDEF UNIC}
function VmtEqualsSlot(C: PVMT): PPointer;
function VmtGetHashCodeSlot(C: PVMT): PPointer;
function VmtToStringSlot(C: PVMT): PPointer;
{$ENDIF}
function VmtSafeCallExceptionSlot(C: PVMT): PPointer;
function VmtAfterConstructionSlot(C: PVMT): PPointer;
function VmtBeforeDestructionSlot(C: PVMT): PPointer;
function VmtDispatchSlot(C: PVMT): PPointer;
function VmtDefaultHandlerSlot(C: PVMT): PPointer;
function VmtNewInstanceSlot(C: PVMT): PPointer;
function VmtFreeInstanceSlot(C: PVMT): PPointer;
function VmtDestroySlot(C: PVMT): PPointer;
{$IFDEF ARC}
function Vmt__ObjAddRefSlot(C: PVMT): PPointer;
function Vmt__ObjReleaseSlot(C: PVMT): PPointer;
{$ENDIF}

function DupException(E: Exception): Exception;
function DupWorkException(E: Exception): Exception;
procedure DuplicateException(var Result: Exception; const E: Exception);
procedure DuplicateWorkException(var Result: Exception; const E: Exception);

type
  TPaxValue = record
    case Byte of
      typeEXTENDED:     (VExtended: Extended; VType: Byte);
      typeSINGLE:       (VSingle: Single);
      typeDOUBLE:       (VDouble: Double);
      typeCURRENCY:     (VCurrency: Currency);
      typeSET:          (VSet: Integer);
      typeENUM:         (VEnum: Byte);
      typePROC:         (VProc: Pointer);
      typeEVENT:        (VEvent: TMethod);
      typePOINTER:      (VPointer: Pointer);
      typeCLASS:        (VObject: Pointer);
      typeCLASSREF:     (VClass: TClass);
      typeBYTE:         (VByte: Byte);
      typeSMALLINT:     (VSmallInt: SmallInt);
      typeSHORTINT:     (VShortInt: ShortInt);
      typeWORD:         (VWord: Word);
      typeCARDINAL:     (VCardinal: Cardinal);
      typeINTEGER:      (VInteger: Integer);
      typeINT64:        (VInt64: Int64);
      typeUINT64:       (VUInt64: UInt64);
      typeBOOLEAN:      (VBoolean: Boolean);
      typeBYTEBOOL:     (VByteBool: ByteBool);
      typeWORDBOOL:     (VWordBool: WordBool);
      typeLONGBOOL:     (VLongBool: LongBool);
{$IFNDEF PAXARM}
      typeANSICHAR:     (VAnsiChar: AnsiChar);
      typeWIDESTRING:   (VWideString: PWideString);
      typeANSISTRING:   (VAnsiString: PAnsiString);
      typeSHORTSTRING:  (VShortString: PShortString);
{$ENDIF}
      typeWIDECHAR:     (VWideChar: WideChar);
      typeUNICSTRING:   (VUnicString: PUnicString);
      typeINTERFACE:    (VInterface: Pointer);
      typeVARIANT:      (VVariant: PVariant);
      typeOLEVARIANT:   (VOleVariant: POleVariant);
      typeARRAY:        (VArray: Pointer);
      typeRECORD:       (VRecord: Pointer);
      typeDYNARRAY:     (VDynarray: Pointer);
    end;

function VariantToPaxValue(const V: Variant; FinTypeId: Integer): TPaxValue;
function AddressOfPaxValue(const P: TPaxValue): Pointer;
procedure DisposePaxValue(var P: TPaxValue);

{$IFNDEF PAX64}
{$IFNDEF PAXARM_DEVICE}
procedure ProcessRet32(R_AX: Integer; //eax
                       R_DX: Integer; //edx
                       RetSize: Integer; //ecx
                       R_BP: Pointer);
{$ENDIF}
{$ENDIF}
procedure LoadDouble(P: Pointer);
procedure LoadSingle(P: Pointer);
procedure LoadExtended(P: Pointer);
procedure LoadCurrency(P: Pointer);
{$IFDEF PAX64}
procedure AssignDouble0(P: Pointer);
procedure AssignSingle0(P: Pointer);
procedure AssignExtended0(P: Pointer);
procedure AssignDouble1(P: Pointer);
procedure AssignSingle1(P: Pointer);
procedure AssignExtended1(P: Pointer);
procedure AssignDouble2(P: Pointer);
procedure AssignSingle2(P: Pointer);
procedure AssignExtended2(P: Pointer);
procedure AssignDouble3(P: Pointer);
procedure AssignSingle3(P: Pointer);
procedure AssignExtended3(P: Pointer);
procedure AssignCurrency(P: Pointer);
{$ENDIF}

function GetAbstractMethodIndex(C: TClass; AbstractMethodCount: Integer;
                                i_Address: Pointer = nil): Integer;
function GetSystemVMTOffset(A: Pointer): Integer;
function GetAbstractMethodCount(C: TClass): Integer;

procedure PShortStringFromString(Dest: PShortString; const Source: String);
function StringFromPShortString(S: PShortString): String;
function PTIName(P: PTypeInfo): String;

{$IFDEF PAXARM}
function SLow(const S: string): Integer;
function SHigh(const S: string): Integer;
{$ELSE}
function SLow(const S: string): Integer; overload;
function SHigh(const S: string): Integer; overload;
{$IFDEF UNIC}
function SLow(const S: AnsiString): Integer; overload;
function SHigh(const S: AnsiString): Integer; overload;
{$ENDIF}
{$ENDIF}

function SCopy(const S: String; Index, Length: Integer): String;
procedure SDelete(var S: String; Index, Length: Integer);
procedure SInsert(const Substr: String; var Dest: String; Index: Integer);

function GetAddressGetCallerEIP: Pointer; assembler;

function R1(sz: Integer): Integer;
function R2(sz: Integer): Integer;
function R3(sz: Integer): Integer;

implementation

function R1(sz: Integer): Integer;
begin
  if sz = 0 then
    result := 0
  else
  begin
    sz := sz - 4;
    sz := sz div 16;
    sz := sz + 1;
    result := sz * 16;
  end;
end;

function R2(sz: Integer): Integer;
begin
  sz := sz mod 16;
  if sz = 0 then
    result := 0
  else
    result := 16 - sz;
end;

function R3(sz: Integer): Integer;
var
  I: Integer;
begin
  if sz = 0 then
    result := $0c
  else
  begin
    sz := sz div 4;
    result := $0c;
    for I := 0 to sz - 1 do
      if I mod 4 = 0 then
        Inc(result, 16);
  end;
end;

{$IFDEF GE_DXE4}
function SLow(const S: string): Integer;
begin
  Result := Low(S);
end;

function SHigh(const S: string): Integer;
begin
  Result := High(S);
end;



{$IFNDEF PAXARM}
function SLow(const S: AnsiString): Integer;
begin
  Result := Low(S);
end;

function SHigh(const S: AnsiString): Integer;
begin
  Result := High(S);
end;
{$ENDIF}



{$ELSE}
function SLow(const S: string): Integer;
begin
  Result := 1;
end;

function SHigh(const S: string): Integer;
begin
  Result := Length(S);
end;

{$IFDEF UNIC}
function SLow(const S: AnsiString): Integer;
begin
  Result := 1;
end;

function SHigh(const S: AnsiString): Integer;
begin
  Result := Length(S);
end;
{$ENDIF}

{$ENDIF}

function SCopy(const S: String; Index, Length: Integer): String;
begin
{$IFDEF SZERO}
//  result := Copy(S, Index + 1, Length);
  result := S.Substring(Index, Length);
{$ELSE}
  result := Copy(S, Index, Length);
{$ENDIF}
end;

procedure SDelete(var S: String; Index, Length: Integer);
begin
{$IFDEF SZERO}
  S := S.Remove(Index, Length);
{$ELSE}
  Delete(S, Index, Length);
{$ENDIF}
end;

procedure SInsert(const Substr: String; var Dest: String; Index: Integer);
begin
{$IFDEF SZERO}
  Dest := Dest.Insert(Index, Substr);
{$ELSE}
  Insert(Substr, Dest, Index);
{$ENDIF}
end;

{$IFDEF PAXARM}
{
procedure PShortStringFromString(Dest: PShortString; const Source: String);
var
  I: Cardinal;
  B: array of Byte;
  P: Pointer;
begin
  SetLength(B, Length(Source) + 2);
  UTF8Encode(Source, B);
  Dec(B[0]);
  P := @B[0];
  Move(P^, Dest^, Length(Source) + 1);
end;
}
procedure PShortStringFromString(Dest: PShortString; const Source: String);
var
  L: Integer;
  I: Byte;
  C: Char;
  B: SysUtils.TBytes;
begin
  L := Length(Source);
  if L > 255 then
    raise Exception.Create('Strings longer than 255 characters cannot be converted');
  SetLength(B, L);
  Dest^[0] := L;
  B := TEncoding.Ansi.GetBytes(Source);
  Move(B[0], Dest^[1], L);
end;

{$ELSE}
procedure PShortStringFromString(Dest: PShortString; const Source: String);
begin
  Dest^ := ShortString(Source);
end;
{$ENDIF}

{$IFDEF PAXARM}
function StringFromPShortString2(S: PShortString): String;
var
  fa: TTypeInfoFieldAccessor;
begin
  fa.SetData(PByte(S));
  result := fa.ToString;
end;

function StringFromPShortString(S: PShortString): String;
var
  B: SysUtils.TBytes;
  L: Byte;
begin
  Result := '';
  L := S^[0];
  SetLength(B, L);
  Move(S^[1], B[0], L);
  Result := TEncoding.Ansi.GetString(B);
end;
{$ELSE}
function StringFromPShortString(S: PShortString): String;
begin
  result := String(S^);
end;
{$ENDIF}

function PTIName(P: PTypeInfo): String;
begin
{$IFDEF PAXARM}
  result := P^.NameFld.ToString;
{$ELSE}
  result := String(P^.Name);
{$ENDIF}
end;

type
  TDummyClass = class
    procedure P; virtual; abstract;
  end;

function GetAbstractMethodAddress: Pointer;
begin
  result := GetVArray(TDummyClass)^[0];
end;

function GetAbstractMethodCount(C: TClass): Integer;
var
  Z, CurrA: Pointer;
  P: PPointerArray;
  I: Integer;
begin
  result := 0;
  Z := GetAbstractMethodAddress;
  P := GetVArray(C);
  for I:=0 to MaxVirtuals do
  begin
    CurrA := P^[I];
    if CurrA = Z then
      Inc(result)
    else if CurrA = nil then
      Exit;
  end;
end;

function GetAbstractMethodIndex(C: TClass; AbstractMethodCount: Integer;
                                i_Address: Pointer = nil): Integer;
var
  P: PPointerArray;
  I, K: Integer;
  CurrA: Pointer;
  Z: Pointer;
begin
  result := -1;
  if i_Address = nil then
    Z := GetAbstractMethodAddress
  else
    Z := i_Address;
  P := GetVArray(C);
  if C = TObject then
    K := 0
  else
    K := - GetAbstractMethodCount(C.ClassParent);
  for I:=0 to MaxVirtuals do
  begin
    CurrA := P^[I];
    if CurrA = Z then
    begin
      Inc(K);
      if K = AbstractMethodCount then
      begin
        result := I;
        Exit;
      end;
    end
    else if CurrA = nil then
      Exit;
  end;
end;

{$IFDEF PAXARM}
function GetAddressGetCallerEIP: Pointer;
begin
  result := nil;
end;
{$ELSE}
{$IFDEF MACOS}
function GetAddressGetCallerEIP: Pointer; assembler;
asm
  lea eax, SysInit.@GetCallerEIP
end;
{$ELSE}
{$IFDEF PAX64}
function GetDummyCallerEIP: Pointer; assembler;
asm
  mov rax, [rsp]
end;
function GetAddressGetCallerEIP: Pointer; assembler;
asm
  lea rax, GetDummyCallerEIP
end;
{$ELSE}
function GetDummyCallerEIP: Pointer; assembler;
asm
  mov eax, [esp]
end;
function GetAddressGetCallerEIP: Pointer; assembler;
asm
  lea eax, GetDummyCallerEIP
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF PAX64} //---------------------------------

{$IFDEF UNIX}
procedure ProcessRet32(R_AX: Integer; //eax
                       R_DX: Integer; //edx
                       RetSize: Integer; //ecx
                       R_BP: Pointer);
asm
  mov esp, R_BP
  pop ebp

  cmp ecx, 0
  jz @@L

  mov edx, dword ptr [esp]

  @@loop:
  add esp, 4
  sub ecx, 4
  jnz @@loop

  mov dword ptr [esp], edx

@@L:

  pop ecx
  jmp ecx
end;
{$ENDIF}


{$IFDEF MACOS}

{$IFNDEF PAXARM_DEVICE}
procedure ProcessRet32(R_AX: Integer; //eax
                       R_DX: Integer; //edx
                       RetSize: Integer; //ecx
                       R_BP: Pointer);
asm
  mov esp, R_BP
  pop ebp

  cmp ecx, 0
  jz @@L

  mov edx, dword ptr [esp]

  @@loop:
  add esp, 4
  sub ecx, 4
  jnz @@loop

  mov dword ptr [esp], edx

@@L:

  pop ecx
  jmp ecx
end;
{$ENDIF} // MACOS
{$ENDIF}

{$IFDEF MSWINDOWS}

{$IFDEF FPC}
{$O-}
procedure ProcessRet32(R_AX: Integer; //eax
                       R_DX: Integer; //edx
                       RetSize: Integer; //ecx
                       R_BP: Pointer);
asm
  mov edi, R_BP

  mov esp, edi
  pop ebp

  cmp ecx, 0
  jz @@L

  mov esi, dword ptr [esp]

  @@loop:
  pop edi
  sub ecx, 4
  jnz @@loop

  mov dword ptr [esp], esi

@@L:

  pop edi
  jmp edi
end;
{$ELSE}
procedure ProcessRet32(R_AX: Integer; //eax
                       R_DX: Integer; //edx
                       RetSize: Integer; //ecx
                       R_BP: Pointer);
asm
  mov ebx, R_BP

  mov esp, ebx
  pop ebp

  cmp ecx, 0
  jz @@L

  mov esi, dword ptr [esp]

  @@loop:
  pop ebx
  sub ecx, 4
  jnz @@loop

  mov dword ptr [esp], esi

@@L:

  pop ebx
  jmp ebx
end;
{$ENDIF}
{$ENDIF}

{$ENDIF} // NOT PAX64 ------------------------------

{$IFDEF PAX64}
procedure LoadDouble(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD XMM0, QWORD PTR [RAX]
end;

procedure LoadSingle(P: Pointer); assembler;
asm
  MOV RAX, P
  CVTSD2SS XMM0, QWORD PTR [RAX]
end;

procedure LoadExtended(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD XMM0, QWORD PTR [RAX]
end;

procedure LoadCurrency(P: Pointer); assembler;
asm
  Mov RAX, P
  MOV RAX, [RAX]
end;

procedure AssignDouble0(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM0
end;

procedure AssignSingle0(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSS DWORD PTR [RAX], XMM0
end;

procedure AssignExtended0(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM0
end;

procedure AssignDouble1(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM1
end;

procedure AssignSingle1(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSS DWORD PTR [RAX], XMM1
end;

procedure AssignExtended1(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM1
end;

procedure AssignDouble2(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM2
end;

procedure AssignSingle2(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSS DWORD PTR [RAX], XMM2
end;

procedure AssignExtended2(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM2
end;

procedure AssignDouble3(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM3
end;

procedure AssignSingle3(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSS DWORD PTR [RAX], XMM3
end;

procedure AssignExtended3(P: Pointer); assembler;
asm
  MOV RAX, P
  MOVSD QWORD PTR [RAX], XMM3
end;

procedure AssignCurrency(P: Pointer); assembler;
asm
  Mov RAX, P
  MOV [RAX], RAX
end;

{$ELSE}

{$IFDEF PAXARM_DEVICE}
procedure LoadDouble(P: Pointer);
begin
  RIE;
end;

procedure LoadSingle(P: Pointer);
begin
  RIE;
end;

procedure LoadExtended(P: Pointer);
begin
  RIE;
end;

procedure LoadCurrency(P: Pointer);
begin
  RIE;
end;
{$ELSE}
procedure LoadDouble(P: Pointer); assembler;
asm
  Mov EAX, P
  FLD QWORD PTR [EAX]
end;

procedure LoadSingle(P: Pointer); assembler;
asm
  Mov EAX, P
  FLD DWORD PTR [EAX]
end;

procedure LoadExtended(P: Pointer); assembler;
asm
  Mov EAX, P
  FLD TBYTE PTR [EAX]
end;

procedure LoadCurrency(P: Pointer); assembler;
asm
  Mov EAX, P
  FILD QWORD PTR [EAX]
end;
{$ENDIF}
{$ENDIF}

function VariantToPaxValue(const V: Variant; FinTypeId: Integer): TPaxValue;
begin
  result.VType := FinTypeId;
  case FinTypeId of
    typeEXTENDED: result.VExtended := V;
    typeSINGLE: result.VSingle := V;
    typeDOUBLE: result.VDouble := V;
    typeCURRENCY: result.VCurrency := V;
    typeSET: result.VSet := TVarData(V).VInteger;
    typeENUM: result.VEnum := TVarData(V).VInteger;
    typePROC: result.VProc := Pointer(TVarData(V).VInteger);
//    typeEVENT:        (VEvent: TMethod);
    typePOINTER: result.VPointer := Pointer(TVarData(V).VInteger);
    typeCLASS: result.VObject := TObject(TVarData(V).VInteger);
    typeCLASSREF: result.VClass := TClass(Pointer(TVarData(V).VInteger));
    typeBYTE: result.VByte := V;
    typeSMALLINT: result.VSmallInt := V;
    typeSHORTINT: result.VShortInt := V;
    typeWORD: result.VWord := V;
    typeCARDINAL: result.VCardinal := V;
    typeINTEGER: result.VInteger := V;
{$IFDEF VARIANTS}
    typeINT64: result.VInt64 := V;
    typeUINT64: result.VUInt64 := V;
{$ELSE}
    typeINT64: result.VInt64 := Integer(V);
    typeUINT64: result.VUInt64 := Cardinal(V);
{$ENDIF}
    typeBOOLEAN: result.VBoolean := V;
    typeBYTEBOOL: result.VByteBool := ByteBool(TVarData(V).VInteger);
    typeWORDBOOL: result.VWordBool := V;
    typeLONGBOOL: result.VLongBool := V;
{$IFNDEF PAXARM}
    typeANSICHAR: result.VAnsiChar := AnsiChar(TVarData(V).VInteger);
    typeSHORTSTRING: result.VShortString^ := ShortString(V);
    typeANSISTRING:
    begin
      New(result.VAnsiString);
      result.VAnsiString^ := AnsiString(V);
    end;
    typeWIDESTRING:
    begin
      New(result.VWideString);
      result.VWideString^ := V;
    end;
{$ENDIF}
    typeWIDECHAR: result.VWideChar := WideChar(TVarData(V).VInteger);
    typeUNICSTRING:
    begin
      New(result.VUnicString);
      result.VUnicString^ := V;
    end;
    typeINTERFACE: Pointer(result.VInterface) := Pointer(TVarData(V).VInteger);
    typeVARIANT:
    begin
      New(result.VVariant);
      result.VVariant^ := V;
    end;
    typeOLEVARIANT:
    begin
      New(result.VOleVariant);
      result.VOleVariant^ := V;
    end;
    typeARRAY: result.VArray := Pointer(TVarData(V).VInteger);
    typeRECORD: result.VRecord := Pointer(TVarData(V).VInteger);
    typeDYNARRAY: result.VDynarray := Pointer(TVarData(V).VInteger);
  end;
end;

function AddressOfPaxValue(const P: TPaxValue): Pointer;
begin
  result := nil;
  case P.VType of
    typeEXTENDED: result := @P.VExtended;
    typeSINGLE: result := @P.VSingle;
    typeDOUBLE: result := @P.VDouble;
    typeCURRENCY: result := @P.VCurrency;
    typeSET: result := @P.VSet;
    typeENUM: result := @P.VEnum;
    typePROC: result := @P.VProc;
    typeEVENT: result := @P.VEvent;
    typePOINTER: result := @P.VPointer;
    typeCLASS: result := @P.VObject;
    typeCLASSREF: result := @P.VClass;
    typeBYTE: result := @P.VByte;
    typeSMALLINT: result := @P.VSmallInt;
    typeSHORTINT: result := @P.VShortInt;
    typeWORD: result := @P.VWord;
    typeCARDINAL: result := @P.VCardinal;
    typeINTEGER: result := @P.VInteger;
    typeINT64: result := @P.VInt64;
    typeUINT64: result := @P.VUInt64;
    typeBOOLEAN: result := @P.VBoolean;
    typeBYTEBOOL: result := @P.VByteBool;
    typeWORDBOOL: result := @P.VWordBool;
    typeLONGBOOL: result := @P.VLongBool;
{$IFNDEF PAXARM}
    typeANSICHAR: result := @P.VAnsiChar;
    typeANSISTRING: result := P.VAnsiString;
    typeWIDESTRING: result := P.VWideString;
    typeSHORTSTRING: result := P.VShortString;
{$ENDIF}
    typeWIDECHAR: result := @P.VWideChar;
    typeUNICSTRING: result := P.VUnicString;
    typeINTERFACE: result := @P.VInterface;
    typeVARIANT: result := P.VVariant;
    typeOLEVARIANT: result := P.VOleVariant;
    typeARRAY: result := P.VArray;
    typeRECORD: result := P.VRecord;
    typeDYNARRAY: result := @P.VDynarray;
  end;
end;

procedure DisposePaxValue(var P: TPaxValue);
begin
  case P.VType of
    typeUNICSTRING:
      if P.VUnicString <> nil then
        Dispose(P.VUnicString);
{$IFNDEF PAXARM}
    typeSHORTSTRING:
      if P.VShortString <> nil then
        Dispose(P.VShortString);
    typeANSISTRING:
      if P.VAnsiString <> nil then
        Dispose(P.VAnsiString);
    typeWIDESTRING:
      if P.VWideString <> nil then
        Dispose(P.VWideString);
{$ENDIF}
    typeVARIANT:
      if P.VVariant <> nil then
        Dispose(P.VVariant);
    typeOLEVARIANT:
      if P.VOleVariant <> nil then
        Dispose(P.VOleVariant);
  end;
end;

function DupException(E: Exception): Exception;
var
  C: TExceptionClass;
begin
  Pointer(C) := Pointer(E.ClassType);
  result := C.Create(E.Message);
  if E is PaxExitException then
    (result as PaxExitException).Mode := (E as PaxExitException).Mode;
end;

function DupWorkException(E: Exception): Exception;
begin
  result := TWorkException.Create(E.Message);
end;

procedure DuplicateException(var Result: Exception; const E: Exception);
begin
  if Assigned(result) then
    FreeAndNil(result);
  result := DupException(E);
end;

procedure DuplicateWorkException(var Result: Exception; const E: Exception);
begin
  if Assigned(result) then
    FreeAndNil(result);
  result := DupWorkException(E);
end;

function ClassFromPVMT(V: PVMT): Pointer;
begin
  result := Pointer(IntPax(V) + SizeOf(TVMT));
end;

function ClassOffset(V: PVMT): IntPax;
begin
{$IFDEF FPC}
  result := IntPax(V);
{$ELSE}
  result := IntPax(V) + SizeOf(TVMT);
{$ENDIF}
end;

function VmtSelfPtrSlot(C: PVMT): PPointer;
begin
{$IFDEF FPC}
   result := nil;
{$ELSE}
   result := Pointer(ClassOffset(C) + vmtSelfPtr);
{$ENDIF}
end;

function VmtIntfTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtIntfTable);
end;

function VmtAutoTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtAutoTable);
end;

function VmtInitTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtInitTable);
end;

function VmtTypeInfoSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtTypeInfo);
end;

function VmtFieldTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtFieldTable);
end;

function VmtMethodTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtMethodTable);
end;

function VmtDynamicTableSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtDynamicTable);
end;

function VmtClassNameSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtClassName);
end;

function VmtInstanceSizeSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtInstanceSize);
end;

function VmtParentSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) + vmtParent);
end;

{$IFDEF UNIC}

{$IFDEF PAXARM}
function VmtEqualsSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.Equals));
end;

function VmtGetHashCodeSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.GetHashCode));
end;

function VmtToStringSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.ToString));
end;

{$ELSE}
function GetSystemVMTOffset_Equals: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.Equals
{$ELSE}
  mov eax, VMTOFFSET TObject.Equals
{$ENDIF}
end;

function VmtEqualsSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_Equals);
end;

function GetSystemVMTOffset_GetHashCode: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.GetHashCode
{$ELSE}
  mov eax, VMTOFFSET TObject.GetHashCode
{$ENDIF}
end;

function VmtGetHashCodeSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_GetHashCode);
end;

function GetSystemVMTOffset_ToString: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.ToString
{$ELSE}
  mov eax, VMTOFFSET TObject.ToString
{$ENDIF}
end;

function VmtToStringSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_ToString);
end;
{$ENDIF} // NOT PAXARM
{$ENDIF} // UNIC

{$IFDEF PAXARM}

function VmtSafeCallExceptionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.SafeCallException));
end;

function VmtAfterConstructionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.AfterConstruction));
end;

function VmtBeforeDestructionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.BeforeDestruction));
end;

function VmtDispatchSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.Dispatch));
end;

function VmtDefaultHandlerSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.DefaultHandler));
end;

function VmtNewInstanceSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.NewInstance));
end;

function VmtFreeInstanceSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.FreeInstance));
end;

{$IFDEF ARC}
function Vmt__ObjAddRefSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.__ObjAddRef));
end;

function Vmt__ObjReleaseSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TObject.__ObjRelease));
end;
{$ENDIF}

type
  TMyObject = class(TObject);

function VmtDestroySlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset(@TMyObject.Destroy));
end;

{$ELSE}

function GetSystemVMTOffset_SafeCallException: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.SafeCallException
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.SafeCallException
  {$ELSE}
    mov eax, vmtSafeCallException
  {$ENDIF}
{$ENDIF}
end;

function VmtSafeCallExceptionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_SafeCallException);
end;

function GetSystemVMTOffset_AfterConstruction: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.AfterConstruction
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.AfterConstruction
  {$ELSE}
    mov eax, vmtAfterConstruction
  {$ENDIF}
{$ENDIF}
end;

function VmtAfterConstructionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_AfterConstruction);
end;

function GetSystemVMTOffset_BeforeDestruction: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.BeforeDestruction
{$ELSE}
  {$IFDEF VARIANTS}

    mov eax, VMTOFFSET TObject.BeforeDestruction
  {$ELSE}
    mov eax, vmtBeforeDestruction
  {$ENDIF}
{$ENDIF}
end;

function VmtBeforeDestructionSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_BeforeDestruction);
end;


{$IFDEF LINUX}
function VmtDispatchSlot(C: PVMT): PPointer;
begin
  result := nil; //Pointer(ClassOffset(C) + GetSystemVMTOffset(@TObject.Dispatch));
end;
{$ELSE}
function GetSystemVMTOffset_Dispatch: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.Dispatch
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.Dispatch
  {$ELSE}
    mov eax, vmtDispatch
  {$ENDIF}
{$ENDIF}
end;

function VmtDispatchSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_Dispatch);
end;

{$ENDIF}


function GetSystemVMTOffset_DefaultHandler: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.DefaultHandler
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.DefaultHandler
  {$ELSE}
    mov eax, vmtDefaultHandler
  {$ENDIF}
{$ENDIF}
end;

function VmtDefaultHandlerSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_DefaultHandler);
end;

function GetSystemVMTOffset_NewInstance: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.NewInstance
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.NewInstance
  {$ELSE}
    mov eax, vmtNewInstance
  {$ENDIF}
{$ENDIF}
end;

function VmtNewInstanceSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_NewInstance);
end;

function GetSystemVMTOffset_FreeInstance: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.FreeInstance
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.FreeInstance
  {$ELSE}
    mov eax, vmtFreeInstance
  {$ENDIF}
{$ENDIF}
end;

function VmtFreeInstanceSlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_FreeInstance);
end;

function GetSystemVMTOffset_Destroy: IntPax; assembler;
asm
{$IFDEF PAX64}
  mov rax, VMTOFFSET TObject.Destroy
{$ELSE}
  {$IFDEF VARIANTS}
    mov eax, VMTOFFSET TObject.Destroy
  {$ELSE}
    mov eax, vmtDestroy
  {$ENDIF}
{$ENDIF}
end;

function VmtDestroySlot(C: PVMT): PPointer;
begin
  result := Pointer(ClassOffset(C) +
  GetSystemVMTOffset_Destroy);
end;
{$ENDIF} // NOT PAXARM

function GetSystemVMTOffset(A: Pointer): Integer;
var
  P: Pointer;
begin
  P := Pointer(TObject);
  Result := 0;
  repeat
    if Pointer(P^) = A then
      Exit
    else
    begin
      P := ShiftPointer(P, - SizeOf(Pointer));
      Dec(Result, SizeOf(Pointer));
    end;
  until False;
end;

function FindNextVirtualMethodAddress(C: TClass; PrevAddress: Pointer): Pointer;
var
  MethodIndex: Integer;
  P: PPointerArray;
begin
  P := GetVArray(C);
  MethodIndex := VirtualMethodIndex(C, PrevAddress);
  if MethodIndex = -1 then
    result := nil
  else
    result := P^[MethodIndex + 1];
end;

function VirtualMethodIndex(AClass: TClass; A: Pointer): Integer;
const
  VMTPackageJump : word = $25FF;
var
  I: Integer;
  CurrA: Pointer;
  P: PPointerArray;
begin
  result := -1;
  if A = nil then
    Exit;

  P := GetVArray(AClass);

  if {ModuleIsPackage and} (A <> nil) and (PWord (A)^ = VMTPackageJump)
    then A := PPointer (PPointer (Integer (A) + sizeof (VMTPackageJump))^)^;

  for I:=0 to MaxVirtuals do
  begin
    CurrA := P^[I];
    if CurrA = A then
    begin
      result := I;
      Exit;
    end
    else if CurrA = nil then
      Exit;
  end;
end;

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  P: PPointerArray;
  I: Integer;
begin
  P := GetVArray(AClass);
  result := 0;
  for I:=0 to MaxVirtuals do
    if P^[I] <> nil then
      Inc(result)
    else
      break;
end;

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
var
  P: Pointer;
begin
  P := GetVArray(AClass);
  Result := PPointer(Integer(P) + Index * SizeOf(Pointer))^;
end;

function GetVirtualMethodOffset(AClass: TClass; Address: Pointer): Integer;
var
  I: Integer;
  P: Pointer;
begin
  for I:=0 to GetVirtualMethodCount(AClass) - 1 do
  begin
    P := GetVirtualMethod(AClass, I);
    if P = Address then
    begin
      result := I * 4;
      Exit;
    end;
  end;
  result := -1;
end;


function BoolToStr(B: Boolean): String;
begin
  if B then result := 'true' else result := 'false';
end;

function NormR(const S: String; K: Integer): String; overload;
begin
  if Length(S) > K then
    result := SCopy(S, SLow(S), K)
  else
  begin
    result := S;
    while Length(result) < K do
      result := result + ' ';
  end;
end;

function NormR(I, K: Integer): String; overload;
begin
  result := NormR(IntToStr(I), K);
end;

function NormR(B: Boolean; K: Integer): String; overload;
begin
  result := NormR(BoolToStr(B), K);
end;

function NormL(const S: String; K: Integer): String; overload;
begin
  if Length(S) > K then
    result := SCopy(S, SLow(S), K)
  else
  begin
    result := S;
    while Length(result) < K do
      result := ' ' + result;
  end;
end;

function NormL(I, K: Integer): String; overload;
begin
  result := NormL(IntToStr(I), K);
end;

function NormL(B: Boolean; K: Integer): String; overload;
begin
  result := NormL(BoolToStr(B), K);
end;

{$IFDEF PAXARM}
procedure DumpSEH2(I: Integer);
begin
  RaiseNotImpl;
end;
function GetPrevHandler(EstablisherFrame: PPaxExcFrame): Pointer;
begin
  result := nil;
  RaiseNotImpl;
end;
{$ELSE}

{$IFDEF PAX64}
procedure DumpSEH2(I: Integer);
begin
  RaiseNotImpl;
end;
function GetPrevHandler(EstablisherFrame: PPaxExcFrame): Pointer;
begin
  result := nil;
  RaiseNotImpl;
end;
{$ELSE}
function GetPrevHandler(EstablisherFrame: PPaxExcFrame): Pointer;
var
  P: PExcFrame;
begin
  result := nil;
  asm
    mov eax, fs:[0]
    mov P, eax
  end;
  repeat
    if Integer(P) = Integer(EstablisherFrame) then
      Exit
    else
    begin
      result := P.desc;
      P := P.next;
    end;
  until false;
end;

procedure DumpSEH2(I: Integer);
var
  P: PPaxExcFrame;
  S: String;
  L: TStringList;

  procedure More;
  begin
    P := P.next;
    S := Format('%x', [Integer(P.desc)]) + ' ' + Format('%x', [Integer(P.hEBP)]);
    if PAX_SEH = P^.Magic then
      S := S + '  ' + 'PAX';

    L.Add(S);
  end;
begin
  L := TStringList.Create;
  asm
    mov eax, fs:[0]
    mov P, eax
  end;
  S := Format('%x', [Integer(P.desc)]);
  L.Add(S);

  More;
  More;
  More;
  More;
  More;
  More;
  More;

  L.SaveToFile('seh' + IntToStr(I) + '.txt');
  FreeAndNil(L);
end;
{$ENDIF}
{$ENDIF}

function ScalarValueToString(Address: Pointer; T: Integer): String;
begin
  result := '';
  case T of
    typeBYTE: result := IntToStr(Byte(Address^));
    typeSMALLINT: result := IntToStr(SmallInt(Address^));
    typeSHORTINT: result := IntToStr(ShortInt(Address^));
    typeWORD: result := IntToStr(Word(Address^));
    typeCARDINAL: result := IntToStr(Cardinal(Address^));
    typeINTEGER: result := IntToStr(Integer(Address^));
{$IFNDEF PAXARM}
    typeANSICHAR: result := '' + AnsiChar(Address^) + '';
    typeSHORTSTRING: result := String('' + ShortString(Address^) + '');
    typeANSISTRING: result := String('' + AnsiString(Address^) + '');
    typeWIDESTRING: result := '' + WideString(Address^) + '';
{$ENDIF}
    typeWIDECHAR: result := '' + WideChar(Address^) + '';

    typeBOOLEAN: if Boolean(Address^) then
      result := 'true' else result := 'false';
    typeBYTEBOOL: if ByteBool(Address^) then
      result := 'true' else result := 'false';
    typeWORDBOOL: if WordBool(Address^) then
      result := 'true' else result := 'false';
    typeLONGBOOL: if LongBool(Address^) then
      result := 'true' else result := 'false';

    typeSINGLE: result := FloatToStr(Single(Address^));
    typeDOUBLE: result := FloatToStr(Double(Address^));
    typeEXTENDED: result := FloatToStr(Extended(Address^));
    typeCURRENCY: result := FloatToStr(Currency(Address^));

    typeUNICSTRING: result := '' + UnicString(Address^) + '';

    typePOINTER: result := Format('%x', [Cardinal(Address^)]);

    typeVARIANT: result := VarToStr(Variant(Address^));
    typeOLEVARIANT: result := VarToStr(OleVariant(Address^));
  end;
end;

function GetDmtFromClass(AClass: TClass): PDmtTable;
begin
  result := PDmtTable(ShiftPointer(Pointer(AClass), vmtDynamicTable)^);
end;

function GetDynamicMethodIndex(AClass: TClass; I: integer): integer;
var
  Dmt: PDmtTable;
begin
  Dmt := GetDmtFromClass(AClass);
  if Assigned(Dmt) and (I < Dmt.Count) then
    Result := Dmt.IndexList[I]
  else
    Result := -1;
end;

procedure SetDynamicMethodIndex(AClass: TClass; I: Integer; value: SmallInt);
var
  Dmt: PDmtTable;
begin
  Dmt := GetDmtFromClass(AClass);
  if Assigned(Dmt) and (I < Dmt.Count) then
    Dmt.IndexList[I] := value;
end;

function GetDynamicMethodIndexByAddress(AClass: TClass; Address: Pointer): Integer;
var
  Dmt: PDmtTable;
  DmtMethodList: PDmtMethodList;
  I: Integer;
begin
  result := 0;
  Dmt := GetDmtFromClass(AClass);
  if Assigned(Dmt) then
  begin
    DmtMethodList := @Dmt^.IndexList[Dmt^.Count];
    for I := 0 to Dmt^.Count - 1 do
      if DmtMethodList[I] = Address then
      begin
        result := Dmt^.IndexList[I];
        Exit;
      end;
  end;
end;

function GetDynamicMethodAddress(AClass: TClass; I: integer): Pointer;
var
  Dmt: PDmtTable;
  DmtMethodList: PDmtMethodList;
begin
  Dmt := GetDmtFromClass(AClass);
  if Assigned(Dmt) and (I < Dmt.Count) then
  begin
    DmtMethodList := @Dmt.IndexList[Dmt.Count];
    Result := DmtMethodList[I];
  end
  else
    Result := nil;
end;

procedure SetDynamicMethodAddress(AClass: TClass; I: Integer; value: Pointer);
var
  Dmt: PDmtTable;
  DmtMethodList: PDmtMethodList;
begin
  Dmt := GetDmtFromClass(AClass);
  if Assigned(Dmt) and (I < Dmt.Count) then
  begin
    DmtMethodList := @Dmt.IndexList[Dmt.Count];
    DmtMethodList[I] := value;
  end;
end;

function BinSearch(List: TIntegerList; const Key: Integer): Integer;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
  Found: Boolean;
begin
  First  := 0;
  Last   := List.Count - 1;
  Found  := False;
  Result := -1;

  while (First <= Last) and (not Found) do
  begin
    Pivot := (First + Last) div 2;
    if List[Pivot] = Key then
    begin
      Found  := True;
      Result := Pivot;
    end
    else if List[Pivot] > Key then
      Last := Pivot - 1
    else
      First := Pivot + 1;
  end;
end;

{$IFNDEF VARIANTS}
function StringToGUID(const S: string): TGUID;

  procedure InvalidGUID;
  begin
    raise Exception.Create(errSyntaxError);
  end;

  function HexChar(c: Char): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      InvalidGUID;
      Result := 0;
    end;
  end;

  function HexByte(p: PChar): Char;
  begin
    Result := Char((HexChar(p[0]) shl 4) + HexChar(p[1]));
  end;

var
  i: Integer;
  src, dest: PChar;
begin
  if ((Length(S) <> 38) or (s[1] <> '{')) then InvalidGUID;
  dest := @Result;
  src := PChar(s);
  Inc(src);
  for i := 0 to 3 do
    dest[i] := HexByte(src+(3-i)*2);
  Inc(src, 8);
  Inc(dest, 4);
  if src[0] <> '-' then InvalidGUID;
  Inc(src);
  for i := 0 to 1 do
  begin
    dest^ := HexByte(src+2);
    Inc(dest);
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 4);
    if src[0] <> '-' then InvalidGUID;
    inc(src);
  end;
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  dest^ := HexByte(src);
  Inc(dest);
  Inc(src, 2);
  if src[0] <> '-' then InvalidGUID;
  Inc(src);
  for i := 0 to 5 do
  begin
    dest^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
end;

function GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',   // do not localize
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
    GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;
{$ENDIF}

function CreateFieldClassTable(InitCount: SmallInt): PFieldClassTable;
var
  SZ: Integer;
begin
  SZ := SizeOf(SmallInt) + InitCount * SizeOf(Pointer);
  result := AllocMem(SZ);
  result^.Count := InitCount;
end;

procedure DestroyFieldClassTable(FieldClassTable: PFieldClassTable);
var
  SZ: Integer;
begin
  SZ := GetFieldClassTableSize(FieldClassTable);
  FreeMem(FieldClassTable, SZ);
end;

function GetFieldClassTableSize(FieldClassTable: PFieldClassTable): Integer;
begin
  result := SizeOf(FieldClassTable^.Count) +
            FieldClassTable^.Count * SizeOf(Pointer);
end;

{$IFDEF PAXARM}
function GetFieldTable(AClass: TClass): PVmtFieldTable;
begin
  result := Pointer(ShiftPointer(Pointer(AClass), vmtFieldTable)^);
end;

function GetMethodTable(AClass: TClass): PVmtMethodTable;
begin
  result := Pointer(ShiftPointer(Pointer(AClass), vmtMethodTable)^);
end;

{$ELSE}

{$IFDEF PAX64}
function GetFieldTable(AClass: TClass): PVmtFieldTable; assembler;
asm
  MOV RAX, [RAX].vmtFieldTable
end;

function GetMethodTable(AClass: TClass): PVmtMethodTable; assembler;
asm
  MOV RAX, [RAX].vmtMethodTable
end;
{$ELSE}
function GetFieldTable(AClass: TClass): PVmtFieldTable; assembler;
asm
  MOV EAX, [EAX].vmtFieldTable
end;

function GetMethodTable(AClass: TClass): PVmtMethodTable; assembler;
asm
  MOV EAX, [EAX].vmtMethodTable
end;
{$ENDIF}
{$ENDIF}


function GetMethodSize(PMethod: PVmtMethod): Cardinal;
begin
{$IFDEF FPC}
  result := SizeOf(TVmtMethod);
{$ELSE}
  {$IFDEF PAX64}
     Result := PMethod^.Size;
  {$ELSE}
    {$ifdef WIN32}
      Result := PMethod^.Size;
    {$else}
      Result := PMethod^.Size;
//      Result := SizeOf(Pointer) + Length(PMethod^.Name) + 1;
    {$endif}
  {$ENDIF}
{$ENDIF}
end;

function CreateInterfaceTable(EntryCount: Integer): PInterfaceTable;
var
  SZ: Integer;
begin
  SZ := SizeOf(Integer) + EntryCount * SizeOf(TInterfaceEntry);
  result := AllocMem(SZ);
end;

function AddEntryToInterfaceTable(P: PInterfaceTable;
                                  var GUID: TGUID;
                                  Address: Pointer;
                                  Offset: Integer): PInterfaceEntry;
begin
  with P^.Entries[P^.EntryCount] do
  begin
{$IFDEF FPC}
    IID := @GUID;
{$ELSE}
    IID := GUID;
{$ENDIF}
    VTable := Address;
    IOffset := Offset;
  end;
  result := @ P^.Entries[P^.EntryCount];
  Inc(P^.EntryCount);
end;

procedure DestroyInterfaceTable(P: PInterfaceTable);
var
  SZ: Integer;
begin
  SZ := SizeOf(Integer) + P^.EntryCount * SizeOf(TInterfaceEntry);
  FreeMem(P, SZ);
end;

function GetFieldSize(PField: PVmtField): Cardinal;
begin
  Result := SizeOf(PField^.Offset) + SizeOf(PField^.ClassIndex) +
{$IFDEF ARC}
            PField^.Name[0] + 1;
{$ELSE}
            Length(PField^.Name) + 1;
{$ENDIF}
end;

function IsDynDestr(OP: Integer): Boolean;
begin
  result := DynDestrList.IndexOf(OP) <> -1;
end;

function GetVArray(C: TClass): PPointerArray;
begin
  result := PPointerArray(C);
  {$IFDEF FPC}
  result := ShiftPointer(result, FPC_VIRTUAL_OFFSET);
  {$ENDIF}
end;

{$IFDEF PAXARM}
function IsDelphiClass(Address: Pointer): Boolean;
begin
  result := Address = Pointer(ShiftPointer(Address, vmtSelfPtr)^);
end;
{$ELSE}

{$IFDEF FPC}
function IsDelphiClass(Address: Pointer): Boolean;
begin
  result := not NativeAddress(Pointer(Address^)); // instance size for class
end;
{$ELSE}
function IsDelphiClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;
{$ENDIF}
{$ENDIF}

{$IFDEF PAXARM}
function GetUnitName(C: TClass): String;
begin
  RaiseNotImpl;
end;
{$ELSE}
function GetUnitName(C: TClass): String;
var
  pti: PTypeInfo;
  ptd: PTypeData;
begin
  pti := C.ClassInfo;
  ptd := GetTypeData(pti);
  result := String(ptd^.UnitName);
end;
{$ENDIF}

{$IFDEF PAXARM_DEVICE}
function GetRBPPtr: Pointer;
begin
end;
{$ELSE}

{$IFDEF PAX64}
function GetRBPPtr: Pointer; assembler;
{$IFDEF FPC}
nostackframe;
{$ENDIF}
asm
  mov rax, rbp
end;
{$ELSE}
function GetRBPPtr: Pointer; assembler;
{$IFDEF FPC}
nostackframe;
{$ENDIF}
asm
  mov eax, ebp
end;
{$ENDIF}
{$ENDIF}

function GetPaxInfo(C: TClass): PPaxInfo;
var
  P: Pointer;
begin
  result := nil;
  if C = nil then
    Exit;
  P := GetVMTFromClass(C);
  P := ShiftPointer(P, - SizeOf(TPaxInfo));

{$IFDEF ARC}
  if CompareMem(P, @strPaxSignature, PaxSignatureLength) then
    result := P;
{$ELSE}
  if Byte(P^) = PaxSignatureLength then
    if PShortString(P)^ = strPaxSignature then
      result := P;
{$ENDIF}
end;

function IsPaxObject(X: TObject): Boolean;
begin
  if X = nil then
  begin
    result := false;
    Exit;
  end;

  result := IsPaxClass(X.ClassType);
end;

function IsPaxClass(C: TClass): Boolean;
begin
  if C = nil then
  begin
    result := false;
    Exit;
  end;

  result := GetPaxInfo(C) <> nil;
end;

function GetHostParentClass(C: TClass): TClass;
begin
  result := C.ClassParent;
  if not IsPaxClass(result) then
    Exit
  else
    result := GetHostParentClass(result);
end;

function GetVmtFromClass(AClass: TClass): PVmt;
begin
  if AClass = nil then
  begin
    result := nil;
    Exit;
  end;

  Result := PVmt(AClass);

{$IFDEF FPC}
  Exit;
{$ENDIF}
{$IFDEF PAX64}
  result := ShiftPointer(result, - SizeOf(TVMT));
{$ELSE}
  Dec(Result);
{$ENDIF}
end;

function GetDestructorAddress(AClass: TClass): Pointer;
begin
  result := ShiftPointer(AClass, - SizeOf(Pointer));
  result := Pointer(result^);
end;

function GetVmtFromObject(Instance: TObject): PVmt;
begin
  Result := GetVmtFromClass(Instance.ClassType);
end;

function GetClassFromVMT(Vmt: PVmt): TClass;
begin
  if Vmt = nil then
  begin
    result := nil;
    Exit;
  end;
  {$IFDEF FPC}
    result := TClass(Vmt);
    Exit;
  {$ENDIF}

  Inc(Vmt);
  result := TClass(Vmt);
end;

function ChCount(const S: String; Ch: Char): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SLow(S) to SHigh(S) do
    if S[I] = Ch then
      Inc(result);
end;

function GetIntResultType(T1, T2: Integer): Integer;
begin
  result := typeINTEGER;
  if not ((T1 in IntegerTypes) and (T2 in IntegerTypes)) then
    raise Exception.Create(errInternalError);
  if (T1 in UnsignedIntegerTypes) or (T2 in UnsignedIntegerTypes) then
    result := typeCARDINAL;
  if (T1 = typeINT64) or (T2 = typeINT64) then
    result := typeINT64;
end;

function GetImplementorOfInterface(const I: IUnknown): TObject;
{ TODO -cDOC : Original code by Hallvard Vassbotn }
{ TODO -cTesting : Check the implemetation for any further version of compiler }
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: Longint of
      AddByte: (AdjustmentByte: ShortInt);
      AddLong: (AdjustmentLong: Longint);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  try
    Result := Pointer(I);
    if Assigned(Result) then
    begin
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    end;
  except
    Result := nil;
  end;
end;

function ByteInSet(B: Char; const S: TByteSet): Boolean;
begin
  Result := Ord(B) in S;
end;

function IsValidName(const S: String): Boolean;
var
  I: Integer;
begin
  result := false;
  if S = '' then
    Exit;
  if not IsAlpha(S[1]) then
    Exit;
  for I := SLow(S) to SHigh(S) do
    if not (IsAlpha(S[I]) or IsDigit(S[I])) then
      Exit;
  result := true;
end;

function IsDigit(C: Char): Boolean;
begin
  result := (C >= '0') and (C <='9');
end;

function IsAlpha(C: Char): Boolean;
begin
  result := ((C >= 'a') and (C <='z')) or
            ((C >= 'A') and (C <='Z')) or
             (C = '_');
end;

function Subst(const S, X, Y: String): String;
var
  I, K: Integer;
  C: Char;
  L, LX, LY: Integer;
  Q: String;
begin
  result := S;
  LX := Length(X);
  LY := Length(Y);
  I := 1;
  while I <= Length(result) do
  begin
    C := result[I];
    L := Length(result);
    while not IsAlpha(C) do
    begin
      Inc(I);
      if I >= L then
        Exit;
      C := result[I];
    end;

    K := I + LX - 1;
    if K > L then
      Exit;
    if K = L then
    begin
      Q := Copy(result, I, LX);
      if StrEql(Q, X) then
      begin
        Delete(result, I, LX);
        Insert(Y, result, I);
      end;
      Exit;
    end;

    // K < L

    C := result[K + 1];
    if not (IsAlpha(C) or IsDigit(C)) then
    begin
      Q := SCopy(result, I, LX);
      if StrEql(Q, X) then
      begin
        SDelete(result, I, LX);
        SInsert(Y, result, I);
        Inc(I, LY - LX + 1);
      end
      else
      begin
        C := result[I];
        L := Length(result);
        while IsAlpha(C) or IsDigit(C) do
        begin
          Inc(I);
          if I >= L then
            Exit;
          C := result[I];
        end;
      end;
    end
    else
    begin
      C := result[I];
      L := Length(result);
      while IsAlpha(C) or IsDigit(C) do
      begin
        Inc(I);
        if I >= L then
          Exit;
        C := result[I];
      end;
    end;
  end;
end;


{$IFDEF PAXARM}
function IsPaxFrame: Boolean;
begin
  result := false;
  RIE;
end;
{$ELSE}
{$IFDEF PAX64}
function IsPaxFrame: Boolean;
begin
  result := false;
  RaiseNotImpl;
end;
{$ELSE}
function IsPaxFrame: Boolean;
var
  EstablisherFrame: PPaxExcFrame;
  K: Integer;
begin
  result := false;
  asm
    mov eax, fs:[0]
    mov EstablisherFrame, eax
  end;
  K := 0;
  while EstablisherFrame^.Magic <> PAX_SEH do
  begin
    EstablisherFrame := EstablisherFrame^.Next;
    Inc(K);
    if K = 5 then
      Exit;
  end;
  result := true;
end;
{$ENDIF}
{$ENDIF}

procedure ErrMessageBox(const S:  String);
begin
{$IFDEF CONSOLE}
  writeln(S);
  Exit;
{$ELSE}

{$IFDEF LINUX}
  ShowMessage(S);
{$ELSE}
  {$IFDEF PAX64}
  MessageBox(GetActiveWindow(), PChar(S), PChar('paxCompiler'), MB_ICONEXCLAMATION or MB_OK);
  {$ELSE}
  {$IFDEF MACOS32}
  ShowMessage(S);
  {$ELSE}
  {$IFDEF PAXARM}
  {$IFDEF PAXARM_DEVICE}
  RIE;
  {$ELSE}
  ShowMessage(S);
  {$ENDIF}
  {$ELSE}
  {$IFNDEF UNIX}
  MessageBox(GetActiveWindow(), PChar(S), PChar('paxCompiler'), MB_ICONEXCLAMATION or MB_OK);
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IsPositiveInt(S: PChar): Boolean;
var
  N: Byte;
  c: Char;
begin
  if S^ = #0 then
  begin
    result := false;
    Exit;
  end;

  result := true;

  repeat
    c := S^;

    if c = #0 then
      Exit;

    N := Ord(c);

    if N < Ord('0') then
    begin
      result := false;
      Exit;
    end;

    if N > Ord('9') then
    begin
      result := false;
      Exit;
    end;

    Inc(S);

  until false;
end;

function HashNumber(const S: String): Integer;
var
  I, J: Integer;
  c: Char;
begin
  if S = '' then
  begin
    raise Exception.Create(errInternalError);
  end;

  I := 0;
  for J:=SLow(S) to SHigh(S) do
  begin
    c := UpCase(S[J]);
    I := I shl 1;
    I := I xor ord(c);
  end;
  if I < 0 then I := - I;
  result := I mod MaxHash;
end;

{$IFNDEF UNIX}
{$IFNDEF PAXARM}
{$IFDEF LINUX}
function CLSIDFromString(psz: PWideString; out clsid: TGUID): HResult; stdcall;
begin
  RaiseNotImpl;
end;
{$ELSE}
function CLSIDFromString; external 'ole32.dll' name 'CLSIDFromString';
{$ENDIF}
{$ENDIF}
{$ENDIF}

function GuidsAreEqual(const G1, G2: TGUID): Boolean;
begin
{$IFDEF VARIANTS}
  result := SysUtils.IsEqualGUID(G1, G2);
{$ELSE}
  result := CompareMem(@G1, @G2, SizeOf(TGUID));
{$ENDIF}
end;

function Norm(const S: String; L: Integer): String;
begin
  result := SCopy(S, SLow(S), L);
  while Length(result) < L do
    result := ' ' + result;
end;

function Int32ToByteSet(value: Integer): TByteSet;
begin
  result := [];
  Move(value, result, 4);
end;

function ByteSetToInt32(value: TByteSet): Integer;
begin
  Move(value, result, 4);
end;

function ByteSetToString(value: TByteSet;
                         FinTypeId: Integer;
                         EnumNames: TStringList = nil): String;
var
  I, B1, B2: Integer;
  X: Boolean;
label
  L;
begin
  result := '[';
  X := true;
  B1 := -1;
  B2 := -1;
  for I:= 0 to 255 do
    if I in value then
    begin
      if X then
      begin
        B1 := I;
        B2 := I;
        X := false;
      end
      else
      begin
        B2 := I;
      end;

      if I = 255 then
        goto L;
    end
    else if B1 >= 0 then
    begin
L:
      if B2 - B1 = 0 then
        case FinTypeId of
{$IFNDEF PAXARM}
          typeANSICHAR:
            result := result + '''' + Chr(B1) + '''';
{$ENDIF}
          typeBOOLEAN:
            if B1 = 0 then
              result := result + 'false'
            else
              result := result + 'true';
          typeENUM:
          begin
            if EnumNames = nil then
              result := result + IntToStr(B1)
            else
              result := result + EnumNames[B1];
          end
          else
            result := result + IntToStr(B1);
        end
      else
        case FinTypeId of
{$IFNDEF PAXARM}
          typeANSICHAR:
            result := result + '''' + Chr(B1) + '''' + '..' +
                               '''' + Chr(B2) + '''';
{$ENDIF}
          typeBOOLEAN:
            result := result + 'false..true';
          else
            result := result + IntToStr(B1) + '..' + IntToStr(B2);
        end;

      B1 := -1;
      X := true;
      result := result + ',';

    end;

  if result[Length(result)] = ',' then
    result[Length(result)] := ']'
  else
    result := result + ']';
end;

procedure SaveIntDynarrayToStream(const A: TIntegerDynArray; P: TStream);
var
  I, K: Integer;
begin
  K := System.Length(A);
  P.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    P.Write(A[I], SizeOf(A[I]));
end;

function LoadIntDynarrayFromStream(P: TStream): TIntegerDynArray;
var
  I, K: Integer;
begin
  P.Read(K, SizeOf(Integer));
  SetLength(result, K);
  for I := 0 to K - 1 do
    P.Read(result[I], SizeOf(result[I]));
end;

{$IFDEF UNIC}
function SaveStringListToStream(L: TStringList; P: TStream): Integer;
var
  I, K: Integer;
begin
  K := L.Count;
  P.Write(K, 4);
  result := 4;
  for I:=0 to L.Count - 1 do
  begin
    SaveStringToStream(L[I], P);
    Inc(result, 4 + Length(L[I]) * SizeOf(Char));
  end;
end;

function LoadStringListFromStream(L: TStringList; P: TStream): Integer;
var
  I, K: Integer;
  S: String;
begin
  P.Read(K, 4);
  result := 4;
  for I:=0 to K - 1 do
  begin
    S := LoadStringFromStream(P);
    L.Add(S);
    Inc(result, 4 + Length(S) * SizeOf(Char));
  end;
end;
{$ELSE}
function SaveStringListToStream(L: TStringList; P: TStream): Integer;
var
  I, K: Integer;
  S: ShortString;
begin
  result := 0;
  K := L.Count;
  P.Write(K, 4);
  Inc(result, 4);
  for I:=0 to L.Count - 1 do
  begin
    S := L[I];
    K := Length(S);
    P.Write(K, 4);
    P.Write(S[1], Length(S));
    Inc(result, Length(S) + 4);
  end;
end;

function LoadStringListFromStream(L: TStringList; P: TStream): Integer;
var
  I, K, Count: Integer;
  S: ShortString;
begin
  result := 0;
  P.Read(Count, 4);
  Inc(result, 4);
  for I:=0 to Count - 1 do
  begin
    P.Read(K, 4);
    P.Read(S[1], K);
    S[0] := AnsiChar(Chr(K));
    L.Add(S);
    Inc(result, Length(S) + 4);
  end;
end;
{$ENDIF}

procedure SaveShortStringToStream(const S: ShortString; P: TStream);
begin
{$IFDEF ARC}
  P.Write(S[0], S[0] + 1);
{$ELSE}
  P.Write(S[0], Length(S) + 1);
{$ENDIF}
end;

function LoadShortStringFromStream(P: TStream): ShortString;
var
  L: Byte;
begin
  P.Read(L, SizeOf(Byte));
  P.Read(result[1], L);
{$IFDEF ARC}
  result[0] := L;
{$ELSE}
  result[0] := AnsiChar(Chr(L));
{$ENDIF}
end;

procedure SaveStringToStream(const S: String; P: TStream);
var
  K: Integer;
begin
  K := Length(S);
  P.Write(K, 4);
  if K > 0 then
    P.Write(Pointer(S)^, K * SizeOf(Char));
end;

function LoadStringFromStream(P: TStream): String;
var
  K: Integer;
begin
  P.Read(K, 4);
  SetLength(result, K);
  if K > 0 then
    P.Read(Pointer(result)^, K * SizeOf(Char));
end;

{$IFNDEF PAXARM}
procedure SaveWideStringToStream(const S: WideString; P: TStream);
var
  K: Integer;
begin
  K := Length(S);
  P.Write(K, 4);
  if K > 0 then
    P.Write(Pointer(S)^, K * 2);
end;

function LoadWideStringFromStream(P: TStream): WideString;
var
  K: Integer;
begin
  P.Read(K, 4);
  SetLength(result, K);
  if K > 0 then
    P.Read(Pointer(result)^, K * 2);
end;
{$ENDIF}

procedure SaveVariantToStream(const Value: Variant; S: TStream);
var
  VType: Integer;
begin
  VType := VarType(Value);
  SaveIntegerToStream(VType, S);
  case VType of
    varString:
      SaveStringToStream(Value, S);
{$IFDEF UNIC}
    varUString:
      SaveStringToStream(Value, S);
{$ENDIF}
{$IFNDEF PAXARM}
    varOleStr:
      SaveWideStringToStream(Value, S);
{$ENDIF}
    else
      S.Write(Value, SizeOf(Variant));
  end;
end;

function LoadVariantFromStream(S: TStream): Variant;
var
  VType: Integer;
begin
  VType := LoadIntegerFromStream(S);
  case VType of
    varString:
      result := LoadStringFromStream(S);
{$IFDEF UNIC}
    varUString:
      result := LoadStringFromStream(S);
{$ENDIF}
{$IFNDEF PAXARM}
    varOleStr:
      result := LoadWideStringFromStream(S);
{$ENDIF}
    else
      S.ReadBuffer(result, SizeOf(Variant));
  end;
end;

function VariantIsString(const V: Variant): Boolean;
begin
  if TVarData(V).VType = varOleStr then
    result := true
{$IFDEF UNIC}
  else if TVarData(V).VType = varUString then
    result := true
{$ENDIF}
  else if TVarData(V).VType = varString then
    result := true
  else
    result := false;
end;

function VariantToDate(const V: Variant): TDateTime;
begin
{$IFDEF VARIANTS}
  result := Variants.VarToDateTime(V);
{$ELSE}
  result := V;
{$ENDIF}
end;

procedure SaveIntegerToStream(Value: Integer; S: TStream);
begin
  S.Write(Value, SizeOf(Integer));
end;

function LoadIntegerFromStream(S: TStream): Integer;
begin
  S.Read(result, SizeOf(Integer));
end;

function IsEmpty(const V: Variant): Boolean;
begin
  result := VarType(V) = varEmpty;
end;

function MPtr(X: Integer): Integer;
begin
  result := X;
  while result mod SizeOf(Pointer) <> 0 do Inc(result);
end;

function StrEql(const S1, S2: String): Boolean;
begin
  result := CompareText(S1, S2) = 0;
end;

{$IFNDEF PAXARM}
function Text(const source: AnsiString): TStringList;
var
  I, L, Start: Integer;
  S: String;
begin
  result := TStringList.Create;
  L := Length(source);
  I := 1;
  Start := I;
  repeat
     if source[I] = #13 then
     begin
       Inc(I);
       if I <= L then
       begin
         if source[I] = #10 then
         begin
           S := String(Copy(Source, Start, I - Start - 1));
           result.Add(S);
           Inc(I);
           if I > L then
             break;
           Start := I;
         end
       end
       else
         break;
     end
     else if source[I] = #10 then
     begin
       S := String(Copy(Source, Start, I - Start - 1));
       result.Add(S);
       Inc(I);
       Start := I;
       if I > L then
         break;
     end
     else
     begin
       Inc(I);
       if I > L then
       begin
         S := String(Copy(Source, Start, I - Start - 1));
         result.Add(S);
         break;
       end;
     end;
  until false;
end;
{$ENDIF}

function ExtractName(const S: String): String;
var
  I, L: Integer;
begin
  L := SHigh(S);
  for I:= SHigh(S) downto SLow(S) do
    if ByteInSet(S[I], [Ord('.'), Ord('/'), Ord('\')]) then
    begin
      result := SCopy(S, I + 1, L - I);
      Exit;
    end;
  result := S;
end;

function ExtractFullName(const S: String): String;
var
  I, L: Integer;
begin
  L := SHigh(S);
  for I:= SLow(S) to L do
    if S[I] = '.' then
    begin
      result := SCopy(S, I + 1, L - I);
      Exit;
    end;
  result := S;
end;

function ExtractFullOwner(const S: String): String;
var
  I, L, K: Integer;
  C: Char;
begin
  K := 0;
  L := SHigh(S);
  for I:= L downto SLow(S) do
  begin
    C := S[I];
    case C of
      '>': Inc(K);
      '<': Dec(K);
      '.':
      if K = 0 then
      begin
        result := SCopy(S, SLow(S), I - SLow(S));
        Exit;
      end;
    end;
  end;
  result := '';
end;

function ExtractClassName(const S: String): String;
var
  I, L, K1, K2: Integer;
begin
  L := SHigh(S);
  result := '';

  if L = 0 then
    Exit;

  K1 := 0;
  K2 := 0;

  for I:= L downto SLow(S) do
    if S[I] = '.' then
      if K2 = 0 then
        K2 := I
      else
      begin
        K1 := I;
        result := SCopy(S, K1 + 1, K2 - K1 - 1);
        Exit;
      end;
  result := SCopy(S, K1 + SLow(S), K2 - K1 - SLow(S));
end;

function ExtractOwner(const S: String): String;
var
  P: Integer;
begin
  result := '';
  P := PosCh('.', S);
  if P > 0 then
    result := SCopy(S, SLow(S), P - SLow(S));
end;

function NativeAddress(P: Pointer): Boolean;
begin
  result := Abs(IntPax(P)) > 65535;
end;


function ShiftPointer(P: Pointer; L: Integer): Pointer;
begin
  result := Pointer(IntPax(P) + L);
end;

function AlignLeft(const S: String; L: Integer): String;
begin
  result := S;
  while Length(result) < L do
    result := result + ' ';
end;

function ByteToHex(B: Byte): String;
begin
  result := Format('%x', [B]);
  if Length(result) = 1 then
    result := '0' + result;
end;

function IsShortInt(I: Integer): Boolean;
begin
  result := Abs(I) <= 127;
end;

function InterfaceRTTIMethodCount(pti: PTypeInfo): Word;
var
  ptd: PTypeData;
  P: Pointer;
begin
  ptd := GetTypeData(pti);
  P := @ ptd^.IntfUnit;
  P := ShiftPointer(P, Length(StringFromPShortString(@ptd^.IntfUnit)) + 1);
  Result := Word(P^);
  if result = $FFFF then
    result := 0;
end;

function HasInterfaceRTTIMethod(pti: PTypeInfo): Boolean;
var
  ptd: PTypeData;
  P: Pointer;
  W: Word;
begin
  ptd := GetTypeData(pti);
  P := @ ptd^.IntfUnit;
  P := ShiftPointer(P, Length(StringFromPShortString(@ptd^.IntfUnit)) + 1 + SizeOf(Word));
  W := Word(P^);
  if W = $FFFF then
    result := false
  else
    result := true;
end;

function InterfaceRefCount(I: Pointer): Integer;
begin
  result := IInterface(I)._AddRef - 1;
  IInterface(I)._Release;
end;

function StrRefCountPtr(S: Pointer): Pointer;
begin
  if S <> nil then
    result := Pointer(Integer(Pointer(S)) - 8)
  else
    result := nil;
end;

function StrRefCount(S: Pointer): Integer;
begin
  result := Integer(StrRefCountPtr(S)^);
end;

function StrSizePtr(S: Pointer): Pointer;
begin
  if S <> nil then
    result := Pointer(Integer(Pointer(S)) - 4)
  else
    result := nil;
end;

function StrSize(S: Pointer): Integer;
begin
  result := Integer(StrSizePtr(S)^);
end;

function Space(K: Integer): String;
var
  I: Integer;
begin
  result := '';
  for I := 1 to K do
    result := result + ' ';
end;

function RemoveWhiteSpaces(const S: String): String;
var
  I: Integer;
  ch: Char;
begin
  result := '';
  for I:=SLow(S) to SHigh(S) do
  begin
    ch := S[I];
    if not ByteInSet(ch, WhiteSpaces) then
      result := result + ch;
  end;
end;

function RemoveChars(C: TByteSet; const S: String): String;
var
  I: Integer;
  ch: Char;
begin
  result := '';
  for I:=SLow(S) to SHigh(S) do
  begin
    ch := S[I];
    if not ByteInSet(ch, C) then
      result := result + ch;
  end;
end;

function RemoveLeftChars(C: TByteSet; const S: String): String;
var
  I: Integer;
  ch: Char;
  b, bb: Boolean;
begin
  result := '';
  bb := true;
  for I:=SLow(S) to SHigh(S) do
  begin
    ch := S[I];

    b := ByteInSet(ch, C);

    if b and bb then
     continue;

    bb := false;
    result := result + ch;
  end;
end;

function RemoveRightChars(C: TByteSet; const S: String): String;
var
  I: Integer;
  ch: Char;
  b, bb: Boolean;
begin
  result := '';
  bb := true;
  for I:=SHigh(S) downto SLow(S) do
  begin
    ch := S[I];
    b := ByteInSet(ch, C);
    if b and bb then
     continue;
    bb := false;
    result := ch + result;
  end;
end;

function RemoveLeftChars1(C: TByteSet; const S: String): String;
var
  I: Integer;
  ch: Char;
  b, bb: Boolean;
begin
  result := '';
  bb := true;
  for I:=SLow(S) to SHigh(S) do
  begin
    ch := S[I];

    b := ByteInSet(ch, C);

    if b and bb then
      continue
    else if b then
      break;

    bb := false;
    result := result + ch;
  end;
end;

function RemoveRightChars1(C: TByteSet; const S: String): String;
var
  I: Integer;
  ch: Char;
  b, bb: Boolean;
begin
  result := '';
  bb := true;
  for I:=SHigh(S) downto SLow(S) do
  begin
    ch := S[I];

    b := ByteInSet(ch, C);

    if b and bb then
      continue
    else if b then
      break;

    bb := false;
    result := result + ch;
  end;
end;

function RemoveBorderChars(C: TByteSet; const S: String): String;
begin
  result := RemoveRightChars(C, S);
  result := RemoveLeftChars(C, result);
end;

function PosCh(ch: Char; const S: String): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SLow(S) to SHigh(S) do
    if S[I] = ch then
    begin
      result := I;
      Exit;
    end;
end;

function LastPosCh(ch: Char; const S: String): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SHigh(S) downto SLow(S) do
    if S[I] = ch then
    begin
      result := I;
      Exit;
    end;
end;

function CountCh(ch: Char; const S: String): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SLow(S) to SHigh(S) do
    if S[I] = ch then
      Inc(result);
end;

function RemoveCh(Ch: Char; const S: String): String;
var
  I: Integer;
begin
  result := '';
  for I:=SLow(S) to SHigh(S) do
    if S[I] <> Ch then
      result := result + S[I];
end;

function ReplaceCh(Source, Dest: Char; const S: String): String;
var
  I: Integer;
begin
  result := S;
  for I := SLow(result) to SHigh(result) do
    if result[I] = Source then
      result[I] := Dest;
end;

function LoadText(const FileName: String): String;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(FileName);
    result := L.Text;
  finally
    FreeAndNil(L);
  end;
end;

function _IsJSType(T: Integer; P: Pointer): Boolean;
begin
  result := false;
end;

function GetVisibility(value: TClassVisibility): TMemberVisibility;
begin
  result := mvPublic;
  case value of
    cvNone, cvPrivate: result := mvPrivate;
    cvProtected: result := mvProtected;
    cvPublic: result := mvPublic;
    cvPublished: result := mvPublished;
  end;
end;

function ExtractNames(const S: String): TStringList;
var
  R: String;
  P: Integer;
begin
  result := TStringList.Create;
  R := S;
  repeat
    P := PosCh('.', R);
    if P = 0 then
    begin
      result.Add(R);
      Exit;
    end;
    result.Add(SCopy(R, SLow(S), P - SLow(S)));
    SDelete(R, SLow(R), P + 1 - SLow(R));
  until false;
end;

procedure RaiseNotImpl;
begin
  raise Exception.Create(errNotImplementedYet);
end;

procedure RIE;
begin
  raise Exception.Create(errInternalError);
end;

initialization

  Types := TStdTypeList.Create;
  with Types do
  begin
    Add('', 0);
    Add('Void', SizeOf(Pointer));
    Add('Boolean', SizeOf(Boolean));
    Add('Byte', SizeOf(Byte));
{$IFDEF PAXARM}
    Add('Char', SizeOf(Char));
 {$ELSE}
    Add('Char', SizeOf(AnsiChar));
{$ENDIF}
    Add('String', SizeOf(Pointer));
    Add('Word', SizeOf(Word));
    Add('Integer', SizeOf(Integer));
    Add('Double', SizeOf(Double));
    Add('Pointer', SizeOf(Pointer));
    Add('#RECORD', 0);
    Add('#ARRAY', 0);
    Add('#ALIAS', 0);
    Add('#ENUM', SizeOf(Byte));
    Add('#PROC', SizeOf(Pointer));
    Add('#SET', 32);
    Add('ShortString', 256);
    Add('Single', SizeOf(Single));
    Add('Extended', SizeOf(Extended));
    Add('#CLASS', SizeOf(Pointer));
    Add('#CLASSREF', SizeOf(Pointer));
    Add('WideChar', SizeOf(WideChar));
{$IFDEF PAXARM}
    Add('WideString', SizeOf(Pointer));
{$ELSE}
    Add('WideString', SizeOf(WideString));
{$ENDIF}
    Add('Variant', SizeOf(Variant));
    Add('#DYNARRAY', SizeOf(Pointer));
    Add('Int64', SizeOf(Int64));
    Add('#INTERFACE', SizeOf(Pointer));
    Add('Cardinal', SizeOf(Cardinal));
    Add('#EVENT', SizeOf(TMethod));
    Add('Currency', SizeOf(Currency));
    Add('SmallInt', SizeOf(SmallInt));
    Add('ShortInt', SizeOf(ShortInt));
    Add('WordBool', SizeOf(WordBool));
    Add('LongBool', SizeOf(LongBool));
    Add('ByteBool', SizeOf(ByteBool));
    Add('OleVariant', SizeOf(OleVariant));
    Add('UnicodeString', SizeOf(UnicString));
    Add('#OPENARRAY', SizeOf(Pointer));
    Add('#TYPEPARAM', 0);
    Add('UInt64', SizeOf(Int64));
    Add('#VOBJECT', SizeOf(VARIANT));
    Add('#HELPER', SizeOf(Pointer));
  end;

  Kinds := TStringList.Create;
  with Kinds do
  begin
    Add('');
    Add('VAR');
    Add('CONST');
    Add('SUB');
    Add('PARAM');
    Add('TYPE');
    Add('T FIELD');
    Add('LABEL');
    Add('NAMESP');
    Add('CONSTR');
    Add('DESTR');
    Add('PROP');
    Add('END CH');
  end;

  Operators := TStringList.Create;
  with Operators do
  begin
    OP_NOP := - Add('NOP');
    OP_SEPARATOR := - Add('SEPARATOR');
    OP_ADD_COMMENT := - Add('COMMENT');
    OP_STMT := - Add('STMT');
    OP_SET_CODE_LINE := - Add('SCL');

    OP_BEGIN_TEXT := - Add('BEGIN TEXT');
    OP_END_TEXT := - Add('END TEXT');

    OP_BEGIN_LOOP := - Add('BEGIN LOOP');
    OP_EPILOGUE_LOOP := - Add('EPILOGUE LOOP');
    OP_END_LOOP := - Add('END LOOP');

    OP_OPTION_EXPLICIT := - Add('EXPLICIT');
    OP_INIT_FWARRAY := - Add('INIT FWARRAY');

    OP_CHECK_FINAL := - Add('CHECK FINAL');

    OP_BEGIN_NAMESPACE := - Add('BEGIN NAMESPACE');
    OP_END_NAMESPACE := - Add('END NAMESPACE');

    OP_BEGIN_TYPE := - Add('BEGIN TYPE');
    OP_END_TYPE := - Add('END TYPE');

    OP_BEGIN_CLASS_TYPE := - Add('BEGIN CLASS TYPE');
    OP_END_CLASS_TYPE := - Add('END CLASS TYPE');

    OP_BEGIN_CLASSREF_TYPE := - Add('BEGIN CLASSREF TYPE');
    OP_END_CLASSREF_TYPE := - Add('END CLASSREF TYPE');

    OP_BEGIN_HELPER_TYPE := - Add('BEGIN HELPER TYPE');
    OP_END_HELPER_TYPE := - Add('END HELPER TYPE');

    OP_BEGIN_INTERFACE_TYPE := - Add('BEGIN INTERFACE TYPE');
    OP_END_INTERFACE_TYPE := - Add('END INTERFACE TYPE');

    OP_BEGIN_RECORD_TYPE := - Add('BEGIN RECORD TYPE');
    OP_END_RECORD_TYPE := - Add('END RECORD TYPE');

    OP_BEGIN_ARRAY_TYPE := - Add('BEGIN ARRAY TYPE');
    OP_END_ARRAY_TYPE := - Add('END ARRAY TYPE');

    OP_BEGIN_DYNARRAY_TYPE := - Add('BEGIN DYNARRAY TYPE');
    OP_END_DYNARRAY_TYPE := - Add('END DYNARRAY TYPE');

    OP_BEGIN_SUBRANGE_TYPE := - Add('BEGIN SUBRANGE TYPE');
    OP_END_SUBRANGE_TYPE := - Add('END SUBRANGE TYPE');

    OP_BEGIN_ENUM_TYPE := - Add('BEGIN ENUM TYPE');
    OP_END_ENUM_TYPE := - Add('END ENUM TYPE');

    OP_BEGIN_SET_TYPE := - Add('BEGIN SET TYPE');
    OP_END_SET_TYPE := - Add('END SET TYPE');

    OP_BEGIN_POINTER_TYPE := - Add('BEGIN POINTER TYPE');
    OP_END_POINTER_TYPE := - Add('END POINTER TYPE');

    OP_BEGIN_PROC_TYPE := - Add('BEGIN PROC TYPE');
    OP_END_PROC_TYPE := - Add('END PROC TYPE');

    OP_BEGIN_ALIAS_TYPE := - Add('BEGIN ALIAS TYPE');
    OP_END_ALIAS_TYPE := - Add('END ALIAS TYPE');

{$IFNDEF PAXARM}
    OP_BEGIN_SHORTSTRING_TYPE := - Add('BEGIN SHORTSTRING TYPE');
    OP_END_SHORTSTRING_TYPE := - Add('END SHORTSTRING TYPE');
{$ENDIF}

    OP_BEGIN_CONST := - Add('BEGIN CONST');
    OP_END_CONST := - Add('END CONST');

    OP_BEGIN_VAR := - Add('BEGIN VAR');
    OP_END_VAR := - Add('END VAR');

    OP_GET_NEXTJSPROP := - Add('GET NEXTJSPROP');
    OP_CLEAR_REFERENCES := - Add('CLEAR REFERENCES');

    OP_BEGIN_LIBRARY := - Add('BEGIN LIBRARY');
    OP_BEGIN_EXPORT := - Add('BEGIN EXPORT');
    OP_BEGIN_MODULE := - Add('BEGIN MODULE');
    OP_END_MODULE := - Add('END MODULE');
    OP_BEGIN_INCLUDED_FILE := - Add('BEGIN INCLUDED FILE');
    OP_END_INCLUDED_FILE := - Add('END INCLUDED FILE');
    OP_END_INTERFACE_SECTION := - Add('END INTERFACE SECTION');
    OP_END_IMPORT := - Add('END IMPORT');
    OP_BEGIN_INITIALIZATION := - Add('BEGIN INITIALIZATION');
    OP_END_INITIALIZATION := - Add('END INITIALIZATION');
    OP_BEGIN_FINALIZATION := - Add('BEGIN FINALIZATION');
    OP_END_FINALIZATION := - Add('END FINALIZATION');

    OP_EXTRA_BYTECODE := - Add('EXTRA BYTECODE');

    OP_WARNINGS_ON := - Add('WARNINGS ON');
    OP_WARNINGS_OFF := - Add('WARNINGS OFF');

    OP_FRAMEWORK_ON := - Add('FRAMEWORK ON');
    OP_FRAMEWORK_OFF := - Add('FRAMEWORK OFF');

    OP_TRY_ON := - Add('TRY ON');
    OP_TRY_OFF := - Add('TRY OFF');
    OP_FINALLY := - Add('FINALLY');
    OP_EXCEPT := - Add('EXCEPT');
    OP_EXCEPT_SEH := - Add('EXCEPT SEH');
    OP_EXCEPT_ON := - Add('EXCEPT ON');
    OP_RAISE := - Add('RAISE');
    OP_COND_RAISE := - Add('COND RAISE');
    OP_BEGIN_EXCEPT_BLOCK := - Add('BEGIN EXCEPT BLOCK');
    OP_END_EXCEPT_BLOCK := - Add('END EXCEPT BLOCK');

    OP_OVERFLOW_CHECK := - Add('OVERFLOW CHECK');

    OP_PAUSE := - Add('PAUSE');
    OP_CHECK_PAUSE := - Add('CHECK PAUSE');
    OP_CHECK_PAUSE_LIGHT := - Add('CHECK PAUSE LIGHT');
    OP_HALT := - Add('HALT');

    OP_EMIT_OFF := - Add('EMIT OFF');
    OP_EMIT_ON := - Add('EMIT ON');

    OP_BEGIN_USING := - Add('BEGIN USING');
    OP_END_USING := - Add('END USING');

    OP_BEGIN_BLOCK := - Add('BEGIN BLOCK');
    OP_END_BLOCK := - Add('END BLOCK');

    OP_EVAL := - Add('EVAL');
    OP_EVAL_OUTER := - Add('EVAL OUTER');

    OP_EVAL_INHERITED := - Add('EVAL INHERITED');
    OP_EVAL_CONSTRUCTOR := - Add('EVAL CONSTRUCTOR');
    OP_UPDATE_INSTANCE := - Add('UPDATE INSTANCE');
    OP_ADJUST_INSTANCE := - Add('ADJUST INSTANCE');
    OP_CLEAR_EDX := - Add('CLEAR EDX');
    OP_IMPLEMENTS := - Add('IMPLEMENTS');

    OP_MYCLASS := - Add('MYCLASS');
    OP_MYBASE := - Add('MYBASE');

    OP_LOAD_PROC := - Add('LOAD PROC');

    OP_CHECK_OVERRIDE := - Add('CHECK OVERRIDE');

    OP_EXIT := - Add('EXIT');
    OP_GO := - Add('GO');
    OP_GO_1 := - Add('GO 1');
    OP_GO_2 := - Add('GO 2');
    OP_GO_3 := - Add('GO 3');
    OP_GO_TRUE := - Add('GO TRUE');
    OP_GO_FALSE := - Add('GO FALSE');
    OP_GO_TRUE_BOOL := - Add('GO TRUE BOOL');
    OP_GO_FALSE_BOOL := - Add('GO FALSE BOOL');
    OP_GO_DL := - Add('GO DL');
    OP_CALL_INHERITED := - Add('CALL INHERITED');
    OP_BEGIN_CALL := - Add('BEGIN CALL');
    OP_CALL := - Add('CALL');
    OP_CALL_DEFAULT_CONSTRUCTOR := - Add('CALL DEFAULT CONSTRUCTOR');
    OP_CHECK_SUB_CALL := - Add('CHECK SUB CALL');
    OP_BEGIN_VCALL := - Add('BEGIN VCALL');
    OP_VCALL := - Add('VCALL');
    OP_PUSH := - Add('PUSH');
    OP_PUSH_INSTANCE := - Add('PUSH INSTANCE');
    OP_PUSH_CLASSREF := - Add('PUSH CLASSREF');
    OP_PUSH_CONTEXT := - Add('PUSH CONTEXT');
    OP_POP_CONTEXT := - Add('POP CONTEXT');
    OP_FIND_CONTEXT := - Add('FIND CONTEXT');
    OP_FIND_JS_FUNC := - Add('FIND JS FUNC');
    OP_LABEL := - Add('LABEL');
    OP_TYPE_CAST := - Add('TYPE CAST');
    OP_DECL_SUB := - Add('DECL SUB');
    OP_DECLARE_MEMBER := - Add('DECLARE MEMBER');
    OP_BEGIN_SUB := - Add('BEGIN SUB');
    OP_DECLARE_LOCAL_VAR := - Add('DECLARE LOCAL VAR');
    OP_DECLARE_TEMP_VAR := - Add('DECLARE TEMP VAR');
    OP_DESTROY_LOCAL_VAR := - Add('DESTROY LOCAL VAR');
    OP_INIT_SUB := - Add('INIT SUB');
    OP_JUMP_SUB := - Add('JUMP SUB');
    OP_END_SUB := - Add('END SUB');
    OP_FIN_SUB := - Add('FIN SUB');
    OP_EPILOGUE_SUB := - Add('EPILOGUE SUB');

    OP_BEGIN_GLOBAL_BLOCK := - Add('BEGIN GLOBAL BLOCK');
    OP_EPILOGUE_GLOBAL_BLOCK := - Add('EPILOGUE_GLOBAL_BLOCK');
    OP_EPILOGUE_GLOBAL_BLOCK2 := - Add('EPILOGUE_GLOBAL_BLOCK2');
    OP_END_GLOBAL_BLOCK := - Add('END GLOBAL BLOCK');

    OP_ABSOLUTE := - Add('ABSOLUTE');

    OP_ASSIGN_TYPE := - Add('ASSIGN TYPE');
    OP_DETERMINE_TYPE := - Add('DETERMINE TYPE');
    OP_ASSIGN_THE_SAME_TYPE := - Add('ASSIGN THE SAME TYPE');
    OP_ASSIGN_TYPE_ALIAS := - Add('ASSIGN TYPE ALIAS');
    OP_ASSIGN_LAMBDA_TYPES := - Add('ASSIGN LAMBDA TYPES');

    OP_SAVE_EDX := - Add('SAVE EDX');
    OP_RESTORE_EDX := - Add('RESTORE EDX');

    OP_BEGIN_WITH := - Add('BEGIN WITH');
    OP_END_WITH := - Add('END WITH');

    OP_BEGIN_INIT_CONST := - Add('BEGIN INIT CONST');
    OP_END_INIT_CONST := - Add('END INIT CONST');

    OP_CREATE_POINTER_TYPE := - Add('CREATE POINTER TYPE');
    OP_CREATE_CLASSREF_TYPE := - Add('CREATE CLASSREF TYPE');
    OP_ADDRESS := - Add('ADDRESS');
    OP_TERMINAL := - Add('TERMINAL');
    OP_ADDRESS_PROG := - Add('ADDRESS PROG');
    OP_ASSIGN_PROG := - Add('ASSIGN PROG');

    OP_LVALUE := - Add('LVALUE');
    OP_POSTFIX_EXPRESSION := - Add('POSTFIX EXPRESSION');

    OP_ASSIGN := - Add(':=');
    OP_ASSIGN_CONST := - Add(':= (const)');
    OP_ASSIGN_ENUM := - Add(':= (enum)');
    OP_CHECK_SUBRANGE_TYPE := - Add('CHECK SUBRANGE TYPE');

    OP_CREATE_DYNAMIC_ARRAY_TYPE := - Add('CREATE DYNARRAY TYPE');

    OP_CREATE_SHORTSTRING_TYPE := - Add('CREATE SHORTSTRING TYPE');

    OP_INC := - Add('INC');
    OP_DEC := - Add('DEC');
    OP_PRED := - Add('PRED');
    OP_SUCC := - Add('SUCC');
    OP_ORD := - Add('ORD');
    OP_CHR := - Add('CHR');
    OP_STR := - Add('STR');
    OP_LOW := - Add('LOW');
    OP_HIGH := - Add('HIGH');

    OP_SET_LENGTH := - Add('SET LENGTH');
    OP_SET_LENGTH_EX := - Add('SET LENGTH EX');
    OP_PUSH_LENGTH := - Add('PUSH LENGTH');
    OP_DYNARRAY_ASSIGN := - Add(':= (dynarray)');
    OP_DYNARRAY_CLR := - Add('CLR (dynarray)');
    OP_DYNARRAY_HIGH := - Add('HIGH (dynarray)');
    OP_CREATE_EMPTY_DYNARRAY := - Add('CREATE EMPTY dynarray');

    OP_SHORTSTRING_HIGH := - Add('HIGH (shortstring)');

    OP_EXPORTS := - Add('EXPORTS');

    OP_PLUS := - Add('+');
    OP_MINUS := - Add('-');
    OP_MULT := - Add('*');
    OP_DIV := - Add('/');
    OP_IDIV := - Add('DIV');
    OP_MOD := - Add('MOD');
    OP_SHL := - Add('SHL');
    OP_SHR := - Add('SHR');

    OP_AND := - Add('AND');
    OP_OR := - Add('OR');
    OP_XOR := - Add('XOR');
    OP_NOT := - Add('NOT');

    OP_NEG := - Add('NEG');
    OP_POSITIVE := - Add('POSITIVE');
    OP_ABS := - Add('ABS');

    OP_EQ := - Add('=');
    OP_NE := - Add('<>');
    OP_GT := - Add('>');
    OP_GE := - Add('>=');
    OP_LT := - Add('<');
    OP_LE := - Add('<=');

    OP_CLASSNAME := - Add('CLASSNAME');

    OP_GET_PROG := - Add('GET_PROG');

    OP_IS := - Add('IS');
    OP_AS := - Add('AS');
    OP_TYPEINFO := - Add('TYPEINFO');
    OP_ADD_TYPEINFO := - Add('ADD_TYPEINFO');
    OP_INSTANCE_OF := - Add('INSTANCE OF');

    OP_RET := - Add('RET');

    OP_VAR_FROM_TVALUE := - Add('VAR FROM TVALUE');

    OP_CURRENCY_FROM_INT64 := - Add('CURRENCY FROM INT64');
    OP_CURRENCY_FROM_UINT64 := - Add('CURRENCY FROM UINT64');
    OP_CURRENCY_FROM_INT := - Add('CURRENCY FROM INT');
    OP_CURRENCY_FROM_REAL := - Add('CURRENCY FROM REAL');

    OP_INT_TO_DOUBLE := - Add('INT TO DOUBLE');
    OP_INT64_TO_DOUBLE := - Add('INT64 TO DOUBLE');
    OP_UINT64_TO_DOUBLE := - Add('UINT64 TO DOUBLE');

    OP_INT_TO_SINGLE := - Add('INT TO SINGLE');
    OP_INT64_TO_SINGLE := - Add('INT64 TO SINGLE');
    OP_UINT64_TO_SINGLE := - Add('UINT64 TO SINGLE');

    OP_INT_TO_EXTENDED := - Add('INT TO EXTENDED');
    OP_INT64_TO_EXTENDED := - Add('INT64 TO EXTENDED');
    OP_UINT64_TO_EXTENDED := - Add('UINT64 TO EXTENDED');

    OP_INT_TO_INT64 := - Add('INT TO INT64');
    OP_BYTE_TO_INT64 := - Add('BYTE TO INT64');
    OP_WORD_TO_INT64 := - Add('WORD TO INT64');
    OP_CARDINAL_TO_INT64 := - Add('CARDINAL TO INT64');
    OP_SMALLINT_TO_INT64 := - Add('SMALLINT TO INT64');
    OP_SHORTINT_TO_INT64 := - Add('SHORTINT TO INT64');

    OP_INT_FROM_INT64 := - Add('INT FROM INT64');
    OP_BYTE_FROM_INT64 := - Add('BYTE FROM INT64');
    OP_WORD_FROM_INT64 := - Add('WORD FROM INT64');
    OP_CARDINAL_FROM_INT64 := - Add('CARDINAL FROM INT64');
    OP_SMALLINT_FROM_INT64 := - Add('SMALLINT FROM INT64');
    OP_SHORTINT_FROM_INT64 := - Add('SHORTINT FROM INT64');

    OP_INT_TO_UINT64 := - Add('INT TO UINT64');
    OP_BYTE_TO_UINT64 := - Add('BYTE TO UINT64');
    OP_WORD_TO_UINT64 := - Add('WORD TO UINT64');
    OP_CARDINAL_TO_UINT64 := - Add('CARDINAL TO UINT64');
    OP_SMALLINT_TO_UINT64 := - Add('SMALLINT TO UINT64');
    OP_SHORTINT_TO_UINT64 := - Add('SHORTINT TO UINT64');

    OP_INT_FROM_UINT64 := - Add('INT FROM UINT64');
    OP_BYTE_FROM_UINT64 := - Add('BYTE FROM UINT64');
    OP_WORD_FROM_UINT64 := - Add('WORD FROM UINT64');
    OP_CARDINAL_FROM_UINT64 := - Add('CARDINAL FROM UINT64');
    OP_SMALLINT_FROM_UINT64 := - Add('SMALLINT FROM UINT64');
    OP_SHORTINT_FROM_UINT64 := - Add('SHORTINT FROM UINT64');

    OP_CURRENCY_TO_EXTENDED := - Add('CURRENCY TO EXTENDED');
    OP_CURRENCY_TO_SINGLE := - Add('CURRENCY TO SINGLE');
    OP_DOUBLE_TO_SINGLE := - Add('DOUBLE TO SINGLE');
    OP_DOUBLE_TO_EXTENDED := - Add('DOUBLE TO EXTENDED');
    OP_SINGLE_TO_DOUBLE := - Add('SINGLE TO DOUBLE');
    OP_CURRENCY_TO_DOUBLE := - Add('CURRENCY TO DOUBLE');
    OP_SINGLE_TO_EXTENDED := - Add('SINGLE TO EXTENDED');
    OP_EXTENDED_TO_DOUBLE := - Add('EXTENDED TO DOUBLE');
    OP_EXTENDED_TO_SINGLE := - Add('EXTENDED TO SINGLE');

    OP_PUSH_EBP := -Add('push ebp');
    OP_POP := -Add('pop');

    OP_FIELD := - Add('FIELD');
    OP_ELEM := - Add('ELEM');

    OP_ITEM := - Add('ITEM');
    OP_RECORD_ITEM := - Add('RECORD ITEM');

    OP_PRINT := - Add('PRINT');
    OP_PRINT_EX := - Add('PRINT_EX');

    OP_PRINT_KWD := - Add('PRINT KWD');
    OP_PRINTLN_KWD := - Add('PRINTLN KWD');

    OP_SET_INCLUDE := - Add('SET INCLUDE');
    OP_SET_INCLUDE_INTERVAL := - Add('SET INCLUDE INTERVAL');
    OP_SET_EXCLUDE := - Add('SET EXCLUDE');
    OP_SET_MEMBERSHIP := -Add('SET MEMBERSHIP');
{$IFNDEF PAXARM}
    OP_INIT_PANSICHAR_LITERAL := - Add('INIT PANSICHAR LITERAL');
{$ENDIF}
    OP_INIT_PWIDECHAR_LITERAL := - Add('INIT PWIDECHAR LITERAL');

    OP_SIZEOF := - Add('SIZEOF');

    OP_SET_READ_ID := - Add('SET READ ID');
    OP_SET_WRITE_ID := - Add('SET WRITE ID');

    OP_OLE_GET := - Add('OLE_GET');
    OP_OLE_SET := - Add('OLE_SET');
    OP_OLE_VALUE := - Add('OLE_VALUE');
    OP_OLE_PARAM := - Add('OLE_PARAM');

    OP_PARAM_CHANGED := - Add('PARAM_CHANGED');

    OP_ONCREATE_OBJECT := - Add('ON CREATE OBJECT');
    OP_ON_AFTER_OBJECT_CREATION := - Add('ON AFTER OBJECT CREATION');
    OP_CREATE_OBJECT := - Add('CREATE OBJECT');
    OP_DESTROY_OBJECT := - Add('DESTROY OBJECT');
    OP_GET_VMT_ADDRESS := - Add('GET VMT ADDRESS');
    OP_ADD_ANCESTOR := - Add('ADD ANCESTOR');
    OP_ADD_INTERFACE := - Add('ADD INTERFACE');
    OP_ADD_METHOD_INDEX := - Add('ADD METHOD INDEX');
    OP_ASSIGNED := - Add('ASSIGNED');

    OP_ONCREATE_HOST_OBJECT := - Add('ON CREATE HOST_OBJECT');
    OP_ONDESTROY_HOST_OBJECT := - Add('ON DESTROY HOST OBJECT');

    OP_BEFORE_CALL_HOST := - Add('ON BEFORE CALL HOST');
    OP_AFTER_CALL_HOST := - Add('ON AFTER CALL HOST');

    OP_SET_SET_PROP := -Add('SET SET PROP');
    OP_SET_ORD_PROP := -Add('SET ORD PROP');
    OP_SET_INTERFACE_PROP := -Add('SET INTERFACE PROP');
{$IFNDEF PAXARM}
    OP_SET_ANSISTR_PROP := -Add('SET ANSISTR PROP');
    OP_SET_WIDESTR_PROP := -Add('SET WIDESTR PROP');
{$ENDIF}
    OP_SET_UNICSTR_PROP := -Add('SET UNICSTR PROP');
    OP_SET_FLOAT_PROP := -Add('SET FLOAT PROP');
    OP_SET_VARIANT_PROP := -Add('SET VARIANT PROP');
    OP_SET_INT64_PROP := -Add('SET INT64 PROP');

    OP_SET_EVENT_PROP := -Add('SET EVENT PROP');
    OP_SET_EVENT_PROP2 := -Add('SET EVENT PROP2');

    OP_VARARRAY_GET := -Add('VARARRAY GET');
    OP_VARARRAY_PUT := -Add('VARARRAY PUT');
    OP_VARARRAY_IDX := -Add('VARARRAY IDX');

    OP_SAVE_REGS := - Add('SAVE REGS');
    OP_RESTORE_REGS := - Add('RESTORE REGS');

    OP_ERR_ABSTRACT := - Add('ERR ABSTRACT');
    OP_UPDATE_DEFAULT_CONSTRUCTOR := - Add('UPDATE DEFAULT CONSTRUCTOR');
    OP_FIND_CONSTRUCTOR := - Add('FIND CONSTRUCTOR');

    OP_BEGIN_CRT_JS_FUNC_OBJECT := - Add('BEGIN_CRT_JS_FUNC_OBJECT');
    OP_END_CRT_JS_FUNC_OBJECT := - Add('END_CRT_JS_FUNC_OBJECT');

    OP_TO_JS_OBJECT := - Add('TO_JS_OBJECT');
    OP_JS_TYPEOF := - Add('JS_TYPEOF');
    OP_JS_VOID := - Add('JS_VOID');
    OP_JS_DELETE := - Add('JS_DELETE');

    OP_TO_FW_OBJECT := - Add('TO_FW_OBJECT');

    OP_ASSIGN_SHIFT := -Add('ASSIGN SHIFT');

    OP_ASSIGN_INT_M := -Add(':= (integer, m)');

    OP_CREATE_METHOD := -Add('CREATE METHOD');

    OP_GET_ENUMERATOR := -Add('GET ENUMERATOR');
    OP_MOVE_NEXT := -Add('MOVE NEXT');
    OP_CURRENT := -Add('CURRENT');
    OP_LOCK_VARRAY := -Add('LOCK VARRAY');
    OP_UNLOCK_VARRAY := -Add('UNLOCK VARRAY');

/////////////////// DETAILED OPERATORS /////////////////////////////////

//    OP_DUMMY := - Add('DUMMY');

    OP_ASSIGN_BYTE_I := -Add(':= (byte, i)');
    OP_ASSIGN_BYTE_M := -Add(':= (byte, m)');
    OP_ASSIGN_WORD_I := -Add(':= (word, i)');
    OP_ASSIGN_WORD_M := -Add(':= (word, m)');
    OP_ASSIGN_CARDINAL_I := -Add(':= (cardinal, i)');
    OP_ASSIGN_CARDINAL_M := -Add(':= (cardinal, m)');
    OP_ASSIGN_SMALLINT_I := -Add(':= (smallint, i)');
    OP_ASSIGN_SMALLINT_M := -Add(':= (smallint, m)');
    OP_ASSIGN_SHORTINT_I := -Add(':= (shortint, i)');
    OP_ASSIGN_SHORTINT_M := -Add(':= (shortint, m)');
    OP_ASSIGN_INT_I := -Add(':= (integer, i)');
//    OP_ASSIGN_INT_M := -Add(':= (integer, m)');
    OP_ASSIGN_DOUBLE := -Add(':= (double)');
    OP_ASSIGN_CURRENCY := -Add(':= (currency)');
    OP_ASSIGN_EVENT := -Add(':= (event)');
    OP_ASSIGN_SINGLE := -Add(':= (single)');
    OP_ASSIGN_EXTENDED := -Add(':= (extended)');
{$IFNDEF PAXARM}
    OP_ASSIGN_PANSICHAR := -Add(':= (pansichar)');
{$ENDIF}
    OP_ASSIGN_PWIDECHAR := -Add(':= (pwidechar)');
    OP_ASSIGN_INT64 := -Add(':= (int64)');
    OP_ASSIGN_UINT64 := -Add(':= (uint64)');
    OP_ASSIGN_INTERFACE := -Add(':= (interface)');

    OP_CREATE_EVENT := -Add('create event');

    OP_MULT_INT64 := -Add('* (int64)');
    OP_IDIV_INT64 := -Add('div (int64)');
    OP_MOD_INT64 := -Add('mod (int64)');
    OP_SHL_INT64 := -Add('shl (int64)');
    OP_SHR_INT64 := -Add('shr (int64)');

{$IFNDEF PAXARM}
    OP_ANSISTRING_FROM_PANSICHAR := -Add('ANSISTRING FROM PANSICHAR');
    OP_ANSISTRING_FROM_PWIDECHAR := -Add('ANSISTRING FROM PWIDECHAR');
    OP_ANSISTRING_FROM_ANSICHAR := -Add('ANSISTRING FROM ANSICHAR');
    OP_ASSIGN_ANSISTRING := -Add(':= (ansistring)');
    OP_ASSIGN_SHORTSTRING := -Add(':= (shortstring)');
    OP_ASSIGN_WIDESTRING := -Add(':= (widestring)');
{$ENDIF}
    OP_ASSIGN_UNICSTRING := -Add(':= (unicstring)');
    OP_ASSIGN_VARIANT := -Add(':= (variant)');
    OP_ASSIGN_OLEVARIANT := -Add(':= (olevariant)');

    OP_ASSIGN_CLASS := -Add(':= (class)');

    OP_ASSIGN_TVarRec := -Add(':= (TVarRec)');

    OP_ASSIGN_RECORD := -Add(':= (record)');
    OP_ASSIGN_ARRAY := -Add(':= (array)');
{$IFNDEF PAXARM}
    OP_SHORTSTRING_FROM_PANSICHAR_LITERAL := -Add('SHORTSTRING FROM PANSICHAR LITERAL');
    OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL := -Add('SHORTSTRING FROM PWIDECHAR LITERAL');
    OP_SHORTSTRING_FROM_ANSICHAR := -Add('SHORTSTRING FROM ANSICHAR');
    OP_SHORTSTRING_FROM_WIDECHAR := -Add('SHORTSTRING FROM WIDECHAR');
    OP_SHORTSTRING_FROM_ANSISTRING := -Add('SHORTSTRING FROM ANSISTRING');
    OP_SHORTSTRING_FROM_WIDESTRING := -Add('SHORTSTRING FROM WIDESTRING');
    OP_UNICSTRING_FROM_WIDESTRING := -Add('UNICSTRING FROM WIDESTRING');
    OP_SHORTSTRING_FROM_UNICSTRING := -Add('SHORTSTRING FROM UNICSTRING');
    OP_ANSISTRING_FROM_SHORTSTRING := -Add('ANSISTRING FROM SHORTSTRING');

    OP_WIDESTRING_FROM_PANSICHAR_LITERAL := -Add('WIDESTRING FROM PANSICHAR LITERAL');
    OP_WIDESTRING_FROM_PWIDECHAR_LITERAL := -Add('WIDESTRING FROM PWIDECHAR LITERAL');
    OP_WIDESTRING_FROM_ANSICHAR := -Add('WIDESTRING FROM ANSICHAR');
    OP_WIDESTRING_FROM_WIDECHAR := -Add('WIDESTRING FROM WIDECHAR');
    OP_ANSISTRING_FROM_WIDECHAR := -Add('ANSISTRING FROM WIDECHAR');
    OP_WIDESTRING_FROM_WIDECHAR_LITERAL := -Add('WIDESTRING FROM WIDECHAR LITERAL');
    OP_WIDESTRING_FROM_ANSISTRING := -Add('WIDESTRING FROM ANSISTRING');
    OP_UNICSTRING_FROM_ANSISTRING := -Add('UNICSTRING FROM ANSISTRING');
    OP_WIDESTRING_FROM_SHORTSTRING := -Add('WIDESTRING FROM SHORTSTRING');
    OP_WIDESTRING_FROM_UNICSTRING := -Add('WIDESTRING FROM UNICSTRING');
    OP_UNICSTRING_FROM_SHORTSTRING := -Add('UNICSTRING FROM SHORTSTRING');
    OP_ANSISTRING_FROM_WIDESTRING := -Add('ANSISTRING FROM WIDESTRING');
    OP_ANSISTRING_FROM_UNICSTRING := -Add('ANSISTRING FROM UNICSTRING');

    OP_UNICSTRING_FROM_PANSICHAR_LITERAL := -Add('UNICSTRING FROM PANSICHAR LITERAL');
    OP_UNICSTRING_FROM_ANSICHAR := -Add('UNICSTRING FROM ANSICHAR');
{$ENDIF}
    OP_UNICSTRING_FROM_PWIDECHAR_LITERAL := -Add('UNICSTRING FROM PWIDECHAR LITERAL');
    OP_UNICSTRING_FROM_WIDECHAR := -Add('UNICSTRING FROM WIDECHAR');
    OP_UNICSTRING_FROM_WIDECHAR_LITERAL := -Add('UNICSTRING FROM WIDECHAR LITERAL');

    OP_VARIANT_FROM_CLASS := -Add('VARIANT FROM CLASS'); // JS only
    OP_VARIANT_FROM_POINTER := -Add('VARIANT FROM POINTER'); // JS only
    OP_CLASS_FROM_VARIANT := -Add('CLASS FROM VARIANT'); // JS only

    OP_INTERFACE_FROM_CLASS := -Add('INTERFACE FROM CLASS');
    OP_INTERFACE_CAST := -Add('INTERFACE CAST');
{$IFNDEF PAXARM}
    OP_VARIANT_FROM_PANSICHAR_LITERAL := -Add('VARIANT FROM PANSICHAR LITERAL');
    OP_VARIANT_FROM_ANSISTRING := -Add('VARIANT FROM ANSISTRING');
    OP_VARIANT_FROM_WIDESTRING := -Add('VARIANT FROM WIDESTRING');
    OP_VARIANT_FROM_SHORTSTRING := -Add('VARIANT FROM SHORTSTRING');
    OP_VARIANT_FROM_ANSICHAR := -Add('VARIANT FROM ANSICHAR');
{$ENDIF}
    OP_VARIANT_FROM_UNICSTRING := -Add('VARIANT FROM UNICSTRING');
    OP_VARIANT_FROM_PWIDECHAR_LITERAL := -Add('VARIANT FROM PWIDECHAR LITERAL');
    OP_VARIANT_FROM_WIDECHAR := -Add('VARIANT FROM WIDECHAR');
    OP_VARIANT_FROM_WIDECHAR_LITERAL := -Add('VARIANT FROM WIDECHAR LITERAL');
    OP_VARIANT_FROM_INT := -Add('VARIANT FROM INT');
    OP_VARIANT_FROM_INT64 := -Add('VARIANT FROM INT64');
    OP_VARIANT_FROM_BYTE := -Add('VARIANT FROM BYTE');
    OP_VARIANT_FROM_BOOL := -Add('VARIANT FROM BOOL');
    OP_VARIANT_FROM_WORD := -Add('VARIANT FROM WORD');
    OP_VARIANT_FROM_CARDINAL := -Add('VARIANT FROM CARDINAL');
    OP_VARIANT_FROM_SMALLINT := -Add('VARIANT FROM SMALLINT');
    OP_VARIANT_FROM_SHORTINT := -Add('VARIANT FROM SHORTINT');
    OP_VARIANT_FROM_DOUBLE := -Add('VARIANT FROM DOUBLE');
    OP_VARIANT_FROM_CURRENCY := -Add('VARIANT FROM CURRENCY');
    OP_VARIANT_FROM_SINGLE := -Add('VARIANT FROM SINGLE');
    OP_VARIANT_FROM_EXTENDED := -Add('VARIANT FROM EXTENDED');
    OP_VARIANT_FROM_INTERFACE := -Add('VARIANT FROM INTERFACE');

    OP_OLEVARIANT_FROM_VARIANT := -Add('OLEVARIANT FROM VARIANT');
{$IFNDEF PAXARM}
    OP_OLEVARIANT_FROM_PANSICHAR_LITERAL := -Add('OLEVARIANT FROM PANSICHAR LITERAL');
    OP_OLEVARIANT_FROM_ANSISTRING := -Add('OLEVARIANT FROM ANSISTRING');
    OP_OLEVARIANT_FROM_WIDESTRING := -Add('OLEVARIANT FROM WIDESTRING');
    OP_OLEVARIANT_FROM_UNICSTRING := -Add('OLEVARIANT FROM UNICSTRING');
    OP_OLEVARIANT_FROM_SHORTSTRING := -Add('OLEVARIANT FROM SHORTSTRING');
    OP_OLEVARIANT_FROM_ANSICHAR := -Add('OLEVARIANT FROM ANSICHAR');
{$ENDIF}
    OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL := -Add('OLEVARIANT FROM PWIDECHAR LITERAL');
    OP_OLEVARIANT_FROM_WIDECHAR := -Add('OLEVARIANT FROM WIDECHAR');
    OP_OLEVARIANT_FROM_WIDECHAR_LITERAL := -Add('OLEVARIANT FROM WIDECHAR LITERAL');
    OP_OLEVARIANT_FROM_INT := -Add('OLEVARIANT FROM INT');
    OP_OLEVARIANT_FROM_INT64 := -Add('OLEVARIANT FROM INT64');
    OP_OLEVARIANT_FROM_BYTE := -Add('OLEVARIANT FROM BYTE');
    OP_OLEVARIANT_FROM_BOOL := -Add('OLEVARIANT FROM BOOL');
    OP_OLEVARIANT_FROM_WORD := -Add('OLEVARIANT FROM WORD');
    OP_OLEVARIANT_FROM_CARDINAL := -Add('OLEVARIANT FROM CARDINAL');
    OP_OLEVARIANT_FROM_SMALLINT := -Add('OLEVARIANT FROM SMALLINT');
    OP_OLEVARIANT_FROM_SHORTINT := -Add('OLEVARIANT FROM SHORTINT');
    OP_OLEVARIANT_FROM_DOUBLE := -Add('OLEVARIANT FROM DOUBLE');
    OP_OLEVARIANT_FROM_CURRENCY := -Add('OLEVARIANT FROM CURRENCY');
    OP_OLEVARIANT_FROM_SINGLE := -Add('OLEVARIANT FROM SINGLE');
    OP_OLEVARIANT_FROM_EXTENDED := -Add('OLEVARIANT FROM EXTENDED');
    OP_OLEVARIANT_FROM_INTERFACE := -Add('OLEVARIANT FROM INTERFACE');
{$IFNDEF PAXARM}
    OP_ANSISTRING_FROM_INT := -Add('ANSISTRING FROM INT'); // JS only
    OP_ANSISTRING_FROM_DOUBLE := -Add('ANSISTRING FROM DOUBLE'); // JS only
    OP_ANSISTRING_FROM_SINGLE := -Add('ANSISTRING FROM SINGLE'); // JS only
    OP_ANSISTRING_FROM_EXTENDED := -Add('ANSISTRING FROM EXTENDED'); // JS only
    OP_ANSISTRING_FROM_BOOLEAN := -Add('ANSISTRING FROM BOOLEAN'); // JS only
{$ENDIF}
    OP_UNICSTRING_FROM_INT := -Add('UNICSTRING FROM INT'); // JS only
    OP_UNICSTRING_FROM_DOUBLE := -Add('UNICSTRING FROM DOUBLE'); // JS only
    OP_UNICSTRING_FROM_SINGLE := -Add('UNICSTRING FROM SINGLE'); // JS only
    OP_UNICSTRING_FROM_EXTENDED := -Add('UNICSTRING FROM EXTENDED'); // JS only
    OP_UNICSTRING_FROM_BOOLEAN := -Add('UNICSTRING FROM BOOLEAN'); // JS only


    OP_JS_FUNC_OBJ_FROM_VARIANT := -Add('JS FUNC OBJ FROM VARIANT'); // JS only
{$IFNDEF PAXARM}
    OP_ANSICHAR_FROM_VARIANT := -Add('ANSICHAR FROM VARIANT');
    OP_ANSISTRING_FROM_VARIANT := -Add('ANSISTRING FROM VARIANT');
    OP_WIDESTRING_FROM_VARIANT := -Add('WIDESTRING FROM VARIANT');
    OP_SHORTSTRING_FROM_VARIANT := -Add('SHORTSTRING FROM VARIANT');
{$ENDIF}
    OP_UNICSTRING_FROM_VARIANT := -Add('UNICSTRING FROM VARIANT');
    OP_WIDECHAR_FROM_VARIANT := -Add('WIDECHAR FROM VARIANT');
    OP_DOUBLE_FROM_VARIANT := -Add('DOUBLE FROM VARIANT');
    OP_CURRENCY_FROM_VARIANT := -Add('CURRENCY FROM VARIANT');
    OP_SINGLE_FROM_VARIANT := -Add('SINGLE FROM VARIANT');
    OP_EXTENDED_FROM_VARIANT := -Add('EXTENDED FROM VARIANT');
    OP_INT64_FROM_VARIANT := -Add('INT64 FROM VARIANT');
    OP_UINT64_FROM_VARIANT := -Add('UINT64 FROM VARIANT');
    OP_INT_FROM_VARIANT := -Add('INT FROM VARIANT');
    OP_BYTE_FROM_VARIANT := -Add('BYTE FROM VARIANT');
    OP_WORD_FROM_VARIANT := -Add('WORD FROM VARIANT');
    OP_CARDINAL_FROM_VARIANT := -Add('CARDINAL FROM VARIANT');
    OP_BOOL_FROM_VARIANT := -Add('BOOL FROM VARIANT');
    OP_BYTEBOOL_FROM_VARIANT := -Add('BYTEBOOL FROM VARIANT');
    OP_WORDBOOL_FROM_VARIANT := -Add('WORDBOOL FROM VARIANT');
    OP_LONGBOOL_FROM_VARIANT := -Add('LONGBOOL FROM VARIANT');
    OP_SMALLINT_FROM_VARIANT := -Add('SMALLINT FROM VARIANT');
    OP_SHORTINT_FROM_VARIANT := -Add('SHORTINT FROM VARIANT');

    OP_BOOL_FROM_BYTEBOOL := -Add('BOOL FROM BYTEBOOL');
    OP_BOOL_FROM_WORDBOOL := -Add('BOOL FROM WORDBOOL');
    OP_BOOL_FROM_LONGBOOL := -Add('BOOL FROM LONGBOOL');

    OP_NOT_BOOL := -Add('not (boolean)');
    OP_NOT_BYTEBOOL := -Add('not (bytebool)');
    OP_NOT_WORDBOOL := -Add('not (wordbool)');
    OP_NOT_LONGBOOL := -Add('not (longbool)');

    OP_NOT_VARIANT := -Add('not (variant)');
    OP_NEG_VARIANT := -Add('neg (variant)');
    OP_ADD_VARIANT := -Add('+ (variant)');
    OP_SUB_VARIANT := -Add('- (variant)');
    OP_MULT_VARIANT := -Add('* (variant)');
    OP_DIV_VARIANT := -Add('/ (variant)');
    OP_IDIV_VARIANT := -Add('div (variant)');
    OP_MOD_VARIANT := -Add('mod (variant)');
    OP_SHL_VARIANT := -Add('shl (variant)');
    OP_SHR_VARIANT := -Add('shr (variant)');
    OP_AND_VARIANT := -Add('and (variant)');
    OP_OR_VARIANT := -Add('or (variant)');
    OP_XOR_VARIANT := -Add('xor (variant)');
    OP_LT_VARIANT := -Add('< (variant)');
    OP_LE_VARIANT := -Add('<= (variant)');
    OP_GT_VARIANT := -Add('> (variant)');
    OP_GE_VARIANT := -Add('>= (variant)');
    OP_EQ_VARIANT := -Add('= (variant)');
    OP_NE_VARIANT := -Add('<> (variant)');

    OP_EQ_EVENT := -Add('= (event)');
    OP_NE_EVENT := -Add('<> (event)');

    OP_VARIANT_CLR := -Add('VARIANT CLR');
{$IFNDEF PAXARM}
    OP_ADD_ANSISTRING := -Add('+ (ansistring)');
    OP_ADD_SHORTSTRING := -Add('+ (shortstring)');
    OP_ADD_WIDESTRING := -Add('+ (widestring)');
{$ENDIF}
    OP_ADD_UNICSTRING := -Add('+ (unicstring)');

    OP_EQ_STRUCT := -Add('= (struct)');
    OP_NE_STRUCT := -Add('<> (struct)');
{$IFNDEF PAXARM}
    OP_EQ_ANSISTRING := -Add('= (ansistring)');
    OP_EQ_SHORTSTRING := -Add('= (shortstring)');
    OP_EQ_WIDESTRING := -Add('= (widestring)');
{$ENDIF}
    OP_EQ_UNICSTRING := -Add('= (unicstring)');
{$IFNDEF PAXARM}
    OP_NE_ANSISTRING := -Add('<> (ansistring)');
    OP_NE_SHORTSTRING := -Add('<> (shortstring)');
    OP_NE_WIDESTRING := -Add('<> (widestring)');
{$ENDIF}
    OP_NE_UNICSTRING := -Add('<> (unicstring)');

{$IFNDEF PAXARM}
    OP_GT_ANSISTRING := -Add('> (ansistring)');
    OP_GE_ANSISTRING := -Add('>= (ansistring)');
    OP_LT_ANSISTRING := -Add('< (ansistring)');
    OP_LE_ANSISTRING := -Add('<= (ansistring)');

    OP_GT_SHORTSTRING := -Add('> (shortstring)');
    OP_GE_SHORTSTRING := -Add('>= (shortstring)');
    OP_LT_SHORTSTRING := -Add('< (shortstring)');
    OP_LE_SHORTSTRING := -Add('<= (shortstring)');

    OP_GT_WIDESTRING := -Add('> (widestring)');
    OP_GE_WIDESTRING := -Add('>= (widestring)');
    OP_LT_WIDESTRING := -Add('< (widestring)');
    OP_LE_WIDESTRING := -Add('<= (widestring)');
{$ENDIF}
    OP_GT_UNICSTRING := -Add('> (unicstring)');
    OP_GE_UNICSTRING := -Add('>= (unicstring)');
    OP_LT_UNICSTRING := -Add('< (unicstring)');
    OP_LE_UNICSTRING := -Add('<= (unicstring)');

{$IFNDEF PAXARM}
    OP_ANSISTRING_CLR := -Add('ANSISTRING CLR');
    OP_WIDESTRING_CLR := -Add('WIDESTRING CLR');
{$ENDIF}
    OP_UNICSTRING_CLR := -Add('UNICSTRING CLR');
    OP_STRUCTURE_CLR := -Add('STRUCTURE CLR');
    OP_INTERFACE_CLR := -Add('INTERFACE CLR');
    OP_CLASS_CLR := -Add('CLASS CLR');

    OP_STRUCTURE_ADDREF := -Add('STRUCTURE ADDREF');
    OP_ADDREF := -Add('ADDREF');

    OP_ADD_INT_MI := -Add('+ (integer, mi)');
    OP_ADD_INT_MM := -Add('+ (integer, mm)');

    OP_SUB_INT_MI := -Add('- (integer, mi)');
    OP_SUB_INT_MM := -Add('- (integer, mm)');

    OP_IMUL_INT_MI := -Add('imul (integer, mi)');
    OP_IMUL_INT_MM := -Add('imul (integer, mm)');

    OP_IDIV_INT_MI := -Add('idiv (integer, mi)');
    OP_IDIV_INT_MM := -Add('idiv (integer, mm)');
    OP_IDIV_INT_IM := -Add('idiv (integer, im)');

    OP_MOD_INT_MI := -Add('mod (integer, mi)');
    OP_MOD_INT_MM := -Add('mod (integer, mm)');
    OP_MOD_INT_IM := -Add('mod (integer, im)');

    OP_SHL_INT_MI := -Add('shl (integer, mi)');
    OP_SHL_INT_MM := -Add('shl (integer, mm)');
    OP_SHL_INT_IM := -Add('shl (integer, im)');

    OP_SHR_INT_MI := -Add('shr (integer, mi)');
    OP_SHR_INT_MM := -Add('shr (integer, mm)');
    OP_SHR_INT_IM := -Add('shr (integer, im)');

    OP_AND_INT_MI := -Add('and (integer, mi)');
    OP_AND_INT_MM := -Add('and (integer, mm)');

    OP_OR_INT_MI := -Add('or (integer, mi)');
    OP_OR_INT_MM := -Add('or (integer, mm)');

    OP_XOR_INT_MI := -Add('xor (integer, mi)');
    OP_XOR_INT_MM := -Add('xor (integer, mm)');

    OP_NEG_INT := -Add('NEG (integer)');
    OP_NEG_INT64 := -Add('NEG64 (integer)');
    OP_NEG_UINT64 := -Add('NEGU64 (integer)');

    OP_ABS_INT := -Add('ABS (integer)');
    OP_ABS_INT64 := -Add('ABS (int64)');
    OP_ABS_DOUBLE := -Add('ABS (double)');
    OP_ABS_SINGLE := -Add('ABS (single)');
    OP_ABS_EXTENDED := -Add('ABS (extended)');
    OP_ABS_CURRENCY := -Add('ABS (currency)');
    OP_ABS_VARIANT := -Add('ABS (variant)');

    OP_LT_INT_MI := -Add('< (integer, mi)');
    OP_LT_INT_MM := -Add('< (integer, mm)');

    OP_LE_INT_MI := -Add('<= (integer, mi)');
    OP_LE_INT_MM := -Add('<= (integer, mm)');

    OP_GT_INT_MI := -Add('> (integer, mi)');
    OP_GT_INT_MM := -Add('> (integer, mm)');

    OP_GE_INT_MI := -Add('>= (integer, mi)');
    OP_GE_INT_MM := -Add('>= (integer, mm)');

    OP_EQ_INT_MI := -Add('= (integer, mi)');
    OP_EQ_INT_MM := -Add('= (integer, mm)');

    OP_NE_INT_MI := -Add('<> (integer, mi)');
    OP_NE_INT_MM := -Add('<> (integer, mm)');

    OP_ADD_INT64 := -Add('+ (int64)');
    OP_SUB_INT64 := -Add('- (int64)');
    OP_AND_INT64 := -Add('AND (int64)');
    OP_OR_INT64 := -Add('OR (int64)');
    OP_XOR_INT64 := -Add('XOR (int64)');

    OP_ADD_UINT64 := -Add('+ (uint64)');
    OP_SUB_UINT64 := -Add('- (uint64)');
    OP_AND_UINT64 := -Add('AND (uint64)');
    OP_OR_UINT64 := -Add('OR (uint64)');
    OP_XOR_UINT64 := -Add('XOR (uint64)');

    OP_LT_INT64 := -Add('< (int64)');
    OP_LE_INT64 := -Add('<= (int64)');
    OP_GT_INT64 := -Add('> (int64)');
    OP_GE_INT64 := -Add('>= (int64)');
    OP_EQ_INT64 := -Add('= (int64)');
    OP_NE_INT64 := -Add('<> (int64)');

    OP_LT_UINT64 := -Add('< (uint64)');
    OP_LE_UINT64 := -Add('<= (uint64)');
    OP_GT_UINT64 := -Add('> (uint64)');
    OP_GE_UINT64 := -Add('>= (uint64)');

    OP_ADD_CURRENCY := -Add('+ (currency)');
    OP_SUB_CURRENCY := -Add('- (currency)');
    OP_MUL_CURRENCY := -Add('* (currency)');
    OP_DIV_CURRENCY := -Add('/ (currency)');

    OP_LT_CURRENCY := -Add('< (currency)');
    OP_LE_CURRENCY := -Add('<= (currency)');
    OP_GT_CURRENCY := -Add('> (currency)');
    OP_GE_CURRENCY := -Add('>= (currency)');
    OP_EQ_CURRENCY := -Add('= (currency)');
    OP_NE_CURRENCY := -Add('<> (currency)');

    OP_ADD_DOUBLE := -Add('+ (double)');
    OP_SUB_DOUBLE := -Add('- (double)');
    OP_MUL_DOUBLE := -Add('* (double)');
    OP_DIV_DOUBLE := -Add('/ (double)');

    OP_NEG_DOUBLE := -Add('NEG (double)');
    OP_NEG_CURRENCY := -Add('NEG (currency)');

    OP_LT_DOUBLE := -Add('< (double)');
    OP_LE_DOUBLE := -Add('<= (double)');
    OP_GT_DOUBLE := -Add('> (double)');
    OP_GE_DOUBLE := -Add('>= (double)');
    OP_EQ_DOUBLE := -Add('= (double)');
    OP_NE_DOUBLE := -Add('<> (double)');

    OP_ADD_SINGLE := -Add('+ (single)');
    OP_SUB_SINGLE := -Add('- (single)');
    OP_MUL_SINGLE := -Add('* (single)');
    OP_DIV_SINGLE := -Add('/ (single)');

    OP_NEG_SINGLE := -Add('NEG (single)');

    OP_LT_SINGLE := -Add('< (single)');
    OP_LE_SINGLE := -Add('<= (single)');
    OP_GT_SINGLE := -Add('> (single)');
    OP_GE_SINGLE := -Add('>= (single)');
    OP_EQ_SINGLE := -Add('= (single)');
    OP_NE_SINGLE := -Add('<> (single)');

    OP_ADD_EXTENDED := -Add('+ (extended)');
    OP_SUB_EXTENDED := -Add('- (EXTENDED)');
    OP_MUL_EXTENDED := -Add('* (EXTENDED)');
    OP_DIV_EXTENDED := -Add('/ (EXTENDED)');

    OP_NEG_EXTENDED := -Add('NEG (EXTENDED)');

    OP_LT_EXTENDED := -Add('< (EXTENDED)');
    OP_LE_EXTENDED := -Add('<= (EXTENDED)');
    OP_GT_EXTENDED := -Add('> (EXTENDED)');
    OP_GE_EXTENDED := -Add('>= (EXTENDED)');
    OP_EQ_EXTENDED := -Add('= (EXTENDED)');
    OP_NE_EXTENDED := -Add('<> (EXTENDED)');

    OP_PUSH_PROG := -Add('push prog');
    OP_PUSH_ADDRESS := -Add('push address');
    OP_PUSH_STRUCTURE := -Add('push struct');
    OP_PUSH_SET := -Add('push set');

    OP_PUSH_BYTE_IMM := -Add('push (byte i)');
    OP_PUSH_BYTE := -Add('push (byte)');
    OP_PUSH_WORD_IMM := -Add('push (word i)');
    OP_PUSH_WORD := -Add('push (word)');
    OP_PUSH_CARDINAL_IMM := -Add('push (cardinal i)');
    OP_PUSH_CARDINAL := -Add('push (cardinal)');
    OP_PUSH_SMALLINT_IMM := -Add('push (smallint i)');
    OP_PUSH_SMALLINT := -Add('push (smallint)');
    OP_PUSH_SHORTINT_IMM := -Add('push (shortint i)');
    OP_PUSH_SHORTINT := -Add('push (shortint)');
    OP_PUSH_INT_IMM := -Add('push (int i)');
    OP_PUSH_INT := -Add('push (int)');
    OP_PUSH_PTR := -Add('push (ptr)');

    OP_PUSH_DOUBLE := -Add('push (double)');
    OP_PUSH_CURRENCY := -Add('push (currency)');
    OP_PUSH_SINGLE := -Add('push (single)');
    OP_PUSH_EXTENDED := -Add('push (extended)');

    OP_PUSH_INT64 := -Add('push (int64)');
    OP_PUSH_DATA := -Add('push (data)');
    OP_PUSH_EVENT := -Add('push (event)');
{$IFNDEF PAXARM}
    OP_PUSH_ANSISTRING := -Add('push (ansistring)');
    OP_PUSH_SHORTSTRING := -Add('push (shortstring)');
    OP_PUSH_WIDESTRING := -Add('push (widestring)');
    OP_PUSH_PANSICHAR_IMM := -Add('push (pansichar i)');
{$ENDIF}
    OP_PUSH_PWIDECHAR_IMM := -Add('push (pwidechar i)');
    OP_PUSH_UNICSTRING := -Add('push (unicstring)');
    OP_PUSH_INST := -Add('push inst');
    OP_PUSH_CLSREF := -Add('push clsref');
    OP_PUSH_DYNARRAY := -Add('push dynarray');
    OP_PUSH_OPENARRAY := -Add('push openarray');

    OP_SET_ASSIGN := -Add('SET ASSIGN');
    OP_SET_COUNTER_ASSIGN := -Add('SET COUNTER ASSIGN');
    OP_SET_UNION := -Add('SET UNION');
    OP_SET_DIFFERENCE :=  -Add('SET DIFFERENCE');
    OP_SET_INTERSECTION := -Add('SET INTERSECTION');
    OP_SET_SUBSET := -Add('SET SUBSET');
    OP_SET_SUPERSET := -Add('SET SUPERSET');
    OP_SET_EQUALITY := -Add('SET EQUALITY');
    OP_SET_INEQUALITY := -Add('SET INEQUALITY');

    OP_DETERMINE_PROP := -Add('DETERMINE PROP');

    OP_GET_COMPONENT := -Add('GET COMPONENT');

    OP_GET_DRTTI_PROP := -Add('GET DRTTI PROP');
    OP_SET_DRTTI_PROP := -Add('SET DRTTI PROP');
{$IFNDEF PAXARM}
    OP_GET_ANSISTR_PROP := -Add('GET ANSISTR PROP');
    OP_GET_WIDESTR_PROP := -Add('GET WIDESTR PROP');
{$ENDIF}
    OP_GET_UNICSTR_PROP := -Add('GET UNICSTR PROP');
    OP_GET_ORD_PROP := -Add('GET ORD PROP');
    OP_GET_SET_PROP := -Add('GET SET PROP');
    OP_GET_INTERFACE_PROP := -Add('GET INTERFACE PROP');
    OP_GET_FLOAT_PROP := -Add('GET FLOAT PROP');
    OP_GET_VARIANT_PROP := -Add('GET VARIANT PROP');
    OP_GET_INT64_PROP := -Add('GET INT64 PROP');
    OP_GET_EVENT_PROP := -Add('GET EVENT PROP');

{$IFDEF UNIC}
    OP_ADD_STRING := OP_ADD_UNICSTRING;
{$ELSE}
    OP_ADD_STRING := OP_ADD_ANSISTRING;
{$ENDIF}
    OP_ADD_MESSAGE := - Add('ADD MESSAGE');

    OP_DUMMY := - Add('DUMMY');

    if IsDump then
      SaveToFile(DUMP_PATH + 'operators.txt');
  end;

  PushOperators := TIntegerList.Create;
  with PushOperators do
  begin
    Add(OP_PUSH_ADDRESS);
    Add(OP_PUSH_STRUCTURE);
    Add(OP_PUSH_SET);

    Add(OP_PUSH_BYTE_IMM);
    Add(OP_PUSH_BYTE);
    Add(OP_PUSH_WORD_IMM);
    Add(OP_PUSH_WORD);
    Add(OP_PUSH_CARDINAL_IMM);
    Add(OP_PUSH_CARDINAL);
    Add(OP_PUSH_SMALLINT_IMM);
    Add(OP_PUSH_SMALLINT);
    Add(OP_PUSH_SHORTINT_IMM);
    Add(OP_PUSH_SHORTINT);
    Add(OP_PUSH_INT_IMM);
    Add(OP_PUSH_INT);

    Add(OP_PUSH_DOUBLE);
    Add(OP_PUSH_CURRENCY);
    Add(OP_PUSH_SINGLE);
    Add(OP_PUSH_EXTENDED);

    Add(OP_PUSH_INT64);
    Add(OP_PUSH_DATA);
    Add(OP_PUSH_EVENT);
{$IFNDEF PAXARM}
    Add(OP_PUSH_ANSISTRING);
    Add(OP_PUSH_SHORTSTRING);
    Add(OP_PUSH_WIDESTRING);
    Add(OP_PUSH_PANSICHAR_IMM);
{$ENDIF}
    Add(OP_PUSH_UNICSTRING);
    Add(OP_PUSH_PWIDECHAR_IMM);
//    Add(OP_PUSH_INST);
//    Add(OP_PUSH_CLSREF);
    Add(OP_PUSH_DYNARRAY);
    Add(OP_PUSH_OPENARRAY);
  end;

  AsmOperators := TStringList.Create;
  with AsmOperators do
  begin
    ASM_NOP := Add('NOP');
    ASM_WAIT := Add('WAIT');
    ASM_CLC := Add('CLC');
    ASM_PUSHFD := Add('PUSHFD');
    ASM_POPFD := Add('POPFD');

    ASM_XCHG := Add('XCHG');

    ASM_MOV := Add('MOV');
    ASM_LEA := Add('LEA');
    ASM_TEST := Add('TEST');

    ASM_ADD := Add('ADD');
    ASM_ADC := Add('ADC');
    ASM_SBB := Add('SBB');
    ASM_NEG := Add('NEG');
    ASM_NOT := Add('NOT');
    ASM_SUB := Add('SUB');
    ASM_MUL := Add('MUL');
    ASM_IMUL := Add('IMUL');
    ASM_DIV := Add('DIV');
    ASM_IDIV := Add('IDIV');
    ASM_XOR := Add('XOR');
    ASM_AND := Add('AND');
    ASM_OR := Add('OR');
    ASM_SHL := Add('SHL');
    ASM_SHR := Add('SHR');

    ASM_CDQ := Add('CDQ');

    ASM_CALL := Add('CALL');
    ASM_RET := Add('RET');
    ASM_PUSH := Add('PUSH');
    ASM_POP := Add('POP');
    ASM_JMP := Add('JMP');

    ASM_JNO := Add('JNO');
    ASM_JNC := Add('JNC');
    ASM_JZ  := Add('JZ');
    ASM_JNZ := Add('JNZ');
    ASM_JBE := Add('JBE');
    ASM_JNLE:= Add('JNLE');

    ASM_FLD := Add('FLD');
    ASM_FILD := Add('FILD');
    ASM_FISTP := Add('FISTP');
    ASM_FSTP := Add('FSTP');
    ASM_FADD := Add('FADD');
    ASM_FSUB := Add('FSUB');
    ASM_FMUL := Add('FMUL');
    ASM_FDIV := Add('FDIV');
    ASM_FCOMP := Add('FCOMP');
    ASM_FCOMPP := Add('FCOMPP');
    ASM_FSTSV := Add('FSTSV');
    ASM_SAHF := Add('SAHF');
    ASM_FCHS := Add('FCHS');
    ASM_FABS := Add('FABS');

    ASM_SETL := Add('SETL'); // <
    ASM_SETLE := Add('SETLE'); // <=
    ASM_SETNLE := Add('SETNLE'); // >
    ASM_SETNL := Add('SETNL'); // >=

    ASM_SETB := Add('SETB'); // <
    ASM_SETBE := Add('SETBE'); // <=
    ASM_SETNBE := Add('SETNBE'); // >
    ASM_SETNB := Add('SETNB'); // >=
    ASM_SETZ := Add('SETZ'); // =
    ASM_SETNZ := Add('SETNZ'); // =

    ASM_CMP := Add('CMP');

    ASM_REP_MOVSB := Add('REP MOVSB');
    ASM_REP_MOVSD := Add('REP MOVSD');

    ASM_MOVSD := Add('MOVSD');
    ASM_MOVSS := Add('MOVSS');
    ASM_CVTSD2SS := Add('CVTSD2SS');
    ASM_CVTSS2SD := Add('CVTSS2SD');

    ASM_INC := Add('INC');
    ASM_DEC := Add('DEC');
  end;

  DynDestrList := TIntegerList.Create;
  with DynDestrList do
  begin
{$IFNDEF PAXARM}
    Add(OP_ANSISTRING_CLR);
    Add(OP_WIDESTRING_CLR);
{$ENDIF}
    Add(OP_VARIANT_CLR);
    Add(OP_UNICSTRING_CLR);
    Add(OP_INTERFACE_CLR);
    Add(OP_DYNARRAY_CLR);
    Add(OP_STRUCTURE_CLR);
  end;

finalization

  FreeAndNil(Types);
  FreeAndNil(Kinds);
  FreeAndNil(Operators);
  FreeAndNil(AsmOperators);
  FreeAndNil(DynDestrList);
  FreeAndNil(PushOperators);
end.




















