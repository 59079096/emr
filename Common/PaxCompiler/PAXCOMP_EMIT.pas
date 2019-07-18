// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_EMIT.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$R-}
unit PAXCOMP_EMIT;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_BYTECODE,
  PAXCOMP_MODULE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_VAROBJECT,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_SYMBOL_PROGRAM;
type
  TRegisters = class
  private
    PAX64: Boolean;
    A: array[_NOREG.._R15] of Boolean;
  public
    constructor Create(aPAX64: Boolean);
    function GetReg64: Integer;
    function GetReg: Integer; overload;
    procedure GetReg(Reg: Integer); overload;
    procedure FreeReg(Reg: Integer);
  end;

  TEmitProc = procedure of object;

  TEmitter = class
  private
    kernel: Pointer;
    R: TCodeRec;
    Prg: TSymbolProg;

    Registers: TRegisters;

    ContextStack: TIntegerStack;

    List1: TList;
    List2: TList;
    List3: TList;

    EmitOff: Boolean;
    HandlesEvents: Boolean;
    OverflowCheck: Boolean;

    EmitList: array of TEmitProc;

    procedure EmitSaveRBX;
    procedure EmitRestoreRBX;
    procedure EmitSaveRDI;
    procedure EmitRestoreRDI;
    function GetTargetPlatform: TTargetPlatform;
    function GetSizeOfPointer: Integer;
    procedure CheckSetElement(S: TSymbolRec);
    procedure EmitPushParam(Reg: Integer);
    procedure EmitPushParam_64(Reg: Integer);
    function GetSaveRegAreaOffset: Integer;
    procedure SaveRegisters(const A: array of Integer;
                            ExtraOffset: Integer = 0);
    procedure RestoreRegisters(const A: array of Integer;
                               ExtraOffset: Integer = 0);

    procedure EmitStdCall(SubId: Integer; InitSize: Integer = - 1);

    procedure EmitCallPro(SubId: Integer; InitSize: Integer = -1);
    procedure EmitCallEpi(SubId: Integer; InitSize: Integer = -1);

    procedure EmitStdCall_Adr1(SubId: Integer);
    procedure EmitStdCall_Adr1_AdrR(SubId: Integer);
    procedure EmitStdCall_Adr1_Adr2_AdrR(SubId: Integer);
    procedure EmitStdCall_Lang_Adr1_Adr2_AdrR(SubId: Integer);
    procedure EmitStdCall_Adr1_from_Int2(SubId: Integer);
    procedure EmitStdCall_Adr1_from_Adr2(SubId: Integer);
    procedure EmitStdCall_AdrR_from_Adr2(SubId: Integer);

    procedure CreateEmitProcList;
    procedure EmitNotImpl;
    procedure EmitNothing;
    procedure EmitComment;
    procedure EmitStartSub(SubId: Integer);
    procedure EmitFinSub(SubId: Integer);
    function GetSymbolRec(Id: Integer): TSymbolRec;
    function SymbolTable: TSymbolTable;
    function ByteCode: TCode;
    function GetOperName: String;
    procedure EmitPCodeOperator;
    function Lookup(const S: String): Integer;
    function GetLanguage: Integer;

    function IsLocalPos: Boolean;

    procedure EmitCheckOperResult(Reg: Integer);

    procedure Emit_PUSH_REGS(SubId: Integer = 0);
    procedure Emit_POP_REGS(SubId: Integer = 0);

    procedure Emit_PUSH_REGS_EX;
    procedure Emit_POP_REGS_EX;

    procedure EmitOP_SEPARATOR(I: Integer);

    procedure EmitOP_EMIT_ON;
    procedure EmitOP_EMIT_OFF;

    procedure EmitOP_PUSH_PROG;  // stdcall expected on win32

    procedure EmitOP_EXPORTS;
    procedure EmitOP_EXPORTS_64;
    procedure EmitOP_PUSH_ADDRESS;
    procedure EmitOP_PUSH_ADDRESS_64;
    procedure EmitOP_PUSH_STRUCTURE;
    procedure EmitOP_PUSH_STRUCTURE_64;
    procedure EmitOP_PUSH_SET;
    procedure EmitOP_PUSH_SET_64;

    procedure EmitOP_PUSH_INT_IMM;
    procedure EmitOP_PUSH_INT_IMM_64;
    procedure EmitOP_PUSH_INT;
    procedure EmitOP_PUSH_INT_64;
    procedure EmitOP_PUSH_INT64;
    procedure EmitOP_PUSH_DOUBLE;
    procedure EmitOP_PUSH_DOUBLE_64;
    procedure EmitOP_PUSH_CURRENCY;
    procedure EmitOP_PUSH_SINGLE;
    procedure EmitOP_PUSH_SINGLE_64;
    procedure EmitOP_PUSH_EXTENDED;
    procedure EmitOP_PUSH_ANSISTRING;
    procedure EmitOP_PUSH_ANSISTRING_64;
    procedure EmitOP_PUSH_WIDESTRING;
    procedure EmitOP_PUSH_WIDESTRING_64;
    procedure EmitOP_PUSH_UNICSTRING;
    procedure EmitOP_PUSH_UNICSTRING_64;
    procedure EmitOP_PUSH_SHORTSTRING;
    procedure EmitOP_PUSH_SHORTSTRING_64;
{$IFNDEF PAXARM}
    procedure EmitOP_PUSH_PANSICHAR_IMM;
    procedure EmitOP_PUSH_PANSICHAR_IMM_64;
{$ENDIF}
    procedure EmitOP_PUSH_PWIDECHAR_IMM;
    procedure EmitOP_PUSH_PWIDECHAR_IMM_64;
    procedure EmitOP_PUSH_INST;
    procedure EmitOP_PUSH_INST_64;
    procedure EmitOP_PUSH_CLSREF;
    procedure EmitOP_PUSH_CLSREF_64;
    procedure EmitOP_UPDATE_INSTANCE;
    procedure EmitOP_UPDATE_INSTANCE_64;
    procedure EmitOP_CLEAR_EDX;
    procedure EmitOP_CLEAR_EDX_64;
    procedure EmitOP_PUSH_DYNARRAY;
    procedure EmitOP_PUSH_DYNARRAY_64;
    procedure EmitOP_PUSH_OPENARRAY;
    procedure EmitOP_PUSH_OPENARRAY_64;
    procedure EmitOP_PUSH_DATA;
    procedure EmitOP_PUSH_DATA_64;
    procedure EmitOP_PUSH_EVENT;
    procedure EmitOP_PUSH_EVENT_64;

    procedure EmitOP_CALL;
    procedure EmitOP_CALL_64;
    procedure EmitOP_BEGIN_CALL;
    procedure EmitOP_BEGIN_CALL_64;

    procedure EmitOP_CURRENCY_FROM_INT;
    procedure EmitOP_CURRENCY_FROM_INT_64;
    procedure EmitOP_CURRENCY_FROM_INT64;
    procedure EmitOP_CURRENCY_FROM_INT64_64;
    procedure EmitOP_CURRENCY_FROM_REAL;
    procedure EmitOP_CURRENCY_FROM_REAL_64;

    procedure EmitOP_ASSIGN_INT_I;
    procedure EmitOP_ASSIGN_INT_I_64;
    procedure EmitOP_ASSIGN_INT_M;
    procedure EmitOP_ASSIGN_INT_M_64;
    procedure EmitOP_ASSIGN_PANSICHAR;
    procedure EmitOP_ASSIGN_PANSICHAR_64;
    procedure EmitOP_ASSIGN_PWIDECHAR;
    procedure EmitOP_ASSIGN_PWIDECHAR_64;
    procedure EmitOP_ASSIGN_DOUBLE;
    procedure EmitOP_ASSIGN_DOUBLE_64;
    procedure EmitOP_ASSIGN_CURRENCY;
    procedure EmitOP_ASSIGN_CURRENCY_64;
    procedure EmitOP_ASSIGN_EVENT;
    procedure EmitOP_ASSIGN_EVENT_64;
    procedure EmitOP_ASSIGN_SINGLE;
    procedure EmitOP_ASSIGN_SINGLE_64;
    procedure EmitOP_ASSIGN_EXTENDED;
    procedure EmitOP_ASSIGN_EXTENDED_64;
    procedure EmitOP_ASSIGN_INT64;
    procedure EmitOP_ASSIGN_INT64_64;
    procedure EmitOP_ASSIGN_RECORD;
    procedure EmitOP_ASSIGN_RECORD_64;
    procedure EmitOP_ASSIGN_RECORD_EX;
    procedure EmitOP_ASSIGN_INTERFACE;
    procedure EmitOP_ASSIGN_INTERFACE_64;

    procedure EmitOP_CREATE_EVENT;
    procedure EmitOP_CREATE_EVENT_64;

    procedure EmitOP_INT_FROM_INT64;
    procedure EmitOP_INT_FROM_INT64_64;
    procedure EmitOP_BYTE_FROM_INT64;
    procedure EmitOP_BYTE_FROM_INT64_64;
    procedure EmitOP_WORD_FROM_INT64;
    procedure EmitOP_WORD_FROM_INT64_64;
    procedure EmitOP_CARDINAL_FROM_INT64;
    procedure EmitOP_CARDINAL_FROM_INT64_64;
    procedure EmitOP_SMALLINT_FROM_INT64;
    procedure EmitOP_SMALLINT_FROM_INT64_64;
    procedure EmitOP_SHORTINT_FROM_INT64;
    procedure EmitOP_SHORTINT_FROM_INT64_64;

    procedure EmitOP_ADD_INT64;
    procedure EmitOP_ADD_INT64_64;
    procedure EmitOP_SUB_INT64;
    procedure EmitOP_SUB_INT64_64;
    procedure EmitOP_AND_INT64;
    procedure EmitOP_AND_INT64_64;
    procedure EmitOP_OR_INT64;
    procedure EmitOP_OR_INT64_64;
    procedure EmitOP_XOR_INT64;
    procedure EmitOP_ABS_INT64;
    procedure EmitOP_ABS_INT64_64;

    procedure EmitOP_ADD_UINT64;
    procedure EmitOP_ADD_UINT64_64;
    procedure EmitOP_SUB_UINT64;
    procedure EmitOP_SUB_UINT64_64;
    procedure EmitOP_AND_UINT64;
    procedure EmitOP_AND_UINT64_64;
    procedure EmitOP_OR_UINT64;
    procedure EmitOP_OR_UINT64_64;
    procedure EmitOP_XOR_UINT64;

    procedure EmitOP_LT_INT64;
    procedure EmitOP_LT_INT64_64;
    procedure EmitOP_LE_INT64;
    procedure EmitOP_LE_INT64_64;
    procedure EmitOP_GT_INT64;
    procedure EmitOP_GT_INT64_64;
    procedure EmitOP_GE_INT64;
    procedure EmitOP_GE_INT64_64;
    procedure EmitOP_EQ_INT64;
    procedure EmitOP_EQ_INT64_64;
    procedure EmitOP_NE_INT64;
    procedure EmitOP_NE_INT64_64;

    procedure EmitOP_LT_UINT64;
    procedure EmitOP_LT_UINT64_64;
    procedure EmitOP_LE_UINT64;
    procedure EmitOP_LE_UINT64_64;
    procedure EmitOP_GT_UINT64;
    procedure EmitOP_GT_UINT64_64;
    procedure EmitOP_GE_UINT64;
    procedure EmitOP_GE_UINT64_64;

    procedure EmitOP_EQ_STRUCT;
    procedure EmitOP_EQ_STRUCT_64;
    procedure EmitOP_NE_STRUCT;
    procedure EmitOP_NE_STRUCT_64;

    procedure EmitOP_ADD_CURRENCY;
    procedure EmitOP_ADD_CURRENCY_64;
    procedure EmitOP_SUB_CURRENCY;
    procedure EmitOP_SUB_CURRENCY_64;
    procedure EmitOP_MUL_CURRENCY;
    procedure EmitOP_MUL_CURRENCY_64;
    procedure EmitOP_DIV_CURRENCY;
    procedure EmitOP_DIV_CURRENCY_64;

    procedure EmitOP_ADD_INT_MI;
    procedure EmitOP_ADD_INT_MI_64;
    procedure EmitOP_ADD_INT_MM;
    procedure EmitOP_ADD_INT_MM_64;
    procedure EmitOP_ADD_DOUBLE;
    procedure EmitOP_ADD_DOUBLE_64;
    procedure EmitOP_ADD_SINGLE;
    procedure EmitOP_ADD_SINGLE_64;
    procedure EmitOP_ADD_EXTENDED;
    procedure EmitOP_ADD_EXTENDED_64;

    procedure EmitOP_SUB_INT_MI;
    procedure EmitOP_SUB_INT_MI_64;
    procedure EmitOP_SUB_INT_MM;
    procedure EmitOP_SUB_INT_MM_64;
    procedure EmitOP_SUB_DOUBLE;
    procedure EmitOP_SUB_DOUBLE_64;
    procedure EmitOP_SUB_SINGLE;
    procedure EmitOP_SUB_SINGLE_64;
    procedure EmitOP_SUB_EXTENDED;
    procedure EmitOP_SUB_EXTENDED_64;

    procedure EmitOP_NEG_INT;
    procedure EmitOP_NEG_INT_64;
    procedure EmitOP_NEG_INT64;
    procedure EmitOP_NEG_INT64_64;
    procedure EmitOP_NOT;
    procedure EmitOP_NOT_64;
    procedure EmitOP_NOT_BOOL;
    procedure EmitOP_NOT_BOOL64;
    procedure EmitOP_NOT_BYTEBOOL;
    procedure EmitOP_NOT_BYTEBOOL64;
    procedure EmitOP_NOT_WORDBOOL;
    procedure EmitOP_NOT_WORDBOOL64;
    procedure EmitOP_NOT_LONGBOOL;
    procedure EmitOP_NOT_LONGBOOL64;
    procedure EmitOP_NEG_DOUBLE;
    procedure EmitOP_NEG_DOUBLE_64;
    procedure EmitOP_NEG_CURRENCY;
    procedure EmitOP_NEG_CURRENCY_64;
    procedure EmitOP_NEG_SINGLE;
    procedure EmitOP_NEG_SINGLE_64;
    procedure EmitOP_NEG_EXTENDED;
    procedure EmitOP_NEG_EXTENDED_64;

    procedure EmitOP_ABS_INT;
    procedure EmitOP_ABS_INT_64;
    procedure EmitOP_ABS_DOUBLE;
    procedure EmitOP_ABS_DOUBLE_64;
    procedure EmitOP_ABS_SINGLE;
    procedure EmitOP_ABS_SINGLE_64;
    procedure EmitOP_ABS_EXTENDED;
    procedure EmitOP_ABS_EXTENDED_64;
    procedure EmitOP_ABS_CURRENCY;
    procedure EmitOP_ABS_CURRENCY_64;

    procedure EmitOP_IMUL_INT_MI;
    procedure EmitOP_IMUL_INT_MI_64;
    procedure EmitOP_IMUL_INT_MM;
    procedure EmitOP_IMUL_INT_MM_64;
    procedure EmitOP_MUL_DOUBLE;
    procedure EmitOP_MUL_DOUBLE_64;
    procedure EmitOP_MUL_SINGLE;
    procedure EmitOP_MUL_SINGLE_64;
    procedure EmitOP_MUL_EXTENDED;
    procedure EmitOP_MUL_EXTENDED_64;

    procedure EmitOP_IDIV_INT_MI;
    procedure EmitOP_IDIV_INT_MI_64;
    procedure EmitOP_IDIV_INT_MM;
    procedure EmitOP_IDIV_INT_MM_64;
    procedure EmitOP_IDIV_INT_IM;
    procedure EmitOP_IDIV_INT_IM_64;
    procedure EmitOP_DIV_DOUBLE;
    procedure EmitOP_DIV_DOUBLE_64;
    procedure EmitOP_DIV_SINGLE;
    procedure EmitOP_DIV_SINGLE_64;
    procedure EmitOP_DIV_EXTENDED;
    procedure EmitOP_DIV_EXTENDED_64;

    procedure EmitOP_MOD_INT_MI;
    procedure EmitOP_MOD_INT_MI_64;
    procedure EmitOP_MOD_INT_MM;
    procedure EmitOP_MOD_INT_MM_64;
    procedure EmitOP_MOD_INT_IM;
    procedure EmitOP_MOD_INT_IM_64;

    procedure EmitOP_SHL_INT_MI;
    procedure EmitOP_SHL_INT_MI_64;
    procedure EmitOP_SHL_INT_MM;
    procedure EmitOP_SHL_INT_MM_64;
    procedure EmitOP_SHL_INT_IM;
    procedure EmitOP_SHL_INT_IM_64;

    procedure EmitOP_SHR_INT_MI;
    procedure EmitOP_SHR_INT_MI_64;
    procedure EmitOP_SHR_INT_MM;
    procedure EmitOP_SHR_INT_MM_64;
    procedure EmitOP_SHR_INT_IM;
    procedure EmitOP_SHR_INT_IM_64;

    procedure EmitOP_AND_INT_MI;
    procedure EmitOP_AND_INT_MI_64;
    procedure EmitOP_AND_INT_MM;
    procedure EmitOP_AND_INT_MM_64;
    procedure EmitOP_OR_INT_MI;
    procedure EmitOP_OR_INT_MI_64;
    procedure EmitOP_OR_INT_MM;
    procedure EmitOP_OR_INT_MM_64;
    procedure EmitOP_XOR_INT_MI;
    procedure EmitOP_XOR_INT_MI_64;
    procedure EmitOP_XOR_INT_MM;
    procedure EmitOP_XOR_INT_MM_64;

    procedure EmitOP_LT_INT_MI;
    procedure EmitOP_LT_INT_MI_64;
    procedure EmitOP_LT_INT_MM;
    procedure EmitOP_LT_INT_MM_64;

    procedure EmitOP_LE_INT_MI;
    procedure EmitOP_LE_INT_MI_64;
    procedure EmitOP_LE_INT_MM;
    procedure EmitOP_LE_INT_MM_64;

    procedure EmitOP_GT_INT_MI;
    procedure EmitOP_GT_INT_MI_64;
    procedure EmitOP_GT_INT_MM;
    procedure EmitOP_GT_INT_MM_64;

    procedure EmitOP_GE_INT_MI;
    procedure EmitOP_GE_INT_MI_64;
    procedure EmitOP_GE_INT_MM;
    procedure EmitOP_GE_INT_MM_64;

    procedure EmitOP_EQ_INT_MI;
    procedure EmitOP_EQ_INT_MI_64;
    procedure EmitOP_EQ_INT_MM;
    procedure EmitOP_EQ_INT_MM_64;

    procedure EmitOP_NE_INT_MI;
    procedure EmitOP_NE_INT_MI_64;
    procedure EmitOP_NE_INT_MM;
    procedure EmitOP_NE_INT_MM_64;

    procedure EmitOP_LT_DOUBLE;
    procedure EmitOP_LT_DOUBLE_64;
    procedure EmitOP_LE_DOUBLE;
    procedure EmitOP_LE_DOUBLE_64;
    procedure EmitOP_GT_DOUBLE;
    procedure EmitOP_GT_DOUBLE_64;
    procedure EmitOP_GE_DOUBLE;
    procedure EmitOP_GE_DOUBLE_64;
    procedure EmitOP_EQ_DOUBLE;
    procedure EmitOP_EQ_DOUBLE_64;
    procedure EmitOP_NE_DOUBLE;
    procedure EmitOP_NE_DOUBLE_64;

    procedure EmitOP_LT_CURRENCY;
    procedure EmitOP_LT_CURRENCY_64;
    procedure EmitOP_LE_CURRENCY;
    procedure EmitOP_LE_CURRENCY_64;
    procedure EmitOP_GT_CURRENCY;
    procedure EmitOP_GT_CURRENCY_64;
    procedure EmitOP_GE_CURRENCY;
    procedure EmitOP_GE_CURRENCY_64;
    procedure EmitOP_EQ_CURRENCY;
    procedure EmitOP_EQ_CURRENCY_64;
    procedure EmitOP_NE_CURRENCY;
    procedure EmitOP_NE_CURRENCY_64;

    procedure EmitOP_LT_SINGLE;
    procedure EmitOP_LT_SINGLE_64;
    procedure EmitOP_LE_SINGLE;
    procedure EmitOP_LE_SINGLE_64;
    procedure EmitOP_GT_SINGLE;
    procedure EmitOP_GT_SINGLE_64;
    procedure EmitOP_GE_SINGLE;
    procedure EmitOP_GE_SINGLE_64;
    procedure EmitOP_EQ_SINGLE;
    procedure EmitOP_EQ_SINGLE_64;
    procedure EmitOP_NE_SINGLE;
    procedure EmitOP_NE_SINGLE_64;

    procedure EmitOP_LT_EXTENDED;
    procedure EmitOP_LT_EXTENDED_64;
    procedure EmitOP_LE_EXTENDED;
    procedure EmitOP_LE_EXTENDED_64;
    procedure EmitOP_GT_EXTENDED;
    procedure EmitOP_GT_EXTENDED_64;
    procedure EmitOP_GE_EXTENDED;
    procedure EmitOP_GE_EXTENDED_64;
    procedure EmitOP_EQ_EXTENDED;
    procedure EmitOP_EQ_EXTENDED_64;
    procedure EmitOP_NE_EXTENDED;
    procedure EmitOP_NE_EXTENDED_64;

    procedure EmitOP_INIT_SUB;
    procedure EmitOP_INIT_SUB_64;
    procedure EmitOP_END_SUB;
    procedure EmitOP_END_SUB_64;
    procedure EmitOP_FIN_SUB;
    procedure EmitOP_FIN_SUB_64;
    procedure EmitOP_RET;
    procedure EmitOP_RET_64;
    procedure EmitOP_PUSH_EBP;
    procedure EmitOP_PUSH_EBP_64;
    procedure EmitOP_POP;
    procedure EmitOP_POP_64;
    procedure EmitOP_SAVE_REGS;
    procedure EmitOP_SAVE_REGS_64;
    procedure EmitOP_RESTORE_REGS;
    procedure EmitOP_RESTORE_REGS_64;

    procedure EmitOP_FIELD;
    procedure EmitOP_FIELD_64;
    procedure EmitOP_ELEM;
    procedure EmitOP_ELEM_64;
    procedure EmitOP_GET_COMPONENT;
    procedure EmitOP_GET_COMPONENT_64;

    procedure EmitOP_PRINT_EX;
    procedure EmitOP_PRINT_EX_64;

    procedure EmitOP_TO_FW_OBJECT;
    procedure EmitOP_TO_FW_OBJECT_64;

    procedure EmitOP_INT_TO_INT64;
    procedure EmitOP_INT_TO_INT64_64;
    procedure EmitOP_BYTE_TO_INT64;
    procedure EmitOP_BYTE_TO_INT64_64;
    procedure EmitOP_WORD_TO_INT64;
    procedure EmitOP_WORD_TO_INT64_64;
    procedure EmitOP_CARDINAL_TO_INT64;
    procedure EmitOP_CARDINAL_TO_INT64_64;
    procedure EmitOP_SMALLINT_TO_INT64;
    procedure EmitOP_SMALLINT_TO_INT64_64;
    procedure EmitOP_SHORTINT_TO_INT64;
    procedure EmitOP_SHORTINT_TO_INT64_64;
    procedure EmitOP_INT_TO_DOUBLE;
    procedure EmitOP_INT_TO_DOUBLE_64;
    procedure EmitOP_INT64_TO_DOUBLE;
    procedure EmitOP_INT64_TO_DOUBLE_64;

    procedure EmitOP_INT_TO_SINGLE;
    procedure EmitOP_INT_TO_SINGLE_64;
    procedure EmitOP_INT64_TO_SINGLE;
    procedure EmitOP_INT64_TO_SINGLE_64;

    procedure EmitOP_INT_TO_EXTENDED;
    procedure EmitOP_INT_TO_EXTENDED_64;
    procedure EmitOP_INT64_TO_EXTENDED;
    procedure EmitOP_INT64_TO_EXTENDED_64;

    procedure EmitOP_MULT_INT64;
    procedure EmitOP_MULT_INT64_64;
    procedure EmitOP_IDIV_INT64;
    procedure EmitOP_IDIV_INT64_64;
    procedure EmitOP_MOD_INT64;
    procedure EmitOP_MOD_INT64_64;
    procedure EmitOP_SHL_INT64_64;
    procedure EmitOP_SHL_INT64;
    procedure EmitOP_SHR_INT64_64;
    procedure EmitOP_SHR_INT64;

    procedure EmitOP_CURRENCY_TO_EXTENDED;
    procedure EmitOP_CURRENCY_TO_EXTENDED_64;
    procedure EmitOP_CURRENCY_TO_SINGLE;
    procedure EmitOP_CURRENCY_TO_SINGLE_64;
    procedure EmitOP_DOUBLE_TO_SINGLE;
    procedure EmitOP_DOUBLE_TO_SINGLE_64;
    procedure EmitOP_DOUBLE_TO_EXTENDED;
    procedure EmitOP_DOUBLE_TO_EXTENDED_64;
    procedure EmitOP_SINGLE_TO_DOUBLE;
    procedure EmitOP_SINGLE_TO_DOUBLE_64;
    procedure EmitOP_CURRENCY_TO_DOUBLE;
    procedure EmitOP_CURRENCY_TO_DOUBLE_64;
    procedure EmitOP_SINGLE_TO_EXTENDED;
    procedure EmitOP_SINGLE_TO_EXTENDED_64;
    procedure EmitOP_EXTENDED_TO_DOUBLE;
    procedure EmitOP_EXTENDED_TO_DOUBLE_64;
    procedure EmitOP_EXTENDED_TO_SINGLE;
    procedure EmitOP_EXTENDED_TO_SINGLE_64;

    procedure EmitOP_GO;
    procedure EmitOP_GO_1;
    procedure EmitOP_GO_2;
    procedure EmitOP_GO_3;
    procedure EmitOP_GO_TRUE;
    procedure EmitOP_GO_TRUE_64;
    procedure EmitOP_GO_FALSE;
    procedure EmitOP_GO_FALSE_64;
    procedure EmitOP_GO_DL;
    procedure EmitOP_GO_DL_64;

    procedure EmitOP_SAVE_EDX;
    procedure EmitOP_SAVE_EDX_64;
    procedure EmitOP_RESTORE_EDX;
    procedure EmitOP_RESTORE_EDX_64;

    procedure EmitOP_ADDRESS;
    procedure EmitOP_ADDRESS_64;
    procedure EmitOP_TERMINAL;
    procedure EmitOP_TERMINAL_64;
    procedure EmitOP_ADDRESS_PROG;
    procedure EmitOP_ADDRESS_PROG_64;
    procedure EmitOP_ASSIGN_PROG;
    procedure EmitOP_ASSIGN_PROG_64;

    procedure EmitOP_SET_INCLUDE;
    procedure EmitOP_SET_INCLUDE_64;
    procedure EmitOP_SET_INCLUDE_INTERVAL;
    procedure EmitOP_SET_INCLUDE_INTERVAL_64;
    procedure EmitOP_SET_EXCLUDE;
    procedure EmitOP_SET_EXCLUDE_64;
    procedure EmitOP_SET_UNION;
    procedure EmitOP_SET_UNION_64;
    procedure EmitOP_SET_DIFFERENCE;
    procedure EmitOP_SET_DIFFERENCE_64;
    procedure EmitOP_SET_INTERSECTION;
    procedure EmitOP_SET_INTERSECTION_64;
    procedure EmitOP_SET_SUBSET;
    procedure EmitOP_SET_SUBSET_64;
    procedure EmitOP_SET_SUPERSET;
    procedure EmitOP_SET_SUPERSET_64;
    procedure EmitOP_SET_EQUALITY;
    procedure EmitOP_SET_EQUALITY_64;
    procedure EmitOP_SET_INEQUALITY;
    procedure EmitOP_SET_INEQUALITY_64;
    procedure EmitOP_SET_MEMBERSHIP;
    procedure EmitOP_SET_MEMBERSHIP_64;
    procedure EmitOP_SET_ASSIGN;
    procedure EmitOP_SET_ASSIGN_64;
    procedure EmitOP_SET_COUNTER_ASSIGN;
    procedure EmitOP_SET_COUNTER_ASSIGN_64;

    procedure EmitOP_ERR_ABSTRACT;
    procedure EmitOP_ERR_ABSTRACT_64;

    procedure EmitOP_LOAD_PROC;
    procedure EmitOP_LOAD_PROC_64;
    procedure EmitOP_ADD_MESSAGE;
    procedure EmitOP_ADD_MESSAGE_64;

    procedure EmitOP_BEGIN_CRT_JS_FUNC_OBJECT;
    procedure EmitOP_BEGIN_CRT_JS_FUNC_OBJECT_64;
    procedure EmitOP_END_CRT_JS_FUNC_OBJECT;
    procedure EmitOP_END_CRT_JS_FUNC_OBJECT_64;

    procedure EmitOP_TO_JS_OBJECT;
    procedure EmitOP_TO_JS_OBJECT_64;
    procedure EmitOP_JS_TYPEOF;
    procedure EmitOP_JS_TYPEOF_64;
    procedure EmitOP_JS_VOID;
    procedure EmitOP_JS_VOID_64;
    procedure EmitOP_GET_NEXTJSPROP;
    procedure EmitOP_GET_NEXTJSPROP_64;
    procedure EmitOP_CLEAR_REFERENCES;
    procedure EmitOP_CLEAR_REFERENCES_64;

    procedure EmitOP_CREATE_METHOD;
    procedure EmitOP_CREATE_METHOD_64;

    procedure EmitOP_VAR_FROM_TVALUE;
    procedure EmitOP_VAR_FROM_TVALUE_64;

    procedure EmitOP_INIT_PCHAR_LITERAL;
    procedure EmitOP_INIT_PWIDECHAR_LITERAL;
    procedure EmitOP_ANSISTRING_FROM_PANSICHAR;
    procedure EmitOP_ANSISTRING_FROM_PANSICHAR_64;
    procedure EmitOP_ANSISTRING_FROM_PWIDECHAR;
    procedure EmitOP_ANSISTRING_FROM_PWIDECHAR_64;
    procedure EmitOP_ANSISTRING_FROM_ANSICHAR;
    procedure EmitOP_ANSISTRING_FROM_ANSICHAR_64;
    procedure EmitOP_ASSIGN_ANSISTRING;
    procedure EmitOP_ASSIGN_ANSISTRING_64;
{$IFNDEF PAXARM}
    procedure EmitOP_ASSIGN_SHORTSTRING;
    procedure EmitOP_ASSIGN_SHORTSTRING_64;
{$ENDIF}
    procedure EmitOP_ASSIGN_WIDESTRING;
    procedure EmitOP_ASSIGN_WIDESTRING_64;
    procedure EmitOP_ASSIGN_UNICSTRING;
    procedure EmitOP_ASSIGN_UNICSTRING_64;
    procedure EmitOP_ADD_ANSISTRING;
    procedure EmitOP_ADD_ANSISTRING_64;
    procedure EmitOP_ADD_SHORTSTRING;
    procedure EmitOP_ADD_SHORTSTRING_64;
    procedure EmitOP_ADD_WIDESTRING;
    procedure EmitOP_ADD_WIDESTRING_64;
    procedure EmitOP_ADD_UNICSTRING;
    procedure EmitOP_ADD_UNICSTRING_64;
    procedure EmitOP_SET_LENGTH;
    procedure EmitOP_SET_LENGTH_64;
    procedure EmitOP_SET_LENGTH_EX;
    procedure EmitOP_SET_LENGTH_EX_64;
    procedure EmitOP_ANSISTRING_CLR;
    procedure EmitOP_ANSISTRING_CLR_64;
    procedure EmitOP_WIDESTRING_CLR;
    procedure EmitOP_WIDESTRING_CLR_64;
    procedure EmitOP_UNICSTRING_CLR;
    procedure EmitOP_UNICSTRING_CLR_64;
    procedure EmitOP_INTERFACE_CLR;
    procedure EmitOP_INTERFACE_CLR_64;
    procedure EmitOP_STRUCTURE_CLR;
    procedure EmitOP_STRUCTURE_CLR_64;
    procedure EmitOP_CLASS_CLR;
    procedure EmitOP_CLASS_CLR_64;
    procedure EmitOP_STRUCTURE_ADDREF;
    procedure EmitOP_STRUCTURE_ADDREF_64;
    procedure EmitOP_ADDREF_64;
    procedure EmitOP_ADDREF;
    procedure EmitOP_DYNARRAY_CLR;
    procedure EmitOP_DYNARRAY_CLR_64;
    procedure EmitOP_DYNARRAY_HIGH;
    procedure EmitOP_DYNARRAY_HIGH_64;
    procedure EmitOP_DYNARRAY_ASSIGN;
    procedure EmitOP_DYNARRAY_ASSIGN_64;
    procedure EmitOP_CREATE_EMPTY_DYNARRAY;
    procedure EmitOP_CREATE_EMPTY_DYNARRAY_64;

    procedure EmitOP_ASSIGN_TVarRec;
    procedure EmitOP_ASSIGN_TVarRec_64;

{$IFNDEF PAXARM}
    procedure EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
    procedure EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL_64;
    procedure EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL;
    procedure EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL_64;
    procedure EmitOP_SHORTSTRING_FROM_ANSISTRING;
    procedure EmitOP_SHORTSTRING_FROM_ANSISTRING_64;
    procedure EmitOP_SHORTSTRING_FROM_WIDESTRING;
    procedure EmitOP_SHORTSTRING_FROM_WIDESTRING_64;
    procedure EmitOP_SHORTSTRING_FROM_UNICSTRING;
    procedure EmitOP_SHORTSTRING_FROM_UNICSTRING_64;
{$ENDIF}
    procedure EmitOP_SHORTSTRING_FROM_ANSICHAR;
    procedure EmitOP_SHORTSTRING_FROM_ANSICHAR_64;
    procedure EmitOP_SHORTSTRING_FROM_WIDECHAR;
    procedure EmitOP_SHORTSTRING_FROM_WIDECHAR_64;
    procedure EmitOP_UNICSTRING_FROM_WIDESTRING;
    procedure EmitOP_UNICSTRING_FROM_WIDESTRING_64;
    procedure EmitOP_ANSISTRING_FROM_SHORTSTRING;
    procedure EmitOP_ANSISTRING_FROM_SHORTSTRING_64;

    procedure EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL;
    procedure EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL_64;
    procedure EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL;
    procedure EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL_64;
    procedure EmitOP_WIDESTRING_FROM_ANSICHAR;
    procedure EmitOP_WIDESTRING_FROM_ANSICHAR_64;
    procedure EmitOP_WIDESTRING_FROM_WIDECHAR;
    procedure EmitOP_WIDESTRING_FROM_WIDECHAR_64;
    procedure EmitOP_ANSISTRING_FROM_WIDECHAR;
    procedure EmitOP_ANSISTRING_FROM_WIDECHAR_64;
    procedure EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL;
    procedure EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL_64;
    procedure EmitOP_WIDESTRING_FROM_ANSISTRING;
    procedure EmitOP_WIDESTRING_FROM_ANSISTRING_64;
    procedure EmitOP_UNICSTRING_FROM_ANSISTRING;
    procedure EmitOP_UNICSTRING_FROM_ANSISTRING_64;
    procedure EmitOP_WIDESTRING_FROM_SHORTSTRING;
    procedure EmitOP_WIDESTRING_FROM_SHORTSTRING_64;
    procedure EmitOP_WIDESTRING_FROM_UNICSTRING;
    procedure EmitOP_WIDESTRING_FROM_UNICSTRING_64;
    procedure EmitOP_UNICSTRING_FROM_SHORTSTRING;
    procedure EmitOP_UNICSTRING_FROM_SHORTSTRING_64;
    procedure EmitOP_ANSISTRING_FROM_WIDESTRING;
    procedure EmitOP_ANSISTRING_FROM_WIDESTRING_64;
    procedure EmitOP_ANSISTRING_FROM_UNICSTRING;
    procedure EmitOP_ANSISTRING_FROM_UNICSTRING_64;

    procedure EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL;
    procedure EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL_64;
    procedure EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL;
    procedure EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL_64;
    procedure EmitOP_UNICSTRING_FROM_ANSICHAR;
    procedure EmitOP_UNICSTRING_FROM_ANSICHAR_64;
    procedure EmitOP_UNICSTRING_FROM_WIDECHAR;
    procedure EmitOP_UNICSTRING_FROM_WIDECHAR_64;
    procedure EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL;
    procedure EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL_64;

    procedure EmitOP_SHORTSTRING_HIGH;
    procedure EmitOP_SHORTSTRING_HIGH_64;

    procedure EmitOP_EQ_ANSISTRING;
    procedure EmitOP_EQ_ANSISTRING_64;
    procedure EmitOP_NE_ANSISTRING;
    procedure EmitOP_NE_ANSISTRING_64;
    procedure EmitOP_EQ_SHORTSTRING;
    procedure EmitOP_EQ_SHORTSTRING_64;
    procedure EmitOP_NE_SHORTSTRING;
    procedure EmitOP_NE_SHORTSTRING_64;
    procedure EmitOP_EQ_WIDESTRING;
    procedure EmitOP_EQ_WIDESTRING_64;
    procedure EmitOP_EQ_UNICSTRING;
    procedure EmitOP_EQ_UNICSTRING_64;
    procedure EmitOP_NE_WIDESTRING;
    procedure EmitOP_NE_WIDESTRING_64;
    procedure EmitOP_NE_UNICSTRING;
    procedure EmitOP_NE_UNICSTRING_64;

    procedure EmitOP_GT_ANSISTRING;
    procedure EmitOP_GT_ANSISTRING_64;
    procedure EmitOP_GE_ANSISTRING;
    procedure EmitOP_GE_ANSISTRING_64;
    procedure EmitOP_LT_ANSISTRING;
    procedure EmitOP_LT_ANSISTRING_64;
    procedure EmitOP_LE_ANSISTRING;
    procedure EmitOP_LE_ANSISTRING_64;

    procedure EmitOP_GT_SHORTSTRING;
    procedure EmitOP_GT_SHORTSTRING_64;
    procedure EmitOP_GE_SHORTSTRING;
    procedure EmitOP_GE_SHORTSTRING_64;
    procedure EmitOP_LT_SHORTSTRING;
    procedure EmitOP_LT_SHORTSTRING_64;
    procedure EmitOP_LE_SHORTSTRING;
    procedure EmitOP_LE_SHORTSTRING_64;

    procedure EmitOP_GT_WIDESTRING;
    procedure EmitOP_GT_WIDESTRING_64;
    procedure EmitOP_GE_WIDESTRING;
    procedure EmitOP_GE_WIDESTRING_64;
    procedure EmitOP_LT_WIDESTRING;
    procedure EmitOP_LT_WIDESTRING_64;
    procedure EmitOP_LE_WIDESTRING;
    procedure EmitOP_LE_WIDESTRING_64;

    procedure EmitOP_GT_UNICSTRING;
    procedure EmitOP_GT_UNICSTRING_64;
    procedure EmitOP_GE_UNICSTRING;
    procedure EmitOP_GE_UNICSTRING_64;
    procedure EmitOP_LT_UNICSTRING;
    procedure EmitOP_LT_UNICSTRING_64;
    procedure EmitOP_LE_UNICSTRING;
    procedure EmitOP_LE_UNICSTRING_64;

    procedure EmitOP_VARIANT_FROM_CLASS; // JS only
    procedure EmitOP_VARIANT_FROM_CLASS_64; // JS only
    procedure EmitOP_VARIANT_FROM_POINTER; //JS only
    procedure EmitOP_VARIANT_FROM_POINTER_64; //JS only
    procedure EmitOP_CLASS_FROM_VARIANT; // JS only
    procedure EmitOP_CLASS_FROM_VARIANT_64; // JS only

    procedure EmitOP_INTERFACE_FROM_CLASS;
    procedure EmitOP_INTERFACE_FROM_CLASS_64;
    procedure EmitOP_INTERFACE_CAST;
    procedure EmitOP_INTERFACE_CAST_64;

    procedure EmitOP_LOCK_VARRAY;
    procedure EmitOP_LOCK_VARRAY_64;
    procedure EmitOP_UNLOCK_VARRAY;
    procedure EmitOP_UNLOCK_VARRAY_64;

    procedure EmitOP_VARIANT_CLR;
    procedure EmitOP_VARIANT_CLR_64;
    procedure EmitOP_ASSIGN_VARIANT;
    procedure EmitOP_ASSIGN_VARIANT_64;
    procedure EmitOP_ASSIGN_OLEVARIANT;
    procedure EmitOP_ASSIGN_OLEVARIANT_64;

    procedure EmitOP_ASSIGN_CLASS;
    procedure EmitOP_ASSIGN_CLASS_64;

    procedure EmitOP_VARIANT_FROM_PANSICHAR_LITERAL;
    procedure EmitOP_VARIANT_FROM_PANSICHAR_LITERAL_64;
    procedure EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL;
    procedure EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL_64;
    procedure EmitOP_VARIANT_FROM_ANSISTRING;
    procedure EmitOP_VARIANT_FROM_ANSISTRING_64;
    procedure EmitOP_VARIANT_FROM_WIDESTRING;
    procedure EmitOP_VARIANT_FROM_WIDESTRING_64;
    procedure EmitOP_VARIANT_FROM_UNICSTRING;
    procedure EmitOP_VARIANT_FROM_UNICSTRING_64;
    procedure EmitOP_VARIANT_FROM_SHORTSTRING;
    procedure EmitOP_VARIANT_FROM_SHORTSTRING_64;
    procedure EmitOP_VARIANT_FROM_ANSICHAR;
    procedure EmitOP_VARIANT_FROM_ANSICHAR_64;
    procedure EmitOP_VARIANT_FROM_WIDECHAR;
    procedure EmitOP_VARIANT_FROM_WIDECHAR_64;
    procedure EmitOP_VARIANT_FROM_WIDECHAR_LITERAL;
    procedure EmitOP_VARIANT_FROM_WIDECHAR_LITERAL_64;
    procedure EmitOP_VARIANT_FROM_INT;
    procedure EmitOP_VARIANT_FROM_INT_64;
    procedure EmitOP_VARIANT_FROM_INT64;
    procedure EmitOP_VARIANT_FROM_INT64_64;
    procedure EmitOP_VARIANT_FROM_BYTE;
    procedure EmitOP_VARIANT_FROM_BYTE_64;
    procedure EmitOP_VARIANT_FROM_BOOL;
    procedure EmitOP_VARIANT_FROM_BOOL_64;
    procedure EmitOP_VARIANT_FROM_WORD;
    procedure EmitOP_VARIANT_FROM_WORD_64;
    procedure EmitOP_VARIANT_FROM_CARDINAL;
    procedure EmitOP_VARIANT_FROM_CARDINAL_64;
    procedure EmitOP_VARIANT_FROM_SMALLINT;
    procedure EmitOP_VARIANT_FROM_SMALLINT_64;
    procedure EmitOP_VARIANT_FROM_SHORTINT;
    procedure EmitOP_VARIANT_FROM_SHORTINT_64;
    procedure EmitOP_VARIANT_FROM_DOUBLE;
    procedure EmitOP_VARIANT_FROM_DOUBLE_64;
    procedure EmitOP_VARIANT_FROM_CURRENCY;
    procedure EmitOP_VARIANT_FROM_CURRENCY_64;
    procedure EmitOP_VARIANT_FROM_SINGLE;
    procedure EmitOP_VARIANT_FROM_SINGLE_64;
    procedure EmitOP_VARIANT_FROM_EXTENDED;
    procedure EmitOP_VARIANT_FROM_EXTENDED_64;
    procedure EmitOP_VARIANT_FROM_INTERFACE;
    procedure EmitOP_VARIANT_FROM_INTERFACE_64;

    procedure EmitOP_OLEVARIANT_FROM_VARIANT;
    procedure EmitOP_OLEVARIANT_FROM_VARIANT_64;
    procedure EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
    procedure EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL_64;
    procedure EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL;
    procedure EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL_64;
    procedure EmitOP_OLEVARIANT_FROM_ANSISTRING;
    procedure EmitOP_OLEVARIANT_FROM_ANSISTRING_64;
    procedure EmitOP_OLEVARIANT_FROM_WIDESTRING;
    procedure EmitOP_OLEVARIANT_FROM_WIDESTRING_64;
    procedure EmitOP_OLEVARIANT_FROM_UNICSTRING;
    procedure EmitOP_OLEVARIANT_FROM_UNICSTRING_64;
    procedure EmitOP_OLEVARIANT_FROM_SHORTSTRING;
    procedure EmitOP_OLEVARIANT_FROM_SHORTSTRING_64;
    procedure EmitOP_OLEVARIANT_FROM_ANSICHAR;
    procedure EmitOP_OLEVARIANT_FROM_ANSICHAR_64;
    procedure EmitOP_OLEVARIANT_FROM_WIDECHAR;
    procedure EmitOP_OLEVARIANT_FROM_WIDECHAR_64;
    procedure EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL;
    procedure EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL_64;
    procedure EmitOP_OLEVARIANT_FROM_INT;
    procedure EmitOP_OLEVARIANT_FROM_INT_64;
    procedure EmitOP_OLEVARIANT_FROM_INT64;
    procedure EmitOP_OLEVARIANT_FROM_INT64_64;
    procedure EmitOP_OLEVARIANT_FROM_BYTE;
    procedure EmitOP_OLEVARIANT_FROM_BYTE_64;
    procedure EmitOP_OLEVARIANT_FROM_BOOL;
    procedure EmitOP_OLEVARIANT_FROM_BOOL_64;
    procedure EmitOP_OLEVARIANT_FROM_WORD;
    procedure EmitOP_OLEVARIANT_FROM_WORD_64;
    procedure EmitOP_OLEVARIANT_FROM_CARDINAL;
    procedure EmitOP_OLEVARIANT_FROM_CARDINAL_64;
    procedure EmitOP_OLEVARIANT_FROM_SMALLINT;
    procedure EmitOP_OLEVARIANT_FROM_SMALLINT_64;
    procedure EmitOP_OLEVARIANT_FROM_SHORTINT;
    procedure EmitOP_OLEVARIANT_FROM_SHORTINT_64;
    procedure EmitOP_OLEVARIANT_FROM_DOUBLE;
    procedure EmitOP_OLEVARIANT_FROM_DOUBLE_64;
    procedure EmitOP_OLEVARIANT_FROM_CURRENCY;
    procedure EmitOP_OLEVARIANT_FROM_CURRENCY_64;
    procedure EmitOP_OLEVARIANT_FROM_SINGLE;
    procedure EmitOP_OLEVARIANT_FROM_SINGLE_64;
    procedure EmitOP_OLEVARIANT_FROM_EXTENDED;
    procedure EmitOP_OLEVARIANT_FROM_EXTENDED_64;
    procedure EmitOP_OLEVARIANT_FROM_INTERFACE;
    procedure EmitOP_OLEVARIANT_FROM_INTERFACE_64;

    procedure EmitOP_ANSISTRING_FROM_INT; // JS only
    procedure EmitOP_ANSISTRING_FROM_INT_64; // JS only
    procedure EmitOP_ANSISTRING_FROM_DOUBLE; // JS only
    procedure EmitOP_ANSISTRING_FROM_DOUBLE_64; // JS only
    procedure EmitOP_ANSISTRING_FROM_SINGLE; // JS only
    procedure EmitOP_ANSISTRING_FROM_SINGLE_64; // JS only
    procedure EmitOP_ANSISTRING_FROM_EXTENDED; // JS only
    procedure EmitOP_ANSISTRING_FROM_EXTENDED_64; // JS only
    procedure EmitOP_ANSISTRING_FROM_BOOLEAN; // JS only
    procedure EmitOP_ANSISTRING_FROM_BOOLEAN_64; // JS only

    procedure EmitOP_UNICSTRING_FROM_INT; // JS only
    procedure EmitOP_UNICSTRING_FROM_INT_64; // JS only
    procedure EmitOP_UNICSTRING_FROM_DOUBLE; // JS only
    procedure EmitOP_UNICSTRING_FROM_DOUBLE_64; // JS only
    procedure EmitOP_UNICSTRING_FROM_SINGLE; // JS only
    procedure EmitOP_UNICSTRING_FROM_SINGLE_64; // JS only
    procedure EmitOP_UNICSTRING_FROM_EXTENDED; // JS only
    procedure EmitOP_UNICSTRING_FROM_EXTENDED_64; // JS only
    procedure EmitOP_UNICSTRING_FROM_BOOLEAN; // JS only
    procedure EmitOP_UNICSTRING_FROM_BOOLEAN_64; // JS only

    procedure EmitOP_JS_FUNC_OBJ_FROM_VARIANT; // JS only
    procedure EmitOP_JS_FUNC_OBJ_FROM_VARIANT_64; // JS only

    procedure EmitOP_ANSICHAR_FROM_VARIANT;
    procedure EmitOP_ANSICHAR_FROM_VARIANT_64;
    procedure EmitOP_WIDECHAR_FROM_VARIANT;
    procedure EmitOP_WIDECHAR_FROM_VARIANT_64;
    procedure EmitOP_ANSISTRING_FROM_VARIANT;
    procedure EmitOP_ANSISTRING_FROM_VARIANT_64;
    procedure EmitOP_WIDESTRING_FROM_VARIANT;
    procedure EmitOP_WIDESTRING_FROM_VARIANT_64;
    procedure EmitOP_UNICSTRING_FROM_VARIANT;
    procedure EmitOP_UNICSTRING_FROM_VARIANT_64;
{$IFNDEF PAXARM}
    procedure EmitOP_SHORTSTRING_FROM_VARIANT;
    procedure EmitOP_SHORTSTRING_FROM_VARIANT_64;
{$ENDIF}
    procedure EmitOP_DOUBLE_FROM_VARIANT;
    procedure EmitOP_DOUBLE_FROM_VARIANT_64;
    procedure EmitOP_CURRENCY_FROM_VARIANT;
    procedure EmitOP_CURRENCY_FROM_VARIANT_64;
    procedure EmitOP_SINGLE_FROM_VARIANT;
    procedure EmitOP_SINGLE_FROM_VARIANT_64;
    procedure EmitOP_EXTENDED_FROM_VARIANT;
    procedure EmitOP_EXTENDED_FROM_VARIANT_64;
    procedure EmitOP_INT_FROM_VARIANT;
    procedure EmitOP_INT_FROM_VARIANT_64;
    procedure EmitOP_INT64_FROM_VARIANT;
    procedure EmitOP_INT64_FROM_VARIANT_64;
    procedure EmitOP_BYTE_FROM_VARIANT;
    procedure EmitOP_BYTE_FROM_VARIANT_64;
    procedure EmitOP_WORD_FROM_VARIANT;
    procedure EmitOP_WORD_FROM_VARIANT_64;
    procedure EmitOP_CARDINAL_FROM_VARIANT;
    procedure EmitOP_CARDINAL_FROM_VARIANT_64;
    procedure EmitOP_BOOL_FROM_VARIANT;
    procedure EmitOP_BOOL_FROM_VARIANT_64;
    procedure EmitOP_BYTEBOOL_FROM_VARIANT;
    procedure EmitOP_BYTEBOOL_FROM_VARIANT_64;
    procedure EmitOP_WORDBOOL_FROM_VARIANT;
    procedure EmitOP_WORDBOOL_FROM_VARIANT_64;
    procedure EmitOP_LONGBOOL_FROM_VARIANT;
    procedure EmitOP_LONGBOOL_FROM_VARIANT_64;
    procedure EmitOP_SMALLINT_FROM_VARIANT;
    procedure EmitOP_SMALLINT_FROM_VARIANT_64;
    procedure EmitOP_SHORTINT_FROM_VARIANT;
    procedure EmitOP_SHORTINT_FROM_VARIANT_64;

    procedure EmitOP_BOOL_FROM_BYTEBOOL;
    procedure EmitOP_BOOL_FROM_BYTEBOOL_64;
    procedure EmitOP_BOOL_FROM_WORDBOOL;
    procedure EmitOP_BOOL_FROM_WORDBOOL_64;
    procedure EmitOP_BOOL_FROM_LONGBOOL;
    procedure EmitOP_BOOL_FROM_LONGBOOL_64;

    procedure EmitOP_NEG_VARIANT;
    procedure EmitOP_NEG_VARIANT_64;
    procedure EmitOP_ABS_VARIANT;
    procedure EmitOP_ABS_VARIANT_64;
    procedure EmitOP_NOT_VARIANT;
    procedure EmitOP_NOT_VARIANT_64;
    procedure EmitOP_ADD_VARIANT;
    procedure EmitOP_ADD_VARIANT_64;
    procedure EmitOP_SUB_VARIANT;
    procedure EmitOP_SUB_VARIANT_64;
    procedure EmitOP_MULT_VARIANT;
    procedure EmitOP_MULT_VARIANT_64;
    procedure EmitOP_DIV_VARIANT;
    procedure EmitOP_DIV_VARIANT_64;
    procedure EmitOP_IDIV_VARIANT;
    procedure EmitOP_IDIV_VARIANT_64;
    procedure EmitOP_MOD_VARIANT;
    procedure EmitOP_MOD_VARIANT_64;
    procedure EmitOP_SHL_VARIANT;
    procedure EmitOP_SHL_VARIANT_64;
    procedure EmitOP_SHR_VARIANT;
    procedure EmitOP_SHR_VARIANT_64;
    procedure EmitOP_AND_VARIANT;
    procedure EmitOP_AND_VARIANT_64;
    procedure EmitOP_OR_VARIANT;
    procedure EmitOP_OR_VARIANT_64;
    procedure EmitOP_XOR_VARIANT;
    procedure EmitOP_XOR_VARIANT_64;
    procedure EmitOP_LT_VARIANT;
    procedure EmitOP_LT_VARIANT_64;
    procedure EmitOP_LE_VARIANT;
    procedure EmitOP_LE_VARIANT_64;
    procedure EmitOP_GT_VARIANT;
    procedure EmitOP_GT_VARIANT_64;
    procedure EmitOP_GE_VARIANT;
    procedure EmitOP_GE_VARIANT_64;
    procedure EmitOP_EQ_VARIANT;
    procedure EmitOP_EQ_VARIANT_64;
    procedure EmitOP_NE_VARIANT;
    procedure EmitOP_NE_VARIANT_64;

    procedure EmitOP_VARARRAY_GET;
    procedure EmitOP_VARARRAY_GET_64;
    procedure EmitOP_VARARRAY_PUT;
    procedure EmitOP_VARARRAY_PUT_64;

    procedure EmitOP_OLE_GET;
    procedure EmitOP_OLE_GET_64;
    procedure EmitOP_OLE_SET;
    procedure EmitOP_OLE_SET_64;
    procedure EmitOP_OLE_PARAM;
    procedure EmitOP_OLE_PARAM_64;

    procedure EmitOP_GENERAL_GET;
    procedure EmitOP_GENERAL_GET_64;
    procedure EmitOP_GENERAL_PUT;
    procedure EmitOP_GENERAL_PUT_64;

    procedure EmitOP_ONCREATE_HOST_OBJECT;
    procedure EmitOP_ONCREATE_HOST_OBJECT_64;
    procedure EmitOP_ONDESTROY_HOST_OBJECT;
    procedure EmitOP_ONDESTROY_HOST_OBJECT_64;

    procedure EmitOP_BEFORE_CALL_HOST;
    procedure EmitOP_AFTER_CALL_HOST;
    procedure EmitOP_BEFORE_CALL_HOST_64;
    procedure EmitOP_AFTER_CALL_HOST_64;

    procedure EmitOP_INIT_FWARRAY;
    procedure EmitOP_INIT_FWARRAY_64;

    procedure EmitOP_ONCREATE_OBJECT;
    procedure EmitOP_ONCREATE_OBJECT_64;
    procedure EmitOP_ON_AFTER_OBJECT_CREATION;
    procedure EmitOP_ON_AFTER_OBJECT_CREATION_64;

    procedure EmitOP_IS;
    procedure EmitOP_IS_64;
    procedure EmitOP_CLASSNAME;
    procedure EmitOP_CLASSNAME_64;
    procedure EmitOP_TYPEINFO;
    procedure EmitOP_TYPEINFO_64;

    procedure EmitOP_PUSH_CONTEXT;
    procedure EmitOP_PUSH_CONTEXT_64;
    procedure EmitOP_POP_CONTEXT;
    procedure EmitOP_POP_CONTEXT_64;
    procedure EmitOP_FIND_CONTEXT;
    procedure EmitOP_FIND_CONTEXT_64;
    procedure EmitOP_FIND_JS_FUNC;
    procedure EmitOP_FIND_JS_FUNC_64;

    procedure EmitOP_GET_PROG;
    procedure EmitOP_GET_PROG_64;

    procedure EmitOP_GET_DRTTI_PROP;
    procedure EmitOP_GET_DRTTI_PROP_64;
    procedure EmitOP_SET_DRTTI_PROP;
    procedure EmitOP_SET_DRTTI_PROP_64;

    procedure EmitOP_GET_ANSISTR_PROP;
    procedure EmitOP_GET_ANSISTR_PROP_64;
    procedure EmitOP_SET_ANSISTR_PROP;
    procedure EmitOP_SET_ANSISTR_PROP_64;

    procedure EmitOP_GET_WIDESTR_PROP;
    procedure EmitOP_GET_WIDESTR_PROP_64;
    procedure EmitOP_SET_WIDESTR_PROP;
    procedure EmitOP_SET_WIDESTR_PROP_64;

    procedure EmitOP_GET_UNICSTR_PROP;
    procedure EmitOP_GET_UNICSTR_PROP_64;
    procedure EmitOP_SET_UNICSTR_PROP;
    procedure EmitOP_SET_UNICSTR_PROP_64;

    procedure EmitOP_GET_ORD_PROP;
    procedure EmitOP_GET_ORD_PROP_64;
    procedure EmitOP_SET_ORD_PROP;
    procedure EmitOP_SET_ORD_PROP_64;

    procedure EmitOP_GET_INTERFACE_PROP;
    procedure EmitOP_GET_INTERFACE_PROP_64;
    procedure EmitOP_SET_INTERFACE_PROP;
    procedure EmitOP_SET_INTERFACE_PROP_64;

    procedure EmitOP_GET_SET_PROP;
    procedure EmitOP_GET_SET_PROP_64;
    procedure EmitOP_SET_SET_PROP;
    procedure EmitOP_SET_SET_PROP_64;

    procedure EmitOP_GET_FLOAT_PROP;
    procedure EmitOP_GET_FLOAT_PROP_64;
    procedure EmitOP_SET_FLOAT_PROP;
    procedure EmitOP_SET_FLOAT_PROP_64;

    procedure EmitOP_GET_VARIANT_PROP;
    procedure EmitOP_GET_VARIANT_PROP_64;
    procedure EmitOP_SET_VARIANT_PROP;
    procedure EmitOP_SET_VARIANT_PROP_64;

    procedure EmitOP_GET_INT64_PROP;
    procedure EmitOP_GET_INT64_PROP_64;
    procedure EmitOP_SET_INT64_PROP;
    procedure EmitOP_SET_INT64_PROP_64;

    procedure EmitOP_GET_EVENT_PROP;
    procedure EmitOP_GET_EVENT_PROP_64;
    procedure EmitOP_SET_EVENT_PROP;
    procedure EmitOP_SET_EVENT_PROP_64;
    procedure EmitOP_SET_EVENT_PROP2;
    procedure EmitOP_SET_EVENT_PROP2_64;

    procedure EmitOP_TRY_ON;
    procedure EmitOP_TRY_ON_64;
    procedure EmitOP_TRY_OFF;
    procedure EmitOP_TRY_OFF_64;

    procedure EmitOP_EXCEPT_SEH;
    procedure EmitOP_EXCEPT_SEH_64;

    procedure EmitOP_FINALLY;
    procedure EmitOP_FINALLY_64;
    procedure EmitOP_EXCEPT;
    procedure EmitOP_EXCEPT_64;
    procedure EmitOP_EXCEPT_ON;
    procedure EmitOP_EXCEPT_ON_64;
    procedure EmitOP_RAISE;
    procedure EmitOP_RAISE_64;
    procedure EmitOP_EXIT;
    procedure EmitOP_EXIT_64;
    procedure EmitOP_COND_RAISE;
    procedure EmitOP_COND_RAISE_64;
    procedure EmitOP_BEGIN_EXCEPT_BLOCK;
    procedure EmitOP_BEGIN_EXCEPT_BLOCK_64;
    procedure EmitOP_END_EXCEPT_BLOCK;
    procedure EmitOP_END_EXCEPT_BLOCK_64;

    procedure EmitOP_OVERFLOW_CHECK;
    procedure EmitOP_OVERFLOW_CHECK_64;

    procedure EmitOP_PAUSE;
    procedure EmitOP_PAUSE_64;
    procedure EmitOP_CHECK_PAUSE;
    procedure EmitOP_CHECK_PAUSE_64;
    procedure EmitOP_CHECK_PAUSE_LIGHT;
    procedure EmitOP_CHECK_PAUSE_LIGHT_64;
    procedure EmitOP_HALT;
    procedure EmitOP_HALT_64;

    procedure EmitOP_CHECK_INIT_ONLY;
    procedure EmitOP_CHECK_BODY_ONLY;

    procedure EmitOP_CREATE_OBJECT;
    procedure EmitOP_CREATE_OBJECT_64;
    procedure EmitOP_DESTROY_OBJECT;
    procedure EmitOP_DESTROY_OBJECT_64;
    procedure EmitOP_GET_VMT_ADDRESS;
    procedure EmitOP_GET_VMT_ADDRESS_64;

    procedure EmitJmp;
    procedure EmitOP_EQ_EVENT_64;
    procedure EmitOP_NE_EVENT_64;

    procedure EmitFLD(S: TSymbolRec);
    procedure EmitFSTP(S: TSymbolRec);

    procedure EmitFild(S: TSymbolRec);
    procedure EmitFistp(S: TSymbolRec);

    procedure EmitFDiv_10000;
    procedure EmitFMul_10000;

    procedure EmitLoadAddress(Reg: Integer; S: TSymbolRec);

    function HasTheSameAddressRegister(S1, S2: TSymbolRec): Boolean; // see next method
    function EmitGetAddressRegister(S: TSymbolRec): Integer; // this method
                   // returns a register.
                   // If it returns ESI or EBP, true address = result + S.Shift !!
                   // otherwise, address = result
                   //   Caller must free the register !!

    procedure EmitLoadIntVal(Reg: Integer; S: TSymbolRec);
    procedure EmitSaveIntVal(Reg: Integer; S: TSymbolRec);
    procedure EmitRestoreEBP(Reg: Integer; S: TSymbolRec);
    procedure EmitRestoreEBP_64(Reg: Integer; S: TSymbolRec);

    procedure EmitPut_REG(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                          // Reg contains a 32-bit value
                          // S - destination
    procedure EmitPut_REG_64(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                          // Reg contains a 32-bit value
                          // S - destination

    procedure EmitGet_REG(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                          // S - source
                          // Reg - destination
    procedure EmitGet_REG_64(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                          // S - source
                          // Reg - destination

    procedure EmitLabel(LabelId: Integer; const LabelName: String);

    procedure RaiseError(const Message: string; params: array of Const);
    procedure CreateError(const Message: string; params: array of Const);

    function Host1: Boolean;
    function Host2: Boolean;

    function ByRef1: Boolean;

    function GetReg: Integer; overload;
    function GetReg(Reg: Integer): integer; overload;
    function GetRegEx: Integer;
    function GetReg64: Integer;

    procedure FreeReg(Reg: Integer);
    function ImmValue1: Cardinal;
    function ImmValue2: Cardinal;

    function GetOffset(S: TSymbolRec): Integer;

    function SymbolRec1: TSymbolRec;
    function SymbolRec2: TSymbolRec;
    function SymbolRecR: TSymbolRec;

    procedure Emit(I: Integer);

    property OperName: String read GetOperName;
    property Language: Integer read GetLanguage;
  public
    constructor Create(akernel: Pointer);
    destructor Destroy; override;
    function CreateSymbolProgram(i_kernel: Pointer): TSymbolProg;
    procedure CopyContextStack(AStack: TIntegerStack);
    property SizeOfPointer: Integer read GetSizeOfPointer;
    property TargetPlatform: TTargetPlatform read GetTargetPlatform;
  end;

procedure EmitProgProc(akernel, aprog: Pointer; context: Pointer = nil);

implementation

uses
  PAXCOMP_BASERUNNER,
  PAXCOMP_PROG,
  PAXCOMP_KERNEL,
  PAXCOMP_STDLIB;

{$IFDEF TRIAL}
var
  _Counter: Integer = 3;
{$ENDIF}

constructor TRegisters.Create(aPAX64: Boolean);
var
  I, K: Integer;
begin
  PAX64 := aPAX64;
  if PAX64 then
    K := _R15
  else
    K := _EDI;
  for I:=_NOREG to K do
    FreeReg(I);
  A[_ESI] := true;
  A[_EDI] := true;
  A[_ESP] := true;
  A[_EBP] := true;
  A[_R12] := true;
  A[_R13] := true;
end;

function TRegisters.GetReg: Integer;
var
  I, K: Integer;
begin
  if PAX64 then
    K := _R9
  else
    K := _EDI;
  for I:=_EAX to K do
    if A[I] then
    begin
      A[I] := false;
      result := I;
      Exit;
    end;
  raise Exception.Create(errInternalError);
end;

function TRegisters.GetReg64: Integer;
var
  I: Integer;
begin
  for I := _R10 to _R15 do
    if A[I] then
    begin
      A[I] := false;
      result := I;
      Exit;
    end;
  raise Exception.Create(errInternalError);
end;

procedure TRegisters.GetReg(Reg: Integer);
begin
  if A[Reg] then
    A[Reg] := false
  else
  begin
    raise Exception.Create(errInternalError);
  end;
end;

procedure TRegisters.FreeReg(Reg: Integer);
begin
  A[Reg] := true;
end;

constructor TEmitter.Create(akernel: Pointer);
begin
  inherited Create;
  kernel := akernel;
  Registers := TRegisters.Create(TargetPlatform = tpWIN64);
  ContextStack := TIntegerStack.Create;
  List1 := TList.Create;
  List2 := TList.Create;
  List3 := TList.Create;
  OverflowCheck := true;

  CreateEmitProcList;
end;

destructor TEmitter.Destroy;
begin
  FreeAndNil(Registers);
  FreeAndNil(ContextStack);
  FreeAndNil(List1);
  FreeAndNil(List2);
  FreeAndNil(List3);
  inherited;
end;

procedure TEmitter.EmitNotImpl;
begin
  RaiseError(errInternalError, []);
end;

procedure TEmitter.EmitNothing;
begin
end;

procedure TEmitter.CreateEmitProcList;
var
  I: Integer;
begin
  SetLength(EmitList, - OP_DUMMY);

  for I:=0 to System.Length(EmitList) - 1 do
    EmitList[I] := EmitNotImpl;

  EmitList[ - OP_EMIT_ON ] := EmitOP_EMIT_ON;
  EmitList[ - OP_EMIT_OFF] := EmitOP_EMIT_OFF;
  EmitList[ - OP_NOP ] := EmitNothing;
  EmitList[ - OP_ADD_COMMENT ] := EmitComment;
  EmitList[ - OP_BEGIN_NAMESPACE  ] := EmitNothing;
  EmitList[ - OP_END_NAMESPACE  ] := EmitNothing;
  EmitList[ - OP_GO  ] := EmitOP_GO;
  EmitList[ - OP_GO_1  ] := EmitOP_GO_1;
  EmitList[ - OP_GO_2  ] := EmitOP_GO_2;
  EmitList[ - OP_GO_3  ] := EmitOP_GO_3;
  EmitList[ - OP_GO_TRUE  ] := EmitOP_GO_TRUE;
  EmitList[ - OP_GO_FALSE  ] := EmitOP_GO_FALSE;
  EmitList[ - OP_GO_DL  ] := EmitOP_GO_DL;
  EmitList[ - OP_SAVE_EDX  ] := EmitOP_SAVE_EDX;
  EmitList[ - OP_RESTORE_EDX  ] := EmitOP_RESTORE_EDX;
  EmitList[ - OP_ASSIGN_BYTE_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_BYTE_M  ] := EmitOP_ASSIGN_INT_M;
  EmitList[ - OP_ASSIGN_WORD_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_WORD_M  ] := EmitOP_ASSIGN_INT_M;
  EmitList[ - OP_ASSIGN_CARDINAL_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_CARDINAL_M  ] := EmitOP_ASSIGN_INT_M;
  EmitList[ - OP_ASSIGN_SMALLINT_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_SMALLINT_M  ] := EmitOP_ASSIGN_INT_M;
  EmitList[ - OP_ASSIGN_SHORTINT_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_SHORTINT_M  ] := EmitOP_ASSIGN_INT_M;
  EmitList[ - OP_ASSIGN_INT_I  ] := EmitOP_ASSIGN_INT_I;
  EmitList[ - OP_ASSIGN_INT_M  ] := EmitOP_ASSIGN_INT_M;
{$IFNDEF PAXARM}
  EmitList[ - OP_ASSIGN_PANSICHAR  ] := EmitOP_ASSIGN_PANSICHAR;
{$ENDIF}
  EmitList[ - OP_ASSIGN_PWIDECHAR  ] := EmitOP_ASSIGN_PWIDECHAR;
  EmitList[ - OP_ASSIGN_EVENT  ] := EmitOP_ASSIGN_EVENT;
  EmitList[ - OP_CREATE_EVENT  ] := EmitOP_CREATE_EVENT;
  EmitList[ - OP_ASSIGN_DOUBLE  ] := EmitOP_ASSIGN_DOUBLE;
  EmitList[ - OP_ASSIGN_CURRENCY  ] := EmitOP_ASSIGN_CURRENCY;
  EmitList[ - OP_ASSIGN_SINGLE  ] := EmitOP_ASSIGN_SINGLE;
  EmitList[ - OP_ASSIGN_EXTENDED  ] := EmitOP_ASSIGN_EXTENDED;
  EmitList[ - OP_ASSIGN_INT64  ] := EmitOP_ASSIGN_INT64;
  EmitList[ - OP_ASSIGN_UINT64  ] := EmitOP_ASSIGN_INT64;
  EmitList[ - OP_ASSIGN_RECORD  ] := EmitOP_ASSIGN_RECORD;
  EmitList[ - OP_ASSIGN_ARRAY  ] := EmitOP_ASSIGN_RECORD;
  EmitList[ - OP_ASSIGN_INTERFACE  ] := EmitOP_ASSIGN_INTERFACE;
  EmitList[ - OP_ADD_INT64  ] := EmitOP_ADD_INT64;
  EmitList[ - OP_SUB_INT64  ] := EmitOP_SUB_INT64;
  EmitList[ - OP_AND_INT64  ] := EmitOP_AND_INT64;
  EmitList[ - OP_OR_INT64  ] := EmitOP_OR_INT64;
  EmitList[ - OP_XOR_INT64  ] := EmitOP_XOR_INT64;
  EmitList[ - OP_ADD_UINT64  ] := EmitOP_ADD_UINT64;
  EmitList[ - OP_SUB_UINT64  ] := EmitOP_SUB_UINT64;
  EmitList[ - OP_AND_UINT64  ] := EmitOP_AND_UINT64;
  EmitList[ - OP_OR_UINT64  ] := EmitOP_OR_UINT64;
  EmitList[ - OP_XOR_UINT64  ] := EmitOP_XOR_UINT64;
  EmitList[ - OP_LT_INT64  ] := EmitOP_LT_INT64;
  EmitList[ - OP_LE_INT64  ] := EmitOP_LE_INT64;
  EmitList[ - OP_GT_INT64  ] := EmitOP_GT_INT64;
  EmitList[ - OP_GE_INT64  ] := EmitOP_GE_INT64;
  EmitList[ - OP_EQ_INT64  ] := EmitOP_EQ_INT64;
  EmitList[ - OP_NE_INT64  ] := EmitOP_NE_INT64;
  EmitList[ - OP_LT_UINT64  ] := EmitOP_LT_UINT64;
  EmitList[ - OP_LE_UINT64  ] := EmitOP_LE_UINT64;
  EmitList[ - OP_GT_UINT64  ] := EmitOP_GT_UINT64;
  EmitList[ - OP_GE_UINT64  ] := EmitOP_GE_UINT64;
  EmitList[ - OP_EQ_STRUCT  ] := EmitOP_EQ_STRUCT;
  EmitList[ - OP_NE_STRUCT  ] := EmitOP_NE_STRUCT;
  EmitList[ - OP_EQ_EVENT  ] := EmitOP_EQ_INT64;
  EmitList[ - OP_NE_EVENT  ] := EmitOP_NE_INT64;
  EmitList[ - OP_ADD_CURRENCY  ] := EmitOP_ADD_CURRENCY;
  EmitList[ - OP_SUB_CURRENCY  ] := EmitOP_SUB_CURRENCY;
  EmitList[ - OP_MUL_CURRENCY  ] := EmitOP_MUL_CURRENCY;
  EmitList[ - OP_DIV_CURRENCY  ] := EmitOP_DIV_CURRENCY;
  EmitList[ - OP_ADD_INT_MI  ] := EmitOP_ADD_INT_MI;
  EmitList[ - OP_ADD_INT_MM  ] := EmitOP_ADD_INT_MM;
  EmitList[ - OP_ADD_DOUBLE  ] := EmitOP_ADD_DOUBLE;
  EmitList[ - OP_ADD_SINGLE  ] := EmitOP_ADD_SINGLE;
  EmitList[ - OP_ADD_EXTENDED  ] := EmitOP_ADD_EXTENDED;
  EmitList[ - OP_NEG_INT  ] := EmitOP_NEG_INT;
  EmitList[ - OP_NEG_INT64  ] := EmitOP_NEG_INT64;
  EmitList[ - OP_NEG_UINT64  ] := EmitOP_NEG_INT64;
  EmitList[ - OP_NOT  ] := EmitOP_NOT;
  EmitList[ - OP_NOT_BOOL  ] := EmitOP_NOT_BOOL;
  EmitList[ - OP_NOT_BYTEBOOL  ] := EmitOP_NOT_BYTEBOOL;
  EmitList[ - OP_NOT_WORDBOOL  ] := EmitOP_NOT_WORDBOOL;
  EmitList[ - OP_NOT_LONGBOOL  ] := EmitOP_NOT_LONGBOOL;
  EmitList[ - OP_NEG_DOUBLE  ] := EmitOP_NEG_DOUBLE;
  EmitList[ - OP_NEG_CURRENCY  ] := EmitOP_NEG_CURRENCY;
  EmitList[ - OP_NEG_SINGLE  ] := EmitOP_NEG_SINGLE;
  EmitList[ - OP_NEG_EXTENDED  ] := EmitOP_NEG_EXTENDED;
  EmitList[ - OP_ABS_INT  ] := EmitOP_ABS_INT;
  EmitList[ - OP_ABS_DOUBLE  ] := EmitOP_ABS_DOUBLE;
  EmitList[ - OP_ABS_SINGLE  ] := EmitOP_ABS_SINGLE;
  EmitList[ - OP_ABS_EXTENDED  ] := EmitOP_ABS_EXTENDED;
  EmitList[ - OP_ABS_CURRENCY  ] := EmitOP_ABS_CURRENCY;
  EmitList[ - OP_SUB_INT_MI  ] := EmitOP_SUB_INT_MI;
  EmitList[ - OP_SUB_INT_MM  ] := EmitOP_SUB_INT_MM;
  EmitList[ - OP_SUB_DOUBLE  ] := EmitOP_SUB_DOUBLE;
  EmitList[ - OP_SUB_SINGLE  ] := EmitOP_SUB_SINGLE;
  EmitList[ - OP_SUB_EXTENDED  ] := EmitOP_SUB_EXTENDED;
  EmitList[ - OP_IMUL_INT_MI  ] := EmitOP_IMUL_INT_MI;
  EmitList[ - OP_IMUL_INT_MM  ] := EmitOP_IMUL_INT_MM;
  EmitList[ - OP_MUL_DOUBLE  ] := EmitOP_MUL_DOUBLE;
  EmitList[ - OP_MUL_SINGLE  ] := EmitOP_MUL_SINGLE;
  EmitList[ - OP_MUL_EXTENDED  ] := EmitOP_MUL_EXTENDED;
  EmitList[ - OP_IDIV_INT_MI  ] := EmitOP_IDIV_INT_MI;
  EmitList[ - OP_IDIV_INT_MM  ] := EmitOP_IDIV_INT_MM;
  EmitList[ - OP_IDIV_INT_IM  ] := EmitOP_IDIV_INT_IM;
  EmitList[ - OP_DIV_DOUBLE  ] := EmitOP_DIV_DOUBLE;
  EmitList[ - OP_DIV_SINGLE  ] := EmitOP_DIV_SINGLE;
  EmitList[ - OP_DIV_EXTENDED  ] := EmitOP_DIV_EXTENDED;
  EmitList[ - OP_MOD_INT_MI  ] := EmitOP_MOD_INT_MI;
  EmitList[ - OP_MOD_INT_MM  ] := EmitOP_MOD_INT_MM;
  EmitList[ - OP_MOD_INT_IM  ] := EmitOP_MOD_INT_IM;
  EmitList[ - OP_SHL_INT_MI  ] := EmitOP_SHL_INT_MI;
  EmitList[ - OP_SHL_INT_MM  ] := EmitOP_SHL_INT_MM;
  EmitList[ - OP_SHL_INT_IM  ] := EmitOP_SHL_INT_IM;
  EmitList[ - OP_SHR_INT_MI  ] := EmitOP_SHR_INT_MI;
  EmitList[ - OP_SHR_INT_MM  ] := EmitOP_SHR_INT_MM;
  EmitList[ - OP_SHR_INT_IM  ] := EmitOP_SHR_INT_IM;
  EmitList[ - OP_AND_INT_MI  ] := EmitOP_AND_INT_MI;
  EmitList[ - OP_AND_INT_MM  ] := EmitOP_AND_INT_MM;
  EmitList[ - OP_OR_INT_MI  ] := EmitOP_OR_INT_MI;
  EmitList[ - OP_OR_INT_MM  ] := EmitOP_OR_INT_MM;
  EmitList[ - OP_XOR_INT_MI  ] := EmitOP_XOR_INT_MI;
  EmitList[ - OP_XOR_INT_MM  ] := EmitOP_XOR_INT_MM;
  EmitList[ - OP_LT_INT_MI  ] := EmitOP_LT_INT_MI;
  EmitList[ - OP_LT_INT_MM  ] := EmitOP_LT_INT_MM;
  EmitList[ - OP_LE_INT_MI  ] := EmitOP_LE_INT_MI;
  EmitList[ - OP_LE_INT_MM  ] := EmitOP_LE_INT_MM;
  EmitList[ - OP_GT_INT_MI  ] := EmitOP_GT_INT_MI;
  EmitList[ - OP_GT_INT_MM  ] := EmitOP_GT_INT_MM;
  EmitList[ - OP_GE_INT_MI  ] := EmitOP_GE_INT_MI;
  EmitList[ - OP_GE_INT_MM  ] := EmitOP_GE_INT_MM;
  EmitList[ - OP_EQ_INT_MI  ] := EmitOP_EQ_INT_MI;
  EmitList[ - OP_EQ_INT_MM  ] := EmitOP_EQ_INT_MM;
  EmitList[ - OP_NE_INT_MI  ] := EmitOP_NE_INT_MI;
  EmitList[ - OP_NE_INT_MM  ] := EmitOP_NE_INT_MM;
  EmitList[ - OP_LT_DOUBLE  ] := EmitOP_LT_DOUBLE;
  EmitList[ - OP_LE_DOUBLE  ] := EmitOP_LE_DOUBLE;
  EmitList[ - OP_GT_DOUBLE  ] := EmitOP_GT_DOUBLE;
  EmitList[ - OP_GE_DOUBLE  ] := EmitOP_GE_DOUBLE;
  EmitList[ - OP_EQ_DOUBLE  ] := EmitOP_EQ_DOUBLE;
  EmitList[ - OP_NE_DOUBLE  ] := EmitOP_NE_DOUBLE;
  EmitList[ - OP_LT_CURRENCY  ] := EmitOP_LT_CURRENCY;
  EmitList[ - OP_LE_CURRENCY  ] := EmitOP_LE_CURRENCY;
  EmitList[ - OP_GT_CURRENCY  ] := EmitOP_GT_CURRENCY;
  EmitList[ - OP_GE_CURRENCY  ] := EmitOP_GE_CURRENCY;
  EmitList[ - OP_EQ_CURRENCY  ] := EmitOP_EQ_CURRENCY;
  EmitList[ - OP_NE_CURRENCY  ] := EmitOP_NE_CURRENCY;
  EmitList[ - OP_LT_SINGLE  ] := EmitOP_LT_SINGLE;
  EmitList[ - OP_LE_SINGLE  ] := EmitOP_LE_SINGLE;
  EmitList[ - OP_GT_SINGLE  ] := EmitOP_GT_SINGLE;
  EmitList[ - OP_GE_SINGLE  ] := EmitOP_GE_SINGLE;
  EmitList[ - OP_EQ_SINGLE  ] := EmitOP_EQ_SINGLE;
  EmitList[ - OP_NE_SINGLE  ] := EmitOP_NE_SINGLE;
  EmitList[ - OP_LT_EXTENDED  ] := EmitOP_LT_EXTENDED;
  EmitList[ - OP_LE_EXTENDED  ] := EmitOP_LE_EXTENDED;
  EmitList[ - OP_GT_EXTENDED  ] := EmitOP_GT_EXTENDED;
  EmitList[ - OP_GE_EXTENDED  ] := EmitOP_GE_EXTENDED;
  EmitList[ - OP_EQ_EXTENDED  ] := EmitOP_EQ_EXTENDED;
  EmitList[ - OP_NE_EXTENDED  ] := EmitOP_NE_EXTENDED;
  EmitList[ - OP_EXPORTS  ] := EmitOP_EXPORTS;
  EmitList[ - OP_PUSH_PROG  ] := EmitOP_PUSH_PROG;
  EmitList[ - OP_PUSH_ADDRESS  ] := EmitOP_PUSH_ADDRESS;
  EmitList[ - OP_PUSH_STRUCTURE  ] := EmitOP_PUSH_STRUCTURE;
  EmitList[ - OP_PUSH_SET  ] := EmitOP_PUSH_SET;
  EmitList[ - OP_PUSH_BYTE_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_BYTE  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_WORD_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_WORD  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_CARDINAL_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_CARDINAL  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_SMALLINT_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_SMALLINT  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_SHORTINT_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_SHORTINT  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_INT_IMM  ] := EmitOP_PUSH_INT_IMM;
  EmitList[ - OP_PUSH_INT  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_PTR  ] := EmitOP_PUSH_INT;
  EmitList[ - OP_PUSH_INST  ] := EmitOP_PUSH_INST;
  EmitList[ - OP_PUSH_CLSREF  ] := EmitOP_PUSH_CLSREF;
  EmitList[ - OP_UPDATE_INSTANCE  ] := EmitOP_UPDATE_INSTANCE;
  EmitList[ - OP_CLEAR_EDX  ] := EmitOP_CLEAR_EDX;
  EmitList[ - OP_PUSH_DYNARRAY  ] := EmitOP_PUSH_DYNARRAY;
  EmitList[ - OP_PUSH_OPENARRAY  ] := EmitOP_PUSH_OPENARRAY;
  EmitList[ - OP_PUSH_DATA  ] := EmitOP_PUSH_DATA;
  EmitList[ - OP_PUSH_EVENT  ] := EmitOP_PUSH_EVENT;
  EmitList[ - OP_PUSH_INT64  ] := EmitOP_PUSH_INT64;
  EmitList[ - OP_PUSH_DOUBLE  ] := EmitOP_PUSH_DOUBLE;
  EmitList[ - OP_PUSH_CURRENCY  ] := EmitOP_PUSH_CURRENCY;
  EmitList[ - OP_PUSH_SINGLE  ] := EmitOP_PUSH_SINGLE;
  EmitList[ - OP_PUSH_EXTENDED  ] := EmitOP_PUSH_EXTENDED;
{$IFNDEF PAXARM}
  EmitList[ - OP_PUSH_ANSISTRING  ] := EmitOP_PUSH_ANSISTRING;
  EmitList[ - OP_PUSH_WIDESTRING  ] := EmitOP_PUSH_WIDESTRING;
  EmitList[ - OP_PUSH_SHORTSTRING  ] := EmitOP_PUSH_SHORTSTRING;
  EmitList[ - OP_PUSH_PANSICHAR_IMM  ] := EmitOP_PUSH_PANSICHAR_IMM;
{$ENDIF}
  EmitList[ - OP_PUSH_UNICSTRING  ] := EmitOP_PUSH_UNICSTRING;
  EmitList[ - OP_PUSH_PWIDECHAR_IMM  ] := EmitOP_PUSH_PWIDECHAR_IMM;
  EmitList[ - OP_BEGIN_CALL  ] := EmitOP_BEGIN_CALL;
  EmitList[ - OP_CALL  ] := EmitOP_CALL;
  EmitList[ - OP_INIT_SUB  ] := EmitOP_INIT_SUB;
  EmitList[ - OP_END_SUB  ] := EmitOP_END_SUB;
  EmitList[ - OP_FIN_SUB  ] := EmitOP_FIN_SUB;
  EmitList[ - OP_EPILOGUE_SUB  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_LOOP  ] := EmitPCodeOperator;
  EmitList[ - OP_EPILOGUE_LOOP  ] := EmitPCodeOperator;
  EmitList[ - OP_END_LOOP ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_GLOBAL_BLOCK  ] := EmitPCodeOperator;
  EmitList[ - OP_EPILOGUE_GLOBAL_BLOCK  ] := EmitPCodeOperator;
  EmitList[ - OP_END_GLOBAL_BLOCK  ] := EmitPCodeOperator;
  EmitList[ - OP_EXTRA_BYTECODE  ] := EmitPCodeOperator;
  EmitList[ - OP_RET  ] := EmitOP_RET;
  EmitList[ - OP_FIELD  ] := EmitOP_FIELD;
  EmitList[ - OP_GET_COMPONENT  ] := EmitOP_GET_COMPONENT;
  EmitList[ - OP_ELEM  ] := EmitOP_ELEM;
  EmitList[ - OP_PRINT_EX  ] := EmitOP_PRINT_EX;
  EmitList[ - OP_TO_FW_OBJECT  ] := EmitOP_TO_FW_OBJECT;
  EmitList[ - OP_PUSH_EBP  ] := EmitOP_PUSH_EBP;
  EmitList[ - OP_POP  ] := EmitOP_POP;
  EmitList[ - OP_SAVE_REGS  ] := EmitOP_SAVE_REGS;
  EmitList[ - OP_RESTORE_REGS  ] := EmitOP_RESTORE_REGS;
  EmitList[ - OP_INT_TO_INT64  ] := EmitOP_INT_TO_INT64;
  EmitList[ - OP_BYTE_TO_INT64  ] := EmitOP_BYTE_TO_INT64;
  EmitList[ - OP_WORD_TO_INT64  ] := EmitOP_WORD_TO_INT64;
  EmitList[ - OP_CARDINAL_TO_INT64  ] := EmitOP_CARDINAL_TO_INT64;
  EmitList[ - OP_SMALLINT_TO_INT64  ] := EmitOP_SMALLINT_TO_INT64;
  EmitList[ - OP_SHORTINT_TO_INT64  ] := EmitOP_SHORTINT_TO_INT64;
  EmitList[ - OP_INT_FROM_INT64  ] := EmitOP_INT_FROM_INT64;
  EmitList[ - OP_BYTE_FROM_INT64  ] := EmitOP_BYTE_FROM_INT64;
  EmitList[ - OP_WORD_FROM_INT64  ] := EmitOP_WORD_FROM_INT64;
  EmitList[ - OP_CARDINAL_FROM_INT64  ] := EmitOP_CARDINAL_FROM_INT64;
  EmitList[ - OP_SMALLINT_FROM_INT64  ] := EmitOP_SMALLINT_FROM_INT64;
  EmitList[ - OP_SHORTINT_FROM_INT64  ] := EmitOP_SHORTINT_FROM_INT64;

  EmitList[ - OP_INT_TO_UINT64  ] := EmitOP_INT_TO_INT64;
  EmitList[ - OP_BYTE_TO_UINT64  ] := EmitOP_BYTE_TO_INT64;
  EmitList[ - OP_WORD_TO_UINT64  ] := EmitOP_WORD_TO_INT64;
  EmitList[ - OP_CARDINAL_TO_UINT64  ] := EmitOP_CARDINAL_TO_INT64;
  EmitList[ - OP_SMALLINT_TO_UINT64  ] := EmitOP_SMALLINT_TO_INT64;
  EmitList[ - OP_SHORTINT_TO_UINT64  ] := EmitOP_SHORTINT_TO_INT64;
  EmitList[ - OP_INT_FROM_UINT64  ] := EmitOP_INT_FROM_INT64;
  EmitList[ - OP_BYTE_FROM_UINT64  ] := EmitOP_BYTE_FROM_INT64;
  EmitList[ - OP_WORD_FROM_UINT64  ] := EmitOP_WORD_FROM_INT64;
  EmitList[ - OP_CARDINAL_FROM_UINT64  ] := EmitOP_CARDINAL_FROM_INT64;
  EmitList[ - OP_SMALLINT_FROM_UINT64  ] := EmitOP_SMALLINT_FROM_INT64;
  EmitList[ - OP_SHORTINT_FROM_UINT64  ] := EmitOP_SHORTINT_FROM_INT64;

  EmitList[ - OP_INT_TO_DOUBLE  ] := EmitOP_INT_TO_DOUBLE;
  EmitList[ - OP_INT64_TO_DOUBLE  ] := EmitOP_INT64_TO_DOUBLE;
  EmitList[ - OP_UINT64_TO_DOUBLE  ] := EmitOP_INT64_TO_DOUBLE;
  EmitList[ - OP_INT_TO_SINGLE  ] := EmitOP_INT_TO_SINGLE;
  EmitList[ - OP_INT64_TO_SINGLE  ] := EmitOP_INT64_TO_SINGLE;
  EmitList[ - OP_UINT64_TO_SINGLE  ] := EmitOP_INT64_TO_SINGLE;
  EmitList[ - OP_INT_TO_EXTENDED  ] := EmitOP_INT_TO_EXTENDED;
  EmitList[ - OP_INT64_TO_EXTENDED  ] := EmitOP_INT64_TO_EXTENDED;
  EmitList[ - OP_UINT64_TO_EXTENDED  ] := EmitOP_INT64_TO_EXTENDED;
  EmitList[ - OP_MULT_INT64  ] := EmitOP_MULT_INT64;
  EmitList[ - OP_IDIV_INT64  ] := EmitOP_IDIV_INT64;
  EmitList[ - OP_MOD_INT64  ] := EmitOP_MOD_INT64;
  EmitList[ - OP_SHL_INT64  ] := EmitOP_SHL_INT64;
  EmitList[ - OP_SHR_INT64  ] := EmitOP_SHR_INT64;
  EmitList[ - OP_ABS_INT64  ] := EmitOP_ABS_INT64;
  EmitList[ - OP_CURRENCY_TO_EXTENDED  ] := EmitOP_CURRENCY_TO_EXTENDED;
  EmitList[ - OP_CURRENCY_TO_SINGLE  ] := EmitOP_CURRENCY_TO_SINGLE;
  EmitList[ - OP_DOUBLE_TO_SINGLE  ] := EmitOP_DOUBLE_TO_SINGLE;
  EmitList[ - OP_DOUBLE_TO_EXTENDED  ] := EmitOP_DOUBLE_TO_EXTENDED;
  EmitList[ - OP_SINGLE_TO_DOUBLE  ] := EmitOP_SINGLE_TO_DOUBLE;
  EmitList[ - OP_CURRENCY_TO_DOUBLE  ] := EmitOP_CURRENCY_TO_DOUBLE;
  EmitList[ - OP_SINGLE_TO_EXTENDED  ] := EmitOP_SINGLE_TO_EXTENDED;
  EmitList[ - OP_EXTENDED_TO_DOUBLE  ] := EmitOP_EXTENDED_TO_DOUBLE;
  EmitList[ - OP_EXTENDED_TO_SINGLE  ] := EmitOP_EXTENDED_TO_SINGLE;
  EmitList[ - OP_ADDRESS  ] := EmitOP_ADDRESS;
  EmitList[ - OP_TERMINAL  ] := EmitOP_TERMINAL;
  EmitList[ - OP_ADDRESS_PROG  ] := EmitOP_ADDRESS_PROG;
  EmitList[ - OP_ASSIGN_PROG  ] := EmitOP_ASSIGN_PROG;
  EmitList[ - OP_SET_INCLUDE  ] := EmitOP_SET_INCLUDE;
  EmitList[ - OP_SET_INCLUDE_INTERVAL  ] := EmitOP_SET_INCLUDE_INTERVAL;
  EmitList[ - OP_SET_EXCLUDE  ] := EmitOP_SET_EXCLUDE;
  EmitList[ - OP_SET_UNION  ] := EmitOP_SET_UNION;
  EmitList[ - OP_SET_DIFFERENCE  ] := EmitOP_SET_DIFFERENCE;
  EmitList[ - OP_SET_INTERSECTION  ] := EmitOP_SET_INTERSECTION;
  EmitList[ - OP_SET_SUBSET  ] := EmitOP_SET_SUBSET;
  EmitList[ - OP_SET_SUPERSET  ] := EmitOP_SET_SUPERSET;
  EmitList[ - OP_SET_EQUALITY  ] := EmitOP_SET_EQUALITY;
  EmitList[ - OP_SET_INEQUALITY  ] := EmitOP_SET_INEQUALITY;
  EmitList[ - OP_SET_MEMBERSHIP  ] := EmitOP_SET_MEMBERSHIP;
  EmitList[ - OP_SET_ASSIGN  ] := EmitOP_SET_ASSIGN;
  EmitList[ - OP_SET_COUNTER_ASSIGN  ] := EmitOP_SET_COUNTER_ASSIGN;
  EmitList[ - OP_ERR_ABSTRACT  ] := EmitOP_ERR_ABSTRACT;
  EmitList[ - OP_VAR_FROM_TVALUE  ] := EmitOP_VAR_FROM_TVALUE;
{$IFNDEF PAXARM}
  EmitList[ - OP_ANSISTRING_FROM_PANSICHAR  ] := EmitOP_ANSISTRING_FROM_PANSICHAR;
  EmitList[ - OP_ANSISTRING_FROM_PWIDECHAR  ] := EmitOP_ANSISTRING_FROM_PWIDECHAR;
  EmitList[ - OP_ANSISTRING_FROM_ANSICHAR  ] := EmitOP_ANSISTRING_FROM_ANSICHAR;
  EmitList[ - OP_ASSIGN_ANSISTRING  ] := EmitOP_ASSIGN_ANSISTRING;
  EmitList[ - OP_ASSIGN_SHORTSTRING  ] := EmitOP_ASSIGN_SHORTSTRING;
  EmitList[ - OP_ASSIGN_WIDESTRING  ] := EmitOP_ASSIGN_WIDESTRING;
  EmitList[ - OP_ASSIGN_UNICSTRING  ] := EmitOP_ASSIGN_UNICSTRING;
  EmitList[ - OP_ADD_ANSISTRING  ] := EmitOP_ADD_ANSISTRING;
  EmitList[ - OP_ADD_SHORTSTRING  ] := EmitOP_ADD_SHORTSTRING;
  EmitList[ - OP_ADD_WIDESTRING  ] := EmitOP_ADD_WIDESTRING;
  EmitList[ - OP_ANSISTRING_CLR  ] := EmitOP_ANSISTRING_CLR;
  EmitList[ - OP_WIDESTRING_CLR  ] := EmitOP_WIDESTRING_CLR;
{$ENDIF}
  EmitList[ - OP_ADD_UNICSTRING  ] := EmitOP_ADD_UNICSTRING;
  EmitList[ - OP_UNICSTRING_CLR  ] := EmitOP_UNICSTRING_CLR;
  EmitList[ - OP_INTERFACE_CLR  ] := EmitOP_INTERFACE_CLR;
  EmitList[ - OP_STRUCTURE_CLR  ] := EmitOP_STRUCTURE_CLR;
  EmitList[ - OP_CLASS_CLR  ] := EmitOP_CLASS_CLR;
  EmitList[ - OP_STRUCTURE_ADDREF  ] := EmitOP_STRUCTURE_ADDREF;
  EmitList[ - OP_ADDREF  ] := EmitOP_ADDREF;
  EmitList[ - OP_DYNARRAY_CLR  ] := EmitOP_DYNARRAY_CLR;
  EmitList[ - OP_DYNARRAY_HIGH  ] := EmitOP_DYNARRAY_HIGH;
  EmitList[ - OP_DYNARRAY_ASSIGN  ] := EmitOP_DYNARRAY_ASSIGN;
  EmitList[ - OP_CREATE_EMPTY_DYNARRAY  ] := EmitOP_CREATE_EMPTY_DYNARRAY;
  EmitList[ - OP_ASSIGN_TVarRec  ] := EmitOP_ASSIGN_TVarRec;
{$IFNDEF PAXARM}
  EmitList[ - OP_SHORTSTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
  EmitList[ - OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL;
  EmitList[ - OP_SHORTSTRING_FROM_ANSICHAR  ] := EmitOP_SHORTSTRING_FROM_ANSICHAR;
  EmitList[ - OP_SHORTSTRING_FROM_WIDECHAR  ] := EmitOP_SHORTSTRING_FROM_WIDECHAR;
  EmitList[ - OP_SHORTSTRING_FROM_ANSISTRING  ] := EmitOP_SHORTSTRING_FROM_ANSISTRING;
  EmitList[ - OP_SHORTSTRING_FROM_WIDESTRING  ] := EmitOP_SHORTSTRING_FROM_WIDESTRING;
  EmitList[ - OP_UNICSTRING_FROM_WIDESTRING  ] := EmitOP_UNICSTRING_FROM_WIDESTRING;
  EmitList[ - OP_SHORTSTRING_FROM_UNICSTRING  ] := EmitOP_SHORTSTRING_FROM_UNICSTRING;
  EmitList[ - OP_ANSISTRING_FROM_SHORTSTRING  ] := EmitOP_ANSISTRING_FROM_SHORTSTRING;
  EmitList[ - OP_EQ_ANSISTRING  ] := EmitOP_EQ_ANSISTRING;
  EmitList[ - OP_NE_ANSISTRING  ] := EmitOP_NE_ANSISTRING;
  EmitList[ - OP_EQ_SHORTSTRING  ] := EmitOP_EQ_SHORTSTRING;
  EmitList[ - OP_NE_SHORTSTRING  ] := EmitOP_NE_SHORTSTRING;
  EmitList[ - OP_EQ_WIDESTRING  ] := EmitOP_EQ_WIDESTRING;
  EmitList[ - OP_NE_WIDESTRING  ] := EmitOP_NE_WIDESTRING;
  EmitList[ - OP_GT_ANSISTRING  ] := EmitOP_GT_ANSISTRING;
  EmitList[ - OP_GE_ANSISTRING  ] := EmitOP_GE_ANSISTRING;
  EmitList[ - OP_LT_ANSISTRING  ] := EmitOP_LT_ANSISTRING;
  EmitList[ - OP_LE_ANSISTRING  ] := EmitOP_LE_ANSISTRING;
  EmitList[ - OP_GT_SHORTSTRING  ] := EmitOP_GT_SHORTSTRING;
  EmitList[ - OP_GE_SHORTSTRING  ] := EmitOP_GE_SHORTSTRING;
  EmitList[ - OP_LT_SHORTSTRING  ] := EmitOP_LT_SHORTSTRING;
  EmitList[ - OP_LE_SHORTSTRING  ] := EmitOP_LE_SHORTSTRING;
  EmitList[ - OP_GT_WIDESTRING  ] := EmitOP_GT_WIDESTRING;
  EmitList[ - OP_GE_WIDESTRING  ] := EmitOP_GE_WIDESTRING;
  EmitList[ - OP_LT_WIDESTRING  ] := EmitOP_LT_WIDESTRING;
  EmitList[ - OP_LE_WIDESTRING  ] := EmitOP_LE_WIDESTRING;
{$ENDIF}
  EmitList[ - OP_EQ_UNICSTRING  ] := EmitOP_EQ_UNICSTRING;
  EmitList[ - OP_NE_UNICSTRING  ] := EmitOP_NE_UNICSTRING;
  EmitList[ - OP_GT_UNICSTRING  ] := EmitOP_GT_UNICSTRING;
  EmitList[ - OP_GE_UNICSTRING  ] := EmitOP_GE_UNICSTRING;
  EmitList[ - OP_LT_UNICSTRING  ] := EmitOP_LT_UNICSTRING;
  EmitList[ - OP_LE_UNICSTRING  ] := EmitOP_LE_UNICSTRING;
  EmitList[ - OP_SHORTSTRING_HIGH  ] := EmitOP_SHORTSTRING_HIGH;
  EmitList[ - OP_LOCK_VARRAY  ] := EmitOP_LOCK_VARRAY;
  EmitList[ - OP_UNLOCK_VARRAY  ] := EmitOP_UNLOCK_VARRAY;
  EmitList[ - OP_VARIANT_CLR  ] := EmitOP_VARIANT_CLR;
{$IFNDEF PAXARM}
  EmitList[ - OP_WIDESTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL;
  EmitList[ - OP_WIDESTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL;
  EmitList[ - OP_WIDESTRING_FROM_ANSICHAR  ] := EmitOP_WIDESTRING_FROM_ANSICHAR;
  EmitList[ - OP_WIDESTRING_FROM_WIDECHAR  ] := EmitOP_WIDESTRING_FROM_WIDECHAR;
  EmitList[ - OP_ANSISTRING_FROM_WIDECHAR  ] := EmitOP_ANSISTRING_FROM_WIDECHAR;
  EmitList[ - OP_WIDESTRING_FROM_WIDECHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL;
  EmitList[ - OP_WIDESTRING_FROM_ANSISTRING  ] := EmitOP_WIDESTRING_FROM_ANSISTRING;
  EmitList[ - OP_UNICSTRING_FROM_ANSISTRING  ] := EmitOP_UNICSTRING_FROM_ANSISTRING;
  EmitList[ - OP_WIDESTRING_FROM_SHORTSTRING  ] := EmitOP_WIDESTRING_FROM_SHORTSTRING;
  EmitList[ - OP_WIDESTRING_FROM_UNICSTRING  ] := EmitOP_WIDESTRING_FROM_UNICSTRING;
  EmitList[ - OP_UNICSTRING_FROM_SHORTSTRING  ] := EmitOP_UNICSTRING_FROM_SHORTSTRING;
  EmitList[ - OP_ANSISTRING_FROM_WIDESTRING  ] := EmitOP_ANSISTRING_FROM_WIDESTRING;
  EmitList[ - OP_ANSISTRING_FROM_UNICSTRING  ] := EmitOP_ANSISTRING_FROM_UNICSTRING;
  EmitList[ - OP_UNICSTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL;
  EmitList[ - OP_UNICSTRING_FROM_ANSICHAR  ] := EmitOP_UNICSTRING_FROM_ANSICHAR;
  EmitList[ - OP_VARIANT_FROM_ANSICHAR  ] := EmitOP_VARIANT_FROM_ANSICHAR;
{$ENDIF}
  EmitList[ - OP_UNICSTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL;
  EmitList[ - OP_UNICSTRING_FROM_WIDECHAR  ] := EmitOP_UNICSTRING_FROM_WIDECHAR;
  EmitList[ - OP_UNICSTRING_FROM_WIDECHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL;
  EmitList[ - OP_VARIANT_FROM_INT  ] := EmitOP_VARIANT_FROM_INT;
  EmitList[ - OP_VARIANT_FROM_INT64  ] := EmitOP_VARIANT_FROM_INT64;
  EmitList[ - OP_VARIANT_FROM_BYTE  ] := EmitOP_VARIANT_FROM_BYTE;
  EmitList[ - OP_VARIANT_FROM_BOOL  ] := EmitOP_VARIANT_FROM_BOOL;
  EmitList[ - OP_VARIANT_FROM_WORD  ] := EmitOP_VARIANT_FROM_WORD;
  EmitList[ - OP_VARIANT_FROM_CARDINAL  ] := EmitOP_VARIANT_FROM_CARDINAL;
  EmitList[ - OP_VARIANT_FROM_SMALLINT  ] := EmitOP_VARIANT_FROM_SMALLINT;
  EmitList[ - OP_VARIANT_FROM_SHORTINT  ] := EmitOP_VARIANT_FROM_SHORTINT;
  EmitList[ - OP_VARIANT_FROM_DOUBLE  ] := EmitOP_VARIANT_FROM_DOUBLE;
  EmitList[ - OP_VARIANT_FROM_CURRENCY  ] := EmitOP_VARIANT_FROM_CURRENCY;
  EmitList[ - OP_VARIANT_FROM_SINGLE  ] := EmitOP_VARIANT_FROM_SINGLE;
  EmitList[ - OP_VARIANT_FROM_EXTENDED  ] := EmitOP_VARIANT_FROM_EXTENDED;
  EmitList[ - OP_VARIANT_FROM_INTERFACE  ] := EmitOP_VARIANT_FROM_INTERFACE;
{$IFNDEF PAXARM}
  EmitList[ - OP_VARIANT_FROM_PANSICHAR_LITERAL  ] := EmitOP_VARIANT_FROM_PANSICHAR_LITERAL;
  EmitList[ - OP_VARIANT_FROM_ANSISTRING  ] := EmitOP_VARIANT_FROM_ANSISTRING;
  EmitList[ - OP_VARIANT_FROM_WIDESTRING  ] := EmitOP_VARIANT_FROM_WIDESTRING;
  EmitList[ - OP_VARIANT_FROM_SHORTSTRING  ] := EmitOP_VARIANT_FROM_SHORTSTRING;
  EmitList[ - OP_OLEVARIANT_FROM_ANSICHAR  ] := EmitOP_OLEVARIANT_FROM_ANSICHAR;
{$ENDIF}
  EmitList[ - OP_VARIANT_FROM_PWIDECHAR_LITERAL  ] := EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL;
  EmitList[ - OP_VARIANT_FROM_UNICSTRING  ] := EmitOP_VARIANT_FROM_UNICSTRING;
  EmitList[ - OP_VARIANT_FROM_WIDECHAR  ] := EmitOP_VARIANT_FROM_WIDECHAR;
  EmitList[ - OP_VARIANT_FROM_WIDECHAR_LITERAL  ] := EmitOP_VARIANT_FROM_WIDECHAR_LITERAL;
  EmitList[ - OP_OLEVARIANT_FROM_VARIANT  ] := EmitOP_OLEVARIANT_FROM_VARIANT;
  EmitList[ - OP_OLEVARIANT_FROM_INT  ] := EmitOP_OLEVARIANT_FROM_INT;
  EmitList[ - OP_OLEVARIANT_FROM_INT64  ] := EmitOP_OLEVARIANT_FROM_INT64;
  EmitList[ - OP_OLEVARIANT_FROM_BYTE  ] :=  EmitOP_OLEVARIANT_FROM_BYTE;
  EmitList[ - OP_OLEVARIANT_FROM_BOOL  ] :=  EmitOP_OLEVARIANT_FROM_BOOL;
  EmitList[ - OP_OLEVARIANT_FROM_WORD  ] :=  EmitOP_OLEVARIANT_FROM_WORD;
  EmitList[ - OP_OLEVARIANT_FROM_CARDINAL  ] := EmitOP_OLEVARIANT_FROM_CARDINAL;
  EmitList[ - OP_OLEVARIANT_FROM_SMALLINT  ] := EmitOP_OLEVARIANT_FROM_SMALLINT;
  EmitList[ - OP_OLEVARIANT_FROM_SHORTINT  ] := EmitOP_OLEVARIANT_FROM_SHORTINT;
  EmitList[ - OP_OLEVARIANT_FROM_DOUBLE  ] := EmitOP_OLEVARIANT_FROM_DOUBLE;
  EmitList[ - OP_OLEVARIANT_FROM_CURRENCY  ] := EmitOP_OLEVARIANT_FROM_CURRENCY;
  EmitList[ - OP_OLEVARIANT_FROM_SINGLE  ] := EmitOP_OLEVARIANT_FROM_SINGLE;
  EmitList[ - OP_OLEVARIANT_FROM_EXTENDED  ] := EmitOP_OLEVARIANT_FROM_EXTENDED;
  EmitList[ - OP_OLEVARIANT_FROM_INTERFACE  ] := EmitOP_OLEVARIANT_FROM_INTERFACE;
{$IFNDEF PAXARM}
  EmitList[ - OP_OLEVARIANT_FROM_PANSICHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
  EmitList[ - OP_OLEVARIANT_FROM_ANSISTRING  ] := EmitOP_OLEVARIANT_FROM_ANSISTRING;
  EmitList[ - OP_OLEVARIANT_FROM_WIDESTRING  ] := EmitOP_OLEVARIANT_FROM_WIDESTRING;
  EmitList[ - OP_OLEVARIANT_FROM_SHORTSTRING  ] := EmitOP_OLEVARIANT_FROM_SHORTSTRING;
{$ENDIF}
  EmitList[ - OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL;
  EmitList[ - OP_OLEVARIANT_FROM_UNICSTRING  ] := EmitOP_OLEVARIANT_FROM_UNICSTRING;
  EmitList[ - OP_OLEVARIANT_FROM_WIDECHAR  ] := EmitOP_OLEVARIANT_FROM_WIDECHAR;
  EmitList[ - OP_OLEVARIANT_FROM_WIDECHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL;

  EmitList[ - OP_BEGIN_LIBRARY  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_EXPORT  ] := EmitPCodeOperator;

  // js only
{$IFNDEF PAXARM}
  EmitList[ - OP_ANSISTRING_FROM_INT  ] := EmitOP_ANSISTRING_FROM_INT;
  EmitList[ - OP_ANSISTRING_FROM_DOUBLE  ] := EmitOP_ANSISTRING_FROM_DOUBLE;
  EmitList[ - OP_ANSISTRING_FROM_SINGLE  ] := EmitOP_ANSISTRING_FROM_SINGLE;
  EmitList[ - OP_ANSISTRING_FROM_EXTENDED  ] := EmitOP_ANSISTRING_FROM_EXTENDED;
  EmitList[ - OP_ANSISTRING_FROM_BOOLEAN  ] := EmitOP_ANSISTRING_FROM_BOOLEAN;
{$ENDIF}
  EmitList[ - OP_UNICSTRING_FROM_INT  ] := EmitOP_UNICSTRING_FROM_INT;
  EmitList[ - OP_UNICSTRING_FROM_DOUBLE  ] := EmitOP_UNICSTRING_FROM_DOUBLE;
  EmitList[ - OP_UNICSTRING_FROM_SINGLE  ] := EmitOP_UNICSTRING_FROM_SINGLE;
  EmitList[ - OP_UNICSTRING_FROM_EXTENDED  ] := EmitOP_UNICSTRING_FROM_EXTENDED;
  EmitList[ - OP_UNICSTRING_FROM_BOOLEAN  ] := EmitOP_UNICSTRING_FROM_BOOLEAN;

  EmitList[ - OP_JS_FUNC_OBJ_FROM_VARIANT  ] := EmitOP_JS_FUNC_OBJ_FROM_VARIANT;
{$IFNDEF PAXARM}
  EmitList[ - OP_ANSICHAR_FROM_VARIANT  ] := EmitOP_ANSICHAR_FROM_VARIANT;
  EmitList[ - OP_ANSISTRING_FROM_VARIANT  ] := EmitOP_ANSISTRING_FROM_VARIANT;
  EmitList[ - OP_WIDESTRING_FROM_VARIANT  ] := EmitOP_WIDESTRING_FROM_VARIANT;
  EmitList[ - OP_SHORTSTRING_FROM_VARIANT  ] := EmitOP_SHORTSTRING_FROM_VARIANT;
{$ENDIF}
  EmitList[ - OP_WIDECHAR_FROM_VARIANT  ] := EmitOP_WIDECHAR_FROM_VARIANT;
  EmitList[ - OP_UNICSTRING_FROM_VARIANT  ] := EmitOP_UNICSTRING_FROM_VARIANT;
  EmitList[ - OP_DOUBLE_FROM_VARIANT  ] := EmitOP_DOUBLE_FROM_VARIANT;
  EmitList[ - OP_CURRENCY_FROM_VARIANT  ] := EmitOP_CURRENCY_FROM_VARIANT;
  EmitList[ - OP_SINGLE_FROM_VARIANT  ] := EmitOP_SINGLE_FROM_VARIANT;
  EmitList[ - OP_EXTENDED_FROM_VARIANT  ] := EmitOP_EXTENDED_FROM_VARIANT;
  EmitList[ - OP_INT_FROM_VARIANT  ] := EmitOP_INT_FROM_VARIANT;
  EmitList[ - OP_INT64_FROM_VARIANT  ] := EmitOP_INT64_FROM_VARIANT;
  EmitList[ - OP_UINT64_FROM_VARIANT  ] := EmitOP_INT64_FROM_VARIANT;
  EmitList[ - OP_BYTE_FROM_VARIANT  ] := EmitOP_BYTE_FROM_VARIANT;
  EmitList[ - OP_WORD_FROM_VARIANT  ] := EmitOP_WORD_FROM_VARIANT;
  EmitList[ - OP_CARDINAL_FROM_VARIANT  ] := EmitOP_CARDINAL_FROM_VARIANT;
  EmitList[ - OP_BOOL_FROM_VARIANT  ] := EmitOP_BOOL_FROM_VARIANT;
  EmitList[ - OP_BYTEBOOL_FROM_VARIANT  ] := EmitOP_BYTEBOOL_FROM_VARIANT;
  EmitList[ - OP_WORDBOOL_FROM_VARIANT  ] := EmitOP_WORDBOOL_FROM_VARIANT;
  EmitList[ - OP_LONGBOOL_FROM_VARIANT  ] := EmitOP_LONGBOOL_FROM_VARIANT;
  EmitList[ - OP_SMALLINT_FROM_VARIANT  ] := EmitOP_SMALLINT_FROM_VARIANT;
  EmitList[ - OP_SHORTINT_FROM_VARIANT  ] := EmitOP_SHORTINT_FROM_VARIANT;
  EmitList[ - OP_BOOL_FROM_BYTEBOOL  ] := EmitOP_BOOL_FROM_BYTEBOOL;
  EmitList[ - OP_BOOL_FROM_WORDBOOL  ] := EmitOP_BOOL_FROM_WORDBOOL;
  EmitList[ - OP_BOOL_FROM_LONGBOOL  ] := EmitOP_BOOL_FROM_LONGBOOL;
  EmitList[ - OP_NEG_VARIANT  ] := EmitOP_NEG_VARIANT;
  EmitList[ - OP_ABS_VARIANT  ] := EmitOP_ABS_VARIANT;
  EmitList[ - OP_NOT_VARIANT  ] := EmitOP_NOT_VARIANT;
  EmitList[ - OP_ADD_VARIANT  ] := EmitOP_ADD_VARIANT;
  EmitList[ - OP_SUB_VARIANT  ] := EmitOP_SUB_VARIANT;
  EmitList[ - OP_MULT_VARIANT  ] := EmitOP_MULT_VARIANT;
  EmitList[ - OP_DIV_VARIANT  ] := EmitOP_DIV_VARIANT;
  EmitList[ - OP_IDIV_VARIANT  ] := EmitOP_IDIV_VARIANT;
  EmitList[ - OP_MOD_VARIANT  ] := EmitOP_MOD_VARIANT;
  EmitList[ - OP_SHL_VARIANT  ] := EmitOP_SHL_VARIANT;
  EmitList[ - OP_SHR_VARIANT  ] := EmitOP_SHR_VARIANT;
  EmitList[ - OP_AND_VARIANT  ] := EmitOP_AND_VARIANT;
  EmitList[ - OP_OR_VARIANT  ] := EmitOP_OR_VARIANT;
  EmitList[ - OP_XOR_VARIANT  ] := EmitOP_XOR_VARIANT;
  EmitList[ - OP_LT_VARIANT  ] := EmitOP_LT_VARIANT;
  EmitList[ - OP_LE_VARIANT  ] := EmitOP_LE_VARIANT;
  EmitList[ - OP_GT_VARIANT  ] := EmitOP_GT_VARIANT;
  EmitList[ - OP_GE_VARIANT  ] := EmitOP_GE_VARIANT;
  EmitList[ - OP_EQ_VARIANT  ] := EmitOP_EQ_VARIANT;
  EmitList[ - OP_NE_VARIANT  ] := EmitOP_NE_VARIANT;
  EmitList[ - OP_CURRENCY_FROM_INT  ] := EmitOP_CURRENCY_FROM_INT;
  EmitList[ - OP_CURRENCY_FROM_INT64  ] := EmitOP_CURRENCY_FROM_INT64;
  EmitList[ - OP_CURRENCY_FROM_UINT64  ] := EmitOP_CURRENCY_FROM_INT64;
  EmitList[ - OP_CURRENCY_FROM_REAL  ] := EmitOP_CURRENCY_FROM_REAL;
  EmitList[ - OP_VARIANT_FROM_CLASS  ] := EmitOP_VARIANT_FROM_CLASS;
  EmitList[ - OP_VARIANT_FROM_POINTER  ] := EmitOP_VARIANT_FROM_POINTER;
  EmitList[ - OP_CLASS_FROM_VARIANT  ] := EmitOP_CLASS_FROM_VARIANT;
  EmitList[ - OP_INTERFACE_FROM_CLASS  ] := EmitOP_INTERFACE_FROM_CLASS;
  EmitList[ - OP_INTERFACE_CAST  ] := EmitOP_INTERFACE_CAST;
  EmitList[ - OP_ASSIGN_VARIANT  ] := EmitOP_ASSIGN_VARIANT;
  EmitList[ - OP_ASSIGN_OLEVARIANT  ] := EmitOP_ASSIGN_OLEVARIANT;
  EmitList[ - OP_ASSIGN_CLASS  ] := EmitOP_ASSIGN_CLASS;
  EmitList[ - OP_VARARRAY_GET  ] := EmitOP_VARARRAY_GET;
  EmitList[ - OP_VARARRAY_PUT  ] := EmitOP_VARARRAY_PUT;
  EmitList[ - OP_OLE_GET  ] := EmitOP_OLE_GET;
  EmitList[ - OP_OLE_SET  ] := EmitOP_OLE_SET;
  EmitList[ - OP_OLE_PARAM  ] := EmitOP_OLE_PARAM;
  EmitList[ - OP_IS  ] := EmitOP_IS;
  EmitList[ - OP_TYPEINFO  ] := EmitOP_TYPEINFO;
  EmitList[ - OP_ADD_TYPEINFO  ] := EmitPCodeOperator;
  EmitList[ - OP_PUSH_CONTEXT  ] := EmitOP_PUSH_CONTEXT;
  EmitList[ - OP_POP_CONTEXT  ] := EmitOP_POP_CONTEXT;
  EmitList[ - OP_FIND_CONTEXT  ] := EmitOP_FIND_CONTEXT;
  EmitList[ - OP_FIND_JS_FUNC  ] := EmitOP_FIND_JS_FUNC;
  EmitList[ - OP_GET_PROG  ] := EmitOP_GET_PROG;
  EmitList[ - OP_ONCREATE_HOST_OBJECT  ] := EmitOP_ONCREATE_HOST_OBJECT;
  EmitList[ - OP_ONDESTROY_HOST_OBJECT  ] := EmitOP_ONDESTROY_HOST_OBJECT;
  EmitList[ - OP_BEFORE_CALL_HOST  ] := EmitOP_BEFORE_CALL_HOST;
  EmitList[ - OP_AFTER_CALL_HOST  ] := EmitOP_AFTER_CALL_HOST;
  EmitList[ - OP_INIT_FWARRAY  ] := EmitOP_INIT_FWARRAY;
  EmitList[ - OP_ONCREATE_OBJECT  ] := EmitOP_ONCREATE_OBJECT;
  EmitList[ - OP_ON_AFTER_OBJECT_CREATION  ] := EmitOP_ON_AFTER_OBJECT_CREATION;
  EmitList[ - OP_CLASSNAME  ] := EmitOP_CLASSNAME;
  EmitList[ - OP_GET_DRTTI_PROP  ] := EmitOP_GET_DRTTI_PROP;
  EmitList[ - OP_SET_DRTTI_PROP  ] := EmitOP_SET_DRTTI_PROP;
{$IFNDEF PAXARM}
  EmitList[ - OP_GET_ANSISTR_PROP  ] := EmitOP_GET_ANSISTR_PROP;
  EmitList[ - OP_SET_ANSISTR_PROP  ] := EmitOP_SET_ANSISTR_PROP;
  EmitList[ - OP_GET_WIDESTR_PROP  ] := EmitOP_GET_WIDESTR_PROP;
  EmitList[ - OP_SET_WIDESTR_PROP  ] := EmitOP_SET_WIDESTR_PROP;
{$ENDIF}
  EmitList[ - OP_GET_UNICSTR_PROP  ] := EmitOP_GET_UNICSTR_PROP;
  EmitList[ - OP_SET_UNICSTR_PROP  ] := EmitOP_SET_UNICSTR_PROP;
  EmitList[ - OP_GET_ORD_PROP  ] := EmitOP_GET_ORD_PROP;
  EmitList[ - OP_SET_ORD_PROP  ] := EmitOP_SET_ORD_PROP;
  EmitList[ - OP_GET_INTERFACE_PROP  ] := EmitOP_GET_INTERFACE_PROP;
  EmitList[ - OP_SET_INTERFACE_PROP  ] := EmitOP_SET_INTERFACE_PROP;
  EmitList[ - OP_GET_SET_PROP  ] := EmitOP_GET_SET_PROP;
  EmitList[ - OP_SET_SET_PROP  ] := EmitOP_SET_SET_PROP;
  EmitList[ - OP_GET_FLOAT_PROP  ] := EmitOP_GET_FLOAT_PROP;
  EmitList[ - OP_SET_FLOAT_PROP  ] := EmitOP_SET_FLOAT_PROP;
  EmitList[ - OP_GET_VARIANT_PROP  ] := EmitOP_GET_VARIANT_PROP;
  EmitList[ - OP_SET_VARIANT_PROP  ] := EmitOP_SET_VARIANT_PROP;
  EmitList[ - OP_GET_INT64_PROP  ] := EmitOP_GET_INT64_PROP;
  EmitList[ - OP_SET_INT64_PROP  ] := EmitOP_SET_INT64_PROP;
  EmitList[ - OP_GET_EVENT_PROP  ] := EmitOP_GET_EVENT_PROP;
  EmitList[ - OP_SET_EVENT_PROP  ] := EmitOP_SET_EVENT_PROP;
  EmitList[ - OP_SET_EVENT_PROP2  ] := EmitOP_SET_EVENT_PROP2;
  EmitList[ - OP_TRY_ON  ] := EmitOP_TRY_ON;
  EmitList[ - OP_EXCEPT_SEH  ] := EmitOP_EXCEPT_SEH;
  EmitList[ - OP_TRY_OFF  ] := EmitOP_TRY_OFF;
  EmitList[ - OP_FINALLY  ] := EmitOP_FINALLY;
  EmitList[ - OP_EXCEPT  ] := EmitOP_EXCEPT;
  EmitList[ - OP_EXCEPT_ON  ] := EmitOP_EXCEPT_ON;
  EmitList[ - OP_RAISE  ] := EmitOP_RAISE;
  EmitList[ - OP_EXIT  ] := EmitOP_EXIT;
  EmitList[ - OP_COND_RAISE  ] := EmitOP_COND_RAISE;
  EmitList[ - OP_BEGIN_EXCEPT_BLOCK  ] := EmitOP_BEGIN_EXCEPT_BLOCK;
  EmitList[ - OP_END_EXCEPT_BLOCK  ] := EmitOP_END_EXCEPT_BLOCK;
  EmitList[ - OP_OVERFLOW_CHECK  ] := EmitOP_OVERFLOW_CHECK;
  EmitList[ - OP_PAUSE  ] := EmitOP_PAUSE;
  EmitList[ - OP_CHECK_PAUSE  ] := EmitOP_CHECK_PAUSE;
  EmitList[ - OP_CHECK_PAUSE_LIGHT  ] := EmitOP_CHECK_PAUSE_LIGHT;
  EmitList[ - OP_HALT  ] := EmitOP_HALT;
  EmitList[ - OP_CREATE_OBJECT  ] := EmitOP_CREATE_OBJECT;
  EmitList[ - OP_DESTROY_OBJECT  ] := EmitOP_DESTROY_OBJECT;
  EmitList[ - OP_GET_VMT_ADDRESS  ] := EmitOP_GET_VMT_ADDRESS;
  EmitList[ - OP_SET_LENGTH  ] := EmitOP_SET_LENGTH;
  EmitList[ - OP_SET_LENGTH_EX  ] := EmitOP_SET_LENGTH_EX;
  EmitList[ - OP_PUSH_LENGTH  ] := EmitPCodeOperator;
  EmitList[ - OP_CREATE_METHOD  ] := EmitOP_CREATE_METHOD;
  EmitList[ - OP_DECLARE_TEMP_VAR  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_USING  ] := EmitPCodeOperator;
  EmitList[ - OP_END_USING  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_BLOCK  ] := EmitPCodeOperator;
  EmitList[ - OP_END_BLOCK  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_WITH  ] := EmitPCodeOperator;
  EmitList[ - OP_END_WITH  ] := EmitPCodeOperator;
  EmitList[ - OP_PARAM_CHANGED  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_SUB  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_CLASS_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_CLASS_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_RECORD_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_RECORD_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_HELPER_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_HELPER_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_ADD_ANCESTOR  ] := EmitNothing;
{$IFNDEF PAXARM}
  EmitList[ - OP_INIT_PANSICHAR_LITERAL  ] := EmitNothing;
{$ENDIF}
  EmitList[ - OP_INIT_PWIDECHAR_LITERAL  ] := EmitNothing;
  EmitList[ - OP_BEGIN_CRT_JS_FUNC_OBJECT  ] := EmitOP_BEGIN_CRT_JS_FUNC_OBJECT;
  EmitList[ - OP_END_CRT_JS_FUNC_OBJECT  ] := EmitOP_END_CRT_JS_FUNC_OBJECT;
  EmitList[ - OP_TO_JS_OBJECT  ] := EmitOP_TO_JS_OBJECT;
  EmitList[ - OP_JS_TYPEOF  ] := EmitOP_JS_TYPEOF;
  EmitList[ - OP_JS_VOID  ] := EmitOP_JS_VOID;
  EmitList[ - OP_GET_NEXTJSPROP  ] := EmitOP_GET_NEXTJSPROP;
  EmitList[ - OP_CLEAR_REFERENCES  ] := EmitOP_CLEAR_REFERENCES;
  EmitList[ - OP_DECLARE_LOCAL_VAR  ] := EmitPCodeOperator;
  EmitList[ - OP_LOAD_PROC  ] := EmitPCodeOperator;
  EmitList[ - OP_ADD_MESSAGE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_ARRAY_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_ARRAY_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_SET_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_SET_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_ENUM_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_ENUM_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_SUBRANGE_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_SUBRANGE_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_POINTER_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_POINTER_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_CLASSREF_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_CLASSREF_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_DYNARRAY_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_DYNARRAY_TYPE  ] := EmitPCodeOperator;
{$IFNDEF PAXARM}
  EmitList[ - OP_BEGIN_SHORTSTRING_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_SHORTSTRING_TYPE  ] := EmitPCodeOperator;
{$ENDIF}
  EmitList[ - OP_BEGIN_INTERFACE_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_END_INTERFACE_TYPE  ] := EmitPCodeOperator;
  EmitList[ - OP_BEGIN_TEXT  ] := EmitPCodeOperator;
  EmitList[ - OP_END_TEXT  ] := EmitPCodeOperator;
  EmitList[ - OP_WARNINGS_ON  ] := EmitPCodeOperator;
  EmitList[ - OP_WARNINGS_OFF  ] := EmitPCodeOperator;
  EmitList[ - OP_FRAMEWORK_ON  ] := EmitPCodeOperator;
  EmitList[ - OP_FRAMEWORK_OFF  ] := EmitPCodeOperator;
  EmitList[ - OP_DECL_SUB  ] := EmitPCodeOperator;
  EmitList[ - OP_DECLARE_MEMBER ] := EmitPCodeOperator;
  EmitList[ - OP_ABSOLUTE  ] := EmitPCodeOperator;

  EmitList[ - OP_BEGIN_INCLUDED_FILE ] := EmitPCodeOperator;
  EmitList[ - OP_END_INCLUDED_FILE ] := EmitPCodeOperator;

///
///
///
///
{$IFNDEF PAXARM}
  if TargetPlatform = tpWIN64 then
  begin
    EmitList[ - OP_EMIT_ON ] := EmitOP_EMIT_ON;
    EmitList[ - OP_EMIT_OFF] := EmitOP_EMIT_OFF;
    EmitList[ - OP_NOP ] := EmitNothing;
    EmitList[ - OP_BEGIN_NAMESPACE  ] := EmitNothing;
    EmitList[ - OP_END_NAMESPACE  ] := EmitNothing;
    EmitList[ - OP_GO  ] := EmitOP_GO;
    EmitList[ - OP_GO_1  ] := EmitOP_GO_1;
    EmitList[ - OP_GO_2  ] := EmitOP_GO_2;
    EmitList[ - OP_GO_3  ] := EmitOP_GO_3;
    EmitList[ - OP_GO_TRUE  ] := EmitOP_GO_TRUE_64;
    EmitList[ - OP_GO_FALSE  ] := EmitOP_GO_FALSE_64;
    EmitList[ - OP_GO_DL  ] := EmitOP_GO_DL_64;
    EmitList[ - OP_SAVE_EDX  ] := EmitOP_SAVE_EDX_64;
    EmitList[ - OP_RESTORE_EDX  ] := EmitOP_RESTORE_EDX_64;
    EmitList[ - OP_ASSIGN_BYTE_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_BYTE_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_WORD_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_WORD_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_CARDINAL_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_CARDINAL_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_SMALLINT_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_SMALLINT_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_SHORTINT_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_SHORTINT_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_INT_I  ] := EmitOP_ASSIGN_INT_I_64;
    EmitList[ - OP_ASSIGN_INT_M  ] := EmitOP_ASSIGN_INT_M_64;
    EmitList[ - OP_ASSIGN_PANSICHAR  ] := EmitOP_ASSIGN_PANSICHAR_64;
    EmitList[ - OP_ASSIGN_PWIDECHAR  ] := EmitOP_ASSIGN_PWIDECHAR_64;
    EmitList[ - OP_ASSIGN_EVENT  ] := EmitOP_ASSIGN_EVENT_64;
    EmitList[ - OP_CREATE_EVENT  ] := EmitOP_CREATE_EVENT_64;
    EmitList[ - OP_ASSIGN_DOUBLE  ] := EmitOP_ASSIGN_DOUBLE_64;
    EmitList[ - OP_ASSIGN_CURRENCY  ] := EmitOP_ASSIGN_CURRENCY_64;
    EmitList[ - OP_ASSIGN_SINGLE  ] := EmitOP_ASSIGN_SINGLE_64;
    EmitList[ - OP_ASSIGN_EXTENDED  ] := EmitOP_ASSIGN_EXTENDED_64;
    EmitList[ - OP_ASSIGN_INT64  ] := EmitOP_ASSIGN_INT64_64;
    EmitList[ - OP_ASSIGN_UINT64  ] := EmitOP_ASSIGN_INT64_64;
    EmitList[ - OP_ASSIGN_RECORD  ] := EmitOP_ASSIGN_RECORD_64;
    EmitList[ - OP_ASSIGN_ARRAY  ] := EmitOP_ASSIGN_RECORD_64;
    EmitList[ - OP_ASSIGN_INTERFACE  ] := EmitOP_ASSIGN_INTERFACE_64;
    EmitList[ - OP_ADD_INT64  ] := EmitOP_ADD_INT64_64;
    EmitList[ - OP_SUB_INT64  ] := EmitOP_SUB_INT64_64;
    EmitList[ - OP_AND_INT64  ] := EmitOP_AND_INT64_64;
    EmitList[ - OP_OR_INT64  ] := EmitOP_OR_INT64_64;
    EmitList[ - OP_XOR_INT64  ] := EmitOP_XOR_INT64;
    EmitList[ - OP_ADD_UINT64  ] := EmitOP_ADD_UINT64_64;
    EmitList[ - OP_SUB_UINT64  ] := EmitOP_SUB_UINT64_64;
    EmitList[ - OP_AND_UINT64  ] := EmitOP_AND_UINT64_64;
    EmitList[ - OP_OR_UINT64  ] := EmitOP_OR_UINT64_64;
    EmitList[ - OP_XOR_UINT64  ] := EmitOP_XOR_UINT64;
    EmitList[ - OP_LT_INT64  ] := EmitOP_LT_INT64_64;
    EmitList[ - OP_LE_INT64  ] := EmitOP_LE_INT64_64;
    EmitList[ - OP_GT_INT64  ] := EmitOP_GT_INT64_64;
    EmitList[ - OP_GE_INT64  ] := EmitOP_GE_INT64_64;
    EmitList[ - OP_EQ_INT64  ] := EmitOP_EQ_INT64_64;
    EmitList[ - OP_NE_INT64  ] := EmitOP_NE_INT64_64;
    EmitList[ - OP_LT_UINT64  ] := EmitOP_LT_UINT64_64;
    EmitList[ - OP_LE_UINT64  ] := EmitOP_LE_UINT64_64;
    EmitList[ - OP_GT_UINT64  ] := EmitOP_GT_UINT64_64;
    EmitList[ - OP_GE_UINT64  ] := EmitOP_GE_UINT64_64;
    EmitList[ - OP_EQ_STRUCT  ] := EmitOP_EQ_STRUCT_64;
    EmitList[ - OP_NE_STRUCT  ] := EmitOP_NE_STRUCT_64;
    EmitList[ - OP_EQ_EVENT  ] := EmitOP_EQ_EVENT_64;
    EmitList[ - OP_NE_EVENT  ] := EmitOP_NE_EVENT_64;
    EmitList[ - OP_SUB_CURRENCY  ] := EmitOP_SUB_CURRENCY_64;
    EmitList[ - OP_MUL_CURRENCY  ] := EmitOP_MUL_CURRENCY_64;
    EmitList[ - OP_DIV_CURRENCY  ] := EmitOP_DIV_CURRENCY_64;
    EmitList[ - OP_ADD_INT_MI  ] := EmitOP_ADD_INT_MI_64;
    EmitList[ - OP_ADD_INT_MM  ] := EmitOP_ADD_INT_MM_64;
    EmitList[ - OP_ADD_CURRENCY  ] := EmitOP_ADD_CURRENCY_64;
    EmitList[ - OP_ADD_DOUBLE  ] := EmitOP_ADD_DOUBLE_64;
    EmitList[ - OP_ADD_SINGLE  ] := EmitOP_ADD_SINGLE_64;
    EmitList[ - OP_ADD_EXTENDED  ] := EmitOP_ADD_EXTENDED_64;
    EmitList[ - OP_NEG_INT  ] := EmitOP_NEG_INT_64;
    EmitList[ - OP_NEG_INT64  ] := EmitOP_NEG_INT64_64;
    EmitList[ - OP_NEG_UINT64  ] := EmitOP_NEG_INT64_64;
    EmitList[ - OP_NOT  ] := EmitOP_NOT_64;
    EmitList[ - OP_NOT_BOOL  ] := EmitOP_NOT_BOOL64;
    EmitList[ - OP_NOT_BYTEBOOL  ] := EmitOP_NOT_BYTEBOOL64;
    EmitList[ - OP_NOT_WORDBOOL  ] := EmitOP_NOT_WORDBOOL64;
    EmitList[ - OP_NOT_LONGBOOL  ] := EmitOP_NOT_LONGBOOL64;
    EmitList[ - OP_NEG_DOUBLE  ] := EmitOP_NEG_DOUBLE_64;
    EmitList[ - OP_NEG_CURRENCY  ] := EmitOP_NEG_CURRENCY_64;
    EmitList[ - OP_NEG_SINGLE  ] := EmitOP_NEG_SINGLE_64;
    EmitList[ - OP_NEG_EXTENDED  ] := EmitOP_NEG_EXTENDED_64;
    EmitList[ - OP_ABS_INT  ] := EmitOP_ABS_INT_64;
    EmitList[ - OP_ABS_DOUBLE  ] := EmitOP_ABS_DOUBLE_64;
    EmitList[ - OP_ABS_SINGLE  ] := EmitOP_ABS_SINGLE_64;
    EmitList[ - OP_ABS_EXTENDED  ] := EmitOP_ABS_EXTENDED_64;
    EmitList[ - OP_ABS_CURRENCY  ] := EmitOP_ABS_CURRENCY_64;
    EmitList[ - OP_SUB_INT_MI  ] := EmitOP_SUB_INT_MI_64;
    EmitList[ - OP_SUB_INT_MM  ] := EmitOP_SUB_INT_MM_64;
    EmitList[ - OP_SUB_DOUBLE  ] := EmitOP_SUB_DOUBLE_64;
    EmitList[ - OP_SUB_SINGLE  ] := EmitOP_SUB_SINGLE_64;
    EmitList[ - OP_SUB_EXTENDED  ] := EmitOP_SUB_EXTENDED_64;
    EmitList[ - OP_IMUL_INT_MI  ] := EmitOP_IMUL_INT_MI_64;
    EmitList[ - OP_IMUL_INT_MM  ] := EmitOP_IMUL_INT_MM_64;
    EmitList[ - OP_MUL_DOUBLE  ] := EmitOP_MUL_DOUBLE_64;
    EmitList[ - OP_MUL_SINGLE  ] := EmitOP_MUL_SINGLE_64;
    EmitList[ - OP_MUL_EXTENDED  ] := EmitOP_MUL_EXTENDED_64;
    EmitList[ - OP_IDIV_INT_MI  ] := EmitOP_IDIV_INT_MI_64;
    EmitList[ - OP_IDIV_INT_MM  ] := EmitOP_IDIV_INT_MM_64;
    EmitList[ - OP_IDIV_INT_IM  ] := EmitOP_IDIV_INT_IM_64;
    EmitList[ - OP_DIV_DOUBLE  ] := EmitOP_DIV_DOUBLE_64;
    EmitList[ - OP_DIV_SINGLE  ] := EmitOP_DIV_SINGLE_64;
    EmitList[ - OP_DIV_EXTENDED  ] := EmitOP_DIV_EXTENDED_64;
    EmitList[ - OP_MOD_INT_MI  ] := EmitOP_MOD_INT_MI_64;
    EmitList[ - OP_MOD_INT_MM  ] := EmitOP_MOD_INT_MM_64;
    EmitList[ - OP_MOD_INT_IM  ] := EmitOP_MOD_INT_IM_64;
    EmitList[ - OP_SHL_INT_MI  ] := EmitOP_SHL_INT_MI_64;
    EmitList[ - OP_SHL_INT_MM  ] := EmitOP_SHL_INT_MM_64;
    EmitList[ - OP_SHL_INT_IM  ] := EmitOP_SHL_INT_IM_64;
    EmitList[ - OP_SHR_INT_MI  ] := EmitOP_SHR_INT_MI_64;
    EmitList[ - OP_SHR_INT_MM  ] := EmitOP_SHR_INT_MM_64;
    EmitList[ - OP_SHR_INT_IM  ] := EmitOP_SHR_INT_IM_64;
    EmitList[ - OP_AND_INT_MI  ] := EmitOP_AND_INT_MI_64;
    EmitList[ - OP_AND_INT_MM  ] := EmitOP_AND_INT_MM_64;
    EmitList[ - OP_OR_INT_MI  ] := EmitOP_OR_INT_MI_64;
    EmitList[ - OP_OR_INT_MM  ] := EmitOP_OR_INT_MM_64;
    EmitList[ - OP_XOR_INT_MI  ] := EmitOP_XOR_INT_MI_64;
    EmitList[ - OP_XOR_INT_MM  ] := EmitOP_XOR_INT_MM_64;
    EmitList[ - OP_LT_INT_MI  ] := EmitOP_LT_INT_MI_64;
    EmitList[ - OP_LT_INT_MM  ] := EmitOP_LT_INT_MM_64;
    EmitList[ - OP_LE_INT_MI  ] := EmitOP_LE_INT_MI_64;
    EmitList[ - OP_LE_INT_MM  ] := EmitOP_LE_INT_MM_64;
    EmitList[ - OP_GT_INT_MI  ] := EmitOP_GT_INT_MI_64;
    EmitList[ - OP_GT_INT_MM  ] := EmitOP_GT_INT_MM_64;
    EmitList[ - OP_GE_INT_MI  ] := EmitOP_GE_INT_MI_64;
    EmitList[ - OP_GE_INT_MM  ] := EmitOP_GE_INT_MM_64;
    EmitList[ - OP_EQ_INT_MI  ] := EmitOP_EQ_INT_MI_64;
    EmitList[ - OP_EQ_INT_MM  ] := EmitOP_EQ_INT_MM_64;
    EmitList[ - OP_NE_INT_MI  ] := EmitOP_NE_INT_MI_64;
    EmitList[ - OP_NE_INT_MM  ] := EmitOP_NE_INT_MM_64;
    EmitList[ - OP_LT_DOUBLE  ] := EmitOP_LT_DOUBLE_64;
    EmitList[ - OP_LE_DOUBLE  ] := EmitOP_LE_DOUBLE_64;
    EmitList[ - OP_GT_DOUBLE  ] := EmitOP_GT_DOUBLE_64;
    EmitList[ - OP_GE_DOUBLE  ] := EmitOP_GE_DOUBLE_64;
    EmitList[ - OP_EQ_DOUBLE  ] := EmitOP_EQ_DOUBLE_64;
    EmitList[ - OP_NE_DOUBLE  ] := EmitOP_NE_DOUBLE_64;
    EmitList[ - OP_LT_CURRENCY  ] := EmitOP_LT_CURRENCY_64;
    EmitList[ - OP_LE_CURRENCY  ] := EmitOP_LE_CURRENCY_64;
    EmitList[ - OP_GT_CURRENCY  ] := EmitOP_GT_CURRENCY_64;
    EmitList[ - OP_GE_CURRENCY  ] := EmitOP_GE_CURRENCY_64;
    EmitList[ - OP_EQ_CURRENCY  ] := EmitOP_EQ_CURRENCY_64;
    EmitList[ - OP_NE_CURRENCY  ] := EmitOP_NE_CURRENCY_64;
    EmitList[ - OP_LT_SINGLE  ] := EmitOP_LT_SINGLE_64;
    EmitList[ - OP_LE_SINGLE  ] := EmitOP_LE_SINGLE_64;
    EmitList[ - OP_GT_SINGLE  ] := EmitOP_GT_SINGLE_64;
    EmitList[ - OP_GE_SINGLE  ] := EmitOP_GE_SINGLE_64;
    EmitList[ - OP_EQ_SINGLE  ] := EmitOP_EQ_SINGLE_64;
    EmitList[ - OP_NE_SINGLE  ] := EmitOP_NE_SINGLE_64;
    EmitList[ - OP_LT_EXTENDED  ] := EmitOP_LT_EXTENDED_64;
    EmitList[ - OP_LE_EXTENDED  ] := EmitOP_LE_EXTENDED_64;
    EmitList[ - OP_GT_EXTENDED  ] := EmitOP_GT_EXTENDED_64;
    EmitList[ - OP_GE_EXTENDED  ] := EmitOP_GE_EXTENDED_64;
    EmitList[ - OP_EQ_EXTENDED  ] := EmitOP_EQ_EXTENDED_64;
    EmitList[ - OP_NE_EXTENDED  ] := EmitOP_NE_EXTENDED_64;
    EmitList[ - OP_EXPORTS  ] := EmitOP_EXPORTS_64;
    EmitList[ - OP_PUSH_ADDRESS  ] := EmitOP_PUSH_ADDRESS_64;
    EmitList[ - OP_PUSH_STRUCTURE  ] := EmitOP_PUSH_STRUCTURE_64;
    EmitList[ - OP_PUSH_SET  ] := EmitOP_PUSH_SET_64;
    EmitList[ - OP_PUSH_BYTE_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_BYTE  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_WORD_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_WORD  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_CARDINAL_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_CARDINAL  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_SMALLINT_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_SMALLINT  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_SHORTINT_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_SHORTINT  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_INT_IMM  ] := EmitOP_PUSH_INT_IMM_64;
    EmitList[ - OP_PUSH_INT  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_PTR  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_INST  ] := EmitOP_PUSH_INST_64;
    EmitList[ - OP_PUSH_CLSREF  ] := EmitOP_PUSH_CLSREF_64;
    EmitList[ - OP_UPDATE_INSTANCE  ] := EmitOP_UPDATE_INSTANCE_64;
    EmitList[ - OP_CLEAR_EDX  ] := EmitOP_CLEAR_EDX_64;
    EmitList[ - OP_PUSH_DYNARRAY  ] := EmitOP_PUSH_DYNARRAY_64;
    EmitList[ - OP_PUSH_OPENARRAY  ] := EmitOP_PUSH_OPENARRAY_64;
    EmitList[ - OP_PUSH_DATA  ] := EmitOP_PUSH_DATA_64;
    EmitList[ - OP_PUSH_EVENT  ] := EmitOP_PUSH_EVENT_64;
    EmitList[ - OP_PUSH_INT64  ] := EmitOP_PUSH_INT64;
    EmitList[ - OP_PUSH_INT64  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_DOUBLE  ] := EmitOP_PUSH_DOUBLE_64;
    EmitList[ - OP_PUSH_CURRENCY  ] := EmitOP_PUSH_INT_64;
    EmitList[ - OP_PUSH_SINGLE  ] := EmitOP_PUSH_SINGLE_64;
    EmitList[ - OP_PUSH_EXTENDED  ] := EmitOP_PUSH_DOUBLE_64;
    EmitList[ - OP_PUSH_ANSISTRING  ] := EmitOP_PUSH_ANSISTRING_64;
    EmitList[ - OP_PUSH_WIDESTRING  ] := EmitOP_PUSH_WIDESTRING_64;
    EmitList[ - OP_PUSH_UNICSTRING  ] := EmitOP_PUSH_UNICSTRING_64;
    EmitList[ - OP_PUSH_SHORTSTRING  ] := EmitOP_PUSH_SHORTSTRING_64;
    EmitList[ - OP_PUSH_PANSICHAR_IMM  ] := EmitOP_PUSH_PANSICHAR_IMM_64;
    EmitList[ - OP_PUSH_PWIDECHAR_IMM  ] := EmitOP_PUSH_PWIDECHAR_IMM_64;
    EmitList[ - OP_BEGIN_CALL  ] := EmitOP_BEGIN_CALL_64;
    EmitList[ - OP_CALL  ] := EmitOP_CALL_64;
    EmitList[ - OP_INIT_SUB  ] := EmitOP_INIT_SUB_64;
    EmitList[ - OP_END_SUB  ] := EmitOP_END_SUB_64;
    EmitList[ - OP_FIN_SUB  ] := EmitOP_FIN_SUB_64;
    EmitList[ - OP_EPILOGUE_SUB  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_GLOBAL_BLOCK  ] := EmitPCodeOperator;
    EmitList[ - OP_EPILOGUE_GLOBAL_BLOCK  ] := EmitPCodeOperator;
    EmitList[ - OP_END_GLOBAL_BLOCK  ] := EmitPCodeOperator;
    EmitList[ - OP_EXTRA_BYTECODE  ] := EmitPCodeOperator;
    EmitList[ - OP_RET  ] := EmitOP_RET_64;
    EmitList[ - OP_FIELD  ] := EmitOP_FIELD_64;
    EmitList[ - OP_GET_COMPONENT ] := EmitOP_GET_COMPONENT_64;
    EmitList[ - OP_ELEM  ] := EmitOP_ELEM_64;
    EmitList[ - OP_PRINT_EX  ] := EmitOP_PRINT_EX_64;
    EmitList[ - OP_TO_FW_OBJECT  ] := EmitOP_TO_FW_OBJECT_64;
    EmitList[ - OP_PUSH_EBP  ] := EmitOP_PUSH_EBP_64;
    EmitList[ - OP_POP  ] := EmitOP_POP_64;
    EmitList[ - OP_SAVE_REGS  ] := EmitOP_SAVE_REGS_64;
    EmitList[ - OP_RESTORE_REGS  ] := EmitOP_RESTORE_REGS_64;

    EmitList[ - OP_INT_TO_INT64  ] := EmitOP_INT_TO_INT64_64;
    EmitList[ - OP_BYTE_TO_INT64  ] := EmitOP_BYTE_TO_INT64_64;
    EmitList[ - OP_WORD_TO_INT64  ] := EmitOP_WORD_TO_INT64_64;
    EmitList[ - OP_CARDINAL_TO_INT64  ] := EmitOP_CARDINAL_TO_INT64_64;
    EmitList[ - OP_SMALLINT_TO_INT64  ] := EmitOP_SMALLINT_TO_INT64_64;
    EmitList[ - OP_SHORTINT_TO_INT64  ] := EmitOP_SHORTINT_TO_INT64_64;
    EmitList[ - OP_INT_FROM_INT64  ] := EmitOP_INT_FROM_INT64_64;
    EmitList[ - OP_BYTE_FROM_INT64  ] := EmitOP_BYTE_FROM_INT64_64;
    EmitList[ - OP_WORD_FROM_INT64  ] := EmitOP_WORD_FROM_INT64_64;
    EmitList[ - OP_CARDINAL_FROM_INT64  ] := EmitOP_CARDINAL_FROM_INT64_64;
    EmitList[ - OP_SMALLINT_FROM_INT64  ] := EmitOP_SMALLINT_FROM_INT64_64;
    EmitList[ - OP_SHORTINT_FROM_INT64  ] := EmitOP_SHORTINT_FROM_INT64_64;

    EmitList[ - OP_INT_TO_UINT64  ] := EmitOP_INT_TO_INT64_64;
    EmitList[ - OP_BYTE_TO_UINT64  ] := EmitOP_BYTE_TO_INT64_64;
    EmitList[ - OP_WORD_TO_UINT64  ] := EmitOP_WORD_TO_INT64_64;
    EmitList[ - OP_CARDINAL_TO_UINT64  ] := EmitOP_CARDINAL_TO_INT64_64;
    EmitList[ - OP_SMALLINT_TO_UINT64  ] := EmitOP_SMALLINT_TO_INT64_64;
    EmitList[ - OP_SHORTINT_TO_UINT64  ] := EmitOP_SHORTINT_TO_INT64_64;
    EmitList[ - OP_INT_FROM_UINT64  ] := EmitOP_INT_FROM_INT64_64;
    EmitList[ - OP_BYTE_FROM_UINT64  ] := EmitOP_BYTE_FROM_INT64_64;
    EmitList[ - OP_WORD_FROM_UINT64  ] := EmitOP_WORD_FROM_INT64_64;
    EmitList[ - OP_CARDINAL_FROM_UINT64  ] := EmitOP_CARDINAL_FROM_INT64_64;
    EmitList[ - OP_SMALLINT_FROM_UINT64  ] := EmitOP_SMALLINT_FROM_INT64_64;
    EmitList[ - OP_SHORTINT_FROM_UINT64  ] := EmitOP_SHORTINT_FROM_INT64_64;

    EmitList[ - OP_INT_TO_DOUBLE  ] := EmitOP_INT_TO_DOUBLE_64;
    EmitList[ - OP_INT64_TO_DOUBLE  ] := EmitOP_INT64_TO_DOUBLE_64;
    EmitList[ - OP_UINT64_TO_DOUBLE  ] := EmitOP_INT64_TO_DOUBLE_64;
    EmitList[ - OP_INT_TO_SINGLE  ] := EmitOP_INT_TO_SINGLE_64;
    EmitList[ - OP_INT64_TO_SINGLE  ] := EmitOP_INT64_TO_SINGLE_64;
    EmitList[ - OP_UINT64_TO_SINGLE  ] := EmitOP_INT64_TO_SINGLE_64;
    EmitList[ - OP_INT_TO_EXTENDED  ] := EmitOP_INT_TO_EXTENDED_64;
    EmitList[ - OP_INT64_TO_EXTENDED  ] := EmitOP_INT64_TO_EXTENDED_64;
    EmitList[ - OP_UINT64_TO_EXTENDED  ] := EmitOP_INT64_TO_EXTENDED_64;
    EmitList[ - OP_MULT_INT64  ] := EmitOP_MULT_INT64_64;
    EmitList[ - OP_IDIV_INT64  ] := EmitOP_IDIV_INT64_64;
    EmitList[ - OP_MOD_INT64  ] := EmitOP_MOD_INT64_64;
    EmitList[ - OP_SHL_INT64  ] := EmitOP_SHL_INT64_64;
    EmitList[ - OP_SHR_INT64  ] := EmitOP_SHR_INT64_64;
    EmitList[ - OP_ABS_INT64  ] := EmitOP_ABS_INT64_64;
    EmitList[ - OP_CURRENCY_TO_EXTENDED  ] := EmitOP_CURRENCY_TO_EXTENDED_64;
    EmitList[ - OP_CURRENCY_TO_SINGLE  ] := EmitOP_CURRENCY_TO_SINGLE_64;
    EmitList[ - OP_DOUBLE_TO_SINGLE  ] := EmitOP_DOUBLE_TO_SINGLE_64;
    EmitList[ - OP_DOUBLE_TO_EXTENDED  ] := EmitOP_DOUBLE_TO_EXTENDED_64;
    EmitList[ - OP_SINGLE_TO_DOUBLE  ] := EmitOP_SINGLE_TO_DOUBLE_64;
    EmitList[ - OP_CURRENCY_TO_DOUBLE  ] := EmitOP_CURRENCY_TO_DOUBLE_64;
    EmitList[ - OP_SINGLE_TO_EXTENDED  ] := EmitOP_SINGLE_TO_EXTENDED_64;
    EmitList[ - OP_EXTENDED_TO_DOUBLE  ] := EmitOP_EXTENDED_TO_DOUBLE_64;
    EmitList[ - OP_EXTENDED_TO_SINGLE  ] := EmitOP_EXTENDED_TO_SINGLE_64;
    EmitList[ - OP_ADDRESS  ] := EmitOP_ADDRESS_64;
    EmitList[ - OP_TERMINAL  ] := EmitOP_TERMINAL_64;
    EmitList[ - OP_ADDRESS_PROG  ] := EmitOP_ADDRESS_PROG_64;
    EmitList[ - OP_ASSIGN_PROG  ] := EmitOP_ASSIGN_PROG_64;
    EmitList[ - OP_SET_INCLUDE  ] := EmitOP_SET_INCLUDE_64;
    EmitList[ - OP_SET_INCLUDE_INTERVAL  ] := EmitOP_SET_INCLUDE_INTERVAL_64;
    EmitList[ - OP_SET_EXCLUDE  ] := EmitOP_SET_EXCLUDE_64;
    EmitList[ - OP_SET_UNION  ] := EmitOP_SET_UNION_64;
    EmitList[ - OP_SET_DIFFERENCE  ] := EmitOP_SET_DIFFERENCE_64;
    EmitList[ - OP_SET_INTERSECTION  ] := EmitOP_SET_INTERSECTION_64;
    EmitList[ - OP_SET_SUBSET  ] := EmitOP_SET_SUBSET_64;
    EmitList[ - OP_SET_SUPERSET  ] := EmitOP_SET_SUPERSET_64;
    EmitList[ - OP_SET_EQUALITY  ] := EmitOP_SET_EQUALITY_64;
    EmitList[ - OP_SET_INEQUALITY  ] := EmitOP_SET_INEQUALITY_64;
    EmitList[ - OP_SET_MEMBERSHIP  ] := EmitOP_SET_MEMBERSHIP_64;
    EmitList[ - OP_SET_ASSIGN  ] := EmitOP_SET_ASSIGN_64;
    EmitList[ - OP_SET_COUNTER_ASSIGN  ] := EmitOP_SET_COUNTER_ASSIGN_64;
    EmitList[ - OP_ERR_ABSTRACT  ] := EmitOP_ERR_ABSTRACT_64;
    EmitList[ - OP_VAR_FROM_TVALUE  ] := EmitOP_VAR_FROM_TVALUE_64;
    EmitList[ - OP_ANSISTRING_FROM_PANSICHAR  ] := EmitOP_ANSISTRING_FROM_PANSICHAR_64;
    EmitList[ - OP_ANSISTRING_FROM_PWIDECHAR  ] := EmitOP_ANSISTRING_FROM_PWIDECHAR_64;
    EmitList[ - OP_ANSISTRING_FROM_ANSICHAR  ] := EmitOP_ANSISTRING_FROM_ANSICHAR_64;
    EmitList[ - OP_ASSIGN_ANSISTRING  ] := EmitOP_ASSIGN_ANSISTRING_64;
    EmitList[ - OP_ASSIGN_SHORTSTRING  ] := EmitOP_ASSIGN_SHORTSTRING_64;
    EmitList[ - OP_ASSIGN_WIDESTRING  ] := EmitOP_ASSIGN_WIDESTRING_64;
    EmitList[ - OP_ASSIGN_UNICSTRING  ] := EmitOP_ASSIGN_UNICSTRING_64;
    EmitList[ - OP_ADD_ANSISTRING  ] := EmitOP_ADD_ANSISTRING_64;
    EmitList[ - OP_ADD_SHORTSTRING  ] := EmitOP_ADD_SHORTSTRING_64;
    EmitList[ - OP_ADD_WIDESTRING  ] := EmitOP_ADD_WIDESTRING_64;
    EmitList[ - OP_ADD_UNICSTRING  ] := EmitOP_ADD_UNICSTRING_64;
    EmitList[ - OP_ANSISTRING_CLR  ] := EmitOP_ANSISTRING_CLR_64;
    EmitList[ - OP_WIDESTRING_CLR  ] := EmitOP_WIDESTRING_CLR_64;
    EmitList[ - OP_UNICSTRING_CLR  ] := EmitOP_UNICSTRING_CLR_64;
    EmitList[ - OP_INTERFACE_CLR  ] := EmitOP_INTERFACE_CLR_64;
    EmitList[ - OP_STRUCTURE_CLR  ] := EmitOP_STRUCTURE_CLR_64;
    EmitList[ - OP_CLASS_CLR  ] := EmitOP_CLASS_CLR_64;
    EmitList[ - OP_STRUCTURE_ADDREF  ] := EmitOP_STRUCTURE_ADDREF_64;
    EmitList[ - OP_ADDREF  ] := EmitOP_ADDREF_64;
    EmitList[ - OP_DYNARRAY_CLR  ] := EmitOP_DYNARRAY_CLR_64;
    EmitList[ - OP_DYNARRAY_HIGH  ] := EmitOP_DYNARRAY_HIGH_64;
    EmitList[ - OP_DYNARRAY_ASSIGN  ] := EmitOP_DYNARRAY_ASSIGN_64;
    EmitList[ - OP_CREATE_EMPTY_DYNARRAY  ] := EmitOP_CREATE_EMPTY_DYNARRAY_64;
    EmitList[ - OP_ASSIGN_TVarRec  ] := EmitOP_ASSIGN_TVarRec_64;
    EmitList[ - OP_SHORTSTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL_64;
    EmitList[ - OP_SHORTSTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL_64;
    EmitList[ - OP_SHORTSTRING_FROM_ANSICHAR  ] := EmitOP_SHORTSTRING_FROM_ANSICHAR_64;
    EmitList[ - OP_SHORTSTRING_FROM_WIDECHAR  ] := EmitOP_SHORTSTRING_FROM_WIDECHAR_64;
    EmitList[ - OP_SHORTSTRING_FROM_ANSISTRING  ] := EmitOP_SHORTSTRING_FROM_ANSISTRING_64;
    EmitList[ - OP_SHORTSTRING_FROM_WIDESTRING  ] := EmitOP_SHORTSTRING_FROM_WIDESTRING_64;
    EmitList[ - OP_UNICSTRING_FROM_WIDESTRING  ] := EmitOP_UNICSTRING_FROM_WIDESTRING_64;
    EmitList[ - OP_SHORTSTRING_FROM_UNICSTRING  ] := EmitOP_SHORTSTRING_FROM_UNICSTRING_64;
    EmitList[ - OP_ANSISTRING_FROM_SHORTSTRING  ] := EmitOP_ANSISTRING_FROM_SHORTSTRING_64;
    EmitList[ - OP_EQ_ANSISTRING  ] := EmitOP_EQ_ANSISTRING_64;
    EmitList[ - OP_NE_ANSISTRING  ] := EmitOP_NE_ANSISTRING_64;
    EmitList[ - OP_EQ_SHORTSTRING  ] := EmitOP_EQ_SHORTSTRING_64;
    EmitList[ - OP_NE_SHORTSTRING  ] := EmitOP_NE_SHORTSTRING_64;
    EmitList[ - OP_EQ_WIDESTRING  ] := EmitOP_EQ_WIDESTRING_64;
    EmitList[ - OP_EQ_UNICSTRING  ] := EmitOP_EQ_UNICSTRING_64;
    EmitList[ - OP_NE_WIDESTRING  ] := EmitOP_NE_WIDESTRING_64;
    EmitList[ - OP_NE_UNICSTRING  ] := EmitOP_NE_UNICSTRING_64;
    EmitList[ - OP_GT_ANSISTRING  ] := EmitOP_GT_ANSISTRING_64;
    EmitList[ - OP_GE_ANSISTRING  ] := EmitOP_GE_ANSISTRING_64;
    EmitList[ - OP_LT_ANSISTRING  ] := EmitOP_LT_ANSISTRING_64;
    EmitList[ - OP_LE_ANSISTRING  ] := EmitOP_LE_ANSISTRING_64;
    EmitList[ - OP_GT_SHORTSTRING  ] := EmitOP_GT_SHORTSTRING_64;
    EmitList[ - OP_GE_SHORTSTRING  ] := EmitOP_GE_SHORTSTRING_64;
    EmitList[ - OP_LT_SHORTSTRING  ] := EmitOP_LT_SHORTSTRING_64;
    EmitList[ - OP_LE_SHORTSTRING  ] := EmitOP_LE_SHORTSTRING_64;
    EmitList[ - OP_GT_WIDESTRING  ] := EmitOP_GT_WIDESTRING_64;
    EmitList[ - OP_GE_WIDESTRING  ] := EmitOP_GE_WIDESTRING_64;
    EmitList[ - OP_LT_WIDESTRING  ] := EmitOP_LT_WIDESTRING_64;
    EmitList[ - OP_LE_WIDESTRING  ] := EmitOP_LE_WIDESTRING_64;
    EmitList[ - OP_GT_UNICSTRING  ] := EmitOP_GT_UNICSTRING_64;
    EmitList[ - OP_GE_UNICSTRING  ] := EmitOP_GE_UNICSTRING_64;
    EmitList[ - OP_LT_UNICSTRING  ] := EmitOP_LT_UNICSTRING_64;
    EmitList[ - OP_LE_UNICSTRING  ] := EmitOP_LE_UNICSTRING_64;
    EmitList[ - OP_SHORTSTRING_HIGH  ] := EmitOP_SHORTSTRING_HIGH_64;
    EmitList[ - OP_LOCK_VARRAY  ] := EmitOP_LOCK_VARRAY_64;
    EmitList[ - OP_UNLOCK_VARRAY  ] := EmitOP_UNLOCK_VARRAY_64;
    EmitList[ - OP_VARIANT_CLR  ] := EmitOP_VARIANT_CLR_64;
    EmitList[ - OP_WIDESTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL_64;
    EmitList[ - OP_WIDESTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL_64;
    EmitList[ - OP_WIDESTRING_FROM_ANSICHAR  ] := EmitOP_WIDESTRING_FROM_ANSICHAR_64;
    EmitList[ - OP_WIDESTRING_FROM_WIDECHAR  ] := EmitOP_WIDESTRING_FROM_WIDECHAR_64;
    EmitList[ - OP_ANSISTRING_FROM_WIDECHAR  ] := EmitOP_ANSISTRING_FROM_WIDECHAR_64;
    EmitList[ - OP_WIDESTRING_FROM_WIDECHAR_LITERAL  ] := EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL_64;
    EmitList[ - OP_WIDESTRING_FROM_ANSISTRING  ] := EmitOP_WIDESTRING_FROM_ANSISTRING_64;
    EmitList[ - OP_UNICSTRING_FROM_ANSISTRING  ] := EmitOP_UNICSTRING_FROM_ANSISTRING_64;
    EmitList[ - OP_WIDESTRING_FROM_SHORTSTRING  ] := EmitOP_WIDESTRING_FROM_SHORTSTRING_64;
    EmitList[ - OP_WIDESTRING_FROM_UNICSTRING  ] := EmitOP_WIDESTRING_FROM_UNICSTRING_64;
    EmitList[ - OP_UNICSTRING_FROM_SHORTSTRING  ] := EmitOP_UNICSTRING_FROM_SHORTSTRING_64;
    EmitList[ - OP_ANSISTRING_FROM_WIDESTRING  ] := EmitOP_ANSISTRING_FROM_WIDESTRING_64;
    EmitList[ - OP_ANSISTRING_FROM_UNICSTRING  ] := EmitOP_ANSISTRING_FROM_UNICSTRING_64;
    EmitList[ - OP_UNICSTRING_FROM_PANSICHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL_64;
    EmitList[ - OP_UNICSTRING_FROM_PWIDECHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL_64;
    EmitList[ - OP_UNICSTRING_FROM_ANSICHAR  ] := EmitOP_UNICSTRING_FROM_ANSICHAR_64;
    EmitList[ - OP_UNICSTRING_FROM_WIDECHAR  ] := EmitOP_UNICSTRING_FROM_WIDECHAR_64;
    EmitList[ - OP_UNICSTRING_FROM_WIDECHAR_LITERAL  ] := EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL_64;
    EmitList[ - OP_VARIANT_FROM_ANSICHAR  ] := EmitOP_VARIANT_FROM_ANSICHAR_64;
    EmitList[ - OP_VARIANT_FROM_INT  ] := EmitOP_VARIANT_FROM_INT_64;
    EmitList[ - OP_VARIANT_FROM_INT64  ] := EmitOP_VARIANT_FROM_INT64_64;
    EmitList[ - OP_VARIANT_FROM_BYTE  ] := EmitOP_VARIANT_FROM_BYTE_64;
    EmitList[ - OP_VARIANT_FROM_BOOL  ] := EmitOP_VARIANT_FROM_BOOL_64;
    EmitList[ - OP_VARIANT_FROM_WORD  ] := EmitOP_VARIANT_FROM_WORD_64;
    EmitList[ - OP_VARIANT_FROM_CARDINAL  ] := EmitOP_VARIANT_FROM_CARDINAL_64;
    EmitList[ - OP_VARIANT_FROM_SMALLINT  ] := EmitOP_VARIANT_FROM_SMALLINT_64;
    EmitList[ - OP_VARIANT_FROM_SHORTINT  ] := EmitOP_VARIANT_FROM_SHORTINT_64;
    EmitList[ - OP_VARIANT_FROM_DOUBLE  ] := EmitOP_VARIANT_FROM_DOUBLE_64;
    EmitList[ - OP_VARIANT_FROM_CURRENCY  ] := EmitOP_VARIANT_FROM_CURRENCY_64;
    EmitList[ - OP_VARIANT_FROM_SINGLE  ] := EmitOP_VARIANT_FROM_SINGLE_64;
    EmitList[ - OP_VARIANT_FROM_EXTENDED  ] := EmitOP_VARIANT_FROM_EXTENDED_64;
    EmitList[ - OP_VARIANT_FROM_INTERFACE  ] := EmitOP_VARIANT_FROM_INTERFACE_64;
    EmitList[ - OP_VARIANT_FROM_PANSICHAR_LITERAL  ] := EmitOP_VARIANT_FROM_PANSICHAR_LITERAL_64;
    EmitList[ - OP_VARIANT_FROM_PWIDECHAR_LITERAL  ] := EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL_64;
    EmitList[ - OP_VARIANT_FROM_ANSISTRING  ] := EmitOP_VARIANT_FROM_ANSISTRING_64;
    EmitList[ - OP_VARIANT_FROM_WIDESTRING  ] := EmitOP_VARIANT_FROM_WIDESTRING_64;
    EmitList[ - OP_VARIANT_FROM_UNICSTRING  ] := EmitOP_VARIANT_FROM_UNICSTRING_64;
    EmitList[ - OP_VARIANT_FROM_SHORTSTRING  ] := EmitOP_VARIANT_FROM_SHORTSTRING_64;
    EmitList[ - OP_VARIANT_FROM_WIDECHAR  ] := EmitOP_VARIANT_FROM_WIDECHAR_64;
    EmitList[ - OP_VARIANT_FROM_WIDECHAR_LITERAL  ] := EmitOP_VARIANT_FROM_WIDECHAR_LITERAL_64;
    EmitList[ - OP_OLEVARIANT_FROM_VARIANT  ] := EmitOP_OLEVARIANT_FROM_VARIANT_64;
    EmitList[ - OP_OLEVARIANT_FROM_ANSICHAR  ] := EmitOP_OLEVARIANT_FROM_ANSICHAR_64;
    EmitList[ - OP_OLEVARIANT_FROM_INT  ] := EmitOP_OLEVARIANT_FROM_INT_64;
    EmitList[ - OP_OLEVARIANT_FROM_INT64  ] := EmitOP_OLEVARIANT_FROM_INT64_64;
    EmitList[ - OP_OLEVARIANT_FROM_BYTE  ] :=  EmitOP_OLEVARIANT_FROM_BYTE_64;
    EmitList[ - OP_OLEVARIANT_FROM_BOOL  ] :=  EmitOP_OLEVARIANT_FROM_BOOL_64;
    EmitList[ - OP_OLEVARIANT_FROM_WORD  ] :=  EmitOP_OLEVARIANT_FROM_WORD_64;
    EmitList[ - OP_OLEVARIANT_FROM_CARDINAL  ] := EmitOP_OLEVARIANT_FROM_CARDINAL_64;
    EmitList[ - OP_OLEVARIANT_FROM_SMALLINT  ] := EmitOP_OLEVARIANT_FROM_SMALLINT_64;
    EmitList[ - OP_OLEVARIANT_FROM_SHORTINT  ] := EmitOP_OLEVARIANT_FROM_SHORTINT_64;
    EmitList[ - OP_OLEVARIANT_FROM_DOUBLE  ] := EmitOP_OLEVARIANT_FROM_DOUBLE_64;
    EmitList[ - OP_OLEVARIANT_FROM_CURRENCY  ] := EmitOP_OLEVARIANT_FROM_CURRENCY_64;
    EmitList[ - OP_OLEVARIANT_FROM_SINGLE  ] := EmitOP_OLEVARIANT_FROM_SINGLE_64;
    EmitList[ - OP_OLEVARIANT_FROM_EXTENDED  ] := EmitOP_OLEVARIANT_FROM_EXTENDED_64;
    EmitList[ - OP_OLEVARIANT_FROM_INTERFACE  ] := EmitOP_OLEVARIANT_FROM_INTERFACE_64;
    EmitList[ - OP_OLEVARIANT_FROM_PANSICHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL_64;
    EmitList[ - OP_OLEVARIANT_FROM_PWIDECHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL_64;
    EmitList[ - OP_OLEVARIANT_FROM_ANSISTRING  ] := EmitOP_OLEVARIANT_FROM_ANSISTRING_64;
    EmitList[ - OP_OLEVARIANT_FROM_WIDESTRING  ] := EmitOP_OLEVARIANT_FROM_WIDESTRING_64;
    EmitList[ - OP_OLEVARIANT_FROM_UNICSTRING  ] := EmitOP_OLEVARIANT_FROM_UNICSTRING_64;
    EmitList[ - OP_OLEVARIANT_FROM_SHORTSTRING  ] := EmitOP_OLEVARIANT_FROM_SHORTSTRING_64;
    EmitList[ - OP_OLEVARIANT_FROM_WIDECHAR  ] := EmitOP_OLEVARIANT_FROM_WIDECHAR_64;
    EmitList[ - OP_OLEVARIANT_FROM_WIDECHAR_LITERAL  ] := EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL_64;
    EmitList[ - OP_ANSISTRING_FROM_INT  ] := EmitOP_ANSISTRING_FROM_INT_64;
    EmitList[ - OP_ANSISTRING_FROM_DOUBLE  ] := EmitOP_ANSISTRING_FROM_DOUBLE_64;
    EmitList[ - OP_ANSISTRING_FROM_SINGLE  ] := EmitOP_ANSISTRING_FROM_SINGLE_64;
    EmitList[ - OP_ANSISTRING_FROM_EXTENDED  ] := EmitOP_ANSISTRING_FROM_EXTENDED_64;
    EmitList[ - OP_ANSISTRING_FROM_BOOLEAN  ] := EmitOP_ANSISTRING_FROM_BOOLEAN_64;
    EmitList[ - OP_UNICSTRING_FROM_INT  ] := EmitOP_UNICSTRING_FROM_INT_64;
    EmitList[ - OP_UNICSTRING_FROM_DOUBLE  ] := EmitOP_UNICSTRING_FROM_DOUBLE_64;
    EmitList[ - OP_UNICSTRING_FROM_SINGLE  ] := EmitOP_UNICSTRING_FROM_SINGLE_64;
    EmitList[ - OP_UNICSTRING_FROM_EXTENDED  ] := EmitOP_UNICSTRING_FROM_EXTENDED_64;
    EmitList[ - OP_UNICSTRING_FROM_BOOLEAN  ] := EmitOP_UNICSTRING_FROM_BOOLEAN_64;
    EmitList[ - OP_JS_FUNC_OBJ_FROM_VARIANT  ] := EmitOP_JS_FUNC_OBJ_FROM_VARIANT_64;
    EmitList[ - OP_ANSICHAR_FROM_VARIANT  ] := EmitOP_ANSICHAR_FROM_VARIANT_64;
    EmitList[ - OP_WIDECHAR_FROM_VARIANT  ] := EmitOP_WIDECHAR_FROM_VARIANT_64;
    EmitList[ - OP_ANSISTRING_FROM_VARIANT  ] := EmitOP_ANSISTRING_FROM_VARIANT_64;
    EmitList[ - OP_WIDESTRING_FROM_VARIANT  ] := EmitOP_WIDESTRING_FROM_VARIANT_64;
    EmitList[ - OP_UNICSTRING_FROM_VARIANT  ] := EmitOP_UNICSTRING_FROM_VARIANT_64;
    EmitList[ - OP_SHORTSTRING_FROM_VARIANT  ] := EmitOP_SHORTSTRING_FROM_VARIANT_64;
    EmitList[ - OP_DOUBLE_FROM_VARIANT  ] := EmitOP_DOUBLE_FROM_VARIANT_64;
    EmitList[ - OP_CURRENCY_FROM_VARIANT  ] := EmitOP_CURRENCY_FROM_VARIANT_64;
    EmitList[ - OP_SINGLE_FROM_VARIANT  ] := EmitOP_SINGLE_FROM_VARIANT_64;
    EmitList[ - OP_EXTENDED_FROM_VARIANT  ] := EmitOP_EXTENDED_FROM_VARIANT_64;
    EmitList[ - OP_INT_FROM_VARIANT  ] := EmitOP_INT_FROM_VARIANT_64;
    EmitList[ - OP_INT64_FROM_VARIANT  ] := EmitOP_INT64_FROM_VARIANT_64;
    EmitList[ - OP_UINT64_FROM_VARIANT  ] := EmitOP_INT64_FROM_VARIANT_64;
    EmitList[ - OP_BYTE_FROM_VARIANT  ] := EmitOP_BYTE_FROM_VARIANT_64;
    EmitList[ - OP_WORD_FROM_VARIANT  ] := EmitOP_WORD_FROM_VARIANT_64;
    EmitList[ - OP_CARDINAL_FROM_VARIANT  ] := EmitOP_CARDINAL_FROM_VARIANT_64;
    EmitList[ - OP_BOOL_FROM_VARIANT  ] := EmitOP_BOOL_FROM_VARIANT_64;
    EmitList[ - OP_BYTEBOOL_FROM_VARIANT  ] := EmitOP_BYTEBOOL_FROM_VARIANT_64;
    EmitList[ - OP_WORDBOOL_FROM_VARIANT  ] := EmitOP_WORDBOOL_FROM_VARIANT_64;
    EmitList[ - OP_LONGBOOL_FROM_VARIANT  ] := EmitOP_LONGBOOL_FROM_VARIANT_64;
    EmitList[ - OP_SMALLINT_FROM_VARIANT  ] := EmitOP_SMALLINT_FROM_VARIANT_64;
    EmitList[ - OP_SHORTINT_FROM_VARIANT  ] := EmitOP_SHORTINT_FROM_VARIANT_64;
    EmitList[ - OP_BOOL_FROM_BYTEBOOL  ] := EmitOP_BOOL_FROM_BYTEBOOL_64;
    EmitList[ - OP_BOOL_FROM_WORDBOOL  ] := EmitOP_BOOL_FROM_WORDBOOL_64;
    EmitList[ - OP_BOOL_FROM_LONGBOOL  ] := EmitOP_BOOL_FROM_LONGBOOL_64;
    EmitList[ - OP_NEG_VARIANT  ] := EmitOP_NEG_VARIANT_64;
    EmitList[ - OP_ABS_VARIANT  ] := EmitOP_ABS_VARIANT_64;
    EmitList[ - OP_NOT_VARIANT  ] := EmitOP_NOT_VARIANT_64;
    EmitList[ - OP_ADD_VARIANT  ] := EmitOP_ADD_VARIANT_64;
    EmitList[ - OP_SUB_VARIANT  ] := EmitOP_SUB_VARIANT_64;
    EmitList[ - OP_MULT_VARIANT  ] := EmitOP_MULT_VARIANT_64;
    EmitList[ - OP_DIV_VARIANT  ] := EmitOP_DIV_VARIANT_64;
    EmitList[ - OP_IDIV_VARIANT  ] := EmitOP_IDIV_VARIANT_64;
    EmitList[ - OP_MOD_VARIANT  ] := EmitOP_MOD_VARIANT_64;
    EmitList[ - OP_SHL_VARIANT  ] := EmitOP_SHL_VARIANT_64;
    EmitList[ - OP_SHR_VARIANT  ] := EmitOP_SHR_VARIANT_64;
    EmitList[ - OP_AND_VARIANT  ] := EmitOP_AND_VARIANT_64;
    EmitList[ - OP_OR_VARIANT  ] := EmitOP_OR_VARIANT_64;
    EmitList[ - OP_XOR_VARIANT  ] := EmitOP_XOR_VARIANT_64;
    EmitList[ - OP_LT_VARIANT  ] := EmitOP_LT_VARIANT_64;
    EmitList[ - OP_LE_VARIANT  ] := EmitOP_LE_VARIANT_64;
    EmitList[ - OP_GT_VARIANT  ] := EmitOP_GT_VARIANT_64;
    EmitList[ - OP_GE_VARIANT  ] := EmitOP_GE_VARIANT_64;
    EmitList[ - OP_EQ_VARIANT  ] := EmitOP_EQ_VARIANT_64;
    EmitList[ - OP_NE_VARIANT  ] := EmitOP_NE_VARIANT_64;
    EmitList[ - OP_CURRENCY_FROM_INT  ] := EmitOP_CURRENCY_FROM_INT_64;
    EmitList[ - OP_CURRENCY_FROM_INT64  ] := EmitOP_CURRENCY_FROM_INT64_64;
    EmitList[ - OP_CURRENCY_FROM_UINT64  ] := EmitOP_CURRENCY_FROM_INT64_64;
    EmitList[ - OP_CURRENCY_FROM_REAL  ] := EmitOP_CURRENCY_FROM_REAL_64;
    EmitList[ - OP_VARIANT_FROM_CLASS  ] := EmitOP_VARIANT_FROM_CLASS_64;
    EmitList[ - OP_VARIANT_FROM_POINTER  ] := EmitOP_VARIANT_FROM_POINTER_64;
    EmitList[ - OP_CLASS_FROM_VARIANT  ] := EmitOP_CLASS_FROM_VARIANT_64;
    EmitList[ - OP_INTERFACE_FROM_CLASS  ] := EmitOP_INTERFACE_FROM_CLASS_64;
    EmitList[ - OP_INTERFACE_CAST  ] := EmitOP_INTERFACE_CAST_64;
    EmitList[ - OP_ASSIGN_VARIANT  ] := EmitOP_ASSIGN_VARIANT_64;
    EmitList[ - OP_ASSIGN_OLEVARIANT  ] := EmitOP_ASSIGN_OLEVARIANT_64;
    EmitList[ - OP_ASSIGN_CLASS  ] := EmitOP_ASSIGN_CLASS_64;
    EmitList[ - OP_VARARRAY_GET  ] := EmitOP_VARARRAY_GET_64;
    EmitList[ - OP_VARARRAY_PUT  ] := EmitOP_VARARRAY_PUT_64;
    EmitList[ - OP_OLE_GET  ] := EmitOP_OLE_GET_64;
    EmitList[ - OP_OLE_SET  ] := EmitOP_OLE_SET_64;
    EmitList[ - OP_OLE_PARAM  ] := EmitOP_OLE_PARAM_64;
    EmitList[ - OP_IS  ] := EmitOP_IS_64;
    EmitList[ - OP_TYPEINFO  ] := EmitOP_TYPEINFO_64;
    EmitList[ - OP_ADD_TYPEINFO  ] := EmitPCodeOperator;
    EmitList[ - OP_PUSH_CONTEXT  ] := EmitOP_PUSH_CONTEXT_64;
    EmitList[ - OP_POP_CONTEXT  ] := EmitOP_POP_CONTEXT_64;
    EmitList[ - OP_FIND_CONTEXT  ] := EmitOP_FIND_CONTEXT_64;
    EmitList[ - OP_FIND_JS_FUNC  ] := EmitOP_FIND_JS_FUNC_64;
    EmitList[ - OP_GET_PROG  ] := EmitOP_GET_PROG_64;
    EmitList[ - OP_ONCREATE_HOST_OBJECT  ] := EmitOP_ONCREATE_HOST_OBJECT_64;
    EmitList[ - OP_ONDESTROY_HOST_OBJECT  ] := EmitOP_ONDESTROY_HOST_OBJECT_64;
    EmitList[ - OP_BEFORE_CALL_HOST  ] := EmitOP_BEFORE_CALL_HOST_64;
    EmitList[ - OP_AFTER_CALL_HOST  ] := EmitOP_AFTER_CALL_HOST_64;
    EmitList[ - OP_INIT_FWARRAY  ] := EmitOP_INIT_FWARRAY_64;
    EmitList[ - OP_ONCREATE_OBJECT  ] := EmitOP_ONCREATE_OBJECT_64;
    EmitList[ - OP_ON_AFTER_OBJECT_CREATION  ] := EmitOP_ON_AFTER_OBJECT_CREATION_64;
    EmitList[ - OP_CLASSNAME  ] := EmitOP_CLASSNAME_64;
    EmitList[ - OP_GET_DRTTI_PROP  ] := EmitOP_GET_DRTTI_PROP_64;
    EmitList[ - OP_SET_DRTTI_PROP  ] := EmitOP_SET_DRTTI_PROP_64;
    EmitList[ - OP_GET_ANSISTR_PROP  ] := EmitOP_GET_ANSISTR_PROP_64;
    EmitList[ - OP_SET_ANSISTR_PROP  ] := EmitOP_SET_ANSISTR_PROP_64;
    EmitList[ - OP_GET_WIDESTR_PROP  ] := EmitOP_GET_WIDESTR_PROP_64;
    EmitList[ - OP_SET_WIDESTR_PROP  ] := EmitOP_SET_WIDESTR_PROP_64;
    EmitList[ - OP_GET_UNICSTR_PROP  ] := EmitOP_GET_UNICSTR_PROP_64;
    EmitList[ - OP_SET_UNICSTR_PROP  ] := EmitOP_SET_UNICSTR_PROP_64;
    EmitList[ - OP_GET_ORD_PROP  ] := EmitOP_GET_ORD_PROP_64;
    EmitList[ - OP_SET_ORD_PROP  ] := EmitOP_SET_ORD_PROP_64;
    EmitList[ - OP_GET_INTERFACE_PROP  ] := EmitOP_GET_INTERFACE_PROP_64;
    EmitList[ - OP_SET_INTERFACE_PROP  ] := EmitOP_SET_INTERFACE_PROP_64;
    EmitList[ - OP_GET_SET_PROP  ] := EmitOP_GET_SET_PROP_64;
    EmitList[ - OP_SET_SET_PROP  ] := EmitOP_SET_SET_PROP_64;
    EmitList[ - OP_GET_FLOAT_PROP  ] := EmitOP_GET_FLOAT_PROP_64;
    EmitList[ - OP_SET_FLOAT_PROP  ] := EmitOP_SET_FLOAT_PROP_64;
    EmitList[ - OP_GET_VARIANT_PROP  ] := EmitOP_GET_VARIANT_PROP_64;
    EmitList[ - OP_SET_VARIANT_PROP  ] := EmitOP_SET_VARIANT_PROP_64;
    EmitList[ - OP_GET_INT64_PROP  ] := EmitOP_GET_INT64_PROP_64;
    EmitList[ - OP_SET_INT64_PROP  ] := EmitOP_SET_INT64_PROP_64;
    EmitList[ - OP_GET_EVENT_PROP  ] := EmitOP_GET_EVENT_PROP_64;
    EmitList[ - OP_SET_EVENT_PROP  ] := EmitOP_SET_EVENT_PROP_64;
    EmitList[ - OP_SET_EVENT_PROP2  ] := EmitOP_SET_EVENT_PROP2_64;
    EmitList[ - OP_TRY_ON  ] := EmitOP_TRY_ON_64;
    EmitList[ - OP_EXCEPT_SEH  ] := EmitOP_EXCEPT_SEH_64;
    EmitList[ - OP_TRY_OFF  ] := EmitOP_TRY_OFF_64;
    EmitList[ - OP_FINALLY  ] := EmitOP_FINALLY_64;
    EmitList[ - OP_EXCEPT  ] := EmitOP_EXCEPT_64;
    EmitList[ - OP_EXCEPT_ON  ] := EmitOP_EXCEPT_ON_64;
    EmitList[ - OP_RAISE  ] := EmitOP_RAISE_64;
    EmitList[ - OP_EXIT  ] := EmitOP_EXIT_64;
    EmitList[ - OP_COND_RAISE  ] := EmitOP_COND_RAISE_64;
    EmitList[ - OP_BEGIN_EXCEPT_BLOCK  ] := EmitOP_BEGIN_EXCEPT_BLOCK_64;
    EmitList[ - OP_END_EXCEPT_BLOCK  ] := EmitOP_END_EXCEPT_BLOCK_64;
    EmitList[ - OP_OVERFLOW_CHECK  ] := EmitOP_OVERFLOW_CHECK_64;
    EmitList[ - OP_PAUSE  ] := EmitOP_PAUSE_64;
    EmitList[ - OP_CHECK_PAUSE  ] := EmitOP_CHECK_PAUSE_64;
    EmitList[ - OP_CHECK_PAUSE_LIGHT  ] := EmitOP_CHECK_PAUSE_LIGHT_64;
    EmitList[ - OP_HALT  ] := EmitOP_HALT_64;
    EmitList[ - OP_CREATE_OBJECT  ] := EmitOP_CREATE_OBJECT_64;
    EmitList[ - OP_DESTROY_OBJECT  ] := EmitOP_DESTROY_OBJECT_64;
    EmitList[ - OP_GET_VMT_ADDRESS  ] := EmitOP_GET_VMT_ADDRESS_64;
    EmitList[ - OP_SET_LENGTH  ] := EmitOP_SET_LENGTH_64;
    EmitList[ - OP_SET_LENGTH_EX  ] := EmitOP_SET_LENGTH_EX_64;
    EmitList[ - OP_PUSH_LENGTH  ] := EmitPCodeOperator;
    EmitList[ - OP_CREATE_METHOD  ] := EmitOP_CREATE_METHOD_64;
    EmitList[ - OP_DECLARE_TEMP_VAR  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_USING  ] := EmitPCodeOperator;
    EmitList[ - OP_END_USING  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_BLOCK  ] := EmitPCodeOperator;
    EmitList[ - OP_END_BLOCK  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_WITH  ] := EmitPCodeOperator;
    EmitList[ - OP_END_WITH  ] := EmitPCodeOperator;
    EmitList[ - OP_PARAM_CHANGED  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_SUB  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_CLASS_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_CLASS_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_RECORD_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_RECORD_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_HELPER_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_HELPER_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_ADD_ANCESTOR  ] := EmitNothing;
    EmitList[ - OP_INIT_PANSICHAR_LITERAL  ] := EmitNothing;
    EmitList[ - OP_INIT_PWIDECHAR_LITERAL  ] := EmitNothing;
    EmitList[ - OP_BEGIN_CRT_JS_FUNC_OBJECT  ] := EmitOP_BEGIN_CRT_JS_FUNC_OBJECT_64;
    EmitList[ - OP_END_CRT_JS_FUNC_OBJECT  ] := EmitOP_END_CRT_JS_FUNC_OBJECT_64;
    EmitList[ - OP_TO_JS_OBJECT  ] := EmitOP_TO_JS_OBJECT_64;
    EmitList[ - OP_JS_TYPEOF  ] := EmitOP_JS_TYPEOF_64;
    EmitList[ - OP_JS_VOID  ] := EmitOP_JS_VOID_64;
    EmitList[ - OP_GET_NEXTJSPROP  ] := EmitOP_GET_NEXTJSPROP_64;
    EmitList[ - OP_CLEAR_REFERENCES  ] := EmitOP_CLEAR_REFERENCES_64;
    EmitList[ - OP_DECLARE_LOCAL_VAR  ] := EmitPCodeOperator;
    EmitList[ - OP_LOAD_PROC  ] := EmitPCodeOperator;
    EmitList[ - OP_ADD_MESSAGE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_ARRAY_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_ARRAY_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_SET_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_SET_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_ENUM_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_ENUM_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_SUBRANGE_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_SUBRANGE_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_POINTER_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_POINTER_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_CLASSREF_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_CLASSREF_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_DYNARRAY_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_DYNARRAY_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_SHORTSTRING_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_SHORTSTRING_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_INTERFACE_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_END_INTERFACE_TYPE  ] := EmitPCodeOperator;
    EmitList[ - OP_BEGIN_TEXT  ] := EmitPCodeOperator;
    EmitList[ - OP_END_TEXT  ] := EmitPCodeOperator;
    EmitList[ - OP_WARNINGS_ON  ] := EmitPCodeOperator;
    EmitList[ - OP_WARNINGS_OFF  ] := EmitPCodeOperator;
    EmitList[ - OP_FRAMEWORK_ON  ] := EmitPCodeOperator;
    EmitList[ - OP_FRAMEWORK_OFF  ] := EmitPCodeOperator;
    EmitList[ - OP_DECL_SUB  ] := EmitPCodeOperator;
    EmitList[ - OP_DECLARE_MEMBER ] := EmitPCodeOperator;
    EmitList[ - OP_ABSOLUTE  ] := EmitPCodeOperator;
  end;
{$ENDIF}
end;

function TEmitter.IsLocalPos: Boolean;
begin
  with TKernel(kernel) do
    result := Code.IsLocalPos(Code.N);
end;

function TEmitter.GetLanguage: Integer;
begin
  with TKernel(kernel) do
  result := Code[Code.N].Language;
end;

function TEmitter.GetOperName: String;
begin
  result := Operators[- R.Op];
end;

function TEmitter.Host1: Boolean;
begin
  result := GetSymbolRec(R.Arg1).Host;
end;

function TEmitter.Host2: Boolean;
begin
  result := GetSymbolRec(R.Arg2).Host;
end;

function TEmitter.ByRef1: Boolean;
begin
  result := GetSymbolRec(R.Arg1).ByRef or GetSymbolRec(R.Arg1).ByRefEx;
end;

function TEmitter.GetReg: Integer;
begin
  result := registers.GetReg;
end;

function TEmitter.GetReg(Reg: Integer): Integer;
begin
  registers.GetReg(Reg);
  result := Reg;
end;

function TEmitter.GetRegEx: Integer;
begin
  result := GetReg(_EBX);
end;

function TEmitter.GetReg64: Integer;
begin
  result := registers.GetReg64;
end;

procedure TEmitter.FreeReg(Reg: Integer);
begin
  Registers.FreeReg(Reg);
end;

function TEmitter.ImmValue1: Cardinal;
begin
  result := SymbolRec1.Value;
end;

function TEmitter.ImmValue2: Cardinal;
begin
  result := SymbolRec2.Value;
end;

function TEmitter.GetOffset(S: TSymbolRec): Integer;
begin
  result := TKernel(kernel).GetOffset(S);
end;

function TEmitter.SymbolRec1: TSymbolRec;
begin
  result := GetSymbolRec(R.Arg1);
end;

function TEmitter.SymbolRec2: TSymbolRec;
begin
  result := GetSymbolRec(R.Arg2);
end;

function TEmitter.SymbolRecR: TSymbolRec;
begin
  result := GetSymbolRec(R.Res);
end;

function TEmitter.Lookup(const S: String): Integer;
begin
  result := SymbolTable.LookUp(S, 0, false);
end;

function TEmitter.CreateSymbolProgram(i_kernel: Pointer): TSymbolProg;
var
  Code: TCode;
  I, J, ShiftValue: Integer;

  SymbolProgRec: TSymbolProgRec;
  SymbolRec: TSymbolRec;
  Modules: TModuleList;
  b: Boolean;
  K, KK: Integer;
  SZ: Integer;
begin
  Self.kernel := i_kernel;

  EmitOff := false;
  HandlesEvents := false;

  Prg := TSymbolProg.Create(kernel);
  Prg.Clear;

  result := Prg;

  Code := TKernel(kernel).Code;

//  Prg.AsmAddREG_Imm(_ECX, $44);
//  Prg.AsmAddREG_Imm(_R10, $44);
//  Exit;
//  Prg.AsmPutREG8_EBPPtr(_ecx, $444444);
//  Prg.AsmPutREG8_EBPPtr(_r8, $444444);
//  Prg.AsmMovREG8_REGPtr(_ecx, _ebx);
//  Prg.AsmMovREGPtr_REG8(_ecx, _r11);
//  Prg.AsmMovREGPtr_REG8(_r10, _ecx);
//  Prg.AsmMovREGPtr_REG8(_r10, _r11);

//  Prg.AsmMovREG8_REGPtr(_ecx, _ebx);
//  exit;
{
  Prg.AsmMovREGPtr_Imm(_R9, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R10, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R11, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R12, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R13, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R14, $80,$80);
  Prg.AsmMovREGPtr_Imm(_R15, $80,$80);
  Exit;
}
{
  Prg.AsmMovREG64_RSPPtr(_EAX, $80);
  Prg.AsmMovREG64_RSPPtr(_ECX, $80);
  Prg.AsmMovREG64_RSPPtr(_EDX, $80);
  Prg.AsmMovREG64_RSPPtr(_EBX, $80);
  Prg.AsmMovREG64_RSPPtr(_ESI, $80);
  Prg.AsmMovREG64_RSPPtr(_EDI, $80);
  Prg.AsmMovREG64_RSPPtr(_R8D, $80);
  Prg.AsmMovREG64_RSPPtr(_R9D, $80);
  Exit;
}
//  Prg.AsmPutREG64_RBPPtr(_R8D, $4000);
//  Exit;
//  Prg.AsmCmpREGPtr_Imm(_EAX, Code.GetSymbolRec(1000014), 4000000000);
//  Prg.AsmNEG_REGPtr(_EAX, Code.GetSymbolRec(1000014));
//  Prg.AsmMovREGPtr_Imm(_EAX, 4000000000, 4000000000);
//  Prg.AsmFstpExtended_REGPtr(_EAX, Code.GetSymbolRec(1000014));
//  Exit;

  try

    Prg.AsmJMP_Imm(0);

    case TargetPlatform of
      tpOSX32, tpIOSSim:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmPush_REG(_EBX);
        Prg.AsmPush_REG(_ESI);
        Prg.AsmPush_REG(_EDI);
        Prg.AsmMovREG_REG(_EBP, _ESP);
        Prg.AsmSubREG_Imm(_ESP, $10);

        Prg.EmitGetCallerEIP;
      end;
      tpWIN64:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmSubReg_Imm(_ESP, $100);
        Prg.AsmMovREG_REG(_EBP, _ESP);

      end;
    end;

    for I:= 1 to Code.Card do
    begin
      R := Code.Records[I];
{$IFNDEF PAXARM}
      if R.Op = OP_INIT_PANSICHAR_LITERAL then
      begin
        EmitOP_INIT_PCHAR_LITERAL;
        R.Op := OP_NOP;
      end
      else
{$ENDIF}
      if R.Op = OP_INIT_PWIDECHAR_LITERAL then
      begin
        EmitOP_INIT_PWIDECHAR_LITERAL;
        R.Op := OP_NOP;
      end;
    end;

    for I:= 1 to Code.Card do
    begin
      R := Code.Records[I];
      if R.Op = OP_LOAD_PROC then
      begin
        if TargetPlatform = tpWIN64 then
          EmitOP_LOAD_PROC_64
        else
          EmitOP_LOAD_PROC;
      end;
    end;

    K := 0;
    b := false;
    for I:= 1 to Code.Card do
    begin
      R := Code.Records[I];
      if R.Op = OP_BEGIN_INIT_CONST then
      begin
        b := true;
        R.Op := OP_NOP;
        Inc(K);
      end
      else if R.Op = OP_END_INIT_CONST then
      begin
        Dec(K);
        if K = 0 then
          b := false;
        R.Op := OP_NOP;
      end
      else if b then
      begin
        Code.N := I;
        Emit(I);
        R.Op := OP_NOP;
      end;
    end;

    for I:= 1 to Code.Card do
    begin
      R := Code.Records[I];
      if R.Op = OP_ADD_MESSAGE then
      begin
        if TargetPlatform = tpWIN64 then
          EmitOP_ADD_MESSAGE_64
        else
          EmitOP_ADD_MESSAGE;
      end;
    end;

    Modules := TKernel(kernel).Modules;
    Modules.Recalc;

    // emit initialization sections

    for J:=0 to Modules.LoadOrder.Count - 1 do
      if Modules[J].PInitBegin > 0 then
        for I:= Modules[J].PInitBegin + 1 to Modules[J].PInitEnd - 1 do
          Emit(I);

    EmitOP_CHECK_INIT_ONLY;
    for KK:=1 to MAGIC_INITIALIZATION_JMP_COUNT do
      Prg.AsmJMP_Imm(0);

    case TargetPlatform of
      tpOSX32, tpIOSSim:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmPush_REG(_EBX);
        Prg.AsmPush_REG(_ESI);
        Prg.AsmPush_REG(_EDI);
        Prg.AsmMovREG_REG(_EBP, _ESP);
        Prg.AsmSubREG_Imm(_ESP, $10);

        Prg.EmitGetCallerEIP;
      end;
      tpWIN64:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmSubReg_Imm(_ESP, $100);
        Prg.AsmMovREG_REG(_EBP, _ESP);
      end;
    end;

    I := 1;
    while I < Code.Card do
    begin
      R := Code.Records[I];
      Code.N := I;
      if R.Op = OP_BEGIN_CRT_JS_FUNC_OBJECT then
      begin
        EmitOP_BEGIN_CRT_JS_FUNC_OBJECT;
        R.Op := OP_NOP;
        repeat
          Inc(I);
          R := Code.Records[I];
          Code.N := I;
          Emit(I);
          if R.Op = OP_END_CRT_JS_FUNC_OBJECT then
          begin
            R.Op := OP_NOP;
            break;
          end
          else
            R.Op := OP_NOP;
        until false;
      end;

      Inc(I);
    end;

    I := 1;
    while I <= Code.Card do
    begin
      R := Code.Records[I];
      Code.N := I;

      if R.Op = OP_SEPARATOR then
      begin
        EmitOP_SEPARATOR(I);
        Inc(I);
        continue;
      end
      else if R.Op = OP_EMIT_ON then
      begin
        EmitOP_EMIT_ON;
        Inc(I);
        continue;
      end
      else if R.Op = OP_EMIT_OFF then
      begin
        EmitOP_EMIT_OFF;
        Inc(I);
        continue;
      end
      else if R.Op = OP_END_INITIALIZATION then
      begin
        EmitOP_EMIT_ON;
        Inc(I);
        continue;
      end
      else if R.Op = OP_BEGIN_INITIALIZATION then
      begin
        EmitOP_EMIT_OFF;
        Inc(I);
        continue;
      end
      else if R.Op = OP_END_FINALIZATION then
      begin
        EmitOP_EMIT_ON;
        Inc(I);
        continue;
      end
      else if R.Op = OP_BEGIN_FINALIZATION then
      begin
        EmitOP_EMIT_OFF;
        Inc(I);
        continue;
      end;

      if not EmitOff then
        Emit(I);

      Inc(I);
    end;

    Prg.Optimization;

    ShiftValue := 0;
    for I:=1 to Prg.Card do
    begin
      Prg[I].ShiftValue := ShiftValue;

      J := Prg[I].LabelId;
      if J > 0 then
        SymbolTable[J].Value := ShiftValue;

      SZ := Prg[I].Size;
      if SZ > 0 then
        ShiftValue := ShiftValue + SZ;
    end;

    for I:=0 to List1.Count - 1 do
    begin
      SymbolProgRec := TSymbolProgRec(List1[I]); // record of symbol code
      ShiftValue := SymbolProgRec.ShiftValue;
      SymbolRec := TSymbolRec(List3[I]); // label
      if SymbolProgRec.Op = ASM_JMP then
      begin
        ShiftValue := SymbolRec.Value - ShiftValue - SymbolProgRec.Size;
        Move(ShiftValue, SymbolProgRec.code[1], 4);
        SymbolProgRec.Decompile;
      end
      else if SymbolProgRec.Op = ASM_PUSH then // pause
      begin
        ShiftValue := SymbolRec.Value;
        Move(ShiftValue, SymbolProgRec.code[1], 4);
        SymbolProgRec.Decompile;
      end
      else
        RaiseError(errInternalError, []);
    end;

    for I:=0 to List2.Count - 1 do
    begin
      SymbolProgRec := TSymbolProgRec(List2[I]); // record of symbol code
      ShiftValue := SymbolTable[SymbolProgRec.SaveSubId].Value;

  //    if ShiftValue < 0 then
  //      RaiseError(errInternalError, []);

      if TargetPlatform = tpWIN64 then
        Move(ShiftValue, SymbolProgRec.code[3], 4)
      else
      begin
        if SymbolProgRec.code[0] = $05 then // ADD EAX, Imm
          Move(ShiftValue, SymbolProgRec.code[1], 4)
        else
          Move(ShiftValue, SymbolProgRec.code[2], 4);
      end;

      SymbolProgRec.Decompile;
    end;
  finally
  end;
end;

procedure TEmitter.Emit(I: Integer);
var
  Code: TCode;
  J, KK, Op: Integer;
  Modules: TModuleList;
begin
  Code := TKernel(kernel).Code;

  R := Code.Records[I];
  Code.N := I;
  Op := R.Op;

  if Op = OP_SEPARATOR then
  begin
    EmitOP_SEPARATOR(I);
  end
  else if Op = OP_SET_CODE_LINE then
  begin
    Prg.AsmComment('***** N ****** ' + IntToStr(Code.N));
    Prg.AsmMovREGPtr_Imm(_ESI, H_ByteCodePtr, I);
  end
  else if Op = OP_STMT then
  begin
    Prg.AsmMovREGPtr_Imm(_ESI, H_ByteCodePtr, I);
  end
  else if Op = OP_BEGIN_MODULE then
  begin
    Prg.AsmComment('Module ' + Code.GetModule(I).Name);
  end
  else if Op = OP_END_INTERFACE_SECTION then
  begin
    Prg.AsmComment('End of interface section of ' + Code.GetModule(I).Name);
  end
  else if Op = OP_END_MODULE then
  begin
    Prg.AsmComment('End of module ' + Code.GetModule(I).Name);

    case TargetPlatform of
      tpOSX32, tpIOSSim:
      begin
        Prg.AsmAddReg_Imm(_ESP, $10);
        Prg.AsmPop_REG(_EDI);
        Prg.AsmPop_REG(_ESI);
        Prg.AsmPop_REG(_EBX);
        Prg.AsmPop_REG(_EBP);
      end;
      tpWIN64:
      begin
        Prg.AsmAddReg_Imm(_ESP, $100);
        Prg.AsmPop_REG(_EBP);
      end;
    end;

    EmitOP_CHECK_BODY_ONLY;

    for KK:=1 to MAGIC_FINALIZATION_JMP_COUNT do
      Prg.AsmJMP_Imm(0);

    case TargetPlatform of
      tpOSX32, tpIOSSim:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmPush_REG(_EBX);
        Prg.AsmPush_REG(_ESI);
        Prg.AsmPush_REG(_EDI);
        Prg.AsmMovREG_REG(_EBP, _ESP);
        Prg.AsmSubReg_Imm(_ESP, $10);

        Prg.EmitGetCallerEIP;
      end;
      tpWIN64:
      begin
        Prg.AsmPush_REG(_EBP);
        Prg.AsmSubReg_Imm(_ESP, $100);
        Prg.AsmMovREG_REG(_EBP, _ESP);
      end;
    end;

    // emit finalization sections
    Modules := TKernel(kernel).Modules;

    for J:=Modules.LoadOrder.Count - 1 downto 0 do
    if Modules[J].PFinBegin > 0 then
      for I:= Modules[J].PFinBegin + 1 to Modules[J].PFinEnd - 1 do
        Emit(I);

    case TargetPlatform of
      tpOSX32, tpIOSSim:
      begin
        Prg.AsmAddReg_Imm(_ESP, $10);
        Prg.AsmPop_REG(_EDI);
        Prg.AsmPop_REG(_ESI);
        Prg.AsmPop_REG(_EBX);
        Prg.AsmPop_REG(_EBP);
      end;
      tpWIN64:
      begin
        Prg.AsmAddReg_Imm(_ESP, $100);
        Prg.AsmPop_REG(_EBP);
      end;
    end;

    Prg.AsmRet;
  end
  else if Op = OP_LABEL then
    EmitLabel(R.Arg1, GetSymbolRec(R.Arg1).Name)
  else
    EmitList[-Op];
end;

procedure TEmitter.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).RaiseError(Message, params);
end;

procedure TEmitter.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

function TEmitter.GetSymbolRec(Id: Integer): TSymbolRec;
begin
  result := TKernel(kernel).SymbolTable[Id];
end;

function TEmitter.SymbolTable: TSymbolTable;
begin
  result := TKernel(kernel).SymbolTable;
end;

function TEmitter.ByteCode: TCode;
begin
  result := TKernel(kernel).Code;
end;

procedure TEmitter.EmitPCodeOperator;
var
  S: String;
  N: Integer;
begin
  N := TKernel(kernel).Code.N;
  if (R.Op = OP_CALL) or (R.Op = OP_LOAD_PROC) then
  begin
    if SymbolRec1.MethodIndex > 0 then
      S := '(' + IntToStr(SymbolRec1.MethodIndex) + ')' + ' *** N *** ' + IntToStr(N)
    else
      S := ' *** N *** ' + IntToStr(N);

    Prg.AsmComment('             ' + OperName + '  ' +
      GetSymbolRec(R.Arg1).FullName + '[' + IntToStr(R.Arg1) + ']' + S);
  end
  else
    Prg.AsmComment('             ' + OperName + ' *** N *** ' + IntToStr(N));
end;

procedure TEmitter.EmitComment;
var
  S: String;
begin
  S := SymbolRec1.Value;
  Prg.AsmComment(S);
end;

procedure TEmitter.EmitStartSub(SubId: Integer);
var
  R: TSymbolProgRec;
begin
  R := Prg.AsmComment('START SUB [' + IntToStr(SubId) + ']; // ' + GetSymbolRec(SubId).FullName);
  if not GetSymbolRec(SubId).IsNestedSub then
    R.MapSub := - SubId;
end;

procedure TEmitter.EmitFinSub(SubId: Integer);
var
  R: TSymbolProgRec;
begin
  R := Prg.AsmComment('FIN   SUB [' + IntToStr(SubId) + ']; // ' + GetSymbolRec(SubId).FullName);
  if not GetSymbolRec(SubId).IsNestedSub then
    R.MapSub := SubId;
end;

procedure TEmitter.EmitOP_SEPARATOR(I: Integer);
{$IFDEF TRIAL}
var SubId: Integer;
{$ENDIF}
begin
{$IFDEF DUMP}
  Prg.AsmComment('------------------------------------------------------');
  Prg.AsmComment(TKernel(kernel).Code.GetSourceLine(I));
  Prg.AsmComment('------------------------------------------------------');
{$ENDIF}

  {$IFDEF TRIAL}
   Inc(_Counter);
   if _Counter mod 101 = 0 then
   begin
     SubId := TKernel(kernel).SymbolTable.LookUp(strShowTrial, 0, false);

     EmitCallPro(SubId);
     EmitStdCall(SubId);
   end;
  {$ENDIF}
end;

procedure TEmitter.EmitOP_GO;
begin
  EmitPCodeOperator;

  EmitJmp;
end;

procedure TEmitter.EmitOP_PAUSE_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_PAUSE;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Pause;

  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESP);
  Prg.AsmPush_REG(Reg);    // push _ESP

  Prg.AsmMovREG_REG(Reg, _EBP);
  Prg.AsmPush_REG(Reg);    // push _EBP

  Prg.AsmPush_Imm(0);

  List3.Add(SymbolRec1);
  List1.Add(Prg.Top);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_CHECK_PAUSE_64;
begin
//  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_CHECK_PAUSE;
begin
  EmitPCodeOperator;

  Prg.AsmNOP;
  Prg.AsmNOP;
  Prg.AsmNOP;
  Prg.AsmNOP;
  Prg.AsmNOP;

  EmitCallPro(Id_CheckPause);

  GetReg(_EAX);
  Prg.AsmMovREG_REG(_EAX, _ESI);
  Prg.AsmAddREG_Imm(_EAX, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(_EAX, _EAX); // load TProgram.Self
  FreeReg(_EAX);

  EmitStdCall(Id_CheckPause);

  Prg.AsmCmpReg32Ptr_Imm(_ESI, H_Flag, 1);
  Prg.AsmJNZ_Imm(29);

  R.Op := OP_PAUSE;
  EmitOP_PAUSE;
  R.Op := OP_CHECK_PAUSE;
end;

procedure TEmitter.EmitOP_CHECK_PAUSE_LIGHT_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_CHECK_PAUSE_LIGHT;
begin
  EmitPCodeOperator;

  Prg.AsmCmpReg32Ptr_Imm(_ESI, H_Flag, 1);
  Prg.AsmJNZ_Imm(29);

  R.Op := OP_PAUSE;
  EmitOP_PAUSE;
  R.Op := OP_CHECK_PAUSE;
end;

procedure TEmitter.EmitOP_HALT_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_HALT;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Halt;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmPush_Imm(SymbolRec1.Value);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_CHECK_INIT_ONLY;
begin
  EmitPCodeOperator;

  case TargetPlatform of
    tpOSX32, tpIOSSim:
    begin
      Prg.AsmCmpReg32Ptr_Imm(_ESI, H_InitOnly, 2);
      Prg.AsmJNZ_Imm(11);
      Prg.AsmAddReg_Imm(_ESP, $10);  // 6
      Prg.AsmPop_REG(_EDI);          // 1
      Prg.AsmPop_REG(_ESI);          // 1
      Prg.AsmPop_REG(_EBX);          // 1
      Prg.AsmPop_REG(_EBP);          // 1
      Prg.AsmRet();                  // 1
    end;
    tpWIN64:
    begin
      Prg.AsmCmpReg32Ptr_Imm(_ESI, H_InitOnly, 2);
      Prg.AsmJNZ_Imm(9);
      Prg.AsmAddReg_Imm(_ESP, $100); // 7
      Prg.AsmPop_Reg(_EBP);          // 1
      Prg.AsmRet();                  // 1
    end;
    else
    begin
      Prg.AsmCmpReg32Ptr_Imm(_ESI, H_InitOnly, 2);
      Prg.AsmJNZ_Imm(1);
      Prg.AsmRet();
    end;
  end;
end;

procedure TEmitter.EmitOP_CHECK_BODY_ONLY;
begin
  EmitPCodeOperator;

  Prg.AsmCmpReg32Ptr_Imm(_ESI, H_BodyOnly, 3);
  Prg.AsmJNZ_Imm(1);
  Prg.AsmRet();
end;

procedure TEmitter.EmitOP_TRY_ON_64;
begin
  EmitPCodeOperator;
{
  SubId := Id_TryOn;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self

  Prg.AsmMovReg_Imm(Reg, R.Arg1);   // block index
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_REG(_R8, _EBP);
  Prg.AsmMovREG_REG(_R9, _ESP);

  EmitGet_REG(Reg, SymbolTable[SubId]);
  Prg.AsmCall_REG(Reg);

  FreeReg(Reg);
}
end;
procedure TEmitter.EmitOP_TRY_ON;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  if TKernel(kernel).ModeSEH then
  begin
    Reg := _EAX;

    Prg.AsmPush_REG(_ESP);

    Prg.AsmPush_Imm(PAX_SEH); // magic
    Prg.AsmPush_Imm(R.Arg1);  // block index

    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
    Prg.AsmPush_REG(Reg);             // push TProgram.Self

    Prg.AsmPush_Imm(0); // Self of method

    Prg.AsmPush_REG(_EBP);

    EmitGet_REG(Reg, SymbolTable[Id_PaxSEHHandler]);
    Prg.AsmPush_REG(Reg);             // push handler

    Prg.AsmXorREG_REG(_EAX, _EAX);
    Prg.AsmPush_FS_REGPtr(_EAX);
    Prg.AsmMovFS_REGPtr_REG32(_EAX, _ESP);
    Exit;
  end;

  SubId := Id_TryOn;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
    Prg.AsmMovREG_REG(_ECX, Reg);             // push TProgram.Self

    Prg.AsmMovReg_Imm(Reg, R.Arg1);   // block index
    Prg.AsmMovREG_REG(_EDX, Reg);

    Prg.AsmMovREG_REG(_R8, _EBP);
    Prg.AsmMovREG_REG(_R9, _ESP);
    FreeReg(Reg);

    EmitStdCall(SubId);
  end
  else
  begin
    Prg.AsmPutREG_ESIPtr(_EDX, 0);

    Reg := GetReg;

    Prg.AsmMovREG_REG(Reg, _ESP);
    Prg.AsmPush_REG(Reg);    // push _ESP

    Prg.AsmMovREG_REG(Reg, _EBP);
    Prg.AsmPush_REG(Reg);    // push _EBP

    Prg.AsmPush_Imm(R.Arg1); // block index

    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
    Prg.AsmPush_REG(Reg);             // push TProgram.Self
    FreeReg(Reg);

    EmitStdCall(SubId);

    Prg.AsmGetREG_ESIPtr(_EDX, 0);
  end;
end;

procedure TEmitter.EmitOP_EXCEPT_SEH_64;
begin
end;
procedure TEmitter.EmitOP_EXCEPT_SEH;
begin
  EmitPCodeOperator;

  if TKernel(kernel).ModeSEH then
  begin
    Prg.AsmXorREG_REG(_EAX, _EAX);
    Prg.AsmPop_REG(_EDX); // 4
    Prg.AsmMovFS_REGPtr_REG32(_EAX, _EDX);
    Prg.AsmAddREG_Imm(_ESP, SizeOf(TPaxExcFrame) - 4);
  end;
end;

procedure TEmitter.EmitOP_TRY_OFF_64;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_TryOff;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self

  Prg.AsmMovReg_Imm(_EDX, R.Arg1); // block index
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_TRY_OFF;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  if TKernel(kernel).ModeSEH then
  begin
    exit;
  end;

  SubId := Id_TryOff;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmPush_Imm(R.Arg1); // block index

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_FINALLY_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Finally;
  EmitCallPro(SubId);

  Reg := _ECX;
  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_FINALLY;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Finally;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_EXCEPT_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_EXCEPT;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_EXCEPT_ON_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_EXCEPT_ON;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_EXIT_64;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Exit;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self

  Prg.AsmMovREG_Imm(Reg, R.Res); // level
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(_R8, R.Arg2); // exit mode
  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_EXIT;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Exit;
  EmitCallPro(SubId);

  Prg.AsmPush_Imm(R.Arg2); // exit mode
  Prg.AsmPush_Imm(R.Res); // level

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_RAISE_64;
begin
  RaiseError(errNotImplementedYet, []);
end;

procedure TEmitter.EmitOP_RAISE;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Raise;
  EmitCallPro(SubId);

  Prg.AsmPush_Reg(_ESP);
  Prg.AsmPush_Imm(R.Arg2); // raise mode

  Reg := GetReg;

  if R.Arg1 = 0 then
    Prg.AsmPush_Imm(R.Arg1) // block index
  else
  begin
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_OVERFLOW_CHECK_64;
begin
  EmitPCodeOperator;
  if R.Arg1 > 0 then
    OverflowCheck := true
  else
    OverflowCheck := false;
end;
procedure TEmitter.EmitOP_OVERFLOW_CHECK;
begin
  EmitPCodeOperator;
  if R.Arg1 > 0 then
    OverflowCheck := true
  else
    OverflowCheck := false;
end;

procedure TEmitter.EmitOP_BEGIN_EXCEPT_BLOCK_64;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_BeginExceptBlock;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_BEGIN_EXCEPT_BLOCK;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_BeginExceptBlock;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_END_EXCEPT_BLOCK_64;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_EndExceptBlock;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_END_EXCEPT_BLOCK;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_EndExceptBlock;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_COND_RAISE_64;
var
  Reg, SubId, temp: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CondRaise;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self

  EmitLoadAddress(Reg, SymbolRecR);  // IsExit
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(_R8, SymbolRecR.Level); // subid

  Prg.AsmMovREG_Imm(_R9, R.Arg2); // last cond raise

  Prg.AsmMovREG_Imm(Reg, _ESP); // subid
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);

  EmitStdCall(SubId);

  if R.Arg1 > 0 then
  begin
    Reg := EmitGetAddressRegister(SymbolRecR);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRecR, 0);
    Prg.AsmJZ_Imm(5);
    EmitJmp;
    FreeReg(Reg);

    if R.BreakLabel > 0 then
    begin
      Reg := EmitGetAddressRegister(SymbolRecR);
      Prg.AsmCmpREGPtr_Imm(Reg, SymbolRecR, 0);
      Prg.AsmJZ_Imm(5);
      temp := R.Arg1;
      R.Arg1 := R.BreakLabel;
      EmitJmp;
      R.Arg1 := temp;
      FreeReg(Reg);
    end;
  end;
end;
procedure TEmitter.EmitOP_COND_RAISE;
var
  Reg, SubId, temp: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CondRaise;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmPush_Reg(_ESP);
  Prg.AsmPush_Imm(R.Arg2); // last cond raise

  Prg.AsmPush_Imm(SymbolRecR.Level); // subid

  EmitLoadAddress(Reg, SymbolRecR);  // IsExit
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);   // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self
  FreeReg(Reg);

  EmitStdCall(SubId);

  if R.Arg1 > 0 then
  begin
    Reg := EmitGetAddressRegister(SymbolRecR);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRecR, 1);
    Prg.AsmJNZ_Imm(5);
    EmitJmp;
    FreeReg(Reg);

    if R.BreakLabel > 0 then
    begin
      Reg := EmitGetAddressRegister(SymbolRecR);
      Prg.AsmCmpREGPtr_Imm(Reg, SymbolRecR, 2);
      Prg.AsmJNZ_Imm(5);
      temp := R.Arg1;
      R.Arg1 := R.BreakLabel;
      EmitJmp;
      R.Arg1 := temp;
      FreeReg(Reg);
    end;
    if R.ContinueLabel > 0 then
    begin
      Reg := EmitGetAddressRegister(SymbolRecR);
      Prg.AsmCmpREGPtr_Imm(Reg, SymbolRecR, 3);
      Prg.AsmJNZ_Imm(5);
      temp := R.Arg1;
      R.Arg1 := R.ContinueLabel;
      EmitJmp;
      R.Arg1 := temp;
      FreeReg(Reg);
    end;
  end;
end;

procedure TEmitter.EmitOP_GO_1;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 1);

  Prg.AsmJNZ_Imm(5);
  EmitJmp;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GO_2;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 2);

  Prg.AsmJNZ_Imm(5);
  EmitJmp;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GO_3;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 3);

  Prg.AsmJNZ_Imm(5);
  EmitJmp;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GO_TRUE_64;
begin
  EmitOP_GO_TRUE;
end;
procedure TEmitter.EmitOP_GO_TRUE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  if SymbolRec2.Kind = kindCONST then
  begin
    if SymbolRec2.Value <> 0 then
      EmitJmp;
    Exit;
  end;

{
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2);
  Prg.AsmCmpByteREGPtr_Imm(Reg, 0);
}

  Reg := EmitGetAddressRegister(SymbolRec2);
  Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec2, 0);

  Prg.AsmJZ_Imm(5);
  EmitJmp;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GO_DL_64;
begin
  EmitOP_GO_DL;
end;
procedure TEmitter.EmitOP_GO_DL;
begin
  EmitPCodeOperator;
  Prg.AsmTestREG8_REG8(_EDX, _EDX);
  Prg.AsmJNZ_Imm(5);
  EmitJmp;
end;

procedure TEmitter.EmitSaveRBX;
var
  Id, SubId, S: Integer;
begin
  SubId := ByteCode.GetCurrSubId(ByteCode.N);
  if SubId > 0 then
  begin
    Id := SymbolTable.GetRBX_Id(SubId);
    S := GetOffset(SymbolTable[Id]);
  end
  else
    S := -4;

  Prg.AsmPutREG32_EBPPtr(_EBX, S);
end;

procedure TEmitter.EmitRestoreRBX;
var
  Id, SubId, S: Integer;
begin
  SubId := ByteCode.GetCurrSubId(ByteCode.N);
  if SubId > 0 then
  begin
    Id := SymbolTable.GetRBX_Id(SubId);
    S := GetOffset(SymbolTable[Id]);
  end
  else
    S := -4;

  Prg.AsmGetREG32_EBPPtr(_EBX, S);
end;

procedure TEmitter.EmitSaveRDI;
var
  Id, SubId, S: Integer;
begin
  SubId := ByteCode.GetCurrSubId(ByteCode.N);
  if SubId > 0 then
  begin
    Id := SymbolTable.GetRDI_Id(SubId);
    S := GetOffset(SymbolTable[Id]);
  end
  else
    S := -8;

  Prg.AsmPutREG32_EBPPtr(_EDI, S);
end;

procedure TEmitter.EmitRestoreRDI;
var
  Id, SubId, S: Integer;
begin
  SubId := ByteCode.GetCurrSubId(ByteCode.N);
  if SubId > 0 then
  begin
    Id := SymbolTable.GetRDI_Id(SubId);
    S := GetOffset(SymbolTable[Id]);
  end
  else
    S := -8;

  Prg.AsmGetREG32_EBPPtr(_EDI, S);
end;

procedure TEmitter.EmitOP_SAVE_EDX_64;
begin
  EmitOP_SAVE_EDX;
end;

procedure TEmitter.EmitOP_RESTORE_EDX_64;
begin
  EmitOP_RESTORE_EDX;
end;

procedure TEmitter.EmitOP_SAVE_EDX;
var
  SubId, DL_id: Integer;
begin
  EmitPCodeOperator;

  if TargetPlatform = tpWin32 then
    Prg.AsmPush_REG(_EDX)
  else
  begin
    SubId := ByteCode.GetCurrSubId(ByteCode.N);
    Dl_Id := SymbolTable.GetDL_Id(SubId);
    EmitSaveIntVal(_EDX, GetSymbolRec(DL_Id));
  end;
end;

procedure TEmitter.EmitOP_RESTORE_EDX;
var
  SubId, DL_id: Integer;
begin
  EmitPCodeOperator;

  if TargetPlatform = tpWin32 then
    Prg.AsmPop_REG(_EDX)
  else
  begin
    SubId := ByteCode.GetCurrSubId(ByteCode.N);
    Dl_Id := SymbolTable.GetDL_Id(SubId);
    EmitLoadIntVal(_EDX, GetSymbolRec(DL_Id));
  end;
end;

procedure TEmitter.EmitOP_GO_FALSE_64;
begin
  EmitOP_GO_FALSE;
end;
procedure TEmitter.EmitOP_GO_FALSE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  if SymbolRec2.Kind = kindCONST then
  begin
    if SymbolRec2.Value = 0 then
      EmitJmp;
    Exit;
  end;

{
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2);
  Prg.AsmCmpByteREGPtr_Imm(Reg, 0);
}

  Reg := EmitGetAddressRegister(SymbolRec2);
  Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec2, 0);

  Prg.AsmJNZ_Imm(5);
  EmitJmp;
  FreeReg(Reg);
end;

procedure TEmitter.EmitLabel(LabelId: Integer; const LabelName: String);
begin
  Prg.AsmComment('LABEL: ' + LabelName);
  Prg.Top.LabelId := LabelId;
end;

procedure TEmitter.EmitOP_ASSIGN_INT_I_64;
begin
  EmitOP_ASSIGN_INT_I;
end;
procedure TEmitter.EmitOP_ASSIGN_INT_I;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := EmitGetAddressRegister(SymbolRec1);
  Prg.AsmMovREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ASSIGN_INT_M_64;
begin
  EmitOP_ASSIGN_INT_M;
end;
procedure TEmitter.EmitOP_ASSIGN_INT_M;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRec1);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ASSIGN_PANSICHAR_64;
begin
  EmitOP_ASSIGN_PANSICHAR;
end;
procedure TEmitter.EmitOP_ASSIGN_PANSICHAR;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitGet_REG(Reg, SymbolRec2);
  if SymbolRec2.Host or SymbolRec2.ByRef or SymbolRec2.ByRefEx then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  EmitSaveIntVal(Reg, SymbolRec1);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ASSIGN_PWIDECHAR_64;
begin
  EmitOP_ASSIGN_PWIDECHAR;
end;
procedure TEmitter.EmitOP_ASSIGN_PWIDECHAR;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitGet_REG(Reg, SymbolRec2);
  if SymbolRec2.Host or SymbolRec2.ByRef or SymbolRec2.ByRefEx then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  EmitSaveIntVal(Reg, SymbolRec1);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ASSIGN_EVENT_64;
begin
  EmitOP_ASSIGN_EVENT;
end;
procedure TEmitter.EmitOP_ASSIGN_EVENT;
var
  Reg, Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  HandlesEvents := true;

  Reg := GetReg;
  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadAddress(Reg1, SymbolRec1);
  EmitLoadAddress(Reg2, SymbolRec2);

  Prg.AsmMovREG_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG(Reg1, Reg);

  Prg.AsmAddREG_Imm(Reg1, SizeOfPointer);
  Prg.AsmAddREG_Imm(Reg2, SizeOfPointer);

  Prg.AsmMovREG_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG(Reg1, Reg);

  FreeReg(Reg);
  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_CREATE_EVENT_64;
begin
  EmitOP_CREATE_EVENT;
end;
procedure TEmitter.EmitOP_CREATE_EVENT;
var
  Reg, Reg1, Reg2: Integer;
  SymbolProgRec: TSymbolProgRec;
begin
  EmitPCodeOperator;

  HandlesEvents := true;

  Reg := GetReg;
  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1); // instance

  if SymbolRec2.IsVirtual then
  begin
    prg.AsmMovREG_REGPtr(Reg2, Reg1);

    if SymbolRec2.MethodIndex = 0 then
    begin
      SymbolProgRec := prg.AsmAddREG_Imm(Reg2, 0);
      SymbolProgRec.MustBeFixed := true;
      SymbolProgRec.OpOffset := 2;
      SymbolProgRec.SubId := SymbolRec2.Id;
    end
    else
      prg.AsmAddREG_Imm(Reg2, (SymbolRec2.MethodIndex - 1) * SizeOfPointer);

    prg.AsmMovREG_REGPtr(Reg2, Reg2);
  end
  else
  begin
    if Host2 then
      EmitLoadAddress(Reg2, SymbolRec2) // address of method
    else
    begin
      Prg.AsmMovREG_REG(Reg2, _EDI);
      Prg.AsmAddREG_Imm(Reg2, 0);
      Prg.Top.SaveSubId := R.Arg2;
      List2.Add(Prg.Top);
    end;
  end;

  EmitLoadAddress(Reg, SymbolRecR); // address of event

  Prg.AsmMovREGPtr_REG(Reg, Reg2); // code
  Prg.AsmAddREG_Imm(Reg, SizeOfPointer);
  Prg.AsmMovREGPtr_REG(Reg, Reg1); // data

  FreeReg(Reg);
  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_ASSIGN_RECORD_64;
var
  Reg, S, TypeId, SubId: Integer;
  L: TIntegerList;
begin
  TypeId := SymbolRec1.TerminalTypeId;
  L := SymbolTable.GetShiftsOfDynamicFields(TypeId);

  try
    if L.Count = 0 then
    begin
      EmitPCodeOperator;

      SubId := Id_RecordAssign;
      EmitCallPro(SubId);

      Reg := GetRegEx;

      EmitLoadAddress(Reg, SymbolRecR); // dest
      Prg.AsmMovREG_REG(_ECX, Reg);

      EmitLoadAddress(Reg, SymbolRec2); // source
      Prg.AsmMovREG_REG(_EDX, Reg);

      S := SymbolRec1.PtrSize;
      Prg.AsmMovREG_Imm(_R8, S);
      FreeReg(Reg);

      EmitStdCall(SubId);
    end
    else
      EmitOP_ASSIGN_RECORD_EX;
  finally
    FreeAndNil(L);
  end;
end;
procedure TEmitter.EmitOP_ASSIGN_RECORD;
var
  Reg, S, TypeId, SubId: Integer;
  L: TIntegerList;
begin
  TypeId := SymbolRec1.TerminalTypeId;
  L := SymbolTable.GetShiftsOfDynamicFields(TypeId);

  try
    if L.Count = 0 then
    begin
      EmitPCodeOperator;

      SubId := Id_RecordAssign;
      EmitCallPro(SubId);

      Reg := GetRegEx;

      S := SymbolRec1.PtrSize;
      Prg.AsmPush_Imm(S);

      EmitLoadAddress(Reg, SymbolRec2); // source
      Prg.AsmPush_REG(Reg);

      EmitLoadAddress(Reg, SymbolRecR); // dest
      Prg.AsmPush_REG(Reg);
      FreeReg(Reg);

      EmitStdCall(SubId);
    end
    else
      EmitOP_ASSIGN_RECORD_EX;
  finally
    FreeAndNil(L);
  end;
end;

procedure TEmitter.EmitOP_ASSIGN_RECORD_EX;

  var SymbolTable: TSymbolTable;

  procedure AssignArr(Reg1, Reg2, TypeId: Integer); forward;

  procedure AssignRec(Reg1, Reg2, TypeId: Integer);
  var
    I, FT, Reg, SubId, S, K: Integer;
    RI: TSymbolRec;
    ArrayTypeId,
    ElTypeId, ElFinalTypeId, ElSize,
    ElTypeId2, ElFinalTypeId2, ElSize2: Integer;
  begin
    K := 0;
    SubId := 0;
    for I := TypeId + 1 to SymbolTable.Card do
    begin
      RI := SymbolTable[I];
      if RI = SymbolTable.SR0 then
        break;
      if RI.Level <> TypeId then
        continue;
      if RI.Kind <> kindTYPE_FIELD then
        continue;
      FT := RI.FinalTypeId;
      S := RI.PtrSize;

      Inc(K);

      Emit_PUSH_REGS;

      if K > 1 then
      begin
        Prg.AsmAddREG_Imm(Reg1, RI.Shift);
        Prg.AsmAddREG_Imm(Reg2, RI.Shift);
      end;

      case FT of
        typeINTEGER, typeCARDINAL, typeLONGBOOL:
        begin
          Reg := GetReg;
          Prg.AsmMovREG32_REGPtr(Reg, Reg2);
          Prg.AsmMovREGPtr_REG32(Reg1, Reg);
          FreeReg(Reg);
        end;
        typePOINTER, typePROC,
        typeCLASS, typeCLASSREF:
        begin
          Reg := GetReg;
          Prg.AsmMovREG_REGPtr(Reg, Reg2);
          Prg.AsmMovREGPtr_REG(Reg1, Reg);
          FreeReg(Reg);
        end;
        typeWORD, typeSMALLINT, typeWORDBOOL, typeWIDECHAR:
        begin
          Reg := GetReg;
          Prg.AsmXorREG_REG(Reg, Reg);
          Prg.AsmMovREG16_REGPtr(Reg, Reg2);
          Prg.AsmMovREGPtr_REG16(Reg1, Reg);
          FreeReg(Reg);
        end;
        typeBYTE,
        typeBOOLEAN,
        typeSHORTINT,
{$IFNDEF PAXARM}
        typeANSICHAR,
{$ENDIF}
        typeBYTEBOOL:
        begin
          Reg := GetReg;
          Prg.AsmXorREG_REG(Reg, Reg);
          Prg.AsmMovREG8_REGPtr(Reg, Reg2);
          Prg.AsmMovREGPtr_REG8(Reg1, Reg);
          FreeReg(Reg);
        end;
{$IFNDEF PAXARM}
        typeANSISTRING,
        typeWIDESTRING,
        typeSHORTSTRING,
{$ENDIF}
        typeUNICSTRING,
        typeVARIANT,
        typeOLEVARIANT,
        typeINTERFACE:
        begin
          case FT of
{$IFNDEF PAXARM}
            typeANSISTRING: SubId := Id_AnsiStringAssign;
            typeWIDESTRING: SubId := Id_WideStringAssign;
            typeSHORTSTRING: SubId := Id_ShortStringAssign;
{$ENDIF}
            typeUNICSTRING: SubId := Id_UnicStringAssign;
            typeVARIANT, typeOLEVARIANT: SubId := Id_VariantAssign;
            typeINTERFACE: SubId := Id_InterfaceAssign;
          end;

          EmitCallPro(SubId);

          Reg := GetReg;
          Prg.AsmPush_REG(Reg1);
          Prg.AsmPush_REG(Reg2);
          FreeReg(Reg);

          EmitStdCall(SubId);
        end;
        typeDYNARRAY:
        begin
          SubId := Id_DynarrayAssign;
          EmitCallPro(SubId);

          Reg := GetReg;
          ArrayTypeId := RI.TerminalTypeId;
          ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
          ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
          ElSize := SymbolTable[ElTypeId].Size;

          ElTypeId2 := 0;
          ElFinalTypeId2 := 0;
          ElSize2 := 0;
          if ElFinalTypeId = typeDYNARRAY then
          begin
            ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
            ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
            ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
            ElSize2 := SymbolTable[ElTypeId2].Size;
          end;

          Prg.AsmPush_Imm(ElSize2);
          Prg.AsmPush_Imm(ElTypeId2);
          Prg.AsmPush_Imm(ElFinalTypeId2);

          Prg.AsmPush_Imm(ElSize);
          Prg.AsmPush_Imm(ElTypeId);
          Prg.AsmPush_Imm(ElFinalTypeId);

          Prg.AsmPush_REG(Reg1); // dest
          Prg.AsmPush_REG(Reg2); // source
          FreeReg(Reg);

          EmitStdCall(SubId);
        end;
        typeRECORD:
          AssignRec(Reg1, Reg2, RI.TerminalTypeId);
        typeARRAY:
          AssignArr(Reg1, Reg2, RI.TerminalTypeId);
        else
        begin
          SubId := Id_RecordAssign;

          EmitCallPro(SubId);

          Prg.AsmPush_Imm(S);
          Prg.AsmPush_REG(Reg2);
          Prg.AsmPush_REG(Reg1);

          EmitStdCall(SubId);
        end;
      end; // case
      Emit_POP_REGS;
    end;
  end;

  procedure AssignArr(Reg1, Reg2, TypeId: Integer);
  var
    RangeTypeId, ElemTypeId, H1, H2, FT, I, ElemSize, S, SubId, Reg: Integer;
  begin
    SymbolTable.GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
    H1 := SymbolTable.GetLowBoundRec(RangeTypeId).Value;
    H2 := SymbolTable.GetHighBoundRec(RangeTypeId).Value;

    ElemSize := SymbolTable[ElemTypeId].Size;

    FT := SymbolTable[ElemTypeId].FinalTypeId;

    SubId := 0;

    case FT of
{$IFNDEF PAXARM}
      typeANSISTRING,
      typeWIDESTRING,
{$ENDIF}
      typeUNICSTRING,
      typeDYNARRAY,
      typeVARIANT,
      typeOLEVARIANT,
      typeINTERFACE:
      begin
        case FT of
{$IFNDEF PAXARM}
          typeANSISTRING: SubId := Id_AnsiStringAssign;
          typeWIDESTRING: SubId := Id_WideStringAssign;
          typeSHORTSTRING: SubId := Id_ShortStringAssign;
{$ENDIF}
          typeUNICSTRING: SubId := Id_UnicStringAssign;
          typeVARIANT, typeOLEVARIANT: SubId := Id_VariantAssign;
          typeINTERFACE: SubId := Id_InterfaceAssign;
        end;
        for I:=0 to H2 - H1 do
        begin
          Emit_PUSH_REGS;

          EmitCallPro(SubId);

          Prg.AsmAddREG_Imm(Reg1, I * ElemSize);
          Prg.AsmAddREG_Imm(Reg2, I * ElemSize);
          Reg := GetReg;
          Prg.AsmPush_REG(Reg1);
          Prg.AsmPush_REG(Reg2);

          FreeReg(Reg);

          EmitStdCall(SubId);

          Emit_POP_REGS;
        end;
      end;
      typeRECORD:
      begin
        TypeID := SymbolTable.TerminalTypeOf(ElemTypeId);
        for I:=0 to H2 - H1 do
        begin
          Emit_PUSH_REGS;
          Prg.AsmAddREG_Imm(Reg1, I * ElemSize);
          Prg.AsmAddREG_Imm(Reg2, I * ElemSize);
          AssignRec(Reg1, Reg2, TypeId);
          Emit_POP_REGS;
        end;
      end;
      typeARRAY:
      begin
        TypeID := SymbolTable.TerminalTypeOf(ElemTypeId);
        for I:=0 to H2 - H1 do
        begin
          Emit_PUSH_REGS;
          Prg.AsmAddREG_Imm(Reg1, I * ElemSize);
          Prg.AsmAddREG_Imm(Reg2, I * ElemSize);
          AssignArr(Reg1, Reg2, TypeId);
          Emit_POP_REGS;
        end;
      end;
      else
      begin
        SubId := Id_RecordAssign;

        EmitCallPro(SubId);

        S := SymbolTable[TypeId].PtrSize;
        Prg.AsmPush_Imm(S);
        Prg.AsmPush_REG(Reg2);
        Prg.AsmPush_REG(Reg1);

        EmitStdCall(SubId);
      end;
    end; // case
  end;

var
  Reg1, Reg2, S, FT: Integer;
begin
  EmitPCodeOperator;
  SymbolTable := TKernel(kernel).SymbolTable;
  Reg1 := GetReg;
  Reg2 := GetReg;
  EmitLoadAddress(Reg1, SymbolRec1); // source
  EmitLoadAddress(Reg2, SymbolRec2); // dest
  FT := SymbolRec1.FinalTypeId;
  case FT of
    typeRECORD:
    begin
      AssignRec(Reg1, Reg2, SymbolRec1.TerminalTypeId);
    end;
    typeARRAY:
    begin
      AssignArr(Reg1, Reg2, SymbolRec1.TerminalTypeId);
    end;
    else
    begin
      EmitCallPro(Id_RecordAssign);

      S := SymbolRec1.PtrSize;
      Prg.AsmPush_Imm(S);
      Prg.AsmPush_REG(Reg2);
      Prg.AsmPush_REG(Reg1);

      EmitStdCall(Id_RecordAssign);
    end;
  end;
  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_ASSIGN_DOUBLE_64;
begin
  EmitOP_ASSIGN_DOUBLE;
end;
procedure TEmitter.EmitOP_ASSIGN_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFstp(SymbolRec1);
end;

procedure TEmitter.EmitOP_ASSIGN_CURRENCY_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRec1);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_ASSIGN_CURRENCY;
var
  Reg, Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadAddress(Reg1, SymbolRec1);
  EmitLoadAddress(Reg2, SymbolRec2);

  Prg.AsmMovREG32_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG32(Reg1, Reg);

  Prg.AsmAddREG_Imm(Reg1, 4);
  Prg.AsmAddREG_Imm(Reg2, 4);

  Prg.AsmMovREG32_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG32(Reg1, Reg);

  FreeReg(Reg);
  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_ASSIGN_SINGLE_64;
begin
  EmitOP_ASSIGN_SINGLE;
end;
procedure TEmitter.EmitOP_ASSIGN_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFstp(SymbolRec1);
end;

procedure TEmitter.EmitOP_ASSIGN_EXTENDED_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRec1);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_ASSIGN_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFstp(SymbolRec1);
end;

procedure TEmitter.EmitOP_ASSIGN_INT64_64;
begin
  EmitOP_ASSIGN_INT64;
end;
procedure TEmitter.EmitOP_ASSIGN_INT64;
var
  Reg, Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadAddress(Reg1, SymbolRec1);
  EmitLoadAddress(Reg2, SymbolRec2);

  Prg.AsmMovREG32_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG32(Reg1, Reg);

  Prg.AsmAddREG_Imm(Reg1, 4);
  Prg.AsmAddREG_Imm(Reg2, 4);

  Prg.AsmMovREG32_REGPtr(Reg, Reg2);
  Prg.AsmMovREGPtr_REG32(Reg1, Reg);

  FreeReg(Reg);
  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitFistp(S: TSymbolRec);
var
  Reg: Integer;
begin
  Reg := GetReg;
  EmitLoadAddress(Reg, S);
  Prg.AsmFistp_REG64Ptr(Reg);
  FreeReg(Reg);
  Prg.AsmWait;
end;

procedure TEmitter.EmitOP_CURRENCY_FROM_INT_64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_FROM_INT;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_CURRENCY_FROM_INT64_64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_FROM_INT64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_CURRENCY_FROM_REAL_64;
begin
  EmitPCodeOperator;

  EmitFLD(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_FROM_REAL;
begin
  EmitPCodeOperator;

  EmitFLD(SymbolRec2);
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitCheckOperResult(Reg: Integer);
var
  SubId, T: Integer;
begin
  if OverflowCheck = false then
    Exit;
  if TargetPlatform = tpWIN64 then
    Exit;
  if TargetPlatform in [tpOSX32, tpIOSSim] then
    Exit;

  T := SymbolRecR.FinalTypeId;

  case T of
    typeINTEGER:
    begin
      SubId := Id_IntOver;

      GetReg(_EBX);
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmJNO_Imm(9);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 3 bytes
      end
      else
      begin
        Prg.AsmJNO_Imm(8);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 2 bytes
      end;
      FreeReg(_EBX);
    end;
    typeCARDINAL:
    begin
      GetReg(_EBX);
      SubId := Id_IntOver;
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmJNC_Imm(9);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 3 bytes
      end
      else
      begin
        Prg.AsmJNC_Imm(8);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 2 bytes
      end;
      FreeReg(_EBX);
    end;
    typeBYTE:
    begin
      GetReg(_EBX);
      SubId := Id_BoundError;
      Prg.AsmCmpReg_Imm(Reg, $ff);
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmJBE_Imm(9);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 3 bytes
      end
      else
      begin
        Prg.AsmJBE_Imm(8);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 2 bytes
      end;
      FreeReg(_EBX);
    end;
    typeWORD:
    begin
      GetReg(_EBX);
      SubId := Id_BoundError;
      Prg.AsmCmpReg_Imm(Reg, $ffff);
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmJBE_Imm(9);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 3 bytes
      end
      else
      begin
        Prg.AsmJBE_Imm(8);
        EmitGet_REG(_EBX, SymbolTable[SubId]); // 6 bytes
        Prg.AsmCall_REG(_EBX);                 // 2 bytes
      end;
      FreeReg(_EBX);
    end;
  end;
end;

procedure TEmitter.EmitOP_ADD_INT_MI_64;
begin
  EmitOP_ADD_INT_MI;
end;
procedure TEmitter.EmitOP_ADD_INT_MI;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  if (SymbolRec1 = SymbolRecR) and (SymbolRecR.Size = 4) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmAddREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    EmitCheckOperResult(Reg);
    FreeReg(Reg);
  end
  else
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmAddREG_Imm(Reg, ImmValue2);
    EmitCheckOperResult(Reg);
    EmitSaveIntVal(Reg, SymbolRecR);
    FreeReg(Reg);
  end;
end;

procedure TEmitter.EmitOP_ADD_INT_MM_64;
begin
  EmitOP_ADD_INT_MM;
end;
procedure TEmitter.EmitOP_ADD_INT_MM;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);

  Prg.AsmAddREG_REG(Reg1, Reg2);
  EmitCheckOperResult(Reg1);

  FreeReg(Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_NEG_INT_64;
begin
  EmitOP_NEG_INT;
end;
procedure TEmitter.EmitOP_NEG_INT;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  if (SymbolRec1 = SymbolRecR) and (SymbolRec1.PtrSize = SizeOfPointer) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmNEG_REGPtr(Reg, SymbolRec1);
    EmitCheckOperResult(Reg);
    FreeReg(Reg);
  end
  else
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmNEGREG(Reg);
    EmitCheckOperResult(Reg);
    EmitSaveIntVal(Reg, SymbolRecR);
    FreeReg(Reg);
  end;
end;

procedure TEmitter.EmitOP_NOT_64;
begin
  EmitOP_NOT;
end;
procedure TEmitter.EmitOP_NOT;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  EmitLoadIntVal(Reg1, SymbolRec1);

  if SymbolRecR.FinalTypeId in BooleanTypes then
  begin
    Reg2 := GetReg;
    Prg.AsmMovREG_Imm(Reg2, 1);
    Prg.AsmXorREG_REG(Reg1, Reg2);
    FreeReg(Reg2);
  end
  else
  begin
    Prg.AsmNotREG(Reg1);
  end;

  EmitSaveIntVal(Reg1, SymbolRecR);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_NOT_BOOL;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  EmitLoadIntVal(Reg1, SymbolRec1);

  Reg2 := GetReg;
  Prg.AsmMovREG_Imm(Reg2, 1);
  Prg.AsmXorREG_REG(Reg1, Reg2);
  FreeReg(Reg2);

  EmitSaveIntVal(Reg1, SymbolRecR);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_NOT_BOOL64;
begin
  EmitOP_NOT_BOOL;
end;

procedure TEmitter.EmitOP_NOT_BYTEBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(7);
  Prg.AsmXorREG_REG(Reg, Reg); //2
  Prg.AsmJMP_Imm(5);           //5
  Prg.AsmMovREG_Imm(Reg, $ff); //5
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_NOT_BYTEBOOL64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(8);
  Prg.AsmXorREG_REG(Reg, Reg); //3
  Prg.AsmJMP_Imm(10);           //5
  Prg.AsmMovREG_Imm(Reg, $ff); //10
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_NOT_WORDBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(7);
  Prg.AsmXorREG_REG(Reg, Reg); //2
  Prg.AsmJMP_Imm(5);           //5
  Prg.AsmMovREG_Imm(Reg, $ffff); //5
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_NOT_WORDBOOL64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(8);
  Prg.AsmXorREG_REG(Reg, Reg); //3
  Prg.AsmJMP_Imm(10);           //5
  Prg.AsmMovREG_Imm(Reg, $ffff); //10
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_NOT_LONGBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(7);
  Prg.AsmXorREG_REG(Reg, Reg); //2
  Prg.AsmJMP_Imm(5);           //5
  Prg.AsmMovREG_Imm(Reg, $ffffffff); //5
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_NOT_LONGBOOL64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmCmpReg_Imm(Reg, 0);
  Prg.AsmJZ_Imm(8);
  Prg.AsmXorREG_REG(Reg, Reg); //3
  Prg.AsmJMP_Imm(10);           //5
  Prg.AsmMovREG_Imm(Reg, $ffffffff); //10
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_SUB_INT_MI_64;
begin
  EmitOP_SUB_INT_MI;
end;
procedure TEmitter.EmitOP_SUB_INT_MI;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmSubREG_Imm(Reg, ImmValue2);
  EmitCheckOperResult(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_SUB_INT_MM_64;
begin
  EmitOP_SUB_INT_MM;
end;
procedure TEmitter.EmitOP_SUB_INT_MM;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);

  Prg.AsmSubREG_REG(Reg1, Reg2);
  EmitCheckOperResult(Reg1);

  FreeReg(Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_IMUL_INT_MI_64;
begin
  EmitOP_IMUL_INT_MI;
end;
procedure TEmitter.EmitOP_IMUL_INT_MI;
var
  Reg2: Integer;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  Reg2 := GetReg;

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmMovREG_Imm(Reg2, SymbolRec2.Value);

  Prg.AsmIMulREG(Reg2);
  EmitCheckOperResult(_EAX);

  FreeReg(Reg2);
  EmitSaveIntVal(_EAX, SymbolRecR);

  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_IMUL_INT_MM_64;
begin
  EmitOP_IMUL_INT_MM;
end;
procedure TEmitter.EmitOP_IMUL_INT_MM;
var
  Reg2: Integer;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  Reg2 := GetReg;

  EmitLoadIntVal(_EAX, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);

  Prg.AsmIMulREG(Reg2);
  EmitCheckOperResult(_EAX);

  FreeReg(Reg2);
  EmitSaveIntVal(_EAX, SymbolRecR);

  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_IDIV_INT_MI_64;
begin
  EmitOP_IDIV_INT_MI;
end;
procedure TEmitter.EmitOP_IDIV_INT_MI;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_ECX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmMovREG_Imm(_ECX, SymbolRec2.Value);

  if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  EmitSaveIntVal(_EAX, SymbolRecR);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_IDIV_INT_MM_64;
begin
  EmitOP_IDIV_INT_MM;
end;
procedure TEmitter.EmitOP_IDIV_INT_MM;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_ECX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  EmitLoadIntVal(_ECX, SymbolRec2);

  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  EmitSaveIntVal(_EAX, SymbolRecR);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_IDIV_INT_IM_64;
begin
  EmitOP_IDIV_INT_IM;
end;
procedure TEmitter.EmitOP_IDIV_INT_IM;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_ECX);

  Prg.AsmMovREG_Imm(_EAX, SymbolRec1.Value);
  EmitLoadIntVal(_ECX, SymbolRec2);

  if SymbolRec2.FinalTypeId in UnsignedIntegerTypes then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  EmitSaveIntVal(_EAX, SymbolRecR);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_MOD_INT_MI_64;
begin
  EmitOP_MOD_INT_MI;
end;
procedure TEmitter.EmitOP_MOD_INT_MI;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmMovREG_Imm(_ECX, SymbolRec2.Value);

  if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  FreeReg(_EAX);
  EmitSaveIntVal(_EDX, SymbolRecR);
  FreeReg(_EDX);
end;

procedure TEmitter.EmitOP_MOD_INT_MM_64;
begin
  EmitOP_MOD_INT_MM;
end;
procedure TEmitter.EmitOP_MOD_INT_MM;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  EmitLoadIntVal(_ECX, SymbolRec2);

  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  EmitSaveIntVal(_EDX, SymbolRecR);
  FreeReg(_EDX);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_MOD_INT_IM_64;
begin
  EmitOP_MOD_INT_IM;
end;
procedure TEmitter.EmitOP_MOD_INT_IM;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  Prg.AsmMovREG_Imm(_EAX, SymbolRec1.Value);
  EmitLoadIntVal(_ECX, SymbolRec2);

  if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
  begin
    Prg.AsmXORReg_Reg(_EDX, _EDX);
    Prg.AsmDivREG(_ECX);
  end
  else
  begin
    Prg.AsmCDQ;
    Prg.AsmIDivREG(_ECX);
  end;

  FreeReg(_ECX);
  EmitSaveIntVal(_EDX, SymbolRecR);

  FreeReg(_EDX);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_SHL_INT_MI_64;
begin
  EmitOP_SHL_INT_MI;
end;
procedure TEmitter.EmitOP_SHL_INT_MI;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmXorREG_REG(Reg, Reg);
  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmMovREG_Imm(_ECX, SymbolRec2.Value);
  Prg.AsmShlREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHL_INT_MM_64;
begin
  EmitOP_SHL_INT_MM;
end;
procedure TEmitter.EmitOP_SHL_INT_MM;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmXorREG_REG(Reg, Reg);
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitLoadIntVal(_ECX, SymbolRec2);
  Prg.AsmShlREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHL_INT_IM_64;
begin
  EmitOP_SHL_INT_IM;
end;
procedure TEmitter.EmitOP_SHL_INT_IM;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmMovREG_Imm(Reg, SymbolRec1.Value);
  EmitLoadIntVal(_ECX, SymbolRec2);
  Prg.AsmShlREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHR_INT_MI_64;
begin
  EmitOP_SHR_INT_MI;
end;
procedure TEmitter.EmitOP_SHR_INT_MI;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmXorREG_REG(Reg, Reg);
  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmMovREG_Imm(_ECX, SymbolRec2.Value);
  Prg.AsmShrREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHR_INT_MM_64;
begin
  EmitOP_SHR_INT_MM;
end;
procedure TEmitter.EmitOP_SHR_INT_MM;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmXorREG_REG(Reg, Reg);
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitLoadIntVal(_ECX, SymbolRec2);
  Prg.AsmShrREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHR_INT_IM_64;
begin
  EmitOP_SHR_INT_IM;
end;
procedure TEmitter.EmitOP_SHR_INT_IM;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg := GetReg;

  Prg.AsmMovREG_Imm(Reg, SymbolRec1.Value);
  EmitLoadIntVal(_ECX, SymbolRec2);
  Prg.AsmShrREG(Reg);
  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_AND_INT_MI_64;
begin
  EmitOP_AND_INT_MI;
end;
procedure TEmitter.EmitOP_AND_INT_MI;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  Prg.AsmMovREG_Imm(Reg2, SymbolRec2.Value);
  Prg.AsmAndREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_AND_INT_MM_64;
begin
  EmitOP_AND_INT_MM;
end;
procedure TEmitter.EmitOP_AND_INT_MM;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmAndREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_OR_INT_MI_64;
begin
  EmitOP_OR_INT_MI;
end;
procedure TEmitter.EmitOP_OR_INT_MI;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  Prg.AsmMovREG_Imm(Reg2, SymbolRec2.Value);
  Prg.AsmOrREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_OR_INT_MM_64;
begin
  EmitOP_OR_INT_MM;
end;
procedure TEmitter.EmitOP_OR_INT_MM;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmOrREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_XOR_INT_MI_64;
begin
  EmitOP_XOR_INT_MI;
end;
procedure TEmitter.EmitOP_XOR_INT_MI;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  Prg.AsmMovREG_Imm(Reg2, SymbolRec2.Value);
  Prg.AsmXorREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_XOR_INT_MM_64;
begin
  EmitOP_XOR_INT_MM;
end;
procedure TEmitter.EmitOP_XOR_INT_MM;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmXorREG_REG(Reg1, Reg2);
  EmitSaveIntVal(Reg1, SymbolRecR);

  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_ADD_INT64_64;
begin
  EmitOP_ADD_INT64;
end;
procedure TEmitter.EmitOP_ADD_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

  Prg.AsmClc;

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// ADD EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAddREG_REG(_EAX, _ECX);

  Prg.AsmPushfd;

// ADC EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmPopfd;
  Prg.AsmAdcREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_ADD_UINT64_64;
begin
  EmitOP_ADD_UINT64;
end;
procedure TEmitter.EmitOP_ADD_UINT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

  Prg.AsmClc;

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// ADD EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAddREG_REG(_EAX, _ECX);

  Prg.AsmPushfd;

// ADC EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmPopfd;
  Prg.AsmAdcREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_NEG_INT64_64;
begin
  EmitOP_NEG_INT64;
end;
procedure TEmitter.EmitOP_NEG_INT64;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  Prg.AsmClc;

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

  Prg.AsmXorREG_REG(_ECX, _ECX);
  Prg.AsmNegREG(_EAX);
  Prg.AsmXCHG(_EDX, _ECX);
  Prg.AsmSbbREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SUB_INT64_64;
begin
  EmitOP_SUB_INT64;
end;
procedure TEmitter.EmitOP_SUB_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// SUB EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmSubREG_REG(_EAX, _ECX);

  Prg.AsmPushfd;

// SBB EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmPopfd;
  Prg.AsmSbbREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SUB_UINT64_64;
begin
  EmitOP_SUB_INT64;
end;
procedure TEmitter.EmitOP_SUB_UINT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// SUB EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmSubREG_REG(_EAX, _ECX);

  Prg.AsmPushfd;

// SBB EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmPopfd;
  Prg.AsmSbbREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_AND_INT64_64;
begin
  EmitOP_AND_INT64;
end;
procedure TEmitter.EmitOP_AND_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// AND EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAndREG_REG(_EAX, _ECX);

// AND EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAndREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_AND_UINT64_64;
begin
  EmitOP_AND_INT64;
end;
procedure TEmitter.EmitOP_AND_UINT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// AND EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAndREG_REG(_EAX, _ECX);

// AND EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmAndREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_OR_INT64_64;
begin
  EmitOP_OR_INT64;
end;
procedure TEmitter.EmitOP_OR_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// OR EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmOrREG_REG(_EAX, _ECX);

// OR EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmOrREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_OR_UINT64_64;
begin
  EmitOP_OR_UINT64;
end;
procedure TEmitter.EmitOP_OR_UINT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// OR EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmOrREG_REG(_EAX, _ECX);

// OR EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmOrREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_XOR_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// XOR EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmXorREG_REG(_EAX, _ECX);

// XOR EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmXorREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_XOR_UINT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  GetReg(_EAX);
  GetReg(_EDX);

//  EAX:EDX <-- [N1]

  EmitLoadAddress(_ECX, SymbolRec1);
  Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

// XOR EAX, [N2]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmXorREG_REG(_EAX, _ECX);

// XOR EDX, [N2 + 4]

  EmitLoadAddress(_ECX, SymbolRec2);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREG32_REGPtr(_ECX, _ECX);
  Prg.AsmXorREG_REG(_EDX, _ECX);

// EAX:EDX --> [N2]

  EmitLoadAddress(_ECX, SymbolRecR);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitStdCall_Adr1(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec1); // string source
    Prg.AsmMovREG_REG(_ECX, Reg);
  end
  else
  begin
    Reg := GetReg;

    EmitLoadAddress(Reg, SymbolRec1); // string source
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitStdCall_Adr1_AdrR(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec1); // source
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRecR); // dest
    Prg.AsmMovREG_REG(_EDX, Reg);

    FreeReg(Reg);

    EmitStdCall(SubId);

  end
  else
  begin
    Reg := GetReg;

    EmitLoadAddress(Reg, SymbolRecR); // dest
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // source
    Prg.AsmPush_REG(Reg);

    FreeReg(Reg);

    EmitStdCall(SubId);
  end;
end;

procedure TEmitter.EmitStdCall_Adr1_from_Int2(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadIntVal(Reg, SymbolRec2); // source
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRec1); // dest
    Prg.AsmMovREG_REG(_EDX, Reg);
  end
  else
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec1); // dest
    Prg.AsmPush_REG(Reg);

    EmitLoadIntVal(Reg, SymbolRec2); // source
    Prg.AsmPush_REG(Reg);
  end;

  FreeReg(Reg);

  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitStdCall_Adr1_from_Adr2(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec1); // dest
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRec2); // source
    Prg.AsmMovREG_REG(_EDX, Reg);
  end
  else
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec2); // source
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // dest
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);

  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitStdCall_AdrR_from_Adr2(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRecR); // dest
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRec2); // source
    Prg.AsmMovREG_REG(_EDX, Reg);
  end
  else
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec2); // source
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRecR); // dest
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);

  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitStdCall_Adr1_Adr2_AdrR(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRec1); // 1 - par
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRec2); // 2 - par
    Prg.AsmMovREG_REG(_EDX, Reg);

    EmitLoadAddress(Reg, SymbolRecR); // result
    Prg.AsmMovREG_REG(_R8, Reg);

    FreeReg(Reg);

    EmitStdCall(SubId);
  end
  else
  begin
    Reg := GetReg;

    EmitLoadAddress(Reg, SymbolRecR); // result
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec2); // 2 - par
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // 1 - par
    Prg.AsmPush_REG(Reg);

    FreeReg(Reg);

    EmitStdCall(SubId);
  end;
end;

procedure TEmitter.EmitStdCall_Lang_Adr1_Adr2_AdrR(SubId: Integer);
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitCallPro(SubId);

  if TargetPlatform = tpWIN64 then
  begin
    Reg := GetRegEx;

    Prg.AsmMovREG_Imm(Reg, Language); // lang
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadAddress(Reg, SymbolRec1); // par 1
    Prg.AsmMovREG_REG(_EDX, Reg);

    EmitLoadAddress(Reg, SymbolRec2); // par 2
    Prg.AsmMovREG_REG(_R8, Reg);

    EmitLoadAddress(Reg, SymbolRecR); // result
    Prg.AsmMovREG_REG(_R9, Reg);

    FreeReg(Reg);

    EmitStdCall(SubId);
  end
  else
  begin
    Reg := GetReg;

    EmitLoadAddress(Reg, SymbolRecR); // result
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec2); // 2 - par
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // 1 - par
    Prg.AsmPush_REG(Reg);

    Prg.AsmPush_Imm(Language);        // lang

    FreeReg(Reg);

    EmitStdCall(SubId);
  end;
end;


procedure TEmitter.EmitOP_GT_INT64_64;
begin
  EmitOP_GT_INT64;
end;
procedure TEmitter.EmitOP_GT_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64GreaterThan);
end;

procedure TEmitter.EmitOP_GE_INT64_64;
begin
  EmitOP_GE_INT64;
end;
procedure TEmitter.EmitOP_GE_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64GreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_LT_INT64_64;
begin
  EmitOP_LT_INT64;
end;
procedure TEmitter.EmitOP_LT_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64LessThan);
end;

procedure TEmitter.EmitOP_LE_INT64_64;
begin
  EmitOP_LE_INT64;
end;
procedure TEmitter.EmitOP_LE_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64LessThanOrEqual);
end;

procedure TEmitter.EmitOP_EQ_INT64_64;
begin
  EmitOP_EQ_INT64;
end;
procedure TEmitter.EmitOP_EQ_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Equality);
end;

procedure TEmitter.EmitOP_NE_INT64_64;
begin
  EmitOP_NE_INT64;
end;
procedure TEmitter.EmitOP_NE_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64NotEquality);
end;

procedure TEmitter.EmitOP_GT_UINT64_64;
begin
  EmitOP_GT_UINT64;
end;
procedure TEmitter.EmitOP_GT_UINT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UInt64GreaterThan);
end;

procedure TEmitter.EmitOP_GE_UINT64_64;
begin
  EmitOP_GE_UINT64;
end;
procedure TEmitter.EmitOP_GE_UINT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UInt64GreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_LT_UINT64_64;
begin
  EmitOP_LT_UINT64;
end;
procedure TEmitter.EmitOP_LT_UINT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UInt64LessThan);
end;

procedure TEmitter.EmitOP_LE_UINT64_64;
begin
  EmitOP_LE_UINT64;
end;
procedure TEmitter.EmitOP_LE_UINT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UInt64LessThanOrEqual);
end;

procedure TEmitter.EmitOP_ADD_CURRENCY_64;
begin
  EmitOP_ADD_CURRENCY;
end;
procedure TEmitter.EmitOP_ADD_CURRENCY;
begin
  EmitPCodeOperator;

  if R.Arg1 <> 0 then
  begin
    if SymbolRec1.FinalTypeId <> typeCURRENCY then
    begin
      if SymbolRec1.FinalTypeId in IntegerTypes then
        EmitFild(SymbolRec1)
      else if SymbolRec1.FinalTypeId in RealTypes then
        EmitFld(SymbolRec1)
      else
        RaiseError(errInternalError, []);
      EmitFMul_10000;
    end
    else
      EmitFild(SymbolRec1);
  end;

  EmitFild(SymbolRec2);

  Prg.AsmFAdd;

  if R.Res <> 0 then
    EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_SUB_CURRENCY_64;
begin
  EmitOP_SUB_CURRENCY;
end;
procedure TEmitter.EmitOP_SUB_CURRENCY;
begin
  EmitPCodeOperator;

  if R.Arg1 <> 0 then
  begin
    if SymbolRec1.FinalTypeId <> typeCURRENCY then
    begin
      if SymbolRec1.FinalTypeId in IntegerTypes then
        EmitFild(SymbolRec1)
      else if SymbolRec1.FinalTypeId in RealTypes then
        EmitFld(SymbolRec1)
      else
        RaiseError(errInternalError, []);

      EmitFMul_10000;

      if TKernel(kernel).Code[TKernel(kernel).Code.N].SwappedArgs then
      begin
        Prg.AsmFChs;
        EmitFild(SymbolRec2);
        Prg.AsmFAdd;
        EmitFistp(SymbolRecR);
        Exit;
      end;
    end
    else
      EmitFild(SymbolRec1);
  end;

  EmitFild(SymbolRec2);

  Prg.AsmFSub;

  if R.Res <> 0 then
    EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_MUL_CURRENCY_64;
begin
  EmitOP_MUL_CURRENCY;
end;
procedure TEmitter.EmitOP_MUL_CURRENCY;
begin
  EmitPCodeOperator;

  if R.Arg1 <> 0 then
  begin
    if SymbolRec1.FinalTypeId <> typeCURRENCY then
    begin
      if SymbolRec1.FinalTypeId in IntegerTypes then
        EmitFild(SymbolRec1)
      else if SymbolRec1.FinalTypeId in RealTypes then
        EmitFld(SymbolRec1)
      else
        RaiseError(errInternalError, []);
      EmitFild(SymbolRec2);
      Prg.AsmFMul;
      EmitFistp(SymbolRecR);
      Exit;
    end
    else
      EmitFild(SymbolRec1);
  end;

  EmitFild(SymbolRec2);
  Prg.AsmFMul;
  EmitFDiv_10000;

  if R.Res <> 0 then
    EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_DIV_CURRENCY_64;
begin
  EmitOP_DIV_CURRENCY;
end;
procedure TEmitter.EmitOP_DIV_CURRENCY;
begin
  EmitPCodeOperator;

  if R.Arg1 <> 0 then
  begin
    if SymbolRec1.FinalTypeId <> typeCURRENCY then
    begin
      if SymbolRec1.FinalTypeId in IntegerTypes then
        EmitFild(SymbolRec1)
      else if SymbolRec1.FinalTypeId in RealTypes then
        EmitFld(SymbolRec1)
      else
        RaiseError(errInternalError, []);

      EmitFild(SymbolRec2);
      Prg.AsmFDiv;

      EmitFMul_10000;
      EmitFMul_10000;

      EmitFistp(SymbolRecR);

      Exit;
    end
    else
    begin
      EmitFild(SymbolRec1);

      if SymbolRec2.FinalTypeId <> typeCURRENCY then
      begin
        if SymbolRec2.FinalTypeId in IntegerTypes then
          EmitFild(SymbolRec2)
        else if SymbolRec2.FinalTypeId in RealTypes then
          EmitFld(SymbolRec2)
        else
          RaiseError(errInternalError, []);

        Prg.AsmFDiv;
        EmitFistp(SymbolRecR);

        Exit;
      end;
    end;
  end;

  // both operands are currency

  EmitFild(SymbolRec2);

  Prg.AsmFDiv;

  EmitFMul_10000;

  if R.Res <> 0 then
    EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ADD_DOUBLE_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_DoubleAddition);
end;
procedure TEmitter.EmitOP_ADD_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFAdd;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ADD_SINGLE_64;
begin
  EmitOP_ADD_SINGLE;
end;
procedure TEmitter.EmitOP_ADD_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFAdd;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ADD_EXTENDED_64;
begin
  EmitOP_ADD_DOUBLE_64;
end;
procedure TEmitter.EmitOP_ADD_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFAdd;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ABS_INT_64;
begin
  EmitOP_ABS_INT;
end;
procedure TEmitter.EmitOP_ABS_INT;
begin
  EmitPCodeOperator;

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmCDQ;
  Prg.AsmXorREG_REG(_EAX, _EDX);
  Prg.AsmSubREG_REG(_EAX, _EDX);
  EmitSaveIntVal(_EAX, SymbolRecR);

  FreeReg(_EAX);
  FreeReg(_EDX);
end;

procedure TEmitter.EmitOP_ABS_DOUBLE_64;
begin
  EmitOP_ABS_DOUBLE;
end;
procedure TEmitter.EmitOP_ABS_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFAbs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ABS_SINGLE_64;
begin
  EmitOP_ABS_SINGLE;
end;
procedure TEmitter.EmitOP_ABS_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFAbs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ABS_EXTENDED_64;
begin
  EmitOP_ABS_EXTENDED;
end;
procedure TEmitter.EmitOP_ABS_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFAbs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_ABS_CURRENCY_64;
begin
  EmitOP_ABS_CURRENCY;
end;
procedure TEmitter.EmitOP_ABS_CURRENCY;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  Prg.AsmFAbs;
  EmitFMul_10000;
  EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_NEG_DOUBLE_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_DoubleNegation;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_NEG_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFChs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_NEG_CURRENCY_64;
begin
  EmitOP_NEG_CURRENCY;
end;
procedure TEmitter.EmitOP_NEG_CURRENCY;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);
  Prg.AsmFChs;

  FreeReg(Reg);

  EmitFistp(SymbolRecR);
end;

procedure TEmitter.EmitOP_NEG_SINGLE_64;
begin
  EmitOP_NEG_SINGLE;
end;
procedure TEmitter.EmitOP_NEG_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFChs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_NEG_EXTENDED_64;
begin
  EmitOP_NEG_DOUBLE_64;
end;
procedure TEmitter.EmitOP_NEG_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  Prg.AsmFChs;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_SUB_DOUBLE_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_DoubleSubtraction);
end;
procedure TEmitter.EmitOP_SUB_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFSub;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_SUB_SINGLE_64;
begin
  EmitOP_SUB_SINGLE;
end;
procedure TEmitter.EmitOP_SUB_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFSub;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_SUB_EXTENDED_64;
begin
  EmitOP_SUB_DOUBLE_64;
end;
procedure TEmitter.EmitOP_SUB_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFSub;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_MUL_DOUBLE_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_DoubleMultiplication);
end;
procedure TEmitter.EmitOP_MUL_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFMul;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_MUL_SINGLE_64;
begin
  EmitOP_MUL_SINGLE;
end;
procedure TEmitter.EmitOP_MUL_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFMul;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_MUL_EXTENDED_64;
begin
  EmitOP_MUL_DOUBLE_64;
end;
procedure TEmitter.EmitOP_MUL_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFMul;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_DIV_DOUBLE_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_DoubleMultiplication);
end;
procedure TEmitter.EmitOP_DIV_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFDiv;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_DIV_SINGLE_64;
begin
  EmitOP_DIV_SINGLE;
end;
procedure TEmitter.EmitOP_DIV_SINGLE;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFDiv;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_DIV_EXTENDED_64;
begin
  EmitOP_DIV_DOUBLE_64;
end;
procedure TEmitter.EmitOP_DIV_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec1);
  EmitFld(SymbolRec2);
  Prg.AsmFDiv;
  EmitFStp(SymbolRecR);
end;

procedure TEmitter.EmitOP_LT_INT_MI_64;
begin
  EmitOP_LT_INT_MI;
end;
procedure TEmitter.EmitOP_LT_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSet_REGPtr(ASM_SETB, Reg, SymbolRecR)
    else
      Prg.AsmSet_REGPtr(ASM_SETL, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSetB_REGPtr(RegR)
    else
      Prg.AsmSetL_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_LT_INT_MM_64;
begin
  EmitOP_LT_INT_MM;
end;
procedure TEmitter.EmitOP_LT_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;
  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
    Prg.AsmSetB_REGPtr(RegR)
  else
    Prg.AsmSetL_REGPtr(RegR);
  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_LE_INT_MI_64;
begin
  EmitOP_LE_INT_MI;
end;
procedure TEmitter.EmitOP_LE_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSet_REGPtr(ASM_SETBE, Reg, SymbolRecR)
    else
      Prg.AsmSet_REGPtr(ASM_SETLE, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSetBE_REGPtr(RegR)
    else
      Prg.AsmSetLE_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_LE_INT_MM_64;
begin
  EmitOP_LE_INT_MM;
end;
procedure TEmitter.EmitOP_LE_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;
  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
    Prg.AsmSetBE_REGPtr(RegR)
  else
    Prg.AsmSetLE_REGPtr(RegR);
  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_GT_INT_MI_64;
begin
  EmitOP_GT_INT_MI;
end;
procedure TEmitter.EmitOP_GT_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSet_REGPtr(ASM_SETNBE, Reg, SymbolRecR)
    else
      Prg.AsmSet_REGPtr(ASM_SETNLE, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSetNBE_REGPtr(RegR)
    else
      Prg.AsmSetNLE_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_GT_INT_MM_64;
begin
  EmitOP_GT_INT_MM;
end;
procedure TEmitter.EmitOP_GT_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;
  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
    Prg.AsmSetNBE_REGPtr(RegR)
  else
    Prg.AsmSetNLE_REGPtr(RegR);
  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_GE_INT_MI_64;
begin
  EmitOP_GE_INT_MI;
end;
procedure TEmitter.EmitOP_GE_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSet_REGPtr(ASM_SETNB, Reg, SymbolRecR)
    else
      Prg.AsmSet_REGPtr(ASM_SETNL, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    if SymbolRec1.FinalTypeId in UnsignedIntegerTypes then
      Prg.AsmSetNB_REGPtr(RegR)
    else
      Prg.AsmSetNL_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_GE_INT_MM_64;
begin
  EmitOP_GE_INT_MM;
end;
procedure TEmitter.EmitOP_GE_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;

  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  if (SymbolRec1.FinalTypeId in UnsignedIntegerTypes) and
     (SymbolRec2.FinalTypeId in UnsignedIntegerTypes) then
    Prg.AsmSetNB_REGPtr(RegR)
  else
    Prg.AsmSetNL_REGPtr(RegR);

  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_EQ_INT_MI_64;
begin
  EmitOP_EQ_INT_MI;
end;
procedure TEmitter.EmitOP_EQ_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    Prg.AsmSet_REGPtr(ASM_SETZ, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    Prg.AsmSetZ_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_EQ_INT_MM_64;
begin
  EmitOP_EQ_INT_MM;
end;
procedure TEmitter.EmitOP_EQ_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;
  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  Prg.AsmSetZ_REGPtr(RegR);
  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_NE_INT_MI_64;
begin
  EmitOP_NE_INT_MI;
end;
procedure TEmitter.EmitOP_NE_INT_MI;
var
  Reg, RegR: Integer;
begin
  EmitPCodeOperator;

  if HasTheSameAddressRegister(SymbolRec1, SymbolRecR) then
  begin
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    Prg.AsmSet_REGPtr(ASM_SETNZ, Reg, SymbolRecR);
    FreeReg(Reg);
  end
  else
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, SymbolRecR);
    Reg := EmitGetAddressRegister(SymbolRec1);
    Prg.AsmCmpREGPtr_Imm(Reg, SymbolRec1, ImmValue2);
    Prg.AsmSetNZ_REGPtr(RegR);
    FreeReg(Reg);
    FreeReg(RegR);
  end;
end;

procedure TEmitter.EmitOP_NE_INT_MM_64;
begin
  EmitOP_NE_INT_MM;
end;
procedure TEmitter.EmitOP_NE_INT_MM;
var
  Reg1, Reg2, RegR: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;
  RegR := GetReg;
  EmitLoadAddress(RegR, SymbolRecR);
  EmitLoadIntVal(Reg1, SymbolRec1);
  EmitLoadIntVal(Reg2, SymbolRec2);
  Prg.AsmCmpREG_REG(Reg1, Reg2);
  Prg.AsmSetNZ_REGPtr(RegR);
  FreeReg(RegR);
  FreeReg(Reg2);
  FreeReg(Reg1);
end;

procedure TEmitter.EmitOP_LT_CURRENCY_64;
begin
  EmitOP_LT_CURRENCY;
end;
procedure TEmitter.EmitOP_LT_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetB_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LE_CURRENCY_64;
begin
  EmitOP_LE_CURRENCY;
end;
procedure TEmitter.EmitOP_LE_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetBE_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GT_CURRENCY_64;
begin
  EmitOP_GT_CURRENCY;
end;
procedure TEmitter.EmitOP_GT_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNBE_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GE_CURRENCY_64;
begin
  EmitOP_GE_CURRENCY;
end;
procedure TEmitter.EmitOP_GE_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNB_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_EQ_CURRENCY_64;
begin
  EmitOP_EQ_CURRENCY;
end;
procedure TEmitter.EmitOP_EQ_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetZ_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_NE_CURRENCY_64;
begin
  EmitOP_NE_CURRENCY;
end;
procedure TEmitter.EmitOP_NE_CURRENCY;
var
  Reg: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  S := SymbolRec2;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  S := SymbolRec1;
  if S.FinalTypeId <> typeCURRENCY then
  begin
    if S.FinalTypeId in IntegerTypes then
      EmitFild(S)
    else if S.FinalTypeId in RealTypes then
      EmitFld(S)
    else
      RaiseError(errInternalError, []);
    EmitFMul_10000;
  end
  else
    EmitFild(S);

  GetReg(_EAX);
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNZ_REGPtr(Reg);
  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LT_DOUBLE_64;
begin
  EmitOP_LT_DOUBLE;
end;
procedure TEmitter.EmitOP_LT_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LT_SINGLE_64;
begin
  EmitOP_LT_SINGLE;
end;
procedure TEmitter.EmitOP_LT_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LT_EXTENDED_64;
begin
  EmitOP_LT_EXTENDED;
end;
procedure TEmitter.EmitOP_LT_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LE_DOUBLE_64;
begin
  EmitOP_LE_DOUBLE;
end;
procedure TEmitter.EmitOP_LE_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LE_SINGLE_64;
begin
  EmitOP_LE_SINGLE;
end;
procedure TEmitter.EmitOP_LE_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_LE_EXTENDED_64;
begin
  EmitOP_LE_EXTENDED;
end;
procedure TEmitter.EmitOP_LE_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GT_DOUBLE_64;
begin
  EmitOP_GT_DOUBLE;
end;
procedure TEmitter.EmitOP_GT_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GT_SINGLE_64;
begin
  EmitOP_GT_SINGLE;
end;
procedure TEmitter.EmitOP_GT_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GT_EXTENDED_64;
begin
  EmitOP_GT_EXTENDED;
end;
procedure TEmitter.EmitOP_GT_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNBE_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GE_DOUBLE_64;
begin
  EmitOP_GE_DOUBLE;
end;
procedure TEmitter.EmitOP_GE_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GE_SINGLE_64;
begin
  EmitOP_GE_SINGLE;
end;
procedure TEmitter.EmitOP_GE_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_GE_EXTENDED_64;
begin
  EmitOP_GE_EXTENDED;
end;
procedure TEmitter.EmitOP_GE_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNB_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_EQ_DOUBLE_64;
begin
  EmitOP_EQ_DOUBLE;
end;
procedure TEmitter.EmitOP_EQ_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_EQ_SINGLE_64;
begin
  EmitOP_EQ_SINGLE;
end;
procedure TEmitter.EmitOP_EQ_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_EQ_EXTENDED_64;
begin
  EmitOP_EQ_EXTENDED;
end;
procedure TEmitter.EmitOP_EQ_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_NE_DOUBLE_64;
begin
  EmitOP_NE_DOUBLE;
end;
procedure TEmitter.EmitOP_NE_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_NE_SINGLE_64;
begin
  EmitOP_NE_SINGLE;
end;
procedure TEmitter.EmitOP_NE_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_NE_EXTENDED_64;
begin
  EmitOP_NE_EXTENDED;
end;
procedure TEmitter.EmitOP_NE_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFld(SymbolRec2);
  EmitFld(SymbolRec1);

  GetReg(_EAX);
  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFCompP;
  Prg.AsmFstsw_AX;
  Prg.AsmSahv;
  Prg.AsmSetNZ_REGPtr(Reg);

  FreeReg(Reg);
  FreeReg(_EAX);
end;

procedure TEmitter.EmitOP_PUSH_STRUCTURE_64;
var
  Reg, RegTemp: Integer;
  SubId, ParamId, ParamNumber, SZ: Integer;
  ByRefer: Boolean;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  S := GetSymbolRec(ParamId);
  SZ := S.Size;

  if SZ > 8 then
    ByRefer := true
  else
    ByRefer := S.ByRef or (S.IsConst and (SZ > 8));

  if (not ByRefer) and (SZ > 8) then
  begin
    while SZ mod 8 <> 0 do
      Inc(SZ);

    Reg := GetReg;
    EmitLoadAddress(Reg, SymbolRec1);

    Dec(SZ, 8);
    if SZ > 0 then
      Prg.AsmAddREG_Imm(Reg, SZ);

    RegTemp := GetReg;

    repeat
      if SZ = 0 then
        Prg.AsmXorREG_REG(RegTemp, RegTemp);

      Prg.AsmMovREG_REGPtr(RegTemp, Reg);
      EmitPushParam(RegTemp);

      Dec(SZ, 8);
      if SZ < 0 then
        break;

      Prg.AsmSubREG_Imm(Reg, 8);
    until false;

    FreeReg(RegTemp);
    FreeReg(Reg);

    Exit;
  end;

  // push address or 4-byte value

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadAddress(Reg, SymbolRec1);

    if not ByRefer then
      Prg.AsmMovREG_REGPtr(Reg, Reg);

    FreeReg(Reg);
    Exit;
  end;

  Reg := GetRegEx;
  EmitLoadAddress(Reg, SymbolRec1);

  if not ByRefer then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  EmitPushParam(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_PUSH_STRUCTURE;
var
  Reg, RegTemp: Integer;
  SubId, ParamId, ParamNumber, SZ: Integer;
  ByRefer: Boolean;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  S := GetSymbolRec(ParamId);
  SZ := S.Size;

  if (GetSymbolRec(SubId).CallConv = ccREGISTER) and (SZ > 4) then
    ByRefer := true
  else
    ByRefer := S.ByRef or (S.IsConst and (SZ > 4));

  if (not ByRefer) and (SZ > 4) then
  begin
    while SZ mod 4 <> 0 do
      Inc(SZ);

    Reg := GetReg;
    EmitLoadAddress(Reg, SymbolRec1);

    Dec(SZ, 4);
    if SZ > 0 then
      Prg.AsmAddREG_Imm(Reg, SZ);

    RegTemp := GetReg;

    repeat
      if SZ = 0 then
        Prg.AsmXorREG_REG(RegTemp, RegTemp);

      Prg.AsmMovREG_REGPtr(RegTemp, Reg);
      Prg.AsmPush_REG(RegTemp);

      Dec(SZ, 4);
      if SZ < 0 then
        break;

      Prg.AsmSubREG_Imm(Reg, 4);
    until false;

    FreeReg(RegTemp);
    FreeReg(Reg);

    Exit;
  end;

  // push address or 4-byte value

  if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    Reg := GetSymbolRec(ParamId).Register;

    if Reg > 0 then
    begin
      GetReg(Reg);
      EmitLoadAddress(Reg, SymbolRec1);

      if not ByRefer then
        Prg.AsmMovREG_REGPtr(Reg, Reg);

      FreeReg(Reg);
      Exit;
    end;
  end;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);

  if not ByRefer then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  Prg.AsmPush_REG(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_EXPORTS_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_EXPORTS;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_PUSH_ADDRESS_64;
begin
  EmitOP_PUSH_ADDRESS;
end;
procedure TEmitter.EmitOP_PUSH_ADDRESS;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;

  if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL, cc64] then
  begin
    if GetSymbolRec(SubId).CallConv = cc64 then
      if SubId = JS_FunctionCallId then
      begin
        Reg := GetRegEx;
        EmitLoadAddress(Reg, SymbolRec1);
        if ParamNumber = 1 then
          Prg.AsmMovREG_REG(_R9, Reg)
        else if ParamNumber > 1 then
          Prg.AsmMovRSPPtr_REG64(Reg, $20 + (ParamNumber - 2)*8)
        else
          RaiseError(errInternalError, []);
        FreeReg(Reg);
        Exit;
      end;

    ParamId := SymbolTable.GetParamId(SubId, ParamNumber);
    Reg := GetSymbolRec(ParamId).Register;

    if Reg > 0 then
    begin
      GetReg(Reg);

      if SymbolRec1.Kind = KindSUB then
      begin
        if Host1 then
          EmitGet_REG(Reg, SymbolRec1)
        else
        begin
          Prg.AsmMovREG_REG(Reg, _EDI);
          Prg.AsmAddREG_Imm(Reg, 0);
          Prg.Top.SaveSubId := R.Arg1;
          List2.Add(Prg.Top);
        end;
      end
      else
        EmitLoadAddress(Reg, SymbolRec1);

      if SymbolRec1.FinalTypeId = typeRECORD then
        if SymbolRec1.PtrSize <= 4 then
        begin
          Prg.AsmMovREG32_REGPtr(Reg, Reg);
        end;

      FreeReg(Reg);
      Exit;
    end;
  end;

  Reg := GetReg;

  if SymbolRec1.Kind = KindSUB then
  begin
    if Host1 then
      EmitGet_REG(Reg, SymbolRec1)
    else
    begin
      Prg.AsmMovREG_REG(Reg, _EDI);
      Prg.AsmAddREG_Imm(Reg, 0);
      Prg.Top.SaveSubId := R.Arg1;
      List2.Add(Prg.Top);
    end;
  end
  else
    EmitLoadAddress(Reg, SymbolRec1);

  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_INT_IMM_64;
begin
  EmitOP_PUSH_INT_IMM;
end;
procedure TEmitter.EmitOP_PUSH_INT_IMM;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  if R.Res = 0 then
  begin
    Prg.AsmPush_IMM(ImmValue1);
    Exit;
  end;

  SubId := R.Res;
  ParamNumber := R.Arg2;

  if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

    Reg := GetSymbolRec(ParamId).Register;

    if Reg > 0 then
    begin
      GetReg(Reg);
      Prg.AsmMovREG_IMM(Reg, ImmValue1);
      FreeReg(Reg);
      Exit;
    end;
  end
  else if GetSymbolRec(SubId).CallConv = cc64 then
  begin
    if SubId = JS_FunctionCallId then
    begin
      Prg.AsmMovREG_IMM(_R8, ImmValue1);
      Exit;
    end;

    ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

    Reg := GetSymbolRec(ParamId).Register;

    if Reg > 0 then
    begin
      GetReg(Reg);
      Prg.AsmMovREG_IMM(Reg, ImmValue1);
      FreeReg(Reg);
      Exit;
    end;

    Reg := GetRegEx;
    Prg.AsmMovREG_Imm(Reg, ImmValue1);
    EmitPushParam(Reg);
    FreeReg(Reg);
    Exit;
  end;

  Prg.AsmPush_IMM(ImmValue1);
end;

procedure TEmitter.EmitOP_PUSH_DATA_64;
begin
  RaiseNotImpl;
end;
procedure TEmitter.EmitOP_PUSH_DATA;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;

  Emit_PUSH_REGS;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmAddREG_Imm(Reg, SizeOfPointer);
  Prg.AsmMovREG_REGPtr(Reg, Reg);
  Emit_POP_REGS;

  if SymbolRecR.CallConv in [ccREGISTER, ccMSFASTCALL] then
    Prg.AsmMovREG_REG(_EAX, Reg)
  else
    Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_INT_64;
begin
  EmitOP_PUSH_INT;
end;
procedure TEmitter.EmitOP_PUSH_INT;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  if R.Res = 0 then
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
    Exit;
  end;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_SET_64;
begin
  EmitOP_PUSH_SET;
end;
procedure TEmitter.EmitOP_PUSH_SET;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber, K: Integer;
begin
  EmitPCodeOperator;

  K := SymbolRec1.Kind;
  try
    SymbolRec1.Kind := KindVAR;

    if R.Res = 0 then
    begin
      Reg := GetReg;
      EmitLoadIntVal(Reg, SymbolRec1);
      Prg.AsmPush_REG(Reg);
      FreeReg(Reg);
      Exit;
    end;

    SubId := R.Res;
    ParamNumber := R.Arg2;
    ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

    Reg := GetSymbolRec(ParamId).Register;

    if Reg > 0 then
    begin
      GetReg(Reg);
      EmitLoadIntVal(Reg, SymbolRec1);
      FreeReg(Reg);
      Exit;
    end;

    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    EmitPushParam(Reg);
    FreeReg(Reg);

  finally
    SymbolRec1.Kind := K;
  end;
end;

procedure TEmitter.EmitOP_PUSH_DYNARRAY_64;
var
  Reg, RegEx: Integer;
  SubId, ParamId, ParamNumber, K, Z: Integer;
  S: String;
begin
  EmitPCodeOperator;

  if R.Res = 0 then
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
    Exit;
  end;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  S := SymbolTable[SubId].Name;

  Reg := GetSymbolRec(ParamId).Register;

  k := SymbolRec1.Count;

  Z := GetSymbolRec(ParamId).RSPOffset;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);

    if SymbolTable[ParamId].IsOpenArray then
    begin
      if Reg = _ECX then
        RegEx := _EDX
      else if Reg = _EDX then
        RegEx := _R8
      else if Reg = _R8 then
        RegEx := _R9
      else
        RegEx := _EBX;

      if (RegEx = _EBX) and (Reg <> _R9) then
      begin
        Prg.AsmMovRSPPtr_REG64(Reg, Z);
        Inc(Z, 8);
      end;

      GetReg(RegEx);

        // load high(A) into RegEx
      Prg.AsmMovREG_Imm(RegEx, k - 1);

      if RegEx = _EBX then
        Prg.AsmMovRSPPtr_REG64(RegEx, Z);

      FreeReg(RegEx);
    end;

    Exit;
  end;

  Reg := GetReg(_EBX);

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmMovRSPPtr_REG64(Reg, Z);
  Inc(Z, 8);

  if SymbolTable[ParamId].IsOpenArray then
  begin
    // load high(A) into RegEx
    Prg.AsmMovREG_Imm(Reg, k - 1);
    Prg.AsmMovRSPPtr_REG64(Reg, Z);
  end;

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_PUSH_DYNARRAY;
var
  Reg, RegEx: Integer;
  SubId, ParamId, ParamNumber: Integer;
  S: String;
begin
  EmitPCodeOperator;

  if R.Res = 0 then
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
    Exit;
  end;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  S := SymbolTable[SubId].Name;

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);

    if SymbolTable[ParamId].IsOpenArray then
    begin
      if Reg = _EAX then
        RegEx := _EDX
      else if Reg = _EDX then
        RegEx := _ECX
      else
        RegEx := _EBX;

      if (RegEx = _EBX) and (Reg <> _ECX) then
        Prg.AsmPush_REG(Reg);

      GetReg(RegEx);

      if SymbolRec1.FinalTypeId = typeDYNARRAY then
      begin
        // load high(A) into RegEx

        Prg.AsmMovREG_REG(RegEx, Reg);

        Prg.AsmCmpREG_Imm(RegEx, 0);
        Prg.AsmJZ_Imm(5 + 2);

        Prg.AsmAddREG_Imm(RegEx, -4); // 5
        Prg.AsmMovREG32_REGPtr(RegEx, RegEx); // 2
        Prg.AsmAddREG_Imm(RegEx, -1);
      end
      else
      begin
        Prg.AsmMovREG_Imm(RegEx, 0);
      end;

      if RegEx = _EBX then
        Prg.AsmPush_REG(RegEx);

      FreeReg(RegEx);
    end;

    Exit;
  end;

  if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    Reg := GetReg;

    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);

    if SymbolTable[ParamId].IsOpenArray then
    begin
      if SymbolRec1.FinalTypeId = typeDYNARRAY then
      begin
        // load high(A) into RegEx
        EmitLoadIntVal(Reg, SymbolRec1);

        Prg.AsmCmpREG_Imm(Reg, 0);
        Prg.AsmJZ_Imm(5 + 2);

        Prg.AsmAddREG_Imm(Reg, -4); //5
        Prg.AsmMovREG32_REGPtr(Reg, Reg); //2
        Prg.AsmAddREG_Imm(Reg, -1);
        Prg.AsmPush_REG(Reg);
      end
      else
        Prg.AsmPush_Imm(0);
    end;

//    EmitLoadIntVal(Reg, SymbolRec1);
//    Prg.AsmPush_REG(Reg);

    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  if SymbolTable[ParamId].IsOpenArray then
  begin
    if SymbolRec1.FinalTypeId = typeDYNARRAY then
    begin
      // load high(A) into RegEx

      EmitLoadIntVal(Reg, SymbolRec1);

      Prg.AsmCmpREG_Imm(Reg, 0);
      Prg.AsmJZ_Imm(5 + 2);

      Prg.AsmAddREG_Imm(Reg, -4); // 5
      Prg.AsmMovREG32_REGPtr(Reg, Reg); // 2
      Prg.AsmAddREG_Imm(Reg, -1);
      Prg.AsmPush_REG(Reg);
    end
    else
      Prg.AsmPush_Imm(0);
  end;

  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmPush_REG(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_OPENARRAY_64;
begin
  RaiseNotImpl;
end;
procedure TEmitter.EmitOP_PUSH_OPENARRAY;
var
  Reg, RegEx: Integer;
  SubId, ParamId, HighParamId, ParamNumber: Integer;
  ArrayTypeId, RangeTypeId, ElemTypeId, B1, B2: Integer;
begin
  EmitPCodeOperator;

  if R.Res = 0 then
  begin
    RaiseError(errInternalError, []);
  end;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    if SymbolRec1.IsOpenArray then
    begin

      GetReg(Reg);
      EmitLoadIntVal(Reg, SymbolRec1);
      FreeReg(Reg);

      if Reg = _EAX then
        RegEx := _EDX
      else if Reg = _EDX then
        RegEx := _ECX
      else
        RegEx := _EBX;

      if (RegEx = _EBX) and (Reg <> _ECX) then
        Prg.AsmPush_REG(Reg);

      GetReg(RegEx);

        // load high(A) into RegEx
      HighParamId := SymbolTable.GetOpenArrayHighId(R.Arg1);
      EmitLoadIntVal(RegEx, GetSymbolRec(HighParamId));

      if RegEx = _EBX then
        Prg.AsmPush_REG(RegEx);

      FreeReg(RegEx);
    end
    else if SymbolRec1.FinalTypeId = typeARRAY then
    begin
      GetReg(Reg);
      EmitLoadAddress(Reg, SymbolRec1);
      FreeReg(Reg);

      if Reg = _EAX then
        RegEx := _EDX
      else if Reg = _EDX then
        RegEx := _ECX
      else
        RegEx := _EBX;

      if (RegEx = _EBX) and (Reg <> _ECX) then
        Prg.AsmPush_REG(Reg);

      GetReg(RegEx);

        // load high(A) into RegEx
      ArrayTypeId := SymbolRec1.TerminalTypeId;
      TKernel(kernel).SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
      B1 := TKernel(kernel).SymbolTable.GetLowBoundRec(RangeTypeId).Value;
      B2 := TKernel(kernel).SymbolTable.GetHighBoundRec(RangeTypeId).Value;
      Prg.AsmMovREG_Imm(RegEx, B2 - B1);

      if RegEx = _EBX then
        Prg.AsmPush_REG(RegEx);

      FreeReg(RegEx);
    end
    else if SymbolRec1.IsFWArrayVar then
    begin

      GetReg(Reg);
      EmitLoadIntVal(Reg, SymbolRec1);
      Prg.AsmAddReg_Imm(Reg, FWArrayOffset);
      Prg.AsmMovREG32_REGPtr(Reg, Reg);
      FreeReg(Reg);

      if Reg = _EAX then
        RegEx := _EDX
      else if Reg = _EDX then
        RegEx := _ECX
      else
        RegEx := _EBX;

      if (RegEx = _EBX) and (Reg <> _ECX) then
        Prg.AsmPush_REG(Reg);

      Prg.AsmMovREG_REG(RegEx, Reg);

      Prg.AsmCmpREG_Imm(RegEx, 0);
      Prg.AsmJZ_Imm(5 + 2);

      Prg.AsmAddREG_Imm(RegEx, -4); // 5
      Prg.AsmMovREG32_REGPtr(RegEx, RegEx); // 2
      Prg.AsmAddREG_Imm(RegEx, -1);

      if RegEx = _EBX then
        Prg.AsmPush_REG(RegEx);
    end;


    Exit;
  end;

  if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
    if SymbolRec1.IsOpenArray then
    begin
      // load high(A) into RegEx
      HighParamId := SymbolTable.GetOpenArrayHighId(R.Arg1);
      EmitLoadIntVal(Reg, GetSymbolRec(HighParamId));
      Prg.AsmPush_REG(Reg);
    end
    else if SymbolRec1.FinalTypeId = typeARRAY then
    begin
        // load high(A) into RegEx
      ArrayTypeId := SymbolRec1.TerminalTypeId;
      TKernel(kernel).SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
      B1 := TKernel(kernel).SymbolTable.GetLowBoundRec(RangeTypeId).Value;
      B2 := TKernel(kernel).SymbolTable.GetHighBoundRec(RangeTypeId).Value;
      Prg.AsmMovREG_Imm(Reg, B2 - B1);
      Prg.AsmPush_REG(Reg);
    end;
    FreeReg(Reg);
    Exit;
  end;

  if SymbolRec1.IsFWArrayVar then
  begin
    Reg := GetReg;
      // load high(A) into RegEx

    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmAddReg_Imm(Reg, FWArrayOffset);
    Prg.AsmMovREG32_REGPtr(Reg, Reg);

    Prg.AsmCmpREG_Imm(Reg, 0);
    Prg.AsmJZ_Imm(5 + 2);

    Prg.AsmAddREG_Imm(Reg, -4); //5
    Prg.AsmMovREG32_REGPtr(Reg, Reg); //2
    Prg.AsmAddREG_Imm(Reg, -1);
    Prg.AsmPush_REG(Reg);

    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmAddReg_Imm(Reg, FWArrayOffset);
    Prg.AsmMovREG32_REGPtr(Reg, Reg);
    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  if SymbolRec1.IsOpenArray then
  begin
    // load high(A) into Reg
    HighParamId := SymbolTable.GetOpenArrayHighId(R.Arg1);
    EmitLoadIntVal(Reg, GetSymbolRec(HighParamId));
    Prg.AsmPush_REG(Reg);
  end
  else if SymbolRec1.FinalTypeId = typeARRAY then
  begin
     // load high(A) into RegEx
    ArrayTypeId := SymbolRec1.TerminalTypeId;
    TKernel(kernel).SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
    B1 := TKernel(kernel).SymbolTable.GetLowBoundRec(RangeTypeId).Value;
    B2 := TKernel(kernel).SymbolTable.GetHighBoundRec(RangeTypeId).Value;
    Prg.AsmMovREG_Imm(Reg, B2 - B1);
    Prg.AsmPush_REG(Reg);
  end;
  EmitLoadIntVal(Reg, SymbolRec1);
  Prg.AsmPush_REG(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_CLEAR_EDX_64;
begin
  EmitOP_CLEAR_EDX;
end;
procedure TEmitter.EmitOP_CLEAR_EDX;
begin
  EmitPCodeOperator;
  Prg.AsmMovREG_Imm(_EDX, 0);
end;

procedure TEmitter.EmitOP_UPDATE_INSTANCE_64;
var
  Reg: Integer;
  SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_UpdateInstance;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Emit_PUSH_REGS;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);
  FreeReg(Reg);

  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_UPDATE_INSTANCE;
var
  Reg: Integer;
  SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_UpdateInstance;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Emit_PUSH_REGS;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_PUSH_INST_64;
var
  Reg: Integer;
  SubId: Integer;
  L, K: Integer;
  SR: TSymbolRec;
begin
  EmitPCodeOperator;

  L := GetSymbolRec(R.Res).Level;
  if GetSymbolRec(L).FinalTypeId = typeHELPER then
    L := GetSymbolRec(L).PatternId;

  K := SymbolTable[R.Res].Kind;
  if (K = kindCONSTRUCTOR) and
     (SymbolTable[SymbolTable[R.Res].Level].FinalTypeId <> typeRECORD) and
      SymbolTable[R.Res].Host and
      (not SymbolTable[R.Res].IsExternal) then
  begin
    SubId := Id_ToParentClass;
    EmitCallPro(SubId);

    Reg := GetRegEx;

    Emit_PUSH_REGS;

    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
    Prg.AsmMovREG_REG(_ECX, Reg);

    EmitLoadIntVal(Reg, SymbolRec1); // instance
    Prg.AsmMovREG_REG(_EDX, Reg);
    FreeReg(Reg);

    EmitStdCall(SubId);

    Emit_POP_REGS;
  end;

  GetReg(_ECX);

  if GetSymbolRec(L).FinalTypeId in [typeCLASS, typeINTERFACE] then
    EmitLoadIntVal(_ECX, SymbolRec1)
  else
    EmitLoadAddress(_ECX, SymbolRec1);

  if SymbolRecR.IsSharedMethod then
    if not StrEql(SymbolRecR.Name, 'ClassType') then
    Prg.AsmMovREG_REGPtr(_ECX, _ECX);

  FreeReg(_ECX);

  SR := SymbolTable[R.Res];

  if SR.Kind in [kindCONSTRUCTOR, kindDESTRUCTOR] then
  begin
    Prg.AsmMovREG_Imm(_EDX, 0);
  end
  else if SR.IsFakeMethod then
     if SR.ExtraParamNeeded then
     begin
       Prg.AsmMovREG_REG(_EDX, _ECX);
     end;
end;
procedure TEmitter.EmitOP_PUSH_INST;
var
  Reg: Integer;
  SubId: Integer;
  L, K: Integer;
begin
  EmitPCodeOperator;

  L := GetSymbolRec(R.Res).Level;
  if GetSymbolRec(L).FinalTypeId = typeHELPER then
    L := GetSymbolRec(L).PatternId;

  if SymbolTable[R.Res].CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    K := SymbolTable[R.Res].Kind;
    if (K = kindCONSTRUCTOR) and
       (SymbolTable[SymbolTable[R.Res].Level].FinalTypeId <> typeRECORD) and
        SymbolTable[R.Res].Host and
        (not SymbolTable[R.Res].IsExternal) then
    begin
      SubId := Id_ToParentClass;
      EmitCallPro(SubId);

      Reg := GetRegEx;

      Emit_PUSH_REGS;

      EmitLoadIntVal(Reg, SymbolRec1); // instance
      Prg.AsmPush_REG(Reg);

      Prg.AsmMovREG_REG(Reg, _ESI);
      Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);

      Emit_POP_REGS;
    end;

    GetReg(_EAX);

    if GetSymbolRec(L).FinalTypeId in [typeCLASS, typeINTERFACE] then
      EmitLoadIntVal(_EAX, SymbolRec1)
    else
      EmitLoadAddress(_EAX, SymbolRec1);

    if SymbolRecR.IsSharedMethod then
      if not StrEql(SymbolRecR.Name, 'ClassType') then
      Prg.AsmMovREG_REGPtr(_EAX, _EAX);

    FreeReg(_EAX);

    if SymbolTable[R.Res].Kind in [kindCONSTRUCTOR, kindDESTRUCTOR] then
    begin
      Prg.AsmMovREG_Imm(_EDX, 0);
    end;
  end
  else
  begin

    if (SymbolTable[R.Res].Kind = kindCONSTRUCTOR) and
        SymbolTable[R.Res].Host and
        (not SymbolTable[R.Res].IsExternal) then
    begin
      SubId := Id_ToParentClass;
      EmitCallPro(SubId);

      Reg := GetRegEx;

      Emit_PUSH_REGS;

      EmitLoadIntVal(Reg, SymbolRec1); // instance
      Prg.AsmPush_REG(Reg);

      Prg.AsmMovREG_REG(Reg, _ESI);
      Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);

      Emit_POP_REGS;
    end;

    GetReg(_EAX);
    Reg := _EAX;

    if GetSymbolRec(L).FinalTypeId in [typeCLASS, typeINTERFACE] then
      EmitLoadIntVal(Reg, SymbolRec1)
    else
      EmitLoadAddress(Reg, SymbolRec1);

    if SymbolRecR.IsSharedMethod then
      if not StrEql(SymbolRecR.Name, 'ClassType') then
        Prg.AsmMovREG_REGPtr(_EAX, _EAX);

    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
  end;
end;

procedure TEmitter.EmitOP_PUSH_CLSREF_64;
begin
  EmitPCodeOperator;

  if SymbolRecR.Kind = kindCONSTRUCTOR then
  begin
    GetReg(_ECX);
    GetReg(_EDX);

{$IFDEF FPC}
    if SymbolRecR.Host then
    begin
      EmitLoadIntVal(_EDX, SymbolRec1);
      Prg.AsmMovREG_Imm(_ECX, 0);
    end
    else
    begin
      EmitLoadIntVal(_ECX, SymbolRec1);
      Prg.AsmMovREG_Imm(_EDX, 1);
    end;
{$ELSE}
    EmitLoadIntVal(_ECX, SymbolRec1);
    Prg.AsmMovREG_Imm(_EDX, 1);
{$ENDIF}

    FreeReg(_EDX);
    FreeReg(_ECX);

    if SymbolRecR.IsExternal then
    begin
      Emit_PUSH_REGS;

      Prg.AsmPush_REG(_EAX);

      Prg.AsmMovREG_REG(_EBX, _ESI);
      Prg.AsmAddREG_Imm(_EBX, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(_EBX, _EBX); // load TProgram.Self
      Prg.AsmPush_REG(_EBX);

      Prg.AsmGetREG_ESIPtr(_EBX, GetOffset(SymbolTable[Id_LoadClassRef]));
      Prg.AsmCall_REG(_EBX);

      Emit_POP_REGS;
    end;
  end
  else
  begin
    GetReg(_EAX);
    EmitLoadIntVal(_EAX, SymbolRec1);
    if SymbolTable[R.Res].CallConv <> ccREGISTER then
       Prg.AsmPush_REG(_EAX);
    FreeReg(_EAX);
  end;
end;

procedure TEmitter.EmitOP_PUSH_CLSREF;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  if SymbolRecR.Kind = kindCONSTRUCTOR then
  begin
    GetReg(_EAX);
    GetReg(_EDX);

{$IFDEF FPC}
    if SymbolRecR.Host then
    begin
      EmitLoadIntVal(_EDX, SymbolRec1);
      Prg.AsmMovREG_Imm(_EAX, 0);
    end
    else
    begin
      EmitLoadIntVal(_EAX, SymbolRec1);
      Prg.AsmMovREG_Imm(_EDX, 1);
    end;
{$ELSE}
    EmitLoadIntVal(_EAX, SymbolRec1);
    Prg.AsmMovREG_Imm(_EDX, 1);
{$ENDIF}

    FreeReg(_EDX);
    FreeReg(_EAX);

    if SymbolRecR.IsExternal then
    begin
      Emit_PUSH_REGS;

      SubId := Id_LoadClassRef;

      EmitCallPro(SubId);

      Reg := GetRegEx;

      Prg.AsmPush_REG(_EAX);

      Prg.AsmMovREG_REG(Reg, _ESI);
      Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);

      Emit_POP_REGS;
    end;
  end
  else
  begin
    GetReg(_EAX);
    EmitLoadIntVal(_EAX, SymbolRec1);
    if SymbolTable[R.Res].CallConv <> ccREGISTER then
       Prg.AsmPush_REG(_EAX);
    FreeReg(_EAX);
  end;
end;

procedure TEmitter.EmitOP_PUSH_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  if Host1 or ByRef1 then
  begin
    EmitLoadAddress(Reg, SymbolRec1);

    Prg.AsmAddREG_Imm(Reg, 4);
    Prg.AsmPush_REGPtr(Reg);
    Prg.AsmAddREG_Imm(Reg, -4);
    Prg.AsmPush_REGPtr(Reg);
  end
  else
  begin
    EmitGet_REG(Reg, SymbolRec1, + 4);
    Prg.AsmPush_REG(Reg);
    EmitGet_REG(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_EVENT_64;
begin
  RaiseNotImpl;
end;
procedure TEmitter.EmitOP_PUSH_EVENT;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  HandlesEvents := true;

  Reg := GetReg;
  if Host1 or ByRef1 then
  begin
    EmitLoadAddress(Reg, SymbolRec1);

    Prg.AsmAddREG_Imm(Reg, 4);
    Prg.AsmPush_REGPtr(Reg);
    Prg.AsmAddREG_Imm(Reg, -4);
    Prg.AsmPush_REGPtr(Reg);
  end
  else
  begin
    EmitGet_REG(Reg, SymbolRec1, + 4);
    Prg.AsmPush_REG(Reg);
    EmitGet_REG(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_DOUBLE_64;
var
  Reg, SubId, ParamNumber, ParamId: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);
  S := GetSymbolRec(ParamId);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  if S.XMMReg > 0 then
  begin
    Prg.AsmMovsdXMM_RegPtr(S.XMMReg, Reg);
  end
  else
  begin
    Prg.AsmMovsdXMM_RegPtr(_XMM4, Reg);
    Prg.AsmMovREG_REG(Reg, _ESP);
    Prg.AsmAddREG_Imm(Reg, S.RSPOffset);
    Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  end;
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_PUSH_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  if Host1 or ByRef1 then
  begin
    EmitLoadAddress(Reg, SymbolRec1);

    Prg.AsmAddREG_Imm(Reg, 4);
    Prg.AsmPush_REGPtr(Reg);
    Prg.AsmAddREG_Imm(Reg, -4);
    Prg.AsmPush_REGPtr(Reg);
  end
  else
  begin
    EmitGet_REG(Reg, SymbolRec1, + 4);
    Prg.AsmPush_REG(Reg);
    EmitGet_REG(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_CURRENCY;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  if Host1 or ByRef1 then
  begin
    EmitLoadAddress(Reg, SymbolRec1);

    Prg.AsmAddREG_Imm(Reg, 4);
    Prg.AsmPush_REGPtr(Reg);
    Prg.AsmAddREG_Imm(Reg, -4);
    Prg.AsmPush_REGPtr(Reg);
  end
  else
  begin
    EmitGet_REG(Reg, SymbolRec1, + 4);
    Prg.AsmPush_REG(Reg);
    EmitGet_REG(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_EXTENDED;
var
  RegAddr, RegE: Integer;
begin
  EmitPCodeOperator;

  RegAddr := GetReg;

  EmitLoadAddress(RegAddr, SymbolRec1);

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    Prg.AsmAddREG_Imm(RegAddr, 12);

    Prg.AsmPush_REGPtr(RegAddr);

    Prg.AsmAddREG_Imm(RegAddr, -4);
    Prg.AsmPush_REGPtr(RegAddr);

    Prg.AsmAddREG_Imm(RegAddr, -4);
    Prg.AsmPush_REGPtr(RegAddr);

    Prg.AsmAddREG_Imm(RegAddr, -4);
    Prg.AsmPush_REGPtr(RegAddr);
  end
  else
  begin
    RegE    := GetReg;
    Prg.AsmAddREG_Imm(RegAddr, 8);
    Prg.AsmMovREG16_REGPtr(RegE, RegAddr);
    Prg.AsmPush_REG(RegE);

    Prg.AsmAddREG_Imm(RegAddr, -4);
    Prg.AsmPush_REGPtr(RegAddr);
    Prg.AsmAddREG_Imm(RegAddr, -4);
    Prg.AsmPush_REGPtr(RegAddr);
    FreeReg(RegE);
  end;

  FreeReg(RegAddr);
end;

procedure TEmitter.EmitOP_PUSH_SINGLE_64;
var
  Reg, SubId, ParamNumber, ParamId: Integer;
  S: TSymbolRec;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);
  S := GetSymbolRec(ParamId);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  if S.XMMReg > 0 then
  begin
    Prg.AsmMovssXMM_RegPtr(S.XMMReg, Reg);
  end
  else
  begin
    Prg.AsmMovssXMM_RegPtr(_XMM4, Reg);
    Prg.AsmMovREG_REG(Reg, _ESP);
    Prg.AsmAddREG_Imm(Reg, S.RSPOffset);
    Prg.AsmMovssRegPtr_XMM(_XMM4, Reg);
  end;
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_PUSH_SINGLE;
var
  Reg: Integer;
  I: Integer;
  S: Single;
begin
  EmitPCodeOperator;

  if SymbolRec1.Kind = KindCONST then
  begin
    S := SymbolRec1.Value;
    Move(S, I, SizeOf(Single));
    Prg.AsmPush_IMM(I);
  end
  else
  begin
    Reg := GetReg;
    EmitLoadIntVal(Reg, SymbolRec1);
    Prg.AsmPush_REG(Reg);
    FreeReg(Reg);
  end;
end;

procedure TEmitter.EmitOP_PUSH_ANSISTRING_64;
begin
  EmitOP_PUSH_ANSISTRING;
end;
procedure TEmitter.EmitOP_PUSH_ANSISTRING;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_WIDESTRING_64;
begin
  EmitOP_PUSH_WIDESTRING;
end;
procedure TEmitter.EmitOP_PUSH_WIDESTRING;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_UNICSTRING_64;
begin
  EmitOP_PUSH_UNICSTRING;
end;
procedure TEmitter.EmitOP_PUSH_UNICSTRING;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadIntVal(Reg, SymbolRec1);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec1);
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_PUSH_SHORTSTRING_64;
begin
  EmitOP_PUSH_SHORTSTRING;
end;
procedure TEmitter.EmitOP_PUSH_SHORTSTRING;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber: Integer;
begin
  EmitPCodeOperator;

  // push address

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);
  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitLoadAddress(Reg, SymbolRec1);
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

{$IFNDEF PAXARM}
procedure TEmitter.EmitOP_PUSH_PANSICHAR_IMM_64;
begin
  EmitOP_PUSH_PANSICHAR_IMM;
end;
procedure TEmitter.EmitOP_PUSH_PANSICHAR_IMM;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber, Id: Integer;
begin
  EmitPCodeOperator;

  // to provide unique constant address
  Id := SymbolTable.FindPAnsiCharConst(SymbolRec1.Value, R.Arg1 - 1);
  if Id > 0 then
  if SymbolTable.InCode[Id] then
    R.Arg1 := Id;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitGet_REG(Reg, SymbolRec1); // pchar source
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitGet_REG(Reg, SymbolRec1); // pchar source
  EmitPushParam(Reg);
  FreeReg(Reg);
end;
{$ENDIF}

procedure TEmitter.EmitOP_PUSH_PWIDECHAR_IMM_64;
begin
  EmitOP_PUSH_PWIDECHAR_IMM;
end;
procedure TEmitter.EmitOP_PUSH_PWIDECHAR_IMM;
var
  Reg: Integer;
  SubId, ParamId, ParamNumber, Id: Integer;
begin
  EmitPCodeOperator;

  // to provide unique constant address
  Id := SymbolTable.FindPWideCharConst(SymbolRec1.Value, R.Arg1 - 1);
  if Id > 0 then
  if SymbolTable.InCode[Id] then
    R.Arg1 := Id;

  SubId := R.Res;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);

  Reg := GetSymbolRec(ParamId).Register;

  if Reg > 0 then
  begin
    GetReg(Reg);
    EmitGet_REG(Reg, SymbolRec1); // pchar source
    FreeReg(Reg);
    Exit;
  end;

  Reg := GetReg;
  EmitGet_REG(Reg, SymbolRec1); // pchar source
  EmitPushParam(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ADDREF_64;
begin
  EmitOP_ADDREF;
end;

procedure TEmitter.EmitOP_ADDREF;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := 0;

  case SymbolRec1.FinalTypeId of
{$IFNDEF PAXARM}
    typeANSISTRING: SubId := Id_StringAddRef;
    typeWIDESTRING: SubId := Id_WideStringAddRef;
{$ENDIF}
    typeUNICSTRING: SubId := Id_UnicStringAddRef;
    typeVARIANT, typeOLEVARIANT: SubId := Id_VariantAddRef;
    typeDYNARRAY: SubId := Id_DynarrayAddRef;
    typeINTERFACE: SubId := Id_InterfaceAddRef;
   else
    RaiseError(errInternalError, []);
  end;

  EmitCallPro(SubId);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmPush_REG(Reg);
  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_BEGIN_CALL_64;
begin
  EmitOP_BEGIN_CALL;
end;
procedure TEmitter.EmitOP_BEGIN_CALL;
var
  Reg, I, SubId: Integer;
  Code: TCode;
begin
  EmitPCodeOperator;

  SubId := SymbolRec1.Id;

  if not SymbolRec1.IsNestedSub then
    SaveRegisters([_ESI, _EDI], 4 * SizeOfPointer);

  EmitCallPro(SubId);

  if SymbolRec1.CallConv = ccSAFECALL then
  begin
    Code := TKernel(kernel).Code;
    I := Code.N;
    while I < Code.Card do
    begin
      Inc(I);
      if Code[I].Op = OP_CALL then
        if Code[I].Arg1 = SubId then
        begin
          if Code[I].Res > 0 then
          begin
            Reg := GetReg;
            EmitLoadAddress(Reg, GetSymbolRec(Code[I].Res));
            Prg.AsmPush_REG(Reg);
            FreeReg(Reg);

            Exit;
          end;
          break;
        end;
    end;
    if Code[I].Res > 0 then
      RaiseError(errInternalError, []);
  end;
end;

procedure TEmitter.EmitOP_CALL_64;
begin
  EmitOP_CALL;
end;
procedure TEmitter.EmitOP_CALL;

function DiscardVirtualCall: Boolean;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;
  I := Code.N - 1;
  while Code[I].Op <> OP_PUSH_INST do
    Dec(I);
  result := Code[I].CodeRecTag = TAG_DISCARD_VIRTUAL_CALL;
end;

procedure EmitInterfaceAddress(MethodIndex: Integer);
var
  Code: TCode;
  I, RegInstance: Integer;
{$IFDEF FPC}
  b: Boolean;
{$ENDIF}
begin
{$IFDEF FPC}
  b := GetSymbolRec(SymbolRec1.Level).FinalTypeId = typeINTERFACE;
{$ENDIF}

  if TargetPlatform = tpWIN64 then
    RegInstance := _ECX
  else
    RegInstance := _EAX;
  Code := TKernel(kernel).Code;
  I := Code.N - 1;
  while Code[I].Op <> OP_PUSH_INST do
    Dec(I);

  if I = Code.N - 1 then
  begin
    prg.AsmMovREG_REGPtr(_EBX, RegInstance);
    prg.AsmAddREG_Imm(_EBX, (MethodIndex - 1) * SizeOfPointer);

{$IFDEF FPC}
    if not b then
      prg.AsmAddREG_Imm(_EBX, FPC_VIRTUAL_OFFSET);
{$ENDIF}
    prg.AsmMovREG_REGPtr(_EBX, _EBX);
  end
  else
  begin
    Emit_PUSH_REGS;
    EmitLoadIntVal(_EAX, GetSymbolRec(Code[I].Arg1));
    prg.AsmMovREG_REGPtr(_EBX, RegInstance);
    prg.AsmAddREG_Imm(_EBX, (MethodIndex - 1) * SizeOfPointer);
{$IFDEF FPC}
    if not b then
      prg.AsmAddREG_Imm(_EBX, FPC_VIRTUAL_OFFSET);
{$ENDIF}
    prg.AsmMovREG_REGPtr(_EBX, _EBX);
    Emit_POP_REGS;
  end;
end;

function FindBeginCall: TCodeRec;
var
  RR: TCodeRec;
  I, K, SubId: Integer;
  Code: TCode;
begin
  result := nil;
  Code := TKernel(kernel).Code;
  I := Code.N - 1;
  RR := Code[Code.N];
  SubId := RR.Arg1;
  K := 0;
  repeat
    if (Code[I].Op = OP_CALL) and (Code[I].Arg1 = SubId) then
      Dec(K)
    else if (Code[I].Op = OP_BEGIN_CALL) and (Code[I].Arg1 = SubId) then
    begin
      if K = 0 then
      begin
        result := Code[I];
        Exit;
      end;

      Inc(K);
    end;

    Dec(I);
  until I = 0;
end;

var
  Reg, ParamId, I, K, T: Integer;

 {$IFDEF FPC}
  SubId: Integer;
 {$ENDIF}

  Code: TCode;
  TrueSubId, cc: Integer;

  RR: TCodeRec;
begin
  EmitPCodeOperator;
  Code := TKernel(kernel).Code;

  if SymbolRec1.Kind = KindVAR then
  begin
    T := SymbolRec1.TerminalTypeId;
    TrueSubId := GetSymbolRec(T).PatternId;

    if not (GetSymbolRec(TrueSubId).Kind in kindSUBS) then
    begin
      K := Code.N - 1;
      repeat
        if Code[K] .Op = OP_GET_VMT_ADDRESS then
          if Code[K] .Res = SymbolRec1.Id then
          begin
            TrueSubId := Code[K] .Arg2;
            break;
          end;

        if Code[K] .Op = OP_SEPARATOR then
          break;

        Dec(K);
      until false;
    end
    else
    begin
    // ok
    end;

  end
  else
    TrueSubId := SymbolRec1.Id;

  cc := GetSymbolRec(TrueSubId).CallConv;

  if (GetSymbolRec(TrueSubId).Level = H_TObject) then
  if (GetSymbolRec(TrueSubId).Name = 'ClassName') then
  begin
    if Code[Code.N - 1].Op = OP_PUSH_CLSREF then
    begin
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmMovREG_REG(_EDX, _ECX);
        Prg.AsmMovREG_Imm(_ECX, 1);
      end
      else
        Prg.AsmPush_Imm(1);
    end
    else if Code[Code.N - 1].Op = OP_PUSH_INST then
    begin
      I := Code.GetCurrSelfId(Code.N);
      if I = Code[Code.N - 1].Arg1 then
      begin
        I := Code.GetCurrSubId(Code.N);
        if GetSymbolRec(I).IsStatic then
        begin
          if TargetPlatform = tpWIN64 then
          begin
            Prg.AsmMovREG_REG(_R8, _ECX);
            Prg.AsmMovREG_Imm(_EDX, 1);
          end
          else
            Prg.AsmPush_Imm(1);
        end
        else
        begin
          if TargetPlatform = tpWIN64 then
          begin
            Prg.AsmMovREG_REG(_R8, _ECX);
            Prg.AsmMovREG_Imm(_EDX, 0);
          end
          else
            Prg.AsmPush_Imm(0);
        end;
      end
      else
      begin
        if TargetPlatform = tpWIN64 then
        begin
          Prg.AsmMovREG_REG(_R8, _ECX);
          Prg.AsmMovREG_Imm(_EDX, 0);
        end
        else
          Prg.AsmPush_Imm(0);
      end;
    end
    else
    begin
      if TargetPlatform = tpWIN64 then
      begin
        Prg.AsmMovREG_REG(_R8, _ECX);
        Prg.AsmMovREG_Imm(_EDX, 0);
      end
      else
        Prg.AsmPush_Imm(0);
    end;
  end;

  {$IFDEF TRIAL}
   Inc(_Counter);
  {$ENDIF}

  Reg := GetReg(_EBX);

  if SymbolRec1.RunnerParameter then
  begin
    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
    Prg.AsmAddREG_Imm(Reg, RUNNER_OWNER_OFFSET);
    Prg.AsmMovREG_REGPtr(_EDI, Reg);
  end;

  if Host1 and (SymbolRec1.PushProgRequired) then
  begin
    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
    if TargetPlatform = tpWIN64 then
      Prg.AsmMovREG_REG(_ECX, Reg)   // push TProgram.Self
    else
      Prg.AsmPush_REG(Reg);           // push TProgram.Self
  end;

  if GetSymbolRec(TrueSubId).ExtraParamNeeded and (cc <> ccSAFECALL) then
  begin
    if GetSymbolRec(TrueSubId).CallConv = ccREGISTER then
    begin
      K := 0;
      if (GetSymbolRec(TrueSubId).IsMethod or
         (GetSymbolRec(TrueSubId-1).FinalTypeId = typeEVENT) and (GetSymbolRec(TrueSubId-1).PatternId = TrueSubId)) and
        (GetSymbolRec(TrueSubId).CallMode <> cmSTATIC) then
        Inc(K);

      for I:= 0 to R.Arg2 - 1 do
      begin
        ParamId := SymbolTable.GetParamId(TrueSubId, I);
        if GetSymbolRec(ParamId).Register > 0 then
        begin
          Inc(K);
          if GetSymbolRec(ParamId).FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
            if GetSymbolRec(ParamId).IsOpenArray then
              Inc(K);
        end;
      end;

      case K of
        0:
        begin
          GetReg(_EAX);
          EmitLoadAddress(_EAX, SymbolRecR);
          FreeReg(_EAX);
        end;
        1:
        begin
          GetReg(_EDX);
          EmitLoadAddress(_EDX, SymbolRecR);
          FreeReg(_EDX);
        end;
        2:
        begin
          GetReg(_ECX);
          EmitLoadAddress(_ECX, SymbolRecR);
          FreeReg(_ECX);
        end;
        else
        begin
          EmitLoadAddress(Reg, SymbolRecR);
          Prg.AsmPush_REG(Reg);
        end;
      end;
    end //ccREGISTER
    else if GetSymbolRec(TrueSubId).CallConv = cc64 then
    begin
      K := 0;
      if (GetSymbolRec(TrueSubId).IsMethod or
         (GetSymbolRec(TrueSubId-1).FinalTypeId = typeEVENT) and (GetSymbolRec(TrueSubId-1).PatternId = TrueSubId)) and
        (GetSymbolRec(TrueSubId).CallMode <> cmSTATIC) then
        Inc(K);

     if GetSymbolRec(TrueSubId).IsNestedSub then
       K := 1;

     if (GetSymbolRec(TrueSubId).Level = H_TObject) then
     if (GetSymbolRec(TrueSubId).Name = 'ClassName') then
       K := 0;

     if GetSymbolRec(TrueSubId).IsFakeMethod then
       K := 0;

      case K of
        0:
        begin
          GetReg(_ECX);
          EmitLoadAddress(_ECX, SymbolRecR);
          FreeReg(_ECX);
        end;
        1:
        begin
          GetReg(_EDX);
          EmitLoadAddress(_EDX, SymbolRecR);
          FreeReg(_EDX);
        end;
        else
        begin
          RaiseError(errInternalError, []);
        end;
      end;
    end //cc64
    else
    begin

      if (GetSymbolRec(TrueSubId).CallConv = ccPASCAL) and
         (GetSymbolRec(TrueSubId).IsMethod) then
      begin
        GetReg(_EDX);
        Prg.AsmPop_REG(_EDX);
        FreeReg(_EDX);
      end;

      EmitLoadAddress(Reg, SymbolRecR);
      Prg.AsmPush_REG(Reg);

      if (GetSymbolRec(TrueSubId).CallConv = ccPASCAL) and
         (GetSymbolRec(TrueSubId).IsMethod) then
      begin
        GetReg(_EDX);
        Prg.AsmPush_REG(_EDX);
        FreeReg(_EDX);
      end;

    end;
  end; // extra param needed

  if Host1 then
  begin
    if (SymbolRec1.MethodIndex > 0) and (GetSymbolRec(SymbolRec1.Level).FinalTypeId = typeINTERFACE) then
      EmitInterfaceAddress(SymbolRec1.MethodIndex)
    else
      EmitGet_REG(Reg, SymbolRec1);
  end
  else
  begin
    if (SymbolRec1.MethodIndex > 0) and (not DiscardVirtualCall) then
      EmitInterfaceAddress(SymbolRec1.MethodIndex)
    else if SymbolRec1.Kind = kindVAR then
      EmitLoadIntVal(Reg, SymbolRec1)
    else
    begin
      Prg.AsmMovREG_REG(Reg, _EDI);
      Prg.AsmAddREG_Imm(Reg, 0);
      Prg.Top.SaveSubId := R.Arg1;
      List2.Add(Prg.Top);
    end;
  end;

  if SymbolRec1.Kind = KindVAR then
    if SymbolRec1.Host then
    if SymbolRec1.FinalTypeId = typeEVENT then
    begin
      Prg.AsmMovREG_REGPtr(Reg, Reg);
    end;

  if GetSymbolRec(TrueSubId).IsNestedSub then
  begin
    if ContextStack.Count >= 2 then
    begin
      if TargetPlatform = tpWin64 then
        EmitRestoreEBP(_ECX, GetSymbolRec(TrueSubId))
      else
        EmitRestoreEBP(_EAX, GetSymbolRec(TrueSubId));
    end
    else
    begin
      if TargetPlatform = tpWin64 then
        Prg.AsmMovREG_REG(_ECX, _EBP)
      else
        Prg.AsmMovREG_REG(_EAX, _EBP);
    end;
  end;

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    EmitSaveRDI;
    Prg.AsmMovREG_REG(_EDI, Reg);
    EmitRestoreRBX;
    Prg.AsmCall_REG(_EDI);
    EmitRestoreRDI;
  end
  else
    Prg.AsmCall_REG(Reg);

  EmitCallEpi(TrueSubId);

  if not (TargetPlatform in [tpOSX32, tpIOSSim]) then
  begin
    if SymbolRec1.Kind = KindVAR then
    begin
      T := SymbolRec1.TerminalTypeId;
      K := GetSymbolRec(T).PatternId;
      if GetSymbolRec(K).CallConv = ccCDECL then
        Prg.AsmAddREG_Imm(_ESP, SymbolTable.GetSizeOfParams(K));
    end
    else
    begin
      if SymbolRec1.CallConv = ccCDECL then
        Prg.AsmAddREG_Imm(_ESP, SymbolTable.GetSizeOfParams(R.Arg1));
    end;
  end;

  if not SymbolRec1.IsNestedSub then
  begin
    RestoreRegisters([_ESI, _EDI], 4 * SizeOfPointer);
  end;

  if R.Res <> 0 then if cc <> ccSAFECALL then
  begin
    case GetSymbolRec(TrueSubId).FinalTypeId of
      typeVOID:
      begin
        // ok
      end;
      typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL,
{$IFNDEF PAXARM}
      typeANSICHAR,
{$ENDIF}
      typeWIDECHAR, typeENUM, typePOINTER,
      typeINTEGER, typeBYTE, typeWORD, typeSMALLINT, typeSHORTINT,
      typeCARDINAL, typeCLASS, typeCLASSREF:
      begin
        if SymbolRec1.CallConv = ccSAFECALL then
        begin
          RR := FindBeginCall;
          if RR = nil then
            RaiseError(errInternalError, []);
          EmitLoadIntVal(_EAX, GetSymbolRec(RR.Res));
        end;

        EmitPut_REG(_EAX, SymbolRecR);
      end;
      typeINT64, typeUINT64:
      begin
        GetReg(_ECX);
        GetReg(_EDX);
        EmitLoadAddress(_ECX, SymbolRecR);
        Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
        Prg.AsmAddREG_Imm(_ECX, 4);
        Prg.AsmMovREGPtr_REG32(_ECX, _EDX);
        FreeReg(_ECX);
        FreeReg(_EDX);
      end;
{$IFNDEF PAXARM}
      typeANSISTRING:
      begin
        {$IFDEF FPC}
{
          GetReg(_EAX);

          Prg.AsmPush_REG(_EAX); // save result of function

          EmitLoadAddress(_EAX, SymbolRecR); // load address into EAX
          SubId := Id_DecStringCounter;
//          SubId := Id_IncStringCounter;
          EmitGet_REG(_EBX, TKernel(kernel).SymbolTable[SubId]);
          Prg.AsmCall_REG(_EBX);

          EmitLoadAddress(_EAX, SymbolRecR); // load address into EAX

          Prg.AsmPop_REG(_EBX);
          Prg.AsmMovREGPtr_REG(_EAX, _EBX);

          FreeReg(_EAX);
}
        {$ELSE}
        // ok
        {$ENDIF}
      end;
      typeSHORTSTRING:
      begin
        // ok
      end;
      typeWIDESTRING:
      begin
        // ok
      end;
{$ENDIF}
      typeUNICSTRING:
      begin
        // ok
      end;
      typeINTERFACE:
      begin
        // ok
      end;
      typeVARIANT, typeOLEVARIANT:
      begin
        // ok
      end;
      typeRECORD:
      begin
        if GetSymbolRec(TrueSubId).CallConv = ccMSFASTCALL then
        begin
          T := GetSymbolRec(TrueSubId).TerminalTypeId;
          if GetSymbolRec(T).Size <= 4 then
          begin
            EmitPut_REG(_EAX, SymbolRecR);
          end
          else if GetSymbolRec(T).Size <= 8 then
          begin
            GetReg(_ECX);
            GetReg(_EDX);
            EmitLoadAddress(_ECX, SymbolRecR);
            Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
            Prg.AsmAddREG_Imm(_ECX, 4);
            Prg.AsmMovREGPtr_REG32(_ECX, _EDX);
            FreeReg(_ECX);
            FreeReg(_EDX);
          end;
        end
        else
        begin
          T := GetSymbolRec(TrueSubId).TerminalTypeId;
          if GetSymbolRec(T).Size <= SizeOf(Pointer) then
          begin
            EmitPut_REG(_EAX, SymbolRecR);
          end
        end;
      end;
      typeARRAY:
      begin
        T := GetSymbolRec(TrueSubId).TerminalTypeId;
        if GetSymbolRec(T).Size <= SizeOf(Pointer) then
        begin
          EmitPut_REG(_EAX, SymbolRecR);
        end
      end;
      typeDYNARRAY:
      begin
        // ok
      end;
      typeSET:
      begin
        if not GetSymbolRec(TrueSubId).ExtraParamNeeded then
          EmitPut_REG(_EAX, SymbolRecR);

        // else - ok
      end;
      typeDOUBLE:
      begin
        EmitLoadAddress(Reg, SymbolRecR);
        if TargetPlatform = tpWIN64 then
          Prg.AsmMovsdRegPtr_XMM(_XMM0, Reg)
        else
          Prg.AsmFStpDouble_REGPtr(Reg);
      end;
      typeSINGLE:
      begin
        EmitLoadAddress(Reg, SymbolRecR);
        if TargetPlatform = tpWIN64 then
          Prg.AsmMovsdRegPtr_XMM(_XMM0, Reg)
        else
          Prg.AsmFStpSingle_REGPtr(Reg);
      end;
      typeEXTENDED:
      begin
        EmitLoadAddress(Reg, SymbolRecR);
        if TargetPlatform = tpWIN64 then
          Prg.AsmMovsdRegPtr_XMM(_XMM0, Reg)
        else
          Prg.AsmFStpExtended_REGPtr(Reg);
      end;
      typeCURRENCY:
      begin
        EmitLoadAddress(Reg, SymbolRecR);
        EmitFistp(SymbolRecR);
      end;
      typeEVENT:
      begin
        // ok
      end;
      else
        RaiseError(errInternalError, []);
    end;
  end;

  FreeReg(Reg);

  if TrueSubId = Id_TObject_Free then
  begin
    I := Code[Code.N - 1].Arg1;
    Reg := EmitGetAddressRegister(SymbolTable[I]);
    Prg.AsmMovREGPtr_Imm(Reg, SymbolTable[I], 0);
    FreeReg(Reg);
  end;

  if HandlesEvents then
  begin
    Reg := GetRegEx;

    Prg.AsmMovREG_REG(Reg, _ESI);
    Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
    Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
    if TargetPlatform = tpWIN64 then
      Prg.AsmMovREG_REG(_ECX, Reg)
    else
      Prg.AsmPush_REG(Reg);             // push TProgram.Self

    EmitGet_REG(Reg, SymbolTable[Id_CondHalt]);
    Prg.AsmCall_REG(Reg);

    FreeReg(Reg);
  end;
end;

procedure TEmitter.EmitOP_INIT_SUB_64;
begin
  EmitOP_INIT_SUB;
end;
procedure TEmitter.EmitOP_INIT_SUB;
var
  SubId, I, J, T, TypeID, Reg, RegEx, S, ParamId, ParamCount, SZ, FT, SubId2: Integer;
  L, TypeList, ProtectedShits: TIntegerList;
  HighParamId: Integer;
begin
  EmitPCodeOperator;

  ProtectedShits := TIntegerList.Create;

  try

  SubId := R.Arg1;
  ParamCount := GetSymbolRec(SubId).Count;

  EmitStartSub(SubId);

  for I:=0 to ParamCount - 1 do
  begin
    ParamId := SymbolTable.GetParamId(SubId, I);
    if GetSymbolRec(ParamId).Register > 0 then
      ProtectedShits.Add(GetSymbolRec(ParamId).Shift);
    if GetSymbolRec(ParamId).IsOpenArray then
    begin
      ParamId := SymbolTable.GetOpenArrayHighId(ParamId);
      ProtectedShits.Add(GetSymbolRec(ParamId).Shift);
    end;
  end;

  case TargetPlatform of
    tpOSX32, tpIOSSim:
    begin
      Prg.AsmPush_REG(_EBP);
      Prg.AsmPush_REG(_ESI);
      Prg.AsmPush_REG(_EDI);
      Prg.AsmPush_REG(_EBX);
      Prg.AsmMovREG_REG(_EBP, _ESP);
      Prg.AsmAddREG_Imm(_ESP, - SymbolTable.GetSizeOfLocalsEx(SubId));
    end;
    tpWIN64:
    begin
      Prg.AsmPush_REG(_EBP);
      Prg.AsmPush_REG(_ESI);
      Prg.AsmPush_REG(_EDI);
      Prg.AsmPush_REG(_EBX);

      Prg.AsmSubREG_Imm(_ESP, SymbolTable.GetSubRSPSize(SubId));
      Prg.AsmMovREG_REG(_EBP, _ESP);
    end;
    else
    begin
      Prg.AsmPush_REG(_EBP);
      Prg.AsmPush_REG(_ESI);
      Prg.AsmPush_REG(_EDI);
      Prg.AsmPush_REG(_EBX);
      Prg.AsmMovREG_REG(_EBP, _ESP);
      Prg.AsmAddREG_Imm(_ESP, - SymbolTable.GetSizeOfLocalsEx(SubId));
    end;
  end;

  Prg.EmitZ;

  ContextStack.Push(SubId);

  if GetSymbolRec(SubId).CallConv = cc64 then
  begin
    if SymbolRec1.IsNestedSub then
    begin
      ParamId := SymbolTable.GetRBP_Id(SubId);
      Prg.AsmPutREG_EBPPtr(_ECX, GetOffset(GetSymbolRec(ParamId)));
    end;

    ParamId := SymbolTable.GetSelfId(SubId);
    if GetSymbolRec(ParamId).Register > 0 then
      Prg.AsmPutREG_EBPPtr(GetSymbolRec(ParamId).Register, GetOffset(GetSymbolRec(ParamId)));

    for I:=0 to GetSymbolRec(SubId).Count - 1 do
    begin
      ParamId := SymbolTable.GetParamId(SubId, I);

      if GetSymbolRec(ParamId).XMMReg > 0 then
      begin
        Prg.AsmMovREG_REG(_EBX, _EBP);
        Prg.AsmAddREG_Imm(_EBX, GetOffset(GetSymbolRec(ParamId)));
        if GetSymbolRec(ParamId).FinalTypeId = typeSINGLE then
          Prg.AsmMovssRegPtr_XMM(GetSymbolRec(ParamId).XMMReg, _EBX)
        else
          Prg.AsmMovsdRegPtr_XMM(GetSymbolRec(ParamId).XMMReg, _EBX);
      end
      else if GetSymbolRec(ParamId).Register > 0 then
      begin
        Prg.AsmPutREG_EBPPtr(GetSymbolRec(ParamId).Register, GetOffset(GetSymbolRec(ParamId)));
        if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
        begin
          Reg := GetSymbolRec(ParamId).Register;
          HighParamId := SymbolTable.GetOpenArrayHighId(ParamId);
          if Reg > 0 then
          begin
            if Reg <> _R9 then
            begin
              if Reg = _ECX then
                Reg := _EDX
              else if Reg = _EDX then
                Reg := _R8
              else if Reg = _R8 then
                Reg := _R9;
              Prg.AsmPutREG32_EBPPtr(Reg, GetOffset(GetSymbolRec(HighParamId)));
            end
            else
            begin
              Prg.AsmGetREG32_EBPPtr(_EBX, 8);
              Prg.AsmPutREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(HighParamId)));
            end;
          end;
        end;
      end
      else
      if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
      begin
        HighParamId := SymbolTable.GetOpenArrayHighId(ParamId);
        Prg.AsmGetREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(ParamId)) - 4);
        Prg.AsmPutREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(HighParamId)));
      end;
    end;

    if GetSymbolRec(SubId).ExtraParamNeeded then
    begin
      ParamId := SymbolTable.GetResultId(SubId);
      RegEx := GetSymbolRec(ParamId).Register;
      if RegEx > 0 then
      begin
        Prg.AsmPutREG_EBPPtr(RegEx, GetOffset(GetSymbolRec(ParamId)));
      end
      else
      begin
        RaiseError(errInternalError, []);
      end;
    end;
  end //cc64
  else if GetSymbolRec(SubId).CallConv in [ccREGISTER, ccMSFASTCALL] then
  begin
    ParamId := SymbolTable.GetSelfId(SubId);
    if GetSymbolRec(ParamId).Register > 0 then
      Prg.AsmPutREG32_EBPPtr(GetSymbolRec(ParamId).Register, GetOffset(GetSymbolRec(ParamId)));

    for I:=0 to GetSymbolRec(SubId).Count - 1 do
    begin
      ParamId := SymbolTable.GetParamId(SubId, I);
      if GetSymbolRec(ParamId).Register > 0 then
      begin
        Prg.AsmPutREG32_EBPPtr(GetSymbolRec(ParamId).Register, GetOffset(GetSymbolRec(ParamId)));
        if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
        begin
          Reg := GetSymbolRec(ParamId).Register;
          HighParamId := SymbolTable.GetOpenArrayHighId(ParamId);
          if Reg > 0 then
          begin
            if Reg <> _ECX then
            begin
              if Reg = _EAX then
                Reg := _EDX
              else if Reg = _EDX then
                Reg := _ECX;
              Prg.AsmPutREG32_EBPPtr(Reg, GetOffset(GetSymbolRec(HighParamId)));
            end
            else
            begin
              Prg.AsmGetREG32_EBPPtr(_EBX, 8);
              Prg.AsmPutREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(HighParamId)));
            end;
          end;
        end;
      end
      else
      if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
      begin
        HighParamId := SymbolTable.GetOpenArrayHighId(ParamId);
        Prg.AsmGetREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(ParamId)) - 4);
        Prg.AsmPutREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(HighParamId)));
      end;
    end;

    if GetSymbolRec(SubId).ExtraParamNeeded then
    begin
      ParamId := SymbolTable.GetResultId(SubId);
      if GetSymbolRec(ParamId).Register > 0 then
        Prg.AsmPutREG32_EBPPtr(GetSymbolRec(ParamId).Register, GetOffset(GetSymbolRec(ParamId)));
    end;
  end
  else if GetSymbolRec(SubId).CallConv in [ccSTDCALL, ccCDECL, ccSAFECALL] then
  begin
    if SymbolRec1.IsNestedSub then
    begin
      ParamId := SymbolTable.GetRBP_Id(SubId);
      Prg.AsmPutREG_EBPPtr(_EAX, GetSymbolRec(ParamId).Shift);
      ProtectedShits.Add(GetSymbolRec(ParamId).Shift);
    end;

    for I:=0 to GetSymbolRec(SubId).Count - 1 do
    begin
      ParamId := SymbolTable.GetParamId(SubId, I);
      if GetSymbolRec(ParamId).FinalTypeId = typeOPENARRAY then
      begin
        HighParamId := SymbolTable.GetOpenArrayHighId(ParamId);
        Prg.AsmGetREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(ParamId)) + 4);
        Prg.AsmPutREG32_EBPPtr(_EBX, GetOffset(GetSymbolRec(HighParamId)));
      end;
    end;
  end;

  // init dynamic vars

  for I:=0 to ParamCount - 1 do
  begin
    ParamId := SymbolTable.GetParamId(SubId, I);
    FT := SymbolTable[ParamId].FinalTypeId;
    if FT in [typeRECORD, typeARRAY] then
      if SymbolTable[I].UnionId = 0 then
      if not (SymbolTable[ParamId].ByRef or SymbolTable[ParamId].ByRefEx) then
      begin
        TypeId := SymbolTable[ParamId].TerminalTypeId;
        L := SymbolTable.GetShiftsOfDynamicFields(TypeId);
        TypeList := SymbolTable.GetTypesOfDynamicFields(TypeId);

        if TypeList.Count <> L.Count then
          RaiseError(errInternalError, []);

        try

          Reg := GetReg;

          for J:=0 to L.Count - 1 do
          begin
            S := L[J];

            SubId2 := 0;
            case GetSymbolRec(TypeList[J]).FinalTypeId of
{$IFNDEF PAXARM}
              typeANSISTRING: SubId2 := Id_StringAddRef;
              typeWIDESTRING: SubId2 := Id_WideStringAddRef;
{$ENDIF}
              typeUNICSTRING: SubId2 := Id_UnicStringAddRef;
              typeVARIANT, typeOLEVARIANT: SubId2 := Id_VariantAddRef;
              typeDYNARRAY: SubId2 := Id_DynarrayAddRef;
              typeINTERFACE: SubId2 := Id_InterfaceAddRef;
             else
              RaiseError(errInternalError, []);
            end;

            EmitCallPro(SubId2);

            EmitLoadAddress(Reg, SymbolTable[ParamId]);
            Prg.AsmAddREG_Imm(Reg, S);
            Prg.AsmPush_REG(Reg);

            EmitStdCall(SubId2);
          end;

          FreeReg(Reg);

        finally

          FreeAndNil(L);
          FreeAndNil(TypeList);

        end;
      end;
  end;

  for I:=SubId + 1 to SymbolTable.Card do
  begin
    if SymbolTable[I].Level = SubId then
       if SymbolTable[I].Kind = KindVAR then
       begin
          if SymbolTable[I].UnionId = 0 then
          if (not (SymbolTable[I].ByRef or SymbolTable[I].ByRefEx)) then
            begin
              if SymbolTable[I].Local and (not SymbolTable[I].Param) then
              if ProtectedShits.IndexOf(SymbolTable[I].Shift) = -1 then
              begin
                T := SymbolTable[I].FinalTypeId;
                case T of
{$IFNDEF PAXARM}
                  typeANSISTRING,
                  typeWIDESTRING,
{$ENDIF}
                  typeUNICSTRING,
                  typeDYNARRAY,
                  typeINTERFACE,
                  typeCLASS,
                  typeCLASSREF:
                  begin
                    Reg := EmitGetAddressRegister(SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, SymbolTable[I], 0);
                    FreeReg(Reg);
                  end;
                  typeSET:
                  begin
                    T := SymbolTable[I].TerminalTypeId;
                    Reg := GetReg;

                    case SymbolTable.GetSizeOfSetType(T) of
                      1, 2, 4:
                      begin
                        Prg.AsmMovREG_Imm(Reg, 0);
                        EmitSaveIntVal(Reg, SymbolTable[I]);
                      end;
                      else
                      begin
                        SZ := SymbolTable.GetSizeOfSetType(T) div 4;
                        if SZ = 0 then
                          SZ := 1;
                        EmitLoadAddress(Reg, SymbolTable[I]);
                        for J := 1 to SZ do
                        begin
                          Prg.AsmMovREGPtr_Imm(Reg, 0);
                          if J < SZ then
                            Prg.AsmAddREG_Imm(Reg, 4);
                        end;
                      end;
                    end; // case
                    FreeReg(Reg);
                  end;
                  typeVARIANT, typeOLEVARIANT:
                  begin
                    Reg := GetReg;
                    EmitLoadAddress(Reg, SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    FreeReg(Reg);
                  end;
                  typeINTEGER,
                  typeBYTE,
                  typeSMALLINT,
                  typeWORD,
                  typeSHORTINT,
                  typeCARDINAL,
                  typePOINTER:
                  if TKernel(Kernel).DEBUG_MODE then
                  begin
                    Reg := EmitGetAddressRegister(SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, SymbolTable[I], 0);
                    FreeReg(Reg);
                  end;
                  typeSINGLE:
                  if TKernel(Kernel).DEBUG_MODE then
                  begin
                    Reg := GetReg;
                    EmitLoadAddress(Reg, SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    FreeReg(Reg);
                  end;
                  typeDOUBLE, typeCURRENCY, typeINT64, typeUINT64:
                  if TKernel(Kernel).DEBUG_MODE then
                  begin
                    Reg := GetReg;
                    EmitLoadAddress(Reg, SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    FreeReg(Reg);
                  end;
                  typeEXTENDED:
                  if TKernel(Kernel).DEBUG_MODE then
                  begin
                    Reg := GetReg;
                    EmitLoadAddress(Reg, SymbolTable[I]);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    Prg.AsmAddREG_Imm(Reg, 4);
                    Prg.AsmMovREGPtr_Imm(Reg, 0);
                    FreeReg(Reg);
                  end;
                  typeRECORD, typeARRAY:
                  begin
                    TypeId := SymbolTable[I].TerminalTypeId;
                    L := SymbolTable.GetShiftsOfDynamicFields(TypeId);

                    Reg := GetReg;

                    for J:=0 to L.Count - 1 do
                    begin
                      S := L[J];
                      EmitLoadAddress(Reg, SymbolTable[I]);
                      Prg.AsmAddREG_Imm(Reg, S);
                      Prg.AsmMovREGPtr_Imm(Reg, 0);
                    end;

                    if TypeId = H_TValue then
                    begin
                      EmitLoadAddress(Reg, SymbolTable[I]);
                      Prg.AsmMovREGPtr_Imm(Reg, 0);
                      Prg.AsmAddREG_Imm(Reg, 4);
                      Prg.AsmMovREGPtr_Imm(Reg, 0);
                      Prg.AsmAddREG_Imm(Reg, 4);
                      Prg.AsmMovREGPtr_Imm(Reg, 0);
                      Prg.AsmAddREG_Imm(Reg, 4);
                      Prg.AsmMovREGPtr_Imm(Reg, 0); //16

                      Prg.AsmAddREG_Imm(Reg, 4);
                      Prg.AsmMovREGPtr_Imm(Reg, 0);
                      Prg.AsmAddREG_Imm(Reg, 4);
                      Prg.AsmMovREGPtr_Imm(Reg, 0); // 24
                    end;

                    FreeReg(Reg);

                    FreeAndNil(L);
                  end;
                end;
              end; // local
            end;
       end;
  end;

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    Prg.EmitGetCallerEIP;
    EmitSaveRBX;
  end;

  if TKernel(Kernel).DEBUG_MODE then
  begin
    Prg.AsmComment('***** N ****** ' + IntToStr(TKernel(kernel).Code.N));
//    Prg.AsmMovREGPtr32_Imm(_ESI, H_ByteCodePtr, TKernel(kernel).Code.N);

    Emit_PUSH_REGS_EX;

    EmitCallPro(Id_InitSub);

    if TargetPlatform = tpWin64 then
    begin
      Prg.AsmMovREG_REG(_ECX, _ESI);
      Prg.AsmAddREG_Imm(_ECX, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(_ECX, _ECX); // load TProgram.Self

      Prg.AsmMovREG_Imm(_EDX, SubId);

      Prg.AsmMovREG_REG(_R8, _EBP);
    end
    else
    begin
      Prg.AsmMovREG_REG(_EAX, _ESI);
      Prg.AsmAddREG_Imm(_EAX, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(_EAX, _EAX); // load TProgram.Self

      Prg.AsmMovREG_Imm(_EDX, SubId);
      Prg.AsmMovREG_REG(_ECX, _EBP);
    end;

    EmitStdCall(Id_InitSub);
    Emit_POP_REGS_EX;
  end;

  finally
    FreeAndNil(ProtectedShits);
  end;
end;

procedure TEmitter.EmitOP_END_SUB_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_END_SUB;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_FIN_SUB_64;
begin
  EmitOP_FIN_SUB;
end;
procedure TEmitter.EmitOP_FIN_SUB;
var
  TypeID, ResultId, SubId, Reg,
  T, I, J, ParamId, ParamCount, FT, SubId2, S,
  ArrayTypeId,
  ElTypeId, ElFinalTypeId, ElSize: Integer;
  ElTypeId2, ElFinalTypeId2, ElSize2: Integer;

  L, TypeList: TIntegerList;
begin
  EmitPCodeOperator;

  SubId := R.Arg1;

  I := SymbolTable[SubId].Level;
  if I > 0 then
    if SymbolTable[I].FinalTypeId = typeINTERFACE then
      Exit;

  ParamCount := GetSymbolRec(SubId).Count;

  // clear dynamic fields in parameters

  for I:=0 to ParamCount - 1 do
  begin
    ParamId := SymbolTable.GetParamId(SubId, I);
    FT := SymbolTable[ParamId].FinalTypeId;
    if FT in [typeRECORD, typeARRAY] then
      if not (SymbolTable[ParamId].ByRef or SymbolTable[ParamId].ByRefEx) then
      begin
        TypeId := SymbolTable[ParamId].TerminalTypeId;
        L := SymbolTable.GetShiftsOfDynamicFields(TypeId);
        TypeList := SymbolTable.GetTypesOfDynamicFields(TypeId);

        if TypeList.Count <> L.Count then
          RaiseError(errInternalError, []);

        try

          Reg := GetReg;

          for J:=0 to L.Count - 1 do
          begin
            S := L[J];

            SubId2 := 0;
            case GetSymbolRec(TypeList[J]).FinalTypeId of
{$IFNDEF PAXARM}
              typeANSISTRING: SubId2 := Id_AnsiStringClr;
              typeWIDESTRING: SubId2 := Id_WideStringClr;
{$ENDIF}
              typeUNICSTRING: SubId2 := Id_UnicStringClr;
              typeVARIANT, typeOLEVARIANT: SubId2 := Id_VariantClr;
              typeDYNARRAY:
              begin
                SubId2 := Id_DynarrayClr;

                ArrayTypeId := TypeList[J];
                ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
                ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
                ElSize := SymbolTable[ElTypeId].Size;

                ElTypeId2 := 0;
                ElFinalTypeId2 := 0;
                ElSize2 := 0;
                if ElFinalTypeId = typeDYNARRAY then
                begin
                  ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
                  ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
                  ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
                  ElSize2 := SymbolTable[ElTypeId2].Size;
                end;

                Prg.AsmPush_Imm(ElSize2);
                Prg.AsmPush_Imm(ElTypeId2);
                Prg.AsmPush_Imm(ElFinalTypeId2);

                Prg.AsmPush_Imm(ElSize);
                Prg.AsmPush_Imm(ElTypeId);
                Prg.AsmPush_Imm(ElFinalTypeId);

              end;
              typeINTERFACE: SubId2 := Id_InterfaceClr;
             else
              RaiseError(errInternalError, []);
            end;

            EmitCallPro(SubId2);

            EmitLoadAddress(Reg, SymbolTable[ParamId]);
            Prg.AsmAddREG_Imm(Reg, S);
            Prg.AsmPush_REG(Reg);

            EmitStdCall(SubId2);
          end;

          FreeReg(Reg);

        finally

          FreeAndNil(L);
          FreeAndNil(TypeList);

        end;
      end;
  end;


  if TKernel(Kernel).DEBUG_MODE then
  begin
    EmitCallPro(Id_EndSub);

    if TargetPlatform = tpWIN64 then
    begin
      Prg.AsmMovREG_REG(_ECX, _ESI);
      Prg.AsmAddREG_Imm(_ECX, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(_ECX, _ECX); // load TProgram.Self
    end
    else
    begin
      Prg.AsmMovREG_REG(_EAX, _ESI);
      Prg.AsmAddREG_Imm(_EAX, H_SelfPtr);
      Prg.AsmMovREG_REGPtr(_EAX, _EAX); // load TProgram.Self
    end;

    EmitStdCall(Id_EndSub);
  end;

  TypeID := SymbolRec1.FinalTypeID;
  ResultId := SymbolTable.GetResultId(SubId);

  if SymbolRec1.Kind = KindCONSTRUCTOR then
  begin
    ResultId := SymbolTable.GetSelfId(SubId);
    EmitGet_REG(_EAX, SymbolTable[ResultId]);
  end
  else if SymbolRec1.Kind = KindDESTRUCTOR then
  begin
    // ok
  end
  else if TypeID in INT64Types then
  begin
    GetReg(_EAX);
    GetReg(_EDX);
    GetReg(_ECX);

    EmitLoadAddress(_ECX, SymbolTable[ResultId]);
    Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
    Prg.AsmAddREG_Imm(_ECX, 4);
    Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

    FreeReg(_EAX);
    FreeReg(_EDX);
    FreeReg(_ECX);
  end
  else if (TypeID = typeRECORD) and
          (SymbolRec1.CallConv = ccMSFASTCALL) then
  begin
    T := SymbolRec1.TerminalTypeId;
    if GetSymbolRec(T).Size <= SizeOf(Pointer) then
    begin
      EmitGet_REG(_EAX, SymbolTable[ResultId]);
    end
    else if GetSymbolRec(T).Size <= 8 then
    begin
      GetReg(_EAX);
      GetReg(_EDX);
      GetReg(_ECX);

      EmitLoadAddress(_ECX, SymbolTable[ResultId]);
      Prg.AsmMovREG32_REGPtr(_EAX, _ECX);
      Prg.AsmAddREG_Imm(_ECX, 4);
      Prg.AsmMovREG32_REGPtr(_EDX, _ECX);

      FreeReg(_EAX);
      FreeReg(_EDX);
      FreeReg(_ECX);
    end;
  end
  else if TypeID in (OrdinalTypes +
                [
{$IFNDEF PAXARM}
                typeANSISTRING, typeWIDESTRING,
{$ENDIF}
                typeUNICSTRING, typeVARIANT,
                typeOLEVARIANT,
                typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
                typeINTERFACE]) then
  begin
    EmitGet_REG(_EAX, SymbolTable[ResultId]);
  end
  else if TypeID = typeSET then
  begin
    if not SymbolTable[SubId].ExtraParamNeeded then
      EmitGet_REG(_EAX, SymbolTable[ResultId]);
  end
  else if TypeID = typeDOUBLE then
  begin
    Reg := GetReg;
    Prg.AsmMovREG_REG(Reg, _EBP);
    Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[ResultId]));
    Prg.AsmFldDouble_REGPtr(Reg);
    FreeReg(Reg);
  end
  else if TypeID = typeSINGLE then
  begin
    Reg := GetReg;
    Prg.AsmMovREG_REG(Reg, _EBP);
    Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[ResultId]));
    Prg.AsmFldSingle_REGPtr(Reg);
    FreeReg(Reg);
  end
  else if TypeID = typeEXTENDED then
  begin
    Reg := GetReg;
    Prg.AsmMovREG_REG(Reg, _EBP);
    Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[ResultId]));
    Prg.AsmFldExtended_REGPtr(Reg);
    FreeReg(Reg);
  end
  else if TypeID = typeCURRENCY then
  begin
    Reg := GetReg;
    Prg.AsmMovREG_REG(Reg, _EBP);
    Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[ResultId]));
    Prg.AsmFild_REG64Ptr(Reg);
    FreeReg(Reg);
  end;

  case TargetPlatform of
    tpOSX32, tpIOSSim:
    begin
      Prg.AsmMovREG_REG(_ESP, _EBP);
      Prg.AsmPop_REG(_EBX);
      Prg.AsmPop_REG(_EDI);
      Prg.AsmPop_REG(_ESI);
      Prg.AsmPop_REG(_EBP);
    end;
    tpWIN64:
    begin
    //  Prg.AsmLeaRSP_RBPPtr(TKernel(kernel).GetSubRSPSize(SubId));
      Prg.AsmAddREG_Imm(_ESP, SymbolTable.GetSubRSPSize(SubId));

      Prg.AsmPop_REG(_EBX);
      Prg.AsmPop_REG(_EDI);
      Prg.AsmPop_REG(_ESI);
      Prg.AsmPop_REG(_EBP);
    end;
    else
    begin
      Prg.AsmMovREG_REG(_ESP, _EBP);
      Prg.AsmPop_REG(_EBX);
      Prg.AsmPop_REG(_EDI);
      Prg.AsmPop_REG(_ESI);
      Prg.AsmPop_REG(_EBP);
    end;
  end;

  if SymbolTable[SubId].CallConv = ccCDECL then
    Prg.AsmRet(0)
  else
  begin
    if TargetPlatform = tpWIN64 then
      Prg.AsmRet(0)
    else
      Prg.AsmRet(SymbolTable.GetSizeOfParams(R.Arg1));
  end;

  ContextStack.Pop;

  EmitFinSub(SubId);
end;

procedure TEmitter.EmitOP_PUSH_EBP_64;
begin
  EmitPCodeOperator;
  Prg.AsmMovREG_REG(_ECX, _EBP);
end;

procedure TEmitter.EmitOP_PUSH_EBP;
var
  SubId, CurrSubId, Height, CurrHeight, Reg, I, D: Integer;
begin
  EmitPCodeOperator;

  if ContextStack.Count >= 2 then
  begin
    SubId := SymbolRecR.Id;
    EmitRestoreEBP(_EAX, GetSymbolRec(SubId));
  end
  else
    Prg.AsmMovREG_REG(_EAX, _EBP);
end;

procedure TEmitter.EmitOP_POP_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_POP;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_SAVE_REGS_64;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;
end;
procedure TEmitter.EmitOP_SAVE_REGS;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;
end;

procedure TEmitter.EmitOP_RESTORE_REGS_64;
begin
  EmitPCodeOperator;
  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_RESTORE_REGS;
begin
  EmitPCodeOperator;
  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_RET_64;
begin
  EmitPCodeOperator;

  Prg.AsmRet;
end;
procedure TEmitter.EmitOP_RET;
begin
  EmitPCodeOperator;

  Prg.AsmRet;
end;

procedure TEmitter.EmitOP_FIELD_64;
var
  Reg: Integer;
  PatternId: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1); // load address of record

  PatternId := SymbolRec2.PatternId; // find id of pattern field

  if SymbolRec1.FinalTypeId = typeCLASS then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[PatternId]));

  EmitPut_REG(Reg, SymbolRec2);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_FIELD;
var
  Reg: Integer;
  PatternId: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1); // load address of record

  PatternId := SymbolRec2.PatternId; // find id of pattern field

  if SymbolRec1.FinalTypeId = typeCLASS then
    Prg.AsmMovREG_REGPtr(Reg, Reg);

  Prg.AsmAddREG_Imm(Reg, GetOffset(SymbolTable[PatternId]));

  EmitPut_REG(Reg, SymbolRec2);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GET_COMPONENT_64;
var
  Reg: Integer;
  SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_GetComponent;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadIntVal(Reg, SymbolRec1); // the first parameter
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(_EDX, R.Arg2);  // the second parameter

  EmitLoadAddress(Reg, SymbolRecR); // the third parameter
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_COMPONENT;
var
  Reg: Integer;
  SubId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_GetComponent;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // the third parameter
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(R.Arg2); // the second parameter

  EmitLoadIntVal(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ELEM_64;
begin
  EmitOP_ELEM;
end;
procedure TEmitter.EmitOP_ELEM;

  function LeftPartOfAssignment: Boolean;
  var
    I, N, Res, Op: Integer;
    Code: TCode;
  begin
    result := false;
    Code := TKernel(kernel).Code;
    N := Code.N;
    Res := Code[N].Res;
    I := N;
    repeat
      Inc(I);
      if I > Code.Card then
        Exit;
      Op := Code[I].GenOp;
      if Op = OP_STMT then
        Exit;
      if Op = OP_ASSIGN then
        if Code[I].Arg1 = Res then
        begin
          result := true;
          Exit;
        end;
    until false;
  end;

var
  Reg, RegIndex: Integer;
  ArrayTypeId, RangeTypeId, ElemTypeId, H1, ElSize: Integer;
  IsFWArray: Boolean;
begin
  EmitPCodeOperator;

  ElemTypeId := SymbolRec2.FinalTypeId;
  if not (ElemTypeId in OrdinalTypes + INT64Types) then
    TKernel(kernel).CreateError(errIncompatibleTypesNoArgs, []);
{$IFNDEF PAXARM}
  if SymbolRec1.HasPAnsiCharType then // pchar
  begin
    RegIndex := GetReg;
    Reg := GetReg;

    if SymbolRec2.Kind = KindCONST then
      Prg.AsmMovReg_Imm(RegIndex, SymbolRec2.Value)
    else
      EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
    Prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
  else
{$ENDIF}
  if SymbolRec1.HasPWideCharType then // pchar
  begin
    RegIndex := GetReg;
    Reg := GetReg;

    if SymbolRec2.Kind = KindCONST then
      Prg.AsmMovReg_Imm(RegIndex, SymbolRec2.Value)
    else
      EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
    Prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
{$IFNDEF PAXARM}
  else if SymbolRec1.FinalTypeId = typeANSISTRING then
  begin
    if LeftPartOfAssignment then
    begin

      SaveRegisters([_ESI, _EDI]);

      EmitCallPro(Id_UniqueAnsiString);

      Reg := GetReg;
      EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(Id_UniqueAnsiString);

      RestoreRegisters([_ESI, _EDI]);
    end;

    RegIndex := GetReg;
    Reg := GetReg;

    if SymbolRec2.Kind = KindCONST then
    begin
      Prg.AsmMovReg_Imm(RegIndex, SymbolRec2.Value - 1);
    end
    else
    begin
      EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>
      Prg.AsmAddReg_Imm(RegIndex, -1);
    end;

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
    Prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
{$ENDIF}
  else if SymbolRec1.FinalTypeId in [{$IFNDEF PAXARM}typeWIDESTRING,{$ENDIF} typeUNICSTRING] then
  begin
    if SymbolRec1.FinalTypeId = typeUNICSTRING then
    if LeftPartOfAssignment then
    begin
      SaveRegisters([_ESI, _EDI]);

      Reg := GetReg;

      EmitCallPro(Id_UniqueUnicString);

      EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(Id_UniqueUnicString);

      RestoreRegisters([_ESI, _EDI]);
    end;

    RegIndex := GetReg;
    Reg := GetReg;

    if SymbolRec2.Kind = KindCONST then
    begin
      Prg.AsmMovReg_Imm(RegIndex, (SymbolRec2.Value - 1) * SizeOf(WideChar));
    end
    else
    begin
      EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>
      Prg.AsmAddReg_Imm(RegIndex, -1);
      Prg.AsmAddREG_REG(RegIndex, RegIndex);
    end;

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
    Prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
{$IFNDEF PAXARM}
  else if SymbolRec1.FinalTypeId = typeSHORTSTRING then
  begin
    RegIndex := GetReg;
    Reg := GetReg;

    EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>
    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of string>
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
{$ENDIF}
  else if (SymbolRec1.FinalTypeId = typeDYNARRAY) or // dynamic array
          (SymbolRec1.FinalTypeId = typeOPENARRAY) or
          (SymbolRec1.IsFWArrayVar) then
  begin
    IsFWArray := SymbolRec1.IsFWArrayVar;

    ArrayTypeId := SymbolRec1.TerminalTypeId;
    ElemTypeId := GetSymbolRec(ArrayTypeId).PatternId;

    if IsFWArray then
      ElemTypeId := GetSymbolRec(ElemTypeId).PatternId;

    ElSize := SymbolTable[ElemTypeId].Size;

  // emit

    RegIndex := _EAX;
    GetReg(RegIndex);
    Reg := GetReg;

    EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>
    Prg.AsmMovREG_Imm(Reg, ElSize);       // Reg := <size of element>
    Prg.AsmMulREG(Reg);                   // RegIndex := RegIndex * Reg

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of array>

    if IsFWArray then
    begin
      Prg.AsmMovREG_REGPtr(Reg, Reg);
      Prg.AsmAddReg_Imm(Reg, FWArrayOffset);
    end;

    Prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end
  else // static array
  begin
    ArrayTypeId := SymbolRec1.TerminalTypeId;
    SymbolTable.GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
    H1 := SymbolTable.GetLowBoundRec(RangeTypeId).Value;
    ElSize := SymbolTable[ElemTypeId].Size;

  // emit

    RegIndex := _EAX;
    GetReg(RegIndex);
    Reg := GetReg;

    EmitLoadIntVal(RegIndex, SymbolRec2); // RegIndex := <index value>
    Prg.AsmAddREG_Imm(RegIndex, - H1);    // RegIndex := RegIndex - H1
    Prg.AsmMovREG_Imm(Reg, ElSize);       // Reg := <size of element>
    Prg.AsmMulREG(Reg);                   // RegIndex := RegIndex * Reg

    EmitLoadAddress(Reg, SymbolRec1);     // Reg := <address of array>
    Prg.AsmAddREG_REG(Reg, RegIndex);     // Reg := Reg + RegIndex
    EmitPut_REG(Reg, SymbolTable[R.Res]); // save address

    FreeReg(Reg);
    FreeReg(RegIndex);
  end;
end;

procedure TEmitter.EmitOP_INT_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmCDQ;
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_INT_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmCDQ;
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_BYTE_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_BYTE_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_WORD_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_WORD_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_CARDINAL_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_CARDINAL_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SMALLINT_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_SMALLINT_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SHORTINT_TO_INT64_64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_SHORTINT_TO_INT64;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  EmitLoadAddress(_ECX, SymbolRecR);

  GetReg(_EAX);
  GetReg(_EDX);

  EmitLoadIntVal(_EAX, SymbolRec1);
  Prg.AsmXorREG_REG(_EDX, _EDX);
  Prg.AsmMovREGPtr_REG32(_ECX, _EAX);
  Prg.AsmAddREG_Imm(_ECX, 4);
  Prg.AsmMovREGPtr_REG32(_ECX, _EDX);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_INT_TO_DOUBLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT_TO_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INT64_TO_DOUBLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT64_TO_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INT_TO_SINGLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT_TO_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INT64_TO_SINGLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT64_TO_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INT_TO_EXTENDED_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT_TO_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INT64_TO_EXTENDED_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT64_TO_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFild_REG64Ptr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_DOUBLE_TO_SINGLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmCvtsd2ssXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_DOUBLE_TO_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldDouble_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_DOUBLE_TO_EXTENDED_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmMovsdXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_DOUBLE_TO_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldDouble_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_EXTENDED_TO_DOUBLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmMovsdXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_EXTENDED_TO_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldExtended_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_EXTENDED_TO_SINGLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmCvtsd2ssXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_EXTENDED_TO_SINGLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldExtended_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpSingle_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_SINGLE_TO_DOUBLE_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmCvtss2sdXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_SINGLE_TO_DOUBLE;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldSingle_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpDouble_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitFDiv_10000;
begin
  Prg.AsmFDiv_ESIPtr32(GetOffset(GetSymbolRec(CURR_FMUL_Id)));
end;

procedure TEmitter.EmitFMul_10000;
begin
  Prg.AsmFMul_ESIPtr32(GetOffset(GetSymbolRec(CURR_FMUL_Id)));
end;

procedure TEmitter.EmitOP_CURRENCY_TO_DOUBLE_64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_TO_DOUBLE;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;

procedure TEmitter.EmitOP_CURRENCY_TO_SINGLE_64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_TO_SINGLE;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;

procedure TEmitter.EmitOP_CURRENCY_TO_EXTENDED_64;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;
procedure TEmitter.EmitOP_CURRENCY_TO_EXTENDED;
begin
  EmitPCodeOperator;

  EmitFild(SymbolRec1);
  EmitFDiv_10000;
  EmitFstp(SymbolRecR);
end;

procedure TEmitter.EmitOP_SINGLE_TO_EXTENDED_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmCvtss2sdXMM_RegPtr(_XMM4, Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovsdRegPtr_XMM(_XMM4, Reg);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_SINGLE_TO_EXTENDED;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmFldSingle_REGPtr(Reg);
  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmFstpExtended_REGPtr(Reg);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ADDRESS_PROG_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_ADDRESS_PROG;
var
  Reg: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;
  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_ASSIGN_PROG_64;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;
  Reg := GetRegEx;

  SubId := JS_AssignProgId;
  EmitCallPro(SubId);

  EmitLoadIntVal(Reg, SymbolRecR);
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ASSIGN_PROG;
var
  Reg, SubId: Integer;
begin
  EmitPCodeOperator;
  Reg := GetReg;

  SubId := JS_AssignProgId;
  EmitCallPro(SubId);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg);
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRecR);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ADDRESS_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  if SymbolRec1.Kind in KindSUBS then
  begin

    if Host1 then
      EmitGet_REG(Reg, SymbolRec1)
    else
    begin
      Prg.AsmMovREG_REG(Reg, _EDI);
      Prg.AsmAddREG_Imm(Reg, 0);
      Prg.Top.SaveSubId := R.Arg1;
      List2.Add(Prg.Top);
    end;
    EmitSaveIntVal(Reg, SymbolRecR);

  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1);
    EmitSaveIntVal(Reg, SymbolRecR);
  end;

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_ADDRESS;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  if SymbolRec1.Kind in KindSUBS then
  begin

    if Host1 then
      EmitGet_REG(Reg, SymbolRec1)
    else
    begin
      Prg.AsmMovREG_REG(Reg, _EDI);
      Prg.AsmAddREG_Imm(Reg, 0);
      Prg.Top.SaveSubId := R.Arg1;
      List2.Add(Prg.Top);
    end;
    EmitSaveIntVal(Reg, SymbolRecR);

  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1);
    EmitSaveIntVal(Reg, SymbolRecR);
  end;

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INIT_PCHAR_LITERAL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmAddREG_Imm(Reg, 12);
  EmitSaveIntVal(Reg, SymbolRec1);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_INIT_PWIDECHAR_LITERAL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1);
  Prg.AsmAddREG_Imm(Reg, 8);
  EmitSaveIntVal(Reg, SymbolRec1);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_TERMINAL_64;
var
  Reg, temp: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  SymbolRecR.ByRef := false;
  temp := SymbolRecR.TypeID;
  SymbolRecR.TypeID := TypePOINTER;
  EmitSaveIntVal(Reg, SymbolRecR);
  SymbolRecR.ByRef := true;
  SymbolRecR.TypeID := temp;

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_TERMINAL;
var
  Reg, temp: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1);
  SymbolRecR.ByRef := false;
  temp := SymbolRecR.TypeID;
  SymbolRecR.TypeID := TypePOINTER;
  EmitSaveIntVal(Reg, SymbolRecR);
  SymbolRecR.ByRef := true;
  SymbolRecR.TypeID := temp;

  FreeReg(Reg);
end;

procedure TEmitter.CheckSetElement(S: TSymbolRec);
var
  I: Integer;
begin
  if S.Kind = KindCONST then
  if not IsVarObject(S.Value) then
  begin
    I := S.Value;
    if (I < 0) or (I > 255) then
      CreateError(errInvalidSet, []);
  end;
end;

procedure TEmitter.EmitOP_SET_INCLUDE_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  CheckSetElement(SymbolRec1);

  SubId := Id_SetInclude;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_INCLUDE;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  CheckSetElement(SymbolRec2);

  SubId := Id_SetInclude;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_INCLUDE_INTERVAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  CheckSetElement(SymbolRec1);
  CheckSetElement(SymbolRec2);

  SubId := Id_SetIncludeInterval;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // the second parameter
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRecR); // the third parameter
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_INCLUDE_INTERVAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  CheckSetElement(SymbolRec1);
  CheckSetElement(SymbolRec2);

  SubId := Id_SetIncludeInterval;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRecR); // the third parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_EXCLUDE_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  CheckSetElement(SymbolRec1);

  SubId := Id_SetExclude;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_EXCLUDE;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  CheckSetElement(SymbolRec2);

  SubId := Id_SetExclude;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_UNION_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetUnion;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // 3 - par
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_UNION;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetUnion;
  EmitCallPro(SubId);

  Reg := GetReg;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRecR); // the third parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_DIFFERENCE_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetDifference;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // 3 - par
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_DIFFERENCE;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetDifference;
  EmitCallPro(SubId);

  Reg := GetReg;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRecR); // the third parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_INTERSECTION_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetIntersection;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // 3 - par
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_INTERSECTION;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetIntersection;
  EmitCallPro(SubId);

  Reg := GetReg;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRecR); // the third parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_SUBSET_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetSubset;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;
procedure TEmitter.EmitOP_SET_SUBSET;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetSubset;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;

procedure TEmitter.EmitOP_SET_SUPERSET_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetSuperset;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
  EmitPut_REG(_EAX, SymbolRecR);
end;
procedure TEmitter.EmitOP_SET_SUPERSET;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetSuperset;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
  EmitPut_REG(_EAX, SymbolRecR);
end;

procedure TEmitter.EmitOP_SET_EQUALITY_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetEquality;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;
procedure TEmitter.EmitOP_SET_EQUALITY;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetEquality;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;

procedure TEmitter.EmitOP_SET_INEQUALITY_64;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetInequality;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R8, Reg);

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmMovREG_Imm(Reg, SZ);
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;
procedure TEmitter.EmitOP_SET_INEQUALITY;
var
  SubId, Reg, T, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetInequality;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  T := SymbolRec2.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  T := SymbolRec1.TerminalTypeId;
  SZ := SymbolTable.GetSizeOfSetType(T);
  Prg.AsmPush_Imm(SZ);

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;

procedure TEmitter.EmitOP_SET_MEMBERSHIP_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetMembership;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadIntVal(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // 2 - par
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;
procedure TEmitter.EmitOP_SET_MEMBERSHIP;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_SetMembership;
  EmitCallPro(SubId);

  GetReg(_EAX);
  Reg := _EAX;

  EmitLoadAddress(Reg, SymbolRec2); // the second parameter
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // the first parameter
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);

  EmitPut_REG(_EAX, SymbolRecR);
end;

procedure TEmitter.EmitOP_SET_ASSIGN_64;
var
  Reg1, Reg2, TypeId, SetSize: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg1 := GetReg;
  Reg2 := GetReg;

  TypeId := SymbolRec1.TerminalTypeId;
  SetSize := TKernel(kernel).SymbolTable.GetSizeOfSetType(TypeId);

  EmitLoadAddress(Reg1, SymbolRec1);
  EmitLoadAddress(Reg2, SymbolRec2);

  Prg.AsmPush_Reg(_ESI);
  Prg.AsmPush_Reg(_EDI);

  Prg.AsmMovREG_REG(_ESI, Reg2);
  Prg.AsmMovREG_REG(_EDI, Reg1);

  Prg.AsmMovREG_Imm(_ECX, SetSize);
  Prg.AsmRep_MOVSB;

  Prg.AsmPop_Reg(_EDI);
  Prg.AsmPop_Reg(_ESI);

  FreeReg(Reg2);
  FreeReg(Reg1);
  FreeReg(_ECX);
end;
procedure TEmitter.EmitOP_SET_ASSIGN;
var
  Reg1, Reg2, TypeId, SetSize: Integer;
begin
  EmitPCodeOperator;

  GetReg(_ECX);
  Reg1 := GetReg;
  Reg2 := GetReg;

  TypeId := SymbolRec1.TerminalTypeId;
  SetSize := TKernel(kernel).SymbolTable.GetSizeOfSetType(TypeId);

  EmitLoadAddress(Reg1, SymbolRec1);
  EmitLoadAddress(Reg2, SymbolRec2);

  SaveRegisters([_ESI, _EDI]);

  Prg.AsmMovREG_REG(_ESI, Reg2);
  Prg.AsmMovREG_REG(_EDI, Reg1);

  Prg.AsmMovREG_Imm(_ECX, SetSize);
  Prg.AsmRep_MOVSB;

  RestoreRegisters([_ESI, _EDI]);

  FreeReg(Reg2);
  FreeReg(Reg1);
  FreeReg(_ECX);
end;

procedure TEmitter.EmitOP_SET_COUNTER_ASSIGN_64;
var
  Reg, TypeId, SetSize: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  TypeId := SymbolRec2.TerminalTypeId;
  SetSize := TKernel(kernel).SymbolTable.GetSizeOfSetType(TypeId);

  Prg.AsmMovREG_Imm(Reg, SetSize * 8);
  EmitSaveIntVal(Reg, SymbolRec1);

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_SET_COUNTER_ASSIGN;
var
  Reg, TypeId, SetSize: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;

  TypeId := SymbolRec2.TerminalTypeId;
  SetSize := TKernel(kernel).SymbolTable.GetSizeOfSetType(TypeId);

  Prg.AsmMovREG_Imm(Reg, SetSize * 8);
  EmitSaveIntVal(Reg, SymbolRec1);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_CREATE_METHOD_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CreateMethod;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadIntVal(Reg, SymbolRec1); // 1 - par - data
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // 2 - par - code
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_CREATE_METHOD;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CreateMethod;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // code
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // data
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);

  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_TO_JS_OBJECT_64;
var
  SubId, Reg, FinTypeId: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := JS_ToObjectId;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmMovREG_REG(_EDX, Reg);

  if (SymbolRec1.Kind = kindCONST) and
     (SymbolRec1.FinalTypeId in IntegerTypes) then
  begin
{
    Id := SymbolRec1.Value;
    Id := TKernel(kernel).SymbolTable.AddInt64Const(Id).Id;

    Prg.AsmPush_Imm(typeINT64);
    EmitLoadAddress(Reg, GetSymbolRec(Id)); // source
}
    RaiseError(errInternalError, []);
  end
  else
  begin
    FinTypeId := GetSymbolRec(R.Arg1).FinalTypeId;
    Prg.AsmMovREG_Imm(Reg, FinTypeId);
    Prg.AsmMovREG_REG(_R8, Reg);
  end;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_TO_JS_OBJECT;
var
  SubId, Reg, FinTypeId: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := JS_ToObjectId;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  if (SymbolRec1.Kind = kindCONST) and
     (SymbolRec1.FinalTypeId in IntegerTypes) then
  begin
{
    Id := SymbolRec1.Value;
    Id := TKernel(kernel).SymbolTable.AddInt64Const(Id).Id;

    Prg.AsmPush_Imm(typeINT64);
    EmitLoadAddress(Reg, GetSymbolRec(Id)); // source
}
    RaiseError(errInternalError, []);
  end
  else
  begin
    FinTypeId := GetSymbolRec(R.Arg1).FinalTypeId;
    Prg.AsmPush_Imm(FinTypeId);

    EmitLoadAddress(Reg, SymbolRec1); // source
  end;

  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_GET_NEXTJSPROP_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(JS_GetNextPropId);
end;
procedure TEmitter.EmitOP_GET_NEXTJSPROP;
begin
  EmitStdCall_Adr1_Adr2_AdrR(JS_GetNextPropId);
end;

procedure TEmitter.EmitOP_CLEAR_REFERENCES_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_ClearReferencesId;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);   // push TProgram.Self

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_CLEAR_REFERENCES;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_ClearReferencesId;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);           // push TProgram.Self

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_JS_TYPEOF_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_TypeOfId;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // 1 - par
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // 2 - result
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_JS_TYPEOF;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_TypeOfId;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // 1st arg - object
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_JS_VOID_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_VoidId;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // 1st arg - object
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_JS_VOID;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_VoidId;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // 1st arg - object
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

{$IFNDEF PAXARM}
procedure TEmitter.EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL_64;
var
  T1, L, L1, L2: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;
  L2 := Length(SymbolRec2.Value);

  if L2 < L1 then
    L := L2
  else
    L := L1;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  Prg.AsmMovREG_Imm(_ECX, L);
  EmitLoadAddress(_EAX, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(_EAX, _ECX); // s[0] := length
  EmitGet_REG(_EDX, SymbolRec2); // pchar source

  Prg.AsmPush_REG(_ESI);
  Prg.AsmPush_REG(_EDI);

  Prg.AsmMovREG_REG(_ESI, _EDX);
  Prg.AsmAddREG_Imm(_EAX, 1);
  Prg.AsmMovREG_REG(_EDI, _EAX);
  Prg.AsmRep_MOVSB;

  Prg.AsmPop_REG(_EDI);
  Prg.AsmPop_REG(_ESI);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_PANSICHAR_LITERAL;
var
  T1, L, L1, L2: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;
  L2 := Length(SymbolRec2.Value);

  if L2 < L1 then
    L := L2
  else
    L := L1;

  GetReg(_EAX);
  GetReg(_EDX);
  GetReg(_ECX);

  Prg.AsmMovREG_Imm(_ECX, L);
  EmitLoadAddress(_EAX, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(_EAX, _ECX); // s[0] := length
  EmitGet_REG(_EDX, SymbolRec2); // pchar source

  SaveRegisters([_ESI, _EDI]);

  Prg.AsmMovREG_REG(_ESI, _EDX);
  Prg.AsmAddREG_Imm(_EAX, 1);
  Prg.AsmMovREG_REG(_EDI, _EAX);
  Prg.AsmRep_MOVSB;

  RestoreRegisters([_ESI, _EDI]);

  FreeReg(_EAX);
  FreeReg(_EDX);
  FreeReg(_ECX);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL_64;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromPWideChar;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

//

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Prg.AsmMovREG_Imm(Reg, L1);
  Prg.AsmMovREG_REG(_EDX, Reg);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitGet_REG(Reg, SymbolRec2); // pwidechar source
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_PWIDECHAR_LITERAL;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromPWideChar;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Reg := GetReg;
  EmitGet_REG(Reg, SymbolRec2); // pwidechar source
  Prg.AsmPush_REG(Reg);
  Prg.AsmPush_Imm(L1);
  Prg.AsmPush_Imm(L1);
  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
{$ENDIF}

procedure TEmitter.EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_PANSICHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_PANSICHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_PWIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_PWIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_VARIANT_FROM_PANSICHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_VARIANT_FROM_PANSICHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_VARIANT_FROM_PWIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_PANSICHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_PWIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // variant dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

{$IFNDEF PAXARM}
procedure TEmitter.EmitOP_SHORTSTRING_FROM_ANSISTRING_64;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromAnsiString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, L1);
  Prg.AsmMovREG_REG(_EDX, Reg);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_ANSISTRING;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromAnsiString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmPush_REG(Reg);
  Prg.AsmPush_Imm(L1);
  Prg.AsmPush_Imm(L1);
  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_SHORTSTRING_FROM_WIDESTRING_64;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromWideString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, L1);
  Prg.AsmMovREG_REG(_EDX, Reg);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_WIDESTRING;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromWideString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmPush_REG(Reg);
  Prg.AsmPush_Imm(L1);
  Prg.AsmPush_Imm(L1);
  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_SHORTSTRING_FROM_UNICSTRING_64;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromUnicString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, L1);
  Prg.AsmMovREG_REG(_EDX, Reg);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_UNICSTRING;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromUnicString;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmPush_REG(Reg);
  Prg.AsmPush_Imm(L1);
  Prg.AsmPush_Imm(L1);
  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
{$ENDIF}
procedure TEmitter.EmitOP_ANSISTRING_FROM_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromShortString);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_SHORTSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromShortString);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDESTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromWideString);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDESTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromWideString);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromShortString);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_SHORTSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromShortString);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_UNICSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromUnicString);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_UNICSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromUnicString);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromShortString);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_SHORTSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromShortString);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_WIDESTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromWideString);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_WIDESTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromWideString);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_UNICSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromUnicString);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_UNICSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_AnsiStringFromUnicString);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_ANSISTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromAnsiString);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_ANSISTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_WideStringFromAnsiString);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_ANSISTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromAnsiString);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_ANSISTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_UnicStringFromAnsiString);
end;

procedure TEmitter.EmitOP_INTERFACE_CAST_64;
var
  SubId, Reg: Integer;
  GuidId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_InterfaceCast;
  EmitCallPro(SubId);

  GuidId := SymbolRec2.TerminalTypeId + 1;

  EmitPCodeOperator;

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRecR); // interface dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, GetSymbolRec(GuidId));
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // interface source
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_INTERFACE_CAST;
var
  SubId, Reg: Integer;
  GuidId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_InterfaceCast;

  GuidId := SymbolRec2.TerminalTypeId + 1;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1); // interface source
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, GetSymbolRec(GuidId));
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRecR); // interface dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_INTERFACE_FROM_CLASS_64;
var
  SubId, Reg: Integer;
  GuidId: Integer;
begin
  SubId := Id_InterfaceFromClass;

  GuidId := SymbolRec1.TerminalTypeId + 1;

  EmitPCodeOperator;

  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // interface dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, GetSymbolRec(GuidId));
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // object
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_INTERFACE_FROM_CLASS;
var
  SubId, Reg: Integer;
  GuidId: Integer;
begin
  SubId := Id_InterfaceFromClass;

  GuidId := SymbolRec1.TerminalTypeId + 1;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec2); // object
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, GetSymbolRec(GuidId));
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // interface dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ASSIGN_INTERFACE_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_InterfaceAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_INTERFACE;
begin
  EmitStdCall_Adr1_from_Adr2(Id_InterfaceAssign);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_CLASS_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromClass);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_CLASS;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromClass);
end;

procedure TEmitter.EmitOP_CLASS_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ClassFromVariant);
end;
procedure TEmitter.EmitOP_CLASS_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ClassFromVariant);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_POINTER_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromPointer);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_POINTER;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromPointer);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_ANSISTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromAnsiString);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_ANSISTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromAnsiString);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_ANSISTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromAnsiString);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_ANSISTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromAnsiString);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_VARIANT_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromVariant);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_VARIANT;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromVariant);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_WIDESTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromWideString);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_WIDESTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromWideString);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_UNICSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromUnicString);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_UNICSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromUnicString);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDESTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromWideString);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDESTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromWideString);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_UNICSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromUnicString);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_UNICSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromUnicString);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromShortString);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_SHORTSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromShortString);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromShortString);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_SHORTSTRING;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromShortString);
end;

procedure TEmitter.EmitOP_SHORTSTRING_FROM_ANSICHAR_64;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  Prg.AsmMovREG_Imm(Reg2, 1);
  EmitLoadAddress(Reg1, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2); // s[0] := length
  EmitGet_REG(Reg2, SymbolRec2); // char

  Prg.AsmAddREG_Imm(Reg1, 1);
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2);

  FreeReg(Reg1);
  FreeReg(Reg2);
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_ANSICHAR;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  Prg.AsmMovREG_Imm(Reg2, 1);
  EmitLoadAddress(Reg1, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2); // s[0] := length
  EmitGet_REG(Reg2, SymbolRec2); // char

  Prg.AsmAddREG_Imm(Reg1, 1);
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2);

  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_SHORTSTRING_FROM_WIDECHAR_64;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  Prg.AsmMovREG_Imm(Reg2, 1);
  EmitLoadAddress(Reg1, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2); // s[0] := length
  EmitGet_REG(Reg2, SymbolRec2); // char

  Prg.AsmAddREG_Imm(Reg1, 1);
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2);

  FreeReg(Reg1);
  FreeReg(Reg2);
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_WIDECHAR;
var
  Reg1, Reg2: Integer;
begin
  EmitPCodeOperator;

  Reg1 := GetReg;
  Reg2 := GetReg;

  Prg.AsmMovREG_Imm(Reg2, 1);
  EmitLoadAddress(Reg1, SymbolRec1); // string dest
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2); // s[0] := length
  EmitGet_REG(Reg2, SymbolRec2); // char

  Prg.AsmAddREG_Imm(Reg1, 1);
  Prg.AsmMovREGPtr_REG8(Reg1, Reg2);

  FreeReg(Reg1);
  FreeReg(Reg2);
end;

procedure TEmitter.EmitOP_BEGIN_CRT_JS_FUNC_OBJECT_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_BEGIN_CRT_JS_FUNC_OBJECT;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_END_CRT_JS_FUNC_OBJECT_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_END_CRT_JS_FUNC_OBJECT;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_LOAD_PROC_64;
var
  SubId, Reg, SubShift, OverCount: Integer;
begin
  EmitPCodeOperator;

  Prg.AsmSubReg_Imm(_ESP, $100);

  Emit_PUSH_REGS;

  SubId := Id_LoadProc;
  EmitCallPro(SubId);

  SymbolRec1.Host := true;

  SubShift := GetOffset(SymbolRec1);
  OverCount := SymbolRec1.OverCount;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, SubShift);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitGet_REG(Reg, SymbolRec2); // proc name (pchar const)
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitGet_REG(Reg, SymbolRecR); // dll name (pchar const)
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_Imm(Reg, OverCount);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;

  Prg.AsmAddReg_Imm(_ESP, $100);

end;
procedure TEmitter.EmitOP_LOAD_PROC;
var
  SubId, Reg, SubShift, OverCount: Integer;
begin
  EmitPCodeOperator;

  Emit_PUSH_REGS;

  SubId := Id_LoadProc;
  EmitCallPro(SubId);

  SymbolRec1.Host := true;

  SubShift := GetOffset(SymbolRec1);
  OverCount := SymbolRec1.OverCount;

  Reg := GetRegEx;

  Prg.AsmPush_Imm(OverCount);

  EmitGet_REG(Reg, SymbolRecR); // dll name (pchar const)
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // proc name (pchar const)
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SubShift);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ADD_MESSAGE_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AddMessage;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // message id
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitGet_REG(Reg, SymbolRecR); // FullName (pchar const)
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ADD_MESSAGE;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AddMessage;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitGet_REG(Reg, SymbolRecR); // FullName (pchar const)
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // message id
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_VAR_FROM_TVALUE_64;
var
  SubId, Reg, T: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VarFromTValue;
  EmitCallPro(SubId);

  if SubId <= 0 then
    RaiseError(errInternalError, []);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmMovREG_REG(_ECX, Reg);

  T := SymbolRec1.FinalTypeId;
  Prg.AsmMovREG_Imm(Reg, T);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_VAR_FROM_TVALUE;
var
  SubId, Reg, T: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VarFromTValue;
  EmitCallPro(SubId);

  if SubId <= 0 then
    RaiseError(errInternalError, []);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmPush_REG(Reg);

  T := SymbolRec1.FinalTypeId;
  Prg.AsmPush_Imm(T);

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_PANSICHAR_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_AnsiStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_PANSICHAR;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_AnsiStringFromPAnsiChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_PWIDECHAR_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_AnsiStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_PWIDECHAR;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_AnsiStringFromPWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ERR_ABSTRACT_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ErrAbstract;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec1); // pchar source
  Prg.AsmMovREG_REG(_ECX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ERR_ABSTRACT;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ErrAbstract;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitGet_REG(Reg, SymbolRec1); // pchar source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_ANSICHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_AnsiStringFromAnsiChar);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_ANSICHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_AnsiStringFromAnsiChar);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_ANSICHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_WideStringFromAnsiChar);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_ANSICHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_WideStringFromAnsiChar);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_ANSICHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_UnicStringFromAnsiChar);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_ANSICHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_UnicStringFromAnsiChar);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_ANSICHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromAnsiChar);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_ANSICHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromAnsiChar);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_ANSICHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromAnsiChar);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_ANSICHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromAnsiChar);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_INT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromInt);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_INT;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromInt);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_INT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromInt);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_INT;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromInt);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_INTERFACE_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromInterface);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_INTERFACE;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromInterface);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_INTERFACE_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromInterface);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_INTERFACE;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromInterface);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_INT64_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromInt64);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_INT64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromInt64);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_INT64_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromInt64);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_INT64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromInt64);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_BYTE_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromByte);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_BYTE;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromByte);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_BYTE_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromByte);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_BYTE;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromByte);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_BOOL_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromBool);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_BOOL;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromBool);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_BOOL_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromBool);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_BOOL;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromBool);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_WORD_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromWord);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_WORD;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromWord);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_WORD_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromWord);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_WORD;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromWord);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_CARDINAL_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromCardinal);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_CARDINAL;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromCardinal);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_CARDINAL_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromCardinal);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_CARDINAL;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromCardinal);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_SMALLINT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromSmallInt);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_SMALLINT;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromSmallInt);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_SMALLINT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromSmallInt);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_SMALLINT;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromSmallInt);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_SHORTINT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromShortInt);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_SHORTINT;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromShortInt);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_SHORTINT_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromShortInt);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_SHORTINT;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromShortInt);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_DOUBLE_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromDouble);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_DOUBLE;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromDouble);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_DOUBLE_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromDouble);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_DOUBLE;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromDouble);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_CURRENCY_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromCurrency);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_CURRENCY;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromCurrency);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_CURRENCY_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromCurrency);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_CURRENCY;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromCurrency);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_SINGLE_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromSingle);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_SINGLE;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromSingle);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_SINGLE_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromSingle);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_SINGLE;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromSingle);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_EXTENDED_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromExtended);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_EXTENDED;
begin
  EmitStdCall_Adr1_from_Adr2(Id_VariantFromExtended);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_EXTENDED_64;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromExtended);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_EXTENDED;
begin
  EmitStdCall_Adr1_from_Adr2(Id_OleVariantFromExtended);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_INT_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromInt);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_INT; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromInt);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_DOUBLE_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromDouble);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_DOUBLE; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromDouble);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_SINGLE_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromSingle);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_SINGLE; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromSingle);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_EXTENDED_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromExtended);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_EXTENDED; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromExtended);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_BOOLEAN_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromBoolean);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_BOOLEAN; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromBoolean);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_INT_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromInt);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_INT; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromInt);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_DOUBLE_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromDouble);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_DOUBLE; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromDouble);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_SINGLE_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromSingle);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_SINGLE; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromSingle);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_EXTENDED_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromExtended);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_EXTENDED; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromExtended);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_BOOLEAN_64; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromBoolean);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_BOOLEAN; // JS only
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromBoolean);
end;

procedure TEmitter.EmitOP_JS_FUNC_OBJ_FROM_VARIANT_64; // JS only
begin
  EmitStdCall_Adr1_AdrR(Id_FuncObjFromVariant);
end;
procedure TEmitter.EmitOP_JS_FUNC_OBJ_FROM_VARIANT; // JS only
begin
  EmitStdCall_Adr1_AdrR(Id_FuncObjFromVariant);
end;

procedure TEmitter.EmitOP_ANSICHAR_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiCharFromVariant);
end;
procedure TEmitter.EmitOP_ANSICHAR_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiCharFromVariant);
end;

procedure TEmitter.EmitOP_WIDECHAR_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideCharFromVariant);
end;
procedure TEmitter.EmitOP_WIDECHAR_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideCharFromVariant);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromVariant);
end;
procedure TEmitter.EmitOP_ANSISTRING_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringFromVariant);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideStringFromVariant);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideStringFromVariant);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromVariant);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringFromVariant);
end;

{$IFNDEF PAXARM}
procedure TEmitter.EmitOP_SHORTSTRING_FROM_VARIANT_64;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromVariant;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, L1);
  Prg.AsmMovREG_REG(_EDX, Reg);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SHORTSTRING_FROM_VARIANT;
var
  T1, L1, SubId, Reg: Integer;
begin
  SubId := Id_ShortStringFromVariant;

  EmitPCodeOperator;
  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  T1 := SymbolRec1.TerminalTypeID;
  L1 := GetSymbolRec(T1).Count;
  if T1 = typeSHORTSTRING then
    L1 := 255;

  Reg := GetReg;
  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmPush_REG(Reg);
  Prg.AsmPush_Imm(L1);
  Prg.AsmPush_Imm(L1);
  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
{$ENDIF}

procedure TEmitter.EmitOP_DOUBLE_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_DoubleFromVariant);
end;
procedure TEmitter.EmitOP_DOUBLE_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_DoubleFromVariant);
end;

procedure TEmitter.EmitOP_CURRENCY_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_CurrencyFromVariant);
end;
procedure TEmitter.EmitOP_CURRENCY_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_CurrencyFromVariant);
end;

procedure TEmitter.EmitOP_SINGLE_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_SingleFromVariant);
end;
procedure TEmitter.EmitOP_SINGLE_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_SingleFromVariant);
end;

procedure TEmitter.EmitOP_EXTENDED_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ExtendedFromVariant);
end;
procedure TEmitter.EmitOP_EXTENDED_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ExtendedFromVariant);
end;

procedure TEmitter.EmitOP_INT_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_IntFromVariant);
end;
procedure TEmitter.EmitOP_INT_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_IntFromVariant);
end;

procedure TEmitter.EmitOP_INT64_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_Int64FromVariant);
end;
procedure TEmitter.EmitOP_INT64_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_Int64FromVariant);
end;

procedure TEmitter.EmitOP_INT_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_INT_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BYTE_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_BYTE_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_WORD_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_WORD_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_CARDINAL_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_CARDINAL_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_SMALLINT_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_SMALLINT_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_SHORTINT_FROM_INT64_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_SHORTINT_FROM_INT64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BYTE_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ByteFromVariant);
end;
procedure TEmitter.EmitOP_BYTE_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ByteFromVariant);
end;

procedure TEmitter.EmitOP_WORD_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WordFromVariant);
end;
procedure TEmitter.EmitOP_WORD_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WordFromVariant);
end;

procedure TEmitter.EmitOP_CARDINAL_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_CardinalFromVariant);
end;
procedure TEmitter.EmitOP_CARDINAL_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_CardinalFromVariant);
end;

procedure TEmitter.EmitOP_SMALLINT_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_SmallIntFromVariant);
end;
procedure TEmitter.EmitOP_SMALLINT_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_SmallIntFromVariant);
end;

procedure TEmitter.EmitOP_SHORTINT_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ShortIntFromVariant);
end;
procedure TEmitter.EmitOP_SHORTINT_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ShortIntFromVariant);
end;

procedure TEmitter.EmitOP_BOOL_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_BoolFromVariant);
end;
procedure TEmitter.EmitOP_BOOL_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_BoolFromVariant);
end;

procedure TEmitter.EmitOP_BOOL_FROM_BYTEBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(5);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BOOL_FROM_BYTEBOOL_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(10);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BOOL_FROM_WORDBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(5);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BOOL_FROM_WORDBOOL_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(10);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BOOL_FROM_LONGBOOL;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetReg;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(5);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BOOL_FROM_LONGBOOL_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;
  EmitLoadIntVal(Reg, SymbolRec2);
  Prg.AsmCmpREG_Imm(Reg, 0);
  Prg.AsmJZ_Imm(10);
  Prg.AsmMovREG_Imm(Reg, 1);
  EmitSaveIntVal(Reg, SymbolRecR);
  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_BYTEBOOL_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ByteBoolFromVariant);
end;
procedure TEmitter.EmitOP_BYTEBOOL_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ByteBoolFromVariant);
end;

procedure TEmitter.EmitOP_WORDBOOL_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WordBoolFromVariant);
end;
procedure TEmitter.EmitOP_WORDBOOL_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WordBoolFromVariant);
end;

procedure TEmitter.EmitOP_LONGBOOL_FROM_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_LongBoolFromVariant);
end;
procedure TEmitter.EmitOP_LONGBOOL_FROM_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_LongBoolFromVariant);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_WIDECHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_WideStringFromWideChar);
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_WIDECHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_WideStringFromWideChar);
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDECHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_UnicStringFromWideChar);
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDECHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_UnicStringFromWideChar);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_WIDECHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_AnsiStringFromWideChar);
end;

procedure TEmitter.EmitOP_ANSISTRING_FROM_WIDECHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_AnsiStringFromWideChar);
end;

procedure TEmitter.EmitOP_VARIANT_FROM_WIDECHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromWideChar);
end;
procedure TEmitter.EmitOP_VARIANT_FROM_WIDECHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_VariantFromWideChar);
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDECHAR_64;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromWideChar);
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDECHAR;
begin
  EmitStdCall_Adr1_from_Int2(Id_OleVariantFromWideChar);
end;

procedure TEmitter.EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovReg_Imm(Reg, SymbolRec2.Value); // widechar walue
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_WIDESTRING_FROM_WIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_WideStringFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_Imm(Reg, SymbolRec2.Value); // widechar walue
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_UNICSTRING_FROM_WIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_UnicStringFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_VARIANT_FROM_WIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_VARIANT_FROM_WIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_VariantFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_OLEVARIANT_FROM_WIDECHAR_LITERAL;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  SubId := Id_OleVariantFromWideChar;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // string dest
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SymbolRec2.Value); // widechar walue

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_ASSIGN_VARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_VariantAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_VARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_VariantAssign);
end;

procedure TEmitter.EmitOP_ASSIGN_OLEVARIANT_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_OleVariantAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_OLEVARIANT;
begin
  EmitStdCall_AdrR_from_Adr2(Id_OleVariantAssign);
end;

procedure TEmitter.EmitOP_ASSIGN_CLASS_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ClassAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_CLASS;
begin
  EmitStdCall_AdrR_from_Adr2(Id_ClassAssign);
end;

procedure TEmitter.EmitOP_ASSIGN_ANSISTRING_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_ANSISTRING;
begin
  EmitStdCall_AdrR_from_Adr2(Id_AnsiStringAssign);
end;

procedure TEmitter.EmitOP_ASSIGN_WIDESTRING_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideStringAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_WIDESTRING;
begin
  EmitStdCall_AdrR_from_Adr2(Id_WideStringAssign);
end;

procedure TEmitter.EmitOP_ASSIGN_UNICSTRING_64;
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringAssign);
end;
procedure TEmitter.EmitOP_ASSIGN_UNICSTRING;
begin
  EmitStdCall_AdrR_from_Adr2(Id_UnicStringAssign);
end;

{$IFNDEF PAXARM}
procedure TEmitter.EmitOP_ASSIGN_SHORTSTRING_64;
var
  L, T, SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ShortStringAssign;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmMovREG_REG(_ECX, Reg);

  T := SymbolRecR.TerminalTypeID;
  if T = typeSHORTSTRING then
    L := 255
  else
    L := GetSymbolRec(T).Count;

  Prg.AsmMovREG_Imm(Reg, L);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // string dest
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ASSIGN_SHORTSTRING;
var
  L, T, SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ShortStringAssign;
  EmitCallPro(SubId);

  T := SymbolRecR.TerminalTypeID;
  if T = typeSHORTSTRING then
    L := 255
  else
    L := GetSymbolRec(T).Count;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // string dest
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(L);

  EmitLoadAddress(Reg, SymbolRec2); // string source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
{$ENDIF}

procedure TEmitter.EmitOP_ADD_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringAddition);
end;
procedure TEmitter.EmitOP_ADD_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringAddition);
end;

procedure TEmitter.EmitOP_ADD_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringAddition);
end;
procedure TEmitter.EmitOP_ADD_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringAddition);
end;

procedure TEmitter.EmitOP_ADD_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringAddition);
end;
procedure TEmitter.EmitOP_ADD_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringAddition);
end;

procedure TEmitter.EmitOP_ADD_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringAddition);
end;
procedure TEmitter.EmitOP_ADD_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringAddition);
end;

procedure TEmitter.EmitOP_MULT_INT64_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Multiplication);
end;
procedure TEmitter.EmitOP_MULT_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Multiplication);
end;

procedure TEmitter.EmitOP_IDIV_INT64_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Division);
end;
procedure TEmitter.EmitOP_IDIV_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Division);
end;

procedure TEmitter.EmitOP_MOD_INT64_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Modulo);
end;
procedure TEmitter.EmitOP_MOD_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64Modulo);
end;

procedure TEmitter.EmitOP_SHL_INT64_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64LeftShift);
end;
procedure TEmitter.EmitOP_SHL_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64LeftShift);
end;

procedure TEmitter.EmitOP_SHR_INT64_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64RightShift);
end;
procedure TEmitter.EmitOP_SHR_INT64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_Int64RightShift);
end;

procedure TEmitter.EmitOP_NEG_VARIANT_64;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantNegation);
end;
procedure TEmitter.EmitOP_NEG_VARIANT;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantNegation);
end;

procedure TEmitter.EmitOP_ABS_VARIANT_64;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantAbs);
end;
procedure TEmitter.EmitOP_ABS_VARIANT;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantAbs);
end;

procedure TEmitter.EmitOP_ABS_INT64_64;
begin
  EmitStdCall_Adr1_AdrR(Id_Int64Abs);
end;
procedure TEmitter.EmitOP_ABS_INT64;
begin
  EmitStdCall_Adr1_AdrR(Id_Int64Abs);
end;

procedure TEmitter.EmitOP_NOT_VARIANT_64;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantNot);
end;
procedure TEmitter.EmitOP_NOT_VARIANT;
begin
  EmitStdCall_Adr1_AdrR(Id_VariantNot);
end;

procedure TEmitter.EmitOP_ADD_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantAddition);
end;
procedure TEmitter.EmitOP_ADD_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantAddition);
end;

procedure TEmitter.EmitOP_SUB_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantSubtraction);
end;
procedure TEmitter.EmitOP_SUB_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantSubtraction);
end;

procedure TEmitter.EmitOP_MULT_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantMultiplication);
end;
procedure TEmitter.EmitOP_MULT_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantMultiplication);
end;

procedure TEmitter.EmitOP_DIV_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantDivision);
end;
procedure TEmitter.EmitOP_DIV_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantDivision);
end;

procedure TEmitter.EmitOP_IDIV_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantIDivision);
end;
procedure TEmitter.EmitOP_IDIV_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantIDivision);
end;

procedure TEmitter.EmitOP_MOD_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantModulo);
end;
procedure TEmitter.EmitOP_MOD_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantModulo);
end;

procedure TEmitter.EmitOP_SHL_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLeftShift);
end;
procedure TEmitter.EmitOP_SHL_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLeftShift);
end;

procedure TEmitter.EmitOP_SHR_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantRightShift);
end;
procedure TEmitter.EmitOP_SHR_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantRightShift);
end;

procedure TEmitter.EmitOP_AND_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantAnd);
end;
procedure TEmitter.EmitOP_AND_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantAnd);
end;

procedure TEmitter.EmitOP_OR_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantOr);
end;
procedure TEmitter.EmitOP_OR_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantOr);
end;

procedure TEmitter.EmitOP_XOR_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantXor);
end;
procedure TEmitter.EmitOP_XOR_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantXor);
end;

procedure TEmitter.EmitOP_LT_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLessThan);
end;
procedure TEmitter.EmitOP_LT_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLessThan);
end;

procedure TEmitter.EmitOP_LE_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLessThanOrEqual);
end;
procedure TEmitter.EmitOP_LE_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantLessThanOrEqual);
end;

procedure TEmitter.EmitOP_GT_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantGreaterThan);
end;
procedure TEmitter.EmitOP_GT_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantGreaterThan);
end;

procedure TEmitter.EmitOP_GE_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantGreaterThanOrEqual);
end;
procedure TEmitter.EmitOP_GE_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantGreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_EQ_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantEquality);
end;
procedure TEmitter.EmitOP_EQ_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantEquality);
end;

procedure TEmitter.EmitOP_NE_VARIANT_64;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantNotEquality);
end;
procedure TEmitter.EmitOP_NE_VARIANT;
begin
  EmitStdCall_Lang_Adr1_Adr2_AdrR(Id_VariantNotEquality);
end;

procedure TEmitter.EmitOP_EQ_STRUCT_64;
var
  SubId, Reg, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_StructEquality;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // s1
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // s2
  Prg.AsmMovREG_REG(_EDX, Reg);

  sz := SymbolRec1.PtrSize;
  Prg.AsmMovREG_Imm(Reg, sz);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_EQ_STRUCT;
var
  SubId, Reg, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_StructEquality;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  sz := SymbolRec1.PtrSize;
  Prg.AsmPush_Imm(sz);

  EmitLoadAddress(Reg, SymbolRec2); // s2
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // s1
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_NE_STRUCT_64;
var
  SubId, Reg, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_StructNotEquality;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // s1
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // s2
  Prg.AsmMovREG_REG(_EDX, Reg);

  sz := SymbolRec1.PtrSize;
  Prg.AsmMovREG_Imm(Reg, sz);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_NE_STRUCT;
var
  SubId, Reg, SZ: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_StructNotEquality;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  sz := SymbolRec1.PtrSize;
  Prg.AsmPush_Imm(sz);

  EmitLoadAddress(Reg, SymbolRec2); // s2
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // s1
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_EQ_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringEquality);
end;
procedure TEmitter.EmitOP_EQ_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringEquality);
end;

procedure TEmitter.EmitOP_GT_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringGreaterThan);
end;
procedure TEmitter.EmitOP_GT_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringGreaterThan);
end;

procedure TEmitter.EmitOP_GE_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringGreaterThanOrEqual);
end;
procedure TEmitter.EmitOP_GE_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringGreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_LT_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringLessThan);
end;
procedure TEmitter.EmitOP_LT_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringLessThan);
end;

procedure TEmitter.EmitOP_LE_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringLessThanOrEqual);
end;
procedure TEmitter.EmitOP_LE_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringLessThanOrEqual);
end;

procedure TEmitter.EmitOP_GT_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringGreaterThan);
end;
procedure TEmitter.EmitOP_GT_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringGreaterThan);
end;

procedure TEmitter.EmitOP_GE_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringGreaterThanOrEqual);
end;
procedure TEmitter.EmitOP_GE_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringGreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_LT_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringLessThan);
end;
procedure TEmitter.EmitOP_LT_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringLessThan);
end;

procedure TEmitter.EmitOP_LE_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringLessThanOrEqual);
end;
procedure TEmitter.EmitOP_LE_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringLessThanOrEqual);
end;

procedure TEmitter.EmitOP_GT_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringGreaterThan);
end;
procedure TEmitter.EmitOP_GT_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringGreaterThan);
end;

procedure TEmitter.EmitOP_GT_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringGreaterThan);
end;
procedure TEmitter.EmitOP_GT_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringGreaterThan);
end;

procedure TEmitter.EmitOP_GE_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringGreaterThanOrEqual);
end;
procedure TEmitter.EmitOP_GE_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringGreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_GE_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringGreaterThanOrEqual);
end;
procedure TEmitter.EmitOP_GE_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringGreaterThanOrEqual);
end;

procedure TEmitter.EmitOP_LT_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringLessThan);
end;
procedure TEmitter.EmitOP_LT_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringLessThan);
end;

procedure TEmitter.EmitOP_LT_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringLessThan);
end;
procedure TEmitter.EmitOP_LT_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringLessThan);
end;

procedure TEmitter.EmitOP_LE_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringLessThanOrEqual);
end;
procedure TEmitter.EmitOP_LE_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringLessThanOrEqual);
end;

procedure TEmitter.EmitOP_LE_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringLessThanOrEqual);
end;
procedure TEmitter.EmitOP_LE_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringLessThanOrEqual);
end;

procedure TEmitter.EmitOP_NE_ANSISTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringNotEquality);
end;
procedure TEmitter.EmitOP_NE_ANSISTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_AnsiStringNotEquality);
end;

procedure TEmitter.EmitOP_EQ_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringEquality);
end;
procedure TEmitter.EmitOP_EQ_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringEquality);
end;

procedure TEmitter.EmitOP_NE_SHORTSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringNotEquality);
end;
procedure TEmitter.EmitOP_NE_SHORTSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_ShortStringNotEquality);
end;

procedure TEmitter.EmitOP_EQ_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringEquality);
end;
procedure TEmitter.EmitOP_EQ_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringEquality);
end;

procedure TEmitter.EmitOP_EQ_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringEquality);
end;
procedure TEmitter.EmitOP_EQ_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringEquality);
end;

procedure TEmitter.EmitOP_NE_WIDESTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringNotEquality);
end;
procedure TEmitter.EmitOP_NE_WIDESTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_WideStringNotEquality);
end;

procedure TEmitter.EmitOP_NE_UNICSTRING_64;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringNotEquality);
end;
procedure TEmitter.EmitOP_NE_UNICSTRING;
begin
  EmitStdCall_Adr1_Adr2_AdrR(Id_UnicStringNotEquality);
end;

procedure TEmitter.EmitOP_VARARRAY_GET_64;
var
  SubId: Integer;
begin
  SubId := 0;
  case R.Arg2 of
    1: SubId := Id_VarArrayGet1;
    2: SubId := Id_VarArrayGet2;
    3: SubId := Id_VarArrayGet3;
  else
    RaiseError(errInternalError, []);
  end;
  EmitStdCall_Adr1_AdrR(SubId);
end;
procedure TEmitter.EmitOP_VARARRAY_GET;
var
  SubId: Integer;
begin
  SubId := 0;
  case R.Arg2 of
    1: SubId := Id_VarArrayGet1;
    2: SubId := Id_VarArrayGet2;
    3: SubId := Id_VarArrayGet3;
  else
    RaiseError(errInternalError, []);
  end;
  EmitStdCall_Adr1_AdrR(SubId);
end;

procedure TEmitter.EmitOP_VARARRAY_PUT_64;
var
  SubId: Integer;
begin
  SubId := 0;
  case R.Arg2 of
    1: SubId := Id_VarArrayPut1;
    2: SubId := Id_VarArrayPut2;
    3: SubId := Id_VarArrayPut3;
  else
    RaiseError(errInternalError, []);
  end;
  EmitStdCall_Adr1_AdrR(SubId);
end;
procedure TEmitter.EmitOP_VARARRAY_PUT;
var
  SubId: Integer;
begin
  SubId := 0;
  case R.Arg2 of
    1: SubId := Id_VarArrayPut1;
    2: SubId := Id_VarArrayPut2;
    3: SubId := Id_VarArrayPut3;
  else
    RaiseError(errInternalError, []);
  end;
  EmitStdCall_Adr1_AdrR(SubId);
end;

procedure TEmitter.EmitOP_GENERAL_GET_64;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I, NP: Integer;
  L: TIntegerList;
begin
  EmitPCodeOperator;

  SubId := JS_GetGenericPropertyId;

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;
  for I:=Code.N downto 1 do
    if Code.Records[I].Op = OP_OLE_PARAM then
      if Code.Records[I].Res = PropNameId then
        L.Insert(0, Code.Records[I].Arg1);

  Reg := GetRegEx;

  NP := L.Count;

  EmitCallPro(SubId, (NP + 5) * SizeOf(Pointer));

  if NP > 0 then
  for I:=0 to NP - 1 do
  begin
    EmitLoadAddress(Reg, GetSymbolRec(L[I]));
    Prg.AsmMovRSPPtr_REG64(Reg, $20 + I * 8);
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // object
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(_R9, NP);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R10, Reg);

  Prg.AsmMovREG_REG(_EAX, _ESP);
  Prg.AsmAddREG_Imm(_EAX, $20);

  FreeReg(Reg);
  EmitStdCall(SubId, (NP + 5) * SizeOf(Pointer));
end;
procedure TEmitter.EmitOP_GENERAL_GET;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I, NP: Integer;
  L: TIntegerList;
begin
  EmitPCodeOperator;

  SubId := JS_GetGenericPropertyId;

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;
  for I:=Code.N downto 1 do
    if Code.Records[I].Op = OP_OLE_PARAM then
      if Code.Records[I].Res = PropNameId then
        L.Insert(0, Code.Records[I].Arg1);

  Reg := GetReg;

  NP := L.Count;

  EmitCallPro(SubId, (NP + 5) * SizeOf(Pointer));

  if NP > 0 then
  for I:=NP - 1 downto 0 do
  begin
    EmitLoadAddress(Reg, GetSymbolRec(L[I]));
    Prg.AsmPush_REG(Reg);
  end;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(NP);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // object
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId, (NP + 5) * SizeOf(Pointer));
end;

procedure TEmitter.EmitOP_GENERAL_PUT_64;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I, NP: Integer;
  L: TIntegerList;
begin
  EmitPCodeOperator;

  SubId := JS_PutGenericPropertyId;

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;
  for I:=Code.N downto 1 do
    if Code.Records[I].Op = OP_OLE_PARAM then
      if Code.Records[I].Res = PropNameId then
        L.Insert(0, Code.Records[I].Arg1);

  Reg := GetRegEx;

  NP := L.Count;
  EmitCallPro(SubId, (NP + 5) * SizeOf(Pointer));

  if NP > 0 then
  for I:=0 to NP - 1 do
  begin
    EmitLoadAddress(Reg, GetSymbolRec(L[I]));
    Prg.AsmMovRSPPtr_REG64(Reg, $20 + I * 8);
  end;

  EmitLoadAddress(Reg, SymbolRec1); // object
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(Reg, NP);
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_REG(_EAX, _ESP);
  Prg.AsmAddREG_Imm(_EAX, $20);

  FreeReg(Reg);
  EmitStdCall(SubId, (NP + 5) * SizeOf(Pointer));
end;
procedure TEmitter.EmitOP_GENERAL_PUT;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I, NP: Integer;
  L: TIntegerList;
begin
  EmitPCodeOperator;

  SubId := JS_PutGenericPropertyId;

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;
  for I:=Code.N downto 1 do
    if Code.Records[I].Op = OP_OLE_PARAM then
      if Code.Records[I].Res = PropNameId then
        L.Insert(0, Code.Records[I].Arg1);

  Reg := GetReg;

  NP := L.Count;
  EmitCallPro(SubId, (NP + 4) * SizeOf(Pointer));

  if NP > 0 then
  for I:=NP - 1 downto 0 do
  begin
    EmitLoadAddress(Reg, GetSymbolRec(L[I]));
    Prg.AsmPush_REG(Reg);
  end;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(NP);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec1); // object
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId, (NP + 4) * SizeOf(Pointer));
end;

procedure TEmitter.EmitOP_OLE_GET_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_OLE_GET;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I: Integer;
  L: TIntegerList;
begin
  if R.Language = JS_LANGUAGE then
  begin
    if TargetPlatform = tpWIN64 then
      EmitOP_GENERAL_GET_64
    else
      EmitOP_GENERAL_GET;
    Exit;
  end;

  EmitPCodeOperator;

  SubId := LookUp(_strGetOLEProperty);
  if SubId = 0 then
    RaiseError(errIMPORT_ActiveX, []);

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;

  try
    for I:=Code.N downto 1 do
      if Code.Records[I].Op = OP_OLE_PARAM then
        if Code.Records[I].Res = PropNameId then
          L.Insert(0, Code.Records[I].Arg1);

    Reg := GetReg;

    for I:=L.Count - 1 downto 0 do
    begin
      EmitLoadAddress(Reg, GetSymbolRec(L[I]));
      Prg.AsmPush_REG(Reg);
    end;

    Prg.AsmPush_Imm(L.Count);

    EmitLoadAddress(Reg, SymbolRecR); // result
    Prg.AsmPush_REG(Reg);

    EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // object
    Prg.AsmPush_REG(Reg);

    EmitGet_REG(Reg, SymbolTable[SubId]);
    Prg.AsmCall_REG(Reg);
    FreeReg(Reg);
  finally
    FreeAndNil(L);
  end;
end;

procedure TEmitter.EmitOP_OLE_SET_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_OLE_SET;
var
  Reg, SubId, PropNameId: Integer;
  Code: TCode;
  I: Integer;
  L: TIntegerList;
begin
  if R.Language = JS_LANGUAGE then
  begin
    if TargetPlatform = tpWIN64 then
      EmitOP_GENERAL_PUT_64
    else
      EmitOP_GENERAL_PUT;
    Exit;
  end;

  EmitPCodeOperator;

  SubId := LookUp(_strSetOLEProperty);
  if SubId = 0 then
    RaiseError(errIMPORT_ActiveX, []);

  PropNameId := SymbolRec2.Id;
  Code := TKernel(kernel).Code;
  L := TIntegerList.Create;

  try
    for I:=Code.N downto 1 do
      if Code.Records[I].Op = OP_OLE_PARAM then
        if Code.Records[I].Res = PropNameId then
          L.Insert(0, Code.Records[I].Arg1);

    Reg := GetReg;

    for I:=L.Count - 1 downto 0 do
    begin
      EmitLoadAddress(Reg, GetSymbolRec(L[I]));
      Prg.AsmPush_REG(Reg);
    end;

    Prg.AsmPush_Imm(L.Count);

    EmitLoadAddress(Reg, SymbolRecR); // value
    Prg.AsmPush_REG(Reg);

    EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
    Prg.AsmPush_REG(Reg);

    EmitLoadAddress(Reg, SymbolRec1); // object
    Prg.AsmPush_REG(Reg);

    EmitGet_REG(Reg, SymbolTable[SubId]);
    Prg.AsmCall_REG(Reg);
    FreeReg(Reg);
  finally
    FreeAndNil(L);
  end;
end;

procedure TEmitter.EmitOP_OLE_PARAM_64;
begin
  EmitPCodeOperator;
end;
procedure TEmitter.EmitOP_OLE_PARAM;
begin
  EmitPCodeOperator;
end;

procedure TEmitter.EmitOP_ANSISTRING_CLR;
begin
  EmitStdCall_Adr1(Id_AnsiStringClr);
end;
procedure TEmitter.EmitOP_ANSISTRING_CLR_64;
begin
  EmitStdCall_Adr1(Id_AnsiStringClr);
end;

procedure TEmitter.EmitOP_SET_LENGTH_64;
var
  SubId, Reg, FinTypeId, T1, L1: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  L1 := 0;
  FinTypeId := SymbolRec1.FinalTypeId;
  case FinTypeId of
    typeVARIANT: SubId := Id_SetVariantLength;
    typeUNICSTRING: SubId := Id_SetUnicStringLength;
{$IFNDEF PAXARM}
    typeANSISTRING: SubId := Id_SetStringLength;
    typeWIDESTRING: SubId := Id_SetWideStringLength;
    typeSHORTSTRING:
    begin
      SubId := Id_SetShortStringLength;
      T1 := SymbolRec1.TerminalTypeID;
      L1 := GetSymbolRec(T1).Count;
      if T1 = typeSHORTSTRING then
        L1 := 255;
    end;
{$ENDIF}
    else
    begin
      SubId := 0;
      RaiseError(errInternalError, []);
    end;
  end;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmMovREG_REG(_ECX, Reg);

{$IFNDEF PAXARM}
  if FinTypeId = typeSHORTSTRING then
    Prg.AsmMovREG_Imm(Reg, L1)
  else
{$ENDIF}
  if FinTypeId = typeVARIANT then
    Prg.AsmMovREG_Imm(Reg, varVariant);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRec2); // L
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_SET_LENGTH;
var
  SubId, Reg, FinTypeId, T1, L1: Integer;
begin
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  L1 := 0;
  FinTypeId := SymbolRec1.FinalTypeId;
  case FinTypeId of
    typeVARIANT: SubId := Id_SetVariantLength;
    typeUNICSTRING: SubId := Id_SetUnicStringLength;
{$IFNDEF PAXARM}
    typeANSISTRING: SubId := Id_SetStringLength;
    typeWIDESTRING: SubId := Id_SetWideStringLength;
    typeSHORTSTRING:
    begin
      SubId := Id_SetShortStringLength;
      T1 := SymbolRec1.TerminalTypeID;
      L1 := GetSymbolRec(T1).Count;
      if T1 = typeSHORTSTRING then
        L1 := 255;
    end;
{$ENDIF}
    else
    begin
      SubId := 0;
      RaiseError(errInternalError, []);
    end;
  end;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec2); // L
  Prg.AsmPush_REG(Reg);

{$IFNDEF PAXARM}
  if FinTypeId = typeSHORTSTRING then
    Prg.AsmPush_Imm(L1)
  else
{$ENDIF}
  if FinTypeId = typeVARIANT then
    Prg.AsmPush_Imm(varVariant);

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_SET_LENGTH_EX_64;
begin
  RaiseError(errNotImplementedYet, []);
end;
procedure TEmitter.EmitOP_SET_LENGTH_EX;
var
  I, K, SubId, Reg, FinTypeId,Id: Integer;
  Lst: TIntegerList;
  Code: TCode;
  ElFinalTypeID, ElTypeID, ElSize: Integer;
  IsFWArray: Boolean;
begin
  Lst := TIntegerList.Create;
  SubId := 0;
  EmitPCodeOperator;
  Emit_PUSH_REGS;

  Reg := GetReg;

  try
    K := SymbolRec2.Id;
    Code := TKernel(kernel).Code;
    for I := Code.N downto 1 do
      if Code[I].GenOp = OP_PUSH_LENGTH then
      begin
        Lst.Add(Code[I].Arg1);
        if Lst.Count = K then
          break;
      end;

    FinTypeId := SymbolRec1.FinalTypeId;

    if FinTypeId = typeCLASS then
    begin
      FinTypeId := typeDYNARRAY;
      IsFWArray := true;
    end
    else
      IsFWArray := false;

    case FinTypeId of
      typeVARIANT:
        case K of
          1: SubId := Id_SetVariantLength;
          2: SubId := Id_SetVariantLength2;
          3: SubId := Id_SetVariantLength3;
        else
          RaiseError(errInternalError, []);
        end;
      typeDYNARRAY:
        case K of
          1: SubId := Id_DynarraySetLength;
          2: SubId := Id_DynarraySetLength2;
          3: SubId := Id_DynarraySetLength3;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;

    EmitCallPro(SubId);

    if FinTypeId = typeDYNARRAY then
    begin
      ElTypeID := SymbolRec1.TerminalTypeId;

      if IsFWArray then
        ElTypeID := GetSymbolRec(ElTypeId).PatternId;

      ElTypeID := GetSymbolRec(ElTypeId).PatternId;

      while GetSymbolRec(ElTypeId).FinalTypeId = typeDYNARRAY do
      begin
        ElTypeID := GetSymbolRec(ElTypeId).TerminalTypeId;
        ElTypeID := GetSymbolRec(ElTypeId).PatternId;
      end;

      ElFinalTypeID := GetSymbolRec(ElTypeId).FinalTypeId;
      ElSize := GetSymbolRec(ElTypeId).Size;

      Prg.AsmPush_Imm(ElSize);
      Prg.AsmPush_Imm(ElTypeID);
      Prg.AsmPush_Imm(ElFinalTypeID);

      for I := 0 to Lst.Count - 1 do
      begin
        Id := Lst[I];
        EmitLoadIntVal(Reg, GetSymbolRec(Id));
        Prg.AsmPush_REG(Reg);
      end;

      EmitLoadAddress(Reg, SymbolRec1); // address of array

      if IsFWArray then
      begin
        Prg.AsmMovREG_REGPtr(Reg, Reg);
        Prg.AsmAddReg_Imm(Reg, FWArrayOffset);
      end;

      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);

      Exit;
    end;

    for I := 0 to Lst.Count - 1 do
    begin
      Id := Lst[I];
      EmitLoadIntVal(Reg, GetSymbolRec(Id));
      Prg.AsmPush_REG(Reg);
    end;

    Prg.AsmPush_Imm(varVariant);

    EmitLoadAddress(Reg, SymbolRec1); // source
    Prg.AsmPush_REG(Reg);

    FreeReg(Reg);
    EmitStdCall(SubId);

  finally
    Emit_POP_REGS;
    FreeAndNil(Lst);
  end;
end;

procedure TEmitter.EmitOP_WIDESTRING_CLR_64;
begin
  EmitStdCall_Adr1(Id_WideStringClr);
end;
procedure TEmitter.EmitOP_WIDESTRING_CLR;
begin
  EmitStdCall_Adr1(Id_WideStringClr);
end;

procedure TEmitter.EmitOP_UNICSTRING_CLR_64;
begin
  EmitStdCall_Adr1(Id_UnicStringClr);
end;
procedure TEmitter.EmitOP_UNICSTRING_CLR;
begin
  EmitStdCall_Adr1(Id_UnicStringClr);
end;

procedure TEmitter.EmitOP_INTERFACE_CLR_64;
begin
  EmitStdCall_Adr1(Id_InterfaceClr);
end;
procedure TEmitter.EmitOP_INTERFACE_CLR;
begin
  EmitStdCall_Adr1(Id_InterfaceClr);
end;

procedure TEmitter.EmitOP_CLASS_CLR_64;
begin
  EmitStdCall_Adr1(Id_ClassClr);
end;
procedure TEmitter.EmitOP_CLASS_CLR;
begin
  EmitStdCall_Adr1(Id_ClassClr);
end;

procedure TEmitter.EmitOP_SHORTSTRING_HIGH_64;
begin
  EmitStdCall_Adr1_AdrR(Id_ShortstringHigh);
end;
procedure TEmitter.EmitOP_SHORTSTRING_HIGH;
begin
  EmitStdCall_Adr1_AdrR(Id_ShortstringHigh);
end;

procedure TEmitter.EmitOP_LOCK_VARRAY_64;
begin
  EmitStdCall_Adr1_AdrR(Id_LockVArray);
end;
procedure TEmitter.EmitOP_LOCK_VARRAY;
begin
  EmitStdCall_Adr1_AdrR(Id_LockVArray);
end;

procedure TEmitter.EmitOP_UNLOCK_VARRAY_64;
begin
  EmitStdCall_Adr1(Id_UnLockVArray);
end;
procedure TEmitter.EmitOP_UNLOCK_VARRAY;
begin
  EmitStdCall_Adr1(Id_UnLockVArray);
end;

procedure TEmitter.EmitOP_VARIANT_CLR_64;
begin
  EmitStdCall_Adr1(Id_VariantClr);
end;
procedure TEmitter.EmitOP_VARIANT_CLR;
begin
  EmitStdCall_Adr1(Id_VariantClr);
end;

procedure TEmitter.EmitOP_DYNARRAY_CLR_64;
var
  SubId, Reg,
  ArrayTypeId,
  ElSize, ElTypeId, ElFinalTypeId: Integer;
  ElSize2, ElTypeId2, ElFinalTypeId2: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_DynarrayClr;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  ArrayTypeId := SymbolRec1.TerminalTypeId;
  ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
  ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
  ElSize := SymbolTable[ElTypeId].Size;

  ElTypeId2 := 0;
  ElFinalTypeId2 := 0;
  ElSize2 := 0;
  if ElFinalTypeId = typeDYNARRAY then
  begin
    ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
    ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
    ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
    ElSize2 := SymbolTable[ElTypeId2].Size;
  end;

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(Reg, ElFinalTypeId);
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(Reg, ElTypeId);
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, ElSize);
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_Imm(Reg, ElFinalTypeId2);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  Prg.AsmMovREG_Imm(Reg, ElTypeId2);
  Prg.AsmMovRSPPtr_REG64(Reg, $28);

  Prg.AsmMovREG_Imm(Reg, ElSize2);
  Prg.AsmMovRSPPtr_REG64(Reg, $30);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_DYNARRAY_CLR;
var
  SubId, Reg,
  ArrayTypeId,
  ElSize, ElTypeId, ElFinalTypeId: Integer;
  ElSize2, ElTypeId2, ElFinalTypeId2: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_DynarrayClr;
  EmitCallPro(SubId);

  Reg := GetReg;

  ArrayTypeId := SymbolRec1.TerminalTypeId;
  ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
  ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
  ElSize := SymbolTable[ElTypeId].Size;

  ElTypeId2 := 0;
  ElFinalTypeId2 := 0;
  ElSize2 := 0;
  if ElFinalTypeId = typeDYNARRAY then
  begin
    ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
    ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
    ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
    ElSize2 := SymbolTable[ElTypeId2].Size;
  end;

  Prg.AsmPush_Imm(ElSize2);
  Prg.AsmPush_Imm(ElTypeId2);
  Prg.AsmPush_Imm(ElFinalTypeId2);

  Prg.AsmPush_Imm(ElSize);
  Prg.AsmPush_Imm(ElTypeId);
  Prg.AsmPush_Imm(ElFinalTypeId);

  EmitLoadAddress(Reg, SymbolRec1); // source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_CREATE_EMPTY_DYNARRAY_64;
begin
  Emit_PUSH_REGS;
  EmitStdCall_Adr1(Id_CreateEmptyDynarray);
  Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_CREATE_EMPTY_DYNARRAY;
begin
  Emit_PUSH_REGS;
  EmitStdCall_Adr1(Id_CreateEmptyDynarray);
  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_DYNARRAY_HIGH_64;
begin
  EmitStdCall_Adr1_AdrR(Id_DynarrayHigh);
end;
procedure TEmitter.EmitOP_DYNARRAY_HIGH;
begin
  EmitStdCall_Adr1_AdrR(Id_DynarrayHigh);
end;

procedure TEmitter.EmitOP_DYNARRAY_ASSIGN_64;
var
  SubId, Reg,
  ArrayTypeId,
  ElSize, ElTypeId, ElFinalTypeId,
  ElSize2, ElTypeId2, ElFinalTypeId2: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_DynarrayAssign;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  ArrayTypeId := SymbolRec1.TerminalTypeId;
  ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
  ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
  ElSize := SymbolTable[ElTypeId].Size;

  ElTypeId2 := 0;
  ElFinalTypeId2 := 0;
  ElSize2 := 0;
  if ElFinalTypeId = typeDYNARRAY then
  begin
    ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
    ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
    ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
    ElSize2 := SymbolTable[ElTypeId2].Size;
  end;

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(Reg, ElFinalTypeId);
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, ElTypeId);
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_Imm(Reg, ElSize);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  Prg.AsmMovREG_Imm(Reg, ElFinalTypeId2);
  Prg.AsmMovRSPPtr_REG64(Reg, $28);

  Prg.AsmMovREG_Imm(Reg, ElTypeId2);
  Prg.AsmMovRSPPtr_REG64(Reg, $30);

  Prg.AsmMovREG_Imm(Reg, ElSize2);
  Prg.AsmMovRSPPtr_REG64(Reg, $38);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_DYNARRAY_ASSIGN;
var
  SubId, Reg,
  ArrayTypeId,
  ElSize, ElTypeId, ElFinalTypeId,
  ElSize2, ElTypeId2, ElFinalTypeId2: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_DynarrayAssign;
  EmitCallPro(SubId);

  Reg := GetReg;

  ArrayTypeId := SymbolRec1.TerminalTypeId;
  ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
  ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
  ElSize := SymbolTable[ElTypeId].Size;

  ElTypeId2 := 0;
  ElFinalTypeId2 := 0;
  ElSize2 := 0;
  if ElFinalTypeId = typeDYNARRAY then
  begin
    ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
    ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
    ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
    ElSize2 := SymbolTable[ElTypeId2].Size;
  end;

  Prg.AsmPush_Imm(ElSize2);
  Prg.AsmPush_Imm(ElTypeId2);
  Prg.AsmPush_Imm(ElFinalTypeId2);

  Prg.AsmPush_Imm(ElSize);
  Prg.AsmPush_Imm(ElTypeId);
  Prg.AsmPush_Imm(ElFinalTypeId);

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ASSIGN_TVarRec_64;
var
  SubId, Reg,
  FinalTypeId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AssignTVarRec;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  FinalTypeId := SymbolRec2.FinalTypeId;
{$IFNDEF PAXARM}
  if SymbolRec2.HasPAnsiCharType then
  begin
    if SymbolRec2.Kind <> KindCONST then
      FinalTypeId := typeANSISTRING
    else
      FinalTypeId := typePANSICHAR;
  end
  else
{$ENDIF}
  if SymbolRec2.HasPWideCharType then
  begin
    if SymbolRec2.Kind <> KindCONST then
      FinalTypeId := typeUNICSTRING
    else
      FinalTypeId := typePWIDECHAR;
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);   // push TProgram.Self

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, FinalTypeId);
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ASSIGN_TVarRec;
var
  SubId, Reg,
  FinalTypeId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AssignTVarRec;
  EmitCallPro(SubId);

  Reg := GetReg;

  FinalTypeId := SymbolRec2.FinalTypeId;
{$IFNDEF PAXARM}
  if SymbolRec2.HasPAnsiCharType then
  begin
    if SymbolRec2.Kind <> KindCONST then
      FinalTypeId := typeANSISTRING
    else
      FinalTypeId := typePANSICHAR;
  end
  else
{$ENDIF}
  if SymbolRec2.HasPWideCharType then
  begin
    if SymbolRec2.Kind <> KindCONST then
      FinalTypeId := typeUNICSTRING
    else
      FinalTypeId := typePWIDECHAR;
  end;

  Prg.AsmPush_Imm(FinalTypeId);

  EmitLoadAddress(Reg, SymbolRec1); // dest
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // source
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);           // push TProgram.Self

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_STRUCTURE_CLR_64;
var
  SubId, Reg, TypeId, I, S, FT,
  ArrayTypeId, DestructorId,
  ElTypeId, ElFinalTypeId, ElSize: Integer;
  ElTypeId2, ElFinalTypeId2, ElSize2: Integer;
  L, T: TIntegerList;
begin
  EmitPCodeOperator;

  TypeId := SymbolRec1.TerminalTypeId;

  if TKernel(kernel).SymbolTable[TypeId].FinalTypeId = typeRECORD then
  begin
    DestructorId := TKernel(kernel).SymbolTable.FindDestructorId(TypeId);
    if DestructorId <> 0 then
    begin
      EmitCallPro(DestructorId, 0);
      if GetSymbolRec(DestructorId).Host then
      begin
        Reg := _EBX;
        EmitGet_REG(Reg, GetSymbolRec(DestructorId));
        Prg.AsmCall_REG(Reg);
      end
      else
      begin
        EmitLoadAddress(_EAX, SymbolRec1);

        Reg := _EBX;
        Prg.AsmMovREG_REG(Reg, _EDI);
        Prg.AsmAddREG_Imm(Reg, 0);
        Prg.Top.SaveSubId := DestructorId;
        List2.Add(Prg.Top);
        Prg.AsmCall_REG(Reg);
      end;
      EmitCallEpi(DestructorId, 0);
    end;
  end;

  L := TKernel(kernel).SymbolTable.GetShiftsOfDynamicFields(TypeId);
  T := TKernel(kernel).SymbolTable.GetTypesOfDynamicFields(TypeId);

  if T.Count <> L.Count then
    RaiseError(errInternalError, []);

  try
    for I:=0 to L.Count - 1 do
    begin
      Reg := GetRegEx;

      S := L[I];

      FT := GetSymbolRec(T[I]).FinalTypeId;

      SubId := 0;
      case FT of
{$IFNDEF PAXARM}
        typeANSISTRING: SubId := Id_AnsiStringClr;
        typeWIDESTRING: SubId := Id_WideStringClr;
{$ENDIF}
        typeUNICSTRING: SubId := Id_UnicStringClr;
        typeVARIANT, typeOLEVARIANT: SubId := Id_VariantClr;
        typeDYNARRAY: SubId := Id_DynarrayClr;
        typeINTERFACE: SubId := Id_InterfaceClr;
        typeCLASS: SubId := Id_ClassClr;
      else
        RaiseError(errInternalError, []);
      end;

      EmitCallPro(SubId);

      if FT = typeDYNARRAY then
      begin
        ArrayTypeId := T[I];
        ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
        ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
        ElSize := SymbolTable[ElTypeId].Size;

        ElTypeId2 := 0;
        ElFinalTypeId2 := 0;
        ElSize2 := 0;
        if ElFinalTypeId = typeDYNARRAY then
        begin
          ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
          ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
          ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
          ElSize2 := SymbolTable[ElTypeId2].Size;
        end;

        EmitLoadAddress(Reg, SymbolRec1);
        Prg.AsmAddREG_Imm(Reg, S);
        Prg.AsmMovREG_REG(_ECX, Reg);

        Prg.AsmMovREG_Imm(Reg, ElFinalTypeId);
        Prg.AsmMovREG_REG(_EDX, Reg);

        Prg.AsmMovREG_Imm(Reg, ElTypeId);
        Prg.AsmMovREG_REG(_R8, Reg);

        Prg.AsmMovREG_Imm(Reg, ElSize);
        Prg.AsmMovREG_REG(_R9, Reg);

        Prg.AsmMovREG_Imm(Reg, ElFinalTypeId2);
        Prg.AsmMovRSPPtr_REG64(Reg, $20);

        Prg.AsmMovREG_Imm(Reg, ElTypeId2);
        Prg.AsmMovRSPPtr_REG64(Reg, $28);

        Prg.AsmMovREG_Imm(Reg, ElSize2);
        Prg.AsmMovRSPPtr_REG64(Reg, $30);
      end
      else
      begin
        EmitLoadAddress(Reg, SymbolRec1);
        Prg.AsmAddREG_Imm(Reg, S);
        Prg.AsmMovREG_REG(_ECX, Reg);
      end;

      FreeReg(Reg);
      EmitStdCall(SubId);
    end; // for-loop
  finally
    FreeAndNil(L);
    FreeAndNil(T);
  end;
end;
procedure TEmitter.EmitOP_STRUCTURE_CLR;
var
  SubId, Reg, TypeId, I, S, FT,
  ArrayTypeId, DestructorId,
  ElTypeId, ElFinalTypeId, ElSize: Integer;
  ElTypeId2, ElFinalTypeId2, ElSize2: Integer;
  L, T: TIntegerList;
begin
  EmitPCodeOperator;

  TypeId := SymbolRec1.TerminalTypeId;

  if TKernel(kernel).SymbolTable[TypeId].FinalTypeId = typeRECORD then
  begin
    DestructorId := TKernel(kernel).SymbolTable.FindDestructorId(TypeId);
    if DestructorId <> 0 then
    begin
      SubId := DestructorId;
      EmitCallPro(SubId);
      if GetSymbolRec(DestructorId).Host then
      begin
        EmitStdCall(SubId);
      end
      else
      begin
        EmitLoadAddress(_EAX, SymbolRec1);

        Reg := _EDX;
        Prg.AsmMovREG_REG(Reg, _EDI);
        Prg.AsmAddREG_Imm(Reg, 0);
        Prg.Top.SaveSubId := DestructorId;
        List2.Add(Prg.Top);
        Prg.AsmCall_REG(Reg);

        EmitCallEpi(SubId);
      end;
    end;
  end;

  L := TKernel(kernel).SymbolTable.GetShiftsOfDynamicFields(TypeId);
  T := TKernel(kernel).SymbolTable.GetTypesOfDynamicFields(TypeId);

  if T.Count <> L.Count then
    RaiseError(errInternalError, []);

  try
    for I:=0 to L.Count - 1 do
    begin
      Reg := GetReg;
      S := L[I];

      FT := GetSymbolRec(T[I]).FinalTypeId;

      SubId := 0;
      case FT of
{$IFNDEF PAXARM}
        typeANSISTRING: SubId := Id_AnsiStringClr;
        typeWIDESTRING: SubId := Id_WideStringClr;
{$ENDIF}
        typeUNICSTRING: SubId := Id_UnicStringClr;
        typeVARIANT, typeOLEVARIANT: SubId := Id_VariantClr;
        typeDYNARRAY: SubId := Id_DynarrayClr;
        typeINTERFACE: SubId := Id_InterfaceClr;
        typeCLASS: SubId := Id_ClassClr;
      else
        RaiseError(errInternalError, []);
      end;

      EmitCallPro(SubId);

      if FT = typeDYNARRAY then
      begin
        ArrayTypeId := T[I];
        ElTypeId := GetSymbolRec(ArrayTypeId).PatternId;
        ElFinalTypeId := GetSymbolRec(ElTypeId).FinalTypeId;
        ElSize := SymbolTable[ElTypeId].Size;

        ElTypeId2 := 0;
        ElFinalTypeId2 := 0;
        ElSize2 := 0;
        if ElFinalTypeId = typeDYNARRAY then
        begin
          ElTypeId2 := GetSymbolRec(ElTypeId).TerminalTypeId;
          ElTypeId2 := GetSymbolRec(ElTypeId2).PatternId;
          ElFinalTypeId2 := GetSymbolRec(ElTypeId2).FinalTypeId;
          ElSize2 := SymbolTable[ElTypeId2].Size;
        end;

        Prg.AsmPush_Imm(ElSize2);
        Prg.AsmPush_Imm(ElTypeId2);
        Prg.AsmPush_Imm(ElFinalTypeId2);

        Prg.AsmPush_Imm(ElSize);
        Prg.AsmPush_Imm(ElTypeId);
        Prg.AsmPush_Imm(ElFinalTypeId);
      end;

      EmitLoadAddress(Reg, SymbolRec1);
      Prg.AsmAddREG_Imm(Reg, S);
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);
    end; // for-loop

  finally
    FreeAndNil(L);
    FreeAndNil(T);
  end;
end;

procedure TEmitter.EmitOP_STRUCTURE_ADDREF_64;
var
  SubId, Reg, TypeId, I, S, FT: Integer;
  L, T: TIntegerList;
begin
  EmitPCodeOperator;

  TypeId := SymbolRec1.TerminalTypeId;
  L := TKernel(kernel).SymbolTable.GetShiftsOfDynamicFields(TypeId);
  T := TKernel(kernel).SymbolTable.GetTypesOfDynamicFields(TypeId);

  if T.Count <> L.Count then
    RaiseError(errInternalError, []);

  try
    for I:=0 to L.Count - 1 do
    begin
      Reg := GetRegEx;

      S := L[I];

      FT := GetSymbolRec(T[I]).FinalTypeId;

      SubId := 0;
      case FT of
{$IFNDEF PAXARM}
        typeANSISTRING: SubId := Id_StringAddRef;
{$ENDIF}
        typeUNICSTRING: SubId := Id_UnicStringAddRef;
        typeVARIANT, typeOLEVARIANT: SubId := Id_VariantAddRef;
        typeDYNARRAY: SubId := Id_DynarrayAddRef;
        typeINTERFACE: SubId := Id_InterfaceAddRef;
      else
        RaiseError(errInternalError, []);
      end;

      EmitCallPro(SubId);

      EmitLoadAddress(Reg, SymbolRec1);
      Prg.AsmAddREG_Imm(Reg, S);
      Prg.AsmMovREG_REG(_ECX, Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);
    end; // for-loop

  finally
    FreeAndNil(L);
    FreeAndNil(T);
  end;
end;
procedure TEmitter.EmitOP_STRUCTURE_ADDREF;
var
  SubId, Reg, TypeId, I, S, FT: Integer;
  L, T: TIntegerList;
begin
  EmitPCodeOperator;

  TypeId := SymbolRec1.TerminalTypeId;
  L := TKernel(kernel).SymbolTable.GetShiftsOfDynamicFields(TypeId);
  T := TKernel(kernel).SymbolTable.GetTypesOfDynamicFields(TypeId);

  if T.Count <> L.Count then
    RaiseError(errInternalError, []);

  try
    for I:=0 to L.Count - 1 do
    begin
      Reg := GetReg;

      S := L[I];

      FT := GetSymbolRec(T[I]).FinalTypeId;

      SubId := 0;
      case FT of
{$IFNDEF PAXARM}
        typeANSISTRING: SubId := Id_StringAddRef;
{$ENDIF}
        typeVARIANT, typeOLEVARIANT: SubId := Id_VariantAddRef;
        typeDYNARRAY: SubId := Id_DynarrayAddRef;
        typeINTERFACE: SubId := Id_InterfaceAddRef;
      else
        RaiseError(errInternalError, []);
      end;

      EmitCallPro(SubId);

      EmitLoadAddress(Reg, SymbolRec1);
      Prg.AsmAddREG_Imm(Reg, S);
      Prg.AsmPush_REG(Reg);

      FreeReg(Reg);

      EmitStdCall(SubId);
    end; // for-loop
  finally
    FreeAndNil(L);
    FreeAndNil(T);
  end;
end;

procedure TEmitter.EmitOP_PRINT_EX_64;
var
  Reg, SubId, L, FT, K: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PrintEx;
  EmitCallPro(SubId);

  K := SymbolRec1.Kind;
{$IFNDEF PAXARM}
  if SymbolRec1.HasPAnsiCharType then
    FT := typePANSICHAR
  else
{$ENDIF}
  if SymbolRec1.HasPWideCharType then
    FT := typePWIDECHAR
  else
    FT := SymbolRec1.FinalTypeId;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  if K = KindCONST then
  begin
    if FT in INT64Types then
    begin
      Prg.AsmMovREG_Imm(_EDX, ImmValue1);
    end
    else if FT in OrdinalTypes then
      Prg.AsmMovREG_Imm(_EDX, ImmValue1)
{$IFNDEF PAXARM}
    else if FT = typePANSICHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
{$ENDIF}
    else if FT = typePWIDECHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
    else
    begin
      EmitLoadAddress(Reg, SymbolRec1); // value
      Prg.AsmMovREG_REG(_EDX, Reg);
    end;
  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1); // value
    Prg.AsmMovREG_REG(_EDX, Reg);
  end;

  Prg.AsmMovREG_Imm(_R8, K);
  Prg.AsmMovREG_Imm(_R9, FT);

///////////////////////////////////////////////////////////////////

  L := R.Arg2;
  if L > 0 then
  begin
    if GetSymbolRec(L).Kind = KindCONST then
    begin
      Prg.AsmMovREG_Imm(Reg, Cardinal(GetSymbolRec(L).Value));
      Prg.AsmMovRSPPtr_REG64(Reg, $20);
    end
    else
    begin
      EmitLoadIntVal(Reg, GetSymbolRec(L));
      Prg.AsmMovRSPPtr_REG64(Reg, $20);
    end;
  end
  else
  begin
    Prg.AsmMovREG_Imm(Reg, 0);
    Prg.AsmMovRSPPtr_REG64(Reg, $20);
  end;

  L := R.Res;
  if L > 0 then
  begin
    if GetSymbolRec(L).Kind = KindCONST then
    begin
      Prg.AsmMovREG_Imm(Reg, Cardinal(GetSymbolRec(L).Value));
      Prg.AsmMovRSPPtr_REG64(Reg, $28);
    end
    else
    begin
      EmitLoadIntVal(Reg, GetSymbolRec(L));
      Prg.AsmMovRSPPtr_REG64(Reg, $28);
    end;
  end
  else
  begin
    Prg.AsmMovREG_Imm(Reg, 0);
    Prg.AsmMovRSPPtr_REG64(Reg, $28);
  end;

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_PRINT_EX;
var
  Reg, SubId, L, FT, K: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PrintEx;

  EmitCallPro(SubId);

  Reg := GetReg;

  L := R.Res;
  if L > 0 then
  begin
    if GetSymbolRec(L).Kind = KindCONST then
      Prg.AsmPush_Imm(Cardinal(GetSymbolRec(L).Value))
    else
    begin
      EmitLoadIntVal(Reg, GetSymbolRec(L));
      Prg.AsmPush_REG(Reg);
    end;
  end
  else
    Prg.AsmPush_Imm(0);

  L := R.Arg2;
  if L > 0 then
  begin
    if GetSymbolRec(L).Kind = KindCONST then
      Prg.AsmPush_Imm(Cardinal(GetSymbolRec(L).Value))
    else
    begin
      EmitLoadIntVal(Reg, GetSymbolRec(L));
      Prg.AsmPush_REG(Reg);
    end;
  end
  else
    Prg.AsmPush_Imm(0);

{$IFNDEF PAXARM}
  if SymbolRec1.HasPAnsiCharType then
    FT := typePANSICHAR
  else
{$ENDIF}
  if SymbolRec1.HasPWideCharType then
    FT := typePWIDECHAR
  else
    FT := SymbolRec1.FinalTypeId;

  K := SymbolRec1.Kind;

  Prg.AsmPush_Imm(FT);
  Prg.AsmPush_Imm(K);

  if K = KindCONST then
  begin
    if FT in INT64Types then
    begin
      Prg.AsmPush_Imm(ImmValue1);
    end
    else if FT in OrdinalTypes then
      Prg.AsmPush_Imm(ImmValue1)
{$IFNDEF PAXARM}
    else if FT = typePANSICHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmPush_REG(Reg);
    end
{$ENDIF}
    else if FT = typePWIDECHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmPush_REG(Reg);
    end
    else
    begin
      EmitLoadAddress(Reg, SymbolRec1); // value
      Prg.AsmPush_REG(Reg);
    end;
  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1); // value
    Prg.AsmPush_REG(Reg);
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_TO_FW_OBJECT_64;
var
  Reg, SubId, FT, K: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ToFWObject;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  K := SymbolRec1.Kind;
{$IFNDEF PAXARM}
  if SymbolRec1.HasPAnsiCharType then
    FT := typePANSICHAR
  else
{$ENDIF}
  if SymbolRec1.HasPWideCharType then
    FT := typePWIDECHAR
  else
    FT := SymbolRec1.FinalTypeId;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  if K = KindCONST then
  begin
    if FT in INT64Types then
    begin
      Prg.AsmMovREG_Imm(Reg, ImmValue1);
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
    else if FT in OrdinalTypes then
    begin
      Prg.AsmMovREG_Imm(Reg, ImmValue1);
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
{$IFNDEF PAXARM}
    else if FT = typePANSICHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
{$ENDIF}
    else if FT = typePWIDECHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmMovREG_REG(_EDX, Reg);
    end
    else
    begin
      EmitLoadAddress(Reg, SymbolRec1); // value
      Prg.AsmMovREG_REG(_EDX, Reg);
    end;
  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1); // value
    Prg.AsmMovREG_REG(_EDX, Reg);
  end;

  Prg.AsmMovREG_Imm(Reg, K);
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, FT);
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_Imm(Reg, SymbolRec1.TypeId);
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovRSPPtr_REG64(Reg, $28);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_TO_FW_OBJECT;
var
  Reg, SubId, FT, K: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_ToFWObject;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

{$IFNDEF PAXARM}
  if SymbolRec1.HasPAnsiCharType then
    FT := typePANSICHAR
  else
{$ENDIF}
  if SymbolRec1.HasPWideCharType then
    FT := typePWIDECHAR
  else
    FT := SymbolRec1.FinalTypeId;

  K := SymbolRec1.Kind;

  Prg.AsmPush_Imm(SymbolRec1.TypeId);
  Prg.AsmPush_Imm(FT);
  Prg.AsmPush_Imm(K);

  if K = KindCONST then
  begin
    if FT in INT64Types then
    begin
      Prg.AsmPush_Imm(ImmValue1);
    end
    else if FT in OrdinalTypes then
      Prg.AsmPush_Imm(ImmValue1)
{$IFNDEF PAXARM}
    else if FT = typePANSICHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmPush_REG(Reg);
    end
{$ENDIF}
    else if FT = typePWIDECHAR then
    begin
      EmitGet_REG(Reg, SymbolRec1); // pchar source
      Prg.AsmPush_REG(Reg);
    end
    else
    begin
      EmitLoadAddress(Reg, SymbolRec1); // value
      Prg.AsmPush_REG(Reg);
    end;
  end
  else
  begin
    EmitLoadAddress(Reg, SymbolRec1); // value
    Prg.AsmPush_REG(Reg);
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_PUSH_CONTEXT_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PushContext;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_PUSH_CONTEXT;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PushContext;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_FIND_CONTEXT_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_FindContext;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitGet_REG(Reg, SymbolRec1); // prop name - pchar
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // alt
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, SymbolRec2.FinalTypeId);
  Prg.AsmMovREG_REG(_R9, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // (var) result
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_FIND_CONTEXT;
var
  SubId, Reg: Integer;
begin

  EmitPCodeOperator;

  SubId := Id_FindContext;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // (var) result
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(SymbolRec2.FinalTypeId);

  EmitLoadAddress(Reg, SymbolRec2); // alt
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec1); // prop name - pchar
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_FIND_JS_FUNC_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_FindFuncId;
  EmitCallPro(SubId);


  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitGet_REG(Reg, SymbolRec1); // prop name - pchar
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRec2); // alt
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // (var) result
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_FIND_JS_FUNC;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := JS_FindFuncId;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // (var) result
  Prg.AsmPush_REG(Reg);

  EmitLoadAddress(Reg, SymbolRec2); // alt
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec1); // prop name - pchar
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_POP_CONTEXT_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PopContext;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_POP_CONTEXT;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_PopContext;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_IS_64;
var
  SubId, Reg, TypeId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Is;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  TypeId := SymbolRec2.TerminalTypeId;
  EmitLoadIntVal(Reg, GetSymbolRec(TypeId + 1)); // instance
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_IS;
var
  SubId, Reg, TypeId: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_Is;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  TypeId := SymbolRec2.TerminalTypeId;
  EmitLoadIntVal(Reg, GetSymbolRec(TypeId + 1)); // instance
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_PROG_64;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self

  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
end;
procedure TEmitter.EmitOP_GET_PROG;
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self

  EmitSaveIntVal(Reg, SymbolRecR);

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_TYPEINFO_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_TypeInfo;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_TYPEINFO;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_TypeInfo;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitGet_REG(Reg, SymbolRec2); // prop name - pchar
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_INIT_FWARRAY_64;
var
  SubId, Reg, T: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_InitFWArray;
  EmitCallPro(SubId);

  T := SymbolRec1.TerminalTypeId;
  T := TKernel(kernel).SymbolTable[T].PatternId; //ArrayTypeId
  T := TKernel(kernel).SymbolTable[T].PatternId; //ElemTypeId

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);     // push TProgram.Self

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  Prg.AsmMovREG_Imm(Reg, R.Arg2); // NBounds
  Prg.AsmMovREG_REG(_R8, Reg);

  Prg.AsmMovREG_Imm(Reg, TKernel(kernel).SymbolTable[T].FinalTypeId);
  Prg.AsmMovREG_REG(_R9, Reg);

  Prg.AsmMovREG_Imm(Reg, T); // ElemTypeId
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  Prg.AsmMovREG_Imm(Reg, TKernel(kernel).SymbolTable[T].FinSize);
  Prg.AsmMovRSPPtr_REG64(Reg, $28);

  Prg.AsmMovREG_Imm(Reg, R.Res);
  Prg.AsmMovRSPPtr_REG64(Reg, $30);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_INIT_FWARRAY;
var
  SubId, Reg, T: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_InitFWArray;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);             // push TProgram.Self

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmPush_Imm(R.Arg2); // NBounds

  T := SymbolRec1.TerminalTypeId;
  T := TKernel(kernel).SymbolTable[T].PatternId; //ArrayTypeId
  T := TKernel(kernel).SymbolTable[T].PatternId; //ElemTypeId

  Prg.AsmPush_Imm(TKernel(kernel).SymbolTable[T].FinalTypeId);
  Prg.AsmPush_Imm(T); // ElemTypeId
  Prg.AsmPush_Imm(TKernel(kernel).SymbolTable[T].FinSize);
  Prg.AsmPush_Imm(R.Res); // ElemTypeId

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_BEFORE_CALL_HOST;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_BeforeCallHost;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmPush_Imm(SymbolRec1.Id);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_AFTER_CALL_HOST;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AfterCallHost;
  EmitCallPro(SubId);

  Reg := GetReg;

  Prg.AsmPush_Imm(SymbolRec1.Id);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_BEFORE_CALL_HOST_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_BeforeCallHost;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(_EDX, SymbolRec1.Id);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_AFTER_CALL_HOST_64;
var
  SubId, Reg: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_AfterCallHost;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  Prg.AsmMovREG_Imm(_EDX, SymbolRec1.Id);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ONCREATE_HOST_OBJECT_64;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnCreateHostObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ONCREATE_HOST_OBJECT;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnCreateHostObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ONDESTROY_HOST_OBJECT_64;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnDestroyHostObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ONDESTROY_HOST_OBJECT;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnDestroyHostObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ONCREATE_OBJECT_64;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnCreateObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ONCREATE_OBJECT;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnCreateObject;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_ON_AFTER_OBJECT_CREATION_64;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnAfterObjectCreation;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_ON_AFTER_OBJECT_CREATION;
var
  SubId, Reg: Integer;
begin
  SubId := Id_OnAfterObjectCreation;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_CLASSNAME_64;
begin
  EmitStdCall_Adr1_AdrR(Id_ClassName);
end;
procedure TEmitter.EmitOP_CLASSNAME;
begin
  EmitStdCall_Adr1_AdrR(Id_ClassName);
end;

procedure TEmitter.EmitOP_GET_DRTTI_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift, FT: Integer;
begin
  FT := SymbolRecR.FinalTypeId;
  if FT in INT64Types then
    SubId := Id_GetDRTTIInt64Property
  else if FT in IntegerTypes then
    SubId := Id_GetDRTTIIntegerProperty
  else if FT in StringTypes then
    SubId := Id_GetDRTTIStringProperty
  else if FT in RealTypes then
    SubId := Id_GetDRTTIExtendedProperty
  else if FT in VariantTypes then
    SubId := Id_GetDRTTIVariantProperty
  else
    SubId := Id_GetDRTTIProperty;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_DRTTI_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift, FT: Integer;
begin
  FT := SymbolRecR.FinalTypeId;
  if FT in INT64Types then
    SubId := Id_GetDRTTIInt64Property
  else if FT in IntegerTypes then
    SubId := Id_GetDRTTIIntegerProperty
  else if FT in StringTypes then
    SubId := Id_GetDRTTIStringProperty
  else if FT in RealTypes then
    SubId := Id_GetDRTTIExtendedProperty
  else if FT in VariantTypes then
    SubId := Id_GetDRTTIVariantProperty
  else
    SubId := Id_GetDRTTIProperty;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_DRTTI_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetDRTTIProperty;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_DRTTI_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetDRTTIProperty;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_ANSISTR_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetAnsiStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_ANSISTR_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetAnsiStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_ANSISTR_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetAnsiStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

{$IFNDEF PAXARM}
  if SymbolRecR.HasPAnsiCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
{$ENDIF}
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_ANSISTR_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetAnsiStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

{$IFNDEF PAXARM}
  if SymbolRecR.HasPAnsiCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
{$ENDIF}
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_WIDESTR_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetWideStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_WIDESTR_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetWideStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_WIDESTR_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetWideStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

{$IFNDEF PAXARM}
  if SymbolRecR.HasPAnsiCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
{$ENDIF}
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_WIDESTR_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetWideStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

{$IFNDEF PAXARM}
  if SymbolRecR.HasPAnsiCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
{$ENDIF}
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_UNICSTR_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetUnicStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_UNICSTR_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetUnicStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_UNICSTR_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetUnicStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  if SymbolRecR.HasPWideCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_UNICSTR_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetUnicStrProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  if SymbolRecR.HasPWideCharType then
    EmitGet_REG(Reg, SymbolRecR)
  else
    EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_ORD_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetOrdProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_ORD_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetOrdProp;

  EmitPCodeOperator;

  Emit_PUSH_REGS;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);

  Emit_POP_REGS;
end;

procedure TEmitter.EmitOP_SET_ORD_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetOrdProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_ORD_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetOrdProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_INTERFACE_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetInterfaceProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_INTERFACE_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetInterfaceProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_INTERFACE_PROP_64;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetOrdProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_INTERFACE_PROP;
var
  SubId, Reg, PropIndex, ClassId, Shift: Integer;
begin
  SubId := Id_SetOrdProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadIntVal(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_SET_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetSetProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_SET_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetSetProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_SET_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetSetProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_SET_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetSetProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_FLOAT_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetFloatProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_FLOAT_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetFloatProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_FLOAT_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetFloatProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_FLOAT_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetFloatProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_VARIANT_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetVariantProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_VARIANT_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetVariantProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_VARIANT_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetVariantProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_VARIANT_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetVariantProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_INT64_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetInt64Prop;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_INT64_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetInt64Prop;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_INT64_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetInt64Prop;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_INT64_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetInt64Prop;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // value
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_GET_EVENT_PROP_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetEventProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  HandlesEvents := true;

  Reg := GetRegEx;

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R8, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_GET_EVENT_PROP;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_GetEventProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  HandlesEvents := true;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_EVENT_PROP2_64;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetEventProp2;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  HandlesEvents := true;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);   // push TProgram.Self

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_R8, Reg);

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmMovREG_REG(_R9, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_EVENT_PROP2;
var
  SubId, Reg, ClassId, PropIndex, Shift: Integer;
begin
  SubId := Id_SetEventProp2;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  HandlesEvents := true;

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR); // result
  Prg.AsmPush_REG(Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);           // push TProgram.Self

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_SET_EVENT_PROP_64;
var
  SubId, Reg, ClassId,
  PropIndex, Shift, CodeId, DataId, CallConv, RetSize: Integer;
  R: TSymbolRec;
begin
  SubId := Id_SetEventProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetRegEx;

  HandlesEvents := true;

  if SymbolRecR.Id = TKernel(kernel).SymbolTable.NilId then
  begin
    R := TKernel(kernel).SymbolTable.AddIntegerConst(0);
    R.MustBeAllocated := true;
    DataId := R.Id;
    R := TKernel(kernel).SymbolTable.AddIntegerConst(0);
    R.MustBeAllocated := true;
    CodeId := R.Id;

    CallConv := ccREGISTER;
    RetSize := 0;
  end
  else
  begin
    DataId := SymbolRecR.OwnerId;
    CodeId := SymbolRecR.PatternId;

    if GetSymbolRec(DataId).Kind = KindTYPE then
      Inc(DataId);

    CallConv := GetSymbolRec(CodeId).CallConv;
    RetSize := SymbolTable.GetSizeOfParams(CodeId);
  end;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmMovREG_REG(_ECX, Reg);   // push TProgram.Self

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmMovREG_REG(_EDX, Reg);

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmMovREG_REG(_R8, Reg);

  if GetSymbolRec(CodeId).Host then
  begin
    EmitGet_REG(Reg, GetSymbolRec(CodeId));
  end
  else
  begin
    Prg.AsmMovREG_REG(Reg, _EDI);
    Prg.AsmAddREG_Imm(Reg, 0);
    Prg.Top.SaveSubId := CodeId;
    List2.Add(Prg.Top);
  end;
  Prg.AsmMovREG_REG(_R9, Reg);

  EmitLoadIntVal(Reg, GetSymbolRec(DataId)); // data
  Prg.AsmMovRSPPtr_REG64(Reg, $20);

  Prg.AsmMovREG_Imm(Reg, CallConv);
  Prg.AsmMovRSPPtr_REG64(Reg, $28);

  Prg.AsmMovREG_Imm(Reg, RetSize);
  Prg.AsmMovRSPPtr_REG64(Reg, $30);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_SET_EVENT_PROP;
var
  SubId, Reg, ClassId,
  PropIndex, Shift, CodeId, DataId, CallConv, RetSize: Integer;
  R: TSymbolRec;
begin
  SubId := Id_SetEventProp;

  EmitPCodeOperator;

  EmitCallPro(SubId);

  Reg := GetReg;

  HandlesEvents := true;

  if SymbolRecR.Id = TKernel(kernel).SymbolTable.NilId then
  begin
    R := TKernel(kernel).SymbolTable.AddIntegerConst(0);
    R.MustBeAllocated := true;
    DataId := R.Id;
    R := TKernel(kernel).SymbolTable.AddIntegerConst(0);
    R.MustBeAllocated := true;
    CodeId := R.Id;

    CallConv := ccREGISTER;
    RetSize := 0;
  end
  else
  begin
    DataId := SymbolRecR.OwnerId;
    CodeId := SymbolRecR.PatternId;

    if GetSymbolRec(DataId).Kind = KindTYPE then
      Inc(DataId);

    CallConv := GetSymbolRec(CodeId).CallConv;
    RetSize := SymbolTable.GetSizeOfParams(CodeId);
  end;

  Prg.AsmPush_Imm(RetSize);
  Prg.AsmPush_Imm(CallConv);

  EmitLoadIntVal(Reg, GetSymbolRec(DataId)); // data
  Prg.AsmPush_REG(Reg);

  if GetSymbolRec(CodeId).Host then
  begin
    EmitGet_REG(Reg, GetSymbolRec(CodeId));
    Prg.AsmPush_REG(Reg);
  end
  else
  begin
    Prg.AsmMovREG_REG(Reg, _EDI);
    Prg.AsmAddREG_Imm(Reg, 0);
    Prg.Top.SaveSubId := CodeId;
    List2.Add(Prg.Top);
    Prg.AsmPush_REG(Reg);
  end;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  Prg.AsmPush_REG(Reg);

  // push ppi
  PropIndex := SymbolRec2.PropIndex;
  ClassId := GetSymbolRec(SymbolRec1.TerminalHostClassId).Id;
  Shift := GetOffset(GetSymbolRec(ClassId + 1));
  Inc(Shift, (PropIndex + 1) * SizeOfPointer);
  Prg.AsmGetREG_ESIPtr(Reg, Shift);
  Prg.AsmPush_REG(Reg);

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  Prg.AsmPush_REG(Reg);           // push TProgram.Self

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_EMIT_ON;
begin
  EmitPCodeOperator;
  EmitOff := false;
end;

procedure TEmitter.EmitOP_EMIT_OFF;
begin
  EmitPCodeOperator;
  EmitOff := true;
end;

procedure TEmitter.EmitOP_CREATE_OBJECT_64;
var
  Reg, SubId, Id: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CreateObject;
  EmitCallPro(SubId);

  Reg := GetRegEx;

  Id := TKernel(kernel).Code.GetCurrSelfId(TKernel(kernel).Code.N);
  EmitLoadIntVal(Reg, GetSymbolRec(Id));
  Prg.AsmMovREG_REG(_ECX, Reg);

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmMovREG_REG(_EDX, Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;
procedure TEmitter.EmitOP_CREATE_OBJECT;
var
  Reg, SubId, Id: Integer;
begin
  EmitPCodeOperator;

  SubId := Id_CreateObject;
  EmitCallPro(SubId);

  Reg := GetReg;

  EmitLoadAddress(Reg, SymbolRecR);
  Prg.AsmPush_REG(Reg);

//  TypeId := SymbolRec1.TerminalTypeId;
//  EmitLoadIntVal(Reg, GetSymbolRec(TypeId + 1));
  Id := TKernel(kernel).Code.GetCurrSelfId(TKernel(kernel).Code.N);
  EmitLoadIntVal(Reg, GetSymbolRec(Id));
  Prg.AsmPush_REG(Reg);

  FreeReg(Reg);
  EmitStdCall(SubId);
end;

procedure TEmitter.EmitOP_DESTROY_OBJECT_64;
begin
end;
procedure TEmitter.EmitOP_DESTROY_OBJECT;
begin
end;

procedure TEmitter.EmitOP_PUSH_PROG;  // stdcall expected on win32
var
  Reg: Integer;
begin
  EmitPCodeOperator;

  Reg := GetRegEx;

  Prg.AsmMovREG_REG(Reg, _ESI);
  Prg.AsmAddREG_Imm(Reg, H_SelfPtr);
  Prg.AsmMovREG_REGPtr(Reg, Reg); // load TProgram.Self
  if TargetPlatform = tpWIN64 then
    Prg.AsmMovREG_REG(_ECX, Reg)   // push TProgram.Self
  else
    Prg.AsmPush_REG(Reg);           // push TProgram.Self

  FreeReg(Reg);
end;

procedure TEmitter.EmitOP_GET_VMT_ADDRESS_64;
var
  Reg, MethodIndex, I, msg_id: Integer;
  SymbolProgRec: TSymbolProgRec;
begin
  EmitPCodeOperator;

  msg_id := SymbolRec2.DynamicMethodIndex;
  if msg_id = 0 then
  begin
    I := TKernel(kernel).MessageList.IndexOf(SymbolRec2.FullName);
    if I >= 0 then
      msg_id := TKernel(kernel).MessageList[I].msg_id;
  end;

  if msg_id <> 0 then
  begin
    Emit_PUSH_REGS;

    Reg := GetRegEx;

    EmitLoadIntVal(Reg, SymbolRec1); // instance
    if SymbolRec1.FinalTypeId <> typeCLASSREF then
      prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmMovREG_REG(_ECX, Reg);

    Prg.AsmMovREG_Imm(Reg, msg_id);
    Prg.AsmMovREG_REG(_EDX, Reg);

    EmitLoadAddress(Reg, SymbolRecR);
    Prg.AsmMovREG_REG(_R8, Reg);

    EmitGet_REG(Reg, SymbolTable[Id_GetDynamicMethodAddress]);
    Prg.AsmCall_REG(Reg);

    FreeReg(Reg);
    Emit_POP_REGS;

    Exit;
  end;

  if IsLocalPos then
    Emit_PUSH_REGS;

  Reg := GetRegEx;

  MethodIndex := SymbolRec2.MethodIndex;

  EmitLoadIntVal(Reg, SymbolRec1); // instance
  if SymbolRec1.FinalTypeId <> typeCLASSREF then
    prg.AsmMovREG_REGPtr(Reg, Reg);

  if MethodIndex = 0 then
  begin
    SymbolProgRec := prg.AsmAddREG_Imm(Reg, 0);
    SymbolProgRec.MustBeFixed := true;
    SymbolProgRec.OpOffset := 2;
    SymbolProgRec.SubId := SymbolRec2.Id;
  end
  else
  begin
    prg.AsmAddREG_Imm(Reg, (MethodIndex - 1) * SizeOfPointer);
{$IFDEF FPC}
    prg.AsmAddREG_Imm(Reg, FPC_VIRTUAL_OFFSET);
{$ENDIF}
  end;
  prg.AsmMovREG_REGPtr(Reg, Reg);

  EmitSaveIntVal(Reg, SymbolRecR); // result

  FreeReg(Reg);

  if IsLocalPos then
    Emit_POP_REGS;
end;
procedure TEmitter.EmitOP_GET_VMT_ADDRESS;
var
  Reg, MethodIndex, I, msg_id: Integer;
  SymbolProgRec: TSymbolProgRec;
begin
  EmitPCodeOperator;

  msg_id := SymbolRec2.DynamicMethodIndex;
  if msg_id = 0 then
  begin
    I := TKernel(kernel).MessageList.IndexOf(SymbolRec2.FullName);
    if I >= 0 then
      msg_id := TKernel(kernel).MessageList[I].msg_id;
  end;

  if msg_id <> 0 then
  begin
    Emit_PUSH_REGS;

    Reg := GetRegEx;

    EmitLoadAddress(Reg, SymbolRecR);
    Prg.AsmPush_REG(Reg);

    Prg.AsmPush_Imm(msg_id);

    EmitLoadIntVal(Reg, SymbolRec1); // instance
    if SymbolRec1.FinalTypeId <> typeCLASSREF then
      prg.AsmMovREG_REGPtr(Reg, Reg);
    Prg.AsmPush_REG(Reg);

    EmitGet_REG(Reg, SymbolTable[Id_GetDynamicMethodAddress]);
    Prg.AsmCall_REG(Reg);

    FreeReg(Reg);
    Emit_POP_REGS;

    Exit;
  end;

  if IsLocalPos then
    Emit_PUSH_REGS;

  Reg := GetRegEx;

  MethodIndex := SymbolRec2.MethodIndex;

  EmitLoadIntVal(Reg, SymbolRec1); // instance

  if SymbolRec1.FinalTypeId <> typeCLASSREF then
    prg.AsmMovREG_REGPtr(Reg, Reg);

  if MethodIndex = 0 then
  begin
    SymbolProgRec := prg.AsmAddREG_Imm(Reg, 0);
    SymbolProgRec.MustBeFixed := true;
    SymbolProgRec.OpOffset := 2;
    SymbolProgRec.SubId := SymbolRec2.Id;
  end
  else
  begin
    prg.AsmAddREG_Imm(Reg, (MethodIndex - 1) * SizeOfPointer);
{$IFDEF FPC}
    prg.AsmAddREG_Imm(Reg, FPC_VIRTUAL_OFFSET);
{$ENDIF}
  end;
  prg.AsmMovREG_REGPtr(Reg, Reg);

  EmitSaveIntVal(Reg, SymbolRecR); // result

  FreeReg(Reg);

  if IsLocalPos then
    Emit_POP_REGS;
end;

function TEmitter.HasTheSameAddressRegister(S1, S2: TSymbolRec): Boolean;

var
  t: Integer;

function RR(S: TSymbolRec): Integer;
begin
  Inc(t);

  if (S.Local or S.Param) and (not S.OverScript) then // local
  begin
    if S.ByRef or S.ByRefEx then
      result := t
    else
    begin
      if S.Level = ContextStack.Top then
        result := _EBP
      else
        result := t;
    end;
  end
  else // global
  begin
    if S.Host or S.ByRef or S.ByRefEx then
      result := t
    else
      result := _ESI;
  end;
end;

begin
  t := 100;
  result := RR(S1) = RR(S2);
end;

function TEmitter.EmitGetAddressRegister(S: TSymbolRec): Integer;
                   // returns a register.
                   // If it returns ESI or EBP, address = result + S.Shift !!
                   // otherwise, address = result
                   // Caller must free the register !!
var
  temp: Integer;
begin
  if (S.Local or S.LocalInternalField or S.Param) and (not S.OverScript) then
  begin
    if S.ByRef or S.ByRefEx then
    begin
      result := GetReg;
      EmitGet_REG(result, S);
    end
    else
    begin
      if S.Level = ContextStack.Top then
        result := _EBP
      else
      begin
        result := GetReg;
        EmitRestoreEBP(result, S);
        Prg.AsmAddREG_Imm(result, GetOffset(S));
      end;
    end;
  end
  else // global
  begin
    if S.Host or S.ByRef or S.ByRefEx then
    begin
      result := GetReg;

      temp := S.FinSize;
      S.FinSize := SizeOf(IntPax);
      EmitGet_REG(result, S);
      S.FinSize := temp;
    end
    else
      result := _ESI;
  end;
end;

procedure TEmitter.EmitFLD(S: TSymbolRec);
var
  Reg: Integer;
begin
  Reg := EmitGetAddressRegister(S);
  case S.FinalTypeId of
    typeDOUBLE: Prg.AsmFldDouble_REGPtr(Reg, S);
    typeSINGLE: Prg.AsmFldSingle_REGPtr(Reg, S);
    typeEXTENDED: Prg.AsmFldExtended_REGPtr(Reg, S);
  else
    RaiseError(errInternalError, []);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitFild(S: TSymbolRec);
var
  Reg, TempReg: Integer;
begin
  Reg := GetReg;
  EmitLoadAddress(Reg, S);
  if S.PtrSize = 8 then
    Prg.AsmFild_REG64Ptr(Reg)
  else if S.FinalTypeId in IntegerTypes then
  begin
    EmitLoadAddress(Reg, S);
    case S.PtrSize of
      1:
      begin
        TempReg := GetReg;
        Prg.AsmXorREG_REG(TempReg, TempReg);
        Prg.AsmMovREG8_REGPtr(TempReg, Reg);
        Prg.AsmPush_REG(TempReg);
        Prg.AsmMovREG_REG(TempReg, _ESP);
        Prg.AsmFild_REG32Ptr(TempReg);
        Prg.AsmPop_REG(TempReg);
        FreeReg(TempReg);
      end;
      2: Prg.AsmFild_REG16Ptr(Reg);
      4: Prg.AsmFild_REG32Ptr(Reg);
    end;
  end
  else
    RaiseError(errInternalError, []);
  FreeReg(Reg);
end;

procedure TEmitter.EmitFSTP(S: TSymbolRec);
var
  Reg: Integer;
begin
  Reg := EmitGetAddressRegister(S);
  case S.FinalTypeId of
    typeDOUBLE: Prg.AsmFStpDouble_REGPtr(Reg, S);
    typeSINGLE: Prg.AsmFStpSingle_REGPtr(Reg, S);
    typeEXTENDED: Prg.AsmFStpExtended_REGPtr(Reg, S);
  else
    RaiseError(errInternalError, []);
  end;
  FreeReg(Reg);
end;

procedure TEmitter.EmitJmp;
begin
  List3.Add(SymbolRec1);
  Prg.AsmJMP_Imm(0);
  List1.Add(Prg.Top);
end;

procedure TEmitter.EmitOP_EQ_EVENT_64;
begin
  RaiseNotImpl;
end;

procedure TEmitter.EmitOP_NE_EVENT_64;
begin
  RaiseNotImpl;
end;

procedure TEmitter.EmitLoadAddress(Reg: Integer; S: TSymbolRec);
var
  temp: Integer;
begin
  if (S.Local or S.LocalInternalField or S.Param) and (not S.OverScript) then
  begin
    if S.ByRef or S.ByRefEx then
    begin
      EmitGet_REG(Reg, S);
    end
    else
    begin
      if S.Level = ContextStack.Top then
      begin
        Prg.AsmMovREG_REG(Reg, _EBP);
        Prg.AsmAddREG_Imm(Reg, GetOffset(S));
      end
      else
      begin
        EmitRestoreEBP(Reg, S);
        Prg.AsmAddREG_Imm(Reg, GetOffset(S));
      end;
    end;
  end
  else // global
  begin
    if S.Host or S.ByRef or S.ByRefEx then
    begin
      temp := S.FinSize;
      S.FinSize := SizeOf(IntPax);
      EmitGet_REG(Reg, S);
      S.FinSize := temp;
    end
    else
    begin
      Prg.AsmMovREG_REG(Reg, _ESI);
      Prg.AsmAddREG_Imm(Reg, GetOffset(S));
    end;
  end;
end;

procedure TEmitter.EmitLoadIntVal(Reg: Integer; S: TSymbolRec);
var
  TempReg, Temp: Integer;
  C: Cardinal;
begin
  if (S.Kind = KindCONST) and (not (S.FinalTypeId in RealTypes + [typeCURRENCY])) then
  begin
    C := S.Value;
    Prg.AsmMovREG_Imm(Reg, C);
  end
  else
  begin
    if S.Host or S.ByRef or S.ByRefEx then
    begin
      temp := S.FinSize;
      S.FinSize := SizeOf(IntPax);
      EmitGet_REG(Reg, S);
      S.FinSize := temp;
      case S.PtrSize of
        1:
        begin
          if TargetPlatform = tpWIN64 then
            TempReg := GetReg64
          else
            TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          Prg.AsmMovREG_Imm(TempReg, 0);
          Prg.AsmMovREG8_REGPtr(TempReg, Reg);
          Prg.AsmMovREG_REG(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
        2:
        begin
          if TargetPlatform = tpWIN64 then
            TempReg := GetReg64
          else
            TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          Prg.AsmMovREG_Imm(TempReg, 0);
          Prg.AsmMovREG16_REGPtr(TempReg, Reg);
          Prg.AsmMovREG_REG(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      else
        begin
          if TargetPlatform = tpWIN64 then
          begin
            case S.PtrSize of
              4: Prg.AsmMovREG32_REGPtr(Reg, Reg);
              8: Prg.AsmMovREG64_REGPtr(Reg, Reg);
              else
                RaiseError(errInternalError, []);
            end;
          end
          else
            Prg.AsmMovREG32_REGPtr(Reg, Reg);
        end;
      end; //case
    end
    else
    begin
      if S.PtrSize < SizeOfPointer then
        Prg.AsmMovREG_Imm(Reg, 0);
      EmitGet_REG(Reg, S);
    end;
  end;
end;

procedure TEmitter.EmitSaveIntVal(Reg: Integer; S: TSymbolRec);
var
  RegR: Integer;
begin
  if S.Host or S.ByRef or S.ByRefEx then
  begin
    RegR := GetReg;
    EmitLoadAddress(RegR, S);
    case S.PtrSize of
      1: Prg.AsmMovREGPtr_REG8(RegR,  Reg);
      2: Prg.AsmMovREGPtr_REG16(RegR, Reg);
    else
      begin
        if TargetPlatform = tpWIN64 then
        begin
          case S.PtrSize of
            4: Prg.AsmMovREGPtr_REG32(RegR, Reg);
            8: Prg.AsmMovREGPtr_REG64(RegR, Reg);
          else
            RaiseError(errInternalError, []);
          end;
        end
        else
          Prg.AsmMovREGPtr_REG32(RegR, Reg);
      end;
    end;
    FreeReg(RegR);
  end
  else
    EmitPut_REG(Reg, S);
end;

procedure TEmitter.EmitPut_REG_64(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                               // Reg contains a 32-bit value
                               // S - destination
var
  SZ, TempReg: Integer;
begin
  SZ := S.Size;

  case SZ of
    1:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG8_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG8(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG8_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    2:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG16_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG16(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG16_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    4:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG32_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG32(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG32_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    8:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG64_RBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG64(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG64_RSIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    else
      RaiseError(errInternalError, []);
  end;
end;
procedure TEmitter.EmitPut_REG(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                               // Reg contains a 32-bit value
                               // S - destination
var
  SZ, TempReg: Integer;
begin
  if TargetPlatform = tpWIN64 then
  begin
    EmitPut_REG_64(Reg, S, ExtraShift);
    Exit;
  end;

  SZ := S.Size;

  case SZ of
    1:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG8_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG8(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG8_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    2:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG16_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG16(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG16_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    else
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmPutREG32_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREGPtr_REG32(TempReg, Reg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmPutREG32_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
  end;
end;

procedure TEmitter.EmitGet_REG_64(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                              // S - source
                              // Reg - destination
var
  SZ, TempReg: Integer;
begin
  SZ := S.Size;

  case SZ of
    1:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG8_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREG_Imm(Reg, 0);
          Prg.AsmMovREG8_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
      begin
        Prg.AsmMovREG_Imm(Reg, 0);
        Prg.AsmGetREG8_ESIPtr(Reg, GetOffset(S) + ExtraShift);
      end;
    end;
    2:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG16_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREG_Imm(Reg, 0);
          Prg.AsmMovREG16_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
      begin
        Prg.AsmMovREG_Imm(Reg, 0);
        Prg.AsmGetREG16_ESIPtr(Reg, GetOffset(S) + ExtraShift);
      end;
    end;
    4:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG32_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREG32_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmGetREG32_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
    else
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG64_RBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg64;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREG64_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmGetREG64_RSIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
  end;
end;
procedure TEmitter.EmitGet_REG(Reg: Integer; S: TSymbolRec; ExtraShift: Integer = 0);
                              // S - source
                              // Reg - destination
var
  SZ, TempReg: Integer;
begin
  if TargetPlatform = tpWIN64 then
  begin
    EmitGet_REG_64(Reg, S, ExtraShift);
    Exit;
  end;

  SZ := S.Size;

  case SZ of
    1:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG8_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmXorREG_REG(Reg, Reg);
          Prg.AsmMovREG8_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
      begin
        Prg.AsmXorREG_REG(Reg, Reg);
        Prg.AsmGetREG8_ESIPtr(Reg, GetOffset(S) + ExtraShift);
      end;
    end;
    2:
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG16_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmXorREG_REG(Reg, Reg);
          Prg.AsmMovREG16_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
      begin
        Prg.AsmXorREG_REG(Reg, Reg);
        Prg.AsmGetREG16_ESIPtr(Reg, GetOffset(S) + ExtraShift);
      end;
    end;
    else
    begin
      if (S.Param or S.Local or S.LocalInternalField) and (not S.OverScript) then // local
      begin
        if S.Level = ContextStack.Top then
          Prg.AsmGetREG32_EBPPtr(Reg, GetOffset(S) + ExtraShift)
        else
        begin
          TempReg := GetReg;
          Prg.AsmPush_REG(TempReg);
          EmitRestoreEBP(TempReg, S);
          Prg.AsmAddREG_Imm(TempReg, GetOffset(S) + ExtraShift);
          Prg.AsmMovREG32_REGPtr(Reg, TempReg);
          Prg.AsmPop_REG(TempReg);
          FreeReg(TempReg);
        end;
      end
      else // global
        Prg.AsmGetREG32_ESIPtr(Reg, GetOffset(S) + ExtraShift);
    end;
  end;
end;

procedure TEmitter.EmitRestoreEBP_64(Reg: Integer; S: TSymbolRec);
var
  I, Index, SubId, RBP_Id: Integer;
begin
  SubId := S.Level;
  Index := ContextStack.IndexOf(SubId);

  if ContextStack.Count > 2 then
    Prg.AsmPush_REG(_EBP);

  for I:=ContextStack.Count - 1 downto Index + 1 do
  begin
    SubId := ContextStack[I];
    RBP_Id := SymbolTable.GetRBP_Id(SubId);
    Prg.AsmGetREG64_RBPPtr(Reg, GetOffset(GetSymbolRec(RBP_Id)));

    if ContextStack.Count > 2 then
    if I <> Index + 1 then
      Prg.AsmMovREG_REG(_EBP, Reg);
  end;

  if ContextStack.Count > 2 then
    Prg.AsmPop_REG(_EBP);
end;

procedure TEmitter.EmitRestoreEBP(Reg: Integer; S: TSymbolRec);
var
  I, Index, SubId, EBP_Id: Integer;
begin
  if TargetPlatform = tpWIN64 then
  begin
    EmitRestoreEBP_64(Reg, S);
    Exit;
  end;

  SubId := S.Level;
  Index := ContextStack.IndexOf(SubId);

  if ContextStack.Count > 2 then
    Prg.AsmPush_REG(_EBP);

  for I:=ContextStack.Count - 1 downto Index + 1 do
  begin
    SubId := ContextStack[I];
    EBP_Id := SymbolTable.GetRBP_Id(SubId);
    Prg.AsmGetREG_EBPPtr(Reg, GetOffset(GetSymbolRec(EBP_Id)));

    if ContextStack.Count > 2 then
    if I <> Index + 1 then
      Prg.AsmMovREG_REG(_EBP, Reg);
  end;

  if ContextStack.Count > 2 then
    Prg.AsmPop_REG(_EBP);
end;

procedure TEmitter.Emit_PUSH_REGS(SubId: Integer = 0);
begin
  if SubId > 0 then
    if not (GetSymbolRec(SubId).CallConv in [ccREGISTER, cc64, ccMSFASTCALL]) then
      Exit;

  case TargetPlatform of
    tpOSX32, tpIOSSim: SaveRegisters([_EAX, _ECX, _EDX, _EBX]);
    tpWIN64: SaveRegisters([_ECX, _EDX, _R8, _R9]);
    else
      SaveRegisters([_EAX, _ECX, _EDX]);
  end;
end;

procedure TEmitter.Emit_POP_REGS(SubId: Integer = 0);
begin
  if SubId > 0 then
    if not (GetSymbolRec(SubId).CallConv in [ccREGISTER, cc64, ccMSFASTCALL]) then
      Exit;

  case TargetPlatform of
    tpOSX32, tpIOSSim: RestoreRegisters([_EAX, _ECX, _EDX, _EBX]);
    tpWIN64: RestoreRegisters([_ECX, _EDX, _R8, _R9]);
    else
      RestoreRegisters([_EAX, _ECX, _EDX]);
  end;
end;

procedure TEmitter.Emit_PUSH_REGS_EX;
begin
  case TargetPlatform of
    tpOSX32, tpIOSSim: SaveRegisters([_EAX, _ECX, _EDX, _EBX, _ESI, _EDI]);
    tpWIN64: SaveRegisters([_EAX, _ECX, _EDX, _ESI, _EDI]);
    else
      SaveRegisters([_EAX, _ECX, _EDX, _ESI, _EDI]);
  end;
end;

procedure TEmitter.Emit_POP_REGS_EX;
begin
  case TargetPlatform of
    tpOSX32, tpIOSSim: RestoreRegisters([_EAX, _ECX, _EDX, _EBX, _ESI, _EDI]);
    tpWIN64: RestoreRegisters([_EAX, _ECX, _EDX, _ESI, _EDI]);
    else
      RestoreRegisters([_EAX, _ECX, _EDX, _ESI, _EDI]);
  end;
end;

procedure TEmitter.CopyContextStack(AStack: TIntegerStack);
var
  I: Integer;
begin
  for I := 0 to AStack.Count - 1 do
    ContextStack.Push(AStack[I]);
end;

function TEmitter.GetSaveRegAreaOffset: Integer;
begin
  result := $60;
end;

procedure TEmitter.SaveRegisters(const A: array of Integer;
                                 ExtraOffset: Integer = 0);
var
  I, S, Reg: Integer;
begin
  case TargetPlatform of
    tpOSX32, tpIOSSim:
    begin
      for I := 0 to System.Length(A) - 1 do
        Prg.AsmPush_REG(A[I]);
      S := System.Length(A) mod 4;
      if S <> 0 then
        Prg.AsmSubREG_Imm(_ESP, 16 - S * 4);
    end;
    tpWIN64:
    begin
      S := GetSaveRegAreaOffset + ExtraOffset;
      for I := 0 to System.Length(A) - 1 do
      begin
        Reg := A[I];
        Prg.AsmMovRSPPtr_REG64(Reg, S);
        Inc(S, 8);
      end;
    end;
    else
      for I := 0 to System.Length(A) - 1 do
        Prg.AsmPush_REG(A[I]);
  end;
end;

procedure TEmitter.RestoreRegisters(const A: array of Integer;
                                    ExtraOffset: Integer = 0);
var
  I, S, Reg: Integer;
begin
  case TargetPlatform of
    tpOSX32, tpIOSSim:
    begin
      S := System.Length(A) mod 4;
      if S <> 0 then
        Prg.AsmAddREG_Imm(_ESP, 16 - S * 4);
      for I := System.Length(A) - 1 downto 0 do
        Prg.AsmPop_REG(A[I]);
    end;
    tpWIN64:
    begin
      S := GetSaveRegAreaOffset + ExtraOffset;
      for I := 0 to System.Length(A) - 1 do
      begin
        Reg := A[I];
        Prg.AsmMovREG64_RSPPtr(Reg, S);
        Inc(S, 8);
      end;
    end;
    else
      for I := System.Length(A) - 1 downto 0 do
        Prg.AsmPop_REG(A[I]);
  end;
end;

procedure TEmitter.EmitPushParam_64(Reg: Integer);
var
  SubId, ParamNumber, ParamId: Integer;
begin
  SubId := R.Res;
  if GetSymbolRec(SubId).CallConv <> cc64 then
  begin
    Prg.AsmPush_REG(Reg);
    Exit;
  end;
  ParamNumber := R.Arg2;
  ParamId := SymbolTable.GetParamId(SubId, ParamNumber);
  Prg.AsmMovRSPPtr_REG64(Reg, GetSymbolRec(ParamId).RSPOffset);
end;

procedure TEmitter.EmitPushParam(Reg: Integer);
begin
  if TargetPlatform = tpWIN64 then
    EmitPushParam_64(Reg)
  else
    Prg.AsmPush_REG(Reg);
end;

procedure TEmitter.EmitCallPro(SubId: Integer; InitSize: Integer = - 1);
var
  K, cc, sz, S: Integer;
begin
  if not (TargetPlatform in [tpOSX32, tpIOSSim]) then
    Exit;
  K := GetSymbolRec(SubId).Kind;
  if not (K in KindSUBS) then
    Exit;

//  Prg.EmitGetCallerEIP;
//  EmitSaveRBX;

  cc := GetSymbolRec(SubId).CallConv;
  if InitSize = - 1 then
    sz := TKernel(kernel).SymbolTable.GetSizeOfParams(SubId)
  else
    sz := InitSize;
  case cc of
    ccREGISTER:
    begin
      S := 12 - (sz mod 16);
      prg.AsmSubREG_Imm(_ESP, S);
    end;
    ccSTDCALL:
    begin
      prg.AsmPush_Imm($80000000 + R1(sz));
      prg.AsmPush_Reg(_EBP);
      prg.AsmPush_Imm($beeffeed);
      prg.AsmSubREG_Imm(_ESP, R2(sz));
    end;
    ccCDECL:
    begin
      prg.AsmPush_Imm(R1(sz));
      prg.AsmPush_Reg(_EBP);
      prg.AsmPush_Imm($beeffeed);
      prg.AsmSubREG_Imm(_ESP, R2(sz));
    end;
    ccPASCAL:
    begin
      S := 12 - (sz mod 16);
      prg.AsmSubREG_Imm(_ESP, S);
    end;
    else
      RIE;
  end;
end;

procedure TEmitter.EmitStdCall(SubId: Integer; InitSize: Integer = - 1);
begin
  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    EmitSaveRDI;
    EmitRestoreRBX;
    EmitGet_REG(_EDI, SymbolTable[SubId]);
    Prg.AsmCall_REG(_EDI);
    EmitCallEpi(SubId, InitSize);
    EmitRestoreRDI;
  end
  else
  begin
    EmitGet_REG(_EBX, SymbolTable[SubId]);
    Prg.AsmCall_REG(_EBX);
    EmitCallEpi(SubId, InitSize);
  end;
end;

procedure TEmitter.EmitCallEpi(SubId: Integer; InitSize: Integer = -1);
var
  K, cc, sz, S: Integer;
begin
  if not (TargetPlatform in [tpOSX32, tpIOSSim]) then
    Exit;
  K := GetSymbolRec(SubId).Kind;
  if not (K in KindSUBS) then
    Exit;
  cc := GetSymbolRec(SubId).CallConv;
  if InitSize = - 1 then
    sz := TKernel(kernel).SymbolTable.GetSizeOfParams(SubId)
  else
    sz := InitSize;
  case cc of
    ccREGISTER:
    begin
      S := 12 - (sz mod 16);
      prg.AsmAddREG_Imm(_ESP, S);
    end;
    ccSTDCALL:
    begin
      prg.AsmAddREG_Imm(_ESP, R2(sz));
      prg.AsmAddREG_Imm(_ESP, $0c);
      Prg.AsmIncBytePtr(_ESP, - $0c);
    end;
    ccCDECL:
    begin
      prg.AsmAddREG_Imm(_ESP, R3(sz));
      Prg.AsmIncBytePtr(_ESP, - $0c);
    end;
    ccPASCAL:
    begin
      S := 12 - (sz mod 16);
      prg.AsmAddREG_Imm(_ESP, S);
    end
    else
      RIE;
  end;
end;

function TEmitter.GetSizeOfPointer: Integer;
begin
  if TargetPlatform = tpWIN64 then
    result := 8
  else
    result := 4;
end;

function TEmitter.GetTargetPlatform: TTargetPlatform;
begin
  result := TKernel(kernel).TargetPlatform;
end;

procedure EmitProgProc(akernel, aprog: Pointer; context: Pointer = nil);
var
  kernel: TKernel;
  prog: TProgram;
  Emitter: TEmitter;
  SymbolProgram: TSymbolProg;
  IsEval: Boolean;
begin
  kernel := TKernel(akernel);
  prog := TProgram(aprog);

  IsEval := context <> nil;

  Emitter := TEmitter.Create(kernel);
  if IsEval then
    Emitter.CopyContextStack(TIntegerStack(context));
  SymbolProgram := Emitter.CreateSymbolProgram(kernel);
  try
    if not IsEval then
    begin
      kernel.Code.CreateMapping(prog.ScriptMapTable, false,
                    prog.HostMapTable, prog.ScriptMapTable);
      prog.CreateMapOffsets;
      kernel.Code.CreateExportList(prog.ScriptMapTable);
    end;

    SymbolProgram.CreateProgram(prog, IsEval);
    Dump_All(DUMP_PATH, kernel, prog, SymbolProgram);

  finally
    FreeAndNil(Emitter);
    FreeAndNil(SymbolProgram);
  end;
end;

end.
