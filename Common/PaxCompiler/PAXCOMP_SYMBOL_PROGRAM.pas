////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_SYMBOL_PROGRAM.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$R-}

unit PAXCOMP_SYMBOL_PROGRAM;
interface

uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_PROG,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_BYTECODE,
  PAXCOMP_MODULE,
  PAXCOMP_CLASSLST,
  PAXCOMP_CLASSFACT,
  PAXCOMP_TRYLST,
  PAXCOMP_RTI,
  PAXCOMP_DISASM,
  PAXCOMP_STDLIB;
type
  TSymbolProgRec = class
  private
    fPAX64: Boolean;
    fLabelId: Integer;
    procedure SetLabelId(value: Integer);
    function GetPAX64: Boolean;
  public
    Op: Integer;
    Arg1, Arg2: TArg;
    Size: Integer;
    code: array[0..11] of byte;
    Comment: String;
    SaveSubId: Integer;
    ShiftValue: Integer;
    Z: Boolean;

    ProgOffset: Integer;
    MustBeFixed: Boolean;
    OpOffset: Integer;
    SubId: Integer;

    MapSub: Integer;

    constructor Create(aPAX64: Boolean);
    function ToStr: String;
    procedure Decompile;
    property LabelId: Integer read fLabelId write SetLabelId;
    property PAX64: Boolean read GetPAX64;
  end;

  TSymbolProg = class(TTypedList)
  private
    function GetRecord(I: Integer): TSymbolProgRec;
    function GetCard: Integer;
    function GetProgSize: Integer;
    function GetMovCode(Reg1, Reg2: Integer): Integer;
    function Get64Code(Reg1, Reg2: Integer): Integer;
    function GetMovESIPtrCode(Reg: Integer): Integer;
    function GetMovEBPPtrCode(Reg: Integer): Integer;
    function GetPAX64: Boolean;
    function GetSizeOfPointer: Integer;
  public
    kernel: Pointer;
    constructor Create(i_kernel: Pointer);
    function AddRecord: TSymbolProgRec;
    function Top: TSymbolProgRec;
    procedure Optimization;
    procedure Delete(I: Integer);
    function GetOffset(S: TSymbolRec): Integer;

    function AsmComment(const S: String): TSymbolProgRec;

    // requires EmitGetAddressRegister
    // MOV <SIZE> PTR [REG], Imm
    procedure AsmMovREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax); overload;
    // requires EmitGetAddressRegister
    // ADD <SIZE> PTR [REG], Imm
    procedure AsmAddREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax);
    // requires EmitGetAddressRegister
    // CMP <SIZE> PTR [REG], Imm
    procedure AsmCmpREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax);
    // requires EmitGetAddressRegister
    // NEG DWORD PTR [REG + Shift]
    procedure AsmNEG_REGPtr(Reg: Integer; S: TSymbolRec);
    // requires EmitGetAddressRegister
    // Fld QWORD PTR [REG + Shift]
    procedure AsmFldDouble_REGPtr(Reg: Integer; S: TSymbolRec); overload;
    // requires EmitGetAddressRegister
    // FStp QWORD PTR [REG + Shift]
    procedure AsmFstpDouble_REGPtr(Reg: Integer; S: TSymbolRec); overload;
    // requires EmitGetAddressRegister
    // Fld DWORD PTR [REG + Shift]
    procedure AsmFldSingle_REGPtr(Reg: Integer; S: TSymbolRec); overload;
    // requires EmitGetAddressRegister
    // Fld TBYTE PTR [REG + Shift]
    procedure AsmFldExtended_REGPtr(Reg: Integer; S: TSymbolRec); overload;
    // requires EmitGetAddressRegister
    // FStp DWORD PTR [REG + Shift]
    procedure AsmFstpSingle_REGPtr(Reg: Integer; S: TSymbolRec); overload;
    // requires EmitGetAddressRegister
    // FStp TBYTE PTR [REG + Shift]
    procedure AsmFstpExtended_REGPtr(Reg: Integer; S: TSymbolRec); overload;

    function AsmAddREG_Imm(Reg: Integer; value: Integer): TSymbolProgRec;
    procedure AsmSubREG_Imm(Reg: Integer; value: IntPax);
    procedure AsmAddREG_REG(Reg1, Reg2: Integer);
    procedure AsmAdcREG_REG(Reg1, Reg2: Integer);
    procedure AsmSbbREG_REG(Reg1, Reg2: Integer);

    procedure AsmMulREG(Reg: Integer);
    procedure AsmIMulREG(Reg: Integer);
    procedure AsmDivREG(Reg: Integer);
    procedure AsmIDivREG(Reg: Integer);
    procedure AsmShlREG(Reg: Integer);
    procedure AsmShrREG(Reg: Integer);

    procedure AsmNotREG(Reg: Integer);
    procedure AsmNegREG(Reg: Integer);

    procedure AsmSubREG_REG(Reg1, Reg2: Integer);
    procedure AsmXorREG_REG(Reg1, Reg2: Integer);
    procedure AsmAndREG_REG(Reg1, Reg2: Integer);
    procedure AsmOrREG_REG(Reg1, Reg2: Integer);

    function AsmMovREG_REG(Reg1, Reg2: Integer): TSymbolProgRec;
{$IFDEF VARIANTS}
    function AsmMovREG_Imm(Reg: Integer; value: Int64): TSymbolProgRec;
{$ELSE}
    function AsmMovREG_Imm(Reg: Integer; value: Integer): TSymbolProgRec;
{$ENDIF}
    procedure AsmMovREGPtr_Imm(Reg: Integer; value: IntPax); overload;
    procedure AsmMovREGPtr_Imm(Reg, shift: Integer; value: IntPax); overload;

    procedure AsmMovFS_REGPtr_REG32(Reg1, Reg2: Integer);

    procedure AsmMovRSPPtr_REG64(Reg: Integer; Shift: Integer);
    procedure AsmMovREG64_RSPPtr(Reg: Integer; Shift: Integer);

    procedure AsmMovREGPtr_REG(Reg1, Reg2: Integer);
    procedure AsmMovREGPtr_REG64(Reg1, Reg2: Integer);
    procedure AsmMovREGPtr_REG32(Reg1, Reg2: Integer);
    procedure AsmMovREGPtr_REG16(Reg1, Reg2: Integer);
    procedure AsmMovREGPtr_REG8(Reg1, Reg2: Integer);

    procedure AsmMovREG_REGPtr(Reg1, Reg2: Integer);
    procedure AsmMovREG64_REGPtr(Reg1, Reg2: Integer);
    procedure AsmMovREG32_REGPtr(Reg1, Reg2: Integer);
    procedure AsmMovREG16_REGPtr(Reg1, Reg2: Integer);
    procedure AsmMovREG8_REGPtr(Reg1, Reg2: Integer);
//< ?
    procedure AsmFldDouble_REGPtr(Reg: Integer); overload;
    procedure AsmFldSingle_REGPtr(Reg: Integer); overload;
    procedure AsmFldExtended_REGPtr(Reg: Integer); overload;
//? >
    procedure AsmFild_REG16Ptr(Reg: Integer);
    procedure AsmFild_REG32Ptr(Reg: Integer);
    procedure AsmFild_REG64Ptr(Reg: Integer);

    procedure AsmFistp_REG64Ptr(Reg: Integer);

    procedure AsmFAdd_REGPtr(Reg: Integer);

    procedure AsmWait;

    procedure AsmFAdd;
    procedure AsmFSub;
    procedure AsmFMul;
    procedure AsmFDiv;
    procedure AsmFChs;
    procedure AsmFAbs;
    procedure AsmFSub_REGPtr(Reg: Integer);
    procedure AsmFMul_REGPtr(Reg: Integer);
    procedure AsmFDiv_REGPtr(Reg: Integer);

    procedure AsmFMul_ESIPtr32(Shift: Integer);
    procedure AsmFDiv_ESIPtr32(Shift: Integer);

    procedure AsmFstpDouble_REGPtr(Reg: Integer); overload;
    procedure AsmFstpSingle_REGPtr(Reg: Integer); overload;
    procedure AsmFstpExtended_REGPtr(Reg: Integer); overload;

    procedure AsmFComp_REGPtr(Reg: Integer);
    procedure AsmFCompP;
    procedure AsmFstsw_AX;
    procedure AsmSahv;

    procedure AsmCDQ;

    procedure AsmSet_REGPtr(ASM_OP, Reg: Integer; S: TSymbolRec);

    procedure AsmSetL_REGPtr(Reg: Integer); // <
    procedure AsmSetLE_REGPtr(Reg: Integer); // <=
    procedure AsmSetNLE_REGPtr(Reg: Integer); // >
    procedure AsmSetNL_REGPtr(Reg: Integer); // >=

    procedure AsmSetB_REGPtr(Reg: Integer);
    procedure AsmSetBE_REGPtr(Reg: Integer);
    procedure AsmSetNBE_REGPtr(Reg: Integer);
    procedure AsmSetNB_REGPtr(Reg: Integer);
    procedure AsmSetZ_REGPtr(Reg: Integer);
    procedure AsmSetNZ_REGPtr(Reg: Integer);

    procedure AsmCmpByteREGPtr_Imm(Reg: Integer; value: Byte);
    procedure AsmCmpREG_REG(Reg1, Reg2: Integer);
    procedure AsmCmpREG_Imm(Reg: Integer; Value: Integer);

    procedure AsmTestREG8_REG8(Reg1, Reg2: Integer);

    procedure AsmCmpReg32Ptr_Imm(Reg: Integer; shift: Integer; value: Integer);

    procedure AsmIncReg32Ptr(Reg: Integer; shift: Integer);
    procedure AsmDecReg32Ptr(Reg: Integer; shift: Integer);

    procedure AsmIncBytePtr(Reg: Integer; shift: Integer);

    function AsmJmp_REG(Reg: Integer): TSymbolProgRec;
    procedure AsmCall_REG(Reg: Integer);
    procedure AsmPush_Imm(value: Integer);
    function AsmPush_REG(Reg: Integer): TSymbolProgRec;
    procedure AsmPush_Reg16(Reg: Integer);

    procedure AsmPush_FS_REGPtr(Reg: Integer);

    function AsmPop_REG(Reg: Integer): TSymbolProgRec;
    procedure AsmPush_REGPtr(Reg: Integer);

    function AsmGetREG_ESIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
    function AsmGetREG64_RSIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
    function AsmGetREG32_ESIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
    procedure AsmGetREG16_ESIPtr(Reg: Integer; shift: Integer);
    procedure AsmGetREG8_ESIPtr(Reg: Integer; shift: Integer);

    procedure AsmPutREG_ESIPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG64_RSIPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG32_ESIPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG16_ESIPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG8_ESIPtr(Reg: Integer; shift: Integer);

    procedure AsmGetREG_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmGetREG64_RBPPtr(Reg: Integer; shift: Integer);
    procedure AsmGetREG32_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmGetREG16_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmGetREG8_EBPPtr(Reg: Integer; shift: Integer);

    procedure AsmPutREG_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG64_RBPPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG32_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG16_EBPPtr(Reg: Integer; shift: Integer);
    procedure AsmPutREG8_EBPPtr(Reg: Integer; shift: Integer);

    procedure AsmNop;
    procedure AsmClc;
    procedure AsmPushfd;
    procedure AsmPopfd;
    procedure AsmXCHG(Reg1, Reg2: Integer);
    procedure AsmRet(value: Word = 0);
    procedure AsmJMP_Imm(value: Integer);

    procedure AsmJNO_Imm(value: Byte);
    procedure AsmJNC_Imm(value: Byte);
    procedure AsmJBE_Imm(value: Byte);
    procedure AsmJNLE_Imm(value: Byte);

    procedure AsmJZ_Imm(value: SmallInt);
    procedure AsmJNZ_Imm(value: SmallInt);

    procedure AsmLeaReg32_RegPtr(Reg1, Reg2: Integer; shift: Integer);

    procedure AsmRep_MOVSB;
    procedure AsmRep_MOVSD;

    procedure AsmCvtsd2ssXMM_RegPtr(XMMReg, Reg: Integer);
    procedure AsmCvtss2sdXMM_RegPtr(XMMReg, Reg: Integer);

    procedure AsmMovsdXMM_RegPtr(XMMReg, Reg: Integer);
    procedure AsmMovsdRegPtr_XMM(XMMReg, Reg: Integer);
    procedure AsmMovssXMM_RegPtr(XMMReg, Reg: Integer);
    procedure AsmMovssRegPtr_XMM(XMMReg, Reg: Integer);

    procedure AsmLoadESI_ESPPtr(shift: Integer); // not used
    procedure AsmLoadEDI_ESPPtr(shift: Integer); // not used

    procedure CreateProgram(result: TProgram; IsEval: Boolean = false);
    procedure CreateProgramSimple(result: TProgram);

    procedure AsmLeaRSP_RBPPtr(Shift: Integer);

    function GetShiftOfRecord(R: TSymbolProgRec): Integer;
    function GetShiftOfLabel(LabelId: Integer): Integer;
    procedure CreateZList(P: TProgram);
    function EmitZ: Integer;
    function EmitGetCallerEIP: Integer;

    procedure RaiseError(const Message: string; params: array of Const);
    procedure CreateError(const Message: string; params: array of Const);
    property Card: Integer read GetCard;
    property ProgSize: Integer read GetProgSize;
    property Records[I: Integer]: TSymbolProgRec read GetRecord; default;
    property PAX64: Boolean read GetPAX64;
    property SizeOfPointer: Integer read GetSizeOfPointer;
  end;

implementation

uses
  PAXCOMP_MAP,
  PAXCOMP_KERNEL;

constructor TSymbolProgRec.Create(aPAX64: Boolean);
begin
  FillChar(code, SizeOf(code), 0);
  Op := 0;
  ClearArg(Arg1);
  ClearArg(Arg2);
  Size := 0;
  LabelId := 0;
  Comment := '';
  SaveSubId := 0;
  ShiftValue := 0;
  Z := false;
  fPAX64 := aPAX64;
end;

procedure TSymbolProgRec.SetLabelId(value: Integer);
begin
  fLabelId := Value;
end;

function TSymbolProgRec.GetPAX64: Boolean;
begin
  result := fPAX64;
end;

procedure SwapRegs(var Reg1, Reg2: Integer);
var
  temp: Integer;
begin
  temp := Reg1;
  Reg1 := Reg2;
  Reg2 := temp;
end;

function TSymbolProgRec.ToStr: String;
var
  I: Integer;
begin
  result := '';
  for I := 0 to Size - 1 do
    result := result + ByteToHex(code[I]);

  if Size = -1 then
    result := AlignLeft('', 25) + Comment
  else
  begin
    if Arg2.valid then
      result := AlignLeft(result, 25) + AsmOperators[Op] + ' ' + ArgToString(Arg1, PAX64) +
        ', ' + ArgToString(Arg2, PAX64)
    else
      result := AlignLeft(result, 25) + AsmOperators[Op] + ' ' + ArgToString(Arg1, PAX64);
   end;
end;

procedure TSymbolProgRec.Decompile;
var
  S: Integer;
begin
  Decomp(@code, S, Op, Arg1, Arg2, PAX64);
  if S <> Size then
    raise Exception.Create(errInternalError);
end;

constructor TSymbolProg.Create(i_kernel: Pointer);
begin
  inherited Create;
  Self.kernel := i_kernel;
end;

function TSymbolProg.GetCard: Integer;
begin
  result := L.Count;
end;

function TSymbolProg.GetMovESIPtrCode(Reg: Integer): Integer;
begin
  result := 0;
  case Reg of
    _EAX: result := $86;
    _ECX: result := $8E;
    _EDX: result := $96;
    _EBX: result := $9E;
    _ESP: result := $A6;
    _EBP: result := $AE;
    _ESI: result := $B6;
    _EDI: result := $BE;
    _R8:  result := $86;
    _R9:  result := $8E;
    _R10: result := $96;
    _R11: result := $9E;
    _R12: result := $A6;
    _R13: result := $AE;
    _R14: result := $B6;
    _R15: result := $BE;
    else
      RaiseError(errInternalError, []);
  end;
end;

function TSymbolProg.GetMovEBPPtrCode(Reg: Integer): Integer;
begin
  result := 0;
  case Reg of
    _EAX: result := $85;
    _ECX: result := $8D;
    _EDX: result := $95;
    _EBX: result := $9D;
    _ESP: result := $A5;
    _EBP: result := $AD;
    _ESI: result := $B5;
    _EDI: result := $BD;
    _R8:  result := $85;
    _R9:  result := $8D;
    _R10: result := $95;
    _R11: result := $9D;
    _R12: result := $A5;
    _R13: result := $AD;
    _R14: result := $B5;
    _R15: result := $BD;
    else
      RaiseError(errInternalError, []);
  end;
end;

function TSymbolProg.GetMovCode(Reg1, Reg2: Integer): Integer;
begin
  result := 0;
  case Reg1 of
    _EAX, _R8:
      case Reg2 of
        _EAX, _R8:  result := $C0;
        _ECX, _R9:  result := $C8;
        _EDX, _R10: result := $D0;
        _EBX, _R11: result := $D8;
        _ESP, _R12: result := $E0;
        _EBP, _R13: result := $E8;
        _ESI, _R14: result := $F0;
        _EDI, _R15: result := $F8;
      else
        RaiseError(errInternalError, []);
      end;
    _ECX, _R9:
      case Reg2 of
        _EAX, _R8:  result := $C1;
        _ECX, _R9:  result := $C9;
        _EDX, _R10: result := $D1;
        _EBX, _R11: result := $D9;
        _ESP, _R12: result := $E1;
        _EBP, _R13: result := $E9;
        _ESI, _R14: result := $F1;
        _EDI, _R15: result := $F9;
      else
        RaiseError(errInternalError, []);
      end;
    _EDX, _R10:
      case Reg2 of
        _EAX, _R8:  result := $C2;
        _ECX, _R9:  result := $CA;
        _EDX, _R10: result := $D2;
        _EBX, _R11: result := $DA;
        _ESP, _R12: result := $E2;
        _EBP, _R13: result := $EA;
        _ESI, _R14: result := $F2;
        _EDI, _R15: result := $FA;
      else
        RaiseError(errInternalError, []);
      end;
    _EBX, _R11:
      case Reg2 of
        _EAX, _R8:  result := $C3;
        _ECX, _R9:  result := $CB;
        _EDX, _R10: result := $D3;
        _EBX, _R11: result := $DB;
        _ESP, _R12: result := $E3;
        _EBP, _R13: result := $EB;
        _ESI, _R14: result := $F3;
        _EDI, _R15: result := $FB;
      else
        RaiseError(errInternalError, []);
      end;
    _ESP:
      case Reg2 of
        _EAX, _R8:  result := $C4;
        _ECX, _R9:  result := $CC;
        _EDX, _R10: result := $D4;
        _EBX, _R11: result := $DC;
        _ESP, _R12: result := $E4;
        _EBP, _R13: result := $EC;
        _ESI, _R14: result := $F4;
        _EDI, _R15: result := $FC;
      else
        RaiseError(errInternalError, []);
      end;
    _EBP:
      case Reg2 of
        _EAX, _R8:  result := $C5;
        _ECX, _R9:  result := $CD;
        _EDX, _R10: result := $D5;
        _EBX, _R11: result := $DD;
        _ESP, _R12: result := $E5;
        _EBP, _R13: result := $ED;
        _ESI, _R14: result := $F5;
        _EDI, _R15: result := $FD;
      else
        RaiseError(errInternalError, []);
      end;
    _ESI, _R13:
      case Reg2 of
        _EAX, _R8:  result := $C6;
        _ECX, _R9:  result := $CE;
        _EDX, _R10: result := $D6;
        _EBX, _R11: result := $DE;
        _ESP, _R12: result := $E6;
        _EBP, _R13: result := $EE;
        _ESI, _R14: result := $F6;
        _EDI, _R15: result := $FE;
      else
        RaiseError(errInternalError, []);
      end;
    _EDI, _R14:
      case Reg2 of
        _EAX, _R8:  result := $C7;
        _ECX, _R9:  result := $CF;
        _EDX, _R10: result := $D7;
        _EBX, _R11: result := $DF;
        _ESP, _R12: result := $E7;
        _EBP, _R13: result := $EF;
        _ESI, _R14: result := $F7;
        _EDI, _R15: result := $FF;
      else
        RaiseError(errInternalError, []);
      end;
  end;
end;

function TSymbolProg.Get64Code(Reg1, Reg2: Integer): Integer;
begin
  result := 0;
  case Reg1 of
    _EAX, _R8:
    case Reg2 of
      _EAX: result := $00;
      _ECX: result := $01;
      _EDX: result := $02;
      _EBX: result := $03;
      _ESP: result := $04;
      _EBP: result := $05;
      _ESI: result := $06;
      _EDI: result := $07;

      _R8:  result := $00;
      _R9:  result := $01;
      _R10: result := $02;
      _R11: result := $03;
      _R12: result := $04;
      _R13: result := $05;
      _R14: result := $06;
      _R15: result := $07;
      else
        RaiseError(errInternalError, []);
    end;
    _ECX, _R9:
    case Reg2 of
      _EAX: result := $08;
      _ECX: result := $09;
      _EDX: result := $0A;
      _EBX: result := $0B;
      _ESP: result := $0C;
      _EBP: result := $0D;
      _ESI: result := $0E;
      _EDI: result := $0F;

      _R8:  result := $08;
      _R9:  result := $09;
      _R10: result := $0A;
      _R11: result := $0B;
      _R12: result := $0C;
      _R13: result := $0D;
      _R14: result := $0E;
      _R15: result := $0F;
      else
        RaiseError(errInternalError, []);
    end;
    _EDX, _R10:
    case Reg2 of
      _EAX: result := $10;
      _ECX: result := $11;
      _EDX: result := $12;
      _EBX: result := $13;
      _ESP: result := $14;
      _EBP: result := $15;
      _ESI: result := $16;
      _EDI: result := $17;

      _R8:  result := $10;
      _R9:  result := $11;
      _R10: result := $12;
      _R11: result := $13;
      _R12: result := $14;
      _R13: result := $15;
      _R14: result := $16;
      _R15: result := $17;
      else
        RaiseError(errInternalError, []);
    end;
    _EBX, _R11:
    case Reg2 of
      _EAX: result := $18;
      _ECX: result := $19;
      _EDX: result := $1A;
      _EBX: result := $1B;
      _ESP: result := $1C;
      _EBP: result := $1D;
      _ESI: result := $1E;
      _EDI: result := $1F;

      _R8:  result := $18;
      _R9:  result := $19;
      _R10: result := $1A;
      _R11: result := $1B;
      _R12: result := $1C;
      _R13: result := $1D;
      _R14: result := $1E;
      _R15: result := $1F;
      else
        RaiseError(errInternalError, []);
    end;
    _ESP, _R12:
    case Reg2 of
      _EAX: result := $20;
      _ECX: result := $21;
      _EDX: result := $22;
      _EBX: result := $23;
      _ESP: result := $24;
      _EBP: result := $25;
      _ESI: result := $26;
      _EDI: result := $27;

      _R8:  result := $20;
      _R9:  result := $21;
      _R10: result := $22;
      _R11: result := $23;
      _R12: result := $24;
      _R13: result := $25;
      _R14: result := $26;
      _R15: result := $27;
      else
        RaiseError(errInternalError, []);
    end;
    _EBP, _R13:
    case Reg2 of
      _EAX: result := $28;
      _ECX: result := $29;
      _EDX: result := $2A;
      _EBX: result := $2B;
      _ESP: result := $2C;
      _EBP: result := $2D;
      _ESI: result := $2E;
      _EDI: result := $2F;

      _R8:  result := $28;
      _R9:  result := $29;
      _R10: result := $2A;
      _R11: result := $2B;
      _R12: result := $2C;
      _R13: result := $2D;
      _R14: result := $2E;
      _R15: result := $2F;
      else
        RaiseError(errInternalError, []);
    end;
    _ESI, _R14:
    case Reg2 of
      _EAX: result := $30;
      _ECX: result := $31;
      _EDX: result := $32;
      _EBX: result := $33;
      _ESP: result := $34;
      _EBP: result := $35;
      _ESI: result := $36;
      _EDI: result := $37;

      _R8:  result := $30;
      _R9:  result := $31;
      _R10: result := $32;
      _R11: result := $33;
      _R12: result := $34;
      _R13: result := $35;
      _R14: result := $36;
      _R15: result := $37;
      else
        RaiseError(errInternalError, []);
    end;
    _EDI, _R15:
    case Reg2 of
      _EAX: result := $38;
      _ECX: result := $39;
      _EDX: result := $3A;
      _EBX: result := $3B;
      _ESP: result := $3C;
      _EBP: result := $3D;
      _ESI: result := $3E;
      _EDI: result := $3F;

      _R8:  result := $38;
      _R9:  result := $39;
      _R10: result := $3A;
      _R11: result := $3B;
      _R12: result := $3C;
      _R13: result := $3D;
      _R14: result := $3E;
      _R15: result := $3F;
      else
        RaiseError(errInternalError, []);
    end;
    else
      RaiseError(errInternalError, []);
  end;
end;

function TSymbolProg.GetProgSize: Integer;
var
  I, SZ: Integer;
begin
  result := 0;
  for I:=1 to Card do
  begin
    SZ := Records[I].Size;
    if SZ > 0 then
      result := result + SZ;
  end;
end;

function TSymbolProg.GetShiftOfLabel(LabelId: Integer): Integer;
var
  I, SZ: Integer;
begin
  result := 0;
  for I:=1 to Card do
  begin
    if Records[I].LabelId = LabelId then
      Exit;

    SZ := Records[I].Size;
    if SZ > 0 then
      result := result + SZ;
  end;
  result := -1;
end;

function TSymbolProg.GetShiftOfRecord(R: TSymbolProgRec): Integer;
var
  I, SZ: Integer;
begin
  result := 0;
  for I:=1 to Card do
  begin
    if Records[I] = R then
      Exit;

    SZ := Records[I].Size;
    if SZ > 0 then
      result := result + SZ;
  end;
  result := -1;
end;

function TSymbolProg.AddRecord: TSymbolProgRec;
begin
  result := TSymbolProgRec.Create(PAX64);
  L.Add(result);
end;

function TSymbolProg.Top: TSymbolProgRec;
begin
  result := Records[Card];
end;

procedure TSymbolProg.Delete(I: Integer);
begin
  Records[I].Free;
  L.Delete(I - 1);
end;

function TSymbolProg.AsmComment(const S: String): TSymbolProgRec;
begin
  result := AddRecord;
  with result do
  begin
    Size := -1;
    Comment := ';' + S;
  end;
end;

{$IFDEF VARIANTS}
function TSymbolProg.AsmMovREG_Imm(Reg: Integer; value: Int64): TSymbolProgRec;
{$ELSE}
function TSymbolProg.AsmMovREG_Imm(Reg: Integer; value: Integer): TSymbolProgRec;
{$ENDIF}
begin
  result := AddRecord;
  if PAX64 then
  begin
    with result do
    begin
      Size := 10;
      code[0] := $48;

      if Reg in R64 then
        Code[0] := $49;

      case Reg of
        _EAX: code[1] := $B8;
        _ECX: code[1] := $B9;
        _EDX: code[1] := $BA;
        _EBX: code[1] := $BB;
        _ESP: code[1] := $BC;
        _EBP: code[1] := $BD;
        _ESI: code[1] := $BE;
        _EDI: code[1] := $BF;

        _R8:  code[1] := $B8;
        _R9:  code[1] := $B9;
        _R10: code[1] := $BA;
        _R11: code[1] := $BB;
        _R12: code[1] := $BC;
        _R13: code[1] := $BD;
        _R14: code[1] := $BE;
        _R15: code[1] := $BF;

        else
          RaiseError(errInternalError, []);
      end;
      Move(value, code[2], 8);
      Decompile;
    end;
  end
  else //32bit
  begin
    with result do
    begin
      Size := 5;
      case Reg of
        _EAX: code[0] := $B8;
        _ECX: code[0] := $B9;
        _EDX: code[0] := $BA;
        _EBX: code[0] := $BB;
        _ESP: code[0] := $BC;
        _EBP: code[0] := $BD;
        _ESI: code[0] := $BE;
        _EDI: code[0] := $BF;
        else
          RaiseError(errInternalError, []);
      end;
      Move(value, code[1], 4);
      Decompile;
    end;
  end;
end;

procedure TSymbolProg.AsmLeaRSP_RBPPtr(Shift: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $8D;
    code[2] := $A5;
    Move(Shift, code[3], 4);
    Decompile;
  end
  else //32bit
    RaiseError(errInternalError, []);
end;

procedure TSymbolProg.AsmMovREGPtr_Imm(Reg: Integer; value: IntPax);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    if Reg in R64 then
      Code[0] := $49;

    code[1] := $C7;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;

      _R8:  code[2] := $00;
      _R9:  code[2] := $01;
      _R10: code[2] := $02;
      _R11: code[2] := $03;
      _R12: code[2] := $04;
      _R13: code[2] := $05;
      _R14: code[2] := $06;
      _R15: code[2] := $07;
      else
        RaiseError(errInternalError, []);
    end;
    Move(value, code[3], 4);
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 6;
    code[0] := $C7;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
      else
        RaiseError(errInternalError, []);
    end;
    Move(value, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREGPtr_Imm(Reg, shift: Integer; value: IntPax);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 11;
    code[0] := $48;
    if Reg in R64 then
      Code[0] := $49;

    code[1] := $C7;
    case Reg of
      _EAX: code[2] := $80;
      _ECX: code[2] := $81;
      _EDX: code[2] := $82;
      _EBX: code[2] := $83;
      _EBP: code[2] := $85;
      _ESI: code[2] := $86;

      _R8:  code[2] := $80;
      _R9:  code[2] := $81;
      _R10: code[2] := $82;
      _R11: code[2] := $83;
      _R12: code[2] := $84;
      _R13: code[2] := $85;
      _R14: code[2] := $86;
      _R15: code[2] := $87;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[3], 4);
    Move(value, code[7], 4);
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 10;
    code[0] := $C7;
    case Reg of
      _EAX: code[1] := $80;
      _ECX: code[1] := $81;
      _EDX: code[1] := $82;
      _EBX: code[1] := $83;
      _EBP: code[1] := $85;
      _ESI: code[1] := $86;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Move(value, code[6], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREG_REGPtr(Reg1, Reg2: Integer);
begin
  if PAX64 then
    AsmMovREG64_REGPtr(Reg1, Reg2)
  else
    AsmMovREG32_REGPtr(Reg1, Reg2);
end;

procedure TSymbolProg.AsmMovREG64_REGPtr(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 3;
        code[0] := $49;
        code[1] := $8B;
        code[2] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 3;
        code[0] := $48;
        code[1] := $8B;
        code[2] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        Code[0] := $4D
      else
        Code[0] := $4C;
      Code[1] := $8B;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREG32_REGPtr(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 4;
        code[0] := $67;
        code[1] := $41;
        code[2] := $8B;
        code[3] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 2;
        code[0] := $8B;
        code[1] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        Code[0] := $45
      else
        Code[0] := $44;
      Code[1] := $8B;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREG16_REGPtr(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 4;
        code[0] := $66;
        code[1] := $41;
        code[2] := $8B;
        code[3] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 3;
        code[0] := $66;
        code[1] := $8B;
        code[2] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 4;
      code[0] := $66;
      if Reg2 in R64 then
        code[1] := $45
      else
        code[1] := $44;
      code[2] := $8B;
      code[3] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREG8_REGPtr(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 3;
        code[0] := $41;
        code[1] := $8A;
        code[2] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 2;
        code[0] := $8A;
        code[1] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        code[0] := $45
      else
        code[0] := $44;
      code[1] := $8A;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

function TSymbolProg.AsmMovREG_REG(Reg1, Reg2: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  if PAX64 then
  with result do
  begin
    Size := 3;
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
        code[0] := $4C
      else
        code[0] := $48;
    end
    else if Reg1 in R64 then
    begin
      if Reg2 in R64 then
        code[0] := $4D
      else
        code[0] := $49;
    end
    else
      RaiseError(errInternalError, []);
    code[1] := $89;
    code[2] := GetMovCode(Reg1, Reg2);
    Decompile;
  end
  else //32bit
  with result do
  begin
    Size := 2;
    code[0] := $89;
    code[1] := GetMovCode(Reg1, Reg2);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCmpREG_Imm(Reg: Integer; Value: Integer);
begin
  with AddRecord do
  begin
    case Reg of
      _EAX:
      begin
        Size := 5;
        code[0] := $3D;
        Move(Value, code[1], 4);
      end;
      _ECX, _EDX, _EBX:
      begin
        Size := 6;
        code[0] := $81;
        case Reg of
          _ECX: code[1] := $F9;
          _EDX: code[1] := $FA;
          _EBX: code[1] := $FB;
          _ESP: code[1] := $FC;
          _EBP: code[1] := $FD;
          _ESI: code[1] := $FE;
          _EDI: code[1] := $FF;
        end;
        Move(Value, code[2], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCmpREG_REG(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $39;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmTestREG8_REG8(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    if (Reg1 = _EDX) and (Reg2 = _EDX) then
    begin
      Code[0] := $84;
      Code[1] := $d2;
    end
    else
     RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovFS_REGPtr_REG32(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $64;
    code[1] := $89;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $00;
          _ECX: code[2] := $08;
          _EDX: code[2] := $10;
          _EBX: code[2] := $18;
          _ESP: code[2] := $20;
          _EBP: code[2] := $28;
          _ESI: code[2] := $30;
          _EDI: code[2] := $38;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $01;
          _ECX: code[2] := $09;
          _EDX: code[2] := $11;
          _EBX: code[2] := $19;
          _ESP: code[2] := $21;
          _EBP: code[2] := $29;
          _ESI: code[2] := $31;
          _EDI: code[2] := $39;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $02;
          _ECX: code[2] := $0A;
          _EDX: code[2] := $12;
          _EBX: code[2] := $1A;
          _ESP: code[2] := $22;
          _EBP: code[2] := $2A;
          _ESI: code[2] := $32;
          _EDI: code[2] := $3A;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $03;
          _ECX: code[2] := $0B;
          _EDX: code[2] := $13;
          _EBX: code[2] := $1B;
          _ESP: code[2] := $23;
          _EBP: code[2] := $2B;
          _ESI: code[2] := $33;
          _EDI: code[2] := $3B;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovRSPPtr_REG64(Reg: Integer; Shift: Integer);
begin
  with AddRecord do
  begin
    Size := 8;
    if Reg in R64 then
      code[0] := $4C
    else
      code[0] := $48;
    code[1] := $89;
    case Reg of
      _EAX: code[2] := $84;
      _ECX: code[2] := $8C;
      _EDX: code[2] := $94;
      _EBX: code[2] := $9C;
      _ESP: code[2] := $A4;
      _EBP: code[2] := $AC;
      _ESI: code[2] := $B4;
      _EDI: code[2] := $BC;

      _R8:  code[2] := $84;
      _R9:  code[2] := $8C;
      _R10: code[2] := $94;
      _R11: code[2] := $9C;
      _R12: code[2] := $A4;
      _R13: code[2] := $AC;
      _R14: code[2] := $B4;
      _R15: code[2] := $BC;
      else
        RaiseError(errInternalError, []);
    end;
    code[3] := $24;
    Move(Shift, code[4], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREG64_RSPPtr(Reg: Integer; Shift: Integer);
begin
  with AddRecord do
  begin
    Size := 8;
    if Reg in R64 then
      code[0] := $4C
    else
      code[0] := $48;
    code[1] := $8B;
    case Reg of
      _EAX: code[2] := $84;
      _ECX: code[2] := $8C;
      _EDX: code[2] := $94;
      _EBX: code[2] := $9C;
      _ESP: code[2] := $A4;
      _EBP: code[2] := $AC;
      _ESI: code[2] := $B4;
      _EDI: code[2] := $BC;

      _R8:  code[2] := $84;
      _R9:  code[2] := $8C;
      _R10: code[2] := $94;
      _R11: code[2] := $9C;
      _R12: code[2] := $A4;
      _R13: code[2] := $AC;
      _R14: code[2] := $B4;
      _R15: code[2] := $BC;
      else
        RaiseError(errInternalError, []);
    end;
    code[3] := $24;
    Move(Shift, code[4], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREGPtr_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
    AsmMovREGPtr_REG64(Reg1, Reg2)
  else
    AsmMovREGPtr_REG32(Reg1, Reg2);
end;

procedure TSymbolProg.AsmMovREGPtr_REG64(Reg1, Reg2: Integer);
begin
  SwapRegs(Reg1, Reg2);
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 3;
        code[0] := $49;
        code[1] := $89;
        code[2] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 3;
        code[0] := $48;
        code[1] := $89;
        code[2] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        Code[0] := $4D
      else
        Code[0] := $4C;
      Code[1] := $89;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREGPtr_REG32(Reg1, Reg2: Integer);
begin
  SwapRegs(Reg1, Reg2);
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 4;
        code[0] := $67;
        code[1] := $41;
        code[2] := $89;
        code[3] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 2;
        code[0] := $89;
        code[1] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        Code[0] := $45
      else
        Code[0] := $44;
      Code[1] := $89;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREGPtr_REG16(Reg1, Reg2: Integer);
begin
  SwapRegs(Reg1, Reg2);
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 4;
        code[0] := $66;
        code[1] := $41;
        code[2] := $89;
        code[3] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 3;
        code[0] := $66;
        code[1] := $89;
        code[2] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 4;
      code[0] := $66;
      if Reg2 in R64 then
        code[1] := $45
      else
        code[1] := $44;
      code[2] := $89;
      code[3] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovREGPtr_REG8(Reg1, Reg2: Integer);
begin
  SwapRegs(Reg1, Reg2);
  with AddRecord do
  begin
    if Reg1 in R32 then
    begin
      if Reg2 in R64 then
      begin
        Size := 3;
        code[0] := $41;
        code[1] := $88;
        code[2] := Get64Code(Reg1, Reg2);
      end
      else
      begin
        Size := 2;
        code[0] := $88;
        code[1] := Get64Code(Reg1, Reg2);
      end;
    end
    else if Reg1 in R64 then
    begin
      Size := 3;
      if Reg2 in R64 then
        code[0] := $45
      else
        code[0] := $44;
      code[1] := $88;
      Code[2] := Get64Code(Reg1, Reg2);
    end
    else
      RaiseError(errInternalError, []);
    Decompile;
  end;
exit;
  with AddRecord do
  begin
    Size := 2;
    code[0] := $88;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $00;
          _ECX: code[1] := $08;
          _EDX: code[1] := $10;
          _EBX: code[1] := $18;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $01;
          _ECX: code[1] := $09;
          _EDX: code[1] := $11;
          _EBX: code[1] := $19;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $02;
          _ECX: code[1] := $0A;
          _EDX: code[1] := $12;
          _EBX: code[1] := $1A;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $03;
          _ECX: code[1] := $0B;
          _EDX: code[1] := $13;
          _EBX: code[1] := $1B;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

function TSymbolProg.AsmAddREG_Imm(Reg: Integer; value: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  with result do
  begin
    if PAX64 then
    begin
      Size := 7;
      if Reg in R64 then
        code[0] := $49
      else
        code[0] := $48;

      code[1] := $81;
      case Reg of
        _EAX: code[2] := $C0;
        _ECX: code[2] := $C1;
        _EDX: code[2] := $C2;
        _EBX: code[2] := $C3;
        _ESP: code[2] := $C4;
        _EBP: code[2] := $C5;
        _ESI: code[2] := $C6;
        _EDI: code[2] := $C7;

        _R8:  code[2] := $C0;
        _R9:  code[2] := $C1;
        _R10: code[2] := $C2;
        _R11: code[2] := $C3;
        _R12: code[2] := $C4;
        _R13: code[2] := $C5;
        _R14: code[2] := $C6;
        _R15: code[2] := $C7;
        else
          RaiseError(errInternalError, []);
      end;
      Move(value, code[3], 4);
    end
    else // 32bit
    case Reg of
      _EAX:
      begin
        Size := 5;
        code[0] := $05;
        Move(value, code[1], 4);
      end;
      _ECX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $C1;
        Move(value, code[2], 4);
      end;
      _EDX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $C2;
        Move(value, code[2], 4);
      end;
      _EBX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $C3;
        Move(value, code[2], 4);
      end;
      _ESP:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $C4;
        Move(value, code[2], 4);
      end;
      _EBP:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $C5;
        Move(value, code[2], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSubREG_Imm(Reg: Integer; value: IntPax);
begin
  with AddRecord do
  begin
    if PAX64 then
    begin
      Size := 7;
      code[0] := $48;
      code[1] := $81;
      case Reg of
        _EAX: code[2] := $E8;
        _ECX: code[2] := $E9;
        _EDX: code[2] := $EA;
        _EBX: code[2] := $EB;
        _ESP: code[2] := $EC;
        _EBP: code[2] := $ED;
        _ESI: code[2] := $EE;
        _EDI: code[2] := $EF;
        else
          RaiseError(errInternalError, []);
      end;
      Move(value, code[3], 4);
    end
    else //32bit
    case Reg of
      _EAX:
      begin
        Size := 5;
        code[0] := $2D;
        Move(value, code[1], 4);
      end;
      _ECX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $E9;
        Move(value, code[2], 4);
      end;
      _EDX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $EA;
        Move(value, code[2], 4);
      end;
      _EBX:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $EB;
        Move(value, code[2], 4);
      end;
      _ESP:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $EC;
        Move(value, code[2], 4);
      end;
      _EBP:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $ED;
        Move(value, code[2], 4);
      end;
      _ESI:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $EE;
        Move(value, code[2], 4);
      end;
      _EDI:
      begin
        Size := 6;
        code[0] := $81;
        code[1] := $EF;
        Move(value, code[2], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// MOV <SIZE> PTR [REG], Imm

procedure TSymbolProg.AsmMovREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;
  with AddRecord do
  begin
    case S.PtrSize of
      1:
      begin
        Size := 7;
        code[0] := $C6;
        case Reg of
          _EAX: code[1] := $80;
          _ECX: code[1] := $81;
          _EDX: code[1] := $82;
          _EBX: code[1] := $83;
          _ESP: code[1] := $84;
          _EBP: code[1] := $85;
          _ESI: code[1] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        code[6] := value;
      end;
      2:
      begin
        Size := 9;
        code[0] := $66;
        code[1] := $C7;
        case Reg of
          _EAX: code[2] := $80;
          _ECX: code[2] := $81;
          _EDX: code[2] := $82;
          _EBX: code[2] := $83;
          _ESP: code[2] := $84;
          _EBP: code[2] := $85;
          _ESI: code[2] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 2);
      end;
      4:
      begin
        Size := 10;
        code[0] := $C7;
        case Reg of
          _EAX: code[1] := $80;
          _ECX: code[1] := $81;
          _EDX: code[1] := $82;
          _EBX: code[1] := $83;
          _EBP: code[1] := $85;
          _ESI: code[1] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        Move(value, code[6], 4);
      end;
      8:
      begin
        Size := 11;
        code[0] := $48;
        code[1] := $C7;
        case Reg of
          _EAX: code[2] := $80;
          _ECX: code[2] := $81;
          _EDX: code[2] := $82;
          _EBX: code[2] := $83;
          _EBP: code[2] := $85;
          _ESI: code[2] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// ADD <SIZE> PTR [REG], Imm
procedure TSymbolProg.AsmAddREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  with AddRecord do
  begin

    case S.PtrSize of
      1:
      begin
        Size := 7;
        code[0] := $80;
        case Reg of
          _EAX: code[1] := $80;
          _ECX: code[1] := $81;
          _EDX: code[1] := $82;
          _EBX: code[1] := $83;
          _ESP: code[1] := $84;
          _EBP: code[1] := $85;
          _ESI: code[1] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        Move(value, code[6], 1);
      end;
      2:
      begin
        Size := 9;
        code[0] := $66;
        code[1] := $81;
        case Reg of
          _EAX: code[2] := $80;
          _ECX: code[2] := $81;
          _EDX: code[2] := $82;
          _EBX: code[2] := $83;
          _ESP: code[2] := $84;
          _EBP: code[2] := $85;
          _ESI: code[2] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 2);
      end;
      4:
      begin
        Size := 10;
        code[0] := $81;
        case Reg of
          _EAX: code[1] := $80;
          _ECX: code[1] := $81;
          _EDX: code[1] := $82;
          _EBX: code[1] := $83;
          _EBP: code[1] := $85;
          _ESI: code[1] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        Move(value, code[6], 4);
      end;
      8:
      begin
        Size := 11;
        code[0] := $48;
        code[1] := $81;
        case Reg of
          _EAX: code[2] := $80;
          _ECX: code[2] := $81;
          _EDX: code[2] := $82;
          _EBX: code[2] := $83;
          _EBP: code[2] := $85;
          _ESI: code[2] := $86;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// CMP <SIZE> PTR [REG], Imm
procedure TSymbolProg.AsmCmpREGPtr_Imm(Reg: Integer; S: TSymbolRec; value: IntPax);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  with AddRecord do
  begin
    case S.PtrSize of
      1:
      begin
        Size := 7;
        code[0] := $80;
        case Reg of
          _EAX: code[1] := $B8;
          _ECX: code[1] := $B9;
          _EDX: code[1] := $BA;
          _EBX: code[1] := $BB;
          _ESP: code[1] := $BC;
          _EBP: code[1] := $BD;
          _ESI: code[1] := $BE;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        Move(value, code[6], 1);
      end;
      2:
      begin
        Size := 9;
        code[0] := $66;
        code[1] := $81;
        case Reg of
          _EAX: code[2] := $B8;
          _ECX: code[2] := $B9;
          _EDX: code[2] := $BA;
          _EBX: code[2] := $BB;
          _ESP: code[2] := $BC;
          _EBP: code[2] := $BD;
          _ESI: code[2] := $BE;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 2);
      end;
      4:
      begin
        Size := 10;
        code[0] := $81;
        case Reg of
          _EAX: code[1] := $B8;
          _ECX: code[1] := $B9;
          _EDX: code[1] := $BA;
          _EBX: code[1] := $BB;
          _ESP: code[1] := $BC;
          _EBP: code[1] := $BD;
          _ESI: code[1] := $BE;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[2], 4);
        Move(value, code[6], 4);
      end;
      8:
      begin
        Size := 11;
        code[0] := $48;
        code[1] := $81;
        case Reg of
          _EAX: code[2] := $B8;
          _ECX: code[2] := $B9;
          _EDX: code[2] := $BA;
          _EBX: code[2] := $BB;
          _ESP: code[2] := $BC;
          _EBP: code[2] := $BD;
          _ESI: code[2] := $BE;
          else
            RaiseError(errInternalError, []);
        end;
        Move(shift, code[3], 4);
        Move(value, code[7], 4);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmIncReg32Ptr(Reg: Integer; shift: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $FF;
    case Reg of
      _EAX: code[1] := $80;
      _ECX: code[1] := $81;
      _EDX: code[1] := $82;
      _EBX: code[1] := $83;
      _EBP: code[1] := $85;
      _ESI: code[1] := $86;
      _EDI: code[1] := $87;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmIncBytePtr(Reg: Integer; shift: Integer);
begin
  with AddRecord do
  begin
    case Reg of
      _ESP:
      begin
        if Shift <> -12 then
          RaiseError(errInternalError, []);

        Size := 4;
        Code[0] := $FE;
        Code[1] := $44;
        Code[2] := $24;
        Code[3] := $F4;
        Decompile;
      end;
      else
        RaiseError(errInternalError, []);
    end;
  end;
end;

procedure TSymbolProg.AsmDecReg32Ptr(Reg: Integer; shift: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $FF;
    case Reg of
      _EAX: code[1] := $88;
      _ECX: code[1] := $89;
      _EDX: code[1] := $8A;
      _EBX: code[1] := $8B;
//      _ESP: code[1] := $8C;  special case
      _EBP: code[1] := $8D;
      _ESI: code[1] := $8E;
      _EDI: code[1] := $8F;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCmpReg32Ptr_Imm(Reg: Integer; shift: Integer; value: Integer);
begin
  with AddRecord do
  begin
    Size := 10;
    code[0] := $81;
    case Reg of
      _EAX: code[1] := $B8;
      _ECX: code[1] := $B9;
      _EDX: code[1] := $BA;
      _EBX: code[1] := $BB;
      _ESP: code[1] := $BC;
      _EBP: code[1] := $BD;
      _ESI: code[1] := $BE;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Move(value, code[6], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmAddREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $01;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $01;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmAdcREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $11;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $11;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSbbREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $19;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $19;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSubREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $29;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $29;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmXorREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $31;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $31;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmAndREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $21;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $21;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmOrREG_REG(Reg1, Reg2: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $09;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[2] := $C0;
          _ECX: code[2] := $C8;
          _EDX: code[2] := $D0;
          _EBX: code[2] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[2] := $C1;
          _ECX: code[2] := $C9;
          _EDX: code[2] := $D1;
          _EBX: code[2] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[2] := $C2;
          _ECX: code[2] := $CA;
          _EDX: code[2] := $D2;
          _EBX: code[2] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[2] := $C3;
          _ECX: code[2] := $CB;
          _EDX: code[2] := $D3;
          _EBX: code[2] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $09;
    case Reg1 of
      _EAX:
        case Reg2 of
          _EAX: code[1] := $C0;
          _ECX: code[1] := $C8;
          _EDX: code[1] := $D0;
          _EBX: code[1] := $D8;
        else
          RaiseError(errInternalError, []);
        end;
      _ECX:
        case Reg2 of
          _EAX: code[1] := $C1;
          _ECX: code[1] := $C9;
          _EDX: code[1] := $D1;
          _EBX: code[1] := $D9;
        else
          RaiseError(errInternalError, []);
        end;
      _EDX:
        case Reg2 of
          _EAX: code[1] := $C2;
          _ECX: code[1] := $CA;
          _EDX: code[1] := $D2;
          _EBX: code[1] := $DA;
        else
          RaiseError(errInternalError, []);
        end;
      _EBX:
        case Reg2 of
          _EAX: code[1] := $C3;
          _ECX: code[1] := $CB;
          _EDX: code[1] := $D3;
          _EBX: code[1] := $DB;
        else
          RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMulREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $E0;
      _ECX: code[2] := $E1;
      _EDX: code[2] := $E2;
      _EBX: code[2] := $E3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $E0;
      _ECX: code[1] := $E1;
      _EDX: code[1] := $E2;
      _EBX: code[1] := $E3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmIMulREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $E8;
      _ECX: code[2] := $E9;
      _EDX: code[2] := $EA;
      _EBX: code[2] := $EB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $E8;
      _ECX: code[1] := $E9;
      _EDX: code[1] := $EA;
      _EBX: code[1] := $EB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmDivREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $F0;
      _ECX: code[2] := $F1;
      _EDX: code[2] := $F2;
      _EBX: code[2] := $F3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $F0;
      _ECX: code[1] := $F1;
      _EDX: code[1] := $F2;
      _EBX: code[1] := $F3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmIDivREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $F8;
      _ECX: code[2] := $F9;
      _EDX: code[2] := $FA;
      _EBX: code[2] := $FB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $F8;
      _ECX: code[1] := $F9;
      _EDX: code[1] := $FA;
      _EBX: code[1] := $FB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmShlREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $D3;
    case Reg of
      _EAX: code[2] := $E0;
      _ECX: code[2] := $E1;
      _EDX: code[2] := $E2;
      _EBX: code[2] := $E3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D3;
    case Reg of
      _EAX: code[1] := $E0;
      _ECX: code[1] := $E1;
      _EDX: code[1] := $E2;
      _EBX: code[1] := $E3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmShrREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $D3;
    case Reg of
      _EAX: code[2] := $E8;
      _ECX: code[2] := $E9;
      _EDX: code[2] := $EA;
      _EBX: code[2] := $EB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D3;
    case Reg of
      _EAX: code[1] := $E8;
      _ECX: code[1] := $E9;
      _EDX: code[1] := $EA;
      _EBX: code[1] := $EB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmNotREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $D0;
      _ECX: code[2] := $D1;
      _EDX: code[2] := $D2;
      _EBX: code[2] := $D3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $D0;
      _ECX: code[1] := $D1;
      _EDX: code[1] := $D2;
      _EBX: code[1] := $D3;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmNegREG(Reg: Integer);
begin
  if PAX64 then
  with AddRecord do
  begin
    Size := 3;
    code[0] := $48;
    code[1] := $F7;
    case Reg of
      _EAX: code[2] := $D8;
      _ECX: code[2] := $D9;
      _EDX: code[2] := $DA;
      _EBX: code[2] := $DB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F7;
    case Reg of
      _EAX: code[1] := $D8;
      _ECX: code[1] := $D9;
      _EDX: code[1] := $DA;
      _EBX: code[1] := $DB;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// NEG <SIZE = 4> PTR [REG + Shift]
procedure TSymbolProg.AsmNEG_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  case S.PtrSize of
    4:
    begin
      with AddRecord do
      begin
        Size := 6;
        code[0] := $F7;

        case Reg of
          _EAX: code[1] := $98;
          _ECX: code[1] := $99;
          _EDX: code[1] := $9A;
          _EBX: code[1] := $9B;
          _ESP: code[1] := $9C;
          _EBP: code[1] := $9D;
          _ESI: code[1] := $9E;
        else
          RaiseError(errInternalError, []);
        end;

        Move(shift, code[2], 4);
        Decompile;
      end;
    end;
    8:
    begin
      with AddRecord do
      begin
        Size := 7;
        code[0] := $48;
        code[1] := $F7;

        case Reg of
          _EAX: code[2] := $98;
          _ECX: code[2] := $99;
          _EDX: code[2] := $9A;
          _EBX: code[2] := $9B;
          _ESP: code[2] := $9C;
          _EBP: code[2] := $9D;
          _ESI: code[2] := $9E;
        else
          RaiseError(errInternalError, []);
        end;

        Move(shift, code[3], 4);
        Decompile;
      end;
    end;
    else
      RaiseError(errInternalError, []);
    end;
end;

procedure TSymbolProg.AsmLoadESI_ESPPtr(shift: Integer);   // not used
begin
  with AddRecord do
  begin
    Size := 7;
    code[0] := $8B;
    code[1] := $B4;
    code[2] := $24;
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmLoadEDI_ESPPtr(shift: Integer);  // not used
begin
  with AddRecord do
  begin
    Size := 7;
    code[0] := $8B;
    code[1] := $BC;
    code[2] := $24;
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

function TSymbolProg.AsmGetREG_ESIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
begin
  if PAX64 then
    result := AsmGetREG64_RSIPtr(Reg, shift)
  else
    result := AsmGetREG32_ESIPtr(Reg, shift);
end;

function TSymbolProg.AsmGetREG64_RSIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  with result do
  begin
    Size := 7;
    if Reg in R64 then
      code[0] := $4C
    else
      code[0] := $48;
    code[1] := $8B;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

function TSymbolProg.AsmGetREG32_ESIPtr(Reg: Integer; shift: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  if Reg in R64 then
  with result do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $8B;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with result do
  begin
    Size := 6;
    code[0] := $8B;
    code[1] := GetMovESIPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG16_ESIPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 8;
    code[0] := $66;
    code[1] := $44;
    code[2] := $8B;
    code[3] := GetMovESIPtrCode(Reg);
    Move(shift, code[4], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $66;
    code[1] := $8B;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG8_ESIPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $8A;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $8A;
    code[1] := GetMovESIPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG_ESIPtr(Reg: Integer; shift: Integer);
begin
  if PAX64 then
    AsmPutREG64_RSIPtr(Reg, shift)
  else
    AsmPutREG32_ESIPtr(Reg, shift);
end;

procedure TSymbolProg.AsmPutREG64_RSIPtr(Reg: Integer; shift: Integer);
begin
  with AddRecord do
  begin
    Size := 7;
    if Reg in R64 then
      code[0] := $4C
    else
      code[0] := $48;
    code[1] := $89;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG32_ESIPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $89;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $89;
    code[1] := GetMovESIPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG16_ESIPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 8;
    code[0] := $66;
    code[1] := $44;
    code[2] := $89;
    code[3] := GetMovESIPtrCode(Reg);
    Move(shift, code[4], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $66;
    code[1] := $89;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG8_ESIPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $88;
    code[2] := GetMovESIPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $88;
    code[1] := GetMovESIPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmLeaReg32_RegPtr(Reg1, Reg2: Integer; shift: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $8D;
    case Reg1 of
      _EAX:
      case Reg2 of
        _EAX: code[1] := $80;
        _ECX: code[1] := $81;
        _EDX: code[1] := $82;
        _EBX: code[1] := $83;

        _EBP: code[1] := $85;
        _ESI: code[1] := $86;
        _EDI: code[1] := $87;
        else
          RaiseError(errInternalError, []);
      end;
      _ECX:
      case Reg2 of
        _EAX: code[1] := $88;
        _ECX: code[1] := $89;
        _EDX: code[1] := $8A;
        _EBX: code[1] := $8B;

        _EBP: code[1] := $8D;
        _ESI: code[1] := $8E;
        _EDI: code[1] := $8F;
        else
          RaiseError(errInternalError, []);
      end;
      _EDX:
      case Reg2 of
        _EAX: code[1] := $90;
        _ECX: code[1] := $91;
        _EDX: code[1] := $92;
        _EBX: code[1] := $93;

        _EBP: code[1] := $95;
        _ESI: code[1] := $96;
        _EDI: code[1] := $97;
        else
          RaiseError(errInternalError, []);
      end;
      _EBX:
      case Reg2 of
        _EAX: code[1] := $98;
        _ECX: code[1] := $99;
        _EDX: code[1] := $9A;
        _EBX: code[1] := $9B;

        _EBP: code[1] := $9D;
        _ESI: code[1] := $9E;
        _EDI: code[1] := $9F;
        else
          RaiseError(errInternalError, []);
      end;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG_EBPPtr(Reg: Integer; shift: Integer);
begin
  if PAX64 then
    AsmGetREG64_RBPPtr(Reg, shift)
  else
    AsmGetREG32_EBPPtr(Reg, shift);
end;

procedure TSymbolProg.AsmGetREG64_RBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $4C;
    code[1] := $8B;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $8B;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG32_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $8B;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $8B;
    code[1] := GetMovEBPPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG16_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 8;
    code[0] := $66;
    code[1] := $44;
    code[2] := $8B;
    code[3] := GetMovEBPPtrCode(Reg);
    Move(shift, code[4], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $66;
    code[1] := $8B;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG_EBPPtr(Reg: Integer; shift: Integer);
begin
  if PAX64 then
    AsmPutREG64_RBPPtr(Reg, shift)
  else
    AsmPutREG32_EBPPtr(Reg, shift);
end;

procedure TSymbolProg.AsmPutREG64_RBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $4C;
    code[1] := $89;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $89;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG32_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $89;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $89;
    code[1] := GetMovEBPPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG16_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 8;
    code[0] := $66;
    code[1] := $44;
    code[2] := $89;
    code[3] := GetMovEBPPtrCode(Reg);
    Move(shift, code[4], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 7;
    code[0] := $66;
    code[1] := $89;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmGetREG8_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $8A;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $8A;
    code[1] := GetMovEBPPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPutREG8_EBPPtr(Reg: Integer; shift: Integer);
begin
  if Reg in R64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $44;
    code[1] := $88;
    code[2] := GetMovEBPPtrCode(Reg);
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $88;
    code[1] := GetMovEBPPtrCode(Reg);
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFldDouble_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DD;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFldSingle_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D9;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFldExtended_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DB;
    case Reg of
      _EAX: code[1] := $28;
      _ECX: code[1] := $29;
      _EDX: code[1] := $2A;
      _EBX: code[1] := $2B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// Fld QWORD PTR [REG + Shift]
procedure TSymbolProg.AsmFldDouble_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  if PAX64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $DD;
    case Reg of
      _EAX: code[2] := $80;
      _ECX: code[2] := $81;
      _EDX: code[2] := $82;
      _EBX: code[2] := $83;
      _EBP: code[2] := $85;
      _ESI: code[2] := $86;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[3], 4);
    Decompile;
  end
  else // 32bit
  with AddRecord do
  begin
    Size := 6;
    code[0] := $DD;
    case Reg of
      _EAX: code[1] := $80;
      _ECX: code[1] := $81;
      _EDX: code[1] := $82;
      _EBX: code[1] := $83;
      _EBP: code[1] := $85;
      _ESI: code[1] := $86;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// Fld DWORD PTR [REG + Shift]
procedure TSymbolProg.AsmFldSingle_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  with AddRecord do
  begin
    Size := 6;
    code[0] := $D9;
    case Reg of
      _EAX: code[1] := $80;
      _ECX: code[1] := $81;
      _EDX: code[1] := $82;
      _EBX: code[1] := $83;
      _EBP: code[1] := $85;
      _ESI: code[1] := $86;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// Fld TBYTE PTR [REG + Shift]
procedure TSymbolProg.AsmFldExtended_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  with AddRecord do
  begin
    Size := 6;
    code[0] := $DB;
    case Reg of
      _EAX: code[1] := $A8;
      _ECX: code[1] := $A9;
      _EDX: code[1] := $AA;
      _EBX: code[1] := $AB;
      _EBP: code[1] := $AD;
      _ESI: code[1] := $AE;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFild_REG32Ptr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DB;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
      _ESI: code[1] := $06;
      _EDI: code[1] := $07;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFild_REG16Ptr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DF;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
      _ESI: code[1] := $06;
      _EDI: code[1] := $07;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFild_REG64Ptr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DF;
    case Reg of
      _EAX: code[1] := $28;
      _ECX: code[1] := $29;
      _EDX: code[1] := $2A;
      _EBX: code[1] := $2B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFistp_REG64Ptr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DF;
    case Reg of
      _EAX: code[1] := $38;
      _ECX: code[1] := $39;
      _EDX: code[1] := $3A;
      _EBX: code[1] := $3B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFAdd_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DC;
    case Reg of
      _EAX: code[1] := $00;
      _ECX: code[1] := $01;
      _EDX: code[1] := $02;
      _EBX: code[1] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFSub_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DC;
    case Reg of
      _EAX: code[1] := $20;
      _ECX: code[1] := $21;
      _EDX: code[1] := $22;
      _EBX: code[1] := $23;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFMul_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DC;
    case Reg of
      _EAX: code[1] := $08;
      _ECX: code[1] := $09;
      _EDX: code[1] := $0A;
      _EBX: code[1] := $0B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFMul_ESIPtr32(Shift: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $D8;
    code[1] := $8E;
    Move(Shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFDiv_ESIPtr32(Shift: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $D8;
    code[1] := $B6;
    Move(Shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFDiv_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DC;
    case Reg of
      _EAX: code[1] := $30;
      _ECX: code[1] := $31;
      _EDX: code[1] := $32;
      _EBX: code[1] := $33;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFComp_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D8;
    case Reg of
      _EAX: code[1] := $18;
      _ECX: code[1] := $19;
      _EDX: code[1] := $1A;
      _EBX: code[1] := $1B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFAdd;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DE;
    code[1] := $C1;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFSub;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DE;
    code[1] := $E9;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFMul;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DE;
    code[1] := $C9;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFDiv;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DE;
    code[1] := $F9;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFChs;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D9;
    code[1] := $E0;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFAbs;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D9;
    code[1] := $E1;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFCompP;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DE;
    code[1] := $D9;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFstsw_AX;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DF;
    code[1] := $E0;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSahv;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $9E;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCDQ;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $99;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetL_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $9C;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetLE_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $9E;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetNLE_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $9F;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetNL_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $9D;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetB_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $92;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSet_REGPtr(ASM_OP, Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  with AddRecord do
  begin
    shift := GetOffset(S);

    Size := 7;
    code[0] := $0F;

    if ASM_OP = ASM_SETB then code[1] := $92
    else if ASM_OP = ASM_SETNB then code[1] := $93
    else if ASM_OP = ASM_SETZ then code[1] := $94
    else if ASM_OP = ASM_SETNZ then code[1] := $95
    else if ASM_OP = ASM_SETBE then code[1] := $96
    else if ASM_OP = ASM_SETNBE then code[1] := $97
    else if ASM_OP = ASM_SETL then code[1] := $9C
    else if ASM_OP = ASM_SETNL then code[1] := $9D
    else if ASM_OP = ASM_SETLE then code[1] := $9E
    else if ASM_OP = ASM_SETNLE then code[1] := $9F
    else
      RaiseError(errInternalError, []);

    case Reg of
      _EAX: code[2] := $80;
      _ECX: code[2] := $81;
      _EDX: code[2] := $82;
      _EBX: code[2] := $83;
      _EBP: code[2] := $85;
      _ESI: code[2] := $86;
      else
        RaiseError(errInternalError, []);
    end;
    Move(shift, code[3], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetBE_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $96;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetNBE_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $97;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetNB_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $93;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetZ_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $94;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmSetNZ_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $0F;
    code[1] := $95;
    case Reg of
      _EAX: code[2] := $00;
      _ECX: code[2] := $01;
      _EDX: code[2] := $02;
      _EBX: code[2] := $03;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFstpDouble_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DD;
    case Reg of
      _EAX: code[1] := $18;
      _ECX: code[1] := $19;
      _EDX: code[1] := $1A;
      _EBX: code[1] := $1B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmFstpSingle_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $D9;
    case Reg of
      _EAX: code[1] := $18;
      _ECX: code[1] := $19;
      _EDX: code[1] := $1A;
      _EBX: code[1] := $1B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// FStp TBYTE PTR [REG]
procedure TSymbolProg.AsmFstpExtended_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $DB;
    case Reg of
      _EAX: code[1] := $38;
      _ECX: code[1] := $39;
      _EDX: code[1] := $3A;
      _EBX: code[1] := $3B;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// FStp QWORD PTR [REG + Shift]
procedure TSymbolProg.AsmFstpDouble_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  if PAX64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $DD;
    case Reg of
      _EAX: code[2] := $98;
      _ECX: code[2] := $99;
      _EDX: code[2] := $9A;
      _EBX: code[2] := $9B;
      _EBP: code[2] := $9D;
      _ESI: code[2] := $9E;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[3], 4);
    Decompile;
  end
  else
  with AddRecord do
  begin
    Size := 6;
    code[0] := $DD;
    case Reg of
      _EAX: code[1] := $98;
      _ECX: code[1] := $99;
      _EDX: code[1] := $9A;
      _EBX: code[1] := $9B;
      _EBP: code[1] := $9D;
      _ESI: code[1] := $9E;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// FStp DWORD PTR [REG + Shift]
procedure TSymbolProg.AsmFstpSingle_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  with AddRecord do
  begin
    Size := 6;
    code[0] := $D9;
    case Reg of
      _EAX: code[1] := $98;
      _ECX: code[1] := $99;
      _EDX: code[1] := $9A;
      _EBX: code[1] := $9B;
      _EBP: code[1] := $9D;
      _ESI: code[1] := $9E;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

// requires EmitGetAddressRegister
// FStp TBYTE PTR [REG + Shift]
procedure TSymbolProg.AsmFstpExtended_REGPtr(Reg: Integer; S: TSymbolRec);
var
  shift: Integer;
begin
  shift := GetOffset(S);
  if Reg in CommonRegisters then
    Shift := 0;

  if PAX64 then
  with AddRecord do
  begin
    Size := 7;
    code[0] := $48;
    code[1] := $DB;
    case Reg of
      _EAX: code[2] := $B8;
      _ECX: code[2] := $B9;
      _EDX: code[2] := $BA;
      _EBX: code[2] := $BB;
      _EBP: code[2] := $BD;
      _ESI: code[2] := $BE;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[3], 4);
    Decompile;
  end
  else //32bit
  with AddRecord do
  begin
    Size := 6;
    code[0] := $DB;
    case Reg of
      _EAX: code[1] := $B8;
      _ECX: code[1] := $B9;
      _EDX: code[1] := $BA;
      _EBX: code[1] := $BB;
      _EBP: code[1] := $BD;
      _ESI: code[1] := $BE;
    else
      RaiseError(errInternalError, []);
    end;
    Move(shift, code[2], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCmpByteREGPtr_Imm(Reg: Integer; value: Byte);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $80;
    case Reg of
      _EAX: code[1] := $38;
      _ECX: code[1] := $39;
      _EDX: code[1] := $3A;
      _EBX: code[1] := $3B;
    else
      RaiseError(errInternalError, []);
    end;
    code[2] := value;
    Decompile;
  end;
end;

function TSymbolProg.AsmJmp_REG(Reg: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  with result do
  begin
    Size := 2;
    code[0] := $FF;
    case Reg of
      _EAX: code[1] := $E0;
      _ECX: code[1] := $E1;
      _EDX: code[1] := $E2;
      _EBX: code[1] := $E3;
    else
    begin
      if Reg in [_R8.._R15] then
      begin
        Size := 3;
        code[0] := $49;
        code[1] := $FF;
        case Reg of
          _R8:  code[2] := $E0;
          _R9:  code[2] := $E1;
          _R10: code[2] := $E2;
          _R11: code[2] := $E3;
          _R12: code[2] := $E4;
          _R13: code[2] := $E5;
          _R14: code[2] := $E6;
          _R15: code[2] := $E7;
        end;
      end
      else
        RaiseError(errInternalError, []);
    end;
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCall_REG(Reg: Integer);
begin
  if PAX64 then
  begin
    with AddRecord do
    begin
      Size := 3;
      code[0] := $48;
      code[1] := $FF;
      case Reg of
        _EAX: code[2] := $D0;
        _ECX: code[2] := $D1;
        _EDX: code[2] := $D2;
        _EBX: code[2] := $D3;
        _ESP: code[2] := $D4;
        _EBP: code[2] := $D5;
        _ESI: code[2] := $D6;
        _EDI: code[2] := $D7;
      else
        RaiseError(errInternalError, []);
      end;
      Decompile;
    end;
  end
  else
  with AddRecord do
  begin
    Size := 2;
    code[0] := $FF;
    case Reg of
      _EAX: code[1] := $D0;
      _ECX: code[1] := $D1;
      _EDX: code[1] := $D2;
      _EBX: code[1] := $D3;
      _ESP: code[1] := $D4;
      _EBP: code[1] := $D5;
      _ESI: code[1] := $D6;
      _EDI: code[1] := $D7;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPush_Imm(value: Integer);
begin
  with AddRecord do
  begin
    Size := 5;
    code[0] := $68;
    Move(value, code[1], 4);
    Decompile;
  end;
end;

function TSymbolProg.AsmPush_REG(Reg: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  if Reg in R64 then
  with result do
  begin
    Size := 2;
    code[0] := $41;
    case Reg of
      _R8:  code[1] := $50;
      _R9:  code[1] := $51;
      _R10: code[1] := $52;
      _R11: code[1] := $53;
      _R12: code[1] := $54;
      _R13: code[1] := $55;
      _R14: code[1] := $56;
      _R15: code[1] := $57;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else
  with result do
  begin
    Size := 1;
    case Reg of
      _EAX: code[0] := $50;
      _ECX: code[0] := $51;
      _EDX: code[0] := $52;
      _EBX: code[0] := $53;
      _ESP: code[0] := $54;
      _EBP: code[0] := $55;
      _ESI: code[0] := $56;
      _EDI: code[0] := $57;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

function TSymbolProg.AsmPop_REG(Reg: Integer): TSymbolProgRec;
begin
  result := AddRecord;
  if Reg in R64 then
  with result do
  begin
    Size := 2;
    code[0] := $41;
    case Reg of
      _R8: code[1] := $58;
      _R9: code[1] := $59;
      _R10: code[1] := $5A;
      _R11: code[1] := $5B;
      _R12: code[1] := $5C;
      _R13: code[1] := $5D;
      _R14: code[1] := $5E;
      _R15: code[1] := $5F;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end
  else
  with result do
  begin
    Size := 1;
    case Reg of
      _EAX: code[0] := $58;
      _ECX: code[0] := $59;
      _EDX: code[0] := $5A;
      _EBX: code[0] := $5B;
      _ESP: code[0] := $5C;
      _EBP: code[0] := $5D;
      _ESI: code[0] := $5E;
      _EDI: code[0] := $5F;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPush_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $FF;
    case Reg of
      _EAX: code[1] := $30;
      _ECX: code[1] := $31;
      _EDX: code[1] := $32;
      _EBX: code[1] := $33;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPush_FS_REGPtr(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 3;
    code[0] := $64;
    code[1] := $FF;
    case Reg of
      _EAX: code[2] := $30;
      _ECX: code[2] := $31;
      _EDX: code[2] := $32;
      _EBX: code[2] := $33;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPush_Reg16(Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $66;
    case Reg of
      _EAX: code[1] := $50;
      _ECX: code[1] := $51;
      _EDX: code[1] := $52;
      _EBX: code[1] := $53;
    else
      RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmRet(value: Word = 0);
begin
  with AddRecord do
  begin
    if value = 0 then
    begin
      Size := 1;
      code[0] := $c3;
    end
    else
    begin
      Size := 3;
      code[0] := $c2;
      Move(value, code[1], 2);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmNop;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $90;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmWait;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $9B;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmClc;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $F8;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPushfd;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $9C;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmPopfd;
begin
  with AddRecord do
  begin
    Size := 1;
    code[0] := $9D;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmXCHG(Reg1, Reg2: Integer);
begin
  with AddRecord do
  begin
    if Reg2 = _EAX then
    begin
      Reg2 := Reg1;
      Reg1 := _EAX;
    end;

    if Reg1 = _EAX then
    begin
      Size := 1;
      case Reg2 of
        _EAX: code[0] := $90; // nop
        _ECX: code[0] := $91;
        _EDX: code[0] := $92;
        _EBX: code[0] := $93;
        _ESP: code[0] := $94;
        _EBP: code[0] := $95;
        _ESI: code[0] := $96;
        _EDI: code[0] := $97;
      else
        RaiseError(errInternalError, []);
      end;
    end
    else if Reg1 = _ECX then
    begin
      Size := 2;
      code[0] := $87;
      case Reg2 of
        _ECX: code[1] := $C9;
        _EDX: code[1] := $CA;
        _EBX: code[1] := $CB;
        _ESP: code[1] := $CC;
        _EBP: code[1] := $CD;
        _ESI: code[1] := $CE;
        _EDI: code[1] := $CF;
      else
        RaiseError(errInternalError, []);
      end;
    end
    else if Reg1 = _EDX then
    begin
      Size := 2;
      code[0] := $87;
      case Reg2 of
        _ECX: code[1] := $D1;
        _EDX: code[1] := $D2;
        _EBX: code[1] := $D3;
        _ESP: code[1] := $D4;
        _EBP: code[1] := $D5;
        _ESI: code[1] := $D6;
        _EDI: code[1] := $D7;
      else
        RaiseError(errInternalError, []);
      end;
    end
    else if Reg1 = _EBX then
    begin
      Size := 2;
      code[0] := $87;
      case Reg2 of
        _ECX: code[1] := $D9;
        _EDX: code[1] := $DA;
        _EBX: code[1] := $DB;
        _ESP: code[1] := $DC;
        _EBP: code[1] := $DD;
        _ESI: code[1] := $DE;
        _EDI: code[1] := $DF;
      else
        RaiseError(errInternalError, []);
      end;
    end
    else
      RaiseError(errInternalError, []);

    Decompile;
  end;
end;

procedure TSymbolProg.AsmJMP_Imm(value: Integer);
begin
  with AddRecord do
  begin
    Size := 5;
    code[0] := $E9;
    Move(value, code[1], 4);
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJNO_Imm(value: Byte);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $71;
    code[1] := value;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJNC_Imm(value: Byte);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $73;
    code[1] := value;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJZ_Imm(value: SmallInt);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $74;
    code[1] := value;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJNZ_Imm(value: SmallInt);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $75;
    code[1] := value;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJBE_Imm(value: Byte);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $76;
    code[1] := value;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmJNLE_Imm(value: Byte);
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $7F;
    code[1] := value;
    Decompile;
  end;
end;


procedure TSymbolProg.AsmRep_MOVSB;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F3;
    code[1] := $A4;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmRep_MOVSD;
begin
  with AddRecord do
  begin
    Size := 2;
    code[0] := $F3;
    code[1] := $A5;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCvtsd2ssXMM_RegPtr(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 4;
    code[0] := $F2;
    code[1] := $0F;
    code[2] := $5A;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[3] := $00;
          _ECX: code[3] := $01;
          _EDX: code[3] := $02;
          _EBX: code[3] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[3] := $08;
          _ECX: code[3] := $09;
          _EDX: code[3] := $0A;
          _EBX: code[3] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[3] := $10;
          _ECX: code[3] := $11;
          _EDX: code[3] := $12;
          _EBX: code[3] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[3] := $18;
          _ECX: code[3] := $19;
          _EDX: code[3] := $1A;
          _EBX: code[3] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[3] := $20;
          _ECX: code[3] := $21;
          _EDX: code[3] := $22;
          _EBX: code[3] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmCvtss2sdXMM_RegPtr(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 4;
    code[0] := $F3;
    code[1] := $0F;
    code[2] := $5A;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[3] := $00;
          _ECX: code[3] := $01;
          _EDX: code[3] := $02;
          _EBX: code[3] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[3] := $08;
          _ECX: code[3] := $09;
          _EDX: code[3] := $0A;
          _EBX: code[3] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[3] := $10;
          _ECX: code[3] := $11;
          _EDX: code[3] := $12;
          _EBX: code[3] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[3] := $18;
          _ECX: code[3] := $19;
          _EDX: code[3] := $1A;
          _EBX: code[3] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[3] := $20;
          _ECX: code[3] := $21;
          _EDX: code[3] := $22;
          _EBX: code[3] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovsdXMM_RegPtr(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $67;
    code[1] := $F2;
    code[2] := $48;
    code[3] := $0F;
    code[4] := $10;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[5] := $00;
          _ECX: code[5] := $01;
          _EDX: code[5] := $02;
          _EBX: code[5] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[5] := $08;
          _ECX: code[5] := $09;
          _EDX: code[5] := $0A;
          _EBX: code[5] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[5] := $10;
          _ECX: code[5] := $11;
          _EDX: code[5] := $12;
          _EBX: code[5] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[5] := $18;
          _ECX: code[5] := $19;
          _EDX: code[5] := $1A;
          _EBX: code[5] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[5] := $20;
          _ECX: code[5] := $21;
          _EDX: code[5] := $22;
          _EBX: code[5] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovsdRegPtr_XMM(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 6;
    code[0] := $67;
    code[1] := $F2;
    code[2] := $48;
    code[3] := $0F;
    code[4] := $11;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[5] := $00;
          _ECX: code[5] := $01;
          _EDX: code[5] := $02;
          _EBX: code[5] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[5] := $08;
          _ECX: code[5] := $09;
          _EDX: code[5] := $0A;
          _EBX: code[5] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[5] := $10;
          _ECX: code[5] := $11;
          _EDX: code[5] := $12;
          _EBX: code[5] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[5] := $18;
          _ECX: code[5] := $19;
          _EDX: code[5] := $1A;
          _EBX: code[5] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[5] := $20;
          _ECX: code[5] := $21;
          _EDX: code[5] := $22;
          _EBX: code[5] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovssXMM_RegPtr(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 5;
    code[0] := $67;
    code[1] := $F3;
    code[2] := $0F;
    code[3] := $10;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[4] := $00;
          _ECX: code[4] := $01;
          _EDX: code[4] := $02;
          _EBX: code[4] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[4] := $08;
          _ECX: code[4] := $09;
          _EDX: code[4] := $0A;
          _EBX: code[4] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[4] := $10;
          _ECX: code[4] := $11;
          _EDX: code[4] := $12;
          _EBX: code[4] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[4] := $18;
          _ECX: code[4] := $19;
          _EDX: code[4] := $1A;
          _EBX: code[4] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[4] := $20;
          _ECX: code[4] := $21;
          _EDX: code[4] := $22;
          _EBX: code[4] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;

procedure TSymbolProg.AsmMovssRegPtr_XMM(XMMReg, Reg: Integer);
begin
  with AddRecord do
  begin
    Size := 5;
    code[0] := $67;
    code[1] := $F3;
    code[2] := $0F;
    code[3] := $11;
    case XMMReg of
      _XMM0:
        case Reg of
          _EAX: code[4] := $00;
          _ECX: code[4] := $01;
          _EDX: code[4] := $02;
          _EBX: code[4] := $03;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM1:
        case Reg of
          _EAX: code[4] := $08;
          _ECX: code[4] := $09;
          _EDX: code[4] := $0A;
          _EBX: code[4] := $0B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM2:
        case Reg of
          _EAX: code[4] := $10;
          _ECX: code[4] := $11;
          _EDX: code[4] := $12;
          _EBX: code[4] := $13;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM3:
        case Reg of
          _EAX: code[4] := $18;
          _ECX: code[4] := $19;
          _EDX: code[4] := $1A;
          _EBX: code[4] := $1B;
          else
            RaiseError(errInternalError, []);
        end;
      _XMM4:
        case Reg of
          _EAX: code[4] := $20;
          _ECX: code[4] := $21;
          _EDX: code[4] := $22;
          _EBX: code[4] := $23;
          else
            RaiseError(errInternalError, []);
        end;
      else
        RaiseError(errInternalError, []);
    end;
    Decompile;
  end;
end;


function TSymbolProg.GetRecord(I: Integer): TSymbolProgRec;
begin
  result := TSymbolProgRec(L[I - 1]);
end;

procedure TSymbolProg.Optimization;
var
  I: Integer;
begin
  for I:= 1 to Card - 2 do
    if Records[I].Op = ASM_MOV then
    begin

      if Records[I + 1].Op = ASM_MOV then
      begin
        if EqualArgs(Records[I].Arg1, Records[I+1].Arg2) and
           EqualArgs(Records[I].Arg2, Records[I+1].Arg1) then
        begin
          Records[I+1].Size := 0;
        end;
      end
      else if (Records[I + 2].Op = ASM_MOV) and (Records[I + 1].Size < 0) then
      begin
        if EqualArgs(Records[I].Arg1, Records[I+2].Arg2) and
           EqualArgs(Records[I].Arg2, Records[I+2].Arg1) then
        begin
          Records[I+2].Size := -1;
        end;
      end
    end;

  for I:=Card downto 1 do
    if Records[I].Size = 0 then
      Delete(I);
end;

procedure TSymbolProg.CreateProgramSimple(result: TProgram);
var
  I, N, SZ: Integer;
begin
  result.AllocateSimple(ProgSize, 0);
  N := 0;
  for I:=1 to Card do
  begin
    SZ := Records[I].Size;
    if SZ >= 0 then
    begin
      Move(Records[I].code, result.CodePtr^[N], SZ);
      Inc(N, SZ);
    end;
  end;
end;

procedure TSymbolProg.CreateProgram(result: TProgram; IsEval: Boolean = false);
var
  I, J, K, N, SZ: Integer;
  SymbolTable: TSymbolTable;
  Shift: Integer;
  ProgOffset: Integer;

  Code: TCode;
  OP: Integer;
  ID: Integer;
  TryRec: TTryRec;
  ClsIndex: Integer;
  ClassRec: TClassRec;
  DestructorId,
  AfterConstructionId,
  BeforeDestructionId,
  SafeCallExceptionId,
  DispatchId,
  DefaultHandlerId,
  NewInstanceId,
  ToStringId,
  FreeInstanceId : Integer;
  RR: TSymbolRec;
  temp: Boolean;
  IntfId: Integer;
  InterfaceMethodIds: TIntegerList;
  ClassMethodIds: TIntegerList;
  IntfRec: TIntfRec;

  UpdatedProgSize, LastCard, NN: Integer;
  PaxInfo: PPaxInfo;
  PaxFactoryRec: TPaxClassFactoryRec;
  SS: String;
  InterfaceToObjectOffset: Integer;
  RegX, RegY: Integer;
  N1, N2: Integer;
  MR: TMapRec;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  Code := TKernel(kernel).Code;

  if TKernel(kernel).SignCompression then
    SZ := TKernel(kernel).OffsetList.GetSize
  else
    SZ := SymbolTable.GetDataSize + 4096;

  temp := result.UseMapping;
  result.UseMapping := false;
  result.Allocate(ProgSize, SZ);
  result.UseMapping := temp;

  N := 0;
  N1 := 0;
  for I:=1 to Card do
  begin
    SZ := Records[I].Size;
    if SZ >= 0 then
    begin
      Records[I].ProgOffset := N;

      Move(Records[I].code, result.CodePtr^[N], SZ);
      Inc(N, SZ);
    end
    else if Records[I].MapSub < 0 then
    begin
      N1 := N;
    end
    else if Records[I].MapSub > 0 then
    begin
      N2 := N;
      MR := result.ScriptMapTable.LookupSub(Records[I].MapSub);
      if MR <> nil then
        MR.SubDesc.SubSize := N2 - N1;
    end;
  end;

  LastCard := Card;
  NN := N;
  UpdatedProgSize := ProgSize;

  TKernel(kernel).AllocateConstants(result.ResultPtr);

  with SymbolTable do
  for I:=FirstLocalId + 1 to Card do
    if Records[I].Host then
    begin
      RR := SymbolTable[I];

      if RR.Address <> nil then
      begin
        if not InCode[I] then
          continue;

        if not TKernel(kernel).ExistsOffset(RR) then
        begin
          continue;
        end;

        result.SetAddress(GetOffset(RR), RR.Address);
      end
      else if RR.ClassIndex <> -1 then
      begin
        RR := SymbolTable[I + 1]; // cls ref
        J := RR.Value;
        if J = 0 then
        begin
          ClassRec := result.ClassList.Add(SymbolTable[I].FullName, SymbolTable[I].Host);
          ClassRec.InstSize := SizeOfPointer;
        end
        else
        begin
          ClassRec := result.RegisterClass(TClass(Pointer(J)), SymbolTable[I].FullName, GetOffset(RR));
          ClassRec.InstSize := TClass(Pointer(J)).InstanceSize;
        end;
        ClassRec.ParentFullName := Records[Records[I].AncestorId].FullName;
      end;
    end
    else
      if Records[I].ClassIndex > 0 then
      begin
        ClassRec := result.ClassList.Add(Records[I].FullName, Records[I].Host);
        ClassRec.Host := false;
        ClassRec.Offset := GetOffset(Records[I + 1]);
        ClassRec.SizeOfScriptClassFields := Records[I].GetSizeOfScriptClassFields;

        ClassRec.PClass := TClass(IntPax(Records[I + 1].Value));
        ClassRec.ParentFullName := Records[Records[I].AncestorId].FullName;

        DestructorId := FindDestructorId(I);
        if DestructorId > 0 then
          ClassRec.DestructorProgOffset := Records[DestructorId].Value;

        SafeCallExceptionId := Lookup('SafeCallException', I, true);
        if SafeCallExceptionId > 0 then
          ClassRec.SafeCallExceptionProgOffset := Records[SafeCallExceptionId].Value;

        AfterConstructionId := Lookup('AfterConstruction', I, true);
        if AfterConstructionId > 0 then
          ClassRec.AfterConstructionProgOffset := Records[AfterConstructionId].Value;

        BeforeDestructionId := Lookup('BeforeDestruction', I, true);
        if BeforeDestructionId > 0 then
          ClassRec.BeforeDestructionProgOffset := Records[BeforeDestructionId].Value;

        DispatchId := Lookup('Dispatch', I, true);
        if DispatchId > 0 then
          ClassRec.DispatchProgOffset := Records[DispatchId].Value;

        DefaultHandlerId := Lookup('DefaultHandler', I, true);
        if DefaultHandlerId > 0 then
          ClassRec.DefaultHandlerProgOffset := Records[DefaultHandlerId].Value;

        NewInstanceId := Lookup('NewInstance', I, true);
        if NewInstanceId > 0 then
          ClassRec.NewInstanceProgOffset := Records[NewInstanceId].Value;

        FreeInstanceId := Lookup('FreeInstance', I, true);
        if FreeInstanceId > 0 then
          ClassRec.FreeInstanceProgOffset := Records[FreeInstanceId].Value;

{$IFDEF UNIC}
        ToStringId := Lookup('ToString', I, true);
        if ToStringId > 0 then
          ClassRec.ToStringProgOffset := Records[ToStringId].Value;
{$ENDIF}

        PaxInfo := GetPaxInfo(ClassRec.PClass);
        if PaxInfo = nil then
          RaiseError(errInternalError, []);

        if not IsEval then
        begin
          PaxInfo^.Prog := result;
          PaxInfo^.ClassIndex := Records[I].ClassIndex;
        end;

        PaxFactoryRec := TKernel(kernel).ClassFactory.FindRecord(ClassRec.PClass);
        if PaxFactoryRec = nil then
          RaiseError(errInternalError, []);

        if Records[I].SupportedInterfaces = nil then
        begin
          ClassRec.InstSize := Records[I].GetSizeOfAllClassFields(result);
          Inc(ClassRec.InstSize, SizeOf(Pointer)); // add monitor space
          PaxFactoryRec.SetInstanceSize(ClassRec.InstSize);
          continue;
        end
        else
        begin
          ClassRec.InstSize := Records[I].GetSizeOfAllClassFields(result) +
            Records[I].SupportedInterfaces.Count * SizeOfPointer;
          Inc(ClassRec.InstSize, SizeOf(Pointer)); // add monitor space
          PaxFactoryRec.SetInstanceSize(ClassRec.InstSize);
        end;

        if Records[I].SupportedInterfaces.Count = 0 then
          continue;

        InterfaceMethodIds := TIntegerList.Create;
        ClassMethodIds := TIntegerList.Create;
        try
          for J:=0 to Records[I].SupportedInterfaces.Count - 1 do
          begin

            IntfId := Records[I].SupportedInterfaces[J].Id;
            CreateInterfaceMethodList(I, IntfId,
                                    InterfaceMethodIds,
                                    ClassMethodIds);
            if (not Records[I].IsAbstract) and (InterfaceMethodIds.Count > 0) then // some methods not found
            begin
              for K:=0 to InterfaceMethodIds.Count - 1 do
              begin
                Code.N := Code.FindRecord1(OP_END_CLASS_TYPE, I);
                CreateError(errUndeclaredIdentifier,
                   [Records[InterfaceMethodIds[K]].Name]);
              end;
              break;
            end
            else
            begin
              InterfaceToObjectOffset :=
                   -Records[I].GetSizeOfAllClassFields(result) + J * SizeOfPointer;

              IntfRec := ClassRec.IntfList.Add;
              IntfRec.GUID := Records[I].SupportedInterfaces[J].GUID;
              for K:=0 to ClassMethodIds.Count - 1 do
              begin
                Id := ClassMethodIds[K];

                IntfRec.IntfMethods.AddMethod(Records[Id].FullName,
                                              UpdatedProgSize,
                                              InterfaceToObjectOffset);
                Inc(UpdatedProgSize, EmitZ);

{$IFDEF PAX64}
                RegX := _R10;
                RegY := _R11;
{$ELSE}
                if Records[Id].Host then
                  RegX := _EDI
                else
                  RegX := _ESI;
                RegY := _EBX;
{$ENDIF}

                if Records[Id].CallConv in [ccSTDCALL, ccCDECL, ccPASCAL, ccSAFECALL] then
                begin
                  if Records[Id].ExtraParamNeeded then
                    Inc(UpdatedProgSize, AsmPop_REG(RegX).Size);

                  Inc(UpdatedProgSize, AsmPop_REG(RegY).Size);
                  Inc(UpdatedProgSize, AsmPop_REG(_EAX).Size);
                end;

                if PAX64 then
                  Inc(UpdatedProgSize, AsmAddREG_Imm(_ECX, InterfaceToObjectOffset).Size)
                else
                  Inc(UpdatedProgSize, AsmAddREG_Imm(_EAX, InterfaceToObjectOffset).Size);
                // EAX contains instance of object

                if Records[Id].CallConv in [ccSTDCALL, ccCDECL, ccPASCAL, ccSAFECALL] then
                begin
                  Inc(UpdatedProgSize, AsmPush_REG(_EAX).Size);
                  Inc(UpdatedProgSize, AsmPush_REG(RegY).Size);
                  if Records[Id].ExtraParamNeeded then
                    Inc(UpdatedProgSize, AsmPush_REG(RegX).Size);
                end;

                // jump to address

                if Records[Id].Host then
                begin
                  Inc(UpdatedProgSize, AsmGetREG_ESIPtr(RegY, GetOffset(Records[Id])).Size);
                end
                else
                begin
                  Inc(UpdatedProgSize, AsmMovREG_REG(RegY, _EDI).Size);
                  Inc(UpdatedProgSize, AsmAddREG_Imm(RegY, Records[Id].Value).Size);
                end;
                Inc(UpdatedProgSize, AsmJMP_REG(RegY).Size);
              end;
            end;
          end;
        finally
          FreeAndNil(InterfaceMethodIds);
          FreeAndNil(ClassMethodIds);
        end;
      end;

  result.Reallocate(UpdatedProgSize);

  for I:= LastCard + 1 to Card do
  begin
    SZ := Records[I].Size;
    if SZ >= 0 then
    begin
      Records[I].ProgOffset := NN;

      Move(Records[I].code, result.CodePtr^[NN], SZ);
      Inc(NN, SZ);
    end;
  end;

  result.TryList.Clear;
  result.RootTryStack.Clear;

  for I:=1 to Code.Card do
    if Code[I].Op = OP_TRY_ON then
    begin
      TryRec := result.TryList.Add;
      TryRec.Level := Code[I].Res;

      for J := I + 1 to Code.Card do
      begin
        Op := Code[J].Op;
        if OP = OP_FINALLY then
        begin
          if Code[J].Arg1 = Code[I].Arg1 then
          begin
            TryRec.TryKind := tryFinally;
            ID := Code[J].Arg2;
            TryRec.ProgOffset := SymbolTable[ID].Value;
            break;
          end;
        end
        else if OP = OP_EXCEPT then
        begin
          if Code[J].Arg1 = Code[I].Arg1 then
          begin
            TryRec.TryKind := tryExcept;
            ID := Code[J].Arg2;
            TryRec.ProgOffset := SymbolTable[ID].Value;
            break;
          end;
        end
        else if OP = OP_TRY_OFF then
        begin
          if Code[J].Arg1 = Code[I].Arg1 then
          begin
            break;
          end;
        end;
      end;
    end;

  for I:=1 to Code.Card do
  begin
    OP := Code[I].Op;
    if OP = OP_FINALLY then
    begin
      ID := Code[I].Arg2;
      ProgOffset := SymbolTable[ID].Value;

      TryRec := result.TryList[Code[I].Arg1];

      TryRec.TryKind := tryFinally;
      TryRec._ESP := 0;                    // will be determined at run-time
      TryRec._EBP := 0;                    // will be determined at run-time
      TryRec.ProgOffset := ProgOffset;

      TryRec.N := I;

      if (Code[I].BreakLabel > 0) and (Code[I].ContinueLabel > 0) then
      begin
        Id := Code[I].BreakLabel;
        TryRec.BreakOffset := SymbolTable[ID].Value;

        Id := Code[I].ContinueLabel;
        TryRec.ContinueOffset := SymbolTable[ID].Value;

        Id := Code[I].LoopLabel;
        N1 := SymbolTable[ID].Value; // begin loop offset
        N2 := TryRec.BreakOffset; // break offset (end loop offset)
        for J := Code[I].Arg1 - 1 downto 0 do
          if result.TryList[J].TryKind = tryFinally then
          begin
            if result.TryList[J].ProgOffset >= N1 then
              if result.TryList[J].ProgOffset <= N2 then
              begin
                TryRec.BreakOffset := 0;
                TryRec.ContinueOffset := 0;
              end;
          end;
      end;
    end
    else if OP = OP_EXCEPT then
    begin
      ID := Code[I].Arg2;
      ProgOffset := SymbolTable[ID].Value;

      TryRec := result.TryList[Code[I].Arg1];

      TryRec.TryKind := tryExcept;
      TryRec._ESP := 0;                    // will be determined at run-time
      TryRec._EBP := 0;                    // will be determined at run-time
      TryRec.ProgOffset := ProgOffset;

      TryRec.N := I;

      if (Code[I].BreakLabel > 0) and (Code[I].ContinueLabel > 0) then
      begin
        Id := Code[I].BreakLabel;
        TryRec.BreakOffset := SymbolTable[ID].Value;

        Id := Code[I].ContinueLabel;
        TryRec.ContinueOffset := SymbolTable[ID].Value;

        Id := Code[I].LoopLabel;
        N1 := SymbolTable[ID].Value; // begin loop offset
        N2 := TryRec.BreakOffset; // break offset (end loop offset)
        for J := Code[I].Arg1 - 1 downto 0 do
          if result.TryList[J].TryKind = tryFinally then
          begin
            if result.TryList[J].ProgOffset >= N1 then
              if result.TryList[J].ProgOffset <= N2 then
              begin
                TryRec.BreakOffset := result.TryList[J].ProgOffset;
                TryRec.ContinueOffset := result.TryList[J].ProgOffset;
              end;
          end;
      end;
    end
    else if OP = OP_EXCEPT_ON then
    begin
      ID := Code[I].Arg2;
      ProgOffset := SymbolTable[ID].Value;

      TryRec := result.TryList[Code[I].Arg1];

      if Code[I].Res = 0 then
        ClsIndex := -1  // else part
      else
      begin
        ID := SymbolTable[Code[I].Res].TerminalTypeId;
        ClsIndex := SymbolTable[ID].ClassIndex;

        if ClsIndex <= 0 then
          RaiseError(errInternalError, []);
      end;

      TryRec.ExceptOnInfo.Add(ClsIndex, ProgOffset);

      TryRec.N := I;
    end;
  end;

  TKernel(kernel).CreateRTI(result);
  CreateZList(result);

  if not IsEval then
  begin
    SymbolTable.ProcessClassFactory(Tkernel(kernel).ClassFactory, result);
    TKernel(kernel).ClassFactory.SetupStdVirtuals(result.ClassList, result.CodePtr);
    result.SetupInterfaces(result.CodePtr);
    result.ProgTypeInfoList.AddToProgram(result);
  end;

  for I:= 1 to Card do
    if Records[I].MustBeFixed then
    begin
      if SymbolTable[Records[I].SubId].MethodIndex = 0 then
      begin
        SS := SymbolTable[Records[I].SubId].Name;

        if StrEql(SS, 'SafeCallException') then
          Shift := -32
        else if StrEql(SS, 'AfterConstruction') then
          Shift := -28
        else if StrEql(SS, 'BeforeDestruction') then
          Shift := -24
        else if StrEql(SS, 'Dispatch') then
          Shift := -20
        else if StrEql(SS, 'DefaultHandler') then
          Shift := -16
        else if StrEql(SS, 'NewInstance') then
          Shift := -12
        else if StrEql(SS, 'FreeInstance') then
          Shift := -8
        else if StrEql(SS, 'Destroy') then
          Shift := -4

{$IFDEF UNIC}
        else if StrEql(SS, 'ToString') then
          Shift := -36
        else if StrEql(SS, 'GetHashCode') then
          Shift := -40
        else if StrEql(SS, 'Equals') then
          Shift := -44
{$ENDIF}

        else
        begin
          TKernel(kernel).CreateError(errInternalErrorMethodIndex, []);
        end;
      end
      else
        Shift := (SymbolTable[Records[I].SubId].MethodIndex - 1) * 4;
      Move(Shift, Records[I].Code[Records[I].OpOffset], 4);
      Records[I].Decompile;
      Move(Shift, ShiftPointer(result.CodePtr, Records[I].ProgOffset + Records[I].OpOffset)^, 4);
    end;
end;

procedure TSymbolProg.CreateZList(P: TProgram);
var
  I, S, SZ: Integer;
  R: TSymbolProgRec;
begin
  P.ZList.Clear;
{

  for I:=1 to Card do
  begin
    R := Records[I];
    if R.Z then
    begin
      S := GetShiftOfRecord(R);
      P.ZList.Add(S);
    end;
  end;
}
  S := 0;
  for I:=1 to Card do
  begin
    R := Records[I];

    if R.Z then
      P.ZList.Add(S);

    SZ := R.Size;
    if SZ > 0 then
      S := S + SZ;
  end;

  P.SetZList;
end;

procedure TSymbolProg.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

procedure TSymbolProg.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

function TSymbolProg.GetOffset(S: TSymbolRec): Integer;
begin
  result := TKernel(kernel).GetOffset(S);
end;

function TSymbolProg.EmitGetCallerEIP: Integer;
begin
  AsmGetREG32_ESIPtr(_EAX, GetOffset(TKernel(kernel).SymbolTable[Id_GetAddressGetCallerEIP]));
  //6
  AsmSubREG_Imm(_ESP, 12);//6
  AsmCall_REG(_EAX); //2
  AsmCall_REG(_EAX); //2
  AsmAddREG_Imm(_ESP, 12); //6
  result := 6 + 6 + 2 + 2 + 6;
end;

function TSymbolProg.EmitZ: Integer;
var
  R: TSymbolProgRec;
begin
  R := AsmMovREG_Imm(_EDI, 400000); // 5 or 10       CodePtr
  R.Z := true;
  AsmMovREG_Imm(_ESI, 400000);      // 5 or 10      DataPtr
  if TKernel(kernel).TargetPlatform = tpWin64 then
    result := 20
  else
    result := 10;
end;

function TSymbolProg.GetPAX64: Boolean;
begin
  if kernel = nil then
    result := false
  else
    result := TKernel(kernel).PAX64;
end;

function TSymbolProg.GetSizeOfPointer: Integer;
begin
  if PAX64 then
    result := 8
  else
    result := 4;
end;

end.
