////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_DISASM.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_DISASM;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS;

type
  TArg = packed record
    valid: Boolean;
    Reg: Byte;
    Ptr: Boolean;
    sz: Byte;
    val: Int64;
    FS: Boolean;
  end;

procedure Decomp(P: Pointer; var Length: Integer; var op: Integer;
                                                  var Arg1: TArg;
                                                  var Arg2: TArg;
                                                  PAX64: Boolean);
procedure ClearArg(var Arg: TArg);
function EqualArgs(const Arg1, Arg2: TArg): Boolean;

procedure SaveDecompiledCode(P: Pointer;
  CodeSize: Integer; const FileName: String; PAX64: Boolean);
function ArgToString(const Arg: TArg; PAX64: Boolean): String;

procedure DiscardDebugMode(P: Pointer; CodeSize: Integer; PAX64: Boolean);
function GetInitializationOffset(P: Pointer; CodeSize: Integer; PAX64: Boolean): Integer;
function GetFinalizationOffset(P: Pointer; CodeSize: Integer; PAX64: Boolean): Integer;

implementation

procedure RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

function GetNextByte(var P: Pointer): Byte;
begin
  P := ShiftPointer(P, 1);
  result := Byte(P^);
end;

function AssignXMM_RegPtr(B: Byte; var Arg1: TArg; var Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $00:
    begin
      Arg1.Reg := _XMM0;
      Arg2.Reg := _EAX;
    end;
    $01:
    begin
      Arg1.Reg := _XMM0;
      Arg2.Reg := _ECX;
    end;
    $02:
    begin
      Arg1.Reg := _XMM0;
      Arg2.Reg := _EDX;
    end;
    $03:
    begin
      Arg1.Reg := _XMM0;
      Arg2.Reg := _EBX;
    end;
    $08:
    begin
      Arg1.Reg := _XMM1;
      Arg2.Reg := _EAX;
    end;
    $09:
    begin
      Arg1.Reg := _XMM1;
      Arg2.Reg := _ECX;
    end;
    $0A:
    begin
      Arg1.Reg := _XMM1;
      Arg2.Reg := _EDX;
    end;
    $0B:
    begin
      Arg1.Reg := _XMM1;
      Arg2.Reg := _EBX;
    end;
    $10:
    begin
      Arg1.Reg := _XMM2;
      Arg2.Reg := _EAX;
    end;
    $11:
    begin
      Arg1.Reg := _XMM2;
      Arg2.Reg := _ECX;
    end;
    $12:
    begin
      Arg1.Reg := _XMM2;
      Arg2.Reg := _EDX;
    end;
    $13:
    begin
      Arg1.Reg := _XMM2;
      Arg2.Reg := _EBX;
    end;
    $18:
    begin
      Arg1.Reg := _XMM3;
      Arg2.Reg := _EAX;
    end;
    $19:
    begin
      Arg1.Reg := _XMM3;
      Arg2.Reg := _ECX;
    end;
    $1A:
    begin
      Arg1.Reg := _XMM3;
      Arg2.Reg := _EDX;
    end;
    $1B:
    begin
      Arg1.Reg := _XMM3;
      Arg2.Reg := _EBX;
    end;
    $20:
    begin
      Arg1.Reg := _XMM4;
      Arg2.Reg := _EAX;
    end;
    $21:
    begin
      Arg1.Reg := _XMM4;
      Arg2.Reg := _ECX;
    end;
    $22:
    begin
      Arg1.Reg := _XMM4;
      Arg2.Reg := _EDX;
    end;
    $23:
    begin
      Arg1.Reg := _XMM4;
      Arg2.Reg := _EBX;
    end;
    else
    begin
      result := false;
    end;
  end;
end;

function AssignRegMovESIPtr(B: Byte; var Arg: TArg): Boolean;
begin
  result := true;
  case B of
    $86: Arg.Reg := _EAX;
    $8E: Arg.Reg := _ECX;
    $96: Arg.Reg := _EDX;
    $9E: Arg.Reg := _EBX;
    $A6: Arg.Reg := _ESP;
    $AE: Arg.Reg := _EBP;
    $B6: Arg.Reg := _ESI;
    $BE: Arg.Reg := _EDI;
    else
      result := false;
  end;
end;

function AssignRegMovEBPPtr(B: Byte; var Arg: TArg): Boolean;
begin
  result := true;
  case B of
    $85: Arg.Reg := _EAX;
    $8D: Arg.Reg := _ECX;
    $95: Arg.Reg := _EDX;
    $9D: Arg.Reg := _EBX;
    $A5: Arg.Reg := _ESP;
    $AD: Arg.Reg := _EBP;
    $B5: Arg.Reg := _ESI;
    $BD: Arg.Reg := _EDI;
    else
      result := false;
  end;
end;

function AssignRegMovRSIPtr(B: Byte; var Arg: TArg): Boolean;
begin
  result := true;
  case B of
    $86: Arg.Reg := _R8;
    $8E: Arg.Reg := _R9;
    $96: Arg.Reg := _R10;
    $9E: Arg.Reg := _R11;
    $A6: Arg.Reg := _R12;
    $AE: Arg.Reg := _R13;
    $B6: Arg.Reg := _R14;
    $BE: Arg.Reg := _R15;
    else
      result := false;
  end;
end;

function AssignRegMovRBPPtr(B: Byte; var Arg: TArg): Boolean;
begin
  result := true;
  case B of
    $85: Arg.Reg := _R8;
    $8D: Arg.Reg := _R9;
    $95: Arg.Reg := _R10;
    $9D: Arg.Reg := _R11;
    $A5: Arg.Reg := _R12;
    $AD: Arg.Reg := _R13;
    $B5: Arg.Reg := _R14;
    $BD: Arg.Reg := _R15;
    else
      result := false;
  end;
end;

function AssignMovR32_R64(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
    $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _R9; end;
    $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _R10; end;
    $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _R11; end;
    $E0: begin Arg1.Reg := _EAX; Arg2.Reg := _R12; end;
    $E8: begin Arg1.Reg := _EAX; Arg2.Reg := _R13; end;
    $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _R14; end;
    $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _R15; end;

    $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _R8; end;
    $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _R9; end;
    $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _R10; end;
    $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _R11; end;
    $E1: begin Arg1.Reg := _ECX; Arg2.Reg := _R12; end;
    $E9: begin Arg1.Reg := _ECX; Arg2.Reg := _R13; end;
    $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _R14; end;
    $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _R15; end;

    $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _R8; end;
    $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _R9; end;
    $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _R10; end;
    $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _R11; end;
    $E2: begin Arg1.Reg := _EDX; Arg2.Reg := _R12; end;
    $EA: begin Arg1.Reg := _EDX; Arg2.Reg := _R13; end;
    $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _R14; end;
    $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _R15; end;

    $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _R8; end;
    $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _R9; end;
    $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _R10; end;
    $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _R11; end;
    $E3: begin Arg1.Reg := _EBX; Arg2.Reg := _R12; end;
    $EB: begin Arg1.Reg := _EBX; Arg2.Reg := _R13; end;
    $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _R14; end;
    $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _R15; end;

    $C4: begin Arg1.Reg := _ESP; Arg2.Reg := _R8; end;
    $CC: begin Arg1.Reg := _ESP; Arg2.Reg := _R9; end;
    $D4: begin Arg1.Reg := _ESP; Arg2.Reg := _R10; end;
    $DC: begin Arg1.Reg := _ESP; Arg2.Reg := _R11; end;
    $E4: begin Arg1.Reg := _ESP; Arg2.Reg := _R12; end;
    $EC: begin Arg1.Reg := _ESP; Arg2.Reg := _R13; end;
    $F4: begin Arg1.Reg := _ESP; Arg2.Reg := _R14; end;
    $FC: begin Arg1.Reg := _ESP; Arg2.Reg := _R15; end;

    $C5: begin Arg1.Reg := _EBP; Arg2.Reg := _R8; end;
    $CD: begin Arg1.Reg := _EBP; Arg2.Reg := _R9; end;
    $D5: begin Arg1.Reg := _EBP; Arg2.Reg := _R10; end;
    $DD: begin Arg1.Reg := _EBP; Arg2.Reg := _R11; end;
    $E5: begin Arg1.Reg := _EBP; Arg2.Reg := _R12; end;
    $ED: begin Arg1.Reg := _EBP; Arg2.Reg := _R13; end;
    $F5: begin Arg1.Reg := _EBP; Arg2.Reg := _R14; end;
    $FD: begin Arg1.Reg := _EBP; Arg2.Reg := _R15; end;

    $C6: begin Arg1.Reg := _ESI; Arg2.Reg := _R8; end;
    $CE: begin Arg1.Reg := _ESI; Arg2.Reg := _R9; end;
    $D6: begin Arg1.Reg := _ESI; Arg2.Reg := _R10; end;
    $DE: begin Arg1.Reg := _ESI; Arg2.Reg := _R11; end;
    $E6: begin Arg1.Reg := _ESI; Arg2.Reg := _R12; end;
    $EE: begin Arg1.Reg := _ESI; Arg2.Reg := _R13; end;
    $F6: begin Arg1.Reg := _ESI; Arg2.Reg := _R14; end;
    $FE: begin Arg1.Reg := _ESI; Arg2.Reg := _R15; end;

    $C7: begin Arg1.Reg := _EDI; Arg2.Reg := _R8; end;
    $CF: begin Arg1.Reg := _EDI; Arg2.Reg := _R9; end;
    $D7: begin Arg1.Reg := _EDI; Arg2.Reg := _R10; end;
    $DF: begin Arg1.Reg := _EDI; Arg2.Reg := _R11; end;
    $E7: begin Arg1.Reg := _EDI; Arg2.Reg := _R12; end;
    $EF: begin Arg1.Reg := _EDI; Arg2.Reg := _R13; end;
    $F7: begin Arg1.Reg := _EDI; Arg2.Reg := _R14; end;
    $FF: begin Arg1.Reg := _EDI; Arg2.Reg := _R15; end;
  else
    result := false;
  end;
end;

function AssignMovR64_R32(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $C0: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
    $C8: begin Arg1.Reg := _R8; Arg2.Reg := _ECX; end;
    $D0: begin Arg1.Reg := _R8; Arg2.Reg := _EDX; end;
    $D8: begin Arg1.Reg := _R8; Arg2.Reg := _EBX; end;
    $E0: begin Arg1.Reg := _R8; Arg2.Reg := _ESP; end;
    $E8: begin Arg1.Reg := _R8; Arg2.Reg := _EBP; end;
    $F0: begin Arg1.Reg := _R8; Arg2.Reg := _ESI; end;
    $F8: begin Arg1.Reg := _R8; Arg2.Reg := _EDI; end;

    $C1: begin Arg1.Reg := _R9; Arg2.Reg := _EAX; end;
    $C9: begin Arg1.Reg := _R9; Arg2.Reg := _ECX; end;
    $D1: begin Arg1.Reg := _R9; Arg2.Reg := _EDX; end;
    $D9: begin Arg1.Reg := _R9; Arg2.Reg := _EBX; end;
    $E1: begin Arg1.Reg := _R9; Arg2.Reg := _ESP; end;
    $E9: begin Arg1.Reg := _R9; Arg2.Reg := _EBP; end;
    $F1: begin Arg1.Reg := _R9; Arg2.Reg := _ESI; end;
    $F9: begin Arg1.Reg := _R9; Arg2.Reg := _EDI; end;

    $C2: begin Arg1.Reg := _R10; Arg2.Reg := _EAX; end;
    $CA: begin Arg1.Reg := _R10; Arg2.Reg := _ECX; end;
    $D2: begin Arg1.Reg := _R10; Arg2.Reg := _EDX; end;
    $DA: begin Arg1.Reg := _R10; Arg2.Reg := _EBX; end;
    $E2: begin Arg1.Reg := _R10; Arg2.Reg := _ESP; end;
    $EA: begin Arg1.Reg := _R10; Arg2.Reg := _EBP; end;
    $F2: begin Arg1.Reg := _R10; Arg2.Reg := _ESI; end;
    $FA: begin Arg1.Reg := _R10; Arg2.Reg := _EDI; end;

    $C3: begin Arg1.Reg := _R11; Arg2.Reg := _EAX; end;
    $CB: begin Arg1.Reg := _R11; Arg2.Reg := _ECX; end;
    $D3: begin Arg1.Reg := _R11; Arg2.Reg := _EDX; end;
    $DB: begin Arg1.Reg := _R11; Arg2.Reg := _EBX; end;
    $E3: begin Arg1.Reg := _R11; Arg2.Reg := _ESP; end;
    $EB: begin Arg1.Reg := _R11; Arg2.Reg := _EBP; end;
    $F3: begin Arg1.Reg := _R11; Arg2.Reg := _ESI; end;
    $FB: begin Arg1.Reg := _R11; Arg2.Reg := _EDI; end;

    $C6: begin Arg1.Reg := _R14; Arg2.Reg := _EAX; end;
    $CE: begin Arg1.Reg := _R14; Arg2.Reg := _ECX; end;
    $D6: begin Arg1.Reg := _R14; Arg2.Reg := _EDX; end;
    $DE: begin Arg1.Reg := _R14; Arg2.Reg := _EBX; end;
    $E6: begin Arg1.Reg := _R14; Arg2.Reg := _ESP; end;
    $EE: begin Arg1.Reg := _R14; Arg2.Reg := _EBP; end;
    $F6: begin Arg1.Reg := _R14; Arg2.Reg := _ESI; end;
    $FE: begin Arg1.Reg := _R14; Arg2.Reg := _EDI; end;

    $C7: begin Arg1.Reg := _R15; Arg2.Reg := _EAX; end;
    $CF: begin Arg1.Reg := _R15; Arg2.Reg := _ECX; end;
    $D7: begin Arg1.Reg := _R15; Arg2.Reg := _EDX; end;
    $DF: begin Arg1.Reg := _R15; Arg2.Reg := _EBX; end;
    $E7: begin Arg1.Reg := _R15; Arg2.Reg := _ESP; end;
    $EF: begin Arg1.Reg := _R15; Arg2.Reg := _EBP; end;
    $F7: begin Arg1.Reg := _R15; Arg2.Reg := _ESI; end;
    $FF: begin Arg1.Reg := _R15; Arg2.Reg := _EDI; end;
  else
    result := false;
  end;
end;

function AssignMovR64_R64(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $C0: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
    $C8: begin Arg1.Reg := _R8; Arg2.Reg := _R9; end;
    $D0: begin Arg1.Reg := _R8; Arg2.Reg := _R10; end;
    $D8: begin Arg1.Reg := _R8; Arg2.Reg := _R11; end;
    $E0: begin Arg1.Reg := _R8; Arg2.Reg := _R12; end;
    $E8: begin Arg1.Reg := _R8; Arg2.Reg := _R13; end;
    $F0: begin Arg1.Reg := _R8; Arg2.Reg := _R14; end;
    $F8: begin Arg1.Reg := _R8; Arg2.Reg := _R15; end;

    $C1: begin Arg1.Reg := _R9; Arg2.Reg := _R8; end;
    $C9: begin Arg1.Reg := _R9; Arg2.Reg := _R9; end;
    $D1: begin Arg1.Reg := _R9; Arg2.Reg := _R10; end;
    $D9: begin Arg1.Reg := _R9; Arg2.Reg := _R11; end;
    $E1: begin Arg1.Reg := _R9; Arg2.Reg := _R12; end;
    $E9: begin Arg1.Reg := _R9; Arg2.Reg := _R13; end;
    $F1: begin Arg1.Reg := _R9; Arg2.Reg := _R14; end;
    $F9: begin Arg1.Reg := _R9; Arg2.Reg := _R15; end;

    $C2: begin Arg1.Reg := _R10; Arg2.Reg := _R8; end;
    $CA: begin Arg1.Reg := _R10; Arg2.Reg := _R9; end;
    $D2: begin Arg1.Reg := _R10; Arg2.Reg := _R10; end;
    $DA: begin Arg1.Reg := _R10; Arg2.Reg := _R11; end;
    $E2: begin Arg1.Reg := _R10; Arg2.Reg := _R12; end;
    $EA: begin Arg1.Reg := _R10; Arg2.Reg := _R13; end;
    $F2: begin Arg1.Reg := _R10; Arg2.Reg := _R14; end;
    $FA: begin Arg1.Reg := _R10; Arg2.Reg := _R15; end;

    $C3: begin Arg1.Reg := _R11; Arg2.Reg := _R8; end;
    $CB: begin Arg1.Reg := _R11; Arg2.Reg := _R9; end;
    $D3: begin Arg1.Reg := _R11; Arg2.Reg := _R10; end;
    $DB: begin Arg1.Reg := _R11; Arg2.Reg := _R11; end;
    $E3: begin Arg1.Reg := _R11; Arg2.Reg := _R12; end;
    $EB: begin Arg1.Reg := _R11; Arg2.Reg := _R13; end;
    $F3: begin Arg1.Reg := _R11; Arg2.Reg := _R14; end;
    $FB: begin Arg1.Reg := _R11; Arg2.Reg := _R15; end;

    $C6: begin Arg1.Reg := _R14; Arg2.Reg := _R8; end;
    $CE: begin Arg1.Reg := _R14; Arg2.Reg := _R9; end;
    $D6: begin Arg1.Reg := _R14; Arg2.Reg := _R10; end;
    $DE: begin Arg1.Reg := _R14; Arg2.Reg := _R11; end;
    $E6: begin Arg1.Reg := _R14; Arg2.Reg := _R12; end;
    $EE: begin Arg1.Reg := _R14; Arg2.Reg := _R13; end;
    $F6: begin Arg1.Reg := _R14; Arg2.Reg := _R14; end;
    $FE: begin Arg1.Reg := _R14; Arg2.Reg := _R15; end;

    $C7: begin Arg1.Reg := _R15; Arg2.Reg := _R8; end;
    $CF: begin Arg1.Reg := _R15; Arg2.Reg := _R9; end;
    $D7: begin Arg1.Reg := _R15; Arg2.Reg := _R10; end;
    $DF: begin Arg1.Reg := _R15; Arg2.Reg := _R11; end;
    $E7: begin Arg1.Reg := _R15; Arg2.Reg := _R12; end;
    $EF: begin Arg1.Reg := _R15; Arg2.Reg := _R13; end;
    $F7: begin Arg1.Reg := _R15; Arg2.Reg := _R14; end;
    $FF: begin Arg1.Reg := _R15; Arg2.Reg := _R15; end;
  else
    result := false;
  end;
end;

function AssignMovR32(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
    $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
    $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
    $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
    $E0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESP; end;
    $E8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBP; end;
    $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
    $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

    $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
    $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
    $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
    $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
    $E1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESP; end;
    $E9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
    $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
    $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

    $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
    $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
    $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
    $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
    $E2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESP; end;
    $EA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
    $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
    $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

    $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
    $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
    $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
    $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
    $E3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESP; end;
    $EB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
    $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
    $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;

    $C4: begin Arg1.Reg := _ESP; Arg2.Reg := _EAX; end;
    $CC: begin Arg1.Reg := _ESP; Arg2.Reg := _ECX; end;
    $D4: begin Arg1.Reg := _ESP; Arg2.Reg := _EDX; end;
    $DC: begin Arg1.Reg := _ESP; Arg2.Reg := _EBX; end;
    $E4: begin Arg1.Reg := _ESP; Arg2.Reg := _ESP; end;
    $EC: begin Arg1.Reg := _ESP; Arg2.Reg := _EBP; end;
    $F4: begin Arg1.Reg := _ESP; Arg2.Reg := _ESI; end;
    $FC: begin Arg1.Reg := _ESP; Arg2.Reg := _EDI; end;

    $C5: begin Arg1.Reg := _EBP; Arg2.Reg := _EAX; end;
    $CD: begin Arg1.Reg := _EBP; Arg2.Reg := _ECX; end;
    $D5: begin Arg1.Reg := _EBP; Arg2.Reg := _EDX; end;
    $DD: begin Arg1.Reg := _EBP; Arg2.Reg := _EBX; end;
    $E5: begin Arg1.Reg := _EBP; Arg2.Reg := _ESP; end;
    $ED: begin Arg1.Reg := _EBP; Arg2.Reg := _EBP; end;
    $F5: begin Arg1.Reg := _EBP; Arg2.Reg := _ESI; end;
    $FD: begin Arg1.Reg := _EBP; Arg2.Reg := _EDI; end;

    $C6: begin Arg1.Reg := _ESI; Arg2.Reg := _EAX; end;
    $CE: begin Arg1.Reg := _ESI; Arg2.Reg := _ECX; end;
    $D6: begin Arg1.Reg := _ESI; Arg2.Reg := _EDX; end;
    $DE: begin Arg1.Reg := _ESI; Arg2.Reg := _EBX; end;
    $E6: begin Arg1.Reg := _ESI; Arg2.Reg := _ESP; end;
    $EE: begin Arg1.Reg := _ESI; Arg2.Reg := _EBP; end;
    $F6: begin Arg1.Reg := _ESI; Arg2.Reg := _ESI; end;
    $FE: begin Arg1.Reg := _ESI; Arg2.Reg := _EDI; end;

    $C7: begin Arg1.Reg := _EDI; Arg2.Reg := _EAX; end;
    $CF: begin Arg1.Reg := _EDI; Arg2.Reg := _ECX; end;
    $D7: begin Arg1.Reg := _EDI; Arg2.Reg := _EDX; end;
    $DF: begin Arg1.Reg := _EDI; Arg2.Reg := _EBX; end;
    $E7: begin Arg1.Reg := _EDI; Arg2.Reg := _ESP; end;
    $EF: begin Arg1.Reg := _EDI; Arg2.Reg := _EBP; end;
    $F7: begin Arg1.Reg := _EDI; Arg2.Reg := _ESI; end;
    $FF: begin Arg1.Reg := _EDI; Arg2.Reg := _EDI; end;
  else
    result := false;
  end;
end;

function AssignR32_R32(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
    $01: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
    $02: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
    $03: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
    $04: begin Arg1.Reg := _EAX; Arg2.Reg := _ESP; end;
    $05: begin Arg1.Reg := _EAX; Arg2.Reg := _EBP; end;
    $06: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
    $07: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

    $08: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
    $09: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
    $0A: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
    $0B: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
    $0C: begin Arg1.Reg := _ECX; Arg2.Reg := _ESP; end;
    $0D: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
    $0E: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
    $0F: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

    $10: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
    $11: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
    $12: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
    $13: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
    $14: begin Arg1.Reg := _EDX; Arg2.Reg := _ESP; end;
    $15: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
    $16: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
    $17: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

    $18: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
    $19: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
    $1A: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
    $1B: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
    $1C: begin Arg1.Reg := _EBX; Arg2.Reg := _ESP; end;
    $1D: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
    $1E: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
    $1F: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;

    $20: begin Arg1.Reg := _ESP; Arg2.Reg := _EAX; end;
    $21: begin Arg1.Reg := _ESP; Arg2.Reg := _ECX; end;
    $22: begin Arg1.Reg := _ESP; Arg2.Reg := _EDX; end;
    $23: begin Arg1.Reg := _ESP; Arg2.Reg := _EBX; end;
    $24: begin Arg1.Reg := _ESP; Arg2.Reg := _ESP; end;
    $25: begin Arg1.Reg := _ESP; Arg2.Reg := _EBP; end;
    $26: begin Arg1.Reg := _ESP; Arg2.Reg := _ESI; end;
    $27: begin Arg1.Reg := _ESP; Arg2.Reg := _EDI; end;

    $28: begin Arg1.Reg := _EBP; Arg2.Reg := _EAX; end;
    $29: begin Arg1.Reg := _EBP; Arg2.Reg := _ECX; end;
    $2A: begin Arg1.Reg := _EBP; Arg2.Reg := _EDX; end;
    $2B: begin Arg1.Reg := _EBP; Arg2.Reg := _EBX; end;
    $2C: begin Arg1.Reg := _EBP; Arg2.Reg := _ESP; end;
    $2D: begin Arg1.Reg := _EBP; Arg2.Reg := _EBP; end;
    $2E: begin Arg1.Reg := _EBP; Arg2.Reg := _ESI; end;
    $2F: begin Arg1.Reg := _EBP; Arg2.Reg := _EDI; end;

    $30: begin Arg1.Reg := _ESI; Arg2.Reg := _EAX; end;
    $31: begin Arg1.Reg := _ESI; Arg2.Reg := _ECX; end;
    $32: begin Arg1.Reg := _ESI; Arg2.Reg := _EDX; end;
    $33: begin Arg1.Reg := _ESI; Arg2.Reg := _EBX; end;
    $34: begin Arg1.Reg := _ESI; Arg2.Reg := _ESP; end;
    $35: begin Arg1.Reg := _ESI; Arg2.Reg := _EBP; end;
    $36: begin Arg1.Reg := _ESI; Arg2.Reg := _ESI; end;
    $37: begin Arg1.Reg := _ESI; Arg2.Reg := _EDI; end;

    $38: begin Arg1.Reg := _EDI; Arg2.Reg := _EAX; end;
    $39: begin Arg1.Reg := _EDI; Arg2.Reg := _ECX; end;
    $3A: begin Arg1.Reg := _EDI; Arg2.Reg := _EDX; end;
    $3B: begin Arg1.Reg := _EDI; Arg2.Reg := _EBX; end;
    $3C: begin Arg1.Reg := _EDI; Arg2.Reg := _ESP; end;
    $3D: begin Arg1.Reg := _EDI; Arg2.Reg := _EBP; end;
    $3E: begin Arg1.Reg := _EDI; Arg2.Reg := _ESI; end;
    $3F: begin Arg1.Reg := _EDI; Arg2.Reg := _EDI; end;
  else
    result := false;
  end;
end;

function AssignR64_R32(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $00: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
    $01: begin Arg1.Reg := _R8; Arg2.Reg := _ECX; end;
    $02: begin Arg1.Reg := _R8; Arg2.Reg := _EDX; end;
    $03: begin Arg1.Reg := _R8; Arg2.Reg := _EBX; end;
    $04: begin Arg1.Reg := _R8; Arg2.Reg := _ESP; end;
    $05: begin Arg1.Reg := _R8; Arg2.Reg := _EBP; end;
    $06: begin Arg1.Reg := _R8; Arg2.Reg := _ESI; end;
    $07: begin Arg1.Reg := _R8; Arg2.Reg := _EDI; end;

    $08: begin Arg1.Reg := _R9; Arg2.Reg := _EAX; end;
    $09: begin Arg1.Reg := _R9; Arg2.Reg := _ECX; end;
    $0A: begin Arg1.Reg := _R9; Arg2.Reg := _EDX; end;
    $0B: begin Arg1.Reg := _R9; Arg2.Reg := _EBX; end;
    $0C: begin Arg1.Reg := _R9; Arg2.Reg := _ESP; end;
    $0D: begin Arg1.Reg := _R8; Arg2.Reg := _EBP; end;
    $0E: begin Arg1.Reg := _R9; Arg2.Reg := _ESI; end;
    $0F: begin Arg1.Reg := _R9; Arg2.Reg := _EDI; end;

    $10: begin Arg1.Reg := _R10; Arg2.Reg := _EAX; end;
    $11: begin Arg1.Reg := _R10; Arg2.Reg := _ECX; end;
    $12: begin Arg1.Reg := _R10; Arg2.Reg := _EDX; end;
    $13: begin Arg1.Reg := _R10; Arg2.Reg := _EBX; end;
    $14: begin Arg1.Reg := _R10; Arg2.Reg := _ESP; end;
    $15: begin Arg1.Reg := _R10; Arg2.Reg := _EBP; end;
    $16: begin Arg1.Reg := _R10; Arg2.Reg := _ESI; end;
    $17: begin Arg1.Reg := _R10; Arg2.Reg := _EDI; end;

    $18: begin Arg1.Reg := _R11; Arg2.Reg := _EAX; end;
    $19: begin Arg1.Reg := _R11; Arg2.Reg := _ECX; end;
    $1A: begin Arg1.Reg := _R11; Arg2.Reg := _EDX; end;
    $1B: begin Arg1.Reg := _R11; Arg2.Reg := _EBX; end;
    $1C: begin Arg1.Reg := _R11; Arg2.Reg := _ESP; end;
    $1D: begin Arg1.Reg := _R11; Arg2.Reg := _EBP; end;
    $1E: begin Arg1.Reg := _R11; Arg2.Reg := _ESI; end;
    $1F: begin Arg1.Reg := _R11; Arg2.Reg := _EDI; end;

    $20: begin Arg1.Reg := _R12; Arg2.Reg := _EAX; end;
    $21: begin Arg1.Reg := _R12; Arg2.Reg := _ECX; end;
    $22: begin Arg1.Reg := _R12; Arg2.Reg := _EDX; end;
    $23: begin Arg1.Reg := _R12; Arg2.Reg := _EBX; end;
    $24: begin Arg1.Reg := _R12; Arg2.Reg := _ESP; end;
    $25: begin Arg1.Reg := _R12; Arg2.Reg := _EBP; end;
    $26: begin Arg1.Reg := _R12; Arg2.Reg := _ESI; end;
    $27: begin Arg1.Reg := _R12; Arg2.Reg := _EDI; end;

    $28: begin Arg1.Reg := _R13; Arg2.Reg := _EAX; end;
    $29: begin Arg1.Reg := _R13; Arg2.Reg := _ECX; end;
    $2A: begin Arg1.Reg := _R13; Arg2.Reg := _EDX; end;
    $2B: begin Arg1.Reg := _R13; Arg2.Reg := _EBX; end;
    $2C: begin Arg1.Reg := _R13; Arg2.Reg := _ESP; end;
    $2D: begin Arg1.Reg := _R13; Arg2.Reg := _EBP; end;
    $2E: begin Arg1.Reg := _R13; Arg2.Reg := _ESI; end;
    $2F: begin Arg1.Reg := _R13; Arg2.Reg := _EDI; end;

    $30: begin Arg1.Reg := _R14; Arg2.Reg := _EAX; end;
    $31: begin Arg1.Reg := _R14; Arg2.Reg := _ECX; end;
    $32: begin Arg1.Reg := _R14; Arg2.Reg := _EDX; end;
    $33: begin Arg1.Reg := _R14; Arg2.Reg := _EBX; end;
    $34: begin Arg1.Reg := _R14; Arg2.Reg := _ESP; end;
    $35: begin Arg1.Reg := _R14; Arg2.Reg := _EBP; end;
    $36: begin Arg1.Reg := _R14; Arg2.Reg := _ESI; end;
    $37: begin Arg1.Reg := _R14; Arg2.Reg := _EDI; end;

    $38: begin Arg1.Reg := _R15; Arg2.Reg := _EAX; end;
    $39: begin Arg1.Reg := _R15; Arg2.Reg := _ECX; end;
    $3A: begin Arg1.Reg := _R15; Arg2.Reg := _EDX; end;
    $3B: begin Arg1.Reg := _R15; Arg2.Reg := _EBX; end;
    $3C: begin Arg1.Reg := _R15; Arg2.Reg := _ESP; end;
    $3D: begin Arg1.Reg := _R15; Arg2.Reg := _EBP; end;
    $3E: begin Arg1.Reg := _R15; Arg2.Reg := _ESI; end;
    $3F: begin Arg1.Reg := _R15; Arg2.Reg := _EDI; end;
    else
      result := false;
  end;
end;

function AssignR64_R64(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
    $01: begin Arg1.Reg := _R8; Arg2.Reg := _R9; end;
    $02: begin Arg1.Reg := _R8; Arg2.Reg := _R10; end;
    $03: begin Arg1.Reg := _R8; Arg2.Reg := _R11; end;
    $04: begin Arg1.Reg := _R8; Arg2.Reg := _R12; end;
    $05: begin Arg1.Reg := _R8; Arg2.Reg := _R13; end;
    $06: begin Arg1.Reg := _R8; Arg2.Reg := _R14; end;
    $07: begin Arg1.Reg := _R8; Arg2.Reg := _R15; end;

    $08: begin Arg1.Reg := _R9; Arg2.Reg := _R8; end;
    $09: begin Arg1.Reg := _R9; Arg2.Reg := _R9; end;
    $0A: begin Arg1.Reg := _R9; Arg2.Reg := _R10; end;
    $0B: begin Arg1.Reg := _R9; Arg2.Reg := _R11; end;
    $0C: begin Arg1.Reg := _R9; Arg2.Reg := _R12; end;
    $0D: begin Arg1.Reg := _R9; Arg2.Reg := _R13; end;
    $0E: begin Arg1.Reg := _R9; Arg2.Reg := _R14; end;
    $0F: begin Arg1.Reg := _R9; Arg2.Reg := _R15; end;

    $10: begin Arg1.Reg := _R10; Arg2.Reg := _R8; end;
    $11: begin Arg1.Reg := _R10; Arg2.Reg := _R9; end;
    $12: begin Arg1.Reg := _R10; Arg2.Reg := _R10; end;
    $13: begin Arg1.Reg := _R10; Arg2.Reg := _R11; end;
    $14: begin Arg1.Reg := _R10; Arg2.Reg := _R12; end;
    $15: begin Arg1.Reg := _R10; Arg2.Reg := _R13; end;
    $16: begin Arg1.Reg := _R10; Arg2.Reg := _R14; end;
    $17: begin Arg1.Reg := _R10; Arg2.Reg := _R15; end;

    $18: begin Arg1.Reg := _R11; Arg2.Reg := _R8; end;
    $19: begin Arg1.Reg := _R11; Arg2.Reg := _R9; end;
    $1A: begin Arg1.Reg := _R11; Arg2.Reg := _R10; end;
    $1B: begin Arg1.Reg := _R11; Arg2.Reg := _R11; end;
    $1C: begin Arg1.Reg := _R11; Arg2.Reg := _R12; end;
    $1D: begin Arg1.Reg := _R11; Arg2.Reg := _R13; end;
    $1E: begin Arg1.Reg := _R11; Arg2.Reg := _R14; end;
    $1F: begin Arg1.Reg := _R11; Arg2.Reg := _R15; end;

    $20: begin Arg1.Reg := _R12; Arg2.Reg := _R8; end;
    $21: begin Arg1.Reg := _R12; Arg2.Reg := _R9; end;
    $22: begin Arg1.Reg := _R12; Arg2.Reg := _R10; end;
    $23: begin Arg1.Reg := _R12; Arg2.Reg := _R11; end;
    $24: begin Arg1.Reg := _R12; Arg2.Reg := _R12; end;
    $25: begin Arg1.Reg := _R12; Arg2.Reg := _R13; end;
    $26: begin Arg1.Reg := _R12; Arg2.Reg := _R14; end;
    $27: begin Arg1.Reg := _R12; Arg2.Reg := _R15; end;

    $28: begin Arg1.Reg := _R13; Arg2.Reg := _R8; end;
    $29: begin Arg1.Reg := _R13; Arg2.Reg := _R9; end;
    $2A: begin Arg1.Reg := _R13; Arg2.Reg := _R10; end;
    $2B: begin Arg1.Reg := _R13; Arg2.Reg := _R11; end;
    $2C: begin Arg1.Reg := _R13; Arg2.Reg := _R12; end;
    $2D: begin Arg1.Reg := _R13; Arg2.Reg := _R13; end;
    $2E: begin Arg1.Reg := _R13; Arg2.Reg := _R14; end;
    $2F: begin Arg1.Reg := _R13; Arg2.Reg := _R15; end;

    $30: begin Arg1.Reg := _R14; Arg2.Reg := _R8; end;
    $31: begin Arg1.Reg := _R14; Arg2.Reg := _R9; end;
    $32: begin Arg1.Reg := _R14; Arg2.Reg := _R10; end;
    $33: begin Arg1.Reg := _R14; Arg2.Reg := _R11; end;
    $34: begin Arg1.Reg := _R14; Arg2.Reg := _R12; end;
    $35: begin Arg1.Reg := _R14; Arg2.Reg := _R13; end;
    $36: begin Arg1.Reg := _R14; Arg2.Reg := _R14; end;
    $37: begin Arg1.Reg := _R14; Arg2.Reg := _R15; end;

    $38: begin Arg1.Reg := _R15; Arg2.Reg := _R8; end;
    $39: begin Arg1.Reg := _R15; Arg2.Reg := _R9; end;
    $3A: begin Arg1.Reg := _R15; Arg2.Reg := _R10; end;
    $3B: begin Arg1.Reg := _R15; Arg2.Reg := _R11; end;
    $3C: begin Arg1.Reg := _R15; Arg2.Reg := _R12; end;
    $3D: begin Arg1.Reg := _R15; Arg2.Reg := _R13; end;
    $3E: begin Arg1.Reg := _R15; Arg2.Reg := _R14; end;
    $3F: begin Arg1.Reg := _R15; Arg2.Reg := _R15; end;
    else
      result := false;
  end;
end;

function AssignR32_R64(B: Byte; var Arg1, Arg2: TArg): Boolean;
begin
  result := true;
  case B of
    $00: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
    $01: begin Arg1.Reg := _EAX; Arg2.Reg := _R9; end;
    $02: begin Arg1.Reg := _EAX; Arg2.Reg := _R10; end;
    $03: begin Arg1.Reg := _EAX; Arg2.Reg := _R11; end;
    $04: begin Arg1.Reg := _EAX; Arg2.Reg := _R12; end;
    $05: begin Arg1.Reg := _EAX; Arg2.Reg := _R13; end;
    $06: begin Arg1.Reg := _EAX; Arg2.Reg := _R14; end;
    $07: begin Arg1.Reg := _EAX; Arg2.Reg := _R15; end;

    $08: begin Arg1.Reg := _ECX; Arg2.Reg := _R8; end;
    $09: begin Arg1.Reg := _ECX; Arg2.Reg := _R9; end;
    $0A: begin Arg1.Reg := _ECX; Arg2.Reg := _R10; end;
    $0B: begin Arg1.Reg := _ECX; Arg2.Reg := _R11; end;
    $0C: begin Arg1.Reg := _ECX; Arg2.Reg := _R12; end;
    $0D: begin Arg1.Reg := _ECX; Arg2.Reg := _R13; end;
    $0E: begin Arg1.Reg := _ECX; Arg2.Reg := _R14; end;
    $0F: begin Arg1.Reg := _ECX; Arg2.Reg := _R15; end;

    $10: begin Arg1.Reg := _EDX; Arg2.Reg := _R8; end;
    $11: begin Arg1.Reg := _EDX; Arg2.Reg := _R9; end;
    $12: begin Arg1.Reg := _EDX; Arg2.Reg := _R10; end;
    $13: begin Arg1.Reg := _EDX; Arg2.Reg := _R11; end;
    $14: begin Arg1.Reg := _EDX; Arg2.Reg := _R12; end;
    $15: begin Arg1.Reg := _EDX; Arg2.Reg := _R13; end;
    $16: begin Arg1.Reg := _EDX; Arg2.Reg := _R14; end;
    $17: begin Arg1.Reg := _EDX; Arg2.Reg := _R15; end;

    $18: begin Arg1.Reg := _EBX; Arg2.Reg := _R8; end;
    $19: begin Arg1.Reg := _EBX; Arg2.Reg := _R9; end;
    $1A: begin Arg1.Reg := _EBX; Arg2.Reg := _R10; end;
    $1B: begin Arg1.Reg := _EBX; Arg2.Reg := _R11; end;
    $1C: begin Arg1.Reg := _EBX; Arg2.Reg := _R12; end;
    $1D: begin Arg1.Reg := _EBX; Arg2.Reg := _R13; end;
    $1E: begin Arg1.Reg := _EBX; Arg2.Reg := _R14; end;
    $1F: begin Arg1.Reg := _EBX; Arg2.Reg := _R15; end;
    else
      result := false;
  end;
end;

function ValToStr(val: Integer): String;
begin
//  result := IntToStr(val);

  if val = 0 then
    result := '0'
  else if val > 0 then
  begin
    result := Format('%x', [val]);
    while Length(result) < 4 do
      result := '0' + result;
    result := '$' + result;
  end
  else
  begin
    result := Format('%x', [-val]);
    while Length(result) < 4 do
      result := '0' + result;
    result := '-$' + result;
  end;
end;

function ArgToString(const Arg: TArg; PAX64: Boolean): String;
var
  I: Integer;
begin
  with Arg do
  begin
    if not valid then
    begin
      result := '';
      Exit;
    end;
    if Reg = 0 then // imm
    begin
      result := ValToStr(val);
      Exit;
    end;
    if not Ptr then
    begin
      case sz of
        1:
        case Reg of
         _EAX: result := 'AL';
         _ECX: result := 'CL';
         _EDX: result := 'DL';
         _EBX: result := 'BL';
         _ESP: result := 'SP';
         _EBP: result := 'BP';
         _ESI: result := 'SI';
         _EDI: result := 'DI';

         _R8:  result := 'R8B';
         _R9:  result := 'R9B';
         _R10: result := 'R10B';
         _R11: result := 'R11B';
         _R12: result := 'R12B';
         _R13: result := 'R13B';
         _R14: result := 'R14B';
         _R15: result := 'R15B';
        else
          RaiseError(errInternalError, []);
        end;
        2:
        case Reg of
         _EAX: result := 'AX';
         _ECX: result := 'CX';
         _EDX: result := 'DX';
         _EBX: result := 'BX';
         _ESP: result := 'SP';
         _EBP: result := 'BP';
         _ESI: result := 'SI';
         _EDI: result := 'DI';

         _R8:  result := 'R8W';
         _R9:  result := 'R9W';
         _R10: result := 'R10W';
         _R11: result := 'R11W';
         _R12: result := 'R12W';
         _R13: result := 'R13W';
         _R14: result := 'R14W';
         _R15: result := 'R15W';
        else
          RaiseError(errInternalError, []);
        end;
        4:
        case Reg of
         _EAX: result := 'EAX';
         _ECX: result := 'ECX';
         _EDX: result := 'EDX';
         _EBX: result := 'EBX';
         _ESI: result := 'ESI';
         _EDI: result := 'EDI';
         _EBP: result := 'EBP';
         _ESP: result := 'ESP';
         _R8:  result := 'R8D';
         _R9:  result := 'R9D';
         _R10: result := 'R10D';
         _R11: result := 'R11D';
         _R12: result := 'R12D';
         _R13: result := 'R13D';
         _R14: result := 'R14D';
         _R15: result := 'R15D';
        end;
        8:
        case Reg of
         _EAX: result := 'RAX';
         _ECX: result := 'RCX';
         _EDX: result := 'RDX';
         _EBX: result := 'RBX';
         _ESI: result := 'RSI';
         _EDI: result := 'RDI';
         _EBP: result := 'RBP';
         _ESP: result := 'RSP';
         _R8:  result := 'R8';
         _R9:  result := 'R9';
         _R10: result := 'R10';
         _R11: result := 'R11';
         _R12: result := 'R12';
         _R13: result := 'R13';
         _R14: result := 'R14';
         _R15: result := 'R15';

         _XMM0: result := 'XMM0';
         _XMM1: result := 'XMM1';
         _XMM2: result := 'XMM2';
         _XMM3: result := 'XMM3';
         _XMM4: result := 'XMM4';
        end;
      end;
    end
    else // Ptr
    begin
      if PAX64 then
        case Reg of
         _EAX: result := '[RAX]';
         _ECX: result := '[RCX]';
         _EDX: result := '[RDX]';
         _EBX: result := '[RBX]';
         _ESI: result := '[RSI]';
         _EDI: result := '[RDI]';
         _EBP: result := '[RBP]';
         _ESP: result := '[RSP]';
         _R8:  result := '[R8]';
         _R9:  result := '[R9]';
         _R10: result := '[R10]';
         _R11: result := '[R11]';
         _R12: result := '[R12]';
         _R13: result := '[R13]';
         _R14: result := '[R14]';
         _R15: result := '[R15]';
        end
      else
        case Reg of
         _EAX: result := '[EAX]';
         _ECX: result := '[ECX]';
         _EDX: result := '[EDX]';
         _EBX: result := '[EBX]';
         _ESI: result := '[ESI]';
         _EDI: result := '[EDI]';
         _EBP: result := '[EBP]';
         _ESP: result := '[ESP]';
        end;

      if FS then
        result := 'FS:' + result;

      if val <> 0 then
      begin
        Delete(result, Length(result), 1);
        if val > 0 then
          result := result + '+' + ValToStr(val) + ']'
        else
          result := result + ValToStr(val) + ']'
      end;

      case sz of
        1: result := 'BYTE PTR ' + result;
        2: result := 'WORD PTR ' + result;
        4: result := 'DWORD PTR ' + result;
        8: result := 'QWORD PTR ' + result;
       10: result := 'TBYTE PTR ' + result;
      else
        RaiseError(errInternalError, []);
      end;

      I := Pos('+-', result);
      if I > 0 then
        Delete(result, I, 1);
    end;
  end;
end;

procedure ClearArg(var Arg: TArg);
begin
  FillChar(Arg, SizeOf(Arg), 0);
end;

function EqualArgs(const Arg1, Arg2: TArg): Boolean;
begin
  result := (Arg1.Reg = Arg2.Reg) and
            (Arg1.Ptr = Arg2.Ptr) and
            (Arg1.sz = Arg2.sz) and
            (Arg1.val = Arg2.val);
end;

procedure SaveDecompiledCode(P: Pointer;
                             CodeSize: Integer;
                             const FileName: String; PAX64: Boolean);
var
  buff: array[1..20] of byte;
  L: Integer;

function HexCode: String;
var
  I: Integer;
begin
  result := '';
  for I:=1 to L do
     result := result + ByteToHex(buff[I]);
end;

var
  T: TextFile;
  K, Line: Integer;
  Op: Integer;
  Arg1, Arg2: TArg;
  S, S1, S2: String;
begin
  Line := 0;
  K := 0;

  AssignFile(T, FileName);
  Rewrite(T);
  try
    repeat
      Inc(Line);
      Decomp(P, L, Op, Arg1, Arg2, PAX64);
      Move(P^, buff, L);

      S  := AsmOperators[Op];
      S1 := ArgToString(Arg1, PAX64);
      S2 := ArgToString(Arg2, PAX64);

      if not Arg2.valid then
        writeln(T, Line:6, HexCode:20, S:10, ' ', S1:20)
      else
        writeln(T, Line:6, HexCode:20, S:10, ' ', S1:20, ',', S2);

      P := ShiftPointer(P, L);
      Inc(K, L);
      if K >= CodeSize then
        Break;
    until false;
  finally
    Close(T);
  end;
end;

procedure DiscardDebugMode(P: Pointer;
                           CodeSize: Integer;
                           PAX64: Boolean);
var
  buff: array[1..11] of byte;
  L: Integer;
  Value: Cardinal;
var
  K: Integer;
  Op: Integer;
  Arg1, Arg2: TArg;
begin
  Value := 58;

  K := 0;

  repeat
    Decomp(P, L, Op, Arg1, Arg2, PAX64);
    Move(P^, buff, L);

    if Op = ASM_NOP then
    begin
      Move(P^, Buff, 5);
      if Buff[2] = $90 then
      if Buff[3] = $90 then
      if Buff[4] = $90 then
      if Buff[5] = $90 then
      begin
        Buff[1] := $E9;
        Move(value, Buff[2], 4);
        Move(Buff, P^, 5);

        P := ShiftPointer(P, 5);
        Inc(K, 5);
      end;
    end
    else
    begin
      P := ShiftPointer(P, L);
      Inc(K, L);
    end;

    if K >= CodeSize then
      Break;

  until false;
end;

function GetMagicOffset(P: Pointer;
   CodeSize: Integer; MAGIC_COUNT: Integer; PAX64: Boolean): Integer;
var
  K, L, Op, Count: Integer;
  Arg1, Arg2: TArg;
begin
  K := 0;

  Count := 0;

  result := -1;

  repeat
    Decomp(P, L, Op, Arg1, Arg2, PAX64);

    if Op = ASM_JMP then
    begin
      if Arg1.val = 0 then
      begin
        Inc(Count);
        if Count = MAGIC_COUNT then
        begin
          result := K;
          Exit;
        end;
      end
      else
        Count := 0;
    end
    else
      Count := 0;

    P := ShiftPointer(P, L);
    Inc(K, L);

    if K >= CodeSize then
      Break;

  until false;
end;

function GetFinalizationOffset(P: Pointer; CodeSize: Integer; PAX64: Boolean): Integer;
begin
  result := GetMagicOffset(P, CodeSize, MAGIC_FINALIZATION_JMP_COUNT, PAX64);
end;

function GetInitializationOffset(P: Pointer; CodeSize: Integer; PAX64: Boolean): Integer;
begin
  result := GetMagicOffset(P, CodeSize, MAGIC_INITIALIZATION_JMP_COUNT, PAX64);
end;

procedure Decomp(P: Pointer; var Length: Integer; var op: Integer;
                 var Arg1: TArg;
                 var Arg2: TArg; PAX64: Boolean);
var
  B: Byte;
begin
  ClearArg(Arg1);
  Arg1.valid := true;
  if PAX64 then
    Arg1.sz := 8
  else
    Arg1.sz := 4;
  ClearArg(Arg2);
  Arg2.valid := true;
  if PAX64 then
    Arg2.sz := 8
  else
    Arg2.sz := 4;

  B := Byte(P^);
  case B of
    $FE:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $44:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $24:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $F4:
                begin
                  op := ASM_INC;
                  Length := 4;
                  Arg1.Reg := _ESP;
                  Arg1.Ptr := true;
                  Arg1.sz := 1;
                  Arg1.Val := -12;
                  Arg2.valid := false;
                end;
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        else
          RaiseError(errInternalError, []);
      end;
    end;

    $C2:
    begin
      op := ASM_RET;
      Length := 3;

      P := ShiftPointer(P, 1);
      Arg1.val := 0;
      Move(P^, Arg1.val, 2);

      Arg2.valid := false;
    end;

    $C3:
    begin
      op := ASM_RET;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $90:
    begin
      op := ASM_NOP;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $9B:
    begin
      op := ASM_WAIT;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $91..$97:
    begin
      op := ASM_XCHG;
      Length := 1;

      Arg1.Reg := _EAX;
      case B of
        $91: Arg2.Reg := _ECX;
        $92: Arg2.Reg := _EDX;
        $93: Arg2.Reg := _EBX;
        $94: Arg2.Reg := _ESP;
        $95: Arg2.Reg := _EBP;
        $96: Arg2.Reg := _ESI;
        $97: Arg2.Reg := _EDI;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $F8:
    begin
      op := ASM_CLC;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $9C:
    begin
      op := ASM_PUSHFD;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $9D:
    begin
      op := ASM_POPFD;
      Length := 1;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $F3:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      if B = $0F then
      begin
        P := ShiftPointer(P, 1);
        B := Byte(P^);
        if B <> $5A then
          RaiseError(errInternalError, []);
        P := ShiftPointer(P, 1);
        B := Byte(P^);
        Op := ASM_CVTSS2SD;
        Length := 4;
        Arg2.Ptr := true;
        Arg2.sz := 4;
        if not AssignXMM_RegPtr(B, Arg1, Arg2) then
          RaiseError(errInternalError, []);
      end
      else
      begin
        Length := 2;
        Arg1.valid := false;
        Arg2.valid := false;
        case B of
          $A4: op := ASM_REP_MOVSB;
          $A5: op := ASM_REP_MOVSD;
        else
          RaiseError(errInternalError, []);
        end;
      end;
    end;

    $71:
    begin
      op := ASM_JNO;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    $73:
    begin
      op := ASM_JNC;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    $74:
    begin
      op := ASM_JZ;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    $75:
    begin
      op := ASM_JNZ;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    $76:
    begin
      op := ASM_JBE;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    $7F:
    begin
      op := ASM_JNLE;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      Arg1.val := B;
      Arg2.valid := false;
    end;

    // Mov EAX, Imm
    $B8:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _EAX;

      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov ECX, Imm
    $B9:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _ECX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EDX, Imm
    $BA:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _EDX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EBX, Imm
    $BB:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _EBX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EBX, Imm
    $BC:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _ESP;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EBX, Imm
    $BD:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _EBP;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EBX, Imm
    $BE:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _ESI;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Mov EBX, Imm
    $BF:
    begin
      Op := ASM_MOV;
      Length := 5;

      Arg1.Reg := _EDI;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // Add REG, REG
    $01:
    begin
      Op := ASM_ADD;
      Length := 2;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
        $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
        $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
        $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
        $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
        $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

        $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
        $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
        $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
        $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

        $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
        $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
        $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
        $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

        $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
        $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
        $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
        $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // Adc REG, REG
    $11:
    begin
      Op := ASM_ADC;
      Length := 2;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
        $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
        $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
        $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
        $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
        $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

        $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
        $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
        $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
        $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

        $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
        $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
        $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
        $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

        $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
        $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
        $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
        $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // Sbb REG, REG
    $19:
    begin
      Op := ASM_SBB;
      Length := 2;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
        $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
        $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
        $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
        $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
        $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

        $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
        $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
        $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
        $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

        $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
        $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
        $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
        $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

        $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
        $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
        $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
        $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // Add EAX, Imm
    $05:
    begin
      Op := ASM_ADD;
      Length := 5;

      Arg1.Reg := _EAX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.val, 4);
    end;

    $81:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $C1..$C6: // ADD Reg, Imm
        begin
          Op := ASM_ADD;
          Length := 6;
          case B of
            $C1: Arg1.Reg := _ECX;
            $C2: Arg1.Reg := _EDX;
            $C3: Arg1.Reg := _EBX;
            $C4: Arg1.Reg := _ESP;
            $C5: Arg1.Reg := _EBP;
            $C6: Arg1.Reg := _ESI;
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg2.val, 4);
        end;

        $80..$86:  // ADD DWORD PTR Shift[Reg], Imm
        begin
          Op := ASM_ADD;
          Length := 10;
          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          end;
          Arg1.Ptr := true;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
          P := ShiftPointer(P, 4);
          Move(P^, Arg2.val, 4);
        end;

        $F9..$FF: // CMP Reg, Imm
        begin
          Op := ASM_CMP;
          Length := 6;
          case B of
            $F9: Arg1.Reg := _ECX;
            $FA: Arg1.Reg := _EDX;
            $FB: Arg1.Reg := _EBX;
            $FC: Arg1.Reg := _ESP;
            $FD: Arg1.Reg := _EBP;
            $FE: Arg1.Reg := _ESI;
            $FF: Arg1.Reg := _EDI;
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg2.val, 4);
        end;

        $E9..$EF: // SUB Reg, Imm
        begin
          Op := ASM_SUB;
          Length := 6;
          case B of
            $E9: Arg1.Reg := _ECX;
            $EA: Arg1.Reg := _EDX;
            $EB: Arg1.Reg := _EBX;
            $EC: Arg1.Reg := _ESP;
            $ED: Arg1.Reg := _EBP;
            $EE: Arg1.Reg := _ESI;
            $EF: Arg1.Reg := _EDI;
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg2.val, 4);
        end;

        $B8..$BE:  // CMP DWORD PTR Shift[Reg], Imm
        begin
          Op := ASM_CMP;
          Length := 10;
          case B of
            $B8: Arg1.Reg := _EAX;
            $B9: Arg1.Reg := _ECX;
            $BA: Arg1.Reg := _EDX;
            $BB: Arg1.Reg := _EBX;
            $BC: Arg1.Reg := _ESP;
            $BD: Arg1.Reg := _EBP;
            $BE: Arg1.Reg := _ESI;
          end;
          Arg1.Ptr := true;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
          P := ShiftPointer(P, 4);
          Move(P^, Arg2.val, 4);
        end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $F7:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $E0..$E3: // mul reg
        begin
          Op := ASM_MUL;
          Length := 2;
          case B of
            $E0: Arg1.Reg := _EAX;
            $E1: Arg1.Reg := _ECX;
            $E2: Arg1.Reg := _EDX;
            $E3: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $E8..$EB: // imul reg
        begin
          Op := ASM_IMUL;
          Length := 2;
          case B of
            $E8: Arg1.Reg := _EAX;
            $E9: Arg1.Reg := _ECX;
            $EA: Arg1.Reg := _EDX;
            $EB: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $F0..$F3: // div reg
        begin
          Op := ASM_DIV;
          Length := 2;
          case B of
            $F0: Arg1.Reg := _EAX;
            $F1: Arg1.Reg := _ECX;
            $F2: Arg1.Reg := _EDX;
            $F3: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $F8..$FB: // idiv reg
        begin
          Op := ASM_IDIV;
          Length := 2;
          case B of
            $F8: Arg1.Reg := _EAX;
            $F9: Arg1.Reg := _ECX;
            $FA: Arg1.Reg := _EDX;
            $FB: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $D0..$D3: // not reg
        begin
          Op := ASM_NOT;
          Length := 2;
          case B of
            $D0: Arg1.Reg := _EAX;
            $D1: Arg1.Reg := _ECX;
            $D2: Arg1.Reg := _EDX;
            $D3: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $D8..$DB: // neg reg
        begin
          Op := ASM_NEG;
          Length := 2;
          case B of
            $D8: Arg1.Reg := _EAX;
            $D9: Arg1.Reg := _ECX;
            $DA: Arg1.Reg := _EDX;
            $DB: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;
        $98..$9E:  // neg dword ptr [reg]
        begin
          Op := ASM_NEG;
          Length := 6;
          case B of
            $98: Arg1.Reg := _EAX;
            $99: Arg1.Reg := _ECX;
            $9A: Arg1.Reg := _EDX;
            $9B: Arg1.Reg := _EBX;
            $9C: Arg1.Reg := _ESP;
            $9D: Arg1.Reg := _EBP;
            $9E: Arg1.Reg := _ESI;
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
          Arg1.Ptr := true;
          Arg2.valid := false;
        end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $D3:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg2.Reg := _ECX;
      Arg2.sz := 1;
      Length := 2;

      case B of
        $E0..$E3: // shl reg, cl
        begin
          Op := ASM_SHL;
          case B of
            $E0: Arg1.Reg := _EAX;
            $E1: Arg1.Reg := _ECX;
            $E2: Arg1.Reg := _EDX;
            $E3: Arg1.Reg := _EBX;
          end;
        end;
        $E8..$EB: // shr reg, cl
        begin
          Op := ASM_SHR;
          case B of
            $E8: Arg1.Reg := _EAX;
            $E9: Arg1.Reg := _ECX;
            $EA: Arg1.Reg := _EDX;
            $EB: Arg1.Reg := _EBX;
          end;
        end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    //GET REG, BYTE PTR ESI or EBP
    $8A:
    begin
      Op := ASM_MOV;
      Arg1.sz := 1;
      Arg2.sz := 1;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      if AssignRegMovESIPtr(B, Arg1) then
      begin
        Length := 6;
        Arg2.Reg := _ESI;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.val, 4);
      end
      else if AssignRegMovEBPPtr(B, Arg1) then
      begin
        Length := 6;
        Arg2.Reg := _EBP;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.val, 4);
      end
      else
      begin
        Length := 2;
        Arg2.Ptr := true;
        if not AssignR32_R32(B, Arg1, Arg2) then
          RaiseError(errInternalError, []);
//      case B of
//        $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
      end;
    end;

    //GET REG, ESI or EDI or EBP
    $8B:
    begin
      Op := ASM_MOV;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.sz := 4;
      Arg2.sz := 4;
      if AssignRegMovESIPtr(B, Arg1) then
      begin
        Length := 6;
        Arg2.Reg := _ESI;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.Val, 4);
      end
      else if AssignRegMovEBPPtr(B, Arg1) then
      begin
        Length := 6;
        Arg2.Reg := _EBP;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.Val, 4);
      end
      else if B = $B4 then
      begin
        Length := 7;
        P := ShiftPointer(P, 1);
        B := Byte(P^);
        if B <> $24 then
          RaiseError(errInternalError, []);
        Arg1.Reg := _ESI;
        Arg2.Reg := _ESP;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.Val, 4);
      end
      else if B = $BC then
      begin
        Length := 7;
        P := ShiftPointer(P, 1);
        B := Byte(P^);
        if B <> $24 then
          RaiseError(errInternalError, []);
        Arg1.Reg := _EDI;
        Arg2.Reg := _ESP;
        Arg2.Ptr := true;
        P := ShiftPointer(P, 1);
        Move(P^, Arg2.Val, 4);
      end
      else
      begin
        Length := 2;
        Arg2.Ptr := true;
        if not AssignR32_R32(B, Arg1, Arg2) then
          RaiseError(errInternalError, []);

//        case B of
//          $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
//          $01: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
      end;
    end; // $8B //GET REG, ESI or EDI or EBP

    // Put BYTE PTR [ESI]| BYTE PTR [EBP],  REG
    $88:
    begin
      Op := ASM_MOV;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.sz := 1;
      Arg2.sz := 1;

      if AssignRegMovESIPtr(B, Arg2) then
      begin
        Length := 6;
        Arg1.Ptr := true;
        Arg1.Reg := _ESI;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);
      end
      else if AssignRegMovEBPPtr(B, Arg2) then
      begin
        Length := 6;
        Arg1.Ptr := true;
        Arg1.Reg := _EBP;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);
      end
      else
      begin
        Length := 2;
        if AssignR32_R32(B, Arg2, Arg1) then
          Arg1.Ptr := true
        else
          RaiseError(errInternalError, []);
      end;
    end;

    // Put [ESI] REG or MOV REGPtr, REG
    $89:
    begin
      Op := ASM_MOV;
      Arg1.sz := 4;
      Arg2.sz := 4;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      if AssignRegMovESIPtr(B, Arg2) then
      begin
        Length := 6;
        Arg1.Ptr := true;
        Arg1.Reg := _ESI;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);
      end
      else if AssignRegMovEBPPtr(B, Arg2) then
      begin
        Length := 6;
        Arg1.Ptr := true;
        Arg1.Reg := _EBP;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);
      end
      else
      begin
        Length := 2;

        if not AssignMovR32(B, Arg1, Arg2) then
          if AssignR32_R32(B, Arg2, Arg1) then
            Arg1.Ptr := true
          else
            RaiseError(errInternalError, []);
      end;
    end;

    //LEA REG32, [REG32 + Shift]
    $8D:
    begin
      Op := ASM_LEA;
      Length := 6;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $80: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
        $81: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
        $82: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
        $83: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
        $85: begin Arg1.Reg := _EAX; Arg2.Reg := _EBP; end;
        $86: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
        $87: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

        $88: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
        $89: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $8A: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $8B: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
        $8D: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
        $8E: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
        $8F: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

        $90: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
        $91: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $92: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $93: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
        $95: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
        $96: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
        $97: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

        $98: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
        $99: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $9A: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $9B: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
        $9D: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
        $9E: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
        $9F: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;

      Arg2.Ptr := true;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.Val, 4);
    end;

    // (SUB|XOR|CMP|OR|AND) REG, REG
    $29, $31, $39, $09, $21:
    begin

      case B of
        $29: Op := ASM_SUB;
        $31: Op := ASM_XOR;
        $39: Op := ASM_CMP;
        $09: Op := ASM_OR;
        $21: Op := ASM_AND;
      end;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Length := 2;

      case B of
        $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
        $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
        $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
        $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;

        $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
        $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;

        $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
        $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;

        $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
        $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $87:
    begin
      Op := ASM_XCHG;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Length := 2;

      case B of
        $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
        $CA: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
        $CB: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
        $CC: begin Arg1.Reg := _ECX; Arg2.Reg := _ESP; end;
        $CD: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
        $CE: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
        $CF: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

        $D1: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
        $D3: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
        $D4: begin Arg1.Reg := _EDX; Arg2.Reg := _ESP; end;
        $D5: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
        $D6: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
        $D7: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

        $D9: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
        $DA: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
        $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
        $DC: begin Arg1.Reg := _EBX; Arg2.Reg := _ESP; end;
        $DD: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
        $DE: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
        $DF: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // FLD|FSTP REG
    $DD:
    begin
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.Ptr := true;
      Arg1.sz := 8;

      Arg2.valid := false;
      case B of
        $00: begin Op := ASM_FLD; Arg1.Reg := _EAX; end;
        $01: begin Op := ASM_FLD; Arg1.Reg := _ECX; end;
        $02: begin Op := ASM_FLD; Arg1.Reg := _EDX; end;
        $03: begin Op := ASM_FLD; Arg1.Reg := _EBX; end;

        $18: begin Op := ASM_FSTP; Arg1.Reg := _EAX; end;
        $19: begin Op := ASM_FSTP; Arg1.Reg := _ECX; end;
        $1A: begin Op := ASM_FSTP; Arg1.Reg := _EDX; end;
        $1B: begin Op := ASM_FSTP; Arg1.Reg := _EBX; end;

        $80:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _EAX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $81:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _ECX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $82:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _EDX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $83:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _EBX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $85:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _EBP;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $86:
        begin
          Length := 6;
          Op := ASM_FLD;
          Arg1.Reg := _ESI;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $98:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _EAX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $99:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _ECX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $9A:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _EDX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $9B:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _EBX;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $9D:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _EBP;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

        $9E:
        begin
          Length := 6;
          Op := ASM_FSTP;
          Arg1.Reg := _ESI;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;

      else
        RaiseError(errInternalError, []);
      end;
    end;

    // FILD REG PTR32
    $DB:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      if B in [$00..$03] then
      begin
        Op := ASM_FILD;
        Length := 2;
        Arg1.Ptr := true;
        Arg1.sz := 4;

        Arg2.valid := false;
        case B of
          $00: Arg1.Reg := _EAX;
          $01: Arg1.Reg := _ECX;
          $02: Arg1.Reg := _EDX;
          $03: Arg1.Reg := _EBX;
        end;
      end
      else if B in [$28..$2B] then // Fld TBYTE PTR [REG]
      begin
        Op := ASM_FLD;
        Length := 2;
        Arg1.Ptr := true;
        Arg1.sz := 10;
        case B of
          $28: Arg1.Reg := _EAX;
          $29: Arg1.Reg := _ECX;
          $2A: Arg1.Reg := _EDX;
          $2B: Arg1.Reg := _EBX;
        end;
        Arg2.valid := false;
      end
      else if B in [$38..$3B] then // FStp TBYTE PTR [REG]
      begin
        Op := ASM_FSTP;
        Length := 2;
        Arg1.Ptr := true;
        Arg1.sz := 10;
        case B of
          $38: Arg1.Reg := _EAX;
          $39: Arg1.Reg := _ECX;
          $3A: Arg1.Reg := _EDX;
          $3B: Arg1.Reg := _EBX;
        end;
        Arg2.valid := false;
      end
      else if B in [$A8..$AE] then
      begin
        Op := ASM_FLD; // Fld TBYTE PTR [REG + Shift]
        Length := 6;
        Arg1.Ptr := true;
        Arg1.sz := 10;
        case B of
          $A8: Arg1.Reg := _EAX;
          $A9: Arg1.Reg := _ECX;
          $AA: Arg1.Reg := _EDX;
          $AB: Arg1.Reg := _EBX;
          $AD: Arg1.Reg := _EBP;
          $AE: Arg1.Reg := _ESP;
        end;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);

        Arg2.valid := false;
      end
      else if B in [$B8..$BE] then
      begin
        Op := ASM_FSTP; // FSTP TBYTE PTR [REG + Shift]
        Length := 6;
        Arg1.Ptr := true;
        Arg1.sz := 10;
        case B of
          $B8: Arg1.Reg := _EAX;
          $B9: Arg1.Reg := _ECX;
          $BA: Arg1.Reg := _EDX;
          $BB: Arg1.Reg := _EBX;
          $BD: Arg1.Reg := _EBP;
          $BE: Arg1.Reg := _ESP;
        end;
        P := ShiftPointer(P, 1);
        Move(P^, Arg1.val, 4);

        Arg2.valid := false;
      end
      else
        RaiseError(errInternalError, []);
    end;

    // FADD, DSUB, FMUL, FDIV REG
    $DC:
    begin
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.Ptr := true;
      Arg1.sz := 8;

      Arg2.valid := false;
      case B of
        $00: begin Op := ASM_FADD; Arg1.Reg := _EAX; end;
        $01: begin Op := ASM_FADD; Arg1.Reg := _ECX; end;
        $02: begin Op := ASM_FADD; Arg1.Reg := _EDX; end;
        $03: begin Op := ASM_FADD; Arg1.Reg := _EBX; end;

        $20: begin Op := ASM_FSUB; Arg1.Reg := _EAX; end;
        $21: begin Op := ASM_FSUB; Arg1.Reg := _ECX; end;
        $22: begin Op := ASM_FSUB; Arg1.Reg := _EDX; end;
        $23: begin Op := ASM_FSUB; Arg1.Reg := _EBX; end;

        $08: begin Op := ASM_FMUL; Arg1.Reg := _EAX; end;
        $09: begin Op := ASM_FMUL; Arg1.Reg := _ECX; end;
        $0A: begin Op := ASM_FMUL; Arg1.Reg := _EDX; end;
        $0B: begin Op := ASM_FMUL; Arg1.Reg := _EBX; end;

        $30: begin Op := ASM_FDIV; Arg1.Reg := _EAX; end;
        $31: begin Op := ASM_FDIV; Arg1.Reg := _ECX; end;
        $32: begin Op := ASM_FDIV; Arg1.Reg := _EDX; end;
        $33: begin Op := ASM_FDIV; Arg1.Reg := _EBX; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // FCOMP
    $D8:
    begin
      Op := ASM_FCOMP;
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.Ptr := true;
      Arg1.sz := 8;

      Arg2.valid := false;

      if B = $8E then
      begin
        Length := 6;
        P := ShiftPointer(P, 1);
        Op := ASM_FMUL;
        Arg1.sz := 4;
        Arg1.Reg := _ESI;
        Move(P^, Arg1.val, 4);
        Exit;
      end
      else if B = $B6 then
      begin
        Length := 6;
        P := ShiftPointer(P, 1);
        Op := ASM_FDIV;
        Arg1.sz := 4;
        Arg1.Reg := _ESI;
        Move(P^, Arg1.val, 4);
        Exit;
      end;

      case B of
        $18: Arg1.Reg := _EAX;
        $19: Arg1.Reg := _ECX;
        $1A: Arg1.Reg := _EDX;
        $1B: Arg1.Reg := _EBX;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // FSTSV
    $DF:
    begin
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      if B in [$00..$03] then
      begin
        Op := ASM_FILD;
        Length := 2;
        Arg1.Ptr := true;
        Arg1.sz := 2;

        Arg2.valid := false;
        case B of
          $00: Arg1.Reg := _EAX;
          $01: Arg1.Reg := _ECX;
          $02: Arg1.Reg := _EDX;
          $03: Arg1.Reg := _EBX;
        end;
        Exit;
      end
      else if B in [$28..$2B] then
      begin
        Op := ASM_FILD;
        Arg1.Ptr := true;
        Arg1.sz := 8;
        Arg2.valid := false;
        case B of
          $28: Arg1.Reg := _EAX;
          $29: Arg1.Reg := _ECX;
          $2A: Arg1.Reg := _EDX;
          $2B: Arg1.Reg := _EBX;
        else
          RaiseError(errInternalError, []);
        end;
        Exit;
      end
      else if B in [$38..$3B] then
      begin
        Op := ASM_FISTP;
        Arg1.Ptr := true;
        Arg1.sz := 8;
        Arg2.valid := false;
        case B of
          $38: Arg1.Reg := _EAX;
          $39: Arg1.Reg := _ECX;
          $3A: Arg1.Reg := _EDX;
          $3B: Arg1.Reg := _EBX;
        else
          RaiseError(errInternalError, []);
        end;
        Exit;
      end;

      Op := ASM_FSTSV;
      Arg1.sz := 2;
      Arg2.valid := false;
      case B of
        $E0: Arg1.Reg := _EAX;
      else
        RaiseError(errInternalError, []);
      end;
    end; // $DF

    $9E: //SAHF
    begin
      op := ASM_SAHF;
      Length := 1;
      Arg1.valid := false;
      Arg2.valid := false;
    end;

    $99: //CDQ
    begin
      op := ASM_CDQ;
      Length := 1;
      Arg1.valid := false;
      Arg2.valid := false;
    end;

    // FADD, FSUB, FMUL, FDIV, FCOMPP
    $DE:
    begin
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $C1: op := ASM_FADD;
        $C9: op := ASM_FMUL;
        $D9: op := ASM_FCOMPP;
        $E9: op := ASM_FSUB;
        $F9: op := ASM_FDIV;
      else
        RaiseError(errInternalError, []);
      end;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    // FCHS
    $D9:
    begin
      Length := 2;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $E0: op := ASM_FCHS;
        $E1: op := ASM_FABS;
      else
        begin
          // FLD|FSTP (Single) REG
          Arg1.Ptr := true;
          Arg1.sz := 4;
          Arg2.valid := false;
          case B of
            $00: begin Op := ASM_FLD; Arg1.Reg := _EAX; end;
            $01: begin Op := ASM_FLD; Arg1.Reg := _ECX; end;
            $02: begin Op := ASM_FLD; Arg1.Reg := _EDX; end;
            $03: begin Op := ASM_FLD; Arg1.Reg := _EBX; end;

            $18: begin Op := ASM_FSTP; Arg1.Reg := _EAX; end;
            $19: begin Op := ASM_FSTP; Arg1.Reg := _ECX; end;
            $1A: begin Op := ASM_FSTP; Arg1.Reg := _EDX; end;
            $1B: begin Op := ASM_FSTP; Arg1.Reg := _EBX; end;

            $80:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _EAX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $81:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _ECX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $82:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _EDX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $83:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _EBX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $85:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _EBP;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $86:
            begin
              Length := 6;
              Op := ASM_FLD;
              Arg1.Reg := _ESI;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $98:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _EAX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $99:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _ECX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $9A:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _EDX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $9B:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _EBX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $9D:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _EBP;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;

            $9E:
            begin
              Length := 6;
              Op := ASM_FSTP;
              Arg1.Reg := _ESI;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.val, 4);
            end;
          else
            RaiseError(errInternalError, []);
          end;
          Exit;
        end;
      end;

      Arg1.valid := false;
      Arg2.valid := false;
    end;

    // SETL, SETLE, SETNLE, SETNL,
    // SETB, SETBE, SETNBE, SETNB, SETZ, SETNZ
    $0F:
    begin
      Length := 3;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $92: Op := ASM_SETB;
        $93: Op := ASM_SETNB;
        $94: Op := ASM_SETZ;
        $95: Op := ASM_SETNZ;
        $96: Op := ASM_SETBE;
        $97: Op := ASM_SETNBE;
        $9C: Op := ASM_SETL;
        $9D: Op := ASM_SETNL;
        $9E: Op := ASM_SETLE;
        $9F: Op := ASM_SETNLE;
      else
        RaiseError(errInternalError, []);
      end;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      Arg1.Ptr := true;
      Arg1.sz := 1;

      Arg2.valid := false;
      case B of
        $00: Arg1.Reg := _EAX;
        $01: Arg1.Reg := _ECX;
        $02: Arg1.Reg := _EDX;
        $03: Arg1.Reg := _EBX;

        $80..$86:
        begin
          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          end;

          Length := 7;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $80:
    begin
      Arg1.sz := 1;
      Arg1.Ptr := true;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $80..$86: // ADD BYTE PTR [REG + Shift], imm
        begin
          Op := ASM_ADD;
          Length := 7;

          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          end;

          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);

          P := ShiftPointer(P, 4);
          B := Byte(P^);
          Arg2.val := B;
        end;

        $B8..$BE: // CMP BYTE PTR [REG + Shift], imm
        begin
          Op := ASM_CMP;
          Length := 7;

          case B of
            $B8: Arg1.Reg := _EAX;
            $B9: Arg1.Reg := _ECX;
            $BA: Arg1.Reg := _EDX;
            $BB: Arg1.Reg := _EBX;
            $BC: Arg1.Reg := _ESP;
            $BD: Arg1.Reg := _EBP;
            $BE: Arg1.Reg := _ESI;
          end;

          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);

          P := ShiftPointer(P, 4);
          B := Byte(P^);
          Arg2.val := B;
        end;

        $38..$3B: // CMP BYTE PTR [REG], imm
        begin
          op := ASM_CMP;
          Length := 3;
          case B of
            $38: Arg1.Reg := _EAX;
            $39: Arg1.Reg := _ECX;
            $3A: Arg1.Reg := _EDX;
            $3B: Arg1.Reg := _EBX;
          end;

          P := ShiftPointer(P, 1);
          B := Byte(P^);
          Arg2.val := B;
        end;
        else
          RaiseError(errInternalError, []);
      end;
    end;

    $E9:
    begin
      op := ASM_JMP;
      Length := 5;
      Arg2.valid := false;
      P := ShiftPointer(P, 1);
      Move(P^, Arg1.val, 4);
    end;

    $2D:
    begin
      op := ASM_SUB;
      Length := 5;
      Arg1.Reg := _EAX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.val, 4);
    end;

    $3D:
    begin
      op := ASM_CMP;
      Length := 5;
      Arg1.Reg := _EAX;
      P := ShiftPointer(P, 1);
      Move(P^, Arg2.val, 4);
    end;

    // CALL REG| JMP REG| PUSH REGPtr| Inc REGPtr, Dec REGPtr
    $FF:
    begin
      Length := 2;
      Arg2.valid := false;
      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $E0: begin Op := ASM_JMP; Arg1.Reg := _EAX; end;
        $E1: begin Op := ASM_JMP; Arg1.Reg := _ECX; end;
        $E2: begin Op := ASM_JMP; Arg1.Reg := _EDX; end;
        $E3: begin Op := ASM_JMP; Arg1.Reg := _EBX; end;

        $D0: begin Op := ASM_CALL; Arg1.Reg := _EAX; end;
        $D1: begin Op := ASM_CALL; Arg1.Reg := _ECX; end;
        $D2: begin Op := ASM_CALL; Arg1.Reg := _EDX; end;
        $D3: begin Op := ASM_CALL; Arg1.Reg := _EBX; end;
        $D4: begin Op := ASM_CALL; Arg1.Reg := _ESP; end;
        $D5: begin Op := ASM_CALL; Arg1.Reg := _EBP; end;
        $D6: begin Op := ASM_CALL; Arg1.Reg := _ESI; end;
        $D7: begin Op := ASM_CALL; Arg1.Reg := _EDI; end;

        $30: begin Op := ASM_PUSH; Arg1.Ptr := true; Arg1.sz := 4; Arg1.Reg := _EAX; end;
        $31: begin Op := ASM_PUSH; Arg1.Ptr := true; Arg1.sz := 4; Arg1.Reg := _ECX; end;
        $32: begin Op := ASM_PUSH; Arg1.Ptr := true; Arg1.sz := 4; Arg1.Reg := _EDX; end;
        $33: begin Op := ASM_PUSH; Arg1.Ptr := true; Arg1.sz := 4; Arg1.Reg := _EBX; end;

        $80..$8F: // INC, DEC
        begin
          Length := 6;
          Arg1.Ptr := true;
          Arg1.sz := 4;
          if B in [$80..$87] then
            Op := ASM_INC
          else
            Op := ASM_DEC;
          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: RaiseError(errInternalError, []);
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
            $87: Arg1.Reg := _EDI;

            $88: Arg1.Reg := _EAX;
            $89: Arg1.Reg := _ECX;
            $8A: Arg1.Reg := _EDX;
            $8B: Arg1.Reg := _EBX;
            $8C: RaiseError(errInternalError, []);
            $8D: Arg1.Reg := _EBP;
            $8E: Arg1.Reg := _ESI;
            $8F: Arg1.Reg := _EDI;
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
        end
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // PUSH Imm
    $68:
    begin
      Op := ASM_PUSH;
      Length := 5;

      P := ShiftPointer(P, 1);
      Move(P^, Arg1.val, 4);

      Arg2.valid := false;
    end;

    $84:
    begin
      Op := ASM_TEST;
      Length := 2;
      Arg1.sz := 1;
      Arg1.sz := 1;
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    // PUSH REG | POP REG
    $50..$57, $58..$5F:
    begin
      Length := 1;
      Arg2.valid := false;

      case B of
        $50: begin Op := ASM_PUSH; Arg1.Reg := _EAX; end;
        $51: begin Op := ASM_PUSH; Arg1.Reg := _ECX; end;
        $52: begin Op := ASM_PUSH; Arg1.Reg := _EDX; end;
        $53: begin Op := ASM_PUSH; Arg1.Reg := _EBX; end;
        $54: begin Op := ASM_PUSH; Arg1.Reg := _ESP; end;
        $55: begin Op := ASM_PUSH; Arg1.Reg := _EBP; end;
        $56: begin Op := ASM_PUSH; Arg1.Reg := _ESI; end;
        $57: begin Op := ASM_PUSH; Arg1.Reg := _EDI; end;

        $58: begin Op := ASM_POP; Arg1.Reg := _EAX; end;
        $59: begin Op := ASM_POP; Arg1.Reg := _ECX; end;
        $5A: begin Op := ASM_POP; Arg1.Reg := _EDX; end;
        $5B: begin Op := ASM_POP; Arg1.Reg := _EBX; end;
        $5C: begin Op := ASM_POP; Arg1.Reg := _ESP; end;
        $5D: begin Op := ASM_POP; Arg1.Reg := _EBP; end;
        $5E: begin Op := ASM_POP; Arg1.Reg := _ESI; end;
        $5F: begin Op := ASM_POP; Arg1.Reg := _EDI; end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $C6:
    begin
      Arg1.sz := 1;
      Arg1.Ptr := true;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $00..$06: // MOV BYTE PTR [REG], imm
        begin
          Op := ASM_MOV;
          Length := 3;
          case B of
            $00: Arg1.Reg := _EAX;
            $01: Arg1.Reg := _ECX;
            $02: Arg1.Reg := _EDX;
            $03: Arg1.Reg := _EBX;
            $04: Arg1.Reg := _ESP;
            $05: Arg1.Reg := _EBP;
            $06: Arg1.Reg := _ESI;
          end;
          P := ShiftPointer(P, 1);
          B := Byte(P^);

          Arg2.val := B;
        end;

        $80..$86: // MOV BYTE PTR [REG + Shift], imm
        begin
          Op := ASM_MOV;
          Length := 7;

          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          end;

          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);

          P := ShiftPointer(P, 4);
          B := Byte(P^);
          Arg2.val := B;
        end;
        else
          RaiseError(errInternalError, []);
      end;
    end;

    $C7:
    begin
      Arg1.sz := 4;
      Arg1.Ptr := true;

      P := ShiftPointer(P, 1);
      B := Byte(P^);

      case B of
        $00..$06: // MOV DWORD PTR [REG], imm
        begin
          Op := ASM_MOV;

          case B of
            $00: Arg1.Reg := _EAX;
            $01: Arg1.Reg := _ECX;
            $02: Arg1.Reg := _EDX;
            $03: Arg1.Reg := _EBX;
            $04: Arg1.Reg := _ESP;
            $05: Arg1.Reg := _EBP;
            $06: Arg1.Reg := _ESI;
          else
            RaiseError(errInternalError, []);
          end;

          Length := 6;

          P := ShiftPointer(P, 1);
          Move(P^, Arg2.val, 4);
        end;

        $80..$86: // MOV DWORD PTR [REG + Shift], imm
        begin
          Op := ASM_MOV;
          Length := 10;

          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          end;

          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);

          P := ShiftPointer(P, 4);
          Move(P^, Arg2.val, 4);
        end;
        else
          RaiseError(errInternalError, []);
      end;
    end;

    $64:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $FF:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $30: Arg1.Reg := _EAX;
            $31: Arg1.Reg := _ECX;
            $32: Arg1.Reg := _EDX;
            $33: Arg1.Reg := _EBX;
          else
            RaiseError(errInternalError, []);
          end;
          Op := ASM_PUSH;
          Length := 3;
          Arg1.Ptr := true;
          Arg1.FS := true;
          Arg2.valid := false;
        end;
        $89:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          Op := ASM_MOV;
          Length := 3;
          Arg1.Ptr := true;
          Arg1.FS := true;
          Arg2.valid := true;
          case B of
            $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
            $08: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
            $10: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
            $18: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
            $20: begin Arg1.Reg := _EAX; Arg2.Reg := _ESP; end;
            $28: begin Arg1.Reg := _EAX; Arg2.Reg := _EBP; end;
            $30: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
            $38: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

            $01: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
            $09: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
            $11: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
            $19: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
            $21: begin Arg1.Reg := _ECX; Arg2.Reg := _ESP; end;
            $29: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
            $31: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
            $39: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

            $02: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
            $0A: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
            $12: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
            $1A: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
            $22: begin Arg1.Reg := _EDX; Arg2.Reg := _ESP; end;
            $2A: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
            $32: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
            $3A: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

            $03: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
            $0B: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
            $13: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
            $1B: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
            $23: begin Arg1.Reg := _EBX; Arg2.Reg := _ESP; end;
            $2B: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
            $33: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
            $3B: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
          else
            RaiseError(errInternalError, []);
          end;
        end;
      else
        RaiseError(errInternalError, []);
      end;
    end;

    $66:
    begin
      P := ShiftPointer(P, 1);
      B := Byte(P^);
      case B of
        $41:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 4;
              Arg1.sz := 2;
              Arg2.sz := 2;
              Arg1.Ptr := true;
              if not AssignR32_R64(B, Arg2, Arg1) then
                RaiseError(errInternalError, []);
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 4;
              Arg1.sz := 2;
              Arg2.sz := 2;
              Arg2.Ptr := true;
              if not AssignR32_R64(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);
//            case B of
//              $00: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        $45:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 4;
              Arg1.sz := 2;
              Arg2.sz := 2;
              Arg1.Ptr := true;
              if not AssignR64_R64(B, Arg2, Arg1) then
                RaiseError(errInternalError, []);
//            case B of
//              $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 4;
              Arg1.sz := 2;
              Arg2.sz := 2;
              Arg2.Ptr := true;
              if not AssignR64_R64(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);
//            case B of
//              $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        $44:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz := 2;
              Arg2.sz := 2;
              if AssignRegMovRBPPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 8;
                Arg1.Reg := _EBP;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 8;
                Arg1.Reg := _ESI;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 4;
                if AssignR64_R32(B, Arg2, Arg1) then
                  Arg1.Ptr := true
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz := 2;
              Arg2.sz := 2;
              if AssignRegMovRBPPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 8;
                Arg2.Reg := _EBP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 8;
                Arg2.Reg := _ESI;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 4;
                Arg2.Ptr := true;
                if not AssignR64_R32(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);
//              case B of
//                 $00: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
              end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        $50..$53: // PUSH REG16
        begin
          Op := ASM_PUSH;
          Length := 2;
          Arg1.sz := 2;
          case B of
            $50: Arg1.Reg := _EAX;
            $51: Arg1.Reg := _ECX;
            $52: Arg1.Reg := _EDX;
            $53: Arg1.Reg := _EBX;
          end;
          Arg2.valid := false;
        end;

        $C7: // MOV WORD PTR [REG], Imm
        begin
          Op := ASM_MOV;
          Length := 9;
          Arg1.Ptr := true;
          Arg1.sz := 2;

          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $80: Arg1.Reg := _EAX;
            $81: Arg1.Reg := _ECX;
            $82: Arg1.Reg := _EDX;
            $83: Arg1.Reg := _EBX;
            $84: Arg1.Reg := _ESP;
            $85: Arg1.Reg := _EBP;
            $86: Arg1.Reg := _ESI;
          else
            RaiseError(errInternalError, []);
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
          P := ShiftPointer(P, 4);
          Move(P^, Arg2.val, 2);
        end;
        $81:
        begin
          Length := 9;
          Arg1.Ptr := true;
          Arg1.sz := 2;

          P := ShiftPointer(P, 1);
          B := Byte(P^);

          case B of
            $80..$86: // ADD WORD PTR [REG], Imm
            begin
              Op := ASM_ADD;
              case B of
                $80: Arg1.Reg := _EAX;
                $81: Arg1.Reg := _ECX;
                $82: Arg1.Reg := _EDX;
                $83: Arg1.Reg := _EBX;
                $84: Arg1.Reg := _ESP;
                $85: Arg1.Reg := _EBP;
                $86: Arg1.Reg := _ESI;
              end;
            end;
            $B8..$BE: // WORD PTR [REG], Imm
            begin
              Op := ASM_CMP;
              case B of
                $B8: Arg1.Reg := _EAX;
                $B9: Arg1.Reg := _ECX;
                $BA: Arg1.Reg := _EDX;
                $BB: Arg1.Reg := _EBX;
                $BC: Arg1.Reg := _ESP;
                $BD: Arg1.Reg := _EBP;
                $BE: Arg1.Reg := _ESI;
              end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
          P := ShiftPointer(P, 1);
          Move(P^, Arg1.val, 4);
          P := ShiftPointer(P, 4);
          Move(P^, Arg2.val, 2);
        end;
        $8B: // MOV Reg16, WORD PTR [REG]
        begin
          Op := ASM_MOV;
          Length := 3;
          Arg1.sz := 2;
          Arg2.sz := 2;

          P := ShiftPointer(P, 1);
          B := Byte(P^);

          if AssignRegMovESIPtr(B, Arg1) then
            // MOV Reg16, WORD PTR [ESI + Shift]
            begin
              Length := 7;
              Arg2.Reg := _ESI;
              Arg2.Ptr := true;
              P := ShiftPointer(P, 1);
              Move(P^, Arg2.Val, 4);
            end
          else if AssignRegMovEBPPtr(B, Arg1) then
            // MOV Reg16, WORD PTR [EBP + Shift]
            begin
              Length := 7;
              Arg2.Reg := _EBP;
              Arg2.Ptr := true;
              P := ShiftPointer(P, 1);
              Move(P^, Arg2.Val, 4);
            end
            // MOV Reg16, WORD PTR [REG]
            else
            begin
              if not AssignR32_R32(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);

//              $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
//              $01: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
            end;

          Arg2.Ptr := true;
        end;
        $89: // MOVE WORD PTR [Reg], Reg16
        begin
          Op := ASM_MOV;
          Arg1.Ptr := true;
          Arg1.sz := 2;
          Arg2.sz := 2;
          Length := 3;

          P := ShiftPointer(P, 1);
          B := Byte(P^);

          if AssignRegMovESIPtr(B, Arg2) then
            // MOV WORD PTR [ESI + Shift], Reg16
            begin
              Length := 7;
              Arg1.Reg := _ESI;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.Val, 4);
            end
          else if AssignRegMovEBPPtr(B, Arg2) then
            // MOV WORD PTR [EBP + Shift], Reg16
            begin
              Length := 7;
              Arg1.Reg := _EBP;
              P := ShiftPointer(P, 1);
              Move(P^, Arg1.Val, 4);
            end
            else
            begin
              if not AssignR32_R32(B, Arg2, Arg1) then
                RaiseError(errInternalError, []);
            end;
        end;
      end;
    end;
    else
    begin
      if not Pax64 then
        RaiseError(errInternalError, []);
      case B of
        $41:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);

          if B in [$50..$57, $58..$5F] then
          begin
            Length := 2;
            Arg2.valid := false;

            case B of
              $50: begin Op := ASM_PUSH; Arg1.Reg := _R8; end;
              $51: begin Op := ASM_PUSH; Arg1.Reg := _R9; end;
              $52: begin Op := ASM_PUSH; Arg1.Reg := _R10; end;
              $53: begin Op := ASM_PUSH; Arg1.Reg := _R11; end;
              $54: begin Op := ASM_PUSH; Arg1.Reg := _R12; end;
              $55: begin Op := ASM_PUSH; Arg1.Reg := _R13; end;
              $56: begin Op := ASM_PUSH; Arg1.Reg := _R14; end;
              $57: begin Op := ASM_PUSH; Arg1.Reg := _R15; end;

              $58: begin Op := ASM_POP; Arg1.Reg := _R8; end;
              $59: begin Op := ASM_POP; Arg1.Reg := _R9; end;
              $5A: begin Op := ASM_POP; Arg1.Reg := _R10; end;
              $5B: begin Op := ASM_POP; Arg1.Reg := _R11; end;
              $5C: begin Op := ASM_POP; Arg1.Reg := _R12; end;
              $5D: begin Op := ASM_POP; Arg1.Reg := _R13; end;
              $5E: begin Op := ASM_POP; Arg1.Reg := _R14; end;
              $5F: begin Op := ASM_POP; Arg1.Reg := _R15; end;
            end;
          end
          else if B in [$B8, $B9] then
          begin
            Op := ASM_MOV;
            Length := 6;

            case B of
              $B8: Arg1.Reg := _R8;
              $B9: Arg1.Reg := _R9;
            else
              RaiseError(errInternalError, []);
            end;
            P := ShiftPointer(P, 1);
            Move(P^, Arg2.Val, 4);
          end
          else if B = $81 then
          begin
            P := ShiftPointer(P, 1);
            B := Byte(P^);
            if B in [$C0, $C1] then
            begin
              Op := ASM_ADD;
              Length := 7;
              case B of
                $C0: Arg1.Reg := _R8;
                $C1: Arg1.Reg := _R9;
              end;
              P := ShiftPointer(P, 1);
              Move(P^, Arg2.Val, 4);
            end
            else
              RaiseError(errInternalError, []);
          end
          else if B = $88 then
          begin
            P := ShiftPointer(P, 1);
            B := Byte(P^);

            Op := ASM_MOV;
            Length := 3;
            Arg1.sz := 1;
            Arg2.sz := 1;
            Arg1.Ptr := true;
            if not AssignR32_R64(B, Arg2, Arg1) then
              RaiseError(errInternalError, []);
          end
          else if B = $89 then
          begin
            Op := ASM_MOV;
            Length := 3;
            P := ShiftPointer(P, 1);
            B := Byte(P^);
            case B of
              $C0: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
              $C8: begin Arg1.Reg := _R8; Arg2.Reg := _ECX; end;
              $D0: begin Arg1.Reg := _R8; Arg2.Reg := _EDX; end;
              $D8: begin Arg1.Reg := _R8; Arg2.Reg := _EBX; end;
              $E0: begin Arg1.Reg := _R8; Arg2.Reg := _ESP; end;
              $E8: begin Arg1.Reg := _R8; Arg2.Reg := _EBP; end;
              $F0: begin Arg1.Reg := _R8; Arg2.Reg := _ESI; end;
              $F8: begin Arg1.Reg := _R8; Arg2.Reg := _EDI; end;

              $C1: begin Arg1.Reg := _R9; Arg2.Reg := _EAX; end;
              $C9: begin Arg1.Reg := _R9; Arg2.Reg := _ECX; end;
              $D1: begin Arg1.Reg := _R9; Arg2.Reg := _EDX; end;
              $D9: begin Arg1.Reg := _R9; Arg2.Reg := _EBX; end;
              $E1: begin Arg1.Reg := _R9; Arg2.Reg := _ESP; end;
              $E9: begin Arg1.Reg := _R9; Arg2.Reg := _EBP; end;
              $F1: begin Arg1.Reg := _R9; Arg2.Reg := _ESI; end;
              $F9: begin Arg1.Reg := _R9; Arg2.Reg := _EDI; end;

              else
                RaiseError(errInternalError, []);
            end;
          end
          else if B = $8A then
          begin
            P := ShiftPointer(P, 1);
            B := Byte(P^);

            Op := ASM_MOV;
            Length := 3;
            Arg1.sz := 1;
            Arg2.sz := 1;
            Arg2.Ptr := true;
            if not AssignR32_R64(B, Arg1, Arg2) then
              RaiseError(errInternalError, []);

//          case B of
//            $00: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
          end
          else
            RaiseError(errInternalError, []);
        end;
        $44:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $88:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz  := 1;
              Arg2.sz  := 1;
              if AssignRegMovRBPPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _EBP;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.Val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _ESI;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.Val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 3;
                Arg1.Ptr := true;
                if not AssignR64_R32(B, Arg2, Arg1) then
                  RaiseError(errInternalError, []);
              end;
            end;
            $8A:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz  := 1;
              Arg2.sz  := 1;
              if AssignRegMovRBPPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _EBP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _ESI;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 3;
                Arg2.Ptr := true;
                if not AssignR64_R32(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);
              end;
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz  := 4;
              Arg2.sz  := 4;
              if AssignRegMovRBPPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _EBP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _ESI;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if B in [$84, $8C] then
              begin
                Op := ASM_MOV;
                Length := 8;

                case B of
                  $84: Arg1.Reg := _R8;
                  $8C: Arg1.Reg := _R9;
                else
                  RaiseError(errInternalError, []);
                end;
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);

                Arg2.Reg := _ESP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 3;
                Arg2.Ptr := true;
                if not AssignR64_R32(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);

//              case B of
//                $00: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
              end;
            end;
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Arg1.sz := 4;
              Arg2.sz := 4;
              if B in [$84, $8C] then
              begin
                Op := ASM_MOV;
                Length := 8;

                case B of
                  $84: Arg2.Reg := _R8;
                  $8C: Arg2.Reg := _R9;
                else
                  RaiseError(errInternalError, []);
                end;

                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);

                Arg1.Reg := _ESP;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.Val, 4);
              end
              else if AssignRegMovRBPPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _EBP;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.Val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _ESI;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.Val, 4);
              end
              else
              begin
                Op := ASM_MOV;
                Length := 3;
                if AssignR64_R32(B, Arg2, Arg1) then
                  Arg1.Ptr := true
                else
                  RaiseError(errInternalError, []);
              end;
            end;
          end;
        end; //$41
        $45:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $88:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              Arg1.sz := 1;
              Arg2.sz := 1;
              Arg1.Ptr := true;
              if not AssignR64_R64(B, Arg2, Arg1) then
                RaiseError(errInternalError, []);
            end;
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              Arg1.sz := 4;
              Arg2.sz := 4;
              Arg1.Ptr := true;
              if not AssignR64_R64(B, Arg2, Arg1) then
                RaiseError(errInternalError, []);
            end;
            $8A:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              Arg1.sz := 1;
              Arg2.sz := 1;
              Arg2.Ptr := true;
              if not AssignR64_R64(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);

//            case B of
//              $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              Arg1.sz := 4;
              Arg2.sz := 4;
              Arg2.Ptr := true;
              if not AssignR64_R64(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);

//            case B of
//              $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        $48:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $01:
            begin
              Op := ASM_ADD;
              Length := 3;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
                $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
                $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

                $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
                $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
                $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

                $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
                $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
                $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

                $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
                $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $05:
            begin
              Op := ASM_ADD;
              Length := 6;
              Arg1.Reg := _EAX;
              P := ShiftPointer(P, 1);
              Move(P^, Arg2.val, 4);
            end;
            $11:
            begin
              Op := ASM_ADC;
              Length := 3;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
                $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
                $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

                $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
                $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
                $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

                $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
                $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
                $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

                $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
                $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $19:
            begin
              Op := ASM_SBB;
              Length := 3;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
                $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
                $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

                $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
                $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
                $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

                $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
                $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
                $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

                $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
                $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            // (SUB|XOR|CMP|OR|AND) REG, REG
            $29, $31, $39, $09, $21:
            begin
              case B of
                $29: Op := ASM_SUB;
                $31: Op := ASM_XOR;
                $39: Op := ASM_CMP;
                $09: Op := ASM_OR;
                $21: Op := ASM_AND;
              end;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              Length := 3;

              case B of
                $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;

                $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;

                $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;

                $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $81:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $C0..$C7: // ADD Reg, Imm
                begin
                  Op := ASM_ADD;
                  Length := 7;
                  case B of
                    $C0: Arg1.Reg := _EAX;
                    $C1: Arg1.Reg := _ECX;
                    $C2: Arg1.Reg := _EDX;
                    $C3: Arg1.Reg := _EBX;
                    $C4: Arg1.Reg := _ESP;
                    $C5: Arg1.Reg := _EBP;
                    $C6: Arg1.Reg := _ESI;
                    $C7: Arg1.Reg := _EDI;
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;

                $80..$86:  // ADD DWORD PTR Shift[Reg], Imm
                begin
                  Op := ASM_ADD;
                  Length := 11;
                  case B of
                    $80: Arg1.Reg := _EAX;
                    $81: Arg1.Reg := _ECX;
                    $82: Arg1.Reg := _EDX;
                    $83: Arg1.Reg := _EBX;
                    $84: Arg1.Reg := _ESP;
                    $85: Arg1.Reg := _EBP;
                    $86: Arg1.Reg := _ESI;
                  end;
                  Arg1.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 4);
                end;

                $F9..$FF: // CMP Reg, Imm
                begin
                  Op := ASM_CMP;
                  Length := 7;
                  case B of
                    $F9: Arg1.Reg := _ECX;
                    $FA: Arg1.Reg := _EDX;
                    $FB: Arg1.Reg := _EBX;
                    $FC: Arg1.Reg := _ESP;
                    $FD: Arg1.Reg := _EBP;
                    $FE: Arg1.Reg := _ESI;
                    $FF: Arg1.Reg := _EDI;
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;

                $E8..$EF: // SUB Reg, Imm
                begin
                  Op := ASM_SUB;
                  Length := 7;
                  case B of
                    $E8: Arg1.Reg := _EAX;
                    $E9: Arg1.Reg := _ECX;
                    $EA: Arg1.Reg := _EDX;
                    $EB: Arg1.Reg := _EBX;
                    $EC: Arg1.Reg := _ESP;
                    $ED: Arg1.Reg := _EBP;
                    $EE: Arg1.Reg := _ESI;
                    $EF: Arg1.Reg := _EDI;
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;

                $B8..$BE:  // CMP DWORD PTR Shift[Reg], Imm
                begin
                  Op := ASM_CMP;
                  Length := 11;
                  case B of
                    $B8: Arg1.Reg := _EAX;
                    $B9: Arg1.Reg := _ECX;
                    $BA: Arg1.Reg := _EDX;
                    $BB: Arg1.Reg := _EBX;
                    $BC: Arg1.Reg := _ESP;
                    $BD: Arg1.Reg := _EBP;
                    $BE: Arg1.Reg := _ESI;
                  end;
                  Arg1.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 4);
                end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $89:
            begin
              Op := ASM_MOV;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              if AssignRegMovESIPtr(B, Arg2) then
              begin
                Length := 7;
                Arg1.Ptr := true;
                Arg1.Reg := _ESI;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if AssignRegMovEBPPtr(B, Arg2) then
              begin
                Length := 7;
                Arg1.Ptr := true;
                Arg1.Reg := _EBP;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if B in [$84, $8C, $94, $9C, $A4, $AC, $B4, $BC] then
              begin
                case B of
                  $84: Arg2.Reg := _EAX;
                  $8C: Arg2.Reg := _ECX;
                  $94: Arg2.Reg := _EDX;
                  $9C: Arg2.Reg := _EBX;
                  $A4: Arg2.Reg := _ESP;
                  $AC: Arg2.Reg := _EBP;
                  $B4: Arg2.Reg := _ESI;
                  $BC: Arg2.Reg := _EDI;
                end;

                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
                Length := 8;
                Arg1.Ptr := true;
                Arg1.Reg := _ESP;
              end
              else
              begin
                Length := 3;
                if not AssignMovR32(B, Arg1, Arg2) then
                begin
                  Arg1.Ptr := true;
                  if not AssignR32_R32(B, Arg2, Arg1) then
                    RaiseError(errInternalError, []);
                end;
              end;
            end; //$89

            //GET REG, ESI or EDI or EBP
            $8B:
            begin
              Op := ASM_MOV;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              if B in [$84, $8C, $94, $9C, $A4, $AC, $B4, $BC] then
              begin
                case B of
                  $84: Arg1.Reg := _EAX;
                  $8C: Arg1.Reg := _ECX;
                  $94: Arg1.Reg := _EDX;
                  $9C: Arg1.Reg := _EBX;
                  $A4: Arg1.Reg := _ESP;
                  $AC: Arg1.Reg := _EBP;
                  $B4: Arg1.Reg := _ESI;
                  $BC: Arg1.Reg := _EDI;
                end;

                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
                Length := 8;
                Arg2.Ptr := true;
                Arg2.Reg := _ESP;
              end
              else if AssignRegMovESIPtr(B, Arg1) then
              begin
                Length := 7;
                Arg2.Reg := _ESI;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if AssignRegMovEBPPtr(B, Arg1) then
              begin
                Length := 7;
                Arg2.Reg := _EBP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if B = $B4 then
              begin
                Length := 8;
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                Arg1.Reg := _ESI;
                Arg2.Reg := _ESP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else if B = $BC then
              begin
                Length := 8;
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                Arg1.Reg := _EDI;
                Arg2.Reg := _ESP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.Val, 4);
              end
              else
              begin
                Length := 3;
                Arg2.Ptr := true;
                if not AssignR32_R32(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);

//              case B of
//                $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
//                $01: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
              end;
            end; // $8B //GET REG, ESI or EDI or EBP

            $8D:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B <> $A5 then
                RaiseError(errInternalError, []);
              P := ShiftPointer(P, 1);

              Op := ASM_LEA;
              Length := 7;
              Arg1.Reg := _ESP;
              Arg2.Reg := _EBP;
              Arg2.Ptr := true;
              Move(P^, Arg2.val, 4);
            end;
            $C7:
            begin
              Arg1.sz := 8;
              Arg1.Ptr := true;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $00..$06: // MOV DWORD PTR [REG], imm
                begin
                  Op := ASM_MOV;

                  case B of
                    $00: Arg1.Reg := _EAX;
                    $01: Arg1.Reg := _ECX;
                    $02: Arg1.Reg := _EDX;
                    $03: Arg1.Reg := _EBX;
                    $04: Arg1.Reg := _ESP;
                    $05: Arg1.Reg := _EBP;
                    $06: Arg1.Reg := _ESI;
                  else
                    RaiseError(errInternalError, []);
                  end;

                  Length := 7;

                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;

                $80..$86: // MOV DWORD PTR [REG + Shift], imm
                begin
                  Op := ASM_MOV;
                  Length := 11;

                  case B of
                    $80: Arg1.Reg := _EAX;
                    $81: Arg1.Reg := _ECX;
                    $82: Arg1.Reg := _EDX;
                    $83: Arg1.Reg := _EBX;
                    $84: Arg1.Reg := _ESP;
                    $85: Arg1.Reg := _EBP;
                    $86: Arg1.Reg := _ESI;
                  end;

                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);

                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 4);
                end;
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            $D3:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              Arg2.Reg := _ECX;
              Arg2.sz := 1;
              Length := 3;

              case B of
                $E0..$E3: // shl reg, cl
                begin
                  Op := ASM_SHL;
                  case B of
                    $E0: Arg1.Reg := _EAX;
                    $E1: Arg1.Reg := _ECX;
                    $E2: Arg1.Reg := _EDX;
                    $E3: Arg1.Reg := _EBX;
                  end;
                end;
                $E8..$EB: // shr reg, cl
                begin
                  Op := ASM_SHR;
                  case B of
                    $E8: Arg1.Reg := _EAX;
                    $E9: Arg1.Reg := _ECX;
                    $EA: Arg1.Reg := _EDX;
                    $EB: Arg1.Reg := _EBX;
                  end;
                end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            // Mov EAX, Imm
            $B8..$BF:
            begin
              Op := ASM_MOV;
              Length := 10;

              case B of
                $B8: Arg1.Reg := _EAX;
                $B9: Arg1.Reg := _ECX;
                $BA: Arg1.Reg := _EDX;
                $BB: Arg1.Reg := _EBX;
                $BC: Arg1.Reg := _ESP;
                $BD: Arg1.Reg := _EBP;
                $BE: Arg1.Reg := _ESI;
                $BF: Arg1.Reg := _EDI;
                else
                  RaiseError(errInternalError, []);
              end;

              P := ShiftPointer(P, 1);
              Move(P^, Arg2.Val, 8);
            end;

            // FILD REG PTR32
            $DB:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              if B in [$00..$03] then
              begin
                Op := ASM_FILD;
                Length := 3;
                Arg1.Ptr := true;
                Arg1.sz := 4;

                Arg2.valid := false;
                case B of
                  $00: Arg1.Reg := _EAX;
                  $01: Arg1.Reg := _ECX;
                  $02: Arg1.Reg := _EDX;
                  $03: Arg1.Reg := _EBX;
                end;
              end
              else if B in [$28..$2B] then // Fld TBYTE PTR [REG]
              begin
                Op := ASM_FLD;
                Length := 3;
                Arg1.Ptr := true;
                Arg1.sz := 10;
                case B of
                  $28: Arg1.Reg := _EAX;
                  $29: Arg1.Reg := _ECX;
                  $2A: Arg1.Reg := _EDX;
                  $2B: Arg1.Reg := _EBX;
                end;
                Arg2.valid := false;
              end
              else if B in [$38..$3B] then // FStp TBYTE PTR [REG]
              begin
                Op := ASM_FSTP;
                Length := 3;
                Arg1.Ptr := true;
                Arg1.sz := 10;
                case B of
                  $38: Arg1.Reg := _EAX;
                  $39: Arg1.Reg := _ECX;
                  $3A: Arg1.Reg := _EDX;
                  $3B: Arg1.Reg := _EBX;
                end;
                Arg2.valid := false;
              end
              else if B in [$A8..$AE] then
              begin
                Op := ASM_FLD; // Fld TBYTE PTR [REG + Shift]
                Length := 7;
                Arg1.Ptr := true;
                Arg1.sz := 10;
                case B of
                  $A8: Arg1.Reg := _EAX;
                  $A9: Arg1.Reg := _ECX;
                  $AA: Arg1.Reg := _EDX;
                  $AB: Arg1.Reg := _EBX;
                  $AD: Arg1.Reg := _EBP;
                  $AE: Arg1.Reg := _ESP;
                end;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);

                Arg2.valid := false;
              end
              else if B in [$B8..$BE] then
              begin
                Op := ASM_FSTP; // FSTP TBYTE PTR [REG + Shift]
                Length := 7;
                Arg1.Ptr := true;
                Arg1.sz := 10;
                case B of
                  $B8: Arg1.Reg := _EAX;
                  $B9: Arg1.Reg := _ECX;
                  $BA: Arg1.Reg := _EDX;
                  $BB: Arg1.Reg := _EBX;
                  $BD: Arg1.Reg := _EBP;
                  $BE: Arg1.Reg := _ESP;
                end;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);

                Arg2.valid := false;
              end
              else
                RaiseError(errInternalError, []);
            end;

            // FLD|FSTP REG
            $DD:
            begin
              Length := 3;
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              Arg1.Ptr := true;
              Arg1.sz := 8;

              Arg2.valid := false;
              case B of
                $00: begin Op := ASM_FLD; Arg1.Reg := _EAX; end;
                $01: begin Op := ASM_FLD; Arg1.Reg := _ECX; end;
                $02: begin Op := ASM_FLD; Arg1.Reg := _EDX; end;
                $03: begin Op := ASM_FLD; Arg1.Reg := _EBX; end;

                $18: begin Op := ASM_FSTP; Arg1.Reg := _EAX; end;
                $19: begin Op := ASM_FSTP; Arg1.Reg := _ECX; end;
                $1A: begin Op := ASM_FSTP; Arg1.Reg := _EDX; end;
                $1B: begin Op := ASM_FSTP; Arg1.Reg := _EBX; end;

                $80:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _EAX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $81:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _ECX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $82:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _EDX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $83:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _EBX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $85:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _EBP;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $86:
                begin
                  Length := 7;
                  Op := ASM_FLD;
                  Arg1.Reg := _ESI;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $98:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _EAX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $99:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _ECX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $9A:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _EDX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $9B:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _EBX;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $9D:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _EBP;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

                $9E:
                begin
                  Length := 7;
                  Op := ASM_FSTP;
                  Arg1.Reg := _ESI;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                end;

              else
                RaiseError(errInternalError, []);
              end;
            end;

            $F7:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $E0..$E3: // mul reg
                begin
                  Op := ASM_MUL;
                  Length := 3;
                  case B of
                    $E0: Arg1.Reg := _EAX;
                    $E1: Arg1.Reg := _ECX;
                    $E2: Arg1.Reg := _EDX;
                    $E3: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $E8..$EB: // imul reg
                begin
                  Op := ASM_IMUL;
                  Length := 3;
                  case B of
                    $E8: Arg1.Reg := _EAX;
                    $E9: Arg1.Reg := _ECX;
                    $EA: Arg1.Reg := _EDX;
                    $EB: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $F0..$F3: // div reg
                begin
                  Op := ASM_DIV;
                  Length := 3;
                  case B of
                    $F0: Arg1.Reg := _EAX;
                    $F1: Arg1.Reg := _ECX;
                    $F2: Arg1.Reg := _EDX;
                    $F3: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $F8..$FB: // idiv reg
                begin
                  Op := ASM_IDIV;
                  Length := 3;
                  case B of
                    $F8: Arg1.Reg := _EAX;
                    $F9: Arg1.Reg := _ECX;
                    $FA: Arg1.Reg := _EDX;
                    $FB: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $D0..$D3: // not reg
                begin
                  Op := ASM_NOT;
                  Length := 3;
                  case B of
                    $D0: Arg1.Reg := _EAX;
                    $D1: Arg1.Reg := _ECX;
                    $D2: Arg1.Reg := _EDX;
                    $D3: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $D8..$DB: // neg reg
                begin
                  Op := ASM_NEG;
                  Length := 3;
                  case B of
                    $D8: Arg1.Reg := _EAX;
                    $D9: Arg1.Reg := _ECX;
                    $DA: Arg1.Reg := _EDX;
                    $DB: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;
                $98..$9E:  // neg dword ptr [reg]
                begin
                  Op := ASM_NEG;
                  Length := 7;
                  case B of
                    $98: Arg1.Reg := _EAX;
                    $99: Arg1.Reg := _ECX;
                    $9A: Arg1.Reg := _EDX;
                    $9B: Arg1.Reg := _EBX;
                    $9C: Arg1.Reg := _ESP;
                    $9D: Arg1.Reg := _EBP;
                    $9E: Arg1.Reg := _ESI;
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                  Arg1.Ptr := true;
                  Arg2.valid := false;
                end;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $FF:
            begin
              Op := ASM_CALL;
              Length := 3;
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $D0: Arg1.Reg := _EAX;
                $D1: Arg1.Reg := _ECX;
                $D2: Arg1.Reg := _EDX;
                $D3: Arg1.Reg := _EBX;
                $D4: Arg1.Reg := _ESP;
                $D5: Arg1.Reg := _EBP;
                $D6: Arg1.Reg := _ESI;
                $D7: Arg1.Reg := _EDI;
                $E0: Arg1.Reg := _R8;
                $E1: Arg1.Reg := _R9;
                else
                  RaiseError(errInternalError, []);
              end;
              Arg2.valid := false;
            end;
          end;
        end; //$48
        $4C:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;

              if AssignRegMovRBPPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _EBP;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg2) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg1.Reg := _ESI;
                Arg1.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if B in [$84, $8C, $94, $9C, $A4, $AC, $B4, $BC] then
              begin
                case B of
                  $84: Arg2.Reg := _R8;
                  $8C: Arg2.Reg := _R9;
                  $94: Arg2.Reg := _R10;
                  $9C: Arg2.Reg := _R11;
                  $A4: Arg2.Reg := _R12;
                  $AC: Arg2.Reg := _R13;
                  $B4: Arg2.Reg := _R14;
                  $BC: Arg2.Reg := _R15;
                end;

                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
                Length := 8;
                Arg1.Ptr := true;
                Arg1.Reg := _ESP;
              end
              else
              begin
                Length := 3;
                if not AssignMovR32_R64(B, Arg1, Arg2) then
                begin
                  Arg1.Ptr := true;
                  if not AssignR64_R32(B, Arg2, Arg1) then
                    RaiseError(errInternalError, []);
                end;
              end;
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if AssignRegMovRBPPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _EBP;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
              end
              else if AssignRegMovRSIPtr(B, Arg1) then
              begin
                Op := ASM_MOV;
                Length := 7;
                Arg2.Reg := _ESI;
                Arg2.Ptr := true;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
              end
              else if B in [$84, $8C, $94, $9C, $A4, $AC, $B4, $BC] then
              begin
                Op := ASM_MOV;
                case B of
                  $84: Arg1.Reg := _R8;
                  $8C: Arg1.Reg := _R9;
                  $94: Arg1.Reg := _R10;
                  $9C: Arg1.Reg := _R11;
                  $A4: Arg1.Reg := _R12;
                  $AC: Arg1.Reg := _R13;
                  $B4: Arg1.Reg := _R14;
                  $BC: Arg1.Reg := _R15;
                end;

                P := ShiftPointer(P, 1);
                B := Byte(P^);
                if B <> $24 then
                  RaiseError(errInternalError, []);
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
                Length := 8;
                Arg2.Ptr := true;
                Arg2.Reg := _ESP;
              end
              else
              begin
                Op := ASM_MOV;
                Length := 3;
                Arg2.Ptr := true;
                if not AssignR64_R32(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);

//              case B of
//                $00: begin Arg1.Reg := _R8; Arg2.Reg := _EAX; end;
              end;
            end;
            else
              RaiseError(errInternalError, []);
          end;
        end;
        $67:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $41:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $89:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 4;
                  Arg1.sz := 4;
                  Arg2.sz := 4;
                  if AssignR32_R64(B, Arg2, Arg1) then
                    Arg1.Ptr := true
                  else
                    RaiseError(errInternalError, []);
                end;
                $8B:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 4;
                  Arg1.sz := 4;
                  Arg2.sz := 4;
                  if AssignR32_R64(B, Arg1, Arg2) then
                    Arg2.Ptr := true
                  else
                    RaiseError(errInternalError, []);

//                case B of
//                  $00: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
                end;
              end;
            end;
            $48:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $89:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 8;
                  if not AssignRegMovESIPtr(B, Arg2) then
                      RaiseError(errInternalError, []);
                  Arg1.Reg := _ESI;
                  Arg1.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;
                $8B:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 8;
                  if not AssignRegMovESIPtr(B, Arg1) then
                      RaiseError(errInternalError, []);
                  Arg2.Reg := _ESI;
                  Arg2.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            $4C:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $89:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 8;
                  if not AssignRegMovRSIPtr(B, Arg2) then
                      RaiseError(errInternalError, []);
                  Arg1.Reg := _ESI;
                  Arg1.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;
                $8B:
                begin
                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  Op := ASM_MOV;
                  Length := 8;
                  if not AssignRegMovRSIPtr(B, Arg1) then
                      RaiseError(errInternalError, []);
                  Arg2.Reg := _ESI;
                  Arg2.Ptr := true;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            $F2:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B <> $48 then
                RaiseError(errInternalError, []);
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B <> $0F then
                RaiseError(errInternalError, []);
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B = $10 then
              begin
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                Op := ASM_MOVSD;
                Length := 6;
                Arg2.Ptr := true;
                if not AssignXMM_RegPtr(B, Arg1, Arg2) then
                    RaiseError(errInternalError, []);
              end
              else if B = $11 then
              begin
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                Op := ASM_MOVSD;
                Length := 6;
                Arg1.Ptr := true;
                if not AssignXMM_RegPtr(B, Arg2, Arg1) then
                    RaiseError(errInternalError, []);
              end
              else
                RaiseError(errInternalError, []);
              // movsd
            end; // $F2
            $F3:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B <> $0F then
                RaiseError(errInternalError, []);
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if B = $10 then
              begin
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                Op := ASM_MOVSS;
                Length := 5;
                Arg2.Ptr := true;
                Arg2.sz := 4;
                if not AssignXMM_RegPtr(B, Arg1, Arg2) then
                  RaiseError(errInternalError, []);
              end
              else if B = $11 then
              begin
                P := ShiftPointer(P, 1);
                B := Byte(P^);
                Op := ASM_MOVSS;
                Length := 5;
                Arg1.Ptr := true;
                Arg1.sz := 4;
                if not AssignXMM_RegPtr(B, Arg2, Arg1) then
                  RaiseError(errInternalError, []);
              end
              else
                RaiseError(errInternalError, []);
              // movss
            end; // $f3
            $66:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of

                $50..$53: // PUSH REG16
                begin
                  Op := ASM_PUSH;
                  Length := 3;
                  Arg1.sz := 2;
                  case B of
                    $50: Arg1.Reg := _EAX;
                    $51: Arg1.Reg := _ECX;
                    $52: Arg1.Reg := _EDX;
                    $53: Arg1.Reg := _EBX;
                  end;
                  Arg2.valid := false;
                end;

                $C7: // MOV WORD PTR [REG], Imm
                begin
                  Op := ASM_MOV;
                  Length := 10;
                  Arg1.Ptr := true;
                  Arg1.sz := 2;

                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  case B of
                    $80: Arg1.Reg := _EAX;
                    $81: Arg1.Reg := _ECX;
                    $82: Arg1.Reg := _EDX;
                    $83: Arg1.Reg := _EBX;
                    $84: Arg1.Reg := _ESP;
                    $85: Arg1.Reg := _EBP;
                    $86: Arg1.Reg := _ESI;
                  else
                    RaiseError(errInternalError, []);
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 2);
                end;
                $81:
                begin
                  Length := 10;
                  Arg1.Ptr := true;
                  Arg1.sz := 2;

                  P := ShiftPointer(P, 1);
                  B := Byte(P^);

                  case B of
                    $80..$86: // ADD WORD PTR [REG], Imm
                    begin
                      Op := ASM_ADD;
                      case B of
                        $80: Arg1.Reg := _EAX;
                        $81: Arg1.Reg := _ECX;
                        $82: Arg1.Reg := _EDX;
                        $83: Arg1.Reg := _EBX;
                        $84: Arg1.Reg := _ESP;
                        $85: Arg1.Reg := _EBP;
                        $86: Arg1.Reg := _ESI;
                      end;
                    end;
                    $B8..$BE: // WORD PTR [REG], Imm
                    begin
                      Op := ASM_CMP;
                      case B of
                        $B8: Arg1.Reg := _EAX;
                        $B9: Arg1.Reg := _ECX;
                        $BA: Arg1.Reg := _EDX;
                        $BB: Arg1.Reg := _EBX;
                        $BC: Arg1.Reg := _ESP;
                        $BD: Arg1.Reg := _EBP;
                        $BE: Arg1.Reg := _ESI;
                      end;
                    end;
                    else
                      RaiseError(errInternalError, []);
                  end;
                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);
                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 2);
                end;
                $8B: // MOV Reg16, WORD PTR [REG]
                begin
                  Op := ASM_MOV;
                  Length := 4;
                  Arg1.sz := 2;
                  Arg2.sz := 2;

                  P := ShiftPointer(P, 1);
                  B := Byte(P^);
                  if AssignRegMovESIPtr(B, Arg1) then
                    // MOV Reg16, WORD PTR [ESI + Shift]
                    begin
                      Length := 8;
                      Arg2.Reg := _ESI;
                      Arg2.Ptr := true;
                      P := ShiftPointer(P, 1);
                      Move(P^, Arg2.Val, 4);
                    end
                  else if AssignRegMovEBPPtr(B, Arg1) then
                    // MOV Reg16, WORD PTR [EBP + Shift]
                    begin
                      Length := 8;
                      Arg2.Reg := _EBP;
                      Arg2.Ptr := true;
                      P := ShiftPointer(P, 1);
                      Move(P^, Arg2.Val, 4);
                    end
                    // MOV Reg16, WORD PTR [REG]
                    else
                    begin
                      if AssignR32_R32(B, Arg1, Arg2) then
                        RaiseError(errInternalError, []);
//                    $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
//                    $01: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                    end;

                  Arg2.Ptr := true;
                end;

                $89: // MOVE WORD PTR [Reg], Reg16
                begin
                  Op := ASM_MOV;
                  Arg1.Ptr := true;
                  Arg1.sz := 2;
                  Arg2.sz := 2;
                  Length := 4;

                  P := ShiftPointer(P, 1);
                  B := Byte(P^);

                  if AssignRegMovESIPtr(B, Arg2) then
                    // MOV WORD PTR [ESI + Shift], Reg16
                    begin
                      Length := 7;
                      Arg1.Reg := _ESI;
                      P := ShiftPointer(P, 1);
                      Move(P^, Arg1.Val, 4);
                    end
                  else if AssignRegMovEBPPtr(B, Arg2) then
                    // MOV WORD PTR [EBP + Shift], Reg16
                    begin
                      Length := 7;
                      Arg1.Reg := _EBP;
                      P := ShiftPointer(P, 1);
                      Move(P^, Arg1.Val, 4);
                    end
                 else
                 case B of
                    $00: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                    $08: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                    $10: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                    $18: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;

                    $01: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                    $09: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                    $11: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                    $19: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;

                    $02: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                    $0A: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                    $12: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                    $1A: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;

                    $03: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                    $0B: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                    $13: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                    $1B: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                  else
                    RaiseError(errInternalError, []);
                  end;
                end;
              end;
            end;

            $89:
            begin
              Op := ASM_MOV;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              if AssignRegMovESIPtr(B, Arg2) then
              begin
                Length := 7;
                Arg1.Ptr := true;
                Arg1.Reg := _ESI;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else if AssignRegMovEBPPtr(B, Arg2) then
              begin
                Length := 7;
                Arg1.Ptr := true;
                Arg1.Reg := _EBP;
                P := ShiftPointer(P, 1);
                Move(P^, Arg1.val, 4);
              end
              else
              begin
                Length := 3;
                Arg1.sz := 4;
                Arg2.sz := 4;

                case B of
                  $C0: begin Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                  $C8: begin Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                  $D0: begin Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                  $D8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;
                  $E0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESP; end;
                  $E8: begin Arg1.Reg := _EAX; Arg2.Reg := _EBP; end;
                  $F0: begin Arg1.Reg := _EAX; Arg2.Reg := _ESI; end;
                  $F8: begin Arg1.Reg := _EAX; Arg2.Reg := _EDI; end;

                  $C1: begin Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                  $C9: begin Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                  $D1: begin Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                  $D9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;
                  $E1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESP; end;
                  $E9: begin Arg1.Reg := _ECX; Arg2.Reg := _EBP; end;
                  $F1: begin Arg1.Reg := _ECX; Arg2.Reg := _ESI; end;
                  $F9: begin Arg1.Reg := _ECX; Arg2.Reg := _EDI; end;

                  $C2: begin Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                  $CA: begin Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                  $D2: begin Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                  $DA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;
                  $E2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESP; end;
                  $EA: begin Arg1.Reg := _EDX; Arg2.Reg := _EBP; end;
                  $F2: begin Arg1.Reg := _EDX; Arg2.Reg := _ESI; end;
                  $FA: begin Arg1.Reg := _EDX; Arg2.Reg := _EDI; end;

                  $C3: begin Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                  $CB: begin Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                  $D3: begin Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                  $DB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                  $E3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESP; end;
                  $EB: begin Arg1.Reg := _EBX; Arg2.Reg := _EBP; end;
                  $F3: begin Arg1.Reg := _EBX; Arg2.Reg := _ESI; end;
                  $FB: begin Arg1.Reg := _EBX; Arg2.Reg := _EDI; end;

                  $E5: begin Arg1.Reg := _EBP; Arg2.Reg := _ESP; end;
                  $EC: begin Arg1.Reg := _ESP; Arg2.Reg := _EBP; end;

                  $C6: begin Arg1.Reg := _ESI; Arg2.Reg := _EAX; end;
                  $CE: begin Arg1.Reg := _ESI; Arg2.Reg := _ECX; end;
                  $D6: begin Arg1.Reg := _ESI; Arg2.Reg := _EDX; end;
                  $DE: begin Arg1.Reg := _ESI; Arg2.Reg := _EBX; end;
                  $E6: begin Arg1.Reg := _ESI; Arg2.Reg := _ESP; end;
                  $EE: begin Arg1.Reg := _ESI; Arg2.Reg := _EBP; end;
                  $F6: begin Arg1.Reg := _ESI; Arg2.Reg := _ESI; end;
                  $FE: begin Arg1.Reg := _ESI; Arg2.Reg := _EDI; end;

                  $C7: begin Arg1.Reg := _EDI; Arg2.Reg := _EAX; end;
                  $CF: begin Arg1.Reg := _EDI; Arg2.Reg := _ECX; end;
                  $D7: begin Arg1.Reg := _EDI; Arg2.Reg := _EDX; end;
                  $DF: begin Arg1.Reg := _EDI; Arg2.Reg := _EBX; end;
                  $E7: begin Arg1.Reg := _EDI; Arg2.Reg := _ESP; end;
                  $EF: begin Arg1.Reg := _EDI; Arg2.Reg := _EBP; end;
                  $F7: begin Arg1.Reg := _EDI; Arg2.Reg := _ESI; end;
                  $FF: begin Arg1.Reg := _EDI; Arg2.Reg := _EDI; end;

                  $00: begin Arg1.Ptr := true; Arg1.Reg := _EAX; Arg2.Reg := _EAX; end;
                  $08: begin Arg1.Ptr := true; Arg1.Reg := _EAX; Arg2.Reg := _ECX; end;
                  $10: begin Arg1.Ptr := true; Arg1.Reg := _EAX; Arg2.Reg := _EDX; end;
                  $18: begin Arg1.Ptr := true; Arg1.Reg := _EAX; Arg2.Reg := _EBX; end;

                  $01: begin Arg1.Ptr := true; Arg1.Reg := _ECX; Arg2.Reg := _EAX; end;
                  $09: begin Arg1.Ptr := true; Arg1.Reg := _ECX; Arg2.Reg := _ECX; end;
                  $11: begin Arg1.Ptr := true; Arg1.Reg := _ECX; Arg2.Reg := _EDX; end;
                  $19: begin Arg1.Ptr := true; Arg1.Reg := _ECX; Arg2.Reg := _EBX; end;

                  $02: begin Arg1.Ptr := true; Arg1.Reg := _EDX; Arg2.Reg := _EAX; end;
                  $0A: begin Arg1.Ptr := true; Arg1.Reg := _EDX; Arg2.Reg := _ECX; end;
                  $12: begin Arg1.Ptr := true; Arg1.Reg := _EDX; Arg2.Reg := _EDX; end;
                  $1A: begin Arg1.Ptr := true; Arg1.Reg := _EDX; Arg2.Reg := _EBX; end;

                  $03: begin Arg1.Ptr := true; Arg1.Reg := _EBX; Arg2.Reg := _EAX; end;
                  $0B: begin Arg1.Ptr := true; Arg1.Reg := _EBX; Arg2.Reg := _ECX; end;
                  $13: begin Arg1.Ptr := true; Arg1.Reg := _EBX; Arg2.Reg := _EDX; end;
                  $1B: begin Arg1.Ptr := true; Arg1.Reg := _EBX; Arg2.Reg := _EBX; end;
                else
                  RaiseError(errInternalError, []);
                end;
              end;
            end;
          end;
        end;
        $49:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);

          case B of
            $FF:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              Length := 3;
              Arg2.valid := false;
              Op := ASM_JMP;
              case B of
                $E0: Arg1.Reg := _R8;
                $E1: Arg1.Reg := _R9;
                $E2: Arg1.Reg := _R10;
                $E3: Arg1.Reg := _R11;
                $E4: Arg1.Reg := _R12;
                $E5: Arg1.Reg := _R13;
                $E6: Arg1.Reg := _R14;
                $E7: Arg1.Reg := _R15;
              else
                RaiseError(errInternalError, []);
              end;
            end;
            $81:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);

              if B in [$C0..$C7] then // ADD Reg, Imm
              begin
                Op := ASM_ADD;
                Length := 7;
                case B of
                  $C0: Arg1.Reg := _R8;
                  $C1: Arg1.Reg := _R9;
                  $C2: Arg1.Reg := _R10;
                  $C3: Arg1.Reg := _R11;
                  $C4: Arg1.Reg := _R12;
                  $C5: Arg1.Reg := _R13;
                  $C6: Arg1.Reg := _R14;
                  $C7: Arg1.Reg := _R15;
                end;
                P := ShiftPointer(P, 1);
                Move(P^, Arg2.val, 4);
              end
              else
                RaiseError(errInternalError, []);
            end;
            $8B:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              case B of
                $FF: begin end;
                else
                begin
                  Length := 3;

                  Arg2.Ptr := true;

                  if not AssignR32_R64(B, Arg1, Arg2) then
                    RaiseError(errInternalError, []);

//                case B of
//                  $00: begin Arg1.Reg := _EAX; Arg2.Reg := _R8; end;
                end;
              end;
            end;
            $C7:
            begin
              Arg1.sz := 8;
              Arg1.Ptr := true;

              P := ShiftPointer(P, 1);
              B := Byte(P^);

              case B of
                $00..$07: // MOV DWORD PTR [REG], imm
                begin
                  Op := ASM_MOV;

                  case B of
                    $00: Arg1.Reg := _R8;
                    $01: Arg1.Reg := _R9;
                    $02: Arg1.Reg := _R10;
                    $03: Arg1.Reg := _R11;
                    $04: Arg1.Reg := _R12;
                    $05: Arg1.Reg := _R13;
                    $06: Arg1.Reg := _R14;
                    $07: Arg1.Reg := _R15;
                    else
                      RaiseError(errInternalError, []);
                  end;

                  Length := 7;

                  P := ShiftPointer(P, 1);
                  Move(P^, Arg2.val, 4);
                end;

                $80..$87: // MOV DWORD PTR [REG + Shift], imm
                begin
                  Op := ASM_MOV;
                  Length := 11;

                  case B of
                    $80: Arg1.Reg := _R8;
                    $81: Arg1.Reg := _R9;
                    $82: Arg1.Reg := _R10;
                    $83: Arg1.Reg := _R11;
                    $84: Arg1.Reg := _R12;
                    $85: Arg1.Reg := _R13;
                    $86: Arg1.Reg := _R14;
                    $87: Arg1.Reg := _R15;
                  end;

                  P := ShiftPointer(P, 1);
                  Move(P^, Arg1.val, 4);

                  P := ShiftPointer(P, 4);
                  Move(P^, Arg2.val, 4);
                end;
                else
                  RaiseError(errInternalError, []);
              end;
            end;
            $B8..$BF:
            begin
              Op := ASM_MOV;
              Length := 10;

              case B of
                $B8: Arg1.Reg := _R8;
                $B9: Arg1.Reg := _R9;
                $BA: Arg1.Reg := _R10;
                $BB: Arg1.Reg := _R11;
                $BC: Arg1.Reg := _R12;
                $BD: Arg1.Reg := _R13;
                $BE: Arg1.Reg := _R14;
                $BF: Arg1.Reg := _R15;
                else
                  RaiseError(errInternalError, []);
              end;

              P := ShiftPointer(P, 1);
              Move(P^, Arg2.Val, 8);
            end;
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              if not AssignMovR64_R32(B, Arg1, Arg2) then
              begin
                Arg1.Ptr := true;
                if not AssignR32_R64(B, Arg2, Arg1) then
                  RaiseError(errInternalError, []);
              end;
            end
            else
              RaiseError(errInternalError, []);
          end;
        end; //$49
        $4D:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          case B of
            $8B:
            begin
              Op := ASM_MOV;
              Length := 3;
              Arg2.Ptr := true;
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              if not AssignR64_R64(B, Arg1, Arg2) then
                RaiseError(errInternalError, []);

//            case B of
//              $00: begin Arg1.Reg := _R8; Arg2.Reg := _R8; end;
            end;
            $89:
            begin
              P := ShiftPointer(P, 1);
              B := Byte(P^);
              Op := ASM_MOV;
              Length := 3;
              if not AssignMovR64_R64(B, Arg1, Arg2) then
              begin
                Arg1.Ptr := true;
                if not AssignR64_R64(B, Arg2, Arg1) then
                  RaiseError(errInternalError, []);
              end;
            end
            else
              RaiseError(errInternalError, []);
          end;
        end; // $4D
        $F2:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          if B <> $0F then
            RaiseError(errInternalError, []);
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          if B <> $5A then
            RaiseError(errInternalError, []);
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          Op := ASM_CVTSD2SS;
          Length := 4;
          Arg2.Ptr := true;
          Arg2.sz := 4;
          if not AssignXMM_RegPtr(B, Arg1, Arg2) then
            RaiseError(errInternalError, []);
        end;
        $F3:
        begin
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          if B <> $0F then
            RaiseError(errInternalError, []);
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          if B <> $5A then
            RaiseError(errInternalError, []);
          P := ShiftPointer(P, 1);
          B := Byte(P^);
          Op := ASM_CVTSS2SD;
          Length := 4;
          Arg2.Ptr := true;
          Arg2.sz := 4;
          if not AssignXMM_RegPtr(B, Arg1, Arg2) then
            RaiseError(errInternalError, []);
        end
        else
          RaiseError(errInternalError, []);
      end; //$48
    end;
  end; // case
end;

end.


