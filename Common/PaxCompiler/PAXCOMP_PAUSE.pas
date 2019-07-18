////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PAUSE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_PAUSE;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS;

type
  TPauseRec = class
  public
    _EBP: IntPax;
    _ESP: IntPax;
    ESP0: IntPax;
    ProgOffset: Integer;
    StackFrame: Pointer;
    StackFrameSize: Integer;

    PaxExcFrame1: PPaxExcFrame;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveStackFrame;
    function GetPtr(EBP_Value, Shift: Integer): Pointer;
  end;

implementation

//-- TPauseRec -----------------------------------------------------------------

constructor TPauseRec.Create;
begin
  inherited;
  StackFrame := nil;
  Clear;
end;

destructor TPauseRec.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPauseRec.Clear;
begin
  if StackFrame <> nil then
    FreeMem(StackFrame, StackFrameSize);
  _ESP := 0;
  ESP0 := 0;
  ProgOffset := 0;
  StackFrame := nil;
  StackFrameSize := 0;
  PaxExcFrame1 := nil;
end;

{$IFDEF PAX64}

procedure _Save(I: Int64; P: Pointer; K: Int64); assembler;
asm
  mov rax, I
  mov rbx, P
  mov rcx, K

  @@loop:

  mov rdx, [rax]
  mov [rbx], rdx

  sub rax, 8
  sub rbx, 8

  sub rcx, 1
  cmp rcx, 0

  jnz @@loop
end;

procedure TPauseRec.SaveStackFrame;
var
  P: Pointer;
  I, K: Integer;
begin
  if ESP0 = _ESP then
    Exit;

  if StackFrame <> nil then
    FreeMem(StackFrame, StackFrameSize);

  StackFrameSize := ESP0 - _ESP;

  if StackFrameSize < 0 then
    Exit;

  StackFrame := AllocMem(StackFrameSize);

  I := ESP0 - SizeOf(Pointer);

  P := ShiftPointer(StackFrame, StackFrameSize - SizeOf(Pointer));

  K := StackFrameSize div SizeOf(Pointer);

  _Save(I, P, K);
end;
{$ELSE}
procedure TPauseRec.SaveStackFrame;
var
  P: Pointer;
  I, K: Integer;
begin
  if ESP0 = _ESP then
    Exit;

  if StackFrame <> nil then
    FreeMem(StackFrame, StackFrameSize);

  StackFrameSize := ESP0 - _ESP;

  if StackFrameSize < 0 then
    Exit;

  StackFrame := AllocMem(StackFrameSize);

  I := ESP0 - 4;

  P := ShiftPointer(StackFrame, StackFrameSize - 4);

  K := StackFrameSize div 4;
  asm
    mov eax, I
    mov ebx, P
    mov ecx, K

    @@loop:

    mov edx, [eax]
    mov [ebx], edx

    sub eax, 4

    sub ebx, 4

    sub ecx, 1
    cmp ecx, 0
    jnz @@loop
  end;
end;
{$ENDIF}

function TPauseRec.GetPtr(EBP_Value, Shift: Integer): Pointer;
var
  K: Integer;
begin
  K := EBP_Value + Shift - _ESP;
  result := ShiftPointer(StackFrame, K);
end;

end.
