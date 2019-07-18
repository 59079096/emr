////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_EVENT.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_EVENT;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

type
  TEventHandlerRec = class
  public
    _ESI: Integer; // 4
    _EDI: Integer; // 8
    Code: Pointer; // 12
    Data: TObject; // 16
    Prog: Pointer; // 20
    CallConv: Integer; // 24
    RetSize: Integer; // 28
    procedure Invoke;
    procedure InvokeDirect;
  end;

  TEventHandlerList = class(TTypedList)
  private
    function GetRecord(I: Integer): TEventHandlerRec;
  public
    function Lookup(Code: Pointer; Data: TObject): TEventHandlerRec;
    function Add(Prog, Code, Data: Pointer; CallConv, RetSize: Integer): TEventHandlerRec;
    property Records[I: Integer]: TEventHandlerRec read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_CONSTANTS,
  PAXCOMP_PROG,
  PAXCOMP_STDLIB,
  PAXCOMP_PAUSE,
  PAXCOMP_INVOKE;

// -- TEventHandlerRec ---------------------------------------------------------

{$IFDEF PAX64}
procedure TEventHandlerRec.InvokeDirect;
begin
  RaiseNotImpl;
end;
{$ELSE}
procedure TEventHandlerRec.InvokeDirect; assembler;
asm
  sub esp, 4 // reserve place for Code

  push ecx
  mov ecx, [eax + 12]
//  add ecx, 14
  mov [esp + 4], ecx // Code
  pop ecx

  mov esi, [eax + 4] // esi
  mov edi, [eax + 8] // edi

  mov eax, [eax + 16] // Data

  add esp, 4
  mov ebx, [esp - 4]
  jmp ebx
end;
{$ENDIF}

{$IFDEF PAX64}
procedure SaveRegisters(var _RAX, _RCX, _RDX, _R8, _R9: IntPax); assembler;
asm
  mov _RAX, rax
  mov _RCX, rcx
  mov _RDX, rdx
  mov _R8, r8
  mov _R9, r9
end;
{$ENDIF}

procedure TEventHandlerRec.Invoke;
var
  P: TProgram;
  EPoint, OldEPoint: TInvoke;
{$IFDEF PAX64}
  _RAX, _RCX, _RDX, _R8, _R9: IntPax;
{$ELSE}
  _EAX, _ECX, _EDX, _EBP: Integer;
{$ENDIF}
  OldESP0: Integer;
  OldIsEvent: Boolean;
  S: TMemoryStream;
begin
{$IFDEF PAX64}
  SaveRegisters(_RAX, _RCX, _RDX, _R8, _R9);
{$ELSE}
  asm
    mov _EBP, ebp
    mov _EAX, eax
    mov _ECX, ecx
    mov _EDX, edx
  end;
{$ENDIF}

  P := Prog;
  P := P.RootProg;

  if not P.IsPauseUpdated then
  begin
    EPoint := TInvoke.CreateInternal;
{$IFDEF PAX64}
    EPoint._EAX := _RAX;
    EPoint._ECX := _RCX;
    EPoint._EDX := _RDX;
    EPoint._R8 := _R8;
    EPoint._R9 := _R9;
{$ELSE}
    EPoint._EAX := _EAX;
    EPoint._ECX := _ECX;
    EPoint._EDX := _EDX;
{$ENDIF}
    EPoint.CallConv := CallConv;
    EPoint.StackSize := RetSize;
    EPoint.StackFrame := Pointer(_EBP + 4 + RetSize);

    OldEPoint := P.EPoint;
    OldESP0 := P.RootESP0;
    OldIsEvent := P.RootIsEvent;

    S := TMemoryStream.Create;
    try
      P.RootTryStack.SaveToStream(S);
      P.RootTryStack.Clear;

      P.RootIsEvent := true;

      EPoint.Address := Code;
      EPoint.SetThis(Data);

      if CallConv = ccREGISTER then
        EPoint._EAX := Integer(Data);

      P.EPoint := EPoint;
      P.RunEx;

    finally
      S.Position := 0;
      P.RootTryStack.LoadFromStream(S);

      P.RootESP0 := OldESP0;
      P.RootIsEvent := OldIsEvent;

      P.EPoint := OldEPoint;
      FreeAndNil(EPoint);
      FreeAndNil(S);
    end;

    // emulate ret RetSize

    if P.IsHalted then
      raise THaltException.Create(P.ExitCode);

    if P.HasError then
      if P.fCurrException <> nil then
      begin
        if P.PauseRec <> nil then
        begin
          P.PauseRec.Clear;
        end;
        raise P.fCurrException;
      end;


  end;

{$IFDEF PAX64}
  Exit;
{$ELSE}
  if RetSize = 0 then
    Exit;

  _ecx := RetSize;
{$ENDIF}
{
  asm
    mov ecx, _ecx

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
}
{$IFDEF PAX64}
  Exit;
{$ELSE}

  asm
    mov ecx, _ecx

    mov esp, ebp
    pop ebp

    fild dword ptr [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop

    fistp dword ptr [esp]

    pop edx
    jmp edx
  end;
{$ENDIF}
end;

// -- TEventHandlerList --------------------------------------------------------

function TEventHandlerList.GetRecord(I: Integer): TEventHandlerRec;
begin
  result := TEventHandlerRec(L[I]);
end;

function TEventHandlerList.Add(Prog, Code, Data: Pointer; CallConv, RetSize: Integer): TEventHandlerRec;
begin
  result := Lookup(Code, Data);
  if result = nil then
  begin
    result := TEventHandlerRec.Create;
    result.Code := Code;
    result.Data := Data;
    result._ESI := Integer(TProgram(Prog).DataPtr);
    result._EDI := Integer(TProgram(Prog).CodePtr);
    result.Prog := Prog;
    result.CallConv := CallConv;
    result.RetSize := RetSize;
    L.Add(result);
  end;
end;

function TEventHandlerList.Lookup(Code: Pointer; Data: TObject): TEventHandlerRec;
var
  R: TEventHandlerRec;
  I: Integer;
begin
  for I:=0 to Count - 1 do
  begin
    R := Records[I];
    if (R.Code = Code) and (R.Data = Data) then
    begin
      result := R;
      Exit;
    end;
  end;
  result := nil;
end;

end.
