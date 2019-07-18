//////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PROGLIB.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
{$R-}
unit PAXCOMP_PROGLIB;
interface

procedure AssignRunnerLib;

implementation

uses {$I uses.def}
  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_BASERUNNER,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_PROG,
  PAXCOMP_MAP,
  PAXCOMP_CLASSLST,
  PAXCOMP_SEH,
  PAXCOMP_EVENT,
  PAXCOMP_STDLIB,
  PAXCOMP_TRYLST;

procedure _Exit(Runner: TBaseRunner; Level: Integer; em: TExitMode); stdcall;
var
  E: PaxExitException;
  TryRec: TTryRec;
  EstablisherFrame: PPaxExcFrame;
  P: TProgram;
begin
  P := TProgram(Runner);
  if P.ModeSEH then
  begin
{$IFDEF PAX64}
{$ELSE}
    asm
      mov eax, fs:[0]
      mov EstablisherFrame, eax
    end;
    while EstablisherFrame^.Magic <> PAX_SEH do
      EstablisherFrame := EstablisherFrame^.Next;
    asm
      mov eax, EstablisherFrame
      mov fs:[0], eax
    end;
{$ENDIF}
  end;
  P.ClearCurrException;

  E := PaxExitException.Create('');
  E.Mode := em;
  P.ExitLevelId := Level;
  P.RootExceptionIsAvailableForHostApplication := false;

  if P.ModeSEH then
  begin
    raise E;
  end;

  if P.RootTryStack.Count > 0 then
  begin
    TryRec := P.RootTryStack.Top.TR;
    while TryRec.TryKind <> tryFinally do
    begin
      P.RootTryStack.Pop;
      if P.RootTryStack.Count > 0 then
        TryRec := P.RootTryStack.Top.TR
      else
        raise E;
    end;

    TryRec.SaveStackFrame;
  end;

  raise E;
end;

procedure _CondRaise(P: TProgram; var IsExit: Byte;
                     SubId: Integer;
                     LastCondRaise: Integer;
                     CurrESP: Integer
                     ); stdcall;
var
  E: Exception;
  CE: TExceptionClass;
  I, K, LevelId: Integer;
  Temp1: Integer;
  Temp2: Pointer;
  EstablisherFrame: PPaxExcFrame;
  em: TExitMode;
begin
  IsExit := 0;
  em := emExit;

  if P.ProcessingExceptBlock then
    Exit;

  Dec(P.RootProg.FinallyCount);
  if P.RootProg.FinallyCount <> 0 then
    Exit;

  if P.CurrException <> nil then
  begin
    if P.CurrException is PaxExitException then
    begin
      em := (P.CurrException as PaxExitException).Mode;
      IsExit := 1;
      if em = emBreak then
        IsExit := 2;
      if em = emContinue then
        IsExit := 3;
    end;

    if P.ModeSEH then
    begin
{$IFDEF PAX64}
{$ELSE}
      asm
        MOV EAX, DWORD PTR FS:[0]
        mov EstablisherFrame, eax
      end;
{$ENDIF}
      if IsExit > 0 then
      begin
        if LastCondRaise = 1 then
        begin
          P.RootExceptionIsAvailableForHostApplication := true;
          P.CurrException.Free;
          P.CurrException := nil;
          Exit;
        end;
      end;

      if (EstablisherFrame <> nil ) and (EstablisherFrame^.Magic <> PAX_SEH) and (LastCondRaise = 1) then
      begin
        IntPax(CE) := IntPax(P.CurrException.ClassType);
        E := CE.Create(P.CurrException.Message);
        P.CurrException.Free;
        P.CurrException := nil;
        P.fPrevException := E;
      end
      else
        E := P.CurrException;

      raise E;
    end;

    if IsExit > 0 then
    begin
      K := 0;
      if P.RootTryStack.Count > 0 then
      begin
        LevelId := P.ExitLevelId;

        for I := P.RootTryStack.Count - 1 downto 0 do
        begin
          if P.RootTryStack[I].Prog <> P then
            continue;

          if P.TryList[P.RootTryStack[I].TryBlockNumber].Level = LevelId then
            Inc(K)
          else
          begin
            P.RootTryStack.Pop;

            P.ExitLevelId := 0;
            P.CurrException.Free;
            P.CurrException := nil;

            Exit;
          end;
        end;
      end;

      if K = 0 then
        Exit
      else if K = 1 then
      begin
        if P.RootTryStack.Count > 0 then
          P.RootTryStack.Pop;

        P.ExitLevelId := 0;
        P.CurrException.Free;
        P.CurrException := nil;

        Exit;
      end;

      P.RootTryStack.Pop;

      _Exit(P, P.ExitLevelId, em);
    end;

    IntPax(CE) := IntPax(P.CurrException.ClassType);
    E := CE.Create(P.CurrException.Message);

    Temp1 := P.RootTryStack.Top.TryBlockNumber;
    Temp2 := P.RootTryStack.Top.Prog;

    P.RootTryStack.Pop;

    if P.RootTryStack.Count > 0 then
    begin
      P.RootExceptionIsAvailableForHostApplication := false;

      if IsExit > 0 then
      while (P.TryList[P.RootTryStack.Top.TryBlockNumber].TryKind = tryFinally) and (P.RootTryStack.Count > 0) do
      begin
        if P.TryList[P.RootTryStack.Top.TryBlockNumber].Level <> SubId then
        begin
          P.RootTryStack.Push(Temp1, Temp2);
          Exit;
        end;
        P.RootTryStack.Pop;

        if P.RootTryStack.Count = 0 then
          break;
      end;
    end
    else
      P.RootExceptionIsAvailableForHostApplication := true;

    raise E;
  end;
end;

{$IFDEF PAX64}
procedure AssignRSI_RDI(Data: Pointer; Code: Pointer); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov rsi, rcx
  mov rdi, rdx
end;
{$ENDIF}

procedure _LoadSeg(P: TProgram;
                   FullProcName: PChar); stdcall;
var
  S: String;
  I: Integer;
  CurrProg: TProgram;
  MR: TMapRec;
  Code, Data: Pointer;
begin
  S := FullProcName;
  P := P.RootProg;
  for I := 0 to P.ProgList.Count - 1 do
  begin
    CurrProg := TProgram(P.ProgList[I].Prog);
    if CurrProg.GetAddress(S, MR) <> nil then
    begin
      Code := CurrProg.CodePtr;
      Data := CurrProg.DataPtr;
{$IFDEF PAX64}
      AssignRSI_RDI(Data, Code);
{$ELSE}
      asm
        mov edi, Code
        mov esi, Data
      end;
{$ENDIF}
    end;
  end;
end;

procedure _CreateObject(C: TClass;
                        var X: TObject); stdcall;
var
  ClassIndex: Integer;
  PaxInfo: PPaxInfo;
  P: TProgram;
label
  Again;
begin
  Again:

  if not IsDelphiClass(C) then
  begin
    if Assigned(X) then
       Exit;
    C := TObject(C).ClassType;
  end;

  PaxInfo := GetPaxInfo(C);
  if PaxInfo = nil then
  begin
    ErrMessageBox(C.ClassName);
    raise Exception.Create(errInternalError);
  end;
  P := TProgram(PaxInfo^.Prog);

  if P.RootProg.PassedClassRef <> nil then
  begin
    C := P.RootProg.PassedClassRef;
    P.RootProg.PassedClassRef := nil;
    goto Again;
  end;

  ClassIndex := PaxInfo^.ClassIndex;
  if ClassIndex = -1 then
  begin
    raise Exception.Create(errInternalError);
  end;

  if C.InstanceSize = 0 then
    P.RaiseError(errInternalError, []);

  Pointer(X) := nil;
  X := C.NewInstance;
{$IFDEF ARC}
  X.__ObjRelease;
{$ENDIF}
end;

{$IFDEF PAX64}
procedure _64_CallDestructor(Self: TObject; //rcx
                             Code: Pointer; //rdx
                             Data: Pointer; //r8
                             _ProgOffset: IntPax //r9
                             ); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  push rbp

  push rsi
  push rdi
  push rbx
  push rcx

  sub rsp, $100
  mov rbp, rsp

//  mov rcx, rcx // Self
  mov rdi, rdx // Code
  mov rsi, r8  // Data

  mov rbx, rdi
//  add rbx, _ProgOffset
  add rbx, r9

  call rbx

  add rsp, $100

  pop rcx
  pop rbx
  pop rdi
  pop rsi

  pop rbp
  ret
end;
{$ENDIF}

procedure ClearStrProps(X: TObject);
var
  pti: PTypeInfo;
  ptd: PTypeData;
  Loop, nProps: Integer;
  pProps: PPropList;
  ppi: PPropInfo;
begin
  try
  pti := X.ClassInfo;
  if pti = nil then Exit;
  ptd := GetTypeData(pti);
  if ptd = nil then Exit;
  nProps := ptd^.PropCount;
  if nProps = 0 then
    Exit;
  GetMem(pProps, SizeOf(PPropInfo) * nProps);
  GetPropInfos(pti, pProps);
  for Loop:=0 to nProps - 1 do
  begin
    try
{$ifdef fpc}
    ppi := pProps^[Loop];
{$else}
    ppi := pProps[Loop];
{$endif}
    if ppi^.SetProc <> nil then
    case ppi^.PropType^.Kind of
{$IFDEF PAXARM}
       tkUString: SetStrProp(X, ppi, '');
{$ELSE}
      tkLString:
      {$IFDEF UNIC}
        SetAnsiStrProp(X, ppi, '');
      {$ELSE}
        SetStrProp(X, ppi, '');
      {$ENDIF}
      tkWString:
      {$IFDEF VARIANTS}
        SetWideStrProp(X, ppi, '');
      {$ENDIF}
{$IFDEF UNIC}
      tkUString:
{$IFDEF VARIANTS}
      {$IFDEF UNIC}
        SetUnicodeStrProp(X, ppi, '');
      {$ELSE}
        SetWideStrProp(X, ppi, '');
      {$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
    end;
    except
    end;
  end;
  FreeMem(pProps, SizeOf(PPropInfo) * nProps);
  except
  end;
end;

procedure _DestroyObject(ASelf: Pointer; OuterMost: ShortInt);
var
  PaxInfo: PPaxInfo;
  ClassRec: TClassRec;
  _ProgOffset, _Self: Integer;
  Data, Code: Pointer;
  P, RootProg, TargetProg: TProgram;
  C: TClass;
  VMT: PVMT;
  I: Integer;
  FullName: String;
  Self: TObject;
begin
  Self := TObject(ASelf);

  C := Self.ClassType;

  VMT := GetVMTFromClass(C);
  if OuterMost <> 0 then
    if not C.InheritsFrom(TInterfacedObject) then
      PBeforeDestruction(vmtBeforeDestructionSlot(VMT)^)(Self);

  PaxInfo := GetPaxInfo(C);
  if PaxInfo = nil then
    raise Exception.Create(errInternalError);

  P := TProgram(PaxInfo^.Prog);

  if Assigned(P.OnDestroyObject) then
    if OuterMost <> 0 then
      P.OnDestroyObject(P.Owner, Self);

  ClassRec := P.ClassList[PaxInfo^.ClassIndex];
  _ProgOffset := ClassRec.DestructorProgOffset;

{$IFDEF FPC}
{$ELSE}
  if C.InheritsFrom(TPersistent) then
    ClearStrProps(Self);
{$ENDIF}

  TargetProg := nil;
  if _ProgOffset = 0 then
  begin
    FullName := ClassRec.FullName;
    RootProg := P.RootProg;
    for I := 0 to RootProg.ProgList.Count - 1 do
    begin
      TargetProg := TProgram(RootProg.ProgList[I].Prog);
      ClassRec := TargetProg.ClassList.Lookup(FullName);
      if ClassRec <> nil then
      begin
        _ProgOffset := ClassRec.DestructorProgOffset;
        break;
      end;
    end;
  end
  else
    TargetProg := P;

    if _ProgOffset > 0 then
    begin
      Data := TargetProg.DataPtr;
      Code := TargetProg.CodePtr;

{$IFDEF PAX64}
      _64_CallDestructor(Self, Code, Data, _ProgOffset);
{$ELSE}

      asm
        push esi
        push edi
        push ebx

        mov esi, Data;
        mov edi, Code;
      end;

      _Self  := Integer(Self);
      asm
        push eax
        mov eax, _Self
        mov ebx, edi
        add ebx, _ProgOffset
        call ebx
        pop eax
      end;

      asm
        pop ebx
        pop edi
        pop esi
      end;

{$ENDIF}
    end;

  if OuterMost = 0 then
    Exit;

  _ToParentClass2(Self);
    Self.CleanupInstance;
  _UpdateInstance2(Self, C);

  FreeMem(Pointer(Self), Self.InstanceSize);

  if Assigned(P.OnAfterObjectDestruction) then
    P.OnAfterObjectDestruction(P.Owner, C);

  Pointer(Self) := nil;
end;

// processing try-except-finally

procedure _TryOn(P: TProgram; TryBlockNumber: Integer; _EBP, _ESP: Integer);
stdcall;
var
  R: TTryRec;
begin
  with P.RootProg do
  begin
    P.SetAddress(H_ExceptionPtr, @ P.fCurrException);
    SourceLineFinally := -1;
  end;

  P.RootTryStack.Push(TryBlockNumber, P);

  R := P.RootTryStack.Top.TR;
  R._EBP := _EBP;
  R._ESP := _ESP;
  R.ESP0 := P.RootESP0;
  R.SaveStackFrame;
end;

procedure _TryOff(P: TProgram; TryBlockNumber: Integer); stdcall;
begin
  P := P.RootProg;

  if P.RootTryStack.Count = 0 then
    Exit;
  P.RootTryStack.Pop;
  P.SetAddress(H_ExceptionPtr, @ P.fCurrException);
end;

procedure _Raise(P: TProgram; E: Exception;
                 RaiseMode: Integer; CurrESP: Integer); stdcall;
var
  CE: TExceptionClass;
  EstablisherFrame: PPaxExcFrame;
begin
  if P.ModeSEH then
  begin
{$IFDEF PAX64}
{$ELSE}
    if not P.ProcessingExceptBlock then
    begin
      asm
        mov eax, fs:[0]
        mov EstablisherFrame, eax
      end;
      while EstablisherFrame^.Magic <> PAX_SEH do
      begin
        EstablisherFrame := EstablisherFrame^.Next;
      end;
      asm
        mov eax, EstablisherFrame
        mov fs:[0], eax
      end;
    end;
{$ENDIF}
{
    if P.ProcessingExceptBlock then
    begin
     asm
        MOV EAX, CurrESP
        MOV EAX, DWORD PTR [EAX]
        XOR EBX, EBX
        MOV DWORD PTR FS:[EBX], EAX
      end;
    end;
}
    if E <> nil then
    begin
      if RaiseMode = 0 then
        P.SourceLineFinally := -1;
      raise E;
    end
    else if P.fCurrException <> nil then
    begin
      IntPax(CE) := IntPax(P.CurrException.ClassType);
      E := CE.Create(P.CurrException.Message);
      raise E;
    end
    else
      raise Exception.Create('');
  end;

  if RaiseMode = 0 then
  begin
  end
  else
  begin
    if P.RootTryStack.Count > 0 then
      P.RootTryStack.Pop;
  end;

  if P.RootTryStack.Count > 0 then
    P.RootTryStack.Top.TR.SaveStackFrame;

  if E <> nil then
    raise E
  else
  begin
    IntPax(CE) := IntPax(P.CurrException.ClassType);
    E := CE.Create(P.CurrException.Message);
    raise E;
  end;
end;

// processing pause

procedure _Pause(P: TProgram; ProgOffset, _EBP, _ESP: Integer); stdcall;
var
  EstablisherFrame: PPaxExcFrame;
begin
  if Assigned(P.OnPauseUpdated) then
  begin
    P.PauseRec._EBP := _EBP;
    P.PauseRec._ESP := _ESP;
    P.PauseRec.ESP0 := P.RootESP0;
    P.PauseRec.ProgOffset := ProgOffset;

{$IFDEF PCU_EX}
    P.PauseRec.ESP0 := _EBP + 4096;
{$ENDIF}

    P.PauseRec.SaveStackFrame;

    P.RootProg.IsPauseUpdated := true;
    P.PauseRec.ProgOffset := 1;
    P.RootProg.PauseRec.ProgOffset := 1;

    try
      P.RootProg.PausedPCU := P;
      P.OnPauseUpdated(P.RootProg.Owner, P.GetModuleName, P.GetSourceLine);
    finally
//      P.RootProg.PausedPCU := nil;
      P.PauseRec.ProgOffset := 0;
      P.RootProg.PauseRec.ProgOffset := 0;
      P.RemovePause;
//    P.RootProg.RemovePause;
      P.RootInitCallStackCount := P.RootCallStack.Count;
      P.RootProg.IsPauseUpdated := false;
    end;
    Exit;
  end;

  P.PauseRec._EBP := _EBP;
  P.PauseRec._ESP := _ESP;
  P.PauseRec.ESP0 := P.RootESP0;
  P.PauseRec.ProgOffset := ProgOffset;
  P.PauseRec.SaveStackFrame;

  P.RootExceptionIsAvailableForHostApplication := false;

  if P.ModeSEH then
  if P.ProcessingExceptBlock then
  begin

    with P do
      if Assigned(OnPause) then
        OnPause(Owner, GetModuleName, GetSourceLine);

{$IFDEF PAX64}
{$ELSE}
    asm
      mov eax, fs:[0]
      mov EstablisherFrame, eax
    end;

    while EstablisherFrame^.Magic <> PAX_SEH do
      EstablisherFrame := EstablisherFrame^.Next;

    P.PauseRec.PaxExcFrame1 := EstablisherFrame;

    while EstablisherFrame^.Magic = PAX_SEH do
      EstablisherFrame := EstablisherFrame^.Next;

    asm
      mov eax, EstablisherFrame
      mov fs:[0], eax
    end;
{$ENDIF}

    P.PauseSEH := true;
  end;

  raise TPauseException.Create;
end;

procedure _InitSub(P: TProgram; SubId: Integer; _EBP: Integer);
var
  N: Integer;
  CallerProg: TProgram;
  MR: TMapRec;
begin
  if Assigned(P.OnBeginProcNotifyEvent) then
  begin
    MR := P.ScriptMapTable.LookupSub(SubId);
    if MR = nil then
      MR := P.HostMapTable.LookupSub(SubId);
    if MR <> nil then
      P.OnBeginProcNotifyEvent(P.Owner, MR.FullName, MR.SubDesc.OverCount);
  end;

  if (P.RunMode <> rmTRACE_INTO) and (P.RunMode <> rmNEXT_SOURCE_LINE) then
    if P.RootCallStack.Count > 0 then
    begin
      CallerProg := P.RootCallStack.Top.Prg;
      if CallerProg <> P then
        P.RemovePause;
    end;

  N := P.GetByteCodeLine;
  P.RootCallStack.Push(_EBP, SubId, N, P);
end;

procedure _EndSub(P: TProgram);
var
  N, SubId: Integer;
  MR: TMapRec;
begin
  if Assigned(P.OnEndProcNotifyEvent) then
  begin
    SubId := P.RootCallStack.Top.SubId;
    MR := P.ScriptMapTable.LookupSub(SubId);
    if MR = nil then
      MR := P.HostMapTable.LookupSub(SubId);
    if MR <> nil then
      P.OnEndProcNotifyEvent(P.Owner, MR.FullName, MR.SubDesc.OverCount);
  end;

  N := P.RootCallStack.Top.NCall;
  P.SetByteCodeLine(N);

  P.RootCallStack.Pop;

  if P.RunMode = rmSTEP_OVER then
  begin
    if P.RootInitCallStackCount >= P.RootCallStack.Count then
      P.Pause;
  end;
end;

procedure _SetEventProp(P: TProgram;
                        PropInfo: PPropInfo;
                        Instance: TObject;
                        Code: Pointer;
                        Data: TObject;
                        CallConv: Integer;
                        RetSize: Integer); stdcall;
var
  M: TMethod;
  ER: TEventHandlerRec;

  B, B1, B2: Integer;
begin
  if Data = nil then
  begin
    M.Code := nil;
    M.Data := nil;
    SetMethodProp(Instance, PropInfo, M);
    Exit;
  end;

  if P.ModeSEH then
  begin
    M.Code := Code;
    M.Data := Data;
    SetMethodProp(Instance, PropInfo, M);
    Exit;
  end;

  B  := Integer(Code);
  B1 := Integer(P.CodePtr);
  B2 := B1 + P.CodeSize;

  if (B >= B1) and (B <= B2) then
  begin
    ER := P.EventHandlerList.Add(P, Code, Data, CallConv, RetSize);
    M.Code := @ TEventHandlerRec.Invoke;
    M.Data := ER;
  end
  else
  begin
    M.Code := Code;
    M.Data := Data;
  end;

  SetMethodProp(Instance, PropInfo, M);
end;

procedure _SetEventProp2(P: TProgram; PropInfo: PPropInfo;
                         Instance: TObject;
                         var N: TMethod); stdcall;
var
  M: TMethod;
  ER: TEventHandlerRec;

  B, B1, B2: Integer;
begin
  B  := Integer(N.Code);
  B1 := Integer(P.CodePtr);
  B2 := B1 + P.CodeSize;

  if (B >= B1) and (B <= B2) then
  begin
    ER := P.EventHandlerList.Add(P, N.Code, TObject(N.Data), ccREGISTER, 0);
    M.Code := @ TEventHandlerRec.Invoke;
    M.Data := ER;
  end
  else
  begin
    M := N;
  end;

  SetMethodProp(Instance, PropInfo, M);
end;

procedure AssignRunnerLib;
begin
  DefaultRunnerClass := TProgram;

  RegisterSEH := PAXCOMP_SEH.Register_SEH;

  Address_Exit := @_Exit;
  Address_CondRaise := @_CondRaise;
  Address_LoadSeg := @_LoadSeg;
  Address_CreateObject := @_CreateObject;
  Address_DestroyObject := @_DestroyObject;
  Address_TryOn := @_TryOn;
  Address_TryOff := @_TryOff;
  Address_Raise := @_Raise;
  Address_Pause := @_Pause;
  Address_InitSub := @_InitSub;
  Address_EndSub := @_EndSub;
  Address_SetEventProp := @_SetEventProp;
  Address_SetEventProp2 := @_SetEventProp2;
end;

initialization

  AssignRunnerLibProc := AssignRunnerLib;

end.
