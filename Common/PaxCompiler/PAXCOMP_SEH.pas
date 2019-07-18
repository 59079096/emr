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
unit PAXCOMP_SEH;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_BASESYMBOL_TABLE;

function _PaxSEHHandler(ExcRecord: Pointer;
                        EstablisherFrame: PPaxExcFrame;
                        ContextRecord: Pointer;
                        DispatcherContext: Pointer): Integer; cdecl;

procedure Register_SEH(st: TBaseSymbolTable);

var
  PaxHandlerAddress: Pointer;

implementation

uses
  PAXCOMP_CONSTANTS,
  PAXCOMP_PROG,
  PAXCOMP_CLASSLST,
  PAXCOMP_PAUSE,
  PAXCOMP_TRYLST,
  PAXCOMP_STDLIB;

{$IFDEF UNIX} // Ozz
procedure Register_SEH(st: TBaseSymbolTable);
begin
end;

function _PaxSEHHandler(ExcRecord: Pointer;
                        EstablisherFrame: PPaxExcFrame;
                        ContextRecord: Pointer;
                        DispatcherContext: Pointer): Integer; cdecl;
begin
  result := 0;
  RaiseNotImpl;
end;
{$ELSE}


type
  PExcDescEntry = ^TExcDescEntry;
  TExcDescEntry = record
    VTable:  LongWord; // 32 bit RVA
    Handler: LongWord; // 32 bit RVA
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc = record
    DescCount: Integer;
    DescTable: array [0..0{DescCount-1}] of TExcDescEntry;
  end;
  PExcScope = ^TExcScope;
  TExcScope = record
    BeginOffset:  LongWord;  // 32 bit RVA
    EndOffset:    LongWord;  // 32 bit RVA
    TableOffset:  LongWord;  // 32 bit RVA. 0:TargetOffset=finally block
                             //             1:TargetOffset=safecall catch block
                             //             2:TargetOffset=catch block
                             //             other:TableOffset=TExcDesc
    TargetOffset: LongWord;  // 32 bit RVA. start of finally/catch block.
                             //   TableOffset=0: signature is _TDelphiFinallyHandlerProc
                             //   TableOffset=1: signature is _TDelphiSafeCallCatchHandlerProc
                             //   TableOffset=2: Location to the catch block
                             //   other: TargetOffset=0
  end;
  PExcData = ^TExcData;
  TExcData = record
    ScopeCount: Integer;
    ScopeTable: array [0..0{ScopeCount-1}] of TExcScope;
  end;

// win32
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord =
  record
    ExceptionCode        : Longint;
    ExceptionFlags       : Longint;
    OuterException       : PExceptionRecord;
    ExceptionAddress     : Pointer;
    NumberParameters     : Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of Longint);
    False: (ExceptAddr: Pointer; ExceptObject: Pointer);
  end;

var _Default8087CW: word;
     pp: Pointer;

procedure       _FpuInit; {$IFDEF UNIX}oldfpccall;{$ENDIF}
asm
        FNINIT
        FWAIT
        FLDCW   _Default8087CW
end;

const
  cNonContinuable     = 1;
  cUnwinding          = 2;
  cUnwindingForExit   = 4;
  cUnwindInProgress   = cUnwinding or cUnwindingForExit;
  cDelphiException    = $0EEDFADE;
  cDelphiReRaise      = $0EEDFADF;

{$IFDEF PAX64}
function _PaxSEHHandler(ExcRecord: Pointer;
                        EstablisherFrame: PPaxExcFrame;
                        ContextRecord: Pointer;
                        DispatcherContext: Pointer): Integer; cdecl;
begin
  result := 0;
  RaiseNotImpl;
end;
{$ELSE}
function _PaxSEHHandler(ExcRecord: Pointer;
                        EstablisherFrame: PPaxExcFrame;
                        ContextRecord: Pointer;
                        DispatcherContext: Pointer): Integer; cdecl;
var
  E: Exception;
  ExceptionRecord: PExceptionRecord;
  Prog: TProgram;
  TryBlockNumber: Integer;
  Data, Code: Pointer;

  IsExitException: Boolean;
  IsBreakException: Boolean;
  IsContinueException: Boolean;
  IsPauseException: Boolean;
  IsHaltException: Boolean;
//  HandledByExcept: Boolean;
  TryRec: TTryRec;
  ProgOffset: Integer;
  I: Integer;
  ClsIndex: Integer;
  ClassRec: TClassRec;
  P: Pointer;
  _EBP: Integer;
  _ESP: Integer;
begin
  asm
    CLD
  end;

  ExceptionRecord := ExcRecord;

  result := 1;

//  if (ExceptionRecord^.ExceptionFlags and cUnwindInProgress = 0) then
  if ExceptionRecord^.ExceptionFlags <> 0 then

  begin
    if (ExceptionRecord^.ExceptionCode = cDelphiException) then
    begin
      E := Exception(ExceptionRecord.ExceptObject);
    end
    else
    begin
      asm
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        CLD
        CALL    _FpuInit
        MOV     EDX, pp //ExceptObjProc
        CALL    EDX
        Mov E, EAX
      end;
    end;

    ProgOffset := -1;
    TryRec := nil;

    if PAX_SEH = EstablisherFrame^.Magic then
    begin
      TryBlockNumber := EstablisherFrame^.TryBlockNumber;
      Prog := EstablisherFrame^.Prog;

      if ExceptionRecord^.ExceptionFlags = 1 then
      begin
        Exit;
      end;

      Prog.RootProg.FinallyCount := 0;

      IsExitException := E is PaxExitException;
      IsBreakException := false;
      IsContinueException := false;

      if IsExitException then
      begin
        IsBreakException := (E as PaxExitException).Mode = emBreak;
        IsContinueException := (E as PaxExitException).Mode = emContinue;
        IsExitException := (not IsBreakException) and (not IsContinueException);
      end;

      IsPauseException := E is TPauseException;
      IsHaltException := (E is THaltException) or
                         ((E is EAbort) and (not IsPauseException)
                         and
                         (not IsExitException)
                         and
                         (not IsBreakException)
                         and
                         (not IsContinueException));

      if IsPauseException {or IsHaltException} then
      begin
        Prog.PauseRec.PaxExcFrame1 := EstablisherFrame;

        while EstablisherFrame^.Magic = PAX_SEH do
        begin
          EstablisherFrame := EstablisherFrame^.Next;
        end;

        P := EstablisherFrame;
        asm
          mov eax, P
          mov fs:[0], eax
        end;
        Prog.PauseSEH := true;
        raise E;
      end;

      Prog.IsHalted := IsHaltException;

      Prog.HasError := true;

      if E is THaltException then
        ExitCode := (E as THaltException).ExitCode;

      with Prog do
      if (not IsPauseException) {and (not IsHaltException)} and
         (TryBlockNumber >= 0) and (TryBlockNumber < TryList.Count) then
      begin
        TryRec := TryList[TryBlockNumber];

        if IsExitException then
        if TryRec.TryKind = tryExcept then
        begin
          P := EstablisherFrame^.Next;
          asm
            mov eax, P
            mov fs:[0], eax
          end;
          raise E;
        end;

        if TryRec.TryKind = tryFinally then
        begin
          ProcessingExceptBlock := false;
          if SourceLineFinally = -1 then
          if (not IsExitException) and (not IsPauseException) then
          begin
            SourceLineFinally := GetSourceLine;
            ModuleNameFinally := GetModuleName;
          end;
        end
        else
        begin
          ProcessingExceptBlock := true;
        end;

        if IsBreakException and (TryRec.BreakOffset > 0) then
        begin
          ProgOffset := TryRec.BreakOffset;
          ProcessingExceptBlock := false;
        end
        else if IsContinueException and (TryRec.ContinueOffset > 0) then
        begin
          ProgOffset := TryRec.ContinueOffset;
          ProcessingExceptBlock := false;
        end
        else if TryRec.ExceptOnInfo.Count = 0 then
          ProgOffset := TryRec.ProgOffset
        else
        begin
          for I:=0 to TryRec.ExceptOnInfo.Count - 1 do
          begin
            ClsIndex := TryRec.ExceptOnInfo.Keys[I];
            ProgOffset := TryRec.ExceptOnInfo.Values[I];
            if ClsIndex >= 0 then
            begin
              ClassRec := ClassList[ClsIndex];
              if ClassRec.PClass <> nil then
              begin
                if E is ClassRec.PClass then
                  break;

                if StrEql(ClassRec.PClass.ClassName, 'TJS_Object') then
                  break;
              end;
            end;
          end;
        end;
      end;

      Prog.fCurrException := E;
      if E <> Prog.fPrevException then
      begin
        Prog.fPrevException := E;
        if Assigned(Prog.OnException) then
          if TryRec <> nil then
//        if TryRec.TryKind <> tryFinally then
             if Prog.RootExceptionIsAvailableForHostApplication  then
          begin
            Prog.ExceptionRec := ExceptionRecord;
            Prog.OnException(Prog.Owner, Prog.fCurrException, Prog.GetModuleName, Prog.GetSourceLine);
            Prog.ExceptionRec := nil;
          end;
      end;

      if ProgOffset > 0 then
      begin
        Data := Prog.DataPtr;
        Code := Prog.CodePtr;
        P := ShiftPointer(Code, ProgOffset);
        _EBP := EstablisherFrame^.hEBP;
        _ESP := EstablisherFrame^.hESP;
        Dec(_ESP, SizeOf(TPaxExcFrame));

        asm
          MOV EAX, _ESP
          MOV EAX, DWORD PTR [EAX]
          XOR EBX, EBX
          MOV DWORD PTR FS:[EBX], EAX
        end;

        Inc(_ESP, SizeOf(TPaxExcFrame));
        asm
          mov esi, Data
          mov edi, Code
          mov ebx, P
          mov esp, _ESP
          mov ebp, _EBP

          jmp ebx
        end;
      end;
    end;
  end;
end;
{$ENDIF}

procedure Register_SEH(st: TBaseSymbolTable);
var
  H: Integer;
begin
  with st do
  begin
    H := RegisterNamespace(0, 'PaxCompilerSEH');
    H_PaxCompilerSEH := H;

    PaxHandlerAddress := @_PaxSEHHandler;
    RegisterRoutine(H, '', typeVOID, ccCDECL, PaxHandlerAddress);
    Id_PaxSEHHandler := LastSubId;
  end;
end;

initialization
{$IFDEF VARIANTS}
 _Default8087CW := Get8087CW;
{$ELSE}
 _Default8087CW := system.Default8087CW;
{$ENDIF}

{$IFDEF MACOS32}
{$ELSE}
 pp := ExceptObjProc;
{$ENDIF}
{$ENDIF}

end.
