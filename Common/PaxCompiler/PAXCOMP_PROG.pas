////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PROG.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}

unit PAXCOMP_PROG;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PaxInfos,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_STDLIB,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_LOCALSYMBOL_TABLE,
  PAXCOMP_CLASSLST,
  PAXCOMP_CLASSFACT,
  PAXCOMP_DISASM,
  PAXCOMP_TRYLST,
  PAXCOMP_PAUSE,
  PAXCOMP_RTI,
  PAXCOMP_EVENT,
  PAXCOMP_MAP,
  PAXCOMP_TYPEINFO,
  PAXCOMP_PROGLIST,
  PAXCOMP_GC,
  PAXCOMP_BASERUNNER,
  PAXCOMP_INVOKE,
  PaxInvoke;

type
  TProgram = class;

  TCallStackRec = class
  public
    EBP: Integer;
    SubId: Integer;
    NCall: Integer;
    Prg: TProgram;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TCallStack = class(TTypedList)
  private
    function GetRecord(I: Integer): TCallStackRec;
  public
    function Push(EBP, SubId, NCall: Integer; Prg: TProgram): TCallStackRec;
    procedure Pop;
    function Top: TCallStackRec;
    function IndexOf(SubId: Integer): Integer;
    function LastIndexOf(SubId: Integer): Integer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TCallStackRec read GetRecord; default;
  end;

  TTryStackRec = class
  public
    TryBlockNumber: Integer;
    Prog: TProgram;
    TR: TTryRec;
    constructor Create;
    destructor Destroy; override;
  end;

  TTryStack = class(TTypedList)
  private
    function GetRecord(I: Integer): TTryStackRec;
    function GetTop: TTryStackRec;
  public
    function Push(ATryBlockNumber: Integer; AProg: TProgram): TTryStackRec;
    procedure Pop;
    function IndexOf(ATryBlockNumber: Integer): Integer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Top: TTryStackRec read GetTop;
    property Records[I: Integer]: TTryStackRec read GetRecord; default;
  end;

  TProgram = class(TBaseRunner)
  private
    fTryList: TTryList;
    fTryStack: TTryStack;
    fPauseRec: TPauseRec;

    fESP0: Integer;

    fCallStack: TCallStack;

    InitialOffset: Integer;

    fVirtualAllocProg: Boolean;

    procedure SetVirtualAllocProg(value: Boolean);

    function GetInteger(Shift: Integer): Integer;
    function GetInt64(Shift: Integer): Int64;
    function GetPChar(Shift: Integer): PChar;
    function GetShortString(Shift: Integer): ShortString;
    function GetRootProg: TProgram;

    function GetTryStack: TTryStack;

    function GetCallStack: TCallStack;
    function GetESP0: Integer;
    procedure SetESP0(value: Integer);
    function GetCurrException: Exception;
    procedure SetCurrException(value: Exception);

protected
    function GetProgramSize: Integer; override;
    function _VirtualAlloc(Address: Pointer;
                           Size, flAllocType, flProtect: Cardinal): Pointer; override;
    procedure _VirtualFree(Address: Pointer; Size: Cardinal); override;

    function GetVirtualAllocProg: Boolean;
    function GetCodePtr: PBytes; override;
    procedure DoOnReaderFindMethod(
                Reader: TReader;
                const MethodName: string;
                var Address: Pointer;
                var Error: Boolean);

  public
    EventHandlerList: TEventHandlerList;

    ZList: TIntegerList;

    OwnerEventHandlerMethod: TMethod;

{$IFDEF MSWINDOWS}
    mbi: TMemoryBasicInformation;
{$ENDIF}
    OldProtect: Cardinal;
    IsProtected: Boolean;
    IsPauseUpdated: Boolean;
    ExitLevelId: Integer;
    FinalizationOffset: Integer;

    SourceLineFinally: Integer;
    ModuleNameFinally: String;

    PauseSEH: Boolean;
    ExcFrame0: PExcFrame;

    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
    procedure ResetRun; override;
    function GetDestructorAddress: Pointer; override;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
    procedure Reallocate(NewCodeSize: Integer);
    procedure AssignEventHandlerRunner(MethodAddress: Pointer;
                                       Instance: TObject); override;
    function GetCallStackCount: Integer; override;
    function GetCallStackItem(I: Integer): Integer; override;
    function GetCallStackLineNumber(I: Integer): Integer; override;
    function GetCallStackModuleName(I: Integer): String; override;
    function GetCallStackModuleIndex(I: Integer): Integer; override;

    procedure RunInternal; override;
    procedure Run; override;

    procedure RunInitialization; override;
    procedure RunExceptInitialization; override;
    procedure RunFinalization; override;

    procedure PushPtrs;
    function GetPauseFlag: Integer;
    procedure InitByteCodeLine;
    function IsPaused: Boolean; override;
    procedure Pause; override;
    procedure DiscardPause; override;

    procedure Terminate;
    procedure RemovePause; override;
    function Valid: Boolean; override;
    procedure SetZList;

    function GetImageCodePtr: Integer;
    function GetImageAddress(const FullName: String; var MR: TMapRec): Integer;

    function CreateScriptObject(const ScriptClassName: String;
                                const ParamList: array of const): TObject; override;
    procedure DiscardDebugMode; override;
    procedure RunEx;
    procedure SaveState(S: TStream); override;
    procedure LoadState(S: TStream); override;

    procedure RebindEvents(AnInstance: TObject); override;

    function CallFunc(const FullName: String;
              This: Pointer;
              const ParamList: array of OleVariant;
              OverCount: Integer = 0): OleVariant; override;
    function CallFuncEx(const FullName: String;
                        This: Pointer;
                        const ParamList: array of const;
                        IsConstructor: Boolean = false;
                        OverCount: integer = 0): Variant;
    procedure Protect; override;
    procedure UnProtect; override;
    procedure ResetException;
    procedure SetEntryPoint(EntryPoint: TPaxInvoke); override;
    procedure ResetEntryPoint(EntryPoint: TPaxInvoke); override;

    function GetParamAddress(Offset: Integer): Pointer; overload; override;
    function GetLocalAddress(Offset: Integer): Pointer; overload; override;
    function GetParamAddress(StackFrameNumber, Offset: Integer): Pointer; overload; override;
    function GetLocalAddress(StackFrameNumber, Offset: Integer): Pointer; overload; override;

    property Integers[Shift: Integer]: Integer read GetInteger;
    property Int64s[Shift: Integer]: Int64 read GetInt64;
    property PChars[Shift: Integer]: PChar read GetPChar;
    property ShortStrings[Shift: Integer]: ShortString read GetShortString;
    property TryList: TTryList read fTryList;
    property PauseRec: TPauseRec read fPauseRec;

    property RootTryStack: TTryStack read GetTryStack;

    property RootCallStack: TCallStack read GetCallStack;
    property RootESP0: Integer read GetESP0 write SetESP0;
    property CurrException: Exception read GetCurrException write SetCurrException;
    property VirtualAllocProg: Boolean
      read GetVirtualAllocProg write SetVirtualAllocProg;
    property RootProg: TProgram read GetRootProg;
  end;

procedure ZZZ;

implementation

uses
  PAXCOMP_PROGLIB,
  PAXCOMP_JavaScript;

// TCallStackRec ---------------------------------------------------------------

procedure TCallStackRec.SaveToStream(S: TStream);
begin
  S.Write(EBP, SizeOf(Integer));
  S.Write(SubId, SizeOf(Integer));
  S.Write(NCall, SizeOf(Integer));
end;

procedure TCallStackRec.LoadFromStream(S: TStream);
begin
  S.Read(EBP, SizeOf(Integer));
  S.Read(SubId, SizeOf(Integer));
  S.Read(NCall, SizeOf(Integer));
end;

// TCallStack ------------------------------------------------------------------

function TCallStack.GetRecord(I: Integer): TCallStackRec;
begin
  result := TCallStackRec(L[I]);
end;

function TCallStack.Push(EBP, SubId, NCall: Integer;
                         Prg: TProgram): TCallStackRec;
begin
  result := TCallStackRec.Create;
  result.EBP := EBP;
  result.SubId := SubId;
  result.NCall := NCall;
  result.Prg := Prg;
  L.Add(result);
end;

procedure TCallStack.Pop;
begin
  RemoveAt(Count - 1);
end;

function TCallStack.Top: TCallStackRec;
begin
  result := TCallStackRec(L[Count - 1]);
end;

function TCallStack.IndexOf(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].SubId = SubId then
    begin
      result := I;
      Exit;
    end;
end;

function TCallStack.LastIndexOf(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := Count - 1 downto 0 do
    if Records[I].SubId = SubId then
    begin
      result := I;
      Exit;
    end;
end;

procedure TCallStack.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TCallStack.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TCallStackRec;
begin
  S.Read(K, SizeOf(Integer));
  for I := 0 to K - 1 do
  begin
    R := TCallStackRec.Create;
    R.LoadFromStream(S);
    L.Add(R);
  end;
end;

// TTryRec ---------------------------------------------------------------------

constructor TTryStackRec.Create;
begin
  inherited;
end;

destructor TTryStackRec.Destroy;
begin
  if TR <> nil then
    FreeAndNil(TR);

  inherited;
end;

// TTryStack -------------------------------------------------------------------

function TTryStack.GetRecord(I: Integer): TTryStackRec;
begin
  result := TTryStackRec(L[I]);
end;

function TTryStack.GetTop: TTryStackRec;
begin
  if Count  = 0 then
    raise PaxCompilerException.Create(errInternalError);
  result := Records[Count - 1];
end;

function TTryStack.IndexOf(ATryBlockNumber: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].TryBlockNumber = ATryBlockNumber then
    begin
      result := I;
      Exit;
    end;
end;

function TTryStack.Push(ATryBlockNumber: Integer; AProg: TProgram): TTryStackRec;
begin
  result := TTryStackRec.Create;
  result.TryBlockNumber := ATryBlockNumber;
  result.Prog := AProg;
  result.TR := AProg.TryList[ATryBlockNumber].Clone;
  L.Add(result);
end;

procedure TTryStack.Pop;
begin
  Records[Count - 1].Free;
  L.Delete(Count - 1);
end;

procedure TTryStack.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
  with Records[I] do
  begin
    S.Write(TryBlockNumber, SizeOf(TryBlockNumber));
    S.Write(Prog, SizeOf(Prog));
    TR.SaveToStream(S);
  end;
end;

procedure TTryStack.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TTryStackRec;
begin
  Clear;
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
  begin
    R := TTryStackRec.Create;
    with R do
    begin
      S.Read(TryBlockNumber, SizeOf(TryBlockNumber));
      S.Read(Prog, SizeOf(Prog));
      TR := TTryRec.Create;
      TR.LoadFromStream(S);
    end;
    L.Add(R);
  end;
end;

// TProgram --------------------------------------------------------------------

constructor TProgram.Create;
begin
  inherited Create;

{$IFDEF MSWINDOWS}
  fVirtualAllocProg := true;
{$ENDIF}

  CurrProg := Self;

  fTryList := TTryList.Create;
  fTryStack := TTryStack.Create;
  fPauseRec := TPauseRec.Create;

  EventHandlerList := TEventHandlerList.Create;

  fCallStack := TCallStack.Create;

  EPoint := nil;

  PCUOwner := nil;
  ZList := TIntegerList.Create;

  IsRunning := false;

  UseMapping := false;
end;

destructor TProgram.Destroy;
begin
  ResetException;
  UnloadDlls;
  FreeAndNil(ZList);

  FreeAndNil(fTryList);
  FreeAndNil(fTryStack);
  FreeAndNil(fPauseRec);
  FreeAndNil(fCallStack);

  try
    Deallocate;
  except
  end;

  FreeAndNil(EventHandlerList);

  ClearCurrException;

  inherited;
end;

procedure TProgram.Reset;
begin
  inherited;

  fImageDataPtr := 0;

  ZList.Clear;
  fTryList.Clear;
  fTryStack.Clear;
  fPauseRec.Clear;
  EventHandlerList.Clear;
  fCallStack.Clear;

  RootInitCallStackCount := 0;
  Deallocate;
  EPoint := nil;

  IsRunning := false;
  RootIsEvent := false;

  PauseSEH := false;

  FinallyCount := 0;

  PCULang := 0;
end;

procedure TProgram.ResetRun;
begin
  fTryStack.Clear;
  fPauseRec.Clear;
  fCallStack.Clear;
  RootInitCallStackCount := 0;
  EPoint := nil;

  IsRunning := false;
  RootIsEvent := false;
  PauseSEH := false;
end;

procedure TProgram.SetZList;
var
  I, S: Integer;
  P: Pointer;
begin
{$IFDEF PAX64}
  for I:=0 to ZList.Count - 1 do
  begin
    S := ZList[I];

    P := ShiftPointer(CodePtr, S + 2);
    Pointer(P^) := CodePtr;

    P := ShiftPointer(P, 10);
    Pointer(P^) := DataPtr;
  end;
{$ELSE}
  for I:=0 to ZList.Count - 1 do
  begin
    S := ZList[I];

    P := ShiftPointer(CodePtr, S + 1);
    Pointer(P^) := CodePtr;

    P := ShiftPointer(P, 5);
    Pointer(P^) := DataPtr;
  end;
{$ENDIF}
end;

function TProgram.GetInteger(Shift: Integer): Integer;
var
  P: Pointer;
begin
  P := ShiftPointer(DataPtr, Shift);
  result := LongInt(P^);
end;

function TProgram.GetInt64(Shift: Integer): Int64;
var
  P: Pointer;
begin
  P := ShiftPointer(DataPtr, Shift);
  result := Int64(P^);
end;

function TProgram.GetPChar(Shift: Integer): PChar;
var
  P: Pointer;
begin
  P := ShiftPointer(DataPtr, Shift);
  result := PChar(P^);
end;

function TProgram.GetShortString(Shift: Integer): ShortString;
var
  P: Pointer;
begin
  P := ShiftPointer(DataPtr, Shift);
  result := ShortString(P^);
end;

procedure TProgram.Reallocate(NewCodeSize: Integer);
var
  buff: Pointer;
begin
  if NewCodeSize = CodeSize then
    Exit;

  if NewCodeSize < CodeSize then
    RaiseError(errInternalError, []);

  Unprotect;

  buff := _VirtualAlloc(nil, CodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  Move(Prog^, buff^, CodeSize);
  _VirtualFree(Prog, CodeSize);
  Prog := _VirtualAlloc(nil, NewCodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  Move(buff^, Prog^, CodeSize);
  _VirtualFree(buff, CodeSize);
  CodeSize := NewCodeSize;

  Protect;
end;

function TProgram.Valid: Boolean;
begin
  result := (Data <> nil) and (Prog <> nil);
end;

function TProgram.GetImageAddress(const FullName: String; var MR: TMapRec): Integer;
begin
  result := 0;
  MR := ScriptMapTable.Lookup(FullName);
  if MR <> nil then
  begin
    case MR.Kind of
      KindVAR, kindTYPE: result := GetImageDataPtr + MR.Offset;
      KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
      begin
        if MR.IsExternal then
          result := 0
        else
          result := GetImageCodePtr + MR.Offset;
      end;
    end;
    Exit;
  end;

  MR := HostMapTable.Lookup(FullName);
  if MR <> nil then
  if MR.Kind in KindSUBS + [KindVAR] then
  begin
    result := GetImageDataPtr + MR.Offset;
//    result := Pointer(result^);
  end;
end;

function TProgram.GetCodePtr: PBytes;
begin
  result := Prog;
end;

function TProgram.GetPauseFlag: Integer;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_Flag);
  result := LongInt(P^);
end;

procedure TProgram.InitByteCodeLine;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_ByteCodePtr);
  LongInt(P^) := -1;
end;

function TProgram.GetImageCodePtr: Integer;
begin
  result := GetImageDataPtr + DataSize;
end;

procedure TProgram.SaveToStream(S: TStream);
var
  StartSize, EndSize, StartPos, EndPos, StreamSize: Integer;
  CustomDataSize, CustomDataSizePos, temp: Integer;
  SS: ShortString;
begin
  StartSize := S.Size;
  StartPos  := S.Position;

  S.Write(StreamSize, SizeOf(Integer));
  S.Write(CompiledScriptVersion, SizeOf(CompiledScriptVersion));

  PShortStringFromString(@ SS, TProgram.ClassName);
  SaveShortStringToStream(SS, S);

  CustomDataSize := 0;
  CustomDataSizePos := S.Position;
  S.Write(CustomDataSize, SizeOf(Integer));
  if Assigned(OnSaveToStream) and IsRootProg then
  begin
    OnSaveToStream(Owner, S);
    CustomDataSize := S.Position - CustomDataSizePos - 4;
    if CustomDataSize > 0 then
    begin
      temp := S.Position;
      S.Position := CustomDataSizePos;
      S.Write(CustomDataSize, SizeOf(Integer));
      S.Position := temp;
    end
    else
    begin
      CustomDataSize := 0;
      S.Position := CustomDataSizePos;
      S.Write(CustomDataSize, SizeOf(Integer));
    end;
  end;

  S.Write(DataSize, SizeOf(Integer));
  S.Write(fCodeSize, SizeOf(Integer));

  fImageDataPtr := S.Position - StartPos;

  S.Write(Data^, DataSize);
  S.Write(Prog^, fCodeSize);
  S.Write(JS_Record, SizeOf(JS_Record));

  S.Write(ModeSEH, SizeOf(ModeSEH));
  S.Write(PAX64, SizeOf(PAX64));

  if GENERICS_ALLOWED then
   S.Write(PCULang, SizeOf(PCULang));

  ClassList.SaveToStream(S);
  RunTimeModuleList.SaveToStream(S);
  TryList.SaveToStream(S);
  ZList.SaveToStream(S);

  HostMapTable.SaveToStream(S);
  ScriptMapTable.SaveToStream(S);
  OffsetList.SaveToStream(S);
  ExportList.SaveToStream(S);
  MessageList.SaveToStream(S);
  ProgTypeInfoList.SaveToStream(S);

  ProgList.SaveToStream(S);

  EndSize := S.Size;
  EndPos  := S.Position;
  StreamSize := EndSize - StartSize;
  S.Position := StartPos;
  S.Write(StreamSize, SizeOf(Integer));
  S.Position := EndPos;
end;

procedure TProgram.LoadFromStream(S: TStream);
var
 Version: Integer;
 K: Integer;
 CustomDataSize, temp: Integer;
 P: Pointer;
 SS: ShortString;
 ST: String;
begin
  Deallocate;
  S.Read(K, SizeOf(Integer));
  S.Read(Version, SizeOf(CompiledScriptVersion));
  if Version <> CompiledScriptVersion then
   RaiseError(errIncorrectCompiledScriptVersion, []);

  SS := LoadShortStringFromStream(S);
  ST := TProgram.ClassName;
  if not StrEql(StringFromPShortString(@SS), ST) then
    RaiseError(errIncorrectCompiledScriptVersion, []);

  S.Read(CustomDataSize, SizeOf(Integer));
  if Assigned(OnLoadFromStream) and IsRootProg then
  begin
    temp := S.Position;
    OnLoadFromStream(Owner, S);
    if S.Position - temp <> CustomDataSize then
      RaiseError(errIncorrectCustomDataSize, []);
  end
  else
    if CustomDataSize > 0 then
    begin
      P := AllocMem(CustomDataSize);
      try
        S.Read(P^, CustomDataSize);
      finally
        FreeMem(P, CustomDataSize);
      end;
    end;

  S.Read(fDataSize, SizeOf(Integer));
  S.Read(fCodeSize, SizeOf(Integer));

  Data := AllocMem(fDataSize);
  Prog := _VirtualAlloc(nil, fCodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

  S.Read(Data^, fDataSize);
  S.Read(Prog^, fCodeSize);
  S.Read(JS_Record, SizeOf(JS_Record));

  S.Read(ModeSEH, SizeOf(ModeSEH));
  S.Read(PAX64, SizeOf(PAX64));
  if GENERICS_ALLOWED then
   S.Read(PCULang, SizeOf(PCULang));

{$IFDEF MACOS}
  ModeSEH := false;
{$ENDIF}

  ClassList.Clear;
  ClassList.LoadFromStream(S, Version);

  RunTimeModuleList.Clear;
  RunTimeModuleList.LoadFromStream(S);
  TryList.Clear;
  TryList.LoadFromStream(S);

  ZList.Clear;
  ZList.LoadFromStream(S);

  HostMapTable.Clear;
  HostMapTable.LoadFromStream(S);
  ScriptMapTable.Clear;
  ScriptMapTable.LoadFromStream(S);

  OffsetList.Clear;
  OffsetList.LoadFromStream(S);
  ExportList.Clear;
  ExportList.LoadFromStream(S);
  MessageList.Clear;
  MessageList.LoadFromStream(S);
  ProgTypeInfoList.Clear;
  ProgTypeInfoList.LoadFromStream(S);

  ProgList.Clear;
  ProgList.LoadFromStream(S, Self);
  ProgList.SetPCUOwner(Self);

  UseMapping := HostMapTable.Count > 0;

  SetAddress(H_SelfPtr, Self);
  SetAddress(H_ExceptionPtr, @ fCurrException);

  RegisterDefinitions(GlobalSym);
  if UseMapping then
  begin
   FreeAndNil(LocalSymbolTable);
   LocalSymbolTable := TProgSymbolTable.Create(GlobalSym);

   LocalSymbolTable.Reset;
   RegisterDefinitions(LocalSymbolTable);
  end;

  SetZList;
  SetupInterfaces(CodePtr);

  ProgClassFactory.ForceCreate := true;
end;

function TProgram.IsPaused: Boolean;
begin
  result := RootProg.PauseRec.ProgOffset > 0;
end;

{
procedure TProgram.Resume;
begin
  if not IsPaused then
    RaiseError(errProgramIsNotPaused, []);
  Run;
end;
}

procedure TProgram.Pause;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_Flag);
  LongInt(P^) := 1;
end;

procedure TProgram.Terminate;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_Flag);
  LongInt(P^) := 2;
end;

procedure TProgram.RemovePause;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_Flag);
  LongInt(P^) := 0;
end;

procedure TProgram.Protect;
begin
{$IFDEF MSWINDOWS}
  if IsProtected then
    Exit;

  VirtualQuery(Prog, mbi, sizeof(mbi));
//  VirtualProtect(mbi.BaseAddress, mbi.RegionSize, PAGE_EXECUTE_READWRITE, OldProtect);
  VirtualProtect(Prog, fCodeSize, PAGE_EXECUTE_READWRITE, OldProtect);
  FlushInstructionCache(GetCurrentProcess, Prog, fCodeSize);
// Applications should call FlushInstructionCache if they generate or modify
// code in memory. The CPU cannot detect the change, and may execute the old
// code it cached.
{$ENDIF}
  IsProtected := true;
end;

procedure TProgram.UnProtect;
begin
{$IFDEF MSWINDOWS}
  if not IsProtected then
    Exit;

//  VirtualProtect(mbi.BaseAddress, mbi.RegionSize, OldProtect, OldProtect);
  VirtualProtect(Prog, fCodeSize, OldProtect, OldProtect);
{$ENDIF}

  IsProtected := false;
end;

procedure TProgram.ResetException;
var
  aPrg  : TProgram;
  i     : integer;
begin
  for i := 0 to ProgList.count - 1 do
  begin
    aPrg := TProgram(ProgList.Records[i].Prog);
    if (aPrg <> nil) then
      aPrg.ResetException;
  end;

  if HasError then
  begin
    if fCurrException <> nil then
      fCurrException.Free;
    fCurrException := nil;
    fPrevException := nil;
    ExceptionRec   := nil;
    HasError       := false;
    fGC.Collect;
  end;
end;

{$IFDEF PAX64}
function GetFS0: Pointer; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov rax, fs:[0]
end;

function GetRSP: IntPax; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov rax, rsp
end;

procedure CopyStackFrame(I: IntPax;
                        StackFrame: Pointer;
                        StackFrameSize, K: IntPax);
asm
  mov rax, I
  mov rbx, StackFrame

  add rbx, StackFrameSize //!!
  sub rbx, 8              //!!

  mov rcx, K

  @@loop:

  mov rdx, [rbx]
  mov [rax], rdx

  sub rax, 8
  sub rbx, 8

  sub rcx, 1
  cmp rcx, 0
  jnz @@loop
end;

procedure AssignFS0(I: IntPax); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov fs:[0], rcx
end;

procedure AssignSegmentsAndJump(D, P0, P: Pointer;
                                _ESP, _EBP: IntPax); assembler;
asm
        // assign code and data registers
  mov rsi, D
  mov rdi, P0

  mov rax, P

  mov rsp, _ESP
  mov rbp, _EBP

  jmp rax
end;

procedure AssignSegmentsAndCall(D, P: Pointer); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  push rbp
  mov rbp, rsp

  mov rsi, rcx
  mov rdi, rdx

  call rdx

  pop rbp
  ret
end;

procedure AssignSegments(D, P: Pointer); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov rsi, rcx
  mov rdi, rdx
end;

procedure Assign_R14(P: Pointer); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov r14, rcx
end;

procedure Assign_R15(P: Pointer); assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov r15, rcx
end;

procedure Call_R15; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  push rbp
  sub rsp, $1000
  mov rbp, rsp

//        if EPoint.IsInternal then
//          EPoint.PushArgumentsBackward
//        else
//          EPoint.PushArguments;

  mov rcx, r14
  call TInvoke.IsInternal
  cmp al, 0
  jz @@l1
  mov rcx, r14
  call TInvoke.PushArgumentsBackward
  jmp @@l2
  @@l1:
  mov rcx, r14
  call TInvoke.PushArguments
  @@l2:

  call r15
  add rsp, $1000
  pop rbp
  ret
end;

{$ENDIF}

procedure TProgram.Run;
var
  PaxFrame: PPaxExcFrame;
  Delta: IntPax;
  I: Integer;
  D, P, P0, temp: Pointer;
  ProgOffset: Integer;
  Handled: Boolean;
  _EBP, _ESP: IntPax;
  TryRec: TTryRec;
  ClsIndex: Integer;
  ClassRec: TClassRec;
  StackFrame: Pointer;
  K: Integer;
  SourceLine: Integer;
  ModuleName: String;
  CE: TExceptionClass;
  PEpoint: Pointer;
  StackFrameSize: Integer;
  IsHaltException: Boolean;
  IsPauseException: Boolean;
  IsExitException: Boolean;
  TryBlockNumber: Integer;

  SelfPtr: TProgram;
  HandledByExcept: Boolean;

label
  Again;
begin
//  PChars[0];
//  ShortStrings[0];
  Integers[0];
  Int64s[0];
  SourceLineFinally := -1;
  ModuleNameFinally := '';

  IsRootProg;

  if ProgClassFactory.ForceCreate then
  begin
    CreateClassFactory;
    ProgClassFactory.ForceCreate := false;
  end;

  IsRunning := true;
  IsHalted := false;
  IsPauseUpdated := false;

  PEpoint := nil;

  ProgOffset := 0;
  Handled := false;

  SelfPtr := Self;

  with SelfPtr do
  begin
    D  := Data;
    P0 := Prog;
  end;

  SourceLine := -1;
  ExitCode := 0;

  IsHaltException := false;
  IsPauseException := false;

  RootInitCallStackCount := fCallStack.Count;

  if IsPaused then
  begin
    Handled := true;
    StackFrameSize := PauseRec.StackFrameSize;
    K := StackFrameSize div 4;
    StackFrame := PauseRec.StackFrame;
    ProgOffset := PauseRec.ProgOffset;
    _ESP := PauseRec._ESP;
    _EBP := PauseRec._EBP;

    PauseRec.ProgOffset := 0;
  end;

  RemovePause;

Again:

  if HasError then
    GetRootProg.ResetException;

  HasError := false;
  HandledByExcept := false;

  try
    if ModeSEH then
    begin
      {$IFDEF PAX64}
//      temp := GetFS0;
      {$ELSE}
      asm
        mov eax, fs:[0]
        mov temp, eax
      end;
      {$ENDIF}
      ExcFrame0 := temp;
    end;

    if Handled then
    begin
      Handled := false;

      {$IFDEF PAX64}
      I := GetRSP;
      {$ELSE}
      asm
        mov I, esp
      end;
      {$ENDIF}

      PaxFrame := PauseRec.PaxExcFrame1;

      Delta := fESP0 - I;
      fESP0 := I;
      _ESP := _ESP - Delta;
      _EBP := _EBP - Delta;

      for I := 0 to fCallStack.Count - 1 do
        fCallStack[I].EBP := fCallStack[I].EBP - Delta;

      {$IFDEF PAX64}
        // restore stack frame
      I := fESP0 - 8;
      CopyStackFrame(I, StackFrame, StackFrameSize, K);
      P := Pointer(LongInt(P0) + ProgOffset);

      if ModeSEH and PauseSEH then
      begin
        IntPax(PaxFrame) :=
          Integer(PaxFrame) - Delta;
        I := IntPax(PaxFrame);
        AssignFS0(I);
        while PaxFrame.Magic = PAX_SEH do
        begin
          PaxFrame^.hEBP := PaxFrame^.hEBP - Delta;
          PaxFrame^.hESP := PaxFrame^.hESP - Delta;

          PaxFrame^.Next := Pointer(Integer(PaxFrame^.Next) - Delta);
          PaxFrame := PaxFrame^.Next;
        end;
        PaxFrame^.Next := Pointer(ExcFrame0);

        PauseSEH := false;
      end;
      AssignSegmentsAndJump(D, P0, P, _ESP, _EBP);
      // end of win64

      {$ELSE} //win32
        // restore stack frame
      I := fESP0 - 4;

      asm
        mov eax, I
        mov ebx, StackFrame

        add ebx, StackFrameSize //!!
        sub ebx, 4              //!!

        mov ecx, K

        @@loop:

        mov edx, [ebx]
        mov [eax], edx

        sub eax, 4

//        add ebx, 4
        sub ebx, 4

        sub ecx, 1
        cmp ecx, 0
        jnz @@loop
      end;

      P := Pointer(LongInt(P0) + ProgOffset);

      if ModeSEH and PauseSEH then
      begin
        Integer(PaxFrame) :=
          Integer(PaxFrame) - Delta;
        I := Integer(PaxFrame);
        asm
          mov eax, I
          mov fs:[0], eax
        end;
        while PaxFrame.Magic = PAX_SEH do
        begin
          PaxFrame^.hEBP := PaxFrame^.hEBP - Delta;
          PaxFrame^.hESP := PaxFrame^.hESP - Delta;

          PaxFrame^.Next := Pointer(Integer(PaxFrame^.Next) - Delta);
          PaxFrame := PaxFrame^.Next;
        end;
        PaxFrame^.Next := Pointer(ExcFrame0);

        PauseSEH := false;
      end;

      asm
        // assign code and data registers
        mov esi, D
        mov edi, P0

        mov eax, P

        mov esp, _ESP
        mov ebp, _EBP

        jmp eax
      end;
      {$ENDIF} // win32
    end
    else
    begin
      InitByteCodeLine;

     {$IFDEF PAX64}
      _ESP := GetRSP();
      {$ELSE}
      asm
        mov _ESP, esp
      end;
      {$ENDIF}

      fESP0 := _ESP;

{$IFDEF PCU_EX}
      RootProg.fESP0 := fESP0;
{$ENDIF}

      if EPoint = nil then
      begin
        P := P0;

        {$IFDEF PAX64}
        AssignSegmentsAndCall(D, P);
        {$ELSE}
        asm
          mov esi, D
          mov edi, P
{$IFDEF MACOS}
          add esp, - $0c
{$ENDIF}
          call P

{$IFDEF MACOS}
          add esp, $0c
{$ENDIF}
       end;
       {$ENDIF}
      end
      else
      begin
        if not EPoint.IsInternal then
          EPoint.Setup;

        PEpoint := EPoint;

//        P  := ShiftPointer(EPoint.Address, 14);
        P  := EPoint.Address;

        {$IFDEF PAX64}
        AssignSegments(D, P0);
        Assign_R14(EPoint);
        Assign_R15(P);
        Call_R15;
        EPoint.SaveResult;
        {$ELSE}
        asm
          mov esi, D
          mov edi, P0
        end;

        if EPoint.IsInternal then
          EPoint.PushArgumentsBackward
        else
          EPoint.PushArguments;

        asm
          call P
        end;

        asm
          mov ebx, PEpoint

          cmp ebx, 0
          jz @@Return

          // if call convention is cdecl then pop arguments
          mov ecx, [ebx + 28] // fCallConv
          cmp ecx, ccCDECL
          jnz @@Ret
          mov ecx, [ebx + 8] // fStackSize
          add esp, ecx

          @@Ret:

          mov ecx, [ebx + 32] // fResultType

          cmp ecx, typeINTEGER
          jnz @@RetDOUBLE
          mov ecx, [ebx + 28] // fCallConv
          cmp ecx, ccSAFECALL
          jz @@Return
          mov [ebx + INVOKE_RESULT_OFFSET], eax
          jmp @@Return
        //

          @@RetDOUBLE:

          cmp ecx, typeDOUBLE
          jnz @@RetSINGLE
          fstp qword ptr [ebx + INVOKE_RESULT_OFFSET]
          jmp @@Return
        //

          @@RetSINGLE:

          cmp ecx, typeSINGLE
          jnz @@RetEXTENDED
          fstp dword ptr [ebx + INVOKE_RESULT_OFFSET]
          jmp @@Return
        //

          @@RetEXTENDED:

          cmp ecx, typeEXTENDED
          jnz @@RetCURRENCY
          fstp tbyte ptr [ebx + INVOKE_RESULT_OFFSET]
          jmp @@Return
        //

          @@RetCURRENCY:

          cmp ecx, typeCURRENCY
          jnz @@RetINT64
          fistp qword ptr [ebx + INVOKE_RESULT_OFFSET]
          jmp @@Return
        //

          @@RetINT64:
          cmp ecx, typeINT64
          jnz @@Return
          mov [ebx + INVOKE_RESULT_OFFSET], eax
          mov [ebx + INVOKE_RESULT_OFFSET + 4], edx

          @@Return:
        end;
        {$ENDIF}

      end;
    end;
  except
    on E: Exception do
    begin
      if fTryStack.Count > 0 then
      begin
        TryBlockNumber := fTryStack.Top.TryBlockNumber;
        SelfPtr := fTryStack.Top.Prog;
      end
      else
      begin
        SelfPtr := Self;
        TryBlockNumber := 0;
      end;

      with SelfPtr do
      begin
        D  := Data;
        P0 := Prog;
        SourceLine := GetSourceLine;
        ModuleName := GetModuleName;
      end;

      IsExitException := E is PaxExitException;
      IsPauseException := E is TPauseException;
      IsHaltException := (E is THaltException) or
                         ((E is EAbort) and (not IsPauseException) and (not IsExitException));

      IsHalted := IsHaltException;

      HasError := true;

      if E is THaltException then
        ExitCode := (E as THaltException).ExitCode;

      with SelfPtr do
      if RootTryStack.Count > 0 then
      if (not IsPauseException) and (not IsHaltException) and
         (TryBlockNumber >= 0) and (TryBlockNumber < TryList.Count) then
      begin
//        TryRec := TryList[TryBlockNumber];
         TryRec := fTryStack.Top.TR;
        _EBP := TryRec._EBP;
        _ESP := TryRec._ESP;

        K := TryRec.StackFrameSize div 4;
        StackFrame := TryRec.StackFrame;
        StackFrameSize := TryRec.StackFrameSize;

        if TryRec.TryKind = tryFinally then
        begin
          ProcessingExceptBlock := false;
          if SourceLineFinally = -1 then
          begin
            SourceLineFinally := GetSourceLine;
            ModuleNameFinally := GetModuleName;
          end;
        end
        else
        begin
          ProcessingExceptBlock := true;
          HandledByExcept := true;
        end;

        if TryRec.ExceptOnInfo.Count = 0 then
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

        Handled := true;
      end;

      if Assigned(fCurrException) then
        FreeAndNil(fCurrException);

      fPrevException := nil;

      if (not IsPauseException) and (not IsHaltException) then
      begin
        IntPax(CE) := IntPax(E.ClassType);
        fCurrException := CE.Create(E.Message);

        if Assigned(OnCustomExceptionHelper) then
          OnCustomExceptionHelper(Owner, E, fCurrException);
      end;

    end; // on: E Exception
    else
    begin
      // custom exception
    end;
  end; // try

  RuntimeModuleList.TempBreakpoint.Clear;

  if Handled then
  begin
    if Assigned(OnException) and RootExceptionIsAvailableForHostApplication then
    if HandledByExcept then
      OnException(Owner, fCurrException, ModuleName, SourceLine);
    RootExceptionIsAvailableForHostApplication := true;
    goto Again;
  end
  else
  begin
    IsRunning := false;

    if (not SuspendFinalization) and (ProgTag <> 1) then
      fGC.ClearObjects;

    if HasError then
    begin
      if Assigned(OnHalt) and IsHaltException then
      begin
        OnHalt(Owner, ExitCode, ModuleName, SourceLine);
        RootExceptionIsAvailableForHostApplication := true;
        fPauseRec.Clear;
        ClearCurrException;
        Exit;
      end
      else if Assigned(OnPause) and IsPauseException then
      begin
        OnPause(Owner, ModuleName, SourceLine);
        RootExceptionIsAvailableForHostApplication := true;
        ClearCurrException;
        Exit;
      end;

      if Assigned(OnUnhandledException) then
      if fCurrException <> nil then
      if RootExceptionIsAvailableForHostApplication then
      begin
        if SourceLineFinally = -1 then
          OnUnhandledException(Owner, fCurrException, ModuleName, SourceLine)
        else
          OnUnhandledException(Owner, fCurrException, ModuleNameFinally, SourceLineFinally);
      end;
      RootExceptionIsAvailableForHostApplication := true;
    end;
  end;
  ClearCurrException;
end;

{$O-}
procedure TProgram.RunInternal;
begin
  Run;
end;

procedure TProgram.RunInitialization;
var
  P: Pointer;
begin
  if InitializationIsProcessed then
    Exit;

  Protect;

  if fGC = RootGC then
    fGC.Clear;

  ProgList.RunInitialization;

  P := ShiftPointer(Data, H_InitOnly);
  LongInt(P^) := 2;
  try
    ProgTag := 1;
    Run;
    InitMessageList;
  finally
    ProgTag := 0;
    LongInt(P^) := 0;
    InitializationIsProcessed := true;
    InitialOffset := GetInitializationOffset(CodePtr, CodeSize, PAX64);
  end;
end;

procedure TProgram.RunExceptInitialization;
var
  P: Pointer;
begin
  if InitialOffset <= 0 then
    InitialOffset := GetInitializationOffset(CodePtr, CodeSize, PAX64);
  if InitialOffset = -1 then
    Exit;

  P := ShiftPointer(Data, H_BodyOnly);
  if SuspendFinalization then
    LongInt(P^) := 3
  else
    LongInt(P^) := 0;

  EPoint := nil;

  P := ShiftPointer(CodePtr, 1);
  Move(InitialOffset, P^, 4);
  try
    Run;
  finally
    LongInt(P^) := 0;
  end;
end;

procedure TProgram.RunFinalization;
var
  P: Pointer;
  Offset: Integer;
begin
  if CodePtr = nil then
    Exit;

  ProgList.RunFinalization;

  Offset := GetFinalizationOffset(CodePtr, CodeSize, PAX64);
  if Offset = -1 then
    Exit;

  EPoint := nil;

  P := ShiftPointer(CodePtr, 1);
  Move(Offset, P^, 4);
  try
    Run;
  finally
    InitializationIsProcessed := false;

    LongInt(P^) := 0;
    Unprotect;
  end;
end;

procedure TProgram.SaveState(S: TStream);
var
  K: Integer;
begin
  S.Write(DataPtr^, DataSize);
  fCallStack.SaveToStream(S);

  K := fTryStack.Count;
  S.Write(K, SizeOf(Integer));

//  fTryStack.SaveToStream(S);
end;

procedure TProgram.LoadState(S: TStream);
var
  K: Integer;
begin
  S.Read(DataPtr^, DataSize);
  fCallStack.Clear;
  fCallStack.LoadFromStream(S);

  S.Read(K, SizeOf(Integer));
  while fTryStack.Count > K do
    fTryStack.Pop;

//  fTryStack.LoadFromStream(S);
end;

{$IFDEF PAX64}
procedure TProgram.PushPtrs;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  mov rcx, [rsp]
  push rcx
  push rcx

  mov rdx, [rax + 8]
  mov [rsp + 8], rdx

  mov rdx, [rax + 8 + 8]
  mov [rsp + 8 + 8], rdx
end;
{$ELSE}
procedure TProgram.PushPtrs; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
{$ENDIF}
  mov ecx, [esp]
  push ecx
  push ecx

  mov edx, [eax + 4]
  mov [esp + 4], edx

  mov edx, [eax + 8]
  mov [esp + 8], edx
end;
{$ENDIF}

function TProgram.GetProgramSize: Integer;
begin
  result := DataSize + CodeSize + 2;
end;

procedure TProgram.DiscardDebugMode;
begin
  PAXCOMP_DISASM.DiscardDebugMode(CodePtr, CodeSize, PAX64);
end;

{$IFDEF PAX64}
procedure Call_M_2(Code, Data: Pointer); assembler;
asm
  CALL RCX
end;
procedure Call_M(M: TMethod);
begin
  Call_M_2(M.Code, M.Data);
end;
{$ENDIF}

{$IFDEF PAX64}
procedure TProgram.RunEx;
var
  M: TMethod;
begin
  M := OwnerEventHandlerMethod;
  if Assigned(M.Code) then
  begin
    Call_M(M);
  end
  else
    Run;
end;
{$ELSE}
procedure TProgram.RunEx;
var
  M: TMethod;
begin
  M := OwnerEventHandlerMethod;
  if Assigned(M.Code) then
  begin
    asm
      MOV  EAX,DWORD PTR M.Data;
      CALL M.Code;
    end;
  end
  else
    Run;
end;
{$ENDIF}

procedure TProgram.DoOnReaderFindMethod(
                      Reader: TReader;
                      const MethodName: string;
                      var Address: Pointer;
                      var Error: Boolean);
Var
  aFullName: String;
  ER: TEventHandlerRec;
  MR: TMapRec;
  M: TMethod;
begin
  aFullName := ProgTypeInfoList.FindMethodFullName(Address);
  Address := GetAddress(aFullName, MR);
  M.Code := Address;
  M.Data := gInstance;
  Error := Address = Nil;

  ER := EventHandlerList.Add(Self,
                             M.Code, M.Data,
                             GetCallConv(aFullName),
                             GetRetSize(aFullName));

  M.Code := @ TEventHandlerRec.Invoke;
  M.Data := ER;

//  Address := nil;
end;

procedure TProgram.RebindEvents(AnInstance: TObject);

  procedure _RebindEvents(Instance: TObject);
  var
    pti, PropType: PTypeInfo;
    ptd: PTypeData;
    Loop, nProps: Integer;
    pProps: PPropList;
    ppi: PPropInfo;
    M: TMethod;
    C: TComponent;
    I: Integer;
    aFullName: String;
    ER: TEventHandlerRec;
  begin
    pti := Instance.ClassInfo;
    if pti = nil then Exit;
    ptd := GetTypeData(pti);
    nProps := ptd^.PropCount;
    if nProps > 0 then
    begin
      GetMem(pProps, SizeOf(PPropInfo) * nProps);
      GetPropInfos(pti, pProps);

      for Loop:=0 to nProps - 1 do
      begin
    {$ifdef fpc}
        ppi := pProps^[Loop];
        PropType := PPropInfo(ppi)^.PropType;
    {$else}
        ppi := pProps[Loop];
        PropType := PPropInfo(ppi)^.PropType^;
    {$endif}
        if PropType^.Kind = tkMethod then
        begin
          M := GetMethodProp(Instance, ppi);
          if Assigned(M.Code) and Assigned(M.Data) then
          begin
            aFullName := ProgTypeInfoList.FindMethodFullName(M.Code);

            if AFullName = '' then
              continue;

            ER := EventHandlerList.Add(Self,
                  M.Code,
                  M.Data,
                  GetCallConv(aFullName),
                  GetRetSize(aFullName));

            M.Code := @ TEventHandlerRec.Invoke;
            M.Data := ER;

            SetMethodProp(Instance, ppi, M);
          end;
        end;
      end;
      FreeMem(pProps, SizeOf(PPropInfo) * nProps);
    end;

    if Instance is TComponent then
    begin
      C := TComponent(Instance);
      for I := 0 to C.ComponentCount - 1 do
        _RebindEvents(C.Components[I]);
    end;
  end;
begin
  _RebindEvents(AnInstance);
end;

{$IFDEF PAX64}
procedure ZZZ; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  pop rsi;
  jmp rsi;
end;
{$ELSE}
procedure ZZZ;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
{$ENDIF}
  pop esi;
  jmp esi;
end;
{$ENDIF}

function TProgram.CallFunc(const FullName: String;
                     This: Pointer;
                     const ParamList: array of OleVariant;
                     OverCount: Integer = 0): OleVariant;
const
  MaxParam = 30;
var
  Invoke, OldEPoint: TInvoke;
  Address: Pointer;
  MR: TMapRec;
  OldESP0, I, NP, T: Integer;
  Value: OleVariant;
{$IFNDEF PAXARM}
  AnsiStrings: array [0..MaxParam] of AnsiString;
  WideStrings: array [0..MaxParam] of WideString;
  ShortStrings: array [0..MaxParam] of ShortString;
  AnsiS: AnsiString;
{$ENDIF}
  UnicStrings: array [0..MaxParam] of UnicString;
  valueDouble: Double;
  valueSingle: Single;
  valueExtended: Extended;
  valueCurrency: Currency;
  UnicS: UnicString;
begin
  Address := GetAddressEx(FullName, OverCount, MR);
  if Address = nil then
    RaiseError(errRoutineNotFound, [FullName]);

  NP := MR.SubDesc.ParamList.Count;
  if NP > System.Length(ParamList) then
    RaiseError(errNotEnoughActualParameters, [])
  else if NP < System.Length(ParamList) then
    RaiseError(errTooManyActualParameters, []);

  Invoke := TInvoke.Create;
  Invoke.CallConv := MR.SubDesc.CallConv;
  if MR.SubDesc.ResTypeId in
    (OrdinalTypes + [typeCLASS, typeCLASSREF, typePOINTER, typePROC, typeINTERFACE]) then
    Invoke.SetResType(typeINTEGER)
  else
    Invoke.SetResType(MR.SubDesc.ResTypeId);
  Invoke.SetResSize(MR.SubDesc.RetSize);

  Invoke.Address := Address;
  Invoke.SetThis(This);

  for I := 0 to NP - 1 do
  begin
    T := MR.SubDesc.ParamList[I].FinTypeId;
    value := ParamList[I];
    case T of
      typeVOID: Invoke.AddArg(value, typeINTEGER);
      typeBOOLEAN: Invoke.AddArg(value, typeINTEGER);
      typeBYTE: Invoke.AddArg(value, typeINTEGER);
{$IFNDEF PAXARM}
      typeANSICHAR: Invoke.AddArg(value, typeINTEGER);
      typeANSISTRING:
      begin
        AnsiStrings[I] := AnsiString(value);
        Invoke.AddArg(IntPax(AnsiStrings[I]), typeINTEGER);
      end;
      typeSHORTSTRING:
      begin
        ShortStrings[I] := ShortString(value);
        Invoke.AddArg(LongInt(@ShortStrings[I]), typeINTEGER);
      end;
      typeWIDESTRING:
      begin
        WideStrings[I] := value;
        Invoke.AddArg(IntPax(WideStrings[I]), typeINTEGER);
      end;
{$ENDIF}
      typeWORD: Invoke.AddArg(value, typeINTEGER);
      typeINTEGER: Invoke.AddArg(value, typeINTEGER);
      typeDOUBLE:
      begin
        valueDouble := value;
        Invoke.AddArgByVal(valueDouble, SizeOf(Double));
      end;
      typePOINTER: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeRECORD: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeARRAY: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeALIAS: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeENUM: Invoke.AddArg(LongInt(value), typeINTEGER);
      typePROC: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeSET: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeSINGLE:
      begin
        valueSingle := value;
        Invoke.AddArgByVal(valueSingle, SizeOf(Single));
      end;
      typeEXTENDED:
      begin
        valueExtended := value;
        Invoke.AddArgByVal(valueExtended, SizeOf(Extended));
      end;
      typeCLASS: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeCLASSREF: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeWIDECHAR: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeVARIANT:
      begin
        if MR.SubDesc.ParamList[I].ParamMod = PM_BYVAL then
          Invoke.AddArg(value, typeVARIANT)
        else
          Invoke.AddArg(LongInt(@ParamList[I]), typeINTEGER);
      end;
      typeDYNARRAY: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeINT64: Invoke.AddArgByVal(value, SizeOf(Int64));
      typeINTERFACE: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeCARDINAL: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeEVENT: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeCURRENCY:
      begin
        valueCurrency := value;
        Invoke.AddArgByVal(valueCurrency, SizeOf(Single));
      end;
      typeSMALLINT: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeSHORTINT: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeWORDBOOL: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeLONGBOOL: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeBYTEBOOL: Invoke.AddArg(LongInt(value), typeINTEGER);
      typeOLEVARIANT: Invoke.AddArg(value, typeVARIANT);
      typeUNICSTRING:
      begin
        UnicStrings[I] := value;
        Invoke.AddArg(IntPax(UnicStrings[I]), typeINTEGER);
      end;
    end;
  end;

  OldEPoint := EPoint;
  OldESP0 := fESP0;

  try
    Invoke.SetUp;
    EPoint := Invoke;

    Run;

  finally

    Address := EPoint.GetResultPtr;

    fESP0 := OldESP0;
    EPoint := OldEPoint;
    FreeAndNil(Invoke);
  end;

  case MR.SubDesc.ResTypeId of
    typeVOID: result := Unassigned;
    typeBOOLEAN: result := Boolean(Address^);
    typeBYTE: result := Byte(Address^);
{$IFNDEF PAXARM}
    typeANSICHAR: result := AnsiChar(Address^);
    typeANSISTRING:
    begin
      AnsiS := AnsiString(Address^);
      if Length(AnsiS) > 0 then
      begin
        Address := StrRefCountPtr(Pointer(AnsiS));
        Integer(Address^) := Integer(Address^) - 1;
      end;
      result := AnsiS;
    end;
    typeSHORTSTRING: result := ShortString(Address^);
    typeWIDESTRING: result := WideString(Address^);
{$ENDIF}
    typeWORD: result := Word(Address^);
    typeINTEGER: result := LongInt(Address^);
    typeDOUBLE: result := Double(Address^);
    typePOINTER: result := LongInt(Address^);
    typeRECORD: result := LongInt(Address);
    typeARRAY: result := LongInt(Address);
    typeALIAS: result := Unassigned;
    typeENUM: result := Byte(Address^);
    typePROC: result := LongInt(Address^);
    typeSET: result := LongInt(Address^);
    typeSINGLE: result := Single(Address^);
    typeEXTENDED: result := Extended(Address^);
    typeCLASS: result := LongInt(Address^);
    typeCLASSREF: result := LongInt(Address^);
    typeWIDECHAR: result := WideChar(Address^);
    typeVARIANT: result := Variant(Address^);
    typeDYNARRAY: result := LongInt(Address^);
    typeINT64: result := Integer(Address^);
    typeINTERFACE: result := LongInt(Address^);
{$IFDEF VARIANTS}
    typeCARDINAL: result := Cardinal(Address^);
{$ELSE}
    typeCARDINAL: result := LongInt(Address^);
{$ENDIF}
    typeEVENT: result := Unassigned;
    typeCURRENCY: result := Currency(Address^);
    typeSMALLINT: result := SmallInt(Address^);
    typeSHORTINT: result := ShortInt(Address^);
    typeWORDBOOL: result := WordBool(Address^);
    typeLONGBOOL: result := LongBool(Address^);
    typeBYTEBOOL: result := ByteBool(Address^);
    typeOLEVARIANT: result := OleVariant(Address^);
    typeUNICSTRING:
    begin
      UnicS := UnicString(Address^);
      if Length(UnicS) > 0 then
      begin
        Address := StrRefCountPtr(Pointer(UnicS));
        Integer(Address^) := Integer(Address^) - 1;
      end;
      result := UnicS;
    end;
  else
    result := Integer(Address^);
  end;

  if IsHalted then
    raise THaltException.Create(ExitCode);
end;

function TProgram.CallFuncEx(const FullName: String;
                             This: Pointer;
                             const ParamList: array of const;
                             IsConstructor: Boolean = false;
                             OverCount: integer = 0): Variant;
const
  MaxParam = 30;
var
  Invoke, OldEPoint: TInvoke;
  Address: Pointer;
  MR: TMapRec;
  OldESP0, I, NP, T: Integer;
{$IFNDEF PAXARM}
  AnsiStrings: array [0..MaxParam] of AnsiString;
  ShortStrings: array [0..MaxParam] of ShortString;
  WideStrings: array [0..MaxParam] of WideString;
{$ENDIF}
  UnicStrings: array [0..MaxParam] of UnicString;
  valueDouble: Double;
  valueSingle: Single;
  valueExtended: Extended;
  valueCurrency: Currency;
  valueInt64: Int64;
begin
  Address := GetAddressEx(FullName, OverCount, MR);
  if Address = nil then
    RaiseError(errRoutineNotFound, [FullName]);

  NP := MR.SubDesc.ParamList.Count;
  if NP > System.Length(ParamList) then
    RaiseError(errNotEnoughActualParameters, [])
  else if NP < System.Length(ParamList) then
    RaiseError(errTooManyActualParameters, []);

  Invoke := TInvoke.Create;
  Invoke.CallConv := MR.SubDesc.CallConv;
  if MR.SubDesc.ResTypeId in
    (OrdinalTypes + [typeCLASS, typeCLASSREF, typePOINTER, typePROC, typeINTERFACE]) then
    Invoke.SetResType(typeINTEGER)
  else
    Invoke.SetResType(MR.SubDesc.ResTypeId);
  Invoke.SetResSize(MR.SubDesc.RetSize);

  Invoke.Address := Address;
  Invoke.SetThis(This);

  if IsConstructor then
    Invoke.AddArg(1, typeINTEGER); // EDX

  for I := 0 to NP - 1 do
  begin
    T := MR.SubDesc.ParamList[I].FinTypeId;
    case T of
      typeVOID: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeBOOLEAN: Invoke.AddArg(ParamList[I].VBoolean, typeINTEGER);
      typeBYTE: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
{$IFNDEF PAXARM}
      typeANSICHAR: Invoke.AddArg(ParamList[I].VChar, typeINTEGER);
      typeANSISTRING:
      begin
        case ParamList[I].VType of
          vtString: AnsiStrings[I] :=
            PShortString(ParamList[I].VString)^;
          vtAnsiString: AnsiStrings[I] :=
            PAnsiString(ParamList[I].VAnsiString)^;
          vtWideString: AnsiStrings[I] :=
            AnsiString(PWideString(ParamList[I].VWideString)^);
         {$IFDEF UNIC}
          vtUnicodeString: AnsiStrings[I] :=
            AnsiString(PUnicodeString(ParamList[I].VUnicodeString)^);
         {$ENDIF}
          vtVariant: AnsiStrings[I] :=
            AnsiString(PVariant(ParamList[I].VVariant)^);
          vtChar: AnsiStrings[I] :=
            ParamList[I].VChar;
          vtWideChar: AnsiStrings[I] :=
            AnsiChar(ParamList[I].VWideChar);
        end;
        Invoke.AddArg(IntPax(AnsiStrings[I]), typeINTEGER);
      end;
      typeSHORTSTRING:
      begin
        case ParamList[I].VType of
          vtString: ShortStrings[I] :=
            PShortString(ParamList[I].VString)^;
          vtAnsiString: ShortStrings[I] :=
            PAnsiString(ParamList[I].VAnsiString)^;
          vtWideString: ShortStrings[I] :=
            ShortString(PWideString(ParamList[I].VWideString)^);
         {$IFDEF UNIC}
          vtUnicodeString: ShortStrings[I] :=
            AnsiString(PUnicodeString(ParamList[I].VUnicodeString)^);
         {$ENDIF}
          vtVariant: ShortStrings[I] :=
            ShortString(PVariant(ParamList[I].VVariant)^);
          vtChar: ShortStrings[I] :=
            ParamList[I].VChar;
          vtWideChar: ShortStrings[I] :=
            AnsiChar(ParamList[I].VWideChar);
        end;
        Invoke.AddArg(LongInt(@ShortStrings[I]), typeINTEGER);
      end;
      typeWIDESTRING:
      begin
        case ParamList[I].VType of
          vtString: WideStrings[I] :=
            WideString(PShortString(ParamList[I].VString)^);
          vtAnsiString: WideStrings[I] :=
            WideString(PAnsiString(ParamList[I].VAnsiString)^);
          vtWideString: WideStrings[I] :=
            PWideString(ParamList[I].VWideString)^;
         {$IFDEF UNIC}
          vtUnicodeString: WideStrings[I] :=
            PUnicodeString(ParamList[I].VUnicodeString)^;
         {$ENDIF}
          vtVariant: WideStrings[I] :=
            PVariant(ParamList[I].VVariant)^;
          vtChar: WideStrings[I] :=
            WideChar(ParamList[I].VChar);
          vtWideChar: WideStrings[I] :=
            ParamList[I].VWideChar;
          vtPWideChar: WideStrings[I] :=
            ParamList[I].VPWideChar;
        end;
        Invoke.AddArg(IntPax(WideStrings[I]), typeINTEGER);
      end;
{$ENDIF}
      typeWORD: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeINTEGER: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeDOUBLE:
      begin
        valueDouble := ParamList[I].VExtended^;
        Invoke.AddArgByVal(valueDouble, SizeOf(Double));
      end;
      typePOINTER: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeRECORD: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeARRAY: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeALIAS: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeENUM: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typePROC: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeSET: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeSINGLE:
      begin
        valueSingle := ParamList[I].VExtended^;
        Invoke.AddArgByVal(valueSingle, SizeOf(Single));
      end;
      typeEXTENDED:
      begin
        valueExtended := ParamList[I].VExtended^;
        Invoke.AddArgByVal(valueExtended, SizeOf(Extended));
      end;
      typeCLASS: Invoke.AddArg(LongInt(ParamList[I].VObject), typeINTEGER);
      typeCLASSREF: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeWIDECHAR: Invoke.AddArg(LongInt(ParamList[I].VWideChar), typeINTEGER);
      typeVARIANT:
      begin
        if MR.SubDesc.ParamList[I].ParamMod = PM_BYVAL then
          Invoke.AddArg(ParamList[I].VVariant^, typeVARIANT)
        else
          Invoke.AddArg(LongInt(ParamList[I].VVariant), typeINTEGER);
      end;
      typeDYNARRAY: Invoke.AddArg(LongInt(ParamList[I].VPointer), typeINTEGER);
      typeINT64:
         case ParamList[i].VType of
           vtInteger: begin
                        valueInt64 := Int64(ParamList[I].VInteger);
                        Invoke.AddArgByVal(valueInt64, SizeOf(Int64));
                      end;
         else
           Invoke.AddArgByVal(ParamList[I].VInt64^, SizeOf(Int64));
         end;
      typeINTERFACE: Invoke.AddArg(LongInt(ParamList[I].VInterface), typeINTEGER);
      typeCARDINAL: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeEVENT: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeCURRENCY:
      begin
        valueCurrency := ParamList[I].VExtended^;
        Invoke.AddArgByVal(valueCurrency, SizeOf(Single));
      end;
      typeSMALLINT: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeSHORTINT: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeWORDBOOL: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeLONGBOOL: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeBYTEBOOL: Invoke.AddArg(ParamList[I].VInteger, typeINTEGER);
      typeOLEVARIANT:
      begin
        if MR.SubDesc.ParamList[I].ParamMod = PM_BYVAL then
          Invoke.AddArg(ParamList[I].VVariant^, typeVARIANT)
        else
          Invoke.AddArg(LongInt(ParamList[I].VVariant), typeINTEGER);
      end;
      typeUNICSTRING:
      begin
        case ParamList[I].VType of
{$IFNDEF PAXARM}
          vtString: UnicStrings[I] :=
            UnicString(PShortString(ParamList[I].VString)^);
          vtAnsiString: UnicStrings[I] :=
            UnicString(PAnsiString(ParamList[I].VAnsiString)^);
          vtWideString: UnicStrings[I] :=
            PWideString(ParamList[I].VWideString)^;
          vtChar: UnicStrings[I] :=
            WideChar(ParamList[I].VChar);
{$ENDIF}
         {$IFDEF UNIC}
          vtUnicodeString: UnicStrings[I] :=
            PUnicodeString(ParamList[I].VUnicodeString)^;
         {$ENDIF}
          vtVariant: UnicStrings[I] :=
            PVariant(ParamList[I].VVariant)^;
          vtWideChar: UnicStrings[I] :=
            ParamList[I].VWideChar;
          vtPWideChar: UnicStrings[I] :=
            ParamList[I].VPWideChar;
        end;
        Invoke.AddArg(IntPax(UnicStrings[I]), typeINTEGER);
      end;
    end;
  end;

  OldEPoint := EPoint;
  OldESP0 := fESP0;

  try
    Invoke.SetUp;
    EPoint := Invoke;
    Run;

  finally

    Address := EPoint.GetResultPtr;

    fESP0 := OldESP0;
    EPoint := OldEPoint;
    FreeAndNil(Invoke);
  end;

  case MR.SubDesc.ResTypeId of
    typeVOID: result := Unassigned;
    typeBOOLEAN: result := Boolean(Address^);
    typeBYTE: result := Byte(Address^);
{$IFNDEF PAXARM}
    typeANSICHAR: result := AnsiChar(Address^);
    typeANSISTRING: result := AnsiString(Address^);
    typeSHORTSTRING: result := ShortString(Address^);
    typeWIDESTRING: result := WideString(Address^);
{$ENDIF}
    typeWORD: result := Word(Address^);
    typeINTEGER: result := LongInt(Address^);
    typeDOUBLE: result := Double(Address^);
    typePOINTER: result := LongInt(Address^);
    typeRECORD: result := LongInt(Address);
    typeARRAY: result := LongInt(Address);
    typeALIAS: result := Unassigned;
    typeENUM: result := Byte(Address^);
    typePROC: result := LongInt(Address^);
    typeSET: result := LongInt(Address^);
    typeSINGLE: result := Single(Address^);
    typeEXTENDED: result := Extended(Address^);
    typeCLASS: result := LongInt(Address^);
    typeCLASSREF: result := LongInt(Address^);
    typeWIDECHAR: result := WideChar(Address^);
    typeVARIANT: result := Variant(Address^);
    typeDYNARRAY: result := LongInt(Address^);
    typeINT64: result := LongInt(Address^);
    typeINTERFACE: result := LongInt(Address^);
{$IFDEF VARIANTS}
    typeCARDINAL: result := Cardinal(Address^);
{$ELSE}
    typeCARDINAL: result := LongInt(Address^);
{$ENDIF}
    typeEVENT: result := Unassigned;
    typeCURRENCY: result := Currency(Address^);
    typeSMALLINT: result := SmallInt(Address^);
    typeSHORTINT: result := ShortInt(Address^);
    typeWORDBOOL: result := WordBool(Address^);
    typeLONGBOOL: result := LongBool(Address^);
    typeBYTEBOOL: result := ByteBool(Address^);
    typeOLEVARIANT: result := OleVariant(Address^);
    typeUNICSTRING: result := UnicString(Address^);
  else
    result := LongInt(Address^);
  end;

  if IsHalted then
    raise THaltException.Create(ExitCode);
end;

function TProgram.CreateScriptObject(const ScriptClassName: String;
                                     const ParamList: array of const): TObject;
var
  ClassIndex: Integer;
  PClass: TClass;
  MR: TMapRec;
  NP: Integer;
  V: Variant;
begin
  result := nil;

  ClassIndex := ClassList.IndexOf(ScriptClassName);

  if ClassIndex = -1 then
    RaiseError(errClassNotFound, [ScriptClassName]);

  PClass := ClassList[ClassIndex].PClass;

  NP := System.Length(ParamList);
  MR := ScriptMapTable.LookupConstructor(ScriptClassName, NP);

  if MR = nil then
    Exit;

  V := CallFuncEx(MR.FullName, PClass, ParamList, true, MR.SubDesc.OverCount);
  result := TObject(TVarData(V).VInteger);
end;

function TProgram.GetTryStack: TTryStack;
begin
  result := RootProg.fTryStack;
end;

function TProgram.GetCallStack: TCallStack;
begin
  result := RootProg.fCallStack;
end;

function TProgram.GetESP0: Integer;
begin
  result := RootProg.fESP0;
end;

procedure TProgram.SetESP0(value: Integer);
begin
  RootProg.fESP0 := value;
end;

function TProgram.GetCurrException: Exception;
begin
  result := fCurrException;
end;

procedure TProgram.SetCurrException(value: Exception);
begin
  fCurrException := value;
end;

function TProgram.GetVirtualAllocProg: Boolean;
begin
  result := RootProg.fVirtualAllocProg;
end;

procedure TProgram.SetVirtualAllocProg(value: Boolean);
begin
  RootProg.fVirtualAllocProg := value;
end;

function TProgram._VirtualAlloc(Address: Pointer;
                                Size, flAllocType, flProtect: Cardinal): Pointer;
begin
{$IFDEF MSWINDOWS}
  if VirtualAllocProg then
    result := VirtualAlloc(Address, Size, flAllocType, flProtect)
  else
    result := AllocMem(Size);
{$ELSE}
  result := AllocMem(Size);
{$ENDIF}
end;

procedure TProgram._VirtualFree(Address: Pointer; Size: Cardinal);
begin
{$IFDEF MSWINDOWS}
  if VirtualAllocProg then
    VirtualFree(Address, 0, MEM_RELEASE)
  else
    FreeMem(Address, Size);
{$ELSE}
  FreeMem(Address, Size);
{$ENDIF}
end;

function TProgram.GetRootProg: TProgram;
begin
  result := Self;
  while result.PCUOwner <> nil do
    result := result.PCUOwner as TProgram;
end;

function TProgram.GetCallStackCount: Integer;
begin
  result := RootCallStack.Count;
end;

function TProgram.GetCallStackItem(I: Integer): Integer;
begin
  if (I >= 0) and (I < GetCallStackCount) then
    result := RootCallStack[I].SubId
  else
    result := 0;
end;

function TProgram.GetCallStackLineNumber(I: Integer): Integer;
var
  N: Integer;
begin
  if (I >= 0) and (I < GetCallStackCount) then
  begin
    N := RootCallStack[I].NCall;
    if N = -1 then
    begin
      N := GetByteCodeLine;
      RootCallStack[I].NCall := N;
    end;

    result := RunTimeModuleList.GetSourceLine(N);
  end
  else
    result := 0;
end;

function TProgram.GetCallStackModuleName(I: Integer): String;
var
  N: Integer;
begin
  result := '';
  if (I >= 0) and (I < GetCallStackCount) then
  begin
    N := RootCallStack[I].NCall;
    if N = - 1 then
      Exit;
    result := RunTimeModuleList.GetModuleName(N);
  end;
end;

function TProgram.GetCallStackModuleIndex(I: Integer): Integer;
var
  N: Integer;
begin
  result := -1;
  if (I >= 0) and (I < GetCallStackCount) then
  begin
    N := RootCallStack[I].NCall;
    if N = - 1 then
      Exit;
    result := RunTimeModuleList.GetModuleIndex(N);
  end;
end;

procedure TProgram.DiscardPause;
begin
  PauseRec.ProgOffset := 0;
end;

procedure TProgram.SetEntryPoint(EntryPoint: TPaxInvoke);
begin
  if EntryPoint = nil then
    EPoint := nil
  else
  begin
    EPoint := TInvoke(EntryPoint.GetImplementation);
    TInvoke(EntryPoint.GetImplementation).OldESP0 := RootESP0;
  end;
end;

procedure TProgram.ResetEntryPoint(EntryPoint: TPaxInvoke);
begin
  if EntryPoint = nil then
    Exit
  else
    RootESP0 := TInvoke(EntryPoint.GetImplementation).OldESP0;
end;

procedure TProgram.AssignEventHandlerRunner(MethodAddress: Pointer;
                                            Instance: TObject);
begin
  OwnerEventHandlerMethod.Code := MethodAddress;
  OwnerEventHandlerMethod.Data := Instance;
end;

function TProgram.GetDestructorAddress: Pointer;
begin
  result := Address_DestroyObject;
end;

function TProgram.GetParamAddress(Offset: Integer): Pointer;
var
  EBP_Value: IntPax;
begin
  EBP_Value := RootCallStack.Top.EBP;
  result := PauseRec.GetPtr(EBP_Value, Offset);
end;

function TProgram.GetLocalAddress(Offset: Integer): Pointer;
var
  EBP_Value: IntPax;
begin
  EBP_Value := RootCallStack.Top.EBP;
  result := PauseRec.GetPtr(EBP_Value, Offset);
end;

function TProgram.GetParamAddress(StackFrameNumber, Offset: Integer): Pointer;
var
  EBP_Value: IntPax;
begin
  result := nil;

  if StackFrameNumber >= 0 then
    EBP_Value := RootCallStack[StackFrameNumber].EBP
  else
    Exit;

  result := PauseRec.GetPtr(EBP_Value, Offset);
end;

function TProgram.GetLocalAddress(StackFrameNumber, Offset: Integer): Pointer;
var
  EBP_Value: IntPax;
begin
  result := nil;

  if StackFrameNumber >= 0 then
    EBP_Value := RootCallStack[StackFrameNumber].EBP
  else
    Exit;

  result := PauseRec.GetPtr(EBP_Value, Offset);
end;

end.

