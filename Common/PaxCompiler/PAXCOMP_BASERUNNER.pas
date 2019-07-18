////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_BASERUNNER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_BASERUNNER;
interface
uses {$I uses.def}

{$ifdef DRTTI}
  RTTI,
  PAXCOMP_2010,
  PAXCOMP_2010REG,
{$endif}

  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_CLASSLST,
  PAXCOMP_CLASSFACT,
  PAXCOMP_TYPEINFO,
  PAXCOMP_OFFSET,
  PAXCOMP_RTI,
  PAXCOMP_PROGLIST,
  PAXCOMP_MAP,
  PAXCOMP_STDLIB,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_LOCALSYMBOL_TABLE,
  PAXCOMP_INVOKE,
  PAXCOMP_GC,
  PAXCOMP_BRIDGE,
  PaxInvoke;
type
  TBaseRunner = class
  private
    fClassList: TClassList;
    fSearchPathList: TStringList;
    fRunMode: Integer;
    fIsEvent: Boolean;
    fInitCallStackCount: Integer;
    fIsRunning: Boolean;
    fProcessingExceptBlock: Boolean;
    fExceptionIsAvailableForHostApplication: Boolean;
    fHasError: Boolean;
    fInitializationProcessed: Boolean;

    function GetRootOwner: TObject;
    function GetDataPtr: PBytes;
    function GetRootSearchPathList: TStringList;
    function GetRunMode: Integer;
    procedure SetRunMode(value: Integer);
    function GetIsEvent: Boolean;
    procedure SetIsEvent(value: Boolean);
    function GetInitCallStackCount: Integer;
    procedure SetInitCallStackCount(value: Integer);
    function GetIsRunning: Boolean;
    procedure SetIsRunning(value: Boolean);
    function GetProcessingExceptBlock: Boolean;
    procedure SetProcessingExceptBlock(value: Boolean);
    function GetExceptionIsAvailableForHostApplication: Boolean;
    procedure SetExceptionIsAvailableForHostApplication(value: Boolean);
    function GetHasError: Boolean;
    procedure SetHasError(value: Boolean);
  protected
    gInstance: TObject;
    function GetCodePtr: PBytes; virtual;
    function GetProgramSize: Integer; virtual; abstract;
    function _VirtualAlloc(Address: Pointer;
                                Size, flAllocType, flProtect: Cardinal): Pointer; virtual;
    procedure _VirtualFree(Address: Pointer; Size: Cardinal); virtual;
    procedure Protect; virtual;
    procedure UnProtect; virtual;
    procedure RunInternal; virtual;
    procedure RunExceptInitialization; virtual;
    function FireAddressEvent(MR: TMapRec): Pointer; virtual;
  public
    Owner: TObject;
    PCUOwner: TBaseRunner;

    Data: Pointer;
    Prog: Pointer;
    fDataSize: Integer;
    fCodeSize: Integer;
    fCurrException: Exception;
    fPrevException: Exception;
    fImageDataPtr: Integer;

    CurrExpr: String;
    IsHalted: Boolean;

    JS_Record: TJS_Record;
    PCULang: Byte;
    ExitCode: Integer;
    SuspendFinalization: Boolean;
    ExceptionRec: Pointer;
    PausedPCU: TBaseRunner;
    UseMapping: Boolean;
    Console: Boolean;
    ModeSEH: Boolean;
    PAX64: Boolean;
    InitializationIsProcessed: Boolean;
{$IFNDEF PAXARM_DEVICE}
    EPoint: TInvoke;
{$ENDIF}
    JS_Object: TObject;
    JS_Boolean: TObject;
    JS_String: TObject;
    JS_Number: TObject;
    JS_Date: TObject;
    JS_Function: TObject;
    JS_Array: TObject;
    JS_RegExp: TObject;
    JS_Math: TObject;
    JS_Error: TObject;
{$IFDEF ARC}
    ContextList: TList<TObject>;
{$ELSE}
    ContextList: TList;
{$ENDIF}
    ProgTag: Integer;
    fGC: TGC;

    PassedClassRef: TClass;
    SavedClass: TClass;

    OnMapTableNamespace: TMapTableNamespaceEvent;
    OnMapTableVarAddress: TMapTableVarAddressEvent;
    OnMapTableProcAddress: TMapTableProcAddressEvent;
    OnMapTableClassRef: TMapTableClassRefEvent;
    OnLoadPCU: TLoadPCUEvent;
    OnException: TErrNotifyEvent;
    OnUnhandledException: TErrNotifyEvent;
    OnPause: TPauseNotifyEvent;
    OnPauseUpdated: TPauseNotifyEvent;
    OnHalt: THaltNotifyEvent;
    OnLoadProc: TLoadProcEvent;
    OnBeforeCallHost: TIdNotifyEvent;
    OnAfterCallHost: TIdNotifyEvent;
    OnCreateObject: TObjectNotifyEvent;
    OnAfterObjectCreation: TObjectNotifyEvent;
    OnDestroyObject: TObjectNotifyEvent;
    OnAfterObjectDestruction: TClassNotifyEvent;
    OnCreateHostObject: TObjectNotifyEvent;
    OnDestroyHostObject: TObjectNotifyEvent;
    OnPrint: TPrintEvent;
    OnPrintEx: TPrintExEvent;
    OnPrintClassTypeField: TPrintClassTypeFieldEvent;
    OnPrintClassTypeProp: TPrintClassTypePropEvent;
    OnCustomExceptionHelper: TCustomExceptionHelperEvent;
    OnSaveToStream: TStreamEvent;
    OnLoadFromStream: TStreamEvent;
    OnBeginProcNotifyEvent: TProcNotifyEvent;
    OnEndProcNotifyEvent: TProcNotifyEvent;
    OnVirtualObjectMethodCall: TVirtualObjectMethodCallEvent;
    OnVirtualObjectPutProperty: TVirtualObjectPutPropertyEvent;

    HostMapTable: TMapTable;
    ScriptMapTable: TMapTable;
    ProgClassFactory: TPaxClassFactory;
    MessageList: TMessageList;
    ExportList: TExportList;
    ProgTypeInfoList: TPaxTypeInfoList;
    OffsetList: TOffsetList;
    RunTimeModuleList: TRuntimeModuleList;
    DllList: TStringList;
    ProgList: TProgList;
    LocalSymbolTable: TProgSymbolTable;
    GlobalSym: TBaseSymbolTable;

    FinallyCount: Integer;
    ByteCodeGlobalEntryList: TIntegerDynArray;
    ByteCodeInterfaceSetupList: TIntegerList;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearCurrException;
    function GetRootGC: TGC;
    function GetDestructorAddress: Pointer; virtual; abstract;
    function NeedAllocAll: Boolean; virtual;
    function GetRootProg: TBaseRunner;
    procedure CreateClassFactory;
    procedure CreateGlobalJSObjects;
    procedure Deallocate;
    procedure Allocate(InitCodeSize, InitDataSize: Integer);
    procedure AllocateSimple(InitCodeSize, InitDataSize: Integer);
    procedure RegisterDefinitions(SymbolTable: TBaseSymbolTable);
    procedure ForceMappingEvents;
    procedure ForceMapping(SymbolTable: TBaseSymbolTable;
                           Reassign: Boolean);
    procedure MapGlobal;
    procedure MapLocal;
    function HasAvailUnit(const FullName: String): Boolean; virtual;
    function LookUpAvailClass(const FullName: String): TClass; virtual;
    function LookUpAvailAddress(const FullName: String;
                                OverCount: Integer): Pointer; virtual;
{$IFDEF DRTTI}
    function LookUpAvailMethod(const FullName: String;
                               OverCount: Integer): TRTTIMethod;
{$ENDIF}
    procedure RegisterMember(LevelId: Integer; const Name: String;
                             Address: Pointer);
    function RegisterNamespace(LevelId: Integer; const Name: String): Integer;
    function RegisterClassType(LevelId: Integer; C: TClass): Integer;
{$IFNDEF PAXARM_DEVICE}
    procedure SetEntryPoint(EntryPoint: TPaxInvoke); virtual; abstract;
    procedure ResetEntryPoint(EntryPoint: TPaxInvoke); virtual; abstract;
{$ENDIF}
    procedure SaveToStream(S: TStream); virtual; abstract;
    procedure LoadFromStream(S: TStream); virtual; abstract;
    procedure SaveToBuff(var Buff);
    procedure LoadFromBuff(var Buff);
    procedure SaveToFile(const Path: String);
    procedure LoadFromFile(const Path: String);
    procedure RebindEvents(AnInstance: TObject); virtual;
    procedure LoadDFMStream(Instance: TObject; S: TStream;
       const OnFindMethod: TFindMethodEvent = nil; const OnError: TReaderError = nil);
    procedure LoadDFMFile(Instance: TObject; const FileName: String;
       const OnFindMethod: TFindMethodEvent = nil; const OnError: TReaderError = nil);
    function CallFunc(const FullName: String;
              This: Pointer;
              const ParamList: array of OleVariant;
              OverCount: Integer = 0): OleVariant; virtual; abstract;
    function CallByteCode(InitN: Integer;
                          This: Pointer;
                          R_AX, R_CX, R_DX, R_8, R_9: IntPax;
                          StackPtr: Pointer;
                          ResultPtr: Pointer;
                          var FT: Integer): Integer; virtual;
    procedure Run; virtual; abstract;
    procedure RunExtended;
    procedure Pause; virtual; abstract;
    procedure DiscardPause; virtual; abstract;
    procedure RemovePause; virtual; abstract;
    procedure ResetRun; virtual; abstract;
    function IsPaused: Boolean; virtual; abstract;
    function GetCallStackCount: Integer; virtual; abstract;
    function GetCallStackItem(I: Integer): Integer; virtual; abstract;
    function GetCallStackLineNumber(I: Integer): Integer; virtual; abstract;
    function GetCallStackModuleName(I: Integer): String; virtual; abstract;
    function GetCallStackModuleIndex(I: Integer): Integer; virtual; abstract;
    function GetCurrentSub: TMapRec;
    function GetCurrentFunctionFullName: String;
    procedure GetCurrentLocalVars(result: TStrings);
    procedure GetCurrentParams(result: TStrings);

    function Valid: Boolean; virtual;
    function GetIsRootProg: Boolean;
    procedure RunInitialization; virtual;
    procedure RunFinalization; virtual;
    procedure DiscardDebugMode; virtual; abstract;
    procedure RaiseError(const Message: string; params: array of Const);
    procedure SetGlobalSym(AGlobalSym: Pointer); virtual;
    function RegisterClass(C: TClass;
                           const FullName: String;
                           Offset: Integer = -1): TClassRec;
    function RegisterClassEx(C: TClass;
                             const FullName: String;
                             Offset: Integer; ClassIndex: Integer): TClassRec;
    procedure AssignEventHandlerRunner(MethodAddress: Pointer;
                                       Instance: TObject); virtual; abstract;
    procedure Reset; virtual;
    function CreateScriptObject(const ScriptClassName: String;
                                const ParamList: array of const): TObject; virtual; abstract;
    procedure DestroyScriptObject(var X: TObject);
    function GetImageSize: Integer;
    function GetImageDataPtr: Integer;
    procedure CopyRootEvents;
    function GetResultPtr: Pointer;
    function GetByteCodeLine: Integer; virtual;
    procedure SetByteCodeLine(N: Integer);
    function GetSourceLine: Integer;
    function GetModuleName: String;
    function GetModuleIndex: Integer;
    function GetParamAddress(Offset: Integer): Pointer; overload; virtual; abstract;
    function GetLocalAddress(Offset: Integer): Pointer; overload; virtual; abstract;
    function GetParamAddress(StackFrameNumber, Offset: Integer): Pointer; overload; virtual; abstract;
    function GetLocalAddress(StackFrameNumber, Offset: Integer): Pointer; overload; virtual; abstract;
    procedure WrapMethodAddress(var Address: Pointer);
    function WrapGlobalAddress(var Address: Pointer): Integer;
    function GetAddress(Handle: Integer): Pointer; overload;
    function GetAddress(const FullName: String; var MR: TMapRec): Pointer; overload;
    function GetFieldAddress(X: TObject; const FieldName: String): Pointer;
    function LoadAddressEx(const FileName, ProcName: String;
                           RunInit: Boolean;
                           OverCount: Integer;
                           var MR: TMapRec;
                           var DestProg: Pointer): Pointer; virtual;
    function GetAddressEx(const FullName: String; OverCount: Integer;
                          var MR: TMapRec): Pointer;
    function GetAddressExtended(const FullName: String; var MR: TMapRec): Pointer; overload;
    function GetAddressExtended(const FullName: String; OverCount: Integer; var MR: TMapRec): Pointer; overload;
    procedure SetAddress(Offset: Integer; P: Pointer);
    function SetHostAddress(const FullName: String; Address: Pointer): Boolean;
    procedure CopyRootBreakpoints(const UnitName: String);
    procedure InitMessageList;
    procedure CreateMapOffsets;
    function GetOffset(Shift: Integer): Integer;
    procedure SetupInterfaces(P: Pointer);
    function GetTypeInfo(const FullTypeName: String): PTypeInfo;
    function GetCallConv(const FullName: String): Integer;
    function GetRetSize(const FullName: String): Integer;
    function FileExists(const FileName: String; out FullPath: String): Boolean;
    procedure UnloadDlls;
    procedure UnloadPCU(const FullPath: String);
    procedure LoadPCU(const FileName: String;
                     var DestProg: Pointer);
    function AddBreakpoint(const ModuleName: String;
                           SourceLineNumber: Integer): TBreakpoint;
    function AddTempBreakpoint(const ModuleName: String;
                               SourceLineNumber: Integer): TBreakpoint;
    function RemoveBreakpoint(const ModuleName: String;
                              SourceLineNumber: Integer): Boolean; overload;
    function RemoveBreakpoint(const ModuleName: String): Boolean; overload;
    function HasBreakpoint(const ModuleName: String;
                                SourceLineNumber: Integer): Boolean;
    procedure RemoveAllBreakpoints;
    function IsExecutableLine(const ModuleName: String;
                              SourceLineNumber: Integer): Boolean;
    procedure SaveState(S: TStream); virtual;
    procedure LoadState(S: TStream); virtual;
    function GetInterfaceToObjectOffset(JumpN: Integer): Integer; virtual;
    function GetReturnFinalTypeId(InitSubN: Integer): Integer; virtual;

    property ClassList: TClassList read fClassList;
    property ResultPtr: Pointer read GetResultPtr;
    property DataPtr: PBytes read GetDataPtr;
    property DataSize: Integer read fDataSize write fDataSize;
    property CodePtr: PBytes read GetCodePtr;
    property IsRootProg: Boolean read GetIsRootProg;
    property RootSearchPathList: TStringList read GetRootSearchPathList;
    property RunMode: Integer read GetRunMode write SetRunMode;
    property IsRunning: Boolean read GetIsRunning write SetIsRunning;
    property CodeSize: Integer read fCodeSize write fCodeSize;
    property ProgramSize: Integer read GetProgramSize;
    property RootIsEvent: Boolean read GetIsEvent write SetIsEvent;
    property CurrException: Exception read fCurrException write fCurrException;
    property RootInitCallStackCount: Integer read GetInitCallStackCount write SetInitCallStackCount;
    property CurrN: Integer read GetByteCodeLine;
    property CurrS: Integer read GetSourceLine;
    property RootGC: TGC read GetRootGC;
    property RootOwner: TObject read GetRootOwner;
    property RootExceptionIsAvailableForHostApplication: Boolean read
      GetExceptionIsAvailableForHostApplication write
      SetExceptionIsAvailableForHostApplication;
    property ProcessingExceptBlock: Boolean read GetProcessingExceptBlock
      write SetProcessingExceptBlock;
    property HasError: Boolean read GetHasError write SetHasError;
    property InitializationProcessed: Boolean read fInitializationProcessed;
  end;

  TBaseRunnerClass = class of TBaseRunner;

type
  TCreate_JSObjects = procedure(Prog: Pointer; R: TJS_Record);
  TEmitProc = procedure (kernel, prog: Pointer; context: Pointer = nil);
  TRegisterProc = procedure (st: TBaseSymbolTable);
  TAssignRunnerLib = procedure;
var
  CrtJSObjects: TCreate_JSObjects = nil;
  CurrProg: TBaseRunner = nil;
  AssignRunnerLibProc: TAssignRunnerLib = nil;
  DefaultRunnerClass: TBaseRunnerClass;
  RegisterSEH: TRegisterProc = nil;
  dmp_procedure: procedure (sprog: Pointer = nil) = nil;
  dump_all_procedure: procedure(path: String; kernel, prog, sprog: Pointer);

  Address_Exit: Pointer = nil;
  Address_CondRaise: Pointer = nil;
  Address_LoadSeg: Pointer = nil;
  Address_CreateObject: Pointer = nil;
  Address_DestroyObject: Pointer = nil;
  Address_TryOn: Pointer = nil;
  Address_TryOff: Pointer = nil;
  Address_Raise: Pointer = nil;
  Address_Pause: Pointer = nil;
  Address_InitSub: Pointer = nil;
  Address_EndSub: Pointer = nil;
  Address_SetEventProp: Pointer = nil;
  Address_SetEventProp2: Pointer = nil;

procedure dmp(sprog: Pointer = nil);
procedure Dump_All(path: String; kernel, prog, sprog: Pointer);

var CurrRunner: TBaseRunner;

implementation

procedure dmp(sprog: Pointer = nil);
begin
  if Assigned(dmp_procedure) then
    dmp_procedure(sprog);
end;

procedure Dump_All(path: String; kernel, prog, sprog: Pointer);
begin
  if Assigned(dump_all_procedure) then
     dump_all_procedure(path, kernel, prog, sprog);
end;

constructor TBaseRunner.Create;
begin
  inherited;

  FindAvailTypes;

  GlobalSym := GlobalSymbolTable;

  fClassList := TClassList.Create;
  fSearchPathList := TStringList.Create;
  HostMapTable := TMapTable.Create;
  ScriptMapTable := TMapTable.Create;
  ProgClassFactory := TPaxClassFactory.Create;
  MessageList := TMessageList.Create;
  ExportList := TExportList.Create;
  ProgTypeInfoList := TPaxTypeInfoList.Create;
  OffsetList := TOffsetList.Create;
  RuntimeModuleList := TRuntimeModuleList.Create(Self);
  DllList := TStringList.Create;
  ProgList := TProgList.Create(Self);
  LocalSymbolTable := TProgSymbolTable.Create(GlobalSym);
{$IFDEF ARC}
  ContextList := TList<TObject>.Create;
{$ELSE}
  ContextList := TList.Create;
{$ENDIF}
  fGC := TGC.Create;

  Data := nil;
  fDataSize := 0;
  fExceptionIsAvailableForHostApplication := true;

  ByteCodeInterfaceSetupList := TIntegerList.Create;
end;

destructor TBaseRunner.Destroy;
begin
  FreeAndNil(fClassList);
  FreeAndNil(fSearchPathList);
  FreeAndNil(HostMapTable);
  FreeAndNil(ScriptMapTable);
  if ProgClassFactory <> nil then
    FreeAndNil(ProgClassFactory);
  FreeAndNil(MessageList);
  FreeAndNil(ExportList);
  FreeAndNil(ProgTypeInfoList);
  FreeAndNil(OffsetList);
  FreeAndNil(RuntimeModuleList);
  FreeAndNil(DllList);
  if ProgList <> nil then
    FreeAndNil(ProgList);
  FreeAndNil(LocalSymbolTable);
  FreeAndNil(ContextList);
  FreeAndNil(fGC);
  FreeAndNil(ByteCodeInterfaceSetupList);
  inherited;
end;

procedure TBaseRunner.Reset;
begin
  fClassList.Clear;
  HostMapTable.Clear;
  ScriptMapTable.Clear;
  ProgClassFactory.Clear;
  MessageList.Clear;
  ExportList.Clear;
  ProgTypeInfoList.Clear;
  OffsetList.Clear;
  LocalSymbolTable.Reset;
  ContextList.Clear;
  if ProgList <> nil then
    ProgList.Clear;
  fGC.Clear;
  ByteCodeInterfaceSetupList.Clear;

  fInitializationProcessed := false;
end;

function TBaseRunner.RegisterClass(C: TClass;
                                   const FullName: String;
                                   Offset: Integer = -1): TClassRec;
var
  P: Pointer;
  I: Integer;
  S: String;
begin
  if (Offset = -1) or (Offset = 0) then
  begin
    S := C.ClassName;
    for I:=0 to fClassList.Count - 1 do
      if StrEql(fClassList.Names[I], S) then
      begin
        Offset := fClassList[I].Offset;
        break;
      end;
    if Offset = -1 then
      RaiseError(errCannotRegisterClass, [S]);
  end;

  P := ShiftPointer(DataPtr, Offset);
  Pointer(P^) := C;

  result := fClassList.AddClass(C, FullName, true, Offset);

  for I:=0 to result.PropInfos.Count - 1 do
  begin
    P := ShiftPointer(P, SizeOf(Pointer));
    Pointer(P^) := result.PropInfos[I];
  end;
end;

function TBaseRunner.RegisterClassEx(C: TClass;
                                     const FullName: String;
                                     Offset: Integer; ClassIndex: Integer): TClassRec;
var
  P: Pointer;
  I: Integer;
begin
  P := ShiftPointer(DataPtr, Offset);
  Pointer(P^) := C;

  result := fClassList.AddClassEx(C, FullName, true, Offset, ClassIndex);

  for I:=0 to result.PropInfos.Count - 1 do
  begin
    P := ShiftPointer(P, SizeOf(Pointer));
    Pointer(P^) := result.PropInfos[I];
  end;
end;

procedure TBaseRunner.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

function TBaseRunner.GetDataPtr: PBytes;
begin
  result := Data;
end;

procedure TBaseRunner.InitMessageList;
var
  I: Integer;
  R: TMessageRec;
  MR: TMapRec;
begin
  for I := 0 to MessageList.Count - 1 do
  begin
    R := MessageList[I];
    R.Address := GetAddress(R.FullName, MR);
    R.Class_Name := ExtractClassName(R.FullName);
    R.Class_Name := ExtractName(R.Class_Name);
  end;

  for I := 0 to ProgClassFactory.Count - 1 do
  begin
    VmtDynamicTableSlot(ProgClassFactory[I].VMTPtr)^ :=
       MessageList.CreateDmtTable(ExtractName(ProgClassFactory[I].FullClassName),
            ProgClassFactory[I].DmtTableSize);
  end;
  for I := 0 to ProgList.Count - 1 do
    TBaseRunner(ProgList[I].Prog).InitMessageList;
end;

procedure TBaseRunner.CreateMapOffsets;
begin
  HostMapTable.CreateOffsets(OffsetList, true);
  ScriptMapTable.CreateOffsets(OffsetList, false);
end;

function TBaseRunner.GetOffset(Shift: Integer): Integer;
begin
  if OffsetList.Count > 0 then
  begin

    if Shift <= 0 then
    begin
      result := Shift;
      Exit;
    end;

    result := OffsetList.GetOffset(Shift);
  end
  else
    result := Shift;
end;

function TBaseRunner.GetInterfaceToObjectOffset(JumpN: Integer): Integer;
begin
  result := 0;
end;

function TBaseRunner.GetReturnFinalTypeId(InitSubN: Integer): Integer;
begin
  result := 0;
end;

procedure TBaseRunner.SetupInterfaces(P: Pointer);
var
  I, J, K, Index: Integer;
  ClassRec: TClassRec;
  IntfRec: TIntfRec;
  IntfMethodRec: TIntfMethodRec;
  A: Pointer;
begin
  ByteCodeInterfaceSetupList.Clear;

  if P = nil then
  begin
    for I := 0 to ClassList.Count - 1 do
    begin
      ClassRec := ClassList[I];
      for J := 0 to ClassRec.IntfList.Count - 1 do
      begin
        IntfRec := ClassRec.IntfList[J];
        for K := 0 to IntfRec.IntfMethods.Count - 1 do
        begin
          IntfMethodRec := IntfRec.IntfMethods[K];
          A := Pointer(IntfMethodRec.MethodOffset);

          Index := 0;
          if not NativeAddress(A) then
          begin
            WrapGlobalAddress(A);
            case K of
              0:
              begin
                Index := ByteCodeInterfaceSetupList.Count;
                ByteCodeInterfaceSetupList.Add(IntfMethodRec.MethodOffset);
              end;
              1: A := GetFakeAddRefAddress(Index);
              2: A := GetFakeReleaseAddress(Index);
            end;
          end;

          IntfMethodRec.MethodOffset := IntPax(A);
        end;
      end;
    end;
  end;

  ClassList.SetupInterfaces(P);
end;

function TBaseRunner.GetTypeInfo(const FullTypeName: String): PTypeInfo;
var
  R: TTypeInfoContainer;
begin
  R := ProgTypeInfoList.LookupFullName(FullTypeName);
  if R = nil then
    result := nil
  else
    result := R.TypeInfoPtr;
end;

function TBaseRunner.GetCallConv(const FullName: String): Integer;
var
  MapRec: TMapRec;
begin
  result := ccREGISTER;
  MapRec := ScriptMapTable.Lookup(FullName);

  if MapRec <> nil then
    if MapRec.Kind in KindSUBS then
    begin
      result := MapRec.SubDesc.CallConv;
      Exit;
    end;

  MapRec := HostMapTable.Lookup(FullName);
  if MapRec <> nil then
    if MapRec.Kind in KindSUBS then
      result := MapRec.SubDesc.CallConv;
end;

function TBaseRunner.GetRetSize(const FullName: String): Integer;
var
  MapRec: TMapRec;
begin
  result := 0;
  MapRec := ScriptMapTable.Lookup(FullName);

  if MapRec <> nil then
    if MapRec.Kind in KindSUBS then
    begin
      result := MapRec.SubDesc.RetSize;
      Exit;
    end;

  MapRec := HostMapTable.Lookup(FullName);
  if MapRec <> nil then
    if MapRec.Kind in KindSUBS then
      result := MapRec.SubDesc.RetSize;
end;

procedure TBaseRunner.SaveToBuff(var Buff);
var
  P: Pointer;
  S: TMemoryStream;
begin
  P := @Buff;
  S := TMemoryStream.Create;
  try
    SaveToStream(S);
    S.Position := 0;
    S.Read(P^, S.Size);
  finally
    FreeAndNil(S);
  end;
end;

procedure TBaseRunner.LoadFromBuff(var Buff);
var
  P: Pointer;
  temp: TMemoryStream;
  SZ: Integer;
begin
  P := @Buff;
  temp := TMemoryStream.Create;
  try
    temp.Write(P^, SizeOf(Integer));
    SZ := LongInt(P^);
    temp.Position := 0;
    temp.Write(P^, SZ);
    temp.Position := 0;
    LoadFromStream(temp);
  finally
    FreeAndNil(temp);
  end;
end;

procedure TBaseRunner.SaveToFile(const Path: String);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    SaveToStream(M);
    M.Position := 0;
    M.SaveToFile(Path);
  finally
    FreeAndNil(M);
  end;
end;

procedure TBaseRunner.LoadFromFile(const Path: String);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(Path);
    M.Position := 0;
    LoadFromStream(M);
  finally
    FreeAndNil(M);
  end;
end;

procedure TBaseRunner.RunInitialization;
begin
  fInitializationProcessed := true;

  CurrRunner := Self;
  if fGC = RootGC then
    fGC.Clear;
end;

procedure TBaseRunner.RunFinalization;
begin

end;

function TBaseRunner.GetRootProg: TBaseRunner;
begin
  result := Self;
  while result.PCUOwner <> nil do
    result := result.PCUOwner;
end;

function TBaseRunner.GetIsRootProg: Boolean;
begin
  result := PCUOwner = nil;
end;

function TBaseRunner.GetRootSearchPathList: TStringList;
begin
  result := GetRootProg.fSearchPathList;
end;

function TBaseRunner.FileExists(const FileName: String; out FullPath: String): Boolean;
var
  I: Integer;
  S: String;
begin
  if SysUtils.FileExists(FileName) then
  begin
    result := true;
    FullPath := FileName;
  end
  else
  begin
    result := false;

    for I := 0 to RootSearchPathList.Count - 1 do
    begin
      S := RootSearchPathList[I] + FileName;
      if SysUtils.FileExists(S) then
      begin
        result := true;
        FullPath := S;
        Exit;
      end;
    end;

    FullPath := FileName;
  end;
end;

procedure TBaseRunner.CopyRootBreakpoints(const UnitName: String);
var
  RP: TBaseRunner;
  I, L: Integer;
begin
  RP := GetRootProg;
  for I := 0 to RP.RuntimeModuleList.BreakpointList.Count - 1 do
    if StrEql(UnitName, RP.RuntimeModuleList.BreakpointList[I].ModuleName) then
    begin
      L := RP.RuntimeModuleList.BreakpointList[I].SourceLine;
      AddBreakpoint(UnitName, L);
    end;
end;

procedure TBaseRunner.UnloadDlls;
var
  H: Cardinal;
begin
  while DllList.Count > 0 do
  begin
    H := Cardinal(DllList.Objects[0]);
    FreeLibrary(H);
    DllList.Delete(0);
  end;
end;

procedure TBaseRunner.CopyRootEvents;
var
  RP: TBaseRunner;
begin
  RP := GetRootProg;
  if Self <> RP then
  begin
    Owner := RP.Owner;

    OnException := RP.OnException;
    OnUnhandledException := RP.OnUnhandledException;
    OnPause := RP.OnPause;
    OnPauseUpdated := RP.OnPauseUpdated;
    OnHalt := RP.OnHalt;
    OnLoadProc := RP.OnLoadProc;
    OnCreateObject := RP.OnCreateObject;
    OnAfterObjectCreation := RP.OnAfterObjectCreation;
    OnDestroyObject := RP.OnDestroyObject;
    OnAfterObjectDestruction := RP.OnAfterObjectDestruction;

    OnMapTableNamespace := RP.OnMapTableNamespace;
    OnMapTableVarAddress := RP.OnMapTableVarAddress;
    OnMapTableProcAddress := RP.OnMapTableProcAddress;
    OnMapTableClassRef := RP.OnMapTableClassRef;
    OnLoadPCU := RP.OnLoadPCU;

    OnPrint := RP.OnPrint;
    OnCustomExceptionHelper := RP.OnCustomExceptionHelper;

    OnBeginProcNotifyEvent := RP.OnBeginProcNotifyEvent;
    OnEndProcNotifyEvent := RP.OnEndProcNotifyEvent;
  end;
end;

procedure TBaseRunner.SetAddress(Offset: Integer; P: Pointer);
begin
  Move(P, DataPtr^[Offset], SizeOf(Pointer));
end;

function TBaseRunner.SetHostAddress(const FullName: String; Address: Pointer): Boolean;
var
  MR: TMapRec;
  P: Pointer;
  I: Integer;
begin
  result := false;
  MR := HostMapTable.Lookup(FullName);
  if MR <> nil then
  if MR.Kind in KindSUBS + [KindVAR] then
  begin
    P := ShiftPointer(DataPtr, MR.Offset);
    Pointer(P^) := Address;
    result := true;
  end;
  for I:=0 to ProgList.Count - 1 do
    TBaseRunner(ProgList[I].Prog).SetHostAddress(FullName, Address);
end;

procedure TBaseRunner.UnloadPCU(const FullPath: String);
var
  I: Integer;
begin
  ProgList.RemoveProg(FullPath);
  for I := 0 to ProgList.Count - 1 do
    TBaseRunner(ProgList[I].Prog).UnloadPCU(FullPath);
end;

procedure TBaseRunner.LoadPCU(const FileName: String;
                              var DestProg: Pointer);
var
  P: TBaseRunner;
  UnitName, FullPath: String;
  ProgRec: TProgRec;
  InputStream: TStream;
  C: TBaseRunnerClass;
begin
  DestProg := nil;

  FullPath := '';
  UnitName := ExtractFullOwner(FileName);

  InputStream := nil;

  if Assigned(OnLoadPCU) then
    OnLoadPCU(Owner, UnitName, InputStream);

  if InputStream = nil then
    if not FileExists(FileName, FullPath) then
    begin
      RaiseError(errFileNotFound, [FileName]);
    end;

  C := TBaseRunnerClass(ClassType);
  P := C.Create;

  P.PCUOwner := Self;
  if InputStream <> nil then
    P.LoadFromStream(InputStream)
  else
    P.LoadFromFile(FullPath);
  ProgRec := TProgRec.Create;
  ProgRec.FullPath := FullPath;
  ProgRec.Prog := P;
  ProgList.Add(ProgRec);
  P.CopyRootEvents;
end;

function TBaseRunner.GetAddressExtended(const FullName: String;
                                        var MR: TMapRec): Pointer;
var
  I: Integer;
begin
  result := GetAddress(FullName, MR);
  if result <> nil then
    Exit;
  for I := 0 to ProgList.Count - 1 do
  begin
    result := TBaseRunner(ProgList[I].Prog).GetAddressExtended(FullName, MR);
    if result <> nil then
      Exit;
  end;
end;

function TBaseRunner.GetAddressExtended(const FullName: String; OverCount: Integer;
                                        var MR: TMapRec): Pointer;
var
  I: Integer;
begin
  result := GetAddressEx(FullName, OverCount, MR);
  if result <> nil then
    Exit;
  for I := 0 to ProgList.Count - 1 do
  begin
    result := TBaseRunner(ProgList[I].Prog).GetAddressExtended(FullName, OverCount, MR);
    if result <> nil then
      Exit;
  end;
end;

function TBaseRunner.AddBreakpoint(const ModuleName: String;
                                   SourceLineNumber: Integer): TBreakpoint;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.AddBreakpoint(ModuleName, SourceLineNumber);
    if result <> nil then
      Exit;
  end;

  result := RunTimeModuleList.AddBreakpoint(ModuleName, SourceLineNumber);
end;

function TBaseRunner.AddTempBreakpoint(const ModuleName: String;
                                    SourceLineNumber: Integer): TBreakpoint;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.AddTempBreakpoint(ModuleName, SourceLineNumber);
    if result <> nil then
      Exit;
  end;

  result := RunTimeModuleList.AddTempBreakpoint(ModuleName, SourceLineNumber);
end;

function TBaseRunner.RemoveBreakpoint(const ModuleName: String;
                                   SourceLineNumber: Integer): Boolean;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.RemoveBreakpoint(ModuleName, SourceLineNumber);
    if result then
      Exit;
  end;

  result := RunTimeModuleList.RemoveBreakpoint(ModuleName, SourceLineNumber);
end;

function TBaseRunner.RemoveBreakpoint(const ModuleName: String): Boolean;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.RemoveBreakpoint(ModuleName);
    if result then
      Exit;
  end;

  result := RunTimeModuleList.RemoveBreakpoint(ModuleName);
end;

function TBaseRunner.HasBreakpoint(const ModuleName: String;
                                SourceLineNumber: Integer): Boolean;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.HasBreakpoint(ModuleName, SourceLineNumber);
    if result then
      Exit;
  end;

  result := RunTimeModuleList.HasBreakpoint(ModuleName, SourceLineNumber);
end;

procedure TBaseRunner.RemoveAllBreakpoints;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    P.RemoveAllBreakpoints;
  end;

  RunTimeModuleList.RemoveAllBreakpoints;
end;

function TBaseRunner.IsExecutableLine(const ModuleName: String;
                                      SourceLineNumber: Integer): Boolean;
var
  I: Integer;
  P: TBaseRunner;
begin
  for I := 0 to ProgList.Count - 1 do
  begin
    P := TBaseRunner(ProgList[I].Prog);
    result := P.IsExecutableLine(ModuleName, SourceLineNumber);
    if result then
      Exit;
  end;

  result := RunTimeModuleList.IsExecutableLine(ModuleName, SourceLineNumber);
end;

function TBaseRunner.GetByteCodeLine: Integer;
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_ByteCodePtr);
  result := LongInt(P^);
end;

procedure TBaseRunner.SetByteCodeLine(N: Integer);
var
  P: Pointer;
begin
  P := ShiftPointer(Data, H_ByteCodePtr);
  LongInt(P^) := N;
end;

function TBaseRunner.GetSourceLine: Integer;
begin
  result := GetByteCodeLine;

  if result = - 1 then
    Exit;

  result := RunTimeModuleList.GetSourceLine(result);
end;

function TBaseRunner.GetModuleName: String;
var
  ByteCodeLine: Integer;
begin
  result := '';
  ByteCodeLine := GetByteCodeLine;

  if ByteCodeLine = - 1 then
    Exit;

  result := RunTimeModuleList.GetModuleName(ByteCodeLine);
end;

function TBaseRunner.GetModuleIndex: Integer;
var
  ByteCodeLine: Integer;
begin
  result := -1;
  ByteCodeLine := GetByteCodeLine;

  if ByteCodeLine = - 1 then
    Exit;

  result := RunTimeModuleList.GetModuleIndex(ByteCodeLine);
end;

function TBaseRunner.GetRunMode: Integer;
begin
  result := GetRootProg.fRunMode;
end;

procedure TBaseRunner.SetRunMode(value: Integer);
begin
  GetRootProg.fRunMode := value;
end;

function TBaseRunner.GetCodePtr: PBytes;
begin
  result := nil;
end;

function TBaseRunner.Valid: Boolean;
begin
  result := (Data <> nil);
end;

function TBaseRunner.GetResultPtr: Pointer;
begin
  result := Data;
end;

procedure TBaseRunner.CreateGlobalJSObjects;
begin
  if Assigned(CrtJSObjects) then
    CrtJSObjects(Self, JS_Record);
end;

function TBaseRunner.GetImageSize: Integer;
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S);
    result := S.Size;
  finally
    FreeAndNil(S);
  end;
end;

function TBaseRunner.GetImageDataPtr: Integer;
begin
  if fImageDataPtr = 0 then
    GetImageSize;
  result := fImageDataPtr;
end;

procedure TBaseRunner.DestroyScriptObject(var X: TObject);
begin
  FreeAndNil(X);
end;

function TBaseRunner.GetFieldAddress(X: TObject; const FieldName: String): Pointer;
var
  MapRec: TMapRec;
  MapFieldRec: TMapFieldRec;
begin
  if IsPaxObject(X) then
  begin
    MapRec := ScriptMapTable.LookupType(X.ClassName);
    if MapRec <> nil then
    begin
      if MapRec.FieldList = nil then
        RaiseError(errInternalError, []);
      MapFieldRec := MapRec.FieldList.Lookup(FieldName);
      if MapFieldRec = nil then
        result := nil
      else
        result := ShiftPointer(X, MapFieldRec.FieldOffset);
    end
    else
      result := X.FieldAddress(FieldName);
  end
  else
    result := X.FieldAddress(FieldName);
end;

function TBaseRunner.GetIsEvent: Boolean;
begin
  result := GetRootProg.fIsEvent;
end;

procedure TBaseRunner.SetIsEvent(value: Boolean);
begin
  GetRootProg.fIsEvent := value;
end;

function TBaseRunner.GetCurrentSub: TMapRec;
var
  N: Integer;
begin
  N := GetByteCodeLine;
  result := ScriptMapTable.GetSub(N);
end;

function TBaseRunner.GetCurrentFunctionFullName: String;
var
  MR: TMapRec;
begin
  result := '';
  MR := GetCurrentSub;
  if MR = nil then
    Exit;
  result := MR.FullName;
end;

procedure TBaseRunner.GetCurrentLocalVars(result: TStrings);
var
  MR: TMapRec;
  I: Integer;
begin
  MR := GetCurrentSub;
  if MR = nil then
    Exit;
  if MR.SubDesc.LocalVarList.Count = 0 then
    Exit;
  for I := 0 to MR.SubDesc.LocalVarList.Count - 1 do
    result.Add(MR.SubDesc.LocalVarList[I].LocalVarName);
end;

procedure TBaseRunner.GetCurrentParams(result: TStrings);
var
  MR: TMapRec;
  I: Integer;
begin
  MR := GetCurrentSub;
  if MR = nil then
    Exit;
  if MR.SubDesc.ParamList.Count = 0 then
    Exit;
  for I := 0 to MR.SubDesc.ParamList.Count - 1 do
    result.Add(MR.SubDesc.ParamList[I].ParamName);
end;

procedure TBaseRunner.SetGlobalSym(AGlobalSym: Pointer);
begin
  GlobalSym := TBaseSymbolTable(AGlobalSym);
  if LocalSymbolTable <> nil then
    FreeAndNil(LocalSymbolTable);
  LocalSymbolTable := TProgSymbolTable.Create(GlobalSym);
end;

procedure TBaseRunner.RegisterMember(LevelId: Integer; const Name: String;
                                  Address: Pointer);
begin
  if LocalSymbolTable.Card = -1 then
    LocalSymbolTable.Reset;
  LocalSymbolTable.RegisterMember(LevelId, Name, Address);
end;

function TBaseRunner.RegisterNamespace(LevelId: Integer; const Name: String): Integer;
begin
  if LocalSymbolTable.Card = -1 then
    LocalSymbolTable.Reset;
  result := LocalSymbolTable.RegisterNamespace(LevelId, Name);
end;

function TBaseRunner.RegisterClassType(LevelId: Integer; C: TClass): Integer;
begin
  if LocalSymbolTable.Card = -1 then
    LocalSymbolTable.Reset;
  result := LocalSymbolTable.RegisterClassType(LevelId, C);
end;

procedure TBaseRunner.MapGlobal;
begin
  ForceMapping(GlobalSym, true);
end;

procedure TBaseRunner.MapLocal;
begin
  ForceMapping(LocalSymbolTable, true);
end;

procedure TBaseRunner.ForceMapping(SymbolTable: TBaseSymbolTable;
                                   Reassign: Boolean);
var
  IsGlobal: Boolean;
  I, J: Integer;
  MapRec: TMapRec;
  FullName: String;
  P: Pointer;
  ClsRef: TClass;
begin
  IsGlobal := SymbolTable = GlobalSym;

  if HostMapTable.Count > 0 then
  begin
    for I:=0 to HostMapTable.Count - 1 do
    begin
      MapRec := HostMapTable[I];

      if MapRec.Global <> IsGlobal then
        continue;

      if MapRec.TypedConst then
        continue;

      FullName := MapRec.FullName;

      J := SymbolTable.LookupFullNameEx(FullName, true, MapRec.SubDesc.OverCount);
      if J > 0 then
      begin
        if MapRec.Kind = KindVAR then
        begin
          P := SymbolTable[J].Address;
          if P <> nil then
          begin
            if Reassign then
            begin
              if Assigned(OnMapTableVarAddress) then
                          OnMapTableVarAddress(Owner,
                                               FullName,
                                               MapRec.Global,
                                               P);

              if P = nil then
                RaiseError(errUnresolvedAddress, [FullName]);

              SymbolTable[J].Address := P;
            end;

            SetAddress(MapRec.Offset, P);
          end
          else
          begin
            P := nil;
            if Assigned(OnMapTableVarAddress) then
                        OnMapTableVarAddress(Owner,
                                             FullName,
                                             MapRec.Global,
                                             P);
            if P = nil then
              RaiseError(errUnresolvedAddress, [FullName]);
            SetAddress(MapRec.Offset, P);
          end;
        end
        else if MapRec.Kind in KindSUBS then
        begin
          P := nil;
          if Assigned(OnMapTableProcAddress) then
                      OnMapTableProcAddress(Owner,
                                            FullName,
                                            MapRec.SubDesc.OverCount,
                                            MapRec.Global,
                                            P)
          else
            P := SymbolTable[J].Address;

          if P = nil then
             P := LookupAvailAddress(FullName, MapRec.SubDesc.OverCount);
          if P = nil then
            RaiseError(errUnresolvedAddress, [FullName]);
          SetAddress(MapRec.Offset, P);
        end
        else if MapRec.Kind = KindTYPE then
        begin
          ClsRef := nil;
          if Assigned(OnMapTableClassRef) then
            OnMapTableClassRef(Owner,
                               FullName,
                               MapRec.Global,
                               ClsRef)
          else
            ClsRef := TClass(IntPax(SymbolTable[J + 1].Value));
          if ClsRef = nil then
            ClsRef := LookupAvailClass(FullName);
          if ClsRef = nil then
            RaiseError(errUnresolvedClassReference, [FullName])
          else
            RegisterClassEx(ClsRef, MapRec.FullName, MapRec.Offset, MapRec.ClassIndex);
        end;
      end
      else
      begin
        if MapRec.Kind = KindVAR then
        begin
          if Assigned(OnMapTableVarAddress) then
          begin
            P := nil;
            OnMapTableVarAddress(Owner,
                                 FullName,
                                 MapRec.Global,
                                 P);
            if P = nil then
              RaiseError(errUnresolvedAddress, [FullName]);
            SetAddress(MapRec.Offset, P);
          end
          else
            RaiseError(errHostMemberIsNotDefined, [FullName]);
        end
        else if MapRec.Kind in KindSUBS then
        begin
          P := nil;
          if Assigned(OnMapTableProcAddress) then
            OnMapTableProcAddress(Owner,
                                  FullName,
                                  MapRec.SubDesc.OverCount,
                                  MapRec.Global,
                                  P);
          if P = nil then
             P := LookupAvailAddress(FullName, MapRec.SubDesc.OverCount);
          if P = nil then
            RaiseError(errUnresolvedAddress, [FullName]);
          SetAddress(MapRec.Offset, P);
        end
        else if MapRec.Kind = KindTYPE then
        begin
          ClsRef := nil;
          if Assigned(OnMapTableClassRef) then
            OnMapTableClassRef(Owner,
                               FullName,
                               MapRec.Global,
                               ClsRef);
          if ClsRef = nil then
            ClsRef := LookupAvailClass(FullName);
          if ClsRef = nil then
          begin
            RaiseError(errUnresolvedClassReference, [FullName]);
          end
          else
            RegisterClassEx(ClsRef, MapRec.FullName, MapRec.Offset, MapRec.ClassIndex);
        end;
      end;
    end;
  end;

  for I:=0 to ProgList.Count - 1 do
    TBaseRunner(ProgList[I].Prog).ForceMapping(SymbolTable, Reassign);
end;

procedure TBaseRunner.ForceMappingEvents;
var
  I, J, TypeId: Integer;
  MapRec: TMapRec;
  P: Pointer;
  ClsRef: TClass;
  L: TIntegerList;
  C: TClass;
  ClassRec: TClassRec;
begin
  if not Assigned(OnMapTableProcAddress) then
    Exit;
  if not Assigned(OnMapTableClassRef) then
    Exit;

  L := TIntegerList.Create;

  try

    for I:=0 to HostMapTable.Count - 1 do
    begin
      MapRec := HostMapTable[I];
      if MapRec.Kind in KindSUBS then
      begin
        P := nil;
        OnMapTableProcAddress(Owner,
                              MapRec.FullName,
                              MapRec.SubDesc.OverCount,
                              MapRec.Global,
                              P);
        if P = nil then
           P := LookupAvailAddress(MapRec.FullName, MapRec.SubDesc.OverCount);
        if P <> nil then
        begin
          J := GlobalSym.LookupFullNameEx(MapRec.FullName, true, MapRec.SubDesc.OverCount);
          if J > 0 then
          begin
            GlobalSym[J].Address := P;
            if GlobalSym[J].IsVirtual then
              if GlobalSym[J].MethodIndex = 0 then
                L.Add(J);
          end;
          SetAddress(MapRec.Offset, P);
        end;
      end
      else if MapRec.Kind = KindTYPE then
      begin
        ClsRef := nil;
        if Assigned(OnMapTableClassRef) then
            OnMapTableClassRef(Owner,
                               MapRec.FullName,
                               MapRec.Global,
                               ClsRef);
        if ClsRef = nil then
          ClsRef := LookupAvailClass(MapRec.FullName);
        if ClsRef <> nil then
        begin
          J := GlobalSym.LookupFullName(MapRec.FullName, true);
          if J > 0 then
          begin
            GlobalSym[J].PClass := ClsRef;
            GlobalSym[J+1].Value := LongInt(ClsRef);
          end;
          ClassRec := ClassList.Lookup(MapRec.FullName);
          if ClassRec <> nil then
            ClassRec.PClass := ClsRef;
        end;
      end;
    end;

    for I:=0 to L.Count - 1 do
    begin
      J := L[I];
      TypeId := GlobalSym[J].Level;
      C := GlobalSym[TypeId].PClass;
      if C = nil then
        RaiseError(errInternalErrorMethodIndex, []);
      GlobalSym[J].MethodIndex := VirtualMethodIndex(C,
         GlobalSym[J].Address) + 1;
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TBaseRunner.RegisterDefinitions(SymbolTable: TBaseSymbolTable);
var
  I, I1, Offset: Integer;
  J: IntPax;
  R, R1: TSymbolRec;
  FullName: String;
  IsGlobal: Boolean;
  MapRec: TMapRec;
begin
  IsGlobal := SymbolTable = GlobalSym;

  TOffsetList_Sort(OffsetList);

  if UseMapping then
  begin
    if HostMapTable.Count > 0 then
    begin
      for I:=0 to HostMapTable.Count - 1 do
      begin
        MapRec := HostMapTable[I];
        if MapRec.Kind <> kindNAMESPACE then
          continue;
        if MapRec.Global <> IsGlobal then
          continue;
        FullName := MapRec.FullName;
        J := SymbolTable.LookupFullName(FullName, true);
        if J = 0 then
          if Assigned(OnMapTableNamespace) then
             OnMapTableNamespace(Owner,
                                 FullName,
                                 MapRec.Global);
      end;
    end;
  end;

  if IsGlobal then
    I1 := 1
  else
    I1 := FirstLocalId + 1;

  for I:=I1 to SymbolTable.Card do
  begin
    R := SymbolTable[I];

    if R.Address <> nil then
    begin
      Offset := GetOffset(R.Shift);
      if Offset = -1 then
        continue;

      SetAddress(Offset, R.Address);
    end
    else if R.ClassIndex <> -1 then
    begin
      R1 := SymbolTable[I + 1]; // cls ref
      J := R1.Value;
      if J = 0 then
        ClassList.Add(R.FullName, R. Host)
      else
      begin
        Offset := GetOffset(R1.Shift);
        if Offset = -1 then
          continue;

        RegisterClass(TClass(J), R.FullName, Offset);
      end;
    end;
  end;
end;

function TBaseRunner._VirtualAlloc(Address: Pointer;
                                Size, flAllocType, flProtect: Cardinal): Pointer;
begin
  result := AllocMem(Size);
end;

procedure TBaseRunner._VirtualFree(Address: Pointer; Size: Cardinal);
begin
  FreeMem(Address, Size);
end;

procedure TBaseRunner.Deallocate;
begin
  if Data <> nil then
  begin
    FreeMem(Data, DataSize);
    Data := nil;
  end;

  if Prog <> nil then
  begin
{$IFDEF PAXARM}
    FreeMem(Prog, fCodeSize);
{$ELSE}
    _VirtualFree(Prog, fCodeSize);
{$ENDIF}
    Prog := nil;
  end;

  ClearCurrException;
  fPrevException := nil;

  UnProtect;
  InitializationIsProcessed := false;
end;

procedure TBaseRunner.Allocate(InitCodeSize, InitDataSize: Integer);
begin
  Deallocate;

  DataSize := InitDataSize;
  Data := AllocMem(DataSize);

  fCodeSize := InitCodeSize;
{$IFDEF PAXARM}
  Prog := AllocMem(fCodeSize);
{$ELSE}
  Prog := _VirtualAlloc(nil, fCodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
{$ENDIF}

  Protect;

  SetAddress(H_SelfPtr, Self);
  SetAddress(H_ExceptionPtr, @ fCurrException);

  RegisterDefinitions(GlobalSym);
end;

procedure TBaseRunner.AllocateSimple(InitCodeSize, InitDataSize: Integer);
begin
  Deallocate;

  DataSize := InitDataSize;
  Data := AllocMem(DataSize);

  fCodeSize := InitCodeSize;
{$IFDEF PAXARM}
  Prog := AllocMem(fCodeSize);
{$ELSE}
  Prog := _VirtualAlloc(nil, fCodeSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
{$ENDIF}
end;

procedure TBaseRunner.Protect;
begin
end;

procedure TBaseRunner.UnProtect;
begin
end;

procedure TBaseRunner.ClearCurrException;
begin
  if Assigned(CurrException) then
  begin
{$IFNDEF ARC}
    CurrException.Free;
{$ENDIF}
    CurrException := nil;
  end;
end;

procedure TBaseRunner.RunExtended;
begin
{$IFDEF PAXARM}
  if IsPaused then
{$ELSE}
  if IsPaused or (EPoint <> nil) then
{$ENDIF}
  begin
    RunInternal;
    Exit;
  end
  else
  begin
    RunInitialization;
    if Assigned(CrtJSObjects) then
      CrtJSObjects(Self, Self.JS_Record);
  end;
  if IsPaused then
    Exit;
  RunExceptInitialization;
  if not IsPaused then
  begin
    if not SuspendFinalization then
    begin
      InitializationIsProcessed := false;
      ProgList.RunFinalization;
    end;
  end;
end;

procedure TBaseRunner.RunInternal;
begin
  Run;
end;

procedure TBaseRunner.RunExceptInitialization;
begin
  Run;
end;

function TBaseRunner.FireAddressEvent(MR: TMapRec): Pointer;
begin
  result := @TBaseRunner.FireAddressEvent;
end;

function TBaseRunner.GetAddress(Handle: Integer): Pointer;
begin
  if Handle < 0 then
  begin
    result := ShiftPointer(CodePtr, - Handle);
  end
  else
  begin
    result := ShiftPointer(DataPtr, GetOffset(Handle));
  end;
end;

function TBaseRunner.GetAddress(const FullName: String; var MR: TMapRec): Pointer;
begin
  result := nil;
  MR := ScriptMapTable.Lookup(FullName);
  if MR <> nil then
  begin
    case MR.Kind of
      KindVAR, kindTYPE: result := ShiftPointer(DataPtr, MR.Offset);
      KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
      begin
        if MR.IsExternal then
          result := nil
        else
        begin
          result := ShiftPointer(CodePtr, MR.Offset);
        end;
      end;
    end;
    Exit;
  end;

  MR := HostMapTable.Lookup(FullName);
  if MR <> nil then
  if MR.Kind in KindSUBS + [KindVAR] then
  begin
    result := ShiftPointer(DataPtr, MR.Offset);
    result := Pointer(result^);
  end;
end;

function TBaseRunner.GetAddressEx(const FullName: String; OverCount: Integer;
                                  var MR: TMapRec): Pointer;
begin
  result := nil;

  if OverCount = 0 then
  begin
    result := GetAddress(FullName, MR);
    Exit;
  end;

  MR := ScriptMapTable.LookupEx(FullName, OverCount);
  if MR <> nil then
  begin
    case MR.Kind of
      KindVAR, kindTYPE: result := ShiftPointer(DataPtr, MR.Offset);
      KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
      begin
        if MR.IsExternal then
          result := nil
        else
        begin
          result := ShiftPointer(CodePtr, MR.Offset);
        end;
      end;
    end;
    Exit;
  end;

  MR := HostMapTable.LookupEx(FullName, OverCount);
  if MR <> nil then
  if MR.Kind in KindSUBS + [KindVAR] then
  begin
    result := ShiftPointer(DataPtr, MR.Offset);
    result := Pointer(result^);
    Exit;
  end;
end;

function TBaseRunner.LoadAddressEx(const FileName, ProcName: String;
                                   RunInit: Boolean;
                                   OverCount: Integer;
                                   var MR: TMapRec;
                                   var DestProg: Pointer): Pointer;
begin
  result := GetRootProg.ProgList.LoadAddress(FileName, ProcName,
                                          RunInit,
                                          OverCount,
                                          MR,
                                          DestProg);
end;

procedure TBaseRunner.CreateClassFactory;
var
  I: Integer;
  ClassRec: TClassRec;
  C: TClass;
  S: String;
  P: Pointer;
  PaxInfo: PPaxInfo;
  MR: TMapRec;
begin
  ProgClassFactory.Clear;

  for I:=0 to ClassList.Count - 1 do
  begin
    ClassRec := ClassList[I];
    if ClassRec.Host then
      continue;
    S := ExtractName(ClassRec.FullName);
    C := ProgClassFactory.CreatePaxClass(ClassRec.FullName, ClassRec.InstSize, TObject,
         GetDestructorAddress);
    ClassRec.PClass := C;

    P := GetAddress(ClassRec.FullName, MR);
    if P <> nil then
      Pointer(P^) := C;

    PaxInfo := GetPaxInfo(ClassRec.PClass);
    if PaxInfo = nil then
       RaiseError(errInternalError, []);
    PaxInfo^.Prog := Self;
    PaxInfo^.ClassIndex := I;
  end;

  ProgClassFactory.SetupParents(Self, ClassList);
  ProgClassFactory.AddInheritedMethods;
  ProgClassFactory.AddOverridenMethods(Self, ScriptMapTable);
  ProgClassFactory.SetupStdVirtuals(ClassList, CodePtr);

  ProgTypeInfoList.AddToProgram(Self);
end;

function TBaseRunner.GetInitCallStackCount: Integer;
begin
  result := GetRootProg.fInitCallStackCount;
end;

procedure TBaseRunner.SetInitCallStackCount(value: Integer);
begin
  GetRootProg.fInitCallStackCount := value;
end;

function TBaseRunner.GetIsRunning: Boolean;
begin
  result := fIsRunning;
end;

procedure TBaseRunner.SetIsRunning(value: Boolean);
begin
  fIsRunning := value;
end;

function TBaseRunner.NeedAllocAll: Boolean;
begin
  result := false;
end;

function TBaseRunner.GetRootGC: TGC;
begin
  result := GetRootProg.fGC;
end;

function TBaseRunner.GetRootOwner: TObject;
begin
  result := GetRootProg.Owner;
end;

procedure TBaseRunner.SaveState(S: TStream);
begin
end;

procedure TBaseRunner.LoadState(S: TStream);
begin
end;

function TBaseRunner.GetProcessingExceptBlock: Boolean;
begin
  result := fProcessingExceptBlock;
end;

procedure TBaseRunner.SetProcessingExceptBlock(value: Boolean);
begin
  fProcessingExceptBlock := value;
end;

function TBaseRunner.GetExceptionIsAvailableForHostApplication: Boolean;
begin
  result := GetRootProg.fExceptionIsAvailableForHostApplication;
end;

procedure TBaseRunner.SetExceptionIsAvailableForHostApplication(value: Boolean);
begin
  GetRootProg.fExceptionIsAvailableForHostApplication := value;
end;

function TBaseRunner.GetHasError: Boolean;
begin
  result := GetRootProg.fHasError;
end;

procedure TBaseRunner.SetHasError(value: Boolean);
begin
  GetRootProg.fHasError := value;
end;

{$IFDEF FPC}
type
  TStreamOriginalFormat = (sofUnknown, sofBinary, sofText, sofUTF8Text);

function TestStreamFormat(Stream: TStream): TStreamOriginalFormat;
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Stream.Position;
  Signature := 0;
  Stream.Read(Signature, SizeOf(Signature));
  Stream.Position := Pos;
  if (Byte(Signature) = $FF) or (Signature = Integer(FilerSignature)) or (Signature = 0) then
    Result := sofBinary
    // text format may begin with "object", "inherited", or whitespace
  else if AnsiChar(Signature) in ['o','O','i','I',' ',#13,#11,#9] then
    Result := sofText
  else if (Signature and $00FFFFFF) = $00BFBBEF then
    Result := sofUTF8Text
  else
    Result := sofUnknown;
end;
{$ENDIF}

procedure TBaseRunner.LoadDFMFile(Instance: TObject; const FileName: String; const OnFindMethod: TFindMethodEvent = nil; const OnError: TReaderError = nil);
var
  fs: TFileStream;
  ms: TMemoryStream;
  Reader: TReader;
  ptd: PTypeData;
  P: Pointer;
  MR: TMapRec;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  ms := TMemoryStream.Create;
  try
    Reader := nil;
    case TestStreamFormat(fs) of
{$IFDEF UNIC}
      sofText,
  sofUTF8Text:
{$ELSE}
      sofText:
{$ENDIF}
      begin
        ObjectTextToBinary(fs, ms);
        ms.Position := 0;
        Reader := TReader.Create(ms, 4096 * 10);
      end;
      sofBinary:
      begin
        fs.ReadResHeader;
        Reader := TReader.Create(fs, 4096 * 10);
      end;
    else
      RaiseError(errUnknownStreamFormat, []);
    end;

    try
      gInstance := Instance;
      Reader.OnFindMethod := OnFindMethod;
      Reader.OnError      := OnError;

      Reader.ReadRootComponent(Instance as TComponent);
    finally
      FreeAndNil(Reader);
    end;
  finally
    FreeAndNil(ms);
    FreeAndNil(fs);
  end;
  if not ModeSEH then
    RebindEvents(Instance);
  ptd := GetTypeData(Instance.ClassInfo);
  P := GetAddress(StringFromPShortString(@ptd^.UnitName) + '.' +
    Copy(Instance.ClassName, 2, Length(Instance.ClassName)), MR);
  if P <> nil then
    Pointer(P^) := Instance;
end;

procedure TBaseRunner.LoadDFMStream(Instance: TObject; S: TStream; const OnFindMethod: TFindMethodEvent = nil; const OnError: TReaderError = nil);
var
  ms: TMemoryStream;
  Reader: TReader;
  ptd: PTypeData;
  P: Pointer;
  MR: TMapRec;
begin
  ms := TMemoryStream.Create;
  try
    Reader := nil;
    case TestStreamFormat(s) of
{$IFDEF UNIC}
      sofText,
  sofUTF8Text:
{$ELSE}
      sofText:
{$ENDIF}
      begin
        ObjectTextToBinary(s, ms);
        ms.Position := 0;
        Reader := TReader.Create(ms, 4096 * 10);
      end;
      sofBinary:
      begin
        s.ReadResHeader;
        Reader := TReader.Create(s, 4096 * 10);
      end;
    else
      RaiseError(errUnknownStreamFormat, []);
    end;

    try
      gInstance := Instance;
      Reader.OnFindMethod := OnFindMethod;
      Reader.OnError      := OnError;

      Reader.ReadRootComponent(Instance as TComponent);
    finally
      FreeAndNil(Reader);
    end;
  finally
    FreeAndNil(ms);
  end;
  if not ModeSEH then
    RebindEvents(Instance);
  ptd := GetTypeData(Instance.ClassInfo);
  P := GetAddress(StringFromPShortString(@ptd^.UnitName) + '.' +
     Copy(Instance.ClassName, 2, Length(Instance.ClassName)), MR);
  if P <> nil then
    Pointer(P^) := Instance;
end;

procedure TBaseRunner.RebindEvents(AnInstance: TObject);
begin

end;

function TBaseRunner.CallByteCode(InitN: Integer;
                       This: Pointer;
                       R_AX, R_CX, R_DX, R_8, R_9: IntPax;
                       StackPtr: Pointer;
                       ResultPtr: Pointer; var FT: Integer): Integer;
begin
  result := 0;
end;

procedure TBaseRunner.WrapMethodAddress(var Address: Pointer);
var
  I, N: integer;
begin
  if Address <> nil then
    if not NativeAddress(Address) then
    begin
      N := IntPax(Address);
      I := ClassList.GetByteCodeMethodEntryIndex(N);
      if I = -1 then
        RaiseError(errInternalError, []);
      Address := GetFakeHandlerAddress(I);
    end;
end;

function TBaseRunner.WrapGlobalAddress(var Address: Pointer): Integer;
var
  I, N: integer;
begin
  result := -1;
  if Address <> nil then
    if not NativeAddress(Address) then
    begin
      N := IntPax(Address);
      for I := 0 to System.Length(ByteCodeGlobalEntryList) - 1 do
        if ByteCodeGlobalEntryList[I] = N then
        begin
          Address := GetFakeGlobalAddress(I);
          result := I;
          Exit;
        end;
      RaiseError(errInternalError, []);
    end;
end;

{$ifdef DRTTI}
function TBaseRunner.HasAvailUnit(const FullName: String): Boolean;
begin
  result := AvailUnitList.IndexOf(FullName) >= 0;
end;

function TBaseRunner.LookUpAvailClass(const FullName: String): TClass;
var
  t: TRTTIType;
  I: Integer;
begin
  result := nil;
  I := AvailTypeList.IndexOf(FullName);
  if I >= 0 then
  begin
    t := TRTTIType(AvailTypeList.Objects[I]);
    if t is TRttiInstanceType then
      result := (t as TRttiInstanceType).MetaclassType;
  end;
end;

function TBaseRunner.LookUpAvailAddress(const FullName: String;
                                        OverCount: Integer): Pointer;
var
  m: TRTTIMethod;
begin
  m := LookUpAvailMethod(FullName, OverCount);
  if m = nil then
    result := nil
  else
    result := m.CodeAddress;
end;

function TBaseRunner.LookUpAvailMethod(const FullName: String;
                                       OverCount: Integer): TRTTIMethod;
var
  t: TRTTIType;
  I, K: Integer;
  TypeName, MethName: String;
{$IFDEF DPULSAR}
  IndexedProp: TRTTIIndexedProperty;
{$ENDIF}
  m: TRTTIMethod;
begin
  result := nil;
  TypeName := ExtractFullOwner(FullName);
  MethName := ExtractName(FullName);
  I := AvailTypeList.IndexOf(TypeName);
  if I = -1 then
    I := AvailTypeList.IndexOf('System.' + TypeName);
  if I = -1 then
    Exit;

  t := TRTTIType(AvailTypeList.Objects[I]);

  K := 0;
  for m in t.GetDeclaredMethods do
    if CheckMethod(t, m) then
    if StrEql(m.Name, MethName) then
    begin
      if OverCount = 0 then
      begin
        result := m;
        Exit;
      end
      else
      begin
        Inc(K);
        if K = OverCount then
          Exit;
      end;
    end;

{$IFDEF DPULSAR}
  for IndexedProp in t.GetDeclaredIndexedProperties do
    if CheckIndexedProperty(t, IndexedProp) then
    begin
      if IndexedProp.IsReadable then
      begin
        result := IndexedProp.ReadMethod;
        if StrEql(result.Name, MethName) then
          Exit;
      end;
      if IndexedProp.IsWritable then
      begin
        result := IndexedProp.WriteMethod;
        if StrEql(result.Name, MethName) then
          Exit;
      end;
    end;
{$ENDIF}

  result := nil;
end;

{$else}

function TBaseRunner.HasAvailUnit(const FullName: String): Boolean;
begin
  result := false;
end;

function TBaseRunner.LookUpAvailClass(const FullName: String): TClass;
begin
  result := nil;
end;

function TBaseRunner.LookUpAvailAddress(const FullName: String;
                                        OverCount: Integer): Pointer;
begin
  result := nil;
end;
{$endif}

end.
