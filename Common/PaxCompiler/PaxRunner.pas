////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxRunner.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxRunner;
interface
uses
  TypInfo,
  SysUtils,
  Classes,
  PaxInfos,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_MAP,
  PAXCOMP_RTI,
  PAXCOMP_BASERUNNER,
  PaxInvoke;

const
 _rmRUN = 0;
 _rmTRACE_INTO = 1;
 _rmSTEP_OVER = 2;
 _rmRUN_TO_CURSOR = 3;
 _rmNEXT_SOURCE_LINE = 4;

type
  TPaxRunner = class;

  TPaxPauseNotifyEvent = procedure (Sender: TPaxRunner;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  TPaxHaltNotifyEvent = procedure (Sender: TPaxRunner; ExitCode: Integer;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  TPaxErrNotifyEvent = procedure (Sender: TPaxRunner; E: Exception;
       const ModuleName: String; SourceLineNumber: Integer) of object;

  TPaxLoadProcEvent = procedure (Sender: TPaxRunner;
       const ProcName, DllName: String; var Address: Pointer) of object;

  TPaxObjectNotifyEvent = procedure (Sender: TPaxRunner;
       Instance: TObject) of object;

  TPaxIdNotifyEvent = procedure (Sender: TPaxRunner;
       Id: Integer) of object;

  TPaxClassNotifyEvent = procedure (Sender: TPaxRunner;
       C: TClass) of object;

  TPaxMapTableNamespaceEvent = procedure (Sender: TPaxRunner;
                                       const FullName: String;
                                       Global: Boolean) of object;

  TPaxMapTableVarAddressEvent = procedure (Sender: TPaxRunner;
       const FullName: String; Global: Boolean; var Address: Pointer) of object;
  TPaxMapTableProcAddressEvent = procedure (Sender: TPaxRunner;
       const FullName: String; OverCount: Byte;
       Global: Boolean; var Address: Pointer) of object;

  TPaxMapTableClassRefEvent = procedure (Sender: TPaxRunner;
       const FullName: String;
       Global: Boolean; var ClassRef: TClass) of object;

  TPaxPrintEvent = procedure (Sender: TPaxRunner;
                              const Text: String) of object;
  TPaxPrintExEvent = procedure (Sender: TPaxRunner;
                                Address: Pointer;
                                Kind: Integer;
                                FT: Integer;
                                L1, L2: Integer) of object;
  TPaxPrintClassTypeFieldEvent = procedure (Sender: TPaxRunner;
                                const Infos: TPrintClassTypeFieldInfo)
                                of object;
  TPaxPrintClassTypePropEvent = procedure (Sender: TPaxRunner;
                                const Infos: TPrintClassTypePropInfo)
                                of object;

  TPaxCustomExceptionHelperEvent = procedure (Sender: TPaxRunner;
                                      RaisedException, DestException: Exception)
                                      of object;

  TPaxRunnerLoadPCUEvent = procedure (Sender: TPaxRunner; const UnitName: String;
                              var result: TStream) of object;

  TPaxStreamEvent = procedure (Sender: TPaxRunner; Stream: TStream) of object;
  TPaxProcNotifyEvent = procedure (Sender: TPaxRunner;
       const FullName: String; OverCount: Byte) of object;

  TPaxVirtualObjectMethodCallEvent = procedure(Sender: TPaxRunner; const ObjectName,
      PropName: String; const Params: array of Variant; var result: Variant) of object;
  TPaxVirtualObjectPutPropertyEvent = procedure(Sender: TPaxRunner; const ObjectName,
      PropName: String; const Params: array of Variant; const value: Variant) of object;

  TPaxRunner = class(TComponent)
  private
    prog: TBaseRunner;
    function GetSourceLine: Integer;
    function GetModuleName: String;
    function GetDataPtr: Pointer;
    function GetCodePtr: Pointer;
    function GetDataSize: Integer;
    function GetCodeSize: Integer;
    function GetProgramSize: Integer;
    function GetResultPtr: Pointer;
    function GetPCUCount: Integer;
    function GetSearchPathList: TStringList;

    function GetRunMode: Integer;
    procedure SetRunMode(value: Integer);

    function GetConsole: Boolean;
    procedure SetConsole(value: Boolean);

    function GetOnPause: TPaxPauseNotifyEvent;
    procedure SetOnPause(value: TPaxPauseNotifyEvent);

    function GetOnPauseUpdated: TPaxPauseNotifyEvent;
    procedure SetOnPauseUpdated(value: TPaxPauseNotifyEvent);

    function GetOnHalt: TPaxHaltNotifyEvent;
    procedure SetOnHalt(value: TPaxHaltNotifyEvent);

    function GetOnException: TPaxErrNotifyEvent;
    procedure SetOnException(value: TPaxErrNotifyEvent);

    function GetOnUnhandledException: TPaxErrNotifyEvent;
    procedure SetOnUnhandledException(value: TPaxErrNotifyEvent);

    function GetOnLoadProc: TPaxLoadProcEvent;
    procedure SetOnLoadProc(value: TPaxLoadProcEvent);

    function GetOnBeforeCallHost: TPaxIdNotifyEvent;
    procedure SetOnBeforeCallHost(value: TPaxIdNotifyEvent);

    function GetOnAfterCallHost: TPaxIdNotifyEvent;
    procedure SetOnAfterCallHost(value: TPaxIdNotifyEvent);

    function GetOnCreateObject: TPaxObjectNotifyEvent;
    procedure SetOnCreateObject(value: TPaxObjectNotifyEvent);

    function GetOnAfterObjectCreation: TPaxObjectNotifyEvent;
    procedure SetOnAfterObjectCreation(value: TPaxObjectNotifyEvent);

    function GetOnAfterObjectDestruction: TPaxClassNotifyEvent;
    procedure SetOnAfterObjectDestruction(value: TPaxClassNotifyEvent);

    function GetOnDestroyObject: TPaxObjectNotifyEvent;
    procedure SetOnDestroyObject(value: TPaxObjectNotifyEvent);

    function GetOnCreateHostObject: TPaxObjectNotifyEvent;
    procedure SetOnCreateHostObject(value: TPaxObjectNotifyEvent);

    function GetOnDestroyHostObject: TPaxObjectNotifyEvent;
    procedure SetOnDestroyHostObject(value: TPaxObjectNotifyEvent);

    function GetOnMapTableNamespace: TPaxMapTableNamespaceEvent;
    procedure SetOnMapTableNamespace(value: TPaxMapTableNamespaceEvent);

    function GetOnMapTableVarAddress: TPaxMapTableVarAddressEvent;
    procedure SetOnMapTableVarAddress(value: TPaxMapTableVarAddressEvent);

    function GetOnMapTableProcAddress: TPaxMapTableProcAddressEvent;
    procedure SetOnMapTableProcAddress(value: TPaxMapTableProcAddressEvent);

    function GetOnMapTableClassRef: TPaxMapTableClassRefEvent;
    procedure SetOnMapTableClassRef(value: TPaxMapTableClassRefEvent);

    function GetOnPrint: TPaxPrintEvent;
    procedure SetOnPrint(value: TPaxPrintEvent);

    function GetOnPrintEx: TPaxPrintExEvent;
    procedure SetOnPrintEx(value: TPaxPrintExEvent);

    function GetCustomExceptionHelper: TPaxCustomExceptionHelperEvent;
    procedure SetCustomExceptionHelper(value: TPaxCustomExceptionHelperEvent);

    function GetOnLoadPCU: TPaxRunnerLoadPCUEvent;
    procedure SetOnLoadPCU(value: TPaxRunnerLoadPCUEvent);

    function GetOnStreamSave: TPaxStreamEvent;
    procedure SetOnStreamSave(value: TPaxStreamEvent);

    function GetOnStreamLoad: TPaxStreamEvent;
    procedure SetOnStreamLoad(value: TPaxStreamEvent);

    function GetOnBeginProcNotify: TPaxProcNotifyEvent;
    procedure SetOnBeginProcNotify(value: TPaxProcNotifyEvent);

    function GetOnEndProcNotify: TPaxProcNotifyEvent;
    procedure SetOnEndProcNotify(value: TPaxProcNotifyEvent);

    function GetOnVirtualObjectMethodCall: TPaxVirtualObjectMethodCallEvent;
    procedure SetOnVirtualObjectMethodCall(value: TPaxVirtualObjectMethodCallEvent);

    function GetOnVirtualObjectPutProperty: TPaxVirtualObjectPutPropertyEvent;
    procedure SetOnVirtualObjectPutProperty(value: TPaxVirtualObjectPutPropertyEvent);

    function GetExitCode: Integer;
    function GetIsEvent: Boolean;
    procedure SetSuspendFinalization(value: Boolean);
    function GetSuspendFinalization: Boolean;

    function GetPausedPCU: TBaseRunner;
    procedure SetPausedPCU(value: TBaseRunner);

    function GetPrintClassTypeField: TPaxPrintClassTypeFieldEvent;
    procedure SetPrintClassTypeField(value: TPaxPrintClassTypeFieldEvent);

    function GetPrintClassTypeProp: TPaxPrintClassTypePropEvent;
    procedure SetPrintClassTypeProp(value: TPaxPrintClassTypePropEvent);

    function GetPCUUnit(I: Integer): TBaseRunner;
  protected
    function GetRunnerClass: TBaseRunnerClass; virtual;
  public
    EmitProc: TEmitProc;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
    function GetProgPtr: TBaseRunner;
    procedure RunInitialization;
    procedure RunFinalization;
    procedure Pause;
    function IsPaused: Boolean;
    function IsRunning: Boolean;
    procedure Resume;
    procedure RegisterClass(C: TClass; const FullName: String = '');
    procedure SaveToBuff(var Buff);
    procedure LoadFromBuff(var Buff);
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure SaveToFile(const Path: String);
    procedure LoadFromFile(const Path: String);
    function GetAddress(Handle: Integer): Pointer; overload;
    function GetAddress(const FullName: String): Pointer; overload;
    function GetAddressEx(const FullName: String; OverCount: Integer): Pointer; overload;
    function GetAddressEx(const FullName: String): Pointer; overload;
    function GetAddress(const FullName: String; OverCount: Integer): Pointer; overload;
    procedure SetAddress(Handle: Integer; P: Pointer);
    function SetHostAddress(const FullName: String; Address: Pointer): Boolean;
    function GetFieldAddress(X: TObject; const FieldName: String): Pointer;
    function GetCallConv(const FullName: String): Integer;
    function GetRetSize(const FullName: String): Integer;
{$IFDEF PAXARM}
{$ELSE}
    procedure SetEntryPoint(EntryPoint: TPaxInvoke);
    procedure ResetEntryPoint(EntryPoint: TPaxInvoke);
{$ENDIF}
    procedure CreateGlobalJSObjects;
    procedure DiscardPause;
    function GetImageSize: Integer;
    procedure DiscardDebugMode;
    procedure AssignEventHandlerRunner(MethodAddress: Pointer;
                                       Instance: TObject);
    function RegisterNamespace(LevelId: Integer; const Name: String): Integer;
    function RegisterClassType(LevelId: Integer; C: TClass): Integer;
    procedure RegisterMember(LevelId: Integer; const Name: String;
                         Address: Pointer);
    procedure MapGlobal;
    procedure MapLocal;
    function CreateScriptObject(const ScriptClassName: String;
                                const ParamList: array of const): TObject;
    procedure DestroyScriptObject(X: TObject);
    procedure LoadDFMFile(Instance: TObject; const FileName: String);
    procedure LoadDFMStream(Instance: TObject; S: TStream);
    function GetTypeInfo(const FullTypeName: String): PTypeInfo;
    function CallRoutine(const FullName: String;
                         const ParamList: array of OleVariant): OleVariant;
    function CallMethod(const FullName: String;
                        Instance: TObject;
                        const ParamList: array of OleVariant): OleVariant;
    function CallClassMethod(const FullName: String;
                             Instance: TClass;
                             const ParamList: array of OleVariant): OleVariant;
    procedure UnloadPCU(const FullPath: String);
    procedure LoadPCU(const FileName: String);
    function GetExceptionRecord: Pointer;
    function AddBreakpoint(const ModuleName: String;
                           SourceLineNumber: Integer): Boolean;
    function AddTempBreakpoint(const ModuleName: String;
                               SourceLineNumber: Integer): Boolean;
    function RemoveBreakpoint(const ModuleName: String;
                              SourceLineNumber: Integer): Boolean; overload;
    function RemoveBreakpoint(const ModuleName: String): Boolean; overload;
    function HasBreakpoint(const ModuleName: String;
                           SourceLineNumber: Integer): Boolean;
    function IsExecutableLine(const ModuleName: String;
                              SourceLineNumber: Integer): Boolean;
    procedure RemoveAllBreakpoints;
    function GetCurrentFunctionFullName: String;
    procedure GetCurrentParams(result: TStrings);
    procedure GetCurrentLocalVars(result: TStrings);
    function HasPCU(const ModuleName: String): Boolean;

    property DataPtr: Pointer read GetDataPtr;
    property CodePtr: Pointer read GetCodePtr;
    property DataSize: Integer read GetDataSize;
    property CodeSize: Integer read GetCodeSize;
    property ProgramSize: Integer read GetProgramSize;
    property ImageSize: Integer read GetImageSize;
    property ResultPtr: Pointer read GetResultPtr;
    property ExitCode: Integer read GetExitCode;
    property IsEvent: Boolean read GetIsEvent;
    property SourceLine: Integer read GetSourceLine;
    property ModuleName: String read GetModuleName;
    property SuspendFinalization: Boolean
      read GetSuspendFinalization write SetSuspendFinalization;
    property RunMode: Integer read GetRunMode write SetRunMode;
    property PCUCount: Integer read GetPCUCount;
    property PausedPCU: TBaseRunner read GetPausedPCU write SetPausedPCU;
    property PCUUnits[I: Integer]: TBaseRunner read GetPCUUnit;
    property SearchPathList: TStringList read GetSearchPathList;
  published

    property Console: boolean read GetConsole write SetConsole;

    property OnPause: TPaxPauseNotifyEvent read GetOnPause write SetOnPause;
    property OnPauseUpdated: TPaxPauseNotifyEvent read GetOnPauseUpdated write SetOnPauseUpdated;
    property OnHalt: TPaxHaltNotifyEvent read GetOnHalt write SetOnHalt;
    property OnException: TPaxErrNotifyEvent read GetOnException write SetOnException;
    property OnUnhandledException: TPaxErrNotifyEvent read GetOnUnhandledException
                                                      write SetOnUnhandledException;
    property OnLoadProc: TPaxLoadProcEvent read GetOnLoadProc
                                           write SetOnLoadProc;

    property OnCreateObject: TPaxObjectNotifyEvent read GetOnCreateObject
                                                   write SetOnCreateObject;

    property OnAfterObjectCreation: TPaxObjectNotifyEvent read GetOnAfterObjectCreation
                                                   write SetOnAfterObjectCreation;

    property OnDestroyObject: TPaxObjectNotifyEvent read GetOnDestroyObject
                                                   write SetOnDestroyObject;

    property OnCreateHostObject: TPaxObjectNotifyEvent read GetOnCreateHostObject
                                                   write SetOnCreateHostObject;
    property OnDestroyHostObject: TPaxObjectNotifyEvent read GetOnDestroyHostObject
                                                   write SetOnDestroyHostObject;

    property OnAfterObjectDestruction: TPaxClassNotifyEvent read GetOnAfterObjectDestruction
                                                   write SetOnAfterObjectDestruction;

    property OnMapTableNamespace: TPaxMapTableNamespaceEvent read GetOnMapTableNamespace
                                           write SetOnMapTableNamespace;
    property OnMapTableVarAddress: TPaxMapTableVarAddressEvent read GetOnMapTableVarAddress
                                           write SetOnMapTableVarAddress;
    property OnMapTableProcAddress: TPaxMapTableProcAddressEvent read GetOnMapTableProcAddress
                                           write SetOnMapTableProcAddress;
    property OnMapTableClassRef: TPaxMapTableClassRefEvent read GetOnMapTableClassRef
                                           write SetOnMapTableClassRef;

    property OnPrintEvent: TPaxPrintEvent read GetOnPrint
                                           write SetOnPrint;
    property OnPrintEx: TPaxPrintExEvent read GetOnPrintEx
                                         write SetOnPrintEx;
    property OnPrintClassTypeField: TPaxPrintClassTypeFieldEvent
      read GetPrintClassTypeField write SetPrintClassTypeField;

    property OnPrintClassTypeProp: TPaxPrintClassTypePropEvent
      read GetPrintClassTypeProp write SetPrintClassTypeProp;

    property OnCustomExceptionHelperEvent: TPaxCustomExceptionHelperEvent
                             read GetCustomExceptionHelper
                             write SetCustomExceptionHelper;

    property OnLoadPCU: TPaxRunnerLoadPCUEvent
                         read GetOnLoadPCU write SetOnLoadPCU;

    property OnSaveToStream: TPaxStreamEvent
                              read GetOnStreamSave write SetOnStreamSave;
    property OnLoadFromStream: TPaxStreamEvent
                              read GetOnStreamLoad write SetOnStreamLoad;
    property OnBeginProc: TPaxProcNotifyEvent
                              read GetOnBeginProcNotify write SetOnBeginProcNotify;
    property OnEndProc: TPaxProcNotifyEvent
                              read GetOnEndProcNotify write SetOnEndProcNotify;
    property OnVirtualObjectMethodCall: TPaxVirtualObjectMethodCallEvent
      read GetOnVirtualObjectMethodCall write SetOnVirtualObjectMethodCall;
    property OnVirtualObjectPutProperty: TPaxVirtualObjectPutPropertyEvent
      read GetOnVirtualObjectPutProperty write SetOnVirtualObjectPutProperty;

    property OnBeforeCallHost: TPaxIdNotifyEvent read GetOnBeforeCallHost
                                                   write SetOnBeforeCallHost;

    property OnAfterCallHost: TPaxIdNotifyEvent read GetOnAfterCallHost
                                                   write SetOnAfterCallHost;
  end;

  TPaxRunnerClass = class of TPaxRunner;

function ScalarValueToString(Address: Pointer; T: Integer): String;

implementation

// TPaxRunner -----------------------------------------------------------------

constructor TPaxRunner.Create(AOwner: TComponent);
begin
  inherited;
  prog := GetRunnerClass.Create;
  prog.Owner := Self;
end;

destructor TPaxRunner.Destroy;
begin
  FreeAndNil(prog);
  inherited;
end;

function TPaxRunner.GetRunnerClass: TBaseRunnerClass;
begin
  result := nil;
end;

procedure TPaxRunner.Run;
begin
  prog.RunExtended;
end;

procedure TPaxRunner.RunInitialization;
begin
  prog.RunInitialization;
end;

procedure TPaxRunner.RunFinalization;
begin
  prog.RunFinalization;
end;

procedure TPaxRunner.Pause;
begin
  prog.Pause;
end;

function TPaxRunner.IsPaused: Boolean;
begin
  result := prog.IsPaused;
end;

function TPaxRunner.IsRunning: Boolean;
begin
  result := prog.IsRunning;
end;

procedure TPaxRunner.Resume;
begin
  prog.Run;
end;

procedure TPaxRunner.DiscardPause;
begin
  prog.DiscardPause;
end;

procedure TPaxRunner.SaveToBuff(var Buff);
begin
  prog.SaveToBuff(Buff);
end;

procedure TPaxRunner.LoadFromBuff(var Buff);
begin
  prog.LoadFromBuff(Buff);
end;

procedure TPaxRunner.SaveToStream(S: TStream);
begin
  prog.SaveToStream(S);
end;

procedure TPaxRunner.LoadFromStream(S: TStream);
begin
  prog.LoadFromStream(S);
end;

procedure TPaxRunner.SaveToFile(const Path: String);
begin
  prog.SaveToFile(path);
end;

procedure TPaxRunner.LoadFromFile(const Path: String);
begin
  prog.LoadFromFile(path);
end;

function TPaxRunner.GetAddress(Handle: Integer): Pointer;
var
  MR: TMapRec;
begin
  result := prog.GetAddress(Handle);
  if not NativeAddress(result) then
  begin
    MR := prog.ScriptMapTable.LookupByOffset(-Handle);
    if MR = nil then
      result := nil
    else if MR.IsMethod then
      prog.WrapMethodAddress(result)
    else if MR.Kind = KindSUB then
      prog.WrapGlobalAddress(result);
  end;
end;

function TPaxRunner.GetAddress(const FullName: String): Pointer;
var
  MR: TMapRec;
  S, MethName: String;
  I: Integer;
  C: TClass;
begin
  result := prog.GetAddress(FullName, MR);
  if result <> nil then
  begin
    if MR.IsMethod then
      prog.WrapMethodAddress(result)
    else if MR.Kind = KindSUB then
      prog.WrapGlobalAddress(result);
    Exit;
  end;
  S := ExtractClassName(FullName);
  if S = '' then
    Exit;
  I := prog.ClassList.IndexOf(S);
  if I = -1 then
    Exit;
  C := prog.ClassList[I].PClass.ClassParent;
  if C = nil then
    Exit;
  MethName := ExtractName(FullName);
  result := GetAddress(C.ClassName + '.' + MethName);
  prog.WrapMethodAddress(result);
end;

function TPaxRunner.GetAddress(const FullName: String; OverCount: Integer): Pointer;
var
  MR: TMapRec;
  S, MethName: String;
  I: Integer;
  C: TClass;
begin
  result := prog.GetAddressEx(FullName, OverCount, MR);
  if result <> nil then
  begin
    if MR.IsMethod then
      prog.WrapMethodAddress(result)
    else if MR.Kind = KindSUB then
      prog.WrapGlobalAddress(result);
    Exit;
  end;
  S := ExtractClassName(FullName);
  if S = '' then
    Exit;
  I := prog.ClassList.IndexOf(S);
  if I = -1 then
    Exit;
  C := prog.ClassList[I].PClass.ClassParent;
  if C = nil then
    Exit;
  MethName := ExtractName(FullName);
  result := GetAddress(C.ClassName + '.' + MethName, OverCount);
  prog.WrapMethodAddress(result);
end;

function TPaxRunner.GetAddressEx(const FullName: String): Pointer;
var
  MR: TMapRec;
begin
  result := prog.GetAddressExtended(FullName, MR);
  if MR = nil then
    Exit;
  if MR.IsMethod then
    prog.WrapMethodAddress(result)
  else if MR.Kind = KindSUB then
    prog.WrapGlobalAddress(result);
end;

function TPaxRunner.GetAddressEx(const FullName: String; OverCount: Integer): Pointer;
var
  MR: TMapRec;
begin
  result := prog.GetAddressExtended(FullName, OverCount, MR);
  if MR = nil then
    Exit;
  if MR.IsMethod then
    prog.WrapMethodAddress(result)
  else if MR.Kind = KindSUB then
    prog.WrapGlobalAddress(result);
end;

function TPaxRunner.GetCallConv(const FullName: String): Integer;
begin
  result := prog.GetCallConv(FullName);
end;

function TPaxRunner.GetRetSize(const FullName: String): Integer;
begin
  result := prog.GetRetSize(FullName);
end;

procedure TPaxRunner.SetAddress(Handle: Integer; P: Pointer);
var
  Offset: Integer;
begin
  Offset := prog.GetOffset(Handle);
  if Offset = -1 then
    Exit;

  prog.SetAddress(Offset, P);
end;

function TPaxRunner.SetHostAddress(const FullName: String; Address: Pointer): Boolean;
begin
  result := prog.SetHostAddress(FullName, Address);
end;

function TPaxRunner.GetResultPtr: Pointer;
begin
  result := prog.ResultPtr;
end;

function TPaxRunner.GetDataPtr: Pointer;
begin
  result := prog.DataPtr;
end;

function TPaxRunner.GetCodePtr: Pointer;
begin
  result := prog.CodePtr;
end;

function TPaxRunner.GetDataSize: Integer;
begin
  result := prog.DataSize;
end;

function TPaxRunner.GetCodeSize: Integer;
begin
  result := prog.CodeSize;
end;

function TPaxRunner.GetProgramSize: Integer;
begin
  result := prog.ProgramSize;
end;

procedure TPaxRunner.RegisterClass(C: TClass; const FullName: String = '');
begin
  if FullName = '' then
    prog.RegisterClass(C, C.ClassName)
  else
    prog.RegisterClass(C, FullName);
end;

function TPaxRunner.GetProgPtr: TBaseRunner;
begin
  result := prog;
end;

function TPaxRunner.GetOnPause: TPaxPauseNotifyEvent;
begin
  result := TPaxPauseNotifyEvent(prog.OnPause);
end;

procedure TPaxRunner.SetOnPause(value: TPaxPauseNotifyEvent);
begin
  prog.OnPause := TPauseNotifyEvent(value);
end;

function TPaxRunner.GetOnPauseUpdated: TPaxPauseNotifyEvent;
begin
  result := TPaxPauseNotifyEvent(prog.OnPauseUpdated);
end;

procedure TPaxRunner.SetOnPauseUpdated(value: TPaxPauseNotifyEvent);
begin
  prog.OnPauseUpdated := TPauseNotifyEvent(value);
end;

function TPaxRunner.GetOnBeforeCallHost: TPaxIdNotifyEvent;
begin
  result := TPaxIdNotifyEvent(prog.OnBeforeCallHost);
end;

procedure TPaxRunner.SetOnBeforeCallHost(value: TPaxIdNotifyEvent);
begin
  prog.OnBeforeCallHost := TIdNotifyEvent(value);
end;

function TPaxRunner.GetOnAfterCallHost: TPaxIdNotifyEvent;
begin
  result := TPaxIdNotifyEvent(prog.OnAfterCallHost);
end;

procedure TPaxRunner.SetOnAfterCallHost(value: TPaxIdNotifyEvent);
begin
  prog.OnAfterCallHost := TIdNotifyEvent(value);
end;

function TPaxRunner.GetOnCreateObject: TPaxObjectNotifyEvent;
begin
  result := TPaxObjectNotifyEvent(prog.OnCreateObject);
end;

procedure TPaxRunner.SetOnCreateObject(value: TPaxObjectNotifyEvent);
begin
  prog.OnCreateObject := TObjectNotifyEvent(value);
end;

function TPaxRunner.GetOnAfterObjectCreation: TPaxObjectNotifyEvent;
begin
  result := TPaxObjectNotifyEvent(prog.OnAfterObjectCreation);
end;

procedure TPaxRunner.SetOnAfterObjectCreation(value: TPaxObjectNotifyEvent);
begin
  prog.OnAfterObjectCreation := TObjectNotifyEvent(value);
end;

function TPaxRunner.GetOnAfterObjectDestruction: TPaxClassNotifyEvent;
begin
  result := TPaxClassNotifyEvent(prog.OnAfterObjectDestruction);
end;

procedure TPaxRunner.SetOnAfterObjectDestruction(value: TPaxClassNotifyEvent);
begin
  prog.OnAfterObjectDestruction := TClassNotifyEvent(value);
end;

function TPaxRunner.GetOnDestroyObject: TPaxObjectNotifyEvent;
begin
  result := TPaxObjectNotifyEvent(prog.OnDestroyObject);
end;

procedure TPaxRunner.SetOnDestroyObject(value: TPaxObjectNotifyEvent);
begin
  prog.OnDestroyObject := TObjectNotifyEvent(value);
end;

function TPaxRunner.GetOnCreateHostObject: TPaxObjectNotifyEvent;
begin
  result := TPaxObjectNotifyEvent(prog.OnCreateHostObject);
end;

procedure TPaxRunner.SetOnCreateHostObject(value: TPaxObjectNotifyEvent);
begin
  prog.OnCreateHostObject := TObjectNotifyEvent(value);
end;

function TPaxRunner.GetOnDestroyHostObject: TPaxObjectNotifyEvent;
begin
  result := TPaxObjectNotifyEvent(prog.OnDestroyHostObject);
end;

procedure TPaxRunner.SetOnDestroyHostObject(value: TPaxObjectNotifyEvent);
begin
  prog.OnDestroyHostObject := TObjectNotifyEvent(value);
end;

function TPaxRunner.GetOnHalt: TPaxHaltNotifyEvent;
begin
  result := TPaxHaltNotifyEvent(prog.OnHalt);
end;

procedure TPaxRunner.SetOnHalt(value: TPaxHaltNotifyEvent);
begin
  prog.OnHalt := THaltNotifyEvent(value);
end;

function TPaxRunner.GetOnLoadProc: TPaxLoadProcEvent;
begin
  result := TPaxLoadProcEvent(prog.OnLoadProc);
end;

procedure TPaxRunner.SetOnLoadProc(value: TPaxLoadProcEvent);
begin
  prog.OnLoadProc := TLoadProcEvent(value);
end;

function TPaxRunner.GetOnPrint: TPaxPrintEvent;
begin
  result := TPaxPrintEvent(prog.OnPrint);
end;

procedure TPaxRunner.SetOnPrint(value: TPaxPrintEvent);
begin
  prog.OnPrint := TPrintEvent(value);
end;

function TPaxRunner.GetOnPrintEx: TPaxPrintExEvent;
begin
  result := TPaxPrintExEvent(prog.OnPrintEx);
end;

procedure TPaxRunner.SetOnPrintEx(value: TPaxPrintExEvent);
begin
  prog.OnPrintEx := TPrintExEvent(value);
end;

function TPaxRunner.GetCustomExceptionHelper: TPaxCustomExceptionHelperEvent;
begin
  result := TPaxCustomExceptionHelperEvent(prog.OnCustomExceptionHelper);
end;

procedure TPaxRunner.SetCustomExceptionHelper(value: TPaxCustomExceptionHelperEvent);
begin
  prog.OnCustomExceptionHelper := TCustomExceptionHelperEvent(value);
end;

function TPaxRunner.GetOnMapTableNamespace: TPaxMapTableNamespaceEvent;
begin
  result := TPaxMapTableNamespaceEvent(prog.OnMapTableNamespace);
end;

procedure TPaxRunner.SetOnMapTableNamespace(value: TPaxMapTableNamespaceEvent);
begin
  prog.OnMapTableNamespace := TMapTableNamespaceEvent(value);
end;

function TPaxRunner.GetOnMapTableVarAddress: TPaxMapTableVarAddressEvent;
begin
  result := TPaxMapTableVarAddressEvent(prog.OnMapTableVarAddress);
end;

procedure TPaxRunner.SetOnMapTableVarAddress(value: TPaxMapTableVarAddressEvent);
begin
  prog.OnMapTableVarAddress := TMapTableVarAddressEvent(value);
end;

function TPaxRunner.GetOnMapTableProcAddress: TPaxMapTableProcAddressEvent;
begin
  result := TPaxMapTableProcAddressEvent(prog.OnMapTableProcAddress);
end;

procedure TPaxRunner.SetOnMapTableProcAddress(value: TPaxMapTableProcAddressEvent);
begin
  prog.OnMapTableProcAddress := TMapTableProcAddressEvent(value);
end;

function TPaxRunner.GetOnMapTableClassRef: TPaxMapTableClassRefEvent;
begin
  result := TPaxMapTableClassRefEvent(prog.OnMapTableClassRef);
end;

procedure TPaxRunner.SetOnMapTableClassRef(value: TPaxMapTableClassRefEvent);
begin
  prog.OnMapTableClassRef := TMapTableClassRefEvent(value);
end;

function TPaxRunner.GetOnException: TPaxErrNotifyEvent;
begin
  result := TPaxErrNotifyEvent(prog.OnException);
end;

procedure TPaxRunner.SetOnException(value: TPaxErrNotifyEvent);
begin
  prog.OnException := TErrNotifyEvent(value);
end;

function TPaxRunner.GetOnUnhandledException: TPaxErrNotifyEvent;
begin
  result := TPaxErrNotifyEvent(prog.OnUnhandledException);
end;

procedure TPaxRunner.SetOnUnhandledException(value: TPaxErrNotifyEvent);
begin
  prog.OnUnhandledException := TErrNotifyEvent(value);
end;

// added in v1.6

{$IFDEF PAXARM}
{$ELSE}
procedure TPaxRunner.SetEntryPoint(EntryPoint: TPaxInvoke);
begin
  prog.SetEntryPoint(EntryPoint);
end;

procedure TPaxRunner.ResetEntryPoint(EntryPoint: TPaxInvoke);
begin
  prog.ResetEntryPoint(EntryPoint);
end;
{$ENDIF}

procedure TPaxRunner.CreateGlobalJSObjects;
begin
  if Assigned(CrtJSObjects) then
    CrtJSObjects(prog, prog.JS_Record);
end;

function TPaxRunner.GetImageSize: Integer;
begin
  result := prog.GetImageSize;
end;

function TPaxRunner.CreateScriptObject(const ScriptClassName: String;
                                        const ParamList: array of const): TObject;
begin
  result := prog.CreateScriptObject(ScriptClassName, ParamList);
end;

procedure TPaxRunner.DestroyScriptObject(X: TObject);
begin
  prog.DestroyScriptObject(X);
end;

function TPaxRunner.GetExitCode: Integer;
begin
  result := prog.ExitCode;
end;

procedure TPaxRunner.RegisterMember(LevelId: Integer; const Name: String;
                         Address: Pointer);
begin
  prog.RegisterMember(LevelId, Name, Address);
end;

function TPaxRunner.RegisterNamespace(LevelId: Integer; const Name: String): Integer;
begin
  result := prog.RegisterNamespace(LevelId, Name);
end;

function TPaxRunner.RegisterClassType(LevelId: Integer; C: TClass): Integer;
begin
  result := prog.RegisterClassType(LevelId, C);
end;

procedure TPaxRunner.MapGlobal;
begin
  prog.MapGlobal;
end;

procedure TPaxRunner.MapLocal;
begin
  prog.MapLocal;
end;

function TPaxRunner.GetFieldAddress(X: TObject; const FieldName: String): Pointer;
begin
  result := prog.GetFieldAddress(X, FieldName);
end;

procedure TPaxRunner.DiscardDebugMode;
begin
  prog.DiscardDebugMode;
end;

procedure TPaxRunner.AssignEventHandlerRunner(MethodAddress: Pointer;
                                               Instance: TObject);
begin
  prog.AssignEventHandlerRunner(MethodAddress, Instance);
end;

function TPaxRunner.GetIsEvent: Boolean;
begin
  result := prog.RootIsEvent;
end;

procedure TPaxRunner.LoadDFMFile(Instance: TObject; const FileName: String);
begin
  prog.LoadDFMFile(Instance, FileName);
end;

procedure TPaxRunner.LoadDFMStream(Instance: TObject; S: TStream);
begin
  prog.LoadDFMStream(Instance, S);
end;

function TPaxRunner.GetTypeInfo(const FullTypeName: String): PTypeInfo;
begin
  result := prog.GetTypeInfo(FullTypeName);
end;

function TPaxRunner.CallRoutine(const FullName: String;
                     const ParamList: array of OleVariant): OleVariant;
begin
  result := prog.CallFunc(FullName, nil, ParamList);
end;

function TPaxRunner.CallMethod(const FullName: String;
                                Instance: TObject;
                                const ParamList: array of OleVariant): OleVariant;
begin
  result := prog.CallFunc(FullName, Instance, ParamList);
end;

function TPaxRunner.CallClassMethod(const FullName: String;
                                     Instance: TClass;
                                     const ParamList: array of OleVariant): OleVariant;
begin
  result := prog.CallFunc(FullName, Instance, ParamList);
end;

function TPaxRunner.GetSourceLine: Integer;
begin
  result := prog.GetSourceLine;
end;

function TPaxRunner.GetModuleName: String;
begin
  result := prog.GetModuleName;
end;

function TPaxRunner.GetOnLoadPCU: TPaxRunnerLoadPCUEvent;
begin
  result := TPaxRunnerLoadPCUEvent(prog.OnLoadPCU);
end;

procedure TPaxRunner.SetOnLoadPCU(value: TPaxRunnerLoadPCUEvent);
begin
  prog.OnLoadPCU := TLoadPCUEvent(value);
end;

procedure TPaxRunner.SetSuspendFinalization(value: Boolean);
begin
  prog.SuspendFinalization := value;
end;

function TPaxRunner.GetOnStreamSave: TPaxStreamEvent;
begin
  result := TPaxStreamEvent(prog.OnSaveToStream);
end;

procedure TPaxRunner.SetOnStreamSave(value: TPaxStreamEvent);
begin
  prog.OnSaveToStream := TStreamEvent(value);
end;

function TPaxRunner.GetOnStreamLoad: TPaxStreamEvent;
begin
  result := TPaxStreamEvent(prog.OnLoadFromStream);
end;

procedure TPaxRunner.SetOnStreamLoad(value: TPaxStreamEvent);
begin
  prog.OnLoadFromStream := TStreamEvent(value);
end;

function TPaxRunner.GetSuspendFinalization: Boolean;
begin
  result := prog.SuspendFinalization;
end;

procedure TPaxRunner.UnloadPCU(const FullPath: String);
begin
  prog.UnloadPCU(FullPath);
end;

procedure TPaxRunner.LoadPCU(const FileName: String);
var
  DestProg: Pointer;
begin
  prog.LoadPCU(FileName, DestProg);
end;

function TPaxRunner.GetExceptionRecord: Pointer;
begin
  result := prog.ExceptionRec;
end;

function TPaxRunner.GetConsole: Boolean;
begin
  result := prog.Console;
end;

procedure TPaxRunner.SetConsole(value: Boolean);
begin
  prog.Console := value;
end;

function TPaxRunner.GetRunMode: Integer;
begin
  result := prog.RunMode;
end;

procedure TPaxRunner.SetRunMode(value: Integer);
begin
  if (value < 0) or (value > _rmRUN_TO_CURSOR) then
    prog.RaiseError(errIncorrectValue, []);
  prog.RunMode := value;
end;

function TPaxRunner.AddBreakpoint(const ModuleName: String;
                                   SourceLineNumber: Integer): Boolean;
var
  B: TBreakpoint;
begin
  B := prog.AddBreakpoint(ModuleName, SourceLineNumber);
  result := B <> nil;
end;

function TPaxRunner.AddTempBreakpoint(const ModuleName: String;
                                       SourceLineNumber: Integer): Boolean;
var
  B: TBreakpoint;
begin
  B := prog.AddTempBreakpoint(ModuleName, SourceLineNumber);
  result := B <> nil;
end;

function TPaxRunner.RemoveBreakpoint(const ModuleName: String;
                                      SourceLineNumber: Integer): Boolean;
begin
  result := prog.RemoveBreakpoint(ModuleName, SourceLineNumber);
end;

function TPaxRunner.RemoveBreakpoint(const ModuleName: String): Boolean;
begin
  result := prog.RemoveBreakpoint(ModuleName);
end;

procedure TPaxRunner.RemoveAllBreakpoints;
begin
  prog.RemoveAllBreakpoints;
end;

function TPaxRunner.HasBreakpoint(const ModuleName: String;
                                   SourceLineNumber: Integer): Boolean;
begin
  result := prog.HasBreakpoint(ModuleName, SourceLineNumber);
end;

function TPaxRunner.IsExecutableLine(const ModuleName: String;
                                      SourceLineNumber: Integer): Boolean;
begin
  result := prog.IsExecutableLine(ModuleName, SourceLineNumber);
end;

function TPaxRunner.GetPCUCount: Integer;
begin
  result := prog.ProgList.Count;
end;

function TPaxRunner.GetOnBeginProcNotify: TPaxProcNotifyEvent;
begin
  result := TPaxProcNotifyEvent(prog.OnBeginProcNotifyEvent);
end;

procedure TPaxRunner.SetOnBeginProcNotify(value: TPaxProcNotifyEvent);
begin
  prog.OnBeginProcNotifyEvent := TProcNotifyEvent(value);
end;

function TPaxRunner.GetOnEndProcNotify: TPaxProcNotifyEvent;
begin
  result := TPaxProcNotifyEvent(prog.OnEndProcNotifyEvent);
end;

procedure TPaxRunner.SetOnEndProcNotify(value: TPaxProcNotifyEvent);
begin
  prog.OnEndProcNotifyEvent := TProcNotifyEvent(value);
end;

function TPaxRunner.GetOnVirtualObjectMethodCall: TPaxVirtualObjectMethodCallEvent;
begin
  result := TPaxVirtualObjectMethodCallEvent(prog.OnVirtualObjectMethodCall);
end;

procedure TPaxRunner.SetOnVirtualObjectMethodCall(value: TPaxVirtualObjectMethodCallEvent);
begin
  prog.OnVirtualObjectMethodCall := TVirtualObjectMethodCallEvent(value);
end;

function TPaxRunner.GetOnVirtualObjectPutProperty: TPaxVirtualObjectPutPropertyEvent;
begin
  result := TPaxVirtualObjectPutPropertyEvent(prog.OnVirtualObjectPutProperty);
end;

procedure TPaxRunner.SetOnVirtualObjectPutProperty(value: TPaxVirtualObjectPutPropertyEvent);
begin
  prog.OnVirtualObjectPutProperty := TVirtualObjectPutPropertyEvent(value);
end;

function TPaxRunner.GetPausedPCU: TBaseRunner;
begin
  result := prog.PausedPCU;
end;

procedure TPaxRunner.SetPausedPCU(value: TBaseRunner);
begin
  prog.PausedPCU := value;
end;

function TPaxRunner.GetPrintClassTypeField: TPaxPrintClassTypeFieldEvent;
begin
  result := TPaxPrintClassTypeFieldEvent(prog.OnPrintClassTypeField);
end;

procedure TPaxRunner.SetPrintClassTypeField(value: TPaxPrintClassTypeFieldEvent);
begin
  prog.OnPrintClassTypeField := TPrintClassTypeFieldEvent(value);
end;

function TPaxRunner.GetPrintClassTypeProp: TPaxPrintClassTypePropEvent;
begin
  result := TPaxPrintClassTypePropEvent(prog.OnPrintClassTypeProp);
end;

procedure TPaxRunner.SetPrintClassTypeProp(value: TPaxPrintClassTypePropEvent);
begin
  prog.OnPrintClassTypeProp := TPrintClassTypePropEvent(value);
end;

function TPaxRunner.GetCurrentFunctionFullName: String;
begin
  result := prog.GetCurrentFunctionFullName;
end;

procedure TPaxRunner.GetCurrentParams(result: TStrings);
begin
  prog.GetCurrentParams(result);
end;

procedure TPaxRunner.GetCurrentLocalVars(result: TStrings);
begin
  prog.GetCurrentLocalVars(result);
end;

function TPaxRunner.GetPCUUnit(I: Integer): TBaseRunner;
begin
  if I < PCUCount then
    result := TBaseRunner(prog.ProgList[I].Prog)
  else
    result := nil;
end;

function TPaxRunner.HasPCU(const ModuleName: String): Boolean;
var
  I: Integer;
  S: String;
begin
  result := false;
  for I := 0 to PCUCount - 1 do
  begin
    S := prog.ProgList[I].FullPath;
    S := ExtractFullOwner(S);
    if StrEql(S, ModuleName) then
    begin
      result := true;
      Exit;
    end;
  end;
end;

function ScalarValueToString(Address: Pointer; T: Integer): String;
begin
  result := PAXCOMP_SYS.ScalarValueToString(Address, T);
end;

function TPaxRunner.GetSearchPathList: TStringList;
begin
  result := prog.RootSearchPathList;
end;

end.
