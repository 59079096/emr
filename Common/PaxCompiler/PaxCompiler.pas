////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxCompiler.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxCompiler;

interface

uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_KERNEL,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_PARSER,
  PAXCOMP_PASCAL_PARSER,
  PAXCOMP_OLE,
  PAXCOMP_MODULE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_TYPEINFO,
  PAXCOMP_CLASSFACT,
  PAXCOMP_BASERUNNER,
  PaxRunner,
  PaxRegister;

const
  PLATFORM_Win32  = 1;
  PLATFORM_Win64  = 2;
  PLATFORM_OSX32  = 3;
  PLATFORM_IOSsim = 4;
  PLATFORM_IOSdev = 5;
  PLATFORM_ANDROID = 6;
  PLATFORM_LINUX32 = 7;

type
  TPaxCompiler = class;
  TPaxCompilerLanguage = class;

  TPaxCompilerNotifyEvent = procedure (Sender: TPaxCompiler) of object;
  TPaxCompilerUnitAliasEvent = procedure (Sender: TPaxCompiler;
                               var UnitName: String) of object;
  TPaxCompilerUsedUnitEvent = function (Sender: TPaxCompiler; const UnitName: String;
                              var SourceCode: String): Boolean of object;
  TPaxCompilerImportMemberEvent = procedure (Sender: TPaxCompiler;
                                  Id: Integer;
                                  const AFullName: String) of object;
  TPaxCompilerSavePCUEvent = procedure (Sender: TPaxCompiler; const UnitName: String;
                              var result: TStream) of object;
  TPaxCompilerLoadPCUEvent = procedure (Sender: TPaxCompiler; const UnitName: String;
                              var result: TStream) of object;
  TPaxCompilerSavePCUFinishedEvent = procedure (Sender: TPaxCompiler; const UnitName: String; var Stream : TStream) of object; // jason
  TPaxCompilerLoadPCUFinishedEvent = procedure(Sender: TPaxCompiler; const UnitName: String; var Stream : TStream) of object; // jason

  TPaxCompilerDirectiveEvent = procedure (Sender: TPaxCompiler;
                          const Directive: String; var ok: Boolean) of object;

  TPaxCompilerIncludeEvent = procedure (Sender: TPaxCompiler; const FileName: String;
                                var Text: String) of object;

  TPaxCompilerUndeclaredIdentifierEvent = function (Sender: TPaxCompiler;
                                             const IdentName: String;
                                             var Scope: String;
                                             var FullTypeName: String): boolean
                                             of object;

  TPaxCompilerCommentEvent = procedure (Sender: TPaxCompiler;
                                        const Comment: String;
                                        const Context: String;
                                        CommentedTokens: TStrings) of object;

  TPaxCompiler = class(TComponent)
  private
    kernel: TKernel;

    function GetTargetPlatform: Integer;
    procedure SetTargetPlatform(value: Integer);
    procedure CreateMapping(Runner: TBaseRunner);
    function GetErrorCount: Integer;
    function GetErrorMessage(I: Integer): String;
    function GetErrorModuleName(I: Integer): String;
    function GetErrorLine(I: Integer): String;
    function GetErrorLineNumber(I: Integer): Integer;
    function GetErrorLinePos(I: Integer): Integer;
    function GetErrorFileName(I: Integer): String;

    function GetWarningCount: Integer;
    function GetWarningMessage(I: Integer): String;
    function GetWarningModuleName(I: Integer): String;
    function GetWarningLine(I: Integer): String;
    function GetWarningLineNumber(I: Integer): Integer;
    function GetWarningLinePos(I: Integer): Integer;
    function GetWarningFileName(I: Integer): String;

    function GetOnCompilerProgress: TPaxCompilerNotifyEvent;
    procedure SetOnCompilerProgress(value: TPaxCompilerNotifyEvent);

    function GetOnUsedUnit: TPaxCompilerUsedUnitEvent;
    procedure SetOnUsedUnit(value: TPaxCompilerUsedUnitEvent);

    function GetOnImportUnit: TPaxCompilerImportMemberEvent;
    procedure SetOnImportUnit(value: TPaxCompilerImportMemberEvent);

    function GetOnImportType: TPaxCompilerImportMemberEvent;
    procedure SetOnImportType(value: TPaxCompilerImportMemberEvent);

    function GetOnImportGlobalMembers: TPaxCompilerNotifyEvent;
    procedure SetOnImportGlobalMembers(value: TPaxCompilerNotifyEvent);

    function GetOnUnitAlias: TPaxCompilerUnitAliasEvent;
    procedure SetOnUnitAlias(value: TPaxCompilerUnitAliasEvent);

    function GetOnSavePCU: TPaxCompilerSavePCUEvent;
    procedure SetOnSavePCU(value: TPaxCompilerSavePCUEvent);

    function GetOnLoadPCU: TPaxCompilerLoadPCUEvent;
    procedure SetOnLoadPCU(value: TPaxCompilerLoadPCUEvent);

    function GetOnInclude: TPaxCompilerIncludeEvent;
    procedure SetOnInclude(value: TPaxCompilerIncludeEvent);

    function GetOnDefineDirective: TPaxCompilerDirectiveEvent;
    procedure SetOnDefineDirective(value: TPaxCompilerDirectiveEvent);

    function GetOnUndefineDirective: TPaxCompilerDirectiveEvent;
    procedure SetOnUndefineDirective(value: TPaxCompilerDirectiveEvent);

    function GetOnUnknownDirective: TPaxCompilerDirectiveEvent;
    procedure SetOnUnknownDirective(value: TPaxCompilerDirectiveEvent);

    function GetOnUndeclaredIdentifier: TPaxCompilerUndeclaredIdentifierEvent;
    procedure SetOnUndeclaredIdentifier(value: TPaxCompilerUndeclaredIdentifierEvent);

    function GetOnComment: TPaxCompilerCommentEvent;
    procedure SetOnComment(value: TPaxCompilerCommentEvent);

    function GetDebugMode: Boolean;
    procedure SetDebugMode(value: Boolean);

    function GetCondDirectiveList: TStringList;

    function GetSourceModule(const ModuleName: String): TStringList;
    function GetCurrLineNumber: Integer;
    function GetCurrModuleNumber: Integer;
    function GetCurrModuleName: String;

    function GetAlignment: Integer;
    procedure SetAlignment(value: Integer);

    function GetCurrLanguage: String;
    procedure SetCurrLanguage(const value: String);

    function GetNativeSEH: Boolean;
    procedure SetNativeSEH(const value: Boolean);

    function GetOnSavePCUFinished: TPaxCompilerSavePCUFinishedEvent; // jason
    procedure SetOnSavePCUFinished(value: TPaxCompilerSavePCUFinishedEvent); // jason

    function GetOnLoadPCUFinished: TPaxCompilerLoadPCUFinishedEvent; // jason
    procedure SetOnLoadPCUFinished(value: TPaxCompilerLoadPCUFinishedEvent); // jason

    function GetCompletionPrefix: String;

    function GetUnicode: Boolean;
    procedure SetUnicode(value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    procedure ResetCompilation;
    procedure AddModule(const ModuleName, LanguageName: String);
    procedure AddCode(const ModuleName, Text: String);
    procedure AddCodeFromFile(const ModuleName, FileName: String);
    procedure RegisterLanguage(L: TPaxCompilerLanguage);
    procedure RegisterDirective(const Directive: string; const value: Variant);

    function RegisterNamespace(LevelId: Integer;
                               const NamespaceName: String): Integer;
    procedure RegisterUsingNamespace(const aNamespaceName: String); overload;
    procedure RegisterUsingNamespace(aNamespaceID: Integer); overload;
    procedure UnregisterUsingNamespace(aNamespaceID: Integer); overload;
    procedure UnregisterUsingNamespaces;
    procedure UnregisterUsingNamespace(const aNamespaceName: String); overload;
    function RegisterInterfaceType(LevelId: Integer;
                                   const TypeName: String;
                                   const GUID: TGUID): Integer; overload;
    function RegisterInterfaceType(LevelId: Integer;
                                   const TypeName: String;
                                   const GUID: TGUID;
                                   const ParentName: String;
                                   const ParentGUID: TGUID): Integer; overload;

    procedure RegisterSupportedInterface(TypeId: Integer;
                                         const SupportedInterfaceName: String;
                                         const GUID: TGUID);
    function RegisterClassType(LevelId: Integer;
                               const TypeName: String; AncestorId: Integer): Integer; overload;
    function RegisterClassType(LevelId: Integer;
                               C: TClass): Integer; overload;
    function RegisterClassReferenceType(LevelId: Integer;
                                const TypeName: String; OriginClassId: Integer): Integer;
    function RegisterClassHelperType(LevelId: Integer;
                                const TypeName: String; OriginClassId: Integer): Integer; overload;
    function RegisterClassHelperType(LevelID: Integer;
                                const TypeName, OriginalTypeName: String): Integer; overload;
    function RegisterRecordHelperType(LevelId: Integer;
                 const TypeName: String; OriginRecordId: Integer): Integer; overload;
    function RegisterRecordHelperType(LevelID: Integer;
                 const TypeName, OriginalTypeName: String): Integer; overload;
    function RegisterClassTypeField(TypeId: Integer; const FieldName: String;
                                    FieldTypeID: Integer; FieldShift: Integer = -1): Integer;
    function RegisterProperty(LevelId: Integer; const PropName: String;
                              PropTypeID, ReadId, WriteId: Integer;
                              IsDefault: Boolean): Integer; overload;
    function RegisterProperty(LevelId: Integer; const Header: String): Integer; overload;
    function RegisterInterfaceProperty(LevelId: Integer;
                                       const PropName: String;
                                       PropTypeID,
                                       ReadIndex,
                                       WriteIndex: Integer): Integer;
    function RegisterRecordType(LevelId: Integer;
                                const TypeName: String;
                                IsPacked: Boolean = false): Integer;
    function RegisterRecordTypeField(TypeId: Integer; const FieldName: String;
                                    FieldTypeID: Integer; FieldShift: Integer = -1): Integer;
    function RegisterVariantRecordTypeField(TypeId: Integer; const FieldName: String;
                                FieldTypeID: Integer;
                                VarCount: Int64): Integer; overload;
    function RegisterVariantRecordTypeField(LevelId: Integer; const Declaration: String;
                                VarCount: Int64): Integer; overload;
    function RegisterSubrangeType(LevelId: Integer;
                                  const TypeName: String;
                                  TypeBaseId: Integer;
                                  B1, B2: Integer): Integer;
    function RegisterEnumType(LevelId: Integer;
                              const TypeName: String;
                              TypeBaseId: Integer = _typeINTEGER): Integer;
    function RegisterEnumValue(EnumTypeId: Integer;
                               const FieldName: String;
                               const Value: Integer): Integer;
    function RegisterArrayType(LevelId: Integer;
                               const TypeName: String;
                               RangeTypeId, ElemTypeId: Integer;
                               IsPacked: Boolean = false): Integer;
    function RegisterDynamicArrayType(LevelId: Integer;
                               const TypeName: String;
                               ElemTypeId: Integer): Integer;
    function RegisterPointerType(LevelId: Integer;
                                 const TypeName: String;
                                 OriginTypeId: Integer;
                                 const OriginTypeName: String = ''): Integer;
    function RegisterMethodReferenceType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;
    function RegisterSetType(LevelId: Integer;
                             const TypeName: String;
                             OriginTypeId: Integer): Integer;
    function RegisterProceduralType(LevelId: Integer;
                                    const TypeName: String;
                                    SubId: Integer): Integer;
{$IFNDEF PAXARM}
    function RegisterShortStringType(LevelId: Integer;
                                     const TypeName: String;
                                     L: Integer): Integer;
{$ENDIF}

    function RegisterEventType(LevelId: Integer;
                               const TypeName: String;
                               SubId: Integer): Integer;
    function RegisterRTTIType(LevelId: Integer;
                              pti: PTypeInfo): Integer;
    function RegisterTypeAlias(LevelId:Integer;
                               const TypeName: String;
                               OriginTypeId: Integer): Integer;
    function RegisterVariable(LevelId: Integer;
                              const VarName: String; TypeId: Integer;
                              Address: Pointer = nil): Integer; overload;
    function RegisterVariable(LevelId: Integer;
                       const Declaration: String; Address: Pointer): Integer; overload;
    function RegisterObject(LevelId: Integer;
                            const ObjectName: String;
                            TypeId: Integer;
                            Address: Pointer = nil): Integer;
    function RegisterVirtualObject(LevelId: Integer;
                                  const ObjectName: String): Integer;
    function RegisterConstant(LevelId: Integer;
                              const ConstName: String;
                              typeID: Integer;
                              const Value: Variant): Integer; overload;
    function RegisterConstant(LevelId: Integer;
                              const ConstName: String;
                              const Value: Variant): Integer; overload;
    function RegisterPointerConstant(LevelId: Integer;
                              const ConstName: String;
                              const Value: Pointer): Integer; overload;
    function RegisterConstant(LevelId: Integer;
                              const ConstName: String;
                              const Value: Extended): Integer; overload;
    function RegisterConstant(LevelId: Integer;
                              const ConstName: String;
                              const Value: Int64): Integer; overload;
    function RegisterConstant(LevelId: Integer;
                              const Declaration: String): Integer; overload;
    function RegisterRoutine(LevelId: Integer;
                             const RoutineName: String; ResultTypeID: Integer;
                             CallConvention: Integer;
                             Address: Pointer = nil): Integer; overload;
    function RegisterRoutine(LevelId: Integer; const Name: String; ResultId: Integer;
                             Address: Pointer;
                             CallConvention: Integer = _ccREGISTER;
                             OverCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterRoutine(LevelId: Integer; const Name, ResultType: String;
                             Address: Pointer;
                             CallConvention: Integer = _ccREGISTER;
                             OverCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterMethod(LevelId: Integer;
                            const RoutineName: String; ResultTypeID: Integer;
                            CallConvention: Integer;
                            Address: Pointer = nil;
                            IsShared: Boolean = false): Integer; overload;
    function RegisterMethod(ClassId: Integer;
                            const Name: String;
                            ResultId: Integer;
                            Address: Pointer;
                            CallConvention: Integer = _ccREGISTER;
                            IsShared: Boolean = false;
                            CallMode: Integer = _cmNONE;
                            MethodIndex: Integer = 0;
                            OverCount: Integer = 0;
                            i_IsAbstract: Boolean = false;
                            i_AbstractMethodCount: Integer = 0;
                            i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterMethod(ClassId: Integer;
                            const Name, ResultType: String;
                            Address: Pointer;
                            CallConvention: Integer = _ccREGISTER;
                            IsShared: Boolean = false;
                            CallMode: Integer = _cmNONE;
                            MethodIndex: Integer = 0;
                            OverCount: Integer = 0;
                            i_IsAbstract: Boolean = false;
                            i_AbstractMethodCount: Integer = 0;
                            i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterConstructor(ClassId: Integer;
                                 const Name: String;
                                 Address: Pointer;
                                 CallConvention: Integer = _ccREGISTER;
                                 IsShared: Boolean = false;
                                 CallMode: Integer = cmNONE;
                                 i_MethodIndex: Integer = 0;
                                 OverCount: Integer = 0;
                                 i_IsAbstract: Boolean = false;
                                 i_AbstractMethodCount: Integer = 0;
                                 i_IsDeprecated: Boolean = false): Integer;
    function RegisterDestructor(ClassId: Integer; const Name: String;
                                Address: Pointer): Integer;
    function RegisterParameter(HSub: Integer; ParamTypeID: Integer;
                               const DefaultValue: Variant;
                               ByRef: Boolean = false): Integer; overload;
    function RegisterParameter(LevelId: Integer;
                               const ParameterName: String;
                               ParamTypeID: Integer;
                               ParamMod: Integer = 0;
                               Optional: Boolean = false;
                               const DefaultValue: String = ''): Integer; overload;
    function RegisterParameter(LevelId: Integer;
                               const ParameterName: String;
                               const ParameterType: String;
                               ParamMod: Integer = 0;
                               Optional: Boolean = false;
                               const DefaultValue: String = ''): Integer; overload;
    function RegisterHeader(LevelId: Integer; const Header: String;
                            Address: Pointer = nil;
                            MethodIndex: Integer = 0): Integer;
    function RegisterFakeHeader(LevelId: Integer;
              const Header: String; Address: Pointer): Integer;
    function RegisterTypeDeclaration(LevelId: Integer;
                            const Declaration: String): Integer;
    function RegisterSomeType(LevelId: Integer;
                           const TypeName: String): Integer;
{$ifdef DRTTI}
    procedure RegisterImportUnit(Level: Integer; const AUnitName: String);
{$endif}
    function GetHandle(LevelId: Integer; const MemberName: String; Upcase: Boolean): Integer;
    function Compile(APaxRunner: TPaxRunner;
                     BuildAll: Boolean = false;
                     BuildWithRuntimePackages: Boolean = false): boolean; overload;
    function Compile: boolean; overload;
    function Parse: boolean;
    function CompileExpression(const Expression: String;
                               APaxRunner: TPaxRunner;
                               const LangName: String = ''): Boolean;
    function CodeCompletion(const ModuleName: String;
       X, Y: Integer; L: TStrings; PaxLang: TPaxCompilerLanguage = nil): Boolean;
    function FindDeclaration(const ModuleName: String;
                                      X, Y: Integer;
                                      PaxLang: TPaxCompilerLanguage = nil): Integer;
    function GetKernelPtr: Pointer;
    procedure RegisterGlobalJSObjects(var R: TJS_Record);
    function GetUndeclaredTypes: TStringList;
    function GetUndeclaredIdentifiers: TStringList;

    function LookupId(const FullName: String; UpCase: Boolean = true): Integer;
    function LookupTypeId(const TypeName: String): Integer;
    function LookupTypeNamespaceId(const TypeName: String): Integer;
    function LookupNamespace(LevelId: Integer; const NamespaceName: String;
                             CaseSensitive: Boolean): Integer; overload;
    function LookupNamespace(const NamespaceName: String): Integer; overload;
    procedure AssignImportTable(ImportTable: Pointer);
    function GetModuleName(Id: Integer): String;
    function GetPosition(Id: Integer): Integer;
    function GetKind(Id: Integer): Integer;
    function GetEvalList: TStringList;
    function InScript(const IdentName: String): Boolean;
    procedure ExtendAlphabet(B1, B2: Word);

    property ErrorCount: Integer read GetErrorCount;
    property ErrorMessage[I: Integer]: String read GetErrorMessage;
    property ErrorModuleName[I: Integer]: String read GetErrorModuleName;
    property ErrorLine[I: Integer]: String read GetErrorLine;
    property ErrorLineNumber[I: Integer]: Integer read GetErrorLineNumber;
    property ErrorLinePos[I: Integer]: Integer read GetErrorLinePos;
    property ErrorFileName[I: Integer]: String read GetErrorFileName;

    property WarningCount: Integer read GetWarningCount;
    property WarningMessage[I: Integer]: String read GetWarningMessage;
    property WarningModuleName[I: Integer]: String read GetWarningModuleName;
    property WarningLine[I: Integer]: String read GetWarningLine;
    property WarningLineNumber[I: Integer]: Integer read GetWarningLineNumber;
    property WarningLinePos[I: Integer]: Integer read GetWarningLinePos;
    property WarningFileName[I: Integer]: String read GetWarningFileName;

    property Modules[const ModuleName: String]: TStringList read GetSourceModule;
    property CurrLineNumber: Integer read GetCurrLineNumber;
    property CurrModuleNumber: Integer read GetCurrModuleNumber;
    property CurrModuleName: String read GetCurrModuleName;
    property CondDirectiveList: TStringList read GetCondDirectiveList;
    property CurrLanguage: String read GetCurrLanguage write SetCurrLanguage;
    property NativeSEH: Boolean read GetNativeSEH write SetNativeSEH;
    property CompletionPrefix: String read GetCompletionPrefix;
    property Unicode: Boolean read GetUnicode write SetUnicode;
    property TargetPlatform: Integer read GetTargetPlatform write SetTargetPlatform;
  published
    property Alignment: Integer read GetAlignment write SetAlignment;
    property OnCompilerProgress: TPaxCompilerNotifyEvent
                                 read GetOnCompilerProgress write SetOnCompilerProgress;
    property OnUnitAlias: TPaxCompilerUnitAliasEvent
                         read GetOnUnitAlias write SetOnUnitAlias;
    property OnUsedUnit: TPaxCompilerUsedUnitEvent
                         read GetOnUsedUnit write SetOnUsedUnit;
    property OnImportUnit: TPaxCompilerImportMemberEvent
                         read GetOnImportUnit write SetOnImportUnit;
    property OnImportType: TPaxCompilerImportMemberEvent
                         read GetOnImportType write SetOnImportType;
    property OnImportGlobalMembers: TPaxCompilerNotifyEvent
                                 read GetOnImportGlobalMembers write SetOnImportGlobalMembers;
    property OnSavePCU: TPaxCompilerSavePCUEvent
                         read GetOnSavePCU write SetOnSavePCU;
    property OnLoadPCU: TPaxCompilerLoadPCUEvent
                         read GetOnLoadPCU write SetOnLoadPCU;
    property OnInclude: TPaxCompilerIncludeEvent
                         read GetOnInclude write SetOnInclude;
    property OnSavePCUFinished: TPaxCompilerSavePCUFinishedEvent
                         read GetOnSavePCUFinished write SetOnSavePCUFinished; // jason
    property OnLoadPCUFinished: TPaxCompilerLoadPCUFinishedEvent
                         read GetOnLoadPCUFinished write SetOnLoadPCUFinished; // jason
    property OnDefineDirective: TPaxCompilerDirectiveEvent
                                read GetOnDefineDirective write SetOnDefineDirective;
    property OnUndefineDirective: TPaxCompilerDirectiveEvent
                                read GetOnUndefineDirective write SetOnUndefineDirective;
    property OnUnknownDirective: TPaxCompilerDirectiveEvent
                                read GetOnUnknownDirective write SetOnUnknownDirective;
    property OnUndeclaredIdentifier: TPaxCompilerUndeclaredIdentifierEvent
                                read GetOnUndeclaredIdentifier write SetOnUndeclaredIdentifier;
    property OnComment: TPaxCompilerCommentEvent
                                read GetOnComment write SetOnComment;
    property DebugMode: Boolean read GetDebugMode write SetDebugMode;
  end;

  TPaxCompilerLanguage = class(TComponent)
  private
    function GetCompleteBooleanEval: Boolean;
    procedure SetCompleteBooleanEval(value: Boolean);
    function GetPrintKeyword: String;
    function GetPrintlnKeyword: String;
    procedure SetPrintKeyword(const value: String);
    procedure SetPrintlnKeyword(const value: String);
    function GetUnitLookup: Boolean;
    procedure SetUnitLookup(value: Boolean);
    function GetExplicitOff: Boolean;
    procedure SetExplicitOff(value: Boolean);
    function GetInitFuncResult: Boolean;
    procedure SetInitFuncResult(value: Boolean);
  protected
    P: TBaseParser;
    function GetLanguageName: String; virtual; abstract;
    function GetParser: TBaseParser; virtual; abstract;
    property ExplicitOff: Boolean read GetExplicitOff write SetExplicitOff;
    property CompleteBooleanEval: Boolean read GetCompleteBooleanEval write SetCompleteBooleanEval;
    property PrintKeyword: String read GetPrintKeyword write SetPrintKeyword;
    property PrintlnKeyword: String read GetPrintlnKeyword write SetPrintlnKeyword;
    property UnitLookup: Boolean read GetUnitLookup write SetUnitLookup;
  public
    destructor Destroy; override;
    procedure SetCallConv(CallConv: Integer); virtual; abstract;
    property LanguageName: String read GetLanguageName;
    property InitFuncResult: Boolean read GetInitFuncResult write SetInitFuncResult;
  end;

  TPaxCompilerLanguageClass = class of TPaxCompilerLanguage;

  TPaxPascalLanguage = class;

  TPaxParserNotifyEvent = procedure(Sender: TPaxPascalLanguage) of object;
  TPaxParserIdentEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer) of object;
  TPaxParserIdentEventEx = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const Declaration: String) of object;
  TPaxParserNamedValueEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const Value: Variant;
    const Declaration: String) of object;
  TPaxParserTypedIdentEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const TypeName: String;
    const Declaration: String) of object;
  TPaxParserVariantRecordFieldEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const TypeName: String; VarCount: Int64;
    const Declaration: String) of object;
  TPaxParserNamedTypedValueEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const TypeName: String;
    const DefaultValue: String;
    const Declaration: String) of object;
  TPaxParserDeclarationEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer; const Declaration: String) of object;
  TPaxParserArrayTypeEvent = procedure(Sender: TPaxPascalLanguage;
    const IdentName: String; Id: Integer;
    Ranges: TStringList;
    const ElemTypeName: String) of object;

  TPaxPascalLanguage = class(TPaxCompilerLanguage)
  private
    function GetOnParseUnitName: TPaxParserIdentEvent;
    procedure SetOnParseUnitName(value: TPaxParserIdentEvent);
    function GetOnParseImplementationSection: TPaxParserNotifyEvent;
    procedure SetOnParseImplementationSection(value: TPaxParserNotifyEvent);
    function GetOnParseBeginUsedUnitList: TPaxParserNotifyEvent;
    procedure SetOnParseBeginUsedUnitList(value: TPaxParserNotifyEvent);
    function GetOnParseEndUsedUnitList: TPaxParserNotifyEvent;
    procedure SetOnParseEndUsedUnitList(value: TPaxParserNotifyEvent);
    function GetOnParseUsedUnitName: TPaxParserIdentEvent;
    procedure SetOnParseUsedUnitName(value: TPaxParserIdentEvent);
    function GetOnParseBeginClassTypeDeclaration: TPaxParserIdentEventEx;
    procedure SetOnParseBeginClassTypeDeclaration(value: TPaxParserIdentEventEx);
    function GetOnParseEndClassTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndClassTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseForwardTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseForwardTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseAncestorTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseAncestorTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseUsedInterface: TPaxParserIdentEvent;
    procedure SetOnParseUsedInterface(value: TPaxParserIdentEvent);
    function GetOnParseClassReferenceTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseClassReferenceTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseAliasTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseAliasTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseProceduralTypeDeclaration: TPaxParserIdentEventEx;
    procedure SetOnParseProceduralTypeDeclaration(value: TPaxParserIdentEventEx);
    function GetOnParseEventTypeDeclaration: TPaxParserIdentEventEx;
    procedure SetOnParseEventTypeDeclaration(value: TPaxParserIdentEventEx);
    function GetOnParseMethodReferenceTypeDeclaration: TPaxParserIdentEventEx;
    procedure SetOnParseMethodReferenceTypeDeclaration(value: TPaxParserIdentEventEx);
    function GetOnParseSetTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseSetTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParsePointerTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParsePointerTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseArrayTypeDeclaration: TPaxParserArrayTypeEvent;
    procedure SetOnParseArrayTypeDeclaration(value: TPaxParserArrayTypeEvent);
    function GetOnParseDynArrayTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseDynArrayTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseShortStringTypeDeclaration: TPaxParserNamedValueEvent;
    procedure SetOnParseShortStringTypeDeclaration(value: TPaxParserNamedValueEvent);
    function GetOnParseSubrangeTypeDeclaration: TPaxParserDeclarationEvent;
    procedure SetOnParseSubrangeTypeDeclaration(value: TPaxParserDeclarationEvent);
    function GetOnParseBeginRecordTypeDeclaration: TPaxParserIdentEventEx;
    procedure SetOnParseBeginRecordTypeDeclaration(value: TPaxParserIdentEventEx);
    function GetOnParseEndRecordTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndRecordTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseBeginClassHelperTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseBeginClassHelperTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseEndClassHelperTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndClassHelperTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseBeginRecordHelperTypeDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseBeginRecordHelperTypeDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseEndRecordHelperTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndRecordHelperTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseBeginInterfaceTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseBeginInterfaceTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseEndInterfaceTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndInterfaceTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseBeginEnumTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseBeginEnumTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseEndEnumTypeDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseEndEnumTypeDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseEnumName: TPaxParserNamedValueEvent;
    procedure SetOnParseEnumName(value: TPaxParserNamedValueEvent);
    function GetOnParseFieldDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseFieldDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseVariantRecordFieldDeclaration: TPaxParserVariantRecordFieldEvent;
    procedure SetOnParseVariantRecordFieldDeclaration(value: TPaxParserVariantRecordFieldEvent);
    function GetOnParsePropertyDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParsePropertyDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseConstantDeclaration: TPaxParserNamedValueEvent;
    procedure SetOnParseConstantDeclaration(value: TPaxParserNamedValueEvent);
    function GetOnParseResourceStringDeclaration: TPaxParserNamedValueEvent;
    procedure SetOnParseResourceStringDeclaration(value: TPaxParserNamedValueEvent);
    function GetOnParseTypedConstantDeclaration: TPaxParserNamedTypedValueEvent;
    procedure SetOnParseTypedConstantDeclaration(value: TPaxParserNamedTypedValueEvent);
    function GetOnParseVariableDeclaration: TPaxParserTypedIdentEvent;
    procedure SetOnParseVariableDeclaration(value: TPaxParserTypedIdentEvent);
    function GetOnParseBeginSubDeclaration: TPaxParserIdentEvent;
    procedure SetOnParseBeginSubDeclaration(value: TPaxParserIdentEvent);
    function GetOnParseEndSubDeclaration: TPaxParserDeclarationEvent;
    procedure SetOnParseEndSubDeclaration(value: TPaxParserDeclarationEvent);
    function GetOnParseBeginFormalParameterList: TPaxParserNotifyEvent;
    procedure SetOnParseBeginFormalParameterList(value: TPaxParserNotifyEvent);
    function GetOnParseEndFormalParameterList: TPaxParserNotifyEvent;
    procedure SetOnParseEndFormalParameterList(value: TPaxParserNotifyEvent);
    function GetOnParseFormalParameterDeclaration: TPaxParserNamedTypedValueEvent;
    procedure SetOnParseFormalParameterDeclaration(value: TPaxParserNamedTypedValueEvent);
    function GetOnParseResultType: TPaxParserIdentEvent;
    procedure SetOnParseResultType(value: TPaxParserIdentEvent);
    function GetOnParseSubDirective: TPaxParserIdentEvent;
    procedure SetOnParseSubDirective(value: TPaxParserIdentEvent);
  protected
    function GetParser: TBaseParser; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCallConv(CallConv: Integer); override;
    function GetLanguageName: String; override;

    property OnParseUnitName: TPaxParserIdentEvent read GetOnParseUnitName
                                                   write SetOnParseUnitName;
    property OnParseImplementationSection: TPaxParserNotifyEvent read GetOnParseImplementationSection
                                                   write SetOnParseImplementationSection;
    property OnParseBeginUsedUnitList: TPaxParserNotifyEvent read GetOnParseBeginUsedUnitList
                                                   write SetOnParseBeginUsedUnitList;
    property OnParseEndUsedUnitList: TPaxParserNotifyEvent read GetOnParseEndUsedUnitList
                                                   write SetOnParseEndUsedUnitList;
    property OnParseUsedUnitName: TPaxParserIdentEvent read GetOnParseUsedUnitName
                                                   write SetOnParseUsedUnitName;
    property OnParseTypeDeclaration: TPaxParserIdentEvent read GetOnParseTypeDeclaration
                                                   write SetOnParseTypeDeclaration;
    property OnParseForwardTypeDeclaration: TPaxParserIdentEvent read GetOnParseForwardTypeDeclaration
                                                   write SetOnParseForwardTypeDeclaration;
    property OnParseBeginClassTypeDeclaration: TPaxParserIdentEventEx read GetOnParseBeginClassTypeDeclaration
                                                   write SetOnParseBeginClassTypeDeclaration;
    property OnParseEndClassTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndClassTypeDeclaration
                                                   write SetOnParseEndClassTypeDeclaration;
    property OnParseAncestorTypeDeclaration: TPaxParserIdentEvent read GetOnParseAncestorTypeDeclaration
                                                   write SetOnParseAncestorTypeDeclaration;
    property OnParseUsedInterface: TPaxParserIdentEvent read GetOnParseUsedInterface
                                                   write SetOnParseUsedInterface;
    property OnParseClassReferenceTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseClassReferenceTypeDeclaration
                                                   write SetOnParseClassReferenceTypeDeclaration;
    property OnParseAliasTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseAliasTypeDeclaration
                                                   write SetOnParseAliasTypeDeclaration;
    property OnParseProceduralTypeDeclaration: TPaxParserIdentEventEx read GetOnParseProceduralTypeDeclaration
                                                   write SetOnParseProceduralTypeDeclaration;
    property OnParseEventTypeDeclaration: TPaxParserIdentEventEx read GetOnParseEventTypeDeclaration
                                                   write SetOnParseEventTypeDeclaration;
    property OnParseMethodReferenceTypeDeclaration: TPaxParserIdentEventEx read GetOnParseMethodReferenceTypeDeclaration
                                                   write SetOnParseMethodReferenceTypeDeclaration;
    property OnParseSetTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseSetTypeDeclaration
                                                   write SetOnParseSetTypeDeclaration;
    property OnParsePointerTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParsePointerTypeDeclaration
                                                   write SetOnParsePointerTypeDeclaration;
    property OnParseArrayTypeDeclaration: TPaxParserArrayTypeEvent read GetOnParseArrayTypeDeclaration
                                                   write SetOnParseArrayTypeDeclaration;
    property OnParseDynArrayTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseDynArrayTypeDeclaration
                                                   write SetOnParseDynArrayTypeDeclaration;
    property OnParseShortStringTypeDeclaration: TPaxParserNamedValueEvent read GetOnParseShortStringTypeDeclaration
                                                   write SetOnParseShortStringTypeDeclaration;
    property OnParseSubrangeTypeDeclaration: TPaxParserDeclarationEvent read GetOnParseSubrangeTypeDeclaration
                                                   write SetOnParseSubrangeTypeDeclaration;
    property OnParseBeginRecordTypeDeclaration: TPaxParserIdentEventEx read GetOnParseBeginRecordTypeDeclaration
                                                   write SetOnParseBeginRecordTypeDeclaration;
    property OnParseEndRecordTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndRecordTypeDeclaration
                                                   write SetOnParseEndRecordTypeDeclaration;
    property OnParseBeginClassHelperTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseBeginClassHelperTypeDeclaration
                                                   write SetOnParseBeginClassHelperTypeDeclaration;
    property OnParseEndClassHelperTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndClassHelperTypeDeclaration
                                                   write SetOnParseEndClassHelperTypeDeclaration;
    property OnParseBeginRecordHelperTypeDeclaration: TPaxParserTypedIdentEvent read GetOnParseBeginRecordHelperTypeDeclaration
                                                   write SetOnParseBeginRecordHelperTypeDeclaration;
    property OnParseEndRecordHelperTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndRecordHelperTypeDeclaration
                                                   write SetOnParseEndRecordHelperTypeDeclaration;
    property OnParseBeginInterfaceTypeDeclaration: TPaxParserIdentEvent read GetOnParseBeginInterfaceTypeDeclaration
                                                   write SetOnParseBeginInterfaceTypeDeclaration;
    property OnParseEndInterfaceTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndInterfaceTypeDeclaration
                                                   write SetOnParseEndInterfaceTypeDeclaration;
    property OnParseBeginEnumTypeDeclaration: TPaxParserIdentEvent read GetOnParseBeginEnumTypeDeclaration
                                                   write SetOnParseBeginEnumTypeDeclaration;
    property OnParseEndEnumTypeDeclaration: TPaxParserIdentEvent read GetOnParseEndEnumTypeDeclaration
                                                   write SetOnParseEndEnumTypeDeclaration;
    property OnParseEnumName: TPaxParserNamedValueEvent read GetOnParseEnumName
                                                   write SetOnParseEnumName;
    property OnParseFieldDeclaration: TPaxParserTypedIdentEvent read GetOnParseFieldDeclaration
                                                   write SetOnParseFieldDeclaration;
    property OnParseVariantRecordFieldDeclaration: TPaxParserVariantRecordFieldEvent read GetOnParseVariantRecordFieldDeclaration
                                                   write SetOnParseVariantRecordFieldDeclaration;
    property OnParsePropertyDeclaration: TPaxParserTypedIdentEvent read GetOnParsePropertyDeclaration
                                                   write SetOnParsePropertyDeclaration;
    property OnParseConstantDeclaration: TPaxParserNamedValueEvent read GetOnParseConstantDeclaration
                                                   write SetOnParseConstantDeclaration;
    property OnParseResourceStringDeclaration: TPaxParserNamedValueEvent read GetOnParseResourceStringDeclaration
                                                   write SetOnParseResourceStringDeclaration;
    property OnParseTypedConstantDeclaration: TPaxParserNamedTypedValueEvent read GetOnParseTypedConstantDeclaration
                                                   write SetOnParseTypedConstantDeclaration;
    property OnParseVariableDeclaration: TPaxParserTypedIdentEvent read GetOnParseVariableDeclaration
                                                   write SetOnParseVariableDeclaration;
    property OnParseBeginSubDeclaration: TPaxParserIdentEvent read GetOnParseBeginSubDeclaration
                                                   write SetOnParseBeginSubDeclaration;
    property OnParseEndSubDeclaration: TPaxParserDeclarationEvent read GetOnParseEndSubDeclaration
                                                   write SetOnParseEndSubDeclaration;
    property OnParseBeginFormalParameterList: TPaxParserNotifyEvent read GetOnParseBeginFormalParameterList
                                                   write SetOnParseBeginFormalParameterList;
    property OnParseEndFormalParameterList: TPaxParserNotifyEvent read GetOnParseEndFormalParameterList
                                                   write SetOnParseEndFormalParameterList;
    property OnParseFormalParameterDeclaration: TPaxParserNamedTypedValueEvent read GetOnParseFormalParameterDeclaration
                                                   write SetOnParseFormalParameterDeclaration;
    property OnParseResultType: TPaxParserIdentEvent read GetOnParseResultType
                                                   write SetOnParseResultType;
    property OnParseSubDirective: TPaxParserIdentEvent read GetOnParseSubDirective
                                                   write SetOnParseSubDirective;
  published
    property ExplicitOff;
    property CompleteBooleanEval;
    property UnitLookup;
    property PrintKeyword;
    property PrintlnKeyword;
  end;

procedure SetDump;

implementation

uses
  PAXCOMP_BYTECODE,
  PAXCOMP_STDLIB;

//--------------------TPaxCompiler----------------------------------------------

constructor TPaxCompiler.Create(AOwner: TComponent);
begin
  inherited;
  kernel := TKernel.Create(Self);
end;

destructor TPaxCompiler.Destroy;
begin
  FreeAndNil(kernel);
  inherited;
end;

procedure TPaxCompiler.Reset;
begin
  kernel.Reset;
end;

procedure TPaxCompiler.ResetCompilation;
begin
  kernel.ResetCompilation;
end;

procedure TPaxCompiler.AddModule(const ModuleName, LanguageName: String);
begin
  kernel.AddModule(ModuleName, LanguageName);
end;

procedure TPaxCompiler.AddCode(const ModuleName, Text: String);
begin
  kernel.AddCode(ModuleName, Text);
end;

procedure TPaxCompiler.AddCodeFromFile(const ModuleName, FileName: String);
begin
  kernel.AddCodeFromFile(ModuleName, FileName);
end;

procedure TPaxCompiler.CreateMapping(Runner: TBaseRunner);
begin
  kernel.code.CreateMapping(Runner.HostMapTable, true,
    Runner.HostMapTable, nil);
  if kernel.SignCompression then
  begin
    kernel.CompressHostClassList(Runner.HostMapTable);
    kernel.SymbolTable.CreateOffsets(Runner.JS_Record.Id_JS_Object,
                                     Runner.JS_Record.Id_JS_Error);
  end;
end;

function TPaxCompiler.CompileExpression(const Expression: String;
                                        APaxRunner: TPaxRunner;
                                        const LangName: String = ''): Boolean;
var
  Runner: TBaseRunner;
begin
  Runner := APaxRunner.GetProgPtr;
  result := false;

  try
    Runner.Reset;
    RegisterGlobalJSObjects(Runner.JS_Record);
    kernel.SetProg(Runner);

    kernel.ParseExpression(Expression, LangName);
    if kernel.HasError then Exit;
    kernel.Link;
    if kernel.HasError then Exit;

    CreateMapping(Runner);

    APaxRunner.EmitProc(kernel, Runner);
  finally
    if kernel.SignCompression then
      kernel.SymbolTable.RestoreClassIndexes;
  end;
  result := true;
end;

function TPaxCompiler.Compile(APaxRunner: TPaxRunner;
                              BuildAll: Boolean = false;
                              BuildWithRuntimePackages: Boolean = false): boolean;
var
  temp: Pointer;
  Runner: TBaseRunner;
begin
  if kernel.Modules.Count = 0 then
    kernel.RaiseError(errEmptyModuleList, []);

  Runner := APaxRunner.GetProgPtr;
  result := false;
  if BuildWithRuntimePackages then
    BuildAll := true;

  temp := CurrKernel;
  CurrKernel := Kernel;
  try
    Runner.Reset;
    RegisterGlobalJSObjects(Runner.JS_Record);
    kernel.SetProg(Runner);

    kernel.BuildAll := BuildAll;
    kernel.BuildWithRuntimePackages := BuildWithRuntimePackages;

    try

      kernel.Parse;
      if kernel.HasError then Exit;

      if kernel.ImportOnly then
      begin
        result := true;
        Exit;
      end;

      kernel.Link;
      if kernel.HasError then Exit;

      if kernel.Canceled then
      begin
        result := true;
        Exit;
      end;

      CreateMapping(Runner);

      APaxRunner.EmitProc(kernel, Runner);

    finally
      if not kernel.ImportOnly then
      begin
        if not kernel.BuildWithRuntimePackages then
          Runner.ProgList.LoadFromStreamList(kernel.PCUStreamList, Runner);

        if kernel.SignCompression then
          kernel.SymbolTable.RestoreClassIndexes;
        if BuildWithRuntimePackages then
          Runner.ProgList.Clear;

        kernel.BuildAll := false;
        kernel.BuildWithRuntimePackages := false;
      end;
    end;

    result := true;
  finally
    CurrKernel := temp;
  end;
end;

function TPaxCompiler.Compile: boolean;
var
  ClassFactory: TPaxClassFactory;
  TypeInfoList: TPaxTypeInfoList;
  ExportList: TExportList;
  MessageList: TMessageList;
begin
  ClassFactory := TPaxClassFactory.Create;
  TypeInfoList := TPaxTypeInfoList.Create;
  ExportList   := TExportList.Create;
  MessageList  := TMessageList.Create;

  CurrKernel := kernel;
  try
    kernel.ClassFactory := ClassFactory;
    kernel.TypeInfoList := TypeInfoList;
    kernel.MessageList := MessageList;
    kernel.ExportList := ExportList;

    result := false;
    kernel.Parse;
    if kernel.HasError then Exit;

    if kernel.ImportOnly then
    begin
      result := true;
      Exit;
    end;

    kernel.Link;
    if kernel.HasError then Exit;
    result := true;
  finally
    FreeAndNil(ClassFactory);
    FreeAndNil(TypeInfoList);
    FreeAndNil(ExportList);
    FreeAndNil(MessageList);
  end;
end;

function TPaxCompiler.Parse: boolean;
var
  ClassFactory: TPaxClassFactory;
  TypeInfoList: TPaxTypeInfoList;
  ExportList: TExportList;
  MessageList: TMessageList;
begin
  ClassFactory := TPaxClassFactory.Create;
  TypeInfoList := TPaxTypeInfoList.Create;
  ExportList   := TExportList.Create;
  MessageList  := TMessageList.Create;

  CurrKernel := kernel;

  try
    kernel.ClassFactory := ClassFactory;
    kernel.TypeInfoList := TypeInfoList;
    kernel.MessageList := MessageList;
    kernel.ExportList := ExportList;

    result := false;
    kernel.Parse;
    if kernel.HasError then Exit;

    if kernel.ImportOnly then
    begin
      result := true;
      Exit;
    end;

//    kernel.InterfaceOnly := true;

//    kernel.Link;
    result := not kernel.HasError;

    dmp;

  finally
    FreeAndNil(ClassFactory);
    FreeAndNil(TypeInfoList);
    FreeAndNil(ExportList);
    FreeAndNil(MessageList);
  end;
end;

function TPaxCompiler.CodeCompletion(const ModuleName: String;
      X, Y: Integer; L: TStrings; PaxLang: TPaxCompilerLanguage = nil): Boolean;
var
  NN, Op, Id, I: Integer;
  ClassFactory: TPaxClassFactory;
  TypeInfoList: TPaxTypeInfoList;
  ExportList  : TExportList;
  MessageList : TMessageList;
  Lst: TIntegerList;
  R: TCodeRec;
  WithIsAllowed: Boolean;
  temp: Pointer;
  SkipParams: Integer;
  VisSet: TMemberVisibilitySet;
begin
  result := false;
  ClassFactory := TPaxClassFactory.Create;
  TypeInfoList := TPaxTypeInfoList.Create;
  ExportList   := TExportList.Create;
  MessageList  := TMessageList.Create;

  temp := CurrKernel;
  CurrKernel := Kernel;
  try
    kernel.ClassFactory := ClassFactory;
    kernel.TypeInfoList := TypeInfoList;
    kernel.MessageList := MessageList;
    kernel.ExportList := ExportList;

    kernel.ParseCompletion(ModuleName, X, Y);

    if kernel.HasError then
    begin
      CurrKernel := kernel;
      Exit;
    end;
    if kernel.Code.Card = 0 then Exit;
    kernel.Link;

  finally
    CurrKernel := temp;

    FreeAndNil(ClassFactory);
    FreeAndNil(TypeInfoList);
    FreeAndNil(ExportList);
    FreeAndNil(MessageList);
  end;

  CurrKernel := kernel;

  kernel.Code.LocateDummyName(NN);
  if NN > 0 then
  begin
    Op := kernel.Code[NN].GenOp;
    Id := kernel.Code[NN].Arg1;
    if Op = OP_EVAL then
    begin
      if NN < kernel.Code.Card then
        if kernel.Code[NN + 1].GenOp = OP_BEGIN_USING then
        begin
          Id := kernel.Code.GetLevel(NN);
          kernel.SymbolTable.ExtractNamespaces(Id, L);
          result := true;
          Exit;
        end;

      if ByteInSet(kernel.CancelChar, [Ord('('), Ord('.')]) then
      begin
        result := true;
        kernel.Errors.Reset;
        Exit;
      end;

      WithIsAllowed := true;
      Lst := TIntegerList.Create;
      try
        for I := NN downto 1 do
        begin
          R := kernel.Code[I];
          if R.Op = OP_BEGIN_MODULE then
            break;
          if R.Op = OP_INIT_SUB then
            WithIsAllowed := false;
          if R.Op = OP_END_SUB then
            WithIsAllowed := false;
          if R.Op = OP_END_WITH then
            WithIsAllowed := false;
          if R.Op = OP_BEGIN_USING then
//            if R.Arg1 > 0 then
              Lst.Add(R.Arg1);
          if R.Op = OP_BEGIN_WITH then
            if WithIsAllowed then
              Lst.Add(R.Arg1);
        end;

        for I := 0 to Lst.Count - 1 do
        begin
          Id := Lst[I];

          if Id > 0 then
          begin
            if kernel.SymbolTable[Id].Kind <> KindNAMESPACE then
            if kernel.SymbolTable[Id].TypeId = 0 then
              kernel.Code.RestoreFieldType(NN);
          end;

          if PaxLang = nil then
            kernel.SymbolTable.ExtractMembers(Id, L)
          else if PaxLang is TPaxPascalLanguage then
            kernel.SymbolTable.ExtractMembers(Id, L)
          else
            kernel.SymbolTable.ExtractMembers(Id, L, lngBasic);
        end;
        kernel.Errors.Reset;

      finally
        FreeAndNil(Lst);
      end;
    end
    else if Op = OP_FIELD then
    begin
      if kernel.SymbolTable[Id].Kind <> KindNAMESPACE then
      if kernel.SymbolTable[Id].TypeId = 0 then
        kernel.Code.RestoreFieldType(NN);

      VisSet := TKernel(kernel).Code.GetCompletionVisibility(Id, NN);

      if PaxLang = nil then
        kernel.SymbolTable.ExtractMembers(Id, L, lngPascal, false, VisSet)
      else if PaxLang is TPaxPascalLanguage then
        kernel.SymbolTable.ExtractMembers(Id, L, lngPascal, false, VisSet)
      else
        kernel.SymbolTable.ExtractMembers(Id, L, lngBasic, false, VisSet);

      if L.Count > 0 then
        kernel.Errors.Reset;
    end
    else if (Op = OP_CALL) or (Op = OP_BEGIN_CALL) then
    begin
      if not (kernel.SymbolTable[Id].Kind in KindSUBS) then
        Exit;

      if kernel.CompletionTarget <> '' then
      begin
        if StrEql(kernel.CompletionTarget, 'New') or
           StrEql(kernel.CompletionTarget, 'Dispose') then
           begin
             L.Add('X: Pointer');
             kernel.Errors.Reset;
           end;
      end
      else
      begin
        if Id = JS_FunctionCallId then
        begin
          with kernel do
          for I := Code.GetStmt(NN) to Code.Card do
            if Code[I].Op = OP_PUSH_INST then
              if Code[I].Res = JS_FunctionCallId then
              begin
                Id := Code[I].Arg1;
                Inc(Id);
                if SymbolTable[Id].Kind = KindSUB then
                  break
                else
                begin
                  kernel.Errors.Reset;
                  Exit;
                end;
              end;
        end;

        SkipParams := 0;
        I := NN;
        while I > 1 do
        begin
          Dec(I);
          with kernel do
          if Code[I].GenOp = OP_PUSH then
          if SymbolTable[Code[I].Arg1].Name = DummyName then
            break;
        end;
        while I > 1 do
        begin
          Dec(I);
          Op := kernel.Code[I].GenOp;
          with kernel do
          if Op = OP_PUSH then
          begin
            if Code[I].Res = Id then
              Inc(SkipParams);
          end
          else
          if Op = OP_BEGIN_CALL then
          begin
            if Code[I].Arg1 = Id then
              break;
          end;
        end;

        kernel.SymbolTable.ExtractParametersEx(Id, L, true, SkipParams);
        kernel.Errors.Reset;
      end;
    end
    else if (Op = OP_PRINT) or (Op = OP_PRINT_EX) then
    begin
      L.Add('P1;[...,PN]');
      kernel.Errors.Reset;
    end
    else if Op = OP_ABS then
    begin
      L.Add('X: Real');
      kernel.Errors.Reset;
    end
    else if (Op = OP_INC) or (Op = OP_DEC) then
    begin
      L.Add('var X: Ordinal; [N: Integer]');
      kernel.Errors.Reset;
    end
    else if (Op = OP_PRED) or
            (Op = OP_SUCC) or
            (Op = OP_ORD) then
    begin
      L.Add('X: Ordinal');
      kernel.Errors.Reset;
    end
    else if Op = OP_CHR then
    begin
      L.Add('X: Byte');
      kernel.Errors.Reset;
    end
    else if Op = OP_STR then
    begin
      L.Add('const X[: Width[:Decimals]]; var S: String');
      kernel.Errors.Reset;
    end
    else if (Op = OP_SIZEOF) or
            (Op = OP_ASSIGNED) or
            (Op = OP_LOW) or
            (Op = OP_HIGH) then
    begin
      L.Add('var X');
      kernel.Errors.Reset;
    end
    else // error
      Exit;
  end
  else
  begin
    kernel.Errors.Reset;
  end;

  result := not kernel.HasError;
end;

function TPaxCompiler.FindDeclaration(const ModuleName: String;
                                      X, Y: Integer;
                                      PaxLang: TPaxCompilerLanguage = nil): Integer;
var
  ClassFactory: TPaxClassFactory;
  TypeInfoList: TPaxTypeInfoList;
  ExportList  : TExportList;
  MessageList : TMessageList;
  temp: Pointer;
  Id, TypeId, LevelId: Integer;
  S: String;
begin
  result := 0;

  ClassFactory := TPaxClassFactory.Create;
  TypeInfoList := TPaxTypeInfoList.Create;
  ExportList   := TExportList.Create;
  MessageList  := TMessageList.Create;

  temp := CurrKernel;
  CurrKernel := Kernel;
  try
    kernel.ClassFactory := ClassFactory;
    kernel.TypeInfoList := TypeInfoList;
    kernel.MessageList := MessageList;
    kernel.ExportList := ExportList;

    kernel.FindDeclId := -1;
    kernel.ParseCompletion(ModuleName, X, Y);

    if kernel.HasError then
      Exit;
    if kernel.Code.Card = 0 then Exit;
    kernel.Link;

  finally
    if kernel.FindDeclId < 0 then
      kernel.FindDeclId := 0;

    CurrKernel := temp;

    FreeAndNil(ClassFactory);
    FreeAndNil(TypeInfoList);
    FreeAndNil(ExportList);
    FreeAndNil(MessageList);
  end;

  result := kernel.FindDeclId;

  if result > 0 then
  with kernel do
  begin
    Id := SymbolTable[result].OwnerId;
    if SymbolTable[result].Kind = KindVAR then
    if Id > 0 then
    begin
      S := SymbolTable[result].Name;
      TypeId := SymbolTable[Id].TerminalTypeId;
      result := SymbolTable.Lookup(S, TypeId, true);
    end
    else if PaxLang = nil then
    begin
      LevelId := SymbolTable[result].Level;
      if LevelId > 0 then
      if SymbolTable[LevelId].Kind = KindSUB then
      if result = SymbolTable.GetResultId(LevelId) then
        result := LevelId;
    end
    else if StrEql(PaxLang.LanguageName, 'Pascal') then
    begin
      LevelId := SymbolTable[result].Level;
      if LevelId > 0 then
      if SymbolTable[LevelId].Kind = KindSUB then
      if result = SymbolTable.GetResultId(LevelId) then
        result := LevelId;
    end;
  end;
end;

procedure TPaxCompiler.RegisterLanguage(L: TPaxCompilerLanguage);
begin
  kernel.RegisterParser(L.GetParser);
end;

procedure TPaxCompiler.RegisterDirective(const Directive: string; const value: Variant);
begin
  kernel.DefList.Add(Directive, value);
end;

function TPaxCompiler.RegisterNamespace(LevelId: Integer;
                                        const NamespaceName: String): Integer;
begin
  result := kernel.SymbolTable.RegisterNamespace(LevelId, NamespaceName);
end;

procedure TPaxCompiler.RegisterUsingNamespace(const aNamespaceName: String);
Var
  H: integer;
begin
  H :=  kernel.SymbolTable.LookupNamespace(aNamespaceName, 0, True);
  if H > 0 then
    RegisterUsingNamespace (H);
end;

procedure TPaxCompiler.RegisterUsingNamespace(aNamespaceID: Integer);
begin
  kernel.SymbolTable.HeaderParser.UsedNamespaceList.Add(aNamespaceID);
end;

procedure TPaxCompiler.UnregisterUsingNamespace(aNamespaceID: Integer);
begin
  kernel.SymbolTable.HeaderParser.UsedNamespaceList.DeleteValue(aNamespaceID);
end;

procedure TPaxCompiler.UnregisterUsingNamespaces;
begin
  kernel.SymbolTable.HeaderParser.UsedNamespaceList.Clear;
end;

procedure TPaxCompiler.UnregisterUsingNamespace(const aNamespaceName: String);
Var
  H: integer;
begin
  H := kernel.SymbolTable.LookupNamespace(aNamespaceName, 0, True);
  if H > 0 then
    UnRegisterUsingNamespace (H);
end;

function TPaxCompiler.RegisterInterfaceType(LevelId: Integer;
                                            const TypeName: String;
                                            const GUID: TGUID): Integer;
begin
  result := kernel.SymbolTable.RegisterInterfaceType(LevelId, TypeName, GUID);
end;

function TPaxCompiler.RegisterInterfaceType(LevelId: Integer;
                                            const TypeName: String;
                                            const GUID: TGUID;
                                            const ParentName: String;
                                            const ParentGUID: TGUID): Integer;
begin
  result := kernel.SymbolTable.RegisterInterfaceType(LevelId, TypeName, GUID);
  kernel.SymbolTable.RegisterSupportedInterface(result, ParentName, ParentGUID);
end;

procedure TPaxCompiler.RegisterSupportedInterface(TypeId: Integer;
                                                  const SupportedInterfaceName: String;
                                                  const GUID: TGUID);
begin
  kernel.SymbolTable.RegisterSupportedInterface(TypeId, SupportedInterfaceName, GUID);
end;

function TPaxCompiler.RegisterClassType(LevelId: Integer;
                           const TypeName: String; AncestorId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterClassType(LevelId, TypeName, AncestorId);
end;

function TPaxCompiler.RegisterClassType(LevelId: Integer;
                                        C: TClass): Integer;
begin
  result := kernel.SymbolTable.RegisterClassType(LevelId, C);
end;

function TPaxCompiler.RegisterClassReferenceType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterClassReferenceType(LevelId, TypeName, OriginClassId);
end;

function TPaxCompiler.RegisterClassHelperType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterHelperType(LevelId, TypeName, OriginClassId);
end;

function TPaxCompiler.RegisterClassHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer;
var
  OriginClassId: Integer;
begin
  OriginClassId := kernel.SymbolTable.LookUpType(OriginalTypeName, 0, true);
  result := kernel.SymbolTable.RegisterHelperType(LevelId, TypeName, OriginClassId);
end;

function TPaxCompiler.RegisterRecordHelperType(LevelId: Integer;
                           const TypeName: String; OriginRecordId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterHelperType(LevelId, TypeName, OriginRecordId);
end;

function TPaxCompiler.RegisterRecordHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer;
var
  OriginRecordId: Integer;
begin
  OriginRecordId := kernel.SymbolTable.LookUpType(OriginalTypeName, 0, true);
  result := kernel.SymbolTable.RegisterHelperType(LevelId, TypeName, OriginRecordId);
end;

function TPaxCompiler.RegisterClassTypeField(TypeId: Integer; const FieldName: String;
                                        FieldTypeID: Integer; FieldShift: Integer = -1): Integer;
begin
  result := kernel.SymbolTable.RegisterTypeField(TypeId, FieldName, FieldTypeID, FieldShift);
end;

function TPaxCompiler.RegisterProperty(LevelId: Integer; const PropName: String;
                                       PropTypeID, ReadId, WriteId: Integer;
                                       IsDefault: Boolean): Integer;
begin
  result := kernel.SymbolTable.RegisterProperty(LevelId, PropName, PropTypeId,
                                    ReadId, WriteId, IsDefault);
end;

function TPaxCompiler.RegisterInterfaceProperty(LevelId: Integer;
                                       const PropName: String;
                                       PropTypeID,
                                       ReadIndex,
                                       WriteIndex: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterInterfaceProperty(LevelId, PropName, PropTypeId,
                                    ReadIndex, WriteIndex);
end;

function TPaxCompiler.RegisterProperty(LevelId: Integer; const Header: String): Integer;
begin
  result := kernel.SymbolTable.RegisterHeader(LevelId, Header, nil);
end;

function TPaxCompiler.RegisterRecordType(LevelId: Integer;
                                         const TypeName: String;
                                         IsPacked: Boolean = false): Integer;
begin
  if IsPacked then
    result := kernel.SymbolTable.RegisterRecordType(LevelId, TypeName, 1)
  else
    result := kernel.SymbolTable.RegisterRecordType(LevelId, TypeName, kernel.Alignment);
end;

function TPaxCompiler.RegisterRecordTypeField(TypeId: Integer; const FieldName: String;
                                        FieldTypeID: Integer; FieldShift: Integer = -1): Integer;
begin
  result := kernel.SymbolTable.RegisterTypeField(TypeId, FieldName, FieldTypeID, FieldShift);
end;

function TPaxCompiler.RegisterVariantRecordTypeField(TypeId: Integer; const FieldName: String;
                                FieldTypeID: Integer;
                                VarCount: Int64): Integer;
begin
  result := kernel.SymbolTable.RegisterVariantRecordTypeField(TypeId,
                                                              FieldName,
                                                              FieldTypeId,
                                                              VarCount);
end;

function TPaxCompiler.RegisterVariantRecordTypeField(LevelId: Integer; const Declaration: String;
                                VarCount: Int64): Integer;
begin
  result := kernel.SymbolTable.RegisterVariantRecordTypeField(LevelId,
      Declaration, VarCount);
end;

function TPaxCompiler.RegisterSubrangeType(LevelId: Integer;
                                           const TypeName: String;
                                           TypeBaseId: Integer;
                                           B1, B2: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterSubrangeType(LevelId, TypeName, TypeBaseId, B1, B2);
end;

function TPaxCompiler.RegisterEnumType(LevelId: Integer;
                                       const TypeName: String;
                                       TypeBaseId: Integer = _typeINTEGER): Integer;
begin
  result := kernel.SymbolTable.RegisterEnumType(LevelId, TypeName, TypeBaseId);
end;

function TPaxCompiler.RegisterEnumValue(EnumTypeId: Integer;
                                            const FieldName: String;
                                            const Value: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterEnumValue(EnumTypeId, FieldName, Value);
end;


function TPaxCompiler.RegisterArrayType(LevelId: Integer;
                                        const TypeName: String;
                                        RangeTypeId, ElemTypeId: Integer;
                                        IsPacked: Boolean = false): Integer;
begin
  if IsPacked then
    result := kernel.SymbolTable.RegisterArrayType(LevelId, TypeName, RangeTypeId, ElemTypeId, 1)
  else
    result := kernel.SymbolTable.RegisterArrayType(LevelId, TypeName, RangeTypeId, ElemTypeId, kernel.Alignment);
end;

function TPaxCompiler.RegisterDynamicArrayType(LevelId: Integer;
                                        const TypeName: String;
                                        ElemTypeId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterDynamicArrayType(LevelId, TypeName, ElemTypeId);
end;

function TPaxCompiler.RegisterPointerType(LevelId: Integer;
                                          const TypeName: String;
                                          OriginTypeId: Integer;
                                          const OriginTypeName: String = ''): Integer;
begin
  result := kernel.SymbolTable.RegisterPointerType(LevelId,
    TypeName, OriginTypeId, OriginTypeName);
end;

function TPaxCompiler.RegisterMethodReferenceType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterMethodReferenceType(LevelId, TypeName, SubId);
end;

function TPaxCompiler.RegisterTypeDeclaration(LevelId: Integer;
                                      const Declaration: String): Integer;
begin
  result := kernel.SymbolTable.RegisterTypeDeclaration(LevelId, Declaration);
end;

function TPaxCompiler.RegisterSomeType(LevelId: Integer;
                           const TypeName: String): Integer;
begin
  result := kernel.SymbolTable.RegisterSomeType(LevelId, TypeName);
end;

function TPaxCompiler.RegisterSetType(LevelId: Integer;
                                      const TypeName: String;
                                      OriginTypeId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterSetType(LevelId, TypeName, OriginTypeId);
end;

function TPaxCompiler.RegisterProceduralType(LevelId: Integer;
                                             const TypeName: String;
                                             SubId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterProceduralType(LevelId, TypeName, SubId);
end;

{$IFNDEF PAXARM}
function TPaxCompiler.RegisterShortStringType(LevelId: Integer;
                                              const TypeName: String;
                                              L: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterShortStringType(LevelId, TypeName, L);
end;
{$ENDIF}

function TPaxCompiler.RegisterEventType(LevelId: Integer;
                                        const TypeName: String;
                                        SubId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterEventType(LevelId, TypeName, SubId);
end;

function TPaxCompiler.RegisterRTTIType(LevelId: Integer; pti: PTypeInfo): Integer;
begin
  result := kernel.SymbolTable.RegisterRTTIType(LevelId, pti);
end;

function TPaxCompiler.RegisterTypeAlias(LevelId:Integer; const TypeName: String;
                               OriginTypeId: Integer): Integer;
begin
  result := kernel.SymbolTable.RegisterTypeAlias(LevelId, TypeName, OriginTypeId);
end;

function TPaxCompiler.RegisterObject(LevelId: Integer;
                                     const ObjectName: String;
                                     TypeId: Integer;
                                     Address: Pointer = nil): Integer;
begin
  result := kernel.SymbolTable.RegisterObject(LevelId, ObjectName, TypeId, Address);
end;

function TPaxCompiler.RegisterVirtualObject(LevelId: Integer;
                                     const ObjectName: String): Integer;
begin
  result := kernel.SymbolTable.RegisterVirtualObject(LevelId, ObjectName);
end;

function TPaxCompiler.RegisterVariable(LevelId: Integer;
                                       const VarName: String;
                                       TypeId: Integer;
                                       Address: Pointer = nil): Integer;
begin
  result := kernel.SymbolTable.RegisterVariable(LevelId, VarName, TypeId, Address);
end;

function TPaxCompiler.RegisterVariable(LevelId: Integer;
                       const Declaration: String; Address: Pointer): Integer;
begin
  result := kernel.SymbolTable.RegisterVariable(LevelId, Declaration, Address);
end;

function TPaxCompiler.RegisterConstant(LevelId: Integer;
                                       const ConstName: String;
                                       typeID: Integer;
                                       const Value: Variant): Integer;
begin
  result := kernel.SymbolTable.RegisterConstant(LevelId, ConstName, TypeId, Value);
end;

function TPaxCompiler.RegisterConstant(LevelId: Integer;
                                       const ConstName: String;
                                       const Value: Variant): Integer;
begin
  result := kernel.SymbolTable.RegisterConstant(LevelId, ConstName, Value);
end;

function TPaxCompiler.RegisterPointerConstant(LevelId: Integer;
                                       const ConstName: String;
                                       const Value: Pointer): Integer;
begin
  result := kernel.SymbolTable.RegisterPointerConstant(LevelId, ConstName, Value);
end;

function TPaxCompiler.RegisterConstant(LevelId: Integer;
                                       const ConstName: String;
                                       const Value: Extended): Integer;
begin
  result := kernel.SymbolTable.RegisterExtendedConstant(LevelId, ConstName, Value);
end;

function TPaxCompiler.RegisterConstant(LevelId: Integer;
                                       const ConstName: String;
                                       const Value: Int64): Integer;
begin
  result := kernel.SymbolTable.RegisterInt64Constant(LevelId, ConstName, Value);
end;

function TPaxCompiler.RegisterConstant(LevelId: Integer;
                              const Declaration: String): Integer;
begin
  result := kernel.SymbolTable.RegisterConstant(LevelId, Declaration);
end;

function TPaxCompiler.RegisterRoutine(LevelId: Integer;
                                      const RoutineName: String; ResultTypeID: Integer;
                                      CallConvention: Integer;
                                      Address: Pointer = nil): Integer;
begin
  result := kernel.SymbolTable.RegisterRoutine(LevelId, RoutineName, ResultTypeId,
                                      CallConvention, Address);
end;

function TPaxCompiler.RegisterRoutine(LevelId: Integer; const Name: String;
                         ResultId: Integer;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterRoutine(LevelId, Name,
       ResultId, CallConvention, Address, OverCount, i_IsDeprecated);
end;

function TPaxCompiler.RegisterRoutine(LevelId: Integer; const Name, ResultType: String;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterRoutine(LevelId, Name, ResultType,
               CallConvention, Address, OverCount, i_IsDeprecated);
end;

function TPaxCompiler.RegisterMethod(LevelId: Integer;
                                     const RoutineName: String; ResultTypeID: Integer;
                                     CallConvention: Integer;
                                     Address: Pointer = nil;
                                     IsShared: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterMethod(LevelId, RoutineName, ResultTypeId,
                  CallConvention, Address, IsShared);
end;

function TPaxCompiler.RegisterMethod(ClassId: Integer;
                        const Name: String;
                        ResultId: Integer;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterMethod(ClassId,
                                             Name,
                                             ResultId,
                                             CallConvention,
                                             Address,
                                             IsShared,
                                             CallMode,
                                             MethodIndex,
                                             OverCount,
                                             i_IsAbstract,
                                             i_AbstractMethodCount,
                                             i_IsDeprecated);
end;

function TPaxCompiler.RegisterMethod(ClassId: Integer;
                        const Name, ResultType: String;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterMethod(ClassId,
                                             Name,
                                             ResultType,
                                             CallConvention,
                                             Address,
                                             IsShared,
                                             CallMode,
                                             MethodIndex,
                                             OverCount,
                                             i_IsAbstract,
                                             i_AbstractMethodCount,
                                             i_IsDeprecated);
end;

function TPaxCompiler.RegisterConstructor(ClassId: Integer;
                             const Name: String;
                             Address: Pointer;
                             CallConvention: Integer = _ccREGISTER;
                             IsShared: Boolean = false;
                             CallMode: Integer = 0;
                             i_MethodIndex: Integer = 0;
                             OverCount: Integer = 0;
                             i_IsAbstract: Boolean = false;
                             i_AbstractMethodCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterConstructor(ClassId,
     Name, Address, IsShared, CallMode, i_MethodIndex, OverCount,
     i_IsAbstract, i_AbstractMethodCount, i_IsDeprecated);
end;

function TPaxCompiler.RegisterDestructor(ClassId: Integer; const Name: String;
                             Address: Pointer): Integer;
begin
  result := kernel.SymbolTable.RegisterDestructor(ClassId, Name, Address);
end;

function TPaxCompiler.RegisterParameter(HSub: Integer; ParamTypeID: Integer;
                                        const DefaultValue: Variant;
                                        ByRef: Boolean = false): Integer;
begin
  result := kernel.SymbolTable.RegisterParameter(HSub, ParamTypeId, DefaultValue, ByRef);
end;

function TPaxCompiler.RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           ParamTypeID: Integer;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer;
begin
  result := kernel.SymbolTable.RegisterParameter(LevelId,
                                                ParameterName,
                                                ParamTypeId,
                                                ParamMod,
                                                Optional,
                                                DefaultValue);
end;

function TPaxCompiler.RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           const ParameterType: String;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer;
begin
  result := kernel.SymbolTable.RegisterParameter(LevelId,
                                                ParameterName,
                                                ParameterType,
                                                ParamMod,
                                                Optional,
                                                DefaultValue);
end;

function TPaxCompiler.RegisterHeader(LevelId: Integer; const Header: String;
                                     Address: Pointer = nil;
                                     MethodIndex: Integer = 0): Integer;
begin
  result := kernel.SymbolTable.RegisterHeader(LevelId, Header, Address, MethodIndex);
end;

function TPaxCompiler.RegisterFakeHeader(LevelId: Integer;
              const Header: String; Address: Pointer): Integer;
begin
  result := kernel.SymbolTable.RegisterFakeHeader(LevelId, Header, Address);
end;

function TPaxCompiler.GetHandle(LevelId: Integer; const MemberName: String; Upcase: Boolean): Integer;
begin
  result := kernel.GetHandle(LevelId, MemberName, Upcase);
end;

function TPaxCompiler.GetTargetPlatform: Integer;
begin
  result := Ord(kernel.TargetPlatform);
end;

procedure TPaxCompiler.SetTargetPlatform(value: Integer);
begin
  kernel.TargetPlatform := TTargetPlatform(value);
end;

function TPaxCompiler.GetErrorCount: Integer;
begin
  result := kernel.Errors.Count;
end;

function TPaxCompiler.GetErrorMessage(I: Integer): String;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].Message
  else
    result := '';
end;

function TPaxCompiler.GetErrorModuleName(I: Integer): String;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].ModuleName
  else
    result := '';
end;

function TPaxCompiler.GetErrorLine(I: Integer): String;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].SourceLine
  else
    result := '';
end;

function TPaxCompiler.GetErrorFileName(I: Integer): String;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].SourceFileName
  else
    result := '';
end;

function TPaxCompiler.GetErrorLineNumber(I: Integer): Integer;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].SourceLineNumber
  else
    result := 0;
end;

function TPaxCompiler.GetErrorLinePos(I: Integer): Integer;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := kernel.Errors[I].LinePos
  else
    result := 0;
end;

function TPaxCompiler.GetWarningCount: Integer;
begin
  result := kernel.Warnings.Count;
end;

function TPaxCompiler.GetWarningMessage(I: Integer): String;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].Message
  else
    result := '';
end;

function TPaxCompiler.GetWarningModuleName(I: Integer): String;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].ModuleName
  else
    result := '';
end;

function TPaxCompiler.GetWarningLine(I: Integer): String;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].SourceLine
  else
    result := '';
end;

function TPaxCompiler.GetWarningLineNumber(I: Integer): Integer;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].SourceLineNumber
  else
    result := 0;
end;

function TPaxCompiler.GetWarningLinePos(I: Integer): Integer;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].LinePos
  else
    result := 0;
end;

function TPaxCompiler.GetWarningFileName(I: Integer): String;
begin
  if (I >= 0) and (I < GetWarningCount) then
    result := kernel.Warnings[I].SourceFileName
  else
    result := '';
end;

function TPaxCompiler.GetOnCompilerProgress: TPaxCompilerNotifyEvent;
begin
  result := TPaxCompilerNotifyEvent(kernel.OnCompilerProgress);
end;

procedure TPaxCompiler.SetOnCompilerProgress(value: TPaxCompilerNotifyEvent);
begin
  kernel.OnCompilerProgress := TNotifyEvent(value);
end;

function TPaxCompiler.GetOnUsedUnit: TPaxCompilerUsedUnitEvent;
begin
  result := TPaxCompilerUsedUnitEvent(kernel.OnUsedUnit);
end;

procedure TPaxCompiler.SetOnUsedUnit(value: TPaxCompilerUsedUnitEvent);
begin
  kernel.OnUsedUnit := TUsedUnitEvent(value);
end;

function TPaxCompiler.GetOnImportUnit: TPaxCompilerImportMemberEvent;
begin
  result := TPaxCompilerImportMemberEvent(kernel.OnImportUnit);
end;

procedure TPaxCompiler.SetOnImportUnit(value: TPaxCompilerImportMemberEvent);
begin
  kernel.OnImportUnit := TImportMemberEvent(value);
end;

function TPaxCompiler.GetOnImportType: TPaxCompilerImportMemberEvent;
begin
  result := TPaxCompilerImportMemberEvent(kernel.OnImportType);
end;

procedure TPaxCompiler.SetOnImportType(value: TPaxCompilerImportMemberEvent);
begin
  kernel.OnImportType := TImportMemberEvent(value);
end;

function TPaxCompiler.GetOnImportGlobalMembers: TPaxCompilerNotifyEvent;
begin
  result := TPaxCompilerNotifyEvent(kernel.OnImportGlobalMembers);
end;

procedure TPaxCompiler.SetOnImportGlobalMembers(value: TPaxCompilerNotifyEvent);
begin
  kernel.OnImportGlobalMembers := TNotifyEvent(value);
end;

function TPaxCompiler.GetOnUnitAlias: TPaxCompilerUnitAliasEvent;
begin
  result := TPaxCompilerUnitAliasEvent(kernel.OnUnitAlias);
end;

procedure TPaxCompiler.SetOnUnitAlias(value: TPaxCompilerUnitAliasEvent);
begin
  kernel.OnUnitAlias := TUnitAliasEvent(value);
end;

function TPaxCompiler.GetOnSavePCU: TPaxCompilerSavePCUEvent;
begin
  result := TPaxCompilerSavePCUEvent(kernel.OnSavePCU);
end;

procedure TPaxCompiler.SetOnSavePCU(value: TPaxCompilerSavePCUEvent);
begin
  kernel.OnSavePCU := TSavePCUEvent(value);
end;

function TPaxCompiler.GetOnLoadPCU: TPaxCompilerLoadPCUEvent;
begin
  result := TPaxCompilerLoadPCUEvent(kernel.OnLoadPCU);
end;

procedure TPaxCompiler.SetOnLoadPCU(value: TPaxCompilerLoadPCUEvent);
begin
  kernel.OnLoadPCU := TLoadPCUEvent(value);
end;

function TPaxCompiler.GetOnInclude: TPaxCompilerIncludeEvent;
begin
  result := TPaxCompilerIncludeEvent(kernel.OnInclude);
end;

procedure TPaxCompiler.SetOnInclude(value: TPaxCompilerIncludeEvent);
begin
  kernel.OnInclude := TIncludeEvent(value);
end;

function TPaxCompiler.GetOnDefineDirective: TPaxCompilerDirectiveEvent;
begin
  result := TPaxCompilerDirectiveEvent(kernel.OnDefineDirective);
end;

procedure TPaxCompiler.SetOnDefineDirective(value: TPaxCompilerDirectiveEvent);
begin
  kernel.OnDefineDirective := TCompilerDirectiveEvent(value);
end;

function TPaxCompiler.GetOnUndefineDirective: TPaxCompilerDirectiveEvent;
begin
  result := TPaxCompilerDirectiveEvent(kernel.OnUndefineDirective);
end;

procedure TPaxCompiler.SetOnUndefineDirective(value: TPaxCompilerDirectiveEvent);
begin
  kernel.OnUndefineDirective := TCompilerDirectiveEvent(value);
end;

function TPaxCompiler.GetOnUnknownDirective: TPaxCompilerDirectiveEvent;
begin
  result := TPaxCompilerDirectiveEvent(kernel.OnUnknownDirective);
end;

procedure TPaxCompiler.SetOnUnknownDirective(value: TPaxCompilerDirectiveEvent);
begin
  kernel.OnUnknownDirective := TCompilerDirectiveEvent(value);
end;

function TPaxCompiler.GetSourceModule(const ModuleName: String): TStringList;
var
  I: Integer;
begin
  I := kernel.Modules.IndexOf(ModuleName);
  if I >= 0 then
    result := kernel.Modules[I].Lines
  else
    result := nil;
end;

function TPaxCompiler.GetCurrLineNumber: Integer;
begin
  result := kernel.Code.GetSourceLineNumber(kernel.Code.N);
end;

function TPaxCompiler.GetCurrModuleNumber: Integer;
begin
  result := kernel.Code.GetModuleNumber(kernel.Code.N);
end;

function TPaxCompiler.GetCurrModuleName: String;
begin
  result := kernel.Modules[CurrModuleNumber].Name;
end;

function TPaxCompiler.GetDebugMode: Boolean;
begin
  result := kernel.DEBUG_MODE;
end;

procedure TPaxCompiler.SetDebugMode(value: Boolean);
begin
  kernel.DEBUG_MODE := value;
end;

// added in v 1.5

function TPaxCompiler.GetKernelPtr: Pointer;
begin
  result := kernel;
end;

// added in v 1.6

procedure TPaxCompiler.RegisterGlobalJSObjects(var R: TJS_Record);
begin
  with kernel.SymbolTable do
  begin
    R.H_JS_Object := RegisterVariable(JS_JavaScriptNamespace,
                  'Object', JS_ObjectClassId, nil);
    R.Id_JS_Object := kernel.SymbolTable.Card;

    R.H_JS_Boolean := RegisterVariable(JS_JavaScriptNamespace,
                  'Boolean', JS_BooleanClassId, nil);
    R.Id_JS_Boolean := kernel.SymbolTable.Card;

    R.H_JS_String := RegisterVariable(JS_JavaScriptNamespace,
                  'String', JS_StringClassId, nil);
    R.Id_JS_String := kernel.SymbolTable.Card;

    R.H_JS_Number := RegisterVariable(JS_JavaScriptNamespace,
                  'Number', JS_NumberClassId, nil);
    R.Id_JS_Number := kernel.SymbolTable.Card;

    R.H_JS_Date := RegisterVariable(JS_JavaScriptNamespace,
                  'Date', JS_DateClassId, nil);
    R.Id_JS_Date := kernel.SymbolTable.Card;

    R.H_JS_Function := RegisterVariable(JS_JavaScriptNamespace,
                  'Function', JS_FunctionClassId, nil);
    R.Id_JS_Function := kernel.SymbolTable.Card;

    R.H_JS_Array := RegisterVariable(JS_JavaScriptNamespace,
                  'Array', JS_ArrayClassId, nil);
    R.Id_JS_Array := kernel.SymbolTable.Card;

    R.H_JS_RegExp := RegisterVariable(JS_JavaScriptNamespace,
                  'RegExp', JS_RegExpClassId, nil);
    R.Id_JS_RegExp := kernel.SymbolTable.Card;

    R.H_JS_Math := RegisterVariable(JS_JavaScriptNamespace,
                  'Math', JS_MathClassId, nil);
    R.Id_JS_Math := kernel.SymbolTable.Card;

    R.H_JS_Error := RegisterVariable(JS_JavaScriptNamespace,
                  'Error', JS_ErrorClassId, nil);
    R.Id_JS_Error := kernel.SymbolTable.Card;
  end;
end;

// added in v 1.9

function TPaxCompiler.GetCondDirectiveList: TStringList;
begin
  result := kernel.CondDirectiveList;
end;

function TPaxCompiler.GetAlignment: Integer;
begin
  result := kernel.Alignment;
end;

procedure TPaxCompiler.SetAlignment(value: Integer);
begin
  if not
     (
     (value = 1) or (value = 2) or
     (value = 4) or (value = 8)
     )
      then
        raise Exception.Create(Format(errInvalidAlignmentValue, [value]));

  kernel.Alignment := value;
end;

function TPaxCompiler.GetUndeclaredTypes: TStringList;
begin
  result := kernel.UndeclaredTypes;
end;

function TPaxCompiler.GetUndeclaredIdentifiers: TStringList;
begin
  result := kernel.UndeclaredIdents;
end;

function TPaxCompiler.GetCurrLanguage: String;
begin
  result := kernel.CurrLanguage;
end;

procedure TPaxCompiler.SetCurrLanguage(const value: String);
begin
  kernel.CurrLanguage := value;
end;

function TPaxCompiler.GetOnUndeclaredIdentifier: TPaxCompilerUndeclaredIdentifierEvent;
begin
  result := TPaxCompilerUndeclaredIdentifierEvent(TKernel(kernel).OnUndeclaredIdentifier);
end;

procedure TPaxCompiler.SetOnUndeclaredIdentifier(value: TPaxCompilerUndeclaredIdentifierEvent);
begin
  TKernel(kernel).OnUndeclaredIdentifier := TUndeclaredIdentifierEvent(value);
end;

function TPaxCompiler.GetOnComment: TPaxCompilerCommentEvent;
begin
  result := TPaxCompilerCommentEvent(TKernel(kernel).OnComment);
end;

procedure TPaxCompiler.SetOnComment(value: TPaxCompilerCommentEvent);
begin
  TKernel(kernel).OnComment := TCommentEvent(value);
end;

function TPaxCompiler.LookupId(const FullName: String; UpCase: Boolean = true): Integer;
begin
  if FullName = '' then
  begin
    result := 0;
    Exit;
  end;
  result := TKernel(kernel).SymbolTable.LookupFullName(FullName, UpCase);
end;

function TPaxCompiler.LookupTypeId(const TypeName: String): Integer;
begin
  result := TKernel(kernel).SymbolTable.LookupType(TypeName, true);
end;

function TPaxCompiler.LookupTypeNamespaceId(const TypeName: String): Integer;
var
  R: TSymbolRec;
  L, Id: Integer;
begin
  result := 0;
  Id := LookupTypeId(TypeName);
  if Id = 0 then
    Exit;

  L := TKernel(kernel).SymbolTable[Id].Level;

  repeat
    if L = 0 then
    begin
      result := 0;
      Exit;
    end;

    R := TKernel(kernel).SymbolTable[L];

    if R.Kind = kindNAMESPACE then
    begin
      result := R.Id;
      Exit;
    end;

    L := R.Level;

  until false;
end;

function TPaxCompiler.LookupNamespace(LevelId: Integer; const NamespaceName: String;
                         CaseSensitive: Boolean): Integer;
begin
  result := TKernel(kernel).SymbolTable.LookupNamespace(NamespaceName, LevelId, not CaseSensitive);
end;

function TPaxCompiler.LookupNamespace(const NamespaceName: String): Integer;
begin
  result := LookupNamespace(0, NamespaceName, true);
end;

function TPaxCompiler.GetNativeSEH: Boolean;
begin
  result := TKernel(kernel).ModeSEH;
end;

procedure TPaxCompiler.SetNativeSEH(const value: Boolean);
begin
  TKernel(kernel).ModeSEH := value;
end;

procedure TPaxCompiler.AssignImportTable(ImportTable: Pointer);
begin
  if ImportTable = nil then
     ImportTable := GlobalSymbolTable;
  TKernel(kernel).AssignImportTable(ImportTable);
end;

function TPaxCompiler.GetOnSavePCUFinished: TPaxCompilerSavePCUFinishedEvent; // jason
begin
  result := TPaxCompilerSavePCUFinishedEvent(kernel.OnSavePCUFinished);
end;

procedure TPaxCompiler.SetOnSavePCUFinished(value: TPaxCompilerSavePCUFinishedEvent); // jason
begin
  kernel.OnSavePCUFinished := TSavePCUFinishedEvent(value);
end;

function TPaxCompiler.GetOnLoadPCUFinished: TPaxCompilerLoadPCUFinishedEvent; // jason
begin
  result := TPaxCompilerLoadPCUFinishedEvent(kernel.OnLoadPCUFinished);
end;

procedure TPaxCompiler.SetOnLoadPCUFinished( value: TPaxCompilerLoadPCUFinishedEvent); // jason
begin
  kernel.OnLoadPCUFinished := TLoadPCUFinishedEvent(value);
end;

function TPaxCompiler.GetCompletionPrefix: String;
begin
  result := TKernel(kernel).CompletionPrefix;
end;

function TPaxCompiler.GetModuleName(Id: Integer): String;
var
  I: Integer;
begin
  result := '';
  if TKernel(kernel).SymbolTable[Id].Host then
  begin
    I := TKernel(kernel).SymbolTable[Id].Level;
    if I > 0 then
    begin
      if TKernel(kernel).SymbolTable[I].Kind = KindNAMESPACE then
      begin
        result := TKernel(kernel).SymbolTable[I].Name;
        Exit;
      end
      else
        result := GetModuleName(I);
    end;
  end;
  I := TKernel(kernel).Modules.IndexOfModuleById(Id);
  if I = -1 then
    Exit;
  result := TKernel(kernel).Modules[I].Name;
end;

function TPaxCompiler.GetPosition(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Position;
end;

function TPaxCompiler.GetKind(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Kind;
end;

function TPaxCompiler.GetUnicode: Boolean;
begin
  result := TKernel(kernel).IsUNIC;
end;

procedure TPaxCompiler.SetUnicode(value: Boolean);
begin
  TKernel(kernel).IsUNIC := value;
end;

{$ifdef DRTTI}
procedure TPaxCompiler.RegisterImportUnit(Level: Integer; const AUnitName: String);
begin
  TKernel(kernel).RegisterImportUnit(Level, AUnitName);
end;
{$endif}

function TPaxCompiler.GetEvalList: TStringList;
begin
  result := TKernel(kernel).EvalList;
end;

function TPaxCompiler.InScript(const IdentName: String): Boolean;
begin
  result := GetEvalList.IndexOf(IdentName) >= 0;
end;

procedure TPaxCompiler.ExtendAlphabet(B1, B2: Word);
begin
  kernel.ExAlphaList.Add(B1, B2);
end;

/////////////// TPaxCompilerLanguage ///////////////////////////////////////////

destructor TPaxCompilerLanguage.Destroy;
begin
  FreeAndNil(P);
  inherited;
end;

function TPaxCompilerLanguage.GetExplicitOff: Boolean;
begin
  result := P.EXPLICIT_OFF;
end;

procedure TPaxCompilerLanguage.SetExplicitOff(value: Boolean);
begin
  P.EXPLICIT_OFF := value;
end;

function TPaxCompilerLanguage.GetCompleteBooleanEval: Boolean;
begin
  result := P.CompleteBooleanEval;
end;

procedure TPaxCompilerLanguage.SetCompleteBooleanEval(value: Boolean);
begin
  P.CompleteBooleanEval := value;
end;

function TPaxCompilerLanguage.GetPrintKeyword: String;
begin
  result := P.PrintKeyword;
end;

function TPaxCompilerLanguage.GetPrintlnKeyword: String;
begin
  result := P.PrintlnKeyword;
end;

procedure TPaxCompilerLanguage.SetPrintKeyword(const value: String);
begin
  P.PrintKeyword := value;
  if P.ParsesModule then
    P.Gen(OP_PRINT_KWD, P.NewConst(typeSTRING, value), 0, 0);
end;

procedure TPaxCompilerLanguage.SetPrintlnKeyword(const value: String);
begin
  P.PrintlnKeyword := value;
  if P.ParsesModule then
    P.Gen(OP_PRINTLN_KWD, P.NewConst(typeSTRING, value), 0, 0);
end;

function TPaxCompilerLanguage.GetUnitLookup: Boolean;
begin
  result := P.UnitLookup;
end;

procedure TPaxCompilerLanguage.SetUnitLookup(value: Boolean);
begin
  P.UnitLookup := value;
end;

function TPaxCompilerLanguage.GetInitFuncResult: Boolean;
begin
  result := P.InitFuncResult;
end;

procedure TPaxCompilerLanguage.SetInitFuncResult(value: Boolean);
begin
  P.InitFuncResult := value;
end;

/////////////// TPaxPascalLanguage /////////////////////////////////////////////

constructor TPaxPascalLanguage.Create(AOwner: TComponent);
begin
  inherited;
  P := TPascalParser.Create;
  P.Owner := Self;
  SetCallConv(_ccREGISTER);
end;

procedure TPaxPascalLanguage.SetCallConv(CallConv: Integer);
begin
  P.CallConv := CallConv;
end;

function TPaxPascalLanguage.GetParser: TBaseParser;
begin
  result := P;
end;

function TPaxPascalLanguage.GetLanguageName: String;
begin
  result := P.LanguageName;
end;

function TPaxPascalLanguage.GetOnParseUnitName: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseUnitName);
end;

procedure TPaxPascalLanguage.SetOnParseUnitName(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseUnitName := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseImplementationSection: TPaxParserNotifyEvent;
begin
  result := TPaxParserNotifyEvent((P as TPascalParser).OnParseImplementationSection);
end;

procedure TPaxPascalLanguage.SetOnParseImplementationSection(value: TPaxParserNotifyEvent);
begin
  (P as TPascalParser).OnParseImplementationSection := TParserNotifyEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginUsedUnitList: TPaxParserNotifyEvent;
begin
  result := TPaxParserNotifyEvent((P as TPascalParser).OnParseBeginUsedUnitList);
end;

procedure TPaxPascalLanguage.SetOnParseBeginUsedUnitList(value: TPaxParserNotifyEvent);
begin
  (P as TPascalParser).OnParseBeginUsedUnitList := TParserNotifyEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndUsedUnitList: TPaxParserNotifyEvent;
begin
  result := TPaxParserNotifyEvent((P as TPascalParser).OnParseEndUsedUnitList);
end;

procedure TPaxPascalLanguage.SetOnParseEndUsedUnitList(value: TPaxParserNotifyEvent);
begin
  (P as TPascalParser).OnParseEndUsedUnitList := TParserNotifyEvent(value);
end;

function TPaxPascalLanguage.GetOnParseUsedUnitName: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseUsedUnitName);
end;

procedure TPaxPascalLanguage.SetOnParseUsedUnitName(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseUsedUnitName := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginClassTypeDeclaration: TPaxParserIdentEventEx;
begin
  result := TPaxParserIdentEventEx((P as TPascalParser).OnParseBeginClassTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginClassTypeDeclaration(value: TPaxParserIdentEventEx);
begin
  (P as TPascalParser).OnParseBeginClassTypeDeclaration := TParserIdentEventEx(value);
end;

function TPaxPascalLanguage.GetOnParseEndClassTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndClassTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndClassTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndClassTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseForwardTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseForwardTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseForwardTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseForwardTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseAncestorTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseAncestorTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseAncestorTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseAncestorTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseUsedInterface: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseUsedInterface);
end;

procedure TPaxPascalLanguage.SetOnParseUsedInterface(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseUsedInterface := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseClassReferenceTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseClassReferenceTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseClassReferenceTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseClassReferenceTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseAliasTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseAliasTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseAliasTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseAliasTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseProceduralTypeDeclaration: TPaxParserIdentEventEx;
begin
  result := TPaxParserIdentEventEx((P as TPascalParser).OnParseProceduralTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseProceduralTypeDeclaration(value: TPaxParserIdentEventEx);
begin
  (P as TPascalParser).OnParseProceduralTypeDeclaration := TParserIdentEventEx(value);
end;

function TPaxPascalLanguage.GetOnParseEventTypeDeclaration: TPaxParserIdentEventEx;
begin
  result := TPaxParserIdentEventEx((P as TPascalParser).OnParseEventTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEventTypeDeclaration(value: TPaxParserIdentEventEx);
begin
  (P as TPascalParser).OnParseEventTypeDeclaration := TParserIdentEventEx(value);
end;

function TPaxPascalLanguage.GetOnParseMethodReferenceTypeDeclaration: TPaxParserIdentEventEx;
begin
  result := TPaxParserIdentEventEx((P as TPascalParser).OnParseMethodReferenceTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseMethodReferenceTypeDeclaration(value: TPaxParserIdentEventEx);
begin
  (P as TPascalParser).OnParseMethodReferenceTypeDeclaration := TParserIdentEventEx(value);
end;


function TPaxPascalLanguage.GetOnParseSetTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseSetTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseSetTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseSetTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParsePointerTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParsePointerTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParsePointerTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParsePointerTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseArrayTypeDeclaration: TPaxParserArrayTypeEvent;
begin
  result := TPaxParserArrayTypeEvent((P as TPascalParser).OnParseArrayTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseArrayTypeDeclaration(value: TPaxParserArrayTypeEvent);
begin
  (P as TPascalParser).OnParseArrayTypeDeclaration := TParserArrayTypeEvent(value);
end;

function TPaxPascalLanguage.GetOnParseDynArrayTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseDynArrayTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseDynArrayTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseDynArrayTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseShortStringTypeDeclaration: TPaxParserNamedValueEvent;
begin
  result := TPaxParserNamedValueEvent((P as TPascalParser).OnParseShortStringTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseShortStringTypeDeclaration(value: TPaxParserNamedValueEvent);
begin
  (P as TPascalParser).OnParseShortStringTypeDeclaration := TParserNamedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseSubrangeTypeDeclaration: TPaxParserDeclarationEvent;
begin
  result := TPaxParserDeclarationEvent((P as TPascalParser).OnParseSubrangeTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseSubrangeTypeDeclaration(value: TPaxParserDeclarationEvent);
begin
  (P as TPascalParser).OnParseSubrangeTypeDeclaration := TParserDeclarationEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginRecordTypeDeclaration: TPaxParserIdentEventEx;
begin
  result := TPaxParserIdentEventEx((P as TPascalParser).OnParseBeginRecordTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginRecordTypeDeclaration(value: TPaxParserIdentEventEx);
begin
  (P as TPascalParser).OnParseBeginRecordTypeDeclaration := TParserIdentEventEx(value);
end;

function TPaxPascalLanguage.GetOnParseEndRecordTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndRecordTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndRecordTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndRecordTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginInterfaceTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseBeginInterfaceTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginInterfaceTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseBeginInterfaceTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndInterfaceTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndInterfaceTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndInterfaceTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndInterfaceTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginEnumTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseBeginEnumTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginEnumTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseBeginEnumTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndEnumTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndEnumTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndEnumTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndEnumTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEnumName: TPaxParserNamedValueEvent;
begin
  result := TPaxParserNamedValueEvent((P as TPascalParser).OnParseEnumName);
end;

procedure TPaxPascalLanguage.SetOnParseEnumName(value: TPaxParserNamedValueEvent);
begin
  (P as TPascalParser).OnParseEnumName := TParserNamedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseFieldDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseFieldDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseFieldDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseFieldDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseVariantRecordFieldDeclaration: TPaxParserVariantRecordFieldEvent;
begin
  result := TPaxParserVariantRecordFieldEvent((P as TPascalParser).OnParseVariantRecordFieldDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseVariantRecordFieldDeclaration(value: TPaxParserVariantRecordFieldEvent);
begin
  (P as TPascalParser).OnParseVariantRecordFieldDeclaration := TParserVariantRecordFieldEvent(value);
end;

function TPaxPascalLanguage.GetOnParsePropertyDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParsePropertyDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParsePropertyDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParsePropertyDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseConstantDeclaration: TPaxParserNamedValueEvent;
begin
  result := TPaxParserNamedValueEvent((P as TPascalParser).OnParseConstantDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseConstantDeclaration(value: TPaxParserNamedValueEvent);
begin
  (P as TPascalParser).OnParseConstantDeclaration := TParserNamedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseResourceStringDeclaration: TPaxParserNamedValueEvent;
begin
  result := TPaxParserNamedValueEvent((P as TPascalParser).OnParseResourceStringDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseResourceStringDeclaration(value: TPaxParserNamedValueEvent);
begin
  (P as TPascalParser).OnParseResourceStringDeclaration := TParserNamedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseTypedConstantDeclaration: TPaxParserNamedTypedValueEvent;
begin
  result := TPaxParserNamedTypedValueEvent((P as TPascalParser).OnParseTypedConstantDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseTypedConstantDeclaration(value: TPaxParserNamedTypedValueEvent);
begin
  (P as TPascalParser).OnParseTypedConstantDeclaration := TParserNamedTypedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseVariableDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseVariableDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseVariableDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseVariableDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginSubDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseBeginSubDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginSubDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseBeginSubDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndSubDeclaration: TPaxParserDeclarationEvent;
begin
  result := TPaxParserDeclarationEvent((P as TPascalParser).OnParseEndSubDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndSubDeclaration(value: TPaxParserDeclarationEvent);
begin
  (P as TPascalParser).OnParseEndSubDeclaration := TParserDeclarationEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginFormalParameterList: TPaxParserNotifyEvent;
begin
  result := TPaxParserNotifyEvent((P as TPascalParser).OnParseBeginFormalParameterList);
end;

procedure TPaxPascalLanguage.SetOnParseBeginFormalParameterList(value: TPaxParserNotifyEvent);
begin
  (P as TPascalParser).OnParseBeginFormalParameterList := TParserNotifyEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndFormalParameterList: TPaxParserNotifyEvent;
begin
  result := TPaxParserNotifyEvent((P as TPascalParser).OnParseEndFormalParameterList);
end;

procedure TPaxPascalLanguage.SetOnParseEndFormalParameterList(value: TPaxParserNotifyEvent);
begin
  (P as TPascalParser).OnParseEndFormalParameterList := TParserNotifyEvent(value);
end;

function TPaxPascalLanguage.GetOnParseFormalParameterDeclaration: TPaxParserNamedTypedValueEvent;
begin
  result := TPaxParserNamedTypedValueEvent((P as TPascalParser).OnParseFormalParameterDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseFormalParameterDeclaration(value: TPaxParserNamedTypedValueEvent);
begin
  (P as TPascalParser).OnParseFormalParameterDeclaration := TParserNamedTypedValueEvent(value);
end;

function TPaxPascalLanguage.GetOnParseResultType: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseResultType);
end;

procedure TPaxPascalLanguage.SetOnParseResultType(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseResultType := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginClassHelperTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseBeginClassHelperTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginClassHelperTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseBeginClassHelperTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndClassHelperTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndClassHelperTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndClassHelperTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndClassHelperTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseBeginRecordHelperTypeDeclaration: TPaxParserTypedIdentEvent;
begin
  result := TPaxParserTypedIdentEvent((P as TPascalParser).OnParseBeginRecordHelperTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseBeginRecordHelperTypeDeclaration(value: TPaxParserTypedIdentEvent);
begin
  (P as TPascalParser).OnParseBeginRecordHelperTypeDeclaration := TParserTypedIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseEndRecordHelperTypeDeclaration: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseEndRecordHelperTypeDeclaration);
end;

procedure TPaxPascalLanguage.SetOnParseEndRecordHelperTypeDeclaration(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseEndRecordHelperTypeDeclaration := TParserIdentEvent(value);
end;

function TPaxPascalLanguage.GetOnParseSubDirective: TPaxParserIdentEvent;
begin
  result := TPaxParserIdentEvent((P as TPascalParser).OnParseSubDirective);
end;

procedure TPaxPascalLanguage.SetOnParseSubDirective(value: TPaxParserIdentEvent);
begin
  (P as TPascalParser).OnParseSubDirective := TParserIdentEvent(value);
end;

procedure SetDump;
begin
  IsDump := true;
end;

end.




