////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_KERNEL.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_KERNEL;
interface
uses {$I uses.def}
  Classes,
  SysUtils,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_MODULE,
  PAXCOMP_SCANNER,
  PAXCOMP_PARSER,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_BYTECODE,
  PAXCOMP_ERROR,
  PAXCOMP_OFFSET,
  PAXCOMP_VAROBJECT,
  PAXCOMP_STDLIB,
  PAXCOMP_TYPEINFO,
  PAXCOMP_MAP,
  PAXCOMP_CLASSFACT,
  PAXCOMP_BASERUNNER,
  PAXCOMP_RTI,
{$ifdef DRTTI}
  RTTI,
  PAXCOMP_2010,
  PAXCOMP_2010REG,
{$endif}
  PAXCOMP_GENERIC;
type
  TWhatParse = (wpProgram, wpExpression, wpEvalExpression);

  TUsedUnitEvent = function (Sender: TObject; const AUnitName: String;
                             var SourceCode: String): Boolean of object;

  TImportMemberEvent = procedure (Sender: TObject;
                                  MemberId: Integer;
                                  const AMemberName: String) of object;

  TIncludeEvent = procedure (Sender: TObject; const FileName: String;
                              var Text: String) of object;

  TCompilerDirectiveEvent = procedure (Sender: TObject;
                                       const Directive: String;
                                       var ok: Boolean)
                                       of object;

  TUndeclaredIdentifierEvent = function (Sender: TObject;
                                         const IdentName: String;
                                         var Scope: String;
                                         var FullTypeName: String): boolean
                                         of object;

  TCommentEvent = procedure (Sender: TObject;
                             const Comment: String;
                             const Context: String;
                             CommentedTokens: TStrings) of object;

  TUnitAliasEvent = procedure (Sender: TObject;
                               var UnitName: String) of object;

  TKernel = class
  private
    fPAX64: Boolean;
    fTargetPlatform: TTargetPlatform;
    fSearchPathList: TStringList;
    function GetPAX64: Boolean;
    procedure SetPAX64(value: Boolean);
    procedure SetFieldOffsets;
    procedure RemoveTypes;
    function GetRootKernel: TKernel;
    function GetRootSearchPathList: TStringList;
    procedure SetupDefaultSettings;
    function GetRunnerKind: TRunnerKind;
    function GetTargetPlatform: TTargetPlatform;
    procedure SetTargetPlatform(value: TTargetPlatform);
    function GetSupportedSEH: Boolean;
  public
    IsUNIC: Boolean;
    ParserList: TParserList;
    SymbolTable: TSymbolTable;
    Code: TCode;
    Errors: TErrorList;
    Warnings: TErrorList;
    UndeclaredTypes: TUndeclaredTypeList;
    UndeclaredIdents: TUndeclaredIdentList;
    Modules: TModuleList;
    OffsetList: TOffsetList;
    ExportList: TExportList;
    HostClassListIds: TIntegerList;
    CondDirectiveList, ExternalSymList: TStringList;
    DefList: TDefList;
    ClassFactory: TPaxClassFactory;
    TypeInfoList: TPaxTypeInfoList;
    MessageList: TMessageList;
    UsedUnitList: TStringList;
    TypeDefList: TTypeDefList;
    EvalList: TStringList;

    Owner: TObject;

    TryCount: Integer;

    OnCompilerProgress: TNotifyEvent;
    OnUsedUnit: TUsedUnitEvent;
    OnImportUnit: TImportMemberEvent;
    OnImportType: TImportMemberEvent;
    OnImportGlobalMembers: TNotifyEvent;
    OnSavePCU: TSavePCUEvent;
    OnLoadPCU: TLoadPCUEvent;
    OnInclude: TIncludeEvent;
    OnSavePCUFinished: TSavePCUFinishedEvent; // jason
    OnLoadPCUFinished: TLoadPCUFinishedEvent; // jason

    OnDefineDirective: TCompilerDirectiveEvent;
    OnUndefineDirective: TCompilerDirectiveEvent;
    OnUnknownDirective: TCompilerDirectiveEvent;
    OnUndeclaredIdentifier: TUndeclaredIdentifierEvent;
    OnComment: TCommentEvent;
    OnUnitAlias: TUnitAliasEvent;

    DEBUG_MODE: Boolean;

    CurrParser: TBaseParser;

    IsConsoleApp: Boolean;
    Alignment: Integer;

    SignCompression: Boolean;

    Canceled: Boolean;
    CancelChar: Char;
    CompletionPrefix: String;

    SignCodeCompletion: Boolean;
    CompletionTarget: String;
    CompletionHasParsed: Boolean;

    InterfaceOnly: Boolean;
    ImportOnly: Boolean;

    UndeclaredIdentifiers: TStringList;
    TypeMap: TTypeMap;

    BuildAll: Boolean;
    BuildWithRuntimePackages: Boolean;
    BuildedUnits: TStringList;
    PCUStreamList: TStreamList;
    PCUOwner: Pointer;

    DefaultParser: TBaseParser;
    CurrLanguage: String;

    ModeSEH: Boolean;
    GT: TBaseSymbolTable;
    IsFramework: Boolean;

    FindDeclId: Integer;

    prog: TBaseRunner;

    AnonymousClassCount: Integer;

{$ifdef DRTTI}
    ImportUnitList: TUnitList;
{$endif}

    ExAlphaList: TAssocIntegers;

    constructor Create(i_Owner: TObject);
    destructor Destroy; override;
    function GetRunnerClass: TBaseRunnerClass;
    function AddModule(const Name, LanguageName: String;
                       IsPCU: Boolean = false): TModule;
    procedure AddCode(const ModuleName, Text: String);
    procedure AddCodeFromFile(const ModuleName, FileName: String);
    procedure RegisterParser(P: TBaseParser);
    function GetHandle(LevelId: Integer; const Name: String; Upcase: Boolean): Integer;
    procedure ParseModule(M: TModule; ModuleNumber: Integer;
                          What: TWhatParse = wpProgram;
                          eval: Pointer = nil);
    procedure Parse(CodeCompletion: Boolean = false;
                    const CancelModuleName: String = '';
                    X: Integer = -1;
                    Y: Integer = -1);
    procedure ParseCompletion(const CancelModuleName: String;
                              X: Integer;
                              Y: Integer);
    procedure ParseExpression(const Expression: String;
                              const LangName: String = '');
    procedure Link;
    function HasError: Boolean;
    procedure RaiseError(const Message: string; params: array of Const);
    procedure CreateError(const Message: string; params: array of Const);
    procedure CreateWarning(const Message: string; params: array of Const);
    function SourceLineToByteCodeLine(const ModuleName: String;
                                      SourceLine: Integer): Integer;
    function IsExecutableLine(const ModuleName: String;
                              SourceLine: Integer): Boolean;
    function FindFullPath(const FileName: String): String;
    function GetOffset(S: TSymbolRec): Integer;
    function ExistsOffset(S: TSymbolRec): Boolean;
    procedure Reset;
    procedure ResetCompilation;
    function GetTypeMapRec(Id: Integer): TTypeMapRec;
    procedure CopyRootEvents;
    procedure AssignImportTable(ImportTable: Pointer);
    procedure SetProg(AProg: TBaseRunner);
    procedure UpdateTypeInfos;
    procedure CompressHostClassList(HostMapTable: TMapTable);
    procedure CreateRTI(aprogram: TBaseRunner);
    procedure AllocateConstants(const PData: Pointer);
    function GetDestructorAddress: Pointer;
{$ifdef DRTTI}
    procedure RegisterImportUnit(Level: Integer; const AUnitName: String);
{$endif}
    property RootKernel: TKernel read GetRootKernel;
    property PAX64: Boolean read GetPAX64 write SetPAX64;
    property RootSearchPathList: TStringList read GetRootSearchPathList;
    property RunnerKind: TRunnerKind read GetRunnerKind;
    property TargetPlatform: TTargetPlatform read GetTargetPlatform write SetTargetPlatform;
    property SupportedSEH: Boolean read GetSupportedSEH;
  end;

function CheckProc(const TypeName: String; Data: Pointer;
                   errKind: TExternRecKind): Boolean;
var
  VarCheckProc: TCheckProc = CheckProc;

var
  CurrKernel: TKernel;

implementation

uses
  PAXCOMP_EVAL,
  PAXCOMP_PASCAL_PARSER;

constructor TKernel.Create(i_Owner: TObject);
begin
  FindAvailTypes;

  GT := GlobalSymbolTable;

  SetupDefaultSettings;

  Owner := i_Owner;
  OnCompilerProgress := nil;

  Errors := TErrorList.Create(Self);
  Warnings := TErrorList.Create(Self);

  UndeclaredTypes := TUndeclaredTypeList.Create;
  UndeclaredIdents := TUndeclaredIdentList.Create;

  SymbolTable := TSymbolTable.Create(Self);
  SymbolTable.Reset;

  Code := TCode.Create(Self);
  Code.Reset;

  Modules := TModuleList.Create(Self);
  Modules.Clear;

  ParserList := TParserList.Create(Self);
  DEBUG_MODE := false;

  CondDirectiveList := TStringList.Create;
  DefList := TDefList.Create;
  ExternalSymList := TStringList.Create;
  UsedUnitList := TStringList.Create;
  TypeDefList := TTypeDefList.Create;
  EvalList := TStringList.Create;

  OffsetList := nil;
  ExportList := nil;
  HostClassListIds := TIntegerList.Create;

  Alignment := GlobalAlignment;

  SignCompression := true;
  Canceled := false;
  SignCodeCompletion := false;
  InterfaceOnly := false;
  UndeclaredIdentifiers := TStringList.Create;
  BuildedUnits := TStringList.Create;
  PCUStreamList := TStreamList.Create;
  TypeMap := TTypeMap.Create;
  DefaultParser := TPascalParser.Create;

{$ifdef DRTTI}
  ImportUnitList := TUnitList.Create;
{$endif}
  fSearchPathList := TStringList.Create;

  ExAlphaList := TAssocIntegers.Create;
end;

destructor TKernel.Destroy;
begin
  FreeAndNil(SymbolTable);
  FreeAndNil(Code);
  FreeAndNil(Modules);

  FreeAndNil(Errors);
  FreeAndNil(Warnings);

  UndeclaredTypes.Reset;
  FreeAndNil(UndeclaredTypes);

  UndeclaredIdents.Reset;
  FreeAndNil(UndeclaredIdents);

  FreeAndNil(HostClassListIds);

  FreeAndNil(ParserList);

  FreeAndNil(DefList);
  FreeAndNil(CondDirectiveList);
  FreeAndNil(ExternalSymList);
  FreeAndNil(UsedUnitList);
  FreeAndNil(TypeDefList);
  FreeAndNil(EvalList);

  FreeAndNil(UndeclaredIdentifiers);
  FreeAndNil(BuildedUnits);
  FreeAndNil(PCUStreamList);

  FreeAndNil(TypeMap);
  FreeAndNil(DefaultParser);

{$ifdef DRTTI}
  FreeAndNil(ImportUnitList);
{$endif}
  FreeAndNil(fSearchPathList);

  FreeAndNil(ExAlphaList);

  inherited;
end;

procedure TKernel.Reset;
begin
  CompletionTarget := '';

  SetupDefaultSettings;

  SymbolTable.Reset;
  Code.Reset;
  Errors.Reset;
  Warnings.Reset;
  UndeclaredTypes.Reset;
  UndeclaredIdents.Reset;
  HostClassListIds.Clear;
  Modules.Clear;
  ParserList.Clear;
  DefList.Clear;
  UsedUnitList.Clear;
  TypeDefList.Clear;
  EvalList.Clear;

  TryCount := 0;
  Canceled := false;
  SignCodeCompletion := false;
  InterfaceOnly := false;
  UndeclaredIdentifiers.Clear;
  BuildedUnits.Clear;
  PCUStreamList.Clear;
  ExternalSymList.Clear;
  TypeMap.Clear;
  PCUOwner := nil;

  AnonymousClassCount := 0;

{$ifdef DRTTI}
  ImportUnitList.Clear;
{$endif}
  fSearchPathList.Clear;

  ExAlphaList.Clear;
end;

procedure TKernel.ResetCompilation;
begin
  CompletionTarget := '';

  if Code.Card = 0 then
    SymbolTable.CompileCard := SymbolTable.Card
  else
    SymbolTable.ResetCompilation;
  Code.Reset;
  Errors.Reset;
  Warnings.Reset;
  UndeclaredTypes.Reset;
  UndeclaredIdents.Reset;
  HostClassListIds.Clear;
  Modules.Clear;
  DefList.Clear;
  UsedUnitList.Clear;
  TypeDefList.Clear;
  EvalList.Clear;
  TryCount := 0;
  Canceled := false;
  InterfaceOnly := false;
  UndeclaredIdentifiers.Clear;
  ExternalSymList.Clear;
  TypeMap.Clear;
  PCUOwner := nil;

  AnonymousClassCount := 0;

{$ifdef DRTTI}
  ImportUnitList.Clear;
{$endif}
end;

function TKernel.AddModule(const Name, LanguageName: String;
                           IsPCU: Boolean = false): TModule;
var
  I: Integer;
  Found: Boolean;
begin
  if Modules.Count = 0 then
  begin
    SymbolTable.CompileCard := SymbolTable.Card;
    CurrLanguage := LanguageName;
  end;

  Found := false;
  for I := 0 to ParserList.Count - 1 do
    if StrEql(LanguageName, ParserList[I].LanguageName) then
      Found := true;

  if not Found then
  begin
    if StrEql(LanguageName, 'Pascal') then
    begin
      RegisterParser(DefaultParser);
    end
    else
      RaiseError(errUnregisteredLanguage, [LanguageName]);
  end;

  result := Modules.AddModule(Name, LanguageName);
  result.IsPCU := IsPCU;
end;

procedure TKernel.AddCode(const ModuleName, Text: String);
var
  I: Integer;
begin
  I := Modules.IndexOf(ModuleName);
  if I = -1 then
    RaiseError(errModuleNotFound, [ModuleName]);
  Modules[I].Lines.Text := Modules[I].Lines.Text + Text;
end;

procedure TKernel.AddCodeFromFile(const ModuleName, FileName: String);
var
  L: TStringList;
  S, FullPath: String;
  I: Integer;
{$IFDEF MACOS}
  J: Integer;
{$ENDIF}
begin
  I := Modules.IndexOf(ModuleName);
  if I = -1 then
    RaiseError(errModuleNotFound, [ModuleName]);

  FullPath := FindFullPath(FileName);
  if not FileExists(FullPath) then
    RaiseError(errFileNotFound, [FileName]);
  L := TStringList.Create;
  try
    L.LoadFromFile(FullPath);

    S := L.Text;
{$IFDEF MACOS}
    for J := SLow(S) to SHigh(S) do
      if (Ord(S[J]) = 8220) or (Ord(S[J]) = 8221) then
        S[J] := '"';
{$ENDIF}

    Modules[I].Lines.Text := Modules[I].Lines.Text + S;
    Modules[I].FileName := FullPath;
  finally
    FreeAndNil(L);
  end;
end;

procedure TKernel.RegisterParser(P: TBaseParser);
begin
  ParserList.AddParser(P);
end;

function TKernel.GetHandle(LevelId: Integer; const Name: String; Upcase: Boolean): Integer;
var
  id, I, P: Integer;
  Ok: Boolean;
  S: String;
begin
  P := 0;
  for I:= Length(Name) downto 1 do
    if Name[I] = '.' then
    begin
      P := I;
      break;
    end;

  if P > 0 then
  begin
    S := Copy(Name, 1, P - 1);
    LevelId := GetHandle(LevelId, S, Upcase);
    if LevelId = 0 then
    begin
      result := 0;
      Exit;
    end;
    S := Copy(Name, P + 1, Length(Name) - P);
  end
  else
    S := Name;

  id := 0;

  with SymbolTable do
  for I := Card downto 1 do
  begin
    if (Records[I].Level = LevelId) and
       (Records[I].OwnerId = 0) and
       (Records[I].Kind <> KindNONE) then
    begin
      if UpCase then
        ok := StrEql(Records[I].Name, S)
      else
        ok := Records[I].Name = S;
      if ok then
      begin
        id := I;
        break;
      end;
    end;
  end;

  if id > 0 then
  begin
    if SymbolTable[id].Kind in KindSUBs then
      result := - SymbolTable[id].Value
    else if SymbolTable[id].Kind in [KindNAMESPACE, KindTYPE] then
      result := id
    else
      result := SymbolTable[id].Shift;
  end
  else
    result := 0;
end;

procedure TKernel.ParseModule(M: TModule; ModuleNumber: Integer;
                              What: TWhatParse = wpProgram;
                              eval: Pointer = nil);
var
  LanguageNamespaceId: Integer;
  L, I, J, Id, ExitLabelId, CurrSelfId, CurrN: Integer;
  parser: TBaseParser;
  R: TSymbolRec;
  ExtraNamespaceList: TStringList;
begin
  UsedUnitList.Clear;

  if eval = nil then
    ExtraNamespaceList := nil
  else
    ExtraNamespaceList := TEval(eval).NamespaceList;

  Canceled := false;
//  DefList.Clear;
  for I := 0 to CondDirectiveList.Count - 1 do
    DefList.Add(CondDirectiveList[I]);

  parser := ParserList.FindParser(M.LanguageName);

  if parser = nil then
    RaiseError(errLanguageNotRegistered, [M.LanguageName]);

  CurrParser := parser;

  M.ModuleNumber := ModuleNumber;

  CurrLanguage := M.LanguageName;

  parser.Init(Self, M);

  ExitLabelId := 0;
  L := 0;
  LanguageNamespaceId := 0;

  try
    parser.ParsesModule := true;

    try
      parser.Gen(OP_BEGIN_MODULE, ModuleNumber, parser.LanguageId, Integer(parser.UpCase));
      parser.Gen(OP_SEPARATOR, ModuleNumber, 0, 0);

      LanguageNamespaceId := SymbolTable.LookUp(M.LanguageName + 'Namespace', 0, true);
      if LanguageNamespaceId = 0 then
        LanguageNamespaceId := H_PascalNamespace;
  //      LanguageNamespaceId := SymbolTable.RegisterNamespace(0, M.LanguageName + 'Namespace');

      IsFramework := false;

{$IFDEF EXPLICIT_OFF}
      parser.EXPLICIT_OFF := true;
      parser.Gen(OP_OPTION_EXPLICIT, 0, 0, 0);
{$ENDIF}

      parser.Gen(OP_WARNINGS_ON, 0, 0, 0);
      parser.Gen(OP_BEGIN_USING, LanguageNamespaceId, 0, 0);
      parser.Gen(OP_BEGIN_USING, 0, 0, 0);

      if ExtraNamespaceList <> nil then
      begin
        for I := 0 to ExtraNamespaceList.Count - 1 do
        begin
          Id := SymbolTable.LookupFullName(ExtraNamespaceList[I], parser.UpCase);
          if Id > 0 then
            parser.Gen(OP_BEGIN_USING, Id, 0, 0);
        end;
      end;

      L := parser.NewLabel;
      parser.SkipLabelStack.Push(L);

      ExitLabelId := parser.NewLabel;
      parser.ExitLabelStack.Push(L);

      case What of
        wpProgram:
        begin
          parser.ParseProgram;
        end;
        wpExpression:
        begin
          parser.Gen(OP_END_INTERFACE_SECTION, 0, 0, 0);
          parser.DECLARE_SWITCH := false;
          parser.Call_SCANNER;
          parser.Gen(OP_ASSIGN, SymbolTable.ResultId, parser.Parse_Expression, SymbolTable.ResultId);
        end;
        wpEvalExpression:
        begin
          R := SymbolTable.AddRecord;
          R.Kind := KindVAR;
          R.Name := StrExprResult;
          R.Level := 0;

          if eval <> nil then
            TEval(eval).ResultId := SymbolTable.Card;

          CurrN := TBaseRunner(TEval(eval).SProg).CurrN;
          if CurrN > 0 then
            CurrSelfId := TKernel(TEval(eval).SKernel).Code.GetCurrSelfId(CurrN)
          else
            CurrSelfId := 0;

          parser.Gen(OP_END_INTERFACE_SECTION, 0, 0, 0);
          if CurrSelfId > 0 then
          begin
            Parser.Gen(OP_BEGIN_WITH, CurrSelfId, 0, 0);
            Parser.WithStack.Push(CurrSelfId);
          end;

          parser.DECLARE_SWITCH := false;
          parser.Call_SCANNER;
          parser.Gen(OP_ASSIGN, R.Id, parser.Parse_Expression, R.Id);

          if CurrSelfId > 0 then
          begin
            Parser.Gen(OP_END_WITH, CurrSelfId, 0, 0);
            Parser.WithStack.Pop;
          end;
        end;
      else
        parser.ParseProgram;
      end;
    except
      on E: Exception do
      begin
        if E is PaxCompilerException then
        begin
          // already has been processed
        end
        else if E is PaxCancelException then
        begin
          if CompletionHasParsed then
          begin
            Errors.Reset;
            Warnings.Reset;
          end;
          Canceled := true;
        end
        else
        begin
          CreateError(E.Message, []);
        end;
      end;
    end;

  finally
    CurrParser := nil;

    parser.SkipLabelStack.Pop;
    parser.SetLabelHere(L);

    parser.ExitLabelStack.Pop;
    parser.SetLabelHere(ExitLabelId);

    parser.Gen(OP_END_USING, LanguageNamespaceId, 0, 0);
    parser.Gen(OP_END_USING, 0, 0, 0);
//    parser.Gen(OP_RET, 0, 0, 0);
    parser.Gen(OP_END_MODULE, ModuleNumber, 0, 0);

    IsConsoleApp := parser.IsConsoleApp;
    if Assigned(prog) then
      prog.Console := IsConsoleApp;

    parser.ParsesModule := false;
  end;

  if parser.InterfaceOnly then
    Exit;

  if Canceled then
  begin
    Parser.Gen(OP_NOP, 0, 0, 0);
    Parser.Gen(OP_NOP, 0, 0, 0);
    Parser.Gen(OP_NOP, 0, 0, 0);
    Parser.Gen(OP_NOP, 0, 0, 0);
    Exit;
  end;

  if What = wpExpression then
    Exit;
  if What = wpEvalExpression then
    Exit;
  if SignCodeCompletion then
    Exit;

  if not StrEql(M.LanguageName, 'C') then
    for I:=FirstLocalId + 1 to SymbolTable.Card do
    begin
      if SymbolTable[I].IsForward then
      begin
        for J := 1 to Code.Card do
          if Code[J].Op = OP_BEGIN_SUB then
            if Code[J].Arg1 = I then
            begin
              Code.N := J;
              break;
            end;

        CreateError(errUnsatisfiedForwardOrExternalDeclaration, [SymbolTable[I].Name]);
      end;
    end;

  if parser.scanner.DefStack.Count > 0 then
    CreateError(errMissingENDIFdirective, ['']);
end;

procedure TKernel.Parse(CodeCompletion: Boolean = false;
                        const CancelModuleName: String = '';
                        X: Integer = -1;
                        Y: Integer = -1);
var
  I, J, CancelPos: Integer;
  Temp: Boolean;
  ExtraModuleName, ExtraCode: String;
begin
  CompletionHasParsed := false;
  SignCodeCompletion := CodeCompletion;
  Temp := false;

  code.Reset;
  SymbolTable.CompileCard := SymbolTable.Card;
  I := 0;
  while (I < Modules.Count) do
  begin

    if CodeCompletion and StrEql(CancelModuleName, Modules[I].Name) then
    begin
      CancelPos := Modules.GetPos(CancelModuleName, X, Y);
      if CancelPos = -1 then
         Exit;
      Modules[I].CancelPos := CancelPos;
    end;

    Modules[I].State := msCompiling;
    if not Modules[I].SkipParsing then
    begin
      ParseModule(Modules[I], I);
    end;

    Modules[I].State := msCompiled;

    Inc(I);

    if Canceled then
    begin
      Temp := True;
      if I >= Modules.Count then
        break;
    end;
  end;

  Canceled := Temp;

  for I := Modules.Count - 1 downto 0 do
    if Modules[I].SkipParsing then
      Modules.Delete(Modules[I]);

  if ImportOnly then
    Exit;

  RemoveTypes;

  TypeDefList.GenPascalUnits;
  for J := 0 to TypeDefList.TypeModuleList.Count - 1 do
  if TypeDefList.TypeModuleList[J].LangId = PASCAL_LANGUAGE then
  if TypeDefList.TypeModuleList[J].Success then
  begin
    ExtraModuleName := strExtraPascalUnit + TypeDefList.TypeModuleList[J].ModuleName;
    ExtraCode := TypeDefList.TypeModuleList[J].Source;
    AddModule(ExtraModuleName, 'Pascal');
    AddCode(ExtraModuleName, ExtraCode);
    I := Modules.Count - 1;
    Modules[I].IsExtra := true;
    Modules[I].State := msCompiling;
    ParseModule(Modules[I], I);
    Modules[I].State := msCompiled;
  end;

  TypeDefList.GenBasicUnits;
  for J := 0 to TypeDefList.TypeModuleList.Count - 1 do
  if TypeDefList.TypeModuleList[J].LangId = BASIC_LANGUAGE then
  if TypeDefList.TypeModuleList[J].Success then
  begin
    ExtraModuleName := strExtraBasicUnit + TypeDefList.TypeModuleList[J].ModuleName;
    ExtraCode := TypeDefList.TypeModuleList[J].Source;
    AddModule(ExtraModuleName, 'Basic');
    AddCode(ExtraModuleName, ExtraCode);
    I := Modules.Count - 1;
    Modules[I].IsExtra := true;
    Modules[I].State := msCompiling;
    ParseModule(Modules[I], I);
    Modules[I].State := msCompiled;
  end;

  TypeDefList.GenJavaUnits;
  for J := 0 to TypeDefList.TypeModuleList.Count - 1 do
  if TypeDefList.TypeModuleList[J].LangId = JAVA_LANGUAGE then
  if TypeDefList.TypeModuleList[J].Success then
  begin
    ExtraModuleName := strExtraJavaUnit + TypeDefList.TypeModuleList[J].ModuleName;
    ExtraCode := TypeDefList.TypeModuleList[J].Source;
    AddModule(ExtraModuleName, 'Java');
    AddCode(ExtraModuleName, ExtraCode);
    I := Modules.Count - 1;
    Modules[I].IsExtra := true;
    Modules[I].State := msCompiling;
    ParseModule(Modules[I], I);
    Modules[I].State := msCompiled;
  end;
end;

procedure TKernel.ParseCompletion(const CancelModuleName: String;
                                  X: Integer;
                                  Y: Integer);
var
  I, J, J1, CancelPos, Card1, Op: Integer;
  S: String;
  ActiveUnitList: TStringList;
begin
  CompletionHasParsed := false;
  SignCodeCompletion := true;

  code.Reset;
  SymbolTable.CompileCard := SymbolTable.Card;
  I := Modules.IndexOf(CancelModuleName);
  if I = -1 then
    Exit;
  CancelPos := Modules.GetPos(CancelModuleName, X, Y);
  if CancelPos = -1 then
    Exit;
  Modules[I].CancelPos := CancelPos;
  Modules[I].State := msCompiling;
  ParseModule(Modules[I], I);
  Modules[I].State := msCompiled;
  Canceled := false;

  Card1 := Code.Card;
  InterfaceOnly := true;

  ActiveUnitList := TStringList.Create;
  try
    for J1 := 0 to UsedUnitList.Count - 1 do
    begin
      S := UpperCase(UsedUnitList[J1]);
      ActiveUnitList.Add(S);
    end;

    J := 0;
    while J < ActiveUnitList.Count do
    begin
      S := ActiveUnitList[J];
      I := Modules.IndexOf(S);
      if I >= 0 then
      begin
        Modules[I].State := msCompiling;
        ParseModule(Modules[I], I);
        Modules[I].State := msCompiled;

        for J1 := 0 to UsedUnitList.Count - 1 do
        begin
          S := UpperCase(UsedUnitList[J1]);
          if ActiveUnitList.IndexOf(S) = -1 then
            ActiveUnitList.Add(S);
        end;
      end;

      Inc(J);
    end;

  finally
    FreeAndNil(ActiveUnitList);
    InterfaceOnly := false;
  end;

  for I := Code.Card downto Card1 do
  begin
    Op := Code[I].Op;
    if (Op <> OP_EVAL) and
       (Op <> OP_BEGIN_MODULE) and
       (Op <> OP_END_MODULE) and
       (Op <> OP_BEGIN_USING) and
       (Op <> OP_ASSIGN_CONST) and
       (Op <> OP_ADD_ANCESTOR) and
       (Op <> OP_ASSIGN_TYPE) then
    begin
      Code.DeleteRecord(I);
    end;
  end;
end;

procedure TKernel.ParseExpression(const Expression: String;
                                  const LangName: String = '');
var
  M: TModule;
begin
  if LangName = '' then
    M := AddModule('$', 'Pascal')
  else
    M := AddModule('$', LangName);
  AddCode('$', Expression);
  ParseModule(M, 0, wpExpression);
end;

procedure TKernel.RaiseError(const Message: string; params: array of Const);
begin
  CreateError(Message, params);
  if SignCodeCompletion then
    raise PaxCancelException.Create(Format(Message, params))
  else
    raise PaxCompilerException.Create(Format(Message, params));
end;

procedure TKernel.CreateError(const Message: string; params: array of Const);
var
  E: TError;
  S: String;
begin
  dmp;
  S := Format(Message, params);
  E := TError.Create(Self, S);
  Errors.Add(E);
end;

procedure TKernel.CreateWarning(const Message: string; params: array of Const);
var
  E: TError;
begin
  dmp;
  E := TError.Create(Self, Format(Message, params));
  Warnings.Add(E);
end;

function TKernel.HasError: Boolean;
begin
  result := Errors.Count > 0;
end;

function CheckProc(const TypeName: String; Data: Pointer; errKind: TExternRecKind): Boolean;
var
  Code: TCode;
  CodeRec: TCodeRec;
  SymbolTable: TSymbolTable;
  I, Id, K: Integer;
  S: String;
begin
  S := ExtractName(TypeName);
  result := false;
  Code := TKernel(Data).Code;
  SymbolTable := TKernel(Data).SymbolTable;
  K := SymbolTable.Card;
  for I := 1 to Code.Card do
  begin
    CodeRec := Code[I];
    Id := CodeRec.Arg1;
    if (Id > StdCard) and (Id <= K) then
      if StrEql(S, SymbolTable[Id].Name) then
      begin
        Code.N := I;
        result := true;
        Exit;
      end;
    Id := CodeRec.Arg2;
    if (Id > StdCard) and (Id <= K) then
      if StrEql(S, SymbolTable[Id].Name) then
      begin
        Code.N := I;
        result := true;
        Exit;
      end;
    Id := CodeRec.Res;
    if (Id > StdCard) and (Id <= K) then
      if StrEql(S, SymbolTable[Id].Name) then
      begin
        Code.N := I;
        result := true;
        Exit;
      end;
  end;
end;

procedure TKernel.Link;
{$ifdef DRTTI}
var
  Q: TStringList;
  I, Id: Integer;
  AUnitName: String;
{$endif}
begin
  CurrProg := Prog;
  SymbolTable.LinkCard := SymbolTable.CompileCard;

  try
{$ifdef DRTTI}

    if ImportUnitList.Count > 0 then
    begin
      dmp;

      Q := TStringList.Create;
      try
        Code.CreateEvalList(EvalList);
        if EvalList.Count > 0 then
        begin
          for I := 0 to ImportUnitList.Count - 1 do
            Q.Add(ImportUnitList[I].Name);
          ImportUnitList.FindMembers(EvalList);

          ImportUnitList.Sort;

          if IsDump then
            ImportUnitList.Dump('simp_units.txt');

          for I := 0 to ImportUnitList.Count - 1 do
          begin
            Id := RegisterUnit(ImportUnitList[I], SymbolTable, EvalList, Self);
            if Assigned(OnImportUnit) then
            begin
              AUnitName := ImportUnitList[I].Name;
              if Q.IndexOf(AUnitName) >= 0 then
              begin
                try
                  SymbolTable.HeaderParser.kernel := Self;
                  SymbolTable.HeaderParser.CurrImportUnit := AUnitName;
                  OnImportUnit(Owner, Id, AUnitName);
                finally
                  SymbolTable.HeaderParser.CurrImportUnit := '';
                  SymbolTable.HeaderParser.kernel := nil;
                end;
              end;
            end;
          end;

          if Assigned(OnImportGlobalMembers) then
            OnImportGlobalMembers(Owner);

          SymbolTable.LinkCard := SymbolTable.Card;
        end;

      finally
        FreeAndNil(Q);
      end;
    end;

{$endif}
    dmp;

    SymbolTable.Update;
    try
      SymbolTable.ResolveExternList(VarCheckProc, Self);
    except
      on E: Exception do
      begin
        CreateError(E.Message, []);
      end;
    end;

    if HasError then Exit;
    code.RemoveEvalOpForTypes;

    if HasError then Exit;
    code.ProcessImplements;

    if HasError then Exit;
    code.RemoveEvalOp;

    if HasError then Exit;

    if HasError then Exit;
    if not Canceled then
    code.GenHostStructConst;

    if HasError then Exit;
    if not Canceled then
    code.UpdateDefaultConstructors;

    if HasError then Exit;
    modules.CreateLoadOrder;

    if HasError then Exit;
    code.CheckOverride;

    if HasError then Exit;
    code.CheckTypes;

    if Canceled then
    begin
      Exit;
    end;

    if InterfaceOnly then
    begin
      Exit;
    end;

    if SignCodeCompletion then
    begin
      Exit;
    end;

    code.CheckExpansions;

    if HasError then Exit;
    code.AddWarnings;

    if HasError then Exit;
    code.InsertDynamicTypeDestructors;

    if HasError then Exit;
    code.InsertFinalizators;

    if HasError then Exit;
    code.InsertTryFinally;

    if HasError then Exit;

    try
      SymbolTable.SetShifts(prog);
    except
      on E: Exception do
        CreateError(E.Message, []);
    end;
    UpdateTypeInfos;

    if HasError then Exit;
    SetFieldOffsets;

    if HasError then Exit;
    code.ProcessSizeOf;

    if HasError then Exit;
    code.ChangeOrderOfActualParams;

    if HasError then Exit;
    code.AssignShifts;

    if HasError then Exit;
    code.RemoveLoadProc;

    if HasError then Exit;
    code.InsertHostMonitoring;

    if HasError then Exit;
    code.Optimization;

    if HasError then Exit;
    code.AdjustTryList;

    if HasError then Exit;
    if DEBUG_MODE then
      Code.InsertCallHostEvents;

    Code.SetLastCondRaise;

    dmp;

  except
  end;
end;

function TKernel.SourceLineToByteCodeLine(const ModuleName: String;
                                          SourceLine: Integer): Integer;
var
  I: Integer;
  M: TModule;
begin
  result := 0;
  I := Modules.IndexOf(ModuleName);
  if I = -1 then
    Exit;
  M := Modules[I];
  for I:=M.P1 to M.P3 do
    if Code[I].OP = OP_SEPARATOR then
      if Code[I].Arg2 = SourceLine then
      begin
        result := I;
        Exit;
      end;
end;

function TKernel.IsExecutableLine(const ModuleName: String;
                                  SourceLine: Integer): Boolean;
var
  I, J: Integer;
  M: TModule;
begin
  result := false;
  I := Modules.IndexOf(ModuleName);
  if I = -1 then
    Exit;
  M := Modules[I];
  for I:=M.P1 to M.P3 do
    if Code[I].OP = OP_SEPARATOR then
      if Code[I].Arg2 = SourceLine then
      begin
        J := I;
        repeat
          Inc(J);

          if J > Code.Card then
            Exit;
          if Code[J].Op = OP_SEPARATOR then
            Exit;

          if Code[J].Op = OP_SET_CODE_LINE then
          begin
            result := true;
            Exit;
          end;
        until false;

        Exit;
      end;
end;

function TKernel.GetOffset(S: TSymbolRec): Integer;
var
  Shift: Integer;
begin
  if SignCompression then
  begin
    Shift := S.Shift;

    if (Shift <= 0) or (S.Kind = kindTYPE_FIELD) then
    begin
      result := Shift;
      Exit;
    end
    else if PAX64 then
    begin
      if S.Param or S.Local then
      begin
        result := Shift;
        Exit;
      end;
    end;

    result := OffsetList.GetOffset(Shift);

    if result = -1 then
      RaiseError(errInternalError, []);
  end
  else
    result := S.Shift;
end;

function TKernel.ExistsOffset(S: TSymbolRec): Boolean; //18.09.2009
var
  Shift: Integer;
begin
  if SignCompression then
  begin
    Shift := S.Shift;

    if (Shift <= 0) or (S.Kind = kindTYPE_FIELD) then
    begin
      result := true;
      Exit;
    end;

    result := OffsetList.GetOffset(Shift) <> - 1;
  end
  else
    result := true;
end;

function TKernel.GetTypeMapRec(Id: Integer): TTypeMapRec;
var
  T: Integer;
begin
  result := nil;

  T := SymbolTable[Id].FinalTypeId;
  if not (T in [typeRECORD, typeCLASS]) then
    Exit;

  T := SymbolTable[Id].TerminalTypeId;
  result := TypeMap.Lookup(T);
  if result = nil then
    result := TypeMap.Add(T);
  if not result.Completed then
  begin
    result.Fields.Clear;
    SymbolTable.GetFieldCount(Id, result);
    result.Completed := true;
  end;
end;

function TKernel.GetRootKernel: TKernel;
begin
  result := Self;
  while result.PCUOwner <> nil do
    result := result.PCUOwner;
end;

procedure TKernel.CopyRootEvents;
var
  RK: TKernel;
begin
  RK := RootKernel;
  if Self <> RK then
  begin
    Owner := RK.Owner;

    OnCompilerProgress := RK.OnCompilerProgress;
    OnUsedUnit := RK.OnUsedUnit;
    OnSavePCU := RK.OnSavePCU;
    OnLoadPCU := RK.OnLoadPCU;
    OnInclude := RK.OnInclude;
    OnDefineDirective := RK.OnDefineDirective;
    OnUndefineDirective := RK.OnUndefineDirective;
    OnUnknownDirective := RK.OnUnknownDirective;
    OnSavePCUFinished := RK.OnSavePCUFinished; // jason
    OnLoadPCUFinished := RK.OnLoadPCUFinished; // jason

  end;
end;

procedure TKernel.AssignImportTable(ImportTable: Pointer);
begin
  GT := ImportTable;
  SymbolTable.SetImportTable(GT);
end;

procedure TKernel.SetProg(AProg: TBaseRunner);
begin
  prog := AProg;

  if prog = nil then
    Exit;

  ClassFactory := prog.ProgClassFactory;
  TypeInfoList := prog.ProgTypeInfoList;
  OffsetList   := prog.OffsetList;
  ExportList   := prog.ExportList;
  MessageList  := prog.MessageList;

  prog.ModeSEH := ModeSEH;
  prog.PAX64 := PAX64;
  prog.UseMapping := true;

  prog.SetGlobalSym(GT);
end;

procedure TKernel.UpdateTypeInfos;
var
  I, J: Integer;
  RI: TTypeInfoContainer;
  ClassTypeDataContainer: TClassTypeDataContainer;
  SR: TSymbolRec;
begin
  for I := 0 to TypeInfoList.Count - 1 do
  begin
    RI := TypeInfoList[I];
    if RI.TypeInfo.Kind = tkClass then
    begin
      ClassTypeDataContainer :=
        RI.TypeDataContainer as
        TClassTypeDataContainer;
      with ClassTypeDataContainer.AnotherFieldListContainer do
      for J := 0 to Count - 1 do
      begin
        SR := SymbolTable[Records[J].Id];
        Records[J].Offset := SR.Shift;
        Records[J].FinalFieldTypeId := SR.FinalTypeId;
      end;
    end;
  end;
end;

function TKernel.GetPAX64: Boolean;
begin
  result := fPAX64;
end;

procedure TKernel.SetPAX64(value: Boolean);
begin
  fPAX64 := value;
  if fPAX64 then
  begin
    ModeSEH := false;
    SymbolTable.SetPAX64(true);
  end;
end;

procedure TKernel.CompressHostClassList(HostMapTable: TMapTable);
begin
  SymbolTable.CompressHostClassList(HostMapTable, HostClassListIds);
end;

procedure TKernel.SetFieldOffsets;
var
  I, J, Id: Integer;
  ClassTypeDataContainer: TClassTypeDataContainer;
  FieldListContainer: TFieldListContainer;
begin
  for I:=0 to TypeInfoList.Count - 1 do
    if TypeInfoList[I].TypeInfo.Kind = tkClass then
    begin
      ClassTypeDataContainer := TypeInfoList[I].TypeDataContainer as
        TClassTypeDataContainer;
      FieldListContainer := ClassTypeDataContainer.FieldListContainer;
      for J := 0 to FieldListContainer.Count - 1 do
      begin
        Id := FieldListContainer[J].Id;
        FieldListContainer[J].Offset := SymbolTable[Id].Shift;
      end;
    end;
end;

procedure TKernel.RemoveTypes;
var
  I, TypeId: Integer;
  S, OldFullName, NewFullName: String;
begin
  for I := 0 to TypeDefList.RemTypeIds.Count - 1 do
  begin
    TypeId := TypeDefList.RemTypeIds[I];
    S := SymbolTable[TypeId].Name;
    OldFullName := SymbolTable[TypeId].FullName;
    S := S + '#';
    SymbolTable[TypeId].Name := S;
    NewFullName := SymbolTable[TypeId].FullName;
    if SymbolTable[TypeId].FinalTypeId = typeCLASS then
      if not ClassFactory.RenameClass(OldFullName, NewFullName) then
        RaiseError(errInternalError, []);
  end;
end;

procedure TKernel.CreateRTI(aprogram: TBaseRunner);
var
  RuntimeModuleList: TRuntimeModuleList;
  I, J, K, Index, Id: Integer;
  UsedName: String;
  MR: TModuleRec;
begin
  RuntimeModuleList := aprogram.RunTimeModuleList;
  RuntimeModuleList.Clear;

  SetLength(RuntimeModuleList.SourceLines, Code.Card + 1);
  SetLength(RuntimeModuleList.ModuleIndexes, Code.Card + 1);

  for I:=1 to Code.Card do
  begin
    RuntimeModuleList.SourceLines[I] := Code.GetSourceLineNumber(I);
    RuntimeModuleList.ModuleIndexes[I] := Code.GetModuleNumber(I);
  end;

  for I:=0 to Modules.Count - 1 do
  begin
    MR := RuntimeModuleList.Modules.AddRecord;
    with MR do
    begin
      ModuleName := Modules[I].Name;
      P1 := Modules[I].P1;
      P2 := Modules[I].P2;
      P3 := Modules[I].P3;

      Code.CreateExecLines(MR);

      UsedModules.Clear;
      for J:=P1 to P2 do
      begin
        if Code[J].Op = OP_BEGIN_USING then
        begin
          Id := Code[J].Arg1;
          if Id > 0 then
          begin
            UsedName := Code.GetSymbolRec(Id).Name;
            if StrEql(ExtractName(UsedName), ExtractName(ModuleName)) then
              continue;
            if StrEql(UsedName, strPascalNamespace) then
              continue;
            if StrEql(UsedName, strBasicNamespace) then
              continue;

            Index := -1;
            for K := 0 to UsedModules.Count - 1 do
              if StrEql(UsedModules[K], UsedName) then
              begin
                Index := K;
                break;
              end;
            if Index = -1 then
              UsedModules.Add(UsedName);
          end;
        end;
      end;
    end;
  end;
end;

procedure TKernel.AllocateConstants(const PData: Pointer);
var
  I, J, Shift: Integer;
  RI: TSymbolRec;
  P: Pointer;
  I64: Int64;
  UI64: UInt64;
  VCardinal: Cardinal;
  ByteSet: TByteSet;
  SetSize, VT, TypeID: Integer;
  SS: String;
  GUID: TGUID;
{$IFDEF PAXARM}
  WS: String;
{$ELSE}
  S: AnsiString;
  WS: WideString;
{$ENDIF}
begin
  I := 0;

  while I < SymbolTable.Card do
  begin
    Inc(I);

    if not SymbolTable.InCode[I] then
      continue;

    RI := SymbolTable[I];

    if RI = SymbolTable.SR0 then
      continue;

    if RI.Shift = 0 then
      continue;

    case RI.Kind of
      KindCONST:
        case RI.FinalTypeID of
          typeDOUBLE:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Double(P^) := Double(RI.Value);
          end;
          typeSINGLE:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Single(P^) := Single(RI.Value);
          end;
          typeEXTENDED:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Extended(P^) := Extended(RI.Value);
          end;
          typeCURRENCY:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Currency(P^) := Currency(RI.Value);
          end;

{$IFDEF VARIANTS}
          typeINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            I64 := RI.Value;
            Int64(P^) := I64;
          end;
          typeUINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            UI64 := RI.Value;
            UInt64(P^) := UI64;
          end;
{$ELSE}
          typeINT64, typeUINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            I64 := Integer(RI.Value);
            Int64(P^) := I64;
          end;
{$ENDIF}
          typeINTEGER, typeCLASS:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Integer(P^) := Integer(RI.Value);
          end;
          typeSHORTINT:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            ShortInt(P^) := Integer(RI.Value);
          end;
          typeSMALLINT:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            SmallInt(P^) := Integer(RI.Value);
          end;
          typeWORD:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            VCardinal := RI.Value;
            Word(P^) := VCardinal;
          end;
          typeWIDECHAR:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            VCardinal := RI.Value;
            Word(P^) := VCardinal;
          end;
          typeBOOLEAN:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Boolean(P^) := RI.Value;
          end;
          typeBYTEBOOL:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
{$IFDEF FPC}
            if RI.Value <> 0 then
              ByteBool(P^) := true
            else
              ByteBool(P^) := false;
{$ELSE}
            ByteBool(P^) := RI.Value;
{$ENDIF}
          end;
          typeWORDBOOL:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            WordBool(P^) := RI.Value;
          end;
          typeLONGBOOL:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            LongBool(P^) := RI.Value;
          end;
          typeBYTE, typeENUM:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            VCardinal := RI.Value;
            Byte(P^) := VCardinal;
          end;
{$IFNDEF PAXARM}
          typeANSICHAR:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            VCardinal := RI.Value;
            Byte(P^) := VCardinal;
          end;
{$ENDIF}
          typeCARDINAL:
          if RI.MustBeAllocated then
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            VCardinal := RI.Value;
            Cardinal(P^) := VCardinal;
          end;
          typeVARIANT:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Variant(P^) := RI.Value;
          end;
          typeOLEVARIANT:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            OleVariant(P^) := RI.Value;
          end;
          typeSET:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            ByteSet := RI.ValueAsByteSet;
            SetSize := SymbolTable.GetSizeOfSetType(RI.TerminalTypeId);
            Move(ByteSet, P^, SetSize);
          end;
          typeCLASSREF:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            IntPax(P^) := IntPax(RI.Value);
          end;
          typeRECORD:
          if RI.TerminalTypeId = H_TGUID then
          begin
            VT := VarType(RI.Value);
            if (VT = varString) or (VT = varUString) then
            begin
              Shift := GetOffset(RI);
              P := ShiftPointer(PData, Shift);
              SS := RI.Value;
              GUID := StringToGuid(SS);
              Move(GUID, P^, SizeOf(TGUID));
            end;
          end;
          typePOINTER:
          begin
            TypeID := RI.TypeId;
            if SymbolTable[TypeId].PatternId = typeWIDECHAR then
            begin
              WS := RI.Value;

              Shift := GetOffset(RI);
              P := ShiftPointer(PData, Shift);

              P := ShiftPointer(P, 4);
              Integer(P^) := Length(WS);

              P := ShiftPointer(P, 4);

              if WS = '' then
                Word(P^) := 0
              else
              begin
                for J := SLow(WS) to SHigh(WS) do
                begin
                  Move(WS[J], P^, SizeOf(WideChar));
                  Inc(IntPax(P), 2);
                end;
                Word(P^) := 0;
              end;
            end
{$IFNDEF PAXARM}
            else if SymbolTable[TypeId].PatternId = typeANSICHAR then
            begin
              S := AnsiString(RI.Value);

              Shift := GetOffset(RI);
              P := ShiftPointer(PData, Shift + 4);
              Integer(P^) := -1;

              P := ShiftPointer(P, 4);
              Integer(P^) := Length(S);

              P := ShiftPointer(P, 4);

              if S = '' then
                Byte(P^) := 0
              else
                Move(Pointer(S)^, P^, Length(S) + 1);
            end
{$ENDIF}
            else
              if RI.MustBeAllocated then
              begin
                Shift := GetOffset(RI);
                P := ShiftPointer(PData, Shift);
                VCardinal := RI.Value;
                Cardinal(P^) := VCardinal;
              end;
          end;
        end;
      KindVAR:
      if (RI.IsStatic) and (not IsEmpty(RI.Value)) then
        case RI.FinalTypeID of
          typeBOOLEAN, typeBYTE:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Byte(P^) := Integer(RI.Value);
          end;
{$IFNDEF PAXARM}
          typeANSICHAR:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Byte(P^) := Integer(RI.Value);
          end;
{$ENDIF}
          typeINTEGER:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Integer(P^) := Integer(RI.Value);
          end;
          typeDOUBLE:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Double(P^) := Double(RI.Value);
          end;
          typeSINGLE:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Single(P^) := Single(RI.Value);
          end;
          typeEXTENDED:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Extended(P^) := Extended(RI.Value);
          end;
          typeCURRENCY:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Currency(P^) := Currency(RI.Value);
          end;
{$IFDEF VARIANTS}
          typeINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            I64 := RI.Value;
            Int64(P^) := I64;
          end;
          typeUINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            UI64 := RI.Value;
            UInt64(P^) := UI64;
          end;
{$ELSE}
          typeINT64, typeUINT64:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            I64 := Integer(RI.Value);
            Int64(P^) := I64;
          end;
{$ENDIF}
          typeSET:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            ByteSet := RI.ValueAsByteSet;
            SetSize := SymbolTable.GetSizeOfSetType(RI.TerminalTypeId);
            Move(ByteSet, P^, SetSize);
          end;

          typeCLASSREF:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            IntPax(P^) := IntPax(RI.Value);
          end;

          typeVARIANT:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            Variant(P^) := Variant(RI.Value);
          end;

          typeOLEVARIANT:
          begin
            Shift := GetOffset(RI);
            P := ShiftPointer(PData, Shift);
            OleVariant(P^) := Variant(RI.Value);
          end;

          typeRECORD:
          if RI.TerminalTypeId = H_TGUID then
          begin
            VT := VarType(RI.Value);
            if (VT = varString) or (VT = varUString) then
            begin
              Shift := GetOffset(RI);
              P := ShiftPointer(PData, Shift);
              SS := RI.Value;
              GUID := StringToGuid(SS);
              Move(GUID, P^, SizeOf(TGUID));
            end;
          end;
      end;
      KindSUB:
      if not RI.Host then
      begin
        Shift := GetOffset(RI);
        P := ShiftPointer(PData, Shift);
        Integer(P^) := RI.Value;
      end;
    end; // case
  end;
end;

function TKernel.GetRunnerClass: TBaseRunnerClass;
begin
  if prog = nil then
    result := DefaultRunnerClass
  else
    result := TBaseRunnerClass(prog.ClassType)
end;

function TKernel.GetDestructorAddress: Pointer;
begin
  if prog = nil then
    result := nil
  else
    result := prog.GetDestructorAddress;
end;

{$ifdef DRTTI}
procedure TKernel.RegisterImportUnit(Level: Integer; const AUnitName: String);

var
  u: TUnit;

  procedure P(t: TRTTIType);
  var
    S: String;
  begin
    S := ExtractUnitName(t);
    if StrEql(S, AUnitName) then
    if u.UsedTypes.IndexOf(t) = -1 then
    begin
      if not CheckType(t) then
        Exit;
      u.UsedTypes.Add(t);
    end;
  end;

var
  t: TRTTIType;
begin
  u := ImportUnitList.AddUnit(AUnitName, Level);
  for t in PaxContext.GetTypes do
    P(t);
end;
{$endif}

function TKernel.GetRootSearchPathList: TStringList;
begin
  result := RootKernel.fSearchPathList;
end;

function TKernel.FindFullPath(const FileName: String): String;
var
  I: Integer;
  S: String;
begin
  result := FileName;
  if SysUtils.FileExists(result) then
    Exit;
  for I := 0 to RootSearchPathList.Count - 1 do
  begin
    S := RootSearchPathList[I];
    if S[Length(S)] <> '\' then
      S := S + '\';
    S := S + FileName;
    if SysUtils.FileExists(S) then
    begin
      result := S;
      Exit;
    end;
  end;
end;

procedure TKernel.SetupDefaultSettings;
begin
  fTargetPlatform := tpNONE;
{$IFDEF MACOS}
  fTargetPlatform := tpOSX32;
{$ENDIF}
{$IFDEF LINUX}
  fTargetPlatform := tpLINUX32;
{$ENDIF}
{$IFDEF IOS}
  fTargetPlatform := tpiOSSim;
{$ENDIF}

{$IFDEF CPUARM}
  {$IFDEF ANDROID}
    fTargetPlatform := tpANDROID;
  {$ELSE}
    fTargetPlatform := tpiOSDev;
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF PAX64}
  fPAX64 := true;
  fTargetPlatform := tpWIN64;
  {$ELSE}
  fTargetPlatform := tpWIN32;
  {$ENDIF}
{$ENDIF}

 {$IFDEF UNIC}
  IsUNIC := true;
  {$ELSE}
  IsUNIC := false;
  {$ENDIF}

  ModeSEH := fTargetPlatform = tpWIN32;
end;

function TKernel.GetRunnerKind: TRunnerKind;
begin
  if prog = nil then
    result := rkNONE
  else if StrEql(prog.ClassName, 'TProgram') then
    result := rkPROGRAM
  else
    result := rkINTERPRETER;
end;

function TKernel.GetTargetPlatform: TTargetPlatform;
begin
  result := fTargetPlatform;
end;

procedure TKernel.SetTargetPlatform(value: TTargetPlatform);
begin
  fTargetPlatform := value;
  PAX64 := (value = tpWin64);
end;

function TKernel.GetSupportedSEH: Boolean;
begin
  result := (TargetPlatform = tpWIN32) or (RunnerKind = rkINTERPRETER);
end;


end.
