////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PARSER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_PARSER;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_MODULE,
  PAXCOMP_SCANNER,
  PAXCOMP_LABEL_STACK,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BYTECODE,
  PAXCOMP_TYPEINFO,
  PAXCOMP_BASERUNNER,
  PAXCOMP_MAP,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_GENERIC;
type
   TBaseParser = class;
   TBaseParserClass = class of TBaseParser;
   TBaseParser = class
   private
     keywords: TFastStringList;
     hidden_keywords: TIntegerList;
     LValueIndex: Integer;
     SkipWhenNotFound: Boolean;
     BeginSubList: TAssocIntegers;
     LoopCounter: Integer;
     LoopCounterStack: TIntegerStack;
     function GetFinSubLabel: Integer;
     function GetCurrLevel: Integer;
     function GetCurrSubId: Integer;
     function GetCurrResultId: Integer;
     function GetOuterClassId: Integer;
     function GetNilId: Integer;
     function GetEmptySetId: Integer;
     function GetCurrExceptionObjectId: Integer;
     function GetTryCount: Integer;
     procedure SetTryCount(Value: Integer);
     function GetDebugMode: Boolean;
     function GetRaiseMode: Integer;
     function GetStCard: Integer;
     function GetCodeCard: Integer;
     function GetTrueId: Integer;
     function GetFalseId: Integer;
     function CreateParser(const FileName: String): TBaseParser;
     function GetUsedUnitList: TStringList;
     function GetInterfaceOnly: Boolean;
     function GetImportOnly: Boolean;
     function GetTargetPlatform: TTargetPlatform;
     function GetRunnerKind: TRunnerKind;
     function GetSupportedSEH: Boolean;
   protected
     function CreateScanner: TBaseScanner; virtual; abstract;
     function GetLanguageName: String; virtual; abstract;
     function GetFileExt: String; virtual; abstract;
     function GetLanguageId: Integer; virtual; abstract;
     function GetUpcase: Boolean; virtual; abstract;
     function ReplaceForwardDeclaration(var ID: Integer; IsExternalUnit: Boolean = false): Boolean; virtual;
     function GetCurrSelfId: Integer; virtual;
     function ConvString(const S: String): String; virtual;
     procedure SetPrevToken;
     function GetIsUNIC: Boolean;
     function Parse_Factor: Integer; virtual; abstract;

     function IsCurrText(const S: String): Boolean; virtual;
     function IsCurrTextIn(L: TStrings): Boolean; virtual;
     function IsNextText(const S: String): Boolean; virtual;
     function IsNext2Text(const S: String): Boolean; virtual;
     function GetNextText: String; virtual;
     function GetNext2Text: String; virtual;
     function GetNextTokenClass: TTokenClass; virtual;
     function GetNext2TokenClass: TTokenClass; virtual;

   public
     kernel: Pointer;
     scanner: TBaseScanner;
     PrintKeyword: String;
     PrintlnKeyword: String;
     UpCase: Boolean;
     LanguageId: Integer;
     CurrToken: TToken;
     FIND_DECL_SWITCH: Boolean;
     DECLARE_SWITCH: Boolean;
     EXECUTABLE_SWITCH: Integer;
     FIELD_OWNER_ID: Integer;
     SKIP_STATEMENT_TERMINATOR: Boolean;
     CurrModule: TModule;
     BreakStack: TEntryStack;
     ContinueStack: TEntryStack;
     WithStack: TIntegerStack;
     SkipLabelStack: TIntegerStack;
     ExitLabelStack: TIntegerStack;
     try_stack: TIntegerStack;
     CallConv: Integer;
     DeclareCallConv: Integer;
     LevelStack: TIntegerStack;
     AnonymStack: TAnonymContextStack;
     UsingList: TIntegerList;
     TypeParams: TTypeParams;
     TypeParamsHistory: TTypeParamsHistory;
     CompleteBooleanEval: Boolean;
     UnitLookup: Boolean;
     IsConsoleApp: Boolean;
     Alignment: Integer;
     LinePos: Integer;
     BRP: Boolean;
     IMPLEMENTATION_SECTION: Boolean;
     FIND_DECL_IMPLEMENTATION_SECTION: Boolean;
     EXPLICIT_OFF: Boolean;
     EXTRA_SWITCH: Boolean;
     ParsesModule: Boolean;
     UseFWArrays: Boolean;
     InitFuncResult: Boolean;
     Tag: Integer;
     CollectSig: Boolean;
     Sig: String;
     operators: TAssocStrings;
     Owner: TObject;
     PrevPosition: Integer;
     PrevLength: Integer;
     constructor Create; virtual;
     destructor Destroy; override;
     procedure ParseProgram; virtual; abstract;
     function Parse_Expression: Integer; virtual; abstract;
     procedure Parse_Unit(IsExternalUnit: Boolean = false); virtual;
     procedure Init(i_kernel: Pointer; M: TModule); virtual;
     procedure InitScanner(const S: String);
     function Lookup(const S: String; Level: Integer): Integer;
     function Lookups(const S: String; Levels: TIntegerStack): Integer;
     function LookupInUsingList(const S: String): Integer;
     function LookupInWithStack(const S: String;
                                var PatternId: Integer;
                                var StackIsOK: Boolean): Integer;

     function FindConstructorId(ClassId: Integer): Integer;
     function FindDestructorId(ClassId: Integer): Integer;
     function FindPrevEvaluation(const S: String): Integer;

     function AddKeyword(const S: String): Integer;
     procedure AddKeywords(L: TStrings);
     function IsKeyword(const S: String): Boolean;
     procedure HideKeyword(KeywordIndex: Integer);
     procedure RestoreKeywords;

     procedure AddOperator(const S1, S2: String);
     function OperatorIndex(const S: String): Integer;

     // generics
     function Parse_TypeParams: String;
     procedure AddTypeParameters(LevelId: Integer;
                                 E: TTypeExtRec = nil); virtual;
     procedure BeginTypeDef(TypeId: Integer); virtual;
     procedure EndTypeDef(TypeId: Integer); virtual;
     procedure BeginTypeExt(TypeId: Integer); virtual;
     procedure InitTypeExt(TypeId: Integer;
                           const MemberName: String;
                           IsMethodImpl: Boolean);
     procedure EndTypeExt(TypeId: Integer); virtual;
     procedure BeginMethodDef; virtual;
     procedure InitMethodDef(SubId: Integer);
     procedure EndMethodDef(SubId: Integer); virtual;
     procedure SaveExtraNamespace(Id: Integer);
     function ParametrizedTypeExpected: Boolean; virtual;
     function ReadType: Integer;
     function IsGeneric(TypeId: Integer): Boolean;
     procedure Parse_TypeRestriction(LocalTypeParams: TStringObjectList); virtual;
     function AltTypeId(const S: String): Integer; virtual;
     procedure RemoveType(TypeId: Integer);
     procedure BeginLoop;
     procedure EndLoop;
     function ExtractText(P1, P2: Integer): String;
     function Parse_Ident: Integer; virtual;
     function Parse_QualId: Integer; virtual;
     function Parse_EnumIdent: Integer;
     function Parse_FormalParameter: Integer; virtual;
     function Parse_BooleanLiteral: Integer; virtual;
     function Parse_CharLiteral: Integer; virtual;
     function Parse_PCharLiteral: Integer; virtual;
     function Parse_IntegerLiteral: Integer; virtual;
     function Parse_DoubleLiteral: Integer; virtual;
     function BinOp(Op, Arg1, Arg2: Integer): Integer;
     function UnaryOp(Op, Arg1: Integer): Integer;
     function NewConst(ATypeID: Integer): Integer; overload;
     function NewConst(ATypeID: Integer; const Value: Variant): Integer; overload;
     function NewVar(const VarName: String): Integer;
     function NewTempVar: Integer; overload;
     function NewTempVar(TypeId: Integer): Integer; overload;
     function NewTypeAlias: Integer;
     function NewLabel: Integer;
     function NewField(const FieldName: String; OwnerId: Integer): Integer;
     function Gen(Op, Arg1, Arg2, Res: Integer): TCodeRec; virtual;
     procedure ReadToken; virtual;
     procedure ReadTokenEx; virtual;
     procedure Call_SCANNER; virtual;

     procedure Match(const S: String); virtual;
     procedure MatchFinal(const S: String);
     procedure SafeMatch(const S: String);
     function NotMatch(const S: String): Boolean; virtual;
     function IsEOF: Boolean;
     function IsNewLine: Boolean;
     function GetFullName(Id: Integer): String;
     function GetName(Id: Integer): String;
     procedure SetName(Id: Integer; const S: String);
     procedure SetValue(Id: Integer; const Value: Variant);
     function GetCount(Id: Integer): Integer;
     procedure SetParam(Id: Integer; value: boolean);
     procedure SetCount(Id: Integer; value: Integer);
     function GetResultId(SubId: Integer): Integer;
     function GetParamId(SubId, J: Integer): Integer;
     procedure SetPatternId(Id: Integer; PatternId: Integer);
     procedure SetAncestorId(Id: Integer; AncestorId: Integer);
     procedure SetOwnerId(Id: Integer; OwnerId: Integer);
     procedure SetByRef(Id: Integer);
     procedure SetIsConst(Id: Integer);
     procedure SetOptional(Id: Integer);
     procedure SetPacked(Id: Integer);
     procedure SetCallConvention(Id: Integer; value: Integer);
     procedure SetOverloaded(SubId: Integer);
     procedure SetCallMode(Id: Integer; value: Integer);
     procedure SetKind(Id: Integer; Kind: Integer);
     function GetLevel(Id: Integer): Integer;
     procedure SetLevel(Id: Integer; Level: Integer);
     function GetType(Id: Integer): Integer;
     procedure SetType(Id: Integer; TypeID: Integer);
     function GetKind(Id: Integer): Integer;
     function GetPosition(Id: Integer): Integer;
     procedure SetPosition(Id, Position: Integer);
     procedure SetFinal(Id: Integer; value: Boolean);
     procedure SetAbstract(Id: Integer; value: Boolean);
     procedure SetVarCount(Id, value: Integer);
     procedure SetLabelHere(Id: Integer;
                            Arg2: Integer = 0;
                            Res: Integer = 0);
     procedure SetVisibility(Id: Integer; vis: TClassVisibility);
     procedure SetReadId(PropId, ReadId: Integer);
     procedure SetWriteId(PropId, WriteId: Integer);
     procedure SetDefault(Id: Integer; value: Boolean);
     procedure SetTypedConst(Id: Integer);
     procedure SetOpenArray(ID: Integer; value: Boolean);
     procedure SetExternal(Id: Integer; value: Boolean);

     procedure SetHost(Id: Integer; value: Boolean);
     function GetHost(Id: Integer): Boolean;
     procedure SetAlignment(TypeId, Value: Integer);

     procedure BeginSub(SubId: Integer); virtual;
     procedure RemoveSub; overload;
     procedure RemoveSub(SubId: Integer); overload;
     procedure BeginClassConstructor(SubId, ClassTypeId: Integer); virtual;
     procedure BeginClassDestructor(SubId, ClassTypeId: Integer); virtual;
     procedure BeginClassMethod(SubId, ClassTypeId: Integer;
                                HasResult: Boolean;
                                IsSharedMethod: Boolean;
                                IsMethodImpl: Boolean); virtual;
     procedure BeginStructureConstructor(SubId, StructTypeId: Integer);
     procedure BeginStructureDestructor(SubId, StructTypeId: Integer);
     procedure BeginStructureMethod(SubId, StructTypeId: Integer;
                                    HasResult: Boolean;
                                    IsSharedMethod: Boolean); virtual;
     procedure BeginStructureOperator(SubId, StructTypeId: Integer); virtual;

     procedure BeginInterfaceMethod(SubId, IntfTypeId: Integer;
                                    HasResult: Boolean); virtual;
     procedure CheckAbstract(SubId: Integer);
     procedure InitSub(var SubId: Integer); virtual;
     procedure EndSub(SubId: Integer); virtual;
     function GetIncludedFileExt: String; virtual; abstract;

     procedure BeginProperty(PropId, ClassTypeId: Integer);
     procedure EndProperty(PropId: Integer);

     procedure BeginNamespace(Id: Integer; Jump: Boolean = true);
     procedure EndNamespace(Id: Integer; Jump: Boolean = true);

     procedure BeginArrayType(TypeId: Integer);
     procedure EndArrayType(TypeId: Integer);

     procedure BeginRecordType(TypeId: Integer);
     procedure EndRecordType(TypeId: Integer);

     procedure BeginClassType(TypeId: Integer);
     procedure EndClassType(TypeId: Integer; IsForward: Boolean = false);

     procedure BeginInterfaceType(TypeId: Integer);
     procedure SetGuid(IntfTypeId: Integer; const S: String);
     procedure SetNewGuid(IntfTypeId: Integer);
     procedure EndInterfaceType(TypeId: Integer; IsForward: Boolean = false);

     procedure BeginMethodRefType(TypeId: Integer);
     procedure EndMethodRefType(TypeId: Integer);

     procedure BeginEnumType(TypeId, TypeBaseId: Integer);
     procedure EndEnumType(TypeId, ACount: Integer);

     procedure BeginSubrangeType(TypeId, TypeBaseId: Integer);
     procedure EndSubrangeType(TypeId: Integer);

     procedure BeginPointerType(TypeId: Integer);
     procedure EndPointerType(TypeId: Integer);

     procedure BeginHelperType(TypeId, TrueTypeId: Integer);
     procedure EndHelperType(TypeId: Integer);

     procedure BeginClassReferenceType(TypeId: Integer);
     procedure EndClassReferenceType(TypeId: Integer);

     procedure BeginDynamicArrayType(TypeId: Integer);
     procedure EndDynamicArrayType(TypeId: Integer);

     procedure BeginOpenArrayType(TypeId: Integer);
     procedure EndOpenArrayType(TypeId: Integer; const ElemName: String);
{$IFNDEF PAXARM}
     procedure BeginShortStringType(TypeId: Integer);
     procedure EndShortStringType(TypeId: Integer);
{$ENDIF}
     procedure BeginProceduralType(TypeId, SubID: Integer);
     procedure EndProceduralType(TypeId: Integer);

     procedure BeginSetType(TypeId, TypeBaseId: Integer);
     procedure EndSetType(TypeId: Integer);

     procedure BeginInitialization;
     procedure EndInitialization;
     procedure BeginFinalization;
     procedure EndFinalization;

     function GenBeginTry: Integer; // returns label
     procedure GenEndTry;
     procedure GenFinally;
     procedure GenExcept;
     procedure GenExceptOn(type_id: Integer);

     procedure GenNOPS(K: Integer);
     procedure GenAssignOuterInstance(Id, ClassId: Integer);
     procedure GenComment(const S: String);

     procedure BeginInitConst(Id: Integer);
     procedure EndInitConst(Id: Integer);

     procedure GenDestroyGlobalDynamicVariables(B1, B2: Integer);

     procedure SetForward(SubId: Integer; value: Boolean);
     function IsForward(SubId: Integer): Boolean;

     procedure BeginCollectSig(SubId: Integer); virtual;
     procedure EndCollectSig(SubId: Integer); virtual;

     procedure AddModuleFromFile(FileName: String;
                                 UsedUnitId: Integer;
                                 IsImplementationSection: Boolean;
                                 ErrMessage: String = '';
                                 NoRaise: Boolean = false);

     function GetValue(id: Integer): Variant;

     procedure RemoveInstruction(Op, Arg1, Arg2, Res: Integer);
     function RemoveLastEvalInstruction(const S: String; Upcase: Boolean = true): Integer;
     function RemoveLastEvalInstructionAndName(const S: String; Upcase: Boolean = true): Boolean;
     procedure RemoveLastIdent(Id: Integer);
     function LastCodeRec(var I: Integer): TCodeRec; overload;
     function LastCodeRec: TCodeRec; overload;
     function LastCodeRec2: TCodeRec;
     function LastEvalRec(Id: Integer): TCodeRec; overload;
     function LastEvalRec(Id: Integer; var I: Integer): TCodeRec; overload;

     function LookupForwardDeclarations(Id: Integer): TIntegerList;
     procedure DiscardLastSTRecord;
     function ScanRegExpLiteral: String;
     function LA(I: Integer): Char;
     function CreatePointerType(type_id, pcount: Integer): Integer;
     function CreateProceduralType(SubId: Integer): Integer;
     function CreateSubrangeType(B1, B2: Integer): Integer;
     function CreateArrayType(RangeTypeId, ElemTypeId: Integer): Integer;

     procedure RaiseError(const Message: string; params: array of Const);
     procedure CreateError(const Message: string; params: array of Const);
     procedure RaiseNotImpl;

     procedure Push_SCANNER;
     procedure Pop_SCANNER;

     function CountAtLevel(const S: String; Level: Integer): Integer; overload;
     function CountAtLevel(const S: String;
                           Level, Kind: Integer; IsSharedMethod: Boolean): Integer; overload;

     function IsStringConst(Id: Integer): Boolean;
     procedure SetCompletionTarget(const S: String);
     function GetCodeRec(I: Integer): TCodeRec;
     function GetSymbolRec(Id: Integer): TSymbolRec;
     function CurrNamespaceId: Integer;
     function BuildingAll: Boolean;
     procedure SetTempName(Id: Integer);
     function Parse_UnitName(var S: String): Integer;
     function ExpandUnitName(const S: String): String;
     function ExpandTypeName(const S: String): String;
     procedure CheckRedeclaredSub(SubId: Integer);
     procedure SetDeprecated(SubId: Integer; value: Boolean);
     function HasBeenDeclared(Id: Integer): Boolean;
     function HasModule(const ModuleName: String): Boolean;
     procedure GenHtml;
     procedure GenCondRaise;

     function GetOpenArrayHighId(Id: Integer): Integer;
     procedure GenPause;

     function IsTryContext(R: TEntryRec): Boolean;
     procedure NewAnonymousNames(var ClsName: String;
                                 var ObjName: String);
     function IsOuterLocalVar(Id: Integer): Boolean;
     procedure RelocateCode(K1, K2: Integer); overload;
     procedure RelocateCode(Ip, K1, K2: Integer); overload;

     property CurrLevel: Integer read GetCurrLevel;
     property CurrResultId: Integer read GetCurrResultId;
     property CurrSelfId: Integer read GetCurrSelfId;
     property CurrSubId: Integer read GetCurrSubId;
     property NilId: Integer read GetNilId;
     property EmptySetId: Integer read GetEmptySetId;
     property CurrExceptionObjectId: Integer read GetCurrExceptionObjectId;
     property LanguageName: String read GetLanguageName;
     property TryCount: Integer read GetTryCount write SetTryCount;
     property DEBUG_MODE: Boolean read GetDebugMode;
     property RaiseMode: Integer read GetRaiseMode;
     property STCard: Integer read GetStCard;
     property FinSubLabel: Integer read GetFinSubLabel;
     property TrueId: Integer read GetTrueId;
     property FalseId: Integer read GetFalseId;
     property CodeCard: Integer read GetCodeCard;
     property UsedUnitList: TStringList read GetUsedUnitList;
     property InterfaceOnly: Boolean read GetInterfaceOnly;
     property ImportOnly: Boolean read GetImportOnly;
     property OuterClassId: Integer read GetOuterClassId;
     property IsUNIC: Boolean read GetIsUNIC;
     property TargetPlatform: TTargetPlatform read GetTargetPlatform;
     property RunnerKind: TRunnerKind read GetRunnerKind;
     property SupportedSEH: Boolean read GetSupportedSEH;
   end;

   TParserList = class
   private
     L: TPtrList;
     kernel: Pointer;
     function GetParser(I: Integer): TBaseParser;
   public
     constructor Create(i_kernel: Pointer);
     destructor Destroy; override;
     procedure AddParser(P: TBaseParser);
     procedure Clear;
     function Count: Integer;
     function FindParser(const LanguageName: String): TBaseParser;
     function FindParserByFileExtension(const FileExtension: String): TBaseParser;
     property Items[I: Integer]: TBaseParser read GetParser; default;
   end;

implementation

uses
{$IFDEF DRTTI}
  PAXCOMP_2010,
{$ENDIF}
  PAXCOMP_KERNEL, PAXCOMP_STDLIB, PAXCOMP_PCU, PAXCOMP_RTI;

constructor TBaseParser.Create;
begin
  kernel := nil;

  scanner := CreateScanner;
  UpCase := true;
  DECLARE_SWITCH := false;
  FIND_DECL_SWITCH := false;
  EXECUTABLE_SWITCH := 0;
  FIELD_OWNER_ID := 0;
  EXTRA_SWITCH := false;
  keywords := TFastStringList.Create;
  hidden_keywords := TIntegerList.Create;
  operators := TAssocStrings.Create;
  BeginSubList := TAssocIntegers.Create;
  levelStack := TIntegerStack.Create;
  levelStack.Push(0);
  AnonymStack := TAnonymContextStack.Create;
  try_stack := TIntegerStack.Create;
  SkipLabelStack := TIntegerStack.Create;
  ExitLabelStack := TIntegerStack.Create;
  BreakStack := TEntryStack.Create;
  ContinueStack := TEntryStack.Create;
  WithStack := TIntegerStack.Create;
  SKIP_STATEMENT_TERMINATOR := false;
  CompleteBooleanEval := false;
  UnitLookup := true;
  IsConsoleApp := false;
  Alignment := GlobalAlignment;
  PrintKeyword := 'print';
  PrintlnKeyword := 'println';
  BRP := false;
  CallConv := ccREGISTER;
  DeclareCallConv := ccSTDCALL;
  UseFWArrays := true;
  UsingList := TIntegerList.Create;
  TypeParams := TTypeParams.Create;
  TypeParamsHistory := TTypeParamsHistory.Create;
  LoopCounterStack := TIntegerStack.Create;
end;

destructor TBaseParser.Destroy;
begin
  FreeAndNil(scanner);
  FreeAndNil(keywords);
  FreeAndNil(hidden_keywords);
  FreeAndNil(operators);
  FreeAndNil(BeginSubList);
  FreeAndNil(levelStack);
  FreeAndNil(AnonymStack);
  FreeAndNil(SkipLabelStack);
  FreeAndNil(ExitLabelStack);
  FreeAndNil(BreakStack);
  FreeAndNil(ContinueStack);
  FreeAndNil(WithStack);
  FreeAndNil(try_stack);
  FreeAndNil(UsingList);
  FreeAndNil(TypeParams);
  FreeAndNil(TypeParamsHistory);
  FreeAndNil(LoopCounterStack);

  inherited;
end;

procedure TBaseParser.Init(i_kernel: Pointer; M: TModule);
begin
  Self.kernel := i_kernel;
  if scanner <> nil then
    scanner.Init(kernel, M.Lines.Text, M.CancelPos);
  levelStack.Clear;
  levelStack.Push(0);
  AnonymStack.Clear;
  SkipLabelStack.Clear;
  ExitLabelStack.Clear;
  BreakStack.Clear;
  BreakStack.SetKernel(i_kernel);
  ContinueStack.Clear;
  ContinueStack.SetKernel(i_kernel);
  WithStack.Clear;
  try_stack.Clear;
  BeginSubList.Clear;
  hidden_keywords.Clear;

  CurrModule := M;
  LValueIndex := 0;

  SKIP_STATEMENT_TERMINATOR := false;
  FIELD_OWNER_ID := 0;

  IsConsoleApp := false;
  Alignment := TKernel(kernel).Alignment;

  LinePos := 0;

  LanguageId := GetLanguageId;

  Upcase := GetUpcase;

  Gen(OP_PRINT_KWD, NewConst(typeSTRING, PrintKeyword), 0, 0);
  Gen(OP_PRINTLN_KWD, NewConst(typeSTRING, PrintlnKeyword), 0, 0);

  DECLARE_SWITCH := false;
  EXECUTABLE_SWITCH := 0;
  IMPLEMENTATION_SECTION := false;
  FIND_DECL_IMPLEMENTATION_SECTION := false;
  FIND_DECL_SWITCH := false;
  CollectSig := false;
  UsingList.Clear;
  TypeParams.Clear;
  TypeParamsHistory.Clear;

  LoopCounter := 0;
  LoopCounterStack.Clear;
end;

procedure TBaseParser.ReadToken;
begin
  scanner.ReadToken;
  CurrToken := scanner.Token;
end;

procedure TBaseParser.ReadTokenEx;
var
  S: String;
  c: Byte;
begin
  scanner.ReadToken;
  S := scanner.Token.Text;
  c := Ord(S[1]);
  while (c = 13) or (c = 10) do
  begin
    scanner.ReadToken;
    S := scanner.Token.Text;
    c := Ord(S[1]);
  end;
end;


function TBaseParser.NewField(const FieldName: String; OwnerId: Integer): Integer;
begin
  result := NewTempVar;
  with TKernel(kernel) do
  begin
    SymbolTable[result].Name := FieldName;
    SymbolTable[result].OwnerId := OwnerId;
  end;
end;

procedure TBaseParser.Call_SCANNER;
var
  IntVal: Integer;
  Int64Val: Int64;
  ExtendedVal: Extended;
  StringVal: String;
  SymbolTable: TSymbolTable;
  I, id, lev: Integer;
  R: TSymbolRec;
  S: String;
  WordVal: Word;
  CurrToken_Text: String;
  cc: Boolean;
  FieldId, PatternId: Integer;
  StackIsOK: Boolean;
label
  LabelEval;
begin
  SymbolTable := TKernel(kernel).SymbolTable;

  LinePos := scanner.LinePos;

  scanner.ReadToken;

  CurrToken := scanner.Token;

  CurrToken_Text := scanner.UpdateToken;

  if CurrModule.IsExtra then
  begin
    if Upcase then
    begin
      if StrEql(EXTRA_KEYWORD, CurrToken_Text) then
      begin
        EXTRA_SWITCH := true;
        Call_SCANNER;
      end;
    end
    else
    begin
      if EXTRA_KEYWORD = CurrToken_Text then
      begin
        EXTRA_SWITCH := true;
        Call_SCANNER;
      end;
    end;
  end;

  if CurrToken.TokenClass = tcHtmlStringConst then
  begin
    GenHtml;
    DECLARE_SWITCH := false;
    Tag := 1;
    Call_SCANNER;
    Exit;
  end;

  if Assigned(TKernel(kernel).OnCompilerProgress) then
    TKernel(kernel).OnCompilerProgress(TKernel(kernel).Owner);

  if CurrToken.TokenClass = tcIntegerConst then
  begin
    if CurrToken_Text[1] = '&' then
    begin
      CurrToken_Text := '$' + Copy(CurrToken_Text, 3, Length(CurrToken_Text) - 2);
    end;

    if CurrToken.Tag = 2 then
    begin
      Int64Val := Scanner.CustomInt64Val;
      CurrToken.Tag := 0;
    end
    else
    begin
//      Int64Val := StrToInt64(CurrToken_Text);
      Val(CurrToken_Text, Int64Val, I);
    end;
    if Abs(Int64Val) > MaxInt then
      CurrToken.Id := SymbolTable.AddInt64Const(Int64Val).Id
    else
    begin
      if Int64Val < - MaxInt then
        CurrToken.Id := SymbolTable.AddInt64Const(Int64Val).Id
      else
        CurrToken.Id := SymbolTable.AddIntegerConst(Int64Val).Id;
    end;
  end
  else if CurrToken.TokenClass = tcCharConst then
  begin
    if CurrToken.Tag = 1 then
    begin
      StringVal := Copy(CurrToken_Text, 2, Length(CurrToken_Text) - 1);

      IntVal := StrToInt(StringVal);
      if IntVal <= 255 then
      begin
        StringVal := Chr(IntVal);

        repeat

          if scanner[1] = '#' then
          begin
            ReadToken;
            S := Copy(CurrToken.Text, 2, Length(CurrToken.Text) - 1);
            WordVal := StrToInt(S);
            if WordVal <= 255 then
              StringVal := StringVal + Chr(WordVal)
            else
              RaiseError(errSyntaxError, []);
          end;

          if scanner[1] = '''' then
          begin
            ReadToken;
            S := CurrToken.Text;
            S := Copy(S, 2, Length(S) - 2);

            I := PosCh(CHAR_REMOVE, S);
            while I > 0 do
            begin
              Delete(S, I, 1);
              I := PosCh(CHAR_REMOVE, S);
            end;
            StringVal := StringVal + S;
          end;

        until not ByteInSet(scanner[1], [Ord('#'), Ord('''')]);

        if IsUNIC then
        begin
          if Length(StringVal) > 1 then
            CurrToken.Id := SymbolTable.AddPWideCharConst(StringVal).Id
          else
            CurrToken.Id := SymbolTable.AddWideCharConst(IntVal).Id;
        end
        else
        begin
{$IFNDEF PAXARM}
          if Length(StringVal) > 1 then
            CurrToken.Id := SymbolTable.AddPAnsiCharConst(AnsiString(StringVal)).Id
          else
            CurrToken.Id := SymbolTable.AddAnsiCharConst(AnsiChar(Chr(IntVal))).Id;
{$ENDIF}
        end;
      end
      else
        CurrToken.Id := SymbolTable.AddWideCharConst(IntVal).Id;
      CurrToken.Tag := 0;
    end
    else
    begin
      if CurrToken.Tag = 2 then
      begin
{$IFDEF PAXARM}
        CurrToken.Id := SymbolTable.AddWideCharConst(Scanner.CustomInt64Val).Id;
{$ELSE}
        if IsUNIC then
          CurrToken.Id := SymbolTable.AddWideCharConst(Scanner.CustomInt64Val).Id
        else
          CurrToken.Id := SymbolTable.AddAnsiCharConst(AnsiChar(Scanner.CustomInt64Val)).Id;
{$ENDIF}
        CurrToken.Tag := 0;
        Exit;
      end;

      StringVal := CurrToken_Text; // length = 3

{$IFDEF PAXARM}
      CurrToken.Id := SymbolTable.AddWideCharConst(Ord(StringVal[1+SLow(StringVal)])).Id;
{$ELSE}
      if IsUNIC then
        CurrToken.Id := SymbolTable.AddWideCharConst(Ord(StringVal[1+SLow(StringVal)])).Id
      else
        CurrToken.Id := SymbolTable.AddAnsiCharConst(AnsiChar(StringVal[2])).Id;
{$ENDIF}
    end;
  end
  else if CurrToken.TokenClass = tcPCharConst then
  begin
    if CurrToken.Tag = 2 then
    begin
{$IFDEF PAXARM}
      CurrToken.Id := SymbolTable.AddPWideCharConst(Scanner.CustomStringVal).Id;
{$ELSE}
      if IsUNIC then
        CurrToken.Id := SymbolTable.AddPWideCharConst(Scanner.CustomStringVal).Id
      else
        CurrToken.Id := SymbolTable.AddPAnsiCharConst(AnsiString(Scanner.CustomStringVal)).Id;
{$ENDIF}
      CurrToken.Tag := 0;
      Exit;
    end;

    StringVal := CurrToken_Text;
    StringVal := Copy(StringVal, 2, Length(StringVal) - 2);

    I := PosCh(CHAR_REMOVE, StringVal);
    while I > 0 do
    begin
      Delete(StringVal, I, 1);
      I := PosCh(CHAR_REMOVE, StringVal);
    end;

    repeat

      if scanner[1] = '#' then
      begin
        ReadToken;
        S := Copy(CurrToken.Text, 2, Length(CurrToken.Text) - 1);
        WordVal := StrToInt(S);
        if WordVal <= 255 then
          StringVal := StringVal + Chr(WordVal)
        else
          RaiseError(errSyntaxError, []);
      end;

      if scanner[1] = '''' then
      begin
        ReadToken;
        S := CurrToken.Text;
        S := Copy(S, 2, Length(S) - 2);

        I := PosCh(CHAR_REMOVE, S);
        while I > 0 do
        begin
          Delete(S, I, 1);
          I := PosCh(CHAR_REMOVE, S);
        end;
        StringVal := StringVal + S;
      end;

    until not ByteInSet(scanner[1], [Ord('#'), Ord('''')]);

    StringVal := ConvString(StringVal);

    if StringVal = '' then
      CurrToken.Id := SymbolTable.EmptyStringId
    else
    begin
{$IFDEF PAXARM}
      CurrToken.Id := SymbolTable.AddPWideCharConst(StringVal).Id;
{$ELSE}
      if IsUNIC then
        CurrToken.Id := SymbolTable.AddPWideCharConst(StringVal).Id
      else
        CurrToken.Id := SymbolTable.AddPAnsiCharConst(AnsiString(StringVal)).Id;
{$ENDIF}
    end;
    CurrToken.Tag := 0;
  end
  else if CurrToken.TokenClass = tcDoubleConst then
  begin
    StringVal := CurrToken_Text;
    Val(StringVal, ExtendedVal, Id);
    if ExtendedVal >= MaxDouble then
      CurrToken.Id := SymbolTable.AddDoubleConst(MaxDouble).Id
    else if (ExtendedVal > 0) and (ExtendedVal <= MinDouble) then
      CurrToken.Id := SymbolTable.AddDoubleConst(MinDouble).Id
    else
      CurrToken.Id := SymbolTable.AddDoubleConst(ExtendedVal).Id;
  end
  else if CurrToken.TokenClass = tcIdentifier then
  begin
    if not DECLARE_SWITCH then
    begin
      if IsCurrText('true') then
      begin
        CurrToken.TokenClass := tcBooleanConst;
        CurrToken.Id := SymbolTable.TrueId;
        Exit;
      end
      else if IsCurrText('false') then
      begin
        CurrToken.TokenClass := tcBooleanConst;
        CurrToken.Id := SymbolTable.FalseId;
        Exit;
      end;
    end;

    if IsKeyword(CurrToken_Text) then
    begin
      CurrToken.Id := 0;
      CurrToken.TokenClass := tcKeyword;
      if FIELD_OWNER_ID = 0 then
        Exit
      else
        CurrToken.TokenClass := tcIdentifier;
    end;

    if ParametrizedTypeExpected then
      CurrToken_Text := CurrToken_Text + Parse_TypeParams;

    if FIELD_OWNER_ID > 0 then
    begin
      with SymbolTable.AddRecord do
      begin
        CurrToken.Id := Id;
        Name := CurrToken_Text;
        Kind := KindVAR;
        Level := CurrLevel;
        OwnerId := FIELD_OWNER_ID;
      end;

      FIELD_OWNER_ID := 0;
      Exit;
    end;

    if DECLARE_SWITCH then
    begin
      id := LookUp(CurrToken_Text, CurrLevel);
      if id <> 0 then
      begin
        if SymbolTable[id].Kind in [kindNAMESPACE] then
        begin
          CurrToken.Id := Id;
          Exit;
        end
        else
        begin
          if SymbolTable[Id].IsForward then
          begin

            if (SymbolTable[Id].Kind = KindTYPE) and
               (SymbolTable[Id].FinalTypeId = typeCLASS) then
            begin
              CurrToken.Id := id;
              SetForward(Id, false);
              SymbolTable[Id].Position := CurrToken.Position - 1;
              Exit;
            end;

            if LanguageId = PASCAL_LANGUAGE then
            if GetKind(id) = KindSUB then
            if GetSymbolRec(id).OverCount = 0 then
            begin
              if not ImportOnly then
                RaiseError(errRedeclaredIdentifier, [CurrToken_Text]);
            end;

            CurrToken.Id := NewTempVar;
            SetName(CurrToken.Id, GetName(Id));
            SymbolTable[Id].Position := CurrToken.Position - 1;
            Exit;
          end
          else if LanguageId = JS_LANGUAGE then
          begin
            // no problem
          end
          else
          begin
            if not (GetKind(id) in KindSUBS) then
              if not TKernel(kernel).SymbolTable[id].Host then
                if Id > TKernel(kernel).GT.Card then
                begin
                  cc := StrEql(CurrToken_Text, 'register') or
                        StrEql(CurrToken_Text, 'pascal') or
                        StrEql(CurrToken_Text, 'stdcall') or
                        StrEql(CurrToken_Text, 'cdecl') or
                        StrEql(CurrToken_Text, 'msfastcall') or
                        StrEql(CurrToken_Text, 'safecall');
                  if cc then
                    SymbolTable[id].Kind := KindNONE;

                  if not (InterfaceOnly or cc) then
                    RaiseError(errRedeclaredIdentifier, [CurrToken_Text]);
                end;
          end;
        end;
      end;

      with SymbolTable.AddRecord do
      begin
        CurrToken.Id := Id;
        Name := CurrToken_Text;
        Kind := KindVAR;
        Level := CurrLevel;

        InImplementation := IMPLEMENTATION_SECTION;

        Position := CurrToken.Position - 1;
      end;
    end
    else
    begin
      CurrToken.Id := 0;

      if WithStack.Count = 0 then
      begin
        CurrToken.Id := LookUps(CurrToken_Text, levelStack);
        if CurrToken.Id = 0 then
          CurrToken.Id := LookupInUsingList(CurrToken_Text);
      end
      else
      begin
        if WithStack.Count = 1 then
        if WithStack[0] = CurrSelfId then
        begin
          Id := LookUps(CurrToken_Text, levelStack);
          if Id > 0 then
          begin
            lev := GetSymbolRec(Id).Level;
            if lev > 0 then
            if GetSymbolRec(lev).Kind in KindSUBS then
            begin
              CurrToken.Id := Id;
              goto LabelEval;
            end;
          end;
        end;

        Id := LookupInWithStack(CurrToken_Text, PatternId, StackIsOK);
        if Id > 0 then
        begin
          if PatternId <> CurrSubId then
          if GetKind(PatternId) <> KindPROP then
          begin
            FieldId := NewField(CurrToken_Text, Id);
            SetType(FieldId, GetType(PatternId));
            Gen(OP_FIELD, Id, FieldId, FieldId);
            LastCodeRec.CodeRecTag := TAG_DISCARD_STMT;
            LastCodeRec.PatternFieldId := PatternId;

            CurrToken.Id := FieldId;
            goto LabelEval;
          end;
        end;

        if (Id = 0) and StackIsOK then
        begin
          CurrToken.Id := LookUps(CurrToken_Text, levelStack);
          if CurrToken.Id = 0 then
            CurrToken.Id := LookupInUsingList(CurrToken_Text);
        end;
      end;

      LabelEval:

      if CurrToken.Id > 0 then
      begin
        if TKernel(kernel).TypeDefList.RemTypeIds.IndexOf(CurrToken.Id) >= 0 then
          CurrToken.Id := 0;
      end;

      if CurrToken.Id = 0 then
      begin
{
        if LanguageId = BASIC_LANGUAGE then
          if IsNextText('.') then
          if LookUps(CurrToken_Text, levelStack) = 0 then
          if not HasModule(CurrToken_Text) then
          begin
            SkipWhenNotFound := true;
            try
              AddModuleFromFile(CurrToken_Text, CurrToken.Id, false,
                Format(errUndeclaredIdentifier, [CurrToken_Text]), true);
            finally
              SkipWhenNotFound := false;
            end;

          end;
}
        R := SymbolTable.AddRecord;
        R.Name := CurrToken_Text;
        R.Kind := KindNONE;
        R.Level := CurrLevel;
        CurrToken.Id := R.Id;
        Gen(OP_EVAL, 0, 0, R.Id);
        R.Position := CurrToken.Position - 1;
      end;
    end;
  end;
end;

function TBaseParser.GetPosition(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Position;
end;

procedure TBaseParser.SetPosition(Id, Position: Integer);
begin
  TKernel(kernel).SymbolTable[Id].Position := Position;
end;

procedure TBaseParser.SetVarCount(Id, value: Integer);
begin
  TKernel(kernel).SymbolTable[Id].VarCount := value;
end;

function TBaseParser.Parse_Ident: Integer;
begin
  if CurrToken.TokenClass <> tcIdentifier then
    RaiseError(errIdentifierExpected, [CurrToken.Text]);
  result := CurrToken.Id;
  if FIND_DECL_SWITCH then
  begin
    FIND_DECL_IMPLEMENTATION_SECTION := IMPLEMENTATION_SECTION;
    TKernel(kernel).FindDeclId := result;
    FIND_DECL_SWITCH := false;
  end;
  Call_SCANNER;
end;

function TBaseParser.Parse_EnumIdent: Integer;
var
  TypeID: Integer;
begin
  TypeID := LevelStack.Top;

  result := Parse_Ident;
  SetKind(result, KindCONST);
  SetType(result, TypeID);
  SetOwnerId(result, TypeID);
end;

function TBaseParser.Parse_FormalParameter: Integer;
begin
  if CurrToken.TokenClass <> tcIdentifier then
    RaiseError(errIdentifierExpected, [CurrToken.Text]);
  result := CurrToken.Id;

  TKernel(kernel).SymbolTable[result].Param := true;

  Call_SCANNER;
end;

function TBaseParser.Parse_CharLiteral: Integer;
begin
  result := CurrToken.Id;
  Call_SCANNER;
end;

function TBaseParser.Parse_PCharLiteral: Integer;
begin
  result := CurrToken.Id;
  if CurrToken.TokenClass <> tcPCharConst then
    RaiseError(errPCharLiteralExpected, []);
  Call_SCANNER;
end;

function TBaseParser.Parse_BooleanLiteral: Integer;
begin
  result := CurrToken.Id;
  Call_SCANNER;
end;

function TBaseParser.Parse_IntegerLiteral: Integer;
begin
  result := CurrToken.Id;
  Call_SCANNER;
end;

function TBaseParser.Parse_DoubleLiteral: Integer;
begin
  result := CurrToken.Id;
  Call_SCANNER;
end;

function TBaseParser.BinOp(Op, Arg1, Arg2: Integer): Integer;
begin
	result := NewTempVar;
  Gen(Op, Arg1, Arg2, result);
end;

function TBaseParser.UnaryOp(Op, Arg1: Integer): Integer;
begin
  result := NewTempVar();
  Gen(Op, Arg1, 0, result);
end;

function TBaseParser.NewVar(const VarName: String): Integer;
var
  R: TSymbolRec;
begin
  R := TKernel(kernel).SymbolTable.AddRecord;
  R.Kind := KindVAR;
  R.Level := CurrLevel;
  R.Name := VarName;
  result := R.Id;
end;

function TBaseParser.NewTempVar: Integer;
var
  R: TSymbolRec;
begin
  R := TKernel(kernel).SymbolTable.AddRecord;
  R.Kind := KindVAR;
  R.Level := CurrLevel;
  result := R.Id;
end;

function TBaseParser.NewTempVar(TypeId: Integer): Integer;
begin
  result := NewTempVar;
  SetType(result, typeID);
end;

function TBaseParser.NewConst(ATypeID: Integer; const Value: Variant): Integer;
var
  R: TSymbolRec;
begin
  case ATypeID of
    typeBOOLEAN: result := TKernel(kernel).SymbolTable.AddBooleanConst(Value).Id;
    typeINTEGER: result := TKernel(kernel).SymbolTable.AddIntegerConst(Value).Id;
    typeDOUBLE: result := TKernel(kernel).SymbolTable.AddDoubleConst(Value).Id;
{$IFNDEF PAXARM}
    typeANSISTRING: result := TKernel(kernel).SymbolTable.AddPAnsiCharConst(AnsiString(Value)).Id;
{$ENDIF}
    typeUNICSTRING: result := TKernel(kernel).SymbolTable.AddPWideCharConst(Value).Id;
  else
    begin
      R := TKernel(kernel).SymbolTable.AddRecord;
      R.Kind := KindCONST;
      R.TypeID := ATypeID;
      R.Level := CurrLevel;
      R.Value := Value;
      result := R.Id;
    end;
  end;
end;

function TBaseParser.NewConst(ATypeID: Integer): Integer;
begin
  with TKernel(kernel).SymbolTable do
  begin
    result := AddRecord.Id;
    with Records[result] do
    begin
      Kind := kindCONST;
      Level := CurrLevel;
      TypeID := ATypeID;
    end;
  end;
end;

function TBaseParser.NewTypeAlias: Integer;
var
  R: TSymbolRec;
begin
  R := TKernel(kernel).SymbolTable.AddRecord;
  R.Kind := KindTYPE;
  R.Level := CurrLevel;
  R.TypeID := typeALIAS;
  result := R.Id;
end;

function TBaseParser.NewLabel: Integer;
begin
  result := TKernel(kernel).SymbolTable.AddLabel.Id;
end;

function TBaseParser.Gen(Op, Arg1, Arg2, Res: Integer): TCodeRec;
var
  L, I: Integer;
  B: Boolean;
  Code: TCode;
  SymbolTable: TSymbolTable;
  TypeId, MemberId: Integer;
  S: String;
begin
  Code := TKernel(kernel).Code;
  SymbolTable := TKernel(kernel).SymbolTable;

  Code.N := Code.Card;

  if Op = OP_STMT then
  begin
    I := Code.Card;
    if Code[I].Op = OP_EVAL then
    begin
      result := Code[I];
      Exit;
    end;
    if Code[I].Op = OP_LABEL then
      Dec(I);
    if Code[I].Op = OP_FIELD then
    if Code[I].CodeRecTag = TAG_DISCARD_STMT then
    begin
      result := Code[Code.Card];
      Exit;
    end;
    Arg1 := CurrLevel;
  end;

  if OP = OP_COND_RAISE then
  begin
    Arg1 := FinSubLabel;
    if Res = 0 then
      Res := NewTempVar(typeBOOLEAN);
  end
  else if OP = OP_ADD_TYPEINFO then
  begin
    RemoveInstruction(OP, Arg1, -1, -1);
  end
  else if OP = OP_ASSIGN_TYPE then
    if GetSymbolRec(Arg2).Kind = KindTYPE then
    begin
      SetType(Arg1, Arg2);
      result := LastCodeRec;
      Exit;
    end;

  result := Code.Add(Op, Arg1, Arg2, Res, CurrLevel, Upcase,
                     LanguageId, CurrModule.ModuleNumber,
                     LinePos);
  if Op = OP_CALL_INHERITED then
  begin
    result.Op := OP_CALL;
    result.IsInherited := true;
  end;

  if Op = OP_FIELD then
  begin
    if GetSymbolRec(Arg1).FinalTypeId = typeCLASS then
    begin
      S := GetSymbolRec(Res).Name;
      if S <> '' then
      begin

        MemberId := TKernel(kernel).SymbolTable.
          Lookup(S, GetSymbolRec(Arg1).TerminalTypeId, UpCase, MaxInt, false);
        if MemberId <> 0 then
        if GetKind(MemberId) in [KindVAR, KindPROP] then
        begin
          TypeId := GetSymbolRec(MemberId).TypeId;
          SetType(Res, TypeId);
          result.PatternFieldId := MemberId;
        end;
      end;
    end;
  end
  else if Op = OP_ELEM then
  begin
    SymbolTable[Res].Kind := KindVAR;
    SymbolTable[Res].OwnerId := Arg1;
  end
  else if Op = OP_BEGIN_MODULE then
  begin
    CurrModule.S1 := SymbolTable.Card + 1;
    CurrModule.P1 := Code.Card;
  end
  else if Op = OP_BEGIN_SUB then
  begin
    BeginSubList.Add(Arg1, Code.Card);
  end
  else if Op = OP_END_INTERFACE_SECTION then
  begin
    CurrModule.S2 := SymbolTable.Card;
    CurrModule.P2 := Code.Card;
  end
  else if Op = OP_END_MODULE then
  begin
    CurrModule.S3 := SymbolTable.Card;
    CurrModule.P3 := Code.Card;
  end
  else if Op = OP_BEGIN_USING then
  begin
    UsingList.Add(Arg1);
  end
  else if Op = OP_END_USING then
  begin
    UsingList.DeleteValue(Arg1);
  end
  else if OP = OP_EVAL_INHERITED then
    Gen(OP_NOP, SkipLabelStack.Top, 0, 0)
  else if Op = OP_ASSIGN_CONST then
  begin
    GetSymbolRec(Arg1).Value := GetSymbolRec(Arg2).Value;
  end
  else if OP = OP_GET_ENUMERATOR then
  begin
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
  end
  else if OP = OP_CURRENT then
  begin
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
  end
  else if OP = OP_MOVE_NEXT then
  begin
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
    Gen(OP_NOP, 0, 0, 0);
  end
  else if Op = OP_SEPARATOR then
  begin
    if (not DECLARE_SWITCH) and (EXECUTABLE_SWITCH > 0) then
    begin
      b := false;
      for I:=Code.Card downto 1 do
      begin
        Op := Code[I].Op;
        if Op = OP_SET_CODE_LINE then
          break
        else if Op = OP_CALL then
          break
        else if Op = OP_PUSH then
        begin
          b := true;
          break;
        end;
      end;

      Gen(OP_SET_CODE_LINE, 0, 0, 0);
      if DEBUG_MODE then if not b then
      begin
        L := NewLabel;
        Gen(OP_CHECK_PAUSE, L, 0, 0);
        SetLabelHere(L);
      end;
    end;
  end;
end;

procedure TBaseParser.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).Code.N := TKernel(kernel).Code.Card;
  TKernel(kernel).RaiseError(Message, params);
end;

procedure TBaseParser.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

function TBaseParser.AddKeyword(const S: String): Integer;
begin
  result := keywords.Add(S);
end;

procedure TBaseParser.AddKeywords(L: TStrings);
var
  I: Integer;
begin
  for I := 0 to L.Count - 1 do
    AddKeyword(L[I]);
end;

function TBaseParser.IsKeyword(const S: String): Boolean;
var
  I: Integer;
begin
  I := keywords.IndexOfEx(S, UpCase);
  result := I >= 0;
  if result then
    result := hidden_keywords.IndexOf(I) = - 1;
end;

procedure TBaseParser.AddOperator(const S1, S2: String);
begin
  operators.Add(S1, S2);
end;

function TBaseParser.OperatorIndex(const S: String): Integer;
begin
  result := operators.Keys.IndexOf(S);
end;

function TBaseParser.IsCurrText(const S: String): Boolean;
begin
  if UpCase then
    result := StrEql(CurrToken.Text, S)
  else
    result := CurrToken.Text = S;
end;

function TBaseParser.IsCurrTextIn(L: TStrings): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := 0 to L.Count - 1 do
    if IsCurrText(L[I]) then
    begin
      result := true;
      Exit;
    end;
end;

function TBaseParser.IsNextText(const S: String): Boolean;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  result := IsCurrText(S);
  Pop_SCANNER;

  scanner.VarNameList.Clear;

  scanner.LookForward := false;
end;

function TBaseParser.IsNext2Text(const S: String): Boolean;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  ReadTokenEx;
  result := IsCurrText(S);
  Pop_SCANNER;

  scanner.LookForward := false;
end;

function TBaseParser.GetNextText: String;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  result := CurrToken.Text;
  Pop_SCANNER;

  scanner.LookForward := false;
end;

function TBaseParser.GetNext2Text: String;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  ReadTokenEx;
  result := CurrToken.Text;
  Pop_SCANNER;

  scanner.LookForward := false;
end;

function TBaseParser.GetNextTokenClass: TTokenClass;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  result := CurrToken.TokenClass;
  Pop_SCANNER;

  scanner.LookForward := false;
end;

function TBaseParser.GetNext2TokenClass: TTokenClass;
begin
  scanner.LookForward := true;

  Push_SCANNER;
  ReadTokenEx;
  ReadTokenEx;
  result := CurrToken.TokenClass;
  Pop_SCANNER;

  scanner.LookForward := false;
end;

procedure TBaseParser.Match(const S: String);
begin
  if IsCurrText(S) then
    Call_SCANNER
  else
  begin
    LinePos := scanner.LinePos;
    Gen(OP_NOP, 0, 0, 0);
    RaiseError(errTokenExpected, [S, CurrToken.Text]);
  end;
end;

procedure TBaseParser.MatchFinal(const S: String);
begin
  if IsCurrText(S) then
  begin
    // ok
  end
  else
  begin
    LinePos := scanner.LinePos;
    Gen(OP_NOP, 0, 0, 0);
    RaiseError(errTokenExpected, [S, CurrToken.Text]);
  end;
end;


procedure TBaseParser.SafeMatch(const S: String);
begin
  if IsCurrText(S) then
    ReadToken
  else
  begin
    LinePos := scanner.LinePos;
    Gen(OP_NOP, 0, 0, 0);
    RaiseError(errTokenExpected, [S, CurrToken.Text]);
  end;
end;

function TBaseParser.NotMatch(const S: String): Boolean;
begin
  if not IsCurrText(S) then
    result := true
  else
  begin
    result := false;
    Call_SCANNER;
  end;
end;

function TBaseParser.IsEOF: Boolean;
begin
  result := scanner.IsEOF;
end;

function TBaseParser.IsNewLine: Boolean;
begin
  result := scanner.IsNewLine;
end;

function TBaseParser.GetType(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].TypeID;
end;

procedure TBaseParser.SetType(Id: Integer; TypeID: Integer);
begin
  TKernel(kernel).SymbolTable[Id].TypeID := TypeID;
end;

procedure TBaseParser.SetValue(Id: Integer; const Value: Variant);
begin
  TKernel(kernel).SymbolTable[Id].Value := Value;
end;

function TBaseParser.GetLevel(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Level;
end;

procedure TBaseParser.SetLevel(Id: Integer; Level: Integer);
begin
  TKernel(kernel).SymbolTable[Id].Level := Level;
end;

procedure TBaseParser.SetTypedConst(Id: Integer);
begin
  TKernel(kernel).SymbolTable[Id].TypedConst := true;
  SetKind(Id, KindVAR);
end;

procedure TBaseParser.SetKind(Id: Integer; Kind: Integer);
var
  S: TSymbolRec;
begin
  S := TKernel(kernel).SymbolTable[Id];
  S.Kind := Kind;
  if Kind = KindNONE then
  begin
    S.Param := false;
    S.Register := 0;
    S.TypeId := 0;
  end;

end;

function TBaseParser.GetKind(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Kind;
end;

procedure TBaseParser.SetFinal(Id: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[Id].IsFinal := value;
end;

procedure TBaseParser.SetAbstract(Id: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[Id].IsAbstract := value;
end;

function TBaseParser.GetFullName(Id: Integer): String;
begin
  result := TKernel(kernel).SymbolTable[Id].FullName;
end;

function TBaseParser.GetName(Id: Integer): String;
begin
  result := TKernel(kernel).SymbolTable[Id].Name;
end;

procedure TBaseParser.SetName(Id: Integer; const S: String);
begin
  TKernel(kernel).SymbolTable[Id].Name := S;
end;

function TBaseParser.GetCount(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable[Id].Count;
end;

procedure TBaseParser.SetCount(Id: Integer; value: Integer);
begin
  TKernel(kernel).SymbolTable[Id].Count := value;
end;

function TBaseParser.GetResultId(SubId: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.GetResultId(SubId);
end;

function TBaseParser.GetParamId(SubId, J: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.GetParamId(SubId, J);
end;

procedure TBaseParser.SetPatternId(Id: Integer; PatternId: Integer);
begin
  TKernel(kernel).SymbolTable[Id].PatternId := PatternId;
end;

procedure TBaseParser.SetAncestorId(Id: Integer; AncestorId: Integer);
begin
  TKernel(kernel).SymbolTable[Id].AncestorId := AncestorId;
end;

procedure TBaseParser.SetParam(Id: Integer; value: boolean);
begin
  TKernel(kernel).SymbolTable[Id].Param := value;
end;

procedure TBaseParser.SetOwnerId(Id: Integer; OwnerId: Integer);
begin
  TKernel(kernel).SymbolTable[Id].OwnerId := OwnerId;
end;

procedure TBaseParser.SetByRef(Id: Integer);
begin
  TKernel(kernel).SymbolTable[Id].ByRef := true;
end;

procedure TBaseParser.SetIsConst(Id: Integer);
begin
  TKernel(kernel).SymbolTable[Id].IsConst := true;
end;

procedure TBaseParser.SetOptional(Id: Integer);
begin
  TKernel(kernel).SymbolTable[Id].Optional := true;
end;

procedure TBaseParser.SetPacked(Id: Integer);
begin
  SetAlignment(Id, 1);
end;

procedure TBaseParser.SetCallConvention(Id: Integer; value: Integer);
begin
  if TargetPlatform = tpWIN64 then
    TKernel(kernel).SymbolTable[Id].CallConv := cc64
  else
    TKernel(kernel).SymbolTable[Id].CallConv := value;
end;

procedure TBaseParser.SetOverloaded(SubId: Integer);
var
  Lst: TIntegerList;
  SymbolTable: TSymbolTable;
  I, K: Integer;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  lst := SymbolTable.LookupAll(SymbolTable[SubId].Name, SymbolTable[SubId].Level, Upcase);
  try
    K := Lst.Count;
    for I := 0 to Lst.Count - 1 do
      if Lst[I] < FirstLocalId then
        Dec(K);
    TKernel(kernel).SymbolTable[SubId].OverCount := K;
  finally
    FreeAndNil(Lst);
  end;
end;

procedure TBaseParser.SetCallMode(Id: Integer; value: Integer);
var
  S: String;
begin
  S := TKernel(kernel).SymbolTable[Id].Name;
  if StrEql(S, 'AfterConstruction') or
     StrEql(S, 'BeforeDestruction') or
     StrEql(S, 'SafeCallexception') or
     StrEql(S, 'Dispatch') or
     StrEql(S, 'DefaultHandler') or
     StrEql(S, 'NewInstance') or
     StrEql(S, 'FreeInstance') then
     value := cmNONE;
  TKernel(kernel).SymbolTable[Id].CallMode := value;
end;

procedure TBaseParser.SetLabelHere(Id: Integer;
                                   Arg2: Integer = 0;
                                   Res: Integer = 0);
begin
  Gen(OP_LABEL, Id, Arg2, Res);
end;

function TBaseParser.GetFinSubLabel: Integer;
begin
  if ExitLabelStack.Count = 0 then
    result := 0
  else
    result := ExitLabelStack.Top;
end;

function TBaseParser.GetCurrLevel: Integer;
begin
  if levelStack.Count = 0 then
    result := 0
  else
    result := levelStack.Top;
end;

function TBaseParser.GetCurrSubId: Integer;
begin
  result := GetCurrLevel;
end;

function TBaseParser.GetOuterClassId: Integer;
var
  I, temp, K: Integer;
begin
  result := typePOINTER;
  K := 0;

  for I := levelStack.Count - 1 downto 0 do
  begin
    temp := levelStack[I];

    if temp > 0 then
    if GetSymbolRec(temp).Kind = KindTYPE then
    if GetSymbolRec(temp).FinalTypeId = typeCLASS then
    begin
      Inc(K);
      if K = 2 then
      begin
        result := temp;
        Exit;
      end;
    end;
  end;
end;


function TBaseParser.GetCurrResultId: Integer;
var
  I: Integer;
begin
  I := levelStack.Count - 1;
  result := levelStack[I];
  while not (GetKind(result) in KindSUBS) do
  begin
    Dec(I);
    result := levelStack[I];
  end;
  result := TKernel(kernel).SymbolTable.GetResultId(result);
end;

function TBaseParser.GetCurrSelfId: Integer;
var
  I, SubId, L: Integer;
begin
  result := 0;
  SubId := 0;

  for I := levelStack.Count - 1 downto 0 do
  begin
    SubId := levelStack[I];
    if not GetSymbolRec(SubId).Kind in KindSUBS  then
      Exit;
    L := GetLevel(SubId);
    if L = 0 then
      Exit;
    if GetSymbolRec(L).Kind = KindTYPE then
      break;
  end;

  result := TKernel(kernel).SymbolTable.GetSelfId(SubId);
end;

function TBaseParser.GetNilId: Integer;
begin
  result := TKernel(kernel).SymbolTable.NilId;
end;

function TBaseParser.GetCurrExceptionObjectId: Integer;
begin
  result := TKernel(kernel).SymbolTable.CurrExceptionObjectId;
end;

function TBaseParser.GetEmptySetId: Integer;
begin
  result := TKernel(kernel).SymbolTable.EmptySetId;
end;

procedure TBaseParser.BeginSub(SubId: Integer);
var
  SelfId, L: Integer;
begin
  SetKind(SubID, KindSUB);
  SetCallConvention(SubID, CallConv);

  levelStack.Push(SubId);

  NewVar('result');
  SelfId := NewVar('');
  SetKind(SelfId, KindNONE);

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  if TKernel(kernel).SymbolTable[SubId].IsNestedSub then
  begin
    L := NewVar('%RBP');
    SetType(L, typePOINTER);
    if TargetPlatform <> tpWIN64 then
      SetCallConvention(SubID, ccSTDCALL);
  end;
  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;

  L := NewLabel;
  ExitLabelStack.Push(L);
end;

procedure TBaseParser.RemoveSub;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;

  I := Code.Card;
  repeat
    if Code[I].Op = OP_EVAL then
    begin
      Dec(I);
      continue;
    end;

    if Code[I].Op = OP_ASSIGN_TYPE then
      Dec(I)
    else if Code[I].Op = OP_ASSIGN_CONST then
      Dec(I)
    else if Code[I].Op = OP_CREATE_DYNAMIC_ARRAY_TYPE then
      Dec(I)
    else if Code[I].Op = OP_ADD_MESSAGE then
      Dec(I)
    else if Code[I].Op = OP_GO then
    begin
      Code[I].Op := OP_NOP;
      Exit;
    end
    else
    begin
      Code[I].Op := OP_NOP;
      Dec(I);
    end;

  until false;
end;

procedure TBaseParser.RemoveSub(SubId: Integer);
var
  Code: TCode;
  I: Integer;
  R: TCodeRec;
  Inside: Boolean;
  id: Integer;
begin
  Code := TKernel(kernel).Code;
  Inside := false;

  for I := 1 to Code.Card do
  begin
    R := Code[I];
    if (R.Op = OP_BEGIN_SUB) and (R.Arg1 = SubId) then
    begin
      R.Op := OP_NOP;
      Inside := true;
    end
    else if (R.Op = OP_FIN_SUB) and (R.Arg1 = SubId) then
    begin
      R.Op := OP_NOP;
      Exit;
    end
    else if Inside then
      R.Op := OP_NOP;
  end;

  Id := TKernel(kernel).SymbolTable.GetResultId(SubId);
  SetName(Id, '');
  SetKind(Id, KindNONE);
  SetName(SubId, '');
  SetKind(SubId, KindNONE);
end;

procedure TBaseParser.BeginClassConstructor(SubId, ClassTypeId: Integer);
var
  L: Integer;
  ResId, SelfId: Integer;
  Id: Integer;
begin
  SetKind(SubID, KindCONSTRUCTOR);
  SetCallConvention(SubID, ccREGISTER);

  levelStack.Push(SubId);

  ResId  := NewVar(''); // result
  SelfId := NewVar('Self');

  SetKind(ResId, KindNONE);
  SetLevel(SubId, ClassTypeId);
  TKernel(kernel).SymbolTable[SelfId].Param := true;

  Gen(OP_ASSIGN_TYPE, SubId,  ClassTypeId, 0);
  Gen(OP_ASSIGN_TYPE, SelfId, ClassTypeId, 0);

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  L := NewLabel;
  ExitLabelStack.Push(L);

  AddTypeParameters(SubId);

  if TargetPlatform <> tpWIN32 then
  begin
    Id := NewVar('%DL');
    SetType(Id, typePOINTER);
  end;
  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;
end;

procedure TBaseParser.BeginClassDestructor(SubId, ClassTypeId: Integer);
var
  L: Integer;
  ResId, SelfId: Integer;
begin
  SetKind(SubID, KindDESTRUCTOR);
  SetCallConvention(SubID, CallConv);

  levelStack.Push(SubId);

  ResId  := NewVar(''); // result
  SelfId := NewVar('Self');

  SetKind(ResId, KindNONE);
  SetLevel(SubId, ClassTypeId);
  TKernel(kernel).SymbolTable[SelfId].Param := true;

  Gen(OP_ASSIGN_TYPE, SubId,  ClassTypeId, 0);
  Gen(OP_ASSIGN_TYPE, SelfId, ClassTypeId, 0);

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  L := NewLabel;
  ExitLabelStack.Push(L);

  AddTypeParameters(SubId);

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;
end;

procedure TBaseParser.BeginClassMethod(SubId, ClassTypeId: Integer;
                                       HasResult: Boolean;
                                       IsSharedMethod: Boolean;
                                       IsMethodImpl: Boolean); // pascal only
var
  L: Integer;
  ResId, SelfId: Integer;
begin
  InitTypeExt(ClassTypeId, GetName(SubId), IsMethodImpl);

  SetKind(SubID, KindSUB);
  SetCallConvention(SubID, CallConv);

  levelStack.Push(SubId);

  if HasResult then
    NewVar('result')
  else
  begin
    ResId := NewVar('');
    SetKind(ResId, KindNONE);
  end;

  SelfId := NewVar('Self');

  SetLevel(SubId, ClassTypeId);

  TKernel(kernel).SymbolTable[SelfId].Param := true;
  TKernel(kernel).SymbolTable[SubId].IsSharedMethod := IsSharedMethod;

  SetType(SubId, typeVOID);
  Gen(OP_ASSIGN_TYPE, SelfId, ClassTypeId, 0);

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  L := NewLabel;
  ExitLabelStack.Push(L);

  AddTypeParameters(SubId);

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;
end;

procedure TBaseParser.BeginStructureConstructor(SubId, StructTypeId: Integer);
begin
  BeginStructureMethod(SubId, StructTypeId, false, false);
  SetKind(SubId, kindCONSTRUCTOR);
  SetType(SubId, StructTypeId);
  SetCallConvention(SubID, ccREGISTER);
end;

procedure TBaseParser.BeginStructureDestructor(SubId, StructTypeId: Integer);
begin
  BeginStructureMethod(SubId, StructTypeId, false, false);
  SetKind(SubId, kindDESTRUCTOR);
  SetCallConvention(SubID, ccREGISTER);
end;

procedure TBaseParser.BeginStructureMethod(SubId, StructTypeId: Integer;
                                           HasResult: Boolean;
                                           IsSharedMethod: Boolean);
var
  L: Integer;
  ResId, SelfId: Integer;
begin
  SetKind(SubID, KindSUB);
  SetCallConvention(SubID, CallConv);

  levelStack.Push(SubId);

  if HasResult then
    NewVar('result')
  else
  begin
    ResId := NewVar('');
    SetKind(ResId, KindNONE);
  end;

  SelfId := NewVar('Self');

  SetLevel(SubId, StructTypeId);

  TKernel(kernel).SymbolTable[SelfId].Param := true;
  TKernel(kernel).SymbolTable[SubId].IsSharedMethod := IsSharedMethod;

  TKernel(kernel).SymbolTable[SelfId].ByRef := true;

  SetType(SubId, typeVOID);
  Gen(OP_ASSIGN_TYPE, SelfId, StructTypeId, 0);

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  L := NewLabel;
  ExitLabelStack.Push(L);

  AddTypeParameters(SubId);

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;
end;

procedure TBaseParser.BeginStructureOperator(SubId, StructTypeId: Integer);
var
  L: Integer;
  SelfId: Integer;
begin
  SetKind(SubID, KindSUB);
  SetCallConvention(SubID, CallConv);

  levelStack.Push(SubId);

  NewVar('result');
  SelfId := NewVar('');
  GetSymbolRec(SelfId).Kind := KindNONE;

  SetLevel(SubId, StructTypeId);

  TKernel(kernel).SymbolTable[SelfId].Param := true;
  TKernel(kernel).SymbolTable[SubId].IsSharedMethod := true;
  TKernel(kernel).SymbolTable[SubId].CallMode := cmSTATIC;

  L := NewLabel;
  Gen(OP_GO, L, 0, 0);
  SkipLabelStack.Push(L);

	Gen(OP_BEGIN_SUB, SubId, 0, 0);
  L := NewLabel;
  SkipLabelStack.Push(L);

  L := NewLabel;
  ExitLabelStack.Push(L);

  if TargetPlatform in [tpOSX32, tpIOSSim] then
  begin
    L := NewVar('%RBX');
    SetType(L, typePOINTER);
    L := NewVar('%RDI');
    SetType(L, typePOINTER);
  end;
end;

procedure TBaseParser.BeginInterfaceMethod(SubId, IntfTypeId: Integer;
                                           HasResult: Boolean);
begin
  BeginClassMethod(SubId, IntfTypeId, HasResult, false, false);
  SetVisibility(SubId, cvPublic);
end;

procedure TBaseParser.CheckAbstract(SubId: Integer);
var
  I, Op: Integer;
  Code: TCode;
  S, Q: String;
  R: TCodeRec;
begin
  Code := TKernel(kernel).Code;
  S := GetFullName(SubId);
  for I:=Code.Card downto 1 do
  begin
    R := Code[I];
    Op := R.Op;
    if Op = OP_ERR_ABSTRACT then
    begin
      if StrEql(GetFullName(R.Res), S) then
      begin
        S := GetSymbolRec(R.Res).Sig;
        Q := GetSymbolRec(SubId).Sig;
        if StrEql(S, Q) then
        begin
          CreateError(errNoDefinitionForAbstractMethodAllowed, [S]);
          break;
        end;
      end;
    end
    else if Op = OP_BEGIN_MODULE then
      break;
  end;
end;

procedure TBaseParser.BeginLoop;
begin
  Inc(LoopCounter);
  LoopCounterStack.Push(LoopCounter);
	Gen(OP_BEGIN_LOOP, LoopCounterStack.Top, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
end;

procedure TBaseParser.EndLoop;
var
  b: Boolean;
  Code: TCode;
  I: Integer;
begin
  // reserved for epilogue
	Gen(OP_EPILOGUE_LOOP, LoopCounterStack.Top, 0, 0);

  Code := TKernel(kernel).Code;

  b := false;

  I := Code.Card;
  repeat
    Dec(I);

    if Code[I].Op = OP_END_LOOP then
      break;
    if Code[I].Op = OP_BEGIN_LOOP then
      break;
    if Code[I].Op = OP_TRY_ON then
    begin
      b := true;
      break;
    end;

  until false;

  if b then
  begin
    if BreakStack.Count > 0 then
    begin
      LastCodeRec.BreakLabel := BreakStack.TopLabel();
      LastCodeRec.LoopLabel := BreakStack.Top.loopLabel;
    end;
    if ContinueStack.Count > 0 then
      LastCodeRec.ContinueLabel := ContinueStack.TopLabel();

    Gen(OP_NOP, 0, 0, 0); // OP_EXCEPT_SEH
    Gen(OP_NOP, 0, 0, 0); // LABEL
    Gen(OP_NOP, 0, 0, 0); // FINALLY

    Gen(OP_NOP, 0, 0, 0); //OP_COND_RAISE
    Gen(OP_NOP, 0, 0, 0); //OP_LABEL
    Gen(OP_NOP, 0, 0, 0); //OP_TRY_OFF
    Gen(OP_NOP, 0, 0, 0); //OP_LABEL
  end;

	Gen(OP_END_LOOP, LoopCounterStack.Top, 0, 0);
  LoopCounterStack.Pop;
end;

procedure TBaseParser.InitSub(var SubId: Integer);
begin
  SetLabelHere(SubId);
	Gen(OP_INIT_SUB, SubId, 0, 0);
  // reserved for prologue
  Gen(OP_NOP, 0, 0, 0);
end;

procedure TBaseParser.EndSub(SubId: Integer);
var
  L: Integer;
  I: Integer;
  Code: TCode;
  R: TCodeRec;
begin
  Code := TKernel(kernel).Code;

  L := SkipLabelStack.Top;
  SetLabelHere(L);
  SkipLabelStack.Pop;

  Gen(OP_EPILOGUE_SUB,  SubId, 0, 0);

  // reserved for epilogue
  Gen(OP_NOP, 0, 0, 0);  // OP_EXCEPT_SEH
  Gen(OP_NOP, 0, 0, 0);  // LABEL
  Gen(OP_NOP, 0, 0, 0);  // FINALLY

  for I:=Code.Card downto 1 do
  begin
    R := Code[I];
    if (R.Op = OP_BEGIN_SUB) and (R.Arg1 = SubId) then
      break;
    if (R.Op = OP_DECLARE_LOCAL_VAR) and (R.Arg1 = SubId) then
    begin
      if GetKind(R.Arg2) = KindVAR then
      begin
        if GetSymbolRec(R.Arg2).IsConst then
          continue;
        if GetSymbolRec(R.Arg2).TypedConst then
          continue;
        if GetSymbolRec(R.Arg2).ByRef then
          continue;

        if not TKernel(kernel).SymbolTable[R.Arg2].Param then
          Gen(OP_DESTROY_LOCAL_VAR, R.Arg2, 0, 0);
      end;
    end;
  end;

  // reserved for epilogue
	Gen(OP_NOP, 0, 0, 0); //OP_COND_RAISE
	Gen(OP_NOP, 0, 0, 0); //OP_LABEL
	Gen(OP_NOP, 0, 0, 0); //OP_TRY_OFF
	Gen(OP_NOP, 0, 0, 0); //OP_LABEL

  levelStack.Pop;
  Gen(OP_END_SUB, SubId, 0, 0);

  L := ExitLabelStack.Top;
  SetLabelHere(L);
  ExitLabelStack.Pop;
  Gen(OP_FIN_SUB, SubId, 0, 0);

  L := SkipLabelStack.Top;
  SetLabelHere(L);
  SkipLabelStack.Pop;
end;

procedure TBaseParser.BeginProperty(PropId, ClassTypeId: Integer);
begin
  SetKind(PropID, KindPROP);
  levelStack.Push(PropId);
  SetLevel(PropId, ClassTypeId);
end;

procedure TBaseParser.EndProperty(PropId: Integer);
begin
  levelStack.Pop;
end;

procedure TBaseParser.BeginNamespace(Id: Integer; Jump: Boolean = true);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  SetKind(Id, KindNAMESPACE);
  levelStack.Push(Id);

  if Jump then if not DeclaredInSub then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    SkipLabelStack.Push(L);
  end;

  Gen(OP_BEGIN_NAMESPACE, Id, 0, 0);
  Gen(OP_BEGIN_USING, Id, 0, 0);
end;

procedure TBaseParser.EndNamespace(Id: Integer; Jump: Boolean = true);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  Gen(OP_END_NAMESPACE, Id, 0, 0);

  levelStack.Pop;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  if Jump then if not DeclaredInSub then
  begin
    L := SkipLabelStack.Top;
    SetLabelHere(L);
    SkipLabelStack.Pop;
  end;

  Gen(OP_END_USING, Id, 0, 0);
end;

procedure TBaseParser.BeginRecordType(TypeId: Integer);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeRECORD);
  levelStack.Push(TypeId);

  SetAlignment(TypeId, Alignment);

  if not DeclaredInSub then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    SkipLabelStack.Push(L);
  end;

  Gen(OP_BEGIN_RECORD_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'RecordType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;

  AddTypeParameters(TypeId);
end;

procedure TBaseParser.EndRecordType(TypeId: Integer);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  TypeParams.Clear;

  Gen(OP_END_RECORD_TYPE, TypeId, 0, 0);

  levelStack.Pop;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  if not DeclaredInSub then
  begin
    L := SkipLabelStack.Top;
    SetLabelHere(L);
    SkipLabelStack.Pop;
  end;
end;

procedure TBaseParser.BeginClassType(TypeId: Integer);
var
  L, SZ: Integer;
  DeclaredInSub: Boolean;
  R: TSymbolRec;
  ClassRefTypeId: Integer;
  S, FullName: String;
  C: TClass;
begin
  SZ := TObject.InstanceSize;
  S := GetName(TypeId);
  FullName := GetSymbolRec(TypeId).FullName;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  SetAlignment(TypeId, 1);

  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeCLASS);

  TKernel(kernel).SymbolTable[TypeId].AncestorId := H_TObject;

  R := TKernel(kernel).SymbolTable.AddClassRefVar(0);

  if TKernel(kernel).ClassFactory.LookupFullName(FullName) = nil then
  begin
    C := TKernel(kernel).ClassFactory.CreatePaxClass(FullName, SZ, TObject,
      TKernel(kernel).GetDestructorAddress);
    R.Value := Integer(C);
    TKernel(kernel).SymbolTable[TypeId].PClass := C;
  end;

  ClassRefTypeId := TKernel(kernel).SymbolTable.RegisterClassReferenceType(0, '', TypeId);
  TKernel(kernel).SymbolTable[ClassRefTypeId].Host := false;
  R.TypeID := ClassRefTypeId;

  levelStack.Push(TypeId);

  if not DeclaredInSub then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    SkipLabelStack.Push(L);
  end;

  Gen(OP_BEGIN_CLASS_TYPE, TypeId, 0, R.Id);

  AddTypeParameters(TypeId);
end;

procedure TBaseParser.EndClassType(TypeId: Integer; IsForward: Boolean = false);
var
  I, L, P: Integer;
  DeclaredInSub: Boolean;
  Code: TCode;
  R: TCodeRec;
  S: String;
begin
  Code := TKernel(kernel).Code;

  if IsGeneric(TypeId) then
  if not CurrModule.IsExtra then
  begin
    for I := Code.Card downto 1 do
    begin
      R := Code[I];
      if R.Op = OP_BEGIN_CLASS_TYPE then
        if R.Arg1 = TypeId then
          break;
       if R.Op = OP_ADD_ANCESTOR then
         if R.Arg1 = TypeId then
         begin
           S := GetName(R.Arg2);
           P := PosCh('<', S);
           if P > 0 then
           begin
             S := Copy(S, 1, P - 1);
             TKernel(kernel).TypeDefList.FindTypeDef(TypeId).AncestorName := S;
           end;
         end;
    end;
  end;

  TypeParams.Clear;

  if IsForward then
  begin
    L := Code.Card;
    while Code[L].Op <> OP_BEGIN_CLASS_TYPE do Dec(L);
    Code[L].Op := OP_NOP;
  end
  else
    Gen(OP_END_CLASS_TYPE, TypeId, 0, 0);

  levelStack.Pop;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  if not DeclaredInSub then
  begin
    L := SkipLabelStack.Top;
    SetLabelHere(L);
    SkipLabelStack.Pop;
  end;
end;

procedure TBaseParser.BeginMethodRefType(TypeId: Integer);
begin
  BeginInterfaceType(TypeId);
  SetNewGuid(TypeId);
  SetPacked(TypeID);
  Gen(OP_ADD_INTERFACE, TypeId, H_IUnknown, 0);
end;

procedure TBaseParser.EndMethodRefType(TypeId: Integer);
begin
  EndInterfaceType(TypeId, false);
end;

procedure TBaseParser.BeginInterfaceType(TypeId: Integer);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  SetAlignment(TypeId, 1);

  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeINTERFACE);

  TKernel(kernel).SymbolTable.AddDoubleConst(0);
  TKernel(kernel).SymbolTable.AddDoubleConst(0);

  levelStack.Push(TypeId);

  if not DeclaredInSub then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    SkipLabelStack.Push(L);
  end;

  Gen(OP_BEGIN_INTERFACE_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'InterfaceType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;

  AddTypeParameters(TypeId);
end;

procedure TBaseParser.SetGuid(IntfTypeId: Integer; const S: String);
var
  D: packed record
       D1, D2: Double;
     end;
  G: TGUID;
begin
{$IFDEF VARIANTS}
  if S[1] = '(' then
    G := SysUtils.StringToGUID(Copy(S, 2, Length(S) - 2))
  else
    G := SysUtils.StringToGUID(S);
{$ELSE}
  if S[1] = '(' then
    G := StringToGUID(Copy(S, 2, Length(S) - 2))
  else
    G := StringToGUID(S);
{$ENDIF}
  Move(G, D, SizeOf(G));
  TKernel(kernel).SymbolTable[IntfTypeId + 1].Value := D.D1;
  TKernel(kernel).SymbolTable[IntfTypeId + 2].Value := D.D2;
end;

{$IFDEF PAXARM}
function CoCreateGuid(out guid: TGUID): HResult;
begin
end;
{$ELSE}
{$IFDEF LINUX}
 // will use uuid.pas in hash folder:
function CoCreateGuid(out guid: TGUID): HResult;
begin
   if uuid.uuid_create(GUID) then result:=1
   else result:=0;
end;
{$ELSE}
function CoCreateGuid(out guid: TGUID): HResult; stdcall;
  external 'ole32.dll' name 'CoCreateGuid';
{$ENDIF}
{$ENDIF}

function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(Guid);
end;

procedure TBaseParser.SetNewGuid(IntfTypeId: Integer);
var
  D: packed record
       D1, D2: Double;
     end;
  G: TGUID;
begin
{$IFDEF VARIANTS}
  SysUtils.CreateGUID(G);
{$ELSE}
  CreateGUID(G);
{$ENDIF}
  Move(G, D, SizeOf(G));
  TKernel(kernel).SymbolTable[IntfTypeId + 1].Value := D.D1;
  TKernel(kernel).SymbolTable[IntfTypeId + 2].Value := D.D2;
end;

procedure TBaseParser.EndInterfaceType(TypeId: Integer; IsForward: Boolean);
var
  L: Integer;
  DeclaredInSub: Boolean;
  Code: TCode;
begin
  TypeParams.Clear;

  if IsForward then
  begin
    Code := TKernel(kernel).Code;
    L := Code.Card;
    while Code[L].Op <> OP_BEGIN_INTERFACE_TYPE do Dec(L);
    Code[L].Op := OP_NOP;
  end
  else
    Gen(OP_END_INTERFACE_TYPE, TypeId, 0, 0);

  levelStack.Pop;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  if not DeclaredInSub then
  begin
    L := SkipLabelStack.Top;
    SetLabelHere(L);
    SkipLabelStack.Pop;
  end;
end;

procedure TBaseParser.BeginArrayType(TypeId: Integer);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeARRAY);

  SetAlignment(TypeId, 1);

  levelStack.Push(TypeId);

  if not DeclaredInSub then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    SkipLabelStack.Push(L);
  end;

  Gen(OP_BEGIN_ARRAY_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'ArrayType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;

  AddTypeParameters(GetSymbolRec(TypeId).Level);
end;

procedure TBaseParser.EndArrayType(TypeId: Integer);
var
  L: Integer;
  DeclaredInSub: Boolean;
begin
  Gen(OP_END_ARRAY_TYPE, TypeId, 0, 0);

  levelStack.Pop;

  DeclaredInSub := GetKind(levelStack.Top) = KindSUB;

  if not DeclaredInSub then
  begin
    L := SkipLabelStack.Top;
    SetLabelHere(L);
    SkipLabelStack.Pop;
  end;
end;

procedure TBaseParser.BeginSetType(TypeId, TypeBaseId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeSET);
  SetPatternId(TypeId, TypeBaseId);

  Gen(OP_BEGIN_SET_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'SetType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
  if GetName(TypeBaseId) = '' then
  begin
    SetName(TypeBaseId, 'OriginSetType_' + IntToStr(TypeBaseId));
    Gen(OP_ADD_TYPEINFO, TypeBaseId, 0, 0);
  end;
end;

procedure TBaseParser.EndSetType(TypeId: Integer);
begin
  Gen(OP_END_SET_TYPE, TypeId, 0, 0);
end;

procedure TBaseParser.BeginEnumType(TypeId, TypeBaseId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeENUM);
  SetPatternId(TypeId, TypeBaseId);

  levelStack.Push(TypeId);

  Gen(OP_BEGIN_ENUM_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'EnumType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
end;

procedure TBaseParser.EndEnumType(TypeId, ACount: Integer);
begin
  Gen(OP_END_ENUM_TYPE, TypeId, 0, 0);

  SetCount(TypeId, ACount);

  levelStack.Pop;
end;

procedure TBaseParser.BeginSubrangeType(TypeId, TypeBaseId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, TypeBaseId);
  levelStack.Push(TypeId);

  Gen(OP_BEGIN_SUBRANGE_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'SubType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
end;

procedure TBaseParser.EndSubrangeType(TypeId: Integer);
begin
  Gen(OP_END_SUBRANGE_TYPE, TypeId, 0, 0);

  levelStack.Pop;
end;

procedure TBaseParser.BeginPointerType(TypeId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typePOINTER);

  Gen(OP_BEGIN_POINTER_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'PointerType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
end;

procedure TBaseParser.EndPointerType(TypeId: Integer);
begin
  Gen(OP_END_POINTER_TYPE, TypeId, 0, 0);
end;

procedure TBaseParser.BeginHelperType(TypeId, TrueTypeId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeHELPER);
  Gen(OP_BEGIN_HELPER_TYPE, TypeId, TrueTypeId, 0);
  levelStack.Push(TypeId);
end;

procedure TBaseParser.EndHelperType(TypeId: Integer);
begin
  Gen(OP_END_HELPER_TYPE, TypeId, 0, 0);
  levelStack.Pop;
end;

procedure TBaseParser.BeginClassReferenceType(TypeId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeCLASSREF);
  Gen(OP_BEGIN_CLASSREF_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'ClassRefType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
end;

procedure TBaseParser.EndClassReferenceType(TypeId: Integer);
begin
  Gen(OP_END_CLASSREF_TYPE, TypeId, 0, 0);
end;

procedure TBaseParser.BeginDynamicArrayType(TypeId: Integer);
var
  L: Integer;
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeDYNARRAY);

  Gen(OP_BEGIN_DYNARRAY_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'DynarrayType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
  L := GetLevel(TypeId);
  AddTypeParameters(L);
  if L = 0 then
    Exit;
  while GetKind(L) in kindSUBS do
    L := GetLevel(L);
  SetLevel(TypeId, L);
end;

procedure TBaseParser.EndDynamicArrayType(TypeId: Integer);
begin
  Gen(OP_END_DYNARRAY_TYPE, TypeId, 0, 0);
end;

procedure TBaseParser.BeginOpenArrayType(TypeId: Integer);
var
  L: Integer;
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeOPENARRAY);

  L := GetLevel(TypeId);
  if L = 0 then
    Exit;
  while GetKind(L) in kindSUBS do
    L := GetLevel(L);
  SetLevel(TypeId, L);
end;

procedure TBaseParser.EndOpenArrayType(TypeId: Integer; const ElemName: String);
var
  HighId: Integer;
begin
  HighId := NewTempVar;
  SetLevel(HighId, CurrLevel);
  SetType(HighId, typeINTEGER);
//  GetSymbolRec(HighId).Param := true;

  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'OA_' + IntToStr(TypeId) + '_' + ElemName);
  end;
end;

{$IFNDEF PAXARM}
procedure TBaseParser.BeginShortStringType(TypeId: Integer);
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typeSHORTSTRING);
  Gen(OP_BEGIN_SHORTSTRING_TYPE, TypeId, 0, 0);
  if GetName(TypeId) = '' then
  begin
    SetName(TypeId, 'ShortStringType_' + IntToStr(TypeId));
    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);
  end;
end;

procedure TBaseParser.EndShortStringType(TypeId: Integer);
begin
  Gen(OP_END_SHORTSTRING_TYPE, TypeId, 0, 0);
end;
{$ENDIF}

procedure TBaseParser.BeginProceduralType(TypeId, SubId: Integer);
var
  DummySubId: Integer;
begin
  SetKind(TypeId, KindTYPE);
  SetType(TypeId, typePROC);

  SetKind(SubID, KindSUB);
  SetCallConvention(SubID, CallConv);

  SetPatternId(TypeId, SubId);

  levelStack.Push(SubId);
  NewVar('result');
  NewVar('');

  if InterfaceOnly then
  begin
//    DummySubId := TypeId + 1;
//    SetName(DummySubId, DUMMYPROC_PREFIX + IntToStr(DummySubId));
    DummySubId := SubId;

    Gen(OP_BEGIN_PROC_TYPE, TypeId, 0, 0);
    Gen(OP_BEGIN_SUB, DummySubId, 0, 0);

    if GetName(TypeId) = '' then
      SetName(TypeId, 'ProcType_' + IntToStr(TypeId));

    SetName(DummySubId, DUMMYPROC_PREFIX + GetName(TypeId));
    SetLevel(DummySubId, TypeId);
  end;

  AddTypeParameters(GetLevel(TypeId));
end;

procedure TBaseParser.EndProceduralType(TypeId: Integer);
begin
  if InterfaceOnly then
  begin
    Gen(OP_END_SUB, TypeId + 1, 0, 0);
    Gen(OP_END_PROC_TYPE, TypeId, 0, 0);
  end;

  levelStack.Pop;
end;


function TBaseParser.GetTryCount: Integer;
begin
  result := TKernel(kernel).TryCount;
end;

procedure TBaseParser.SetTryCount(Value: Integer);
begin
  TKernel(kernel).TryCount := Value;
end;

function TBaseParser.GetDebugMode: Boolean;
begin
  result := TKernel(kernel).DEBUG_MODE;
end;

function TBaseParser.GenBeginTry: Integer; // returns label
begin
  SetTryCount(TryCount + 1);
  try_stack.Push(TryCount);

  result := NewLabel;
  Gen(OP_TRY_ON, try_stack.Top, 0, CurrLevel);
end;

procedure TBaseParser.GenFinally;
var
  L: Integer;
begin
  L := NewLabel;
  SetLabelHere(L);
  Gen(OP_FINALLY, try_stack.Top, L, CurrLevel);

  if BreakStack.Count > 0 then
  begin
    LastCodeRec.BreakLabel := BreakStack.TopLabel();
    LastCodeRec.LoopLabel := BreakStack.Top.loopLabel;
  end;
  if ContinueStack.Count > 0 then
    LastCodeRec.ContinueLabel := ContinueStack.TopLabel();
end;

procedure TBaseParser.GenExcept;
var
  L: Integer;
begin
  L := NewLabel;
  SetLabelHere(L);
  Gen(OP_EXCEPT, try_stack.Top, L, CurrLevel);

  if BreakStack.Count > 0 then
  begin
    LastCodeRec.BreakLabel := BreakStack.TopLabel();
    LastCodeRec.LoopLabel := BreakStack.Top.loopLabel;
  end;
  if ContinueStack.Count > 0 then
    LastCodeRec.ContinueLabel := ContinueStack.TopLabel();
end;

procedure TBaseParser.GenExceptOn(type_id: Integer);
var
  L: Integer;
  BlockNumber: Integer;
begin
  L := NewLabel;
  SetLabelHere(L);
  BlockNumber := try_stack.Top;
  Gen(OP_EXCEPT_ON, BlockNumber, L, type_id);
end;

procedure TBaseParser.GenEndTry;
begin
  Gen(OP_TRY_OFF, try_stack.Top, 0, CurrLevel);
  try_stack.Pop;
end;

procedure TBaseParser.GenDestroyGlobalDynamicVariables(B1, B2: Integer);
var
  I: Integer;
  R: TCodeRec;
  Code: TCode;
begin
  Code := TKernel(kernel).Code;

  for I:=B2 downto B1 do
  begin
    R := Code[I];
    if R.Op = OP_DECLARE_LOCAL_VAR then
    if GetKind(R.Arg2) = KindVAR then
    begin
      if (R.Arg1 = 0) or (TKernel(kernel).SymbolTable[R.Arg1].Kind = kindNAMESPACE) then
      begin
        if not TKernel(kernel).SymbolTable[R.Arg2].Param then
          Gen(OP_DESTROY_LOCAL_VAR, R.Arg2, 0, 0);
      end
      else if GetSymbolRec(R.Arg2).TypedConst then
      begin
        Gen(OP_DESTROY_LOCAL_VAR, R.Arg2, 0, 0);
      end;
    end;
  end;
end;

function TBaseParser.IsForward(SubId: Integer): Boolean;
begin
  result := TKernel(kernel).SymbolTable[SubId].IsForward;
end;

procedure TBaseParser.SetForward(SubId: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[SubId].IsForward := value;
end;

function TBaseParser.ReplaceForwardDeclaration(var ID: Integer; IsExternalUnit: Boolean = false): Boolean;
var
  I, J, ParamId: Integer;
  Code: TCode;
  SymbolTable: TSymbolTable;
  ForwardID: Integer;
  L, TypeParams: TIntegerList;
  BestId: Integer;
label
  ok;
begin
  BestId := 0;
  Code := TKernel(kernel).Code;
  SymbolTable := TKernel(kernel).SymbolTable;

  ForwardId := SymbolTable.LookupForwardDeclaration(Id, Upcase, BestId);

  if ForwardId = Id then
  begin
    result := true;
    Exit;
  end;

  if ForwardId = 0 then
  begin
    if IsExternalUnit then
    begin
      result := true;
      Exit;
    end;

    if BestId > 0 then
    begin
      ForwardId := BestId;

      if SymbolTable[id].Count > 0 then
      begin
        CreateError(errDeclarationDiffersFromPreviousDeclaration,
        [SymbolTable[Id].FullName]);
      end;

      goto ok;
    end;

    if SymbolTable[Id].Count > 0 then
    begin
      result := false;
      Exit;
    end;

    L := LookupForwardDeclarations(Id);

    if L = nil then
    begin
      I := SymbolTable[Id].Level;
      if (I > 0) then if SymbolTable[I].Kind = kindTYPE then
      begin
        if SymbolTable.LookupAnotherDeclaration(Id, Upcase, BestId) = 0 then
        begin
          CreateError(errUndeclaredIdentifier, [SymbolTable[Id].FullName]);
        end
        else
        begin
          CreateError(errRedeclaredIdentifier, [SymbolTable[Id].FullName]);
        end;
      end;

      result := false;
      Exit;
    end
    else if L.Count = 1 then
    begin
      // ok
      ForwardId := L[0];
      FreeAndNil(L);
    end
    else
    begin
      FreeAndNil(L);
      result := false;
      Exit;
    end;

  end;

  ok:

  if GetSymbolRec(Id).IsSharedMethod <> GetSymbolRec(ForwardId).IsSharedMethod then
    CreateError(errDeclarationDiffersFromPreviousDeclaration,
     [GetName(ForwardId)]);

  result := true;

  // remove forward definition from Code

{
  I := 1;
  repeat
    if (Code[I].Op = OP_BEGIN_SUB) and (Code[I].Arg1 = ForwardId) then
    begin
      Code[I].Op := OP_NOP;
      break;
    end;
    Inc(I);
    if I = Code.Card then
      RaiseError(errInternalError, []);
  until false;
}
//

  I := BinSearch(BeginSubList.Keys, ForwardId);
  if I = -1 then
    RaiseError(errInternalError, []);
  I := BeginSubList.Values[I];
  if (Code[I].Op <> OP_BEGIN_SUB) or (Code[I].Arg1 <> ForwardId) then
    RaiseError(errInternalError, []);
  Code[I].Op := OP_DECL_SUB;
//

  Code[I-1].Op := OP_NOP; //OP_GO

  Inc(I); // 15 july 2007
  repeat
// new
    if Code[I].Op = OP_HIGH then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_LOW then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_SIZEOF then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_PRED then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_SUCC then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_ABS then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_ORD then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_CHR then
    begin
      // ok. 30 match 2009
    end
    else if Code[I].Op = OP_EVAL then
    begin
      // ok. 2 october 2007
    end
    else if Code[I].Op = OP_ASSIGN_CONST then
    begin
      // ok. 18 november 2007
    end
    else if (Code[I].Op = OP_PUSH) or (Code[I].Op = OP_CALL) then
    begin
      // ok. 3 december 2007
    end
    else if (Code[I].Op = OP_CHECK_OVERRIDE) then
    begin
      // ok. 23 July 2008
    end
    else if (Code[I].Op = OP_CREATE_DYNAMIC_ARRAY_TYPE) then
    begin
      // ok. 6 October 2008
    end
    else if Code[I].Op = OP_SEPARATOR then
    begin
      // ok. 3 August 2009
    end
    else if Code[I].Op = OP_ADD_MESSAGE then
    begin
      // ok. 9 June 2010
    end
    else if Code[I].Op = OP_FIELD then
    begin
      // ok. 18 november 2007
    end
    else
    if Code[I].Op = OP_ASSIGN_TYPE then
    begin
      if (Code[I].Arg1 = ForwardId) or
         (SymbolTable[Code[I].Arg1].Level = ForwardId) then
        begin
          // ok
        end
        else
          Code[I].Op := OP_NOP;
    end
    else
// new
      Code[I].Op := OP_NOP;
    Inc(I);
    if I = Code.Card then
      RaiseError(errInternalError, []);
  until (Code[I].Op = OP_FIN_SUB) and (Code[I].Arg1 = ForwardId);
  Code[I].Op := OP_NOP;
  Code[I+1].Op := OP_NOP; //OP_LABEL

  // update OP_ASSIGN_TYPE instructions for parameters and result
  {
  I := 1;
  repeat
    if (Code[I].Op = OP_BEGIN_SUB) and (Code[I].Arg1 = Id) then
      break;
    Inc(I);
    if I > Code.Card then // 15 july 2007
      RaiseError(errInternalError, []);
  until false;
  }
//
  I := BinSearch(BeginSubList.Keys, Id);
  if I = -1 then
    RaiseError(errInternalError, []);
  I := BeginSubList.Values[I];
  if I > Code.Card then
    RaiseError(errInternalError, []);
//

  if I < Code.Card then // 15 july 2007
  repeat
    Inc(I);
    if Code[I].Op = OP_ASSIGN_TYPE then
    begin
      if SymbolTable[Code[I].Arg2].FinalTypeId = typeOPENARRAY then
        continue;

      if SymbolTable[Code[I].Arg1].Level = Id then
      begin
        for J:=0 to SymbolTable[Id].Count - 1 do
        begin
          ParamId := SymbolTable.GetParamId(Id, J);
          if ParamId = Code[I].Arg1 then
          begin
            ParamId := SymbolTable.GetParamId(ForwardId, J);
            Code[I].Arg1 := ParamId;
            break;
          end;
        end;
        if Code[I].Arg1 = SymbolTable.GetResultId(Id) then
           Code[I].Arg1 := SymbolTable.GetResultId(ForwardId);
      end;
    end;
    if I >= Code.Card then
      break;
  until false;

  Code.ReplaceId(Id, ForwardId);

  TypeParams := SymbolTable.GetTypeParameters(Id);
  for I := 0 to TypeParams.Count - 1 do
    GetSymbolRec(TypeParams[I]).Level := ForwardId;
  FreeAndNil(TypeParams);

  if FIND_DECL_IMPLEMENTATION_SECTION then
  begin
    if LanguageId = PASCAL_LANGUAGE then
    if TKernel(kernel).FindDeclId = - 1 then
    if SymbolTable[ForwardId].Count > 0 then
    begin
      for I := 0 to SymbolTable[ForwardId].Count - 1 do
      begin
        ParamId := SymbolTable.GetParamId(ForwardId, I);
        J := SymbolTable.GetParamId(Id, I);
        SymbolTable[ParamId].Position := SymbolTable[J].Position;
      end;
    end;
  end
  else
  begin
    if TKernel(kernel).FindDeclId = ForwardId then
      TKernel(kernel).FindDeclId := Id;
  end;

  SetKind(Id, KindNONE);
  SetName(Id, '');

  SetKind(SymbolTable.GetResultId(Id), KindNONE);
  SetKind(SymbolTable.GetSelfId(Id),   KindNONE);

  SetKind(ForwardId + 3, KindNONE);               // label
  SetKind(ForwardId + 4, KindNONE);               // label

  for I:=ID to SymbolTable.Card do
    if (SymbolTable[I].Level = Id) and (SymbolTable[I].Kind <> KindTYPE) then
    begin
      SetKind(I, KindNONE);
//    SetName(I, ''); // 25 march 2011  / 6 April 2011
    end;

  SetForward(ForwardId, false);

  Id := ForwardId;
  LevelStack.Pop;
  LevelStack.Push(id);
end;

constructor TParserList.Create(i_kernel: Pointer);
begin
  inherited Create;
  L := TPtrList.Create;
  kernel := i_kernel;
end;

destructor TParserList.Destroy;
begin
  Clear;
  FreeAndNil(L);
  inherited;
end;

function TParserList.GetParser(I: Integer): TBaseParser;
begin
  result := TBaseParser(L[I]);
end;

function TParserList.Count: Integer;
begin
  result := L.Count;
end;

procedure TParserList.Clear;
begin
  L.Clear;
end;

procedure TParserList.AddParser(P: TBaseParser);
begin
  if FindParser(P.LanguageName) = nil then
    L.Add(P);
end;

function TParserList.FindParser(const LanguageName: String): TBaseParser;
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    if StrEql(Items[I].LanguageName, LanguageName) then
    begin
      result := Items[I];
      Exit;
    end;
  result := nil;
end;

function TParserList.FindParserByFileExtension(const FileExtension: String): TBaseParser;
var
  I: Integer;
begin
  if FindParser(TKernel(kernel).DefaultParser.LanguageName) = nil then
    AddParser(TKernel(kernel).DefaultParser);

  for I:=0 to Count - 1 do
    if StrEql(Items[I].GetFileExt, FileExtension) then
    begin
      result := Items[I];
      Exit;
    end;
  result := nil;
end;

{$IFDEF VARIANTS}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
procedure TBaseParser.AddModuleFromFile(FileName: String;
                                        UsedUnitId: Integer;
                                        IsImplementationSection: Boolean;
                                        ErrMessage: String = '';
                                        NoRaise: Boolean = false);
var
  CodeLines: String;
  UnitName, ALanguageName, Ext: String;
  P, I: Integer;
  Parser, UnitParser: TBaseParser;
  Prog: TBaseRunner;
  SourceAge, PCUAge: TDateTime;
  PCUFileName: String;
  M: TModule;
  RootKernel: TKernel;
  InputStream, OutputStream: TStream;
  OutputStream_Position, Temp_Position, PosStream: Integer;
  CompilePCU: Boolean;
  RunnerClass: TBaseRunnerClass;
begin
  RunnerClass := TKernel(kernel).GetRunnerClass;

  if TKernel(kernel).Modules.IndexOf(FileName) >= 0 then
    Exit;

  RootKernel := TKernel(Kernel).RootKernel;

  CodeLines := '';

  P := LastPosCh('.', FileName);
  if P > 0 then
  begin
    UnitName := SCopy(FileName, SLow(FileName), P - SLow(FileName));
    Ext := SCopy(FileName, P + 1, 10);
    Parser := TKernel(kernel).ParserList.FindParserByFileExtension(Ext);
    if Parser = nil then
      RaiseError(errUnregisteredLanguage, [FileName]);

    ALanguageName := Parser.LanguageName;
  end
  else
  begin
    ALanguageName := LanguageName;
    UnitName := FileName;
    Parser := Self;
  end;

  PCUFileName := UnitName + '.' + PCU_FILE_EXT;
  OutputStream := nil;
  OutputStream_Position := 0;
  InputStream := nil;

  I := RootKernel.Modules.IndexOf(UnitName);
  if I >= 0 then
  begin
    if IsImplementationSection then
    begin
      if RootKernel.Modules[I].State <> msCompiling then
        IsImplementationSection := false;
    end
    else
    begin
      if RootKernel.Modules[I].State = msCompiling then
        IsImplementationSection := true
      else if RootKernel.BuildedUnits.IndexOf(UpperCase(PCUFileName)) = -1 then
        if RootKernel.Modules[I].State <> msNone then
          if RootKernel.BuildAll then
          RaiseError(errCircularUnitReference, [UnitName]);
    end;
  end;

  if Assigned(RootKernel.OnLoadPCU) then
  begin
    RootKernel.OnLoadPCU(RootKernel.Owner, UnitName, InputStream);
  end;

  try // jason

//  if RootKernel.BuildAll then

  if RootKernel.BuildAll or
     ((InputStream = nil) and
       Assigned(RootKernel.OnSavePCU) and
       (RootKernel.BuildedUnits.IndexOf(UpperCase(PCUFileName)) = - 1)) then
  begin
    if Assigned(RootKernel.OnSavePCU) and (InputStream = nil) then
      if RootKernel.BuildedUnits.IndexOf(UpperCase(PCUFileName)) = - 1 then
      if not IsImplementationSection then
      begin
        RootKernel.OnSavePCU(RootKernel.Owner, UnitName, OutputStream);
        if OutputStream <> nil then
          OutputStream_Position := OutputStream.Position;
      end;

    if IsImplementationSection then
    begin
      RemoveInstruction(OP_EVAL, -1, -1, UsedUnitId);

      I := TKernel(Kernel).Modules.IndexOf(UnitName);
      if I = -1 then
      begin
        if Assigned(RootKernel.OnUsedUnit) then
        begin
          RootKernel.CurrLanguage := ALanguageName;
          if (RootKernel.OnUsedUnit(RootKernel, UnitName, CodeLines)) and
            (CodeLines <> '') then
            begin
              ALanguageName := RootKernel.CurrLanguage;
              M := TKernel(kernel).AddModule(UnitName, ALanguageName);
              M.SkipParsing := true;
              TKernel(kernel).AddCode(UnitName, CodeLines);
            end
            else
            begin
              M := TKernel(kernel).AddModule(UnitName, ALanguageName);
              M.SkipParsing := true;
            end;
        end
        else
        begin
          I := RootKernel.Modules.IndexOf(UnitName);
          M := TKernel(kernel).AddModule(UnitName, ALanguageName);
          M.SkipParsing := true;
          if I = - 1 then
            TKernel(kernel).AddCodeFromFile(UnitName, FileName)
          else
            TKernel(kernel).AddCode(UnitName,
              RootKernel.Modules[I].Lines.Text);
        end;

        UnitParser := CreateParser(FileName);
        try
          UnitParser.Init(kernel, M);
          UnitParser.BRP := true;
          UnitParser.Call_SCANNER;
          UnitParser.Parse_Unit(true);
        finally
          FreeAndNil(UnitParser);
        end;
      end;
      Exit;
    end
    else
    begin
      try  // jason
        if RootKernel.BuildedUnits.IndexOf(UpperCase(PCUFileName)) = - 1 then
          if not CompileUnit(kernel, UnitName, FileName, PCUFileName, Parser, true, OutputStream) then
             Exit;
      finally // jason
        if (OutputStream <> nil) and
            Assigned(RootKernel.OnSavePCU) and
            Assigned(RootKernel.OnSavePCUFinished) then // jason
        begin // jason
          RootKernel.OnSavePCUFinished(RootKernel.Owner, UnitName, OutputStream); // jason
        end; // jason
      end; // jason
    end;
  end;

  if not RootKernel.BuildAll then
    if Assigned(RootKernel.OnUsedUnit) then
      if TKernel(Kernel).Modules.IndexOf(UnitName) = -1 then
      begin
        RootKernel.CurrLanguage := LanguageName;
        if RootKernel.OnUsedUnit(RootKernel.Owner, UnitName, CodeLines) then
        begin
          if CodeLines <> '' then
          begin
            TKernel(kernel).AddModule(UnitName, TKernel(kernel).CurrLanguage);
            TKernel(kernel).AddCode(UnitName, CodeLines);
          end;
          Exit;
        end;
      end;

  if (InputStream = nil) or (OutputStream = nil) then
  begin
    if not FileExists(FileName) then
    begin
      if FileExists('System.' + FileName) then
      begin
        FileName := 'System.' + FileName;
        UnitName := 'System.' + UnitName;
      end;
    end;

    if not FileExists(FileName) then
      SourceAge := 0
    else
      SourceAge := FileDateToDateTime(FileAge(FileName));

    if not FileExists(PCUFileName) then
      PCUAge := 0
    else
      PCUAge := FileDateToDateTime(FileAge(PCUFileName));

    CompilePCU := ((PCUAge > SourceAge) and FileExists(PCUFileName)) or
                  ((SourceAge = 0) and (PCUAge = 0));
  end
  else
    CompilePCU := true;

  if CompilePCU then
  begin
    FileName := PCUFileName;

    if TKernel(kernel).Modules.IndexOf(UnitName) = -1 then
    begin
      Prog := RunnerClass.Create;
      try
        PosStream := 0;
        if InputStream <> nil then
        begin
          PosStream := InputStream.Position;
          Prog.LoadFromStream(InputStream);
        end
        else if OutputStream <> nil then
        begin
          Temp_Position := OutputStream.Position;
          OutputStream.Position := OutputStream_Position;
          Prog.LoadFromStream(OutputStream);
          OutputStream.Position := Temp_Position;
        end
        else
        begin

          if not FileExists(FileName) then
          begin
            if NoRaise then
              Exit;

{$IFDEF DRTTI}
            if Assigned(TKernel(kernel).prog) then
            if TKernel(kernel).prog.HasAvailUnit(UnitName) then
              TKernel(kernel).RegisterImportUnit(0, UnitName);
            Exit;
{$ENDIF}
            if ErrMessage = '' then
              RaiseError(errFileNotFound, [FileName])
            else
              RaiseError(errMessage, []);
          end;

          Prog.LoadFromFile(FileName);
        end;

        if not RootKernel.BuildWithRuntimePackages then
        begin
          if InputStream <> nil then
          begin
            InputStream.Position := PosStream;
            RootKernel.PCUStreamList.AddFromStream(InputStream, FileName);
          end
          else
            RootKernel.PCUStreamList.AddFromFile(FileName);
        end;

        CodeLines := PCUToString(Prog, UnitName, FileName);
        case Prog.PCULang of
          PASCAL_LANGUAGE: TKernel(kernel).AddModule(UnitName, 'Pascal', true);
          BASIC_LANGUAGE: TKernel(kernel).AddModule(UnitName, 'Basic', true);
        else
          TKernel(kernel).AddModule(UnitName, 'Pascal', true);
        end;
        TKernel(kernel).AddCode(UnitName, CodeLines);
      finally
        FreeAndNil(Prog);
      end;
    end;

    Exit;
  end;

  if not
   UnitLookup then
    Exit;

  if SkipWhenNotFound then
  begin
    SkipWhenNotFound := false;
    Exit;
  end;

  if TKernel(kernel).Modules.IndexOf(UnitName) = -1 then
  begin
    if not FileExists(FileName) then
    begin
{$IFDEF DRTTI}
      TKernel(kernel).RegisterImportUnit(0, UnitName);
      Exit;
{$ENDIF}
      if ErrMessage = '' then
        RaiseError(errFileNotFound, [FileName])
      else
        raise Exception.Create(errMessage);
    end;

    TKernel(kernel).AddModule(UnitName, ALanguageName);
    TKernel(kernel).AddCodeFromFile(UnitName, FileName);
  end;

  finally // jason
    if Assigned(RootKernel.OnLoadPCU) and
       Assigned(RootKernel.OnLoadPCUFinished) and
      (InputStream <> nil) then // jason
      RootKernel.OnLoadPCUFinished(RootKernel.Owner, UnitName, InputStream); // jason
  end; // jason
end;

function TBaseParser.GetValue(id: Integer): Variant;
begin
  result := TKernel(kernel).SymbolTable[id].Value;
end;

procedure TBaseParser.BeginInitialization;
begin
  Gen(OP_STMT, 0, 0, 0);
  Gen(OP_BEGIN_INITIALIZATION, CurrModule.ModuleNumber, 0, 0);
end;

procedure TBaseParser.EndInitialization;
begin
  Gen(OP_STMT, 0, 0, 0);
  Gen(OP_END_INITIALIZATION, CurrModule.ModuleNumber, 0, 0);
end;

procedure TBaseParser.BeginFinalization;
begin
  Gen(OP_STMT, 0, 0, 0);
  Gen(OP_BEGIN_FINALIZATION, CurrModule.ModuleNumber, 0, 0);
end;

procedure TBaseParser.EndFinalization;
begin
  Gen(OP_STMT, 0, 0, 0);
  Gen(OP_END_FINALIZATION, CurrModule.ModuleNumber, 0, 0);
end;

function TBaseParser.GetRaiseMode: Integer;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;
  result := 0;
  for I:=Code.Card downto 1 do
    if Code[I].Op = OP_EXCEPT then
    begin
      result := 1;
      Exit;
    end
    else if Code[I].Op = OP_FINALLY then
    begin
      result := 1;
      Exit;
    end
    else if Code[I].Op = OP_TRY_ON then
      break
    else if Code[I].Op = OP_TRY_OFF then
      break;
end;

function TBaseParser.Lookup(const S: String; Level: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.LookUp(S, Level, UpCase, MaxInt, false);
end;

function TBaseParser.Lookups(const S: String; Levels: TIntegerStack): Integer;
begin
  result := TKernel(kernel).SymbolTable.LookUps(S, Levels, UpCase, MaxInt, false);
  if result = 0 then
    if ImportOnly then
       result := Lookup(S, H_PascalNamespace);
end;

procedure TBaseParser.SetVisibility(Id: Integer; vis: TClassVisibility);
begin
  TKernel(kernel).SymbolTable[id].Vis := vis;
end;

procedure TBaseParser.SetReadId(PropId, ReadId: Integer);
begin
  TKernel(kernel).SymbolTable[PropId].ReadId := ReadId;
end;

procedure TBaseParser.SetWriteId(PropId, WriteId: Integer);
begin
  TKernel(kernel).SymbolTable[PropId].WriteId := WriteId;
end;

procedure TBaseParser.SetDefault(Id: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[Id].IsDefault := value;
end;

procedure TBaseParser.RemoveInstruction(Op, Arg1, Arg2, Res: Integer);
begin
  TKernel(kernel).Code.RemoveInstruction(Op, Arg1, Arg2, Res);
end;

function TBaseParser.RemoveLastEvalInstruction(const S: String; Upcase: Boolean = true): Integer;
begin
  result := TKernel(kernel).Code.RemoveLastEvalInstruction(S);
end;

function TBaseParser.RemoveLastEvalInstructionAndName(const S: String; Upcase: Boolean = true): Boolean;
var
  Id: Integer;
begin
  dmp;
  Id := TKernel(kernel).Code.RemoveLastEvalInstruction(S);
  result := Id > 0;
  if result then
    RemoveLastIdent(Id);
end;

procedure TBaseParser.RemoveLastIdent(Id: Integer);
var
  R: TCodeRec;
begin
  if Id = TKernel(kernel).SymbolTable.Card then
  begin
    R := LastEvalRec(Id);
    if R <> nil then
    begin
      R.Op := OP_NOP;
      R.GenOp := OP_NOP;
      R.Res := 0;
    end;
    TKernel(kernel).SymbolTable.RemoveLastRecord;
  end;
end;

function TBaseParser.LastCodeRec(var I: Integer): TCodeRec;
var
  Code: TCode;
begin
  Code := TKernel(kernel).Code;
  I := Code.Card;
  while Code[I].Op = OP_SEPARATOR do
    Dec(I);
  result := Code[I];
end;

function TBaseParser.LastCodeRec: TCodeRec;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;
  I := Code.Card;
  while Code[I].Op = OP_SEPARATOR do
    Dec(I);
  result := Code[I];
end;

function TBaseParser.LastCodeRec2: TCodeRec;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;
  I := Code.Card;
  while Code[I].Op = OP_SEPARATOR do
    Dec(I);
  Dec(I);
  while Code[I].Op = OP_SEPARATOR do
    Dec(I);
  result := Code[I];
end;

function TBaseParser.LastEvalRec(Id: Integer; var I: Integer): TCodeRec;
var
  Code: TCode;
begin
  Code := TKernel(kernel).Code;

  result := LastCodeRec(I);
  if result.Op = OP_EVAL then
    if result.Res = Id then
      Exit;
  I := Code.Card;
  result := nil;
  repeat
    Dec(I);
    if I <= 1 then
      Exit;
    if Code[I].Op = OP_BEGIN_MODULE then
      Exit;
    if Code[I].Op = OP_EVAL then
      if Code[I].Res = Id then
      begin
        result := Code[I];
        Exit;
      end;
  until false;
end;

function TBaseParser.LastEvalRec(Id: Integer): TCodeRec;
var
  Code: TCode;
  I: Integer;
begin
  Code := TKernel(kernel).Code;

  result := LastCodeRec(I);
  if result.Op = OP_EVAL then
    if result.Res = Id then
      Exit;
  I := Code.Card;
  result := nil;
  repeat
    Dec(I);
    if I <= 1 then
      Exit;
    if Code[I].Op = OP_BEGIN_MODULE then
      Exit;
    if Code[I].Op = OP_EVAL then
      if Code[I].Res = Id then
      begin
        result := Code[I];
        Exit;
      end;
  until false;
end;

function TBaseParser.LookupForwardDeclarations(Id: Integer): TIntegerList;
begin
  result := TKernel(kernel).SymbolTable.LookupForwardDeclarations(Id, Upcase);
end;

procedure TBaseParser.Push_SCANNER;
begin
  scanner.Push;
end;

procedure TBaseParser.Pop_SCANNER;
begin
  scanner.Pop;
end;

function TBaseParser.FindConstructorId(ClassId: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.FindConstructorId(ClassId);
end;

function TBaseParser.FindDestructorId(ClassId: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.FindDestructorId(ClassId);
end;

function TBaseParser.IsStringConst(Id: Integer): Boolean;
var
  Code: TCode;
  I: Integer;
begin
  result := (GetKind(Id) = KindCONST) and
    (
{$IFNDEF PAXARM}
    TKernel(kernel).SymbolTable[Id].HasPAnsiCharType or
{$ENDIF}
    TKernel(kernel).SymbolTable[Id].HasPWideCharType);

  if not result then
  begin
    Code := TKernel(kernel).Code;
    for I:=1 to Code.Card do
      if Code[I].Op = OP_ASSIGN_CONST then
      if Code[I].Arg1 = Id then
      begin
        result := IsStringConst(Code[I].Arg2);
        break;
      end;
  end;
end;

procedure TBaseParser.SetHost(Id: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[Id].Host := value;
end;

function TBaseParser.GetHost(Id: Integer): Boolean;
begin
  result := TKernel(kernel).SymbolTable[Id].Host;
end;

procedure TBaseParser.SetAlignment(TypeId, Value: Integer);
begin
  TKernel(kernel).SymbolTable[TypeId].DefaultAlignment := Value;
end;

procedure TBaseParser.SetCompletionTarget(const S: String);
begin
  TKernel(kernel).CompletionTarget := S;
end;

function TBaseParser.GetStCard: Integer;
begin
  result := TKernel(kernel).SymbolTable.Card;
end;

function TBaseParser.GetCodeCard: Integer;
begin
  result := TKernel(kernel).Code.Card;
end;

procedure TBaseParser.SetOpenArray(ID: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[ID].IsOpenArray := value;
end;

procedure TBaseParser.DiscardLastSTRecord;
begin
  if CurrToken.Id = StCard then
  with TKernel(kernel).SymbolTable[TKernel(kernel).SymbolTable.Card] do
  begin
    Kind := kindNONE;
    Name := '';
  end;
end;

function TBaseParser.GetSymbolRec(Id: Integer): TSymbolRec;
begin
  result := TKernel(kernel).SymbolTable[Id];
end;

procedure TBaseParser.InitScanner(const S: String);
begin
  scanner.Init(kernel, S, -1);
end;

function TBaseParser.GetTrueId: Integer;
begin
  result := TKernel(kernel).SymbolTable.TrueId;
end;

function TBaseParser.GetFalseId: Integer;
begin
  result := TKernel(kernel).SymbolTable.FalseId;
end;

function TBaseParser.GetCodeRec(I: Integer): TCodeRec;
begin
  result := TKernel(kernel).Code[I];
end;

function TBaseParser.CurrNamespaceId: Integer;
begin
  result := CurrLevel;
  if result = 0 then
    Exit;
  while GetSymbolRec(result).Kind <> kindNAMESPACE do
  begin
    result := GetSymbolRec(result).Level;
    if result = 0 then
      break;
  end;
end;

function TBaseParser.CreateParser(const FileName: String): TBaseParser;
var
  Parser: TBaseParser;
  Ext: String;
  P: Integer;
  C: TBaseParserClass;
begin
  Parser := nil;
  P := LastPosCh('.', FileName);
  if P > 0 then
  begin
    Ext := Copy(FileName, P + 1, 10);
    Parser := TKernel(kernel).ParserList.FindParserByFileExtension(Ext);
  end;
  if Parser = nil then
    RaiseError(errUnregisteredLanguage, [FileName]);
  C := TBaseParserClass(Parser.ClassType);
  result := C.Create;
end;

procedure TBaseParser.Parse_Unit(IsExternalUnit: Boolean = false);
begin
end;

function TBaseParser.BuildingAll: Boolean;
begin
  result := TKernel(kernel).RootKernel.BuildAll;
end;

procedure TBaseParser.SetExternal(Id: Integer; value: Boolean);
begin
  GetSymbolRec(Id).IsExternal := value;
end;

procedure TBaseParser.SetTempName(Id: Integer);
begin
  SetName(Id, '_temp_' + IntToStr(id));
end;

function TBaseParser.ScanRegExpLiteral: String;
begin
  result := scanner.ScanRegExpLiteral;
end;

function TBaseParser.LA(I: Integer): Char;
begin
  result := scanner.LA(I);
end;

function TBaseParser.Parse_QualId: Integer;
var
  id: Integer;
begin
  result := Parse_Ident;

  while IsCurrText('.') do
  begin
    FIELD_OWNER_ID := result;
    id := FIELD_OWNER_ID;

    Match('.');
    if CurrToken.TokenClass = tcBooleanConst then
      result := Parse_BooleanLiteral
    else if CurrToken.TokenClass = tcPCharConst then
      result := Parse_PCharLiteral
    else if CurrToken.TokenClass = tcIntegerConst then
      result := Parse_IntegerLiteral
    else if CurrToken.TokenClass = tcDoubleConst then
      result := Parse_DoubleLiteral
    else
      result := Parse_Ident;
    Gen(OP_FIELD, id, result, result);
  end;
end;

function TBaseParser.Parse_UnitName(var S: String): Integer;
var
  Id, FieldId, J: Integer;
  Q: TStringList;
  S1: String;
begin
  if DECLARE_SWITCH then
  begin
    result := Parse_Ident;
    S := GetName(result);
    if not StrEql('System', S) then
      BeginNamespace(result);
    while IsCurrText('.') do
    begin
      Match('.');
      result := Parse_Ident;
      BeginNamespace(result);
      S := S + '.' + GetName(result);
    end;

    Exit;
  end;

  S := CurrToken.Text;
  ReadToken;
  while IsCurrText('.') do
  begin
    ReadToken;
    S := S + '.' + CurrToken.Text;
    ReadToken;
  end;

  TKernel(kernel).EvalList.Add(S);

  try

    result := Lookup(S, 0);
    if result > 0 then
      Exit;

    if LastCodeRec.Op = OP_EVAL then
      LastCodeRec.Op := OP_NOP;

    S1 := S;
    if not ImportOnly then
      S := ExpandUnitName(S1);
    if S <> S1 then
      TKernel(kernel).EvalList.Add(S);

    if Assigned(TKernel(kernel).OnUnitAlias) then
      TKernel(kernel).OnUnitAlias(TKernel(kernel).Owner, S);

    Q := ExtractNames(S);

    try
      if StrEql(Q[0], 'System') then
      begin
        Q.Delete(0);
        if Q.Count = 0 then
          RaiseError(errUndeclaredIdentifier, ['System']);
      end;

      Id := 0;
      for J := 0 to Q.Count - 1 do
      begin
        Id := Lookup(Q[J], Id);
        if Id = 0 then
          break;
      end;
      if Id > 0 then
      begin
        result := Id;
        Exit;
      end;

      Id := NewVar(Q[0]);
      SetKind(Id, KindNONE);
      Gen(OP_EVAL, 0, 0, Id);

      for J := 1 to Q.Count - 1 do
      begin
        FieldId := NewField(Q[J], Id);
        Gen(OP_FIELD, Id, FieldId, FieldId);
        Id := FieldId;
      end;

    finally
      FreeAndNil(Q);
    end;

    result := Id;

  finally
    if UsedUnitList.IndexOf(Uppercase(S)) >= 0 then
      CreateError(errRedeclaredIdentifier, [S]);
    UsedUnitList.Add(Uppercase(S));
  end;
end;

function TBaseParser.ExpandUnitName(const S: String): String;
var
  I: Integer;
  Q: String;
begin
  result := S;

  if AvailUnitList1.Count = 0 then
    Exit;

  I := AvailUnitList.IndexOf(S);
  if I >= 0 then
    Exit;

  for I := 0 to AvailUnitList1.Count - 1 do
  begin
    Q := AvailUnitList1[I] + '.' + S;
    if AvailUnitList.IndexOf(Q) >= 0 then
    begin
      result := Q;
      Exit;
    end;
  end;
end;

function TBaseParser.ExpandTypeName(const S: String): String;
var
  I: Integer;
  Q: String;
begin
  result := S;

  if AvailTypeList.Count = 0 then
    Exit;

  I := AvailTypeList.IndexOf(S);
  if I >= 0 then
    Exit;

  Q := 'System.' + S;
  I := AvailTypeList.IndexOf(Q);
  if I >= 0 then
  begin
    result := Q;
    Exit;
  end;

  for I := 0 to UsedUnitList.Count - 1 do
  begin
    Q := UsedUnitList[I] + '.' + S;
    if AvailTypeList.IndexOf(Q) >= 0 then
    begin
      result := Q;
      Exit;
    end;
    Q := 'System.' + Q;
    if AvailTypeList.IndexOf(Q) >= 0 then
    begin
      result := Q;
      Exit;
    end;
  end;
end;

procedure TBaseParser.CheckRedeclaredSub(SubId: Integer);

function GetSignature(Id: Integer): String;
var
  I, J, T, ParamId: Integer;
  S: String;
  SymbolTable: TSymbolTable;
  Code: TCode;
begin
  SymbolTable := TKernel(kernel).SymbolTable;
  Code := TKernel(kernel).Code;

  result := '';
  for I:=0 to GetCount(Id) - 1 do
  begin
    ParamId := SymbolTable.GetParamId(Id, I);
    S := '';

    T := GetSymbolRec(ParamId).TypeID;
    if GetSymbolRec(ParamId).TypeID > 0 then
    begin
      S := GetSymbolRec(T).Name;
    end
    else
    for J := 1 to Code.Card do
      if Code[J].Op = OP_ASSIGN_TYPE then
        if Code[J].Arg1 = ParamId then
        begin
          S := GetSymbolRec(Code[J].Arg2).Name;
          break;
        end;

    result := result + ' ' + S;
  end;

end;

var
  I, Level, Kind: Integer;
  S: String;
  L: TIntegerList;
  ok: Boolean;
  Signature: String;
begin
  if ImportOnly then
    Exit;

  S := GetSymbolRec(SubId).Name;
  Level := GetSymbolRec(SubId).Level;
  Kind := GetSymbolRec(SubId).Kind;
  L := TKernel(kernel).SymbolTable.LookupAll(S, Level, Upcase);

  try
    if L.Count <= 1 then
      Exit;

    Signature := GetSignature(SubId);

    for I := 0 to L.Count - 1 do
    begin
      if L[I] = SubId then
        continue;

      S := GetSignature(L[I]);

      if UpCase then
        ok := StrEql(S, Signature)
      else
        ok := S = Signature;

      if ok then
      begin
        if Kind in [kindCONSTRUCTOR, KindDESTRUCTOR] then
           if GetKind(L[I]) = Kind then
              if GetSymbolRec(L[I]).IsSharedMethod <> GetSymbolRec(SubId).IsSharedMethod then
                 continue;

        CreateError(errRedeclaredIdentifier, [GetSymbolRec(SubId).Name]);
        break;
      end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TBaseParser.CountAtLevel(const S: String; Level: Integer): Integer;
var
  L: TIntegerList;
begin
  L := TKernel(kernel).SymbolTable.LookupAll(S, Level, Upcase);
  try
    result := L.Count;
  finally
    FreeAndNil(L);
  end;
end;

function TBaseParser.CountAtLevel(const S: String; Level, Kind: Integer; IsSharedMethod: Boolean): Integer;
var
  L: TIntegerList;
  I: Integer;
begin
  L := TKernel(kernel).SymbolTable.LookupAll(S, Level, Upcase);
  try
    if Kind in [KindCONSTRUCTOR, KindDESTRUCTOR] then
    begin
      result := 0;
      for I := 0 to L.Count - 1 do
        if GetSymbolRec(L[I]).IsSharedMethod = IsSharedMethod then
           Inc(result);
    end
    else
      result := L.Count;
  finally
    FreeAndNil(L);
  end;
end;

procedure TBaseParser.SetDeprecated(SubId: Integer; value: Boolean);
begin
  TKernel(kernel).SymbolTable[SubId].IsDeprecated := value;
end;

function TBaseParser.HasBeenDeclared(Id: Integer): Boolean;
var
  I: Integer;
  R: TCodeRec;
begin
  result := false;
  I := TKernel(kernel).Code.N;
  repeat
     Dec(I);
     if I = 0 then
       Exit;
     R := TKernel(kernel).Code[I];
     if R.Op = OP_BEGIN_MODULE then
       Exit;
     if R.Op = OP_DECLARE_LOCAL_VAR then
       if R.Arg2 = Id then
       begin
         result := true;
         Exit;
       end;
  until false;
end;

function TBaseParser.HasModule(const ModuleName: String): Boolean;
begin
  result := TKernel(kernel).Modules.IndexOf(ModuleName) >= 0;
end;

procedure TBaseParser.GenHtml;
var
  Id: Integer;
  S: String;
begin
  S := CurrToken.Text;
  S := RemoveLeftChars(WhiteSpaces, S);
  Id := NewConst(typeSTRING, S);
  Gen(OP_PRINT_EX, Id, 0, 0);
end;

procedure TBaseParser.GenCondRaise;
var
  cond_id: Integer;
begin
  Gen(OP_COND_RAISE, 0, 0, 0);
  cond_id := LastCodeRec.Res;

  if BreakStack.Count > 0 then
    LastCodeRec.BreakLabel := BreakStack.TopLabel;
  if ContinueStack.Count > 0 then
    LastCodeRec.ContinueLabel := ContinueStack.TopLabel;

  Gen(OP_GO_TRUE, SkipLabelStack.Top, cond_id, 0);
end;

function TBaseParser.GetUsedUnitList: TStringList;
begin
  result := TKernel(kernel).UsedUnitList;
end;

function TBaseParser.GetInterfaceOnly: Boolean;
begin
  result := TKernel(kernel).InterfaceOnly;
end;

function TBaseParser.GetImportOnly: Boolean;
begin
  result := TKernel(kernel).ImportOnly;
end;

function TBaseParser.ConvString(const S: String): String;
begin
  result := S;
end;

procedure TBaseParser.BeginInitConst(Id: Integer);
begin
  if GetKind(Id) = KindCONST then
    SetTypedConst(ID);
  Gen(OP_BEGIN_INIT_CONST, ID, 0, 0);
  if GetSymbolRec(ID).Name = '' then
    GetSymbolRec(ID).Name := '@';
end;

procedure TBaseParser.EndInitConst(Id: Integer);
var
  I: Integer;
begin
  Gen(OP_END_INIT_CONST, ID, 0, 0);
  if GetSymbolRec(ID).Name <> '' then
  begin
    for I := Id + 1 to TKernel(kernel).SymbolTable.Card do
      GetSymbolRec(I).NoRelocate := true;
  end;
end;

function TBaseParser.FindPrevEvaluation(const S: String): Integer;
var
  L, I, Op, Id: Integer;
  SymbolTable: TSymbolTable;
  Code: TCode;
  R: TCodeRec;
  b: Boolean;
begin
  result := 0;
  L := CurrLevel;
  SymbolTable := TKernel(kernel).SymbolTable;
  Code := TKernel(kernel).Code;
  I := Code.Card;
  repeat
    Dec(I);
    if I < 1 then
      break;
    R := Code[I];
    Op := R.Op;

    if Op = OP_BEGIN_WITH then
      break;
    if Op = OP_BEGIN_MODULE then
      break;
    if Op = OP_BEGIN_SUB then
      break;
    if Op <> OP_EVAL then
      continue;

    Id := R.Res;
    if SymbolTable[Id].Level <> L then
      break;

    if Upcase then
      b := StrEql(SymbolTable[Id].Name, S)
    else
      b := SymbolTable[I].Name = S;

    if b then
    begin
      result := R.Res;
      break;
    end;
  until false;

  if result = 0 then
    Exit;

  repeat
     Inc(I);
     if I > Code.Card then
     begin
       Exit;
     end;
     if Code[I].Op = OP_STMT then
     begin
       result := 0;
       Exit;
     end;
     if Code[I].Op = OP_CALL then
     if Code[I].Arg1 = result then
     begin
       Exit;
     end;
  until false;
end;

procedure TBaseParser.GenNOPS(K: Integer);
var
  I: Integer;
begin
  for I := 0 to K - 1 do
    Gen(OP_NOP, 0, 0, 0);
end;

procedure TBaseParser.GenAssignOuterInstance(Id, ClassId: Integer);
var
  RefId, OuterInstanceId: Integer;
begin
  OuterInstanceId := NewTempVar;
  Gen(OP_EVAL_OUTER, ClassId, 0, OuterInstanceId);

  RefId := NewField(StrOuterThis, Id);
  Gen(OP_FIELD, Id, RefId, RefId);
  Gen(OP_ASSIGN, RefId, OuterInstanceId, RefId);
end;

procedure TBaseParser.BeginCollectSig(SubId: Integer);
begin
  CollectSig := true;
  Sig := '';
end;

procedure TBaseParser.EndCollectSig(SubId: Integer);
begin
  CollectSig := false;
  Sig := RemoveChars(WhiteSpaces, Sig);
  GetSymbolRec(SubId).Sig := Sig;
end;

function TBaseParser.LookupInUsingList(const S: String): Integer;
var
  I, Id, temp: Integer;
begin
  result := 0;
  for I := UsingList.Count - 1 downto 0 do
  begin
    Id := UsingList[I];
    if Id = 0 then
      break;
    if GetSymbolRec(Id).Kind = KindNAMESPACE then
    begin
      temp := Lookup(S, Id);
      if temp > 0 then
      begin
        result := temp;
        Exit;
      end;
    end
    else
      continue;
  end;
end;

function TBaseParser.LookupInWithStack(const S: String;
                                       var PatternId: Integer;
                                       var StackIsOK: Boolean): Integer;
var
  I, Id, TypeId, temp: Integer;
begin
  result := 0;
  StackIsOK := true;
  for I := WithStack.Count - 1 downto 0 do
  begin
    Id := WithStack[I];

    TypeId := GetSymbolRec(Id).TypeId;
    if TypeId = 0 then
    begin
      StackIsOK := false;
      break;
    end;
    if GetSymbolRec(TypeId).Kind = KindTYPE then
    begin
      temp := TKernel(kernel).SymbolTable.
          Lookup(S, TypeId, UpCase, MaxInt, false);
      if temp > 0 then
      begin
        result := Id;
        PatternId := temp;
        Exit;
      end;
    end
    else
    begin
      StackIsOK := false;
      break;
    end;
  end;
end;

function TBaseParser.GetOpenArrayHighId(Id: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.GetOpenArrayHighId(Id);
end;

function TBaseParser.Parse_TypeParams: String;

  function Parse_Internal(KK: Integer): String;
  var
    OldPosition: Integer;
    S, TypeName: String;
    R: TTypeExpRec;
    I, J, Id, FieldId: Integer;
    ParamList: TStringList;
    TR: TTypeRestrictionRec;
    LocalTypeParams: TTypeParams;
    Q: TStringList;
  label
    Next;
  begin
    LocalTypeParams := TTypeParams.Create;
    try
      TypeName := CurrToken.Text;

      OldPosition := CurrToken.Position;

      result := '<';

      ReadToken;
      ReadToken; // "<"
      repeat
         S := CurrToken.Text;

         while IsNextText('.') do
         begin
           ReadToken;
           ReadToken;
           S := S + '.' + CurrToken.Text;
         end;

         if IsNextText('<') then
           S := S + Parse_Internal(KK + 1);

         S := ExpandTypeName(S);

         LocalTypeParams.Add(S);

         if AvailTypeList.Count = 0 then
           result := result + CurrToken.Text
         else
           result := result + S;

         ReadToken;

         Next:
         if IsCurrText(',') then
         begin
           ReadToken;
           result := result + ',';
         end
         else if IsCurrText(';') then
         begin
           ReadToken;
           result := result + ',';
         end
         else
         begin
           if IsCurrText('>') then
             break;

           if DECLARE_SWITCH then
           begin
             Parse_TypeRestriction(LocalTypeParams);
             goto Next;
           end;

         end;
      until false;
      result := result + '>';
      CurrToken.TokenClass := tcIdentifier;
      CurrToken.Length := CurrToken.Position - OldPosition + 1;
      CurrToken.Position := OldPosition;

      result := RemoveChars(WhiteSpaces, result);

      if (not DECLARE_SWITCH) and (not CurrModule.IsExtra) then
      begin
        if TKernel(kernel).TypeDefList.IndexOf(TypeName, LocalTypeParams, Upcase) >= 0 then
          Exit;

        if TKernel(kernel).TypeDefList.TypeExpList.IndexOf(TypeName, LocalTypeParams, Upcase) >= 0 then
          Exit;

        R := TKernel(kernel).TypeDefList.TypeExpList.Add;
        R.Name := TypeName;

        for I := 0 to LocalTypeParams.Count - 1 do
        begin
          ParamList := TKernel(kernel).TypeDefList.TypeExpList.Top.ParamList;

          S := LocalTypeParams[I];
          if PosCh('.', S) > 0 then
          begin
            Q := ExtractNames(S);

            if AvailTypeList.Count = 0 then
              LocalTypeParams[I] := Q[Q.Count - 1];

            Id := NewVar(Q[0]);
            SetKind(Id, KindNONE);
            Gen(OP_EVAL, 0, 0, Id);

            for J := 1 to Q.Count - 1 do
            begin
              FieldId := NewField(Q[J], Id);
              Gen(OP_FIELD, Id, FieldId, FieldId);
              Id := FieldId;
            end;

            if AvailTypeList.Count = 0 then
              S := LocalTypeParams[I];

            TR := TTypeRestrictionRec.Create;
            TR.Id := Id;
            TR.N := TKernel(kernel).Code.Card;

            ParamList.AddObject(S, TR);

            Q.Free;
          end
          else
          begin
            Id := AltTypeId(S);

            if Id = 0 then
            begin
              Id := NewVar(S);
              SetKind(Id, KindNONE);
              Gen(OP_EVAL, 0, 0, Id);
            end;

            TR := TTypeRestrictionRec.Create;
            TR.Id := Id;
            TR.N := TKernel(kernel).Code.Card;

            ParamList.AddObject(S, TR);
          end;
        end;
      end;
    finally
      if KK = 0 then
      begin
        LocalTypeParams.AssTo(TypeParams);
        TypeParamsHistory.Add(TypeParams);
      end;

      FreeAndNil(LocalTypeParams);
    end;
  end;

begin
  TypeParams.Clear;
  result := Parse_Internal(0);
end;

procedure TBaseParser.AddTypeParameters(LevelId: Integer;
                                        E: TTypeExtRec = nil);
var
  I, Id: Integer;
  S: String;
  ParamList: TStringList;
  TR: TTypeRestrictionRec;
begin
  if TypeParams.Count = 0 then
    Exit;

  try
//    if CurrModule.IsExtra then
//      Exit;

    for I := 0 to TypeParams.Count - 1 do
    begin
      if CurrModule.IsExtra then
        if E = nil then
          continue;

      if EXTRA_SWITCH then
        continue;

      S := TypeParams[I];
      TR := TTypeRestrictionRec(TypeParams.Objects[I]);

      if TR <> nil then
      begin
        Id := NewTempVar(typeALIAS);
        GetSymbolRec(Id).PatternId := TR.Id;
      end
      else
      begin
        Id := NewTempVar(typeTYPEPARAM);
      end;

      SetKind(Id, KindTYPE);
      SetName(Id, S);
      SetLevel(Id, LevelId);

      if CurrModule.IsExtra then
        continue;

      if E <> nil then
        ParamList := E.ParamList
      else
        ParamList := TKernel(kernel).TypeDefList.Top.ParamList;

      if TR = nil then
        ParamList.Add(S)
      else
        ParamList.AddObject(S, TR.Clone);
    end;

    if CurrModule.IsExtra then
      Exit;

    for I := 0 to TKernel(kernel).UsedUnitList.Count - 1 do
    begin
      S := TKernel(kernel).UsedUnitList[I];
      if TKernel(kernel).TypeDefList.CurrTypeModuleRec.UsingList.IndexOf(S) = -1 then
        TKernel(kernel).TypeDefList.CurrTypeModuleRec.UsingList.Add(S);
    end;

  finally
    TypeParams.Clear;
    EXTRA_SWITCH := false;
  end;
end;

procedure TBaseParser.BeginTypeDef(TypeId: Integer);
var
  S: String;
  P: Integer;
  R: TTypeDefRec;
begin
//  if CurrModule.IsExtra then
//    Exit;

  S := GetName(TypeId);

  P := PosCh('<', S);
  if P > 0 then
    S := SCopy(S, SLow(S), P - SLow(S));

  R := TKernel(kernel).TypeDefList.Add(CurrModule.Name, LanguageId);
  R.IsGeneric := IsGeneric(TypeId);
  R.IsExtra := CurrModule.IsExtra;
  R.TypeId := TypeId;
  R.LangId := LanguageId;
  R.P1 := Scanner.Position - CurrToken.Length;
  R.Name := S;
end;

procedure TBaseParser.EndTypeDef(TypeId: Integer);
var
  S: String;
  R: TTypeDefRec;
begin
  TypeParams.Clear;

//  if CurrModule.IsExtra then
//    Exit;
//  if not IsGeneric(TypeId) then
//    Exit;

  R := TKernel(kernel).TypeDefList.FindTypeDef(TypeID);
  with R do
  begin
    P2 := Scanner.Position;
    S := SCopy(Scanner.Buff, P1, P2 - P1 + CurrToken.Length);
    Definition := S;
  end;
end;

procedure TBaseParser.BeginTypeExt(TypeId: Integer);
var
  R: TTypeDefRec;
  E: TTypeExtRec;
begin
  TypeParams.Clear;

//  if CurrModule.IsExtra then
//    Exit;

  R := TKernel(kernel).TypeDefList.FindTypeDef(TypeID);
  if R <> nil then
  begin
    E := R.TypeExtList.Add;
    E.P1 := CurrToken.Position;
    E.IsExtra := CurrModule.IsExtra;
  end;
end;

procedure TBaseParser.InitTypeExt(TypeId: Integer;
                                  const MemberName: String;
                                  IsMethodImpl: Boolean);
var
  R: TTypeDefRec;
  E: TTypeExtRec;
  P: Integer;
begin
//  if CurrModule.IsExtra then
//    Exit;

  if IsMethodImpl then
    R := TKernel(kernel).TypeDefList.Top
  else
    R := TKernel(kernel).TypeDefList.FindTypeDef(TypeID);

  if R <> nil then
  begin
    if R.TypeExtList.Count = 0 then
      Exit;

    E := R.TypeExtList.Top;
    P := PosCh('<', MemberName);
    if P > 0 then
    begin
      E.Valid := true;
      E.Name := SCopy(MemberName, SLow(MemberName), P - SLow(MemberName));
      AddTypeParameters(TypeId, E);

      if IsMethodImpl then
      begin
        if IsGeneric(TypeId) then
          TypeParamsHistory[TypeParamsHistory.Count - 2].AssTo(TypeParams)
        else
        begin
          RemoveType(TypeId);
        end;
      end;
    end
    else
      E.Name := MemberName;
  end;
end;

procedure TBaseParser.EndTypeExt(TypeId: Integer);
var
  S: String;
  R: TTypeDefRec;
  E: TTypeExtRec;
begin
  TypeParams.Clear;

//  if CurrModule.IsExtra then
//    Exit;

  R := TKernel(kernel).TypeDefList.FindTypeDef(TypeID);
  if R <> nil then
  begin
    E := R.TypeExtList.Top;

    if not E.Valid then
    begin
      R.TypeExtList.RemoveTop;
      Exit;
    end;

    E.P2 := CurrToken.Position;
    S := SCopy(Scanner.Buff, E.P1, E.P2 - E.P1 + CurrToken.Length);
    E.Extension := S;

    R.IsGeneric := true;

    if not IsGeneric(TypeId) then
      RemoveType(TypeId);
  end;
end;

procedure TBaseParser.BeginMethodDef;
var
  E: TTypeExtRec;
  R: TTypeDefRec;
begin
//  if CurrModule.IsExtra then
//    Exit;
  R := TKernel(kernel).TypeDefList.Add(CurrModule.Name, LanguageId);

  with R do
  begin
    IsExtra := CurrModule.IsExtra;
    LangId := LanguageId;
    P1 := Scanner.Position - CurrToken.Length;

    E := TypeExtList.Add;
    E.P1 := P1;
    E.IsExtra := CurrModule.IsExtra;
  end;
end;

procedure TBaseParser.InitMethodDef(SubId: Integer);
var
  TypeId: Integer;
  R: TTypeDefRec;
begin
//  if CurrModule.IsExtra then
//    Exit;

  TypeId := GetLevel(SubId);
  if TypeId = 0 then
    Exit;
  if GetKind(TypeId) <> KindTYPE then
    Exit;
//  if not IsGeneric(TypeId) then
//  begin
//    Exit;
//  end;

  R := TKernel(kernel).TypeDefList.Top;
  R.SubId := SubId;
  R.IsGeneric := IsGeneric(TypeId);
  R.TypeId := TypeId;
end;

procedure TBaseParser.EndMethodDef(SubId: Integer);
var
  S: String;
  P, TypeId: Integer;
  R: TTypeDefRec;
  E: TTypeExtRec;
begin
  TypeParams.Clear;

  TypeId := GetLevel(SubId);

//  if CurrModule.IsExtra then
//    Exit;
  if TypeId = 0 then
    Exit;
  if GetKind(TypeId) <> KindTYPE then
    Exit;

  S := GetName(TypeId);
  P := PosCh('<', S);
  if P > 0 then
    S := SCopy(S, SLow(S), P - SLow(S));

  R := TKernel(kernel).TypeDefList.FindMethodDef(SubId);
  with R do
  begin
    IsGeneric := P > 0;
    Name := S;
    P2 := Scanner.Position;
    S := SCopy(Scanner.Buff, P1, P2 - P1 + CurrToken.Length);
    Definition := S;
    IsMethodImplementation := true;
  end;

  E := R.TypeExtList.Top;

  if not E.Valid then
  begin
    R.TypeExtList.RemoveTop;
    Exit;
  end;

  E.P2 := R.P2;
  E.Extension := R.Definition;

  R.IsGeneric := true;
end;

procedure TBaseParser.SaveExtraNamespace(Id: Integer);
begin
  TKernel(kernel).Code.Extra_using_list.Add(Id);
end;

function TBaseParser.ParametrizedTypeExpected: Boolean;
begin
  if not GENERICS_ALLOWED then
    result := false
  else
    result := Scanner.ParametrizedTypeExpected;
end;

function TBaseParser.ReadType: Integer;
var
  S: String;
begin
  ReadToken;
  S := CurrToken.Text;
  if ParametrizedTypeExpected then
    S := S + Parse_TypeParams;
  result := Lookup(S, CurrLevel);
end;

function TBaseParser.IsGeneric(TypeId: Integer): Boolean;
begin
  result := PosCh('<', GetName(TypeId)) > 0;
end;

procedure TBaseParser.Parse_TypeRestriction(LocalTypeParams: TStringObjectList);
begin
end;

function TBaseParser.AltTypeId(const S: String): Integer;
begin
  result := 0;
end;

procedure TBaseParser.RemoveType(TypeId: Integer);
begin
  if CurrModule.IsExtra then
    Exit;

  if TKernel(kernel).TypeDefList.RemTypeIds.IndexOf(TypeId) = -1 then
    TKernel(kernel).TypeDefList.RemTypeIds.Add(TypeId);
end;

procedure TBaseParser.GenPause;
var
  b: Boolean;
  I, Op, L: Integer;
  Code: TCode;
begin
  Code := TKernel(kernel).Code;
  if (not DECLARE_SWITCH) and (EXECUTABLE_SWITCH > 0) then
  begin
    b := false;
    for I:=Code.Card downto 1 do
    begin
      Op := Code[I].Op;
      if Op = OP_SET_CODE_LINE then
        break
      else if Op = OP_CALL then
        break
      else if Op = OP_PUSH then
      begin
        b := true;
        break;
      end;
    end;

    Gen(OP_SET_CODE_LINE, 0, 0, 0);
    if DEBUG_MODE then if not b then
    begin
      L := NewLabel;
      Gen(OP_CHECK_PAUSE, L, 0, 0);
      SetLabelHere(L);
    end;
  end;
end;

function TBaseParser.IsTryContext(R: TEntryRec): Boolean;
var
  Op, I, K, CodeN: Integer;
  Code: TCode;
begin
  result := false;
  if try_stack.Count = 0 then
    Exit;
  CodeN := R.CodeN;
  Code := TKernel(kernel).Code;
  I := Code.Card + 1;
  K := 0;
  repeat
     Dec(I);
     if I = CodeN then
       break;
     Op := Code[I].Op;
     if Op = OP_TRY_ON then
       Inc(K)
     else if Op = OP_TRY_OFF then
       Dec(K);
  until false;
  result := K > 0;
end;

function TBaseParser.GetIsUNIC: Boolean;
begin
  result := TKernel(kernel).IsUNIC;
end;

procedure TBaseParser.NewAnonymousNames(var ClsName: String;
                                        var ObjName: String);
begin
  Inc(TKernel(kernel).AnonymousClassCount);
  ClsName := ANONYMOUS_CLASS_PREFIX +
    IntToStr(TKernel(kernel).AnonymousClassCount);
  ObjName := ANONYMOUS_OBJECT_PREFIX +
    IntToStr(TKernel(kernel).AnonymousClassCount);
end;

function TBaseParser.IsOuterLocalVar(Id: Integer): Boolean;
var
  I, L: Integer;
begin
  result := false;
  if GetKind(Id) <> KindVAR then
    Exit;
  if AnonymStack.Count = 0 then
    Exit;
  L := GetLevel(Id);
  if L = AnonymStack.Top.SubId then
    Exit;
  if L = CurrLevel then
    Exit;

  for I := 0 to LevelStack.Count - 2 do
  begin
    if LevelStack[I] > 0 then
      if GetKind(LevelStack[I]) in KindSUBS then
        if L = LevelStack[I] then
        begin
          result := true;
          Exit;
        end;
  end;
end;

procedure TBaseParser.GenComment(const S: String);
begin
{$IFDEF PAXARM}
  Gen(OP_ADD_COMMENT,
    TKernel(kernel).SymbolTable.AddPWideCharConst('//- ' + S).Id, 0, 0);
{$ELSE}
  Gen(OP_ADD_COMMENT,
    TKernel(kernel).SymbolTable.AddPAnsiCharConst('//- ' + AnsiString(S)).Id, 0, 0);
{$ENDIF}
end;

procedure TBaseParser.RelocateCode(K1, K2: Integer);
var
  I: Integer;
  Code: TCode;
  R: TCodeRec;
begin
  Code := TKernel(kernel).Code;

  for I := K1 to K2 do
  begin
    R := Code[I];
    Code.Add(R.Op, R.Arg1, R.Arg2, R.Res, R.SavedLevel, R.Upcase,
                     R.Language, CurrModule.ModuleNumber,
                     R.LinePos);
    R.Op := OP_NOP;
    R.GenOp := OP_NOP;
  end;
end;

procedure TBaseParser.RelocateCode(Ip, K1, K2: Integer);
var
  I: Integer;
  Code: TCode;
  R: TCodeRec;
  L: TCodeRecList;
begin
  Code := TKernel(kernel).Code;

  L := TCodeRecList.Create;
  for I := K1 to K2 do
  begin
    R := Code[I];
    L.Add(R);
  end;
  for I := K1 to K2 do
    Code.A.Delete(Code.A.Count - 1);

  while Ip > Code.A.Count do
    Dec(Ip);

  for I := 0 to L.Count - 1 do
  begin
    R := L[I];
    Code.A.Insert(Ip, R);
    Inc(Ip);
  end;

  FreeAndNil(L);
end;

procedure TBaseParser.RaiseNotImpl;
begin
  RaiseError(errNotImplementedYet, []);
end;

procedure TBaseParser.HideKeyword(KeywordIndex: Integer);
begin
  hidden_keywords.Add(KeywordIndex);
end;

procedure TBaseParser.RestoreKeywords;
begin
  hidden_keywords.Clear;
end;

function TBaseParser.ExtractText(P1, P2: Integer): String;
begin
  result := SCopy(scanner.Buff, P1, P2 - P1 + 1);
end;

procedure TBaseParser.SetPrevToken;
begin
  if CurrToken = nil then
  begin
    PrevPosition := SLow(scanner.Buff);
    PrevLength := 0;
  end
  else
  begin
    PrevPosition := CurrToken.Position;
    PrevLength := CurrToken.Length;
  end;
end;

function TBaseParser.CreatePointerType(type_id, pcount: Integer): Integer;
var
  S: String;
  T: Integer;
begin
  result := type_id;
  S := GetName(result);
  while pcount > 0 do
  begin
    T := GetPointerType(result);
    if T > 0 then
      result := T
    else
    begin
      S := S + '*';
      result := TKernel(kernel).SymbolTable.RegisterPointerType(0,
            S, result);
      SetHost(result, false);
      GetSymbolRec(result).Completed := false;
    end;
    Dec(pcount);
  end;
end;

function TBaseParser.CreateProceduralType(SubId: Integer): Integer;
var
  S: String;
begin
  S := GetName(SubId) + '&';
  result := TKernel(kernel).SymbolTable.RegisterProceduralType(0,
          S, - SubId);
  GetSymbolRec(result).Completed := false;
  SetHost(result, false);
end;

function TBaseParser.CreateSubrangeType(B1, B2: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.RegisterSubrangeType(0, '', typeINTEGER,
    B1, B2);
  GetSymbolRec(result).Completed := false;
  SetHost(result, false);
  SetName(result, 'SubType_' + IntToStr(result));
end;

function TBaseParser.CreateArrayType(RangeTypeId, ElemTypeId: Integer): Integer;
begin
  result := TKernel(kernel).SymbolTable.RegisterArrayType(0, '',
    RangeTypeId, ElemTypeId, 1);
  GetSymbolRec(result).Completed := false;
  SetHost(result, false);
  SetName(result, 'ArrayType_' + IntToStr(result));
end;

function TBaseParser.GetTargetPlatform: TTargetPlatform;
begin
  if kernel = nil then
    result := tpNONE
  else
    result := TKernel(kernel).TargetPlatform;
end;

function TBaseParser.GetRunnerKind: TRunnerKind;
begin
  if kernel = nil then
    result := rkNONE
  else
    result := TKernel(kernel).RunnerKind;
end;

function TBaseParser.GetSupportedSEH: Boolean;
begin
  if kernel = nil then
    result := false
  else
    result := TKernel(kernel).SupportedSEH;
end;

end.
