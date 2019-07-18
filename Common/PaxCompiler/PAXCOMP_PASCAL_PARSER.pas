///////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PASCAL_PARSER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_PASCAL_PARSER;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SCANNER,
  PAXCOMP_BYTECODE,
  PAXCOMP_MODULE,

  PAXCOMP_STDLIB,
  PAXCOMP_PARSER,
  PAXCOMP_KERNEL,
  PAXCOMP_BASERUNNER,
  PAXCOMP_CLASSFACT,
  PAXCOMP_GENERIC,
  PAXCOMP_PASCAL_SCANNER;

const
  dirNONE = 0;
  dirFORWARD = 1;
  dirOVERLOAD = 2;
  dirABSTRACT = 3;
  dirVIRTUAL = 4;
  dirOVERRIDE = 5;
  dirREINTRODUCE = 6;
  dirDYNAMIC = 7;
  dirSTATIC = 8;
  dirFINAL = 9;

type
  TPascalParser = class(TBaseParser)
  private
    I_STRICT: Integer;
    I_PRIVATE: Integer;
    I_PROTECTED: Integer;
    I_PUBLIC: Integer;
    I_PUBLISHED: Integer;
    WasInherited: Boolean;
    ForInCounter: Integer;
    CONST_ONLY: Boolean;
    OuterList: TAssocStringInt;
    procedure GenExternalSub(SubId: Integer);
    function MatchEx(const S: String): Boolean;
    function InScope(const S: String): Boolean;
    function Parse_AnonymousRoutine(IsFunc: Boolean): Integer;
    procedure RemoveKeywords;
    procedure RestoreKeywords;
  protected
    function CreateScanner: TBaseScanner; override;
    function GetLanguageName: String; override;
    function GetFileExt: String; override;
    function GetLanguageId: Integer; override;
    function GetUpcase: Boolean; override;
  public
    OnParseUnitName: TParserIdentEvent;
    OnParseImplementationSection: TParserNotifyEvent;
    OnParseBeginUsedUnitList: TParserNotifyEvent;
    OnParseEndUsedUnitList: TParserNotifyEvent;
    OnParseUsedUnitName: TParserIdentEvent;
    OnParseTypeDeclaration: TParserIdentEvent;
    OnParseForwardTypeDeclaration: TParserIdentEvent;
    OnParseBeginClassTypeDeclaration: TParserIdentEventEx;
    OnParseEndClassTypeDeclaration: TParserIdentEvent;
    OnParseAncestorTypeDeclaration: TParserIdentEvent;
    OnParseUsedInterface: TParserIdentEvent;
    OnParseClassReferenceTypeDeclaration: TParserTypedIdentEvent;
    OnParseAliasTypeDeclaration: TParserTypedIdentEvent;
    OnParseProceduralTypeDeclaration: TParserIdentEventEx;
    OnParseEventTypeDeclaration: TParserIdentEventEx;
    OnParseMethodReferenceTypeDeclaration: TParserIdentEventEx;
    OnParseSetTypeDeclaration: TParserTypedIdentEvent;
    OnParsePointerTypeDeclaration: TParserTypedIdentEvent;
    OnParseArrayTypeDeclaration: TParserArrayTypeEvent;
    OnParseDynArrayTypeDeclaration: TParserTypedIdentEvent;
    OnParseShortStringTypeDeclaration: TParserNamedValueEvent;
    OnParseSubrangeTypeDeclaration: TParserDeclarationEvent;
    OnParseBeginRecordTypeDeclaration: TParserIdentEventEx;
    OnParseEndRecordTypeDeclaration: TParserIdentEvent;
    OnParseBeginClassHelperTypeDeclaration: TParserTypedIdentEvent;
    OnParseEndClassHelperTypeDeclaration: TParserIdentEvent;
    OnParseBeginRecordHelperTypeDeclaration: TParserTypedIdentEvent;
    OnParseEndRecordHelperTypeDeclaration: TParserIdentEvent;
    OnParseBeginInterfaceTypeDeclaration: TParserIdentEvent;
    OnParseEndInterfaceTypeDeclaration: TParserIdentEvent;
    OnParseBeginEnumTypeDeclaration: TParserIdentEvent;
    OnParseEndEnumTypeDeclaration: TParserIdentEvent;
    OnParseEnumName: TParserNamedValueEvent;
    OnParseFieldDeclaration: TParserTypedIdentEvent;
    OnParseVariantRecordFieldDeclaration: TParserVariantRecordFieldEvent;
    OnParsePropertyDeclaration: TParserTypedIdentEvent;
    OnParseConstantDeclaration: TParserNamedValueEvent;
    OnParseResourceStringDeclaration: TParserNamedValueEvent;
    OnParseTypedConstantDeclaration: TParserNamedTypedValueEvent;
    OnParseVariableDeclaration: TParserTypedIdentEvent;
    OnParseBeginSubDeclaration: TParserIdentEvent;
    OnParseEndSubDeclaration: TParserDeclarationEvent;
    OnParseBeginFormalParameterList: TParserNotifyEvent;
    OnParseEndFormalParameterList: TParserNotifyEvent;
    OnParseFormalParameterDeclaration: TParserNamedTypedValueEvent;
    OnParseResultType: TParserIdentEvent;
    OnParseSubDirective: TParserIdentEvent;

    constructor Create; override;
    destructor Destroy; override;
    procedure ParseProgram; override;
    procedure Call_SCANNER; override;
    procedure Match(const S: String); override;
    procedure ReadToken; override;
    procedure InitSub(var SubId: Integer); override;
    function GetIncludedFileExt: String; override;
    procedure Init(i_kernel: Pointer; M: TModule); override;
    function Parse_DirectiveList(SubId: Integer): TIntegerList;
    function Parse_PortabilityDirective: TPortDir;

    procedure GenDefaultConstructor(ClassId: Integer);
    procedure GenDefaultDestructor(ClassId: Integer);

    procedure Parse_Attribute;
    procedure Parse_Message(SubId: Integer);
    procedure Parse_Library;
    procedure Parse_ProgramBlock(namespace_id: Integer);
    procedure Parse_Unit(IsExternalUnit: Boolean = false); override;
    procedure Parse_Block;

    procedure Parse_NamespaceDeclaration;
    procedure Parse_UsesClause(IsImplementationSection: Boolean);
    procedure Parse_NamespaceMemberDeclaration;

    procedure Parse_DeclarationPart(IsImplementation: Boolean = false);
    procedure Parse_VariableDeclaration(vis: TClassVisibility = cvNone);
    procedure Parse_ConstantDeclaration(vis: TClassVisibility = cvNone);
    procedure Parse_ResourcestringDeclaration;
    procedure Parse_LabelDeclaration;
    function Parse_FormalParameterList(SubId: Integer;
                                       bracket: Char = '('): Integer;
    procedure Parse_ProcedureDeclaration(IsSharedMethod: Boolean = false);
    procedure Parse_FunctionDeclaration(IsSharedMethod: Boolean = false);
    procedure Parse_OperatorDeclaration;
    procedure Parse_ConstructorDeclaration;
    procedure Parse_DestructorDeclaration;
    procedure Parse_SubBlock;
    procedure Parse_ConstantInitialization(ID: Integer);
    function Parse_VariableInitialization: Integer;

    // types
    procedure Parse_TypeDeclaration(IsExternalUnit: Boolean = false;
                                    vis: TClassVisibility = cvPublic);
    procedure Parse_ProceduralTypeDeclaration(TypeID: Integer;
                                              var SubId: Integer);
    procedure Parse_ArrayTypeDeclaration(ArrayTypeID: Integer; IsPacked: Boolean);
    function Parse_RecordConstructorHeading(IsSharedMethod: Boolean;
                                            RecordTypeId: Integer;
                                            Vis: TClassVisibility;
                                            IsExternalUnit: Boolean): Integer;
    function Parse_RecordDestructorHeading(IsSharedMethod: Boolean;
                                           RecordTypeId: Integer;
                                           Vis: TClassVisibility;
                                           IsExternalUnit: Boolean): Integer;
    function Parse_RecordProcedureHeading(IsSharedMethod: Boolean;
                                          RecordTypeId: Integer;
                                          Vis: TClassVisibility;
                                          IsExternalUnit: Boolean): Integer;
    function Parse_RecordFunctionHeading(IsSharedMethod: Boolean;
                                         RecordTypeId: Integer;
                                         Vis: TClassVisibility;
                                         IsExternalUnit: Boolean): Integer;
    function Parse_RecordOperatorHeading(RecordTypeId: Integer;
                                         Vis: TClassVisibility;
                                         IsExternalUnit: Boolean): Integer;
    function Parse_RecordProperty(RecordTypeId: Integer;
                                  Vis: TClassVisibility;
                                  IsExternalUnit: Boolean): Integer;
    procedure Parse_RecordVariantPart(VarLevel: Integer;
                                      CurrVarCnt: Int64;
                                      vis: TClassVisibility);
    procedure Parse_RecordHelperItem;
    procedure Parse_RecordTypeDeclaration(RecordTypeID: Integer; IsPacked: Boolean;
                                IsExternalUnit: Boolean = false);
    function Parse_ClassConstructorHeading(IsSharedMethod: Boolean;
                                           ClassTypeId: Integer;
                                           vis: TClassVisibility;
                                           IsExternalUnit: Boolean): Integer;
    function Parse_ClassDestructorHeading(IsSharedMethod: Boolean;
                                          ClassTypeId: Integer;
                                          vis: TClassVisibility;
                                          IsExternalUnit: Boolean): Integer;
    function Parse_ClassProcedureHeading(IsSharedMethod: Boolean;
                                         ClassTypeId: Integer;
                                         vis: TClassVisibility;
                                         IsExternalUnit: Boolean): Integer;
    function Parse_ClassFunctionHeading(IsSharedMethod: Boolean;
                                        ClassTypeId: Integer;
                                        vis: TClassVisibility;
                                        IsExternalUnit: Boolean): Integer;
    function Parse_ClassProperty(IsShared: Boolean;
                                 ClassTypeId: Integer;
                                 vis: TClassVisibility;
                                 IsExternalUnit: Boolean): Integer;
    procedure Parse_ClassTypeDeclaration(ClassTypeID: Integer; IsPacked: Boolean;
                                         IsExternalUnit: Boolean = false);
    procedure Parse_InterfaceTypeDeclaration(IntfTypeID: Integer);
    procedure Parse_MethodRefTypeDeclaration(TypeID: Integer);
    procedure Parse_EnumTypeDeclaration(TypeID: Integer);
    procedure Parse_PointerTypeDeclaration(TypeID: Integer);
{$IFNDEF PAXARM}
    procedure Parse_ShortStringTypeDeclaration(TypeID: Integer);
{$ENDIF}
    procedure Parse_SetTypeDeclaration(TypeID: Integer);
    procedure Parse_SubrangeTypeDeclaration(TypeID, TypeBaseId: Integer;
                                            var Declaration: String;
                                            Expr1ID: Integer = 0);
    function Parse_OrdinalType(var Declaration: String): Integer;
    function Parse_Type: Integer;
    function Parse_OpenArrayType(var ElemTypeName: String): Integer;

    procedure ParseExternalSub(SubId: Integer);

    //statements
    function Parse_Statement: Boolean;
    procedure Parse_CompoundStmt;
    procedure Parse_StmtList;
    procedure Parse_Write;
    procedure Parse_Writeln;
    procedure Parse_Print;
    procedure Parse_Println;
    procedure Parse_AssignmentStmt;
    procedure Parse_CaseStmt;
    procedure Parse_IfStmt;
    procedure Parse_GotoStmt;
    procedure Parse_BreakStmt;
    procedure Parse_ContinueStmt;
    procedure Parse_ExitStmt;
    procedure Parse_WhileStmt;
    procedure Parse_RepeatStmt;
    procedure Parse_ForStmt;
    procedure Parse_WithStmt;
    procedure Parse_TryStmt;
    procedure Parse_RaiseStmt;

    function Parse_LoopStmt(l_break, l_continue, l_loop: Integer): Boolean;

    //expressions
    function Parse_LambdaParameters(SubId: Integer) : Integer;
    function Parse_LambdaExpression: Integer;
    function Parse_AnonymousFunction: Integer;
    function Parse_AnonymousProcedure: Integer;
    function Parse_ArgumentList(SubId: Integer): Integer;
    function Parse_ConstantExpression: Integer;
    function Parse_Expression: Integer; override;
    function Parse_SimpleExpression: Integer;
    function Parse_Term: Integer;
    function Parse_Factor: Integer; override;
    function Parse_SetConstructor: Integer;
    function Parse_Designator(init_id: Integer = 0): Integer;
    function Parse_Label: Integer;
    function Parse_Ident: Integer; override;
    // generic
    procedure EndMethodDef(SubId: Integer); override;
    procedure Parse_TypeRestriction(LocalTypeParams: TStringObjectList); override;
  end;

  TPascalExprParser = class
  private
    kernel: Pointer;
    scanner: TPascalScanner;
    function GetCurrToken: String;
    function GetTokenClass: TTokenClass;
    function Parse_SetConstructor: Variant;
    function IsCurrText(const S: String): Boolean;
    procedure Match(const S: String);
    function NotMatch(const S: String): Boolean;
    procedure Cancel;
    procedure Call_SCANNER;
    function Parse_Expression: Variant;
    function Parse_SimpleExpression: Variant;
    function Parse_Term: Variant;
    function Parse_Factor: Variant;
  public
    LevelList: TIntegerList;
    IsSet: Boolean;
    ResExpr: String;
    constructor Create(akernel: Pointer; const Expr: String);
    destructor Destroy; override;
    function ParseExpression: Variant;
    function Lookup(const S: String): Variant;
    function LegalValue(const V: Variant): Boolean;
    property CurrToken: String read GetCurrToken;
    property TokenClass: TTokenClass read GetTokenClass;
  end;

implementation

uses
  PAXCOMP_VAROBJECT;

constructor TPascalExprParser.Create(akernel: Pointer; const Expr: String);
begin
  inherited Create;
  LevelList := TIntegerList.Create(true);
  kernel := akernel;
  scanner := TPascalScanner.Create;
  scanner.Init(kernel, Expr, 0);
  ResExpr := '';
end;

destructor TPascalExprParser.Destroy;
begin
  scanner.Free;
  LevelList.Free;
  inherited;
end;

function TPascalExprParser.GetCurrToken: String;
begin
  result := scanner.Token.Text;
end;

function TPascalExprParser.GetTokenClass: TTokenClass;
begin
  result := scanner.Token.TokenClass;
end;

function TPascalExprParser.IsCurrText(const S: String): Boolean;
begin
  result := StrEql(S, CurrToken);
end;

function TPascalExprParser.LegalValue(const V: Variant): Boolean;
begin
  result := VarType(V) <> varEmpty;
end;

procedure TPascalExprParser.Match(const S: String);
begin
  if IsCurrText(S) then
    Call_SCANNER
  else
    Cancel;
end;

procedure TPascalExprParser.Cancel;
begin
  raise PaxCancelException.Create('');
end;

procedure TPascalExprParser.Call_SCANNER;
begin
  scanner.ReadToken;
  ResExpr := ResExpr + CurrToken;
end;

function TPascalExprParser.NotMatch(const S: String): Boolean;
begin
  if not IsCurrText(S) then
    result := true
  else
  begin
    result := false;
    Call_SCANNER;
  end;
end;

function TPascalExprParser.Lookup(const S: String): Variant;
var
  I, L, Id: Integer;
begin
  for I := 0 to LevelList.Count - 1 do
  begin
    L := LevelList[I];
    Id := TKernel(kernel).SymbolTable.LookUp(S, L, true);
    if Id > 0 then
    begin
      result := TKernel(kernel).SymbolTable[Id].Value;
      Exit;
    end;
  end;
end;

function TPascalExprParser.ParseExpression: Variant;
begin
  Call_SCANNER;
  Call_SCANNER;
  Call_SCANNER;
  try
    result := Parse_Expression;
  except
    // canceled
  end;
end;

function TPascalExprParser.Parse_Expression: Variant;
var
  Op: Integer;
  V: Variant;
begin
  result := Parse_SimpleExpression;
  if not LegalValue(result) then
    Cancel;

  if CurrToken = '=' then
    Op := OP_EQ
  else if CurrToken = '<>' then
    Op := OP_NE
  else if CurrToken = '>' then
    Op := OP_GT
  else if CurrToken = '>=' then
    Op := OP_GE
  else if CurrToken = '<' then
    Op := OP_LT
  else if CurrToken = '<=' then
    Op := OP_LT
  else
    Op := 0;

  while Op <> 0 do
  begin
    Call_SCANNER;
    V := Parse_SimpleExpression;
    if not LegalValue(V) then
      Cancel;

    if Op = OP_EQ then
  		result := result = V
    else if Op = OP_NE then
  		result := result <> V
    else if Op = OP_GT then
  		result := result > V
    else if Op = OP_GE then
  		result := result >= V
    else if Op = OP_LT then
  		result := result < V
    else if Op = OP_LE then
  		result := result <= V;

    if CurrToken = '=' then
      Op := OP_EQ
    else if CurrToken = '<>' then
      Op := OP_NE
    else if CurrToken = '>' then
      Op := OP_GT
    else if CurrToken = '>=' then
      Op := OP_GE
    else if CurrToken = '<' then
      Op := OP_LT
    else if CurrToken = '<=' then
      Op := OP_LT
    else
      Op := 0;
  end;
end;

function TPascalExprParser.Parse_SimpleExpression: Variant;
var
  Op: Integer;
  V: Variant;
begin
  result := Parse_Term;
  if not LegalValue(result) then
    Cancel;

  if CurrToken = '+' then
    Op := OP_PLUS
  else if CurrToken = '-' then
    Op := OP_MINUS
  else if StrEql(CurrToken, 'or') then
    Op := OP_OR
  else if StrEql(CurrToken, 'xor') then
    Op := OP_XOR
  else
    Op := 0;

  while Op <> 0 do
  begin
    Call_SCANNER;
    V := Parse_Term;
    if not LegalValue(V) then
      Cancel;

    if Op = OP_PLUS then
   		result := result + V
    else if Op = OP_MINUS then
  		result := result - V
    else if Op = OP_OR then
  		result := result or V
    else if Op = OP_XOR then
  		result := result xor V;

    if CurrToken = '+' then
      Op := OP_PLUS
    else if CurrToken = '-' then
      Op := OP_MINUS
    else if StrEql(CurrToken, 'or') then
      Op := OP_OR
    else if StrEql(CurrToken, 'xor') then
      Op := OP_XOR
    else
      Op := 0;
  end;
end;

function TPascalExprParser.Parse_Term: Variant;
var
  Op: Integer;
  V: Variant;
begin
  result := Parse_Factor;
  if not LegalValue(result) then
    Cancel;

  if CurrToken = '*' then
    Op := OP_MULT
  else if CurrToken = '/' then
    Op := OP_DIV
  else if StrEql(CurrToken, 'div') then
    Op := OP_IDIV
  else if StrEql(CurrToken, 'mod') then
    Op := OP_MOD
  else if StrEql(CurrToken, 'shl') then
    Op := OP_SHL
  else if StrEql(CurrToken, 'shr') then
    Op := OP_SHL
  else if StrEql(CurrToken, 'and') then
    Op := OP_AND
  else
    Op := 0;

  while Op <> 0 do
  begin
    Call_SCANNER;
    V := Parse_Factor;
    if not LegalValue(V) then
      Cancel;

    if Op = OP_MULT then
  		result := result * V
    else if Op = OP_DIV then
    begin
      if V = 0.0 then
      begin
        if result = 0.0 then
          result := NaN
        else if result = 1.0 then
          result := Infinity
        else if result = - 1.0 then
          result := NegInfinity
      end
      else
    		result := result / V;
    end
    else if Op = OP_IDIV then
  		result := result div V
    else if Op = OP_MOD then
  		result := result mod V
    else if Op = OP_SHL then
  		result := result shl V
    else if Op = OP_SHR then
  		result := result shr V
    else if Op = OP_AND then
  		result := result and V;

    if CurrToken = '*' then
      Op := OP_MULT
    else if CurrToken = '/' then
      Op := OP_DIV
    else if StrEql(CurrToken, 'div') then
      Op := OP_IDIV
    else if StrEql(CurrToken, 'mod') then
      Op := OP_MOD
    else if StrEql(CurrToken, 'shl') then
      Op := OP_SHL
    else if StrEql(CurrToken, 'shr') then
      Op := OP_SHL
    else if StrEql(CurrToken, 'and') then
      Op := OP_AND
    else
      Op := 0;
  end;
end;

function TPascalExprParser.Parse_Factor: Variant;
var
  I, J: Integer;
  D: Double;
begin
{$IFDEF PAXARM}
  if TokenClass = tcCharConst then
  begin
    result := Ord(CurrToken[1]);
    Call_SCANNER;
  end
  else if TokenClass = tcNumCharConst then
  begin
    result := StrToInt(CurrToken);
    Call_SCANNER;
  end
  else if TokenClass = tcPCharConst then
  begin
    result := CurrToken;
    Call_SCANNER;
  end
{$ELSE}
  if TokenClass = tcCharConst then
  begin
    result := Ord(CurrToken[1]);
    Call_SCANNER;
  end
  else if TokenClass = tcNumCharConst then
  begin
    result := StrToInt(CurrToken);
    Call_SCANNER;
  end
  else if TokenClass = tcPCharConst then
  begin
    result := CurrToken;
    Call_SCANNER;
  end
{$ENDIF}
  else if TokenClass = tcIntegerConst then
  begin
    val(CurrToken, i, j);
    if j = 0 then begin
      if Pos('$', CurrToken) > 0 then
      begin
{$IFDEF VARIANTS}
        result := Cardinal(i);
{$ELSE}
        result := Integer(i);
{$ENDIF}
      end
      else
      begin
        result := i;
      end;
    end;
    Call_SCANNER;
  end
  else if TokenClass = tcVariantConst then
  begin
    Call_SCANNER;
  end
  else if TokenClass = tcDoubleConst then
  begin
    Val(CurrToken, D, I);
    result := D;
    Call_SCANNER;
  end
  else if IsCurrText('nil') then
  begin
    result := 0;
    Call_SCANNER;
  end
  else if IsCurrText('true') then
  begin
    result := true;
    Call_SCANNER;
  end
  else if IsCurrText('false') then
  begin
    result := false;
    Call_SCANNER;
  end
	else if IsCurrText('+') then
  begin
		Call_SCANNER;
		result := Parse_Factor;
  end
	else if IsCurrText('-') then
  begin
		Call_SCANNER;
		result := - Parse_Factor;
  end
	else if IsCurrText('not') then
  begin
		Call_SCANNER;
		result := not Parse_Factor;
  end
	else if IsCurrText('low') then
  begin
    Call_SCANNER;
    Match('(');
    result := Lookup(CurrToken);
		Call_SCANNER;
    Match(')');
  end
	else if IsCurrText('high') then
  begin
    Call_SCANNER;
    Match('(');
    I := Lookup(CurrToken);
		Call_SCANNER;
    Match(')');
  end
	else if IsCurrText('SizeOf') then
  begin
    Call_SCANNER;
    Match('(');
    I := Lookup(CurrToken);
		Call_SCANNER;
    Match(')');
  end
	else if IsCurrText('pred') then
  begin
		Call_SCANNER;
    Match('(');
		result := Parse_Expression - 1;
    Match(')');
  end
	else if IsCurrText('succ') then
  begin
		Call_SCANNER;
    Match('(');
		result := Parse_Expression + 1;
    Match(')');
  end
	else if IsCurrText('ord') then
  begin
		Call_SCANNER;
    Match('(');
		result := Parse_Expression;
    Match(')');
  end
	else if IsCurrText('chr') then
  begin
		Call_SCANNER;
    Match('(');
		result := Parse_Expression;
    Match(')');
  end
  else if IsCurrText('(') then
  begin
    Match('(');
    result := Parse_Expression;
    Match(')');
  end
  else if IsCurrText('@') then
  begin
    Cancel;
  end
  else if IsCurrText('[') then
  begin
    result := Parse_SetConstructor;
  end
  else if IsCurrText('procedure') then
  begin
    Cancel;
  end
  else if IsCurrText('function') then
  begin
    Cancel;
  end
  else if IsCurrText('array') then
  begin
    Cancel;
  end
  else if IsCurrText('deprecated') then
  begin
    Cancel;
  end
  else if IsCurrText('pchar') then
  begin
    Cancel;
  end
  else if IsCurrText('[') then
  begin
    result := Parse_SetConstructor;
  end
  else
  begin
    result := LookUp(CurrToken);
    Call_SCANNER;
    while IsCurrText('.') do
    begin
      Match('.');
      result := LookUp(CurrToken);
      Call_SCANNER;
    end;
    if IsCurrText('(') then
    begin
      Match('(');
      result := Parse_Expression;
      Match(')');
    end;
  end;
end;

function TPascalExprParser.Parse_SetConstructor: Variant;
begin
  Match('[');
  if not IsCurrText(']') then
  begin
    repeat
      Parse_Expression;
      if IsCurrText('..') then
      begin
        Match('..');
        Parse_Expression;
      end;
      If NotMatch(',') then
        break;
    until false;
  end;
  Match(']');
  IsSet := true;
  result := ResExpr;
end;

constructor TPascalParser.Create;
begin
  inherited;

  OuterList := TAssocStringInt.Create;

  AddKeyword('and');
  AddKeyword('array');
  AddKeyword('as');
  AddKeyword('asm');
  AddKeyword('begin');
  AddKeyword('case');
  AddKeyword('class');
  AddKeyword('const');
  AddKeyword('constructor');
  AddKeyword('destructor');
  AddKeyword('dispinterface');
  AddKeyword('div');
  AddKeyword('do');
  AddKeyword('downto');
  AddKeyword('else');

  AddKeyword('end');
  AddKeyword('except');
  AddKeyword('exports');

  AddKeyword('external');

  AddKeyword('file');
  AddKeyword('finalization');
  AddKeyword('finally');
  AddKeyword('for');
  AddKeyword('function');
  AddKeyword('goto');
  AddKeyword('if');
  AddKeyword('implementation');
  AddKeyword('in');
  AddKeyword('inherited');
  AddKeyword('initialization');
  AddKeyword('inline');
  AddKeyword('interface');
  AddKeyword('is');
  AddKeyword('label');
  AddKeyword('library');
  AddKeyword('mod');
  AddKeyword('nil');
  AddKeyword('not');
  AddKeyword('object');
  AddKeyword('of');
  AddKeyword('on');
  AddKeyword('or');
  AddKeyword('out');
  AddKeyword('packed');
  I_STRICT := AddKeyword('strict');
  I_PRIVATE := AddKeyword('private');
  AddKeyword('procedure');
  AddKeyword('program');
  AddKeyword('property');
  I_PROTECTED := AddKeyword('protected');
  I_PUBLIC := AddKeyword('public');
  I_PUBLISHED := AddKeyword('published');
  AddKeyword('raise');
  AddKeyword('record');
  AddKeyword('repeat');
  AddKeyword('resourcestring');
  AddKeyword('set');
  AddKeyword('shl');
  AddKeyword('shr');
  AddKeyword('string');
  AddKeyword('then');
  AddKeyword('threadvar');
  AddKeyword('to');
  AddKeyword('try');
  AddKeyword('type');
  AddKeyword('unit');
  AddKeyword('until');
  AddKeyword('uses');
  AddKeyword('var');
  AddKeyword('while');
  AddKeyword('with');
  AddKeyword('xor');
  AddKeyword(EXTRA_KEYWORD);

  AddOperator(pascal_Implicit, gen_Implicit);
  AddOperator(pascal_Explicit, gen_Explicit);
  AddOperator(pascal_Add, gen_Add);
  AddOperator(pascal_Divide, gen_Divide);
  AddOperator(pascal_IntDivide, gen_IntDivide);
  AddOperator(pascal_Modulus, gen_Modulus);
  AddOperator(pascal_Multiply, gen_Multiply);
  AddOperator(pascal_Subtract, gen_Subtract);
  AddOperator(pascal_Negative, gen_Negative);
  AddOperator(pascal_Positive, gen_Positive);
  AddOperator(pascal_LogicalNot, gen_LogicalNot);
  AddOperator(pascal_LeftShift, gen_LeftShift);
  AddOperator(pascal_RightShift, gen_RightShift);
  AddOperator(pascal_LogicalAnd, gen_LogicalAnd);
  AddOperator(pascal_LogicalOr, gen_LogicalOr);
  AddOperator(pascal_LogicalXor, gen_LogicalXor);
  AddOperator(pascal_LessThan, gen_LessThan);
  AddOperator(pascal_LessThanOrEqual, gen_LessThanOrEqual);
  AddOperator(pascal_GreaterThan, gen_GreaterThan);
  AddOperator(pascal_GreaterThanOrEqual, gen_GreaterThanOrEqual);
  AddOperator(pascal_Equal, gen_Equal);
  AddOperator(pascal_NotEqual, gen_NotEqual);
  AddOperator(pascal_Inc, gen_Inc);
  AddOperator(pascal_Dec, gen_Dec);
end;

destructor TPascalParser.Destroy;
begin
  FreeAndNil(OuterList);
  inherited;
end;

function TPascalParser.CreateScanner: TBaseScanner;
begin
  result := TPascalScanner.Create;
end;

function TPascalParser.GetLanguageName: String;
begin
  result := 'Pascal';
end;

function TPascalParser.GetFileExt: String;
begin
  result := 'pas';
end;

function TPascalParser.GetIncludedFileExt: String;
begin
  result := 'pas';
end;

function TPascalParser.GetLanguageId: Integer;
begin
  result := PASCAL_LANGUAGE;
end;

function TPascalParser.GetUpcase: Boolean;
begin
  result := true;
end;

procedure TPascalParser.Init(i_kernel: Pointer; M: TModule);
begin
  Inherited Init(i_kernel, M);
  WasInherited := true;
  ForInCounter := 0;
  IMPLEMENTATION_SECTION := false;
  OuterList.Clear;
end;

procedure TPascalParser.GenDefaultConstructor(ClassId: Integer);
var
  SubId, ResId, L: Integer;
begin
  GenComment('BEGIN OF DEFAULT CONSTRUCTOR OF ' + GetName(ClassId));

  LevelStack.Push(ClassId);
  SubId := NewTempVar;
  SetName(SubId, 'Create');

  BeginClassConstructor(SubId, ClassId);
  SetVisibility(SubId, cvPublic);
  inherited InitSub(SubId);

  SetCallMode(SubId, cmOVERRIDE);
  Gen(OP_ADD_MESSAGE, SubId, NewConst(typeINTEGER, -1000), 0);
  Gen(OP_CHECK_OVERRIDE, SubId, 0, 0);

  Gen(OP_SAVE_EDX, 0, 0, 0);

  L := NewLabel;
  Gen(OP_GO_DL, L, 0, 0);
  Gen(OP_CREATE_OBJECT, ClassId, 0, CurrSelfId);

  if GetSymbolRec(ClassId).IsAbstract then
    Gen(OP_ERR_ABSTRACT, NewConst(typeSTRING,
          GetFullName(ClassId)), 0, SubId);

  SetLabelHere(L);

  Gen(OP_BEGIN_WITH, CurrSelfId, 0, 0);
  WithStack.Push(CurrSelfId);

  NewTempVar;
  ResId := NewTempVar;

  Gen(OP_PUSH_CLASSREF, CurrSelfId, 0, ResId);
  Gen(OP_EVAL_INHERITED, SubId, 0, ResId);

  SetDefault(SubId, true);
  Gen(OP_UPDATE_DEFAULT_CONSTRUCTOR, SubId, 0, ResId);
// will insertion here

  Gen(OP_CALL_INHERITED, ResId, 0, 0);

  Gen(OP_END_WITH, WithStack.Top, 0, 0);
  WithStack.Pop;

  Gen(OP_RESTORE_EDX, 0, 0, 0);
  L := NewLabel;
  Gen(OP_GO_DL, L, 0, 0);
  Gen(OP_ONCREATE_OBJECT, CurrSelfId, 0, 0);
  Gen(OP_ON_AFTER_OBJECT_CREATION, CurrSelfId, 0, 0);
  SetLabelHere(L);

  EndSub(SubId);
  LevelStack.Pop;

  GenComment('END OF DEFAULT CONSTRUCTOR OF ' + GetName(ClassId));
end;

procedure TPascalParser.GenDefaultDestructor(ClassId: Integer);
var
  SubId, Id, ResId: Integer;
begin
  GenComment('BEGIN OF DEFAULT DESTRUCTOR OF ' + GetName(ClassId));

  LevelStack.Push(ClassId);
  SubId := NewTempVar;
  SetName(SubId, 'Destroy');
  BeginClassDestructor(SubId, ClassId);
  SetVisibility(SubId, cvPublic);
  SetCallMode(SubId, cmOVERRIDE);
  inherited InitSub(SubId);

  Gen(OP_BEGIN_WITH, CurrSelfId, 0, 0);
  WithStack.Push(CurrSelfId);

  Id := NewTempVar;
  ResId := NewTempVar;
  SetName(Id, 'Destroy');
  Gen(OP_EVAL, 0, 0, Id);
  Gen(OP_EVAL_INHERITED, Id, 0, ResId);

  Gen(OP_CALL, ResId, 0, 0);

  Gen(OP_END_WITH, WithStack.Top, 0, 0);
  WithStack.Pop;

  EndSub(SubId);
  LevelStack.Pop;

  GenComment('END OF DEFAULT DESTRUCTOR OF ' + GetName(ClassId));
end;

procedure TPascalParser.Parse_DeclarationPart(IsImplementation: Boolean = false);
var
  ok: Boolean;
begin
  repeat
    ok := false;
    if IsCurrText('label') then
    begin
      Parse_LabelDeclaration;
      ok := true;
    end
    else if IsCurrText('var') then
    begin
      Parse_VariableDeclaration;
      ok := true;
    end
    else if IsCurrText('threadvar') then
    begin
      Parse_VariableDeclaration;
      ok := true;
    end
    else if IsCurrText('const') then
    begin
      Parse_ConstantDeclaration;
      ok := true;
    end
    else if IsCurrText('resourcestring') then
    begin
      Parse_ResourcestringDeclaration;
      ok := true;
    end
    else if IsCurrText('procedure') then
    begin
      Parse_ProcedureDeclaration;
      ok := true;
    end
    else if IsCurrText('function') then
    begin
      Parse_FunctionDeclaration;
      ok := true;
    end
    else if IsCurrText('class') then
    begin
      Call_SCANNER;
      if IsCurrText('procedure') then
        Parse_ProcedureDeclaration(true)
      else if IsCurrText('function') then
        Parse_FunctionDeclaration(true)
      else if IsCurrText('operator') then
        Parse_OperatorDeclaration
      else
        Match('procedure');
      ok := true;
    end
    else if IsCurrText('constructor') then
    begin
      Parse_ConstructorDeclaration;
      ok := true;
    end
    else if IsCurrText('destructor') then
    begin
      Parse_DestructorDeclaration;
      ok := true;
    end
    else if IsCurrText('type') then
    begin
      Parse_TypeDeclaration;
      ok := true;
    end
  until not ok;

  if GetKind(LevelStack.Top) in KindSUBS then
    Exit;
end;

procedure TPascalParser.ParseProgram;
var
  namespace_id: Integer;
begin
  EXECUTABLE_SWITCH := 0;
  Call_SCANNER;

  if IsEOF then
    Exit;

  namespace_id := 0;

  if IsCurrText('program') then
  begin
    DECLARE_SWITCH := true;
    Call_SCANNER;
//    SetKind(CurrToken.Id, KindNONE);
//    Call_SCANNER;
    namespace_id := Parse_Ident;
    DECLARE_SWITCH := false;
    Match(';');
  end;

  if IsCurrText('unit') then
    Parse_Unit
  else if IsCurrText('library') then
    Parse_Library
  else
    Parse_ProgramBlock(namespace_id);
end;

procedure TPascalParser.Parse_Library;
var
  id, I: Integer;
  L: TAssocStringInt;
  S: String;
begin
  DECLARE_SWITCH := true;
  Match('library');

  Gen(OP_BEGIN_LIBRARY, Parse_Ident, 0, 0);
  Match(';');

  while IsCurrText('uses') do
  begin
    Parse_UsesClause(false);
  end;

  Gen(OP_END_IMPORT, 0, 0, 0);

  repeat
    if IsEOF then
      Match('exports');
    if IsCurrText('exports') then
      break;
    Parse_NamespaceMemberDeclaration;
  until false;

  DECLARE_SWITCH := false;

  Gen(OP_BEGIN_EXPORT, 0, 0, 0);
  Match('exports');

  L := TAssocStringInt.Create;
  try
    repeat
      S := CurrToken.Text;
      id := Parse_Ident;
      L.AddValue(S, id);
      if IsCurrText(',') then
        Call_SCANNER
      else
        break;
    until false;
//    L.Sort;
    for I := 0 to L.Count - 1 do
    begin
      Id := L.Values[I];
      Gen(OP_EXPORTS, id, 0, 0);
    end;
  finally
    FreeAndNil(L);
  end;
  Match(';');

  Parse_CompoundStmt;
  MatchFinal('.');
end;

procedure TPascalParser.Parse_NamespaceDeclaration;
var
  l: TIntegerList;
  i, namespace_id: Integer;
begin
  DECLARE_SWITCH := true;
  RemoveLastIdent(CurrToken.Id);
	Match('namespace');

  l := TIntegerList.Create;

  try
    repeat // ParseQualifiedIdentifier
      namespace_id := Parse_Ident;
      l.Add(namespace_id);
      BeginNamespace(namespace_id);
      if NotMatch('.') then
        break;
    until false;

    // Parse namespace body

    repeat
      if IsEOF then
        Match('end');
      if IsCurrText('end') then
        break;
      Parse_NamespaceMemberDeclaration;
    until false;

    for i := l.Count - 1 downto 0 do
    begin
      EndNamespace(l[i]);
      Gen(OP_BEGIN_USING, l[i], 0, 0);
    end;

  finally
    FreeAndNil(L);
  end;

  Match('end');
  Match(';');
end;

procedure TPascalParser.Parse_UsesClause(IsImplementationSection: Boolean);
var
  unit_id, id: Integer;
  S: String;
  AlreadyExists: Boolean;
  RootKernel: TKernel;
begin
  RootKernel := TKernel(Kernel).RootKernel;

  UsedUnitList.Clear;

  DECLARE_SWITCH := false;
	Match('uses');
  if Assigned(OnParseBeginUsedUnitList) then
    OnParseBeginUsedUnitList(Owner);

  repeat
    unit_id := Parse_UnitName(S);
    if Assigned(OnParseUsedUnitName) then
      OnParseUsedUnitName(Owner, S, unit_id);

    AlreadyExists := GetKind(unit_id) = kindNAMESPACE;

    Gen(OP_BEGIN_USING, unit_id, 0, 0);

    if IsCurrText('in') then
    begin
      Call_SCANNER;
      id := Parse_PCharLiteral;
      S := GetValue(id);
      if (PosCh('\', S) > 0) or (PosCh('/', S) > 0) then
      if not Assigned(RootKernel.OnUsedUnit) then
      begin
        if (Pos('.\', S) > 0) or (Pos('./', S) > 0) then
          S := ExpandFileName(S)
        else
          S := GetCurrentDir + S;
      end;

      AlreadyExists := false;
    end
    else
      S := S + '.' + GetFileExt;

    if not AlreadyExists then
      if not ImportOnly then
      AddModuleFromFile(S, unit_id, IsImplementationSection);

    if NotMatch(',') then
      Break;
  until false;

  Match(';');
end;

procedure TPascalParser.Parse_NamespaceMemberDeclaration;
begin
  if IsCurrText('type') then
    Parse_TypeDeclaration
  else if IsCurrText('procedure') then
    Parse_ProcedureDeclaration
  else if IsCurrText('function') then
    Parse_FunctionDeclaration
  else if IsCurrText('class') then
  begin
    Call_SCANNER;
    if IsCurrText('procedure') then
      Parse_ProcedureDeclaration(true)
    else if IsCurrText('function') then
      Parse_FunctionDeclaration(true)
    else if IsCurrText('operator') then
      Parse_OperatorDeclaration
    else
      Match('procedure');
  end
  else if IsCurrText('var') then
    Parse_VariableDeclaration
  else if IsCurrText('const') then
    Parse_ConstantDeclaration
  else if IsCurrText('resourcestring') then
    Parse_ResourcestringDeclaration
  else
    Match('end');
end;

function TPascalParser.Parse_Statement: Boolean;
begin
  result := false;

  if CurrToken.TokenClass = tcIdentifier then
    if GetKind(CurrToken.Id) = KindLABEL then
    if GetName(CurrToken.Id) <> '' then
    begin
      SetLabelHere(CurrToken.Id);
      Call_SCANNER;
      Match(':');
    end;

  Gen(OP_STMT, 0, 0, 0);

  if IsCurrText('begin') then
  begin
    Parse_CompoundStmt;
    result := true;
  end
  else if IsCurrText('case') then
    Parse_CaseStmt
  else if IsCurrText('if') then
    Parse_IfStmt
  else if IsCurrText('goto') then
    Parse_GotoStmt
  else if IsCurrText('while') then
    Parse_WhileStmt
  else if IsCurrText('repeat') then
    Parse_RepeatStmt
  else if IsCurrText('for') then
    Parse_ForStmt
  else if IsCurrText('break') then
  begin
  	if (BreakStack.Count = 0) or (Lookups('break', LevelStack) > 0) then
      Parse_AssignmentStmt
    else
    begin
      RemoveLastEvalInstructionAndName('break');
      Parse_BreakStmt;
    end;
  end
  else if IsCurrText('continue') then
  begin
  	if (ContinueStack.Count = 0) or (Lookups('continue', LevelStack) > 0) then
      Parse_AssignmentStmt
    else
    begin
      RemoveLastEvalInstructionAndName('continue');
      Parse_ContinueStmt;
    end;
  end
  else if IsCurrText('exit') then
  begin
  	if Lookups('exit', LevelStack) > 0 then
      Parse_AssignmentStmt
    else
    begin
      RemoveLastEvalInstructionAndName('exit');
      Parse_ExitStmt;
    end;
  end
  else if IsCurrText('with') then
    Parse_WithStmt
  else if IsCurrText('try') then
    Parse_TryStmt
  else if IsCurrText('raise') then
    Parse_RaiseStmt
  else
  begin
    if IsCurrText(PrintKeyword) then
    begin
      if (CurrToken.Id > StdCard) and (GetKind(CurrToken.Id) = kindSUB) then
        Parse_AssignmentStmt
      else
      begin
        Call_SCANNER;
        Parse_Print;
      end;
    end
    else if IsCurrText(PrintlnKeyword) then
    begin
      if (CurrToken.Id > StdCard) and (GetKind(CurrToken.Id) = kindSUB) then
        Parse_AssignmentStmt
      else
      begin
        Call_SCANNER;
        Parse_Println;
      end;
    end
    else if IsCurrText('write') then
    begin
      Call_SCANNER;
      Parse_Write;
    end
    else if IsCurrText('writeln') then
    begin
      Call_SCANNER;
      Parse_Writeln;
    end
    else
      Parse_AssignmentStmt;
  end;
  Gen(OP_STMT, 0, 0, 0);
end;

procedure TPascalParser.Parse_Write;
var
  ID, ID_L1, ID_L2: Integer;
begin
  IsConsoleApp := true;

  Match('(');
  repeat
    ID := Parse_Expression;
    ID_L1 := 0;
    ID_L2 := 0;
    if IsCurrText(':') then
    begin
      Call_SCANNER;
      ID_L1 := Parse_Expression;
    end;
    if IsCurrText(':') then
    begin
      Call_SCANNER;
      ID_L2 := Parse_Expression;
    end;

    Gen(OP_PRINT, ID, ID_L1, ID_L2);
    if NotMatch(',') then
      Break;
  until false;
  Match(')');
end;

procedure TPascalParser.Parse_Writeln;
begin
  IsConsoleApp := true;

  if IsCurrText('(') then
    Parse_Write;
  Gen(OP_PRINT, 0, 0, 0);
end;

procedure TPascalParser.Parse_Print;
var
  ID, ID_L1, ID_L2: Integer;
begin
  if IsCurrText(';') then
  begin
    Exit;
  end;

  repeat
    ID := Parse_Expression;
    ID_L1 := 0;
    ID_L2 := 0;
    if IsCurrText(':') then
    begin
      Call_SCANNER;
      ID_L1 := Parse_Expression;
    end;
    if IsCurrText(':') then
    begin
      Call_SCANNER;
      ID_L2 := Parse_Expression;
    end;

    Gen(OP_PRINT_EX, ID, ID_L1, ID_L2);
    if NotMatch(',') then
      Break;
  until false;
end;

procedure TPascalParser.Parse_Println;
begin
  if not IsCurrText(';') then
    Parse_Print;
{$IFDEF PAXARM}
  Gen(OP_PRINT_EX, NewConst(typeUNICSTRING, #13#10), 0, 0);
{$ELSE}
  Gen(OP_PRINT_EX, NewConst(typeANSISTRING, #13#10), 0, 0);
{$ENDIF}
end;

procedure TPascalParser.Parse_Block;
begin
  DECLARE_SWITCH := true;
  Parse_DeclarationPart;
  Parse_CompoundStmt;
end;

procedure TPascalParser.Parse_ProgramBlock(namespace_id: Integer);
var
  B1, B2: Integer;
begin
  {$IFDEF ZERO_NS}
  namespace_id := 0;
  {$ENDIF}
  while IsCurrText('uses') do
    Parse_UsesClause(false);

  Gen(OP_END_IMPORT, 0, 0, 0);
  B1 := CodeCard;

  if namespace_id > 0 then
  begin
    BeginNamespace(namespace_id, false);
  end;

  while IsCurrText('namespace') do
    Parse_NamespaceDeclaration;

// parse block

  DECLARE_SWITCH := true;
  Parse_DeclarationPart;

  Gen(OP_END_INTERFACE_SECTION, CurrModule.ModuleNumber, 0, 0);

  Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);

{$IFDEF HTML}
  Inc(EXECUTABLE_SWITCH);
  DECLARE_SWITCH := false;
  Parse_StmtList;
  Dec(EXECUTABLE_SWITCH);
{$ELSE}
  Parse_CompoundStmt;
{$ENDIF}

  B2 := CodeCard;

  Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

  GenDestroyGlobalDynamicVariables(B1, B2);

  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);
  Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

  if IsCurrText('.') then
  begin
    // ok
  end
  else
  begin
{$IFDEF HTML}
    // ok
    CALL_SCANNER;
{$ELSE}
    MatchFinal('.');
{$ENDIF}
  end;

  if namespace_id > 0 then
    EndNamespace(namespace_id, false);
end;


procedure TPascalParser.ParseExternalSub(SubId: Integer);
var
  SubNameId, LibId, L, I: Integer;
  S: String;
  b: Boolean;
label
  LabName;
begin
  ReplaceForwardDeclaration(SubId);

  SetExternal(SubId, true);

  S := GetName(SubId);
  L := GetLevel(SubId);
  if L > 0 then
    if GetKind(L) = KindTYPE then
    begin
      b := false;
      for I := 0 to OuterList.Count - 1 do
        if OuterList.Values[I] = L then
        begin
          S := ExtractFullName(OuterList.Keys[I]) + '.' + GetName(L) + '.' + S;
          b := true;
          break;
        end;

      if not b then
        S := GetName(L) + '.' + S;
    end;

  SubNameId := NewConst(typeSTRING, S);

  ReadToken; // skip external

  EndSub(SubId);

  if ImportOnly then
  begin
    if IsCurrText(';') then
    begin
      Call_SCANNER;
      Exit;
    end;
  end;

  if CurrToken.TokenClass = tcPCharConst then
  begin
    S := RemoveCh('''', CurrToken.Text);
    LibId := NewConst(typeSTRING, S);
  end
  else
  begin
    LibId := Lookup(CurrToken.Text, CurrLevel);
    if (LibId = 0) or (not IsStringConst(LibId)) then
      LibId := Lookup(S, CurrLevel);
    if not ImportOnly then
    begin
      if LibId = 0 then
        RaiseError(errUndeclaredIdentifier, [S]);
      if not IsStringConst(LibId) then
        RaiseError(errIncompatibleTypesNoArgs, []);
    end;
  end;

  if ImportOnly then
  if IsCurrText('name') then
    goto LabName;


  ReadToken;
  RemoveSub;

  if IsCurrText(';') then
  begin
    Gen(OP_LOAD_PROC, SubId, SubNameId, LibId);
    Match(';');
  end
  else
  begin
    if IsCurrText('delayed') then
    begin
      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;
      Match(';');
      Exit;
    end;

LabName:

    Match('name');
    SubNameId := CurrToken.Id;
    Gen(OP_LOAD_PROC, SubId, SubNameId, LibId);
    if ImportOnly then
      Parse_ConstantExpression
    else
      Call_SCANNER;

    if IsCurrText('delayed') then
    begin
      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;
    end;

    Match(';');
  end;
end;

procedure TPascalParser.GenExternalSub(SubId: Integer);
var
  SubNameId, LibId, namespace_id: Integer;
  S, TypeName: String;
begin
  namespace_id := GetLevel(SubId);
  if GetKind(namespace_id) = kindTYPE then
    TypeName := GetName(namespace_id)
  else
    TypeName := '';

  while GetKind(namespace_id) <> kindNAMESPACE do
    namespace_id := GetLevel(namespace_id);

  SetForward(SubId, false);

  SetExternal(SubId, true);

  ReplaceForwardDeclaration(SubId, true);
  S := GetName(SubId);
  if TypeName <> '' then
    S := TypeName + '.' + S;
  SubNameId := NewConst(typeSTRING, S);
  EndSub(SubId);
  RemoveSub;

  LibId := NewConst(typeSTRING,
    GetName(namespace_id) + '.' + PCU_FILE_EXT);

  Gen(OP_LOAD_PROC, SubId, SubNameId, LibId);
end;

procedure TPascalParser.Parse_Unit(IsExternalUnit: Boolean = false);
var
  B1, B2: Integer;

  procedure Parse_InterfaceSection;

    procedure Parse_ProcedureHeading;
    var
      SubId: Integer;
      DirectiveList: TIntegerList;
      Declaration: String;
      SavedPosition: Integer;
    begin
      SavedPosition := CurrToken.Position;
      DECLARE_SWITCH := true;
      Match('procedure');
      SubId := Parse_Ident;
      BeginSub(SubId);
      if Assigned(OnParseBeginSubDeclaration) then
        OnParseBeginSubDeclaration(Owner, GetName(SubId), SubId);
      try
        Parse_FormalParameterList(SubId);
        SetName(CurrResultId, '');
        SetKind(CurrResultId, KindNONE);
        SetType(SubId, TypeVOID);
        SetType(CurrResultId, TypeVOID);

        if InterfaceOnly then
        begin
          if IsCurrText(';') then
            Match(';');
        end
        else
          Match(';');

        DirectiveList := Parse_DirectiveList(SubId);
        FreeAndNil(DirectiveList);

        if IsCurrText('external') then
        begin
          ParseExternalSub(SubId);
          Exit;
        end;

        if IsExternalUnit then
        begin
          GenExternalSub(SubId);
          Exit;
        end;

        SetForward(SubId, true);
        EndSub(SubId);
      finally
        if Assigned(OnParseEndSubDeclaration) then
        begin
          Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
          OnParseEndSubDeclaration(Owner, GetName(SubId), SubId, Declaration);
        end;
      end;
    end;

    procedure Parse_FunctionHeading;
    var
      SubId, TypeId: Integer;
      DirectiveList: TIntegerList;
      Declaration, StrType: String;
      SavedPosition: Integer;
    begin
      SavedPosition := CurrToken.Position;
      DECLARE_SWITCH := true;
      Match('function');
      SubId := Parse_Ident;
      BeginSub(SubId);
      if Assigned(OnParseBeginSubDeclaration) then
        OnParseBeginSubDeclaration(Owner, GetName(SubId), SubId);
      try
        Parse_FormalParameterList(SubId);
        DECLARE_SWITCH := false;

        StrType := '';

        TypeId := 0;
        if ImportOnly then
        begin
          if IsCurrText(':') then
          begin
            Match(':');
            Parse_Attribute;
            DECLARE_SWITCH := true;
            TypeID := Parse_Type;
            StrType := GetName(TypeId);
            Gen(OP_ASSIGN_TYPE, SubId, TypeID, 0);
            Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
          end;
        end
        else
        begin
          Match(':');
          Parse_Attribute;
          DECLARE_SWITCH := true;
          TypeID := Parse_Type;
          StrType := GetName(TypeId);
          Gen(OP_ASSIGN_TYPE, SubId, TypeID, 0);
          Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
        end;

        if Assigned(OnParseResultType) then
          OnParseResultType(Owner, StrType, TypeId);

        DECLARE_SWITCH := true;

        if InterfaceOnly then
        begin
          if IsCurrText(';') then
            Match(';');
        end
        else
          Match(';');

        DirectiveList := Parse_DirectiveList(SubId);
        FreeAndNil(DirectiveList);

        if IsCurrText('external') then
        begin
          ParseExternalSub(SubId);
          Exit;
        end;

        if IsExternalUnit then
        begin
          GenExternalSub(SubId);
          Exit;
        end;

        SetForward(SubId, true);
        EndSub(SubId);
      finally
        Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
        if Assigned(OnParseEndSubDeclaration) then
          OnParseEndSubDeclaration(Owner, GetName(SubId), SubId, Declaration);
      end;
    end;

  var
    ok: Boolean;
  begin
    Match('interface');
    while IsCurrText('uses') do
      Parse_UsesClause(false);
    if Assigned(OnParseEndUsedUnitList) then
      OnParseEndUsedUnitList(Owner);

    Gen(OP_END_IMPORT, 0, 0, 0);

    B1 := CodeCard;

    repeat
      ok := false;
      if IsCurrText('var') then
      begin
        Parse_VariableDeclaration;
        ok := true;
      end
      else if IsCurrText('threadvar') then
      begin
        Parse_VariableDeclaration;
        ok := true;
      end
      else if IsCurrText('const') then
      begin
        Parse_ConstantDeclaration;
        ok := true;
      end
      else if IsCurrText('resourcestring') then
      begin
        Parse_ResourcestringDeclaration;
        ok := true;
      end
      else if IsCurrText('procedure') then
      begin
        Parse_ProcedureHeading;
        ok := true;
      end
      else if IsCurrText('function') then
      begin
        Parse_FunctionHeading;
        ok := true;
      end
      else if IsCurrText('type') then
      begin
        Parse_TypeDeclaration(IsExternalUnit);
        ok := true;
      end
    until not ok;
  end; // interface section

  procedure Parse_ImplementationSection;
  var
    I, InnerTypeId, OuterTypeId: Integer;
    S, OldFullName: String;
    R: TPaxClassFactoryRec;
  begin
    for I := 0 to OuterList.Count - 1 do
    begin
      S := OuterList.Keys[I];
      InnerTypeId := OuterList.Values[I];
      OuterTypeId := TKernel(kernel).SymbolTable.LookupFullName(S, true);
      if OuterTypeId = 0 then
        RaiseError(errUndeclaredIdentifier, [S]);
      OldFullName := GetFullName(InnerTypeId);
      R := TKernel(kernel).ClassFactory.FindRecordByFullName(OldFullName);
      if R = nil then
        RaiseError(errInternalError, [S]);
      R.FullClassName := S + '.' + GetName(InnerTypeId);

      SetLevel(InnerTypeId, OuterTypeId);
    end;

    IMPLEMENTATION_SECTION := true;
    Match('implementation');
    while IsCurrText('uses') do
      Parse_UsesClause(true);
    Parse_DeclarationPart(true);
  end;

  procedure Parse_InitSection;
  begin
    DECLARE_SWITCH := false;
    if IsCurrText('initialization') then
    begin
      BeginInitialization;

      Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      Call_SCANNER;
      Parse_StmtList;

      Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

      EndInitialization;
      if IsCurrText('finalization') then
      begin
        BeginFinalization;

        Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);

        Call_SCANNER;
        Parse_StmtList;

        B2 := CodeCard;

        Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

        GenDestroyGlobalDynamicVariables(B1, B2);

        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

        EndFinalization;
      end
      else
      begin
        BeginFinalization;

        Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);

        B2 := CodeCard;

        Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

        GenDestroyGlobalDynamicVariables(B1, B2);

        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_NOP, 0, 0, 0);
        Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

        EndFinalization;
      end;

      Match('end');
    end
    else if IsCurrText('begin') then
    begin
      BeginInitialization;

      Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      Call_SCANNER;
      Parse_StmtList;

      Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

      EndInitialization;
      Match('end');

      BeginFinalization;

      Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      B2 := CodeCard;

      Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

      GenDestroyGlobalDynamicVariables(B1, B2);

      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

      EndFinalization;

    end
    else if IsCurrText('finalization') then
    begin
      BeginFinalization;

      Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      Call_SCANNER;
      Parse_StmtList;

      B2 := CodeCard;

      Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

      GenDestroyGlobalDynamicVariables(B1, B2);

      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

      EndFinalization;
      Match('end');
    end
    else if IsCurrText('end') then
    begin
      BeginFinalization;

      Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);

      Call_SCANNER;

      B2 := CodeCard;

      Gen(OP_EPILOGUE_GLOBAL_BLOCK, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_EPILOGUE_GLOBAL_BLOCK2, 0, 0, 0);

      GenDestroyGlobalDynamicVariables(B1, B2);

      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_NOP, 0, 0, 0);
      Gen(OP_END_GLOBAL_BLOCK, 0, 0, 0);

      EndFinalization;
    end
    else
      Match('end');
  end;

var
  namespace_name: String;
  namespace_id: Integer;
begin
  DECLARE_SWITCH := true;
  Match('unit');
  namespace_id := Parse_UnitName(namespace_name);
  if Assigned(OnParseUnitName) then
    OnParseUnitName(Owner, namespace_name, namespace_id);

  if CurrModule.IsExtra then
    SaveExtraNamespace(namespace_id);

  Parse_PortabilityDirective;
  Match(';');
  Parse_InterfaceSection;

  if IsExternalUnit then
  begin
    Match('implementation');
    Exit;
  end;

  Gen(OP_END_INTERFACE_SECTION, CurrModule.ModuleNumber, 0, 0);

  if InterfaceOnly then
  begin
    if Assigned(OnParseImplementationSection) then
      OnParseImplementationSection(Owner);
    if ImportOnly then
       Exit;
    Match('implementation');
    while not scanner.IsEOF do
      ReadToken;
    EndNamespace(namespace_id);
  end
  else
  begin
    Parse_ImplementationSection;
    Inc(EXECUTABLE_SWITCH);
    Parse_InitSection;
    Dec(EXECUTABLE_SWITCH);
    EndNamespace(namespace_id);
    MatchFinal('.');
  end;
end;

procedure TPascalParser.Parse_VariableDeclaration(vis: TClassVisibility = cvNone);
var
  L: TIntegerList;
  I, ID, TypeID, ExprID: Integer;
  S, Declaration: String;
  VarNameId, LibId: Integer;
begin
  L := TIntegerList.Create;
  try
    if InterfaceOnly then
      Gen(OP_BEGIN_VAR, 0, 0, 0);

    DECLARE_SWITCH := true;
    if IsCurrText('threadvar') then
      Call_SCANNER
    else
      Match('var');

    repeat
      Parse_Attribute;

      L.Clear;
      repeat
        ID := Parse_Ident;
        SetVisibility(ID, vis);
        Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, ID, 0);
        L.Add(ID);
        if NotMatch(',') then break;
      until false;
      DECLARE_SWITCH := false;

      if EXPLICIT_OFF then
      begin
        TypeId := 0;
        if IsCurrText(';') then
        begin
          //ok
        end
        else
        begin
          Match(':');
          TypeID := Parse_Type;
        end;
      end
      else
      begin
        Match(':');
        TypeID := Parse_Type;
      end;

      for I:=0 to L.Count - 1 do
        Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);

      S := '';
      if IsCurrText('absolute') then
      begin
        RemoveLastIdent(CurrToken.Id);

        DECLARE_SWITCH := false;

        Call_SCANNER;
        ExprId := Parse_Ident;
        for I:=0 to L.Count - 1 do
          Gen(OP_ABSOLUTE, L[I], ExprID, 0);
      end
      else if IsCurrText('=') then
      begin
        if GetKind(CurrLevel) = KindSUB then
          CreateError(errCannotInitializeLocalVariables, []);
        DECLARE_SWITCH := false;

        Match('=');

        Id := L[0];
        BeginInitConst(Id);

        ExprID := Parse_VariableInitialization;
        S := ExtractText(PrevPosition, PrevPosition + PrevLength - 1);
        for I:=0 to L.Count - 1 do
          Gen(OP_ASSIGN, L[I], ExprID, L[I]);

        EndInitConst(Id);
      end;

      Parse_PortabilityDirective;

      if Assigned(OnParseVariableDeclaration) then
        for I:=0 to L.Count - 1 do
        begin
          if S = '' then
            Declaration := GetName(L[I]) + ':' + GetName(TypeID) + ';'
          else
            Declaration := GetName(L[I]) + ':' + GetName(TypeID) + '=' + ';';

          OnParseVariableDeclaration(Owner, GetName(L[I]), L[I], GetName(TypeID),
            Declaration);
        end;

      DECLARE_SWITCH := true;
      if not MatchEx(';') then
        break;

      if IsCurrText('external') then
      begin
        SetExternal(Id, true);

        S := GetName(Id);
        VarNameId := NewConst(typeSTRING, S);
        ReadToken; // skip external
        if CurrToken.TokenClass = tcPCharConst then
        begin
          S := RemoveCh('''', CurrToken.Text);
          LibId := NewConst(typeSTRING, S);
        end
        else
        begin
          LibId := Lookup(S, CurrLevel);

          if LibId = 0 then
            RaiseError(errUndeclaredIdentifier, [S]);

          if not IsStringConst(LibId) then
            RaiseError(errIncompatibleTypesNoArgs, []);
        end;

        ReadToken;
        Gen(OP_LOAD_PROC, Id, VarNameId, LibId);
        Match(';');
        Exit;
      end;


    until CurrToken.TokenClass <> tcIdentifier;

  finally
    if InterfaceOnly then
      Gen(OP_END_VAR, 0, 0, 0);

    FreeAndNil(L);
  end;
end;

procedure TPascalParser.Parse_ConstantDeclaration(vis: TClassVisibility = cvNone);
var
  ID: Integer;
  S: String;
  VarNameId, LibId, ConstId, TypeId: Integer;
  Declaration: String;
  SavedPosition: Integer;
  IsBuildingPCU: Boolean;
begin
  IsBuildingPCU := BuildingAll;

  Gen(OP_EMIT_OFF, 0, 0, 0);
  try

    DECLARE_SWITCH := true;
    Match('const');

    repeat
      Parse_Attribute;

      SavedPosition := CurrToken.Position;
      ID := Parse_Ident;

      SetVisibility(Id, vis);

      if InterfaceOnly then
        Gen(OP_BEGIN_CONST, Id, 0, 0);

      Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, ID, 0);

      SetKind(ID, kindCONST);
      DECLARE_SWITCH := false;
      if IsCurrText(':') then
      begin
        Match(':');
        TypeId := Parse_Type;
        Gen(OP_ASSIGN_TYPE, ID, TypeId, 0);

        if not ImportOnly then
          levelStack.Push(CurrNamespaceId);
        Match('=');
        Parse_ConstantInitialization(ID);
        if Assigned(OnParseTypedConstantDeclaration) then
        begin
          Declaration := ExtractText(SavedPosition,
              CurrToken.Position + CurrToken.Length - 1);
          OnParseTypedConstantDeclaration(Owner,
            GetName(ID), ID, GetName(TypeId), GetValue(ID), Declaration);
        end;
        if not ImportOnly then
          levelStack.Pop;
      end
      else
      begin
        if not ImportOnly then
          levelStack.Push(CurrNamespaceId);
        Match('=');
        if IsBuildingPCU or IsCurrText('[') then
        begin
          Parse_ConstantInitialization(ID);
          ConstId := Id;
        end
        else
        begin
          ConstId := Parse_ConstantExpression;
          if TKernel(kernel).SignCodeCompletion then
            SetType(ID, GetSymbolRec(ConstId).TypeID);

          Gen(OP_ASSIGN_CONST, ID, ConstId, ID);
        end;
        if Assigned(OnParseConstantDeclaration) then
        begin
          Declaration := ExtractText(SavedPosition,
            CurrToken.Position + CurrToken.Length - 1);
          OnParseConstantDeclaration(Owner, GetName(ID), ID, GetValue(ConstID), Declaration);
        end;
        if not ImportOnly then
          levelStack.Pop;
      end;

      DECLARE_SWITCH := true;

      if InterfaceOnly then
        Gen(OP_END_CONST, Id, 0, 0);

      Parse_PortabilityDirective;

      if not MatchEx(';') then
        break;

      if IsCurrText('external') then
      begin
        S := GetName(Id);
        VarNameId := NewConst(typeSTRING, S);
        ReadToken; // skip external
        if CurrToken.TokenClass = tcPCharConst then
        begin
          S := RemoveCh('''', CurrToken.Text);
          LibId := NewConst(typeSTRING, S);
        end
        else
        begin
          LibId := Lookup(S, CurrLevel);

          if LibId = 0 then
            RaiseError(errUndeclaredIdentifier, [S]);

          if not IsStringConst(LibId) then
            RaiseError(errIncompatibleTypesNoArgs, []);
        end;

        ReadToken;
        Gen(OP_LOAD_PROC, Id, VarNameId, LibId);
        Match(';');
        Exit;
      end;

    until CurrToken.TokenClass <> tcIdentifier;

  finally
    Gen(OP_EMIT_ON, 0, 0, 0);
  end;
end;

procedure TPascalParser.Parse_ResourcestringDeclaration;
var
  ID: Integer;
  Value: Variant;
  Declaration: String;
  IsBuildingPCU: Boolean;
begin
  IsBuildingPCU := BuildingAll;

  Gen(OP_EMIT_OFF, 0, 0, 0);
  try

    DECLARE_SWITCH := true;
    Match('resourcestring');

    repeat

      ID := Parse_Ident;
      Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, ID, 0);

      SetKind(ID, kindCONST);
      DECLARE_SWITCH := false;
      Match('=');
      if IsBuildingPCU then
        Parse_ConstantInitialization(ID)
      else
        Gen(OP_ASSIGN_CONST, ID, Parse_ConstantExpression, ID);

      if Assigned(OnParseConstantDeclaration) then
      begin
        Value := GetValue(Id);
        if not VariantIsString(Value) then
          Value := String(Chr(Integer(Value)));
        Declaration := GetName(Id) + ' = ' + Value;
        OnParseResourceStringDeclaration(Owner, GetName(ID), ID, Value, Declaration);
      end;

      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      if not MatchEx(';') then
        break;

    until CurrToken.TokenClass <> tcIdentifier;

  finally
    Gen(OP_EMIT_ON, 0, 0, 0);
  end;
end;

procedure TPascalParser.Parse_ConstantInitialization(ID: Integer);
var
  ExprId, ItemId, NameId, K, ConstId: Integer;
begin
  BeginInitConst(Id);

  if IsCurrText('(') then
  begin
    K := -1;
    Call_SCANNER;
    repeat
      if IsCurrText(')') then
        break;

      Inc(K);
      if IsCurrText('(') then
      begin
        ExprId := NewTempVar();
        SetLevel(ExprId, 0);
        Parse_ConstantInitialization(ExprId);
        Gen(OP_ASSIGN_SHIFT, 0, K, ExprId);
        if NotMatch(',') then
          break;
      end
      else
      begin
        ItemId := NewTempVar();
        SetLevel(ItemId, 0);

        if IsNextText(':') then
        begin
          SetName(CurrToken.Id, '');
          SetKind(CurrToken.Id, KindNONE);
          NameId := NewConst(typeSTRING,  CurrToken.Text);
          SetKind(NameId, kindNONE);

          Call_SCANNER;
          Match(':');
          if IsCurrText('(') then
          begin
            ExprId := NewTempVar();
            SetLevel(ExprId, 0);

            Parse_ConstantInitialization(ExprId);
            Gen(OP_ASSIGN_SHIFT, 0, K, ExprId);
          end
          else
          begin
            ExprId := Parse_ConstantExpression;
            Gen(OP_RECORD_ITEM, ID, NameId, ItemId);
            Gen(OP_ASSIGN, ItemId, ExprId, ItemId);
          end;
          if NotMatch(';') then
            break;
        end
        else
        begin
          ExprId := Parse_ConstantExpression;
          Gen(OP_ITEM, ID, K, ItemId);
          Gen(OP_ASSIGN, ItemId, ExprId, ItemId);
          if NotMatch(',') then
            break;
        end;
      end;
    until false;
    Match(')');
  end
  else
  begin
    ConstId := Parse_ConstantExpression;
    Gen(OP_ASSIGN, ID, ConstId, ID);
    SetValue(Id, GetValue(ConstId));
  end;

  EndInitConst(Id);
end;

function TPascalParser.Parse_VariableInitialization: Integer;
var
  ExprId, ItemId, NameId, K: Integer;
begin
  if IsCurrText('(') then
  begin
    result := NewTempVar;
    K := -1;
    Call_SCANNER;
    repeat
      if IsCurrText(')') then
        break;

      Inc(K);
      if IsCurrText('(') then
      begin
        ExprId := NewTempVar();
        SetLevel(ExprId, 0);
        Gen(OP_ASSIGN, ExprId, Parse_VariableInitialization, ExprId);
        Gen(OP_ASSIGN_SHIFT, 0, K, ExprId);
        if NotMatch(',') then
          break;
      end
      else
      begin
        ItemId := NewTempVar();
        SetLevel(ItemId, 0);

        if IsNextText(':') then
        begin
          SetName(CurrToken.Id, '');
          SetKind(CurrToken.Id, KindNONE);
          NameId := NewConst(typeSTRING,  CurrToken.Text);
          SetKind(NameId, kindNONE);

          Call_SCANNER;
          Match(':');
          if IsCurrText('(') then
          begin
            ExprId := NewTempVar();
            SetLevel(ExprId, 0);

            Gen(OP_ASSIGN, ExprId, Parse_VariableInitialization, ExprId);
            Gen(OP_ASSIGN_SHIFT, 0, K, ExprId);
          end
          else
          begin
            ExprId := Parse_Expression;
            Gen(OP_RECORD_ITEM, result, NameId, ItemId);
            Gen(OP_ASSIGN, ItemId, ExprId, ItemId);
          end;
          if NotMatch(';') then
            break;
        end
        else
        begin
          ExprId := Parse_Expression;
          Gen(OP_ITEM, result, K, ItemId);
          Gen(OP_ASSIGN, ItemId, ExprId, ItemId);
          if NotMatch(',') then
            break;
        end;
      end;
    until false;
    Match(')');
  end
  else
    result := Parse_Expression;
end;

procedure TPascalParser.Parse_LabelDeclaration;
begin
  DECLARE_SWITCH := true;
  Match('label');
  repeat
    Parse_Label;
    if NotMatch(',') then break;
  until false;
  Match(';');
end;

procedure TPascalParser.Parse_TypeDeclaration(IsExternalUnit: Boolean = false;
                                              vis: TClassVisibility = cvPublic);
var
  ok: Boolean;
  TypeID, T, SubId, TypeBaseId: Integer;
  IsPacked: Boolean;
  S, Q: String;
begin
  TypeParams.Clear;
  DECLARE_SWITCH := true;
  Match('type');
  repeat
    Parse_Attribute;

    BeginTypeDef(CurrToken.Id);
    TypeId := Parse_Ident;
    SetVisibility(TypeId, vis);
    SetKind(TypeId, KindTYPE);
    SetLevel(TypeId, CurrLevel);

    if Assigned(OnParseTypeDeclaration) then
      OnParseTypeDeclaration(Owner, GetName(TypeId), TypeId);

    S := GetFullName(TypeId);
    while IsCurrText('.') do
    begin
      SetKind(TypeId, KindNONE);
      SetName(TypeId, '');

      Call_SCANNER;
      TypeId := Parse_Ident;

      S := S + '.' + GetName(TypeId);
    end;

    if S <> GetFullName(TypeId) then
    begin
      S := ExtractFullOwner(S);
      OuterList.AddValue(S, TypeId);
    end;

    SetKind(TypeID, KindTYPE);
    if InterfaceOnly then
      Gen(OP_BEGIN_TYPE, TypeId, 0, 0);

    DECLARE_SWITCH := false;
    Match('=');
    ok := false;

    if IsCurrText('packed') then
    begin
      Match('packed');
      IsPacked := true;
    end
    else
      IsPacked := false;

    if IsCurrText('array') then
    begin
      IsPacked := true;
      Parse_ArrayTypeDeclaration(TypeID, IsPacked);
      Parse_PortabilityDirective;

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('record') then
    begin
      DECLARE_SWITCH := true;
      Parse_RecordTypeDeclaration(TypeID, IsPacked);
      Parse_PortabilityDirective;

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('class') then
    begin
      IsPacked := true;
      DECLARE_SWITCH := true;
      Parse_ClassTypeDeclaration(TypeID, IsPacked, IsExternalUnit);
      Parse_PortabilityDirective;

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('interface') then
    begin
      DECLARE_SWITCH := true;
      Parse_InterfaceTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('dispinterface') then
    begin
      if not ImportOnly then
        RaiseNotImpl;

      DECLARE_SWITCH := true;
      Parse_InterfaceTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('reference') then
    begin
      RemoveLastIdent(CurrToken.Id);
      DECLARE_SWITCH := true;
      Call_SCANNER;
      Match('to');
      Parse_MethodRefTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := true;
    end
    else if IsCurrText('^') then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      Parse_PointerTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('(') then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      DECLARE_SWITCH := true;
      Parse_EnumTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if IsCurrText('procedure') or IsCurrText('function') then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);

      DECLARE_SWITCH := true;
      Parse_ProceduralTypeDeclaration(TypeID, SubId);

      EndTypeDef(TypeId);

      DECLARE_SWITCH := true;
      if InterfaceOnly then
      begin
        if IsCurrText(';') then
          ok := MatchEx(';');
      end
      else
        ok := MatchEx(';');
    end
    else if IsCurrText('set') then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      DECLARE_SWITCH := true;
      Parse_SetTypeDeclaration(TypeID);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if CurrToken.TokenClass = tcIntegerConst then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      Parse_SubrangeTypeDeclaration(TypeID, typeINTEGER, Q, 0);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if CurrToken.TokenClass = tcCharConst then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      Parse_SubrangeTypeDeclaration(TypeID, typeCHAR, Q, 0);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else if CurrToken.TokenClass = tcBooleanConst then
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);
      if TypeParams.Count > 0 then
        RaiseError(errTypeParameterNotAllowed, []);

      Parse_SubrangeTypeDeclaration(TypeID, typeBOOLEAN, Q, 0);
      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      ok := MatchEx(';');
    end
    else
    begin
      if IsPacked then
        CreateError(errPACKEDNotAllowedHere, []);

      if IsCurrText('type') then
      begin
        Call_SCANNER;
        if IsCurrText('of') then
          Call_SCANNER;
      end;

      if IsCurrText('string') then
      begin
        Call_SCANNER;
        if IsCurrText('[') then
        begin
{$IFDEF PAXARM}
          Match(';');
{$ELSE}
          Parse_ShortStringTypeDeclaration(TypeID);
{$ENDIF}
        end
        else
        begin
          if InterfaceOnly then
            Gen(OP_BEGIN_ALIAS_TYPE, TypeId, 0, 0);
          SetType(TypeId, typeALIAS);
          Gen(OP_ASSIGN_TYPE_ALIAS, TypeId, typeSTRING, 0);
          if Assigned(OnParseAliasTypeDeclaration) then
            OnParseAliasTypeDeclaration(Owner, GetName(TypeId), TypeId, 'string',
              GetName(TypeId) + ' = string;');
          if InterfaceOnly then
            Gen(OP_END_ALIAS_TYPE, TypeId, 0, 0);
        end;
      end
      else
      begin
        case CurrToken.TokenClass of
          tcSpecial: typeBaseId := typeINTEGER;
          tcIntegerConst: typeBaseId := typeINTEGER;
          tcCharConst: typeBaseId := typeCHAR;
          tcBooleanConst: typeBaseId := typeBOOLEAN;
          tcIdentifier: typeBaseId := GetType(CurrToken.Id);
        else
          TypeBaseId := typeINTEGER;
        end;

        T := Parse_Expression;

        if IsCurrText('..') then
          Parse_SubrangeTypeDeclaration(TypeID, TypeBaseId, Q, T)
        else
        begin
          if InterfaceOnly then
            Gen(OP_BEGIN_ALIAS_TYPE, TypeId, 0, 0);
          SetType(TypeId, typeALIAS);
          Gen(OP_ASSIGN_TYPE_ALIAS, TypeId, T, 0);
          if Assigned(OnParseAliasTypeDeclaration) then
            OnParseAliasTypeDeclaration(Owner, GetName(TypeId), TypeId, GetName(T),
              GetName(TypeId) + ' = ' + GetName(T) + ';');
          if InterfaceOnly then
            Gen(OP_END_ALIAS_TYPE, TypeId, 0, 0);
        end;
      end;

      Parse_PortabilityDirective;

      DECLARE_SWITCH := true;
      TypeParams.Clear;
      ok := MatchEx(';');
    end;

    Gen(OP_ADD_TYPEINFO, TypeId, 0, 0);

    if InterfaceOnly then
      Gen(OP_END_TYPE, TypeId, 0, 0);

    if CurrToken.TokenClass = tcKeyword then
      Break;
  until not ok;
end;

function TPascalParser.Parse_OrdinalType(var Declaration: String): Integer;
var
  T: Integer;
begin
  Declaration := '';
  if IsCurrText('(') then
  begin
    result := NewTempVar;
    Parse_EnumTypeDeclaration(result);
  end
  else if (CurrToken.TokenClass = tcIntegerConst) or IsCurrText('-') then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeINTEGER, Declaration, 0);
  end
  else if CurrToken.TokenClass = tcCharConst then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeCHAR, Declaration, 0);
  end
  else if CurrToken.TokenClass = tcBooleanConst then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeBOOLEAN, Declaration, 0);
  end
  else if IsCurrText('ord') and IsNextText('(') then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeBYTE, Declaration, 0);
  end
  else if IsCurrText('chr') and IsNextText('(') then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeCHAR, Declaration, 0);
  end
  else if IsCurrText('low') and IsNextText('(') then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeBYTE, Declaration, 0);
  end
  else if IsCurrText('high') and IsNextText('(') then
  begin
    result := NewTempVar;
    Parse_SubrangeTypeDeclaration(result, typeBYTE, Declaration, 0);
  end
  else
  begin
    T := Parse_QualId;

    if IsCurrText('..') then
    begin
      result := NewTempVar;
      Parse_SubrangeTypeDeclaration(result, typeENUM, Declaration, T);
    end
    else
    begin
      result := T;
      Declaration := GetName(T);
//      AddTypeExpRec(result);
    end;
  end;
end;

function TPascalParser.Parse_OpenArrayType(var ElemTypeName: String): Integer;
begin
  DECLARE_SWITCH := true;
  Match('array');
  DECLARE_SWITCH := false;
  Match('of');
  ElemTypeName := CurrToken.Text;
  if IsCurrText('const') then
  begin
    Call_SCANNER;
    result := H_Dynarray_TVarRec;
  end
  else if IsCurrText('Integer') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Integer;
  end
{$IFDEF UNIC}
  else if IsCurrText('String') then
  begin
    ElemTypeName := 'UnicodeString';
    Call_SCANNER;
    result := H_Dynarray_UnicodeString;
  end
  else if IsCurrText('Char') then
  begin
    ElemTypeName := 'WideChar';
    Call_SCANNER;
    result := H_Dynarray_AnsiChar;
  end
{$ELSE}
  else if IsCurrText('String') then
  begin
    ElemTypeName := 'AnsiString';
    Call_SCANNER;
    result := H_Dynarray_AnsiString;
  end
  else if IsCurrText('Char') then
  begin
    ElemTypeName := 'AnsiChar';
    Call_SCANNER;
    result := H_Dynarray_WideChar;
  end
{$ENDIF}
  else if IsCurrText('Byte') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Byte;
  end
  else if IsCurrText('Word') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Word;
  end
  else if IsCurrText('ShortInt') then
  begin
    Call_SCANNER;
    result := H_Dynarray_ShortInt;
  end
  else if IsCurrText('SmallInt') then
  begin
    Call_SCANNER;
    result := H_Dynarray_SmallInt;
  end
  else if IsCurrText('Cardinal') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Cardinal;
  end
  else if IsCurrText('Int64') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Int64;
  end
  else if IsCurrText('UInt64') then
  begin
    Call_SCANNER;
    result := H_Dynarray_UInt64;
  end
  else if IsCurrText('AnsiChar') then
  begin
    Call_SCANNER;
    result := H_Dynarray_AnsiChar;
  end
  else if IsCurrText('WideChar') then
  begin
    Call_SCANNER;
    result := H_Dynarray_WideChar;
  end
  else if IsCurrText('AnsiString') then
  begin
    Call_SCANNER;
    result := H_Dynarray_AnsiString;
  end
  else if IsCurrText('WideString') then
  begin
    Call_SCANNER;
    result := H_Dynarray_WideString;
  end
  else if IsCurrText('UnicodeString') then
  begin
    Call_SCANNER;
    result := H_Dynarray_UnicodeString;
  end
  else if IsCurrText('ShortString') then
  begin
    Call_SCANNER;
    result := H_Dynarray_ShortString;
  end
  else if IsCurrText('Double') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Double;
  end
  else if IsCurrText('Single') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Single;
  end
  else if IsCurrText('Extended') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Extended;
  end
  else if IsCurrText('Currency') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Currency;
  end
  else if IsCurrText('Boolean') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Boolean;
  end
  else if IsCurrText('ByteBool') then
  begin
    Call_SCANNER;
    result := H_Dynarray_ByteBool;
  end
  else if IsCurrText('WordBool') then
  begin
    Call_SCANNER;
    result := H_Dynarray_WordBool;
  end
  else if IsCurrText('LongBool') then
  begin
    Call_SCANNER;
    result := H_Dynarray_LongBool;
  end
  else if IsCurrText('Variant') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Variant;
  end
  else if IsCurrText('OleVariant') then
  begin
    Call_SCANNER;
    result := H_Dynarray_OleVariant;
  end
  else if IsCurrText('Pointer') then
  begin
    Call_SCANNER;
    result := H_Dynarray_Pointer;
  end
  else
  begin
    result := NewTempVar;
    BeginOpenArrayType(result);
    Gen(OP_CREATE_DYNAMIC_ARRAY_TYPE, result, Parse_Ident, 0);
    EndOpenArrayType(result, ElemTypeName);
  end;
  DECLARE_SWITCH := false;
end;

function TPascalParser.Parse_Type: Integer;
var
  IsPacked: Boolean;
  SubId: Integer;
  S: String;
begin
  IsPacked := false;

  if IsCurrText('packed') then
  begin
    Match('packed');
    IsPacked := true;
  end
  else if IsCurrText('System') then
  begin
    Match('System');
    Match('.');
  end;

  if IsCurrText('array') then
  begin
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Parse_ArrayTypeDeclaration(result, IsPacked);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('record') then
  begin
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Parse_RecordTypeDeclaration(result, IsPacked);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('class') then
  begin
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Parse_ClassTypeDeclaration(result, IsPacked);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('interface') then
  begin
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Parse_InterfaceTypeDeclaration(result);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('dispinterface') then
  begin
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Parse_InterfaceTypeDeclaration(result);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('reference') then
  begin
    RemoveLastIdent(CurrToken.Id);
    result := NewTempVar;
    DECLARE_SWITCH := true;
    Call_SCANNER;
    Match('to');
    Parse_MethodRefTypeDeclaration(result);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('^') then
  begin
    result := NewTempVar;
    Parse_PointerTypeDeclaration(result);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('set') then
  begin
    result := NewTempVar;
    Parse_SetTypeDeclaration(result);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('procedure') or IsCurrText('function') then
  begin
    result := NewTempVar;
    Parse_ProceduralTypeDeclaration(result, SubId);
    DECLARE_SWITCH := false;
  end
  else if IsCurrText('string') then
  begin
//    result := Parse_Ident;
{$IFDEF PAXARM}
     result := typeUNICSTRING;
{$ELSE}
    if IsUNIC then
      result := typeUNICSTRING
    else
      result := typeANSISTRING;
{$ENDIF}

    Call_SCANNER;

    if IsCurrText('[') then
    begin
      result := NewTempVar;
{$IFDEF PAXARM}
      Match(';');
{$ELSE}
      Parse_ShortStringTypeDeclaration(result);
{$ENDIF}
      DECLARE_SWITCH := false;
    end;
  end
  else if IsCurrText('double') then
  begin
    result := Parse_Ident;
  end
  else
  begin
    result := Parse_OrdinalType(S);
  end;

  Gen(OP_ADD_TYPEINFO, result, 0, 0);
end;

procedure TPascalParser.Parse_ArrayTypeDeclaration(ArrayTypeID: Integer; IsPacked: Boolean);
var
  I, T, RangeTypeId, ElemTypeId: Integer;
  L: TIntegerList;
  RangeList: TStringList;
  Declaration, S: String;
begin
  L := TIntegerList.Create;
  RangeList := TStringList.Create;

  try
    Match('array');
    DECLARE_SWITCH := false;

    if IsCurrText('of') then // dynamic array
    begin
      Match('of');
      BeginDynamicArrayType(ArrayTypeID);
      if IsCurrText('const') then
      begin
        Call_SCANNER;
        Gen(OP_CREATE_DYNAMIC_ARRAY_TYPE, ArrayTypeId, H_TVarRec, 0);
        ElemTypeId := H_TVarRec;
      end
      else
      begin
        ElemTypeId := Parse_Type;
        Gen(OP_CREATE_DYNAMIC_ARRAY_TYPE, ArrayTypeId, ElemTypeId, 0);
      end;
      EndDynamicArrayType(ArrayTypeID);
      if Assigned(OnParseDynArrayTypeDeclaration) then
      begin
        Declaration := 'array of ' + ExtractText(PrevPosition, PrevPosition + PrevLength - 1);
        OnParseDynArrayTypeDeclaration(Owner, GetName(ArrayTypeId), ArrayTypeId,
          GetName(ElemTypeId), Declaration);
      end;
    end
    else // static array
    begin

      BeginArrayType(ArrayTypeID);
      if IsPacked then
        SetPacked(ArrayTypeID);
      L.Add(ArrayTypeId);

      Match('[');
      repeat
        T := NewTypeAlias;
        RangeTypeId := Parse_OrdinalType(S);
        Gen(OP_ASSIGN_TYPE_ALIAS, T, RangeTypeId, 0);
        RangeList.Add(S);

        if IsCurrText(',') then
        begin
          Match(',');

          ArrayTypeId := NewTempVar;
          BeginArrayType(ArrayTypeID);
          if IsPacked then
            SetPacked(ArrayTypeID);
          L.Add(ArrayTypeId);
        end
        else
          break;
      until false;

      Match(']');
      Match('of');

      T := NewTypeAlias;
      ElemTypeId := Parse_Type;
      Gen(OP_ASSIGN_TYPE_ALIAS, T, ElemTypeId, 0);

      DECLARE_SWITCH := true;

      for I:=0 to L.Count - 1 do
      begin
        EndArrayType(L[I]);
      end;

      if Assigned(OnParseArrayTypeDeclaration) then
      begin
        S := GetName(ElemTypeId);
        OnParseArrayTypeDeclaration(Owner, GetName(L[0]), L[0],
          RangeList,
          S);
      end;
    end;
  finally
    FreeAndNil(L);
    FreeAndNil(RangeList);
  end;
end;

function TPascalParser.Parse_RecordConstructorHeading(IsSharedMethod: Boolean;
                                                      RecordTypeId: Integer;
                                                      Vis: TClassVisibility;
                                                      IsExternalUnit: Boolean): Integer;
var
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('constructor');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);
  SetVisibility(result, vis);
  BeginStructureConstructor(result, RecordTypeId);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);
  Match(';');

  DirectiveList := Parse_DirectiveList(result);
  if (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
     (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
     (DirectiveList.IndexOf(dirOVERRIDE) >= 0) then
  begin
    CreateError(errE2379, []);
    // Virtual methods not allowed in record types.
  end;

  FreeAndNil(DirectiveList);
  SetForward(result, true);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_RecordDestructorHeading(IsSharedMethod: Boolean;
                                                     RecordTypeId: Integer;
                                                     Vis: TClassVisibility;
                                                     IsExternalUnit: Boolean): Integer;
var
  NP: Integer;
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('destructor');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);
  SetVisibility(result, vis);
  BeginStructureDestructor(result, RecordTypeId);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  NP := 0;
  if IsCurrText('(') then
  begin
    Call_SCANNER;
    Match(')');
  end;
  SetCount(result, NP);
  Match(';');

  DirectiveList := Parse_DirectiveList(result);
  if (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
          (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
          (DirectiveList.IndexOf(dirOVERRIDE) >= 0) then
  begin
    CreateError(errE2379, []);
    // Virtual methods not allowed in record types.
  end;
  FreeAndNil(DirectiveList);
  SetForward(result, true);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_RecordProcedureHeading(IsSharedMethod: Boolean;
                                                    RecordTypeId: Integer;
                                                    Vis: TClassVisibility;
                                                    IsExternalUnit: Boolean): Integer;
var
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('procedure');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  if IsCurrText('.') then
  begin
    Scanner.CurrComment.AllowedDoComment := false;
    Match('.');
    Parse_Ident;
    DECLARE_SWITCH := false;
    Match('=');
    Parse_Ident;
    Match(';');
    Exit;
  end;

  SetVisibility(result, vis);
  BeginStructureMethod(result, RecordTypeId, false, IsSharedMethod);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);
  Match(';');

  DirectiveList := Parse_DirectiveList(result);

  if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
  begin
    inherited InitSub(result);
    Gen(OP_ERR_ABSTRACT, NewConst(typeSTRING,
                         GetName(RecordTypeId) + '.' + GetName(result)), 0, result);
  end
  else if (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
          (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
          (DirectiveList.IndexOf(dirOVERRIDE) >= 0) then
  begin
    CreateError(errE2379, []);
    // Virtual methods not allowed in record types.
  end
  else if IsSharedMethod and (DirectiveList.IndexOf(dirSTATIC) = -1) then
  begin
    CreateError(errE2398, []);
    // Class methods in record types must be static.
  end
  else
    SetForward(result, true);
  FreeAndNil(DirectiveList);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_RecordFunctionHeading(IsSharedMethod: Boolean;
                                                   RecordTypeId: Integer;
                                                   Vis: TClassVisibility;
                                                   IsExternalUnit: Boolean): Integer;
var
  TypeID: Integer;
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('function');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  if IsCurrText('.') then
  begin
    Scanner.CurrComment.AllowedDoComment := false;
    Match('.');
    Parse_Ident;
    DECLARE_SWITCH := false;
    Match('=');
    Parse_Ident;
    Match(';');
    Exit;
  end;

  SetVisibility(result, vis);
  BeginStructureMethod(result, RecordTypeId, true, IsSharedMethod);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);

  DECLARE_SWITCH := false;
  Match(':');
  Parse_Attribute;
  TypeID := Parse_Type;
  Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
  Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
  if Assigned(OnParseResultType) then
    OnParseResultType(Owner, GetName(TypeId), TypeId);

  DECLARE_SWITCH := true;
  Match(';');

  DirectiveList := Parse_DirectiveList(result);
  if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
  begin
    inherited InitSub(result);
    Gen(OP_ERR_ABSTRACT, NewConst(typeSTRING,
                         GetName(RecordTypeId) + '.' + GetName(result)), 0, result);
  end
  else if (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
          (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
          (DirectiveList.IndexOf(dirOVERRIDE) >= 0) then
  begin
    CreateError(errE2379, []);
    // Virtual methods not allowed in record types.
  end
  else if IsSharedMethod and (DirectiveList.IndexOf(dirSTATIC) = -1) then
  begin
    CreateError(errE2398, []);
    // Class methods in record types must be static.
  end
  else
    SetForward(result, true);

  FreeAndNil(DirectiveList);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_RecordOperatorHeading(RecordTypeId: Integer;
                                                   Vis: TClassVisibility;
                                                   IsExternalUnit: Boolean): Integer;
var
  I, TypeID: Integer;
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  RemoveLastIdent(CurrToken.Id);
  Match('operator');
  I := OperatorIndex(CurrToken.Text);
  if I = -1 then
    CreateError(errE2393, []);
        // errE2393 = 'Invalid operator declaration';
  result := Parse_Ident;
  SetName(result, operators.Values[I]);
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  SetVisibility(result, vis);
  BeginStructureOperator(result, RecordTypeId);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);

  DECLARE_SWITCH := false;
  Match(':');
  Parse_Attribute;
  TypeID := Parse_Type;
  Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
  Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
  if Assigned(OnParseResultType) then
    OnParseResultType(Owner, GetName(TypeId), TypeId);

  DECLARE_SWITCH := true;
  Match(';');
  DirectiveList := Parse_DirectiveList(result);
  FreeAndNil(DirectiveList);

  SetForward(result, true);
  SetOverloaded(result);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := 'class ' + ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_RecordProperty(RecordTypeId: Integer;
                                            Vis: TClassVisibility;
                                            IsExternalUnit: Boolean): Integer;
var
  TypeID, ReadId, WriteId, ImplementsId: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  DECLARE_SWITCH := true;
  SavedPosition := CurrToken.Position;
  Match('property');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  SetVisibility(result, vis);
  BeginProperty(result, RecordTypeId);
  ReadId := 0;
  WriteId := 0;
  TypeId := 0;

  try

    Parse_FormalParameterList(result, '[');

    SetReadId(result, ReadId);
    SetWriteId(result, WriteId);

    if IsCurrText(';') then
    begin
      Match(';');
      Gen(OP_DETERMINE_PROP, result, 0, 0);
      EndProperty(result);
      Exit;
    end;

    if IsCurrText(':') then
    begin
      DECLARE_SWITCH := false;
      Match(':');
      TypeID := Parse_QualId;
      Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
    end;

    repeat
      DECLARE_SWITCH := false;
      if IsCurrText('read') and (ReadId = 0) then
      begin
        RemoveLastIdent(CurrToken.Id);
        Call_SCANNER;
        ReadId := Parse_QualId;
        ReadId := Lookup(GetName(ReadId), RecordTypeId);
        if ReadId = 0 then
          RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
        SetReadId(result, ReadId);
      end
      else if IsCurrText('write') and (WriteId = 0) then
      begin
        RemoveLastIdent(CurrToken.Id);
        Call_SCANNER;
        WriteId := Parse_QualId;
        WriteId := Lookup(GetName(WriteId), RecordTypeId);
        if WriteId = 0 then
          RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
        SetWriteId(result, WriteId);
      end
      else if IsCurrText('implements') then
      begin
        RemoveLastIdent(CurrToken.Id);
        DECLARE_SWITCH := false;
        Call_SCANNER;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        ImplementsId := Parse_Ident;
        Gen(OP_IMPLEMENTS, result, ImplementsId, 0);
      end
      else if IsCurrText('stored') then
      begin
        DECLARE_SWITCH := false;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        Parse_Expression;
      end
      else if IsCurrText('index') then
      begin
        DECLARE_SWITCH := false;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        Parse_Expression;
      end
      else if IsCurrText('default') then
      begin
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        if IsCurrText(';') then
          SetDefault(result, true)
        else
          Parse_Expression;
      end
      else if IsCurrText('nodefault') then
      begin
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
      end
      else
        break;
    until false;

    if IsNextText('default') then
    begin
      Call_SCANNER;
      Match('default');
      SetDefault(result, true);
    end;

    if ReadId + WriteId = 0 then
      RaiseError(errSyntaxError, []);

    if ReadId > 0 then
      TKernel(kernel).Code.used_private_members.Add(ReadId);
    if WriteId > 0 then
      TKernel(kernel).Code.used_private_members.Add(WriteId);

    Match(';');
    EndProperty(result);
  finally
    if Assigned(OnParsePropertyDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParsePropertyDeclaration(Owner, GetName(result), result,
        GetName(TypeId), Declaration);
    end;
  end;
end;

procedure TPascalParser.Parse_RecordVariantPart(VarLevel: Integer;
                                                CurrVarCnt: Int64;
                                                vis: TClassVisibility);
var
  id, I, TypeId: Integer;
  V, VarCnt: Int64;
  L: TIntegerList;
  S, Declaration: String;
  SavedPosition: Integer;
begin

  L := TIntegerList.Create;

  try
    VarCnt := 0;

    if IsNext2Text(':') then
    begin
      DECLARE_SWITCH := true;
      Match('case');
      DECLARE_SWITCH := false;
      id := Parse_Ident;
      Match(':');
      TypeId := Parse_Ident;
      SetKind(Id, KindTYPE_FIELD);
      Gen(OP_ASSIGN_TYPE, id, TypeId, 0);
    end
    else
    begin
      DECLARE_SWITCH := false;
      Match('case');
      TypeId := Parse_Ident;
      Gen(OP_EVAL, 0, 0, TypeId);
    end;

    DECLARE_SWITCH := false;
    Match('of');

    repeat
      Inc(VarCnt);

      if IsEOF then
        Break;
      if IsCurrText('end') then
        Break;
      if IsCurrText(')') then
      begin
        Break;
      end;

      // RecVariant

      // ConstList
      repeat
        if IsEOF then
          Break;
        if IsCurrText('end') then
          Break;

        Parse_Expression;

        if NotMatch(',') then
          break;
      until false;

      Match(':');

      // FieldList
      DECLARE_SWITCH := true;

      Match('(');
      if not IsCurrText(')') then
      begin
        if IsCurrText('case') then
        begin
           case VarLevel of
             1: V := VarCnt;
             2: V := VarCnt * 100 + CurrVarCnt;
             3: V := VarCnt * 10000 + CurrVarCnt;
             4: V := VarCnt * 1000000 + CurrVarCnt;
             5: V := VarCnt * 100000000 + CurrVarCnt;
             6: V := VarCnt * 10000000000 + CurrVarCnt;
             7: V := VarCnt * 1000000000000 + CurrVarCnt;
           else
             begin
               V := 0;
               RaiseError(errTooManyNestedCaseBlocks, []);
             end;
           end;

          Parse_RecordVariantPart(VarLevel + 1, V, vis);
        end
        else
        begin

          repeat

            L.Clear;
            repeat  // parse ident list
              L.Add(Parse_Ident);
              if NotMatch(',') then
                break;
            until false;

           DECLARE_SWITCH := false;
           Match(':');

           SavedPosition := CurrToken.Position;
           TypeID := Parse_Type;
           S := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);

           case VarLevel of
             1: V := VarCnt;
             2: V := VarCnt * 100 + CurrVarCnt;
             3: V := VarCnt * 10000 + CurrVarCnt;
             4: V := VarCnt * 1000000 + CurrVarCnt;
             5: V := VarCnt * 100000000 + CurrVarCnt;
             6: V := VarCnt * 10000000000 + CurrVarCnt;
             7: V := VarCnt * 1000000000000 + CurrVarCnt;
           else
             begin
               V := 0;
               RaiseError(errTooManyNestedCaseBlocks, []);
             end;
           end;

           for I:=0 to L.Count - 1 do
           begin
             SetKind(L[I], KindTYPE_FIELD);
             SetVarCount(L[I], V);
             SetVisibility(L[I], vis);
             Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);

             if Assigned(OnParseVariantRecordFieldDeclaration) then
             begin
               Declaration := GetName(L[I]) + ' = ' + S + ';';
               OnParseVariantRecordFieldDeclaration(Owner, GetName(L[I]), L[I],
                 GetName(TypeId), V, Declaration);
             end;
           end;

           if IsCurrText(';') then
           begin
             DECLARE_SWITCH := true;
             Match(';');

             if IsCurrText(')') then
               break;

             if IsCurrText('case') then
             begin
               Parse_RecordVariantPart(VarLevel + 1, V, vis);
               break;
             end;

           end
           else
             break;

          until false;
        end;

        DECLARE_SWITCH := true;
      end;

      DECLARE_SWITCH := false;

      Match(')');
      if IsCurrText(';') then
        Match(';');

    until false;
  finally
    FreeAndNil(L);
  end;
end;

procedure TPascalParser.Parse_RecordHelperItem;
begin
  if IsCurrText('const') then
    Parse_ConstantDeclaration;
end;

procedure TPascalParser.Parse_RecordTypeDeclaration(RecordTypeID: Integer; IsPacked: Boolean;
                                    IsExternalUnit: Boolean = false);
var
  vis: TClassVisibility;

var
  L: TIntegerList;
  I, TypeID, Id, TrueRecordId: Integer;
  b: Boolean;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;

  TrueRecordId := 0;
  vis := cvPublic;

  if IsNextText('helper') then
  begin
    Match('record');
    DECLARE_SWITCH := false;
    RemoveLastIdent(CurrToken.Id);
    Match('helper');
    Match('for');
    TrueRecordId := Parse_QualId;
    BeginHelperType(RecordTypeId, TrueRecordId);

    if Assigned(OnParseBeginRecordHelperTypeDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      Declaration := GetName(RecordTypeId) + ' = ' + Declaration;
      if Assigned(OnParseBeginClassHelperTypeDeclaration) then
      begin
        OnParseBeginRecordHelperTypeDeclaration(Owner, GetName(RecordTypeId), RecordTypeId,
        GetName(TrueRecordId),
        Declaration);
      end;
    end;
  end
  else
  begin
    BeginRecordType(RecordTypeID);
    if IsPacked then
      SetPacked(RecordTypeID);
    Match('record');

    if Assigned(OnParseBeginRecordTypeDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      Declaration := GetName(RecordTypeId) + ' = ' + Declaration;
      OnParseBeginRecordTypeDeclaration(Owner, GetName(RecordTypeId), RecordTypeId, Declaration);
    end;
  end;

  b := false;
  L := TIntegerList.Create;
  try
    repeat
      if IsEOF then
        Break;

      if IsCurrText('end') then
        Break;

      if IsCurrText('case') then

      begin
        Parse_RecordVariantPart(1, 0, vis);
        break;
      end
      else
      begin
        repeat
          if IsCurrText('strict') then
          begin
            RemoveLastIdent(CurrToken.Id);
            Call_SCANNER;
            if IsCurrText('protected') then
            begin
              Call_SCANNER;
              vis := cvStrictProtected;
            end
            else
            begin
              Match('private');
              vis := cvStrictPrivate;
            end;
            b := false;
          end
          else if IsCurrText('private') then
          begin
            Call_SCANNER;
            vis := cvPrivate;
            b := false;
          end
          else if IsCurrText('protected') then
          begin
            Call_SCANNER;
            vis := cvProtected;
            b := false;
          end
          else if IsCurrText('public') then
          begin
            Call_SCANNER;
            vis := cvPublic;
            b := false;
          end
          else if IsCurrText('published') then
          begin
            Call_SCANNER;
            vis := cvPublished;
            b := false;
          end
          else
            break;
        until false;

        if IsCurrText('end') then
          Break;
        Parse_Attribute;

        if IsCurrText('case') then
        begin
          Parse_RecordVariantPart(1, 0, vis);
          break;
        end
        else if IsCurrText('constructor') then
        begin
          Parse_RecordConstructorHeading(false, RecordTypeId, vis, IsExternalUnit);
          b := true;
        end
        else if IsCurrText('destructor') then
        begin
          Parse_RecordDestructorHeading(false, RecordTypeId, vis, IsExternalUnit);
          b := true;
        end
        else if IsCurrText('procedure') then
        begin
          Parse_RecordProcedureHeading(false, RecordTypeId, vis, IsExternalUnit);
          b := true;
        end
        else if IsCurrText('function') then
        begin
          Parse_RecordFunctionHeading(false, RecordTypeId, vis, IsExternalUnit);
          b := true;
        end
        else if IsCurrText('var') or IsCurrText('threadvar') then
        begin
          if IsCurrText('threadvar') then
            Call_SCANNER
          else
            Match('var');

          repeat
            Parse_Attribute;

            L.Clear;
            repeat  // parse ident list
              Id := Parse_Ident;
              Gen(OP_DECLARE_MEMBER, CurrLevel, Id, 0);
              L.Add(Id);
              if NotMatch(',') then
                break;
            until false;

            DECLARE_SWITCH := false;
            Match(':');

            TypeID := Parse_Type;

            for I:=0 to L.Count - 1 do
            begin
              SetKind(L[I], KindTYPE_FIELD);
              SetVisibility(L[I], vis);
              Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);
            end;

            DECLARE_SWITCH := true;

            Parse_PortabilityDirective;

            if IsCurrText(';') then
              Match(';');

            if CurrToken.TokenClass <> tcIdentifier then
            if not IsCurrText('[') then
              break;
          until false;
        end
        else if IsCurrText('class') then
        begin
          b := true;
          Call_SCANNER;
          if IsCurrText('constructor') then
            Parse_RecordConstructorHeading(true, RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('destructor') then
            Parse_RecordDestructorHeading(true, RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('procedure') then
            Parse_RecordProcedureHeading(true, RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('function') then
            Parse_RecordFunctionHeading(true, RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('operator') then
            Parse_RecordOperatorHeading(RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('property') then
            Parse_RecordProperty(RecordTypeId, vis, IsExternalUnit)
          else if IsCurrText('var') or IsCurrText('threadvar') then
            Parse_VariableDeclaration(vis);
        end
        else if IsCurrText('property') then
        begin
          Parse_RecordProperty(RecordTypeId, vis, IsExternalUnit);
          b := true;
        end
        else if IsCurrText('type') then
        begin
          Parse_TypeDeclaration(false, vis);
          b := true;
        end
        else if IsCurrText('const') then
        begin
          Parse_ConstantDeclaration(vis);
          b := true;
        end
        else
        begin
          if IsCurrText('threadvar') then
            Call_SCANNER
          else if IsCurrText('var') then
            Call_SCANNER;

          if b then
            CreateError(errFieldDefinitionNotAllowedAfter, []);

          L.Clear;
          repeat  // parse ident list
            Id := Parse_Ident;
            SetVisibility(Id, Vis);
            Gen(OP_DECLARE_MEMBER, CurrLevel, Id, 0);
            L.Add(Id);
            if NotMatch(',') then
              break;
          until false;

          DECLARE_SWITCH := false;
          Match(':');

          SavedPosition := CurrToken.Position;

          TypeID := Parse_Type;

          for I:=0 to L.Count - 1 do
          begin
            SetKind(L[I], KindTYPE_FIELD);
            Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);

            if Assigned(OnParseFieldDeclaration) then
            begin
              Declaration := GetName(L[I]) + ':' +
                ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
              OnParseFieldDeclaration(Owner, GetName(L[I]), L[I], GetName(TypeId),
               Declaration);
            end;
          end;

          DECLARE_SWITCH := true;

          Parse_PortabilityDirective;

          if IsCurrText(';') then
            Match(';');
        end;

        if IsCurrText('case') then
        begin
          Parse_RecordVariantPart(1, 0, vis);
          break;
        end;
      end;

    until false;
  finally
    FreeAndNil(L);
  end;

  if TrueRecordId > 0 then
  begin
    EndHelperType(RecordTypeId);
    if Assigned(OnParseEndRecordHelperTypeDeclaration) then
      OnParseEndRecordHelperTypeDeclaration(Owner, GetName(RecordTypeId), RecordTypeId);
  end
  else
  begin
    EndRecordType(RecordTypeId);
    if Assigned(OnParseEndRecordTypeDeclaration) then
      OnParseEndRecordTypeDeclaration(Owner, GetName(RecordTypeId), RecordTypeId);
  end;

  Match('end');
end;

procedure TPascalParser.Parse_Message(SubId: Integer);
begin
  DECLARE_SWITCH := false;
  Call_SCANNER;
  if CurrToken.TokenClass = tcIntegerConst then
  begin
    Gen(OP_ADD_MESSAGE, SubId, CurrToken.Id, 0);
  end
  else if CurrToken.TokenClass = tcIdentifier then
  begin
    Gen(OP_ADD_MESSAGE, SubId, CurrToken.Id, 0);
  end
  else
  begin
    RaiseError(errIncompatibleTypesNoArgs, []);
  end;
  ReadToken;
end;

function TPascalParser.Parse_ClassConstructorHeading(IsSharedMethod: Boolean;
                                                     ClassTypeId: Integer;
                                                     vis: TClassVisibility;
                                                     IsExternalUnit: Boolean): Integer;
var
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('constructor');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);
  SetVisibility(result, vis);
  BeginClassConstructor(result, ClassTypeId);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);

  GetSymbolRec(result).IsSharedMethod := IsSharedMethod;

  Parse_FormalParameterList(result);
  Match(';');
  CheckRedeclaredSub(result);

  DirectiveList := Parse_DirectiveList(result);
  try
    if not ImportOnly then
    if CountAtLevel(GetName(result), GetLevel(result), KindCONSTRUCTOR, IsSharedMethod) > 1 then
      if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
        CreateError(errOverloadExpected, [GetName(result)]);

    if (DirectiveList.IndexOf(dirSTATIC) >= 0) and
       (
       (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
       (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
       (DirectiveList.IndexOf(dirOVERRIDE) >= 0)
       ) then
    begin
      CreateError(errE2376, []);
        //STATIC can only be used on non-virtual class methods.
    end;

  finally
     FreeAndNil(DirectiveList);
  end;
  SetForward(result, true);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_ClassDestructorHeading(IsSharedMethod: Boolean;
                                                    ClassTypeId: Integer;
                                                    vis: TClassVisibility;
                                                    IsExternalUnit: Boolean): Integer;
var
  NP: Integer;
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('destructor');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);
  SetVisibility(result, vis);
  BeginClassDestructor(result, ClassTypeId);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);

  GetSymbolRec(result).IsSharedMethod := IsSharedMethod;

  NP := 0;
  if IsCurrText('(') then
  begin
    Call_SCANNER;
    Match(')');
  end;
  SetCount(result, NP);
  Match(';');
  CheckRedeclaredSub(result);

  DirectiveList := Parse_DirectiveList(result);
  try
    if not ImportOnly then
    begin
      if not IsSharedMethod then
        if DirectiveList.IndexOf(dirOVERRIDE) = -1 then
          Match('override');
      if CountAtLevel(GetName(result), GetLevel(result), KindDESTRUCTOR, IsSharedMethod) > 1 then
        if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
          CreateError(errOverloadExpected, [GetName(result)]);
    end;
  finally
    FreeAndNil(DirectiveList);
  end;

  SetForward(result, true);

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_ClassProcedureHeading(IsSharedMethod: Boolean;
                                                   ClassTypeId: Integer;
                                                   vis: TClassVisibility;
                                                   IsExternalUnit: Boolean): Integer;
var
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  BeginTypeExt(ClassTypeId);
  Match('procedure');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  if IsCurrText('.') then
  begin
    Scanner.CurrComment.AllowedDoComment := false;
    Match('.');
    Parse_Ident;
    DECLARE_SWITCH := false;
    Match('=');
    Parse_Ident;
    Match(';');
    Exit;
  end;

  SetVisibility(result, vis);
  BeginClassMethod(result, ClassTypeId, false, IsSharedMethod, false);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);
  EndTypeExt(ClassTypeId);
  Match(';');

  CheckRedeclaredSub(result);

  if IsCurrText('message') then
    Parse_Message(result);

  DirectiveList := Parse_DirectiveList(result);
  try
    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
    begin
      inherited InitSub(result);
      Gen(OP_ERR_ABSTRACT, NewConst(typeSTRING,
                           GetName(ClassTypeId) + '.' + GetName(result)), 0, result);
    end
    else if (DirectiveList.IndexOf(dirSTATIC) >= 0) and
       (
       (IsSharedMethod = false) or
       (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
       (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
       (DirectiveList.IndexOf(dirOVERRIDE) >= 0)
       ) then
    begin
      CreateError(errE2376, []);
        //STATIC can only be used on non-virtual class methods.
    end
    else if DirectiveList.IndexOf(dirOVERRIDE) >= 0 then
    begin
      SetForward(result, true);
    end
    else
      SetForward(result, true);

    if CountAtLevel(GetName(result), GetLevel(result)) > 1 then
      if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
        if not ImportOnly then
          CreateError(errOverloadExpected, [GetName(result)]);

  finally
    FreeAndNil(DirectiveList);
  end;

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_ClassFunctionHeading(IsSharedMethod: Boolean;
                                                  ClassTypeId: Integer;
                                                  vis: TClassVisibility;
                                                  IsExternalUnit: Boolean): Integer;
var
  TypeID: Integer;
  DirectiveList: TIntegerList;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := true;
  Match('function');
  result := Parse_Ident;
  Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);

  if IsCurrText('.') then
  begin
    Scanner.CurrComment.AllowedDoComment := false;
    Match('.');
    Parse_Ident;
    DECLARE_SWITCH := false;
    Match('=');
    Parse_Ident;
    Match(';');
    Exit;
  end;

  SetVisibility(result, vis);
  BeginClassMethod(result, ClassTypeId, true, IsSharedMethod, false);
  if Assigned(OnParseBeginSubDeclaration) then
    OnParseBeginSubDeclaration(Owner, GetName(result), result);
  Parse_FormalParameterList(result);

  DECLARE_SWITCH := false;
  Match(':');
  Parse_Attribute;
  TypeID := Parse_Type;
  Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
  Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
  if Assigned(OnParseResultType) then
    OnParseResultType(Owner, GetName(TypeId), TypeId);

  DECLARE_SWITCH := true;
  Match(';');
  CheckRedeclaredSub(result);

  if IsCurrText('message') then
    Parse_Message(result);

  DirectiveList := Parse_DirectiveList(result);
  try

    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
    begin
      inherited InitSub(result);
      Gen(OP_ERR_ABSTRACT, NewConst(typeSTRING,
                           GetName(ClassTypeId) + '.' + GetName(result)), 0, result);
    end
    else if (DirectiveList.IndexOf(dirSTATIC) >= 0) and
       (
       (IsSharedMethod = false) or
       (DirectiveList.IndexOf(dirVIRTUAL) >= 0) or
       (DirectiveList.IndexOf(dirDYNAMIC) >= 0) or
       (DirectiveList.IndexOf(dirOVERRIDE) >= 0)
       ) then
    begin
      CreateError(errE2376, []);
        //STATIC can only be used on non-virtual class methods.
    end
    else if DirectiveList.IndexOf(dirOVERRIDE) >= 0 then
    begin
      SetForward(result, true);
    end
    else
      SetForward(result, true);

    if CountAtLevel(GetName(result), GetLevel(result)) > 1 then
      if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
        if not ImportOnly then
          CreateError(errOverloadExpected, [GetName(result)]);

  finally
    FreeAndNil(DirectiveList);
  end;

  if IsCurrText('external') then
  begin
    ParseExternalSub(result);
    SetForward(result, false);
    Exit;
  end;

  if IsExternalUnit then
  begin
    GenExternalSub(result);
    Exit;
  end;

  EndSub(result);
  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    if IsSharedMethod then
      Declaration := 'class ' + Declaration;
    OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
  end;
end;

function TPascalParser.Parse_ClassProperty(IsShared: Boolean;
                                           ClassTypeId: Integer;
                                           vis: TClassVisibility;
                                           IsExternalUnit: Boolean): Integer;
var
  ReadId, WriteId, ImplementsId, TypeID, K: Integer;
  StrType, StrDefault, Declaration: String;
  SavedPosition: Integer;
begin
  RemoveKeywords;

  SavedPosition := CurrToken.Position;
  ReadId := 0;
  WriteId := 0;

  DECLARE_SWITCH := true;
  Match('property');
  result := Parse_Ident;

  StrType := '';
  StrDefault := '';

  try
    Gen(OP_DECLARE_MEMBER, CurrLevel, result, 0);
    SetVisibility(result, vis);
    BeginProperty(result, ClassTypeId);

    Parse_FormalParameterList(result, '[');

    if IsCurrText(';') then
    begin
      Gen(OP_DETERMINE_PROP, result, 0, 0);
      EndProperty(result);
      Exit;
    end
    else if IsCurrText('default') then
    begin
      Call_SCANNER;
      StrDefault := CurrToken.Text;
      if not IsCurrText(';') then
        Parse_Expression;
      Gen(OP_DETERMINE_PROP, result, 0, 0);
      EndProperty(result);
      Exit;
    end
    else if IsCurrText('nodefault') then
    begin
      Call_SCANNER;
      Gen(OP_DETERMINE_PROP, result, 0, 0);
      EndProperty(result);
      Exit;
    end;

    if IsCurrText(':') then
    begin
      DECLARE_SWITCH := false;
      Match(':');
      TypeID := Parse_QualId;
      StrType := GetName(TypeId);
      Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
    end;

    repeat
      DECLARE_SWITCH := false;
      if IsCurrText('read') and (ReadId = 0) then
      begin
        RemoveLastIdent(CurrToken.Id);
        Call_SCANNER;
        ReadId := Parse_QualId;
        Gen(OP_SET_READ_ID, result, ReadId, 0);
        SetReadId(result, ReadId);

        if IsCurrText(';') then
          if IsNextText('default') then
            Call_SCANNER;
      end
      else if IsCurrText('write') and (WriteId = 0) then
      begin
        RemoveLastIdent(CurrToken.Id);
        Call_SCANNER;
        WriteId := Parse_QualId;
        Gen(OP_SET_WRITE_ID, result, WriteId, 0);
        SetWriteId(result, WriteId);

        if IsCurrText(';') then
          if IsNextText('default') then
            Call_SCANNER;
      end
      else if IsCurrText('implements') then
      begin
        RemoveLastIdent(CurrToken.Id);
        DECLARE_SWITCH := false;
        Call_SCANNER;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        repeat
          ImplementsId := Parse_QualId;
          Gen(OP_IMPLEMENTS, result, ImplementsId, 0);
          if NotMatch(',') then
            break;
        until false;
      end
      else if IsCurrText('stored') then
      begin
        DECLARE_SWITCH := false;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        Parse_Expression;
      end
      else if IsCurrText('index') then
      begin
        DECLARE_SWITCH := false;
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        Parse_Expression;
      end
      else if IsCurrText('default') then
      begin
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
        if IsCurrText(';') then
          SetDefault(result, true)
        else
          Parse_Expression;
      end
      else if IsCurrText('nodefault') then
      begin
        if not StrEql(GetName(result), CurrToken.Text) then
          DiscardLastStRecord;
        Call_SCANNER;
      end
      else
        break;
    until false;

    if IsNextText('default') then
    begin
      Call_SCANNER;
      Match('default');
      StrDefault := CurrToken.Text;
      SetDefault(result, true);
    end;

    if ReadId > 0 then
      TKernel(kernel).Code.used_private_members.Add(ReadId);
    if WriteId > 0 then
      TKernel(kernel).Code.used_private_members.Add(WriteId);

    EndProperty(result);
  finally
    RestoreKeywords;
    if Assigned(OnParsePropertyDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition,
        CurrToken.Position + CurrToken.Length - 1);
      if IsShared then
        Declaration := 'class ' + Declaration;

      if StrType = '' then
        if GetSymbolRec(result).Vis in [cvPublic, cvProtected] then
        begin
          if ReadId > 0 then
          begin
            TypeId := GetSymbolRec(ReadId).TypeID;
            StrType := GetName(TypeId);
          end;

          if StrType = '' then
          if WriteId > 0 then
          begin
            if GetKind(WriteId) = KindSUB then
            begin
              K := GetCount(result);
              K := GetParamId(WriteId, K);
              TypeId := GetSymbolRec(K).TypeID;
              StrType := GetName(TypeId);
            end
            else
            begin
              TypeId := GetSymbolRec(WriteId).TypeID;
              StrType := GetName(TypeId);
            end;
          end;

          if StrType = '' then
            if StrDefault <> '' then
            begin
              if StrEql(StrDefault, 'true') or StrEql(StrDefault, 'false') then
                StrType := 'Boolean';
            end;
        end;
      OnParsePropertyDeclaration(Owner, GetName(result), result, StrType,
        Declaration);
    end;
  end;
end;

procedure TPascalParser.Parse_ClassTypeDeclaration(ClassTypeID: Integer; IsPacked: Boolean;
                                                   IsExternalUnit: Boolean = false);
var
  vis: TClassVisibility;

var
  L: TIntegerList;
  I, TypeID, AncestorId, RefTypeId, Id: Integer;
  b: Boolean;
  Declaration: String;
  TrueClassId: Integer;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;

  if IsNextText('of') then
  begin
    DECLARE_SWITCH := false;
    BeginClassReferenceType(ClassTypeID);
    Match('class');
    Match('of');
    RefTypeId := Parse_QualId;
    Gen(OP_CREATE_CLASSREF_TYPE, ClassTypeId, RefTypeId, 0);
    EndClassReferenceType(ClassTypeID);
    if Assigned(OnParseClassReferenceTypeDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, CurrToken.Position + CurrToken.Length - 1);
      OnParseClassReferenceTypeDeclaration(Owner,
        GetName(ClassTypeId), ClassTypeId, GetName(RefTypeId), Declaration);
    end;
    Exit;
  end
  else if IsNextText('helper') then
  begin
    Match('class');
    DECLARE_SWITCH := false;
    RemoveLastIdent(CurrToken.Id);
    Match('helper');
    Match('for');
    TrueClassId := Parse_QualId;
    BeginHelperType(ClassTypeId, TrueClassId);
  end
  else
  begin
    BeginClassType(ClassTypeID);
    TrueClassId := 0;

    if IsPacked then
      SetPacked(ClassTypeID);

    Match('class');

    if IsCurrText(';') then // forward declaration
    begin
      SetForward(ClassTypeId, true);
      EndClassType(ClassTypeId, true);
      if Assigned(OnParseForwardTypeDeclaration) then
        OnParseForwardTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId);
      Exit;
    end;

    if IsCurrText('abstract') then
    begin
      RemoveLastEvalInstruction('abstract');
      SetAbstract(ClassTypeId, true);
      Call_SCANNER;
    end
    else if IsCurrText('sealed') then
    begin
      SetFinal(ClassTypeId, true);
      Call_SCANNER;
    end;
  end;

  if TrueClassId > 0 then
  begin
    if Assigned(OnParseBeginClassHelperTypeDeclaration) then
    begin
      OnParseBeginClassHelperTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId,
      GetName(TrueClassId),
      Declaration);
    end;
  end
  else if Assigned(OnParseBeginClassTypeDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    Declaration := GetName(ClassTypeId) + ' = ' + Declaration;
    OnParseBeginClassTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId, Declaration);
  end;

  if IsCurrText('(') then
  begin
    DECLARE_SWITCH := false;
    Match('(');
    AncestorId := Parse_QualId;
    Gen(OP_ADD_ANCESTOR, ClassTypeId, AncestorId, 0);
    if Assigned(OnParseAncestorTypeDeclaration) then
      OnParseAncestorTypeDeclaration(Owner, GetName(AncestorId), AncestorId);

    if StrEql(GetName(ClassTypeId), GetName(AncestorId)) then
      RaiseError(errRedeclaredIdentifier, [GetName(AncestorId)]);

    if IsCurrText(',') then
    begin
      Call_SCANNER;
      repeat
         AncestorId := Parse_QualId;
         Gen(OP_ADD_INTERFACE, ClassTypeId, AncestorId, 0);
         if Assigned(OnParseUsedInterface) then
           OnParseUsedInterface(Owner, GetName(AncestorId), AncestorId);
        if NotMatch(',') then
          break;
      until false;
    end;

    DECLARE_SWITCH := true;
    Match(')')
  end
  else
    if Assigned(OnParseAncestorTypeDeclaration) then
      OnParseAncestorTypeDeclaration(Owner, GetName(H_TObject), H_TObject);


  if IsCurrText(';') then
  begin
    if TrueClassId > 0 then
    begin
      EndHelperType(ClassTypeId);

      if Assigned(OnParseEndClassHelperTypeDeclaration) then
        OnParseEndClassHelperTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId);
    end
    else
    begin
      EndClassType(ClassTypeId);

      if Assigned(OnParseEndClassTypeDeclaration) then
        OnParseEndClassTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId);

      if FindConstructorId(ClassTypeId) = 0 then
        GenDefaultConstructor(ClassTypeId);
      if FindDestructorId(ClassTypeId) = 0 then
        GenDefaultDestructor(ClassTypeId);
    end;
    Exit;
  end;

  vis := cvPublished;

  b := false;

  L := TIntegerList.Create;
  try
    repeat
      if IsEOF then
        Break;
      if IsCurrText('end') then
        Break;

      repeat
        if IsCurrText('strict') then
        begin
          RemoveLastIdent(CurrToken.Id);
          Call_SCANNER;
          if IsCurrText('protected') then
          begin
            Call_SCANNER;
            vis := cvStrictProtected;
          end
          else
          begin
            Match('private');
            vis := cvStrictPrivate;
          end;
          b := false;
        end
        else if IsCurrText('private') then
        begin
          Call_SCANNER;
          vis := cvPrivate;
          b := false;
        end
        else if IsCurrText('protected') then
        begin
          Call_SCANNER;
          vis := cvProtected;
          b := false;
        end
        else if IsCurrText('public') then
        begin
          Call_SCANNER;
          vis := cvPublic;
          b := false;
        end
        else if IsCurrText('published') then
        begin
          Call_SCANNER;
          vis := cvPublished;
          b := false;
        end
        else
          break;

      until false;

      if IsCurrText('end') then
        Break;

      Parse_Attribute;

      if IsCurrText('constructor') then
      begin
        Parse_ClassConstructorHeading(false, ClassTypeId, vis, IsExternalUnit);
        b := true;
      end
      else if IsCurrText('destructor') then
      begin
        Parse_ClassDestructorHeading(false, ClassTypeId, vis, IsExternalUnit);
        b := true;
      end
      else if IsCurrText('procedure') then
      begin
        Parse_ClassProcedureHeading(false, ClassTypeId, vis, IsExternalUnit);
        b := true;
      end
      else if IsCurrText('function') then
      begin
        Parse_ClassFunctionHeading(false, ClassTypeId, vis, IsExternalUnit);
        b := true;
      end
      else if IsCurrText('var') or IsCurrText('threadvar') then
      begin
        if IsCurrText('threadvar') then
          Call_SCANNER
        else
          Match('var');

        repeat
          Parse_Attribute;

          L.Clear;
          repeat  // parse ident list
            Id := Parse_Ident;
            Gen(OP_DECLARE_MEMBER, CurrLevel, Id, 0);
            L.Add(Id);
            if NotMatch(',') then
              break;
          until false;

          DECLARE_SWITCH := false;
          Match(':');

          TypeID := Parse_Type;

          for I:=0 to L.Count - 1 do
          begin
            SetKind(L[I], KindTYPE_FIELD);
            SetVisibility(L[I], vis);
            Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);
          end;

          DECLARE_SWITCH := true;

          Parse_PortabilityDirective;

          if IsCurrText(';') then
            Match(';');

          if CurrToken.TokenClass <> tcIdentifier then
          if not IsCurrText('[') then
            break;
        until false;
      end
      else if IsCurrText('class') then
      begin
        b := true;
        Call_SCANNER;
        if IsCurrText('constructor') then
          Parse_ClassConstructorHeading(true, ClassTypeId, vis, IsExternalUnit)
        else if IsCurrText('destructor') then
          Parse_ClassDestructorHeading(true, ClassTypeId, vis, IsExternalUnit)
        else if IsCurrText('procedure') then
          Parse_ClassProcedureHeading(true, ClassTypeId, vis, IsExternalUnit)
        else if IsCurrText('function') then
          Parse_ClassFunctionHeading(true, ClassTypeId, vis, IsExternalUnit)
        else if IsCurrText('property') then
          Parse_ClassProperty(true, ClassTypeId, vis, IsExternalUnit)
        else if IsCurrText('var') or IsCurrText('threadvar') then
        begin
          if vis = cvPublished then
            Parse_VariableDeclaration(cvPublic)
          else
            Parse_VariableDeclaration(vis);
        end;
      end
      else if IsCurrText('property') then
      begin
        Parse_ClassProperty(false, ClassTypeId, vis, IsExternalUnit);
        b := true;
      end
      else if IsCurrText('type') then
      begin
        Parse_TypeDeclaration(false, vis);
        b := true;
      end
      else if IsCurrText('const') then
      begin
        if vis = cvPublished then
          Parse_ConstantDeclaration(cvPublic)
        else
          Parse_ConstantDeclaration(vis);
        b := true;
      end
      else
      begin
        if IsCurrText('var') then
           Call_SCANNER
        else if IsCurrText('threadvar') then
           Call_SCANNER;

        if b then
          CreateError(errFieldDefinitionNotAllowedAfter, []);

        Parse_Attribute;

        L.Clear;
        repeat  // parse ident list
          Id := Parse_Ident;
          Gen(OP_DECLARE_MEMBER, CurrLevel, Id, 0);
          L.Add(Id);
          if NotMatch(',') then
            break;
        until false;

        DECLARE_SWITCH := false;
        Match(':');

        SavedPosition := CurrToken.Position;

        TypeID := Parse_Type;

        for I:=0 to L.Count - 1 do
        begin
          SetKind(L[I], KindTYPE_FIELD);
          SetVisibility(L[I], vis);
          Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);

          if Assigned(OnParseFieldDeclaration) then
          begin
            Declaration := GetName(L[I]) + ':' +
              ExtractText(SavedPosition, CurrToken.Position + CurrToken.Length - 1);
            OnParseFieldDeclaration(Owner, GetName(L[I]), L[I], GetName(TypeId),
              Declaration);
          end;
        end;
      end;

      DECLARE_SWITCH := true;

      Parse_PortabilityDirective;

      if IsCurrText(';') then
        Match(';');
    until false;
  finally
    FreeAndNil(L);
  end;

  if TrueClassId > 0 then
  begin
    EndHelperType(ClassTypeId);

    if Assigned(OnParseEndClassHelperTypeDeclaration) then
      OnParseEndClassHelperTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId);
  end
  else
  begin
    EndClassType(ClassTypeId);

    if Assigned(OnParseEndClassTypeDeclaration) then
      OnParseEndClassTypeDeclaration(Owner, GetName(ClassTypeId), ClassTypeId);

    if FindConstructorId(ClassTypeId) = 0 then
      GenDefaultConstructor(ClassTypeId);
    if FindDestructorId(ClassTypeId) = 0 then
      GenDefaultDestructor(ClassTypeId);
  end;

  Match('end');
end;

procedure TPascalParser.Parse_MethodRefTypeDeclaration(TypeID: Integer);

var
  NegativeMethodIndex: Integer;

  function Parse_ProcedureHeading: Integer;
  var
    DirectiveList: TIntegerList;
    Declaration: String;
    SavedPosition: Integer;
  begin
    SavedPosition := CurrToken.Position;

    Dec(NegativeMethodIndex);

    DECLARE_SWITCH := true;
    Match('procedure');
    result := NewTempVar();
    SetName(result, ANONYMOUS_METHOD_NAME);
    BeginInterfaceMethod(result, TypeId, false);
    if Assigned(OnParseBeginSubDeclaration) then
      OnParseBeginSubDeclaration(Owner, GetName(result), result);

    Parse_FormalParameterList(result);
    Gen(OP_ADD_METHOD_INDEX, result, NegativeMethodIndex, 0);

    DECLARE_SWITCH := true;
    EndTypeDef(TypeId);
    Match(';');

    DirectiveList := Parse_DirectiveList(result);
    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
      CreateError(errUnknownDirective, ['abstract']);
    EndSub(result);
    FreeAndNil(DirectiveList);

    if Assigned(OnParseEndSubDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
    end;
  end;

  function Parse_FunctionHeading: Integer;
  var
    ResTypeID: Integer;
    DirectiveList: TIntegerList;
    Declaration: String;
    SavedPosition: Integer;
  begin
    SavedPosition := CurrToken.Position;

    Dec(NegativeMethodIndex);

    DECLARE_SWITCH := true;
    Match('function');
    result := NewTempVar();
    SetName(result, ANONYMOUS_METHOD_NAME);
    BeginInterfaceMethod(result, TypeId, true);
    if Assigned(OnParseBeginSubDeclaration) then
      OnParseBeginSubDeclaration(Owner, GetName(result), result);
    Parse_FormalParameterList(result);

    DECLARE_SWITCH := false;
    Match(':');
    Parse_Attribute;
    ResTypeID := Parse_Type;
    Gen(OP_ASSIGN_TYPE, result, ResTypeID, 0);
    Gen(OP_ASSIGN_TYPE, CurrResultId, ResTypeID, 0);
    Gen(OP_ADD_METHOD_INDEX, result, NegativeMethodIndex, 0);
    if Assigned(OnParseResultType) then
      OnParseResultType(Owner, GetName(ResTypeId), TypeId);

    DECLARE_SWITCH := true;
    EndTypeDef(TypeId);
    Match(';');

    DirectiveList := Parse_DirectiveList(result);
    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
      CreateError(errUnknownDirective, ['abstract']);
    EndSub(result);
    FreeAndNil(DirectiveList);

    if Assigned(OnParseEndSubDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
    end;
  end;

var
  SavedPosition: Integer;
  Declaration: String;
begin
  SavedPosition := CurrToken.Position;

  NegativeMethodIndex := 0;

  BeginMethodRefType(TypeID);

  if IsCurrText('procedure') then
  begin
    Parse_ProcedureHeading;
  end
  else if IsCurrText('function') then
  begin
    Parse_FunctionHeading;
  end
  else
    Match('procedure');

  EndMethodRefType(TypeId);

  if Assigned(OnParseMethodReferenceTypeDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition +
                                              PrevLength - 1);
    OnParseMethodReferenceTypeDeclaration(Owner, GetName(TypeId), TypeId,
      Declaration);
  end;
end;

procedure TPascalParser.Parse_InterfaceTypeDeclaration(IntfTypeID: Integer);

const
  IsPacked = true;

var
  NegativeMethodIndex: Integer;

  function Parse_ProcedureHeading: Integer;
  var
    DirectiveList: TIntegerList;
    Declaration: String;
    SavedPosition: Integer;
  begin
    SavedPosition := CurrToken.Position;

    Dec(NegativeMethodIndex);

    DECLARE_SWITCH := true;
    Match('procedure');
    result := Parse_Ident;
    BeginInterfaceMethod(result, IntfTypeId, false);
    if Assigned(OnParseBeginSubDeclaration) then
      OnParseBeginSubDeclaration(Owner, GetName(result), result);

    Parse_FormalParameterList(result);
    Gen(OP_ADD_METHOD_INDEX, result, NegativeMethodIndex, 0);

    DECLARE_SWITCH := true;
    Match(';');

    if IsCurrText('dispid') then
    begin
      Call_SCANNER;
      Parse_Expression;
    end;

    DirectiveList := Parse_DirectiveList(result);
    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
      CreateError(errUnknownDirective, ['abstract']);
    EndSub(result);
    if Assigned(OnParseEndSubDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
    end;

    FreeAndNil(DirectiveList);
  end;

  function Parse_FunctionHeading: Integer;
  var
    TypeID: Integer;
    DirectiveList: TIntegerList;
    Declaration: String;
    SavedPosition: Integer;
  begin
    SavedPosition := CurrToken.Position;
    Dec(NegativeMethodIndex);

    DECLARE_SWITCH := true;
    Match('function');
    result := Parse_Ident;
    BeginInterfaceMethod(result, IntfTypeId, true);
    if Assigned(OnParseBeginSubDeclaration) then
      OnParseBeginSubDeclaration(Owner, GetName(result), result);
    Parse_FormalParameterList(result);

    DECLARE_SWITCH := false;
    Match(':');
    Parse_Attribute;
    TypeID := Parse_Type;
    Gen(OP_ASSIGN_TYPE, result, TypeID, 0);
    Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
    Gen(OP_ADD_METHOD_INDEX, result, NegativeMethodIndex, 0);
    if Assigned(OnParseResultType) then
      OnParseResultType(Owner, GetName(TypeId), TypeId);

    DECLARE_SWITCH := true;
    Match(';');

    if IsCurrText('dispid') then
    begin
      Call_SCANNER;
      Parse_Expression;
    end;

    DirectiveList := Parse_DirectiveList(result);
    if DirectiveList.IndexOf(dirABSTRACT) >= 0 then
      CreateError(errUnknownDirective, ['abstract']);
    EndSub(result);
    if Assigned(OnParseEndSubDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParseEndSubDeclaration(Owner, GetName(result), result, Declaration);
    end;
    FreeAndNil(DirectiveList);
  end;

  function Parse_Property: Integer;
  var
    TypeID, ReadId, WriteId: Integer;
    Declaration: String;
    SavedPosition: Integer;
  begin
    DECLARE_SWITCH := true;
    SavedPosition := CurrToken.Position;
    Match('property');
    result := Parse_Ident;
    BeginProperty(result, IntfTypeId);

    ReadId := 0;
    WriteId := 0;
    TypeId := 0;

    try
      SetVisibility(result, cvPublic);
      Parse_FormalParameterList(result, '[');

      DECLARE_SWITCH := false;
      Match(':');
      TypeID := Parse_QualId;
      Gen(OP_ASSIGN_TYPE, result, TypeID, 0);

      if IsCurrText('readonly') then
      begin
        Call_SCANNER;
      end
      else if IsCurrText('writeonly') then
      begin
        Call_SCANNER;
      end;

      if IsCurrText('dispid') then
      begin
        Call_SCANNER;
        Parse_Expression;
        if IsNextText('default') then
        begin
          Match(';');
          Call_SCANNER;
          SetDefault(result, true);
        end;
        EndProperty(result);
        Exit;
      end;

      repeat
        if IsCurrText('read') and (ReadId = 0) then
        begin
          RemoveLastIdent(CurrToken.Id);
          Call_SCANNER;
          ReadId := Parse_QualId;
          ReadId := Lookup(GetName(ReadId), IntfTypeId);
          if ReadId = 0 then
            RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
          SetReadId(result, ReadId);
        end
        else if IsCurrText('write') and (WriteId = 0) then
        begin
          RemoveLastIdent(CurrToken.Id);
          Call_SCANNER;
          WriteId := Parse_QualId;
          WriteId := Lookup(GetName(WriteId), IntfTypeId);
          if WriteId = 0 then
            RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
          SetWriteId(result, WriteId);
        end
        else
          break;
      until false;

      if IsCurrText(';') then
        Call_SCANNER
      else
        RaiseError(errTokenExpected, [';', CurrToken.Text]);

      if IsCurrText('default') then
      begin
        Call_SCANNER;
        SetDefault(result, true);
      end;

      if ReadId + WriteId = 0 then
        RaiseError(errSyntaxError, []);

      EndProperty(result);
    finally
      if Assigned(OnParsePropertyDeclaration) then
      begin
        Declaration := ExtractText(SavedPosition, PrevPosition +
          PrevLength - 1);
        OnParsePropertyDeclaration(Owner, GetName(result), result, GetName(TypeId),
          Declaration);
      end;
    end;
  end;

var
  L: TIntegerList;
  I, AncestorId: Integer;
  S: String;
  IntfList: TIntegerList;
begin
  IntfList := TIntegerList.Create;
  try
    NegativeMethodIndex := 0;

    BeginInterfaceType(IntfTypeID);
    SetPacked(IntfTypeID);

    if IsCurrText('dispinterface') then
      Call_SCANNER
    else
      Match('interface');

    if IsCurrText(';') then // forward declaration
    begin
      SetForward(IntfTypeId, true);
      EndInterfaceType(IntfTypeId, true);
      if Assigned(OnParseForwardTypeDeclaration) then
        OnParseForwardTypeDeclaration(Owner, GetName(IntfTypeId), IntfTypeId);
      Exit;
    end;

    if IsCurrText('(') then
    begin
      DECLARE_SWITCH := false;
      Match('(');

      repeat
         AncestorId := Parse_Ident;
         IntfList.Add(AncestorId);
         Gen(OP_ADD_INTERFACE, IntfTypeId, AncestorId, 0);
        if NotMatch(',') then
          break;
      until false;

      DECLARE_SWITCH := true;
      Match(')');
    end
    else
    begin
      Gen(OP_ADD_INTERFACE, IntfTypeId, H_IUnknown, 0);
      IntfList.Add(H_IUnknown);
    end;

    if IsCurrText('[') then
    begin
      Match('[');
      if CurrToken.TokenClass = tcPCharConst then
      begin
        I := Parse_PCharLiteral;
        S := GetValue(I);
        SetGuid(IntfTypeId, S);
      end
      else
      begin
        I := Parse_Ident;
        S := GetValue(I);
  //      SetGuid(IntfTypeId, S);
        if ImportOnly then
          GetSymbolRec(IntfTypeId).NoGUID := true;
      end;
      Match(']');
    end
    else
    begin
      if ImportOnly then
        GetSymbolRec(IntfTypeId).NoGUID := true;
      SetNewGuid(IntfTypeId);
    end;

    if Assigned(OnParseBeginInterfaceTypeDeclaration) then
      OnParseBeginInterfaceTypeDeclaration(Owner, GetName(IntfTypeId), IntfTypeId);
    if Assigned(OnParseAncestorTypeDeclaration) then
    begin
      AncestorId := IntfList[0];
      OnParseAncestorTypeDeclaration(Owner, GetName(AncestorId), AncestorId);
    end;
    if Assigned(OnParseUsedInterface) then
      for I := 1 to IntfList.Count - 1 do
      begin
        AncestorId := IntfList[I];
        OnParseUsedInterface(Owner, GetName(AncestorId), AncestorId);
      end;

    L := TIntegerList.Create;
    try
      repeat
        if IsEOF then
          Break;
        if IsCurrText('end') then
          Break;

        repeat
          if IsCurrText('private') then
          begin
            CreateError(errKeywordNotAllowedInInterfaceDeclaration, [CurrToken.Text]);
            Call_SCANNER;
          end
          else if IsCurrText('protected') then
          begin
            CreateError(errKeywordNotAllowedInInterfaceDeclaration, [CurrToken.Text]);
            Call_SCANNER;
          end
          else if IsCurrText('public') then
          begin
            CreateError(errKeywordNotAllowedInInterfaceDeclaration, [CurrToken.Text]);
            Call_SCANNER;
          end
          else if IsCurrText('published') then
          begin
            CreateError(errKeywordNotAllowedInInterfaceDeclaration, [CurrToken.Text]);
            Call_SCANNER;
          end
          else
            break;

        until false;

        if IsCurrText('end') then
          Break;

        if IsCurrText('procedure') then
        begin
          Parse_ProcedureHeading;
        end
        else if IsCurrText('function') then
        begin
          Parse_FunctionHeading;
        end
        else if IsCurrText('property') then
        begin
          Parse_Property;
        end
        else if IsCurrText('[') then
          Parse_Attribute
        else
          Match('end');

        DECLARE_SWITCH := true;

        if IsCurrText(';') then
          Match(';');
      until false;
    finally
      FreeAndNil(L);
    end;

    EndInterfaceType(IntfTypeId);
    if Assigned(OnParseEndInterfaceTypeDeclaration) then
      OnParseEndInterfaceTypeDeclaration(Owner, GetName(IntfTypeId), IntfTypeId);

    Match('end');
  finally
    FreeAndNil(IntfList);
  end;
end;

procedure TPascalParser.Parse_PointerTypeDeclaration(TypeID: Integer);
var
  RefTypeId: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := false;
  Match('^');
  BeginPointerType(TypeID);
  RefTypeId := Parse_QualId;
  Gen(OP_CREATE_POINTER_TYPE, TypeId, RefTypeId, 0);
  EndPointerType(TypeID);
  if Assigned(OnParsePointerTypeDeclaration) then
  begin
    Declaration := GetName(TypeId) + '=' +
      ExtractText(SavedPosition, CurrToken.Position + CurrToken.Length - 1);
    OnParsePointerTypeDeclaration(Owner,
      GetName(TypeId), TypeId, GetName(RefTypeId), Declaration);
  end;
end;

{$IFNDEF PAXARM}
procedure TPascalParser.Parse_ShortStringTypeDeclaration(TypeID: Integer);
var
  ExprId: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := false;
  Match('[');
  BeginShortStringType(TypeID);
  ExprId := Parse_ConstantExpression;
  Gen(OP_CREATE_SHORTSTRING_TYPE, TypeId, ExprId, 0);
  EndShortStringType(TypeID);
  Match(']');
  if Assigned(OnParseShortStringTypeDeclaration) then
  begin
    Declaration := 'String' + ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
    OnParseShortStringTypeDeclaration(Owner, GetName(TypeId), TypeId, GetValue(ExprId),
      Declaration);
  end;
end;
{$ENDIF}

procedure TPascalParser.Parse_ProceduralTypeDeclaration(TypeID: Integer;
                                                        var SubId: Integer);
var
  IsFunc: Boolean;
  SubTypeId: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  if IsCurrText('function') then
  begin
    Match('function');
    IsFunc := true;
  end
  else
  begin
    Match('procedure');
    IsFunc := false;
  end;
  SubTypeId := typeVOID;
  SubId := NewTempVar;
  BeginProceduralType(TypeID, SubId);
  if Assigned(OnParseBeginSubDeclaration) then
     OnParseBeginSubDeclaration(Owner, GetName(SubId), SubId);
  Parse_FormalParameterList(SubId);
  DECLARE_SWITCH := false;
  if IsFunc then
  begin
    Match(':');
    DECLARE_SWITCH := true;
    SubTypeID := Parse_Type;
    if Assigned(OnParseResultType) then
      OnParseResultType(Owner, GetName(SubTypeID), SubTypeID);
  end;
  Gen(OP_ASSIGN_TYPE, SubId, SubTypeID, 0);
  Gen(OP_ASSIGN_TYPE, CurrResultId, SubTypeID, 0);
  EndProceduralType(TypeID);

  if Assigned(OnParseEndSubDeclaration) then
  begin
    Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
  end;

  if IsCurrText('of') then
  begin
    Match('of');
    Match('object');
    SetType(TypeId, typeEVENT);
  end;

  DECLARE_SWITCH := true;

  if IsCurrText('stdcall') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  else if IsCurrText('safecall') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  else if IsCurrText('register') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  else if IsCurrText('cdecl') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  else if IsCurrText('msfastcall') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  else if IsCurrText('pascal') then
  begin
    RemoveLastIdent(CurrToken.Id);
    Call_SCANNER;
    SetCallConvention(SubId, ccSTDCALL);
  end
  //--------------
  else if IsNextText('stdcall') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccSTDCALL);
    Call_SCANNER;
  end
  else if IsNextText('safecall') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccSAFECALL);
    Call_SCANNER;
  end
  else if IsNextText('register') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccREGISTER);
    Call_SCANNER;
  end
  else if IsNextText('cdecl') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccCDECL);
    Call_SCANNER;
  end
  else if IsNextText('msfastcall') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccMSFASTCALL);
    Call_SCANNER;
  end
  else if IsNextText('pascal') then
  begin
    Call_SCANNER;
    RemoveLastIdent(CurrToken.Id);
    SetCallConvention(SubId, ccPASCAL);
    Call_SCANNER;
  end;

  if Assigned(OnParseEndSubDeclaration) then
  begin
    OnParseEndSubDeclaration(Owner, GetName(SubId), SubId, Declaration);
  end;

  if GetType(TypeId) = typePROC then
  begin
    if Assigned(OnParseProceduralTypeDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, CurrToken.Position +
                                                CurrToken.Length - 1);
      OnParseProceduralTypeDeclaration(Owner, GetName(TypeId), TypeId,
        Declaration);
    end;
  end
  else
  begin
    if Assigned(OnParseEventTypeDeclaration) then
    begin
      Declaration := ExtractText(SavedPosition, CurrToken.Position +
                                                CurrToken.Length - 1);
      OnParseEventTypeDeclaration(Owner, GetName(TypeId), TypeId,
        Declaration);
    end;
  end;
end;

procedure TPascalParser.Parse_SetTypeDeclaration(TypeID: Integer);
var
  TypeBaseId: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  SavedPosition := CurrToken.Position;
  DECLARE_SWITCH := false;
  Match('set');
  Match('of');
  TypeBaseId := Parse_OrdinalType(Declaration);
  BeginSetType(TypeID, TypeBaseId);
  EndSetType(TypeID);
  if Assigned(OnParseSetTypeDeclaration) then
  begin
    Declaration := GetName(TypeId) + '=' +
      ExtractText(SavedPosition, CurrToken.Position + CurrToken.Length - 1);
    OnParseSetTypeDeclaration(Owner, GetName(TypeId), TypeId, GetName(TypeBaseId),
      Declaration);
  end;
end;

procedure TPascalParser.Parse_EnumTypeDeclaration(TypeID: Integer);
var
  ID, TempID, L, K: Integer;
  Declaration: String;
  SavedPosition: Integer;
begin
  L := CurrLevel;

  BeginEnumType(TypeID, TypeINTEGER);
  if Assigned(OnParseBeginEnumTypeDeclaration) then
    OnParseBeginEnumTypeDeclaration(Owner, GetName(TypeId), TypeId);

  DECLARE_SWITCH := true;
  Match('(');

  TempID := NewConst(TypeID, 0);

  K := 0;

  repeat
    SavedPosition := CurrToken.Position;

    ID := Parse_EnumIdent;

    Inc(K);

    SetLevel(ID, L);

    if IsCurrText('=') then
    begin
      DECLARE_SWITCH := false;
      Match('=');
      Gen(OP_ASSIGN_ENUM, ID, Parse_ConstantExpression, ID);
      Gen(OP_ASSIGN_ENUM, TempID, ID, TempID);
      Gen(OP_INC, TempID, NewConst(typeINTEGER, 1), tempID);
      DECLARE_SWITCH := true;
    end
    else
    begin
      Gen(OP_ASSIGN_ENUM, ID, TempID, ID);
      Gen(OP_INC, TempID, NewConst(typeINTEGER, 1), tempID);
    end;

    if Assigned(OnParseEnumName) then
    begin
      Declaration := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
      OnParseEnumName(Owner, GetName(ID), ID, K - 1, Declaration);
    end;

    if NotMatch(',') then
      Break;
  until false;
  Match(')');
  EndEnumType(TypeID, K);
  if Assigned(OnParseEndEnumTypeDeclaration) then
    OnParseEndEnumTypeDeclaration(Owner, GetName(TypeId), TypeId);
end;

procedure TPascalParser.Parse_SubrangeTypeDeclaration(TypeID, TypeBaseId: Integer;
                                                      var Declaration: String;
                                                      Expr1ID: Integer = 0);
var
  ID1, ID2, ExprId1, ExprId2: Integer;
  SavedPosition: Integer;
begin
  SavedPosition := Scanner.FindPosition([Ord('='), Ord('['), Ord(':'), Ord(',')]) + 1;

  BeginSubrangeType(TypeID, TypeBaseID);

  ID1 := NewConst(TypeBaseId);
  ID2 := NewConst(TypeBaseId);

  if Expr1ID = 0 then
  begin
    ExprId1 := Parse_ConstantExpression;
    Gen(OP_ASSIGN_CONST, ID1, ExprId1, ID1);
  end
  else
  begin
    Gen(OP_ASSIGN_CONST, ID1, Expr1ID, ID1);
  end;

  Match('..');
  ExprId2 := Parse_ConstantExpression;
  Gen(OP_ASSIGN_CONST, ID2, ExprId2, ID2);

  Gen(OP_CHECK_SUBRANGE_TYPE, ID1, ID2, 0);

  EndSubrangeType(TypeID);

  Declaration := GetName(TypeId) + ' = ' + ExtractText(SavedPosition, PrevPosition + PrevLength - 1);
  if Assigned(OnParseSubrangeTypeDeclaration) then
    OnParseSubrangeTypeDeclaration(Owner, GetName(TypeId), TypeId, Declaration);
end;

function TPascalParser.Parse_FormalParameterList(SubId: Integer;
                                                 bracket: Char = '('): Integer;
var
  L: TIntegerList;
  I, ID, TypeId, ExprId: Integer;
  ByRef, IsConst, HasDefaultParameters, IsOpenArray, IsOut: Boolean;
  Declaration, DefaultValue, ParamMod, StrType: String;
  SavedPosition: Integer;
begin
  result := 0;
  if Assigned(OnParseBeginFormalParameterList) then
    OnParseBeginFormalParameterList(Owner);

  BeginCollectSig(SubId);
  try
    DECLARE_SWITCH := true;
    if IsCurrText('(') then
      Call_SCANNER
    else if IsCurrText('[') then
      Call_SCANNER
    else
    begin
      if IsCurrText(':') then
        Sig := '( ) : ' + GetNextText
      else
        Sig := '( ) ;';
      Exit;
    end;

    HasDefaultParameters := false;

    if not IsCurrText(')') then
    begin
      L := TIntegerList.Create;

      StrType := '';

      try

        repeat
          ByRef := false;
          IsConst := false;
          IsOut := false;

          Parse_Attribute;

          ParamMod := '';
          if IsCurrText('var') then
          begin
            ParamMod := 'var';
            Match('var');
            ByRef := true;
          end
          else if IsCurrText('out') then
          begin
            ParamMod := 'out';
            Match('out');
            ByRef := true;
            IsOut := true;
          end
          else if IsCurrText('const') then
          begin
            ParamMod := 'const';
            Match('const');
            IsConst := true;
          end;

          Parse_Attribute;

          L.Clear;
          repeat
            Inc(result);
            ID := Parse_FormalParameter;
            Gen(OP_DECLARE_LOCAL_VAR, SubId, ID, 0);
            L.Add(ID);
            if NotMatch(',') then
              break;
          until false;
          DECLARE_SWITCH := false;

          IsOpenArray := false;
          if ByRef or IsConst then
          begin
            if IsCurrText(':') then
            begin
              Match(':');
              IsOpenArray := IsCurrText('array');
              if IsOpenArray then
                TypeId := Parse_OpenArrayType(StrType)
              else
              begin
                TypeId := Parse_Type;
                StrType := GetName(TypeId);
              end;
            end
            else
              TypeId := typePVOID;
          end
          else
          begin
            Match(':');
            IsOpenArray := IsCurrText('array');
            if IsOpenArray then
              TypeId := Parse_OpenArrayType(StrType)
            else
            begin
              TypeId := Parse_Type;
              StrType := GetName(TypeId);
            end;
          end;

          DECLARE_SWITCH := true;

          for I:=0 to L.Count - 1 do
          begin
            if ByRef then if not IsOpenArray then
              SetByRef(L[I]);

            if IsOut then
              GetSymbolRec(L[I]).IsOut := true;

            if IsConst then
              SetIsConst(L[I]);

            if IsOpenArray then
            begin
              SetOpenArray(L[I], true);
            end;

            Gen(OP_ASSIGN_TYPE, L[I], TypeID, 0);
          end;

          DefaultValue := '';

          if IsCurrText('=') then
          begin
  //          if L.Count > 1 then
  //            CreateError(errParameterNotAllowedHere, [GetName(L[1])]);
            DECLARE_SWITCH := false;
            CollectSig := false;
            Sig := RemoveCh('=', Sig);

            Match('=');

            SavedPosition := CurrToken.Position;
            if ImportOnly then
              ExprId := Parse_Expression
            else
              ExprId := Parse_ConstantExpression;
            DefaultValue := ExtractText(SavedPosition, PrevPosition + PrevLength - 1);

            for I := 0 to L.Count - 1 do
            begin
              Gen(OP_ASSIGN_CONST, L[I], ExprId, L[I]);
              SetOptional(L[I]);
            end;
            CollectSig := true;
            Sig := Sig + CurrToken.Text;

            DECLARE_SWITCH := true;
            HasDefaultParameters := true;
          end
          else
          begin
            if HasDefaultParameters then
              CreateError(errDefaultValueRequired, [GetName(L[0])]);
          end;

          if Assigned(OnParseFormalParameterDeclaration) then
          begin
            if IsOpenArray then
              StrType := 'ARRAY OF ' + StrType;

            for I := 0 to L.Count - 1 do
            begin
              if DefaultValue = '' then
                Declaration := GetName(L[I]) + ' : ' + StrType + ';'
              else
                Declaration := GetName(L[I]) + ' : ' +
                  StrType + '=' + DefaultValue + ';';

              if ParamMod <> '' then
                Declaration := ParamMod + ' ' + Declaration;

              OnParseFormalParameterDeclaration(Owner,
                GetName(L[I]), L[I], StrType, DefaultValue, Declaration);
            end;
          end;

          if NotMatch(';') then
            Break;
        until false;

      finally
        FreeAndNil(L);
      end;
    end;

    if bracket = '(' then
      Match(')')
    else if bracket = '[' then
      Match(']');

    if IsCurrText(':') then
      Sig := Sig + ' ' + GetNextText;
  finally
    SetCount(SubId, result);
    EndCollectSig(SubId);

    if Assigned(OnParseEndFormalParameterList) then
      OnParseEndFormalParameterList(Owner);
  end;
end;

procedure TPascalParser.Parse_SubBlock;
begin
  if GetName(CurrSelfId) = '' then
  begin
    Gen(OP_STMT, 0, 0, 0);
    Parse_Block;
  end
  else
  begin
    Gen(OP_STMT, 0, 0, 0);
    DECLARE_SWITCH := true;
    Parse_DeclarationPart;

    Gen(OP_BEGIN_WITH, CurrSelfId, 0, 0);
    WithStack.Push(CurrSelfId);
    Parse_CompoundStmt;
    Gen(OP_END_WITH, WithStack.Top, 0, 0);
    WithStack.Pop;
  end;
end;

procedure TPascalParser.Parse_ProcedureDeclaration(IsSharedMethod: Boolean = false);
var
  SubId, ForwardId: Integer;
  DirectiveList: TIntegerList;
  NotDeclared, WaitOverload: Boolean;
  K: Integer;
begin
  DECLARE_SWITCH := true;
  K := 0;
  BeginMethodDef;
  try

    NotDeclared := false;
    WaitOverload := false;

    if IsSharedMethod then
    begin
      ForwardId := ReadType;

      if ForwardId = 0 then
        CreateError(errUndeclaredIdentifier, [CurrToken.Text]);

      Call_SCANNER;
      DECLARE_SWITCH := true;
      Scanner.CurrComment.AllowedDoComment := false;
      Match('.');
      SubId := Parse_Ident;
      BeginClassMethod(SubId, ForwardId, false, true, true);
    end
    else
    begin
      ForwardId := ReadType;

      if (ForwardId > 0) and (GetKind(ForwardId) = KindTYPE) then
      begin
        Call_SCANNER;

        while GetNext2Text = '.' do
        begin
          Inc(K);
          levelStack.Push(ForwardId);
          ReadToken;
          ForwardId := Lookup(CurrToken.Text, CurrLevel);
          if ForwardId = 0 then
             RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
          ReadToken;
        end;

        DECLARE_SWITCH := true;
        Scanner.CurrComment.AllowedDoComment := false;
        Match('.');
        SubId := Parse_Ident;
        if Lookup(GetName(SubId), ForwardId) = 0 then
          NotDeclared := true;
        BeginClassMethod(SubId, ForwardId, false, false, true);
      end
      else
      begin
        if ForwardId > 0 then
        if GetKind(ForwardId) in KindSUBS then
          if not GetSymbolRec(ForwardId).IsForward then
          if GetSymbolRec(ForwardId).Host = false then
          if GetSymbolRec(ForwardId).OverCount = 0 then
            RaiseError(errRedeclaredIdentifier, [CurrToken.Text])
          else
            WaitOverload := true;

        SubId := NewVar(CurrToken.Text);
        SetPosition(SubId, CurrToken.Position - 1);
        CurrToken.Id := SubId;
        Parse_Ident;
        BeginSub(SubId);
      end;
    end;

    Parse_FormalParameterList(SubId);
    SetName(CurrResultId, '');
    SetKind(CurrResultId, KindNONE);
    SetType(SubId, TypeVOID);
    SetType(CurrResultId, TypeVOID);

    Match(';');

    if NotDeclared then
      CreateError(errUndeclaredIdentifier, [GetName(SubId)]);

    DirectiveList := Parse_DirectiveList(SubId);
    try
      if DirectiveList.IndexOf(dirFORWARD) >= 0 then
      begin
        SetForward(SubId, true);
        EndSub(SubId);
        Exit;
      end;
      if WaitOverload then
      begin
        if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
          CreateError(errOverloadExpected, [GetName(SubId)]);
      end;
    finally
      FreeAndNil(DirectiveList);
    end;

    if IsCurrText('external') then
    begin
      ParseExternalSub(SubId);
      Exit;
    end;

    InitSub(SubId);

    if ForwardId > 0 then
      if not GetSymbolRec(ForwardId).IsForward then
        CheckRedeclaredSub(SubId);

    Parse_SubBlock;
    EndSub(SubId);

    EndMethodDef(SubId);

    Match(';');

  finally
    while K > 0 do
    begin
      Dec(K);
      levelStack.Pop;
    end;
  end;
end;

function TPascalParser.Parse_AnonymousFunction: Integer;
begin
  result := Parse_AnonymousRoutine(true);
end;

function TPascalParser.Parse_AnonymousProcedure: Integer;
begin
  result := Parse_AnonymousRoutine(false);
end;

function TPascalParser.Parse_AnonymousRoutine(IsFunc: Boolean): Integer;
var
  I, Id, RefId, ClassId, SubId, ResTypeId: Integer;
  ClsName, ObjName: String;
begin
  NewAnonymousNames(ClsName, ObjName);
  GenComment('BEGIN OF ANONYMOUS CLASS ' + ClsName);

  TypeParams.Clear;

  ClassId := NewTempVar;
  SetName(ClassId, ClsName);
  BeginClassType(ClassId);
  SetPacked(ClassId);
  SetAncestorId(ClassId, H_TInterfacedObject);
//  Gen(OP_ADD_INTERFACE, ClassId, 0, 0); // 0 - anonymous

  GenDefaultConstructor(ClassId);
  GenDefaultDestructor(ClassId);

  DECLARE_SWITCH := true;
  if IsFunc then
    Match('function')
  else
    Match('procedure');

  DECLARE_SWITCH := false;

  SubId := NewTempVar;
  SetName(SubId, ANONYMOUS_METHOD_NAME);
  BeginClassMethod(SubId,
                   ClassId,
                   IsFunc, // has result
                   false, // is shared
                   true); // is implementation

  Parse_FormalParameterList(SubId);
  DECLARE_SWITCH := false;
  if IsFunc then
  begin
    Match(':');
    Parse_Attribute;
    ResTypeId := Parse_Type;
    Gen(OP_ASSIGN_TYPE, SubId, ResTypeId, 0);
    Gen(OP_ASSIGN_TYPE, CurrResultId, ResTypeId, 0);
  end;

  DECLARE_SWITCH := true;

  AnonymStack.Push(SubId);
  try
    InitSub(SubId);
    Parse_SubBlock;

    EndSub(SubId);

    for I := 0 to AnonymStack.Top.BindList.Count - 1 do
    begin
      Id := NewTempVar;
      SetName(Id, GetName(AnonymStack.Top.BindList[I]));
      SetLevel(Id, ClassId);
      SetKind(Id, KindTYPE_FIELD);
      SetVisibility(Id, cvPublic);
      Gen(OP_ASSIGN_THE_SAME_TYPE, Id, AnonymStack.Top.BindList[I], 0);
    end;

    EndClassType(ClassId);
    GenComment('END OF ANONYMOUS CLASS ' + ClsName);
    Gen(OP_ADD_TYPEINFO, ClassId, 0, 0);

    result := NewTempVar;
    Gen(OP_DECLARE_LOCAL_VAR, CurrSubId, result, 0);
    SetName(result, ObjName);
    SetType(result, ClassId);

    RefId := NewField('Create', result);
    Gen(OP_FIELD, ClassId, RefId, RefId);
    Gen(OP_ASSIGN, result, RefId, result);

    for I := 0 to AnonymStack.Top.BindList.Count - 1 do
    begin
      RefId := NewField(GetName(AnonymStack.Top.BindList[I]), result);
      Gen(OP_FIELD, result, RefId, RefId);
      Gen(OP_ASSIGN, RefId, AnonymStack.Top.BindList[I], RefId);
    end;
  finally
    AnonymStack.Pop;
  end;
  Gen(OP_ADD_INTERFACE, ClassId, 0, 0); // 0 - anonymous
end;

function TPascalParser.Parse_LambdaExpression: Integer;
var
  I, Id, RefId, ClassId, SubId: Integer;
  ClsName, ObjName: String;
begin
  NewAnonymousNames(ClsName, ObjName);
  GenComment('BEGIN OF ANONYMOUS CLASS ' + ClsName);

  TypeParams.Clear;

  ClassId := NewTempVar;
  SetName(ClassId, ClsName);
  BeginClassType(ClassId);
  SetPacked(ClassId);
  SetAncestorId(ClassId, H_TInterfacedObject);

  GenDefaultConstructor(ClassId);
  GenDefaultDestructor(ClassId);

  SubId := NewTempVar;
  Gen(OP_ASSIGN_LAMBDA_TYPES, SubId, 0, 0);

  SetName(SubId, ANONYMOUS_METHOD_NAME);
  BeginClassMethod(SubId,
                   ClassId,
                   true, // has result
                   false, // is shared
                   true); // is implementation

  DECLARE_SWITCH := true;
  Match('lambda');

  Parse_LambdaParameters(SubId);
  DECLARE_SWITCH := false;

  Match('=>');

  AnonymStack.Push(SubId);
  try
    InitSub(SubId);
    Gen(OP_BEGIN_WITH, CurrSelfId, 0, 0);
    WithStack.Push(CurrSelfId);
    Id := CurrResultId;
    Gen(OP_ASSIGN, Id, Parse_Expression, Id);
    Gen(OP_END_WITH, WithStack.Top, 0, 0);
    WithStack.Pop;
    EndSub(SubId);

    for I := 0 to AnonymStack.Top.BindList.Count - 1 do
    begin
      Id := NewTempVar;
      SetName(Id, GetName(AnonymStack.Top.BindList[I]));
      SetLevel(Id, ClassId);
      SetKind(Id, KindTYPE_FIELD);
      SetVisibility(Id, cvPublic);
      Gen(OP_ASSIGN_THE_SAME_TYPE, Id, AnonymStack.Top.BindList[I], 0);
    end;

    EndClassType(ClassId);
    GenComment('END OF ANONYMOUS CLASS ' + ClsName);
    Gen(OP_ADD_TYPEINFO, ClassId, 0, 0);

    result := NewTempVar;
    Gen(OP_DECLARE_LOCAL_VAR, CurrSubId, result, 0);
    SetName(result, ObjName);
    SetType(result, ClassId);

    RefId := NewField('Create', result);
    Gen(OP_FIELD, ClassId, RefId, RefId);
    Gen(OP_ASSIGN, result, RefId, result);

    for I := 0 to AnonymStack.Top.BindList.Count - 1 do
    begin
      RefId := NewField(GetName(AnonymStack.Top.BindList[I]), result);
      Gen(OP_FIELD, result, RefId, RefId);
      Gen(OP_ASSIGN, RefId, AnonymStack.Top.BindList[I], RefId);
    end;
  finally
    AnonymStack.Pop;
  end;

  Gen(OP_ASSIGN_LAMBDA_TYPES, SubId, ClassId, result);
end;

function TPascalParser.Parse_LambdaParameters(SubId: Integer) : Integer;
var
  ID: Integer;
begin
  result := 0;

  if not IsCurrText('(') then
  repeat
    Inc(result);
    ID := Parse_FormalParameter;
    Gen(OP_DECLARE_LOCAL_VAR, SubId, ID, 0);
    SetCount(SubId, result);
    if NotMatch(',') then
      Exit;
  until false;

  Match('(');
  if IsCurrText(')') then
  begin
    Match(')');
    SetCount(SubId, result);
    Exit;
  end;

  repeat
    Inc(result);
    ID := Parse_FormalParameter;
    Gen(OP_DECLARE_LOCAL_VAR, SubId, ID, 0);
    if NotMatch(',') then
      break;
  until false;

  Match(')');
  SetCount(SubId, result);
end;

procedure TPascalParser.Parse_FunctionDeclaration(IsSharedMethod: Boolean = false);
var
  SubId, TypeId, ForwardId: Integer;
  DirectiveList: TIntegerList;
  L: TIntegerList;
  NotDeclared, WaitOverload: Boolean;
  K: Integer;
begin
  DECLARE_SWITCH := true;
  K := 0;
  BeginMethodDef;
  try

    NotDeclared := false;
    WaitOverload := false;

    if IsSharedMethod then
    begin
      ForwardId := ReadType;

      if ForwardId = 0 then
        CreateError(errUndeclaredIdentifier, [CurrToken.Text]);

      Call_SCANNER;
      DECLARE_SWITCH := true;
      Scanner.CurrComment.AllowedDoComment := false;
      Match('.');
      SubId := Parse_Ident;
      BeginClassMethod(SubId, ForwardId, true, true, true);
    end
    else
    begin
      ForwardId := ReadType;

      if (ForwardId > 0) and (GetKind(ForwardId) = KindTYPE) then
      begin
        Call_SCANNER;

        while GetNext2Text = '.' do
        begin
          Inc(K);
          levelStack.Push(ForwardId);
          ReadToken;
          ForwardId := Lookup(CurrToken.Text, CurrLevel);
          if ForwardId = 0 then
             RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
          ReadToken;
        end;

        DECLARE_SWITCH := true;
        Scanner.CurrComment.AllowedDoComment := false;
        Match('.');
        SubId := Parse_Ident;
        if Lookup(GetName(SubId), ForwardId) = 0 then
          NotDeclared := true;
        BeginClassMethod(SubId, ForwardId, true, false, true);
      end
      else
      begin
        if ForwardId > 0 then
        if GetKind(ForwardId) in KindSUBS then
          if not GetSymbolRec(ForwardId).IsForward then
          if GetSymbolRec(ForwardId).OverCount = 0 then
            RaiseError(errRedeclaredIdentifier, [CurrToken.Text])
          else
            WaitOverload := true;

        SubId := NewVar(CurrToken.Text);
        SetPosition(SubId, CurrToken.Position - 1);
        Parse_Ident;
        BeginSub(SubId);
      end;
    end;

    Parse_FormalParameterList(SubId);
    DECLARE_SWITCH := false;

    if IsCurrText(';') then
    begin
      L := LookupForwardDeclarations(SubId);
      if L = nil then
        RaiseError(errUnsatisfiedForwardOrExternalDeclaration, [GetName(SubId)])
      else
        FreeAndNil(L);
    end
    else
    begin
      Match(':');
      Parse_Attribute;
      TypeID := Parse_Type;
      Gen(OP_ASSIGN_TYPE, SubId, TypeID, 0);
      Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
    end;

    DECLARE_SWITCH := true;

    if IsCurrText(';') then
      Match(';');

    if NotDeclared then
      CreateError(errUndeclaredIdentifier, [GetName(SubId)]);

    DirectiveList := Parse_DirectiveList(SubId);
    try
      if DirectiveList.IndexOf(dirFORWARD) >= 0 then
      begin
        SetForward(SubId, true);
        EndSub(SubId);
        Exit;
      end;
      if WaitOverload then
        if DirectiveList.IndexOf(dirOVERLOAD) = -1 then
          CreateError(errOverloadExpected, [GetName(SubId)]);
    finally
      FreeAndNil(DirectiveList);
    end;

    if IsCurrText('external') then
    begin
      ParseExternalSub(SubId);
      Exit;
    end;

    InitSub(SubId);

    if ForwardId > 0 then
      if not GetSymbolRec(ForwardId).IsForward then
        CheckRedeclaredSub(SubId);

    if InitFuncResult then
      Gen(OP_CALL_DEFAULT_CONSTRUCTOR, CurrResultId, 0, 0);

    Parse_SubBlock;
    EndSub(SubId);

    EndMethodDef(SubId);
    Match(';');

  finally

    while K > 0 do
    begin
      Dec(K);
      levelStack.Pop;
    end;

  end;
end;

procedure TPascalParser.Parse_OperatorDeclaration;
var
  I, SubId, TypeId, ForwardId: Integer;
  L: TIntegerList;
  NotDeclared: Boolean;
begin
  NotDeclared := false;

  ReadToken;
  ForwardId := Lookup(CurrToken.Text, CurrLevel);
  if ForwardId = 0 then
    CreateError(errUndeclaredIdentifier, [CurrToken.Text]);

  Call_SCANNER;
  DECLARE_SWITCH := true;
  Match('.');
  I := OperatorIndex(CurrToken.Text);
  if I = -1 then
    CreateError(errE2393, []);
        // errE2393 = 'Invalid operator declaration';
  SubId := Parse_Ident;
  SetName(SubId, operators.Values[I]);
  BeginStructureOperator(SubId, ForwardId);

  Parse_FormalParameterList(SubId);
  DECLARE_SWITCH := false;

  if IsCurrText(';') then
  begin
    L := LookupForwardDeclarations(SubId);
    if L = nil then
      RaiseError(errUnsatisfiedForwardOrExternalDeclaration, [GetName(SubId)])
    else
      FreeAndNil(L);
  end
  else
  begin
    Match(':');
    Parse_Attribute;
    TypeID := Parse_Type;
    Gen(OP_ASSIGN_TYPE, SubId, TypeID, 0);
    Gen(OP_ASSIGN_TYPE, CurrResultId, TypeID, 0);
  end;

  DECLARE_SWITCH := true;
  Match(';');

  if NotDeclared then
    CreateError(errUndeclaredIdentifier, [GetName(SubId)]);

  if IsCurrText('external') then
  begin
    ParseExternalSub(SubId);
    Exit;
  end;

  InitSub(SubId);

  if ForwardId > 0 then
    if not GetSymbolRec(ForwardId).IsForward then
      if not StrEql(GetName(SubId), pascal_Implicit) then
      if not StrEql(GetName(SubId), pascal_Explicit) then
        CheckRedeclaredSub(SubId);

  Parse_SubBlock;
  EndSub(SubId);
  Match(';');
end;

procedure TPascalParser.Parse_ConstructorDeclaration;
var
  ClassTypeId, SubId, L: Integer;
  DirectiveList: TIntegerList;
  OldSubId: Integer;
  K, ForwardId: Integer;
begin
  DECLARE_SWITCH := true;
  K := 0;
  ClassTypeId := 0;
  BeginMethodDef;
  try
    ForwardId := ReadType;

    if (ForwardId > 0) and (GetKind(ForwardId) = KindTYPE) then
    begin
      Call_SCANNER;

      while GetNext2Text = '.' do
      begin
        Inc(K);
        levelStack.Push(ForwardId);
        ReadToken;
        ForwardId := Lookup(CurrToken.Text, CurrLevel);
        if ForwardId = 0 then
           RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
        ReadToken;
      end;

      ClassTypeId := ForwardId;
      DECLARE_SWITCH := true;
      Match('.');
      SubId := Parse_Ident;
      if GetSymbolRec(ClassTypeId).FinalTypeId = typeRECORD then
        BeginStructureConstructor(SubId, ClassTypeId)
      else
        BeginClassConstructor(SubId, ClassTypeId);
    end
    else
      RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);

    Parse_FormalParameterList(SubId);

    Inc(EXECUTABLE_SWITCH);
    Match(';');

    DirectiveList := Parse_DirectiveList(SubId);
    if DirectiveList.IndexOf(dirFORWARD) >= 0 then
    begin
      SetForward(SubId, true);
      EndSub(SubId);
      FreeAndNil(DirectiveList);

      Dec(EXECUTABLE_SWITCH);
      Exit;
    end;
    FreeAndNil(DirectiveList);

    OldSubId := SubId;
    InitSub(SubId);

    if OldSubId = SubId then
      RaiseError(errUndeclaredIdentifier, [GetName(OldSubId)]);

    if GetSymbolRec(ClassTypeId).FinalTypeId = typeRECORD then
    begin
      Parse_SubBlock;
    end
    else
    begin

      WasInherited := false;

      Gen(OP_SAVE_EDX, 0, 0, 0);
      L := NewLabel;
      Gen(OP_GO_DL, L, 0, 0);
      Gen(OP_CREATE_OBJECT, ClassTypeId, 0, CurrSelfId);
      SetLabelHere(L);

      Parse_SubBlock;

//      if not WasInherited then
//        CreateError(errTheCallOfInheritedConstructorIsMandatory, []);

      Gen(OP_RESTORE_EDX, 0, 0, 0);
      L := NewLabel;
      Gen(OP_GO_DL, L, 0, 0);
      Gen(OP_ON_AFTER_OBJECT_CREATION, CurrSelfId, 0, 0);
      SetLabelHere(L);
    end;

    EndSub(SubId);

    Dec(EXECUTABLE_SWITCH);

    EndMethodDef(SubId);
    Match(';');

  finally

    while K > 0 do
    begin
      Dec(K);
      levelStack.Pop;
    end;

  end;
end;

procedure TPascalParser.Parse_DestructorDeclaration;
var
  ClassTypeId, SubId, NP: Integer;
  DirectiveList: TIntegerList;
  OldSubId: Integer;
  K: Integer;
begin
  DECLARE_SWITCH := true;
  K := 0;
  BeginMethodDef;
  try

    ClassTypeId := ReadType;

    if (ClassTypeId > 0) and (GetKind(ClassTypeId) = KindTYPE) then
    begin
      Call_SCANNER;

      while GetNext2Text = '.' do
      begin
        Inc(K);
        levelStack.Push(ClassTypeId);
        ReadToken;
        ClassTypeId := Lookup(CurrToken.Text, CurrLevel);
        if ClassTypeId = 0 then
           RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);
        ReadToken;
      end;

      DECLARE_SWITCH := true;
      Match('.');
      SubId := Parse_Ident;
      BeginClassDestructor(SubId, ClassTypeId);
    end
    else
      RaiseError(errUndeclaredIdentifier, [CurrToken.Text]);

    NP := 0;
    if IsCurrText('(') then
    begin
      Call_SCANNER;
      Match(')');
    end;

    SetCount(SubId, NP);

    Inc(EXECUTABLE_SWITCH);
    Match(';');

    DirectiveList := Parse_DirectiveList(SubId);
    if DirectiveList.IndexOf(dirFORWARD) >= 0 then
    begin
      SetForward(SubId, true);
      EndSub(SubId);
      FreeAndNil(DirectiveList);

      Dec(EXECUTABLE_SWITCH);
      Exit;
    end;
    FreeAndNil(DirectiveList);

    OldSubId := SubId;
    InitSub(SubId);

    if OldSubId = SubId then
      RaiseError(errUndeclaredIdentifier, [GetName(OldSubId)]);

    Parse_SubBlock;
    EndSub(SubId);

    Dec(EXECUTABLE_SWITCH);
    EndMethodDef(SubId);
    Match(';');

  finally

    while K > 0 do
    begin
      Dec(K);
      levelStack.Pop;
    end;

  end;
end;

// STATEMENTS

procedure TPascalParser.Parse_CompoundStmt;
begin
  Inc(EXECUTABLE_SWITCH);
  DECLARE_SWITCH := false;
  Match('begin');
  Parse_StmtList;
  Match('end');
  Dec(EXECUTABLE_SWITCH);
end;

procedure TPascalParser.Parse_StmtList;
begin
  DECLARE_SWITCH := false;
  repeat
    if IsEOF then
      break;
    if IsCurrText('end') then
      break;
    if IsCurrText('finalization') then
      break;
    Parse_Statement;
    if NotMatch(';') then break;
  until false;
end;

procedure TPascalParser.Parse_AssignmentStmt;
var
  I, LeftID, RightId, SizeId, SubId, L, ID1, ID2: Integer;
  R: TCodeRec;
  Lst: TIntegerList;
//  SignProp: Boolean;
  K1: Integer;
begin
  if IsCurrText('inherited') then
  begin
    Call_SCANNER;
    LeftId := NewTempVar;
    if IsCurrText(';') or IsCurrText('else') then
    begin
      SubId := CurrLevel;
      L := NewTempVar;
      SetName(L, GetName(SubId));
      Gen(OP_EVAL, 0, 0, L);
      Gen(OP_EVAL_INHERITED, L, 0, LeftId);
      for I:=0 to GetCount(SubId) - 1 do
        Gen(OP_PUSH, GetParamId(SubId, I), I, LeftId);
      Gen(OP_CALL_INHERITED, LeftID, 0, 0);
    end
    else
    begin
      L := Parse_Ident;

      if IsCurrText('[') then
      begin
//        SignProp := true;
        RemoveInstruction(OP_EVAL, -1, -1, L);
      end
      else
      begin
        RemoveInstruction(OP_EVAL, -1, -1, L);
//        SignProp := false;
      end;

      Gen(OP_EVAL_INHERITED, L, 0, LeftId);
      if IsCurrText('(') or IsCurrText('[') then
        Gen(OP_CALL_INHERITED, LeftID, Parse_ArgumentList(LeftId), 0)
      else
        Gen(OP_CALL_INHERITED, LeftID, 0, 0);

//      if SignProp then
      if IsCurrText(':=') then
      begin
        K1 := CodeCard;
        Call_SCANNER;
        Gen(OP_PUSH, Parse_Expression, GetCodeRec(K1).Arg2, LeftId);
        GetCodeRec(K1).Arg2 := GetCodeRec(K1).Arg2 + 1;
        Gen(OP_CALL, LeftId, GetCodeRec(K1).Arg2, 0);
        GetCodeRec(K1).Op := OP_NOP;
        Exit;
      end;

    end;
    if GetKind(CurrSubId) = kindCONSTRUCTOR then
    begin
      Gen(OP_RESTORE_EDX, 0, 0, 0);

      L := NewLabel;
      Gen(OP_GO_DL, L, 0, 0);
      Gen(OP_ONCREATE_OBJECT, CurrSelfId, 0, 0);
      SetLabelHere(L);

      Gen(OP_SAVE_EDX, 0, 0, 0);

      WasInherited := true;
    end;
    Exit;
  end
  else if IsCurrText('Include') and (not InScope('Include')) then
  begin
    RemoveInstruction(OP_EVAL, -1, -1, -1);
    Call_SCANNER;
    Match('(');
    ID1 := Parse_Expression;
    Match(',');
    ID2 := Parse_Expression;
    Match(')');
    Gen(OP_SET_INCLUDE, ID1, ID2, 0);
    Exit;
  end
  else if IsCurrText('Exclude') and (not InScope('Exclude')) then
  begin
    RemoveInstruction(OP_EVAL, -1, -1, -1);
    Call_SCANNER;
    Match('(');
    ID1 := Parse_Expression;
    Match(',');
    ID2 := Parse_Expression;
    Match(')');
    Gen(OP_SET_EXCLUDE, ID1, ID2, 0);
    Exit;
  end
  else if IsCurrText('inc') and (not InScope('inc')) then
  begin
    Call_SCANNER;
    if not IsCurrText('(') then
      RaiseError(errTokenExpected, ['(', CurrToken.Text]);
    Push_SCANNER;
    Call_SCANNER;
    ID1 := Parse_Designator;
    Pop_SCANNER;
    Call_SCANNER;
    ID2 := Parse_Designator;
    if IsCurrText(',') then
    begin
      Call_SCANNER;
      Gen(OP_INC, ID2, Parse_Expression, ID1);
    end
    else
      Gen(OP_INC, ID2, NewConst(typeINTEGER, 1), ID1);
    Match(')');

    Exit;
  end
  else if IsCurrText('dec') and (not InScope('dec')) then
  begin
    Call_SCANNER;
    if not IsCurrText('(') then
      RaiseError(errTokenExpected, ['(', CurrToken.Text]);
    Push_SCANNER;
    Call_SCANNER;
    ID1 := Parse_Designator;
    Pop_SCANNER;
    Call_SCANNER;
    ID2 := Parse_Designator;
    if IsCurrText(',') then
    begin
      Call_SCANNER;
      Gen(OP_DEC, ID2, Parse_Expression, ID1);
    end
    else
      Gen(OP_DEC, ID2, NewConst(typeINTEGER, 1), ID1);
    Match(')');

    Exit;
  end
  else if IsCurrText('SetLength') and (not InScope('SetLength')) then
  begin
    Lst := TIntegerList.Create;
    try
      Call_SCANNER;
      Match('(');
      LeftID := Parse_Designator;

      Call_SCANNER;
      repeat
        Lst.Add(Parse_Expression);
        if NotMatch(',') then
          break;
      until false;

      if Lst.Count = 1 then
        Gen(OP_SET_LENGTH, LeftID, Lst[0], 0)
      else
      begin
        for I := 0 to Lst.Count - 1 do
          Gen(OP_PUSH_LENGTH, Lst[I], 0, 0);
        Gen(OP_SET_LENGTH_EX, LeftID, Lst.Count, 0);
      end;

      Match(')');
    finally
      FreeAndNil(Lst);
    end;
    Exit;
  end
  else if IsCurrText('str') and (not InScope('str')) then
  begin
    LeftID := NewTempVar;

    Call_SCANNER;
    Match('(');

    try

    Gen(OP_PUSH, Parse_Expression, 3, LeftID);

    if IsCurrText(':') then
    begin
      Call_SCANNER;
      Gen(OP_PUSH, Parse_Expression, 2, LeftID);
    end
    else
      Gen(OP_PUSH, NewConst(typeINTEGER, 0), 2, LeftID);

    if IsCurrText(':') then
    begin
      Call_SCANNER;
      Gen(OP_PUSH, Parse_Expression, 1, LeftID);
    end
    else
      Gen(OP_PUSH, NewConst(typeINTEGER, 0), 1, LeftID);

    Match(',');
    Gen(OP_PUSH, Parse_Expression, 0, LeftID);

    finally
    Gen(OP_STR, LeftID, 0, 0);
    end;

    Match(')');
    Exit;
  end
  else if IsCurrText('new') and (not InScope('new')) then
  begin
    SetCompletionTarget('new');

    Call_SCANNER;

    Match('(');
    LeftId := Parse_Designator;
    SizeId := NewTempVar;
    SubId := NewTempVar;
    SetName(SubId, 'GetMem');
    SetKind(SubId, kindNONE);
    Gen(OP_EVAL, 0, 0, SubId);
    Gen(OP_SIZEOF, LeftId, 0, SizeId);
    Gen(OP_PUSH, LeftId, 0, SubId);
    Gen(OP_PUSH, SizeId, 1, SubId);
    Gen(OP_CALL, SubId, 0, 0);
    Match(')');
    Exit;
  end
  else if IsCurrText('dispose') and (not InScope('dispose')) then
  begin
    SetCompletionTarget('Dispose');

    Call_SCANNER;
    Match('(');
    LeftId := Parse_Designator;
    SizeId := NewTempVar;
    SubId := NewTempVar;
    SetName(SubId, 'FreeMem');
    SetKind(SubId, kindNONE);
    Gen(OP_EVAL, 0, 0, SubId);
    Gen(OP_SIZEOF, LeftId, 0, SizeId);
    Gen(OP_PUSH, LeftId, 0, SubId);
    Gen(OP_PUSH, SizeId, 1, SubId);
    Gen(OP_CALL, SubId, 0, 0);
    Match(')');
    Exit;
  end
  else if IsCurrText('pause') and (not InScope('pause')) then
  begin
    Call_SCANNER;
    if IsCurrText('(') then
    begin
      Match('(');
      Match(')');
    end;
    L := NewLabel;
    Gen(OP_PAUSE, L, 0, 0);
    SetLabelHere(L);
    Exit;
  end
  else if IsCurrText('halt') or IsCurrText('abort') then
  begin
    Call_SCANNER;
    if IsCurrText('(') then
    begin
      Match('(');
      if not IsCurrText(')') then
      begin
        Gen(OP_HALT, Parse_ConstantExpression, 0, 0);
      end
      else
        Gen(OP_HALT, NewConst(typeINTEGER, 0), 0, 0);
      Match(')');
    end
    else
      Gen(OP_HALT, NewConst(typeINTEGER, 0), 0, 0);
    Exit;
  end;

  if IsCurrText('(') then
    LeftID := Parse_Factor
  else
    LeftID := Parse_SimpleExpression;

  if IsEOF then
    Exit;

  if IsCurrText(';') or (CurrToken.TokenClass = tcKeyword) then
  begin
    R := LastCodeRec;
    if R.Op = OP_CALL then
    begin
      SetKind(R.Res, KindNONE);
      R.Res := 0;
    end
    else if GetKind(LeftId) = kindCONST then
      RaiseError(errIdentifierExpectedNoArgs, [])
    else
    begin
      {$IFDEF CPP_SYN}
      if (R.Arg1 = LeftId) and (R.Op = OP_ASSIGN) then
      begin
        if (LastCodeRec2.Op = OP_PLUS) or (LastCodeRec2.Op = OP_MINUS) then
          Exit;
      end
      else if R.Op = OP_POSTFIX_EXPRESSION then
        Exit;
      {$ENDIF}
      Gen(OP_CALL, LeftID, 0, 0);
    end;
    Exit;
  end;

  Gen(OP_LVALUE, LeftId, 0, 0);

  if IsCurrText(':=') then
  begin
    Call_SCANNER;
    RightId := Parse_Expression;
    Gen(OP_ASSIGN, LeftID, RightId, LeftID);
  end
{$IFDEF CPP_SYN}
  else if IsCurrText('+=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_PLUS, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('-=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_MINUS, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('*=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_MULT, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('/=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_DIV, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('~=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_IDIV, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('%=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_MOD, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('^=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_XOR, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('|=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_OR, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
  else if IsCurrText('&=') then
  begin
    Call_SCANNER;
		ID1 := NewTempVar;
  	Gen(OP_AND, LeftId, Parse_Expression, ID1);
		Gen(OP_ASSIGN, LeftId, ID1, LeftId);
  end
{$ENDIF}
  else if IsCurrText('(') then
  begin
    R := Gen(OP_CALL, LeftID, Parse_ArgumentList(LeftId), 0);

    if IsCurrText(':=') then
    begin
      R.Res := NewTempVar;
      Call_SCANNER;
      Gen(OP_ASSIGN, R.Res, Parse_Expression, R.Res);
    end
    else if IsCurrText('(') then
    begin
      R.Res := NewTempVar;
      Gen(OP_CALL, R.Res, Parse_ArgumentList(R.Res), 0);
    end;
  end
  else
  begin
    Gen(OP_CALL, LeftID, 0, 0);
  end;
end;

procedure TPascalParser.Parse_CaseStmt;
var
  lg, lf, lt, lc, id, expr1_id, cond_id: Integer;
begin
  Match('case');
  lg := NewLabel;
  cond_id := NewTempVar;
  id := NewTempVar;
  Gen(OP_ASSIGN, Id, Parse_Expression, id);
  Match('of');
  repeat
    // Parse case selector
    lt := NewLabel;
    lf := NewLabel;
    repeat
      lc := NewLabel;
      expr1_id := Parse_ConstantExpression;

      if IsCurrText('..') then
      begin
        Gen(OP_GE, id, expr1_id, cond_id);
        Gen(OP_GO_FALSE, lc, cond_id, 0);
        Match('..');
        Gen(OP_LE, id, Parse_ConstantExpression, cond_id);
        Gen(OP_GO_FALSE, lc, cond_id, 0);
      end
      else
        Gen(OP_EQ, id, expr1_id, cond_id);
      Gen(OP_GO_TRUE, lt, cond_id, 0);
      SetLabelHere(lc);

      if NotMatch(',') then
        break;
    until false;
    Gen(OP_GO, lf, 0, 0);
    SetLabelHere(lt);
    Match(':');
    if IsCurrText(';') then
    begin
    end
    else
      Parse_Statement;
    Gen(OP_GO, lg, 0, 0);
    SetLabelHere(lf);
    // end of case selector
    if NotMatch(';') then
      Break;
    if IsCurrText('else') then
      break;
    if IsCurrText('end') then
      break;
  until false;
  if IsCurrText('else') then
  begin
    Match('else');
    Parse_StmtList;
  end;
  if IsCurrText(';') then
    Match(';');
  Match('end');
  SetLabelHere(lg);
end;

procedure TPascalParser.Parse_IfStmt;
var
  lf, lg: Integer;
begin
  Match('if');
  lf := NewLabel;
  Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
  Match('then');
  if not IsCurrText('else') then
    Parse_Statement;
  if IsCurrText('else') then
  begin
		Gen(OP_NOP, 0, 0, 0);
		lg := NewLabel();
		Gen(OP_GO, lg, 0, 0);
  	SetLabelHere(lf);
    Match('else');
    Parse_Statement;
		SetLabelHere(lg);
  end
  else
    SetLabelHere(lf);
end;

procedure TPascalParser.Parse_GotoStmt;
begin
  Match('goto');
  Gen(OP_GO, Parse_Label, 0, 0);
end;

procedure TPascalParser.Parse_BreakStmt;
begin
	if BreakStack.Count = 0 then
    RaiseError(errBreakOrContinueOutsideOfLoop, []);
	Match('break');
  if IsCurrText('(') then
  begin
    Match('(');
    Match(')');
  end;
  if not SupportedSEH then
  	Gen(OP_GO, BreakStack.TopLabel, 0, 0)
  else
  begin
    if IsTryContext(BreakStack.Top) then
      Gen(OP_EXIT, BreakStack.TopLabel, Integer(emBreak), CurrLevel)
    else
      Gen(OP_GO, BreakStack.TopLabel, 0, 0);
  end;
end;

procedure TPascalParser.Parse_ContinueStmt;
begin
	if ContinueStack.Count = 0 then
    RaiseError(errBreakOrContinueOutsideOfLoop, []);
	Match('continue');
  if IsCurrText('(') then
  begin
    Match('(');
    Match(')');
  end;
  if not SupportedSEH then
  	Gen(OP_GO, ContinueStack.TopLabel, 0, 0)
  else
  begin
    if IsTryContext(ContinueStack.Top) then
      Gen(OP_EXIT, ContinueStack.TopLabel, Integer(emContinue), CurrLevel)
    else
      Gen(OP_GO, ContinueStack.TopLabel, 0, 0);
  end;
end;

procedure TPascalParser.Parse_ExitStmt;
begin
	Match('exit');
  if IsCurrText('(') then
  begin
    Match('(');
    Match(')');
  end;
  if not SupportedSEH then
    Gen(OP_GO, SkipLabelStack.Top, 0, CurrLevel)
  else
    Gen(OP_EXIT, SkipLabelStack.Top, 0, CurrLevel);
end;

procedure TPascalParser.Parse_WhileStmt;
var
  lf, lg, l_loop: Integer;
begin
	Match('while');
  lf := NewLabel;
	lg := NewLabel;
	SetLabelHere(lg);
  l_loop := lg;
	Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
	Match('do');

  Parse_LoopStmt(lf, lg, l_loop);

	Gen(OP_GO, lg, 0, 0);
  SetLabelHere(lf);
end;

procedure TPascalParser.Parse_RepeatStmt;
var
  lf, lg, l_loop: Integer;
begin
	Match('repeat');
  lf := NewLabel;
	lg := NewLabel;
  SetLabelHere(lf);
  l_loop := lf;
  repeat
    if IsCurrText('until') then
      Break;
    if IsEOF then
      Break;

    Parse_LoopStmt(lg, lf, l_loop);

    if NotMatch(';') then
      Break;

  until false;
  Match('until');
	Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
  SetLabelHere(lg);
end;

procedure TPascalParser.Parse_ForStmt;
var
  id, expr1_id, expr2_id, limit_cond_id1, limit_cond_id2: Integer;
  i, compound: Boolean;
  lf, lg, lc, l_loop: Integer;

  element_id, collection_id, enumerator_id, bool_id: Integer;
begin
  l_loop := NewLabel;
  SetLabelHere(l_loop);
  Match('for');
  if IsNextText('in') then
  begin
    Inc(ForInCounter);
    lf := NewLabel;
	  lg := NewLabel;
  	lc := NewLabel;
    enumerator_id := NewTempVar;
    bool_id := NewTempVar;
    element_id := Parse_Ident;
    Match('in');
    collection_id := Parse_Expression;
    Match('do');
    Gen(OP_LOCK_VARRAY, collection_id, ForInCounter, 0);
    Gen(OP_GET_ENUMERATOR, collection_id, ForInCounter, enumerator_id);
    SetLabelHere(lg);
    Gen(OP_CURRENT, enumerator_id, ForInCounter, element_id);

    compound := Parse_LoopStmt(lf, lc, l_loop);

    SetLabelHere(lc, ForInCounter);
    if not compound then
      GenPause;
    Gen(OP_MOVE_NEXT, element_id, ForInCounter, bool_id);
    Gen(OP_GO_FALSE, lf, bool_id, 0);
    Gen(OP_GO, lg, 0, 0);
    SetLabelHere(lf, 0, ForInCounter);
    Gen(OP_UNLOCK_VARRAY, collection_id, ForInCounter, 0);
    Exit;
  end;

  lf := NewLabel;
	lg := NewLabel;
	lc := NewLabel;
  limit_cond_id1 := NewTempVar;
  limit_cond_id2 := NewTempVar;
  expr1_id := NewTempVar;
  expr2_id := NewTempVar;
  id := Parse_Ident;
  Match(':=');
  Gen(OP_ASSIGN, expr1_id, Parse_Expression, expr1_id);
  Gen(OP_ASSIGN, id, expr1_id, id);
  if IsCurrText('downto') then
  begin
    Match('downto');
    Gen(OP_ASSIGN, expr2_id, Parse_Expression, expr2_id);
    Gen(OP_LT, id, expr2_id, limit_cond_id1);
    i := false;
  end
  else
  begin
    Match('to');
    Gen(OP_ASSIGN, expr2_id, Parse_Expression, expr2_id);
    Gen(OP_GT, id, expr2_id, limit_cond_id1);
    i := true;
  end;
	Gen(OP_GO_TRUE, lg, limit_cond_id1, 0);
  Match('do');
  SetLabelHere(lf);

  compound := Parse_LoopStmt(lg, lc, l_loop);

  SetLabelHere(lc);
  if i then
  begin
    Gen(OP_INC, id, NewConst(typeINTEGER, 1), id);
    Gen(OP_GT, id, expr2_id, limit_cond_id2);
  end
  else
  begin
    Gen(OP_DEC, id, NewConst(typeINTEGER, 1), id);
    Gen(OP_LT, id, expr2_id, limit_cond_id2);
  end;
  if not compound then
    GenPause;
	Gen(OP_GO_FALSE, lf, limit_cond_id2, 0);
  SetLabelHere(lg);
end;

procedure TPascalParser.Parse_WithStmt;
var
  id, K: Integer;
begin
  K := WithStack.Count;
  Match('with');
  repeat
    id := Parse_Expression;
    Gen(OP_BEGIN_WITH, id, 0, 0);
    WithStack.Push(id);
    if NotMatch(',') then
      Break;
  until false;
  Match('do');
  Parse_Statement;

  while WithStack.Count > K do
  begin
    id := WithStack.Top;
    Gen(OP_END_WITH, id, 0, 0);
    WithStack.Pop;
  end;
end;

procedure TPascalParser.Parse_TryStmt;
var
  id, type_id, l_try, BlockId: Integer;
begin
  if not SupportedSEH then
    RaiseError(errTryExceptNotImplemented, []);

  l_try := GenBeginTry;

  Match('try');

  repeat
    if IsCurrText('except') then
      Break;
    if IsCurrText('finally') then
      Break;
    if IsEOF then
      Break;
    Parse_Statement;
    if NotMatch(';') then
      Break;
  until false;

  Gen(OP_EXCEPT_SEH, 0, 0, 0);

  if IsCurrText('except') then
  begin
    Gen(OP_GO, l_try, 0, 0);
    GenExcept;

    Call_SCANNER;
    //ExceptionBlock

    if IsCurrText('on') then
    begin
      while IsCurrText('on') do
      begin
        BlockId := NewTempVar;
        LevelStack.push(BlockId);
        Gen(OP_BEGIN_BLOCK, BlockId, 0, 0);

        if IsNext2Text(':') then
        begin
          DECLARE_SWITCH := true;
          Match('on');
          id := Parse_Ident;
          DECLARE_SWITCH := false;
          Match(':');
          type_id := Parse_Ident;
        end
        else
        begin
          DECLARE_SWITCH := false;
          Match('on');
          type_id := Parse_Ident;
          id := NewTempVar;
        end;

        Gen(OP_ASSIGN_TYPE, id, type_id, 0);

        GenExceptOn(type_id);
        Gen(OP_ASSIGN, id, CurrExceptionObjectId, id);

        Gen(OP_BEGIN_EXCEPT_BLOCK, 0, 0, 0);
        Match('do');
        Parse_Statement;
        Gen(OP_END_EXCEPT_BLOCK, 0, 0, 0);

        Gen(OP_GO, l_try, 0, 0);

        Gen(OP_END_BLOCK, BlockId, 0, 0);
        LevelStack.Pop;
        if IsCurrText(';') then
          Match(';');
      end;

      GenExceptOn(0);

      if IsCurrText('else') then
      begin
        Gen(OP_BEGIN_EXCEPT_BLOCK, 0, 0, 0);
        Call_SCANNER;
        Parse_Statement;

        if IsCurrText(';') then
          Match(';');
        Gen(OP_END_EXCEPT_BLOCK, 0, 0, 0);
      end;
    end
    else
    begin
      Gen(OP_BEGIN_EXCEPT_BLOCK, 0, 0, 0);
      repeat
        if IsCurrText('end') then
          Break;
        if IsEOF then
          Break;
        Parse_Statement;
        if NotMatch(';') then
          Break;
      until false;
      Gen(OP_END_EXCEPT_BLOCK, 0, 0, 0);
    end;
  end // except
  else if IsCurrText('finally') then
  begin
    GenFinally;
    Call_SCANNER;
    repeat
      if IsCurrText('end') then
        Break;
      if IsEOF then
        Break;
      Parse_Statement;
      if NotMatch(';') then
        Break;
    until false;
    GenCondRaise;
  end // finally
  else
    Match('finally');
  SetLabelHere(l_try);
  GenEndTry;
  Match('end');
end;

procedure TPascalParser.Parse_RaiseStmt;
begin
  if not SupportedSEH then
    RaiseError(errRaiseNotImplemented, []);

  Match('raise');
  if IsCurrText(';') then
    Gen(OP_RAISE, 0, RaiseMode, 0)
  else
  begin
    Gen(OP_RAISE, Parse_Expression, RaiseMode, 0);
  end;
end;

// EXPRESSIONS

function TPascalParser.Parse_ArgumentList(SubId: Integer): Integer;
var
  I: Integer;
  L: TIntegerList;
  bracket: String;
begin
  try
    bracket := ')';
    L := TIntegerList.Create;
    try
      if IsCurrText('(') then
      begin
        Match('(');
        bracket := ')';
      end
      else if IsCurrText('[') then
      begin
        Match('[');
        bracket := ']';
      end
      else
        Match('(');
      result := 0;
      if (not IsCurrText(')')) then
      begin
        repeat
          Inc(result);
          L.Add(Parse_Expression);
          if NotMatch(',') then
            Break;
        until false;
      end;

      for I:=0 to L.Count - 1 do
        Gen(OP_PUSH, L[I], I, SubID);

      Match(bracket);
    finally
      FreeAndNil(L);
    end;
  except
     Gen(OP_CALL, SubId, 0, 0);
     raise;
  end;
end;

function TPascalParser.Parse_ConstantExpression: Integer;
begin
  try
    CONST_ONLY := true;
    result := Parse_Expression;
  finally
    CONST_ONLY := false;
  end;
end;

function TPascalParser.Parse_Expression: Integer;
var
  Op: Integer;
begin
  if IsCurrText('procedure') then
  begin
    result := Parse_AnonymousProcedure;
    Exit;
  end
  else if IsCurrText('function') then
  begin
    result := Parse_AnonymousFunction;
    Exit;
  end
  else if IsCurrText('lambda') then
  begin
    RemoveLastIdent(CurrToken.Id);
    result := Parse_LambdaExpression;
    Exit;
  end;

  result := Parse_SimpleExpression;
  while (CurrToken.Id = OP_LT) or
        (CurrToken.Id = OP_LE) or
        (CurrToken.Id = OP_GT) or
        (CurrToken.Id = OP_GE) or
        (CurrToken.Id = OP_EQ) or
        (CurrToken.Id = OP_NE) or
        (CurrToken.Id = OP_IS) or
        (CurrToken.Id = OP_AS) or
        (CurrToken.Id = OP_SET_MEMBERSHIP) do
  begin
    Op := CurrToken.Id;
    Call_SCANNER;
		result := BinOp(Op, result, Parse_SimpleExpression);
  end;
end;

function TPascalParser.Parse_SimpleExpression: Integer;
var
  Op, L, I: Integer;
  Lst: TCodeRecList;
  R: TCodeRec;
begin
  if CompleteBooleanEval then
  begin
    result := Parse_Term;
    while IsCurrText('+') or
          IsCurrText('-') or
          IsCurrText('or') or
          IsCurrText('xor') do
    begin
      Op := CurrToken.Id;
      Call_SCANNER;
      result := BinOp(Op, result, Parse_Term);
    end;
    Exit;
  end;

  L := 0;
  Lst := TCodeRecList.Create;
  try
    result := Parse_Term;
    while (CurrToken.Id = OP_PLUS) or
          (CurrToken.Id = OP_MINUS) or
          (CurrToken.Id = OP_OR) or
          (CurrToken.Id = OP_XOR) do
    begin
      if (CurrToken.Id = OP_OR) and (Lst.Count = 0) then
        L := NewLabel;

      if CurrToken.Id = OP_OR then
      begin
        R := Gen(OP_ASSIGN, 0, result, 0);
        Lst.Add(R);
        Gen(OP_GO_TRUE_BOOL, L, result, 0);
      end;

      Op := CurrToken.Id;
      Call_SCANNER;
      result := BinOp(Op, result, Parse_Term);
    end;

    if Lst.Count > 0 then
    begin
      for I:=0 to Lst.Count - 1 do
      begin
        R := TCodeRec(Lst[I]);
        R.Arg1 := result;
        R.Res := result;
      end;
      SetLabelHere(L);
    end;
  finally
    FreeAndNil(Lst);
  end;
end;

function TPascalParser.Parse_Term: Integer;
var
  Op, L, I: Integer;
  Lst: TCodeRecList;
  R: TCodeRec;
begin
  if CompleteBooleanEval then
  begin
    result := Parse_Factor;
    while (CurrToken.Id = OP_MULT) or
          (CurrToken.Id = OP_DIV) or
          (CurrToken.Id = OP_IDIV) or
          (CurrToken.Id = OP_MOD) or
          (CurrToken.Id = OP_SHL) or
          (CurrToken.Id = OP_SHR) or
          (CurrToken.Id = OP_AND) do
    begin
      Op := CurrToken.Id;
      Call_SCANNER;
      result := BinOp(Op, result, Parse_Factor);
    end;
    Exit;
  end;

  L := 0;
  Lst := TCodeRecList.Create;
  try
    result := Parse_Factor;
    while (CurrToken.Id = OP_MULT) or
          (CurrToken.Id = OP_DIV) or
          (CurrToken.Id = OP_IDIV) or
          (CurrToken.Id = OP_MOD) or
          (CurrToken.Id = OP_SHL) or
          (CurrToken.Id = OP_SHR) or
          (CurrToken.Id = OP_AND) do
    begin
      if (CurrToken.Id = OP_AND) and (Lst.Count = 0) then
        L := NewLabel;

      if CurrToken.Id = OP_AND then
      begin
        R := Gen(OP_ASSIGN, 0, result, 0);
        Lst.Add(R);
        Gen(OP_GO_FALSE_BOOL, L, result, 0);
      end;

      Op := CurrToken.Id;
      Call_SCANNER;
      result := BinOp(Op, result, Parse_Factor);
    end;

    if Lst.Count > 0 then
    begin
      for I:=0 to Lst.Count - 1 do
      begin
        R := TCodeRec(Lst[I]);
        R.Arg1 := result;
        R.Res := result;
      end;
      SetLabelHere(L);
    end;

  finally
    FreeAndNil(Lst);
  end;
end;

function TPascalParser.Parse_Factor: Integer;
var
  SubId, K, Id: Integer;
  ValidConst: Boolean;
  {$IFDEF CPP_SYN}
  temp, r: Integer;
  {$ENDIF}
  S: String;
  v: Variant;
label
  LabelDesignator;
begin

  if CurrToken.TokenClass = tcBooleanConst then
  begin
    result := Parse_BooleanLiteral;
    if IsCurrText('.') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);
      result := Parse_Designator(result);
    end;
  end
  else if CurrToken.TokenClass = tcCharConst then
  begin
    result := Parse_CharLiteral;
    if IsCurrText('.') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);
      result := Parse_Designator(result);
    end;
  end
  else if CurrToken.TokenClass = tcPCharConst then
  begin
    result := Parse_PCharLiteral;
    if IsCurrText('.') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);
      result := Parse_Designator(result);
    end;
  end
  else if CurrToken.TokenClass = tcIntegerConst then
  begin
    result := Parse_IntegerLiteral;
    if IsCurrText('.') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);
      result := Parse_Designator(result);
    end;
  end
  else if CurrToken.TokenClass = tcDoubleConst then
  begin
    result := Parse_DoubleLiteral;
    if IsCurrText('.') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);
      result := Parse_Designator(result);
    end;
  end
	else if IsCurrText('+') then
  begin
		Call_SCANNER;
		result := UnaryOp(OP_POSITIVE, Parse_Factor);
  end
	else if IsCurrText('-') then
  begin
		Call_SCANNER;
    ValidConst := CurrToken.TokenClass in [tcIntegerConst, tcDoubleConst];
    Id := Parse_Factor;
    if ValidConst then
    begin
      result := Id;
      v := GetValue(id);
      if v > 0 then
       SetValue(Id, - v);
    end
    else
  		result := UnaryOp(OP_NEG, Id);
  end
  {$IFDEF CPP_SYN}
	else if IsCurrText('++') then
  begin
    if CONST_ONLY then
      CreateError(errConstantExpressionExpected, []);

    Call_SCANNER;
    result := Parse_Expression;
    Id := NewTempVar;
    Gen(OP_PLUS, result, NewConst(typeINTEGER, 1), Id);
    Gen(OP_ASSIGN, result, Id, result);
  end
	else if IsCurrText('--') then
  begin
    if CONST_ONLY then
      CreateError(errConstantExpressionExpected, []);

    Call_SCANNER;
    result := Parse_Expression;
    Id := NewTempVar;
    Gen(OP_MINUS, result, NewConst(typeINTEGER, 1), Id);
    Gen(OP_ASSIGN, result, Id, result);
  end
  {$ENDIF}
	else if IsCurrText('*') then
  begin
		Call_SCANNER;
		result := UnaryOp(OP_POSITIVE, Parse_Factor);
  end
	else if IsCurrText('not') then
  begin
		Call_SCANNER;
		result := UnaryOp(OP_NOT, Parse_Factor);
  end
  else if IsCurrText('(') then
  begin
    Match('(');
    result := Parse_Expression;
    Match(')');
    if IsCurrText('.') or IsCurrText('[') then
      result := Parse_Designator(result);
  end
  else if IsCurrText('[') then
  begin
    result := Parse_SetConstructor;
  end
  else if IsCurrText('@') then
  begin
    Match('@');
    result := NewTempVar;
    Gen(OP_ADDRESS, Parse_Designator, 0, result);
  end
  else if IsCurrText('assigned') and (not InScope('assigned')) then
  begin
    if CONST_ONLY then
      CreateError(errConstantExpressionExpected, []);

    Call_SCANNER;
    Match('(');
    result := NewTempVar;
    Gen(OP_ASSIGNED, Parse_Expression, 0, result);
    Match(')');
    Exit;
  end
  else if IsCurrText('sizeof') and (not InScope('sizeof'))  then
  begin
    Match('sizeof');
    Match('(');
    result := NewTempVar;
    Gen(OP_SIZEOF, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('typeinfo') and (not InScope('typeinfo')) then
  begin
    if CONST_ONLY then
      CreateError(errConstantExpressionExpected, []);

    Match('typeinfo');
    Match('(');
    result := NewTempVar;
    SetType(result, typePOINTER);
    Gen(OP_TYPEINFO, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('pred') and (not InScope('pred')) then
  begin
    Match('pred');
    Match('(');
    result := NewTempVar;
    Gen(OP_PRED, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('succ') and (not InScope('succ')) then
  begin
    Match('succ');
    Match('(');
    result := NewTempVar;
    Gen(OP_SUCC, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('ord') and (not InScope('ord')) then
  begin
    Match('ord');
    Match('(');
    result := NewTempVar;
    Gen(OP_ORD, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('chr') and (not InScope('chr'))  then
  begin
    Match('chr');
    Match('(');
    result := NewTempVar;
    Gen(OP_CHR, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('high') and (not InScope('high'))  then
  begin
    Match('high');
    Match('(');
    result := NewTempVar;
    Gen(OP_HIGH, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('low') and (not InScope('low')) then
  begin
    Match('low');
    Match('(');
    result := NewTempVar;
    Gen(OP_LOW, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('abs') and (not InScope('abs')) then
  begin
    Match('abs');
    Match('(');
    result := NewTempVar;
    Gen(OP_ABS, Parse_Expression, 0, result);
    Match(')');
  end
  else if IsCurrText('length') and (not InScope('length')) then
  begin
    S := GetNext2Text;
    Id := Lookup(S, CurrLevel);
    if Id = 0 then
      goto LabelDesignator;
    if GetSymbolRec(Id).FinalTypeId <> typeOPENARRAY then
      goto LabelDesignator;
    Id := GetOpenArrayHighId(Id);
    result := NewTempVar;
    Gen(OP_PLUS, Id, NewConst(typeINTEGER, 1), result);
    Match('length');
    Match('(');
    Parse_Expression;
    Match(')');
  end
  else if IsCurrText('inherited') then
  begin
    if CONST_ONLY then
      CreateError(errConstantExpressionExpected, []);

    Call_SCANNER;
    SubId := NewTempVar;
    result := NewTempVar;
    K := Parse_Ident;
    RemoveInstruction(OP_EVAL, -1, -1, K);
    Gen(OP_EVAL_INHERITED, K, 0, SubId);
    if IsCurrText('(') or IsCurrText('[') then
      Gen(OP_CALL_INHERITED, SubID, Parse_ArgumentList(SubId), result)
    else
      Gen(OP_CALL_INHERITED, SubID, 0, result);
  end
  else
  begin
LabelDesignator:
    result := Parse_Designator;
    if IsCurrText(':=') then
      if GetSymbolRec(result).OwnerId = 0 then
      if CurrLevel > 0 then
      if GetKind(CurrLevel) in KindSUBS then
      if (GetName(result) <> '') and StrEql(GetName(result), GetName(CurrSubId)) then
   	  	result := CurrResultId;

    if IsCurrText('(') then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);

      SubId := result;
      result := NewTempVar;
      K := Parse_ArgumentList(SubId);
      Gen(OP_CALL, SubID, K, result);

      if IsCurrText('.') or IsCurrText('[') then
        result := Parse_Designator(result);
    end
    else if GetKind(result) = KindSUB then
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);

      SubId := result;
      result := NewTempVar;
      SetName(result, GetName(SubId));
      SetKind(result, KindNONE);
      Gen(OP_EVAL, 0, 0, result);

      if IsCurrText('.') or IsCurrText('[') then
        result := Parse_Designator(result);
    end;

    {$IFDEF CPP_SYN}
    if IsCurrText('++') then // post increment expression
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);

      Match('++');
      temp := NewTempVar;
      Gen(OP_ASSIGN, temp, result, temp);
      r := NewTempVar;
      Gen(OP_PLUS, result, NewConst(typeINTEGER, 1), r);
      Gen(OP_ASSIGN, result, r, result);
      Gen(OP_POSTFIX_EXPRESSION, 0, 0, 0);
      result := temp;
    end
    else if IsCurrText('--') then // post decrement expression
    begin
      if CONST_ONLY then
        CreateError(errConstantExpressionExpected, []);

      Match('--');
      temp := NewTempVar;
      Gen(OP_ASSIGN, temp, result, temp);
      r := NewTempVar;
      Gen(OP_MINUS, result, NewConst(typeINTEGER, 1), r);
      Gen(OP_ASSIGN, result, r, result);
      Gen(OP_POSTFIX_EXPRESSION, 0, 0, 0);
      result := temp;
    end;
    {$ENDIF}
  end;
end;

function TPascalParser.Parse_SetConstructor: Integer;
var
  id1, id2, k: Integer;
begin
  Match('[');
  if not IsCurrText(']') then
  begin
    k := 0;
    result := NewTempVar;

    repeat
      if IsEOF then
        break;
      // parse member group

      id1 := Parse_Expression;
      if IsCurrText('..') then
      begin
        Match('..');
        id2 := Parse_Expression;
        Gen(OP_CHECK_SUBRANGE_TYPE, id1, id2, 0);
        Gen(OP_SET_INCLUDE_INTERVAL, result, id1, id2);
      end
      else
        Gen(OP_SET_INCLUDE, result, id1, 0);

      Inc(k);

      If NotMatch(',') then
        break;
    until false;

    SetCount(result, k);

  end
  else
    result := EmptySetId;

  Match(']');
end;

function TPascalParser.Parse_Designator(init_id: Integer = 0): Integer;
var
  ok: Boolean;
  id: Integer;
  S: String;
begin
  if init_id = 0 then
    result := Parse_QualId
  else
    result := init_id;

  if IsOuterLocalVar(result) then
  begin
    AnonymStack.Top.BindList.Add(result);
    S := GetName(result);
    result := NewTempVar;
    SetName(result, S);
    Gen(OP_EVAL, 0, 0, result);
  end;

  repeat
    if IsCurrText('.') then
    begin
      FIELD_OWNER_ID := result;
      id := FIELD_OWNER_ID;

      Match('.');
      result := Parse_Ident;
      Gen(OP_FIELD, id, result, result);
      ok := true;
    end
    else if IsCurrText('[') then // index
    begin
      Match('[');
      repeat
        id := result;
        result := NewTempVar;
        Gen(OP_ELEM, id, Parse_Expression, result);
        if NotMatch(',') then
           Break;
      until false;
      Match(']');
      ok := true;
    end
    else if IsCurrText('(') then
    begin
      Id := result;
      result := NewTempVar;
      Gen(OP_CALL, Id, Parse_ArgumentList(Id), result);
      ok := true;
    end
    else if IsCurrText('^') then
    begin
      Match('^');
      id := result;
      result := NewTempVar;
      Gen(OP_TERMINAL, id, 0, result);
      ok := true;
    end
    else
      ok := false;
  until not ok;
end;

function TPascalParser.Parse_Label: Integer;
begin
  if not (CurrToken.TokenClass in [tcIntegerConst, tcIdentifier]) then
    RaiseError(errIdentifierExpected, [CurrToken.Text]);
  result := CurrToken.Id;
  if DECLARE_SWITCH then
    SetKind(result, KindLABEL)
  else if GetKind(result) <> KindLABEL then
    RaiseError(errLabelExpected, []);
  Call_SCANNER;
end;

function TPascalParser.Parse_Ident: Integer;
begin
  if CurrToken.TokenClass = tcKeyword then
  begin
    if IsCurrText('nil') then
    begin
      result := NilId;
      Call_SCANNER;
      Exit;
    end;
  end;
  result := inherited Parse_Ident;
end;

procedure TPascalParser.Call_SCANNER;
var
  S: String;
begin
  SetPrevToken;

  inherited;

  while CurrToken.TokenClass = tcSeparator do
  begin
    Gen(OP_SEPARATOR, CurrModule.ModuleNumber, CurrToken.Id, 0);
    inherited Call_SCANNER;
  end;

  if CollectSig then
    Sig := Sig + ' ' + CurrToken.Text;

  if DECLARE_SWITCH then
    Exit;

  if CurrToken.TokenClass = tcKeyword then
  begin
    if StrEql(CurrToken.Text, 'String') then
    begin
{$IFDEF PAXARM}
       CurrToken.Id := typeUNICSTRING;
{$ELSE}
      if IsUNIC then
         CurrToken.Id := typeUNICSTRING
      else
         CurrToken.Id := typeANSISTRING;
{$ENDIF}
       CurrToken.TokenClass := tcIdentifier;
       Exit;
    end
    else if StrEql(CurrToken.Text, 'File') then
    begin
       CurrToken.Id := H_TFileRec;
       CurrToken.TokenClass := tcIdentifier;
       Exit;
    end;
  end;

  if CurrToken.TokenClass = tcIdentifier then
  begin
    S := CurrToken.Text;
    if StrEql(S, 'Char') then
    begin
{$IFDEF PAXARM}
       CurrToken.Id := typeWIDECHAR;
{$ELSE}
      if IsUNIC then
        CurrToken.Id := typeWIDECHAR
      else
        CurrToken.Id := typeANSICHAR;
{$ENDIF}
    end
    else if StrEql(S, 'PChar') then
    begin
{$IFDEF PAXARM}
      CurrToken.Id := typePWIDECHAR;
{$ELSE}
      if IsUNIC then
        CurrToken.Id := typePWIDECHAR
      else
        CurrToken.Id := typePANSICHAR;
{$ENDIF}
    end
    else if StrEql(S, 'NativeInt') then
    begin
      CurrToken.Id := typeNATIVEINT;
    end;
  end;
end;

procedure TPascalParser.ReadToken;
begin
  inherited;

  while CurrToken.TokenClass = tcSeparator do
  begin
    Gen(OP_SEPARATOR, CurrModule.ModuleNumber, CurrToken.Id, 0);
    inherited ReadToken;
  end;
end;

function TPascalParser.Parse_DirectiveList(SubId: Integer): TIntegerList;
var
  S: String;
begin
  result := TIntegerList.Create;

  repeat
    if Parse_PortabilityDirective <> portNone then
      if IsCurrText(';') then
        Match(';');
    S := CurrToken.Text;

    if StrEql(S, 'overload') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, dirOVERLOAD);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirOVERLOAD);
      SetOverloaded(SubId);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'forward') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, dirFORWARD);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirFORWARD);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'message') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, 0);

      RemoveLastIdent(CurrToken.Id);

      if DECLARE_SWITCH then
        if CurrToken.Id = StCard then
          DiscardLastSTRecord;

      Call_SCANNER;
      Parse_Expression;

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'inline') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, 0);
      Call_SCANNER;

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'stdcall') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccSTDCALL);

      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;
      SetCallConvention(SubId, ccSTDCALL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'safecall') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccSAFECALL);

      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;
      SetCallConvention(SubId, ccSAFECALL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'register') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccREGISTER);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      SetCallConvention(SubId, ccREGISTER);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'cdecl') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccCDECL);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      SetCallConvention(SubId, ccCDECL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'msfastcall') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccMSFASTCALL);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      SetCallConvention(SubId, ccMSFASTCALL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'pascal') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, ccPASCAL);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      SetCallConvention(SubId, ccPASCAL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'virtual') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, cmVIRTUAL);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirVIRTUAL);
      SetCallMode(SubId, cmVIRTUAL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'static') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, cmSTATIC);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirSTATIC);
      SetCallMode(SubId, cmSTATIC);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'dynamic') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, cmDYNAMIC);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirDYNAMIC);
      SetCallMode(SubId, cmDYNAMIC);
      Gen(OP_ADD_MESSAGE, SubId, NewConst(typeINTEGER, -1000), 0);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'assembler') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, 0);

      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'override') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, cmOVERRIDE);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirOVERRIDE);
      SetCallMode(SubId, cmOVERRIDE);
      Gen(OP_ADD_MESSAGE, SubId, NewConst(typeINTEGER, -1000), 0);
      Gen(OP_CHECK_OVERRIDE, SubId, 0, 0);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'abstract') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, dirABSTRACT);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirABSTRACT);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'final') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, dirFINAL);

      RemoveLastIdent(CurrToken.Id);

      SetFinal(SubId, true);
      Call_SCANNER;
      result.Add(dirFINAL);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else if StrEql(S, 'reintroduce') then
    begin
      if Assigned(OnParseSubDirective) then
        OnParseSubDirective(Owner, S, dirREINTRODUCE);

      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      result.Add(dirREINTRODUCE);

      if Parse_PortabilityDirective <> portNone then
        Match(';')
      else if IsCurrText(';') then
        Match(';');
    end
    else
      break;
  until false;

  if result.IndexOf(dirVIRTUAL) >= 0 then
    if result.IndexOf(dirVIRTUAL) = -1 then
      CreateError(errAbstractMethodsMustBeVirtual, []);

  Parse_PortabilityDirective;

  if IsCurrText(';') then
    Match(';');
end;

function TPascalParser.Parse_PortabilityDirective: TPortDir;
var
  ok: Boolean;
  S: String;
begin
  result := portNone;
  repeat
    ok := false;
    S := CurrToken.Text;
    if StrEql(S, 'platform') then
    begin
      RemoveLastIdent(CurrToken.Id);
      Call_SCANNER;
      if IsCurrText('=') then
      begin
        Call_SCANNER;
        Parse_Expression;
      end;
      result := portPlatform;
      ok := true;
    end;
    if StrEql(S, 'deprecated') then
    begin
      RemoveLastIdent(CurrToken.Id);

      Call_SCANNER;
      if not IsCurrText(';') then
         Call_SCANNER;
      result := portDeprecated;
      ok := true;
    end;
    if StrEql(S, 'library') then
    begin
      Call_SCANNER;
      result := portLibrary;
      ok := true;
    end;
  until not ok;
end;

procedure TPascalParser.InitSub(var SubId: Integer);
begin
  if AnonymStack.Count = 0 then
  begin
    CheckAbstract(SubId);
    ReplaceForwardDeclaration(SubId);
  end;

  inherited InitSub(SubId);

  Scanner.AttachId(SubId, true);
  Scanner.DoComment;

  if GetSymbolRec(SubId).CallMode = cmSTATIC then
    GetSymbolRec(CurrSelfId).Name := '';

  InitMethodDef(SubId);
end;

procedure TPascalParser.Match(const S: String);
begin
  inherited;
end;

function TPascalParser.MatchEx(const S: String): Boolean;
begin
  result := true;
  Tag := 0;
  Match(S);
  if Tag = 1 then
    result := false;
end;

function TPascalParser.InScope(const S: String): Boolean;
var
  id: Integer;
begin
  id := Lookups(S, LevelStack);
  if id = 0 then
    result := false
  else
    result := not GetSymbolRec(id).Host;
end;

procedure TPascalParser.EndMethodDef(SubId: Integer);
var
  TypeId: Integer;
begin
  inherited;

  TypeId := GetLevel(SubId);

//  if CurrModule.IsExtra then
//    Exit;

  if TypeId = 0 then
    Exit;
  if GetKind(TypeId) <> KindTYPE then
    Exit;

//  if not IsGeneric(TypeId) then
//    Exit;

  if GetSymbolRec(SubId).IsSharedMethod then
    with TKernel(kernel).TypeDefList.FindMethodDef(SubId) do
      Definition := 'class ' + Definition;
end;

procedure TPascalParser.Parse_TypeRestriction(LocalTypeParams: TStringObjectList);
var
  temp: Boolean;
  I: Integer;
  TR: TTypeRestrictionRec;
begin
  temp := DECLARE_SWITCH;
  try
    DECLARE_SWITCH := false;
    if not IsCurrText(':') then
      Exit;
    Call_SCANNER;
    TR := TTypeRestrictionRec.Create;
    TR.N := TKernel(kernel).Code.Card;
    if IsCurrText('class') then
    begin
      Call_SCANNER;
      if IsCurrText(',') then
      begin
        Match(',');
        Match('constructor');
      end;
      TR.Id := H_TObject;
    end
    else if IsCurrText('constructor') then
    begin
      Call_SCANNER;
      if IsCurrText(',') then
      begin
        Match(',');
        Match('class');
      end;
      TR.Id := H_TObject;
    end
    else if IsCurrText('record') then
    begin
      Call_SCANNER;
      TR.Id := typeRECORD;
    end
    else
    begin
      TR.Id := Parse_QualId;
      if IsCurrText(',') then
      begin
        Match(',');
        Match('constructor');
      end;
    end;
  finally
    DECLARE_SWITCH := temp;
  end;
  if TR = nil then
     Exit;
  for I := LocalTypeParams.Count - 1 downto 0 do
  begin
    if LocalTypeParams.Objects[I] <> nil then
      break;
    LocalTypeParams.Objects[I] := TR.Clone;
  end;
  FreeAndNil(TR);
end;

procedure TPascalParser.Parse_Attribute;
begin
  while IsCurrText('[') do
  begin
    Call_SCANNER;
    repeat
      Parse_Expression;
      if NotMatch(',') then
        break;
    until false;
    Match(']');
  end;
end;

procedure TPascalParser.RemoveKeywords;
begin
  HideKeyword(I_STRICT);
  HideKeyword(I_PRIVATE);
  HideKeyword(I_PROTECTED);
  HideKeyword(I_PUBLIC);
  HideKeyword(I_PUBLISHED);
end;

procedure TPascalParser.RestoreKeywords;
begin
  inherited;
end;

function TPascalParser.Parse_LoopStmt(l_break, l_continue, l_loop: Integer): Boolean;
begin
  BreakStack.Push(l_break, l_loop);
  ContinueStack.Push(l_continue, l_loop);
  BeginLoop;
  result := Parse_Statement;
  EndLoop;
  BreakStack.Pop;
  ContinueStack.Pop;
end;

end.
