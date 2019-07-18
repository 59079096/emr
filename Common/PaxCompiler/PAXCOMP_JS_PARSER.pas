///////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_JS_PARSER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_JS_PARSER;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_SCANNER,
  PAXCOMP_BYTECODE,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_BASERUNNER,
  PAXCOMP_MODULE,
  PAXCOMP_STDLIB,
  PAXCOMP_PARSER;

type
  TJavaScriptParser = class(TBaseParser)
  private
    anonymous_count: Integer;
    StatementLabel: String;
    ProcessedLineTerminator: Boolean;
    ClosureIds: TIntegerList;

    function DupRes(res: Integer): Integer;

    function IsLabelId: boolean;
    function Parse_SetLabel: Integer;
    function InContext: Boolean;

    procedure Parse_SourceElements;
    procedure Parse_SourceElement;

    function Parse_FunctionDeclaration: Integer;
    function Parse_FormalParameterList(SubId: Integer): Integer;
    function Parse_ArrayLiteral: Integer;
    function Parse_ObjectLiteral: Integer;
    function Parse_RegExpLiteral: Integer;

    function Parse_ArgumentList(SubId: Integer): Integer;

    function Parse_PrimaryExpression(ResId: Integer = 0): Integer;
    function Parse_MemberExpression(res: Integer): Integer;
    function Parse_NewExpression(res: Integer): Integer;
    function Parse_CallExpression(res: Integer): Integer;
    function Parse_PostfixExpression(res: Integer): Integer;
    function Parse_UnaryExpression(res: Integer): Integer;
    function Parse_LeftHandSideExpression(res: Integer): Integer;
    function Parse_MultiplicativeExpression(res: Integer): Integer;
    function Parse_AdditiveExpression(res: Integer): Integer;
    function Parse_ShiftExpression(res: Integer): Integer;
    function Parse_RelationalExpression(res: Integer): Integer;
    function Parse_EqualityExpression(res: Integer): Integer;
    function Parse_BitwiseANDExpression(res: Integer): Integer;
    function Parse_BitwiseXORExpression(res: Integer): Integer;
    function Parse_BitwiseORExpression(res: Integer): Integer;
    function Parse_LogicalANDExpression(res: Integer): Integer;
    function Parse_LogicalORExpression(res: Integer): Integer;

    function Parse_AssignmentExpression: Integer;
    function Parse_FunctionExpression: Integer;

    procedure Parse_Module;
    procedure Parse_Statement;
    procedure Parse_Namespace;
    procedure Parse_Block;
    procedure Parse_StatementList;
    procedure Parse_VariableStatement;
    procedure Parse_VariableDeclarationList;
    procedure Parse_VariableDeclaration;
    procedure Parse_EmptyStatement;
    procedure Parse_IfStatement;
    procedure Parse_DoStatement;
    procedure Parse_WhileStatement;
    procedure Parse_WithStatement;
    procedure Parse_SwitchStatement;
    procedure Parse_TryStatement;
    procedure Parse_ThrowStatement;
    procedure Parse_ForStatement;
    procedure Parse_BreakStatement;
    procedure Parse_GotoStatement;
    procedure Parse_ContinueStatement;
    procedure Parse_ReturnStatement;
    procedure Parse_ExpressionStatement;

    procedure Parse_LoopStmt(l_break, l_continue, l_loop: Integer);
    // misc

    function IsLineTerminator: Boolean;
    procedure MatchLineTerminator;

    function Parse_Label: Integer;
    function IsAssignment_operator(const S: String): Boolean;
    function Parse_Assignment_operator: Integer;
    function Parse_LogicalOR_operator: Integer;
    function Parse_LogicalAND_operator: Integer;
    function Parse_BitwiseOR_operator: Integer;
    function Parse_BitwiseXOR_operator: Integer;
    function Parse_BitwiseAND_operator: Integer;
    function Parse_Equality_operator: Integer;
    function Parse_Relational_operator: Integer;
    function Parse_Shift_operator: Integer;
    function Parse_Additive_operator: Integer;
    function Parse_Multiplicative_operator: Integer;

    // extension
    procedure Parse_UsingStatement;
    procedure Parse_PrintStatement;
    procedure Parse_PrintlnStatement;

  protected
    function CreateScanner: TBaseScanner; override;
    function GetLanguageName: String; override;
    function GetFileExt: String; override;
    function GetLanguageId: Integer; override;
    function GetUpcase: Boolean; override;
    function GetCurrSelfId: Integer; override;
    function ConvString(const S: String): String; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Parse_Expression: Integer; override;
    function GetIncludedFileExt: String; override;
    procedure Init(i_kernel: Pointer; M: TModule); override;

    procedure ParseProgram; override;
    procedure Call_SCANNER; override;
    procedure Match(const S: String); override;
    function Parse_Ident: Integer; override;
  end;

implementation

uses
  PAXCOMP_KERNEL,
  PAXCOMP_JS_SCANNER;

constructor TJavaScriptParser.Create;
begin
  inherited;
  AddKeyword('base');
  AddKeyword('break');
  AddKeyword('case');
  AddKeyword('catch');
  AddKeyword('continue');
  AddKeyword('debugger');
  AddKeyword('default');
  AddKeyword('delete');
  AddKeyword('do');

  AddKeyword('else');
  AddKeyword('finally');
  AddKeyword('for');
  AddKeyword('function');
  AddKeyword('if');
  AddKeyword('in');
  AddKeyword('instanceof');

  AddKeyword('new');
  AddKeyword('return');
  AddKeyword('switch');
  AddKeyword('this');
  AddKeyword('throw');
  AddKeyword('try');
  AddKeyword('typeof');

  AddKeyword('var');
  AddKeyword('void');
  AddKeyword('while');
  AddKeyword('with');

  AddKeyword('using'); // extension
  AddKeyword('print'); // extension
  AddKeyword('println'); // extension
  AddKeyword('namespace'); // extension
//  AddKeyword('module'); // extension

  ClosureIds := TIntegerList.Create(true);
end;

destructor TJavaScriptParser.Destroy;
begin
  FreeAndNil(ClosureIds);
  inherited;
end;

procedure TJavaScriptParser.Init(i_kernel: Pointer; M: TModule);
begin
  ClosureIds.Clear;
  inherited;
end;

function TJavaScriptParser.CreateScanner: TBaseScanner;
begin
  result := TJavaScriptScanner.Create;
end;

function TJavaScriptParser.GetLanguageName: String;
begin
  result := 'JavaScript';
end;

function TJavaScriptParser.GetFileExt: String;
begin
  result := 'js';
end;

function TJavaScriptParser.GetLanguageId: Integer;
begin
  result := JS_LANGUAGE;
end;

function TJavaScriptParser.GetUpcase: Boolean;
begin
  result := false;
end;

function TJavaScriptParser.GetIncludedFileExt: String;
begin
  result := 'js';
end;

procedure TJavaScriptParser.ParseProgram;
var
  B1, B2: Integer;
begin
  ProcessedLineTerminator := false;

  if IsLineTerminator then
    MatchLineTerminator;

  anonymous_count := 0;
  EXECUTABLE_SWITCH := 0;
  DECLARE_SWITCH := false;
  Call_SCANNER;

  if IsEOF then
  begin
    Exit;
  end;

  if IsCurrText('module') then
  begin
    Parse_Module;
    Exit;
  end;
  Gen(OP_END_INTERFACE_SECTION, CurrModule.ModuleNumber, 0, 0);

  B1 := CodeCard;
  Parse_SourceElements;
  B2 := CodeCard;

  BeginFinalization;

  Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);

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

procedure TJavaScriptParser.Parse_Module;
var
  namespace_id, B1, B2: Integer;
  S: String;
begin
  DECLARE_SWITCH := true;
  Match('module');

  namespace_id := Parse_UnitName(S);

  BeginNamespace(namespace_id);
  Match('{');

  while IsCurrText('using') do
    Parse_UsingStatement;

  Gen(OP_END_IMPORT, 0, 0, 0);

  B1 := CodeCard;

  repeat
    if IsEOF then
      break;
    if IsCurrText('}') then
      break;

    if IsCurrText('namespace') then
      Parse_Namespace
    else if IsCurrText('function') then
      Parse_FunctionDeclaration
    else if IsCurrText('var') then
      Parse_VariableStatement
    else
    Parse_ExpressionStatement;
  until false;

  EndNamespace(namespace_id);

  Gen(OP_END_INTERFACE_SECTION, CurrModule.ModuleNumber, 0, 0);

  B2 := CodeCard;

  Match('}');

  BeginFinalization;

  Gen(OP_BEGIN_GLOBAL_BLOCK, 0, 0, 0);
  Gen(OP_NOP, 0, 0, 0);

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

procedure TJavaScriptParser.Parse_Namespace;
var
  namespace_id: Integer;
  S: String;
begin
  Match('namespace');
  namespace_id := Parse_UnitName(S);

  Match('{');

  repeat
    if IsEOF then
      break;
    if IsCurrText('}') then
      break;

    if IsCurrText('namespace') then
      Parse_Namespace
    else if IsCurrText('function') then
      Parse_FunctionDeclaration
    else if IsCurrText('var') then
      Parse_VariableStatement
    else
    Parse_ExpressionStatement;
  until false;

  EndNamespace(namespace_id);

  Match('}');
end;

procedure TJavaScriptParser.Parse_SourceElements;
begin
  repeat
    if IsCurrText('}') then
      break;
    if IsEOF then
      break;
    Parse_SourceElement;
  until false;
end;

procedure TJavaScriptParser.Parse_SourceElement;
var
  L, Id, RefId: Integer;
  S: String;
begin
  if IsCurrText('function') then
  begin
    if CurrLevel = 0 then
      Parse_FunctionDeclaration
    else
    begin
      L := CurrLevel;
      if GetKind(L) = KindSUB then
      begin
        Id := Parse_FunctionDeclaration;
        SetLevel(Id, 0);

        S := GetName(Id);
        RefId := NewField(S, CurrSelfId);
        Gen(OP_FIELD, CurrSelfId, RefId, RefId);
        Gen(OP_ASSIGN, RefId, Id, RefId);
      end
      else
        RaiseError(errSyntaxError, []);
    end;
  end
  else
    Parse_Statement;
end;

function TJavaScriptParser.Parse_FunctionDeclaration: Integer;
begin
  result := Parse_FunctionExpression;
end;

function TJavaScriptParser.Parse_FunctionExpression: Integer;
var
  I, SubId, NP, L,
  FunctionConstructorId, ObjectConstructorId, ProtoObjectId, RefId,
  ObjectId, AddressId: Integer;
  global: Boolean;
  Id: Integer;
  S: String;
begin
  EXECUTABLE_SWITCH := 0;
  DECLARE_SWITCH := true;
  global := true;

  ClosureIds.Clear;

// JS only
  ObjectId := NewTempVar;
  result := ObjectId;
//  Gen(OP_DECLARE_LOCAL_VAR, JS_TempNamespaceId, ObjectId, 0);
//

  Match('function');

  if IsCurrText('(') then
  begin
    SubId := NewTempVar; // anonymous function
    Inc(anonymous_count);
    SetName(SubId, IntToStr(anonymous_count));
  end
  else
    SubId := Parse_Ident;

  L := CurrLevel;
  if L > 0 then
    if GetKind(L) in KindSUBS then
      global := false;

// JS only
  SetCallConvention(SubId, ccSTDCALL);
  SetType(ObjectId, JS_FunctionClassId);
  SetName(ObjectId, GetName(SubId));
  SetName(SubId, '#' + GetName(SubId));
//

  BeginSub(SubId);

  if IsCurrText('(') then
    NP := Parse_FormalParameterList(SubId)
  else
  begin
    NP := 0;
    Match(')');
  end;
  SetCount(SubId, NP);
  SetType(SubId, typeVARIANT);
  SetType(CurrResultId, typeVARIANT);
  SetType(CurrSelfId, JS_FunctionClassId);
  SetName(CurrSelfId, 'this');
  SetKind(CurrSelfId, KindVAR);
  SetParam(CurrSelfId, true);
  DECLARE_SWITCH := false;

  GetSymbolRec(SubId).IsJSFunction := true;

  InitSub(SubId);
  Match('{');

  Parse_SourceElements;

  EndSub(SubId);
  Match('}');

//  if global then
  begin
    L := NewLabel;
    Gen(OP_GO, L, 0, 0);
    Gen(OP_BEGIN_CRT_JS_FUNC_OBJECT, 0, 0, 0);
  end;

  FunctionConstructorId := NewField(strInternalCreate, JS_FunctionClassId);
  Gen(OP_FIELD, JS_FunctionClassId, FunctionConstructorId, FunctionConstructorId);

  AddressId := NewTempVar();
  SetLevel(AddressId, JS_TempNamespaceId);
  Gen(OP_ADDRESS, SubId, 0, AddressId);
  Gen(OP_PUSH, AddressId, 0, FunctionConstructorId);
  Gen(OP_PUSH, NewConst(typeVARIANT, NP), 1, FunctionConstructorId);
  AddressId := NewTempVar(typePOINTER);
  SetLevel(AddressId, JS_TempNamespaceId);
  Gen(OP_ADDRESS_PROG, 0, 0, AddressId);
  Gen(OP_PUSH, AddressId, 2, FunctionConstructorId);
  Gen(OP_CALL, FunctionConstructorId, 3, ObjectId);

// create prototype object
  ProtoObjectId := NewTempVar();
  SetLevel(ProtoObjectId, JS_TempNamespaceId);
  ObjectConstructorId := NewField(strCreate, JS_ObjectClassId);
  Gen(OP_FIELD, JS_ObjectClassId, ObjectConstructorId, ObjectConstructorId);
  Gen(OP_CALL, ObjectConstructorId, 0, ProtoObjectId);

  Gen(OP_ASSIGN_PROG, 0, 0, ProtoObjectId);

// create constructor property
  RefId := NewField(strInternalConstructor, ProtoObjectId);
  SetLevel(RefId, JS_TempNamespaceId);
  Gen(OP_FIELD, ProtoObjectId, RefId, RefId);
  Gen(OP_ASSIGN, RefId, ObjectId, RefId);

// create prototype property
  RefId := NewField('prototype', ObjectId);
  SetLevel(RefId, JS_TempNamespaceId);
  Gen(OP_FIELD, ObjectId, RefId, RefId);
  Gen(OP_ASSIGN, RefId, ProtoObjectId, RefId);

//  if global then
  begin
    Gen(OP_END_CRT_JS_FUNC_OBJECT, 0, 0, 0);
    SetLabelHere(L);
//    Exit;
  end;

  if global then
    Exit;

  FunctionConstructorId := NewField(strInternalCreate, JS_FunctionClassId);
  Gen(OP_FIELD, JS_FunctionClassId, FunctionConstructorId, FunctionConstructorId);

  AddressId := NewTempVar();
  SetLevel(AddressId, JS_TempNamespaceId);
  Gen(OP_ADDRESS, SubId, 0, AddressId);
  Gen(OP_PUSH, AddressId, 0, FunctionConstructorId);
  Gen(OP_PUSH, NewConst(typeVARIANT, NP), 1, FunctionConstructorId);
  AddressId := NewTempVar(typePOINTER);
  SetLevel(AddressId, JS_TempNamespaceId);
  Gen(OP_ADDRESS_PROG, 0, 0, AddressId);
  Gen(OP_PUSH, AddressId, 2, FunctionConstructorId);
  Gen(OP_CALL, FunctionConstructorId, 3, ObjectId);

// create prototype object
  ProtoObjectId := NewTempVar();
  SetLevel(ProtoObjectId, JS_TempNamespaceId);
  ObjectConstructorId := NewField(strCreate, JS_ObjectClassId);
  Gen(OP_FIELD, JS_ObjectClassId, ObjectConstructorId, ObjectConstructorId);
  Gen(OP_CALL, ObjectConstructorId, 0, ProtoObjectId);

  Gen(OP_ASSIGN_PROG, 0, 0, ProtoObjectId);

// create constructor property
  RefId := NewField(strInternalConstructor, ProtoObjectId);
  SetLevel(RefId, JS_TempNamespaceId);
  Gen(OP_FIELD, ProtoObjectId, RefId, RefId);
  Gen(OP_ASSIGN, RefId, ObjectId, RefId);

// create prototype property
  RefId := NewField('prototype', ObjectId);
  SetLevel(RefId, JS_TempNamespaceId);
  Gen(OP_FIELD, ObjectId, RefId, RefId);
  Gen(OP_ASSIGN, RefId, ProtoObjectId, RefId);

// closure:

  for I := 0 to ClosureIds.Count - 1 do
  begin
    Id := ClosureIds[I];
    if GetLevel(Id) = SubId then
      continue;

    S := GetName(Id);
    RefId := NewField(S, ObjectId);
    SetLevel(RefId, JS_TempNamespaceId);
    Gen(OP_FIELD, ObjectId, RefId, RefId);
    Gen(OP_ASSIGN, RefId, Id, RefId);
  end;
end;


function TJavaScriptParser.Parse_FormalParameterList(SubId: Integer): Integer;
var
  ID: Integer;
begin
  result := 0;

  DECLARE_SWITCH := true;
  Match('(');
  if not IsCurrText(')') then
    repeat
      Inc(result);
      ID := Parse_FormalParameter;
      SetType(ID, typeVARIANT);
      Gen(OP_DECLARE_LOCAL_VAR, SubId, ID, 0);
      if NotMatch(',') then
        break;
    until false;

  DECLARE_SWITCH := false;
  Match(')');
end;

function TJavaScriptParser.Parse_ArrayLiteral: Integer;
var
  sub_id, type_id, RefId, elem_id, expr_id, K: Integer;
label Next;
begin
  type_id := JS_ArrayClassId;
  sub_id := NewTempVar();
  Gen(OP_FIND_CONSTRUCTOR, type_id, 0, sub_id);

  result := NewTempVar();
  Gen(OP_CALL, sub_id, 0, result);

  Gen(OP_ASSIGN_PROG, 0, 0, result);

  RefId := NewField(strInternalConstructor, result);
  Gen(OP_FIELD, result, RefId, RefId);
  Gen(OP_ASSIGN, RefId, result, RefId);

  K := 0;

  Match('[');
  repeat
    Next:

    if IsCurrText(',') then
    begin
      Match(',');
      Inc(K);
      goto Next;
    end
    else if IsCurrText(']') then
      break
    else
    begin
      elem_id := NewTempVar;
      expr_id := Parse_AssignmentExpression;
      Gen(OP_ELEM, result, NewConst(typeINTEGER, K), elem_id);
      Gen(OP_ASSIGN, elem_id, expr_id, elem_id);
    end;
    if NotMatch(',') then
      Break
    else
      Inc(K);
  until false;

  Match(']');
end;

function TJavaScriptParser.Parse_RegExpLiteral: Integer;
var
  S: String;
  I, sub_id, type_id, RefId: Integer;
  c: Char;
begin
  S := ScanRegExpLiteral;

  type_id := JS_RegExpClassId;
  sub_id := NewTempVar();
  Gen(OP_FIND_CONSTRUCTOR, type_id, 0, sub_id);

  result := NewTempVar();
  Gen(OP_CALL, sub_id, 0, result);

  Gen(OP_ASSIGN_PROG, 0, 0, result);

  RefId := NewField(strInternalConstructor, result);
  Gen(OP_FIELD, result, RefId, RefId);
  Gen(OP_ASSIGN, RefId, result, RefId);

  RefId := NewField('source', result);
  Gen(OP_FIELD, result, RefId, RefId);
  Gen(OP_ASSIGN, RefId, NewConst(typeSTRING, S), RefId);

  Call_SCANNER;
  Match('/');

  c := CurrToken.Text[SLow(CurrToken.Text)];

  if ByteInSet(c, [Ord('i'), Ord('I'), Ord('g'), Ord('G'), Ord('m'), Ord('M')]) then
  begin
    for I:=SLow(CurrToken.Text) to SHigh(CurrToken.Text) do
      case CurrToken.Text[I] of
        'g','G':
        begin
          RefId := NewField('global', result);
          Gen(OP_FIELD, result, RefId, RefId);
          Gen(OP_ASSIGN, RefId, TrueId, RefId);
        end;
        'i','I':
        begin
          RefId := NewField('ignoreCase', result);
          Gen(OP_FIELD, result, RefId, RefId);
          Gen(OP_ASSIGN, RefId, TrueId, RefId);
        end;
        'm','M':
        begin
          RefId := NewField('multiline', result);
          Gen(OP_FIELD, result, RefId, RefId);
          Gen(OP_ASSIGN, RefId, TrueId, RefId);
        end;
      end;
    Call_SCANNER;
  end;
end;

function TJavaScriptParser.Parse_ObjectLiteral: Integer;
var
  sub_id, type_id, RefId, elem_id, expr_id, right_id: Integer;
  S: String;
  R: TCodeRec;
begin
  type_id := JS_ObjectClassId;
  sub_id := NewTempVar();
  Gen(OP_FIND_CONSTRUCTOR, type_id, 0, sub_id);

  result := NewTempVar();
  Gen(OP_CALL, sub_id, 0, result);

  Gen(OP_ASSIGN_PROG, 0, 0, result);

  RefId := NewField(strInternalConstructor, result);
  Gen(OP_FIELD, result, RefId, RefId);
  Gen(OP_ASSIGN, RefId, result, RefId);

  Match('{');
  if not IsCurrText('}') then
  repeat
    elem_id := NewTempVar;
    expr_id := Parse_Expression;
    if GetKind(expr_id) <> KindCONST then
    begin
      S := GetName(expr_id);
      R := LastEvalRec(expr_id);
      if R <> nil then
        R.Op := OP_NOP;
      SetName(expr_id, '');
      SetKind(expr_id, KindNONE);
      {$IFDEF UNIC}
        expr_id := NewConst(typeUNICSTRING, S);
      {$ELSE}
        expr_id := NewConst(typeANSISTRING, S);
      {$ENDIF}
    end;
    Match(':');
    right_id := Parse_AssignmentExpression;
    Gen(OP_ELEM, result, expr_id, elem_id);
    Gen(OP_ASSIGN, elem_id, right_id, elem_id);
    if NotMatch(',') then
      Break;
  until False;
  Match('}');
end;

function TJavaScriptParser.Parse_ArgumentList(SubId: Integer): Integer;
var
  I: Integer;
  L: TIntegerList;
begin
  L := TIntegerList.Create;
  try
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

    Match(')');
  finally
    FreeAndNil(L);
  end;
end;

// EXPRESSIONS -----------------------------------------------------------------

function TJavaScriptParser.DupRes(res: Integer): Integer;
var
  id, res_owner, I: Integer;
  R: TCodeRec;
begin
  res_owner := GetSymbolRec(res).OwnerId;
  R := LastCodeRec(I);
  if (R.Op = OP_PLUS) or (R.Op = OP_MINUS) then
    R := GetCodeRec(I-1);

  id := 0;

  if res_owner > 0 then
  begin
    if R.Op = OP_ELEM then
    begin
      Id := NewTempVar;
      Gen(OP_ELEM, R.Arg1, R.Arg2, Id);
    end
    else if R.Op = OP_FIELD then
    begin
      id := NewField(GetName(res), res_owner);
      Gen(OP_FIELD, R.Arg1, id, id);
    end
    else
      RaiseError(errInternalError, []);
    result := id;
  end
  else
    result := res;
end;

function TJavaScriptParser.Parse_PrimaryExpression(ResId: Integer = 0): Integer;
begin
  if ResId > 0 then
    result := ResId
  else if IsCurrText('this') then
  begin
    result := CurrSelfId;
    Match('this');
  end
  else if CurrToken.TokenClass = tcBooleanConst then
    result := Parse_BooleanLiteral
  else if CurrToken.TokenClass = tcPCharConst then
    result := Parse_PCharLiteral
  else if CurrToken.TokenClass = tcIntegerConst then
    result := Parse_IntegerLiteral
  else if CurrToken.TokenClass = tcDoubleConst then
    result := Parse_DoubleLiteral
  else if IsCurrText('[') then
    result := Parse_ArrayLiteral
  else if IsCurrText('{') then
    result := Parse_ObjectLiteral
  else if IsCurrText('/') then
    result := Parse_RegExpLiteral
  else if IsCurrText('(') then
  begin
    Match('(');
    result := Parse_Expression;
    Match(')');
  end
  else if IsCurrText('@') then
  begin
    Match('@');
    result := NewTempVar;
    Gen(OP_ADDRESS, Parse_Ident, 0, result);
  end
  else
    result := Parse_Ident;
end;

function TJavaScriptParser.Parse_MemberExpression(res: Integer): Integer;
var
  id, type_id, sub_id, RefId, ip, expr_id, K1, K2: Integer;
begin
  if IsCurrText('function') then
  begin
    result := Parse_FunctionExpression;
    SetLevel(result, JS_TempNamespaceId);
  end
  else if IsCurrText('new') then
  begin
    Match('new');
    type_id := Parse_MemberExpression(res);
    sub_id := NewTempVar();
    Gen(OP_FIND_CONSTRUCTOR, type_id, 0, sub_id);

    result := NewTempVar();
    if IsCurrText('(') then // invocation
    begin
      Gen(OP_CALL, sub_id, Parse_ArgumentList(sub_id), result);
    end
    else
      Gen(OP_CALL, sub_id, 0, result);

    RefId := NewField(strProgram, result);
    Gen(OP_FIELD, result, RefId, RefId);
    Gen(OP_ASSIGN_PROG, 0, 0, result);

    RefId := NewField(strInternalConstructor, result);
    Gen(OP_FIELD, result, RefId, RefId);
    Gen(OP_ASSIGN, RefId, result, RefId);
  end
  else
    result := Parse_PrimaryExpression(res);

  ip := CodeCard;

  while IsCurrText('.') or IsCurrText('[') do
  begin
    if IsCurrText('[') then // element access
    begin
      Match('[');
      repeat
        id := result;
        result := NewTempVar;
        K1 := CodeCard;
        expr_id := Parse_Expression;
        K2 := CodeCard;
        if K2 - K1 > 0 then
          RelocateCode(Ip, K1, K2);
        Gen(OP_ELEM, id, expr_id, result);
        if NotMatch(',') then
           Break;
      until false;
      Match(']');
    end
    else if (IsCurrText('.')) then // member access
    begin
      FIELD_OWNER_ID := result;
      id := FIELD_OWNER_ID;
      Match('.');
      result := Parse_Ident;
      Gen(OP_FIELD, id, result, result);
    end;
  end;
end;

function TJavaScriptParser.Parse_NewExpression(res: Integer): Integer;
begin
  result := Parse_MemberExpression(res);
end;

function TJavaScriptParser.Parse_CallExpression(res: Integer): Integer;
var
  sub_id, id: Integer;
begin
  result := Parse_MemberExpression(res);
  if not IsCurrText('(') then
    Exit;
  sub_id := result;
  result := NewTempVar();
  Gen(OP_CALL, sub_id, Parse_ArgumentList(sub_id), result);
  repeat
    if IsCurrText('(') then // invocation
    begin
      sub_id := result;
      result := NewTempVar();
      Gen(OP_CALL, sub_id, Parse_ArgumentList(sub_id), result);
    end
    else if IsCurrText('[') then // element access
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
    end
    else if (IsCurrText('.')) then // member access
    begin
      if GetKind(result) = kindCONST then
      begin
        id := NewTempVar;
        Gen(OP_ASSIGN, id, result, id);
        result := Id;
      end;

      FIELD_OWNER_ID := result;
      id := FIELD_OWNER_ID;

      Match('.');
      result := Parse_Ident;
      Gen(OP_FIELD, id, result, result);
    end
    else
      break;
  until false;
end;

function TJavaScriptParser.Parse_LeftHandSideExpression(res: Integer): Integer;
begin
  if IsCurrText('new') then
    result := Parse_NewExpression(res)
  else
    result := Parse_CallExpression(res);
end;

function TJavaScriptParser.Parse_PostfixExpression(res: Integer): Integer;
var
  temp, r: Integer;
begin
  if res > 0 then
  begin
    result := res;
    Exit;
  end;

  result := Parse_LeftHandSideExpression(res);
  repeat
    if IsCurrText('++') then
    begin
      Match('++');

      temp := NewTempVar;

      if IsCurrText(';') then
      begin
        Gen(OP_PLUS, DupRes(result), NewConst(typeINTEGER, 1), temp);
        Gen(OP_ASSIGN, result, temp, result);
        Exit;
      end;

      res := DupRes(result);
      Gen(OP_ASSIGN, temp, res, temp);
      r := NewTempVar;
      Gen(OP_PLUS, res, NewConst(typeINTEGER, 1), r);
      Gen(OP_ASSIGN, result, r, result);
      result := temp;
    end
    else if IsCurrText('--') then
    begin
      Match('--');

      temp := NewTempVar;

      if IsCurrText(';') then
      begin
        Gen(OP_PLUS, DupRes(result), NewConst(typeINTEGER, 1), temp);
        Gen(OP_ASSIGN, result, temp, result);
        Exit;
      end;

      res := DupRes(result);
      Gen(OP_ASSIGN, temp, res, temp);
      r := NewTempVar;
      Gen(OP_MINUS, res, NewConst(typeINTEGER, 1), r);
      Gen(OP_ASSIGN, result, r, result);
      result := temp;
    end
    else
      break;
  until false;
end;

function TJavaScriptParser.Parse_UnaryExpression(res: Integer): Integer;
var
  temp: Integer;
begin
  if res > 0 then
  begin
    result := res;
    Exit;
  end;

  if IsCurrText('delete') then
  begin
    Match('delete');
    result := UnaryOp(OP_JS_DELETE, Parse_UnaryExpression(0));
  end
  else if IsCurrText('void') then
  begin
    Match('void');
    result := UnaryOp(OP_JS_VOID, Parse_UnaryExpression(0));
  end
  else if IsCurrText('typeof') then
  begin
    Match('typeof');
    result := UnaryOp(OP_JS_TYPEOF, Parse_UnaryExpression(0));
  end
  else if IsCurrText('+') then
  begin
    Match('+');
    result := Parse_UnaryExpression(0);
  end
  else if IsCurrText('-') then
  begin
    Match('-');
		result := UnaryOp(OP_NEG, Parse_UnaryExpression(0));
  end
  else if IsCurrText('++') then
  begin
    Match('++');
    result := Parse_UnaryExpression(0);
    temp := NewTempVar;
    Gen(OP_PLUS, result, NewConst(typeINTEGER, 1), temp);
    Gen(OP_ASSIGN, DupRes(result), temp, result);
    result := temp;
  end
  else if IsCurrText('--') then
  begin
    Match('--');
    result := Parse_UnaryExpression(0);
    temp := NewTempVar;
    Gen(OP_MINUS, result, NewConst(typeINTEGER, 1), temp);
    Gen(OP_ASSIGN, DupRes(result), temp, result);
    result := temp;
  end
  else if IsCurrText('!') then
  begin
    Match('!');
    result := UnaryOp(OP_NOT, Parse_UnaryExpression(0));
  end
  else if IsCurrText('~') then
  begin
    Match('~');
    result := UnaryOp(OP_NOT, Parse_UnaryExpression(0));
  end
  else
    result := Parse_PostfixExpression(res);
end;

function TJavaScriptParser.Parse_MultiplicativeExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_UnaryExpression(res);

	op := Parse_Multiplicative_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_UnaryExpression(0));
  	op := Parse_Multiplicative_operator;
  end;
end;

function TJavaScriptParser.Parse_AdditiveExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_MultiplicativeExpression(res);

	op := Parse_Additive_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_MultiplicativeExpression(0));
  	op := Parse_Additive_operator;
  end;
end;

function TJavaScriptParser.Parse_ShiftExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_AdditiveExpression(res);

	op := Parse_Shift_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_AdditiveExpression(0));
  	op := Parse_Shift_operator;
  end;
end;

function TJavaScriptParser.Parse_RelationalExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_ShiftExpression(res);

	op := Parse_Relational_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_ShiftExpression(0));
  	op := Parse_Relational_operator;
  end;
end;

function TJavaScriptParser.Parse_EqualityExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_RelationalExpression(res);

	op := Parse_Equality_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_RelationalExpression(0));
  	op := Parse_Equality_operator;
  end;
end;

function TJavaScriptParser.Parse_BitwiseANDExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_EqualityExpression(res);

	op := Parse_BitwiseAND_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_EqualityExpression(0));
  	op := Parse_BitwiseAND_operator;
  end;
end;

function TJavaScriptParser.Parse_BitwiseXORExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_BitwiseANDExpression(res);

	op := Parse_BitwiseXOR_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_BitwiseANDExpression(0));
   	op := Parse_BitwiseXOR_operator;
  end;
end;

function TJavaScriptParser.Parse_BitwiseORExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_BitwiseXORExpression(res);

	op := Parse_BitwiseOR_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_BitwiseXORExpression(0));
  	op := Parse_BitwiseOR_operator;
  end;
end;

function TJavaScriptParser.Parse_LogicalANDExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_BitwiseORExpression(res);

	op := Parse_LogicalAND_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_BitwiseORExpression(0));
  	op := Parse_LogicalAND_operator;
  end;
end;

function TJavaScriptParser.Parse_LogicalORExpression(res: Integer): Integer;
var
  op: Integer;
begin
	result := Parse_LogicalANDExpression(res);

	op := Parse_LogicalOR_operator;
	while op <> 0 do
  begin
    Call_SCANNER;
		result := BinOp(op, result, Parse_LogicalANDExpression(0));
  	op := Parse_LogicalOR_operator;
  end;
end;

function TJavaScriptParser.Parse_Expression: Integer;
begin
  result := Parse_AssignmentExpression;
end;

function TJavaScriptParser.Parse_AssignmentExpression: Integer;
var
  op, temp, lg, lf, id1, id2, L, K1, K2: Integer;
  R: TCodeRec;
  S1: String;
  NotDeclared: Boolean;
begin
  R := LastEvalRec(CurrToken.Id, K1);
  if R <> nil then
  if IsAssignment_operator(GetNextText) then
  begin
    if not InContext then
    begin
      SetKind(R.Res, KindVAR);
      R.Op := OP_NOP;
      R := nil;
    end;
  end;

  S1 := CurrToken.Text;

  if R = nil then
    K1 := CodeCard + 1;

	result := Parse_UnaryExpression(0);
  K2 := CodeCard;

  if IsCurrText(';') then
    Exit;
  if IsCurrText(')') then
    Exit;

  NotDeclared := (not HasBeenDeclared(result)) and (GetSymbolRec(result).Name <> '');

	op := Parse_Assignment_operator;
  if op <> 0 then
  begin
    Gen(OP_LVALUE, result, 0, 0);

    Call_SCANNER;
		if op = OP_ASSIGN then
    begin
      if NotDeclared then
        Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, result, 0);

      id1 := result;
      id2 := Parse_AssignmentExpression;

      R := LastCodeRec;

//    if K2 - K1 > 0 then
      begin
        RelocateCode(K1, K2);
      end;

      if (R.Op = op) and (R.Arg1 = Id2) then
        Gen(op, id1, R.Arg2, id1)
      else
        Gen(op, id1, id2, id1);
    end
		else
    begin
      if NotDeclared then
        Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, result, 0);

			temp := NewTempVar;
			Gen(op, result, Parse_AssignmentExpression, temp);
			Gen(OP_ASSIGN, result, temp, result);
    end;
  end
  else
    result := Parse_LogicalORExpression(result);
    if IsCurrText('?') then
    begin
      Match('?');
      lg := NewLabel;
      lf := NewLabel;
      Gen(OP_GO_FALSE, lf, result, 0);
      result := NewTempVar;
      id1 := Parse_AssignmentExpression;
      Gen(OP_ASSIGN, result, id1, result);
      Match(':');
      Gen(OP_GO, lg, 0, 0);
      SetLabelHere(lf);
      id2 := Parse_AssignmentExpression;
      Gen(OP_ASSIGN, result, id2, result);
      SetLabelHere(lg);
    end;
end;

// STATEMENTS ------------------------------------------------------------------

procedure TJavaScriptParser.Parse_Statement;
begin
  EXECUTABLE_SWITCH := 1;

  Gen(OP_STMT, 0, 0, 0);

  if IsLabelID then
  begin
    StatementLabel := CurrToken.Text;
    Parse_SetLabel;
    Match(':');
  end;

  if IsCurrText('using') then
  begin
    EXECUTABLE_SWITCH := 0;
    Parse_UsingStatement;
  end
  else if IsCurrText('namespace') then
    Parse_Namespace
  else if IsCurrText('{') then
    Parse_Block
  else if IsCurrText('var') then
  begin
    Parse_VariableStatement;
    if WithStack.Count > 0 then
      Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
  end
  else if IsCurrText(';') then
    Parse_EmptyStatement
  else if IsCurrText('if') then
  begin
    Parse_IfStatement;
  end
  else if IsCurrText('do') then
    Parse_DoStatement
  else if IsCurrText('for') then
    Parse_ForStatement
  else if IsCurrText('goto') then
    Parse_GotoStatement
  else if IsCurrText('break') then
    Parse_BreakStatement
  else if IsCurrText('continue') then
    Parse_ContinueStatement
  else if IsCurrText('return') then
    Parse_ReturnStatement
  else if IsCurrText('while') then
    Parse_WhileStatement
  else if IsCurrText('with') then
    Parse_WithStatement
  else if IsCurrText('switch') then
    Parse_SwitchStatement
  else if IsCurrText('throw') then
    Parse_ThrowStatement
  else if IsCurrText('try') then
    Parse_TryStatement
  else if IsCurrText('print') then
  begin
    Match('print');
    Parse_PrintStatement;
  end
  else if IsCurrText('println') then
  begin
    Match('println');
    Parse_PrintlnStatement;
  end
  else
    Parse_ExpressionStatement;
end;

procedure TJavaScriptParser.Parse_Block;
begin
  Match('{');
  if not IsCurrText('}') then
    Parse_StatementList;
  Match('}');
end;

procedure TJavaScriptParser.Parse_StatementList;
begin
  repeat
    if IsCurrText('}') then
      break;
    if IsEOF then
      break;
    Parse_Statement;
  until false;
end;

procedure TJavaScriptParser.Parse_VariableStatement;
begin
  DECLARE_SWITCH := true;
  Match('var');
  Parse_VariableDeclarationList;
  DECLARE_SWITCH := false;
  Match(';');
  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
end;

procedure TJavaScriptParser.Parse_VariableDeclarationList;
begin
  repeat
    Parse_VariableDeclaration;
    if NotMatch(',') then
      break;
  until false;
end;

procedure TJavaScriptParser.Parse_VariableDeclaration;
var
  Id, temp, L: Integer;
begin
  Id := Parse_Ident;
  Gen(OP_DECLARE_LOCAL_VAR, CurrLevel, Id, 0);
  if IsCurrText('=') then
  begin
    DECLARE_SWITCH := false;
    Match('=');
    if Scanner.IsConstToken(CurrToken) then
    begin
      L := CurrLevel;
      if (L = 0) or (GetKind(L) = KindNAMESPACE) then
        Gen(OP_BEGIN_INIT_CONST, ID, 0, 0);

      temp := Parse_AssignmentExpression;
      Gen(OP_ASSIGN, Id, temp, Id);
      if GetKind(temp) = KindCONST then
      begin
{$IFNDEF PAXARM}
        if GetSymbolRec(temp).HasPAnsiCharType then
          Gen(OP_ASSIGN_TYPE, Id, typeSTRING, 0)
        else
{$ENDIF}
        if GetSymbolRec(temp).HasPWideCharType then
          Gen(OP_ASSIGN_TYPE, Id, typeSTRING, 0)
        else
          Gen(OP_ASSIGN_TYPE, Id, GetType(temp), 0);
      end;

      if (L = 0) or (GetKind(L) = KindNAMESPACE) then
        Gen(OP_END_INIT_CONST, ID, 0, 0);
    end
    else
      Gen(OP_ASSIGN, Id, Parse_AssignmentExpression, Id);
    DECLARE_SWITCH := true;
  end;
end;

procedure TJavaScriptParser.Parse_EmptyStatement;
begin
  Match(';');
end;

procedure TJavaScriptParser.Parse_IfStatement;
var
  lf, lg: Integer;
begin
	lf := NewLabel();
	Match('if');
	Match('(');
	Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
	Match(')');
	Parse_Statement;
	if IsCurrText('else') then
  begin
		lg := NewLabel;
		Gen(OP_GO, lg, 0, 0);
		SetLabelHere(lf);
		Match('else');
		Parse_Statement;
		SetLabelHere(lg);
  end
	else
		SetLabelHere(lf);
  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
end;

procedure TJavaScriptParser.Parse_DoStatement;
var
  lt, lg, l_loop: Integer;
begin
	lt := NewLabel();
	lg := NewLabel();
	SetLabelHere(lt);
  l_loop := lt;
	Match('do');

	Parse_LoopStmt(lg, lt, l_loop);

  Match('while');
	Match('(');
	Gen(OP_GO_TRUE, lt, Parse_Expression, 0);
	SetLabelHere(lg);
	Match(')');
	Match(';');
end;

procedure TJavaScriptParser.Parse_WithStatement;
var
  id, temp_id, object_id, l_try, cond_id: Integer;
begin
  Match('with');
  Match('(');

  id := Parse_Expression;

  if GetKind(id) = kindCONST then
  begin
    temp_id := NewTempVar;
    Gen(OP_ASSIGN, temp_id, id, temp_id);
    id := temp_id;
  end;

  object_id := NewTempVar(typeVARIANT);
  Gen(OP_TO_JS_OBJECT, id, 0, object_id);

  Gen(OP_BEGIN_WITH, id, 0, 0);
  WithStack.Push(id);
  Gen(OP_PUSH_CONTEXT, object_id, 0, 0);

  l_try := GenBeginTry;

  Match(')');
  Parse_Statement;

  Gen(OP_EXCEPT_SEH, 0, 0, 0);
  GenFinally;

  Gen(OP_POP_CONTEXT, object_id, 0, 0);
  Gen(OP_END_WITH, id, 0, 0);
  WithStack.Pop;

  Gen(OP_COND_RAISE, 0, 0, 0);
  cond_id := LastCodeRec.Res;
  Gen(OP_GO_TRUE, SkipLabelStack.Top, cond_id, 0);

  SetLabelHere(l_try);
  GenEndTry;
end;

procedure TJavaScriptParser.Parse_SwitchStatement;
var
  lg, l_loop, l_default, bool_id, expr_id, case_expr_id, lf, l_skip, I, N1, N2: Integer;
  lt: TIntegerStack;
  n_skips: TIntegerList;
begin
  l_loop := NewLabel;
  SetLabelHere(l_loop);
	lg := NewLabel();
	l_default := NewLabel();

	lt := TIntegerStack.Create;
  n_skips := TIntegerList.Create;

	bool_id := NewTempVar;
	Gen(OP_ASSIGN, bool_id, TrueId, bool_id);
  BreakStack.Push(lg, StatementLabel, l_loop);

	Match('switch');
	Match('(');
	expr_id := Parse_Expression();
	Match(')');
	Match('{'); // parse switch block

  repeat // parse switch sections

    repeat // parse switch labels
      if (IsCurrText('case')) then
      begin
        Match('case');

        lt.Push(NewLabel());
        case_expr_id := Parse_Expression();
        Gen(OP_EQ, expr_id, case_expr_id, bool_id);
        Gen(OP_GO_TRUE, lt.Top(), bool_id, 0);
      end
      else if (IsCurrText('default')) then
      begin
        Match('default');
        SetLabelHere(l_default);
        Gen(OP_ASSIGN, bool_id, TrueId, bool_id);
      end
      else
        break; //switch labels
      Match(':');
    until false;

    while (lt.Count > 0) do
      SetLabelHere(lt.Pop);

    lf := NewLabel();
    Gen(OP_GO_FALSE, lf, bool_id, 0);

    // parse statement list
    repeat
      if (IsCurrText('case')) then
        break;
      if (IsCurrText('default')) then
        break;
      if (IsCurrText('}')) then
        break;

      l_skip := NewLabel();
      SetLabelHere(l_skip);
      Parse_Statement;
      Gen(OP_GO, l_skip, 0, 0);
      n_skips.Add(CodeCard);

    until false;
    SetLabelHere(lf);

    if (IsCurrText('}')) then
      break;
  until false;

	BreakStack.Pop();
	SetLabelHere(lg);
	Match('}');

  if n_skips.Count >= 2 then
  for I:=0 to n_skips.Count - 2 do
  begin
    N1 := n_skips[I];
    N2 := n_skips[I+1];
    GetCodeRec(N1).Arg1 := GetCodeRec(N2).Arg1;
  end;
  N1 := n_skips[n_skips.Count - 1];
  GetCodeRec(N1).Op := OP_NOP;

  FreeAndNil(lt);

  FreeAndNil(n_skips);

  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
end;

procedure TJavaScriptParser.Parse_WhileStatement;
var
  lf, lg, l_loop: Integer;
begin
  lf := NewLabel;
  lg := NewLabel;
  SetLabelHere(lg);
  l_loop := lg;
  Match('while');
  Match('(');
  Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
  Match(')');

	Parse_LoopStmt(lf, lg, l_loop);

  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
  Gen(OP_GO, lg, 0, 0);
  SetLabelHere(lf);
end;

procedure TJavaScriptParser.Parse_ThrowStatement;
begin
  Match('throw');

  if ProcessedLineTerminator then
  begin
    Gen(OP_RAISE, 0, RaiseMode, 0);
    Exit;
  end;

  if IsCurrText(';') then
    Gen(OP_RAISE, 0, RaiseMode, 0)
  else
    Gen(OP_RAISE, Parse_Expression, RaiseMode, 0);

  Match(';');
end;

procedure TJavaScriptParser.Parse_TryStatement;
var
  id, type_id, l_try, BlockId: Integer;
begin
  Match('try');
  l_try := GenBeginTry;

  Parse_Block;
  Gen(OP_EXCEPT_SEH, 0, 0, 0);

  if IsCurrText('catch') then
  begin
    Gen(OP_GO, l_try, 0, 0);
    Match('catch');

    //ExceptionBlock

    GenExcept;

    BlockId := NewTempVar;
    LevelStack.push(BlockId);
    Gen(OP_BEGIN_BLOCK, BlockId, 0, 0);

    DECLARE_SWITCH := true;
    Match('(');
    id := Parse_Ident;
    DECLARE_SWITCH := false;
    Match(')');

    type_id := JS_ObjectClassId;

    Gen(OP_ASSIGN_TYPE, id, type_id, 0);

    GenExceptOn(type_id);
    Gen(OP_ASSIGN, id, CurrExceptionObjectId, id);

    Gen(OP_BEGIN_EXCEPT_BLOCK, 0, 0, 0);
    Parse_Block;
    if WithStack.Count > 0 then
      Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
    Gen(OP_END_EXCEPT_BLOCK, 0, 0, 0);
    Gen(OP_GO, l_try, 0, 0);
    Gen(OP_END_BLOCK, BlockId, 0, 0);
    LevelStack.Pop;

    GenExceptOn(0);

    if IsCurrText('finally') then
    begin
      Match('finally');
      GenFinally;
      Parse_Block;
      if WithStack.Count > 0 then
        Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
      GenCondRaise;
    end;

  end // except
  else if IsCurrText('finally') then
  begin
    Match('finally');
    GenFinally;
    Parse_Block;
    if WithStack.Count > 0 then
      Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
    GenCondRaise;
  end // finally
  else
    Match('finally');
  SetLabelHere(l_try);
  GenEndTry;
end;

procedure TJavaScriptParser.Parse_ForStatement;
var
  lf, l_iter, l_cond, l_stmt, l_loop: Integer;
  prop_id, obj_id, success_id: Integer;
begin
  l_loop := NewLabel;
  SetLabelHere(l_loop);

	Match('for');

	lf := NewLabel;
	l_iter := NewLabel;
	l_cond := NewLabel;
	l_stmt := NewLabel;

	Match('(');

	// parse for-initializer
	if not IsCurrText(';') then
  begin
    if IsNextText('in') then
    begin
      success_id := NewTempVar;
      obj_id := NewTempVar(typeVARIANT);
      prop_id := Parse_Ident;
      Match('in');
      SetLabelHere(l_iter);
      Gen(OP_TO_JS_OBJECT, Parse_Expression, 0, obj_id);
      Gen(OP_GET_NEXTJSPROP, obj_id, prop_id, success_id);
      Gen(OP_GO_FALSE, lf, success_id, 0);
      Match(')');
      // parse embedded statement

    	Parse_LoopStmt(lf, l_iter, l_loop);

      if WithStack.Count > 0 then
        Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
      Gen(OP_GO, l_iter, 0, 0);
      SetLabelHere(lf);

      Exit;
    end;

    if IsCurrText('var') then
    begin
      DECLARE_SWITCH := true;
      Match('var');

      repeat
        prop_id := CurrToken.Id;
        Parse_VariableDeclaration;
        DECLARE_SWITCH := false;

        if IsCurrText('in') then
        begin
          success_id := NewTempVar;
          obj_id := NewTempVar(typeVARIANT);
          Match('in');
          SetLabelHere(l_iter);
          Gen(OP_TO_JS_OBJECT, Parse_Expression, 0, obj_id);
          Gen(OP_GET_NEXTJSPROP, obj_id, prop_id, success_id);
          Gen(OP_GO_FALSE, lf, success_id, 0);
          Match(')');
         // parse embedded statement
	        Parse_LoopStmt(lf, l_iter, l_loop);

          if WithStack.Count > 0 then
            Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
          Gen(OP_GO, l_iter, 0, 0);
          SetLabelHere(lf);

          Exit;
        end;

        if IsCurrText(',') then
          DECLARE_SWITCH := true;

        if NotMatch(',') then
          break;

      until false;
    end
    else
      while true do
      begin
        Parse_Expression;
        if NotMatch(',') then
          break;
      end;
  end;

  // parse for-condition
  SetLabelHere(l_cond);
  Match(';');
  if not IsCurrText(';') then
    Gen(OP_GO_FALSE, lf, Parse_Expression, 0);
  Gen(OP_GO, l_stmt, 0, 0);

  // parse for-iterator
  SetLabelHere(l_iter);
  Match(';');
  if not IsCurrText(')') then
    while true do
    begin
      Parse_Expression;
      if NotMatch(',') then
        break;
    end;

  Gen(OP_GO, l_cond, 0, 0);

  // parse embedded statement
  SetLabelHere(l_stmt);
  Match(')');

	Parse_LoopStmt(lf, l_iter, l_loop);

  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
  Gen(OP_GO, l_iter, 0, 0);
  SetLabelHere(lf);
end;

procedure TJavaScriptParser.Parse_GotoStatement;
begin
  Match('goto');
  Gen(OP_GO, Parse_Label, 0, 0);
end;

procedure TJavaScriptParser.Parse_BreakStatement;
begin
	if BreakStack.Count = 0 then
    RaiseError(errBreakOrContinueOutsideOfLoop, []);
	Match('break');
	Gen(OP_GO, BreakStack.TopLabel, 0, 0);
  Match(';');
end;

procedure TJavaScriptParser.Parse_ContinueStatement;
begin
	if ContinueStack.Count = 0 then
    RaiseError(errBreakOrContinueOutsideOfLoop, []);
	Match('continue');
	Gen(OP_GO, ContinueStack.TopLabel, 0, 0);
  Match(';');
end;

procedure TJavaScriptParser.Parse_ReturnStatement;
begin
  Match('return');
  if ProcessedLineTerminator then
  begin
    if WithStack.Count > 0 then
       Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
  	Gen(OP_GO, SkipLabelStack.Top, 0, 0);
//    Gen(OP_EXIT, SkipLabelStack.Top, 0, CurrLevel);
    Exit;
  end;

  if not IsCurrText(';') then
    Gen(OP_ASSIGN, CurrResultId, Parse_Expression, CurrResultId);
  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
	Gen(OP_GO, SkipLabelStack.Top, 0, 0);
//  Gen(OP_EXIT, SkipLabelStack.Top, 0, CurrLevel);
  Match(';');
end;

procedure TJavaScriptParser.Parse_ExpressionStatement;
begin
  Parse_Expression;
  Match(';');
  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
end;

procedure TJavaScriptParser.Parse_PrintStatement;
var
  ID, ID_L1, ID_L2: Integer;
  b: Boolean;
begin
  if IsCurrText('(') then
  begin
    Match('(');
    b := true;
  end
  else
    b := false;

  repeat
    ID := Parse_Expression;
    ID_L1 := 0;
    ID_L2 := 0;
    if IsCurrText(':') then
    begin
      Match(':');
      ID_L1 := Parse_Expression;
    end;
    if IsCurrText(':') then
    begin
      Match(':');
      ID_L2 := Parse_Expression;
    end;

    Gen(OP_PRINT_EX, ID, ID_L1, ID_L2);
    if NotMatch(',') then
      Break;
  until false;

  if b then
    Match(')');

  if WithStack.Count > 0 then
    Gen(OP_CLEAR_REFERENCES, 0, 0, 0);
end;

procedure TJavaScriptParser.Parse_PrintlnStatement;
begin
  Parse_PrintStatement;
{$IFDEF PAXARM}
  Gen(OP_PRINT_EX, NewConst(typeUNICSTRING, #13#10), 0, 0);
{$ELSE}
  Gen(OP_PRINT_EX, NewConst(typeANSISTRING, #13#10), 0, 0);
{$ENDIF}
end;

procedure TJavaScriptParser.Parse_UsingStatement;
var
  unit_id, id: Integer;
  S: String;
  AlreadyExists: Boolean;
begin
  DECLARE_SWITCH := false;
	Match('using');

  UsedUnitList.Clear;

	repeat
    unit_id := Parse_UnitName(S);

    Gen(OP_BEGIN_USING, unit_id, 0, 0);
    AlreadyExists := GetKind(unit_id) = kindNAMESPACE;

    if IsCurrText('in') then
    begin
      Match('in');
      id := Parse_PCharLiteral;
      S := GetValue(id);

      if (PosCh('\', S) > 0) or (PosCh('/', S) > 0) then
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
      AddModuleFromFile(S, unit_id, false);

		if NotMatch(',') then
			Break;
  until false;

  Match(';');
end;

function TJavaScriptParser.IsLineTerminator: Boolean;
begin
  result := IsNewLine;
end;

procedure TJavaScriptParser.MatchLineTerminator;
begin
	if IsEOF then
    Exit;

	if not IsNewLine then
		RaiseError(errLineTerminatorExpected, []);

  while CurrToken.TokenClass = tcSeparator do
  begin
    Gen(OP_SEPARATOR, CurrModule.ModuleNumber, CurrToken.Id, 0);
    inherited Call_SCANNER;

    if IsEOF then
      Exit;
  end;
end;

function TJavaScriptParser.Parse_Label: Integer;
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

function TJavaScriptParser.IsAssignment_operator(const S: String): Boolean;
begin
  if S = '=' then
    result := true
  else if S = '*=' then
    result := true
  else if S = '/=' then
    result := true
  else if S = '%=' then
    result := true
  else if S = '+=' then
    result := true
  else if S = '-=' then
    result := true
  else if S = '<<=' then
    result := true
  else if S = '>>=' then
    result := true
  else if S = '>>>=' then
    result := true
  else if S = '&=' then
    result := true
  else if S = '^=' then
    result := true
  else if S = '|=' then
    result := true
  else
    result := false;
end;

function TJavaScriptParser.Parse_Assignment_operator: Integer;
begin
  result := 0;
  if IsCurrText('=') then
    result := OP_ASSIGN
  else if IsCurrText('*=') then
    result := OP_MULT
  else if IsCurrText('/=') then
    result := OP_DIV
  else if IsCurrText('%=') then
    result := OP_MOD
  else if IsCurrText('+=') then
    result := OP_PLUS
  else if IsCurrText('-=') then
    result := OP_MINUS
  else if IsCurrText('<<=') then
    result := OP_SHL
  else if IsCurrText('>>=') then
    result := OP_SHR
  else if IsCurrText('>>>=') then
    result := OP_SHR
  else if IsCurrText('&=') then
    result := OP_AND
  else if IsCurrText('^=') then
    result := OP_XOR
  else if IsCurrText('|=') then
    result := OP_OR
end;

function TJavaScriptParser.Parse_LogicalOR_operator: Integer;
begin
  result := 0;
  if IsCurrText('||') then
    result := OP_OR;
end;

function TJavaScriptParser.Parse_LogicalAND_operator: Integer;
begin
  result := 0;
  if IsCurrText('&&') then
    result := OP_AND;
end;

function TJavaScriptParser.Parse_BitwiseOR_operator: Integer;
begin
  result := 0;
  if IsCurrText('|') then
    result := OP_OR;
end;

function TJavaScriptParser.Parse_BitwiseXOR_operator: Integer;
begin
  result := 0;
  if IsCurrText('^') then
    result := OP_XOR;
end;

function TJavaScriptParser.Parse_BitwiseAND_operator: Integer;
begin
  result := 0;
  if IsCurrText('&') then
    result := OP_AND;
end;

function TJavaScriptParser.Parse_Equality_operator: Integer;
begin
  result := 0;
  if IsCurrText('==') then
    result := OP_EQ
  else if IsCurrText('!=') then
    result := OP_NE
  else if IsCurrText('===') then
    result := OP_EQ
  else if IsCurrText('!==') then
    result := OP_NE
end;

function TJavaScriptParser.Parse_Relational_operator: Integer;
begin
  result := 0;
  if IsCurrText('>') then
    result := OP_GT
  else if IsCurrText('>=') then
    result := OP_GE
  else if IsCurrText('<') then
    result := OP_LT
  else if IsCurrText('<=') then
    result := OP_LE
  else if IsCurrText('instanceof') then
    result := OP_INSTANCE_OF
end;

function TJavaScriptParser.Parse_Shift_operator: Integer;
begin
  result := 0;
  if IsCurrText('<<') then
    result := OP_SHL
  else if IsCurrText('>>') then
    result := OP_SHR
  else if IsCurrText('>>>') then
    result := OP_SHR
end;

function TJavaScriptParser.Parse_Additive_operator: Integer;
begin
  result := 0;
  if IsCurrText('+') then
    result := OP_PLUS
  else if IsCurrText('-') then
    result := OP_MINUS;
end;

function TJavaScriptParser.Parse_Multiplicative_operator: Integer;
begin
  result := 0;
  if IsCurrText('*') then
    result := OP_MULT
  else if IsCurrText('/') then
    result := OP_DIV
  else if IsCurrText('%') then
    result := OP_MOD;
end;

function TJavaScriptParser.IsLabelId: boolean;
begin
  result := (CurrToken.TokenClass = tcIdentifier) and IsNextText(':');
end;

function TJavaScriptParser.Parse_SetLabel: Integer;
begin
  result := Parse_Ident;
  Gen(OP_LABEL, 0, 0, 0);
  SetKind(result, KindLABEL);
end;

function TJavaScriptParser.InContext: Boolean;
begin
  result := WithStack.Count > 0;
end;

procedure TJavaScriptParser.Call_SCANNER;
begin
  ProcessedLineTerminator := false;
  inherited Call_SCANNER;
  if IsLineTerminator then
  begin
    ProcessedLineTerminator := true;
    MatchLineTerminator;
  end;
end;

procedure TJavaScriptParser.Match(const S: String);
begin
  if S = ';' then
  begin
    if IsLineTerminator then
      MatchLineTerminator
    else if IsCurrText(';') then
    begin
      Call_SCANNER;
    end
    else if ProcessedLineTerminator then
    begin
      // ok
    end
    else // error
      inherited Match(S);

    Exit;
  end;

  inherited Match(S);
end;

function TJavaScriptParser.GetCurrSelfId: Integer;
begin
  result := levelStack.Top;
  result := TKernel(kernel).SymbolTable.GetSelfId(result);
end;

function TJavaScriptParser.ConvString(const S: String): String;
begin
  result := PAXCOMP_SCANNER.ConvertString(S);
end;

function TJavaScriptParser.Parse_Ident: Integer;
var
  L, RefId, ObjectId: Integer;
  S: String;
begin
  result := inherited Parse_Ident;

  if DECLARE_SWITCH then
    Exit;
  L := GetLevel(result);
  if L = 0 then
    Exit;
  if L = CurrLevel then
    Exit;

  if GetKind(L) in KindSUBS then
  begin
    ClosureIds.Add(result);

    S := GetName(result);
    ObjectId := CurrSelfId;
    RefId := NewField(S, ObjectId);
    Gen(OP_FIELD, ObjectId, RefId, RefId);
    result := RefId;
  end;
end;

procedure TJavaScriptParser.Parse_LoopStmt(l_break, l_continue, l_loop: Integer);
begin
  BreakStack.Push(l_break, l_loop);
  ContinueStack.Push(l_continue, l_loop);
  BeginLoop;
  Parse_Statement;
  EndLoop;
  BreakStack.Pop;
  ContinueStack.Pop;
end;


end.
