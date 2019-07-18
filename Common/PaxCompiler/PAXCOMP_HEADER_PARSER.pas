////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_HEADER_PARSER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_HEADER_PARSER;
interface
uses {$I uses.def}

{$IFDEF DRTTI}
  RTTI,
{$ENDIF}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_VAROBJECT;
const
  MAX_PARAM = 30;
type
  TKindSub = (ksFunction, ksProcedure, ksConstructor, ksDestructor);
  TParamMod = (pmByVal, pmByRef, pmConst, pmOut);

  THeaderParser = class
  private
    Buff: String;
    P: Integer;
    L: Integer;
    symbol_table: Pointer;
    LevelId: Integer;
    CurrTypeName: String;
    DefVal: String;
    SignDefVal: Boolean;
    GenTypeExpected: Boolean;
    operator_list: TAssocStrings;

    procedure AddOperator(const S1, S2: String);
    procedure ScanGenType;
    procedure ScanIdent;
    procedure ScanDigits;
    procedure ScanHexDigits;
    procedure ScanNumberLiteral;
    procedure ScanHexLiteral;
    procedure ScanStringLiteral;
    procedure ScanNumCharLiteral;

    function MaxTypeId(T1, T2: Integer): Integer;
    procedure ScanToken;
    function NotMatch(const S: String): Boolean;
    function IsCurrText(const S: String): Boolean;
    function IsNextText(const S: String): Boolean;
    function ParseFullName: String;
    function Parse_Type: String;
    function Parse_SimpleExpression: Variant;
    function Parse_Term: Variant;
    function Parse_Factor: Variant;
    procedure Parse_ConstantInitialization(ArrObject: TArrObject; var TypeId: Integer);

    procedure Parse_FormalParameterList(ch: Char);
    procedure RaiseError(const Message: String; params: array of Const);
    function Parse_SetConstructor: Variant;
  public
    Name: String;
    ResType: String;
    KS: TKindSub;
    NP: Integer;
    CC: Integer;
    IsShared: Boolean;
    IsProperty: Boolean;
    IsDeprecated: Boolean;
    Params: array[1..MAX_PARAM] of String;
    Types: array[1..MAX_PARAM] of String;
    Mods: array[1..MAX_PARAM] of TParamMod;
    Values: array[1..MAX_PARAM] of Variant;
    Optionals: array[1..MAX_PARAM] of Boolean;
    DefVals: array[1..MAX_PARAM] of String;
    ReadIdent: String;
    WriteIdent: String;
    IsDefault: Boolean;
    CallMode: Integer;
    SavedMessage: String;
    NamespaceId: Integer;
    UsedNamespaceList: TIntegerList;
    IsAbstract: Boolean;

    LastFactorTypeId: Integer;
    DestFactorTypeId: Integer;

    TokenClass: TTokenClass;
    CurrToken: String;

    IsOverloaded: Boolean;

    AbstractMethodCount: Integer;

    CurrImportUnit: String;
    kernel: Pointer;

    constructor Create;
    destructor Destroy; override;
    procedure Init(const Header: String; i_symbol_table: Pointer;
                  i_LevelId: Integer);
    procedure Match(const S: String);
    procedure Call_SCANNER;
    function Parse_Expression: Variant;
    function Parse: Boolean;
    function Parse_Ident: String;
    function Parse_QualTypeId: Integer;

    function Register_TypeDeclaration: Integer;
    function RegisterTypeAlias(const TypeName: String;
                               OriginTypeId: Integer): Integer;
    function Register_SubrangeTypeDeclaration(const TypeName: String): Integer;
    function Register_EnumTypeDeclaration(const TypeName: String): Integer;
    function Register_SetTypeDeclaration(const TypeName: String): Integer;
    function Register_ArrayTypeDeclaration(const TypeName: String): Integer;
    function Register_RecordTypeDeclaration(const TypeName: String): Integer;
    function Register_StringTypeDeclaration(const TypeName: String): Integer;
    function Register_OrdinalType: Integer;
    function Register_Type: Integer;

    function LookupId(const S: String): Integer;
    function LookupTypeId(const S: String): Integer;
    function LookupAllIds(const S: String): TIntegerList;
    function Register_Variable(const VarName: String; Address: Pointer): Integer;
    function Register_Constant(const ConstName: String): Integer;
    function Register_RecordTypeField(const FieldName: String; Offset: Integer = - 1): Integer;
    function Register_VariantRecordTypeField(const FieldName: String;
                                VarCount: Integer): Integer;
    function Register_TypeAlias(const TypeName: String): Integer;
  end;

  ESilentException = class(EAbort)
  end;

implementation

uses
{$IFDEF DRTTI}
  PAXCOMP_2010,
  PAXCOMP_2010REG,
{$ENDIF}
  PAXCOMP_KERNEL,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_STDLIB;

var
  Undefined: Variant;

constructor THeaderParser.Create;
begin
  inherited;
  NamespaceId := 0;
  UsedNamespaceList := TIntegerList.Create;

  operator_list := TAssocStrings.Create;

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

destructor THeaderParser.Destroy;
begin
  UsedNamespaceList.Free;
  operator_list.Free;
  inherited;
end;

procedure THeaderParser.AddOperator(const S1, S2: String);
begin
  operator_list.Add(S1, S2);
end;

procedure THeaderParser.Init(const Header: String; i_symbol_table: Pointer;
                             i_LevelId: Integer);
begin
  Buff := Header + #255#255#255;

  P := SLow(Buff);
  symbol_table := i_symbol_table;
  LevelId := i_LevelId;
  CallMode := cmNONE;
  IsAbstract := false;
  LastFactorTypeId := 0;
  DestFactorTypeId := 0;
  ReadIdent := '';
  WriteIdent := '';
  CurrTypeName := '';
  ResType := '';
  IsOverloaded := false;
  IsShared := false;
  IsDeprecated := false;
end;

procedure THeaderParser.ScanIdent;
begin
  while ByteInSet(Buff[P + L], IdsSet) do
    Inc(L);
  TokenClass := tcIdentifier;
end;

procedure THeaderParser.ScanGenType;
var
  K: Integer;
begin
  L := 1;
  K := 0;
  repeat
    if Buff[P + L] = '>' then
    begin
      if K = 0 then
      begin
        break;
      end
      else
      begin
        Dec(K);
        Inc(L);
      end;
    end
    else if Buff[P + L] = '<' then
    begin
      Inc(K);
      Inc(L);
    end
    else
      Inc(L);
  until false;
end;

procedure THeaderParser.ScanDigits;
begin
  while IsDigit(Buff[P + L]) do
    Inc(L);
end;

procedure THeaderParser.ScanHexDigits;
begin
  while ByteInSet(Buff[P + L], [Ord('0')..Ord('9'),
             Ord('a')..Ord('f'), Ord('A')..Ord('F')]) do
    Inc(L);
end;

procedure THeaderParser.ScanNumberLiteral;
begin
  ScanDigits;
  TokenClass := tcIntegerConst;

  if (Buff[P + L] = '.') and (Buff[P + L + 1] <> '.') then
  begin
    Inc(L);
    ScanDigits;
    TokenClass := tcDoubleConst;
  end;
end;

procedure THeaderParser.ScanHexLiteral;
begin
  Inc(L);
  ScanHexDigits;
  TokenClass := tcIntegerConst;
end;

procedure THeaderParser.ScanNumCharLiteral;
begin
  Inc(P); // #
  if Buff[P] = '$' then
  begin
    Inc(L);
    ScanHexDigits;
  end
  else
    ScanDigits;
  TokenClass := tcNumCharConst;
end;

procedure THeaderParser.ScanStringLiteral;
var
  K: Integer;
begin
  K := 0;
  Inc(P);
  if (Buff[P] = CHAR_AP) and (Buff[P+1] <> CHAR_AP) then // empty string
  begin
    TokenClass := tcPCharConst;
    Exit;
  end;

  repeat
     if Buff[P] = #255 then
     begin
       RaiseError(errUnterminatedString, []);
       Exit;
     end;

     if (Buff[P + L] = CHAR_AP) and (Buff[P + L + 1] = CHAR_AP) then
     begin
       Inc(L);
       buff[P + L] := CHAR_REMOVE;
     end
     else if (Buff[P + L] = CHAR_AP) then
        break;

     Inc(L);
     Inc(K);
  until false;

  if K = 1 then
    TokenClass := tcCharConst
  else
    TokenClass := tcPCharConst;
end;

procedure THeaderParser.ScanToken;
begin
  L := 0;
  repeat
    case Buff[P] of
      'a'..'z','A'..'Z','_':
      begin
        ScanIdent;
        Exit;
      end;
      '0'..'9':
      begin
        ScanNumberLiteral;
        Exit;
      end;
      '$':
      begin
        ScanHexLiteral;
        Exit;
      end;
      '#':
      begin
        ScanNumCharLiteral;
        Exit;
      end;
      CHAR_AP:
      begin
        ScanStringLiteral;
        Exit;
      end;
      '>':
      begin
        TokenClass := tcSpecial;
        L := 1;
        if Buff[P+1] = '=' then
          L := 2;
        Exit;
      end;
      '<':
      begin
        if GenTypeExpected then
        begin
          ScanGenType;
          Exit;
        end;
        TokenClass := tcSpecial;
        L := 1;
        if ByteInSet(Buff[P+1], [Ord('='), Ord('>')]) then
          L := 2;
        Exit;
      end;
      '(', ')','[', ']', ',', ':', ';', '=', '+', '/', '-', '*', #255:
      begin
        TokenClass := tcSpecial;
        L := 1;
        Exit;
      end;
      '.':
      begin
        TokenClass := tcSpecial;
        L := 1;
        if Buff[P+1] = '.' then
          L := 2;
        Exit;
      end;
      ' ', #9, #13, #10:
      begin
        Inc(P);
      end;
      else
        RaiseError(errSyntaxError, []);
    end;
  until false;
end;

function THeaderParser.IsNextText(const S: String): Boolean;
var
  temp_L, temp_P: Integer;
  temp_token: String;
  tempTokenClass: TTokenClass;
begin
  temp_token := CurrToken;
  temp_L := L;
  temp_P := P;
  tempTokenClass := TokenClass;
  try
    Call_SCANNER;
    result := IsCurrText(S);
  finally
    L := temp_L;
    P := temp_P;
    CurrToken := temp_token;
    TokenClass := tempTokenClass;
  end;
end;

procedure THeaderParser.Call_SCANNER;
begin
  ScanToken;
  CurrToken := SCopy(Buff, P, L);
  Inc(P, L);

  if TokenClass in [tcPCharConst, tcCharConst] then
    Inc(P);

  if StrEql(CurrToken, 'Undefined') then
    TokenClass := tcVariantConst
  else
   {$IFDEF UNIC}
   begin
      if StrEql(CurrToken, 'String') then
        CurrToken := 'UnicodeString'
      else if StrEql(CurrToken, 'Char') then
        CurrToken := 'WideChar'
      else if StrEql(CurrToken, 'PChar') then
        CurrToken := 'PWideChar';
   end;
   {$ELSE}
   begin

   end;
   {$ENDIF}

   if SignDefVal then
     DefVal := DefVal + CurrToken;
end;

function THeaderParser.Parse_Ident: String;
begin
  result := CurrToken;

  if TokenClass <> tcIdentifier then
    RaiseError(errIdentifierExpected, [CurrToken]);
  Call_SCANNER;
end;

function THeaderParser.Parse_Type: String;
begin
  result := UpperCase(CurrToken);

  if IsCurrText('ARRAY') then
  begin
    Call_SCANNER;
    Match('OF');
    result := 'ARRAY OF ' + CurrToken;
    Parse_Ident;
  end
  else
    Parse_Ident;

  while IsCurrText('.') do
  begin
    Call_SCANNER;
    result := result + '.' + UpperCase(CurrToken);
    Parse_Ident;
  end;

  if IsCurrText('<') then
  begin
    GenTypeExpected := true;
    try
      Dec(P);
      Call_SCANNER;
      result := result + CurrToken;
      Call_SCANNER;
      result := result + '>';
      Call_SCANNER;
    finally
      GenTypeExpected := false;
    end;
  end;
end;

procedure THeaderParser.Parse_FormalParameterList(ch: Char);
var
  I, K: Integer;
  S: String;
  V: Variant;
  PM: TParamMod;
  Opt: Boolean;
begin
  Call_SCANNER;
  NP := 0;
  if not IsCurrText(ch) then
  begin
    repeat
      if IsCurrText('var') then
      begin
        Call_SCANNER;
        PM := pmByRef;
      end
      else if IsCurrText('out') then
      begin
        Call_SCANNER;
        PM := pmOut;
      end
      else if IsCurrText('const') then
      begin
        Call_SCANNER;
        PM := pmConst;
      end
      else
        PM := pmByVal;

      K := 0;
      repeat
        Inc(K);
        Params[NP + K] := Parse_Ident;
        if NotMatch(',') then
          break;
      until false;

      if PM in [pmByRef, pmOut] then
      begin
        if IsCurrText(':') then
        begin
          Match(':');
          S := Parse_Type;
        end
        else
          S := 'PVOID';
      end
      else
      begin
        if (PM = pmConst) and (not IsCurrText(':')) then
        begin
          S := 'PVOID';
        end
        else
        begin
          Match(':');
          S := Parse_Type;
        end;
      end;

      if IsCurrText('=') then
      begin
        CurrTypeName := S;

        DefVal := '';
        SignDefVal := true;
        try
          Match('=');
          V := Parse_Expression;
        finally
          SignDefVal := false;
          DefVal := Copy(DefVal, 1, Length(DefVal) - 1);
        end;
        Opt := true;
      end
      else
      begin
        V := Unassigned;
        Opt := false;
      end;

      for I:=1 to K do
      begin
        Inc(NP);
        Types[NP] := S;
        Mods[NP] := PM;
        Values[NP] := V;
        Optionals[NP] := Opt;
        DefVals[NP] := DefVal;
      end;

      if NotMatch(';') then
        Break;
    until false;
  end;
  Match(ch);
end;

function THeaderParser.MaxTypeId(T1, T2: Integer): Integer;
var
  F1, F2, S1, S2: Integer;
begin
  F1 := TBaseSymbolTable(symbol_table)[T1].FinalTypeId;
  F2 := TBaseSymbolTable(symbol_table)[T2].FinalTypeId;

  if F1 = F2 then
  begin
    result := T1;
    Exit;
  end;
  if (F1 in IntegerTypes) and (F2 in IntegerTypes) then
  begin
    S1 := PAXCOMP_SYS.Types.GetSize(F1);
    S2 := PAXCOMP_SYS.Types.GetSize(F2);
    if S1 = S2 then
    begin
      if F1 in UnsignedIntegerTypes then
        result := F1
      else
        result := F2;
    end
    else if S1 > S2 then
      result := F1
    else
      result := F2;
  end
  else if F1 in IntegerTypes then
    result := F2
  else if F2 in IntegerTypes then
    result := F1
{$IFNDEF PAXARM}
  else if TBaseSymbolTable(symbol_table)[T1].HasPAnsiCharType then
    result := T1
{$ENDIF}
  else if TBaseSymbolTable(symbol_table)[T1].HasPWideCharType then
    result := T1
{$IFNDEF PAXARM}
  else if TBaseSymbolTable(symbol_table)[T2].HasPAnsiCharType then
    result := T2
{$ENDIF}
  else if TBaseSymbolTable(symbol_table)[T2].HasPWideCharType then
    result := T2
  else if (F1 in RealTypes) and (F2 in RealTypes) then
  begin
    S1 := PAXCOMP_SYS.Types.GetSize(F1);
    S2 := PAXCOMP_SYS.Types.GetSize(F2);
    if S1 > S2 then
      result := F1
    else
      result := F2;
  end
  else
    result := F1;
end;

function THeaderParser.Parse_Expression: Variant;
var
  Op: Integer;
begin
  result := Parse_SimpleExpression;

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

    if Op = OP_EQ then
  		result := result = Parse_SimpleExpression
    else if Op = OP_NE then
  		result := result <> Parse_SimpleExpression
    else if Op = OP_GT then
  		result := result > Parse_SimpleExpression
    else if Op = OP_GE then
  		result := result >= Parse_SimpleExpression
    else if Op = OP_LT then
  		result := result < Parse_SimpleExpression
    else if Op = OP_LE then
  		result := result <= Parse_SimpleExpression;

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

    LastFactorTypeId := typeBOOLEAN;
  end;
end;

function VarTypeIsString(const V: Variant): Boolean;
var
  VT: Integer;
begin
  VT := VarType(V);
  result := (VT = varString) {$IFDEF UNIC}or (VT = varUString){$ENDIF};
end;

function THeaderParser.Parse_SimpleExpression: Variant;
var
  Op, T1, FT1: Integer;
  V: Variant;
  W: Word;
  S: String;
  SetObject, SetObject1, SetObject2: TSetObject;
begin
  result := Parse_Term;

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
    T1 := TBaseSymbolTable(symbol_table)[LastFactorTypeId].TerminalTypeId;
    FT1 := TBaseSymbolTable(symbol_table)[T1].FinalTypeId;

    Call_SCANNER;

    V := Parse_Term;
    if VarType(result) = varString then
{$IFNDEF PAXARM}
      if TBaseSymbolTable(symbol_table)[LastFactorTypeId].FinalTypeId = typeANSICHAR then
      begin
        S := Chr(Integer(V));
        V := S;
        LastFactorTypeId := typePANSICHAR;
      end
      else
{$ENDIF}
      if TBaseSymbolTable(symbol_table)[LastFactorTypeId].FinalTypeId in IntegerTypes then
      begin
        W := 0;

        if Length(result) = 1 then
        begin
          S := result;
          W := Ord(S[1]) + V;
        end
        else if Length(result) = 2 then
        begin
          S := result;
          W := 256 * Ord(S[1]) + Ord(S[2]) + V;
        end
        else
          RaiseError(errInternalError, []);

        if W <= 255 then
          result := String(chr(W))
        else
          result := String(chr(Hi(W))) + String(chr(Lo(W)));
        V := '';
      end;

    if Op = OP_PLUS then
    begin
      if FT1 = typeSET then
      begin
        SetObject1 := VariantToVarObject(result) as TSetObject;
        SetObject2 := VariantToVarObject(V) as TSetObject;
        SetObject := TSetObject.Create(symbol_table,
          SetObject1.Value + SetObject2.Value, H_TByteSet, typeBYTE);
        result := VarObjectToVariant(SetObject);
      end
      else
      begin
        if VarTypeIsString(V) and (not VarTypeIsString(result)) then
          result := Chr(Integer(result))
        else if (not VarTypeIsString(V)) and VarTypeIsString(result) then
          V := Chr(Integer(V));

    		result := result + V;
      end;
    end
    else if Op = OP_MINUS then
    begin
      if FT1 = typeSET then
      begin
        SetObject1 := VariantToVarObject(result) as TSetObject;
        SetObject2 := VariantToVarObject(V) as TSetObject;
        SetObject := TSetObject.Create(symbol_table,
          SetObject1.Value - SetObject2.Value, H_TByteSet, typeBYTE);
        result := VarObjectToVariant(SetObject);
      end
      else
    		result := result - V;
    end
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

    LastFactorTypeId := MaxTypeId(T1, LastFactorTypeId);
  end;
end;

function THeaderParser.Parse_Term: Variant;
var
  Op, FT1: Integer;
  V: Variant;
  SetObject, SetObject1, SetObject2: TSetObject;
begin
  result := Parse_Factor;

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
    FT1 := TBaseSymbolTable(symbol_table)[LastFactorTypeId].FinalTypeId;

    Call_SCANNER;

    V := Parse_Factor;

    if Op = OP_MULT then
    begin
      if FT1 = typeSET then
      begin
        SetObject1 := VariantToVarObject(result) as TSetObject;
        SetObject2 := VariantToVarObject(V) as TSetObject;
        SetObject := TSetObject.Create(symbol_table,
          SetObject1.Value * SetObject2.Value, H_TByteSet, typeBYTE);
        result := VarObjectToVariant(SetObject);
      end
      else
    		result := result * V;
    end
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
        else
          RaiseError(errDivisionByZero, []);
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

    LastFactorTypeId := MaxTypeId(FT1, LastFactorTypeId);
    if Op = OP_DIV then
      LastFactorTypeId := typeEXTENDED;

  end;
end;

function THeaderParser.Parse_Factor: Variant;
var
  I, J: Integer;
  W: Word;
  temp_LevelId: Integer;
  SubName: String;
  D: Double;
  curr: Currency;
label
  again, fin;
begin
  temp_LevelId := LevelId;

  LastFactorTypeId := 0;

{$IFDEF PAXARM}
  if TokenClass = tcCharConst then
  begin
    LastFactorTypeId := typeWIDECHAR;
    result := Ord(CurrToken[1]);
    Call_SCANNER;
  end
  else if TokenClass = tcNumCharConst then
  begin
    LastFactorTypeId := typeWIDECHAR;

    result := StrToInt(CurrToken);
    Call_SCANNER;
  end
  else if TokenClass = tcPCharConst then
  begin
    LastFactorTypeId := typePWIDECHAR;

    result := CurrToken;
    Call_SCANNER;
  end
{$ELSE}
  if TokenClass = tcCharConst then
  begin
    LastFactorTypeId := typeANSICHAR;
    result := Ord(CurrToken[1]);
    Call_SCANNER;
  end
  else if TokenClass = tcNumCharConst then
  begin
    LastFactorTypeId := typeANSICHAR;

    result := StrToInt(CurrToken);
    Call_SCANNER;
  end
  else if TokenClass = tcPCharConst then
  begin
    LastFactorTypeId := typePANSICHAR;

    result := CurrToken;
    Call_SCANNER;
  end
{$ENDIF}
  else if TokenClass = tcIntegerConst then
  begin
    LastFactorTypeId := typeINTEGER;

    val(CurrToken, i, j);
    if j = 0 then begin
      if Pos('$', CurrToken) > 0 then
      begin
        LastFactorTypeId := typeCARDINAL;
{$IFDEF VARIANTS}
        result := Cardinal(i);
{$ELSE}
        result := Integer(i);
{$ENDIF}
      end
      else
      begin
        LastFactorTypeId := typeINTEGER;
        result := i;
      end;
    end
    else begin
      LastFactorTypeId := typeINT64;
{$IFDEF VARIANTS}
      result := StrToInt64 (CurrToken);
{$ELSE}
      result := Integer(StrToInt64 (CurrToken));
{$ENDIF}
    end;

    Call_SCANNER;
  end
  else if TokenClass = tcVariantConst then
  begin
    LastFactorTypeId := typeVARIANT;

    result := Undefined;
    Call_SCANNER;
  end
  else if TokenClass = tcDoubleConst then
  begin
    if DestFactorTypeId <> 0 then
      LastFactorTypeId := DestFactorTypeId
    else
      LastFactorTypeId := typeDOUBLE;

    Val(CurrToken, D, I);
    result := D;
    Call_SCANNER;
  end
  else if IsCurrText('nil') then
  begin
    LastFactorTypeId := typePOINTER;

    result := 0;
    Call_SCANNER;
  end
  else if IsCurrText('true') then
  begin
    LastFactorTypeId := typeBOOLEAN;

    result := true;
    Call_SCANNER;
  end
  else if IsCurrText('false') then
  begin
    LastFactorTypeId := typeBOOLEAN;

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
    I := LookupId(CurrToken);
    if I > 0 then
      result := TBaseSymbolTable(symbol_table).GetLowBoundRec(I).Value
    else
      RaiseError(errUndeclaredIdentifier, [CurrToken]);
    LastFactorTypeId := TBaseSymbolTable(symbol_table)[I].FinalTypeId;
		Call_SCANNER;
    Match(')');
  end
	else if IsCurrText('high') then
  begin
    Call_SCANNER;
    Match('(');
    I := LookupId(CurrToken);
    if I > 0 then
       result := TBaseSymbolTable(symbol_table).GetHighBoundRec(I).Value
    else
      RaiseError(errUndeclaredIdentifier, [CurrToken]);
    LastFactorTypeId := TBaseSymbolTable(symbol_table)[I].FinalTypeId;
		Call_SCANNER;
    Match(')');
  end
	else if IsCurrText('SizeOf') then
  begin
    Call_SCANNER;
    Match('(');
    I := LookupId(CurrToken);
    if I > 0 then
      result := TBaseSymbolTable(symbol_table)[I].Size
    else if TokenClass = tcPCharConst then
      result := Length(CurrToken) + 1
    else
      RaiseError(errUndeclaredIdentifier, [CurrToken]);
    LastFactorTypeId := typeINTEGER;
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
    LastFactorTypeId := typeINTEGER;
  end
	else if IsCurrText('chr') then
  begin
		Call_SCANNER;
    Match('(');
		result := Parse_Expression;
    Match(')');
{$IFDEF PAXARM}
    LastFactorTypeId := typeWIDECHAR;
{$ELSE}
    LastFactorTypeId := typeANSICHAR;
{$ENDIF}
  end
  else if IsCurrText('(') then
  begin
    Match('(');
    result := Parse_Expression;
    Match(')');
  end
  else if IsCurrText('[') then
  begin
    result := Parse_SetConstructor;
    LastFactorTypeId := H_TByteSet;
  end
  else
  begin

again:

    I := TBaseSymbolTable(symbol_table).LookUp(CurrToken, LevelId, true);
    if (LevelId > 0) and (I = 0) then
    begin
      if (NamespaceId > 0) and (I = 0) then
        I := TBaseSymbolTable(symbol_table).LookUp(CurrToken, NamespaceId, true);

      if I = 0 then
      begin
        for J := 0 to UsedNamespaceList.Count - 1 do
        begin
          I := TBaseSymbolTable(symbol_table).LookUp(CurrToken,
              UsedNamespaceList[J], true);
          if I > 0 then
            break;
        end;
      end;
    end;

    if I = 0 then
      I := TBaseSymbolTable(symbol_table).LookUp(CurrToken, H_PascalNamespace, true);

    if I = 0 then
      I := TBaseSymbolTable(symbol_table).LookUp(CurrToken, 0, true);

    if I > 0 then
    begin
      LastFactorTypeId := TBaseSymbolTable(symbol_table)[I].TerminalTypeId;

      if (TBaseSymbolTable(symbol_table)[I].Kind = kindTYPE) and
          (TBaseSymbolTable(symbol_table)[I].FinalTypeId = typeCLASS) then
        result := Integer(TBaseSymbolTable(symbol_table)[I].PClass)
      else
        result := TBaseSymbolTable(symbol_table)[I].Value;

      if not IsEmpty(result) then
      begin
        Call_SCANNER;
        goto fin;
      end;
    end;

    if I = 0 then
    begin
      I := LookupId(CurrToken);
      if I = 0 then
        RaiseError(errUndeclaredIdentifier, [CurrToken]);
    end;

    if TBaseSymbolTable(symbol_table)[I].Kind = KindTYPE then
    begin
      Call_SCANNER;
      if IsCurrText('(') then
      begin
        Match('(');
        result := Parse_Expression;
        Match(')');
{$IFNDEF PAXARM}
        if TBaseSymbolTable(symbol_table)[I].HasPAnsiCharType then
        begin
          if VarType(result) in [varByte, varInteger] then
          begin
            W := Word(result);
            if W <= 255 then
              result := String(chr(W))
            else
              result := String(chr(Hi(W))) + String(chr(Lo(W)));
          end
          else
            result := String(result);

          LastFactorTypeId := typePANSICHAR;
        end
        else
{$ENDIF}
        if TBaseSymbolTable(symbol_table)[I].HasPWideCharType then
        begin
          if VarType(result) in [varByte, varInteger] then
          begin
            W := Word(result);
            if W <= 255 then
              result := String(chr(W))
            else
              result := String(chr(Hi(W))) + String(chr(Lo(W)));
          end
          else
            result := String(result);

          LastFactorTypeId := typePWIDECHAR;
        end
        else
          LastFactorTypeId := TBaseSymbolTable(symbol_table)[I].TerminalTypeId;
      end;

      goto fin;
    end
    else if TBaseSymbolTable(symbol_table)[I].Kind = KindNAMESPACE then
    begin
      Call_SCANNER;
      Match('.');
      LevelId := I;
      goto again;
    end
    else if TBaseSymbolTable(symbol_table)[I].Kind = KindSUB then
    begin
      SubName := CurrToken;
      Call_SCANNER;
      Match('(');
      result := Parse_Expression;
      if StrEql(SubName, 'Trunc') then
      begin
{$IFDEF VARIANTS}
        curr := result;
        result := Trunc(curr)
{$ELSE}
        result := Integer(Trunc(result))
{$ENDIF}
      end
      else if StrEql(SubName, 'Abs') then
        result := Abs(result);
      Match(')');
      Exit;
    end;

    I := LookupId(CurrToken);
    if I > 0 then
      if TBaseSymbolTable(symbol_table)[I].Kind = kindCONST then
      begin
        result := TBaseSymbolTable(symbol_table)[I].Value;
        Call_SCANNER;
        Exit;
      end;

    RaiseError(errConstantExpressionExpected, []);
  end;

  fin:

  LevelId := temp_LevelId;

end;

function THeaderParser.IsCurrText(const S: String): Boolean;
begin
  result := StrEql(CurrToken, S);
end;

procedure THeaderParser.Match(const S: String);
begin
  if IsCurrText(S) then
    Call_SCANNER
  else
    RaiseError(errTokenExpected, [S, CurrToken]);
end;

function THeaderParser.NotMatch(const S: String): Boolean;
begin
  if not IsCurrText(S) then
    result := true
  else
  begin
    result := false;
    Call_SCANNER;
  end;
end;

procedure THeaderParser.RaiseError(const Message: String; params: array of Const);
begin
  SavedMessage := Message;

  if RaiseE then
    raise PaxCompilerException.Create(Format(Message, params))
  else
    raise ESilentException.Create(Format(Message, params));
end;

function THeaderParser.Parse: Boolean;
var
  HasResult, IsOperator: Boolean;
  I: Integer;
begin
  result := true;

  IsAbstract := false;

  try
    Call_SCANNER;

    IsShared := false;
    IsDeprecated := false;
    IsOverloaded := false;
    IsProperty := false;
    HasResult := false;
    cc := ccREGISTER;

    if IsCurrText('class') then
    begin
      Call_SCANNER;
      IsShared := true;
    end;

    if IsCurrText('property') then
    begin
      Call_SCANNER;
      IsProperty := true;

      Name := CurrToken;

      Parse_Ident;

      if IsCurrText(';') then
      begin
        Match(';');
        ResType := '';
        Exit;
      end;

      if IsCurrText('[') then
        Parse_FormalParameterList(']')
      else
        NP := 0;

      Match(':');
      ResType := Parse_Type;

      ReadIdent := '';
      WriteIdent := '';

      while IsCurrText('read') or IsCurrText('write') do
      begin
        if IsCurrText('read') then
        begin
          Call_SCANNER;
          ReadIdent := CurrToken;
          Parse_Ident;
        end
        else if IsCurrText('write') then
        begin
          Call_SCANNER;
          WriteIdent := CurrToken;
          Parse_Ident;
        end;
      end;
      Match(';');
      IsDefault := IsCurrText('default');

      Exit;
    end;

    IsOperator := false;

    if IsCurrText('function') then
    begin
      KS := ksFunction;
      HasResult := true;
      Call_SCANNER;
    end
    else if IsCurrText('procedure') then
    begin
      KS := ksProcedure;
      Call_SCANNER;
    end
    else if IsCurrText('operator') then
    begin
      Call_SCANNER;
      CallMode := cmSTATIC;
      IsOperator := true;
    end
    else if IsCurrText('constructor') then
    begin
      KS := ksConstructor;
      Call_SCANNER;
    end
    else if IsCurrText('destructor') then
    begin
      KS := ksDestructor;
      Call_SCANNER;
    end
    else
      Match('procedure');

    Name := CurrToken;
    if IsOperator then
    begin
      I := operator_list.Keys.IndexOf(Name);
      if I >= 0 then
        Name := operator_list.Values[I];
    end;

    if not (IsCurrText('(') or IsCurrText(';')) then
      Parse_Ident;

    if IsCurrText('(') then
      Parse_FormalParameterList(')')
    else
      NP := 0;

    if HasResult then
    begin
      Match(':');
      ResType := UpperCase(CurrToken);
      Parse_Ident;
    end
    else if IsCurrText(':') then
    begin
      KS := ksFUNCTION;
      Match(':');
      ResType := UpperCase(CurrToken);
      Parse_Ident;
    end
    else
      ResType := 'VOID';

    if IsCurrText(';') then
      Match(';');

    repeat
      if IsCurrText('abstract') then
      begin
        Call_SCANNER;
        Match(';');
        IsAbstract := true;
        Inc(AbstractMethodCount);
      end
      else if IsCurrText('static') then
      begin
        CallMode := cmSTATIC;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('virtual') then
      begin
        CallMode := cmVIRTUAL;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('overload') then
      begin
        IsOverloaded := true;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('deprecated') then
      begin
        IsDeprecated := true;
        Call_SCANNER;
        if not IsCurrText(';') then
          Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('reintroduce') then
      begin
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('dynamic') then
      begin
        CallMode := cmDYNAMIC;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('override') then
      begin
        CallMode := cmOVERRIDE;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('register') then
      begin
        cc := ccREGISTER;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('stdcall') then
      begin
        cc := ccSTDCALL;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('safecall') then
      begin
        cc := ccSAFECALL;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('cdecl') then
      begin
        cc := ccCDECL;
        Call_SCANNER;
        Match(';');
      end
      else if IsCurrText('msfastcall') then
      begin
        cc := ccMSFASTCALL;
        Call_SCANNER;
        Match(';');
      end
      else
        break;
    until false;

  except
    result := false;
  end;
end;

function THeaderParser.Parse_SetConstructor: Variant;
var
  v1, v2: Variant;
  J, TypeId, TypeBaseId: Integer;
  ByteSet: TByteSet;
  SetObject: TSetObject;
begin
  ByteSet := [];

  Match('[');
  if not IsCurrText(']') then
  begin
    repeat
      v1 := Parse_Expression;
      if IsCurrText('..') then
      begin
        Match('..');
        v2 := Parse_Expression;
        for J:=Integer(V1) to Integer(V2) do
          ByteSet := ByteSet + [J];
      end
      else
      begin
        ByteSet := ByteSet + [Integer(v1)];
      end;

      If NotMatch(',') then
        break;
    until false;

  end
  else
    result := Undefined;

  Match(']');

  if ByteSet <> [] then
  begin
    if CurrTypeName = '' then
    begin
      TypeId := H_TByteSet;
      TypeBaseId := typeBYTE;
    end
    else
    begin
      TypeId := LookupId(CurrTypeName);
      typeBaseId := TBaseSymbolTable(symbol_table).GetTypeBase(TypeId);
      typeBaseId := TBaseSymbolTable(symbol_table)[TypeBaseId].FinalTypeId;
    end;

    SetObject := TSetObject.Create(symbol_table, ByteSet, TypeId, typeBaseId);
    result := VarObjectToVariant(SetObject);
  end;
end;

function THeaderParser.LookupTypeId(const S: String): Integer;
begin
  result := TBaseSymbolTable(symbol_table).LookUpType(S, true);
end;

function THeaderParser.LookupId(const S: String): Integer;
var
  J: Integer;
begin
  result := TBaseSymbolTable(symbol_table).LookUp(S, LevelId, true);
  if result = 0 then
  begin
    if NamespaceId > 0 then
      result := TBaseSymbolTable(symbol_table).LookUp(S, NamespaceId, true);

    if result = 0 then
    begin
      for J := 0 to UsedNamespaceList.Count - 1 do
      begin
        result := TBaseSymbolTable(symbol_table).LookUp(S, UsedNamespaceList[J], true);
        if result > 0 then
          break;
      end;
    end;

    if result = 0 then
      result := TBaseSymbolTable(symbol_table).LookUp(S, H_PascalNamespace, true);

    if result = 0 then
      result := TBaseSymbolTable(symbol_table).LookUp(S, 0, true);
  end;
end;

function THeaderParser.LookupAllIds(const S: String): TIntegerList;
var
  Id, J: Integer;
begin
  result := TIntegerList.Create;

  Id := TBaseSymbolTable(symbol_table).LookUp(S, LevelId, true);
  if Id > 0 then
    result.Add(id);

  if NamespaceId > 0 then
  begin
    Id := TBaseSymbolTable(symbol_table).LookUp(S, NamespaceId, true);
    if Id > 0 then
      result.Add(id);
  end;

  for J := 0 to UsedNamespaceList.Count - 1 do
  begin
    Id := TBaseSymbolTable(symbol_table).LookUp(S, UsedNamespaceList[J], true);
    if Id > 0 then
      result.Add(id);
  end;

  Id := TBaseSymbolTable(symbol_table).LookUp(S, H_PascalNamespace, true);
  if Id > 0 then
    result.Add(id);

  Id := TBaseSymbolTable(symbol_table).LookUp(S, 0, true);
  if Id > 0 then
    result.Add(id);
end;

function THeaderParser.Register_SubrangeTypeDeclaration(const TypeName: String): Integer;
var
  V1, V2: Variant;
  TypeBaseId: Integer;
begin
  V1 := Parse_Expression;
  Match('..');
  V2 := Parse_Expression;

  TypeBaseId := LastFactorTypeId;

  result := TBaseSymbolTable(symbol_table).RegisterSubrangeType(LevelId, TypeName, TypeBaseId, V1, V2);
end;

function THeaderParser.Register_EnumTypeDeclaration(const TypeName: String): Integer;
var
  Temp: Integer;
  S: String;
begin
  result := TBaseSymbolTable(symbol_table).RegisterEnumType(LevelId, TypeName, typeINTEGER);

  Match('(');
  Temp := -1;

  repeat
    S := Parse_Ident;
    if IsCurrText('=') then
    begin
      Match('=');
      temp := Parse_Expression;
    end
    else
      Inc(temp);

    TBaseSymbolTable(symbol_table).RegisterEnumValue(result, S, temp);

    if NotMatch(',') then
      Break;

  until false;
  Match(')');
end;

function THeaderParser.Register_SetTypeDeclaration(const TypeName: String): Integer;
var
  TypeBaseId: Integer;
begin
  Match('set');
  Match('of');
  TypeBaseId := Register_OrdinalType;
  result := TBaseSymbolTable(symbol_table).RegisterSetType(LevelId, TypeName, TypeBaseId);
end;

function THeaderParser.Register_ArrayTypeDeclaration(const TypeName: String): Integer;
var
  RangeTypeId, ElemTypeId, I: Integer;
  RangeTypeIds: TIntegerList;
begin
  result := 0;
  RangeTypeIds := TIntegerList.Create;
  try
    Match('array');
    Match('[');
    repeat
      if IsNextText('..') then
        RangeTypeId := Register_SubrangeTypeDeclaration('')
      else
      begin
        RangeTypeId := LookupId(CurrToken);
        if RangeTypeId = 0 then
          RaiseError(errUndeclaredIdentifier, [CurrToken]);
        Call_SCANNER;
      end;
      RangeTypeIds.Add(RangeTypeId);
      if not IsCurrText(',') then
        break
      else
        Call_SCANNER;
    until false;
    Match(']');
    Match('of');

    ElemTypeId := Register_Type;

    for I := RangeTypeIds.Count - 1 downto 0 do
    begin
      RangeTypeId := RangeTypeIds[I];
      result := TBaseSymbolTable(symbol_table).RegisterArrayType(LevelId, TypeName, RangeTypeId, ElemTypeId, 1);
      ElemTypeId := result;
    end;
  finally
    RangeTypeIds.Free;
  end;
end;

function THeaderParser.Register_RecordTypeDeclaration(const TypeName: String): Integer;
var
  L: TStringList;
  I, TypeId: Integer;
begin
  result := TBaseSymbolTable(symbol_table).RegisterRecordType(LevelId, TypeName, 1);

  Match('record');
  L := TStringList.Create;
  try
    repeat
      if IsCurrText('end') then
        Break;

      L.Clear;
      repeat  // parse ident list
        L.Add(Parse_Ident);
        if NotMatch(',') then
          break;
      until false;

      Match(':');

      TypeID := Register_Type;

      for I:=0 to L.Count - 1 do
        TBaseSymbolTable(symbol_table).RegisterTypeField(result, L[I], TypeId);

      if IsCurrText(';') then
        Match(';');

    until false;
  finally
    L.Free;
  end;

  Match('end');
end;

function THeaderParser.Register_OrdinalType: Integer;
begin
  if TokenClass = tcIdentifier then
  begin
    result := LookupId(CurrToken);
    if result > 0 then
    begin
      Call_SCANNER;
      Exit;
    end;
  end;

  if IsCurrText('(') then
    result := Register_EnumTypeDeclaration('')
  else
    result := Register_SubrangeTypeDeclaration('');
end;

function THeaderParser.Register_Type: Integer;
const TypeName = '';
var
  Id: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
begin
  if not (IsCurrText('set') or IsCurrText('array') or IsCurrText('record')) then
  if TokenClass = tcIdentifier then
  begin
    Id := LookupId(CurrToken);
    if Id > 0 then
      if TBaseSymbolTable(symbol_table)[id].Kind = KindTYPE then
      begin
        Call_SCANNER;
{$IFDEF PAXARM}
        result := Id;
{$ELSE}
        if (id = typeANSISTRING) and IsCurrText('[') then
        begin
          result := 0;

          Match('[');
          V := Parse_Expression;
          if VarType(V) in [varInteger, varByte] then
          begin
            result := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
              TypeName, V);
          end
          else
            RaiseError(errIncompatibleTypesNoArgs, []);
          Match(']');
        end
        else
          result := Id;
{$ENDIF}

        Exit;
      end;
  end;

  if IsCurrText('set') then
    result := Register_SetTypeDeclaration(TypeName)
  else if IsCurrText('array') then
    result := Register_ArrayTypeDeclaration(TypeName)
  else if IsCurrText('record') then
    result := Register_RecordTypeDeclaration(TypeName)
  else if IsCurrText('(') then
    result := Register_EnumTypeDeclaration(TypeName)
  else
    result := Register_SubrangeTypeDeclaration(TypeName);
end;

function THeaderParser.Register_StringTypeDeclaration(const TypeName: String): Integer;
{$IFNDEF PAXARM}
var
  V: Variant;
{$ENDIF}
begin
  Match('string');
{$IFDEF PAXARM}
  result := typeUNICSTRING;
{$ELSE}
  {$IFDEF UNIC}
  result := typeUNICSTRING;
  {$ELSE}
  result := typeANSISTRING;
  {$ENDIF}
  if IsCurrText('[') then
  begin
    Match('[');
    V := Parse_Expression;
    if VarType(V) in [varInteger, varByte] then
    begin
      result := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
        TypeName, V);
    end
    else
      RaiseError(errIncompatibleTypesNoArgs, []);
    Match(']');
  end
  else
    result := typeANSISTRING;
{$ENDIF}
end;

function THeaderParser.ParseFullName: String;
begin
  result := CurrToken;
  while Buff[P] = '.' do
  begin
    Call_SCANNER;
    Call_SCANNER;
    result := result + '.' + CurrToken;
  end;
end;

function THeaderParser.Parse_QualTypeId: Integer;
var
  S: String;
  temp: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
begin
  S := Parse_Ident;
  result := LookupId(S);

  temp := LevelId;

  try

    while IsCurrText('.')
{$IFNDEF PAXARM}
          or IsCurrText('[')
{$ENDIF}
          do
    begin
      LevelId := result;

      if IsCurrText('.') then
      begin
        Match('.');
        S := Parse_Ident;
        result := LookupId(S);
      end
{$IFNDEF PAXARM}
      else
      begin
        Match('[');
        V := Parse_Expression;
        if VarType(V) in [varInteger, varByte] then
        begin
          result := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
            '', V);
        end
        else
          RaiseError(errIncompatibleTypesNoArgs, []);
        Match(']');
      end;
{$ENDIF}
    end;

  finally

    LevelId := temp;

  end;
end;

function THeaderParser.Register_TypeAlias(const TypeName: String): Integer;
var
  OriginTypeId: Integer;
begin
  if IsCurrText('=') then
    Match('=')
  else if IsCurrText(':') then
    Match(':');

  OriginTypeId := Parse_QualTypeId;
  result := TBaseSymbolTable(symbol_table).RegisterTypeAlias(LevelId,
      TypeName, OriginTypeId);
end;

function THeaderParser.RegisterTypeAlias(const TypeName: String;
                                         OriginTypeId: Integer): Integer;
begin
  result := TBaseSymbolTable(symbol_table).RegisterTypeAlias(LevelId, TypeName,
    OriginTypeId);
end;

function THeaderParser.Register_TypeDeclaration: Integer;
var
  TypeName: String;
  id: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
begin
  TypeName := Parse_Ident;
  Match('=');

  if not (IsCurrText('set') or IsCurrText('array') or IsCurrText('record')) then
  if TokenClass = tcIdentifier then
  begin
    Id := LookupId(CurrToken);
    if Id > 0 then
      if TBaseSymbolTable(symbol_table)[id].Kind = KindTYPE then
      begin
        Call_SCANNER;
{$IFNDEF PAXARM}
        if (id = typeANSISTRING) and IsCurrText('[') then
        begin
          result := 0;

          Match('[');
          V := Parse_Expression;
          if VarType(V) in [varInteger, varByte] then
          begin
            result := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
              TypeName, V);
          end
          else
            RaiseError(errIncompatibleTypesNoArgs, []);
          Match(']');
        end
        else
{$ENDIF}
          result := RegisterTypeAlias(TypeName, Id);

        Exit;
      end;
  end;

  if IsCurrText('set') then
    result := Register_SetTypeDeclaration(TypeName)
  else if IsCurrText('array') then
    result := Register_ArrayTypeDeclaration(TypeName)
  else if IsCurrText('record') then
    result := Register_RecordTypeDeclaration(TypeName)
  else if IsCurrText('(') then
    result := Register_EnumTypeDeclaration(TypeName)
  else
    result := Register_SubrangeTypeDeclaration(TypeName);
end;

function THeaderParser.Register_Variable(const VarName: String; Address: Pointer): Integer;
var
  TypeId: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
  S: String;
{$IFDEF DRTTI}
  t: TRTTIType;
  curr_kernel: TKernel;
{$ENDIF}
begin
  TypeId := 0;

  S := '';
  Match(':');

  if not (IsCurrText('set') or IsCurrText('array') or IsCurrText('record')) then
  if TokenClass = tcIdentifier then
  begin
    S := ParseFullName;

    TypeId := LookupTypeId(S);
    if TypeId > 0 then
    begin
      if TBaseSymbolTable(symbol_table)[TypeId].Kind = KindTYPE then
      begin
        Call_SCANNER;
{$IFNDEF PAXARM}
        if (TypeId = typeANSISTRING) and IsCurrText('[') then
        begin
          Match('[');
          V := Parse_Expression;
          if VarType(V) in [varInteger, varByte] then
          begin
            TypeId := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
              '', V);
          end
          else
            RaiseError(errIncompatibleTypesNoArgs, []);
          Match(']');
        end;
{$ENDIF}
        result := TBaseSymbolTable(symbol_table).RegisterVariable(LevelId, VarName, TypeId, Address);
        Exit;
      end;
    end
    else // TypeId = 0
    begin
{$IFDEF DRTTI}
      if CurrImportUnit <> '' then
      if kernel <> nil then
      begin
        curr_kernel := TKernel(kernel);

        S := CurrImportUnit + '.' + S;
        t := PaxContext.FindType(S);
        if t <> nil then
        begin
          TypeId := RegisterType(LevelId, t,
             curr_kernel.SymbolTable);
          result := curr_kernel.SymbolTable.RegisterVariable(LevelId,
            VarName, TypeId, Address);
          Exit;
        end;
      end;
{$ENDIF}
    end;
  end;

  if IsCurrText('set') then
    TypeId := Register_SetTypeDeclaration('')
  else if IsCurrText('array') then
    TypeId := Register_ArrayTypeDeclaration('')
  else if IsCurrText('record') then
    TypeId := Register_RecordTypeDeclaration('')
  else if IsCurrText('(') then
    TypeId := Register_EnumTypeDeclaration('')
  else if not IsNextText(';') then
    TypeId := Register_SubrangeTypeDeclaration('');

  if TypeId = 0 then
    if S <> '' then
    with TBaseSymbolTable(symbol_table) do
    begin
      ExternList.Add(Card + 1,
                     S,
                     erTypeId);
    end;

  result := TBaseSymbolTable(symbol_table).RegisterVariable(LevelId, VarName, TypeId, Address);
end;

procedure THeaderParser.Parse_ConstantInitialization(ArrObject: TArrObject; var TypeId: Integer);
var
  TempArrObject: TArrObject;
  SimpleObject: TSimpleObject;
  V: Variant;
  S: String;
  DummyId, TempId, J: Integer;
  AllTypes: TIntegerList;
begin
  DummyId := -1;

  Match('(');
  repeat
    if IsCurrText('(') then
    begin
      TempArrObject := TArrObject.Create(nil);
      Parse_ConstantInitialization(TempArrObject, DummyId);
      ArrObject.AddVarObject(TempArrObject);
      if NotMatch(',') then
        break;
    end
    else if IsCurrText(')') then
      break
    else
    begin
      if IsNextText(':') then // record init
      begin
        S := CurrToken;

        if TypeId > 0 then
        begin
          TempId := TBaseSymbolTable(symbol_table).Lookup(S, TypeId, true);
          if TempId = 0 then
          begin
            AllTypes := LookupAllIds(S);
            try
              for J := 0 to AllTypes.Count - 1 do
              begin
                TempId := TBaseSymbolTable(symbol_table).Lookup(S, AllTypes[J], true);
                if TempId > 0 then
                begin
                  TypeId := Alltypes[J];
                  break;
                end;
              end;
            finally
              Alltypes.Free;
            end;
          end;
        end;

        Call_SCANNER;
        Match(':');
        if IsCurrText('(') then
        begin
          TempArrObject := TArrObject.Create(nil);
          Parse_ConstantInitialization(TempArrObject, DummyId);
          ArrObject.AddVarObject(TempArrObject);
        end
        else
        begin
          V := Parse_Expression;
          SimpleObject := TSimpleObject.Create(nil, V, S);
          ArrObject.AddVarObject(SimpleObject);
        end;
        if NotMatch(';') then
          break;
      end
      else // array init
      begin

        V := Parse_Expression;
        SimpleObject := TSimpleObject.Create(nil, V, S);
        ArrObject.AddVarObject(SimpleObject);

        if NotMatch(',') then
          break;
      end;
    end;
  until false;

  Match(')');
end;

function THeaderParser.Register_Constant(const ConstName: String): Integer;
var
  TypeId, temp, I: Integer;
  V: Variant;
  ArrObject: TArrObject;
  SimpleObject: TSimpleObject;
  S: String;
begin
  if IsCurrText('=') then
  begin
    Call_SCANNER;
    V := Parse_Expression;

    if LastFactorTypeId = 0 then
      result := TBaseSymbolTable(symbol_table).RegisterConstant(LevelId, ConstName, V)
    else
      result := TBaseSymbolTable(symbol_table).RegisterConstant(LevelId, ConstName, LastFactorTypeId, V);
    Exit;
  end;

  Match(':');

  if not (IsCurrText('set') or IsCurrText('array') or IsCurrText('record')) then
  begin
    if TokenClass = tcIdentifier then
    begin
      TypeId := LookupTypeId(CurrToken);
      if IsNextText('.') then
      begin
        Call_SCANNER;
        Call_SCANNER;
        temp := LevelId;
        LevelId := TypeId;
        TypeId := LookupId(CurrToken);
        LevelId := temp;
      end;

      if TypeId > 0 then
        if TBaseSymbolTable(symbol_table)[TypeId].Kind = KindTYPE then
        begin
          Call_SCANNER;
{$IFNDEF PAXARM}
          if (TypeId = typeANSISTRING) and IsCurrText('[') then
          begin
            Match('[');
            V := Parse_Expression;
            if VarType(V) in [varInteger, varByte] then
            begin
              TypeId := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
                '', V);
            end
            else
              RaiseError(errIncompatibleTypesNoArgs, []);
            Match(']');
          end;
{$ENDIF}
          Match('=');

          if IsCurrText('(') then
          begin
            ArrObject := TArrObject.Create(symbol_table);
            Parse_ConstantInitialization(ArrObject, TypeId);
            V := VarObjectToVariant(ArrObject);
          end
          else
          begin
            DestFactorTypeId := TBaseSymbolTable(symbol_table)[TypeId].FinalTypeId;
            V := Parse_Expression;
            DestFactorTypeId := 0;
          end;

          result := TBaseSymbolTable(symbol_table).RegisterConstant(LevelId, ConstName, TypeId, V);
          Exit;
        end;
    end;
  end;

  if IsCurrText('set') then
    TypeId := Register_SetTypeDeclaration('')
  else if IsCurrText('array') then
    TypeId := Register_ArrayTypeDeclaration('')
  else if IsCurrText('record') then
    TypeId := Register_RecordTypeDeclaration('')
  else if IsCurrText('(') then
    TypeId := Register_EnumTypeDeclaration('')
  else
    TypeId := Register_SubrangeTypeDeclaration('');

  Match('=');

  if IsCurrText('(') then
  begin
    ArrObject := TArrObject.Create(symbol_table);
    Parse_ConstantInitialization(ArrObject, TypeId);
    V := VarObjectToVariant(ArrObject);
  end
  else
  begin
    V := Parse_Expression;
    if TBaseSymbolTable(symbol_table)[TypeId].FinalTypeId = typeARRAY then
    begin
      ArrObject := TArrObject.Create(symbol_table);

      S := V;
      for I:=SLow(S) to SHigh(S) do
      begin
        SimpleObject := TSimpleObject.Create(nil, S[I], '');
        ArrObject.AddVarObject(SimpleObject);
      end;

      V := VarObjectToVariant(ArrObject);
    end;
  end;

  result := TBaseSymbolTable(symbol_table).RegisterConstant(LevelId, ConstName, TypeId, V);
end;

function THeaderParser.Register_RecordTypeField(const FieldName: String; Offset: Integer = - 1): Integer;
var
  TypeId: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
begin
  Match(':');
  if TokenClass = tcIdentifier then
  begin
    TypeId := LookupTypeId(CurrToken);
    if TypeId > 0 then
      if TBaseSymbolTable(symbol_table)[TypeId].Kind = KindTYPE then
      begin
        Call_SCANNER;
{$IFNDEF PAXARM}
        if (TypeId = typeANSISTRING) and IsCurrText('[') then
        begin
          Match('[');
          V := Parse_Expression;
          if VarType(V) in [varInteger, varByte] then
          begin
            TypeId := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
              '', V);
          end
          else
            RaiseError(errIncompatibleTypesNoArgs, []);
          Match(']');
        end;
{$ENDIF}
        result := TBaseSymbolTable(symbol_table).RegisterTypeField(LevelId, FieldName, TypeId, Offset);
        Exit;
      end;
  end;

  if IsCurrText('set') then
    TypeId := Register_SetTypeDeclaration('')
  else if IsCurrText('(') then
    TypeId := Register_EnumTypeDeclaration('')
  else
    TypeId := Register_SubrangeTypeDeclaration('');

  result := TBaseSymbolTable(symbol_table).RegisterTypeField(LevelId, FieldName, TypeId, Offset);
end;

function THeaderParser.Register_VariantRecordTypeField(const FieldName: String;
                                VarCount: Integer): Integer;
var
  TypeId: Integer;
{$IFNDEF PAXARM}
  V: Variant;
{$ENDIF}
begin
  Match(':');
  if TokenClass = tcIdentifier then
  begin
    TypeId := LookupTypeId(CurrToken);
    if TypeId > 0 then
      if TBaseSymbolTable(symbol_table)[TypeId].Kind = KindTYPE then
      begin
        Call_SCANNER;
{$IFNDEF PAXARM}
        if (TypeId = typeANSISTRING) and IsCurrText('[') then
        begin
          Match('[');
          V := Parse_Expression;
          if VarType(V) in [varInteger, varByte] then
          begin
            TypeId := TBaseSymbolTable(symbol_table).RegisterShortStringType(LevelId,
              '', V);
          end
          else
            RaiseError(errIncompatibleTypesNoArgs, []);
          Match(']');
        end;
{$ENDIF}
        result := TBaseSymbolTable(symbol_table).RegisterVariantRecordTypeField(LevelId, FieldName, TypeId, VarCount);
        Exit;
      end;
  end;

  if IsCurrText('set') then
    TypeId := Register_SetTypeDeclaration('')
  else if IsCurrText('(') then
    TypeId := Register_EnumTypeDeclaration('')
  else
    TypeId := Register_SubrangeTypeDeclaration('');

  result := TBaseSymbolTable(symbol_table).RegisterVariantRecordTypeField(LevelId, FieldName, TypeId, VarCount);
end;

end.
