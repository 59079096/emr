////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PASCAL_SCANNER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_PASCAL_SCANNER;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_SCANNER;

type
  TPascalScanner = class(TBaseScanner)
    procedure ScanCharLiteral;
    procedure ScanStringLiteral(ch: Char); override;
    procedure ReadCustomToken; override;

    function Scan_Expression: Variant; override;
    function Scan_SimpleExpression: Variant;
    function Scan_Term: Variant;
    function Scan_Ident: Variant; override;
  end;

implementation

uses
  PAXCOMP_KERNEL;

procedure TPascalScanner.ScanCharLiteral;
var
  s: String;
  I, P1, P2: Integer;
begin
  GetNextChar; // #
  ScanHexDigits;
  Token.TokenClass := tcCharConst;
  Token.Tag := 1;
  if LA(1) <> '#' then
    Exit;
  CustomStringVal := '';
  Token.TokenClass := tcPCharConst;
  Token.Tag := 2;

  s := SCopy(Buff, Token.Position + 1, Position - Token.Position);
  I := StrToInt(s);
  CustomStringVal := CustomStringVal + Chr(I);

  while LA(1) = '#' do
  begin
    GetNextChar; // #
    P1 := Position;
    if LA(1) = '$' then
      GetNextChar;
    ScanHexDigits;
    P2 := Position;
    s := SCopy(Buff, P1 + 1, P2 - P1);
    I := StrToInt(s);
    CustomStringVal := CustomStringVal + Chr(I);
  end;

  while ByteInSet(LA(1), [Ord('#'), Ord(CHAR_AP)]) do
  begin
    GetNextChar; // #
    if LA(0) = CHAR_AP then
    begin
      I := Position + 1;
      inherited ScanStringLiteral(CHAR_AP);
      s := SCopy(Buff, I, Position - I);
      I := PosCh(CHAR_REMOVE, s);
      while I > 0 do
      begin
        Delete(s, I, 1);
        I := PosCh(CHAR_REMOVE, s);
      end;
      CustomStringVal := CustomStringVal + s;
    end
    else if LA(1) = '$' then
    begin
      GetNextChar;
      I := Position + 1;
      ScanHexDigits;
      s := SCopy(Buff, I, Position - I + 1);
      I := StrToInt('$' + s);
      CustomStringVal := CustomStringVal + chr(I);
    end
    else
    begin
      I := Position + 1;
      ScanDigits;
      s := SCopy(Buff, I, Position - I + 1);
      I := StrToInt(s);
      CustomStringVal := CustomStringVal + chr(I);
    end;
  end;
end;

procedure TPascalScanner.ScanStringLiteral(ch: Char);
var
  s: String;
  I: Integer;
begin
  inherited;

  if LA(1) <> '#' then
    Exit;

  s := SCopy(Buff, Token.Position + 1, Position - Token.Position - 1);
  I := PosCh(CHAR_REMOVE, s);
  while I > 0 do
  begin
    Delete(s, I, 1);
    I := PosCh(CHAR_REMOVE, s);
  end;
  CustomStringVal := s;

  while ByteInSet(LA(1), [Ord('#'), Ord(ch)]) do
  begin
    GetNextChar; // #
    if LA(1) = '$' then
    begin
      GetNextChar;
      I := Position + 1;
      ScanHexDigits;
      s := SCopy(Buff, I, Position - I + 1);
      I := StrToInt('$' + s);
      CustomStringVal := CustomStringVal + chr(I);
    end
    else if LA(0) = ch then
    begin
      I := Position + 1;
      inherited ScanStringLiteral(ch);
      s := SCopy(Buff, I, Position - I);
      CustomStringVal := CustomStringVal + s;
    end
    else
    begin
      I := Position + 1;
      ScanDigits;
      s := SCopy(Buff, I, Position - I + 1);
      I := StrToInt(s);
      CustomStringVal := CustomStringVal + chr(I);
    end;
  end;

  Token.TokenClass := tcPCharConst;
  Token.Tag := 2;
end;

procedure TPascalScanner.ReadCustomToken;
var
  c: Char;
  S: String;
begin
  repeat
    GetNextChar;
    c := LA(0);
    Token.Position := Position;
    if IsWhiteSpace(c) then
    begin
      continue;
    end
{$IFDEF LINUX}
    else if c = #10 then
      ScanSeparator
{$ELSE}
{$IFDEF MACOS}
    else if c = #10 then
      ScanSeparator
{$ELSE}
    else if c = #13 then
      ScanSeparator
{$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
    else if c = #10 then
      ScanSeparator
{$ENDIF}
    else if IsEOF(c) then
      ScanEOF
    else if IsEOF then
      ScanEOF

{$IFDEF HTML}
    else if (c = '?') or (c = '%') then
    begin
      case ScannerState of
        ScanText:
          RaiseError(errSyntaxError, []);
        ScanProg:
        if LA(1) = '>' then
        begin
          ScannerState := scanText;
          GetNextChar;

          if (LA(1) = '<') and (LA(2) in ['?','%']) then
          begin
            ScannerState := scanText;
            continue;
          end;

//          GetNextChar;
          Token.Position := Position + 1;

          ScanHtmlString('');
        end
        else
        begin
          {$IFDEF CPP_SYN}
          if LA(1) = '=' then
          begin
            GetNextChar;
            ScanSpecial;
            Token.Id := OP_ASSIGN;
            Exit;
          end;
          {$ENDIF}
        end;
      end;
    end
    else if c = '<' then
    begin
      case ScannerState of
        scanText:
        begin
          if LA(1) = '?' then
          begin
            GetNextChar;
            GetNextChar;
            ScanChars([Ord('a')..Ord('z')] + [Ord('A')..Ord('Z')]);

            if not StrEql('pax', Trim(Token.Text)) then
              RaiseError(errSyntaxError, []);

            ScannerState := scanProg;
            Token.TokenClass := tcNone;
            Continue;
          end
          else if LA(1) = '%' then
          begin
            GetNextChar;
            ScannerState := scanProg;

            if LA(1) = '=' then
            begin
              GetNextChar;
              InsertText('print');
            end;

            Continue;
          end
          else if ByteInSet(LA(1), [Ord('a')..Ord('z'),Ord('A')..Ord('Z'), Ord('!')]) then
            ScanHtmlString(c)
          else
            RaiseError(errSyntaxError, []);
        end;
        scanProg:
        begin
          ScanSpecial;
          if LA(1) = '=' then
          begin
            GetNextChar;
            ScanSpecial;
            Token.Id := OP_LE;
          end
          else if LA(1) = '>' then
          begin
            GetNextChar;
            ScanSpecial;
            Token.Id := OP_NE;
          end
          else
            Token.Id := OP_LT;
        end;
      end;
    end

{$ENDIF}

    else if IsAlpha(c) then
    begin
      ScanIdentifier;

      token.Length := Position - token.Position + 1;

      S := Token.Text;

      if StrEql(S, 'in') then
      begin
        ScanSpecial;
        Token.Id := OP_SET_MEMBERSHIP;
      end
      else if StrEql(S, 'div') then
      begin
        ScanSpecial;
        Token.Id := OP_IDIV;
      end
      else if StrEql(S, 'mod') then
      begin
        ScanSpecial;
        Token.Id := OP_MOD;
      end
      else if StrEql(S, 'shl') then
      begin
        ScanSpecial;
        Token.Id := OP_SHL;
      end
      else if StrEql(S, 'shr') then
      begin
        ScanSpecial;
        Token.Id := OP_SHR;
      end
      else if StrEql(S, 'and') then
      begin
        ScanSpecial;
        Token.Id := OP_AND;
      end
      else if StrEql(S, 'or') then
      begin
        ScanSpecial;
        Token.Id := OP_OR;
      end
      else if StrEql(S, 'xor') then
      begin
        ScanSpecial;
        Token.Id := OP_XOR;
      end
      else if StrEql(S, 'not') then
      begin
        ScanSpecial;
        Token.Id := OP_NOT;
      end
      else if StrEql(S, 'is') then
      begin
        ScanSpecial;
        Token.Id := OP_IS;
      end
      else if StrEql(S, 'as') then
      begin
        ScanSpecial;
        Token.Id := OP_AS;
      end;

    end
    else if IsDigit(c) then
      ScanNumberLiteral
    else if c = '$' then
      ScanHexLiteral
    else if c = CHAR_AP then
      ScanStringLiteral(CHAR_AP)
    else if c = '+' then
    begin
      ScanSpecial;
      Token.Id := OP_PLUS;
      {$IFDEF CPP_SYN}
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end
      else if LA(1) = '+' then
      begin
        GetNextChar;
        ScanSpecial;
      end;
      {$ENDIF}
    end
    else if c = '-' then
    begin
      ScanSpecial;
      Token.Id := OP_MINUS;
      {$IFDEF CPP_SYN}
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end
      else if LA(1) = '-' then
      begin
        GetNextChar;
        ScanSpecial;
      end;
      {$ENDIF}
    end
    else if c = '*' then
    begin
      ScanSpecial;
      Token.Id := OP_MULT;
      {$IFDEF CPP_SYN}
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end;
      {$ENDIF}
    end
    else if c = '/' then
    begin
      {$IFDEF CPP_SYN}
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end
      else
      {$ENDIF}
      if LA(1) = '/' then
      begin
        ScanSingleLineComment();
				continue;
      end
      else
      begin
        ScanSpecial;
        Token.Id := OP_DIV;
      end;
    end
    else if ByteInSet(c, [Ord('~'), Ord('%'), Ord('^'), Ord('&'), Ord('|')]) then
    begin
      {$IFDEF CPP_SYN}
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end
      else
      if c = '^' then
      begin
        ScanSpecial;
        Exit;
      end
      else
        RaiseError(errSyntaxError, []);
      {$ELSE}
      if c = '^' then
        ScanSpecial
      else if c = '&' then
      begin
        GetNextChar;
        continue;
      end
      else
        RaiseError(errSyntaxError, []);
      {$ENDIF}
    end
    else if c = '=' then
    begin
      ScanSpecial;
      Token.Id := OP_EQ;
      if LA(1) = '>' then
      begin
        GetNextChar;
        ScanSpecial;
      end
    end
    else if c = '<' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_LE;
      end
      else if LA(1) = '>' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_NE;
      end
      else
        Token.Id := OP_LT;
    end
    else if c = '>' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_GE;
      end
      else
        Token.Id := OP_GT;
    end
    else if c = ':' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
      begin
        GetNextChar;
        ScanSpecial;
        Token.Id := OP_ASSIGN;
      end;
    end
    else if c = ',' then
      ScanSpecial
    else if c = '.' then
    begin
      if LA(1) = '.' then
        GetNextChar;
      ScanSpecial;
    end
    else if c = '#' then
      ScanCharLiteral
    else if c = '(' then
    begin
      if LA(1) = '*' then
      begin
        BeginComment(2);
        repeat
          GetNextChar;
          c := LA(0);
          if ByteInSet(c, [10,13]) then
          begin
            Inc(LineCount);
            GenSeparator;
            if c = #13 then
              GetNextChar;
          end;
        until ByteInSet(LA(1), [Ord('*'), Ord(CHAR_EOF)]) and
              ByteInSet(LA(2), [Ord(')'), Ord(CHAR_EOF)]);
        GetNextChar;
        GetNextChar;

        EndComment(2);
      end
      else
        ScanSpecial;
    end
    else if c = ')' then
      ScanSpecial
    else if c = '[' then
      ScanSpecial
    else if c = ']' then
      ScanSpecial
    else if c = '^' then
      ScanSpecial
    else if c = '@' then
      ScanSpecial
    else if c = ':' then
      ScanSpecial
    else if c = ';' then
      ScanSpecial
    else if c = '{' then
    begin
      if LA(1) = '$' then
      begin

        if (not LookForward) then
        begin
          ScanCondDir('{', [Ord('$')]);
          Token.TokenClass := tcNone;
          continue;
        end;

        GetNextChar;
        if ByteInSet(LA(1), [Ord('b'), Ord('B')]) then
        begin
          GetNextChar;
          if LA(1) = '+' then
          begin
            GetNextChar;
            SetCompleteBooleanEval(true);
          end
          else if LA(1) = '-' then
          begin
            GetNextChar;
            SetCompleteBooleanEval(false);
          end;
        end
        else if ByteInSet(LA(1), [Ord('a'), Ord('A')]) then
        begin
          GetNextChar;
          if LA(1) = '1' then
          begin
            GetNextChar;
            SetAlignment(1);
          end
          else if LA(1) = '2' then
          begin
            GetNextChar;
            SetAlignment(2);
          end
          else if LA(1) = '4' then
          begin
            GetNextChar;
            SetAlignment(4);
          end
          else if LA(1) = '8' then
          begin
            GetNextChar;
            SetAlignment(8);
          end
          else if LA(1) = '-' then
          begin
            GetNextChar;
            SetAlignment(1);
          end
          else
            RaiseError(errInvalidCompilerDirective, []);
        end;

      end;

      if LA(1) = '}' then
      begin
        GetNextChar;
        continue;
      end;

      BeginComment(1);
      repeat
        GetNextChar;

        if IsEOF then
          break;

        c := LA(0);
        if ByteInSet(c, [10,13]) then
        begin
          Inc(LineCount);
          GenSeparator;
          if c = #13 then
            GetNextChar;
        end;
      until LA(1) = '}';
      GetNextChar;
      EndComment(1);
    end
    else if c = #254 then
    begin
      raise PaxCancelException.Create(TKernel(kernel).CancelChar);
    end
    else
    begin
      if SCAN_EXPR and (c = '}') then
      begin
        ScanSpecial;
        Exit;
      end;

      RaiseError(errSyntaxError, []);
    end;
  until Token.TokenClass <> tcNone;
end;

function TPascalScanner.Scan_Expression: Variant;
var
  Op: Integer;
begin
  result := Scan_SimpleExpression;
  while IsCurrText('>') or
        IsCurrText('>=') or
        IsCurrText('<') or
        IsCurrText('<=') or
        IsCurrText('=') or
        IsCurrText('<>') do
  begin
    Op := 0;
    if IsCurrText('>') then
      Op := OP_GT
    else if IsCurrText('>=') then
      Op := OP_GE
    else if IsCurrText('<') then
      Op := OP_LT
    else if IsCurrText('<=') then
      Op := OP_LE
    else if IsCurrText('=') then
      Op := OP_EQ
    else if IsCurrText('<>') then
      Op := OP_NE;
    ReadToken;
    if Op = OP_GT then
      result := result > Scan_SimpleExpression
    else if Op = OP_GE then
      result := result >= Scan_SimpleExpression
    else if Op = OP_LT then
      result := result < Scan_SimpleExpression
    else if Op = OP_LE then
      result := result <= Scan_SimpleExpression
    else if Op = OP_EQ then
      result := result = Scan_SimpleExpression
    else if Op = OP_NE then
      result := result <> Scan_SimpleExpression;
  end;
end;

function TPascalScanner.Scan_SimpleExpression: Variant;
var
  Op: Integer;
begin
  result := Scan_Term;
  while IsCurrText('+') or
        IsCurrText('-') or
        IsCurrText('or') or
        IsCurrText('xor') do
  begin
    Op := 0;
    if IsCurrText('+') then
      Op := OP_PLUS
    else if IsCurrText('-') then
      Op := OP_MINUS
    else if IsCurrText('or') then
      Op := OP_OR
    else if IsCurrText('xor') then
      Op := OP_XOR;
    ReadToken;
    if Op = OP_PLUS then
      result := result + Scan_Term
    else if Op = OP_MINUS then
      result := result - Scan_Term
    else if Op = OP_OR then
      result := result or Scan_Term
    else if Op = OP_XOR then
      result := result xor Scan_Term;
  end;
end;

function TPascalScanner.Scan_Term: Variant;
var
  Op: Integer;
begin
  result := Scan_Factor;
  while IsCurrText('*') or
        IsCurrText('/') or
        IsCurrText('div') or
        IsCurrText('mod') or
        IsCurrText('shl') or
        IsCurrText('shr') or
        IsCurrText('and') do
  begin
    Op := 0;
    if IsCurrText('*') then
      Op := OP_MULT
    else if IsCurrText('/') then
      Op := OP_DIV
    else if IsCurrText('div') then
      Op := OP_IDIV
    else if IsCurrText('mod') then
      Op := OP_MOD
    else if IsCurrText('shl') then
      Op := OP_SHL
    else if IsCurrText('shr') then
      Op := OP_SHR
    else if IsCurrText('and') then
      Op := OP_AND;
    ReadToken;
    if Op = OP_MULT then
      result := result * Scan_Factor
    else if Op = OP_DIV then
      result := result / Scan_Factor
    else if Op = OP_IDIV then
      result := result div Scan_Factor
    else if Op = OP_MOD then
      result := result mod Scan_Factor
    else if Op = OP_SHL then
      result := result shl Scan_Factor
    else if Op = OP_SHR then
      result := result shr Scan_Factor
    else if Op = OP_AND then
      result := result and Scan_Factor;
  end;
end;

function TPascalScanner.Scan_Ident: Variant;
var
  I, Id: Integer;
  DefList: TDefList;
  S: String;
begin
  DefList := TKernel(kernel).DefList;
  S := Token.Text;
  I := DefList.IndexOf(S);
  if I >= 0 then
  begin
    result := DefList[I].value;
  end
  else
  begin
    with TKernel(kernel).CurrParser do
    begin
      Id := LookUps(S, levelStack);
      if Id = 0 then
        Id := LookupInUsingList(S);
      if Id = 0 then
        RaiseError(errConstantExpressionExpected, []);
      if GetSymbolRec(Id).Kind <> KindCONST then
        RaiseError(errConstantExpressionExpected, []);
      result := GetSymbolRec(Id).Value;
    end;
  end;
  ReadToken;
end;


end.
