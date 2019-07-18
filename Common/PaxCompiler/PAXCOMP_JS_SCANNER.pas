////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_JS_SCANNER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_JS_SCANNER;
interface
uses {$I uses.def}
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_SCANNER;

type
  TJavaScriptScanner = class(TBaseScanner)
  private
    procedure ScanCustDir;
    procedure ScanOctalLiteral;
    procedure ScanHexLiteral;
    function GetNextSpecialChar: Char;
  public
    procedure ScanStringLiteral(ch: Char); override;
    procedure ReadCustomToken; override;
  end;

implementation

uses
  PAXCOMP_KERNEL;

procedure TJavaScriptScanner.ReadCustomToken;
var
  c: Char;
begin
  repeat
    GetNextChar;
    c := LA(0);
    Token.Position := Position;
    if IsWhiteSpace(c) then
      continue
    else if c = #13 then
      ScanSeparator
{$IFDEF MACOS}
    else if c = #10 then
      ScanSeparator
{$ENDIF}
{$IFDEF LINUX}
    else if c = #10 then
      ScanSeparator
{$ENDIF}
{$IFDEF ANDROID}
    else if c = #10 then
      ScanSeparator
{$ENDIF}
    else if IsEOF(c) then
      ScanSpecial
    else if IsEOF(c) then
      ScanSpecial
    else if IsEOF then
      ScanEOF

    else if IsAlpha(c) or (c = '$') then
    begin
      while IsAlpha(LA(1)) or IsDigit(LA(1)) or (LA(1) = '$') do
        GetNextChar;
      Token.TokenClass := tcIdentifier;
      SetScannerState(scanProg);
      token.Length := Position - token.Position + 1;
      if StrEql(Token.Text, 'in') then
        ScanSpecial;
    end
    else if c = CHAR_DOUBLE_AP then
    begin
      ScanStringLiteral(CHAR_DOUBLE_AP);
      Token.TokenClass := tcPCharConst;
    end
    else if c = CHAR_AP then
    begin
      ScanStringLiteral(CHAR_AP);
      Token.TokenClass := tcPCharConst;
    end
    else if IsDigit(c) then
    begin
      if (c = '0') and (LA(1) = 'x') then
      begin
        GetNextChar;
        ScanHexLiteral;
      end
      else
        ScanNumberLiteral;
    end

    else if c = '{' then
      ScanSpecial
    else if c = '}' then
      ScanSpecial
    else if c = '(' then
      ScanSpecial
    else if c = ')' then
      ScanSpecial
    else if c = '[' then
      ScanSpecial
    else if c = ']' then
      ScanSpecial
    else if c = '.' then
      ScanSpecial
    else if c = ';' then
      ScanSpecial
    else if c = ',' then
      ScanSpecial
    else if c = '<' then
    begin
      ScanSpecial;
      if LA(1) = '<' then
      begin
        GetNextChar;
        if LA(1) = '=' then
          GetNextChar;
      end
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '>' then
    begin
      ScanSpecial;
      if LA(1) = '>' then
      begin
        GetNextChar;
        if LA(1) = '>' then
        begin
          GetNextChar;
          if LA(1) = '=' then
            GetNextChar;
        end
        else if LA(1) = '=' then
          GetNextChar;
      end
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '=' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
      begin
        GetNextChar;
        if LA(1) = '=' then
          GetNextChar;
      end;
    end
    else if c = '!' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
      begin
        GetNextChar;
        if LA(1) = '=' then
          GetNextChar;
      end
    end
    else if c = '+' then
    begin
      ScanSpecial;
      if LA(1) = '+' then
        GetNextChar
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '-' then
    begin
      ScanSpecial;
      if LA(1) = '-' then
        GetNextChar
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '*' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '%' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '|' then
    begin
      ScanSpecial;
      if LA(1) = '|' then
        GetNextChar
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '^' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '~' then
    begin
      ScanSpecial;
      if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '&' then
    begin
      ScanSpecial;
      if LA(1) = '&' then
        GetNextChar
      else if LA(1) = '=' then
        GetNextChar;
    end
    else if c = '?' then
      ScanSpecial
    else if c = ':' then
      ScanSpecial
    else if c = '@' then
    begin
      if LA(1) = '@' then
      begin
        GetNextChar;
        ScanCustDir;
        Token.TokenClass := tcNone;
        continue;
      end
      else
        ScanSpecial;
    end
    else if c = '/' then
    begin
      if LA(1) = '/' then
      begin
        ScanSingleLineComment();
				continue;
      end
      else if LA(1) = '*' then
      begin
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

          if LA(1) = '*' then
          begin
            if Position + 1 >= BuffLength then
              break
            else if LA(2) = '/' then
            begin
              GetNextChar;
              GetNextChar;
              break;
            end
          end;
        until false;
        EndComment(2);
      end
      else
        ScanSpecial;
    end
    else
      RaiseError(errSyntaxError, []);
  until Token.TokenClass <> tcNone;
end;

procedure TJavaScriptScanner.ScanCustDir;
label
  NextComment, Fin;
const
  IdsSet = paxcomp_constants.IdsSet + [Ord('\'), Ord('/'), Ord('"')];
  Start1 = '@';
var
  S: String;
  DirName: String;
  ok: Boolean;
begin
  DirName := '';

  S := '';
  repeat
   GetNextChar;
   if ByteInSet(LA(0), [10,13]) then
   begin
    Inc(LineCount);
    GenSeparator;

    if LA(0) = #13 then
      GetNextChar;
   end
   else
    S := S + LA(0);
  until not ByteInSet(LA(0), IdsSet);


  ScanChars(IdsSet + [Ord('.'), Ord('-'), Ord('['), Ord(']'), Ord('('), Ord(')'),
     Ord(','), Ord('\'), Ord('/'), Ord('"'), Ord(' '), Ord(':')]);

  DirName := s + Token.Text;

  with TKernel(kernel) do
   if Assigned(OnUnknownDirective) then
   begin
    ok := true;
    OnUnknownDirective(Owner, DirName, ok);
    if not ok then
      Self.RaiseError(errInvalidCompilerDirective, [DirName]);
   end;

  ScanChars(WhiteSpaces);
  GenSeparator;
end;


procedure TJavaScriptScanner.ScanOctalLiteral;
var
  c: Char;
  V: array[1..30] of Integer;
  L, P: Integer;
begin
  L := 0;
  while IsDigit(LA(1)) do
  begin
    c := GetNextChar;
    if ByteInSet(c, [8,9]) then
      RaiseError(errSyntaxError, []);
    Inc(L);
    V[L] := ord(c) - ord('0');
  end;
  CustomInt64Val := 0;
  if L > 0 then
  begin
    CustomInt64Val := V[L];
    P := 1;
    while L > 1 do
    begin
      Dec(L);
      P := P * 8;
      CustomInt64Val := CustomInt64Val + P * V[L];
    end;
  end;
  Token.TokenClass := tcIntegerConst;
  Token.Tag := 2;
  SetScannerState(scanProg);
end;

procedure TJavaScriptScanner.ScanHexLiteral;
var
  c: Char;
  V: array[1..30] of Integer;
  L, P: Integer;
begin
  GetNextChar;
  L := 0;
  while IsHexDigit(LA(1)) do
  begin
    c := GetNextChar;
    Inc(L);
    if IsDigit(c) then
      V[L] := ord(c) - ord('0')
    else
      V[L] := ord(c) - ord('A') + 10
  end;
  CustomInt64Val := 0;
  if L > 0 then
  begin
    CustomInt64Val := V[L];
    P := 1;
    while L > 1 do
    begin
      Dec(L);
      P := P * 16;
      CustomInt64Val := CustomInt64Val + P * V[L];
    end;
  end;
  Token.TokenClass := tcIntegerConst;
  Token.Tag := 2;
  SetScannerState(scanProg);
end;

function TJavaScriptScanner.GetNextSpecialChar: Char;
var
  c: Char;
begin
  c := LA(1);
  if ByteInSet(c, [Ord('u'),Ord('U')]) then
    ScanHexLiteral
  else if ByteInSet(c, [Ord('0')..Ord('7')]) then
    ScanOctalLiteral
  else if ByteInSet(c, [Ord(CHAR_AP), Ord(CHAR_DOUBLE_AP), Ord('\')]) then
  begin
    CustomInt64Val := Ord(c);
    GetNextChar;
  end
  else if c = 'r' then
  begin
    CustomInt64Val := 13;
    GetNextChar;
  end
  else if c = 'n' then
  begin
    CustomInt64Val := 10;
    GetNextChar;
  end
  else if c = 't' then
  begin
    CustomInt64Val := 9;
    GetNextChar;
  end
  else if c = 'f' then
  begin
    CustomInt64Val := 12;
    GetNextChar;
  end
  else if c = 'b' then
  begin
    CustomInt64Val := 8;
    GetNextChar;
  end;
  result := Char(CustomInt64Val);
end;


procedure TJavaScriptScanner.ScanStringLiteral(ch: Char);
var
  c: Char;
begin
  CustomStringVal := '';
  c := #0;

  repeat
    if LA(1) = '\' then
    begin
      GetNextChar;
      CustomStringVal := CustomStringVal + GetNextSpecialChar;
      continue;
    end
    else
      c := GetNextChar;
    if IsEOF then
    begin
      RaiseError(errUnterminatedString, []);
      Exit;
    end;
    if c = ch then
    begin
      break;
    end
    else
      CustomStringVal := CustomStringVal + c
  until false;

  Token.TokenClass := tcPCharConst;
  Token.Tag := 2;
  SetScannerState(scanProg);
end;


end.
