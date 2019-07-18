////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_SCANNER.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_SCANNER;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

const
  _IFDEF = 1;
  _IFNDEF = 2;
  _ELSE = 3;
  _ENDIF = 4;
  _ELSEIF = 5;
type
  TScannerState = (scanText, scanProg);

  TBaseScanner = class;

  TDefRec = class
  public
    Word: Integer;
    What: String;
    Vis: boolean;
    value: variant;
  end;

  TDefList = class(TTypedList)
  private
    function GetRecord(I: Integer): TDefRec;
  public
    procedure Add(const S: String); overload;
    procedure Add(const S: String; const value: Variant); overload;
    function IndexOf(const S: String): Integer;
    property Records[I: Integer]: TDefRec read GetRecord; default;
  end;

  TDefStack = class(TDefList)
  private
    function GetTop: TDefRec;
    function GetOuterVis: Boolean;
  public
    procedure Push(Word: Integer; What: String; Vis: Boolean);
    procedure Pop;
    property OuterVis: Boolean read GetOuterVis;
    property Top: TDefRec read GetTop;
  end;

  TToken = class
  private
    scanner: TBaseScanner;
    function GetText: String;
  public
    TokenClass: TTokenClass;
    Position: Integer;
    Length: Integer;
    Id: Integer;
    Tag: Integer;
    constructor Create(i_scanner: TBaseScanner);
    procedure Push(StateStack: TIntegerStack);
    procedure Pop(StateStack: TIntegerStack);
    property Text: String read GetText;
  end;

  TScannerRec = class
  private
    LineCount: Integer;
    P: Integer;
    Buff: String;
  public
    IncludedFileName: String;
  end;

  TScannerStack = class(TTypedList)
  private
    function GetTop: TScannerRec;
  public
    procedure Push(Scanner: TBaseScanner; const IncludedFileName: String);
    procedure Pop(Scanner: TBaseScanner);
    property Top: TScannerRec read GetTop;
  end;

  TCurrComment = class
  private
    P1: Integer;
  public
    Comment: String;
    CommentValue: Integer;
    CurrCommN: Integer;
    CommentedTokens: TAssocStringInt;
    AllowedDoComment: Boolean;
    constructor Create;
    procedure Clear;
    function Valid: Boolean;
    destructor Destroy; override;
  end;

  TBaseScanner = class
  private
    StateStack: TIntegerStack;
    p: Integer;
    fScannerState: TScannerState;
    fBackSlash: Boolean;
    function GetLinePos: Integer;
    function GetBuffLength: Integer;
    function _GetCurrText: String;
    function GetExAlphaList: TAssocIntegers;
  public
    DefStack: TDefStack;
    kernel: Pointer;
    Token: TToken;
    Buff: String;
    LineCount: Integer;
    LookForward: Boolean;
    ScannerStack: TScannerStack;
    CancelPos: Integer;
    VarNameList: TStringList;

    CurrComment: TCurrComment;

    CustomInt64Val: Int64;
{$IFDEF PAXARM}
    CustomStringVal: String;
{$ELSE}
    CustomStringVal: WideString;
{$ENDIF}
    SCAN_EXPR: Boolean;
    MacroList: TMacroList;

    constructor Create;
    destructor Destroy; override;
    procedure Init(i_kernel: Pointer;
                   const SourceCode: string;
                   i_CancelPos: Integer);
    procedure GenSeparator;
    procedure GenWarnings(OnOff: Boolean);
    procedure GenFramework(OnOff: Boolean);
    procedure GenBeginText;
    procedure GenEndText;

    function LA(N: Integer): Char;
    function GetNextChar: Char;
    function IsNewLine: Boolean;
    function IsEOF: Boolean; overload;
    class function IsEOF(c: Char): Boolean; overload;
    function IsAlpha(c: Char): Boolean;
    function IsAlphaEx(c: Char): Boolean;
    class function IsDigit(c: Char): Boolean;
    class function IsHexDigit(c: Char): Boolean;
    class function IsWhiteSpace(c: Char): Boolean;
    procedure ScanEOF;
    procedure ScanSpecial;
    procedure ScanSeparator;
    procedure ScanIdentifier;
    procedure ScanNumberLiteral;
    procedure ScanHexLiteral;
    procedure ScanStringLiteral(ch: Char); virtual;
    procedure ScanDigits;
    procedure ScanHexDigits;
    procedure ScanSingleLineComment;
    procedure ReadToken;
    procedure ReadCustomToken; virtual; abstract;
    function UpdateToken: String; virtual;
    procedure RaiseError(const Message: string; params: array of Const);
    procedure CreateError(const Message: string; params: array of Const);
    procedure Push;
    procedure Pop;
    procedure SetCompleteBooleanEval(value: Boolean);
    procedure SetOverflowCheck(value: Boolean);
    procedure SetAlignment(value: Integer);
    procedure ScanCondDir(Start1: Char;
                          Start2: TByteSet);
    function ScanRegExpLiteral: String;
    procedure ScanChars(CSet: TByteSet);
    procedure ScanHtmlString(const Ch: String);
    function ScanFormatString: String;
    procedure IncLineCount;
    procedure InsertText(const S: String);
    class function IsValidToken(const S: String): Boolean; virtual;

    procedure BeginComment(value: Integer);
    procedure EndComment(value: Integer);
    procedure DoComment;
    procedure AttachId(Id: Integer; Upcase: Boolean);
    class function IsConstToken(AToken: TToken): Boolean;
    procedure SetScannerState(Value: TScannerState);

    function IsCurrText(const S: String): Boolean; virtual;
    procedure Match(const S: String); virtual;
    function Scan_Factor: Variant; virtual;
    function Scan_Expression: Variant; virtual;
    function Scan_Ident: Variant; virtual;

    function ParametrizedTypeExpected: Boolean; virtual;
    function FindPosition(Chars: TByteSet): Integer;
    function Precede(ch1, ch2: Char): Boolean;

    function AreNextChars(const S: String): Boolean;

    property Position: Integer read p write p;
    property LinePos: Integer read GetLinePos;
    property BuffLength: Integer read GetBuffLength;
    property LookAhead[N: Integer]: Char read LA; default;
    property ScannerState: TScannerState read fScannerState write SetScannerState;
    property _CurrText: String read _GetCurrText;
    property ExAlphaList: TAssocIntegers read GetExAlphaList;
  end;

function ConvertString(const S: String): String;

implementation

uses
  PAXCOMP_KERNEL,
  PAXCOMP_MODULE,
  PAXCOMP_BYTECODE;

// TCurrComment ----------------------------------------------------------------

constructor TCurrComment.Create;
begin
  inherited;
  CommentedTokens := TAssocStringInt.Create;
  AllowedDoComment := true;
end;

procedure TCurrComment.Clear;
begin
  Comment := '';
  CommentedTokens.Clear;
  CurrCommN := 0;
  AllowedDoComment := true;
end;

function TCurrComment.Valid: Boolean;
begin
  result := Comment <> '';
end;

destructor TCurrComment.Destroy;
begin
  FreeAndNil(CommentedTokens);
  inherited;
end;

// TDefList --------------------------------------------------------------------

function TDefList.GetRecord(I: Integer): TDefRec;
begin
  result := TDefRec(L[I]);
end;

procedure TDefList.Add(const S: String);
var
  R: TDefRec;
  I: Integer;
  V: Variant;
  Q: String;
begin
  R := TDefRec.Create;
  L.Add(R);
  R.What := S;

  I := PosCh('=', S);
  if I > 0 then
  begin
    R.What := SCopy(S, SLow(S), I - SLow(S));
    R.What := RemoveChars([ord(' ')], R.What);
    Q := SCopy(S, I + 1, 100);
    Q := RemoveChars([ord(' ')], Q);
    if PosCh('.', Q ) > 0 then
      V := StrToFloat(Q)
    else
      V := StrToInt(Q);
    R.Value := V;
  end;
end;

procedure TDefList.Add(const S: String; const value: Variant);
var
  R: TDefRec;
  I: Integer;
begin
  I := IndexOf(S);
  if I >= 0 then
  begin
    R := Records[I];
    R.value := value;
    Exit;
  end;

  R := TDefRec.Create;
  L.Add(R);
  R.What := S;
  R.value := value;
end;

function TDefList.IndexOf(const S: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].What, S) then
    begin
      result := I;
      Exit;
    end;
end;

// TDefStack ----------------------------------------------------------------

function TDefStack.GetTop: TDefRec;
begin
  result := TDefRec(L[Count - 1]);
end;

procedure TDefStack.Push(Word: Integer; What: String; Vis: Boolean);
var
  R: TDefRec;
begin
  R := TDefRec.Create;
  R.Word := Word;
  R.What := What;
  R.Vis := Vis;
  L.Add(R);
end;

procedure TDefStack.Pop;
var
  R: TDefRec;
begin
  R := TDefRec(L[Count - 1]);
  L.Delete(Count - 1);
  FreeAndNil(R);
end;

function TDefStack.GetOuterVis: Boolean;
var
  I: Integer;
begin
  result := true;
  for I:= Count - 1 downto 0 do
    if Records[I].Word in [_IFDEF, _IFNDEF] then
    begin
      if I > 0 then
        result := Records[I-1].Vis;
      Exit;
    end;
end;

//------- TToken ---------------------------------------------------------------

constructor TToken.Create(i_scanner: TBaseScanner);
begin
  Self.scanner := i_scanner;
  Length := 0;
  Id := 0;
  Tag := 0;
end;

function TToken.GetText: String;
begin
  result := SCopy(scanner.Buff, Position, Length);
end;

procedure TToken.Push(StateStack: TIntegerStack);
begin
  StateStack.Push(Integer(TokenClass));
  StateStack.Push(Position);
  StateStack.Push(Length);
  StateStack.Push(Id);
  StateStack.Push(Tag);
end;

procedure TToken.Pop(StateStack: TIntegerStack);
begin
  Tag := StateStack.Pop;
  Id := StateStack.Pop;
  Length := StateStack.Pop;
  Position := StateStack.Pop;
  TokenClass := TTokenClass(StateStack.Pop);
end;

//------- TScannerStack ---------------------------------------------------------

procedure TScannerStack.Push(Scanner: TBaseScanner; const IncludedFileName: String);
var
  R: TScannerRec;
  kernel: TKernel;
  M: TModule;
begin
  R := TScannerRec.Create;
  R.LineCount := Scanner.LineCount;
  R.P := Scanner.P;
  R.Buff := Scanner.Buff;
  R.IncludedFileName := IncludedFileName;
  L.Add(R);

  if Assigned(Scanner.kernel) then
  begin
    kernel := TKernel(Scanner.kernel);
    kernel := kernel.RootKernel;
    M := kernel.CurrParser.CurrModule;
    M.IncludedFiles.Add(IncludedFileName);
    kernel.CurrParser.Gen(OP_BEGIN_INCLUDED_FILE,
      M.IncludedFiles.Count - 1, 0, 0);
  end;
end;

procedure TScannerStack.Pop(Scanner: TBaseScanner);
var
  R: TScannerRec;
  kernel: TKernel;
  M: TModule;
begin
  R := TScannerRec(L[Count - 1]);
  L.Delete(Count - 1);

  if Scanner <> nil then
  begin
    Scanner.LineCount := R.LineCount;
    Scanner.P := R.P;
    Scanner.Buff := R.Buff;

    if Assigned(Scanner.kernel) then
    begin
      kernel := TKernel(Scanner.kernel);
      kernel := kernel.RootKernel;
      M := kernel.CurrParser.CurrModule;
      kernel.CurrParser.Gen(OP_END_INCLUDED_FILE,
        M.IncludedFiles.Count - 1, 0, 0);
    end;
  end;

  FreeAndNil(R);
end;

function TScannerStack.GetTop: TScannerRec;
begin
  if Count = 0 then
    result := nil
  else
    result := TScannerRec(L[Count - 1]);
end;

//------- TBaseScanner ---------------------------------------------------------

constructor TBaseScanner.Create;
begin
  inherited;

  p := 0;
  Buff := '';
  token := TToken.Create(Self);

  StateStack := TIntegerStack.Create;
  DefStack := TDefStack.Create;
  LookForward := false;

  ScannerStack := TScannerStack.Create;
  CancelPos := -1;

  VarNameList := TStringList.Create;
  CurrComment := TCurrComment.Create;
  MacroList := TMacroList.Create;
end;

destructor TBaseScanner.Destroy;
begin
  FreeAndNil(token);
  FreeAndNil(StateStack);
  FreeAndNil(DefStack);
  FreeAndNil(ScannerStack);
  FreeAndNil(VarNameList);
  FreeAndNil(CurrComment);
  FreeAndNil(MacroList);
  inherited;
end;

procedure TBaseScanner.Init(i_kernel: Pointer;
                            const SourceCode: string;
                            i_CancelPos: Integer);
begin
  Self.kernel := i_kernel;
  Buff := SourceCode + CHAR_EOF;
  p := SLow(Buff) - 1;
  LineCount := 0;
  StateStack.Clear;
  LookForward := false;
  ScannerStack.Clear;
  CancelPos := i_CancelPos;
  SetScannerState(scanText);
  with DefStack do
  while Count > 0 do
    Pop;
  VarNameList.Clear;

  CurrComment.Clear;
  MacroList.Clear;
end;

procedure TBaseScanner.GenSeparator;
begin
  if Assigned(kernel) then
  with TKernel(kernel) do
    CurrParser.Gen(OP_SEPARATOR, CurrParser.CurrModule.ModuleNumber, LineCount, 0);
end;

procedure TBaseScanner.GenWarnings(OnOff: Boolean);
begin
  if Assigned(kernel) then
  with TKernel(kernel) do
    if OnOff = true then
      CurrParser.Gen(OP_WARNINGS_ON, 0, 0, 0)
    else
      CurrParser.Gen(OP_WARNINGS_OFF, 0, 0, 0);
end;

procedure TBaseScanner.GenFramework(OnOff: Boolean);
begin
  if Assigned(kernel) then
  with TKernel(kernel) do
    if OnOff = true then
      CurrParser.Gen(OP_FRAMEWORK_ON, 0, 0, 0)
    else
      CurrParser.Gen(OP_FRAMEWORK_OFF, 0, 0, 0);
end;

procedure TBaseScanner.GenBeginText;
begin
  if Assigned(kernel) then
  with TKernel(kernel) do
    CurrParser.Gen(OP_BEGIN_TEXT, CurrParser.CurrModule.ModuleNumber, LineCount, 0);
end;

procedure TBaseScanner.GenEndText;
begin
  if Assigned(kernel) then
  with TKernel(kernel) do
    CurrParser.Gen(OP_END_TEXT, CurrParser.CurrModule.ModuleNumber, LineCount, 0);
end;

function TBaseScanner.LA(N: Integer): Char;
begin
  result := Buff[p + N];
end;

function TBaseScanner.GetNextChar: Char;
var
  I: Integer;
begin
  Inc(p);
  result := Buff[P];

  if P = CancelPos then
  begin
    TKernel(kernel).CompletionHasParsed := true;

    TKernel(kernel).CurrParser.CurrModule.S3 := TKernel(kernel).SymbolTable.Card;
    TKernel(kernel).CurrParser.CurrModule.P3 := TKernel(kernel).Code.Card;


    if TKernel(kernel).FindDeclId < 0 then
    begin
      TKernel(kernel).CurrParser.FIND_DECL_SWITCH := true;
      Exit;
    end;

    case result of
      '.':
      begin
        Buff := Copy(Buff, 1, P) + DummyName +
                   result + #254;
      end;
      '(': Buff := Copy(Buff, 1, P) + DummyName + ')' +
                   result + #254;
      ' ', #13, #10: Buff := Copy(Buff, 1, P) + DummyName +
                   result + #254;
      ',': Buff := Copy(Buff, 1, P) + DummyName + ')' +
                   result + #254;
    else
      RaiseError(errInternalError, []);
    end;

    if P <= 1 then
    begin
      TKernel(kernel).CancelChar := #255;
      Exit;
    end;
    I := P;
    while ByteInSet(Buff[I], [32, 13, 10]) do
    begin
      Dec(I);
      if I = 1 then
        Exit;
    end;
    TKernel(kernel).CancelChar := Buff[I];
  end;
end;

class function TBaseScanner.IsEOF(c: Char): Boolean;
begin
  result := (c = CHAR_EOF);
end;

function TBaseScanner.IsEOF: Boolean;
begin
  result := P >= SHigh(Buff);
end;

function TBaseScanner.IsNewLine: Boolean;
begin
  if p = 0 then
    result := false
  else
    result := ByteInSet(LA(0), [13, 10]);
end;

function TBaseScanner.IsAlpha(c: Char): Boolean;
begin
  result := PAXCOMP_SYS.IsAlpha(c);
  if not result then
    if ExAlphaList <> nil then
      result := ExAlphaList.Inside(ord(c));
end;

function TBaseScanner.IsAlphaEx(c: Char): Boolean;
begin
  if ByteInSet(c, [Ord('e'),Ord('E')]) then
    result := false
  else
    result := IsAlpha(c);
end;

class function TBaseScanner.IsDigit(c: Char): Boolean;
begin
  result := PAXCOMP_SYS.IsDigit(c);
end;

class function TBaseScanner.IsHexDigit(c: Char): Boolean;
begin
  result := ByteInSet(c, [Ord('0')..Ord('9'),Ord('A')..Ord('F'),Ord('a')..Ord('f')]);
end;

class function TBaseScanner.IsWhiteSpace(c: Char): Boolean;
begin
  result := ByteInSet(c, WhiteSpaces);
end;

procedure TBaseScanner.ScanIdentifier;
begin
  while IsAlpha(LA(1)) or IsDigit(LA(1)) do
    GetNextChar;
  Token.TokenClass := tcIdentifier;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanDigits;
begin
  while IsDigit(LA(1)) do
    GetNextChar;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanHexDigits;
begin
  while IsHexDigit(LA(1)) do
    GetNextChar;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanNumberLiteral;
begin
  ScanDigits;
  Token.TokenClass := tcIntegerConst;

  if (LA(1) = '.') and (LA(2) <> '.') and (not IsAlphaEx(LA(2))) then
  begin
    GetNextChar();

    if IsDigit(LA(1)) then
      ScanDigits;

    if ByteInSet(LA(1), [Ord('e'), Ord('E')]) then
    begin
      GetNextChar();
      if ByteInSet(LA(1), [Ord('+'), Ord('-')]) then
        GetNextChar();
      ScanDigits;
    end;

    Token.TokenClass := tcDoubleConst;
  end
  else if ByteInSet(LA(1), [Ord('e'),Ord('E')]) and ByteInSet(LA(2), [Ord('0')..Ord('9'),Ord('+'),Ord('-')]) then
  begin
    GetNextChar();
    if ByteInSet(LA(1), [Ord('+'), Ord('-')]) then
      GetNextChar();
    ScanDigits;
    Token.TokenClass := tcDoubleConst;
  end;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanHexLiteral;
begin
  GetNextChar;
  ScanHexDigits;
  Token.TokenClass := tcIntegerConst;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanStringLiteral(ch: Char);
var
  K: Integer;
begin
  K := 0;
  GetNextChar;
  if (LA(0) = ch) and (LA(1) <> ch) then // empty string
  begin
    Token.TokenClass := tcPCharConst;
    Exit;
  end;

  repeat
     if IsEOF then
     begin
       RaiseError(errUnterminatedString, []);
       Exit;
     end;

     if (LA(0) = ch) and (LA(1) = ch) then
     begin
       GetNextChar;
       buff[p] := CHAR_REMOVE;
     end
     else if (LA(0) = ch) then
        break;

     GetNextChar;
     Inc(K);
  until false;

  if K = 1 then
    Token.TokenClass := tcCharConst
  else
    Token.TokenClass := tcPCharConst;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanSpecial;
begin
  Token.TokenClass := tcSpecial;
  Token.Id := 0;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanSeparator;
begin
  if LA(1) = #10 then
    GetNextChar;
  Token.TokenClass := tcSeparator;
  Inc(LineCount);
  Token.Id := LineCount;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanEOF;
begin
  SetScannerState(scanProg);
  if ScannerStack.Count > 0 then
  begin
    ScannerStack.Pop(Self);
    Exit;
  end;

  Token.TokenClass := tcSpecial;
  Token.Id := 0;
end;

procedure TBaseScanner.ReadToken;
begin
  token.TokenClass := tcNone;
  token.Position := p;
  ReadCustomToken;
  token.Length := p - token.Position + 1;

  with CurrComment do
  if Valid then
  if IsValidToken(Token.Text) then
  begin
    CommentedTokens.Add(Token.Text);
    if CurrComment.CommentedTokens.Count >= MAX_COMMENTED_TOKENS then
    if AllowedDoComment then
      DoComment;
  end;
end;

function TBaseScanner.UpdateToken: String;
begin
  result := Token.Text;
end;

procedure TBaseScanner.ScanSingleLineComment;
begin
  BeginComment(0);
  repeat
    if IsEOF then
      break;
    GetNextChar;

  until ByteInSet(LA(1), [13, 10]);
  SetScannerState(scanProg);
  EndComment(0);
end;

procedure TBaseScanner.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).Code.N := TKernel(kernel).Code.Card;
  TKernel(kernel).RaiseError(Message, params);
end;

procedure TBaseScanner.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

procedure TBaseScanner.Push;
begin
  StateStack.Push(p);
  StateStack.Push(LineCount);
  StateStack.Push(Integer(ScannerState));

  Token.Push(StateStack);
end;

procedure TBaseScanner.Pop;
begin
  Token.Pop(StateStack);

  ScannerState := TScannerState(StateStack.Pop);
  LineCount := StateStack.Pop;
  p := StateStack.Pop;
end;

procedure TBaseScanner.SetCompleteBooleanEval(value: Boolean);
begin
  TKernel(kernel).CurrParser.CompleteBooleanEval := value;
end;

procedure TBaseScanner.SetOverflowCheck(value: Boolean);
begin
  if value then
    TKernel(kernel).CurrParser.Gen(OP_OVERFLOW_CHECK, 1, 0, 0)
  else
    TKernel(kernel).CurrParser.Gen(OP_OVERFLOW_CHECK, 0, 0, 0);
end;

procedure TBaseScanner.SetAlignment(value: Integer);
begin
  TKernel(kernel).CurrParser.Alignment := value;
end;

procedure TBaseScanner.ScanCondDir(Start1: Char;
                                   Start2: TByteSet);

  procedure ScanChars(CSet: TByteSet);
  begin
    Token.Position := P;
    while ByteInSet(LA(1), CSet) do
      GetNextChar;

    token.Length := p - token.Position + 1;
    Token.TokenClass := tcIdentifier;
  end;


label
  NextComment, Fin;
var
  S: String;
  I, J, J1, J2: Integer;
  Visible: Boolean;
  FileName, DirName: String;
  ok: Boolean;
  value: variant;
  ch: Char;
begin
  GetNextChar; // skip $

  DirName := '';

//  writeln(CurrKernel.Code.CurrSourceLineNumber);
//  if CurrKernel.Code.CurrSourceLineNumber = 54 then
//    I := 1;

  if ByteInSet(LA(1), [Ord('b'),Ord('B')]) and ByteInSet(LA(2), [Ord('+'),Ord('-')]) then
  begin
    DirName := LA(1);

    GetNextChar;
    if LA(1) = '+' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetCompleteBooleanEval(true);
    end
    else if LA(1) = '-' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetCompleteBooleanEval(false);
    end;

    ScanChars(WhiteSpaces);
    if LA(1) = '}' then
      GetNextChar
    else
      Self.RaiseError(errInvalidCompilerDirective, [DirName]);

    Exit;
  end;

  if ByteInSet(LA(1),[Ord('q'),Ord('Q')]) and ByteInSet(LA(2), [Ord('+'),Ord('-')]) then
  begin
    DirName := LA(1);

    GetNextChar;
    if LA(1) = '+' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetOverflowCheck(true);
    end
    else if LA(1) = '-' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetOverflowCheck(false);
    end;

    ScanChars(WhiteSpaces);
    if LA(1) = '}' then
      GetNextChar
    else
      Self.RaiseError(errInvalidCompilerDirective, [DirName]);

    Exit;
  end;

  if ByteInSet(LA(1), [Ord('a'), Ord('A')]) and ByteInSet(LA(2),
                [Ord('1'), Ord('2'), Ord('4'), Ord('8'), Ord('-')]) then
  begin
    DirName := LA(1);

    GetNextChar;
    if LA(1) = '1' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetAlignment(1);
    end
    else if LA(1) = '2' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetAlignment(2);
    end
    else if LA(1) = '4' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetAlignment(4);
    end
    else if LA(1) = '8' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetAlignment(8);
    end
    else if LA(1) = '-' then
    begin
      DirName := DirName + LA(1);

      GetNextChar;
      SetAlignment(1);
    end;

    ScanChars(WhiteSpaces);
    if LA(1) = '}' then
      GetNextChar
    else
      Self.RaiseError(errInvalidCompilerDirective, [DirName]);

    Exit;
  end;

  Visible := true;

NextComment:

  S := '';
  if LA(1) = '$' then
    GetNextChar;
  repeat
    GetNextChar;
    S := S + LA(0);
    if ByteInSet(LA(0), [10,13]) then
    begin
      Inc(LineCount);

      if LA(0) = #13 then
        GetNextChar;
    end;
  until not ByteInSet(LA(0), (IdsSet + Start2));

  I := Pos('INCLUDE ', UpperCase(S) + ' ');
  if I = 0 then
    I := Pos('I ', UpperCase(S));

  DirName := RemoveLeftChars1(WhiteSpaces + [Ord('}')], S);

  if I = 1 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + [32]);

    FileName := Token.Text;
    if LA(1) = '.' then
    begin
      GetNextChar;

      ScanChars(IdsSet);

      FileName := FileName + Token.Text;
    end;
    if LA(1) = CHAR_AP then
      GetNextChar;

    if Assigned(TKernel(kernel).OnInclude) then
    begin
      S := '';
      TKernel(kernel).OnInclude(TKernel(kernel).Owner, FileName, S);
    end
    else
    begin
      if Pos('.', FileName) = 0 then
        if TKernel(kernel).CurrParser.GetIncludedFileExt <> '' then
          FileName := FileName + '.' + TKernel(kernel).CurrParser.GetIncludedFileExt;

      S := TKernel(kernel).FindFullPath(FileName);

      if not FileExists(S) then
        Self.RaiseError(errFileNotFound, [FileName])
      else
      begin
        FileName := S;
        S := LoadText(FileName);
      end;
    end;

    ScanChars(WhiteSpaces);
    if LA(1) = '}' then
      GetNextChar;

    if S = '' then
      Exit;

    ScannerStack.Push(Self, FileName);

    P := 0;
    Buff := S + CHAR_EOF;
    LineCount := 0;

    Exit;
  end;

  I := Pos('WARNINGS ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet);
    DirName := Token.Text;
    if StrEql(DirName, 'On') then
      GenWarnings(true)
    else
    if StrEql(DirName, 'Off') then
      GenWarnings(false)
    else
      Self.RaiseError(errInvalidCompilerDirective, ['WARNINGS ' + DirName]);

    if LA(1) = '}' then
      GetNextChar;

    Exit;
  end;

  I := Pos('FRAMEWORK ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet);
    DirName := Token.Text;
    if StrEql(DirName, 'On') then
      GenFramework(true)
    else
    if StrEql(DirName, 'Off') then
      GenFramework(false)
    else
      Self.RaiseError(errInvalidCompilerDirective, ['FRAMEWORK ' + DirName]);

    if LA(1) = '}' then
      GetNextChar;

    Exit;
  end;

  I := Pos('NODEFINE ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    I := TKernel(kernel).DefList.IndexOf(DirName);
    if I <> -1 then
      TKernel(kernel).DefList.RemoveAt(I);

    with TKernel(kernel) do
      if Assigned(OnUndefineDirective) then
      begin
        ok := true;
        OnUndefineDirective(Owner, DirName, ok);
        if not ok then
          Self.RaiseError(errInvalidCompilerDirective, [DirName]);
      end;

    while LA(0) <> '}' do
      GetNextChar;

    Exit;
  end;

  I := Pos('DEFINE ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do
      GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    if LA(1) = '=' then
    begin
      SCAN_EXPR := true;
      try
        ScanChars([Ord('='), Ord(' ')]);
        ReadToken;
        value := Scan_Expression;
      finally
        SCAN_EXPR := false;
      end;
    end;

    TKernel(kernel).DefList.Add(DirName, value);

    with TKernel(kernel) do
      if Assigned(OnDefineDirective) then
      begin
        ok := true;
        OnDefineDirective(Owner, DirName, ok);
        if not ok then
          Self.RaiseError(errInvalidCompilerDirective, [DirName]);
      end;

    if LA(1) = '}' then
      GetNextChar;

    Exit;
  end;

  I := Pos('UNDEF ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    I := TKernel(kernel).DefList.IndexOf(DirName);
    if I <> -1 then
      TKernel(kernel).DefList.RemoveAt(I);

    with TKernel(kernel) do
      if Assigned(OnUndefineDirective) then
      begin
        ok := true;
        OnUndefineDirective(Owner, DirName, ok);
        if not ok then
          Self.RaiseError(errInvalidCompilerDirective, [DirName]);
      end;

    if LA(1) = '}' then
      GetNextChar;

    Exit;
  end;

  I := Pos('IFOPT ', UpperCase(S) + ' ');

  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    ch := LA(1);

    Visible := false;
    if UpperCase(DirName) = 'B' then
    begin
      if ch = '+' then
        Visible := TKernel(kernel).CurrParser.CompleteBooleanEval
      else if ch = '-' then
        Visible := not TKernel(kernel).CurrParser.CompleteBooleanEval;
    end
    else if UpperCase(DirName) = 'H' then
    begin
      if ch = '+' then
        Visible := TKernel(kernel).CurrParser.IsUNIC
      else if ch = '-' then
        Visible := not TKernel(kernel).CurrParser.IsUNIC;
    end;

    DefStack.Push(_IFDEF, DirName, Visible);

    while LA(0) <> '}' do
      GetNextChar;

    goto Fin;
  end;

  I := Pos('IFDEF ', UpperCase(S) + ' ');

  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    Visible := TKernel(kernel).DefList.IndexOf(DirName) <> -1;
    Visible := Visible and DefStack.OuterVis;

    DefStack.Push(_IFDEF, DirName, Visible);

    while LA(0) <> '}' do
      GetNextChar;

    goto Fin;
  end;

  I := Pos('IFNDEF ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    while not ByteInSet(LA(0), IdsSet) do GetNextChar;

    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);
    DirName := RemoveLeftChars1(WhiteSpaces, Token.Text);

    Visible := TKernel(kernel).DefList.IndexOf(DirName) = -1;
    Visible := Visible and (DefStack.OuterVis);

    DefStack.Push(_IFNDEF, DirName, Visible);

    while LA(0) <> '}' do
      GetNextChar;

    goto Fin;
  end;

  I := Pos('ELSE ', UpperCase(S) + ' ');
  if I = 0 then
    I := Pos('ELSE}', UpperCase(S) + '}');

  if I > 0 then
  begin
    if DefStack.Count = 0 then
      Self.RaiseError(errInvalidCompilerDirective, ['']);

    Visible := DefStack.OuterVis;

    if DefStack.Top.Word in [_IFDEF, _IFNDEF, _ELSEIF] then
    begin

      for J:=DefStack.Count - 1 downto 0 do
      begin
        if DefStack[J].Vis then
        begin
          Visible := false;
          Break;
        end;
        if DefStack[J].Word = _IFDEF then
          break;
        if DefStack[J].Word = _IFNDEF then
        begin
//          Visible := not Visible;
          break;
        end;
      end;

    end
    else
      Self.RaiseError(errInvalidCompilerDirective, ['']);

    DefStack.Push(_ELSE, '', Visible);

    while LA(0) <> '}' do
      GetNextChar;
    goto Fin;
  end;

  I := Pos('ENDIF ', UpperCase(S) + ' ');
  if I = 0 then
    I := Pos('ENDIF}', UpperCase(S) + '}');

  if I > 0 then
  begin
    while LA(0) <> '}' do
      GetNextChar;

    J1 := 0;
    J2 := 0;
    for I := DefStack.Count - 1 downto 0 do
      if DefStack[I].Word in [_IFDEF, _IFNDEF] then
        Inc(J1)
      else if DefStack[I].Word = _ENDIF then
        Inc(J2);
    if J2 >= J1 then
      Self.RaiseError(errInvalidCompilerDirective, ['']);

    for I:=DefStack.Count - 1 downto 0 do
      if DefStack[I].Word in [_IFDEF, _IFNDEF] then
      begin
        while DefStack.Count > I do
          DefStack.Pop;
        Break;
      end;

    if DefStack.Count = 0 then
      Visible := true
    else
      Visible := DefStack[DefStack.Count - 1].Vis;

    while LA(0) <> '}' do
      GetNextChar;
    goto Fin;
  end;

  I := Pos('IF ', UpperCase(S) + ' ');
  if I = 1 then
  begin
    SCAN_EXPR := true;
    try
      ScanChars(WhiteSpaces);
      ReadToken;
      value := Scan_Expression;
    finally
      SCAN_EXPR := false;
    end;

    if value then
      Visible := DefStack.OuterVis
    else
      Visible := false;

    DefStack.Push(_IFDEF, '', Visible);

    ScanChars(WhiteSpaces);
    if not Visible then
      GenSeparator;
    goto Fin;
  end;

  I := Pos('CONST ', UpperCase(S) + ' ');
  if I = 1 then
  begin
    while not ByteInSet(LA(0), IdsSet) do
      GetNextChar;

    ScanChars(IdsSet + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);

    DirName := Token.Text;

    ScanChars(WhiteSpaces);

    if LA(1) = '=' then
    begin
      SCAN_EXPR := true;
      try
        ScanChars([Ord('='), Ord(' ')]);
        ReadToken;
        value := Scan_Expression;
      finally
        SCAN_EXPR := false;
      end;
    end;

    TKernel(kernel).DefList.Add(DirName, value);

    with TKernel(kernel) do
      if Assigned(OnDefineDirective) then
      begin
        ok := true;
        OnDefineDirective(Owner, DirName, ok);
        if not ok then
          Self.RaiseError(errInvalidCompilerDirective, [DirName]);
      end;

    ScanChars(WhiteSpaces);
    GenSeparator;
    Exit;
  end;

  I := Pos('ELSEIF ', UpperCase(S) + ' ');
  if I > 0 then
  begin
    SCAN_EXPR := true;
    try
      ScanChars(WhiteSpaces);
      ReadToken;
      value := Scan_Expression;
    finally
      SCAN_EXPR := false;
    end;
    if value then
      Visible := DefStack.OuterVis
    else
      Visible := false;

  	if DefStack.Count = 0 then
      Self.RaiseError(errInvalidCompilerDirective, ['ElseIf']);

    if not DefStack.Top.Word in [_IFDEF, _ELSEIF] then
      Self.RaiseError(errInvalidCompilerDirective, ['ElseIf']);

    for J:=DefStack.Count - 1 downto 0 do
    begin
      if DefStack[J].Vis then
      begin
        Visible := false;
        Break;
      end;
      if DefStack[J].Word = _IFDEF then
        break;
    end;

    DefStack.Push(_ELSEIF, '', Visible);

    ScanChars(WhiteSpaces);
    if not Visible then
      GenSeparator;
    goto Fin;
  end;


  I := Pos('IFEND ', UpperCase(S) + ' ');
  if I = 0 then
    I := Pos('IFEND}', UpperCase(S) + '}');

  if I = 1 then
  begin
    ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(',')]);
    if LA(1) = '}' then
      GetNextChar;

    J1 := 0;
    J2 := 0;
    for I := DefStack.Count - 1 downto 0 do
      if DefStack[I].Word in [_IFDEF, _IFNDEF] then
        Inc(J1)
      else if DefStack[I].Word = _ENDIF then
        Inc(J2);
    if J2 >= J1 then
      Self.RaiseError(errInvalidCompilerDirective, ['']);

    for I:=DefStack.Count - 1 downto 0 do
      if DefStack[I].Word in [_IFDEF, _IFNDEF] then
      begin
        while DefStack.Count > I do
          DefStack.Pop;
        Break;
      end;

    if DefStack.Count = 0 then
      Visible := true
    else
      Visible := DefStack[DefStack.Count - 1].Vis;

    goto Fin;
  end;

  with TKernel(kernel) do
    if Assigned(OnUnknownDirective) then
    begin
      ok := true;

      if LA(0) <> '}' then
      if LA(1) <> '}' then
      begin
        ScanChars(IdsSet + WhiteSpaces + [Ord('.'), Ord('-'), Ord('['), Ord(']'),
              Ord('('), Ord(')'), Ord(','), Ord('+'), Ord('*'),
              Ord(''''), Ord('"'), Ord('\'), Ord('/'), Ord(':')]);

        if Token.Text <> '}' then
          DirName := DirName + ' ' + Token.Text;
      end;
      DirName := RemoveRightChars(WhiteSpaces, DirName);

      OnUnknownDirective(Owner, DirName, ok);
      if not ok then
        Self.RaiseError(errInvalidCompilerDirective, [DirName]);

      while LA(0) <> '}' do
        GetNextChar;
    end
  else
    Self.RaiseError(errInvalidCompilerDirective, [DirName]);

Fin:

  if not Visible then
  begin
    I := 1;
    repeat
      GetNextChar;
      if ByteInSet(LA(0), [10, 13]) then
      begin
        Inc(LineCount);
        if LA(0) = #13 then
          GetNextChar;
      end;
      if IsEOF then
        break;

      if LA(0) = Start1 then
        if ByteInSet(LA(1), Start2) then
        begin
          S := Copy(Buff, P + 2, 3);
          if StrEql(S, 'if ') then
            Inc(I);

          S := Copy(Buff, P + 2, 6);
          if StrEql(S, 'ifdef ') then
            Inc(I);

          S := Copy(Buff, P + 2, 7);
          if StrEql(S, 'ifndef ') then
            Inc(I);

          S := Copy(Buff, P + 2, 6);
          if StrEql(S, 'endif ') or StrEql(S, 'endif}') then
          begin
            if I <= 1 then
              break;
            Dec(I);
          end;

          if StrEql(S, 'ifend ') or StrEql(S, 'ifend}') then
          begin
            if I <= 1 then
              break;
            Dec(I);
          end;

          S := Copy(Buff, P + 2, 5);
          if StrEql(S, 'else ') or StrEql(S, 'else}') then
          begin
            if I <= 1 then
              break;
          end;

          S := Copy(Buff, P + 2, 7);
          if StrEql(S, 'elseif ') or StrEql(S, 'elseif}') then
          begin
            if I <= 1 then
              break;
          end;
        end;

    until false;

    if IsEOF then
      Self.RaiseError(errMissingENDIFdirective, []);

    goto NextComment;
  end;
end;

function TBaseScanner.GetLinePos: Integer;
var
  I: Integer;
begin
  I := P;
  result := P;

  if I = 0 then
    Exit;

  repeat
    if ByteInSet(Buff[I], [13, 10]) then
      Break
    else
      Dec(I);
  until I <= 0;
  result := P - I;

end;

function TBaseScanner.ScanRegExpLiteral: String;
begin
  result := '';
  while not ((LA(1) = '/') and
    ByteInSet(LA(2), [Ord('i'), Ord('I'), Ord('g'), Ord('G'), Ord('m'), Ord('M'),
         Ord('.'), Ord(','), Ord(')')])) do
  begin
    GetNextChar;
    if IsEOF(LA(0)) then Exit;
    result := result + LA(0);
  end;
  SetScannerState(scanProg);
end;

procedure TBaseScanner.ScanChars(CSet: TByteSet);
begin
  Token.Position := P;
  while ByteInSet(LA(1), CSet) do
    GetNextChar;

  token.Length := p - token.Position + 1;
  Token.TokenClass := tcIdentifier;
  SetScannerState(scanProg);
end;

function TBaseScanner.GetBuffLength: Integer;
begin
  result := Length(Buff);
end;

procedure TBaseScanner.SetScannerState(Value: TScannerState);
begin
  fScannerState := Value;
  if Value = scanText then
    fBackslash := true
  else
    fBackslash := false;
end;

function TBaseScanner.ScanFormatString: String;
var
  P1, P2: Integer;
  StrFormat, VarName: String;
begin
//  "%" [index ":"] ["-"] [width] ["." prec] type

  GetNextChar;
  P1 := P;
  if IsDigit(LA(1)) then // Index
  begin
    while IsDigit(LA(1)) do
      GetNextChar;
    if LA(1) <> ':' then
      RaiseError(errTokenExpected, [':', LA(1)]);

    GetNextChar;
  end;
  if LA(1) = '-' then
    GetNextChar;
  if IsDigit(LA(1)) then // width
    while IsDigit(LA(1)) do
      GetNextChar;
  if LA(1) = '.' then
    GetNextChar;
  if IsDigit(LA(1)) then // prec
    while IsDigit(LA(1)) do
      GetNextChar;
  GetNextChar; // type

  P2 := P;

  StrFormat := Copy(Buff, P1, P2 - P1 + 1);

  result := Token.Text;
  result := result + StrFormat;

  if LA(1) <> '=' then
    RaiseError(errTokenExpected, ['=', LA(1)]);
  GetNextChar;

  if not IsAlpha(LA(1)) then
    RaiseError(errIdentifierExpected, []);
  VarName := GetNextChar;
  while ByteInSet(LA(1), IdsSet) do
    VarName := VarName + GetNextChar;

  VarNameList.Add(VarName);
end;

procedure TBaseScanner.ScanHtmlString(const Ch: String);
var
  K1, K2: Integer;
  c: Char;
begin
  GenBeginText;
  K1 := 0;
  K2 := 0;
  with Token do
  begin
    TokenClass := tcHtmlStringConst;
    repeat
      c := GetNextChar;
      case c of
        #0,#13,#10:
        begin
          if c = #13 then
            GetNextChar;
          IncLineCount;
          GenSeparator;
        end;
        '\':
        if (K1 mod 2 = 0) and (K2 mod 2 = 0) then
        begin
          if fBackslash then
            case LA(1) of
              '%': ScanFormatString;
              'b':
              begin
                Buff[P] := #$08;
                GetNextChar;
              end;
              't':
              begin
                Buff[P] := #$09;
                GetNextChar;
              end;
              'n':
              begin
                Buff[P] := #$0A;
                GetNextChar;
              end;
              'v':
              begin
                Buff[P] := #$0B;
                GetNextChar;
              end;
              'f':
              begin
                Buff[P] := #$0C;
                GetNextChar;
              end;
              'r':
              begin
                Buff[P] := #$0D;
                GetNextChar;
              end;
              '\':
              begin
                Buff[P] := #$5C;
                GetNextChar;
              end;
            end; // case
          end;
        '''':
        begin
          Inc(K1);
        end;
        '"':
        begin
          Inc(K2);
        end;
        '<':
        if LA(1) = '?' then
        begin
          Dec(P);
          Break;
        end
        else if LA(1) = '%' then
        begin
          Dec(P);
          Break;
        end;
        #255:
        begin
          Dec(P);
          Break;
        end;
      end;
    until false;
  end;
  GenEndText;
end;

procedure TBaseScanner.IncLineCount;
begin
  if ScannerStack.Count = 0 then
  begin
    Inc(LineCount);
  end;
end;

procedure TBaseScanner.InsertText(const S: String);
begin
  Insert(S, Buff, P + 1);
end;

function ConvertString(const S: String): String;
var
  I, L: Integer;
  Ch: Char;
begin
  result := '';
  L := SHigh(S);
  if L = 0 then
    Exit;
  I := SLow(S);
  repeat
    Ch := S[I];
    if Ch = '\' then
    begin
      if I = L then
      begin
        result := result + Ch;
        Exit;
      end;
      case S[I + 1] of
        'b':
        begin
          result := result + #$08;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        't':
        begin
          result := result + #$09;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        'n':
        begin
          result := result + #$0A;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        'v':
        begin
          result := result + #$0B;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        'f':
        begin
          result := result + #$0C;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        'r':
        begin
          result := result + #$0D;
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        '\':
        begin
          result := result + '\';
          Inc(I);
          Inc(I);
          if I > L then
            break;
        end;
        else
        begin
          result := result + Ch;
          Inc(I);
          if I > L then
            break;
        end;
      end;
    end
    else
    begin
      result := result + Ch;
      Inc(I);
      if I > L then
        break;
    end;
  until false;
end;

class function TBaseScanner.IsValidToken(const S: String): Boolean;
var
  I: Integer;
begin
  if S = '' then
  begin
    result := false;
    Exit;
  end;
  result := true;
  for I := SLow(S) to SHigh(S) do
    if ByteInSet(S[I], [10, 13] + Whitespaces) then
    begin
      result := false;
      Exit;
    end;
end;

procedure TBaseScanner.BeginComment(value: Integer);
begin
  with CurrComment do
  if Valid then
  if CommentedTokens.Count = 0 then
  begin
    if CommentValue = value then
      Exit;
  end;

  if CurrComment.Valid then
    DoComment;
  CurrComment.P1 := P;
end;

procedure TBaseScanner.EndComment(value: Integer);
begin
  with CurrComment do
  begin
    Comment := Copy(Buff, P1, Position - P1 + 1);
    CommentValue := value;
    CurrCommN := TKernel(kernel).Code.Card;
  end;
end;

procedure TBaseScanner.DoComment;
var
  N, ClsId, NsId: Integer;
  Context, S: String;
begin
  if Assigned(TKernel(kernel).OnComment) then
  with CurrComment do
  if Valid then
  begin
    S := Comment;

    N := CurrCommN;
    ClsId := TKernel(kernel).Code.GetCurrClassId(N);
    NsId := TKernel(kernel).Code.GetCurrNamespaceId(N);
    if NsId > 0 then
    begin
      Context := TKernel(kernel).SymbolTable[NsId].FullName;
      if ClsId > 0 then
        Context := Context + '.' + TKernel(kernel).SymbolTable[ClsId].Name;
    end
    else
      if ClsId > 0 then
        Context := TKernel(kernel).SymbolTable[ClsId].Name;


    CommentedTokens.Pack;
    TKernel(kernel).OnComment(TKernel(kernel).Owner,
                              S,
                              Context,
                              CommentedTokens.Keys);
    Clear;
  end;
end;

procedure TBaseScanner.AttachId(Id: Integer; Upcase: Boolean);
var
  S: String;
  I: Integer;
  b: Boolean;
begin
  S := TKernel(kernel).SymbolTable[Id].Name;
  with CurrComment do
    for I := 0 to CommentedTokens.Count - 1 do
    begin
      if Upcase then
        b := StrEql(S, CommentedTokens.Keys[I])
      else
        b := S = CommentedTokens.Keys[I];
      if b then
      begin
        CommentedTokens.Values[I] := Id;
        break;
      end;
    end;
end;

class function TBaseScanner.IsConstToken(AToken: TToken): Boolean;
begin
  result := AToken.TokenClass in
   [tcBooleanConst, tcCharConst, tcPCharConst, tcIntegerConst,
    tcDoubleConst, tcNumCharConst, tcVariantConst,
    tcHtmlStringConst];
end;

function TBaseScanner.IsCurrText(const S: String): Boolean;
begin
  result := StrEql(Token.Text, S);
end;

procedure TBaseScanner.Match(const S: String);
begin
  if IsCurrText(S) then
    ReadToken
  else
    RaiseError(errTokenExpected, [S, Token.Text]);
end;

function TBaseScanner.Scan_Factor: Variant;
begin
  if IsCurrText('true') then
  begin
    result := true;
    ReadToken;
  end
  else if IsCurrText('false') then
  begin
    result := false;
    ReadToken;
  end
  else if IsCurrText('defined') then
  begin
    ReadToken;
    Match('(');
    result := TKernel(kernel).DefList.IndexOf(Token.Text) >= 0;
    ReadToken;
    Match(')');
  end
  else if IsCurrText('declared') then
  begin
    ReadToken;
    Match('(');
    result := TKernel(kernel).SymbolTable.Lookup(Token.Text,
              TKernel(kernel).CurrParser.CurrLevel, true) > 0;
    ReadToken;
    Match(')');
  end
  else if IsCurrText('sizeof') then
  begin
    ReadToken;
    Match('(');
    if IsCurrText('Pointer') then
      result := SizeOf(Pointer)
    else
      result := 0;
    ReadToken;
    Match(')');
  end
  else if Token.TokenClass = tcIntegerConst then
  begin
    result := StrToInt(Token.Text);
    ReadToken;
  end
  else if Token.TokenClass = tcCharConst then
  begin
    result := Copy(Token.Text, 2, Length(Token.Text) - 2);
    ReadToken;
  end
  else if Token.TokenClass = tcPCharConst then
  begin
    result := Copy(Token.Text, 2, Length(Token.Text) - 2);
    ReadToken;
  end
  else if Token.TokenClass = tcDoubleConst then
  begin
    result := StrToFloat(Token.Text);
    ReadToken;
  end
	else if IsCurrText('+') then
  begin
		ReadToken;
		result := Scan_Factor;
  end
	else if IsCurrText('-') then
  begin
		ReadToken;
		result := - Scan_Factor;
  end
	else if IsCurrText('not') then
  begin
		ReadToken;
		result := not Scan_Factor;
  end
  else if IsCurrText('(') then
  begin
    Match('(');
    result := Scan_Expression;
    Match(')');
  end
  else
    result := Scan_Ident;
end;

function TBaseScanner.Scan_Ident: Variant;
begin
  RaiseError(errNotImplementedYet, []);
end;

function TBaseScanner.Scan_Expression: Variant;
begin
  RaiseError(errNotImplementedYet, []);
end;

function _ParametrizedTypeExpected(scanner: TBaseScanner; const Buff: String; P: Integer): Boolean;
var
  I, L: Integer;
label again, again2;
begin
  result := false;
  L := Length(Buff);
  I := P;
  while ByteInSet(Buff[I], WhiteSpaces) do
  begin
    Inc(I);
    if I > L then
      Exit;
  end;
  if Buff[I] <> '<' then
    Exit;
  Inc(I);
again:
  while ByteInSet(Buff[I], WhiteSpaces) do
  begin
    Inc(I);
    if I > L then
      Exit;
  end;

  if not scanner.IsAlpha(Buff[I]) then
    Exit;

  Inc(I);
  while scanner.IsAlpha(Buff[I]) or TBaseScanner.IsDigit(Buff[I]) do
  begin
    Inc(I);
    if I > L then
      Exit;
  end;
again2:
  while ByteInSet(Buff[I], WhiteSpaces) do
  begin
    Inc(I);
    if I > L then
      Exit;
  end;
  if Buff[I] = '>' then
  begin
    result := true;
    Exit;
  end;
  if Buff[I] = ',' then
  begin
    Inc(I);
    goto again;
  end;
  if Buff[I] = ';' then
  begin
    Inc(I);
    goto again;
  end;
  if Buff[I] = ':' then
  begin
    result := true;
    Exit;
  end;
  if Buff[I] = '<' then
  begin
    result := true;
    Exit;
  end;
  if Buff[I] = '.' then
  begin
    Inc(I);
    goto again;
  end;
  if Buff[I] = '{' then
  begin
    while Buff[I] <> '}' do
    begin
      Inc(I);
      if I > L then
        Exit;
    end;
    Inc(I);
    goto again2;
  end;
  if (Buff[I] = '(') and (Buff[I+1] = '*') then
  begin
    while not ((Buff[I] = '*') and (Buff[I+1] = ')')) do
    begin
      Inc(I);
      if I > L then
        Exit;
    end;
    Inc(I);
    Inc(I);
    goto again2;
  end;
end;

function TBaseScanner.ParametrizedTypeExpected: Boolean;
begin
  result := _ParametrizedTypeExpected(Self, Buff, P + 1);
end;

function TBaseScanner._GetCurrText: String;
begin
  result := Copy(Buff, P, 30);
end;

function TBaseScanner.FindPosition(Chars: TByteSet): Integer;
begin
  result := P;
  while not (Ord(Buff[result]) in Chars) do
  begin
    Dec(result);
    if result = 0 then
      Exit;
  end;
end;

function TBaseScanner.Precede(ch1, ch2: Char): Boolean;
var
  I: Integer;
  c: Char;
begin
  result := false;
  I := P;
  repeat
    Inc(I);
    c := Buff[I];
    if c = CHAR_EOF then
      Exit;
    if c = ch1 then
    begin
      result := true;
      Exit;
    end;
    if c = ch2 then
      Exit;
  until false;
end;

function TBaseScanner.GetExAlphaList: TAssocIntegers;
begin
  if kernel = nil then
    result := nil
  else
    result := TKernel(kernel).ExAlphaList;
end;

function TBaseScanner.AreNextChars(const S: String): Boolean;
var
  Q: String;
begin
  Q := Copy(Buff, P + 1, Length(S));
  result := Q = S;
end;

end.
