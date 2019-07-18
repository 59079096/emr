////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_Basic.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
{$Q-}
{$B-}
{$R-}
unit PAXCOMP_Basic;
interface

{$IFNDEF LINUX} // Just to compile PASCAL only (using FPC on Mac)
{$IFDEF UNIX} // Just to compile PASCAL only (using FPC on Mac)
implementation

end.
{$ENDIF}
{$ENDIF}

uses {$I uses.def}
  SysUtils,
  Classes,
  Math,
{$IFDEF VARIANTS}
  DateUtils,
{$ENDIF}  
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_STDLIB,
  PAXCOMP_BASESYMBOL_TABLE;

procedure Register_StdBasic(st: TBaseSymbolTable);

var
  H_Namespace, H_Sub: Integer;
const
  ByRef = true;

implementation

uses
  PAXCOMP_JavaScript;

const
  vbEmpty = 0;
  vbNull = 1;
  vbInteger = 2;
  vbLong = 3;
  vbSingle = 4;
  vbDouble = 5;
  vbCurrency = 6;
  vbDate = 7;
  vbString = 8;
  vbObject = 9;
  vbError = 10;
  vbBoolean = 11;
  vbVariant = 12;
  vbDataObject = 13;
  vbByte = 17;
  vbArray = 8192;

  vbGeneralDate = 0;
  vbLongDate = 1;
  vbShortDate = 2;
  vbLongTime = 3;
  vbShortTime = 4;

  MonthNames: array [1..12] of string =
  (
  'January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December'
  );

  WeekDayNames: array [1..7] of string =
  (
  'Sunday',
  'Monday',
  'Tuesday',
  'Wednesday',
  'Thursday',
  'Friday',
  'Saturday'
  );

function TestVar(P: PVariant): Boolean;
begin
  result := P <> nil;
  if result then
    result := VarType(P^) <> varEmpty;
end;

function _Tan(const X: Extended): Extended;
begin
  result := ArcTan(X);
end;

function _Atn(const X: Extended): Extended;
begin
  result := ArcTan(X);
end;

function _Sin(const X: Extended): Extended;
begin
  result := Sin(X);
end;

function _Cos(const X: Extended): Extended;
begin
  result := Cos(X);
end;

function _Exp(const X: Extended): Extended;
begin
  result := Exp(X);
end;

function _Sqr(const X: Extended): Extended;
begin
  result := Sqr(X);
end;

function _CBool(const X: Variant): Boolean;
begin
  result := X;
end;

function _CByte(const X: Variant): Byte;
begin
  result := X;
end;

function _CCurr(const X: Variant): Currency;
begin
  result := X;
end;

function _CDate(const X: Variant): TDateTime;
begin
  result := X;
end;

function _CDbl(const X: Variant): Double;
begin
  result := X;
end;

function _CInt(const X: Variant): Integer;
begin
  result := X;
end;

function _CLong(const X: Variant): Integer;
begin
  result := X;
end;

{$IFNDEF PAXARM}
{$IFNDEF MACOS32}
{$IFNDEF LINUX}
function _CreateObject(const ClassName: String): Variant;
begin
  result := CreateOleObject(ClassName);
end;

function _GetObject(const ClassName: String): Variant;
begin
  result := GetActiveOleObject(ClassName);
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

function _Date(): TDateTime;
begin
  result := Date;
end;

function _DateSerial(Y, M, D: Word): TDateTime;
begin
  result := EncodeDate(Y,M,D);
end;

function _TimeSerial(Hour, Min, Sec: Word; MSec: Word = 0): TDateTime;
begin
  result := EncodeTime(Hour, Min, Sec, MSec);
end;

function _Day(const V: Variant): Integer;
var
  Y, M, D: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeDate(ADate, Y, M, D);
  result := D;
end;

function _Month(const V: Variant): Integer;
var
  Y, M, D: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeDate(ADate, Y, M, D);
  result := M;
end;

function _MonthName(const V: Variant; abbreviate: Boolean = false): String;
var
  M: Integer;
begin
  M := V;
  if (M < 1) or (M > 12) then
  begin
    result := 'Undefined';
    Exit;
  end;
  result := MonthNames[M];
  if abbreviate then
    result := SCopy(result, SLow(result), 3);
end;

function _WeekDayName(const V: Variant; abbreviate: Boolean = false): String;
var
  D: Integer;
begin
  D := V;
  if (D < 1) or (D > 7) then
  begin
    result := 'Undefined';
    Exit;
  end;
  result := WeekDayNames[D];
  if abbreviate then
    result := SCopy(result, SLow(result), 3);
end;

function _WeekDay(const V: Variant): Integer;
var
  ADate: Double;
begin
  ADate := VariantToDate(V);
  result := DayOfWeek(ADate);
end;

function _Year(const V: Variant): Integer;
var
  Y, M, D: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeDate(ADate, Y, M, D);
  result := Y;
end;

{$IFDEF VARIANTS}
function _DateAdd(const Interval: String; Number: Integer; const V: Variant): TDateTime;
var
  ADate: TDateTime;
begin
  ADate := VariantToDate(V);
  if Interval = 'yyyy' then
    result := IncYear(ADate, Number)
  else if Interval = 'm' then
    result := IncMonth(ADate, Number)
  else if Interval = 'q' then
    result := IncMonth(ADate, Number * 4)
  else if Interval = 'd' then
    result := IncDay(ADate, Number)
  else if Interval = 'h' then
    result := IncHour(ADate, Number)
  else if Interval = 'n' then
    result := IncMinute(ADate, Number)
  else if Interval = 's' then
    result := IncSecond(ADate, Number)
  else if Interval = 'd' then
  begin
    ADate := StartOfTheYear(ADate);
    result := IncDay(ADate, Number);
  end
  else if Interval = 'w' then
  begin
    ADate := StartOfTheWeek(ADate);
    result := IncDay(ADate, Number);
  end
  else if Interval = 'ww' then
  begin
    ADate := StartOfTheYear(ADate);
    result := IncDay(ADate, Number * 7);
  end
  else
    raise Exception.Create('Wrong Interval parameter');
end;

function _DateDiff(const Interval: String; const V1, V2: Variant): Integer;
var
  ADate1, ADate2: TDateTime;
begin
  ADate1 := VariantToDate(V1);
  ADate2 := VariantToDate(V2);
  if Interval = 'yyyy' then
    result := YearOf(ADate2) - YearOf(ADate1)
  else if Interval = 'm' then
    result := MonthOf(ADate2) - MonthOf(ADate1)
  else if Interval = 'q' then
    result := (MonthOf(ADate2) - MonthOf(ADate1)) mod 4
  else if Interval = 'd' then
    result := DayOf(ADate2) - DayOf(ADate1)
  else if Interval = 'h' then
    result := HourOf(ADate2) - HourOf(ADate1)
  else if Interval = 'n' then
    result := MinuteOf(ADate2) - MinuteOf(ADate1)
  else if Interval = 's' then
    result := SecondOf(ADate2) - SecondOf(ADate1)
  else if Interval = 'd' then
    result := DayOf(ADate2) - DayOf(ADate1)
  else if Interval = 'w' then
    result := WeekOf(ADate2) - WeekOf(ADate1)
  else if Interval = 'ww' then
    result := WeekOfTheYear(ADate2) - WeekOfTheYear(ADate1)
  else
    raise Exception.Create('Wrong Interval parameter');
end;

function _DatePart(const Interval: String; const V: Variant): Integer;
var
  ADate: TDateTime;
begin
  ADate := VariantToDate(V);
  if Interval = 'yyyy' then
    result := YearOf(ADate)
  else if Interval = 'm' then
    result := MonthOf(ADate)
  else if Interval = 'q' then
    result := MonthOf(ADate) mod 4 + 1
  else if Interval = 'd' then
    result := DayOf(ADate)
  else if Interval = 'h' then
    result := HourOf(ADate)
  else if Interval = 'n' then
    result := MinuteOf(ADate)
  else if Interval = 's' then
    result := SecondOf(ADate)
  else if Interval = 'd' then
    result := DayOfTheYear(ADate)
  else if Interval = 'w' then
    result := WeekOf(ADate)
  else if Interval = 'ww' then
    result := WeekOfTheYear(ADate)
  else
    raise Exception.Create('Wrong Interval parameter');
end;
{$ENDIF}

function _Hour(const V: Variant): Integer;
var
  Hour, Min, Sec, MSec: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeTime(ADate, Hour, Min, Sec, MSec);
  result := Hour;
end;

function _Minute(const V: Variant): Integer;
var
  Hour, Min, Sec, MSec: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeTime(ADate, Hour, Min, Sec, MSec);
  result := Min;
end;

function _Second(const V: Variant): Integer;
var
  Hour, Min, Sec, MSec: Word;
  ADate: Double;
begin
  ADate := VariantToDate(V);
  DecodeTime(ADate, Hour, Min, Sec, MSec);
  result := Sec;
end;

function _Filter(const Source: Variant; const Match: String;
                 Include: Boolean = true): Variant;
var
  L: TStringList;
  I: Integer;
  S: String;
begin
  L := TStringList.Create;
  try
    for I:=0 to VarArrayHighBound(Source, 1) do
    begin
      S := Source[I];
      if Include then
      begin
        if Pos(Match, S) > 0 then
          L.Add(S);
      end
      else
      begin
        if Pos(Match, S) = 0 then
          L.Add(S);
      end;
    end;
    result := VarArrayCreate([0, L.Count - 1], varVariant);
    for I:=0 to L.Count - 1 do
      result[I] := L[I];
  finally
    FreeAndNil(L);
  end;
end;

function _Join(const Source: Variant; const Delimiter: String = ' '): String;
var
  I, K: Integer;
begin
  result := '';
  K := VarArrayHighBound(Source, 1);
  for I:=0 to K do
  begin
    result := result + Source[I];
    if I < K then
      result := result + Delimiter;
  end;
end;

function _Split(const Expression: String;
                const Delimiter: String = ' ';
                Limit: Integer = -1): Variant;
var
  L: TStringList;
  I: Integer;
  S: String;
begin
  L := TStringList.Create;
  S := Expression;
  try
    I := Pos(Delimiter, S);
    while I > 0 do
    begin
      L.Add(Copy(S, 1, I - 1));
      Delete(S, I, Length(Delimiter));
      I := Pos(Delimiter, S);
    end;
    result := VarArrayCreate([0, L.Count - 1], varVariant);
    for I:=0 to L.Count - 1 do
      result[I] := L[I];
  finally
    FreeAndNil(L);
  end;
end;

function _UCase(const S: String): String;
begin
  result := UpperCase(S);
end;

function _LCase(const S: String): String;
begin
  result := LowerCase(S);
end;

function _LBound(const V: Variant): Integer;
begin
  result := 0;
end;

function _UBound(const V: Variant; D: Integer = 1): Integer;
begin
  result := VarArrayHighBound(V, D)
end;

function _CStr(const V: Variant): Variant;
begin
  result := VarToStr(V);
end;

function _InList(P1: PVariant = nil; P2: PVariant = nil): Boolean;
var
  sParam1, sParam2 : String;
  lsParam2 : TStringList;
begin
  result := False;
  if (not TestVar(P1)) or (not TestVar(P2)) then Exit;
  sParam1 := VarToStr(P1^);
  sParam2 := VarToStr(P2^);
  lsParam2 := TStringList.Create;
  try
    lsParam2.CommaText := sParam2;
    result := lsParam2.IndexOf(sParam1) > -1;
  finally
    FreeAndNil(lsParam2);
  end;
end;

function  _AtLeastOneInList(P1: PVariant = nil; P2: PVariant = nil): Boolean;
var
  sParam1, sParam2, sParam : String;
  lsParam1, lsParam2 : TStringList;
  iItem, iIndex : Integer;
begin
  result := False;
  if (not TestVar(P1)) or (not TestVar(P2)) then Exit;
  sParam1 := VarToStr(P1^);
  sParam2 := VarToStr(P2^);

  lsParam1 := TStringList.Create;
  lsParam2 := TStringList.Create;
  try
    lsParam1.Sorted := True;
    lsParam1.Duplicates := dupIgnore;
    lsParam1.CommaText := sParam1;

    lsParam2.Sorted := True;
    lsParam2.Duplicates := dupIgnore;
    lsParam2.CommaText := sParam2;

    for iItem := 0 to Pred(lsParam1.Count) do
    begin
      sParam := lsParam1[iItem];
      if lsParam2.Find(sParam, iIndex) then
      begin
        result := True;
        Exit;
      end;
    end;
  finally
    FreeAndNil(lsParam1);
    FreeAndNil(lsParam2);
  end;
end;

function _StrReverse(const S: String): String;
var
  I: Integer;
begin
  result := '';
  for I := SLow(S) to SHigh(S) do
    result := S[I] + result;
end;

function _InStr(const S1, S2: string): Integer;
begin
  result := Pos(S2, S1);
end;

function _InStrRev(const P0, P1: string; P2: PVariant = nil): Integer;
var
  V1, V2: Variant;
  S1, S2: String;
  ParamCount, start, I: Integer;
begin
  start := 0;
  ParamCount := 2;
  if TestVar(P2) then
     ParamCount := 3;
  case ParamCount of
    2:
    begin
      start := 0;
      V1 := P0;
      V2 := P1;
    end;
    3:
    begin
      V1 := P0;
      V2 := P1;
      start := P2^;
    end;
  end;

  S1 := VarToStr(V1);
  S2 := VarToStr(V2);

  if Length(S1) = 0 then
    result := 0
  else if Length(S2) = 0 then
    result := start
  else if start > Length(S1) then
    result := 0
  else if Length(S2) > Length(S1) then
    result := 0
  else if Length(S2) = Length(S1) then
  begin
    if S1 = S2 then
      result := 1
    else
      result := 0;
  end
  else
  begin
    if Start > 0 then
      S1 := Copy(S1, 1, Start);
    for I:= Length(S1) - Length(S2) + 1 downto 1 do
      if S2 = Copy(S1, I, Length(S2)) then
      begin
        result := I;
        Exit;
      end;
    result := 0;
  end;
end;

function _Len(const S: String): Integer;
begin
  result := Length(S);
end;

function _Replace(const S, OldPattern, NewPattern: string): String;
begin
  result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
end;

function _RGB(red, green, blue: Integer): Integer;
begin
  result := red + (green * 256) + (blue * 65536);
end;

function _FormatNumber(const V: Variant; P2: PVariant = nil; P3: PVariant = nil): Variant;
var
  D: Double;
  Fmt: String;
  NumDigitsAfterDecimal: Integer;
begin
  if (not TestVar(P2)) and (not TestVar(P3)) then
  begin
    result := VarToStr(V);
    Exit;
  end;
  if TestVar(P2) then
  begin
    D := V;
    NumDigitsAfterDecimal := P2^;
    Fmt := '%*.' + IntToStr(NumDigitsAfterDecimal) + 'f';
    result := Format(Fmt, [D]);
  end;
end;

function _FormatPercent(const V: Variant; P2: PVariant = nil; P3: PVariant = nil): Variant;
var
  D: Double;
  Fmt: String;
  NumDigitsAfterDecimal: Integer;
begin
  if (not TestVar(P2)) and (not TestVar(P3)) then
  begin
    result := VarToStr(V * 100) + '%';
    Exit;
  end;
  if TestVar(P2) then
  begin
    D := V * 100;
    NumDigitsAfterDecimal := P2^;
    Fmt := '%*.' + IntToStr(NumDigitsAfterDecimal) + 'f';
    result := Format(Fmt, [D]) + '%';
  end;
end;

function _Rnd(const V: Variant; P2: PVariant = nil): Variant;
begin
  if TestVar(P2) then
    result := Random(Integer(P2^ - V)) + V
  else
    result := Random(Integer(V));
end;

function _Round(const V: Variant; P2: PVariant = nil): Variant;
var
  D: Double;
  L, P: Integer;
  S: String;
begin
  if not TestVar(P2) then
{$IFDEF VARIANTS}
    result := Round(Double(V))
{$ELSE}
    result := Integer(Round(V))
{$ENDIF}
  else
  begin
    L := P2^;
    D := V;
    S := FloatToStr(D);
    S := _Replace(S, ',', '.');
    P := Pos('.', S);
    if P > 0 then
    begin
      S := Copy(S, 1, P + L);
      D := StrToFloat(S);
      result := D;
    end
    else
{$IFDEF VARIANTS}
      result := Round(Double(V));
{$ELSE}
      result := Integer(Round(V));
{$ENDIF}
  end;
end;

function _Log(const V: Extended): Extended;
begin
  result := Ln(V);
end;

function _TimeToStr(const DateTime: TDateTime): string;
begin
  result := TimeToStr(DateTime);
end;

function _DateToStr(const DateTime: TDateTime): string;
begin
  result := DateToStr(DateTime);
end;

function _TypeName(const V: Variant): String;
var
  S: String;
begin
  S := '';
  case VarType(V) of
    varEmpty: S := 'Empty';
    varNull: S := 'Null';
    varSmallInt: S := 'Integer';
    varInteger: S := 'Long';
    varSingle: S := 'Single';
    varDouble: S := 'Double';
    varCurrency: S := 'Currency';
    varDate: S := 'Date';
    varString,
{$IFDEF UNIC}
    varUString,
{$ENDIF}
    varOleStr: S := 'String';
    varBoolean: S := 'Boolean';
    varVariant: S := 'Variant';
    varDispatch: S := 'Dispatch';
    varByte: S := 'Byte';
  end;

  if VarType(V) >= varArray then
    S := 'Array';

  result := S;
end;

function IsNumericString(const S: String): Boolean;
var
  I: Integer;
begin
  if S = '' then
  begin
    result := false;
    Exit;
  end;

  result := true;
  for I:=1 to Length(S) do
    if not ByteInSet(S[I], [Ord('0')..Ord('9'),
      Ord('.'),Ord('+'),Ord('-'),Ord('e'),Ord('E')]) then
    begin
      result := false;
      Exit;
    end;
end;

function _IsNumeric(const V: Variant): Boolean;
begin
  result := false;
  case VarType(V) of
    varInteger, varByte, varDouble: result := true;
    varString: result := IsNumericString(V);
   end;
end;

function _Int(D: Double): Integer;
begin
  if D >= 0 then
    result := Trunc(D)
  else
  begin
    if Frac(D) <> 0.0 then
      result := Trunc(D) - 1
    else
      result := Trunc(D);
  end;
end;

function _Fix(D: Double): Integer;
begin
  if D >= 0 then
    result := _Int(D)
  else
    result := -1 * _Int(Abs(D));
end;

function _Chr(I: Integer): Char;
begin
  result := Chr(I);
end;

function _Left(const S: String; L: Integer): String;
begin
  result := SCopy(S, SLow(S), L);
end;

function _Right(const S: String; L: Integer): String;
var
  I: Integer;
begin
  if L > Length(S) then
    L := Length(S);

  result := '';

  for I:=Length(S) downto Length(S) - L + 1 do
    result := S[I] + result;
end;

function _Sgn(V: Integer): Integer;
begin
  if V > 0 then
    result := 1
  else if V < 0 then
    result := -1
  else
    result := 0;
end;

function _Mid(const P0: String; P1: Integer; P2: PVariant = nil): String;
var
  L: Integer;
begin
  if TestVar(P2) then
    L := P2^
  else
    L := Length(P0);
  result := Copy(P0, P1, L);
end;

function _CLng(const X: Variant): Variant;
begin
  result := JS_ToInt32(X);
end;

function _CSng(const X: Variant): Variant;
begin
  result := JS_ToNumber(X);
end;

function _Hex(const X: Variant): String;
var
  I: Integer;
begin
  I := JS_ToInt32(X);
  result := Format('%x', [I]);
end;

function _IsEmpty(const X: Variant): Boolean;
begin
  result := VarType(X) = varEmpty;
end;

function _IsArray(const X: Variant): Boolean;
begin
  result := VarType(X) = varArray;
end;

procedure Register_StdBasic(st: TBaseSymbolTable);
begin
{$IFNDEF PAXARM}
{$IFNDEF MACOS32}
{$IFNDEF LINUX}
  CoInitialize(nil);
{$ENDIF}
{$ENDIF}
{$ENDIF}

  with st do
  begin
    H_Namespace := RegisterNamespace(0, StrBasicNamespace);
    H_BasicNamespace := H_Namespace;

    RegisterConstant(H_Namespace, 'vbEmpty', 0);
    RegisterConstant(H_Namespace, 'vbNull', 1);
    RegisterConstant(H_Namespace, 'vbInteger', 2);
    RegisterConstant(H_Namespace, 'vbLong', 3);
    RegisterConstant(H_Namespace, 'vbSingle', 4);
    RegisterConstant(H_Namespace, 'vbDouble', 5);
    RegisterConstant(H_Namespace, 'vbCurrency', 6);
    RegisterConstant(H_Namespace, 'vbDate', 7);
    RegisterConstant(H_Namespace, 'vbString', 8);
    RegisterConstant(H_Namespace, 'vbObject', 9);
    RegisterConstant(H_Namespace, 'vbError', 10);
    RegisterConstant(H_Namespace, 'vbBoolean', 11);
    RegisterConstant(H_Namespace, 'vbVariant', 12);
    RegisterConstant(H_Namespace, 'vbDataObject', 13);
    RegisterConstant(H_Namespace, 'vbByte', 17);
    RegisterConstant(H_Namespace, 'vbArray', 8192);

    H_Sub := RegisterRoutine(H_Namespace, 'Abs', typeVOID, ccSTDCALL, nil);
    RegisterParameter(H_Sub, typeVOID, Unassigned, ByRef, 'X');

    RegisterRoutine(H_Namespace, 'Asc', typeVOID, ccSTDCALL, nil);

    H_Sub := RegisterRoutine(H_Namespace, 'Tan', typeEXTENDED, ccREGISTER,
                             @_Tan);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Atn', typeEXTENDED, ccREGISTER,
                             @_Atn);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Sin', typeEXTENDED, ccREGISTER,
                             @_Sin);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Cos', typeEXTENDED, ccREGISTER,
                             @_Cos);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Exp', typeEXTENDED, ccREGISTER,
                             @_Exp);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Sqr', typeEXTENDED, ccREGISTER,
                             @_Sqr);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'Log', typeEXTENDED, ccREGISTER,
                             @_Log);
    RegisterParameter(H_Sub, typeEXTENDED, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CBool', typeBOOLEAN, ccREGISTER,
                             @_CBool);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CByte', typeBYTE, ccREGISTER,
                             @_CByte);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CCurr', typeCURRENCY, ccREGISTER,
                             @_CCurr);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CDate', typeDOUBLE, ccREGISTER,
                             @_CDate);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CDbl', typeDOUBLE, ccREGISTER,
                             @_CDbl);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    RegisterRoutine(H_Namespace, 'CChar', typeVOID, ccSTDCALL, nil);

    H_Sub := RegisterRoutine(H_Namespace, 'CInt', typeINTEGER, ccREGISTER,
                             @_CInt);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

    H_Sub := RegisterRoutine(H_Namespace, 'CLong', typeINTEGER, ccREGISTER,
                             @_CLong);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'X');

{$IFNDEF PAXARM}
{$IFNDEF TAB}

{$IFDEF MSWINDOWS}
    H_Sub := RegisterRoutine(H_Namespace, 'CreateObject', typeVARIANT, ccREGISTER,
                             @_CreateObject);
{$ELSE}
    H_Sub := RegisterRoutine(H_Namespace, '', typeVARIANT, ccREGISTER,
                             nil);
{$ENDIF}
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'X');


{$IFDEF MSWINDOWS}
    H_Sub := RegisterRoutine(H_Namespace, 'GetObject', typeVARIANT, ccREGISTER,
                             @_GetObject);
{$ELSE}
    H_Sub := RegisterRoutine(H_Namespace, '', typeVARIANT, ccREGISTER,
                             nil);
{$ENDIF}
    RegisterParameter(H_Sub, typeSTRING, Unassigned, false, 'X');

{$ENDIF}
{$ENDIF}

{$IFNDEF TAB}
    H_Sub := RegisterRoutine(H_Namespace, 'Date', typeDOUBLE, ccREGISTER,
                             @_Date);
{$ENDIF}
    H_Sub := RegisterRoutine(H_Namespace, 'Now', typeDOUBLE, ccREGISTER,
                             @_Date);

    RegisterHeader(H_Namespace,
      'function StrToDate(const S: string): TDateTime;',
      @ StrToDate);

    H_Sub := RegisterRoutine(H_Namespace, 'DateSerial', typeDOUBLE, ccREGISTER,
                             @_DateSerial);
    RegisterParameter(H_Sub, typeWORD, Unassigned, false, 'D');
    RegisterParameter(H_Sub, typeWORD, Unassigned, false, 'M');
    RegisterParameter(H_Sub, typeWORD, Unassigned, false, 'Y');

    RegisterHeader(H_Namespace,
      'function TimeSerial(Hour, Min, Sec: Word; MSec: Word = 0): TDateTime;',
      @ _TimeSerial);

    H_Sub := RegisterRoutine(H_Namespace, 'Day', typeINTEGER, ccREGISTER,
                             @_Day);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    H_Sub := RegisterRoutine(H_Namespace, 'Month', typeINTEGER, ccREGISTER,
                             @_Month);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    RegisterHeader(H_Namespace,
      'function MonthName(const V: Variant; abbreviate: Boolean = false): String;',
       @_MonthName);

    RegisterHeader(H_Namespace,
      'function WeekDayName(const V: Variant; abbreviate: Boolean = false): String;',
       @_WeekDayName);

    H_Sub := RegisterRoutine(H_Namespace, 'WeekDay', typeINTEGER, ccREGISTER,
                             @_WeekDay);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

{$IFDEF MSWINDOWS}
    RegisterRoutine(H_Namespace, 'Timer', typeINTEGER, ccREGISTER,
                             @GetTickCount);
{$ELSE}
    RegisterRoutine(H_Namespace, '', typeINTEGER, ccREGISTER,
                             nil);
{$ENDIF}

    H_Sub := RegisterRoutine(H_Namespace, 'Year', typeINTEGER, ccREGISTER,
                             @_Year);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    H_Sub := RegisterRoutine(H_Namespace, 'Hour', typeINTEGER, ccREGISTER,
                             @_Hour);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    H_Sub := RegisterRoutine(H_Namespace, 'Minute', typeINTEGER, ccREGISTER,
                             @_Minute);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    H_Sub := RegisterRoutine(H_Namespace, 'Second', typeINTEGER, ccREGISTER,
                             @_Second);
    RegisterParameter(H_Sub, typeVARIANT, Unassigned, false, 'ADate');

    RegisterHeader(H_Namespace, 'function Filter(const Source: Variant;' +
                                'const Match: String;' +
                                'Include: Boolean = true): Variant;',
                                @ _Filter);

    RegisterHeader(H_Namespace, 'function Join(const Source: Variant;' +
                                'const Delimiter: String = '' ''): String;',
                                @ _Join);

    RegisterHeader(H_Namespace, 'function Split(const Expression: String;' +
                                'const Delimiter: String = '' '';' +
                                'Limit: Integer = -1): Variant;',
                                @ _Split);

    RegisterHeader(H_Namespace, 'function LBound(const V: Variant): Integer;',
                                @ _LBound);

    RegisterHeader(H_Namespace, 'function UBound(const V: Variant; D: Integer = 1): Integer;',
                                @ _UBound);

    RegisterHeader(H_Namespace, 'function UCase(const S: String): String;',
                                @ _UCase);
    RegisterHeader(H_Namespace, 'function LCase(const S: String): String;',
                                @ _LCase);

    RegisterConstant(H_Namespace, 'Nothing', typeVARIANT, Null);
    RegisterRoutine(H_Namespace, 'IsNull', typeVOID, ccSTDCALL, nil);
    RegisterRoutine(H_Namespace, 'IsNothing', typeVOID, ccSTDCALL, nil);

    RegisterHeader(H_Namespace,
      'function InStr(const S1, S2: string): Integer;',
      @ _InStr);

    RegisterHeader(H_Namespace,
      'function _InStrRev(const P0, P1: string; const P2: Variant = Undefined): Integer;',
      @ _InStrRev);

    RegisterHeader(H_Namespace,
      'function Len(const S: String): Integer;',
      @ _Len);

    RegisterHeader(H_Namespace,
      'function Replace(const S, OldPattern, NewPattern: string): String;',
      @ _Replace);

    RegisterHeader(H_Namespace,
      'function LTrim(const S: string): string;',
      @ TrimLeft);

    RegisterHeader(H_Namespace,
      'function RTrim(const S: string): string;',
      @ TrimRight);

    RegisterHeader(H_Namespace,
      'function Trim(const S: string): string;',
      @ Trim);

    RegisterHeader(H_Namespace,
     'function Substr(const S: String; Index, Length: Integer): String;',
      @ SCopy);

    RegisterHeader(H_Namespace,
      'function StrComp(const S1, S2: string): Boolean;',
      @ SameText);

    RegisterHeader(H_Namespace,
      'function StrReverse(const S: String): String;',
      @ _StrReverse);

    RegisterHeader(H_Namespace,
      'function Space(K: Integer): String;',
      @ PAXCOMP_SYS.Space);

    RegisterHeader(H_Namespace,
      'function CStr(const V: Variant): Variant;',
      @ _CStr);

{$IFNDEF TAB}
    RegisterHeader(H_Namespace,
      'function _RGB(red, green, blue: Integer): Integer;',
      @ _RGB);
{$ENDIF}

    RegisterHeader(H_Namespace,
      'function DateValue(const DateTime: TDateTime): string;',
      @ _DateToStr);

    RegisterHeader(H_Namespace,
      'function TimeValue(const DateTime: TDateTime): string;',
      @ _TimeToStr);

    RegisterHeader(H_Namespace,
      'function FormatCurrency(Value: Currency): string;',
      @ CurrToStr);

    RegisterHeader(H_Namespace,
      'function FormatDateTime(const DateTime: TDateTime): string;',
      @ DateTimeToStr);

    RegisterHeader(H_Namespace,
      'function FormatNumber(const V: Variant; const P2, P3: Variant = Undefined): Variant;',
      @ _FormatNumber);

    RegisterHeader(H_Namespace,
      'function FormatPercent(const V: Variant; const P2, P3: Variant = Undefined): Variant;',
      @ _FormatPercent);

    RegisterHeader(H_Namespace,
      'function Rnd(const V: Variant; const P2: Variant = Undefined): Variant;',
      @ _Rnd);

    RegisterHeader(H_Namespace,
      'function Round(const V: Variant; const P2: Variant = Undefined): Variant;',
      @ _Round);

    RegisterHeader(H_Namespace,
      'function VarType(const V: Variant): Word;',
      @ VarType);

    RegisterHeader(H_Namespace,
      'function TypeName(const V: Variant): String;',
      @ _TypeName);

    RegisterHeader(H_Namespace,
      'function IsNumeric(const V: Variant): Boolean;',
      @ _IsNumeric);

    RegisterHeader(H_Namespace,
      'function IsNull(const V: Variant): Boolean;',
      @ VarIsNull);

    RegisterHeader(H_Namespace,
      'function _Int(D: Double): Integer;',
      @ _Int);

    RegisterHeader(H_Namespace,
      'function _Fix(D: Double): Integer;',
      @ _Fix);

    RegisterHeader(H_Namespace,
      'function Chr(I: Integer): Char;',
      @ _Chr);

    RegisterHeader(H_Namespace,
      'function Left(const S: String; L: Integer): String;',
      @ _Left);

    RegisterHeader(H_Namespace,
      'function Right(const S: String; L: Integer): String;',
      @ _Right);

    RegisterHeader(H_Namespace,
      'function Sgn(V: Integer): Integer;',
      @ _Sgn);

    RegisterHeader(H_Namespace,
      'function _Mid(const P0: String; P1: Integer; const P2: Variant = Undefined): String;',
      @ _Mid);

    RegisterHeader(H_Namespace,
      'function CLng(const X: Variant): Variant;',
      @ _CLng);

    RegisterHeader(H_Namespace,
      'function CSng(const X: Variant): Variant;',
      @ _CSng);

    RegisterHeader(H_Namespace,
      'function _Hex(const X: Variant): String;',
      @ _Hex);

    RegisterHeader(H_Namespace,
      'function _IsEmpty(const X: Variant): Boolean;',
      @ _IsEmpty);

    RegisterHeader(H_Namespace,
      'function _IsArray(const X: Variant): Boolean;',
      @ _IsArray);

{$IFDEF VARIANTS}
    RegisterHeader(H_Namespace,
     'function DateAdd(const Interval: String; Number: Integer; const Date: Variant): TDateTime;',
      @ _DateAdd);

    RegisterHeader(H_Namespace,
      'function _DateDiff(const Interval: String; const V1, V2: Variant): Integer;',
      @ _DateDiff);

    RegisterHeader(H_Namespace,
       'function _DatePart(const Interval: String; const V: Variant): Integer;',
      @ _DatePart);
{$ENDIF}      

{$IFDEF TAB}
    RegisterHeader(H_Namespace,
      'function InList(const P1, P2: Variant = Undefined): Boolean;',
      @ _InList);

    RegisterHeader(H_Namespace,
      'function AtLeastOneInList(const P1, P2: Variant = Undefined): Boolean;',
      @ _AtLeastOneInList);
{$ENDIF}
  end;
end;

end.
