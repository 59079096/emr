////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_FRAMEWORK.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}

unit PAXCOMP_FRAMEWORK;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  Math,
{$IFNDEF FPC}
  Masks,
{$ENDIF}
{$IFNDEF PAXARM}
  PAXCOMP_MASKS, // (cross compiler/platform!)
{$ENDIF}
{$IFDEF UNIC}
  DateUtils,
  StrUtils,
{$IFNDEF PAXARM}
  AnsiStrings,
{$ENDIF}
  EncdDecd,
{$ENDIF}
  PAXCOMP_SYS,
  PAXCOMP_CONSTANTS,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_GC;

type
{$IFNDEF PAXARM}
  TAnsiStringDynArray = array of AnsiString;
{$ENDIF}
  TStringDynArray = array of UnicString;
  TVarType = Word;

  TFW_Object = class(TGC_Object)
  private
    procedure AddToGC;
  public
    prog: Pointer;
    function GetGC: TGC; override;
    function _ToString: String; virtual; abstract;
  end;

  TFW_Boolean = class(TFW_Object)
  private
    val: Boolean;
  public
    function _ToString: String; override;
    function _ToInt32: integer;
    function _ToISOString: String;
{$IFNDEF PAXARM}
    function _ToISOAnsiString: AnsiString;
    function _FromISOAnsiString(const Value: AnsiString): Boolean;
{$ENDIF}
    function _FromISOString(const Value: String): Boolean;
    function _Equals(const Value: Boolean): boolean;
  end;

  TFW_ByteBool = class(TFW_Object)
  private
    val: ByteBool;
  public
    function _ToString: String; override;
    function _ToInt32: integer;
    function _Equals(const Value: ByteBool): boolean;
  end;

  TFW_WordBool = class(TFW_Object)
  private
    val: WordBool;
  public
    function _ToString: String; override;
    function _ToInt32: integer;
    function _Equals(const Value: WordBool): boolean;
  end;

  TFW_LongBool = class(TFW_Object)
  private
    val: LongBool;
  public
    function _ToString: String; override;
    function _ToInt32: integer;
    function _Equals(const Value: LongBool): boolean;
  end;

  TFW_Byte = class(TFW_Object)
  private
    val: Byte;
  public
    function _ToString: String; override;
    function _Equals(const Value: Byte): boolean;
  end;

  TFW_SmallInt = class(TFW_Object)
  private
    val: SmallInt;
  public
    function _ToString: String; override;
    function _Equals(const Value: SmallInt): boolean;
    function _MinValue: SmallInt;
    function _MaxValue: SmallInt;
  end;

  TFW_ShortInt = class(TFW_Object)
  private
    val: ShortInt;
  public
    function _ToString: String; override;
    function _Equals(const Value: ShortInt): boolean;
    function _MinValue: ShortInt;
    function _MaxValue: ShortInt;
  end;

  TFW_Word = class(TFW_Object)
  private
    val: Word;
  public
    function _ToString: String; override;
    function _Equals(const Value: Word): boolean;
    function _MinValue: Word;
    function _MaxValue: Word;
  end;

  TFW_Cardinal = class(TFW_Object)
  private
    val: Cardinal;
  public
    function _ToString: String; override;
    function _Equals(const Value: Cardinal): boolean;
    function _MinValue: Cardinal;
    function _MaxValue: Cardinal;
  end;

  TFW_Double = class(TFW_Object)
  private
    val: Double;
  public
    function _ToString: String; override;
    function _ToStringFormat(const Format: String): String;
{$IFDEF UNIC}
    function _ToISOString: String;
{$IFNDEF PAXARM}
    function _ToISOAnsiString: AnsiString;
    function _FromISOAnsiString(const Value: AnsiString): double;
{$ENDIF}
    function _FromISOString(const Value: String): double;
    function _RoundTo(const Digit: integer): double;
{$ENDIF}
    function _Equals(const Value: Double): boolean;
    function _Round: Int64;
    function _Power(const Exponent: Extended): double;
    function _Trunc: Int64;
    function _Floor: Integer;
    function _Ceil: Integer;
    function _Min(const Value: Double): Double;
    function _Max(const Value: Double): Double;
  end;

  TFW_DateTime = class(TFW_Object)
  private
    val: TDateTime;
  public
    function _ToString: String; override;
    function _ToStringFormat(const Format: String): String;
    function _ToStringISO: String;
{$IFNDEF PAXARM}
    function _FromISOAnsiString(const Value: AnsiString): TDateTime;
{$IFDEF UNIC}
    function _ToAnsiStringISO: AnsiString;
{$ENDIF}
{$ENDIF}
    function _FromISOString(const Value: String): TDateTime;
    function _Equals(const Value: TDateTime): boolean;
    function _ToInt32: Integer;

    function _IsDate: boolean;
    function _IsDateTime: boolean;
    function _IsTime: boolean;

    function _Date: TDateTime;
    function _Time: TDateTime;
    function _Now: TDateTime;

    function _IsInLeapYear: boolean;
    function _DateOf: TDateTime;
    function _TimeOf: TDateTime;
    function _YearOf: Word;
    function _MonthOf: Word;
    function _DayOf: Word;
    function _HourOf: Word;
    function _MinuteOf: Word;
    function _SecondOf: Word;
    function _MilliSecondOf: Word;
    function _WeeksInYear: Word;
    function _DaysInYear: Word;
    function _Today: TDateTime;
    function _Yesterday: TDateTime;
    function _Tomorrow: TDateTime;
    function _YearSpan(const Value: TDateTime): Double;
    function _MonthSpan(const Value: TDateTime): Double;
    function _WeekSpan(const Value: TDateTime): Double;
    function _DaySpan(const Value: TDateTime): Double;
    function _HourSpan(const Value: TDateTime): Double;
    function _MinuteSpan(const Value: TDateTime): Double;
    function _SecondSpan(const Value: TDateTime): Double;
    function _MilliSecondSpan(const Value: TDateTime): Double;
    function _AddYears(const ANumberOfYears: Integer = 1): TDateTime;
    function _AddWeeks(const ANumberOfWeeks: Integer = 1): TDateTime;
    function _AddDays(const ANumberOfDays: Integer = 1): TDateTime;
    function _AddHours(const ANumberOfHours: Int64 = 1): TDateTime;
    function _AddMinutes(const ANumberOfMinutes: Int64 = 1): TDateTime;
    function _AddSeconds(const ANumberOfSeconds: Int64 = 1): TDateTime;
    function _AddMilliSeconds(const ANumberOfMilliSeconds: Int64 = 1): TDateTime;
{$IFDEF UNIC}
    function _EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
{$ENDIF}
    function _EncodeDate(const AYear, AMonth, ADay: Word): TDateTime;
    function _EncodeTime(const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
    function _Min(const Value: TDateTime): TDateTime;
    function _Max(const Value: TDateTime): TDateTime;
    function _MinValue: TDateTime;
    function _MaxValue: TDateTime;
  end;

  TFW_Single = class(TFW_Object)
  private
    val: Single;
  public
    function _ToString: String; override;
    function _Equals(const Value: Single): boolean;
    function _Min(const Value: Single): Single;
    function _Max(const Value: Single): Single;
  end;

  TFW_Extended = class(TFW_Object)
  private
    val: Extended;
  public
    function _ToString: String; override;
    function _ToStringFormat(const Format: String): String;
{$IFDEF UNIC}
    function _ToISOString: String;
{$IFNDEF PAXARM}
    function _ToISOAnsiString: AnsiString;
    function _FromISOAnsiString(const Value: AnsiString): Extended;
{$ENDIF}
    function _RoundTo(const Digit: integer): Extended;
    function _FromISOString(const Value: String): Extended;
{$ENDIF}
    function _Equals(const Value: Extended): boolean;
    function _Round: Int64;
    function _Power(const Exponent: Extended): Extended;
    function _Trunc: Int64;
    function _Floor: Integer;
    function _Ceil: Integer;
    function _Min(const Value: Extended): Extended;
    function _Max(const Value: Extended): Extended;
  end;

  TFW_Currency = class(TFW_Object)
  private
    val: Currency;
  public
    function _ToString: String; override;
    function _ToStringFormat(const Format: String): String;
{$IFDEF UNIC}
    function _ToISOString: String;
    function _FromISOString(const Value: String): Currency;
{$IFNDEF PAXARM}
    function _ToISOAnsiString: AnsiString;
    function _FromISOAnsiString(const Value: AnsiString): Currency;
{$ENDIF}
    function _RoundTo(const Digit: integer): Currency;
{$ENDIF}
    function _Equals(const Value: Currency): boolean;
    function _Round: Int64;
    function _Power(const Exponent: Extended): Currency;
    function _Trunc: Int64;
    function _Floor: Integer;
    function _Ceil: Integer;
    function _Min(const Value: Currency): Currency;
    function _Max(const Value: Currency): Currency;
  end;

{$IFNDEF PAXARM}
  TFW_AnsiChar = class(TFW_Object)
  private
    val: AnsiChar;
  public
    function _ToString: String; override;
    function _Equals(const Value: AnsiChar): boolean;
  end;
{$ENDIF}

  TFW_WideChar = class(TFW_Object)
  private
    val: WideChar;
  public
    function _ToString: String; override;
    function _Equals(const Value: WideChar): boolean;
  end;

  TFW_Integer = class(TFW_Object)
  private
    val: Integer;
  public
    function _ToString: String; override;
    function _ToDate: TDateTime;
    function _ToHex(Digits: Integer = 8): String;
    function _FromHex(const Value: String): Integer;
{$IFNDEF PAXARM}
    function _FromHexAnsi(const Value: AnsiString): Integer;
{$ENDIF}
    function _Equals(const Value: Integer): boolean;
    function _Min(const Value: Integer): Integer;
    function _Max(const Value: Integer): Integer;
    function _MinValue: Integer;
    function _MaxValue: Integer;
  end;

  TFW_Int64 = class(TFW_Object)   //-- NOT WORKING
  private
    val: Int64;
  public
    function _ToString: String; override;
    function _ToDate: TDateTime;
    function _ToHex(Digits: Integer = 8): String;
    function _FromHex(const Value: String): Int64;
{$IFNDEF PAXARM}
    function _FromHexAnsi(const Value: AnsiString): Int64;
{$ENDIF}
    function _Equals(const Value: Int64): boolean;
    function _Min(const Value: Int64): Int64;
    function _Max(const Value: Int64): Int64;
    function _MinValue: Int64;
    function _MaxValue: Int64;
  end;

  TFW_Variant = class(TFW_Object)   //-- NOT WORKING
  private
    val: Variant;
  public
    function _ToString: String; override;
    function _ToDate: TDateTime;
    function _ToDateTime: TDateTime;
    function _ToTime: TDateTime;
    function _Equals(const Value: Variant): boolean;
    function _IsType: TVarType;
    function _IsNull: boolean;
    function _IsEmpty: boolean;
{$IFDEF VARIANTS}
    function _IsEmptyParam: boolean;
    function _IsError: boolean;
    function _IsArray: boolean;
    function _IsFilled: boolean;
{$ENDIF}
    function _Null: Variant;
    function _Unassigned: Variant;
    function _Clear: Variant;
    function _DimCount: Integer;
    function _LowBound(const Dim: integer): integer;
    function _HighBound(const Dim: integer): integer;
  end;
{$IFNDEF PAXARM}
  TFW_AnsiString = class(TFW_Object)
  private
    val: AnsiString;
  public
    function _ToString: String; override;
    function _Replace(const OldPattern: AnsiString; const NewPattern: AnsiString): AnsiString;
    function _Equals(const Value: AnsiString): boolean;
    function _Length: integer;
{$IFDEF UNIC}
    function _ToDate: TDateTime;
    function _ToTime: TDateTime;
    function _ToDateTime: TDateTime;
    function _ToCurrency: Currency;
    function _ToExtended: Extended;
    function _ToDouble: Double;
    function _ToCardinal: Cardinal;
    function _ToShortInt: ShortInt;
    function _ToSmallInt: SmallInt;
    function _ToSingle: Single;
    function _ToWord: Word;
    function _ToInt32: Integer;
    function _ToInt64: Int64;
    function _ToBoolean: Boolean;
    function _ToByteBool: ByteBool;
    function _ToLongBool: LongBool;
    function _ToWordBool: WordBool;
    function _ToUTF8: UTF8String;
    function _FromUTF8(const Value: UTF8String): AnsiString;
    function _ToUnicode: UnicodeString;
    function _FromUnicode(const Value: UnicodeString): AnsiString;

    function _ToBase64: AnsiString;
    function _FromBase64(const Value: AnsiString): AnsiString;

    function _ISOToBoolean: Boolean;
    function _ISOToDate: TDateTime;
    function _ISOToTime: TDateTime;
    function _ISOToDateTime: TDateTime;
    function _ISOToCurrency: Currency;
    function _ISOToExtended: Extended;
    function _ISOToDouble: Double;

    function _Copy(Index: integer; Count: integer): AnsiString;
    function _Delete(Index: integer; Count: integer): AnsiString;
    function _Trim: AnsiString;
    function _TrimLeft: AnsiString;
    function _TrimRight: AnsiString;
    function _Contains(const Value: AnsiString): boolean;
    function _Pos(const Value: AnsiString): integer;
    function _IndexOf(const Value: AnsiString; const StartIndex: integer = 1): integer;
    function _Quoted(const Quote: AnsiChar = '"'): AnsiString;
    function _Dequoted(const Quote: AnsiChar = '"'): AnsiString;
    function _SplitEx(const Seperator: AnsiChar; const Quotes: Boolean; const Quote: AnsiChar = '"'; const TrimText: Boolean = false): TAnsiStringDynArray;
    function _ToUpper: AnsiString;
    function _ToLower: AnsiString;
    function _Split(const Seperator: AnsiString): TAnsiStringDynArray;
    function _Join(const Value: TAnsiStringDynArray; const Seperator: AnsiString): AnsiString;
    function _Insert(const Value: AnsiString; Index: integer): AnsiString;
    function _IsNumeric: boolean;
    function _IsAlpha: boolean;
    function _IsAlphaNumeric: boolean;
    function _Match(const Mask: String): boolean;
    function _EndsWith(const Value: AnsiString): boolean;
    function _StartsWith(const Value: AnsiString): boolean;
    function _Reverse: AnsiString;
    function _Left(const Length: Integer): AnsiString;
    function _Right(const Length: Integer): AnsiString;
    function _AppendA(const Value: AnsiString): AnsiString;
    function _AppendW(const Value: String): AnsiString;
    function _AppendLineA(const Value: AnsiString): AnsiString;
    function _AppendLineW(const Value: String): AnsiString;
    function _Lastchar: AnsiChar;
    function _LastDelimiter(const Delimiters: AnsiString = ';'): Integer;
    function _FindDelimiter(const Delimiters: AnsiString = ';'; const StartIdx: integer = 1): Integer;
    function _StringOfChar(const Ch: AnsiChar; const Count: integer): AnsiString;
{$ENDIF}
  end;
{$ENDIF} // PAXARM

  TFW_UnicString = class(TFW_Object)
  private
    val: UnicString;
  public
    constructor Create;
    function _ToString: String; override;
    function _Replace(const OldPattern: String; const NewPattern: String): String;
    function _Equals(const Value: String): boolean;
    function _Length: integer;
{$IFDEF UNIC}
    function _ToDate: TDateTime;
    function _ToTime: TDateTime;
    function _ToDateTime: TDateTime;
    function _ToCurrency: Currency;
    function _ToExtended: Extended;
    function _ToDouble: Double;
    function _ToCardinal: Cardinal;
    function _ToShortInt: ShortInt;
    function _ToSmallInt: SmallInt;
    function _ToSingle: Single;
    function _ToWord: Word;
    function _ToInt32: Integer;
    function _ToInt64: Int64;
    function _ToBoolean: Boolean;
    function _ToByteBool: ByteBool;
    function _ToLongBool: LongBool;
    function _ToWordBool: WordBool;
{$IFNDEF PAXARM}
    function _ToUTF8: UTF8String;
    function _FromUTF8(const Value: UTF8String): String;
    function _ToAnsi: AnsiString;
    function _FromAnsi(const Value: AnsiString): String;
{$ENDIF}
    function _ToBase64: String;
    function _FromBase64(const Value: String): String;

    function _ISOToBoolean: Boolean;
    function _ISOToDate: TDateTime;
    function _ISOToTime: TDateTime;
    function _ISOToDateTime: TDateTime;
    function _ISOToCurrency: Currency;
    function _ISOToExtended: Extended;
    function _ISOToDouble: Double;

    function _Copy(Index: integer; Count: integer): String;
    function _Delete(Index: integer; Count: integer): String;
    function _Trim: String;
    function _TrimLeft: String;
    function _TrimRight: String;
    function _Contains(const Value: String): boolean;
    function _Pos(const Value: String): integer;
    function _IndexOf(const Value: String; const StartIndex: integer = 1): integer;
    function _Quoted(const Quote: WideChar = '"'): String;
    function _Dequoted(const Quote: WideChar = '"'): String;
    function _ToUpper: String;
    function _ToLower: String;
    function _Split(const Seperator: String): TStringDynArray;
    function _SplitEx(const Seperator: WideChar; const Quotes: Boolean; const Quote: WideChar = '"'; const TrimText: Boolean = false): TStringDynArray;
    function _Join(const Value: TStringDynArray; const Seperator: String): String;
    function _Insert(const Value: String; Index: integer): String;
    function _IsNumeric: boolean;
    function _IsAlpha: boolean;
    function _IsAlphaNumeric: boolean;
    function _Match(const Mask: String): boolean;
    function _EndsWith(const Value: String): boolean;
    function _StartsWith(const Value: String): boolean;
    function _Reverse: String;
    function _Left(const Length: Integer): String;
    function _Right(const Length: Integer): String;
{$IFNDEF PAXARM}
    function _AppendLineA(const Value: AnsiString): String;
    function _AppendA(const Value: AnsiString): String;
{$ENDIF}
    function _AppendW(const Value: String): String;
    function _AppendLineW(const Value: String): String;
    function _Lastchar: WideChar;
    function _LastDelimiter(const Delimiters: String = ';'): Integer;
    function _FindDelimiter(const Delimiters: String = ';'; const StartIdx: integer = 1): Integer;
    function _StringOfChar(const Ch: WideChar; const Count: integer): String;
{$ENDIF}
  end;

  TFW_Array = class(TFW_Object)
  private
    NBounds: Integer;
    ElTypeId: Integer;
    ElFinalTypeID: Integer;
    ElSize: Integer;
    function GetBound(I: Integer): Integer;
    function GetLength: Integer;
  public
    P: Pointer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetLength(const Bounds: array of Integer);
    function AddressOfElement(const Indexes: array of Integer): Pointer;
    function Get(const Indexes: array of Integer): Variant;
    procedure Put(const Indexes: array of Integer; const Value: Variant);
    property Length: Integer read GetLength;
    property Bound[I: Integer]: Integer read GetBound;
  end;

procedure _InitFWArray(P: Pointer;
                       A: TFW_Array;
                       NBounds: Integer;
                       ElFinalTypeId: Integer;
                       ElTypeId: Integer;
                       ElSize: Integer;
                       DecRefCount: Integer); pascal;

procedure Register_Framework(st: TBaseSymbolTable);

implementation

uses
  PAXCOMP_BASERUNNER,
  PAXCOMP_STDLIB,
  PAXCOMP_JavaScript;

procedure _ToFWObject(Prog: TBaseRunner;
                      Address: Pointer;
                      Kind: Integer;
                      FinTypeId: Integer;
                      TypeId: Integer;
                      var result: TFW_Object); stdcall; forward;

procedure _Boxing(Prog: TBaseRunner;
                  Address: Pointer; // value
                  Kind: Integer;    // kind
                  FinTypeId: Integer; // of value
                  TypeId: Integer;    // of value
                  var result: TFW_Object); stdcall;
var
  r: TFW_Object;
  p: PGC_Object;
begin
  p := @result;
  r := nil;
  _ToFWObject(Prog, Address, Kind, FinTypeId, TypeId, r);
  GC_Assign(p, r);
end;

procedure _ToFWObject(Prog: TBaseRunner;
                      Address: Pointer;
                      Kind: Integer;
                      FinTypeId: Integer;
                      TypeId: Integer;
                      var result: TFW_Object); stdcall;
begin
  case Kind of
    KindCONST:
      case FinTypeId of
        typeBOOLEAN:
        begin
          result := TFW_Boolean.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Boolean(result).val := Boolean(Address);
        end;
        typeBYTEBOOL:
        begin
          result := TFW_ByteBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_ByteBool(result).val := ByteBool(Address);
        end;
        typeWORDBOOL:
        begin
          result := TFW_WordBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_WordBool(result).val := WordBool(Address);
        end;
        typeLONGBOOL:
        begin
          result := TFW_LongBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_LongBool(result).val := LongBool(Address);
        end;
        typeBYTE:
        begin
          result := TFW_Byte.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Byte(result).val := Byte(Address);
        end;
        typeSMALLINT:
        begin
          result := TFW_SmallInt.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_SmallInt(result).val := SmallInt(Address);
        end;
        typeSHORTINT:
        begin
          result := TFW_ShortInt.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_ShortInt(result).val := ShortInt(Address);
        end;
        typeWORD:
        begin
          result := TFW_Word.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Word(result).val := Word(Address);
        end;
        typeCARDINAL:
        begin
          result := TFW_Cardinal.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Cardinal(result).val := Cardinal(Address);
        end;
        typeDOUBLE:
        begin
          if TypeId = Id_TDateTime then
          begin
            result := TFW_DateTime.Create;
            result.prog := Prog;
            result.AddToGC;
            TFW_DateTime(result).val := Double(Address^);
          end
          else
          begin
            result := TFW_Double.Create;
            result.prog := Prog;
            result.AddToGC;
            TFW_Double(result).val := Double(Address^);
          end;
        end;
        typeSINGLE:
        begin
          result := TFW_Single.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Single(result).val := Single(Address^);
        end;
        typeEXTENDED:
        begin
          result := TFW_Extended.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Extended(result).val := Extended(Address^);
        end;
        typeCURRENCY:
        begin
          result := TFW_Currency.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Currency(result).val := Currency(Address^);
        end;
{$IFNDEF PAXARM}
        typeANSICHAR:
        begin
          result := TFW_AnsiChar.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_AnsiChar(result).val := AnsiChar(Address);
        end;
{$ENDIF}
        typeWIDECHAR:
        begin
          result := TFW_WideChar.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_WideChar(result).val := WideChar(Address);
        end;
        typeINT64:
        begin
          result := TFW_Int64.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Int64(result).val := Integer(Address);
        end;
        typeINTEGER:
        begin
          result := TFW_Integer.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Integer(result).val := Integer(Address);
        end;
{$IFNDEF PAXARM}
        typePANSICHAR:
        begin
          result := TFW_AnsiString.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_AnsiString(result).val := AnsiString(PAnsiChar(Address));
        end;
{$ENDIF}
        typePWIDECHAR:
        begin
          result := TFW_UnicString.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_UnicString(result).val := UnicString(PWideChar(Address));
        end;
        typeVARIANT:
        begin
          result := TFW_Variant.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Variant(result).val := Variant(PVariant(Address)^);
        end;
        else
          Prog.RaiseError(errInternalError, []);
      end; // KindCONST
    KindVAR:
      case FinTypeId of
        typeBOOLEAN:
        begin
          result := TFW_Boolean.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Boolean(result).val := Boolean(Address^);
        end;
        typeBYTEBOOL:
        begin
          result := TFW_ByteBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_ByteBool(result).val := ByteBool(Address^);
        end;
        typeWORDBOOL:
        begin
          result := TFW_WordBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_WordBool(result).val := WordBool(Address^);
        end;
        typeLONGBOOL:
        begin
          result := TFW_LongBool.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_LongBool(result).val := LongBool(Address^);
        end;
        typeBYTE:
        begin
          result := TFW_Byte.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Byte(result).val := Byte(Address^);
        end;
        typeSMALLINT:
        begin
          result := TFW_SmallInt.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_SmallInt(result).val := SmallInt(Address^);
        end;
        typeSHORTINT:
        begin
          result := TFW_ShortInt.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_ShortInt(result).val := ShortInt(Address^);
        end;
        typeWORD:
        begin
          result := TFW_Word.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Word(result).val := Word(Address^);
        end;
        typeCARDINAL:
        begin
          result := TFW_Cardinal.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Cardinal(result).val := Cardinal(Address^);
        end;
        typeDOUBLE:
        begin
          if TypeId = Id_TDateTime then
          begin
            result := TFW_DateTime.Create;
            result.prog := Prog;
            result.AddToGC;
            TFW_DateTime(result).val := Double(Address^);
          end
          else
          begin
            result := TFW_Double.Create;
            result.prog := Prog;
            result.AddToGC;
            TFW_Double(result).val := Double(Address^);
          end;
        end;
        typeSINGLE:
        begin
          result := TFW_Single.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Single(result).val := Single(Address^);
        end;
        typeEXTENDED:
        begin
          result := TFW_Extended.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Extended(result).val := Extended(Address^);
        end;
        typeCURRENCY:
        begin
          result := TFW_Currency.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Currency(result).val := Currency(Address^);
        end;
{$IFNDEF PAXARM}
        typeANSICHAR:
        begin
          result := TFW_AnsiChar.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_AnsiChar(result).val := AnsiChar(Address^);
        end;
{$ENDIF}
        typeWIDECHAR:
        begin
          result := TFW_WideChar.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_WideChar(result).val := WideChar(Address^);
        end;
        typeINTEGER:
        begin
          result := TFW_Integer.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Integer(result).val := Integer(Address^);
        end;
        typeINT64:
        begin
          result := TFW_Int64.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Int64(result).val := Int64(Address^);
        end;
{$IFNDEF PAXARM}
        typeANSISTRING:
        begin
          result := TFW_AnsiString.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_AnsiString(result).val := AnsiString(Address^);
        end;
{$ENDIF}
        typeUNICSTRING:
        begin
          result := TFW_UnicString.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_UnicString(result).val := UnicString(Address^);
        end;
        typeVariant:
        begin
          result := TFW_Variant.Create;
          result.prog := Prog;
          result.AddToGC;
          TFW_Variant(result).val := Variant(Address^);
        end;
        else
          Prog.RaiseError(errInternalError, []);
      end;
    else
      Prog.RaiseError(errInternalError, []);
  end;
end;

// TFW_Object ------------------------------------------------------------------

procedure TFW_Object.AddToGC;
begin
  if prog = nil then
    raise Exception.Create(errInternalError);
  TBaseRunner(prog).RootGC.AddObject(Self);
end;

function TFW_Object.GetGC: TGC;
begin
  if prog = nil then
    raise Exception.Create(errInternalError);
  result := TBaseRunner(prog).RootGC;
end;

// TFW_Boolean -----------------------------------------------------------------

function TFW_Boolean._ToString: String;
begin
  if val then
    result := 'true'
  else
    result := 'false';
end;

function TFW_Boolean._toInt32: integer;
begin
  if val then
    result := 1
  else
    result := 0;
end;

function TFW_Boolean._ToISOString: String;
begin
  if val then
    result := '1'
  else
    result := '0';
end;

{$IFNDEF PAXARM}
function TFW_Boolean._ToISOAnsiString: AnsiString;
begin
  if val then
    result := '1'
  else
    result := '0';
end;

function TFW_Boolean._FromISOAnsiString(const Value: AnsiString): Boolean;
begin
  result := false;
  if Value = '1' then
     result := true;
end;

{$ENDIF}

function TFW_Boolean._FromISOString(const Value: String): Boolean;
begin
  result := false;
  if Value = '1' then
     result := true;
end;

function TFW_Boolean._Equals(const Value: Boolean): boolean;
begin
  result := val = Value;
end;

// TFW_ByteBool ----------------------------------------------------------------

function TFW_ByteBool._ToString: String;
begin
  if val then
    result := 'true'
  else
    result := 'false';
end;

function TFW_ByteBool._toInt32: integer;
begin
  if val then
    result := 1
  else
    result := 0;
end;

function TFW_ByteBool._Equals(const Value: ByteBool): boolean;
begin
  result := val = Value;
end;

// TFW_WordBool ----------------------------------------------------------------

function TFW_WordBool._ToString: String;
begin
  if val then
    result := 'true'
  else
    result := 'false';
end;

function TFW_WordBool._toInt32: integer;
begin
  if val then
    result := 1
  else
    result := 0;
end;

function TFW_WordBool._Equals(const Value: WordBool): boolean;
begin
  result := val = Value;
end;

// TFW_LongBool ----------------------------------------------------------------

function TFW_LongBool._ToString: String;
begin
  if val then
    result := 'true'
  else
    result := 'false';
end;

function TFW_LongBool._toInt32: integer;
begin
  if val then
    result := 1
  else
    result := 0;
end;

function TFW_LongBool._Equals(const Value: LongBool): boolean;
begin
  result := val = Value;
end;

// TFW_Byte --------------------------------------------------------------------

function TFW_Byte._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_Byte._Equals(const Value: Byte): boolean;
begin
  result := val = Value;
end;

// TFW_SmallInt ----------------------------------------------------------------

function TFW_SmallInt._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_SmallInt._Equals(const Value: SmallInt): boolean;
begin
  result := val = Value;
end;

function TFW_SmallInt._MinValue: SmallInt;
begin
  result := -32768;
end;

function TFW_SmallInt._MaxValue: SmallInt;
begin
  result := 32767;
end;

// TFW_ShortInt ----------------------------------------------------------------

function TFW_ShortInt._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_ShortInt._Equals(const Value: ShortInt): boolean;
begin
  result := val = Value;
end;

function TFW_ShortInt._MinValue: ShortInt;
begin
  result := -127;
end;

function TFW_ShortInt._MaxValue: ShortInt;
begin
  result := 127;
end;

// TFW_Word --------------------------------------------------------------------

function TFW_Word._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_Word._Equals(const Value: Word): boolean;
begin
  result := val = Value;
end;

function TFW_Word._MinValue: Word;
begin
  result := 0;
end;

function TFW_Word._MaxValue: Word;
begin
  result := 65535;
end;

// TFW_Cardinal ----------------------------------------------------------------

function TFW_Cardinal._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_Cardinal._Equals(const Value: Cardinal): boolean;
begin
  result := val = Value;
end;

function TFW_Cardinal._MinValue: Cardinal;
begin
  result := 0;
end;

function TFW_Cardinal._MaxValue: Cardinal;
begin
  result := 4294967295;
end;

// TFW_DateTime ------------------------------------------------------------------

function TFW_DateTime._ToString: String;
begin
  result := DateTimeToStr(val);
end;

function TFW_DateTime._ToStringFormat(const Format: String): String;
begin
  result := FormatDateTime(Format, val);
end;

function TFW_DateTime._ToStringISO: String;
begin
  if _IsDateTime then
  begin
    result := FormatDateTime('yyyymmddhhnnsszzz', val);
  end else
  if _IsDate then
  begin
    result := FormatDateTime('yyyymmdd', val);
  end else
  if _IsTime then
  begin
    result := FormatDateTime('hhnnsszzz', val);
  end;
end;

{$IFDEF UNIC}

{$IFNDEF PAXARM}
function TFW_DateTime._ToAnsiStringISO: AnsiString;
begin
  if _IsDateTime then
  begin
    result := AnsiString(UTF8ToAnsi(UTF8Encode(FormatDateTime('yyyymmddhhnnsszzz', val))));
  end else
  if _IsDate then
  begin
    result := AnsiString(UTF8ToAnsi(UTF8Encode(FormatDateTime('yyyymmdd', val))));
  end else
  if _IsTime then
  begin
    result := AnsiString(UTF8ToAnsi(UTF8Encode(FormatDateTime('hhnnsszzz', val))));
  end;
end;
{$ENDIF}
{$ENDIF}

function TFW_DateTime._ToInt32: Integer;
var
  AYear  : Word;
  AMonth : Word;
  aDay   : Word;
begin
  DecodeDate(val, AYear, AMonth, aDay);
  result := AYear * 10000 + AMonth * 100 + aDay;
end;

function TFW_DateTime._FromISOString(const Value: String): TDateTime;
var
  aYear  : Word;
  aMonth : Word;
  aDay   : Word;
  aHour  : Word;
  aMin   : Word;
  aSec   : Word;
  aMSec  : Word;
  aLen   : Integer;
begin
  result := 0;
  aLen   := Length(Value);
  if aLen >= 8 then
  begin
     aYear  := strtoint(copy(Value, 1, 4));
     aMonth := strtoint(copy(Value, 5, 2));
     aDay   := strtoint(copy(Value, 7, 2));
     aHour  := 0;
     aMin   := 0;
     aSec   := 0;
     aMSec  := 0;
     if aLen > 9 then
     begin
        try
          aHour := strtoint(copy(Value, 9, 2));
          aMin  := strtoint(copy(Value, 11, 2));
          aSec  := strtoint(copy(Value, 13, 2));
          aMSec := strtoint(copy(Value, 15, 2));
        except
        end;
     end;

     result := EncodeDate(aYear, aMonth, aDay) +
               EncodeTime(aHour, aMin, aSec, aMSec);
  end;
end;

{$IFNDEF PAXARM}
function TFW_DateTime._FromISOAnsiString(const Value: AnsiString): TDateTime;
var
  aYear  : Word;
  aMonth : Word;
  aDay   : Word;
  aHour  : Word;
  aMin   : Word;
  aSec   : Word;
  aMSec  : Word;
  aLen   : Integer;
begin
  result := 0;
  aLen   := Length(Value);
  if aLen >= 8 then
  begin
     aYear  := strtoint(copy(String(Value), 1, 4));
     aMonth := strtoint(copy(String(Value), 5, 2));
     aDay   := strtoint(copy(String(Value), 7, 2));
     aHour  := 0;
     aMin   := 0;
     aSec   := 0;
     aMSec  := 0;
     if aLen > 9 then
     begin
        try
          aHour := strtoint(copy(String(Value), 9, 2));
          aMin  := strtoint(copy(String(Value), 11, 2));
          aSec  := strtoint(copy(String(Value), 13, 2));
          aMSec := strtoint(copy(String(Value), 15, 2));
        except
        end;
     end;

     result := EncodeDate(aYear, aMonth, aDay) +
               EncodeTime(aHour, aMin, aSec, aMSec);
  end;
end;
{$ENDIF}

function TFW_DateTime._IsDate: boolean;
var
  aYear        : Word;
  aMonth       : Word;
  aDay         : Word;
  aHour        : Word;
  aMinute      : Word;
  aSecond      : Word;
  aMilliSecond : Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  DecodeTime(val, aHour, aMinute, aSecond, aMilliSecond);
  result := (
              (aHour = 0) and
              (aMinute = 0) and
              (aSecond = 0) and
              (aMilliSecond = 0)
            ) and
            (
              (aYear <> 0) or
              (aMonth <> 0) or
              (aDay <> 0)
            );
end;

function TFW_DateTime._IsDateTime: boolean;
var
  aYear        : Word;
  aMonth       : Word;
  aDay         : Word;
  aHour        : Word;
  aMinute      : Word;
  aSecond      : Word;
  aMilliSecond : Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  DecodeTime(val, aHour, aMinute, aSecond, aMilliSecond);
  result := (
              (aHour <> 0) or
              (aMinute <> 0) or
              (aSecond <> 0) or
              (aMilliSecond <> 0)
            ) and
            (
              (aYear <> 0) or
              (aMonth <> 0) or
              (aDay <> 0)
            );
end;

function TFW_DateTime._IsTime: boolean;
var
  aYear        : Word;
  aMonth       : Word;
  aDay         : Word;
  aHour        : Word;
  aMinute      : Word;
  aSecond      : Word;
  aMilliSecond : Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  DecodeTime(val, aHour, aMinute, aSecond, aMilliSecond);
  result := (
              (aHour <> 0) or
              (aMinute <> 0) or
              (aSecond <> 0) or
              (aMilliSecond <> 0)
            ) and
            (
              (aYear = 0) and
              (aMonth = 0) and
              (aDay = 0)
            );
end;

function TFW_DateTime._Date: TDateTime;
begin
  result := Date;
end;

function TFW_DateTime._Time: TDateTime;
begin
  result := Time;
end;

function TFW_DateTime._Now: TDateTime;
begin
  result := Now;
end;

function TFW_DateTime._IsInLeapYear: boolean;
var
  aYear, aMonth, aDay: Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  Result := IsLeapYear(aYear);
end;

function TFW_DateTime._DateOf: TDateTime;
begin
  Result := Trunc(val);
end;

function TFW_DateTime._TimeOf: TDateTime;
begin
  Result := Frac(val);
end;

function TFW_DateTime._YearOf: Word;
var
  aMonth, aDay: Word;
begin
  DecodeDate(val, Result, aMonth, aDay);
end;

function TFW_DateTime._MonthOf: Word;
var
  aYear, aDay: Word;
begin
  DecodeDate(val, aYear, result, aDay);
end;

function TFW_DateTime._DayOf: Word;
var
  aYear, aMonth: Word;
begin
  DecodeDate(val, aYear, aMonth, result);
end;

function TFW_DateTime._HourOf: Word;
var
  aMinute      : Word;
  aSecond      : Word;
  aMilliSecond : Word;
begin
  DecodeTime(val, result, aMinute, aSecond, aMilliSecond);
end;

function TFW_DateTime._MinuteOf: Word;
var
  aHour        : Word;
  aSecond      : Word;
  aMilliSecond : Word;
begin
  DecodeTime(val, aHour, result, aSecond, aMilliSecond);
end;

function TFW_DateTime._SecondOf: Word;
var
  aHour        : Word;
  aMinute      : Word;
  aMilliSecond : Word;
begin
  DecodeTime(val, aHour, aMinute, result, aMilliSecond);
end;

function TFW_DateTime._MilliSecondOf: Word;
var
  aHour        : Word;
  aMinute      : Word;
  aSecond      : Word;
begin
  DecodeTime(val, aHour, aMinute, aSecond, result);
end;

const
  DayMonday = 1;
  DayTuesday = 2;
  DayWednesday = 3;
  DayThursday = 4;
  DayFriday = 5;
  DaySaturday = 6;
  DaySunday = 7;
  ApproxDaysPerMonth: Double = 30.4375;
  ApproxDaysPerYear: Double  = 365.25;
  DaysPerWeek = 7;
  WeeksPerFortnight = 2;
  MonthsPerYear = 12;
  YearsPerDecade = 10;
  YearsPerCentury = 100;
  YearsPerMillennium = 1000;
  HoursPerDay = 24;
  MinsPerHour = 60;
  MinsPerDay = 24 * 60;
  MSecsPerSec = 1000;

function DayOfTheWeek(const AValue: TDateTime): Word;
begin
  Result := (DateTimeToTimeStamp(AValue).Date - 1) mod 7 + 1;
end;

function WeeksInAYear(const AYear: Word): Word;
var
  LDayOfWeek: Word;
begin
  Result := 52;
  LDayOfWeek := DayOfTheWeek(EncodeDate(AYear, 1, 1));
  if (LDayOfWeek = DayThursday) or
     ((LDayOfWeek = DayWednesday) and IsLeapYear(AYear)) then
    Inc(Result);
end;

function TFW_DateTime._WeeksInYear: Word;
var
  aYear, aMonth, aDay: Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  Result := WeeksInAYear(aYear);
end;

function TFW_DateTime._DaysInYear: Word;
var
  aYear, aMonth, aDay: Word;
begin
  DecodeDate(val, aYear, aMonth, aDay);
  if IsLeapYear(AYear) then
    result := 366
  else
    result := 365;
end;

function TFW_DateTime._Today: TDateTime;
begin
  Result := Date;
end;

function TFW_DateTime._Yesterday: TDateTime;
begin
  Result := Date - 1;
end;

function TFW_DateTime._Tomorrow: TDateTime;
begin
  Result := Date + 1;
end;

function DaySpan(const ANow, AThen: TDateTime): Double; forward;
function IncDay(const AValue: TDateTime;
  const ANumberOfDays: Integer = 1): TDateTime; forward;
function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime; forward;
function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64 = 1): TDateTime; forward;
function IncSecond(const AValue: TDateTime;
  const ANumberOfSeconds: Int64 = 1): TDateTime; forward;
function IncMilliSecond(const AValue: TDateTime;
  const ANumberOfMilliSeconds: Int64 = 1): TDateTime; forward;

function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
begin
  if ANow < AThen then
    Result := AThen - ANow
  else
    Result := ANow - AThen;
end;

function YearSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := DaySpan(ANow, AThen) / ApproxDaysPerYear;
end;

function MonthSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := DaySpan(ANow, AThen) / ApproxDaysPerMonth;
end;

function WeekSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := DaySpan(ANow, AThen) / DaysPerWeek;
end;

function DaySpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SpanOfNowAndThen(ANow, AThen);
end;

function HourSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := HoursPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function MinuteSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := MinsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function SecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SecsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function MilliSecondSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := MSecsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function IncYear(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
begin
  Result := IncMonth(AValue, ANumberOfYears * MonthsPerYear);
end;

function IncWeek(const AValue: TDateTime;
  const ANumberOfWeeks: Integer): TDateTime;
begin
  Result := IncDay(AValue, ANumberOfWeeks * DaysPerWeek);
end;

function IncDay(const AValue: TDateTime;
  const ANumberOfDays: Integer = 1): TDateTime;
begin
  Result := IncHour(AValue, ANumberOfDays * HoursPerDay);
end;

function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64 = 1): TDateTime;
begin
  Result := IncMinute(AValue, ANumberOfHours * MinsPerHour);
end;

function IncMinute(const AValue: TDateTime;
  const ANumberOfMinutes: Int64 = 1): TDateTime;
begin
  Result := IncSecond(AValue, ANumberOfMinutes * MinsPerHour);
end;

function IncSecond(const AValue: TDateTime;
  const ANumberOfSeconds: Int64 = 1): TDateTime;
begin
  Result := IncMilliSecond(Avalue, ANumberOfSeconds * MSecsPerSec);
end;

function IncMilliSecond(const AValue: TDateTime;
  const ANumberOfMilliSeconds: Int64 = 1): TDateTime;
var
  TS: TTimeStamp;
  TempTime: Comp;
begin
  TS := DateTimeToTimeStamp(AValue);
  TempTime := TimeStampToMSecs(TS);
  TempTime := TempTime + ANumberOfMilliSeconds;
  TS := MSecsToTimeStamp(TempTime);
  Result := TimeStampToDateTime(TS);
end;


function TFW_DateTime._YearSpan(const Value: TDateTime): Double;
begin
  result := YearSpan(val, Value);
end;

function TFW_DateTime._MonthSpan(const Value: TDateTime): Double;
begin
  result := MonthSpan(val, Value);
end;

function TFW_DateTime._WeekSpan(const Value: TDateTime): Double;
begin
  result := WeekSpan(val, Value);
end;

function TFW_DateTime._DaySpan(const Value: TDateTime): Double;
begin
  result := DaySpan(val, Value);
end;

function TFW_DateTime._HourSpan(const Value: TDateTime): Double;
begin
  result := HourSpan(val, Value);
end;

function TFW_DateTime._MinuteSpan(const Value: TDateTime): Double;
begin
  result := MinuteSpan(val, Value);
end;

function TFW_DateTime._SecondSpan(const Value: TDateTime): Double;
begin
  result := SecondSpan(val, Value);
end;

function TFW_DateTime._MilliSecondSpan(const Value: TDateTime): Double;
begin
  result := MinuteSpan(val, Value);
end;

function TFW_DateTime._AddYears(const ANumberOfYears: Integer = 1): TDateTime;
begin
  result := IncYear(val, ANumberOfYears);
end;

function TFW_DateTime._AddWeeks(const ANumberOfWeeks: Integer = 1): TDateTime;
begin
  result := IncWeek(val, ANumberOfWeeks);
end;

function TFW_DateTime._AddDays(const ANumberOfDays: Integer = 1): TDateTime;
begin
  result := IncDay(val, ANumberOfDays);
end;

function TFW_DateTime._AddHours(const ANumberOfHours: Int64 = 1): TDateTime;
begin
  result := IncHour(val, ANumberOfHours);
end;

function TFW_DateTime._AddMinutes(const ANumberOfMinutes: Int64 = 1): TDateTime;
begin
  result := IncMinute(val, ANumberOfMinutes);
end;

function TFW_DateTime._AddSeconds(const ANumberOfSeconds: Int64 = 1): TDateTime;
begin
  result := IncSecond(val, ANumberOfSeconds);
end;

function TFW_DateTime._AddMilliSeconds(const ANumberOfMilliSeconds: Int64 = 1): TDateTime;
begin
  result := IncMilliSecond(val, ANumberOfMilliSeconds);
end;

{$IFDEF UNIC}
function TFW_DateTime._EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  result := DateUtils.EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;
{$ENDIF}

function TFW_DateTime._EncodeDate(const AYear, AMonth, ADay: Word): TDateTime;
begin
  result := EncodeDate(AYear, AMonth, ADay);
end;

function TFW_DateTime._EncodeTime(const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  result := EncodeTime(AHour, AMinute, ASecond, AMilliSecond);
end;

function TFW_DateTime._Min(const Value: TDateTime): TDateTime;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_DateTime._Max(const Value: TDateTime): TDateTime;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_DateTime._Equals(const Value: TDateTime): boolean;
begin
  result := val = Value;
end;

function TFW_DateTime._MinValue: TDateTime;
begin
  result := 0;
end;

function TFW_DateTime._MaxValue: TDateTime;
begin
  result := 2958465.99998843; //31.12.9999 23:59:59:999;
end;

// TFW_Double ------------------------------------------------------------------

function TFW_Double._ToString: String;
begin
  result := FloatToStr(val);
end;

function TFW_Double._ToStringFormat(const Format: String): String;
begin
  result := FormatFloat(Format, val);
end;

{$IFDEF UNIC}
function TFW_Double._ToISOString: String;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  result := FloatToStrF(val, ffFixed, 18, 6, aFormat);
end;

{$IFNDEF PAXARM}
function TFW_Double._ToISOAnsiString: AnsiString;
begin
  result := UTF8Encode(_ToISOString);
end;

function TFW_Double._FromISOAnsiString(const Value: AnsiString): double;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToFloat(String(Value), result, aFormat) then
     result := 0;
end;
{$ENDIF}

function TFW_Double._FromISOString(const Value: String): double;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToFloat(Value, result, aFormat) then
     result := 0;
end;
{$ENDIF}

function TFW_Double._Round: Int64;
begin
  result := Round(val);
end;

{$IFDEF UNIC}
function TFW_Double._RoundTo(const Digit: integer): double;
begin
  result := Math.RoundTo(val, Digit);
end;
{$ENDIF}

function TFW_Double._Power(const Exponent: Extended): double;
begin
  result := Math.Power(val, Exponent);
end;

function TFW_Double._Trunc: Int64;
begin
  result := System.Trunc(val);
end;

function TFW_Double._Floor: Integer;
begin
  result := Math.Floor(val);
end;

function TFW_Double._Ceil: Integer;
begin
  result := Math.Ceil(val);
end;

function TFW_Double._Min(const Value: Double): Double;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Double._Max(const Value: Double): Double;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Double._Equals(const Value: Double): boolean;
begin
  result := val = Value;
end;


// TFW_Single ------------------------------------------------------------------

function TFW_Single._ToString: String;
begin
  result := FloatToStr(val);
end;

function TFW_Single._Min(const Value: Single): Single;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Single._Max(const Value: Single): Single;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Single._Equals(const Value: Single): boolean;
begin
  result := val = Value;
end;

// TFW_Extended ----------------------------------------------------------------

function TFW_Extended._ToString: String;
begin
  result := FloatToStr(val);
end;

function TFW_Extended._ToStringFormat(const Format: String): String;
begin
  result := FormatFloat(Format, val);
end;

{$IFDEF UNIC}
function TFW_Extended._ToISOString: String;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  result := FloatToStrF(val, ffFixed, 18, 6, aFormat);
end;

{$IFNDEF PAXARM}
function TFW_Extended._ToISOAnsiString: AnsiString;
begin
  result := UTF8Encode(_ToISOString);
end;

function TFW_Extended._FromISOAnsiString(const Value: AnsiString): Extended;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToFloat(String(Value), result, aFormat) then
     result := 0;
end;
{$ENDIF}

function TFW_Extended._FromISOString(const Value: String): Extended;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToFloat(Value, result, aFormat) then
     result := 0;
end;

function TFW_Extended._RoundTo(const Digit: integer): Extended;
begin
  result := Math.RoundTo(val, Digit);
end;
{$ENDIF}

function TFW_Extended._Round: Int64;
begin
  result := Round(val);
end;

function TFW_Extended._Power(const Exponent: Extended): Extended;
begin
  result := Math.Power(val, Exponent);
end;

function TFW_Extended._Trunc: Int64;
begin
  result := System.Trunc(val);
end;

function TFW_Extended._Floor: Integer;
begin
  result := Math.Floor(val);
end;

function TFW_Extended._Ceil: Integer;
begin
  result := Math.Ceil(val);
end;

function TFW_Extended._Min(const Value: Extended): Extended;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Extended._Max(const Value: Extended): Extended;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Extended._Equals(const Value: Extended): boolean;
begin
  result := val = Value;
end;

// TFW_Currency ----------------------------------------------------------------

function TFW_Currency._ToString: String;
begin
  result := CurrToStr(val);
end;

function TFW_Currency._ToStringFormat(const Format: String): String;
begin
  result := FormatFloat(Format, val);
end;

{$IFDEF UNIC}
function TFW_Currency._ToISOString: String;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  result := FloatToStrF(val, ffFixed, 18, 6, aFormat);
end;

{$IFNDEF PAXARM}
function TFW_Currency._ToISOAnsiString: AnsiString;
begin
  result := UTF8Encode(_ToISOString);
end;

function TFW_Currency._FromISOAnsiString(const Value: AnsiString): Currency;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToCurr(String(Value), result, aFormat) then
     result := 0;
end;
{$ENDIF}

function TFW_Currency._FromISOString(const Value: String): Currency;
var
  aFormat: TFormatSettings;
begin
  aFormat.DecimalSeparator := '.';
  if not TryStrToCurr(Value, result, aFormat) then
     result := 0;
end;

function TFW_Currency._RoundTo(const Digit: integer): Currency;
begin
  result := Math.RoundTo(val, Digit);
end;
{$ENDIF}

function TFW_Currency._Round: Int64;
begin
  result := Round(val);
end;

function TFW_Currency._Power(const Exponent: Extended): Currency;
begin
  result := Math.Power(val, Exponent);
end;

function TFW_Currency._Trunc: Int64;
begin
  result := Trunc(val);
end;

function TFW_Currency._Floor: Integer;
begin
  result := Math.Floor(val);
end;

function TFW_Currency._Ceil: Integer;
begin
  result := Math.Ceil(val);
end;

function TFW_Currency._Min(const Value: Currency): Currency;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Currency._Max(const Value: Currency): Currency;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Currency._Equals(const Value: Currency): boolean;
begin
  result := val = Value;
end;

// TFW_AnsiChar ----------------------------------------------------------------

{$IFNDEF PAXARM}
function TFW_AnsiChar._ToString: String;
begin
  result := String(val);
end;

function TFW_AnsiChar._Equals(const Value: AnsiChar): boolean;
begin
  result := val = Value;
end;
{$ENDIF}

// TFW_WideChar ----------------------------------------------------------------

function TFW_WideChar._ToString: String;
begin
  result := val;
end;

function TFW_WideChar._Equals(const Value: WideChar): boolean;
begin
  result := val = Value;
end;

// TFW_Integer -----------------------------------------------------------------

function TFW_Integer._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_Integer._ToDate: TDateTime;
var
  AYear   : Word;
  AMonth  : Word;
  ADay    : Word;
begin
  AYear := val div (10000);
  AMonth := (val - AYear * 10000) div (100);
  aDay := val - (AYear * 10000) - (AMonth * 100);
  try
    result := EncodeDate(AYear, AMonth, aDay);
  except
    result := 0;
  end;
end;

function TFW_Integer._ToHex(Digits: Integer = 8): String;
begin
  result := IntToHex(val, Digits);
end;

function TFW_Integer._FromHex(const Value: String): Integer;
var
  aTemp: String;
begin
  aTemp := Value;
  if Copy(aTemp, 1, 1) <> '$' then
     aTemp := '$' + aTemp;
  result := StrToInt(aTemp);
end;

{$IFNDEF PAXARM}
function TFW_Integer._FromHexAnsi(const Value: AnsiString): Integer;
var
  aTemp: String;
begin
  aTemp := String(Value);
  if Copy(aTemp, 1, 1) <> '$' then
     aTemp := '$' + aTemp;
  result := StrToInt(aTemp);
end;
{$ENDIF}

function TFW_Integer._Min(const Value: Integer): Integer;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Integer._Max(const Value: Integer): Integer;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Integer._Equals(const Value: Integer): boolean;
begin
  result := val = Value;
end;

function TFW_Integer._MinValue: Integer;
begin
  result := -MaxInt
end;

function TFW_Integer._MaxValue: Integer;
begin
  result := MaxInt;
end;

// TFW_Int64 -----------------------------------------------------------------

function TFW_Int64._ToString: String;
begin
  result := IntToStr(val);
end;

function TFW_Int64._ToDate: TDateTime;
var
  AYear   : Word;
  AMonth  : Word;
  ADay    : Word;
begin
  AYear := val div (10000);
  AMonth := (val - AYear * 10000) div (100);
  aDay := val - (AYear * 10000) - (AMonth * 100);
  try
    result := EncodeDate(AYear, AMonth, aDay);
  except
    result := 0;
  end;
end;

function TFW_Int64._ToHex(Digits: Integer = 8): String;
begin
  result := IntToHex(val, Digits);
end;

function TFW_Int64._FromHex(const Value: String): Int64;
var
  aTemp: String;
begin
  aTemp := Value;
  if Copy(aTemp, 1, 1) <> '$' then
     aTemp := '$' + aTemp;
  result := StrToInt64(aTemp);
end;

{$IFNDEF PAXARM}
function TFW_Int64._FromHexAnsi(const Value: AnsiString): Int64;
var
  aTemp: String;
begin
  aTemp := String(Value);
  if Copy(aTemp, 1, 1) <> '$' then
     aTemp := '$' + aTemp;
  result := StrToInt64(aTemp);
end;
{$ENDIF}

function TFW_Int64._Min(const Value: Int64): Int64;
begin
  if val < Value then
    result := val
  else
    result := Value;
end;

function TFW_Int64._Max(const Value: Int64): Int64;
begin
  if val > Value then
    result := val
  else
    result := Value;
end;

function TFW_Int64._Equals(const Value: Int64): boolean;
begin
  result := val = Value;
end;

function TFW_Int64._MinValue: Int64;
begin
  result := - 9223372036854775807;
end;

function TFW_Int64._MaxValue: Int64;
begin
  result := 9223372036854775807;
end;

// TFW_Variant -----------------------------------------------------------------

function TFW_Variant._ToString: String;
begin
  result := VarToStr(val);
end;

function TFW_Variant._ToDate: TDateTime;
begin
  result := Trunc(_ToDateTime);
end;

function TFW_Variant._ToDateTime: TDateTime;
begin
  result := VarToDateTime(val);
end;

function TFW_Variant._ToTime: TDateTime;
begin
  result := Frac(_ToDateTime);
end;

function TFW_Variant._IsType: TVarType;
begin
  result := VarType(val);
end;

function TFW_Variant._IsNull: boolean;
begin
  result := VarIsNull(val);
end;

function TFW_Variant._IsEmpty: boolean;
begin
  result := VarIsEmpty(val);
end;

{$IFDEF VARIANTS}
function TFW_Variant._IsEmptyParam: boolean;
begin
  result := Variants.VarIsEmptyParam(val);
end;

function FindVarData(const V: Variant): PVarData;
begin
  Result := @TVarData(V);
  while Result.VType = varByRef or varVariant do
    Result := PVarData(Result.VPointer);
end;

function VarIsError(const V: Variant; out AResult: HRESULT): Boolean; overload;
var
  LVarData: PVarData;
begin
  LVarData := FindVarData(V);
  Result := Assigned(LVarData) and (LVarData^.VType = varError);
  if Result then
    AResult := LVarData^.VError;
end;

function VarIsError(const V: Variant): Boolean; overload;
var
  LResult: HRESULT;
begin
  Result := PAXCOMP_FRAMEWORK.VarIsError(V, LResult);
end;

function TFW_Variant._IsError: boolean;
begin
  result := PAXCOMP_FRAMEWORK.VarIsError(val);
end;

function TFW_Variant._IsArray: boolean;
begin
  result := Variants.VarIsArray(val);
end;

function TFW_Variant._IsFilled: boolean;
begin
  result := false;
  if not PAXCOMP_FRAMEWORK.VarIsError(val) and
     not Variants.VarIsEmpty(val) and
     not Variants.VarIsEmptyParam(val) and
     not Variants.VarIsNULL(val) then
  begin
     result := true;
  end;
end;

{$ENDIF}

function TFW_Variant._Null: Variant;
begin
  result := Null;
end;


function TFW_Variant._Unassigned: Variant;
begin
  result := Unassigned;
end;

function TFW_Variant._Clear: Variant;
begin
  result := val;
{$IFDEF FPC}
  Variants.VarClear(result);
{$ELSE}
  System.VarClear(result);
{$ENDIF}
end;

function TFW_Variant._DimCount: Integer;
begin
  result := VarArrayDimCount(val);
end;

function TFW_Variant._LowBound(const Dim: integer): integer;
begin
  result := VarArrayLowBound(val, Dim);
end;

function TFW_Variant._HighBound(const Dim: integer): integer;
begin
  result := VarArrayHighBound(val, Dim);
end;

function TFW_Variant._Equals(const Value: Variant): boolean;
begin
  result := val = Value;
end;

// TFW_AnsiString --------------------------------------------------------------
{$IFNDEF PAXARM}
function TFW_AnsiString._ToString: String;
begin
  result := String(val);
end;

function TFW_AnsiString._Replace(const OldPattern: AnsiString; const NewPattern: AnsiString): AnsiString;
begin
  result := StringReplace(val, OldPattern, NewPattern, [rfReplaceAll]);
end;

function TFW_AnsiString._Equals(const Value: AnsiString): boolean;
begin
  result := CompareText(val, value) = 0;
end;

function TFW_AnsiString._Length: integer;
begin
  result := System.Length(val);
end;

{$IFDEF UNIC}

function TFW_AnsiString._ToDate: TDateTime;
begin
  if not SysUtils.TryStrToDate(String(val), result) then
    result := 0;
end;

function TFW_AnsiString._ToTime: TDateTime;
begin
  if not SysUtils.TryStrToTime(String(val), result) then
    result := 0;
end;

function TFW_AnsiString._ToDateTime: TDateTime;
begin
  if not SysUtils.TryStrToDateTime(String(val), result) then
    result := 0;
end;

function TFW_AnsiString._ToCurrency: Currency;
begin
  if not SysUtils.TryStrToCurr(String(val), result) then
     result := 0;
end;

function TFW_AnsiString._ToExtended: Extended;
begin
  if not SysUtils.TryStrToFloat(String(val), result) then
     result := 0;
end;

function TFW_AnsiString._ToDouble: Double;
begin
  if not SysUtils.TryStrToFloat(String(val), result) then
     result := 0;
end;

function TFW_AnsiString._ToCardinal: Cardinal;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(String(val), aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_AnsiString._ToShortInt: ShortInt;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(String(val), aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_AnsiString._ToSmallInt: SmallInt;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(String(val), aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_AnsiString._ToSingle: Single;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(String(val), aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_AnsiString._ToWord: Word;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(String(val), aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_AnsiString._ToInt32: Integer;
begin
  if not SysUtils.TryStrToInt(String(val), result) then
     result := 0;
end;

function TFW_AnsiString._ToInt64: Int64;
begin
  if not SysUtils.TryStrToInt64(String(val), result) then
     result := 0;
end;

function TFW_AnsiString._ToBoolean: Boolean;
begin
  if not SysUtils.TryStrToBool(String(val), result) then
     result := false;
end;

function TFW_AnsiString._ToByteBool: ByteBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(String(val), aTemp) then
     aTemp := false;
  result := aTemp;
end;

function TFW_AnsiString._ToLongBool: LongBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(String(val), aTemp) then
     aTemp := false;
  result := aTemp;
end;

function TFW_AnsiString._ToWordBool: WordBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(String(val), aTemp) then
     aTemp := false;
  result := aTemp;
end;

function TFW_AnsiString._ToUTF8: UTF8String;
begin
  result := System.AnsiToUtf8(String(val));
end;

function TFW_AnsiString._FromUTF8(const Value: UTF8String): AnsiString;
begin
  result := AnsiString(System.Utf8ToAnsi(Value));
end;

function TFW_AnsiString._ToUnicode: UnicodeString;
begin
{$IFDEF DRTTI}
  result := UTF8ToString(Val);
{$ELSE}
  result := UTF8Decode(System.AnsiToUtf8(String(val)));
{$ENDIF}
end;

function TFW_AnsiString._FromUnicode(const Value: UnicodeString): AnsiString;
begin
  result := UTF8Encode(System.Utf8ToAnsi(RawByteString(Value)));
end;

function TFW_AnsiString._ToBase64: AnsiString;
begin
  result := AnsiString(UTF8ToAnsi(UTF8Encode(EncdDecd.EncodeString(String(val)))));
end;

function TFW_AnsiString._FromBase64(const Value: AnsiString): AnsiString;
begin
  result := AnsiString(UTF8ToAnsi(UTF8Encode(EncdDecd.DecodeString(String(Value)))));
end;

function TFW_AnsiString._ISOToBoolean: Boolean;
var
  aObj: TFW_Boolean;
begin
  aObj := TFW_Boolean.create;
  try
    result := aObj._FromISOString(String(val));
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToDate: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := DateOf(aObj._FromISOAnsiString(val));
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToTime: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := TimeOf(aObj._FromISOAnsiString(val));
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToDateTime: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := aObj._FromISOAnsiString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToCurrency: Currency;
var
  aObj: TFW_Currency;
begin
  aObj := TFW_Currency.create;
  try
    result := aObj._FromISOAnsiString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToExtended: Extended;
var
  aObj: TFW_Extended;
begin
  aObj := TFW_Extended.create;
  try
    result := aObj._FromISOAnsiString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._ISOToDouble: Double;
var
  aObj: TFW_Double;
begin
  aObj := TFW_Double.create;
  try
    result := aObj._FromISOAnsiString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_AnsiString._Copy(Index: integer; Count: integer): AnsiString;
begin
  result := System.Copy(val, Index, Count);
end;

function TFW_AnsiString._Delete(Index: integer; Count: integer): AnsiString;
begin
  result := val;
  System.Delete(result, Index, Count);
end;

function TFW_AnsiString._Trim: AnsiString;
begin
  result := AnsiStrings.Trim(val);
end;

function TFW_AnsiString._TrimLeft: AnsiString;
begin
  result := AnsiStrings.TrimLeft(val);
end;

function TFW_AnsiString._TrimRight: AnsiString;
begin
  result := AnsiStrings.TrimRight(val);
end;

function TFW_AnsiString._Contains(const Value: AnsiString): boolean;
begin
  result := AnsiStrings.PosEx(value, val, 1) <> 0;
end;

function TFW_AnsiString._Pos(const Value: AnsiString): integer;
begin
  result := AnsiStrings.PosEx(value, val, 1);
end;

function TFW_AnsiString._IndexOf(const Value: AnsiString; const StartIndex: integer = 1): integer;
begin
  result := AnsiStrings.PosEx(value, val, StartIndex);
end;

function TFW_AnsiString._Quoted(const Quote: AnsiChar = '"'): AnsiString;
begin
  result := AnsiStrings.AnsiQuotedStr(val, Quote);
end;

function TFW_AnsiString._Dequoted(const Quote: AnsiChar = '"'): AnsiString;
begin
  result := AnsiStrings.AnsiDequotedStr(val, Quote);
end;

function TFW_AnsiString._ToUpper: AnsiString;
begin
  result := AnsiStrings.UpperCase(val);
end;

function TFW_AnsiString._ToLower: AnsiString;
begin
  result := AnsiStrings.LowerCase(val);
end;

function TFW_AnsiString._Split(const Seperator: AnsiString): TAnsiStringDynArray;
var
  i: Integer;
  S: AnsiString;
begin
  S := val;
  SetLength(Result, 0);
  i := 0;
  while AnsiStrings.PosEx(Seperator, S, 1) > 0 do
  begin
    SetLength(Result, Length(Result) +1);
    Result[i] := Copy(S, 1, Pos(Seperator, S) -1);
    Inc(i);
    S := Copy(S, AnsiStrings.PosEx(Seperator, S, 1) + Length(Seperator), Length(S));
  end;
  SetLength(Result, Length(Result) + 1);
  Result[i] := Copy(S, 1, Length(S));
end;

function TFW_AnsiString._SplitEx(const Seperator: AnsiChar; const Quotes: Boolean; const Quote: AnsiChar = '"'; const TrimText: Boolean = false): TAnsiStringDynArray;
var
  i       : Integer;
  a       : integer;
  aBuffer : AnsiString;
  aQuote  : Boolean;
begin
  SetLength(Result, 0);

  aBuffer := '';
  a       := -1;
  aQuote  := true;
  for i := 1 to Length(val) do
  begin
    if (Quotes and (val[I] = Seperator) and aQuote) or
       (not (Quotes) and (val[I] = Seperator)) then
    begin
      if TrimText then aBuffer := Trim(aBuffer);
      if aBuffer = '' then aBuffer := Seperator;
      if aBuffer[1] = Seperator then
        aBuffer := Copy(aBuffer, 2, Length(aBuffer));
      inc(a);
      SetLength(Result, a + 1);
      result[a] := aBuffer;
      aBuffer   := '';
    end;
    if Quotes then
    begin
      if val[I] = Quote then
      begin
        aQuote := not(aQuote);
        Continue;
      end;
      if (val[i] <> Seperator) or
         ((val[i] = Seperator) and (aQuote=false)) then
      begin
        aBuffer := aBuffer + val[i];
      end;
    end else
    begin
      if val[i] <> Seperator then
        aBuffer := aBuffer + val[i];
    end;
  end;
  if aBuffer <> '' then
  begin
    if TrimText then aBuffer := Trim(aBuffer);
    inc(a);
    SetLength(Result, a + 1);
    result[a] := aBuffer;
   end;
end;

function TFW_AnsiString._Join(const Value: TAnsiStringDynArray; const Seperator: AnsiString): AnsiString;
var
  i: integer;
begin
  result := '';
  for i := low(Value) to high(Value) do
  begin
    if i > 0 then
       result := result + Seperator;
    result := result + Value[i];
  end;
end;

function TFW_AnsiString._Insert(const Value: AnsiString; Index: integer): AnsiString;
begin
  result := val;
  Insert(Value, result, Index);
end;

function TFW_AnsiString._EndsWith(const Value: AnsiString): boolean;
begin
  result := StrUtils.AnsiEndsText(String(Value), String(val));
end;

function TFW_AnsiString._StartsWith(const Value: AnsiString): boolean;
begin
  result := StrUtils.AnsiStartsText(String(Value), String(val));
end;

function TFW_AnsiString._IsNumeric: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: AnsiChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not (aChar in ['0'..'9']) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_AnsiString._IsAlpha: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: AnsiChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not (aChar in ['A'..'Z', ' ']) and
         not (aChar in ['a'..'z']) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_AnsiString._IsAlphaNumeric: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: AnsiChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not (aChar in ['A'..'Z', ' ']) and
         not (aChar in ['0'..'9']) and
         not (aChar in ['a'..'z']) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_AnsiString._Match(const Mask: String): boolean;
begin
  result := Masks.MatchesMask(String(val), Mask);
end;

function TFW_AnsiString._Reverse: AnsiString;
begin
  result := AnsiString(StrUtils.AnsiReverseString(String(val)));
end;

function TFW_AnsiString._Left(const Length: Integer): AnsiString;
begin
  result := AnsiString(StrUtils.LeftStr(String(val), Length));
end;

function TFW_AnsiString._Right(const Length: Integer): AnsiString;
begin
  result := AnsiStrings.RightStr(val, Length);
end;

function TFW_AnsiString._AppendA(const Value: AnsiString): AnsiString;
begin
  result := val + Value;
end;

function TFW_AnsiString._AppendW(const Value: String): AnsiString;
begin
  result := AnsiString(val + AnsiString(UTF8ToAnsi(UTF8Encode(Value))));
end;

function TFW_AnsiString._AppendLineA(const Value: AnsiString): AnsiString;
begin
  result := val + Value + #13#10;
end;

function TFW_AnsiString._AppendLineW(const Value: String): AnsiString;
begin
  result := AnsiString(val + AnsiString(UTF8ToAnsi(UTF8Encode(Value))) + #13#10);
end;

function TFW_AnsiString._Lastchar: AnsiChar;
var
{$IFDEF GE_DXE3}
  aResult: PWideChar;
{$ELSE}
  aResult: PAnsiChar;
{$ENDIF}
begin
  aResult := Sysutils.AnsiLastChar(AnsiString(val));
  if aResult <> nil then
    result := AnsiChar(aResult^)
  else
    result := #0;
end;

function TFW_AnsiString._LastDelimiter(const Delimiters: AnsiString = ';'): Integer;
begin
  result := AnsiStrings.LastDelimiter(Delimiters, val);
end;

function TFW_AnsiString._FindDelimiter(const Delimiters: AnsiString = ';'; const StartIdx: integer = 1): Integer;
begin
  result := FindDelimiter(String(Delimiters), String(val), StartIdx);
end;

function TFW_AnsiString._StringOfChar(const Ch: AnsiChar; const Count: integer): AnsiString;
begin
  result := StringOfChar(Ch, Count)
end;
{$ENDIF}
{$ENDIF}

// TFW_UnicString --------------------------------------------------------------

constructor TFW_UnicString.Create;
begin
  inherited;
end;

function TFW_UnicString._ToString: String;
begin
  result := val;
end;

function TFW_UnicString._Replace(const OldPattern: String; const NewPattern: String): String;
begin
  result := SysUtils.StringReplace(val, OldPattern, NewPattern, [rfReplaceAll]);
end;

function TFW_UnicString._Equals(const Value: String): boolean;
begin
  result := SysUtils.CompareText(val, value) = 0;
end;

function TFW_UnicString._Length: integer;
begin
  result := Length(val);
end;

{$IFDEF UNIC}
function TFW_UnicString._ToDate: TDateTime;
begin
  if not SysUtils.TryStrToDate(val, result) then
    result := 0;
end;

function TFW_UnicString._ToTime: TDateTime;
begin
  if not SysUtils.TryStrToTime(val, result) then
    result := 0;
end;

function TFW_UnicString._ToDateTime: TDateTime;
begin
  if not SysUtils.TryStrToDateTime(val, result) then
    result := 0;
end;

function TFW_UnicString._ToCurrency: Currency;
begin
  if not SysUtils.TryStrToCurr(val, result) then
     result := 0;
end;

function TFW_UnicString._ToExtended: Extended;
begin
  if not SysUtils.TryStrToFloat(val, result) then
     result := 0;
end;

function TFW_UnicString._ToDouble: Double;
begin
  if not SysUtils.TryStrToFloat(val, result) then
     result := 0;
end;

function TFW_UnicString._ToCardinal: Cardinal;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(val, aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_UnicString._ToShortInt: ShortInt;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(val, aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_UnicString._ToSmallInt: SmallInt;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(val, aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_UnicString._ToSingle: Single;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(val, aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_UnicString._ToWord: Word;
var
  aTemp: integer;
begin
  if not SysUtils.TryStrToInt(val, aTemp) then
     aTemp := 0;
  result := aTemp;
end;

function TFW_UnicString._ToInt32: Integer;
begin
  if not SysUtils.TryStrToInt(val, result) then
     result := 0;
end;

function TFW_UnicString._ToInt64: Int64;
begin
  if not SysUtils.TryStrToInt64(val, result) then
     result := 0;
end;

function TFW_UnicString._ToBoolean: Boolean;
begin
  if not SysUtils.TryStrToBool(val, result) then
     result := false;
end;

function TFW_UnicString._ToByteBool: ByteBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(val, aTemp) then
     aTemp := false;
  result := aTemp;
end;

function TFW_UnicString._ToLongBool: LongBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(val, aTemp) then
     aTemp := false;
  result := aTemp;
end;

function TFW_UnicString._ToWordBool: WordBool;
var
  aTemp: boolean;
begin
  if not SysUtils.TryStrToBool(val, aTemp) then
     aTemp := false;
  result := aTemp;
end;

{$IFNDEF PAXARM}
function TFW_UnicString._ToUTF8: UTF8String;
begin
  result := System.UTF8Encode(val);
end;

function TFW_UnicString._FromUTF8(const Value: UTF8String): String;
begin
{$IFDEF DRTTI}
  result := UTF8ToString(Value);
{$ELSE}
  result := System.UTF8Decode(Value);
{$ENDIF}
end;

function TFW_UnicString._ToAnsi: AnsiString;
begin
  result := AnsiString(Utf8ToAnsi(System.UTF8Encode(val)));
end;

function TFW_UnicString._FromAnsi(const Value: AnsiString): String;
begin
{$IFDEF DRTTI}
  result := UTF8ToString(Value);
{$ELSE}
  result := System.UTF8Decode(AnsiToUtf8(String(Value)));
{$ENDIF}
end;
{$ENDIF}

function TFW_UnicString._ToBase64: String;
begin
  result := EncdDecd.EncodeString(val);
end;

function TFW_UnicString._FromBase64(const Value: String): String;
begin
  result := EncdDecd.DecodeString(Value);
end;

function TFW_UnicString._ISOToBoolean: Boolean;
var
  aObj: TFW_Boolean;
begin
  aObj := TFW_Boolean.create;
  try
    result := aObj._FromISOString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToDate: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := DateOf(aObj._FromISOString(val));
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToTime: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := TimeOf(aObj._FromISOString(val));
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToDateTime: TDateTime;
var
  aObj: TFW_DateTime;
begin
  aObj := TFW_DateTime.create;
  try
    result := aObj._FromISOString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToCurrency: Currency;
var
  aObj: TFW_Currency;
begin
  aObj := TFW_Currency.create;
  try
    result := aObj._FromISOString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToExtended: Extended;
var
  aObj: TFW_Extended;
begin
  aObj := TFW_Extended.create;
  try
    result := aObj._FromISOString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._ISOToDouble: Double;
var
  aObj: TFW_Double;
begin
  aObj := TFW_Double.create;
  try
    result := aObj._FromISOString(val);
  finally
    FreeAndNil(aObj);
  end;
end;

function TFW_UnicString._Copy(Index: integer; Count: integer): String;
begin
  result := System.Copy(val, Index, Count);
end;

function TFW_UnicString._Delete(Index: integer; Count: integer): String;
begin
  result := val;
  System.Delete(result, Index, Count);
end;

function TFW_UnicString._Trim: String;
begin
  result := SysUtils.Trim(val);
end;

function TFW_UnicString._TrimLeft: String;
begin
  result := SysUtils.TrimLeft(val);
end;

function TFW_UnicString._TrimRight: String;
begin
  result := SysUtils.TrimRight(val);
end;

function TFW_UnicString._Contains(const Value: String): boolean;
begin
  result := System.Pos(value, val) <> 0;
end;

function TFW_UnicString._Pos(const Value: String): integer;
begin
  result := System.Pos(value, val);
end;

function TFW_UnicString._IndexOf(const Value: String; const StartIndex: integer = 1): integer;
var
 aTemp: String;
begin
 if StartIndex <> 1 then
 begin
    aTemp  := Copy(val, StartIndex, Length(val) - StartIndex + 1);
    result := System.Pos(Value, aTemp);
    if result <> 0 then
       result := result + StartIndex - 1;
 end else
 begin
    result := System.Pos(Value, val);
 end;
end;

function TFW_UnicString._Quoted(const Quote: WideChar = '"'): String;
begin
  result := SysUtils.AnsiQuotedStr(val, Quote);
end;

function TFW_UnicString._Dequoted(const Quote: WideChar = '"'): String;
begin
  result := SysUtils.AnsiDequotedStr(val, Quote);
end;

function TFW_UnicString._ToUpper: String;
begin
  result := SysUtils.UpperCase(val);
end;

function TFW_UnicString._ToLower: String;
begin
  result := SysUtils.LowerCase(val);
end;

function TFW_UnicString._Split(const Seperator: String): TStringDynArray;
var
  i: Integer;
  S: String;
begin
  S := val;
  SetLength(Result, 0);
  i := 0;
  while Pos(Seperator, S) > 0 do
  begin
    SetLength(Result, System.Length(Result) +1);
    Result[i] := Copy(S, 1, Pos(Seperator, S) -1);
    Inc(i);
    S := Copy(S, Pos(Seperator, S) + Length(Seperator), Length(S));
  end;
  SetLength(Result, System.Length(Result) + 1);
  Result[i] := Copy(S, 1, Length(S));
end;

function TFW_UnicString._SplitEx(const Seperator: WideChar; const Quotes: Boolean; const Quote: WideChar = '"'; const TrimText: Boolean = false): TStringDynArray;
var
  i       : Integer;
  a       : integer;
  aBuffer : String;
  aQuote  : Boolean;
begin
  SetLength(Result, 0);

  aBuffer := '';
  a       := -1;
  aQuote  := true;
  for i := 1 to Length(val) do
  begin
    if (Quotes and (val[I] = Seperator) and aQuote) or
       (not (Quotes) and (val[I] = Seperator)) then
    begin
      if TrimText then aBuffer := Trim(aBuffer);
      if aBuffer = '' then aBuffer := Seperator;
      if aBuffer[1] = Seperator then
        aBuffer := Copy(aBuffer, 2, Length(aBuffer));
      inc(a);
      SetLength(Result, a + 1);
      result[a] := aBuffer;
      aBuffer   := '';
    end;
    if Quotes then
    begin
      if val[I] = Quote then
      begin
        aQuote := not(aQuote);
        Continue;
      end;
      if (val[i] <> Seperator) or
         ((val[i] = Seperator) and (aQuote=false)) then
      begin
        aBuffer := aBuffer + val[i];
      end;
    end else
    begin
      if val[i] <> Seperator then
        aBuffer := aBuffer + val[i];
    end;
  end;
  if aBuffer <> '' then
  begin
    if TrimText then aBuffer := Trim(aBuffer);
    inc(a);
    SetLength(Result, a + 1);
    result[a] := aBuffer;
   end;
end;

function TFW_UnicString._Join(const Value: TStringDynArray; const Seperator: String): String;
var
  i: integer;
begin
  result := '';
  for i := low(Value) to high(Value) do
  begin
    if i > 0 then
       result := result + Seperator;
    result := result + Value[i];
  end;
end;

function TFW_UnicString._Insert(const Value: String; Index: integer): String;
begin
  result := val;
  Insert(Value, result, Index);
end;

function TFW_UnicString._EndsWith(const Value: String): boolean;
begin
  result := StrUtils.EndsText(Value, val);
end;

function TFW_UnicString._StartsWith(const Value: String): boolean;
begin
  result := StrUtils.StartsText(Value, val);
end;

function TFW_UnicString._IsNumeric: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: WideChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not IsDigit(aChar) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_UnicString._IsAlpha: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: WideChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not IsAlpha(aChar) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_UnicString._IsAlphaNumeric: boolean;
var
  i    : integer;
  aLen : integer;
  aChar: WideChar;
begin
  result := true;
  aLen   := Length(val);
  if aLen > 0 then
  begin
    for i := 1 to aLen - 1 do
    begin
      aChar := val[i];
      if not ByteInSet(aChar, IdsSet) then
      begin
        result := false;
        break;
      end;
    end;
  end else
  begin
    result := false;
  end;
end;

function TFW_UnicString._Match(const Mask: String): boolean;
begin
  result := Masks.MatchesMask(val, Mask);
end;

function TFW_UnicString._Reverse: String;
begin
  result := StrUtils.ReverseString(val);
end;

function TFW_UnicString._Left(const Length: Integer): String;
begin
  result := StrUtils.LeftStr(val, Length);
end;

function TFW_UnicString._Right(const Length: Integer): String;
begin
  result := StrUtils.RightStr(val, Length);
end;

{$IFNDEF PAXARM}
function TFW_UnicString._AppendA(const Value: AnsiString): String;
begin
{$IFDEF DRTTI}
  result := val + String(AnsiToUtf8(UTF8ToString(Value)));
{$ELSE}
  result := val + String(AnsiToUtf8(UTF8Decode(Value)));
{$ENDIF}
end;

function TFW_UnicString._AppendLineA(const Value: AnsiString): String;
begin
{$IFDEF DRTTI}
  result := val + String(AnsiToUtf8(UTF8ToString(Value))) + #13#10;
{$ELSE}
  result := val + String(AnsiToUtf8(UTF8Decode(Value))) + #13#10;
{$ENDIF}
end;
{$ENDIF}

function TFW_UnicString._AppendW(const Value: String): String;
begin
  result := val + Value;
end;

function TFW_UnicString._AppendLineW(const Value: String): String;
begin
  result := val + Value + #13#10;
end;

function TFW_UnicString._Lastchar: WideChar;
var
  aResult: PWideChar;
begin
  aResult := AnsiLastChar(val);
  if aResult <> nil then
    result := aResult^
  else
    result := #0;
end;

function TFW_UnicString._LastDelimiter(const Delimiters: String = ';'): Integer;
begin
  result := Sysutils.LastDelimiter(Delimiters, val);
end;

function TFW_UnicString._FindDelimiter(const Delimiters: String = ';'; const StartIdx: integer = 1): Integer;
begin
  result := FindDelimiter(Delimiters, val, StartIdx);
end;

function TFW_UnicString._StringOfChar(const Ch: WideChar; const Count: integer): String;
begin
  result := StringOfChar(Ch, Count)
end;

{$ENDIF}

// TFW_Array -------------------------------------------------------------------

constructor TFW_Array.Create;
begin
  inherited;
  P := nil;
end;

destructor TFW_Array.Destroy;
begin
  Clear;
  inherited;
end;

function TFW_Array.GetBound(I: Integer): Integer;
var
  A: Pointer;
begin
  result := -1;
  if P = nil then
  begin
    result := 0;
    Exit;
  end;

  case I of
    1: result := _DynArrayLength(P);
    2: result := _DynArrayLength(DynarrayPointer(P)[0]);
    3: begin
         A := DynarrayPointer(P)[0];
         result := _DynArrayLength(DynarrayPointer(A)[0]);
       end;
  end;
end;

function TFW_Array.GetLength: Integer;
begin
  result := GetBound(1);
end;

procedure TFW_Array.SetLength(const Bounds: array of Integer);
begin
  NBounds := System.Length(Bounds);
  case NBounds of
    1: _DynarraySetLength(P, Bounds[0],
        ElFinalTypeID, ElTypeID, ElSize);
    2: _DynarraySetLength2(P, Bounds[0], Bounds[1],
        ElFinalTypeID, ElTypeID, ElSize);
    3: _DynarraySetLength3(P, Bounds[0], Bounds[1], Bounds[2],
        ElFinalTypeID, ElTypeID, ElSize);
  end;
end;

function TFW_Array.AddressOfElement(const Indexes: array of Integer): Pointer;
var
  L: Integer;
begin
  L := System.Length(Indexes);
  case L of
    1: result := ShiftPointer(P, ElSize * Indexes[0]);
    2:
    begin
      result := ShiftPointer(P, SizeOf(Pointer) * Indexes[0]);
      if Pointer(result^) = nil then
      begin
        result := nil;
        Exit;
      end;
      result := ShiftPointer(Pointer(result^), ElSize * Indexes[1]);
    end;
    3:
    begin
      result := ShiftPointer(P, SizeOf(Pointer) * Indexes[0]);
      if Pointer(result^) = nil then
      begin
        result := nil;
        Exit;
      end;
      result := ShiftPointer(Pointer(result^), SizeOf(Pointer) * Indexes[1]);
      if Pointer(result^) = nil then
      begin
        result := nil;
        Exit;
      end;
      result := ShiftPointer(Pointer(result^), ElSize * Indexes[2]);
    end;
    else
      raise Exception.Create(errInternalError);
  end;
end;

procedure TFW_Array.Put(const Indexes: array of Integer; const Value: Variant);
var
  Q: Pointer;
begin
  Q := AddressOfElement(Indexes);
  PutVariantValue(Q, ElFinalTypeID, Value);
end;

function TFW_Array.Get(const Indexes: array of Integer): Variant;
var
  Q: Pointer;
begin
  Q := AddressOfElement(Indexes);
  result := GetVariantValue(Q, ElFinalTypeID);
end;

procedure TFW_Array.Clear;
begin
  case NBounds of
    1: _DynarrayClr1(P, ElFinalTypeId, ElTypeId, ElSize);
    2: _DynarrayClr2(P, ElFinalTypeId, ElTypeId, ElSize);
    3: _DynarrayClr3(P, ElFinalTypeId, ElTypeId, ElSize);
  end;
end;

procedure _InitFWArray(P: Pointer;
                       A: TFW_Array;
                       NBounds: Integer;
                       ElFinalTypeId: Integer;
                       ElTypeId: Integer;
                       ElSize: Integer;
                       DecRefCount: Integer); pascal;
begin
  A.prog := P;
  A.NBounds := NBounds;
  A.ElFinalTypeId := ElFinalTypeId;
  A.ElTypeId := ElTypeId;
  A.ElSize := ElSize;
  if DecRefCount > 0 then
    A.RefCount := A.RefCount - 1;
end;

function _FWArrayLength(A: TFW_Array): Integer;
begin
  result := A.GetLength;
end;

const
  ByRef = true;

procedure Register_Framework(st: TBaseSymbolTable);
var
  H, G, H_Sub: Integer;
begin
  with st do
  begin
    H := RegisterNamespace(0, 'PaxCompilerFramework');
    H_PaxCompilerFramework := H;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_ToFWObject);
    Id_ToFWObject := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned); //runner
    RegisterParameter(H_Sub, typePOINTER, Unassigned); //adr
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //kind
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //ft
    RegisterParameter(H_Sub, typeINTEGER, Unassigned); //type_id
    RegisterParameter(H_Sub, typePOINTER, Unassigned, ByRef); //ret object

    G := RegisterClassType(H, TFW_Object);
    H_TFW_Object := G;

{$IFNDEF PAXARM}
    RegisterDynamicArrayType(H, 'TAnsiStringDynArray', typeANSISTRING);
{$ENDIF}
    RegisterDynamicArrayType(H, 'TStringDynArray', typeUNICSTRING);

    G := RegisterClassType(H, TFW_Boolean);
    H_TFW_Boolean := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Boolean._ToString);
    RegisterHeader(G, 'function Equals(const Value: Boolean): boolean;', @TFW_Boolean._Equals);
    RegisterHeader(G, 'function ToInt32: integer;', @TFW_Boolean._toInt32);
    RegisterHeader(G, 'function ToISOString: String;', @TFW_Boolean._ToISOString);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToISOAnsiString: AnsiString;', @TFW_Boolean._ToISOAnsiString);
    RegisterHeader(G, 'function FromISOString(const Value: AnsiString): Boolean; overload;', @TFW_Boolean._FromISOAnsiString);
{$ENDIF}
    RegisterHeader(G, 'function FromISOString(const Value: String): Boolean; overload;', @TFW_Boolean._FromISOString);

    G := RegisterClassType(H, TFW_ByteBool);
    H_TFW_ByteBool := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_ByteBool._ToString);
    RegisterHeader(G, 'function Equals(const Value: ByteBool): boolean;', @TFW_ByteBool._Equals);
    RegisterHeader(G, 'function ToInt32: integer;', @TFW_ByteBool._toInt32);

    G := RegisterClassType(H, TFW_WordBool);
    H_TFW_WordBool := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_WordBool._ToString);
    RegisterHeader(G, 'function Equals(const Value: WordBool): boolean;', @TFW_WordBool._Equals);
    RegisterHeader(G, 'function ToInt32: integer;', @TFW_WordBool._toInt32);

    G := RegisterClassType(H, TFW_LongBool);
    H_TFW_LongBool := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_LongBool._ToString);
    RegisterHeader(G, 'function Equals(const Value: LongBool): boolean;', @TFW_LongBool._Equals);
    RegisterHeader(G, 'function ToInt32: integer;', @TFW_LongBool._toInt32);

    G := RegisterClassType(H, TFW_Byte);
    H_TFW_Byte := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Byte._ToString);
    RegisterHeader(G, 'function Equals(const Value: Byte): boolean;', @TFW_Byte._Equals);

    G := RegisterClassType(H, TFW_SmallInt);
    H_TFW_SmallInt := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_SmallInt._ToString);
    RegisterHeader(G, 'function Equals(const Value: SmallInt): boolean;', @TFW_SmallInt._Equals);
    RegisterHeader(G, 'function MinValue: SmallInt;', @TFW_SmallInt._MinValue);
    RegisterHeader(G, 'function MaxValue: SmallInt;', @TFW_SmallInt._MaxValue);

    G := RegisterClassType(H, TFW_ShortInt);
    H_TFW_ShortInt := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_ShortInt._ToString);
    RegisterHeader(G, 'function Equals(const Value: ShortInt): boolean;', @TFW_ShortInt._Equals);
    RegisterHeader(G, 'function MinValue: ShortInt;', @TFW_ShortInt._MinValue);
    RegisterHeader(G, 'function MaxValue: ShortInt;', @TFW_ShortInt._MaxValue);

    G := RegisterClassType(H, TFW_Word);
    H_TFW_Word := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Word._ToString);
    RegisterHeader(G, 'function Equals(const Value: Word): boolean;', @TFW_Word._Equals);
    RegisterHeader(G, 'function MinValue: Word;', @TFW_Word._MinValue);
    RegisterHeader(G, 'function MaxValue: Word;', @TFW_Word._MaxValue);

    G := RegisterClassType(H, TFW_Cardinal);
    H_TFW_Cardinal := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Cardinal._ToString);
    RegisterHeader(G, 'function Equals(const Value: Cardinal): boolean;', @TFW_Cardinal._Equals);
    RegisterHeader(G, 'function MinValue: Cardinal;', @TFW_Cardinal._MinValue);
    RegisterHeader(G, 'function MaxValue: Cardinal;', @TFW_Cardinal._MaxValue);

    G := RegisterClassType(H, TFW_Double);
    H_TFW_Double := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Double._ToString);
    RegisterHeader(G, 'function ToString(const Format: String): String; overload;', @TFW_Double._ToStringFormat);
    RegisterHeader(G, 'function Equals(const Value: Double): boolean;', @TFW_Double._Equals);
{$IFDEF UNIC}
    RegisterHeader(G, 'function ToISOString: String;', @TFW_Double._ToISOString);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToISOAnsiString: AnsiString;', @TFW_Double._ToISOAnsiString);
    RegisterHeader(G, 'function FromISOString(const Value: AnsiString): double; overload;', @TFW_Double._FromISOAnsiString);
{$ENDIF}
    RegisterHeader(G, 'function FromISOString(const Value: String): double; overload;', @TFW_Double._FromISOString);
    RegisterHeader(G, 'function RoundTo(const Digit: integer): double;', @TFW_Double._RoundTo);
{$ENDIF}
    RegisterHeader(G, 'function Round: Int64;', @TFW_Double._Round);
    RegisterHeader(G, 'function Trunc: Int64;', @TFW_Double._Trunc);
    RegisterHeader(G, 'function Floor: Integer;', @TFW_Double._Floor);
    RegisterHeader(G, 'function Min(const Value: double): double;', @TFW_Double._Min);
    RegisterHeader(G, 'function Max(const Value: double): double;', @TFW_Double._Max);

    G := RegisterClassType(H, TFW_Single);
    H_TFW_Single := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Single._ToString);
    RegisterHeader(G, 'function Equals(const Value: Single): boolean;', @TFW_Single._Equals);
    RegisterHeader(G, 'function Min(const Value: Single): Single;', @TFW_Single._Min);
    RegisterHeader(G, 'function Max(const Value: Single): Single;', @TFW_Single._Max);

    G := RegisterClassType(H, TFW_Extended);
    H_TFW_Extended := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Extended._ToString);
    RegisterHeader(G, 'function ToString(const Format: String): String; overload;', @TFW_Extended._ToStringFormat);
    RegisterHeader(G, 'function Equals(const Value: Extended): boolean;', @TFW_Extended._Equals);
{$IFDEF UNIC}
    RegisterHeader(G, 'function ToISOString: String;', @TFW_Extended._ToISOString);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToISOAnsiString: AnsiString;', @TFW_Extended._ToISOAnsiString);
    RegisterHeader(G, 'function FromISOString(const Value: AnsiString): Extended; overload;', @TFW_Extended._FromISOAnsiString);
{$ENDIF}
    RegisterHeader(G, 'function FromISOString(const Value: String): Extended; overload;', @TFW_Extended._FromISOString);
    RegisterHeader(G, 'function RoundTo(const Digit: integer): Extended;', @TFW_Extended._RoundTo);
{$ENDIF}
    RegisterHeader(G, 'function Round: Int64;', @TFW_Extended._Round);
    RegisterHeader(G, 'function Trunc: Int64;', @TFW_Extended._Trunc);
    RegisterHeader(G, 'function Floor: Integer;', @TFW_Extended._Floor);
    RegisterHeader(G, 'function Min(const Value: Extended): Extended;', @TFW_Extended._Min);
    RegisterHeader(G, 'function Max(const Value: Extended): Extended;', @TFW_Extended._Max);

    G := RegisterClassType(H, TFW_Currency);
    H_TFW_Currency := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Currency._ToString);
    RegisterHeader(G, 'function ToString(const Format: String): String; overload;', @TFW_Currency._ToStringFormat);
    RegisterHeader(G, 'function Equals(const Value: Currency): boolean;', @TFW_Currency._Equals);
{$IFDEF UNIC}
    RegisterHeader(G, 'function ToISOString: String;', @TFW_Currency._ToISOString);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToISOAnsiString: AnsiString;', @TFW_Currency._ToISOAnsiString);
    RegisterHeader(G, 'function FromISOString(const Value: AnsiString): Currency; overload;', @TFW_Currency._FromISOAnsiString);
{$ENDIF}
    RegisterHeader(G, 'function FromISOString(const Value: String): Currency; overload;', @TFW_Currency._FromISOString);
    RegisterHeader(G, 'function RoundTo(const Digit: integer): Currency;', @TFW_Currency._RoundTo);
{$ENDIF}
    RegisterHeader(G, 'function Round: Int64;', @TFW_Currency._Round);
    RegisterHeader(G, 'function Trunc: Int64;', @TFW_Currency._Trunc);
    RegisterHeader(G, 'function Floor: Integer;', @TFW_Currency._Floor);
    RegisterHeader(G, 'function Min(const Value: Currency): Currency;', @TFW_Currency._Min);
    RegisterHeader(G, 'function Max(const Value: Currency): Currency;', @TFW_Currency._Max);

    G := RegisterClassType(H, TFW_Integer);
    H_TFW_Integer := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Integer._ToString);
    RegisterHeader(G, 'function Equals(const Value: Integer): boolean;', @TFW_Integer._Equals);
    RegisterHeader(G, 'function ToDate: TDateTime; override;', @TFW_Integer._ToDate);
    RegisterHeader(G, 'function ToHex(Digits: Integer = 8): String;', @TFW_Integer._ToHex);
    RegisterHeader(G, 'function FromHex(const Value: String): Integer; overload;', @TFW_Integer._FromHex);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function FromHex(const Value: AnsiString): Integer; overload;', @TFW_Integer._FromHexAnsi);
{$ENDIF}
    RegisterHeader(G, 'function Min(const Value: Integer): Integer;', @TFW_Integer._Min);
    RegisterHeader(G, 'function Max(const Value: Integer): Integer;', @TFW_Integer._Max);
    RegisterHeader(G, 'function MinValue: Integer;', @TFW_Integer._MinValue);
    RegisterHeader(G, 'function MaxValue: Integer;', @TFW_Integer._MaxValue);

    G := RegisterClassType(H, TFW_Int64);
    H_TFW_Int64 := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Int64._ToString);
    RegisterHeader(G, 'function Equals(const Value: Int64): boolean;', @TFW_Int64._Equals);
    RegisterHeader(G, 'function ToDate: TDateTime; override;', @TFW_Int64._ToDate);
    RegisterHeader(G, 'function ToHex(Digits: Integer = 8): String;', @TFW_Int64._ToHex);
    RegisterHeader(G, 'function FromHex(const Value: String): Int64; overload;', @TFW_Int64._FromHex);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function FromHex(const Value: AnsiString): Int64; overload;', @TFW_Int64._FromHexAnsi);
{$ENDIF}
    RegisterHeader(G, 'function Min(const Value: Int64): Int64;', @TFW_Int64._Min);
    RegisterHeader(G, 'function Max(const Value: Int64): Int64;', @TFW_Int64._Max);
    RegisterHeader(G, 'function MinValue: Int64;', @TFW_Int64._MinValue);
    RegisterHeader(G, 'function MaxValue: Int64;', @TFW_Int64._MaxValue);

    G := RegisterClassType(H, TFW_Variant);
    H_TFW_Variant := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_Variant._ToString);
    RegisterHeader(G, 'function Equals(const Value: Variant): boolean;', @TFW_Variant._Equals);
    RegisterHeader(G, 'function ToDate: TDateTime;', @TFW_Variant._ToDate);
    RegisterHeader(G, 'function ToDateTime: TDateTime;', @TFW_Variant._ToDateTime);
    RegisterHeader(G, 'function ToTime: TDateTime;', @TFW_Variant._ToTime);

    RegisterHeader(G, 'function Clear: Variant;', @TFW_Variant._Clear);
    RegisterHeader(G, 'function IsType: TVarType;', @TFW_Variant._IsType);
    RegisterHeader(G, 'function IsNull: boolean;', @TFW_Variant._IsNull);
    RegisterHeader(G, 'function IsEmpty: boolean;', @TFW_Variant._IsEmpty);
{$IFDEF VARIANTS}
    RegisterHeader(G, 'function IsEmptyParam: boolean;', @TFW_Variant._IsEmptyParam);
    RegisterHeader(G, 'function IsError: boolean;', @TFW_Variant._IsError);
    RegisterHeader(G, 'function IsArray: boolean;', @TFW_Variant._IsArray);
    RegisterHeader(G, 'function IsFilled: boolean;', @TFW_Variant._IsFilled);
{$ENDIF}
    RegisterHeader(G, 'function Null: Variant;', @TFW_Variant._Null);
    RegisterHeader(G, 'function Unassigned: Variant;', @TFW_Variant._Unassigned);
    RegisterHeader(G, 'function DimCount: Integer;', @TFW_Variant._DimCount);
    RegisterHeader(G, 'function LowBound(const Dim: integer): integer;', @TFW_Variant._LowBound);
    RegisterHeader(G, 'function HighBound(const Dim: integer): integer;', @TFW_Variant._HighBound);

    G := RegisterClassType(H, TFW_DateTime);
    H_TFW_DateTime := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_DateTime._ToString);
    RegisterHeader(G, 'function ToString(const Format: String): String; overload;', @TFW_DateTime._ToStringFormat);
    RegisterHeader(G, 'function Equals(const Value: TDateTime): boolean;', @TFW_DateTime._Equals);
    RegisterHeader(G, 'function ToInt32: Integer;', @TFW_DateTime._ToInt32);
    RegisterHeader(G, 'function ToISOString: String;', @TFW_DateTime._ToStringISO);
{$IFDEF UNIC}
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToISOAnsiString: AnsiString;', @TFW_DateTime._ToAnsiStringISO);
{$ENDIF}
{$ENDIF}
    RegisterHeader(G, 'function FromISOString(const Value: String): TDateTime; overload;', @TFW_DateTime._FromISOString);
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function FromISOString(const Value: AnsiString): TDateTime; overload;', @TFW_DateTime._FromISOAnsiString);
{$ENDIF}

    RegisterHeader(G, 'function IsDate: boolean;', @TFW_DateTime._IsDate);
    RegisterHeader(G, 'function IsDateTime: boolean;', @TFW_DateTime._IsDateTime);
    RegisterHeader(G, 'function IsTime: boolean;', @TFW_DateTime._IsTime);

    RegisterHeader(G, 'function Date: TDateTime;', @TFW_DateTime._Date);
    RegisterHeader(G, 'function Time: TDateTime;', @TFW_DateTime._Time);
    RegisterHeader(G, 'function Now: TDateTime;', @TFW_DateTime._Now);

    RegisterHeader(G, 'function IsInLeapYear: boolean;', @TFW_DateTime._IsInLeapYear);
    RegisterHeader(G, 'function DateOf: TDateTime;', @TFW_DateTime._DateOf);
    RegisterHeader(G, 'function TimeOf: TDateTime;', @TFW_DateTime._TimeOf);
    RegisterHeader(G, 'function YearOf: Word;', @TFW_DateTime._YearOf);
    RegisterHeader(G, 'function MonthOf: Word;', @TFW_DateTime._MonthOf);
    RegisterHeader(G, 'function DayOf: Word;', @TFW_DateTime._DayOf);
    RegisterHeader(G, 'function HourOf: Word;', @TFW_DateTime._HourOf);
    RegisterHeader(G, 'function MinuteOf: Word;', @TFW_DateTime._MinuteOf);
    RegisterHeader(G, 'function SecondOf: Word;', @TFW_DateTime._SecondOf);
    RegisterHeader(G, 'function MilliSecondOf: Word;', @TFW_DateTime._MilliSecondOf);
    RegisterHeader(G, 'function WeeksInYear: Word;', @TFW_DateTime._WeeksInYear);
    RegisterHeader(G, 'function DaysInYear: Word;', @TFW_DateTime._DaysInYear);
    RegisterHeader(G, 'function Today: TDateTime;', @TFW_DateTime._Today);
    RegisterHeader(G, 'function Yesterday: TDateTime;', @TFW_DateTime._Yesterday);
    RegisterHeader(G, 'function Tomorrow: TDateTime;', @TFW_DateTime._Tomorrow);
    RegisterHeader(G, 'function YearSpan(const Value: TDateTime): Double;', @TFW_DateTime._YearSpan);
    RegisterHeader(G, 'function MonthSpan(const Value: TDateTime): Double;', @TFW_DateTime._MonthSpan);
    RegisterHeader(G, 'function WeekSpan(const Value: TDateTime): Double;', @TFW_DateTime._WeekSpan);
    RegisterHeader(G, 'function DaySpan(const Value: TDateTime): Double;', @TFW_DateTime._DaySpan);
    RegisterHeader(G, 'function HourSpan(const Value: TDateTime): Double;', @TFW_DateTime._HourSpan);
    RegisterHeader(G, 'function MinuteSpan(const Value: TDateTime): Double;', @TFW_DateTime._MinuteSpan);
    RegisterHeader(G, 'function SecondSpan(const Value: TDateTime): Double;', @TFW_DateTime._SecondSpan);
    RegisterHeader(G, 'function MilliSecondSpan(const Value: TDateTime): Double;', @TFW_DateTime._MilliSecondSpan);
    RegisterHeader(G, 'function AddYears(const ANumberOfYears: Integer = 1): TDateTime;', @TFW_DateTime._AddYears);
    RegisterHeader(G, 'function AddWeeks(const ANumberOfWeeks: Integer = 1): TDateTime;', @TFW_DateTime._AddWeeks);
    RegisterHeader(G, 'function AddDays(const ANumberOfDays: Integer = 1): TDateTime;', @TFW_DateTime._AddDays);
    RegisterHeader(G, 'function AddHours(const ANumberOfHours: Int64 = 1): TDateTime;', @TFW_DateTime._AddHours);
    RegisterHeader(G, 'function AddMinutes(const ANumberOfMinutes: Int64 = 1): TDateTime;', @TFW_DateTime._AddMinutes);
    RegisterHeader(G, 'function AddSeconds(const ANumberOfSeconds: Int64 = 1): TDateTime;', @TFW_DateTime._AddSeconds);
    RegisterHeader(G, 'function AddMilliSeconds(const ANumberOfMilliSeconds: Int64 = 1): TDateTime;', @TFW_DateTime._AddMilliSeconds);
{$IFDEF UNIC}
    RegisterHeader(G, 'function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;', @TFW_DateTime._EncodeDateTime);
{$ENDIF}
    RegisterHeader(G, 'function EncodeDate(const AYear, AMonth, ADay: Word): TDateTime;', @TFW_DateTime._EncodeDate);
    RegisterHeader(G, 'function EncodeTime(const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;', @TFW_DateTime._EncodeTime);
    RegisterHeader(G, 'function Min(const Value: TDateTime): TDateTime;', @TFW_DateTime._Min);
    RegisterHeader(G, 'function Max(const Value: TDateTime): TDateTime;', @TFW_DateTime._Max);
    RegisterHeader(G, 'function MinValue: TDateTime;', @TFW_DateTime._MinValue);
    RegisterHeader(G, 'function MaxValue: TDateTime;', @TFW_DateTime._MaxValue);
{$IFNDEF PAXARM}
    G := RegisterClassType(H, TFW_AnsiChar);
    H_TFW_AnsiChar := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_AnsiChar._ToString);
    RegisterHeader(G, 'function Equals(const Value: AnsiChar): boolean;', @TFW_AnsiChar._Equals);
{$ENDIF}
    G := RegisterClassType(H, TFW_WideChar);
    H_TFW_WideChar := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_WideChar._ToString);
    RegisterHeader(G, 'function Equals(const Value: WideChar): boolean;', @TFW_WideChar._Equals);
{$IFNDEF PAXARM}
    G := RegisterClassType(H, TFW_AnsiString);
    H_TFW_AnsiString := G;
    RegisterHeader(G, 'function ToString: String; override;', @TFW_AnsiString._ToString);
    RegisterHeader(G, 'function Replace(const OldPattern: String; const NewPattern: String): String;', @TFW_AnsiString._Replace);
    RegisterHeader(G, 'function Equals(const Value: AnsiString): boolean;', @TFW_AnsiString._Equals);
    RegisterHeader(G, 'function Length: integer;', @TFW_AnsiString._Length);
{$IFDEF UNIC}
    RegisterHeader(G, 'function ToUTF8: UTF8String;', @TFW_AnsiString._ToUTF8);
    RegisterHeader(G, 'function FromUTF8(const Value: UTF8String): AnsiString;', @TFW_AnsiString._FromUTF8);
    RegisterHeader(G, 'function ToUnicode: UnicodeString;', @TFW_AnsiString._ToUnicode);
    RegisterHeader(G, 'function FromUnicode(const Value: UnicodeString): AnsiString;', @TFW_AnsiString._FromUniCode);
    RegisterHeader(G, 'function ToBase64: AnsiString;', @TFW_AnsiString._ToBase64);
    RegisterHeader(G, 'function FromBase64(const Value: AnsiString): AnsiString;', @TFW_AnsiString._FromBase64);

    RegisterHeader(G, 'function ToDate: TDateTime;', @TFW_AnsiString._ToDate);
    RegisterHeader(G, 'function ToTime: TDateTime;', @TFW_AnsiString._ToTime);
    RegisterHeader(G, 'function ToDateTime: TDateTime;', @TFW_AnsiString._ToDateTime);
    RegisterHeader(G, 'function ToCurrency: Currency;', @TFW_AnsiString._ToCurrency);
    RegisterHeader(G, 'function ToExtended: Extended;', @TFW_AnsiString._ToExtended);
    RegisterHeader(G, 'function ToDouble: Double;', @TFW_AnsiString._ToDouble);
    RegisterHeader(G, 'function ToCardinal: Cardinal;', @TFW_AnsiString._ToCardinal);
    RegisterHeader(G, 'function ToShortInt: ShortInt;', @TFW_AnsiString._ToShortInt);
    RegisterHeader(G, 'function ToSmallInt: SmallInt;', @TFW_AnsiString._ToSmallInt);
    RegisterHeader(G, 'function ToSingle: Single;', @TFW_AnsiString._ToSingle);
    RegisterHeader(G, 'function ToWord: Word;', @TFW_AnsiString._ToWord);
    RegisterHeader(G, 'function ToInt32: Integer;', @TFW_AnsiString._ToInt32);
    RegisterHeader(G, 'function ToInt64: Int64;', @TFW_AnsiString._ToInt64);
    RegisterHeader(G, 'function ToBoolean: Boolean;', @TFW_AnsiString._ToBoolean);
    RegisterHeader(G, 'function ToByteBool: ByteBool;', @TFW_AnsiString._ToByteBool);
    RegisterHeader(G, 'function ToLongBool: LongBool;', @TFW_AnsiString._ToLongBool);
    RegisterHeader(G, 'function ToWordBool: WordBool;', @TFW_AnsiString._ToWordBool);

    RegisterHeader(G, 'function ISOToBoolean: Boolean;', @TFW_AnsiString._ISOToBoolean);
    RegisterHeader(G, 'function ISOToDate: TDateTime;', @TFW_AnsiString._ISOToDate);
    RegisterHeader(G, 'function ISOToTime: TDateTime;', @TFW_AnsiString._ISOToTime);
    RegisterHeader(G, 'function ISOToDateTime: TDateTime;', @TFW_AnsiString._ISOToDateTime);
    RegisterHeader(G, 'function ISOToCurrency: Currency;', @TFW_AnsiString._ISOToCurrency);
    RegisterHeader(G, 'function ISOToExtended: Extended;', @TFW_AnsiString._ISOToExtended);
    RegisterHeader(G, 'function ISOToDouble: Double;', @TFW_AnsiString._ISOToDouble);

    RegisterHeader(G, 'function Copy(Index: integer; Count: integer): AnsiString;', @TFW_AnsiString._Copy);
    RegisterHeader(G, 'function Delete(Index: integer; Count: integer): AnsiString;', @TFW_AnsiString._Delete);
    RegisterHeader(G, 'function Trim: AnsiString;', @TFW_AnsiString._Trim);
    RegisterHeader(G, 'function TrimLeft: AnsiString;', @TFW_AnsiString._TrimLeft);
    RegisterHeader(G, 'function TrimRight: AnsiString;', @TFW_AnsiString._TrimRight);
    RegisterHeader(G, 'function Contains(const Value: AnsiString): boolean;', @TFW_AnsiString._Contains);
    RegisterHeader(G, 'function Pos(const Value: AnsiString): integer;', @TFW_AnsiString._Pos);
    RegisterHeader(G, 'function IndexOf(const Value: AnsiString; const StartIndex: integer = 0): integer;', @TFW_AnsiString._IndexOf);
    RegisterHeader(G, 'function Quoted(const Quote: AnsiChar = ' + #39 + '"' + #39 + '): AnsiString;', @TFW_AnsiString._Quoted);
    RegisterHeader(G, 'function Dequoted(const Quote: AnsiChar = ' + #39 + '"' + #39 + '): AnsiString;', @TFW_AnsiString._Dequoted);
    RegisterHeader(G, 'function ToUpper: AnsiString;', @TFW_AnsiString._ToUpper);
    RegisterHeader(G, 'function ToLower: AnsiString;', @TFW_AnsiString._ToLower);
    RegisterHeader(G, 'function Split(const Seperator: AnsiString): TAnsiStringDynArray; overload;', @TFW_AnsiString._Split);
    RegisterHeader(G, 'function Split(const Seperator: AnsiChar; const Quotes: Boolean; const Quote: AnsiChar = ' + #39 + '"' + #39 + '; const TrimText: Boolean = false): TAnsiStringDynArray; overload;', @TFW_AnsiString._SplitEx);
    RegisterHeader(G, 'function Join(const Value: TAnsiStringDynArray; const Seperator: AnsiString): AnsiString;', @TFW_UnicString._Join);
    RegisterHeader(G, 'function Insert(const Value: AnsiString; Index: integer): AnsiString;', @TFW_AnsiString._Insert);
    RegisterHeader(G, 'function IsNumeric: boolean;', @TFW_AnsiString._IsNumeric);
    RegisterHeader(G, 'function IsAlpha: boolean;', @TFW_AnsiString._IsAlpha);
    RegisterHeader(G, 'function IsAlphaNumeric: boolean;', @TFW_AnsiString._IsAlphaNumeric);
    RegisterHeader(G, 'function Match(const Mask: String): boolean;', @TFW_AnsiString._Match);
    RegisterHeader(G, 'function EndsWith(const Value: AnsiString): boolean;', @TFW_AnsiString._EndsWith);
    RegisterHeader(G, 'function StartsWith(const Value: AnsiString): boolean;', @TFW_AnsiString._StartsWith);
    RegisterHeader(G, 'function Reverse: AnsiString;', @TFW_AnsiString._Reverse);
    RegisterHeader(G, 'function Left(const Length: Integer): AnsiString;', @TFW_AnsiString._Left);
    RegisterHeader(G, 'function Right(const Length: Integer): AnsiString;', @TFW_AnsiString._Right);
    RegisterHeader(G, 'function Append(const Value: AnsiString): AnsiString; overload;', @TFW_AnsiString._AppendA);
    RegisterHeader(G, 'function Append(const Value: String): AnsiString; overload;', @TFW_AnsiString._AppendW);
    RegisterHeader(G, 'function AppendLine(const Value: AnsiString): AnsiString; overload;', @TFW_AnsiString._AppendLineA);
    RegisterHeader(G, 'function AppendLine(const Value: String): AnsiString; overload;', @TFW_AnsiString._AppendLineW);
    RegisterHeader(G, 'function Lastchar: AnsiChar;', @TFW_AnsiString._Lastchar);
    RegisterHeader(G, 'function LastDelimiter(const Delimiters: AnsiString = ' + #39 + ';' + #39 + '): Integer;', @TFW_AnsiString._LastDelimiter);
    RegisterHeader(G, 'function FindDelimiter(const Delimiters: AnsiString = ' + #39 + ';' + #39 + '; const StartIdx: integer = 1): Integer;', @TFW_AnsiString._FindDelimiter);
    RegisterHeader(G, 'function StringOfChar(const Ch: AnsiChar; const Count: integer): AnsiString;', @TFW_AnsiString._StringOfChar);
{$ENDIF}
{$ENDIF}

    G := RegisterClassType(H, TFW_UnicString);
    H_TFW_UnicString := G;
    RegisterHeader(G, 'constructor Create;', @TFW_UnicString.Create);
    RegisterHeader(G, 'function ToString: String; override;', @TFW_UnicString._ToString);
    RegisterHeader(G, 'function Replace(const OldPattern: String; const NewPattern: String): String;', @TFW_UnicString._Replace);
    RegisterHeader(G, 'function StringReplace(const OldPattern: String; const NewPattern: String): String;', @TFW_UnicString._Replace);
    RegisterHeader(G, 'function Equals(const Value: String): boolean;', @TFW_UnicString._Equals);
    RegisterHeader(G, 'function Length: integer;', @TFW_UnicString._Length);
{$IFDEF UNIC}
{$IFNDEF PAXARM}
    RegisterHeader(G, 'function ToUTF8: UTF8String;', @TFW_UnicString._ToUTF8);
    RegisterHeader(G, 'function FromUTF8(const Value: UTF8String): String;', @TFW_UnicString._FromUTF8);
    RegisterHeader(G, 'function ToAnsi: AnsiString;', @TFW_UnicString._ToAnsi);
    RegisterHeader(G, 'function FromAnsi(const Value: AnsiString): String;', @TFW_UnicString._FromAnsi);
    RegisterHeader(G, 'function Append(const Value: AnsiString): String; overload;', @TFW_UnicString._AppendA);
    RegisterHeader(G, 'function AppendLine(const Value: AnsiString): String; overload;', @TFW_UnicString._AppendLineA);
{$ENDIF}
    RegisterHeader(G, 'function ToBase64: String;', @TFW_UnicString._ToBase64);
    RegisterHeader(G, 'function FromBase64(const Value: String): String;', @TFW_UnicString._FromBase64);

    RegisterHeader(G, 'function ToDate: TDateTime;', @TFW_UnicString._ToDate);
    RegisterHeader(G, 'function ToTime: TDateTime;', @TFW_UnicString._ToTime);
    RegisterHeader(G, 'function ToDateTime: TDateTime;', @TFW_UnicString._ToDateTime);
    RegisterHeader(G, 'function ToCurrency: Currency;', @TFW_UnicString._ToCurrency);
    RegisterHeader(G, 'function ToExtended: Extended;', @TFW_UnicString._ToExtended);
    RegisterHeader(G, 'function ToDouble: Double;', @TFW_UnicString._ToDouble);
    RegisterHeader(G, 'function ToCardinal: Cardinal;', @TFW_UnicString._ToCardinal);
    RegisterHeader(G, 'function ToShortInt: ShortInt;', @TFW_UnicString._ToShortInt);
    RegisterHeader(G, 'function ToSmallInt: SmallInt;', @TFW_UnicString._ToSmallInt);
    RegisterHeader(G, 'function ToSingle: Single;', @TFW_UnicString._ToSingle);
    RegisterHeader(G, 'function ToWord: Word;', @TFW_UnicString._ToWord);
    RegisterHeader(G, 'function ToInt32: Integer;', @TFW_UnicString._ToInt32);
    RegisterHeader(G, 'function ToInt64: Int64;', @TFW_UnicString._ToInt64);
    RegisterHeader(G, 'function ToBoolean: Boolean;', @TFW_UnicString._ToBoolean);
    RegisterHeader(G, 'function ToByteBool: ByteBool;', @TFW_UnicString._ToByteBool);
    RegisterHeader(G, 'function ToLongBool: LongBool;', @TFW_UnicString._ToLongBool);
    RegisterHeader(G, 'function ToWordBool: WordBool;', @TFW_UnicString._ToWordBool);
    RegisterHeader(G, 'function ISOToBoolean: Boolean;', @TFW_UnicString._ISOToBoolean);
    RegisterHeader(G, 'function ISOToDate: TDateTime;', @TFW_UnicString._ISOToDate);
    RegisterHeader(G, 'function ISOToTime: TDateTime;', @TFW_UnicString._ISOToTime);
    RegisterHeader(G, 'function ISOToDateTime: TDateTime;', @TFW_UnicString._ISOToDateTime);
    RegisterHeader(G, 'function ISOToCurrency: Currency;', @TFW_UnicString._ISOToCurrency);
    RegisterHeader(G, 'function ISOToExtended: Extended;', @TFW_UnicString._ISOToExtended);
    RegisterHeader(G, 'function ISOToDouble: Double;', @TFW_UnicString._ISOToDouble);

    RegisterHeader(G, 'function Copy(Index: integer; Count: integer): String;', @TFW_UnicString._Copy);
    RegisterHeader(G, 'function Delete(Index: integer; Count: integer): String;', @TFW_UnicString._Delete);
    RegisterHeader(G, 'function Trim: String;', @TFW_UnicString._Trim);
    RegisterHeader(G, 'function TrimLeft: String;', @TFW_UnicString._TrimLeft);
    RegisterHeader(G, 'function TrimRight: String;', @TFW_UnicString._TrimRight);
    RegisterHeader(G, 'function Contains(const Value: String): boolean;', @TFW_UnicString._Contains);
    RegisterHeader(G, 'function Pos(const Value: String): integer;', @TFW_UnicString._Pos);
    RegisterHeader(G, 'function IndexOf(const Value: String; const StartIndex: integer = 0): integer;', @TFW_UnicString._IndexOf);
    RegisterHeader(G, 'function Quoted(const Quote: WideChar = ' + #39 + '"' + #39 + '): String;', @TFW_UnicString._Quoted);
    RegisterHeader(G, 'function Dequoted(const Quote: WideChar = ' + #39 + '"' + #39 + '): String;', @TFW_UnicString._Dequoted);
    RegisterHeader(G, 'function ToUpper: String;', @TFW_UnicString._ToUpper);
    RegisterHeader(G, 'function ToLower: String;', @TFW_UnicString._ToLower);
    RegisterHeader(G, 'function Split(const Seperator: String): TStringDynArray; overload;', @TFW_UnicString._Split);
    RegisterHeader(G, 'function Split(const Seperator: WideChar; const Quotes: Boolean; const Quote: WideChar = ' + #39 + '"' + #39 + '; const TrimText: Boolean = false): TStringDynArray; overload;', @TFW_UnicString._SplitEx);
    RegisterHeader(G, 'function Join(const Value: TStringDynArray; const Seperator: String): String;', @TFW_UnicString._Join);
    RegisterHeader(G, 'function Insert(const Value: String; Index: integer): String;', @TFW_UnicString._Insert);
    RegisterHeader(G, 'function IsNumeric: boolean;', @TFW_UnicString._IsNumeric);
    RegisterHeader(G, 'function IsAlpha: boolean;', @TFW_UnicString._IsAlpha);
    RegisterHeader(G, 'function IsAlphaNumeric: boolean;', @TFW_UnicString._IsAlphaNumeric);
    RegisterHeader(G, 'function Match(const Mask: String): boolean;', @TFW_UnicString._Match);
    RegisterHeader(G, 'function EndsWith(const Value: String): boolean;', @TFW_UnicString._EndsWith);
    RegisterHeader(G, 'function StartsWith(const Value: String): boolean;', @TFW_UnicString._StartsWith);
    RegisterHeader(G, 'function Reverse: String;', @TFW_UnicString._Reverse);
    RegisterHeader(G, 'function Left(const Length: Integer): String;', @TFW_UnicString._Left);
    RegisterHeader(G, 'function Right(const Length: Integer): String;', @TFW_UnicString._Right);

    RegisterHeader(G, 'function Append(const Value: String): String; overload;', @TFW_UnicString._AppendW);
    RegisterHeader(G, 'function AppendLine(const Value: String): String; overload;', @TFW_UnicString._AppendLineW);
    RegisterHeader(G, 'function Lastchar: WideChar;', @TFW_UnicString._Lastchar);
    RegisterHeader(G, 'function LastDelimiter(const Delimiters: String = ' + #39 + ';' + #39 + '): Integer;', @TFW_UnicString._LastDelimiter);
    RegisterHeader(G, 'function FindDelimiter(const Delimiters: String = ' + #39 + ';' + #39 + '; const StartIdx: integer = 1): Integer;', @TFW_UnicString._FindDelimiter);
    RegisterHeader(G, 'function StringOfChar(const Ch: WideChar; const Count: integer): String;', @TFW_UnicString._StringOfChar);
{$ENDIF}

    G := RegisterClassType(H, TFW_Array);
    H_TFW_Array := G;
    FWArrayOffset := Integer(@TFW_Array(nil).P);
    RegisterTypeField(G, strNBounds, typeINTEGER, Integer(@TFW_Array(nil).NBounds));
    RegisterTypeField(G, strElFinalTypeId, typeINTEGER, Integer(@TFW_Array(nil).ElFinalTypeId));
    RegisterTypeField(G, strElTypeId, typeINTEGER, Integer(@TFW_Array(nil).ElTypeId));
    RegisterTypeField(G, strElSize, typeINTEGER, Integer(@TFW_Array(nil).ElSize));
    RegisterHeader(G, 'constructor ' + strInternalFWArrayCreate + ';', @TFW_Array.Create);
    Id_FWArray_Create := LastSubId;

    H_Sub := RegisterRoutine(0, 'length', typeINTEGER, ccREGISTER, @_FWArrayLength);
    RegisterParameter(H_Sub, H_TFW_Array, Unassigned, false, 'X');
    Id_FWArray_GetLength := LastSubId;

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_InitFWArray);
    Id_InitFWArray := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
  end;
end;

end.



