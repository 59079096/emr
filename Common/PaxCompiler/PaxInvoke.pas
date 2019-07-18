////////////////////////////////////////////////////////////////////////////
// PaxInvoke
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxInvoke.pas
// Implements dynamically invoke of a global function or a method of object
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PaxInvoke;
interface
{$IFDEF PAXARM_DEVICE}
implementation
end.
{$ENDIF}
uses
  Classes,
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_INVOKE;
const
  __ccSTDCALL = 1;
  __ccREGISTER = 2;
  __ccCDECL = 3;
  __ccPASCAL = 4;
  __ccSAFECALL = 5;
  __ccMSFASTCALL = 6;
type
  TPaxInvoke = class(TComponent)
  private
    function GetAddr: Pointer;
    procedure SetAddr(value: Pointer);
    procedure SetCallConv(value: Integer);
    function GetCallConv: Integer;
    function GetThis: Pointer;
    procedure SetThis(value: Pointer);
    function GetFake: Boolean;
    procedure SetFake(value: Boolean);
  public
{$IFNDEF PAXARM_DEVICE}
    base_invoke: TInvoke;
{$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadAddress(const DllName, ProcName: String);

    procedure ClearArguments;

    procedure AddArgAsByte(value: Byte);
    procedure AddArgAsWord(value: Word);
    procedure AddArgAsCardinal(value: Cardinal);
    procedure AddArgAsShortInt(value: ShortInt);
    procedure AddArgAsSmallInt(value: SmallInt);
    procedure AddArgAsInteger(value: Integer);
    procedure AddArgAsInt64(value: Int64);
    procedure AddArgAsUInt64(value: UInt64);
    procedure AddArgAsBoolean(value: Boolean);
    procedure AddArgAsWordBool(value: WordBool);
    procedure AddArgAsLongBool(value: LongBool);
    procedure AddArgAsChar(value: Char);
    procedure AddArgAsWideChar(value: WideChar);
    procedure AddArgAsDouble(value: Double);
    procedure AddArgAsSingle(value: Single);
    procedure AddArgAsExtended(value: Extended);
    procedure AddArgAsCurrency(value: Currency);
{$IFNDEF PAXARM}
    procedure AddArgAsAnsiString(const value: AnsiString);
    procedure AddArgAsWideString(const value: WideString);
    procedure AddArgAsShortString(const value: ShortString);
    procedure AddArgAsPAnsiChar(value: PAnsiChar);
{$ENDIF}
    procedure AddArgAsString(const value: String);
{$IFDEF UNIC}
    procedure AddArgAsUnicString(const value: UnicodeString);
{$ENDIF}
    procedure AddArgAsPChar(value: PChar);
    procedure AddArgAsPWideChar(value: PWideChar);
    procedure AddArgAsPointer(value: Pointer);
    procedure AddArgAsRecord(pvalue: Pointer; Size: Integer);
    procedure AddArgAsRecordByVal(var value; Size: Integer);
    procedure AddArgAsArray(pvalue: Pointer; Size: Integer);
    procedure AddArgAsArrayByVal(var value; Size: Integer);
    procedure AddArgAsEvent(var value);
    procedure AddArgAsDynArray(var value);
    procedure AddArgAsObject(value: TObject);
    procedure AddArgAsClassRef(value: TClass);
    procedure AddArgAsVariant(const value: Variant);
    procedure AddArgAsSet(pvalue: Pointer; Size: Integer);
    procedure AddArgAsInterface(const value: IUnknown);

    procedure SetResultAsVoid;
    procedure SetResultAsByte;
    procedure SetResultAsWord;
    procedure SetResultAsCardinal;
    procedure SetResultAsShortInt;
    procedure SetResultAsSmallInt;
    procedure SetResultAsInteger;
    procedure SetResultAsBoolean;
    procedure SetResultAsWordBool;
    procedure SetResultAsLongBool;
    procedure SetResultAsChar;
    procedure SetResultAsWideChar;
    procedure SetResultAsDouble;
    procedure SetResultAsSingle;
    procedure SetResultAsExtended;
    procedure SetResultAsCurrency;
    procedure SetResultAsString(ResAddress: Pointer = nil);
    procedure SetResultAsUnicodeString(ResAddress: Pointer = nil);
{$IFNDEF PAXARM}
    procedure SetResultAsAnsiChar;
    procedure SetResultAsAnsiString(ResAddress: Pointer = nil);
    procedure SetResultAsWideString(ResAddress: Pointer = nil);
    procedure SetResultAsShortString(ResAddress: Pointer = nil);
{$ENDIF}
    procedure SetResultAsPChar;
    procedure SetResultAsPWideChar;
    procedure SetResultAsPointer;
    procedure SetResultAsVariant(ResAddress: Pointer = nil);
    procedure SetResultAsArray(Size: Integer;
                               ResAddress: Pointer = nil);
    procedure SetResultAsDynArray(ResAddress: Pointer = nil);
    procedure SetResultAsRecord(Size: Integer;
                                ResAddress: Pointer = nil);
    procedure SetResultAsSet(Size: Integer;
                             ResAddress: Pointer = nil);
    procedure SetResultAsObject(ResAddress: Pointer = nil);
    procedure SetResultAsClassRef;
    procedure SetResultAsInterface(ResAddress: Pointer = nil);
    procedure SetResultAsEvent(ResAddress: Pointer = nil);
    procedure SetResultAsInt64;
    procedure SetResultAsUInt64;
    procedure CallHost;
    function GetResultPtr: Pointer;
    procedure ClearResult;
    function GetImplementation: Pointer;
    property Address: Pointer read GetAddr write SetAddr;
    property CallConv: Integer read GetCallConv write SetCallConv;
    property This: Pointer read GetThis write SetThis;
    property Fake: Boolean read GetFake write SetFake;
  end;

implementation

constructor TPaxInvoke.Create(AOwner: TComponent);
begin
  inherited;
  base_invoke := TInvoke.Create;
end;

destructor TPaxInvoke.Destroy;
begin
  FreeAndNil(base_invoke);
  inherited;
end;

function TPaxInvoke.GetAddr: Pointer;
begin
  result := base_invoke.Address;
end;

procedure TPaxInvoke.SetAddr(value: Pointer);
begin
  base_invoke.Address := value;
end;

procedure TPaxInvoke.LoadAddress(const DllName, ProcName: String);
begin
  base_invoke.LoadAddress(DllName, ProcName);
end;

procedure TPaxInvoke.ClearArguments;
begin
  base_invoke.ClearArguments;
end;

procedure TPaxInvoke.AddArgAsByte(value: Byte);
begin
  base_invoke.AddArg(value, typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsWord(value: Word);
begin
  base_invoke.AddArg(value, typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsCardinal(value: Cardinal);
begin
{$IFDEF VARIANTS}
  base_invoke.AddArg(value, typeINTEGER);
{$ELSE}
  base_invoke.AddArg(Integer(value), typeINTEGER);
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsShortInt(value: ShortInt);
begin
  base_invoke.AddArg(value, typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsSmallInt(value: SmallInt);
begin
  base_invoke.AddArg(value, typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsInteger(value: Integer);
begin
  base_invoke.AddArg(value, typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsInt64(value: Int64);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeINT64);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(Int64));
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsUInt64(value: UInt64);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeINT64);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(UInt64));
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsBoolean(value: Boolean);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsWordBool(value: WordBool);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsLongBool(value: LongBool);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsChar(value: Char);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsWideChar(value: WideChar);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsDouble(value: Double);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeDOUBLE);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(Double));
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsSingle(value: Single);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeSINGLE);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(Single));
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsExtended(value: Extended);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeEXTENDED);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(Extended));
{$ENDIF}
end;

procedure TPaxInvoke.AddArgAsCurrency(value: Currency);
begin
{$IFDEF PAX64}
  base_invoke.AddArg(value, typeCURRENCY);
{$ELSE}
  base_invoke.AddArgByVal(value, SizeOf(Currency));
{$ENDIF}
end;

{$IFNDEF PAXARM}
procedure TPaxInvoke.AddArgAsPAnsiChar(value: PAnsiChar);
begin
  base_invoke.AddArg(IntPax(value), typePOINTER);
end;
{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsAnsiString(const value: AnsiString);
begin
  base_invoke.AddArg(IntPax(Pointer(value)), typeANSISTRING);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsAnsiString(const value: AnsiString);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;
{$ENDIF}

{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsWideString(const value: WideString);
begin
  base_invoke.AddArg(IntPax(Pointer(value)), typeWIDESTRING);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsWideString(const value: WideString);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;
{$ENDIF}

procedure TPaxInvoke.AddArgAsShortString(const value: ShortString);
begin
  base_invoke.AddArg(Integer(@value), typeINTEGER);
end;
{$ENDIF}

{$IFDEF UNIC}
{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsUnicString(const value: UnicodeString);
begin
  base_invoke.AddArg(IntPax(Pointer(value)), typeUNICSTRING);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsUnicString(const value: UnicodeString);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;
{$ENDIF}
{$ELSE}
{$ENDIF}

{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsString(const value: String);
begin
  base_invoke.AddArg(IntPax(Pointer(value)), typeUNICSTRING);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsString(const value: String);
begin
  base_invoke.AddArg(Integer(value), typeINTEGER);
end;
{$ENDIF}

procedure TPaxInvoke.AddArgAsPChar(value: PChar);
begin
  base_invoke.AddArg(IntPax(value), typePOINTER);
end;

procedure TPaxInvoke.AddArgAsPWideChar(value: PWideChar);
begin
  base_invoke.AddArg(IntPax(value), typePOINTER);
end;

procedure TPaxInvoke.AddArgAsPointer(value: Pointer);
begin
  base_invoke.AddArg(IntPax(value), typePOINTER);
end;

procedure TPaxInvoke.AddArgAsRecord(pvalue: Pointer; Size: Integer);
var
  P: Pointer;
begin
  if Size > SizeOf(IntPax) then
    base_invoke.AddArg(IntPax(pvalue), typeINTEGER)
  else
  begin
    Move(pvalue^, P, Size);
    base_invoke.AddArg(IntPax(P), typeINTEGER);
  end;
end;

procedure TPaxInvoke.AddArgAsRecordByVal(var value; Size: Integer);
begin
  base_invoke.AddArgByVal(value, Size);
end;

procedure TPaxInvoke.AddArgAsArray(pvalue: Pointer; Size: Integer);
begin
  if Size > SizeOf(IntPax) then
    base_invoke.AddArg(IntPax(pvalue), typeINTEGER)
  else
    base_invoke.AddArg(IntPax(pvalue^), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsArrayByVal(var value; Size: Integer);
begin
  base_invoke.AddArgByVal(value, Size);
end;

procedure TPaxInvoke.AddArgAsEvent(var value);
begin
  base_invoke.AddArgByVal(value, SizeOf(TMethod));
end;

{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsDynArray(var value);
begin
  base_invoke.AddArg(Int64(Pointer(value)), typeDYNARRAY);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsDynArray(var value);
var
  P: Pointer;
  H: Integer;
begin
  P := Pointer(value);
  base_invoke.AddArg(IntPax(value), typeINTEGER);

  if P = nil then
    H := 0
  else
  begin
    Dec(Integer(P), SizeOf(Integer));
    H := Integer(P^) - 1;
  end;

  base_invoke.AddArg(H, typeINTEGER);
end;
{$ENDIF}

procedure TPaxInvoke.AddArgAsSet(pvalue: Pointer; Size: Integer);
begin
  if Size > SizeOf(IntPax) then
    base_invoke.AddArg(IntPax(pvalue), typeINTEGER)
  else
    base_invoke.AddArg(IntPax(pvalue^), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsObject(value: TObject);
begin
  base_invoke.AddArg(IntPax(value), typeINTEGER);
end;

procedure TPaxInvoke.AddArgAsClassRef(value: TClass);
begin
  base_invoke.AddArg(IntPax(value), typeINTEGER);
end;

{$IFDEF PAX64}
procedure TPaxInvoke.AddArgAsInterface(const value: IUnknown);
begin
  base_invoke.AddArg(IntPax(Pointer(value)), typeINTERFACE);
end;
{$ELSE}
procedure TPaxInvoke.AddArgAsInterface(const value: IUnknown);
begin
  base_invoke.AddArg(Integer(value), typeINTERFACE);
end;
{$ENDIF}

procedure TPaxInvoke.AddArgAsVariant(const value: Variant);
begin
  base_invoke.AddArg(value, typeVARIANT);
end;

{$IFDEF PAX64}

procedure AssignRDI(P: Pointer); assembler;
asm
  mov RDI, P
end;

procedure TPaxInvoke.CallHost;
var
  P: Pointer;
begin
  P := base_invoke.RunnerParam;

  base_invoke.Setup;

  if P <> nil then
    AssignRDI(P);

  base_invoke.CallHost;
  base_invoke.AdjustResult;
end;
{$ELSE}
{$IFDEF MACOS32}
procedure TPaxInvoke.CallHost;
var
  P: Pointer;
begin
  P := base_invoke.RunnerParam;
  base_invoke.Setup;

  if P <> nil then
  asm
    mov edi, P
  end;

{$IFDEF MACOS}
  case base_invoke.CallConv of
    __ccCDECL: base_invoke.CallHostCDECL;
    __ccSTDCALL: base_invoke.CallHostSTDCALL;
  else
    base_invoke.CallHost;
  end;
{$ELSE}
  base_invoke.CallHost;
{$ENDIF}

  base_invoke.AdjustResult;
end;
{$ELSE}
procedure TPaxInvoke.CallHost;
var
  P: Pointer;
begin
  P := base_invoke.RunnerParam;

  asm
    push esi;
    push edi;
    push ebx;
  end;

  base_invoke.Setup;

  if P <> nil then
  asm
    mov edi, P
  end;

  base_invoke.CallHost;
  base_invoke.AdjustResult;

  asm
    pop ebx;
    pop edi;
    pop esi;
  end;
end;
{$ENDIF}
{$ENDIF}

function TPaxInvoke.GetImplementation: Pointer;
begin
  result := base_invoke;
end;

procedure TPaxInvoke.SetCallConv(value: Integer);
begin
  base_invoke.CallConv := value;
end;

function TPaxInvoke.GetCallConv: Integer;
begin
  result := base_invoke.CallConv;
end;

function TPaxInvoke.GetResultPtr: Pointer;
begin
  result := base_invoke.GetResultPtr;
end;

procedure TPaxInvoke.SetResultAsVoid;
begin
  base_invoke.SetResType(typeVOID);
  base_invoke.SetResSize(SizeOf(Pointer));
end;

procedure TPaxInvoke.SetResultAsByte;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Byte));
end;

procedure TPaxInvoke.SetResultAsWord;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Word));
end;

procedure TPaxInvoke.SetResultAsCardinal;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Cardinal));
end;

procedure TPaxInvoke.SetResultAsShortInt;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(ShortInt));
end;

procedure TPaxInvoke.SetResultAsSmallInt;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(SmallInt));
end;

procedure TPaxInvoke.SetResultAsInteger;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Integer));
end;

procedure TPaxInvoke.SetResultAsBoolean;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Boolean));
end;

procedure TPaxInvoke.SetResultAsWordBool;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(WordBool));
end;

procedure TPaxInvoke.SetResultAsLongBool;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(LongBool));
end;

procedure TPaxInvoke.SetResultAsChar;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Char));
end;

procedure TPaxInvoke.SetResultAsWideChar;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(WideChar));
end;

procedure TPaxInvoke.SetResultAsDouble;
begin
  base_invoke.SetResType(typeDOUBLE);
  base_invoke.SetResSize(SizeOf(Double));
end;

procedure TPaxInvoke.SetResultAsSingle;
begin
  base_invoke.SetResType(typeSINGLE);
  base_invoke.SetResSize(SizeOf(Single));
end;

procedure TPaxInvoke.SetResultAsExtended;
begin
  base_invoke.SetResType(typeEXTENDED);
  base_invoke.SetResSize(SizeOf(Extended));
end;

procedure TPaxInvoke.SetResultAsCurrency;
begin
{$IFDEF PAX64}
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Int64));
{$ELSE}
  base_invoke.SetResType(typeCURRENCY);
  base_invoke.SetResSize(SizeOf(Currency));
{$ENDIF}
end;

procedure TPaxInvoke.SetResultAsEvent(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeEVENT);
  base_invoke.SetResSize(SizeOf(TMethod));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsUnicodeString(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeUNICSTRING);
  base_invoke.SetResSize(SizeOf(String));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsString(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeSTRING);
  base_invoke.SetResSize(SizeOf(String));
  base_invoke.CustomResultAddress := ResAddress;
end;

{$IFNDEF PAXARM}

procedure TPaxInvoke.SetResultAsAnsiChar;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(AnsiChar));
end;

procedure TPaxInvoke.SetResultAsAnsiString(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeSTRING);
  base_invoke.SetResSize(SizeOf(String));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsWideString(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeWIDESTRING);
  base_invoke.SetResSize(SizeOf(WideString));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsShortString(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeSHORTSTRING);
  base_invoke.SetResSize(SizeOf(Pointer));
  base_invoke.CustomResultAddress := ResAddress;
end;
{$ENDIF}

procedure TPaxInvoke.SetResultAsPChar;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Pointer));
end;

procedure TPaxInvoke.SetResultAsPWideChar;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Pointer));
end;

procedure TPaxInvoke.SetResultAsPointer;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Pointer));
end;

procedure TPaxInvoke.SetResultAsVariant(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeVARIANT);
  base_invoke.SetResSize(SizeOf(Pointer));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsSet(Size: Integer;
                                    ResAddress: Pointer = nil);
begin
  if Size > SizeOf(Pointer) then
  begin
    base_invoke.SetResType(typeSET);
    base_invoke.SetResSize(Size);
  end
  else
  begin
    base_invoke.SetResType(typeINTEGER);
    base_invoke.SetResSize(SizeOf(Pointer));
  end;
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsArray(Size: Integer;
                                      ResAddress: Pointer = nil);
begin
  if Size > SizeOf(Pointer) then
  begin
    base_invoke.SetResType(typeARRAY);
    base_invoke.SetResSize(Size);
  end
  else
  begin
    base_invoke.SetResType(typeINTEGER);
    base_invoke.SetResSize(SizeOf(Pointer));
  end;
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsDynArray(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeDYNARRAY);
  base_invoke.SetResSize(SizeOf(Pointer));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsRecord(Size: Integer; ResAddress: Pointer = nil);
begin
  if Size > SizeOf(Pointer) then
  begin
    base_invoke.SetResType(typeRECORD);
    base_invoke.SetResSize(Size);
  end
  else
  begin
    base_invoke.SetResType(typeINTEGER);
    base_invoke.SetResSize(Size);
  end;
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsObject(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeCLASS);
  base_invoke.SetResSize(SizeOf(Pointer));
  base_invoke.CustomResultAddress := ResAddress;
{$IFDEF ARC}
  if ResAddress = nil then
    base_invoke.SetResType(typeINTEGER);
{$ENDIF}
end;

procedure TPaxInvoke.SetResultAsClassRef;
begin
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Pointer));
end;

procedure TPaxInvoke.SetResultAsInterface(ResAddress: Pointer = nil);
begin
  base_invoke.SetResType(typeINTERFACE);
  base_invoke.SetResSize(SizeOf(Pointer));
  base_invoke.CustomResultAddress := ResAddress;
end;

procedure TPaxInvoke.SetResultAsInt64;
begin
{$IFDEF PAX64}
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Int64));
{$ELSE}
  base_invoke.SetResType(typeINT64);
  base_invoke.SetResSize(SizeOf(Int64));
{$ENDIF}
end;

procedure TPaxInvoke.SetResultAsUInt64;
begin
{$IFDEF PAX64}
  base_invoke.SetResType(typeINTEGER);
  base_invoke.SetResSize(SizeOf(Int64));
{$ELSE}
  base_invoke.SetResType(typeUINT64);
  base_invoke.SetResSize(SizeOf(UInt64));
{$ENDIF}
end;

function TPaxInvoke.GetThis: Pointer;
begin
  result := base_invoke.GetThis;
end;

procedure TPaxInvoke.SetThis(value: Pointer);
begin
  base_invoke.SetThis(value);
end;

function TPaxInvoke.GetFake: Boolean;
begin
  result := base_invoke.IsFakeMethod;
end;

procedure TPaxInvoke.SetFake(value: Boolean);
begin
  base_invoke.IsFakeMethod := value;
end;

procedure TPaxInvoke.ClearResult;
begin
  base_invoke.ClearResult;
end;

end.
