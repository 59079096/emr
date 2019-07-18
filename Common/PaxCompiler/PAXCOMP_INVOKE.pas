////////////////////////////////////////////////////////////////////////////
// PaxInvoke
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_INVOKE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_INVOKE;
interface
{$IFDEF PAXARM_DEVICE}
implementation
end.
{$ENDIF}

uses
  {$I uses.def}
  PAXCOMP_CONSTANTS,
  SysUtils,
  Classes;

const
  MaxArgs = 20;
  typeVALVALUE = 52;

{$IFDEF MACOS32}
  varValValue = varError;
{$ELSE}
  varValValue = $0E;
{$ENDIF}

{$IFDEF PAX64}
  INVOKE_RESULT_OFFSET = 152;
  INVOKE_ADDRESS_OFFSET = 64;
{$ELSE}
  INVOKE_RESULT_OFFSET = 36;
{$ENDIF}

type
  TIntegerArr = array[0..4096] of Integer;
  PIntegerArr = ^TIntegerArr;

  TValValue = class
  private
    fBuff: PIntegerArr;
    fSize: Integer;
  public
    constructor Create(var V; iSize: Integer);
    destructor Destroy; override;
    procedure Push(P: Pointer);
    property Size: Integer read fSize;
  end;

  TInvoke = class
  private
{$IFDEF PAX64}
    fStackFrame: Pointer; //8
    fStackSize: Integer; //16
    dummy: Integer;
    fEAX: IntPax; //24
    fEDX: IntPax; //32
    fECX: IntPax; //40
    fR8: IntPax; //48
    fR9: IntPax; //56
    fAddress: Pointer; // 64
    fCallConv: IntPax; // 72
    fResultType: IntPax; // 80
    fXMM0: Double; // 88
    fXMM1: Double; // 96
    fXMM2: Double; // 104
    fXMM3: Double; // 112

    XMM0type: IntPax; // 120
    XMM1type: IntPax; // 128
    XMM2type: IntPax; // 136
    XMM3type: IntPax; // 144

    fResult: array[0..SizeOf(Variant) - 1] of Byte; // 152

{$ELSE}
    fStackFrame: Pointer; //4
    fStackSize: Integer; //8

    fEAX: IntPax; //12
    fEDX: IntPax; //16
    fECX: IntPax; //20
    fAddress: Pointer; // 24
    fCallConv: Integer; // 28
    fResultType: Integer; // 32
    fResult: array[0..SizeOf(Variant) - 1] of Byte; // 36
{$ENDIF}

    fResultSize: Integer;

    fCount: Integer;
    NeedSetup: Boolean;
    A: array of Variant;
    Types: array of Integer;
    DllList: TStringList;
    fIsInternal: Boolean;
  public
    This: Pointer;
    OldESP0: Integer;
    CustomResultAddress: Pointer;
    IsFakeMethod: Boolean;
    IsConstructor: Boolean;
    RunnerParam: Pointer;

    Outer: Pointer;

    constructor Create;
    constructor CreateInternal;
    destructor Destroy; override;
    function IsInternal: Boolean;
    procedure ClearArguments;
    procedure PushArguments;
    procedure PushArgumentsBackward;

    procedure AddArg(const value: Variant; T: Integer);
    procedure AddArgByVal(var V; Size: Integer);

    property ArgumentCount: Integer read fCount;
    procedure Setup;
    procedure CallHost;
{$IFDEF MACOS}
    procedure CallHostCDECL;
    procedure CallHostSTDCALL;
{$ENDIF}
    function GetResultPtr: Pointer;
    procedure SetResType(value : Integer);
    procedure SetResSize(value : Integer);
    function ExtraParamNeeded: Boolean;
    function GetThis: Pointer;
    procedure SetThis(value: Pointer);
    procedure LoadAddress(const DllName, ProcName: String);
    procedure UloadDlls;
    procedure ClearResult;
{$IFDEF PAX64}
    procedure SaveResult;
{$ENDIF}
    procedure AdjustResult;
    procedure RaiseError(const Message: string; params: array of Const);
    property CallConv: IntPax read fCallConv write fCallConv;
    property Address: Pointer read fAddress write fAddress;
    property StackFrame: Pointer read fStackFrame write fStackFrame;
    property StackSize: Integer read fStackSize write fStackSize;
    property ResultType: IntPax read fResultType;
    property _EAX: IntPax read fEAX write fEAX;
    property _ECX: IntPax read fECX write fECX;
    property _EDX: IntPax read fEDX write fEDX;
{$IFDEF PAX64}
    property _R8: IntPax read fR8 write fR8;
    property _R9: IntPax read fR9 write fR9;
{$ENDIF}
  end;

var
  ARR_R1, ARR_R2, ARR_R3: array[0..1023] of Integer;

implementation

uses
  PAXCOMP_SYS;

{$IFDEF MACOS32}
var
  STACKFRAME_OFFSET,
  STACKSIZE_OFFSET,
  RAX_OFFSET,
  RCX_OFFSET,
  RDX_OFFSET,
  ADDRESS_OFFSET,
  CALLCONV_OFFSET,
  RESULTTYPE_OFFSET,
  RESULT_OFFSET: Integer;
{$ENDIF}

function VariantToValValue(const V: Variant): TValValue;
begin
  result := TValValue(TVarData(V).VInteger);
end;

function ValValueToVariant(X: TValValue): Variant;
begin
  with TVarData(result) do
  begin
    VType := varValValue;
    VInteger := Integer(X);
  end;
end;

procedure ClearVariant(var V: Variant);
var
  X: TValValue;
begin
  if VarType(V) = varValValue then
  with TVarData(V) do
  begin
    X := TValValue(VInteger);
    FreeAndNil(X);
    VType := varInteger;
  end;

  VarClear(V);
end;

constructor TValValue.Create(var V; iSize: Integer);
begin
  inherited Create;
  fSize := iSize;
  while fSize mod 4 <> 0 do
    Inc(fSize);
  fBuff := AllocMem(fSize);
  Move(V, fBuff^, fSize);
end;

destructor TValValue.Destroy;
begin
  FreeMem(fBuff, fSize);
  inherited;
end;

{$IFDEF PAX64}
procedure TValValue.Push(P: Pointer);
var
  I, K: Integer;
begin
  K := fSize div 8;

  for I:=K - 1 downto 0 do
  begin
    IntPax(P^) := fBuff^[I];
    Inc(IntPax(P), SizeOf(IntPax));
  end;
end;
{$ELSE}
procedure TValValue.Push(P: Pointer);
var
  I, K: Integer;
begin
  K := fSize div 4;

  for I:=K - 1 downto 0 do
  begin
    Integer(P^) := fBuff^[I];
    Inc(Integer(P), SizeOf(Integer));
  end;
end;
{$ENDIF}

constructor TInvoke.Create;
begin
  inherited;
  fCount := 0;
  SetLength(A, MaxArgs);
  SetLength(Types, MaxArgs);
  fCallConv := ccSTDCALL;
  fStackFrame := AllocMem(MaxArgs * 16);
  fAddress := nil;
  This := nil;
  NeedSetup := true;
  DllList := TStringList.Create;
  fIsInternal := false;
end;

constructor TInvoke.CreateInternal;
begin
  inherited;
  fCount := 0;
{$IFDEF PAX64}
  fCallConv := cc64;
{$ELSE}
  fCallConv := ccREGISTER;
{$ENDIF}
  fStackFrame := nil;
  fAddress := nil;
  This := nil;
  NeedSetup := false;
  fIsInternal := true;
end;

destructor TInvoke.Destroy;
begin
  if IsInternal then
    Exit;

  ClearArguments;

  UloadDlls;
  FreeAndNil(DllList);
  FreeMem(fStackFrame, MaxArgs * 16);
  inherited;
end;

procedure TInvoke.ClearArguments;
var
  I: Integer;
begin
  CustomResultAddress := nil;
  for I:=0 to fCount - 1 do
    if Types[I] = typeVALVALUE then
      ClearVariant(A[I])
    else
      VarClear(A[I]);

  fCount := 0;
  NeedSetup := true;
end;

procedure TInvoke.AddArg(const value: Variant; T: Integer);
begin
  A[fCount] := value;
  Types[fCount] := T;
  Inc(fCount);
  NeedSetup := true;
end;

procedure TInvoke.AddArgByVal(var V; Size: Integer);
var
  VV: TValValue;
begin
  VV := TValValue.Create(V, Size);
{$IFDEF ARC}
  VV.__ObjAddRef;
{$ENDIF}
  A[fCount] := ValValueToVariant(VV);
  Types[fCount] := typeVALVALUE;
  Inc(fCount);
  NeedSetup := true;
end;

procedure TInvoke.Setup;
var
  I, T: Integer;
  P: Pointer;
  EAXbusy, EDXbusy, ECXbusy: Boolean;
{$IFDEF PAX64}
  R8busy, R9busy: Boolean;
{$ENDIF}
  IDX, ICX: Integer;
  X: TValValue;
begin
  if not NeedSetup then
    Exit;

  EAXbusy := false;
  EDXbusy := false;
  ECXbusy := false;
{$IFDEF PAX64}
  R8busy := false;
  R9busy := false;
  XMM0type := 0;
  XMM1type := 0;
  XMM2type := 0;
  XMM3type := 0;
{$ENDIF}
  P := fStackFrame;
  fStackSize := 0;

{$IFDEF PAX64}
  fCallConv := cc64;
{$ENDIF}

  if ExtraParamNeeded and (fCallConv = cc64) then
  begin
    FillChar(fResult, SizeOf(fResult), 0);
    Pointer(P^) := GetResultPtr;

{$IFDEF PAX64}
    if XMM0type = 0 then
    begin
      XMM0type := 1;
    end
    else if XMM1type = 0 then
    begin
      XMM1type := 1;
    end
    else if XMM2type = 0 then
    begin
      XMM2type := 1;
    end
    else if XMM3type = 0 then
    begin
      XMM3type := 1;
    end;
{$ENDIF}

    if IsFakeMethod then
    begin
      fECX := IntPax(GetResultPtr);
      ECXbusy := true;
    end
    else
    begin
      if This <> nil then
      begin
        fEDX := IntPax(GetResultPtr);
        EDXbusy := true;
      end
      else
      begin
        fECX := IntPax(GetResultPtr);
        ECXbusy := true;
      end;
    end;

  end;

  case fCallConv of
    ccSAFECALL:
    begin
      Pointer(P^) := GetResultPtr;
      I := 0;
      Move(I, fResult, SizeOf(Integer));
      Inc(IntPax(P), SizeOf(Integer));
      Inc(fStackSize, SizeOf(Integer));

      for I:= fCount - 1 downto 0 do
      begin
        T := Types[I];
        case T of
          typeINTEGER, typePOINTER, typeINTERFACE:
          begin
            Integer(P^) := Integer(A[I]);
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          typeVALVALUE:
          begin
            X := VariantToValValue(A[I]);
            X.Push(P);
            Inc(IntPax(P), X.Size);
            Inc(fStackSize, X.Size);
          end;
          typeVARIANT:
          begin
            Pointer(P^) := @ A[I];
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;
    end; // ccSAFECALL

    ccSTDCALL, ccCDECL:
    begin
      for I:= fCount - 1 downto 0 do
      begin
        T := Types[I];
        case T of
          typeINTEGER, typePOINTER, typeINTERFACE:
          begin
            Integer(P^) := Integer(A[I]);
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          typeVALVALUE:
          begin
            X := VariantToValValue(A[I]);
            X.Push(P);
            Inc(IntPax(P), X.Size);
            Inc(fStackSize, X.Size);
          end;
          typeVARIANT:
          begin
            Pointer(P^) := @ A[I];
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;
    end; // ccSTDCALL, ccCDECL

    ccMSFASTCALL:
    begin
      EAXbusy := true;
      IDX := -1;
      ICX := -1;

      if fResultType in [typeRECORD, typeARRAY] then
      begin
        if fResultSize <= 4 then
        begin
          fResultType := typeINTEGER;
        end
        else if fResultSize <= 8 then
        begin
          fResultType := typeINT64;
        end;
      end;

      for I:= 0 to fCount - 1 do
      begin
        if Types[I] in [typeINTEGER, typePOINTER, typeINTERFACE] then
        begin
          if not ECXbusy then
          begin
            fECX := Integer(A[I]);
            ECXbusy := true;
            ICX := I;
          end
          else if not EDXbusy then
          begin
            fEDX := Integer(A[I]);
            EDXbusy := true;
            IDX := I;
          end
        end;
      end;

      for I:= fCount - 1 downto 0 do
      begin
        if I = IDX then
          continue;
        if I = ICX then
          continue;

        T := Types[I];
        case T of
          typeINTEGER, typePOINTER, typeINTERFACE:
          begin
            Integer(P^) := Integer(A[I]);
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          typeVALVALUE:
          begin
            X := VariantToValValue(A[I]);
            X.Push(P);
            Inc(IntPax(P), X.Size);
            Inc(fStackSize, X.Size);
          end;
          typeVARIANT:
          begin
            Pointer(P^) := @ A[I];
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;
    end; // ccMSFASTCALL

    ccPASCAL:
    begin
      for I:= 0 to fCount - 1 do
      begin
        T := Types[I];
        case T of
          typeINTEGER, typePOINTER, typeINTERFACE:
          begin
            Integer(P^) := Integer(A[I]);
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          typeVALVALUE:
          begin
            X := VariantToValValue(A[I]);
            X.Push(P);
            Inc(IntPax(P), X.Size);
            Inc(fStackSize, X.Size);
          end;
          typeVARIANT:
          begin
            Pointer(P^) := @ A[I];
            Inc(IntPax(P), SizeOf(Integer));
            Inc(fStackSize, SizeOf(Integer));
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;
    end; // ccPASCAL

    ccREGISTER:
    begin

      if This <> nil then
      begin
{$IFDEF FPC}
        if IsConstructor then
        begin
          fEDX := Integer(This);
          EDXbusy := true;
        end
        else
        begin
          fEAX := Integer(This);
          EAXbusy := true;
        end;
{$ELSE}
        fEAX := Integer(This);
        EAXbusy := true;
{$ENDIF}
      end;

      for I:=0 to fCount - 1 do
      begin
        T := Types[I];
        case T of
          typeINTEGER, typePOINTER, typeINTERFACE:
          begin
            if not EAXbusy then
            begin
              fEAX := Integer(A[I]);
              EAXbusy := true;
            end
            else if not EDXbusy then
            begin
              fEDX := Integer(A[I]);
              EDXbusy := true;
            end
            else if not ECXbusy then
            begin
              fECX := Integer(A[I]);
              ECXbusy := true;
            end
            else
            begin
              Integer(P^) := Integer(A[I]);
              Inc(IntPax(P), SizeOf(Integer));
              Inc(fStackSize, SizeOf(Integer));
            end;
          end;
          typeVALVALUE:
          begin
            X := VariantToValValue(A[I]);
            X.Push(P);
            Inc(IntPax(P), X.Size);
            Inc(fStackSize, X.Size);
          end;
          typeVARIANT:
          begin
            if not EAXbusy then
            begin
              fEAX := IntPax(@ A[I]);
              EAXbusy := true;
            end
            else if not EDXbusy then
            begin
              fEDX := IntPax(@ A[I]);
              EDXbusy := true;
            end
            else if not ECXbusy then
            begin
              fECX := IntPax(@ A[I]);
              ECXbusy := true;
            end
            else
            begin
              Pointer(P^) := @ A[I];
              Inc(IntPax(P), SizeOf(Integer));
              Inc(fStackSize, SizeOf(Integer));
            end;
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;
    end; // ccREGISTER
    cc64:
    begin
      if This <> nil then
      begin
{$IFDEF PAX64}
        if XMM0type = 0 then
        begin
          XMM0type := 1;
        end
        else if XMM1type = 0 then
        begin
          XMM1type := 1;
        end
        else if XMM2type = 0 then
        begin
          XMM2type := 1;
        end
        else if XMM3type = 0 then
        begin
          XMM3type := 1;
        end;
{$ENDIF}

{$IFDEF FPC}
        if IsConstructor then
        begin
          fEDX := IntPax(This);
          EDXbusy := true;
        end
        else
        begin
          if not ECXbusy then
          begin
            fECX := IntPax(This);
            ECXbusy := true;
          end
          else if not EDXbusy then
          begin
            fEDX := IntPax(This);
            EDXbusy := true;
          end
          else
            RaiseError(errInternalError, []);
        end;
{$ELSE}
        if not ECXbusy then
        begin
          fECX := IntPax(This);
          ECXbusy := true;
        end
        else if not EDXbusy then
        begin
          fEDX := IntPax(This);
          EDXbusy := true;
        end
        else
          RaiseError(errInternalError, []);
{$ENDIF}
      end;

      for I:=0 to fCount - 1 do
      begin
        T := Types[I];
        case T of
          typeINTEGER,
          typePOINTER,
{$IFNDEF PAXARM}
          typeANSISTRING,
          typeWIDESTRING,
{$ENDIF}
          typeUNICSTRING,
          typeCLASS,
          typeCLASSREF,
          typeINT64,
          typeUINT64,
          typeCURRENCY,
          typeINTERFACE:
          begin
{$IFDEF PAX64}
            if XMM0type = 0 then
            begin
              XMM0type := 1;
            end
            else if XMM1type = 0 then
            begin
              XMM1type := 1;
            end
            else if XMM2type = 0 then
            begin
              XMM2type := 1;
            end
            else if XMM3type = 0 then
            begin
              XMM3type := 1;
            end;
{$ENDIF}
            if not ECXbusy then
            begin
              fECX := IntPax(A[I]);
              ECXbusy := true;
            end
            else if not EDXbusy then
            begin
              fEDX := IntPax(A[I]);
              EDXbusy := true;
            end
{$IFDEF PAX64}
            else if not R8busy then
            begin
              fR8 := IntPax(A[I]);
              R8busy := true;
            end
            else if not R9busy then
            begin
              fR9 := IntPax(A[I]);
              R9busy := true;
            end
{$ENDIF}
            else
            begin
              Byte(P^) := typeINTEGER;
              Inc(IntPax(P), 1);

              IntPax(P^) := IntPax(A[I]);
              Inc(IntPax(P), SizeOf(IntPax));
              Inc(fStackSize, SizeOf(IntPax));
            end;
          end;
          typeDOUBLE, typeSINGLE, typeEXTENDED:
          begin
{$IFDEF PAX64}

            if not ECXbusy then
            begin
              ECXbusy := true;
            end
            else if not EDXbusy then
            begin
              EDXbusy := true;
            end
            else if not R8busy then
            begin
              R8busy := true;
            end
            else if not R9busy then
            begin
              R9busy := true;
            end;

            if XMM0type = 0 then
            begin
              fXMM0 := Double(A[I]);
              XMM0type := T;
            end
            else if XMM1type = 0 then
            begin
              fXMM1 := Double(A[I]);
              XMM1type := T;
            end
            else if XMM2type = 0 then
            begin
              fXMM2 := Double(A[I]);
              XMM2type := T;
            end
            else if XMM3type = 0 then
            begin
              fXMM3 := Double(A[I]);
              XMM3type := T;
            end
            else
            begin
              Byte(P^) := T;
              Inc(IntPax(P), 1);

              Double(P^) := Double(A[I]);
              Inc(IntPax(P), SizeOf(Double));
              Inc(fStackSize, SizeOf(Double));
            end;
{$ENDIF}
          end;
          typeVALVALUE:
          begin
{$IFDEF PAX64}
            if XMM0type = 0 then
            begin
              XMM0type := 1;
            end
            else if XMM1type = 0 then
            begin
              XMM1type := 1;
            end
            else if XMM2type = 0 then
            begin
              XMM2type := 1;
            end
            else if XMM3type = 0 then
            begin
              XMM3type := 1;
            end;
{$ENDIF}
            X := VariantToValValue(A[I]);
            if not ECXbusy then
            begin
              fECX := IntPax(X.fBuff);
              ECXbusy := true;
            end
            else if not EDXbusy then
            begin
              fEDX := IntPax(X.fBuff);
              EDXbusy := true;
            end
{$IFDEF PAX64}
            else if not R8busy then
            begin
              fR8 := IntPax(X.fBuff);
              R8busy := true;
            end
            else if not R9busy then
            begin
              fR9 := IntPax(X.fBuff);
              R9busy := true;
            end
{$ENDIF}
            else
            begin
              Byte(P^) := typeVARIANT;
              Inc(IntPax(P), 1);

              IntPax(P^) := IntPax(X.fBuff);
              Inc(IntPax(P), SizeOf(Pointer));
              Inc(fStackSize, SizeOf(Pointer));
            end;
          end;
          typeVARIANT:
          begin
{$IFDEF PAX64}
            if XMM0type = 0 then
            begin
              XMM0type := 1;
            end
            else if XMM1type = 0 then
            begin
              XMM1type := 1;
            end
            else if XMM2type = 0 then
            begin
              XMM2type := 1;
            end
            else if XMM3type = 0 then
            begin
              XMM3type := 1;
            end;
{$ENDIF}
            if not ECXbusy then
            begin
              fECX := IntPax(@A[I]);
              ECXbusy := true;
            end
            else if not EDXbusy then
            begin
              fEDX := IntPax(@A[I]);
              EDXbusy := true;
            end
{$IFDEF PAX64}
            else if not R8busy then
            begin
              fR8 := IntPax(@A[I]);
              R8busy := true;
            end
            else if not R9busy then
            begin
              fR9 := IntPax(@A[I]);
              R9busy := true;
            end
{$ENDIF}
            else
            begin
              Byte(P^) := typeVARIANT;
              Inc(IntPax(P), 1);

              IntPax(P^) := IntPax(@A[I]);
              Inc(IntPax(P), SizeOf(Pointer));
              Inc(fStackSize, SizeOf(Pointer));
            end;
          end;
          else
            RaiseError(errInternalError, []);
        end;
      end;

    end; // c64
  end;

  NeedSetup := false;

  if This <> nil then
  if fCallConv in [ccSTDCALL, ccCDECL, ccPASCAL, ccSAFECALL] then
  begin
    Pointer(P^) := This;
    Inc(IntPax(P), SizeOf(Pointer));
    Inc(fStackSize, SizeOf(Pointer));
  end;

  if fCallConv = ccSAFECALL then
    Exit;

  if ExtraParamNeeded and (fCallConv <> cc64) then
  begin
    FillChar(fResult, SizeOf(fResult), 0);
    Pointer(P^) := GetResultPtr;

    if fCallConv = ccREGISTER then
    begin
      if not EAXbusy then
        fEAX := Integer(GetResultPtr)
      else if not EDXbusy then
        fEDX := Integer(GetResultPtr)
      else if not ECXbusy then
        fECX := Integer(GetResultPtr)
      else
        Inc(fStackSize, SizeOf(Pointer));
    end
    else
      Inc(fStackSize, SizeOf(Pointer));
  end;

{$IFNDEF ARC}
  if fResultType = typeCLASS then
    fResultType := typeINTEGER;
{$ENDIF}
  if fResultType = typeUINT64 then
    fResultType := typeINT64;
end;

{$IFDEF PAX64}
procedure TInvoke.PushArguments; assembler;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}

  pop rbx // ret address

  mov r10, rcx // Self

  mov rdx, [r10 + 8] // fStackFrame
  mov rcx, [r10 + 16] // fStackSize

  cmp rcx, 0
  jz @@Next

  mov r14, rsp
  add r14, $20

  @@loop:

  cmp byte ptr[rdx], typeDOUBLE
  jz @@push_double
  cmp byte ptr[rdx], typeEXTENDED
  jz @@push_double
  cmp byte ptr[rdx], typeSINGLE
  jz @@push_single

  add rdx, 1
  mov r11, [rdx]
  mov [r14], r11
  jmp @@next_param

@@push_double:
  add rdx, 1
  movsd xmm4, [rdx]
  movsd [r14], xmm4
  jmp @@next_param

@@push_single:
  add rdx, 1
  cvtsd2ss xmm4, [rdx]
  movss [r14], xmm4
  jmp @@next_param

@@next_param:

  add rdx, 8 ////////////
  add r14, 8
  sub rcx, 8
  jnz @@loop

  @@Next:

  mov rax, [r10 + 120]
  cmp rax, typeDOUBLE
  jz @@mov_double0
  cmp rax, typeEXTENDED
  jz @@mov_double0
  cmp rax, typeSINGLE
  jz @@mov_single0

  jmp @@mov_1

  @@mov_double0:
  movsd xmm0, [r10 + 88]
  jmp @@mov_1

  @@mov_single0:
  cvtsd2ss xmm0, [r10 + 88]
  jmp @@mov_1

  @@mov_1:

  mov rax, [r10 + 128]
  cmp rax, typeDOUBLE
  jz @@mov_double1
  cmp rax, typeEXTENDED
  jz @@mov_double1
  cmp rax, typeSINGLE
  jz @@mov_single1

  jmp @@mov_2

  @@mov_double1:
  movsd xmm1, [r10 + 96]
  jmp @@mov_2

  @@mov_single1:
  cvtsd2ss xmm1, [r10 + 96]
  jmp @@mov_2

  @@mov_2:

  mov rax, [r10 + 136]
  cmp rax, typeDOUBLE
  jz @@mov_double2
  cmp rax, typeEXTENDED
  jz @@mov_double2
  cmp rax, typeSINGLE
  jz @@mov_single2

  jmp @@mov_3

  @@mov_double2:
  movsd xmm2, [r10 + 104]
  jmp @@mov_3

  @@mov_single2:
  cvtsd2ss xmm2, [r10 + 104]
  jmp @@mov_3

  @@mov_3:

  mov rax, [r10 + 144]
  cmp rax, typeDOUBLE
  jz @@mov_double3
  cmp rax, typeEXTENDED
  jz @@mov_double3
  cmp rax, typeSINGLE
  jz @@mov_single3

  jmp @@mov_4

  @@mov_double3:
  movsd xmm3, [r10 + 112]
  jmp @@mov_4

  @@mov_single3:
  cvtsd2ss xmm3, [r10 + 112]
  jmp @@mov_4

  @@mov_4:

  mov rcx, [r10 + 40]
  mov rdx, [r10 + 32]
  mov rax, [r10 + 24]
  mov r8,  [r10 + 48]
  mov r9,  [r10 + 56]

  jmp rbx
end;
{$ELSE}
procedure TInvoke.PushArguments;
asm
  pop ebx // ret address

  mov edx, eax // Self
  mov ecx, [edx + 8] // fStackSize
  mov edx, [edx + 4] // fStackFrame

  cmp ecx, 0
  jz @@Next

  @@loop:
  mov esi, [edx]
  push esi
  add edx, 4 ////////////
  sub ecx, 4
  jnz @@loop

  @@Next:

  mov ecx, [eax + 20]
  mov edx, [eax + 16]
  mov eax, [eax + 12]

  jmp ebx
end;
{$ENDIF}

{$IFDEF PAX64}
procedure TInvoke.PushArgumentsBackward;
{$IFDEF FPC}
nostackframe;
{$ENDIF}
asm
  pop rbx // ret address

  mov r10, rcx // Self

  mov rdx, [r10 + 8] // fStackFrame
  mov rcx, [r10 + 16] // fStackSize

  cmp rcx, 0
  jz @@Next

  mov r14, rsp
  add r14, $20

  @@loop:

  cmp byte ptr[rdx], typeDOUBLE
  jz @@push_double
  cmp byte ptr[rdx], typeEXTENDED
  jz @@push_double
  cmp byte ptr[rdx], typeSINGLE
  jz @@push_single

  add rdx, 1
  mov r11, [rdx]
  mov [r14], r11
  jmp @@next_param

@@push_double:
  add rdx, 1
  movsd xmm4, [rdx]
  movsd [r14], xmm4
  jmp @@next_param

@@push_single:
  add rdx, 1
  cvtsd2ss xmm4, [rdx]
  movss [r14], xmm4
  jmp @@next_param

@@next_param:

  sub rdx, 8 ////////////
  add r14, 8
  sub rcx, 8
  jnz @@loop

  @@Next:

  mov rax, [r10 + 120]
  cmp rax, typeDOUBLE
  jz @@mov_double0
  cmp rax, typeEXTENDED
  jz @@mov_double0
  cmp rax, typeSINGLE
  jz @@mov_single0

  jmp @@mov_1

  @@mov_double0:
  movsd xmm0, [r10 + 88]
  jmp @@mov_1

  @@mov_single0:
  cvtsd2ss xmm0, [r10 + 88]
  jmp @@mov_1

  @@mov_1:

  mov rax, [r10 + 128]
  cmp rax, typeDOUBLE
  jz @@mov_double1
  cmp rax, typeEXTENDED
  jz @@mov_double1
  cmp rax, typeSINGLE
  jz @@mov_single1

  jmp @@mov_2

  @@mov_double1:
  movsd xmm1, [r10 + 96]
  jmp @@mov_2

  @@mov_single1:
  cvtsd2ss xmm1, [r10 + 96]
  jmp @@mov_2

  @@mov_2:

  mov rax, [r10 + 136]
  cmp rax, typeDOUBLE
  jz @@mov_double2
  cmp rax, typeEXTENDED
  jz @@mov_double2
  cmp rax, typeSINGLE
  jz @@mov_single2

  jmp @@mov_3

  @@mov_double2:
  movsd xmm2, [r10 + 104]
  jmp @@mov_3

  @@mov_single2:
  cvtsd2ss xmm2, [r10 + 104]
  jmp @@mov_3

  @@mov_3:

  mov rax, [r10 + 144]
  cmp rax, typeDOUBLE
  jz @@mov_double3
  cmp rax, typeEXTENDED
  jz @@mov_double3
  cmp rax, typeSINGLE
  jz @@mov_single3

  jmp @@mov_4

  @@mov_double3:
  movsd xmm3, [r10 + 112]
  jmp @@mov_4

  @@mov_single3:
  cvtsd2ss xmm3, [r10 + 112]
  jmp @@mov_4

  @@mov_4:

  mov rcx, [r10 + 40]
  mov rdx, [r10 + 32]
  mov rax, [r10 + 24]
  mov r8,  [r10 + 48]
  mov r9,  [r10 + 56]

  jmp rbx
end;

{$ELSE}

procedure TInvoke.PushArgumentsBackward;
asm
  pop ebx // ret address

  mov edx, eax // Self
  mov ecx, [edx + 8] // fStackSize
  mov edx, [edx + 4] // fStackFrame

  cmp ecx, 0
  jz @@Next

  @@loop:
  mov esi, [edx]
  push esi
  sub edx, 4 ////////////
  sub ecx, 4
  jnz @@loop

  @@Next:

  mov ecx, [eax + 20]
  mov edx, [eax + 16]
  mov eax, [eax + 12]

  jmp ebx
end;
{$ENDIF}

{$O-}

{$IFDEF PAX64}
procedure TInvoke.CallHost;
{$IFDEF FPC}
nostackframe; asm
{$ELSE}
asm
 .NOFRAME
{$ENDIF}
  push rbp
  push rdi
  push rsi
  push rbx
  push r10
  push r11
  push r14
  push r15
  sub rsp, $108
  mov rbp, rsp

  mov r14, rcx
  mov [rsp + $100 - 8], rcx
  mov [rsp + $100 - 16], r14

  call TInvoke.PushArguments
  mov r14, [rsp + $100 - 16]

  call [r14 + INVOKE_ADDRESS_OFFSET]

  mov rcx, [rsp + $100 - 8]
  call TInvoke.SaveResult

  add rsp, $108
  pop r15
  pop r14
  pop r11
  pop r10
  pop rbx
  pop rsi
  pop rdi
  pop rbp
  ret
end;

procedure TInvoke.SaveResult; assembler;
asm
  mov r10, rcx

  cmp r10, 0
  jz @@Return

  mov rcx, [r10 + 80] // fResultType

  cmp rcx, typeINTEGER
  jnz @@RetUINT64
  mov [r10 + INVOKE_RESULT_OFFSET], rax
  jmp @@Return

  @@RetUINT64:
  cmp rcx, typeUINT64
  jnz @@RetINT64
  mov [r10 + INVOKE_RESULT_OFFSET], rax
  jmp @@Return

  @@RetINT64:
  cmp rcx, typeINT64
  jnz @@RetDOUBLE
  mov [r10 + INVOKE_RESULT_OFFSET], rax
  jmp @@Return

  @@RetDOUBLE:
  cmp rcx, typeDOUBLE
  jnz @@RetEXTENDED
  movsd qword ptr [r10 + INVOKE_RESULT_OFFSET], xmm0
  jmp @@Return

  @@RetEXTENDED:
  cmp rcx, typeEXTENDED
  jnz @@RetSINGLE
  movsd qword ptr [r10 + INVOKE_RESULT_OFFSET], xmm0
  jmp @@Return

  @@RetSINGLE:
  movss dword ptr [r10 + INVOKE_RESULT_OFFSET], xmm0
  jmp @@Return

  @@Return:
end;

{$ELSE}

{$IFDEF MACOS32}

procedure TInvoke.CallHostCDECL; assembler;
asm
  push ebp

  push ebx
  push esi
  push edi

  mov ebp, esp
  sub esp, $20

  mov [ebp - 4], eax // save self

  add esp,-$0c
  call SysInit.@GetCallerEIP
  add esp, $0c

  mov edx, eax        // edx := Self
  add edx, STACKSIZE_OFFSET
  mov ecx, [edx]
  mov [ebp - 16], ecx // save fStackSize

  mov edx, eax // edx := Self
  add edx, ADDRESS_OFFSET
  mov edx, [edx]
  mov [ebp - 20], edx // save address of function

  mov edx, eax // edx := Self
  add edx, STACKFRAME_OFFSET
  mov edx, [edx] // fStackFrameOffset
  mov [ebp - 24], edx // save stack fStackFrameOffset

  lea esi, ARR_R1
  mov eax, [esi]
  add eax, ecx
  mov edx, [eax]

  lea esi, ARR_R2
  mov eax, [esi]
  add eax, ecx
  mov eax, [eax]

  push edx
  push ebp
  push $beeffeed
  sub esp, eax

  @@push:
  cmp ecx, 0
  jz @@Call

  mov edx, [ebp - 24]

  @@loop:
  mov esi, [edx]
  push esi
  add edx, 4
  sub ecx, 4
  jnz @@loop

  @@Call:

  mov edx, [ebp - 20] // restore address of function
  call edx

  mov [ebp - 8],  eax // save eax
  mov [ebp - 12], edx // save edx

  mov ecx, [ebp - 16] // restore fStackSize

  lea esi, ARR_R3
  mov eax, [esi]
  add eax, ecx
  mov eax, [eax]

  add esp, eax
  inc byte ptr [esp-$0c]

  @@result:

  mov eax, [ebp - 4] // restore Self

  mov edx, eax // Self
  add edx, RESULTTYPE_OFFSET
  mov ecx, [edx] // fResultType

  cmp ecx, typeINTEGER
  jnz @@RetINT64
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  mov eax, [ebp - 8]
  mov [edx], eax
  jmp @@Return

  @@RetINT64:
  cmp ecx, typeINT64
  jnz @@RetDOUBLE
  mov ecx, eax // Self
  add ecx, RESULT_OFFSET
  mov eax, [ebp - 8]
  mov edx, [ebp - 12]
  mov [ecx], eax
  mov [ecx + 4], edx
  jmp @@Return

  @@RetDOUBLE:
  cmp ecx, typeDOUBLE
  jnz @@RetSINGLE
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp qword ptr [edx]
  jmp @@Return

  @@RetSINGLE:
  cmp ecx, typeSINGLE
  jnz @@RetEXTENDED
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp dword ptr [edx]
  jmp @@Return

  @@RetEXTENDED:
  cmp ecx, typeEXTENDED
  jnz @@RetCURRENCY
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp tbyte ptr [edx]
  jmp @@Return

  @@RetCURRENCY:
  cmp ecx, typeCURRENCY
  jnz @@Return
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fistp qword ptr [edx]
  jmp @@Return

  @@Return:
  mov eax, [ebp - 8]  // retore eax
  mov edx, [ebp - 12] // retore eax

  add esp, $20

  pop edi
  pop esi
  pop ebx

  pop ebp
  ret
end;

procedure TInvoke.CallHostSTDCALL; assembler;
asm
  push ebp

  push ebx
  push esi
  push edi

  mov ebp, esp
  sub esp, $20

  mov [ebp - 4], eax // save self

  add esp,-$0c
  call SysInit.@GetCallerEIP
  add esp, $0c

  mov edx, eax        // edx := Self
  add edx, STACKSIZE_OFFSET
  mov ecx, [edx]
  mov [ebp - 16], ecx // save fStackSize

  mov edx, eax // edx := Self
  add edx, ADDRESS_OFFSET
  mov edx, [edx]
  mov [ebp - 20], edx // save address of function

  mov edx, eax // edx := Self
  add edx, STACKFRAME_OFFSET
  mov edx, [edx] // fStackFrameOffset
  mov [ebp - 24], edx // save stack fStackFrameOffset

  lea esi, ARR_R1
  mov eax, [esi]
  add eax, ecx
  mov edx, [eax]
  add edx, $80000000

  lea esi, ARR_R2
  mov eax, [esi]
  add eax, ecx
  mov eax, [eax]

  push edx
  push ebp
  push $beeffeed
  sub esp, eax

  @@push:
  cmp ecx, 0
  jz @@Call

  mov edx, [ebp - 24]

  @@loop:
  mov esi, [edx]
  push esi
  add edx, 4
  sub ecx, 4
  jnz @@loop

  @@Call:

  mov edx, [ebp - 20] // restore address of function
  call edx

  mov [ebp - 8],  eax // save eax
  mov [ebp - 12], edx // save edx

  mov ecx, [ebp - 16] // restore fStackSize

  lea esi, ARR_R2
  mov eax, [esi]
  add eax, ecx
  mov eax, [eax]

  add eax, $0c
  add esp, eax
  inc byte ptr [esp-$0c]

  @@result:
  mov eax, [ebp - 4] // self

  mov edx, eax // Self
  add edx, RESULTTYPE_OFFSET
  mov ecx, [edx] // fResultType

  cmp ecx, typeINTEGER
  jnz @@RetINT64
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  mov eax, [ebp - 8]
  mov [edx], eax
  jmp @@Return

  @@RetINT64:
  cmp ecx, typeINT64
  jnz @@RetDOUBLE
  mov ecx, eax // Self
  add ecx, RESULT_OFFSET
  mov eax, [ebp - 8]
  mov edx, [ebp - 12]
  mov [ecx], eax
  mov [ecx + 4], edx
  jmp @@Return

  @@RetDOUBLE:
  cmp ecx, typeDOUBLE
  jnz @@RetSINGLE
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp qword ptr [edx]
  jmp @@Return

  @@RetSINGLE:
  cmp ecx, typeSINGLE
  jnz @@RetEXTENDED
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp dword ptr [edx]
  jmp @@Return

  @@RetEXTENDED:
  cmp ecx, typeEXTENDED
  jnz @@RetCURRENCY
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fstp tbyte ptr [edx]
  jmp @@Return

  @@RetCURRENCY:
  cmp ecx, typeCURRENCY
  jnz @@Return
  mov edx, eax // Self
  add edx, RESULT_OFFSET
  fistp qword ptr [edx]
  jmp @@Return

  @@Return:
  mov eax, [ebp - 8]  // retore eax
  mov edx, [ebp - 12] // retore eax

  add esp, $20

  pop edi
  pop esi
  pop ebx

  pop ebp
  ret
end;

procedure TInvoke.CallHost;
asm
  push ebp
  mov ebp, esp

  push ebx
  push esi
  push edi

  add esp,-$100

  mov [esp + $100 - 4], eax

  add esp,-$0c
  call SysInit.@GetCallerEIP
  add esp, $0c

  mov edx, eax // Self
  add edx, STACKSIZE_OFFSET
  mov ecx, [edx] // fStackSize
  sub edx, STACKSIZE_OFFSET

  add edx, STACKFRAME_OFFSET
  mov edx, [edx] // fStackFrame

  mov edi, 12
  cmp ecx, 12
  jg @@L1
  sub edi, ecx
@@L1:
  sub esp, edi

  cmp ecx, 0
  jz @@Next

  @@loop:
  mov esi, [edx]
  push esi
  add edx, 4
  sub ecx, 4
  jnz @@loop

  @@Next:

  add eax, ADDRESS_OFFSET
  mov esi, [eax] // address of procedure
  sub eax, ADDRESS_OFFSET

  add eax, RCX_OFFSET
  mov ecx, [eax]
  sub eax, RCX_OFFSET

  add eax, RDX_OFFSET
  mov edx, [eax]
  sub eax, RDX_OFFSET

  add eax, RAX_OFFSET
  mov eax, [eax]

  call esi

  add esp, edi

  mov ebx, [esp + $100 - 4]

  // if call convention is cdecl then pop arguments
  add ebx, CALLCONV_OFFSET
  mov ecx, [ebx] // fCallConv
  sub ebx, CALLCONV_OFFSET
  cmp ecx, ccCDECL
  jnz @@Ret
  add ebx, STACKSIZE_OFFSET
  mov ecx, [ebx] // fStackSize
  sub ebx, STACKSIZE_OFFSET
  add esp, ecx

  @@Ret:

  add ebx, RESULTTYPE_OFFSET
  mov ecx, [ebx] // fResultType
  sub ebx, RESULTTYPE_OFFSET

  cmp ecx, typeINTEGER
  jnz @@RetDOUBLE
  add ebx, CALLCONV_OFFSET
  mov ecx, [ebx] // fCallConv
  sub ebx, CALLCONV_OFFSET
  cmp ecx, ccSAFECALL
  jz @@Return
  add ebx, RESULT_OFFSET
  mov [ebx], eax
  sub ebx, RESULT_OFFSET
  jmp @@Return
//

  @@RetDOUBLE:

  cmp ecx, typeDOUBLE
  jnz @@RetSINGLE
  add ebx, RESULT_OFFSET
  fstp qword ptr [ebx]
  sub ebx, RESULT_OFFSET
  jmp @@Return
//

  @@RetSINGLE:

  cmp ecx, typeSINGLE
  jnz @@RetEXTENDED
  add ebx, RESULT_OFFSET
  fstp dword ptr [ebx]
  sub ebx, RESULT_OFFSET
  jmp @@Return
//

  @@RetEXTENDED:

  cmp ecx, typeEXTENDED
  jnz @@RetCURRENCY
  add ebx, RESULT_OFFSET
  fstp tbyte ptr [ebx]
  sub ebx, RESULT_OFFSET
  jmp @@Return
//

  @@RetCURRENCY:

  cmp ecx, typeCURRENCY
  jnz @@RetUINT64
  add ebx, RESULT_OFFSET
  fistp qword ptr [ebx]
  sub ebx, RESULT_OFFSET
  jmp @@Return
//

  @@RetUINT64:
  cmp ecx, typeUINT64
  jnz @@RetINT64
  add ebx, RESULT_OFFSET
  mov [ebx], eax
  mov [ebx + 4], edx
  sub ebx, RESULT_OFFSET

  @@RetINT64:
  cmp ecx, typeINT64
  jnz @@Return
  add ebx, RESULT_OFFSET
  mov [ebx], eax
  mov [ebx + 4], edx
  sub ebx, RESULT_OFFSET

  @@Return:

  add esp, $100

  pop edi
  pop esi
  pop ebx // TInvoke instance

  pop ebp
  ret
end;
{$ELSE}

procedure TInvoke.CallHost;
asm
  push ebp
  mov ebp, esp
  sub esp, 4
  mov [ebp - 4], eax

  mov edx, eax // Self
  mov ecx, [edx + 8] // fStackSize
  mov edx, [edx + 4] // fStackFrame

  cmp ecx, 0
  jz @@Next

  @@loop:
  mov esi, [edx]
  push esi
  add edx, 4
  sub ecx, 4
  jnz @@loop

  @@Next:

  mov ebx, [eax + 24] // address of procedure

  mov ecx, [eax + 20]
  mov edx, [eax + 16]
  mov eax, [eax + 12]

  call ebx

  mov ebx, [ebp - 4]

  // if call convention is cdecl then pop arguments
  mov ecx, [ebx + 28] // fCallConv
  cmp ecx, ccCDECL
  jnz @@Ret
  mov ecx, [ebx + 8] // fStackSize
  add esp, ecx

  @@Ret:

  mov ecx, [ebx + 32] // fResultType

  cmp ecx, typeINTEGER
  jnz @@RetDOUBLE
  mov ecx, [ebx + 28] // fCallConv
  cmp ecx, ccSAFECALL
  jz @@Return
  mov [ebx + INVOKE_RESULT_OFFSET], eax
  jmp @@Return
//

  @@RetDOUBLE:

  cmp ecx, typeDOUBLE
  jnz @@RetSINGLE
  fstp qword ptr [ebx + INVOKE_RESULT_OFFSET]
  jmp @@Return
//

  @@RetSINGLE:

  cmp ecx, typeSINGLE
  jnz @@RetEXTENDED
  fstp dword ptr [ebx + INVOKE_RESULT_OFFSET]
  jmp @@Return
//

  @@RetEXTENDED:

  cmp ecx, typeEXTENDED
  jnz @@RetCURRENCY
  fstp tbyte ptr [ebx + INVOKE_RESULT_OFFSET]
  jmp @@Return
//

  @@RetCURRENCY:

  cmp ecx, typeCURRENCY
  jnz @@RetUINT64
  fistp qword ptr [ebx + INVOKE_RESULT_OFFSET]
  jmp @@Return
//

  @@RetUINT64:
  cmp ecx, typeUINT64
  jnz @@RetINT64
  mov [ebx + INVOKE_RESULT_OFFSET], eax
  mov [ebx + INVOKE_RESULT_OFFSET + 4], edx

  @@RetINT64:
  cmp ecx, typeINT64
  jnz @@Return
  mov [ebx + INVOKE_RESULT_OFFSET], eax
  mov [ebx + INVOKE_RESULT_OFFSET + 4], edx

  @@Return:

  mov esp, ebp
  pop ebp
  ret
end;
{$ENDIF}
{$ENDIF}

function TInvoke.GetResultPtr: Pointer;
begin
  if CustomResultAddress <> nil then
    result := CustomResultAddress
  else
    result := @ fResult;
end;

procedure TInvoke.SetResType(value : Integer);
begin
  fResultType := value;
  NeedSetup := true;
end;

procedure TInvoke.SetResSize(value : Integer);
begin
  fResultSize := value;
  NeedSetup := true;
end;

function TInvoke.GetThis: Pointer;
begin
  result := This;
  NeedSetup := true;
end;

procedure TInvoke.SetThis(value: Pointer);
begin
  This := value;
  NeedSetup := true;
end;

function TInvoke.ExtraParamNeeded: Boolean;
begin
  result := fResultType in [typeUNICSTRING,
{$IFNDEF PAXARM}
                            typeANSISTRING,
                            typeWIDESTRING,
                            typeSHORTSTRING,
{$ENDIF}
{$IFDEF ARC}
                            typeCLASS,
{$ENDIF}
                            typeRECORD,
                            typeARRAY,
                            typeDYNARRAY,
                            typeSET,
                            typeEVENT,
                            typeVARIANT,
                            typeOLEVARIANT,
                            typeINTERFACE];
end;

procedure TInvoke.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

procedure TInvoke.LoadAddress(const DllName, ProcName: String);
var
  H: Cardinal;
  I: Integer;
begin
  I := DllList.IndexOf(DllName);
  if I = - 1 then
  begin
   {$IFDEF LINUX}
    H := HMODULE(dynlibs.LoadLibrary(DLLName));
    Address := dynlibs.GetProcedureAddress(H, ProcName);
    {$ELSE}
    H := LoadLibrary(PChar(DllName));
    Address := GetProcAddress(H, PChar(ProcName));
    {$ENDIF}

    if Address <> nil then
      DllList.AddObject(DllName, TObject(H));
  end
  else
  begin
    H := Cardinal(DllList.Objects[I]);
   {$IFDEF LINUX}
    Address := dynlibs.GetProcedureAddress(H, ProcName);
    {$ELSE}
    Address := GetProcAddress(H, PChar(ProcName));
    {$ENDIF}
  end;

  if H = 0 then
    raise Exception.Create(Format('Dynamic link library %s was not found.',
     [DllName]));

  if Address = nil then
    raise Exception.Create(Format('The procedure entry point %s could not be located in the' +
   ' dynamic link library %s.',
     [ProcName, DllName]));
end;

procedure TInvoke.UloadDlls;
var
  H: Cardinal;
begin
  while DllList.Count > 0 do
  begin
    H := Cardinal(DllList.Objects[0]);
    FreeLibrary(H);
    DllList.Delete(0);
  end;
end;

procedure TInvoke.ClearResult;
begin
  case fResultType of
    typeSTRING: String(GetResultPtr^) := '';
    typeINTERFACE: IUnknown(GetResultPtr^)._Release;
    typeVARIANT: VarClear(Variant(GetResultPtr^));
  end;
end;

function TInvoke.IsInternal: Boolean;
begin
  result := fIsInternal;
end;

procedure TInvoke.AdjustResult;
begin
  if CustomResultAddress <> nil then
    if not ExtraParamNeeded then
      Move(fResult, CustomResultAddress^, fResultSize);
end;

procedure CreateMacOSArr;
var
  I: Integer;
begin
  for I := 0 to High(ARR_R1) do
    ARR_R1[I] := R1(I * 4);
  for I := 0 to High(ARR_R2) do
    ARR_R2[I] := R2(I * 4);
  for I := 0 to High(ARR_R3) do
    ARR_R3[I] := R3(I * 4);
end;

initialization
{$IFDEF MACOS}
  STACKFRAME_OFFSET := IntPax(@TInvoke(nil).fStackFrame);
  STACKSIZE_OFFSET := IntPax(@TInvoke(nil).fStackSize);
  RAX_OFFSET := IntPax(@TInvoke(nil).fEAX);
  RCX_OFFSET := IntPax(@TInvoke(nil).fECX);
  RDX_OFFSET := IntPax(@TInvoke(nil).fEDX);
  ADDRESS_OFFSET := IntPax(@TInvoke(nil).fAddress);
  CALLCONV_OFFSET := IntPax(@TInvoke(nil).fCallConv);
  RESULTTYPE_OFFSET := IntPax(@TInvoke(nil).fResultType);
  RESULT_OFFSET := IntPax(@TInvoke(nil).fResult);
{$ENDIF}
  CreateMacOSArr;

end.
