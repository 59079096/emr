////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_OLE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_OLE;
interface

{$IFDEF PAXARM}
implementation
end.
{$ENDIF}

{$IFDEF FPC}
implementation
end.
{$ENDIF}

{$IFDEF MACOS}
implementation
end.
{$ENDIF}

uses
{$IFDEF VARIANTS}
  Variants,
{$ENDIF}
   SysUtils,
  {$IFDEF DPULSAR}
    {$IFDEF MACOS32}
    {$ELSE}
  Winapi.Windows,
  System.Win.ComObj,
  System.Win.ComConst,
  Winapi.ActiveX,
    {$ENDIF}
  {$ELSE}
   Windows,
   ComConst,
   ComObj,
   ActiveX,
   {$ENDIF}
   PAXCOMP_TYPES,
   PAXCOMP_SYS;

procedure _GetOLEProperty(const D: Variant; PropName: PChar;
                          var Result: Variant;
                          ParamsCount: Integer); stdcall;
procedure _SetOLEProperty(const D: Variant; PropName: PChar;
                          const Value: Variant;
                          ParamsCount: Integer); stdcall;
const
  atVarMask  = $3F;
  atTypeMask = $7F;
  atByRef    = $80;

var
  Unassigned: Variant;

implementation

uses
  PAXCOMP_CONSTANTS,
  PaxCompiler,
  AnsiStrings;

type
  TOleHelperRec = class
  public
    DispId: Integer;
    Index: Integer;
  end;

  TOleHelperList = class(TTypedList)
  private
    function GetRecord(I: Integer): TOleHelperRec;
  public
    function Add(DispId: Integer; Index: Integer): TOleHelperRec;
    function FindIndex(DispId: Integer): Integer;
    property Records[I: Integer]: TOleHelperRec read GetRecord; default;
  end;

function TOleHelperList.GetRecord(I: Integer): TOleHelperRec;
begin
  result := TOleHelperRec(L[I]);
end;
function TOleHelperList.Add(DispId: Integer; Index: Integer): TOleHelperRec;
begin
  result := TOleHelperRec.Create;
  result.DispId := DispId;
  result.Index := Index;
  L.Add(result);
end;
function TOleHelperList.FindIndex(DispId: Integer): Integer;
var
  I: Integer;
  R: TOleHelperRec;
begin
  result := -1;
  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if R.DispId = DispId then
      begin
        result := R.Index;
        Exit;
      end;
  end;
end;

var FOleHelperList: TOleHelperList;

function OLEHelperList : TOleHelperList;
begin
  if not assigned(FOleHelperList) then
    FOleHelperList := TOleHelperList.create;
  result := FOleHelperList;
end;

const
  MaxDispArgs = 64;
type
  TIntArr = array[0..100] of LongInt;
  PIntArr = ^TIntArr;
  TBoolArr = array[0..30] of Boolean;
  PBoolArr = ^TBoolArr;
  TStringArr = array[0..30] of String;
  PStringArr = ^TStringArr;
  TDoubleArr = array[0..30] of Double;
  PDoubleArr = ^TDoubleArr;
  TCurrencyArr = array[0..30] of Currency;
  PCurrencyArr = ^TCurrencyArr;
  TVariantArr = array[0..30] of Variant;
  PVariantArr = ^TVariantArr;

{$IFDEF PAX64}
procedure GetIDsOfNames(const Dispatch: IDispatch; Names: PAnsiChar;
  NameCount: Integer; DispIDs: PDispIDList);
type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..100] of PWideChar;

  TArrayOfNamesArray = array[0..20] of TNamesArray;

  procedure RaiseNameException;
  begin
    raise EOleError.CreateFmt(SNoMethod, [Names]);
  end;

var
  N, SrcLen, DestLen: Integer;
  Src: PAnsiChar;
  Dest: PWideChar;
  NameRefs: TNamesArray;
  StackTop: Pointer;
  Temp: Integer;

  buff: array[0..20] of TNamesArray;

begin
  Src := Names;
  N := 0;

  repeat
    SrcLen := SysUtils.StrLen(Src);
    DestLen := MultiByteToWideChar(0, 0, Src, SrcLen, nil, 0) + 1;

    Dest := @ buff[N];

    if N = 0 then
      NameRefs[0] := Dest
    else
      NameRefs[NameCount - N] := Dest;

    MultiByteToWideChar(0, 0, Src, SrcLen, Dest, DestLen);
    Dest[DestLen-1] := #0;
    Inc(Src, SrcLen+1);
    Inc(N);
  until N = NameCount;
  Temp := Dispatch.GetIDsOfNames(GUID_NULL, @NameRefs, NameCount,
    GetThreadLocale, DispIDs);
  if Temp = Integer(DISP_E_UNKNOWNNAME) then RaiseNameException else OleCheck(Temp);
end;
{$ELSE}

procedure GetIDsOfNames(const Dispatch: IDispatch; Names: PAnsiChar;
  NameCount: Integer; DispIDs: PDispIDList);

  procedure RaiseNameException;
  begin
    raise EOleError.CreateFmt(SNoMethod, [Names]);
  end;

type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..0] of PWideChar;
var
  N, SrcLen, DestLen: Integer;
  Src: PAnsiChar;
  Dest: PWideChar;
  NameRefs: PNamesArray;
  StackTop: Pointer;
  Temp: Integer;
begin
  Src := Names;
  N := 0;
  asm
    MOV  StackTop, ESP
    MOV  EAX, NameCount
    INC  EAX
    SHL  EAX, 2  // sizeof pointer = 4
    SUB  ESP, EAX
    LEA  EAX, NameRefs
    MOV  [EAX], ESP
  end;
  repeat
    SrcLen := AnsiStrings.StrLen(Src);
    DestLen := MultiByteToWideChar(0, 0, Src, SrcLen, nil, 0) + 1;
    asm
      MOV  EAX, DestLen
      ADD  EAX, EAX
      ADD  EAX, 3      // round up to 4 byte boundary
      AND  EAX, not 3
      SUB  ESP, EAX
      LEA  EAX, Dest
      MOV  [EAX], ESP
    end;
    if N = 0 then NameRefs[0] := Dest else NameRefs[NameCount - N] := Dest;
    MultiByteToWideChar(0, 0, Src, SrcLen, Dest, DestLen);
    Dest[DestLen-1] := #0;
    Inc(Src, SrcLen+1);
    Inc(N);
  until N = NameCount;
  Temp := Dispatch.GetIDsOfNames(GUID_NULL, NameRefs, NameCount,
    GetThreadLocale, DispIDs);
  if Temp = Integer(DISP_E_UNKNOWNNAME) then RaiseNameException else OleCheck(Temp);
  asm
    MOV  ESP, StackTop
  end;
end;
{$ENDIF}

procedure MyDispatchInvoke(const Dispatch: IDispatch;
  CallDesc: PCallDesc;
  DispIDs: PDispIDList;
  Params: Pointer;
  Result: PVariant;
  var ByRefs: TBoolArr;
  ParamsCount: Integer;
  const P: Variant;
  var SS: TStringArr;
  var II: TIntArr;
  var DD: TDoubleArr;
  var CC: TCurrencyArr;
  var VV: TVariantArr);
type
  PVarArg = ^TVarArg;
{$IFDEF PAX64}
  TVarArg = array[0..5] of DWORD;
{$ELSE}
  TVarArg = array[0..3] of DWORD;
{$ENDIF}
  TStringDesc = record
    BStr: PWideChar;
    PStr: PString;
  end;
var
  I, J, K, ArgType, ArgCount, StrCount, DispID, InvKind, Status: Integer;
  VarFlag: Byte;
  ParamPtr: ^Integer;
  ArgPtr, VarPtr: PVarArg;
  DispParams: TDispParams;
  ExcepInfo: TExcepInfo;
  Strings: array[0..MaxDispArgs - 1] of TStringDesc;
  Args: array[0..MaxDispArgs - 1] of TVarArg;

  TypeInfoCount: Integer;
  TypeInfo: ITypeInfo2;
  pfdesc: PFuncDesc;
  FuncIndex: Cardinal;
  W: Word;
  VT: Integer;
  VCount: Integer;

  VTypes: array[0..30] of Integer;
  Processed: Boolean;
  B1, B2: Integer;
begin
  FillChar(ByRefs, SizeOf(ByRefs), 0);
  FillChar(VTypes, SizeOf(VTypes), 0);

  if ParamsCount > 0 then
  begin
    Dispatch.GetTypeInfoCount(TypeInfoCount);
    if TypeInfoCount = 1 then
    begin
      if Dispatch.GetTypeInfo(0, GetThreadLocale, TypeInfo) = S_OK then
      begin
        DispID := DispIDs[0];
        Processed := false;

        B1 := OleHelperList.FindIndex(DispId);
        if B1 >= 0 then
          B2 := B1
        else
        begin
          B1 := 0;
          B2 := 1000;
        end;

        for FuncIndex := B1 to B2 do
        begin
          if Processed then
            break;

          if TypeInfo.GetFuncDesc(FuncIndex, pfdesc) <> S_OK then
          begin
            TypeInfo.ReleaseFuncDesc(pfdesc);
            break;
          end;

          if pfdesc^.cparams < ParamsCount then
            continue;

          if pfdesc^.memid = DispId then
          try
            OleHelperList.Add(DispId, FuncIndex);

            for I:=0 to ParamsCount - 1 do
            begin
              W := pfdesc^.lprgelemdescParam[I].paramdesc.wParamFlags;
              VTypes[I] := pfdesc^.lprgelemdescParam[I].tdesc.vt;
//              if (W = PARAMFLAG_FOUT) or (W = PARAMFLAG_FRETVAL) then
              if ((W and PARAMFLAG_FOUT) = PARAMFLAG_FOUT) or ((W and PARAMFLAG_FRETVAL) = PARAMFLAG_FRETVAL) then
              begin
                ByRefs[I] := true;
                CallDesc.ArgTypes[I] := CallDesc.ArgTypes[I] or atByRef;
              end;
            end;
          finally
            Processed := true;
            TypeInfo.ReleaseFuncDesc(pfdesc);
          end;
        end; // for-loop
      end;
    end;
  end;

  K := -1;
  for I := 1 to ParamsCount do
  begin
    VT := TVarData(P[I]).VType;
    VCount := VarArrayDimCount(P[I]);

    if VT = 0 then
    begin
      VT := VTypes[I-1];
      if VT = 26 then
        VT := varOleStr;
    end;
    if VT = varUnknown then
      VT := varDispatch;

    if (VT in [VarInteger,VarSmallInt,VarByte]) and (VCount=0) then
    begin
      II[I] := P[I];
      Inc(K);
      if ByRefs[I-1] then
        PIntArr(Params)^[K] := IntPax(@II[I])
      else
        PIntArr(Params)^[K] := II[I];
    end
    else if   VT = VarError then
    begin
      Inc(K);
    end
    else if VT = VarOleStr then
    begin
      SS[I] := P[I];
      Inc(K);
      if ByRefs[I-1] then
        PIntArr(Params)^[K] := IntPax(@SS[I])
      else
        PIntArr(Params)^[K] := IntPax(SS[I]); // byval only
    end
    else if (VT = VarVariant) or (VT = VarDispatch) or (VCount > 0) then
    begin
      VV[I] := P[I];
      Inc(K);

      if ByRefs[I-1] then
        PIntArr(Params)^[K] := IntPax(@VV[I])
      else
      begin
        Move(VV[I], PIntArr(Params)^[K], SizeOf(Variant));
        Inc(K);
        Inc(K);
        Inc(K);
      end;
    end
    else if VT = VarDouble then
    begin
      DD[I] := P[I];
      Inc(K);

      if ByRefs[I-1] then
        PIntArr(Params)^[K] := Integer(@DD[I])
      else
      begin
        Move(DD[I], PIntArr(Params)^[K], SizeOf(Double));
        Inc(K);
      end;
    end
    else if VT = VarCurrency then
    begin
      CC[I] := P[I];
      Inc(K);

      if ByRefs[I-1] then
        PIntArr(Params)^[K] := Integer(@CC[I])
      else
      begin
        Move(CC[I], PIntArr(Params)^[K], SizeOf(Currency));
        Inc(K);
      end;
    end;
  end;

  StrCount := 0;
  try
    ArgCount := CallDesc^.ArgCount;
    if ArgCount > MaxDispArgs then raise EOleException.CreateRes(@STooManyParams);
    if ArgCount <> 0 then
    begin
      ParamPtr := Params;
      ArgPtr := @Args[ArgCount];
      I := 0;
      repeat
        Dec(IntPax(ArgPtr), SizeOf(TVarData));
        ArgType := CallDesc^.ArgTypes[I] and atTypeMask;
        VarFlag := CallDesc^.ArgTypes[I] and atByRef;
        if ArgType = varError then
        begin
          ArgPtr^[0] := varError;
          ArgPtr^[2] := DWORD(DISP_E_PARAMNOTFOUND);
        end
        else
        begin
          if ArgType = varStrArg then
          begin
            with Strings[StrCount] do
              if VarFlag <> 0 then
              begin
                BStr := StringToOleStr(PString(ParamPtr^)^);
                PStr := PString(ParamPtr^);
                PVarData(ArgPtr).VType   := varOleStr or VarByRef;
                PVarData(ArgPtr).VString := @BStr;
              end
              else
              begin
                BStr := StringToOleStr(PString(ParamPtr)^);
                PStr := nil;
                PVarData(ArgPtr).VType := varOleStr;
                PVarData(ArgPtr).VString := BStr;
              end;
            Inc(StrCount);
          end

          else if VarFlag <> 0 then
          begin
            if (ArgType = varVariant) and
               (PVarData(ParamPtr^)^.VType = varString) then
              VarCast(PVariant(ParamPtr^)^, PVariant(ParamPtr^)^, varOleStr);

            ArgPtr^[0] := ArgType or varByRef;
            ArgPtr^[2] := ParamPtr^;
          end

          else if ArgType = varVariant then
          begin
            if PVarData(ParamPtr)^.VType = varString then
            begin
              with Strings[StrCount] do
              begin
                BStr := StringToOleStr(string(PVarData(ParamPtr)^.VString));
                PStr := nil;
                PVarData(ArgPtr).VType := varOleStr;
                PVarData(ArgPtr).VString := BStr;
              end;
              Inc(StrCount);
            end
            else
            begin
              VarPtr := PVarArg(ParamPtr);
              ArgPtr^[0] := VarPtr^[0];
              ArgPtr^[1] := VarPtr^[1];
              ArgPtr^[2] := VarPtr^[2];
              ArgPtr^[3] := VarPtr^[3];
              Inc(IntPax(ParamPtr), 12);
            end;
          end

          else
          begin
            ArgPtr^[0] := ArgType;
            ArgPtr^[2] := ParamPtr^;
            if (ArgType >= varDouble) and (ArgType <= varDate) then
            begin
              Inc(IntPax(ParamPtr), 4);
              ArgPtr^[3] := ParamPtr^;
            end;
          end;
          Inc(IntPax(ParamPtr), 4);
        end;
        Inc(I);
      until I = ArgCount;
    end;
    DispParams.rgvarg := @Args;
    DispParams.rgdispidNamedArgs := @DispIDs[1];
    DispParams.cArgs := ArgCount;
    DispParams.cNamedArgs := CallDesc^.NamedArgCount;
    DispID := DispIDs[0];
    InvKind := CallDesc^.CallType;
    if InvKind = DISPATCH_PROPERTYPUT then
    begin
      if Args[0][0] and varTypeMask = varDispatch then
        InvKind := DISPATCH_PROPERTYPUTREF;
      DispIDs[0] := DISPID_PROPERTYPUT;
      Dec(IntPax(DispParams.rgdispidNamedArgs), SizeOf(Integer));
      Inc(DispParams.cNamedArgs);
    end else
      if (InvKind = DISPATCH_METHOD) and (ArgCount = 0) and (Result <> nil) then
        InvKind := DISPATCH_METHOD or DISPATCH_PROPERTYGET;
    Status := Dispatch.Invoke(DispID, GUID_NULL, 0, InvKind, DispParams,
      Result, @ExcepInfo, nil);
    if Status <> 0 then DispatchInvokeError(Status, ExcepInfo);
    J := StrCount;
    while J <> 0 do
    begin
      Dec(J);
      with Strings[J] do
        if PStr <> nil then OleStrToStrVar(BStr, PStr^);
    end;
  finally
    K := StrCount;
    while K <> 0 do
    begin
      Dec(K);
      SysFreeString(Strings[K].BStr);
    end;
  end;
end;

{ Call GetIDsOfNames method on the given IDispatch interface }

{ Central call dispatcher }

procedure MyVarDispInvoke(Result: PVariant; const Instance: Variant;
  CallDesc : PCallDesc; Params: Pointer; var ByRefs: TBoolArr;
  ParamsCount: Integer;
  const InitParam: Variant;
  var SS: TStringArr;
  var II: TIntArr;
  var DD: TDoubleArr;
  var CC: TCurrencyArr;
  var VV: TVariantArr);

  procedure RaiseException;
  begin
    raise EOleError.Create(SVarNotObject);
  end;

var
  Dispatch: Pointer;
  DispIDs: array[0..MaxDispArgs - 1] of Integer;
begin

  if TVarData(Instance).VType = varDispatch then
    Dispatch := TVarData(Instance).VDispatch
  else if TVarData(Instance).VType = (varDispatch or varByRef) then
    Dispatch := Pointer(TVarData(Instance).VPointer^)
  else
    RaiseException;

  GetIDsOfNames(IDispatch(Dispatch), @CallDesc^.ArgTypes[CallDesc^.ArgCount],
    CallDesc^.NamedArgCount + 1, @DispIDs);

  if Result <> nil then VarClear(Result^);

  MyDispatchInvoke(IDispatch(Dispatch), CallDesc, @DispIDs, Params, Result, ByRefs, ParamsCount, InitParam, SS, II, DD, CC, VV);
end;

function DispatchProcedure(ModeCall: Byte; const Instance: Variant; const Name: String;
                           var P: Variant; ParamsCount: Integer;
                           var ByRefs: TBoolArr): Variant;
var
  CallDesc: TCallDesc;
  Params: TIntArr;
  S: ShortString;
  I, VCount: Integer;
  VT: Byte;
  SS: TStringArr;
  II: TIntArr;
  DD: TDoubleArr;
  CC: TCurrencyArr;
  VV: TVariantArr;
begin
  FillChar(CallDesc, SizeOf(TCallDesc ), 0);
  FillChar(Params, SizeOf(Params), 0);

  S := ShortString(Name);

  with CallDesc do
  begin
    CallType := ModeCall;
    NamedArgCount := 0;
    ArgCount := 0;
    for I := 1 to ParamsCount do
    begin
      VT := TVarData(P[I]).VType;
      VCount := VarArrayDimCount(P[I]);
      if VT = varUnknown then
        VT := varVariant;
      ArgTypes[ArgCount] := VT;
      if (VT = VarOleStr) and (VCount = 0) then
        ArgTypes[ArgCount] := VarStrArg
      else if (VT = VarVariant) or (VT = VarDispatch) or (VCount > 0) then
        ArgTypes[ArgCount] := VarVariant;
      ArgTypes[ ArgCount ] := ArgTypes[ ArgCount ];// or atTypeMask;
      Inc(ArgCount);
    end;
    Move(S[1], ArgTypes[ArgCount], Length(S));
  end;

  MyVarDispInvoke(@Result, Instance, @CallDesc, @Params, ByRefs, ParamsCount, P, SS, II, DD, CC, VV);

  for I:=1 to ParamsCount do
  begin
    VT := TVarData(P[I]).VType;
    VCount := VarArrayDimCount(P[I]);

    if not ByRefs[I - 1] then
      continue;

    if (VT in [VarInteger,VarSmallInt,VarByte]) and (VCount=0) then
      P[I] := II[I]
    else if VT = VarOleStr then
      P[I] := SS[I]
    else if (VT = VarVariant) or (VT = VarDispatch) or (VCount > 0) then
      P[I] := VV[I]
    else if VT = VarDouble then
      P[I] := DD[I]
    else if VT = VarCurrency then
      P[I] := CC[I];
  end;
end;


{$IFDEF PAX64}

function GetParams: TPtrList; assembler;
asm
  mov rax, r15
end;

procedure _GetOLEProperty(const D: Variant; PropName: PChar;
                          var Result: Variant;
                          ParamsCount: Integer); stdcall;
var
  L: TPtrList;
  I: Integer;
  Params: Variant;
  ModeCall: Byte;
  V: PVariant;
  ByRefs: TBoolArr;
  ATempPropName: String;
begin
  L := GetParams;

  Params := VarArrayCreate([1, ParamsCount], varVariant);
  for I:=1 to ParamsCount do
  begin
    V := L[I - 1];
    if VarType(V^) = varBoolean then
    begin
      if V^ then
        Params[I] := Integer(1)
      else
        Params[I] := Integer(0);
    end
    else
      Params[I] := V^;
  end;

  ModeCall := DISPATCH_METHOD + DISPATCH_PROPERTYGET;
  ATempPropName := PropName;
  result := DispatchProcedure(ModeCall, D, ATempPropName, Params, ParamsCount, ByRefs);
  ATempPropName := '';

  for I:=1 to ParamsCount do
  begin
    if not ByRefs[I-1] then
      continue;
    V := L[I - 1];
    V^ := Params[I];
  end;
end;

procedure _SetOLEProperty(const D: Variant; PropName: PChar;
                          const Value: Variant;
                          ParamsCount: Integer); stdcall;
var
  L: TPtrList;
  I: Integer;
  Params: Variant;
  V: PVariant;
  ModeCall: Byte;
  ByRefs: TBoolArr;
  A: array of PVariant;
  ATempPropName: String;
begin
  L := GetParams;

  Params := VarArrayCreate([1, ParamsCount + 1], varVariant);
  for I:=1 to ParamsCount do
  begin
    V := A[I - 1];
    if VarType(V^) = varBoolean then
    begin
      if V^ then
        Params[I] := Integer(1)
      else
        Params[I] := Integer(0);
    end
    else
      Params[I] := V^;
  end;

  if VarType(Value) = varBoolean then
  begin
    if Value then
      Params[ParamsCount + 1] := Integer(1)
    else
      Params[ParamsCount + 1] := Integer(0);
  end
  else
    Params[ParamsCount + 1] := Value;

  ModeCall := DISPATCH_PROPERTYPUT;
  ATempPropName := PropName;
  DispatchProcedure(ModeCall, D, ATempPropName, Params, ParamsCount + 1, ByRefs);
  ATempPropName := '';
end;

{$ELSE}
procedure _GetOLEProperty(const D: Variant; PropName: PChar;
                          var Result: Variant;
                          ParamsCount: Integer); stdcall;

var
  P: Pointer;
procedure Nested;
var
  I: Integer;
  Params: Variant;
  ModeCall: Byte;
  V: PVariant;
  ByRefs: TBoolArr;
  A: array of PVariant;
  ATempPropName: String;
begin
  SetLength(A, ParamsCount);

  for I:=0 to ParamsCount - 1 do
  begin
    A[I] := Pointer(P^);
    Inc(IntPax(P), 4);
  end;

  Params := VarArrayCreate([1, ParamsCount], varVariant);
  for I:=1 to ParamsCount do
  begin
    V := A[I - 1];
    if VarType(V^) = varBoolean then
    begin
      if V^ then
        Params[I] := Integer(1)
      else
        Params[I] := Integer(0);
    end
    else
      Params[I] := V^;
  end;

  ModeCall := DISPATCH_METHOD + DISPATCH_PROPERTYGET;
  ATempPropName := PropName;
  result := DispatchProcedure(ModeCall, D, ATempPropName, Params, ParamsCount, ByRefs);
  ATempPropName := '';

  for I:=1 to ParamsCount do
  begin
    if not ByRefs[I-1] then
      continue;
    A[I - 1]^ := Params[I];
  end;
end; // nested

var
  RetSize: Integer;
begin
  asm
    mov P, ebp
  end;
  Inc(Integer(P), 24);
  Nested;

  RetSize := 16 + ParamsCount * 4;

  asm
  // emulate ret RetSize
    mov ecx, RetSize

    mov esp, ebp
    pop ebp
    mov ebx, [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop
    pop edx
    jmp ebx
  end;

end;

procedure _SetOLEProperty(const D: Variant; PropName: PChar;
                          const Value: Variant;
                          ParamsCount: Integer); stdcall;

var
  P: Pointer;
procedure Nested;
var
  I: Integer;
  Params: Variant;
  V: PVariant;
  ModeCall: Byte;
  ByRefs: TBoolArr;
  A: array of PVariant;
  ATempPropName: String;
begin
  SetLength(A, ParamsCount);

  for I:=0 to ParamsCount - 1 do
  begin
    A[I] := Pointer(P^);
    Inc(Integer(P), 4);
  end;

  Params := VarArrayCreate([1, ParamsCount + 1], varVariant);
  for I:=1 to ParamsCount do
  begin
    V := A[I - 1];
    if VarType(V^) = varBoolean then
    begin
      if V^ then
        Params[I] := Integer(1)
      else
        Params[I] := Integer(0);
    end
    else
      Params[I] := V^;
  end;

  if VarType(Value) = varBoolean then
  begin
    if Value then
      Params[ParamsCount + 1] := Integer(1)
    else
      Params[ParamsCount + 1] := Integer(0);
  end
  else
    Params[ParamsCount + 1] := Value;

  ModeCall := DISPATCH_PROPERTYPUT;
  ATempPropName := PropName;
  DispatchProcedure(ModeCall, D, ATempPropName, Params, ParamsCount + 1, ByRefs);
  ATempPropName := '';

end; // nested

var
  RetSize: Integer;
begin
  asm
    mov P, ebp
  end;
  Inc(Integer(P), 24);
  Nested;

  RetSize := 16 + ParamsCount * 4;

  asm
  // emulate ret RetSize
    mov ecx, RetSize

    mov esp, ebp
    pop ebp
    mov ebx, [esp]

    @@loop:
    pop edx
    sub ecx, 4
    jnz @@loop
    pop edx
    jmp ebx
  end;
end;

{$ENDIF}

initialization
  FOleHelperList := nil;
//  OleHelperList:= TOleHelperList.Create;
finalization
  if assigned(FOleHelperList) then
    FOleHelperList.Free;

end.
