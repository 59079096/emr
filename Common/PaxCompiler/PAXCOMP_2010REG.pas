////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_2010REG.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_2010REG;
interface
{$ifdef DRTTI}
uses {$I uses.def}
  Classes,
  SysUtils,
  TypInfo,
  RTTI,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_2010,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_CLASSLST,
  PAXCOMP_STDLIB;

function RegisterType(Level: Integer; t: TRTTIType;
                      SymbolTable: TBaseSymbolTable;
                      AcceptList: TStrings = nil): Integer;
function RegisterField(Level: Integer; f: TRTTIField;
                        SymbolTable: TBaseSymbolTable): Integer;
function RegisterProperty(ALevel: Integer;
                          p: TRTTIProperty;
                          Index: Integer;
                          SymbolTable: TBaseSymbolTable): Integer;
function RegisterMethod(Level: Integer; m: TRTTIMethod;
                        SymbolTable: TBaseSymbolTable): Integer;
function RegisterRecordType(Level: Integer; t: TRTTIRecordType;
                            SymbolTable: TBaseSymbolTable): Integer;
function RegisterClassType(Level: Integer; t: TRTTIInstanceType;
                           SymbolTable: TBaseSymbolTable;
                           AcceptList: TStrings = nil): Integer;
function RegisterInterfaceType(Level: Integer; t: TRTTIInterfaceType;
                               SymbolTable: TBaseSymbolTable): Integer;
function RegisterArrayType(Level: Integer; t: TRTTIArrayType;
                            SymbolTable: TBaseSymbolTable): Integer;
function RegisterDynamicArrayType(Level: Integer; t: TRTTIDynamicArrayType;
                                  SymbolTable: TBaseSymbolTable): Integer;
function RegisterPointerType(Level: Integer; t: TRTTIPointerType;
                                  SymbolTable: TBaseSymbolTable): Integer;
function RegisterClassRefType(Level: Integer; t: TRTTIClassRefType;
                              SymbolTable: TBaseSymbolTable): Integer;
function RegisterProceduralType(Level: Integer; t: TRTTIProcedureType;
                                SymbolTable: TBaseSymbolTable): Integer;
function RegisterEventType(Level: Integer; t: TRTTIMethodType;
                           SymbolTable: TBaseSymbolTable): Integer;
function RegisterUnit(AUnit: TUnit;
                      SymbolTable: TBaseSymbolTable;
                      AcceptList: TStrings = nil;
                      kernel: Pointer = nil): Integer;
procedure RegisterUnits(UnitList: TUnitList; SymbolTable: TBaseSymbolTable); overload;
procedure RegisterUnits(UnitList: TUnitList); overload;

// Added event to provide a header for known methods that have no RTTI extended info
// It is necessary to register some functions with 'array of const' parameters.
type
  TGetMethodHeaderEvent = procedure(var aHeader: string; aMethod: TRttiMethod) of object;
  PTValue = ^TValue;
var
  OnMethodHasNoExtendedInfo: TGetMethodHeaderEvent;

procedure InitializePAXCOMP_2010Reg;
procedure _VarFromTValue(V: PTValue; T: Integer; Dest: Pointer);
stdcall;

procedure _GetDRTTIProperty(p: TRTTIProperty;
                            X: TObject;
                            var Result: TValue); stdcall;
procedure _GetDRTTIIntegerProperty(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Integer); stdcall;
procedure _GetDRTTIStringProperty(p: TRTTIProperty;
                                  X: TObject;
                                  var Result: String); stdcall;
procedure _GetDRTTIExtendedProperty(p: TRTTIProperty;
                                    X: TObject;
                                    var Result: Extended); stdcall;
procedure _GetDRTTIVariantProperty(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Variant); stdcall;
procedure _GetDRTTIInt64Property(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Int64); stdcall;
procedure _SetDRTTIProperty(p: TRTTIProperty;
                            X: TObject;
                            Value: PTValue); stdcall;
function CheckMethod(t: TRTTIType; m: TRTTIMethod): Boolean;
{$IFDEF DPULSAR}
// Added function to support indexed properties.
function CheckIndexedProperty(aRttiType: TRTTIType; aIndexedProperty: TRTTIIndexedProperty): Boolean;
{$ENDIF}

implementation

uses
  PAXCOMP_KERNEL;

function CheckField(t: TRTTIType; f: TRTTIField): Boolean;
begin
  result := false;

  if not CheckType(f.FieldType) then
    Exit;

  if not (f.Visibility in [mvPublic, mvPublished]) then
    Exit;

  if f.Parent <> t then
    Exit;

  result := true;
end;

function CheckMethod(t: TRTTIType; m: TRTTIMethod): Boolean;
var
  param: TRttiParameter;
begin
  result := false;

  if not (m.Visibility in [mvPublic, mvPublished]) then
    Exit;

  if not m.HasExtendedInfo then
    Exit;

  // Class constructors and class destructor should not be registered.
  // They are called by the application.
  if m.MethodKind in [mkClassConstructor, mkClassDestructor] then
    Exit;

  if assigned(m.ReturnType) then
    if not CheckType(m.ReturnType) then
      Exit;

  for param in m.GetParameters() do
    if not CheckType(param.ParamType) then
      Exit;

  if m.Parent <> t then
  if not (m.DispatchKind in [dkVtable, dkDynamic]) then
    Exit;

  result := true;
end;

function CheckProperty(t: TRTTIType; p: TRTTIProperty): Boolean;
begin
  result := false;

  if not CheckType(p.PropertyType) then
    Exit;

  if not (p.Visibility in [mvPublic, mvPublished]) then
    Exit;

  if not p.Parent.InheritsFrom(t.ClassType) then
    Exit;

  result := true;
end;

// Added function to support indexed properties.
function CheckIndexedPropertyMethod(aRttiType: TRTTIType; aMethod: TRTTIMethod): Boolean;
var
  param: TRttiParameter;
begin
  Result := False;
  if not aMethod.HasExtendedInfo then
    Exit;

  if Assigned(aMethod.ReturnType) then
    if not CheckType(aMethod.ReturnType) then
      Exit;

  if aMethod.IsClassMethod and aMethod.IsStatic then
    Exit;

  for Param in aMethod.GetParameters() do
    if not CheckType(param.ParamType) then
      Exit;

  if aMethod.Parent <> aRttiType then
    if not (aMethod.DispatchKind in [dkVtable, dkDynamic]) then
      Exit;

  Result := True;
end;

{$IFDEF DPULSAR}
// Added function to support indexed properties.
function CheckIndexedProperty(aRttiType: TRTTIType; aIndexedProperty: TRTTIIndexedProperty): Boolean;
begin
  Result := False;
  if not CheckType(aIndexedProperty.PropertyType) then
    Exit;
  if not (aIndexedProperty.Visibility in [mvPublic]) then
    Exit;
  if not aIndexedProperty.Parent.InheritsFrom(aRttiType.ClassType) then
    Exit;
  if aIndexedProperty.IsReadable then
    if not CheckIndexedPropertyMethod(aRttiType, aIndexedProperty.ReadMethod) then
      Exit;
  if aIndexedProperty.IsWritable then
    if not CheckIndexedPropertyMethod(aRttiType, aIndexedProperty.WriteMethod) then
      Exit;
  Result := True;
end;

// Added function to support indexed properties.
function GetIndexedPropertyDecl(aProperty: TRttiIndexedProperty): string;
var
  Method: TRTTIMethod;
  Param: TRttiParameter;
  ParamCount, I: Integer;
begin
  if aProperty.IsReadable then begin
    Method := aProperty.ReadMethod;
    ParamCount := System.Length(Method.GetParameters);
  end else if aProperty.IsWritable then begin
    Method := aProperty.WriteMethod;
    ParamCount := System.Length(Method.GetParameters) - 1;
  end else begin
    Method := nil;
    ParamCount := 0;
  end;

  Result := 'property ' + aProperty.Name + '[';
  for I := 0 to ParamCount - 1 do begin
    Param := Method.GetParameters[I];
    if I > 0 then
      Result := Result + '; ';
    Result := Result + Param.Name + ': ' + Param.ParamType.Name;
  end;
  Result := Result + ']: ' + aProperty.PropertyType.Name;
  if aProperty.IsReadable then
    Result := Result + ' read ' + aProperty.ReadMethod.Name;
  if aProperty.IsWritable then
    Result := Result + ' write ' + aProperty.WriteMethod.Name;
  Result := Result + ';';
  if aProperty.IsDefault then
    Result := Result + ' default;';
end;
{$ENDIF}

function RegisterField(Level: Integer; f: TRTTIField;
                       SymbolTable: TBaseSymbolTable): Integer;
var
  S: String;
  TypeId: Integer;
  t: TRTTIType;
begin
  with SymbolTable do
  begin
    t := f.FieldType;
    S := t.Name;
    TypeId := LookUpType(S, true);
    if TypeId = 0 then
       ExternList.Add(Card + 1, S, erTypeId);
    result := RegisterTypeField(Level, f.Name, TypeId, f.Offset);
  end;
end;

function RegisterMethod(Level: Integer; m: TRTTIMethod;
                        SymbolTable: TBaseSymbolTable): Integer;
var
  S: String;
  cc: Integer;
  CallMode: Integer;
  MethodIndex: Integer;
  TypeId: Integer;
  C: TClass;
  SubId: Integer;
  K: Integer;
  param: TRttiParameter;
  CodeAddress: Pointer;
  R: TSymbolRec;
  OverCount: Integer;
  t: TRttiType;
  mm: TRTTIMethod;
begin
  t := m.Parent;

  if t is TRttiInstanceType then
    C := (t as TRttiInstanceType).MetaClassType
  else
    C := nil;

  OverCount := 0;
  K := 0;
  S := m.ToString;
  for mm in t.GetDeclaredMethods do
  begin
    if CheckMethod(t, mm) then
    if mm.Name = m.Name then
      Inc(K);
    if mm.ToString = S then
      OverCount := K;
  end;
  if K = 1 then
    OverCount := 0;

  if m.ReturnType = nil then
    typeId := typeVOID
  else
  begin
    S := m.ReturnType.Name;
    typeId := SymbolTable.LookUpType(S, true);
    // Moved test inside if-statement.
    if TypeId = 0 then
       SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erTypeId);
  end;

  cc := ccREGISTER;
  case m.CallingConvention of
    TypInfo.ccReg: cc := ccREGISTER;
    TypInfo.ccCdecl: cc := ccCDECL;
    TypInfo.ccPascal: cc := ccPASCAL;
    TypInfo.ccStdCall: cc := ccSTDCALL;
    TypInfo.ccSafeCall: cc := ccSAFECALL;
  end;

  // Set CallMode, MethodIndex and CodeAddress all at once.
  // Note that for cases dkVTable and dkDynamic m.CodeAddress returns an
  // incorrect value. Therefore the code address
  // is read from the virtual / dynamic method table
  // using m.VirtualIndex.
  CallMode := cmNONE;
  MethodIndex := 0;
  CodeAddress := m.CodeAddress;
  case m.DispatchKind of
    dkStatic:
    begin
      if C = nil then
        CallMode := cmSTATIC;
    end;
    dkVtable:
    begin
      CallMode := cmVIRTUAL;
      MethodIndex := m.VirtualIndex + 1;
    end;
    dkDynamic: CallMode := cmDYNAMIC;
    dkMessage: CallMode := cmDYNAMIC;
    dkInterface: MethodIndex := m.VirtualIndex + 1;
  end;

  // Use CodeAddress instead of m.CodeAddress.
  if m.IsConstructor then
  begin
    result := SymbolTable.RegisterConstructor(Level, m.Name, CodeAddress, m.IsClassMethod, CallMode);
    R := SymbolTable.Records[SymbolTable.LastSubId];
  end
  else if m.IsDestructor then
  begin
    result := SymbolTable.RegisterMethod(Level, m.Name, TypeId, cc, CodeAddress, false, CallMode, MethodIndex);
    R := SymbolTable.Records[SymbolTable.LastSubId];
    R.Kind := kindDESTRUCTOR;
  end
  else
  begin
    result := SymbolTable.RegisterMethod(Level, m.Name, TypeId, cc, CodeAddress, m.IsClassMethod, CallMode, MethodIndex);
    R := SymbolTable.Records[SymbolTable.LastSubId];
  end;
  R.MethodIndex := MethodIndex;
  R.OverCount := OverCount;

  case m.Visibility of
    mvPrivate:   R.Vis := cvPrivate;
    mvProtected: R.Vis := cvProtected;
    mvPublic:    R.Vis := cvPublic;
    mvPublished: R.Vis := cvPublished;
  end;

  if C <> nil then
    if CallMode = cmDYNAMIC then
      R.DynamicMethodIndex :=
        GetDynamicMethodIndexByAddress(C, CodeAddress);

  SubId := SymbolTable.LastSubId;
  K := 0;
  for param in m.GetParameters() do
  begin
    if param.ParamType = nil then
      typeID := typeVOID
    else
    begin
      S := param.ParamType.Name;
      typeId := SymbolTable.LookUpType(S, true);
      if TypeId = 0 then
      begin
        SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erTypeId);
      end;
    end;

    SymbolTable.RegisterParameter(result, TypeId, Unassigned, false, param.Name, 0);
    R := SymbolTable.Records[SymbolTable.Card];

    if [pfVar, pfReference, pfOut] * Param.Flags <> [] then
      R.ByRef := true;
    if pfConst in param.Flags then
      R.IsConst := true;
    if pfArray in param.Flags then
      R.IsOpenArray := true;

    Inc(K);
  end;

  SymbolTable.Records[SubId].Count := K;
end;

function RegisterRecordType(Level: Integer; t: TRTTIRecordType;
                            SymbolTable: TBaseSymbolTable): Integer;
var
  m: TRTTIMethod;
  f: TRTTIField;
begin
  result := SymbolTable.RegisterRecordType(Level,
                                           t.Name, GlobalAlignment);
  for f in t.GetFields do
    if CheckField(t, f) then
      RegisterField(result, f, SymbolTable);
  for m in t.GetDeclaredMethods do
    if CheckMethod(t, m) then
      RegisterMethod(Result, m, SymbolTable);
end;

function RegisterClassType(Level: Integer; t: TRTTIInstanceType;
                           SymbolTable: TBaseSymbolTable;
                           AcceptList: TStrings = nil): Integer;
var
  m: TRTTIMethod;
  f: TRTTIField;
  C: TClass;
{$IFDEF DPULSAR}
  IndexedProp: TRTTIIndexedProperty;
{$ENDIF}
{$IFDEF ARC}
  RegisteredMethods: TList<TRTTIMethod>;
{$ELSE}
  RegisteredMethods: TList;
{$ENDIF}
{$IFDEF DPULSAR}
  Index, id: Integer;
  Decl: string;
  it: TRttiInterfaceType;
{$ENDIF}
  MethodIndex: Integer;
  Header: String;
begin
  C := t.MetaclassType;

  result := SymbolTable.RegisterClassType(Level, C);

  for f in t.GetDeclaredFields do
    if CheckField(t, f) then
      RegisterField(result, f, SymbolTable);

  // Store registered methods in a local list to be used during registration of indexed properties.
{$IFDEF ARC}
  RegisteredMethods := TList<TRTTIMethod>.Create;
{$ELSE}
  RegisteredMethods := TList.Create;
{$ENDIF}
  try
    for m in t.GetDeclaredMethods do
    begin
      if AcceptList <> nil then
        if AcceptList.IndexOf(m.Name) = -1 then
          continue;

      if m.HasExtendedInfo then
      begin
        if CheckMethod(t, m) then
        begin
          RegisterMethod(Result, m, SymbolTable);
          // Add method to list.
          RegisteredMethods.Add(m);
        end;
      end
      else
      begin
        // Call an event to provide a header for known methods that have no RTTI extended info.
        if (m.Visibility in [mvPublic, mvPublished]) and Assigned(OnMethodHasNoExtendedInfo) then begin
          Header := '';
          OnMethodHasNoExtendedInfo(Header, m);
          if Header <> '' then begin
            MethodIndex := 0;
            if m.DispatchKind = dkVtable then
              MethodIndex := m.VirtualIndex;
            SymbolTable.RegisterHeader(Result, Header, m.CodeAddress, MethodIndex);
          end;
        end;
      end;
    end;
{$IFDEF DPULSAR}
    // Added support for indexed properties.
    for IndexedProp in t.GetDeclaredIndexedProperties do
    begin
      if CheckIndexedProperty(t, IndexedProp) then
      begin
        if IndexedProp.IsReadable then begin
          Index := RegisteredMethods.IndexOf(IndexedProp.ReadMethod);
          if Index = -1 then
          begin
            RegisterMethod(Result, IndexedProp.ReadMethod, SymbolTable);
            RegisteredMethods.Add(IndexedProp.ReadMethod);
          end;
        end;
        if IndexedProp.IsWritable then
        begin
          Index := RegisteredMethods.IndexOf(IndexedProp.WriteMethod);
          if Index = -1 then begin
            RegisterMethod(Result, IndexedProp.WriteMethod, SymbolTable);
            RegisteredMethods.Add(IndexedProp.WriteMethod);
          end;
        end;
        Decl := GetIndexedPropertyDecl(IndexedProp);
        SymbolTable.RegisterHeader(Result, Decl, nil);
      end;
    end;
{$ENDIF}
  finally
    FreeAndNil(RegisteredMethods);
  end;

{$IFDEF DPULSAR}
  for it in t.GetDeclaredImplementedInterfaces do
  begin
    id := SymbolTable.LookUpFullName(it.QualifiedName, true);
    if id = 0 then
    begin
      if not CheckType(it) then
        continue;
      RegisterInterfaceType(Level, it, SymbolTable);
    end;

    SymbolTable.RegisterSupportedInterface(result, it.Name, it.GUID);
  end;
{$ENDIF}
end;

function RegisterInterfaceType(Level: Integer; t: TRTTIInterfaceType;
                               SymbolTable: TBaseSymbolTable): Integer;
var
  m: TRTTIMethod;
  f: TRTTIField;
  p: TRTTIProperty;
  S: String;
begin
  S := t.Name;

  result := SymbolTable.RegisterInterfaceType(Level, S, t.guid);
  for f in t.GetDeclaredFields do
    if CheckField(t, f) then
      RegisterField(result, f, SymbolTable);
  for m in t.GetDeclaredMethods do
    if CheckMethod(t, m) then
    begin
      RegisterMethod(result, m, SymbolTable);
    end;
  for p in t.GetProperties do
    if CheckProperty(t, p) then
      RegisterProperty(result, p, 0, SymbolTable);
end;

function RegisterArrayType(Level: Integer; t: TRTTIArrayType;
                           SymbolTable: TBaseSymbolTable): Integer;
var
  I, K, HR, HE, Align: Integer;
  S: String;
begin
  Align := GlobalAlignment;
  K := t.DimensionCount;
  result := 0;
  for I := 0 to K - 1 do
  begin
    if t.Dimensions[I] = nil then
    begin
      result := 0;
      Exit;
    end;

    HR := RegisterType(Level, t.Dimensions[I], SymbolTable);
    if I = 0 then
      HE := RegisterType(Level, t.ElementType, SymbolTable)
    else
      HE := result;
    if I = K - 1 then
      S := t.Name
    else
      S := 'Array_' + IntToStr(SymbolTable.Card + 1);
    result := SymbolTable.RegisterArrayType(Level, S, HR, HE, Align);
  end;
end;

function RegisterDynamicArrayType(Level: Integer; t: TRTTIDynamicArrayType;
                                  SymbolTable: TBaseSymbolTable): Integer;
begin
  result := SymbolTable.RegisterRTTIType(Level, t.Handle)
end;

function RegisterPointerType(Level: Integer; t: TRTTIPointerType;
                             SymbolTable: TBaseSymbolTable): Integer;
var
  H: Integer;
  S: String;
begin
  if t.ReferredType = nil then
    H := typeVOID
  else
  begin
    S := t.ReferredType.Name;
    H := SymbolTable.LookUpType(S, true);
    if H = 0 then
      SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erPatternId);
  end;
  result := SymbolTable.RegisterPointerType(Level, t.Name, H);
end;

function RegisterClassRefType(Level: Integer; t: TRTTIClassRefType;
                              SymbolTable: TBaseSymbolTable): Integer;
var
  H: Integer;
  S: String;
begin
  S := t.InstanceType.Name;
  H := SymbolTable.LookUpType(S, true);
  if H = 0 then
    SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erPatternId);
  result := SymbolTable.RegisterClassReferenceType(Level, t.Name, H);
end;

{$IFNDEF GE_DXE2}
function RegisterProceduralType(Level: Integer; t: TRTTIProcedureType;
                                SymbolTable: TBaseSymbolTable): Integer;
var
  cc, ResultTypeId, TypeId, SubId, I, K, H: Integer;
  rt: PPTypeInfo;
  S: String;
  R: TSymbolRec;
  ProcSig: PProcedureSignature;
  P: PProcedureParam;
  pti: PTypeInfo;
  ptd: PTypeData;
begin
  result := 0;
  pti := t.Handle;
  ptd := GetTypeData(pti);
  ProcSig := ptd^.ProcSig;
  if not NativeAddress(ProcSig) then
    Exit;
  rt := ProcSig.ResultType;
  if rt = nil then
    ResultTypeId := typeVOID
  else
    ResultTypeId := SymbolTable.LookUpType(String(rt^.Name), true);

  cc := ccREGISTER;
  case ProcSig.CC of
    TypInfo.ccReg: cc := ccREGISTER;
    TypInfo.ccCdecl: cc := ccCDECL;
    TypInfo.ccPascal: cc := ccPASCAL;
    TypInfo.ccStdCall: cc := ccSTDCALL;
    TypInfo.ccSafeCall: cc := ccSAFECALL;
  end;

  H := SymbolTable.RegisterRoutine(Level, '', ResultTypeId, cc, nil);
  SubId := SymbolTable.LastSubId;

  K := ProcSig.ParamCount;
  P := ShiftPointer(ProcSig, SizeOf(TProcedureSignature));

  for I := 0 to K - 1 do
  begin
    if P.ParamType = nil then
      typeID := typeVOID
    else
    begin
      S := String(p.ParamType^.Name);
      typeId := SymbolTable.LookUpType(S, true);
      if TypeId = 0 then
        SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erTypeId);
    end;

    SymbolTable.RegisterParameter(H, TypeId, Unassigned, false, String(P.Name), 0);
    R := SymbolTable.Records[SymbolTable.Card];

    if [pfVar, pfReference, pfOut] * TParamFlags(P.Flags) <> [] then
      R.ByRef := true;
    if pfConst in TParamFlags(P.Flags) then
      R.IsConst := true;
    if pfArray in TParamFlags(P.Flags) then
      R.IsOpenArray := true;

    P := ShiftPointer(P, SizeOf(TProcedureParam));
  end;

  SymbolTable.Records[SubId].Count := K;

  result := SymbolTable.RegisterProceduralType(Level, t.Name, H);
end;
{$ELSE}
function RegisterProceduralType(Level: Integer; t: TRTTIProcedureType;
                               SymbolTable: TBaseSymbolTable): Integer;
var
  cc, ResultTypeId, TypeId, SubId, K, H: Integer;
  rt: TRTTIType;
  param: TRTTIParameter;
  S: String;
  R: TSymbolRec;
begin
  rt := t.ReturnType;
  if rt = nil then
    ResultTypeId := typeVOID
  else
    ResultTypeId := SymbolTable.LookUpType(rt.Name, true);

  cc := ccREGISTER;
  case t.CallingConvention of
    TypInfo.ccReg: cc := ccREGISTER;
    TypInfo.ccCdecl: cc := ccCDECL;
    TypInfo.ccPascal: cc := ccPASCAL;
    TypInfo.ccStdCall: cc := ccSTDCALL;
    TypInfo.ccSafeCall: cc := ccSAFECALL;
  end;

  H := SymbolTable.RegisterRoutine(Level, '', ResultTypeId, cc, nil);

  SubId := SymbolTable.LastSubId;
  K := 0;
  for param in t.GetParameters() do
  begin
    if param.ParamType = nil then
      TypeId := typeVOID
    else
    begin
      S := param.ParamType.Name;
      typeId := SymbolTable.LookUpType(S, true);
      if TypeId = 0 then
        SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erTypeId);
    end;

    SymbolTable.RegisterParameter(H, TypeId, Unassigned, false, param.Name, 0);
    R := SymbolTable.Records[SymbolTable.Card];

    if [pfVar, pfReference, pfOut] * Param.Flags <> [] then
      R.ByRef := true;
    if pfConst in param.Flags then
      R.IsConst := true;
    if pfArray in param.Flags then
      R.IsOpenArray := true;

    Inc(K);
  end;

  SymbolTable.Records[SubId].Count := K;

  result := SymbolTable.RegisterProceduralType(Level, t.Name, H);
end;
{$ENDIF}

{$IFNDEF GE_DXE2}
function RegisterEventType(Level: Integer; t: TRTTIMethodType;
                           SymbolTable: TBaseSymbolTable): Integer;
var
  cc, TypeId, SubId, I, K, H, L: Integer;
  R: TSymbolRec;
  ProcSig: PProcedureSignature;
  P: Pointer;
  pti: PTypeInfo;
  ptd: PTypeData;
  Flags: TParamFlags;
  ParamName: ShortString;
  TypeName: ShortString;
  ResultType: ShortString;
  CallConv: TCallConv;
begin
  pti := t.Handle;
  ptd := GetTypeData(pti);
  K := ptd^.ParamCount;
  H := SymbolTable.RegisterRoutine(Level, '', 0, ccREGISTER, nil);
  SubId := SymbolTable.LastSubId;
  P := @ ptd^.ParamCount;
  P := ShiftPointer(P, 1);
  for I := 0 to K - 1 do
  begin
    Flags := TParamFlags(P^);
    P := ShiftPointer(P, SizeOf(TParamFlags));
    L := Byte(P^);
    Move(P^, ParamName, L + 1);
    P := ShiftPointer(P, L + 1);
    L := Byte(P^);
    Move(P^, TypeName, L + 1);
    P := ShiftPointer(P, L + 1);
    TypeId := SymbolTable.LookUpType(String(TypeName), true);
    if TypeId = 0 then
      SymbolTable.ExternList.Add(SymbolTable.Card + 1, String(TypeName), erTypeId);
    SymbolTable.RegisterParameter(H, TypeId, Unassigned, false, String(ParamName), 0);
    R := SymbolTable.Records[SymbolTable.Card];

    if [pfVar, pfReference, pfOut] * TParamFlags(Flags) <> [] then
      R.ByRef := true;
    if pfConst in TParamFlags(Flags) then
      R.IsConst := true;
    if pfArray in TParamFlags(Flags) then
      R.IsOpenArray := true;
  end;

  if ptd^.MethodKind = mkFunction then
  begin
    L := Byte(P^);
    Move(P^, ResultType, L + 1);
    TypeId := SymbolTable.LookUpType(String(ResultType), true);
    SymbolTable.Records[SubId].TypeId := TypeId;
    I := SymbolTable.GetResultId(SubId);
    SymbolTable.Records[I].TypeId := TypeId;

    P := ShiftPointer(P, L + 1);
    P := ShiftPointer(P, SizeOf(Pointer));
  end;

  CallConv := TCallConv(P^);
  cc := ccREGISTER;
  case CallConv of
    TypInfo.ccReg: cc := ccREGISTER;
    TypInfo.ccCdecl: cc := ccCDECL;
    TypInfo.ccPascal: cc := ccPASCAL;
    TypInfo.ccStdCall: cc := ccSTDCALL;
    TypInfo.ccSafeCall: cc := ccSAFECALL;
  end;
  SymbolTable.Records[SubId].CallConv := cc;

  SymbolTable.Records[SubId].Count := K;
  result := SymbolTable.RegisterEventType(Level, t.Name, H);
end;
{$ELSE}
function RegisterEventType(Level: Integer; t: TRTTIMethodType;
                           SymbolTable: TBaseSymbolTable): Integer;
var
  cc, ResultTypeId, TypeId, SubId, K, H: Integer;
  rt: TRTTIType;
  param: TRTTIParameter;
  S: String;
  R: TSymbolRec;
begin
  rt := t.ReturnType;
  if rt = nil then
    ResultTypeId := typeVOID
  else
    ResultTypeId := SymbolTable.LookUpType(rt.Name, true);

  cc := ccREGISTER;
  case t.CallingConvention of
    TypInfo.ccReg: cc := ccREGISTER;
    TypInfo.ccCdecl: cc := ccCDECL;
    TypInfo.ccPascal: cc := ccPASCAL;
    TypInfo.ccStdCall: cc := ccSTDCALL;
    TypInfo.ccSafeCall: cc := ccSAFECALL;
  end;

  H := SymbolTable.RegisterRoutine(Level, '', ResultTypeId, cc, nil);

  SubId := SymbolTable.LastSubId;
  K := 0;
  for param in t.GetParameters() do
  begin
    if param.ParamType = nil then
      TypeId := typeVOID
    else
    begin
      S := param.ParamType.Name;
      typeId := SymbolTable.LookUpType(S, true);
      if TypeId = 0 then
        SymbolTable.ExternList.Add(SymbolTable.Card + 1, S, erTypeId);
    end;

    SymbolTable.RegisterParameter(H, TypeId, Unassigned, false, param.Name, 0);
    R := SymbolTable.Records[SymbolTable.Card];

    if [pfVar, pfReference, pfOut] * Param.Flags <> [] then
      R.ByRef := true;
    if pfConst in param.Flags then
      R.IsConst := true;
    if pfArray in param.Flags then
      R.IsOpenArray := true;

    Inc(K);
  end;

  SymbolTable.Records[SubId].Count := K;

  result := SymbolTable.RegisterEventType(Level, t.Name, H);
end;
{$ENDIF}

function RegisterType(Level: Integer; t: TRTTIType;
                      SymbolTable: TBaseSymbolTable;
                      AcceptList: TStrings = nil): Integer;
var
  S: String;
begin
  result := 0;
  if t = nil then
    Exit;
  S := t.Name;

  result := SymbolTable.LookUpType(S, true);
  if result > 0 then
    Exit;

  case t.TypeKind of
    tkRecord:
      result := RegisterRecordType(Level, t as TRTTIRecordType, SymbolTable);
    tkClass:
      result := RegisterClassType(Level, t as TRTTIInstanceType,
        SymbolTable, AcceptList);
    tkInterface:
      result := RegisterInterfaceType(Level, t as TRTTIInterfaceType, SymbolTable);
    tkArray:
      result := RegisterArrayType(Level, t as TRTTIArrayType, SymbolTable);
    tkDynArray:
      result := RegisterDynamicArrayType(Level, t as TRTTIDynamicArrayType, SymbolTable);
    tkPointer:
      result := RegisterPointerType(Level, t as TRTTIPointerType, SymbolTable);
    tkProcedure:
      result := RegisterProceduralType(Level, t as TRTTIProcedureType, SymbolTable);
    tkMethod:
      result := RegisterEventType(Level, t as TRTTIMethodType, SymbolTable);
    tkClassRef:
      result := RegisterClassRefType(Level, t as TRTTIClassRefType, SymbolTable);
    else
      result := SymbolTable.RegisterRTTIType(Level, t.Handle);
  end;
end;

function RegisterUnit(AUnit: TUnit;
                      SymbolTable: TBaseSymbolTable;
                      AcceptList: TStrings = nil;
                      kernel: Pointer = nil): Integer;
var
  I, Id: Integer;
  Q: TStringList;
  S: String;
begin
  result := 0;
  Q := ExtractNames(AUnit.Name);
  try

    for I := 0 to Q.Count - 1 do
    begin
      S := Q[I];
      if StrEql(S, 'System') then
        result := 0
      else
        result := SymbolTable.RegisterNamespace(result, S);
    end;

    for I := 0 to AUnit.Count - 1 do
    begin
      Id := RegisterType(result, AUnit[I].T, SymbolTable, AcceptList);
      if kernel <> nil then
        if Assigned(TKernel(kernel).OnImportType) then
          TKernel(kernel).OnImportType(TKernel(kernel).Owner,
                                       Id,
                                       AUnit[I].T.QualifiedName);
    end;

  finally
    FreeAndNil(Q);
  end;
end;

procedure RegisterUnits(UnitList: TUnitList; SymbolTable: TBaseSymbolTable);
var
  I: Integer;
begin
  for I := 0 to UnitList.Count - 1 do
    RegisterUnit(UnitList[I], SymbolTable);
  for I := 0 to UnitList.ForbiddenClasses.Count - 1 do
    SymbolTable.HideClass(TClass(UnitList.ForbiddenClasses[I]));
end;

procedure RegisterUnits(UnitList: TUnitList);
var
  I: Integer;
begin
  for I := 0 to UnitList.Count - 1 do
    RegisterUnit(UnitList[I], GlobalImportTable);
end;

// TValue ---------

function TValue_Implicit_String(const Value: string): TValue;
begin
  result := Value;
end;

function TValue_Implicit_Integer(Value: Integer): TValue;
begin
  result := Value;
end;

function TValue_Implicit_Extended(Value: Extended): TValue;
begin
  result := Value;
end;

function TValue_Implicit_Int64(Value: Int64): TValue;
begin
  result := Value;
end;

function TValue_Implicit_TObject(Value: TObject): TValue;
begin
  result := Value;
end;

function TValue_Implicit_TClass(Value: TClass): TValue;
begin
  result := Value;
end;

function TValue_Implicit_Boolean(Value: Boolean): TValue;
begin
  result := Value;
end;

function TValue_GetDataSize(Self: TValue): Integer;
begin
  result := Self.DataSize;
end;

procedure _VarFromTValue(V: PTValue; T: Integer; Dest: Pointer);
stdcall;
begin
  case T of
    0, typeVOID: Exit;
    typeINTEGER: Integer(Dest^) := V.AsInteger;
    typeSMALLINT: SmallInt(Dest^) := V.AsInteger;
    typeSHORTINT: ShortInt(Dest^) := V.AsInteger;
    typeINT64: Int64(Dest^) := V.AsInt64;
{$IFDEF GE_DXE4}
    typeUINT64: UInt64(Dest^) := V.AsUInt64;
    typeBYTE: Byte(Dest^) := V.AsUInt64;
    typeWORD: Word(Dest^) := V.AsUInt64;
    typeCARDINAL: Cardinal(Dest^) := V.AsUInt64;
{$ELSE}
    typeUINT64: UInt64(Dest^) := V.AsInt64;
    typeBYTE: Byte(Dest^) := V.AsOrdinal;
    typeWORD: Word(Dest^) := V.AsOrdinal;
    typeCARDINAL: Cardinal(Dest^) := V.AsOrdinal;
{$ENDIF}
{$IFNDEF PAXARM}
    typeANSICHAR: Byte(Dest^) := V.AsOrdinal;
    typeSHORTSTRING: PShortStringFromString(Dest, V.AsString);
    typeANSISTRING: AnsiString(Dest^) := AnsiString(V.AsString);
    typeWIDESTRING: WideString(Dest^) := V.AsString;
{$ENDIF}
    typeWIDECHAR: Word(Dest^) := V.AsOrdinal;
    typeENUM: Byte(Dest^) := V.AsOrdinal;
    typeBOOLEAN: Boolean(Dest^) := V.AsBoolean;
    typeBYTEBOOL: ByteBool(Dest^) := ByteBool(V.AsOrdinal);
    typeWORDBOOL: WordBool(Dest^) := WordBool(V.AsOrdinal);
    typeLONGBOOL: LongBool(Dest^) := LongBool(V.AsOrdinal);
    typeDOUBLE: Double(Dest^) := V.AsExtended;
    typeSINGLE: Single(Dest^) := V.AsExtended;
    typeEXTENDED: Extended(Dest^) := V.AsExtended;
    typeCURRENCY: Currency(Dest^) := V.AsCurrency;
    typeUNICSTRING: UnicString(Dest^) := V.AsString;
    typeCLASS: TObject(Dest^) := V.AsObject;
    typeCLASSREF: Pointer(Dest^) := V.AsClass;
    typePOINTER: V.ExtractRawData(Dest);
    typePROC: Pointer(Dest^) := V.AsType<POINTER>;
    typeVARIANT: Variant(Dest^) := V.AsVariant;
    typeOLEVARIANT: OleVariant(Dest^) := V.AsVariant;
    typeINTERFACE: IUnknown(Dest^) := V.AsType<IUnknown>;
    else
      V.ExtractRawDataNoCopy(Dest);
  end;
end;

procedure _TValueToObject(var V: TValue; var result: TObject); stdcall;
begin
  result := V.AsObject;
end;

procedure _GetDRTTIProperty(p: TRTTIProperty;
                            X: TObject;
                            var Result: TValue); stdcall;
begin
  result := p.GetValue(X);
end;

procedure _GetDRTTIIntegerProperty(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Integer); stdcall;
begin
  result := p.GetValue(X).AsInteger;
end;

procedure _GetDRTTIStringProperty(p: TRTTIProperty;
                                  X: TObject;
                                  var Result: String); stdcall;
begin
  result := p.GetValue(X).AsString;
end;

procedure _GetDRTTIExtendedProperty(p: TRTTIProperty;
                                    X: TObject;
                                    var Result: Extended); stdcall;
begin
  result := p.GetValue(X).AsExtended;
end;

procedure _GetDRTTIVariantProperty(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Variant); stdcall;
begin
  result := p.GetValue(X).AsVariant;
end;

procedure _GetDRTTIInt64Property(p: TRTTIProperty;
                                   X: TObject;
                                   var Result: Int64); stdcall;
begin
  result := p.GetValue(X).AsOrdinal;
end;

procedure _SetDRTTIProperty(p: TRTTIProperty;
                            X: TObject;
                            Value: PTValue); stdcall;
var
  i: Int64;
begin
  if p.PropertyType.Handle.Kind = tkEnumeration then
  begin
    if Value^.IsType<Boolean> then
      i := Integer(Value.AsBoolean)
    else
      i := Value.AsInteger;
    Value^ := TValue.FromOrdinal(p.PropertyType.Handle, i);
    p.SetValue(X, Value^);
  end
  else
    p.SetValue(X, Value^);
end;

function RegisterProperty(ALevel: Integer;
                          p: TRTTIProperty;
                          Index: Integer;
                          SymbolTable: TBaseSymbolTable): Integer;
var
  S: String;
  T: Integer;
  typ: TRTTIType;
begin
  with SymbolTable do
  begin
    if p = nil then
    begin
      with AddRecord do
      begin
        Name := '';
        Kind := KindPROP;
        TypeID := 0;
        Host := true;
        Shift := 0;
        Level := ALevel;
        IsPublished := false;
        IsDRTTI := true;
        PropIndex := Index;

        result := Card;
      end;
      Exit;
    end;

    typ := p.PropertyType;
    S := typ.Name;
    T := LookUpType(S, true);
    if T = 0 then
       ExternList.Add(Card + 1, S, erTypeId);

    with AddRecord do
    begin
      Name := p.Name;
      Kind := KindPROP;
      TypeID := T;
      Host := true;
      Shift := 0;
      Level := ALevel;
      IsPublished := false;
      IsDRTTI := true;
      PropIndex := Index;

      result := Card;
    end;
  end;
end;

procedure RegisterDRTTIPropertiesImpl(Level: Integer;
                                      C: TClass;
                                      SymbolTable: TBaseSymbolTable);

  function PublishedPropertyCount: Integer;
  var
    pti: PTypeInfo;
    ptd: PTypeData;
  begin
    result := 0;
    pti := C.ClassInfo;
    if pti = nil then Exit;
    ptd := GetTypeData(pti);
    result := ptd^.PropCount;
  end;

var
  I, K, LastPropertyIndex: Integer;
  p: TRTTIProperty;
  t: TRTTIType;
begin
  if IsPaxClass(C) then
  begin
    SymbolTable.RegisterClassTypeInfos(Level,C);
    Exit;
  end;

  LastPropertyIndex := PublishedPropertyCount;
  K := 0;

  repeat
    t := PaxContext.GetType(C);
    for p in t.GetDeclaredProperties do
    begin
      if CheckProperty(t, p) then
      begin
        if SymbolTable.Lookup(p.Name, Level, true, MaxInt, false) = 0 then
        begin
          RegisterProperty(Level, p, K + LastPropertyIndex, SymbolTable);
        end
        else
        begin
          RegisterProperty(Level, nil, K + LastPropertyIndex, SymbolTable);
        end;
        Inc(K);
      end;
    end;
    C :=  C.ClassParent;
    if C = nil then
      break;
  until false;

  for I:=1 to K do
    SymbolTable.AddPointerVar(0);
end;

// Added this function to get the namespace of a type.
function GetNamespaceOfTypeImpl(aSymbolTable: TBaseSymbolTable; aTypeInfo: PTypeInfo): Integer;
var
  T: TRttiType;
  Namespace: string;
begin
  Result := 0;
  T := PaxContext.GetType(aTypeInfo);
  if Assigned(T) then
  begin
    Namespace := ExtractUnitName(T);
    Result := aSymbolTable.LookupNamespace(Namespace, 0, True);
  end;
end;

procedure AddPropInfosDRTTIImpl(C:TClass; PropInfos: TPropList);
var
  p: TRTTIProperty;
  t: TRTTIType;
  LastOffset: Integer;
begin
  if isPaxClass(C) then
    exit;
  if PropInfos.Count = 0 then
    LastOffset := 0
  else
    LastOffset := PropInfos.Top.PropOffset + SizeOf(Pointer);

  repeat
    t := PaxContext.GetType(C.ClassInfo);

    for p in t.GetDeclaredProperties do
      if CheckProperty(t, p) then
      begin
        PropInfos.Add(p, LastOffset);
        Inc(LastOffset, SizeOf(Pointer));
      end;

    C := C.ClassParent;
    if C = nil then
      break;

  until false;
end;

procedure Import_TValueImpl(Level: Integer; SymbolTable: TBaseSymbolTable);
var
  H, H_Sub: Integer;
begin
  with SymbolTable do
  begin
    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_VarFromTValue);
    Id_VarFromTValue := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typeINTEGER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIProperty);
    Id_GetDRTTIProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIIntegerProperty);
    Id_GetDRTTIIntegerProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIStringProperty);
    Id_GetDRTTIStringProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIExtendedProperty);
    Id_GetDRTTIExtendedProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIVariantProperty);
    Id_GetDRTTIVariantProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_GetDRTTIInt64Property);
    Id_GetDRTTIInt64Property := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H_Sub := RegisterRoutine(0, '', typeVOID, ccSTDCALL, @_SetDRTTIProperty);
    Id_SetDRTTIProperty := LastSubId;
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);
    RegisterParameter(H_Sub, typePOINTER, Unassigned);

    H := RegisterRecordType(Level, 'TValue', 1);
    H_TValue := H;
    RegisterTypeField(H, 'dummy1', typeINT64);
    RegisterTypeField(H, 'dummy2', typeINT64);
    RegisterTypeField(H, 'dummy3', typeINT64);

    RegisterHeader(H, 'function GetDataSize: Integer;',
      @ TValue_GetDataSize);

    RegisterHeader(H, 'class operator Implicit(const Value: string): TValue;',
      @ TValue_Implicit_String);

    RegisterHeader(H, 'class operator Implicit(Value: Extended): TValue;',
      @ TValue_Implicit_Extended);

    RegisterHeader(H, 'class operator Implicit(Value: Int64): TValue;',
      @ TValue_Implicit_Int64);

    RegisterHeader(H, 'class operator Implicit(Value: TObject): TValue;',
      @ TValue_Implicit_TObject);

    RegisterHeader(H, 'class operator Implicit(Value: TClass): TValue;',
      @ TValue_Implicit_TClass);

    RegisterHeader(H, 'class operator Implicit(Value: Boolean): TValue;',
      @ TValue_Implicit_Boolean);

    RegisterHeader(H, 'class operator Implicit(Value: Integer): TValue;',
      @ TValue_Implicit_Integer);
    Id_ImplicitInt := LastSubId;

    RegisterHeader(H, 'class function FromVariant(const Value: Variant): TValue; static;',
      @TValue.FromVariant);

//    class function From<T>(const Value: T): TValue; static;
    RegisterHeader(H, 'class function FromOrdinal(ATypeInfo: Pointer; AValue: Int64): TValue; static;',
      @TValue.FromOrdinal);

    RegisterHeader(H, 'class function FromArray(ArrayTypeInfo: Pointer; const Values: array of TValue): TValue; static;',
      @TValue.FromArray);

    // Easy out
    //property Kind: TTypeKind read GetTypeKind;
    //property TypeInfo: PTypeInfo read GetTypeInfo;
    //property TypeData: PTypeData read GetTypeDataProp;
    //property IsEmpty: Boolean read GetIsEmpty;

    RegisterHeader(H, 'function IsObject: Boolean;',
      @TValue.IsObject);
    RegisterHeader(H, 'function AsObject: TObject;',
      @TValue.AsObject);
    RegisterHeader(H, 'function IsInstanceOf(AClass: TClass): Boolean;',
      @TValue.IsInstanceOf);
    RegisterHeader(H, 'function IsClass: Boolean;',
      @TValue.IsClass);
    RegisterHeader(H, 'function AsClass: TClass;',
      @TValue.AsClass);
    RegisterHeader(H, 'function IsOrdinal: Boolean;',
      @ TValue.IsOrdinal);
    RegisterHeader(H, 'function AsOrdinal: Int64;',
      @TValue.AsOrdinal);
    RegisterHeader(H, 'function TryAsOrdinal(out AResult: Int64): Boolean;',
      @TValue.TryAsOrdinal);

    // TValue -> concrete type
    // IsType returns true if AsType or Cast would succeed
    // AsType / Cast are only for what would normally be implicit conversions in Delphi.

    // function IsType<T>: Boolean; overload;
    // function IsType(ATypeInfo: PTypeInfo): Boolean; overload;
    // function AsType<T>: T;
    // function TryAsType<T>(out AResult: T): Boolean;

    // TValue -> TValue conversions
    // function Cast<T>: TValue; overload;
    // function Cast(ATypeInfo: PTypeInfo): TValue; overload;
    // function TryCast(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean;

    RegisterHeader(H, 'function AsInteger: Integer;',
      @TValue.AsInteger);
    RegisterHeader(H, 'function AsBoolean: Boolean;',
      @TValue.AsBoolean);
    RegisterHeader(H, 'function AsExtended: Extended;',
      @TValue.AsExtended);
    RegisterHeader(H, 'function AsInt64: Int64;',
      @TValue.AsInt64);
    RegisterHeader(H, 'function AsInterface: IInterface;',
      @TValue.AsInterface);
    RegisterHeader(H, 'function AsInt64: Int64;',
      @TValue.AsInt64);
    RegisterHeader(H, 'function AsString: String;',
      @TValue.AsString);
    RegisterHeader(H, 'function AsVariant: Variant;',
      @TValue.AsVariant);
    RegisterHeader(H, 'function AsCurrency: Currency;',
      @TValue.AsCurrency);
    RegisterHeader(H, 'function IsArray: Boolean;',
      @TValue.IsArray);

    RegisterHeader(H, 'function GetArrayLength: Integer;',
      @TValue.GetArrayLength);
    RegisterHeader(H, 'function GetArrayElement(Index: Integer): TValue;',
      @TValue.GetArrayElement);
    RegisterHeader(H, 'procedure SetArrayElement(Index: Integer; const AValue: TValue);',
      @TValue.SetArrayElement);

    // Low-level in
    //class procedure Make(ABuffer: Pointer; ATypeInfo: PTypeInfo; out Result: TValue); overload; static;
    //class procedure MakeWithoutCopy(ABuffer: Pointer; ATypeInfo: PTypeInfo; out Result: TValue); overload; static;
    //class procedure Make(AValue: NativeInt; ATypeInfo: PTypeInfo; out Result: TValue); overload; static;

    // Low-level out
    RegisterHeader(H, 'property DataSize: Integer read GetDataSize;', nil);

    RegisterHeader(H, 'procedure ExtractRawData(ABuffer: Pointer);',
      @TValue.ExtractRawData);
    // If internal data is something with lifetime management, this copies a
    // reference out *without* updating the reference count.
    RegisterHeader(H, 'procedure ExtractRawDataNoCopy(ABuffer: Pointer);',
      @TValue.ExtractRawDataNoCopy);

    RegisterHeader(H, 'function GetReferenceToRawData: Pointer;',
      @TValue.GetReferenceToRawData);
    RegisterHeader(H, 'function GetReferenceToRawArrayElement(Index: Integer): Pointer;',
      @TValue.GetReferenceToRawArrayElement);

    RegisterHeader(H, 'function ToString: string;',
      @TValue.ToString);
  end;
end;

procedure InitializePAXCOMP_2010Reg;
begin
  RegisterDRTTIProperties := RegisterDRTTIPropertiesImpl;
  // Set event to get the namespace of a type.
  GetNamespaceOfType := GetNamespaceOfTypeImpl;
  AddPropInfosDRTTI := AddPropInfosDRTTIImpl;
  Import_TValue := Import_TValueImpl;
end;

initialization
  InitializePAXCOMP_2010Reg;
{$else}
implementation
{$endif}
end.

