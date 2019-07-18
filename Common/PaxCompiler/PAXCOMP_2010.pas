////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_2010.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_2010;
interface
{$ifdef DRTTI}
uses {$I uses.def}
  Classes,
  SysUtils,
  TypInfo,
  RTTI,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;
type
  TUnitList = class;
  TUnit = class;

  TType = class
  private
    // Added to reuse code for methods and property getters and setters.
    procedure DispatchMethod(aMethod: TRttiMethod);
    procedure DispatchType(AType: TRTTIType; Recursive: Boolean = true);
  public
    Owner: TUnit;

{$IFDEF ARC}
    UsedTypes: TList<TRTTIType>;
{$ELSE}
    UsedTypes: TList;
{$ENDIF}

    T: TRTTIType;
    constructor Create(AOwner: TUnit);
    destructor Destroy; override;
    procedure Expand;
  end;

  TUnit = class(TTypedList)
  private
    Z: Integer;
    Owner: TUnitList;
    function GetRecord(I: Integer): TType;
  public
    Level: Integer;
    Name: String;
    UsedUnits: TUpStringList;
{$IFDEF ARC}
    UsedTypes: TList<TRTTIType>;
{$ELSE}
    UsedTypes: TList;
{$ENDIF}
    constructor Create(AOwner: TUnitList; const AName: String);
    destructor Destroy; override;
    function IndexOf(t: TRTTIType): Integer;
    procedure AddType(t: TRTTIType; Recursive: Boolean = true);
    procedure Sort;
    property Records[I: Integer]: TType read GetRecord; default;
  end;

  TUnitList = class(TTypedList)
  private
    function GetRecord(I: Integer): TUnit;
  public
{$IFDEF ARC}
    ForbiddenClasses: TList<TClass>;
{$ELSE}
    ForbiddenClasses: TList;
{$ENDIF}
    AcceptList: TStrings;
    constructor Create;
    destructor Destroy; override;
    function AddUnit(const UnitName: String; ALevel: Integer = 0): TUnit;
    function IndexOf(const AUnitName: String): Integer;
    procedure ForbidClass(C: TClass);
    function AddClass(C: TClass): TUnit;
    function AddType(t: TRTTIType): TUnit;
    procedure AddAvailableTypes;
    procedure FindMembers(L: TStringList);
    procedure Sort;
    procedure Dump(const FileName: String);
    property Records[I: Integer]: TUnit read GetRecord; default;
  end;

function CheckType(t: TRTTIType): Boolean;
var
  PaxContext: TRTTIContext;

// Changed event from function to procedure to allow accepting of types as well.
// Added event to do additional checking on types (to exclude certain types).
type
  TCheckTypeEvent = procedure(aType: TRTTIType; var aAccept: Boolean) of object;
var
  OnCheckType: TCheckTypeEvent;

procedure CreateAvailTypes;

procedure Dump_Types(const FileName: String);
procedure Dump_Units(const FileName: String);
procedure Dump_Units1(const FileName: String);

function ExtractUnitName(t: TRTTIType): String;
procedure Initialize_paxcomp_2010;
procedure Finalize_paxcomp_2010;

implementation

uses PAXCOMP_STDLIB;

function ExtractUnitName(t: TRTTIType): String;
var
  I: Integer;
begin
  result := t.QualifiedName;
  I := Pos(t.Name, result);
  if I > 0 then
    Delete(result, I - 1, Length(t.Name) + 1);
end;

function CheckType(t: TRTTIType): Boolean;

  function CheckArray(t: TRTTIType): Boolean;
  var
    K: Integer;
    ta: TRTTIArrayType;
  begin
    result := true;
    if t is TRTTIArrayType then
    begin
      ta := t as TRTTIArrayType;
      K := ta.DimensionCount;
      if K > 1 then
        result := false
      else if ta.Dimensions[0] = nil then
        result := false;
    end;
  end;

var
  S: String;
begin
  result := false;
  if t = nil then
    Exit;

  result := true;
  if not t.IsPublicType then
    result := false
//  else if PosCh('.', t.Name) > 0 then
//    result := false
  else if t.Handle = TypeInfo(Comp) then
    result := false
  else if not CheckArray(t) then
    result := false
  else
  begin
    S := UpperCase(t.QualifiedName);
    if Pos('GENERICS.', S) > 0 then
      result := false;
  end;

  if Assigned(OnCheckType) then
    OnCheckType(t, result);
end;

// TType -------------------------------------------------------------------

constructor TType.Create(AOwner: TUnit);
begin
  inherited Create;
  Owner := AOwner;
{$IFDEF ARC}
  UsedTypes := TList<TRTTIType>.Create;
{$ELSE}
  UsedTypes := TList.Create;
{$ENDIF}
end;

destructor TType.Destroy;
begin
  FreeAndNil(UsedTypes);
  inherited;
end;

// Added DispatchMethod.
procedure TType.DispatchMethod(aMethod: TRttiMethod);
var
  Param: TRTTIParameter;
  t: TRTTIType;
begin
  for Param in aMethod.GetParameters do
  begin
    t := Param.ParamType;
    DispatchType(t, false);
  end;

  if aMethod.HasExtendedInfo then
  begin
    t := aMethod.ReturnType;
    DispatchType(t, false);
  end;
end;

procedure TType.DispatchType(AType: TRTTIType; Recursive: Boolean = true);
var
  UnitName: String;
  I: Integer;
  UnitList: TUnitList;
  U: TUnit;
begin
  if AType = T then
    Exit;
  if not CheckType(AType) then
    Exit;

  UnitList := Owner.Owner;
  UnitName := ExtractUnitName(AType);

  if StrEql(UnitName, Owner.Name) then
    if UsedTypes.IndexOf(AType) = -1 then
       UsedTypes.Add(AType);

  I := UnitList.IndexOf(UnitName);
  if I = -1 then
    U := UnitList.AddUnit(UnitName)
  else
    U := UnitList[I];

  if not StrEql(UnitName, Owner.Name) and (not StrEql(UnitName, 'System')) then
  begin
    I := Owner.UsedUnits.IndexOf(UnitName);
    if I = -1 then
      Owner.UsedUnits.Add(UnitName);
  end;

  U.AddType(AType, Recursive);
end;

procedure TType.Expand;
var
  method: TRttiMethod;
  field: TRTTIField;
  prop: TRTTIProperty;
{$IFDEF DPULSAR}
  IndexedProp: TRTTIIndexedProperty;
  it: TRttiInterfaceType;
{$ENDIF}
  AType: TRTTIType;
{$IFDEF DPULSAR}
  InstType: TRTTIInstanceType;
{$ENDIF}
begin
  AType := T.BaseType;
  while AType <> nil do
  begin
    DispatchType(AType, false);
    AType := AType.BaseType;
  end;

{$IFDEF DPULSAR}
  if T is TRTTIInstanceType then
  begin
    InstType := t as TRTTIInstanceType;
    for it in InstType.GetDeclaredImplementedInterfaces do
      DispatchType(it, false);
  end;
{$ENDIF}

  // Exclude record methods for two reasons:
  //   1. There is a bug in TRttiRecordMethod.GetReturnType. It uses the handle immediately but should use FSig.Handle.
  //   2. Record methods are not supported by the PAX compiler.
  if T.TypeKind <> tkRecord then
  begin
    // Replace GetMethods by GetDeclaredMethods.
    for method in T.GetDeclaredMethods do
    begin
      // Only dispatch public and published methods.
      if method.Visibility in [mvPublic, mvPublished] then
      begin
        if Owner.Owner.AcceptList = nil then
          DispatchMethod(method)
        else
          if Owner.Owner.AcceptList.IndexOf(method.Name) >= 0 then
            DispatchMethod(method);
      end;
    end;
  end;

  // Replace GetFields by GetDeclaredFields.
  for field in T.GetDeclaredFields do
  begin
    // Only dispatch public and published fields.
    if field.Visibility in [mvPublic, mvPublished] then
    begin
      AType := field.FieldType;
      DispatchType(AType, false);
    end;
  end;

  // Replace GetProperties by GetDeclaredProperties.
  for prop in T.GetDeclaredProperties do
  begin
    // Only dispatch public and published properties.
    if prop.Visibility in [mvPublic, mvPublished] then
    begin
      AType := prop.PropertyType;
      DispatchType(AType, false);
      AType := prop.Parent;
      DispatchType(AType, false);
    end;
  end;

{$IFDEF DPULSAR}
  // Added enumeration of indexed properties.
  for IndexedProp in T.GetDeclaredIndexedProperties do
  begin
    if IndexedProp.Visibility in [mvPublic, mvPublished] then
    begin
      AType := IndexedProp.PropertyType;
      DispatchType(AType, false);
      AType := IndexedProp.Parent;
      DispatchType(AType, false);
      if IndexedProp.IsReadable then
        DispatchMethod(IndexedProp.ReadMethod);
      if IndexedProp.IsWritable then
        DispatchMethod(IndexedProp.WriteMethod);
    end;
  end;
{$ENDIF}
end;

// TUnit -------------------------------------------------------------------

constructor TUnit.Create(AOwner: TUnitList; const AName: String);
begin
  inherited Create;
  Owner := AOwner;
  Name := AName;
  UsedUnits := TUpStringList.Create;
{$IFDEF ARC}
  UsedTypes := TList<TRTTIType>.Create;
{$ELSE}
  UsedTypes := TList.Create;
{$ENDIF}
end;

destructor TUnit.Destroy;
begin
  FreeAndNil(UsedUnits);
  FreeAndNil(UsedTypes);
  inherited;
end;

function TUnit.GetRecord(I: Integer): TType;
begin
  result := TType(L[I]);
end;

function TUnit.IndexOf(t: TRTTIType): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].T = t then
    begin
      result := I;
      Exit;
    end;
end;

procedure TUnit.AddType(t: TRTTIType; Recursive: Boolean = true);
var
  R: TType;
  I: Integer;
  b: TRTTIType;
begin
  if not CheckType(t) then
    Exit;

  I := IndexOf(t);
  if I = -1 then
    R := TType.Create(Self)
  else
    Exit;

  R.T := t;
  L.Add(R);

  if t is TRttiDynamicArrayType then
  begin
    b := (t as TRttiDynamicArrayType).ElementType;
    if b <> nil then
      R.UsedTypes.Add(b);
  end
  else if t is TRttiArrayType then
  begin
    b := (t as TRttiArrayType).ElementType;
    if b <> nil then
      R.UsedTypes.Add(b);
  end
  else if t is TRttiPointerType then
  begin
    b := (t as TRttiPointerType).ReferredType;
    if b <> nil then
      R.UsedTypes.Add(b);
  end
  else
  begin
    b := t.BaseType;
    if b <> nil then
      if t.UnitName = b.UnitName then
        R.UsedTypes.Add(b);
  end;

  if Recursive then
    R.Expand;
end;

procedure TUnit.Sort;
var
  A: array of TType;
  I, J, K, InitCount: Integer;
  anc: TRTTIType;
  found: Boolean;
begin
  SetLength(A, Count);
  K := -1;
  for I := L.Count - 1 downto 0 do
    if Records[I].UsedTypes.Count = 0 then
    begin
      Inc(K);
      A[K] := Records[I];
      L.Delete(I);
    end;

  repeat
    InitCount := L.Count;
    for I := L.Count - 1 downto 0 do
    begin
      anc :=  Records[I].UsedTypes[0];
      found := false;
      for J := 0 to L.Count - 1 do
        if I <> J then
          if Records[J].T = anc then
          begin
            found := true;
            break;
          end;
      if not Found then
      begin
        Inc(K);
        A[K] := Records[I];
        L.Delete(I);
        break;
      end;
    end;
  until L.Count = InitCount;

  for I := L.Count - 1 downto 0 do
    begin
      Inc(K);
      A[K] := Records[I];
      L.Delete(I);
    end;

  for I := 0 to K do
    L.Add(A[I]);
end;

// TUnitList -------------------------------------------------------------------

constructor TUnitList.Create;
begin
  inherited;
{$IFDEF ARC}
  ForbiddenClasses := TList<TClass>.Create;
{$ELSE}
  ForbiddenClasses := TList.Create;
{$ENDIF}
end;

destructor TUnitList.Destroy;
begin
  FreeAndNil(ForbiddenClasses);
  inherited;
end;

function TUnitList.GetRecord(I: Integer): TUnit;
begin
  result := TUnit(L[I]);
end;

function TUnitList.IndexOf(const AUnitName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].Name, AUnitName) then
    begin
      result := I;
      Exit;
    end;
end;

function TUnitList.AddUnit(const UnitName: String; ALevel: Integer = 0): TUnit;
begin
  result := TUnit.Create(Self, UnitName);
  result.Level := ALevel;
  L.Add(result);
end;

procedure TUnitList.ForbidClass(C: TClass);
begin
  ForbiddenClasses.Add(C);
end;

function TUnitList.AddClass(C: TClass): TUnit;
var
  UnitName: String;
  I: Integer;
begin
  UnitName := C.UnitName;
  I := IndexOf(UnitName);
  if I = -1 then
    result := AddUnit(UnitName)
  else
    result := Records[I];
  result.AddType(PaxContext.GetType(C));
end;

function TUnitList.AddType(t: TRTTIType): TUnit;
var
  UnitName: String;
  I: Integer;
begin
  if not CheckType(t) then
  begin
    result := nil;
    Exit;
  end;

  UnitName := ExtractUnitName(t);
  I := IndexOf(UnitName);
  if I = -1 then
    result := AddUnit(UnitName)
  else
    result := Records[I];
  result.AddType(t);
end;

procedure TUnitList.AddAvailableTypes;
var
  t: TRTTIType;
begin
  for t in PaxContext.GetTypes do
  begin
    if t.IsPublicType then
    begin
      AddType(t);
    end;
  end;
end;

procedure TUnitList.FindMembers(L: TStringList);
var
  t: TRTTIType;
  u: TUnit;
  I, J: Integer;
begin
  AcceptList := L;
  for I := 0 to Count - 1 do
  begin
    u := Records[I];
    for J := u.UsedTypes.Count - 1 downto 0 do
    begin
      t := u.UsedTypes[J];
      AddType(t);
    end;
  end;
end;

procedure TUnitList.Sort;
var
  I, J, K: Integer;
  A: array of TUnit;
  UnitName: String;
  found: Boolean;
begin
  for I := 0 to Count - 1 do
  begin
    Records[I].Sort;
    Records[I].Z := Records[I].UsedUnits.Count;
  end;
  SetLength(A, Count);

  K := -1;

  repeat
    found := false;

    for I := 0 to Count - 1 do
      if Records[I].Z = 0 then
      begin
        found := true;

        Records[I].Z := -1;
        Inc(K);
        A[K] := Records[I];

        UnitName := Records[I].Name;

        for J := 0 to Count - 1 do
          if I <> J then
             if Records[J].Z > 0 then
               if Records[J].UsedUnits.IndexOf(UnitName) >= 0 then
                 Dec(Records[J].Z);
      end;

  until not found;

  Assert(K + 1 = Count, errInternalError);

  L.Clear;

  for I := 0 to K do
    L.Add(A[I]);
end;

procedure TUnitList.Dump(const FileName: String);
var
  L: TStringList;
  I, J, K: Integer;
  U: TUnit;
  S: String;
  t: TRTTIType;
begin
  if not IsDump then
    Exit;

  L := TStringList.Create;
  try
    for I := 0 to Count - 1 do
    begin
      U := Records[I];

      L.Add('Unit: ' + U.Name);
      L.Add('----');
      for J := 0 to U.UsedUnits.Count - 1 do
        L.Add('    ' + U.UsedUnits[J]);
      L.Add('----');

      for J := 0 to U.Count - 1 do
      begin

        S := '';
        for K := 0 to U[J].UsedTypes.Count - 1 do
        begin
          t := TRTTIType(U[J].UsedTypes[K]);
          S := S + t.Name;
          if K < U[J].UsedTypes.Count - 1 then
            S := S + ',';
        end;

        L.Add(U[J].T.Name + '(' + S + ')');
      end;

      L.Add('--------------------------------');
      L.Add('');
      L.Add('');
      L.Add('');
    end;
    L.SaveToFile(DUMP_PATH + FileName);
  finally
    FreeAndNil(L);
  end;
end;

procedure Dump_Types(const FileName: String);
var
  t: TRTTIType;
  L: TStringList;
  c: TRTTIContext;
begin
  L := TStringList.Create;
  c := TRTTIContext.Create;
  for t in c.GetTypes do
  begin
    if t.IsPublicType then
      L.Add(t.QualifiedName);
  end;
  L.SaveToFile(DUMP_PATH + FileName);
  FreeAndNil(L);
end;

procedure CreateAvailTypes;
var
  t: TRTTIType;
  S, UnitName: String;
begin
  AvailTypeList.Clear;
  for t in PaxContext.GetTypes do
    if t.IsPublicType then
    begin
      AvailTypeList.AddObject(t.QualifiedName, Pointer(t));
      UnitName := ExtractUnitName(t);
      if UnitName <> '' then
      begin
        AvailUnitList.Add(UnitName);
        S := ExtractOwner(UnitName);
        if S <> '' then
          AvailUnitList1.Add(S);
      end;
    end;
end;

procedure Dump_Units(const FileName: String);
begin
  AvailUnitList.SaveToFile(DUMP_PATH + FileName);
end;

procedure Dump_Units1(const FileName: String);
begin
  AvailUnitList1.SaveToFile(DUMP_PATH + FileName);
end;

procedure Initialize_paxcomp_2010;
begin
  PaxContext := TRTTIContext.Create;
end;

procedure Finalize_paxcomp_2010;
begin
  PaxContext.Free;
end;

{$else}  // Delphi version < 210


implementation
{$endif}
end.
