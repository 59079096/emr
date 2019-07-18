////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_MAP.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_MAP;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_OFFSET;
type
  TMapFieldRec = class
  public
    FieldName: String;
    FieldOffset: Integer;
{$IFDEF PCU_EX}
    FieldTypeName: String;
{$ENDIF}
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TMapFieldList = class(TTypedList)
  private
    function GetRecord(I: Integer): TMapFieldRec;
  public
    function Add(const FieldName: String;
                 FieldOffset: Integer;
                 const FieldTypeName: String): TMapFieldRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function Lookup(const FieldName: String): TMapFieldRec;
    property Records[I: Integer]: TMapFieldRec read GetRecord; default;
  end;

  TSubParamRec = class
  public
    FinTypeId: Byte;
    ParamMod: Byte;
    ParamSize: Integer;
    // pcu only
    ParamName: String;
    ParamTypeName: String;
    OptValue: String;
    ParamOffset: Integer;
  end;

  TSubParamList = class(TTypedList)
  private
    function GetRecord(I: Integer): TSubParamRec;
  public
    function IndexOf(const AName: String): Integer;
    function AddRecord: TSubParamRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TSubParamRec read GetRecord; default;
  end;

  TSubLocalVarRec = class
  public
    LocalVarName: String;
    LocalVarTypeName: String;
    LocalVarOffset: Integer;
    IsByRef: Boolean;
  end;

  TSubLocalVarList = class(TTypedList)
  private
    function GetRecord(I: Integer): TSubLocalVarRec;
  public
    function IndexOf(const AName: String): Integer;
    function AddRecord: TSubLocalVarRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TSubLocalVarRec
      read GetRecord; default;
  end;

  TSubDesc = class
  public
    OverCount: Byte;
    CallConv: Byte;
    CallMode: Byte;
    ResTypeId: Byte;
    MethodIndex: Integer;
    RetSize: Integer;
    ParamList: TSubParamList;

    // pcu only
    LocalVarList: TSubLocalVarList;
    ResTypeName: String;
    IsMethod: Boolean;
    IsShared: Boolean;
    SubName: String;
    DllName: String;
    AliasName: String;

    SId: Integer;
    N1: Integer;
    N2: Integer;
    SelfOffset: Integer;
    SubSize: Integer; // not save to stream

    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TSubDescList = class(TTypedList)
  private
    function GetRecord(I: Integer): TSubDesc;
  public
    function AddRecord: TSubDesc;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TSubDesc read GetRecord; default;
  end;

  TMapRec = class
  private
    function GetIsMethod: Boolean;
  public
    FullName: String;
    Shift: Integer;
    Offset: Integer;
    ClassIndex: Integer;
    Kind: Byte;
    Global: Boolean;
    Vis: TClassVisibility;
    TypedConst: Boolean;
    FullTypeName: String;
    IsExternal: Boolean;
    SubDesc: TSubDesc;
    FieldList: TMapFieldList;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property IsMethod: Boolean read GetIsMethod;
  end;

  TMapTable = class(TTypedList)
  private
    function GetRecord(I: Integer): TMapRec;
    function Add: TMapRec;
  public
    function AddRec(const FullName: String;
                    Shift: Integer;
                    ClassIndex: Integer;
                    Kind: Byte;
                    Global: Boolean;
                    OverCount: Byte;
                    CallMode: Byte): TMapRec;
    function LookupByOffset(Offset: Integer): TMapRec;
    function Lookup(const FullName: String): TMapRec;
    function LookupEx(const FullName: String; OverCount: Integer): TMapRec;
    function LookupType(const FullName: String): TMapRec;
    function LookupConstructor(const AClassName: String; NP: Integer): TMapRec;
    function LookupSub(SubId: Integer): TMapRec;
    function GetSub(N: Integer): TMapRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure CreateOffsets(OffsetList: TOffsetList; Host: Boolean);
    property Records[I: Integer]: TMapRec read GetRecord; default;
  end;

  TTypeMapRec = class
  public
    TypeId: Integer;
    Fields: TIntegerList;
    Completed: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TTypeMap = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeMapRec;
  public
    function Add(TypeId: Integer): TTypeMapRec;
    function Lookup(TypeId: Integer): TTypeMapRec;
    property Records[I: Integer]: TTypeMapRec read GetRecord; default;
  end;

implementation

// TSubLocalVarList ------------------------------------------------------------

function TSubLocalVarList.GetRecord(I: Integer): TSubLocalVarRec;
begin
  result := TSubLocalVarRec(L[I]);
end;

function TSubLocalVarList.IndexOf(const AName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].LocalVarName, AName) then
    begin
      result := I;
      Exit;
    end;
end;

function TSubLocalVarList.AddRecord: TSubLocalVarRec;
begin
  result := TSubLocalVarRec.Create;
  L.Add(result);
end;

procedure TSubLocalVarList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));

  for I := 0 to K - 1 do
    with Records[I] do
    begin
      SaveStringToStream(LocalVarName, S);
      SaveStringToStream(LocalVarTypeName, S);
      S.Write(LocalVarOffset, SizeOf(LocalVarOffset));
      S.Write(IsByRef, SizeOf(IsByRef));
    end;
end;

procedure TSubLocalVarList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TSubLocalVarRec;
begin
  S.Read(K, SizeOf(Integer));

  for I := 0 to K - 1 do
  begin
    R := AddRecord;
    with R do
    begin
      LocalVarName := LoadStringFromStream(S);
      LocalVarTypeName := LoadStringFromStream(S);
      S.Read(LocalVarOffset, SizeOf(LocalVarOffset));
      S.Read(IsByRef, SizeOf(IsByRef));
    end;
  end;
end;

// TSubParamList ---------------------------------------------------------------

function TSubParamList.GetRecord(I: Integer): TSubParamRec;
begin
  result := TSubParamRec(L[I]);
end;

function TSubParamList.IndexOf(const AName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].ParamName, AName) then
    begin
      result := I;
      Exit;
    end;
end;

function TSubParamList.AddRecord: TSubParamRec;
begin
  result := TSubParamRec.Create;
  L.Add(result);
end;

procedure TSubParamList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));

  for I := 0 to K - 1 do
    with Records[I] do
    begin
      S.Write(FinTypeId, SizeOf(FinTypeId));
      S.Write(ParamMod, SizeOf(ParamMod));
      S.Write(ParamSize, SizeOf(ParamSize));
      SaveStringToStream(ParamName, S);
      SaveStringToStream(ParamTypeName, S);
      SaveStringToStream(OptValue, S);
{$IFDEF PCU_EX}
      S.Write(ParamOffset, SizeOf(ParamOffset));
{$ENDIF}
    end;
end;

procedure TSubParamList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TSubParamRec;
begin
  S.Read(K, SizeOf(Integer));

  for I := 0 to K - 1 do
  begin
    R := AddRecord;
    with R do
    begin
      S.Read(FinTypeId, SizeOf(FinTypeId));
      S.Read(ParamMod, SizeOf(ParamMod));
      S.Read(ParamSize, SizeOf(ParamSize));
      ParamName := LoadStringFromStream(S);
      ParamTypeName := LoadStringFromStream(S);
      OptValue := LoadStringFromStream(S);
{$IFDEF PCU_EX}
      S.Read(ParamOffset, SizeOf(ParamOffset));
{$ENDIF}
    end;
  end;
end;

// TSubDesc --------------------------------------------------------------------

constructor TSubDesc.Create;
begin
  inherited;
  ParamList := TSubParamList.Create;
  LocalVarList := TSubLocalVarList.Create;
end;

destructor TSubDesc.Destroy;
begin
  FreeAndNil(ParamList);
  FreeAndNil(LocalVarList);
  inherited;
end;

procedure TSubDesc.SaveToStream(S: TStream);
begin
  S.Write(OverCount, SizeOf(OverCount));
  S.Write(CallConv, SizeOf(CallConv));
  S.Write(CallMode, SizeOf(CallMode));
  S.Write(ResTypeId, SizeOf(ResTypeId));
  S.Write(MethodIndex, SizeOf(MethodIndex));
  S.Write(RetSize, SizeOf(RetSize));
  S.Write(IsMethod, SizeOf(IsMethod));
  S.Write(IsShared, SizeOf(IsShared));
  SaveStringToStream(ResTypeName, S);
  ParamList.SaveToStream(S);
  SaveStringToStream(SubName, S);
  SaveStringToStream(DllName, S);
  SaveStringToStream(AliasName, S);
{$IFDEF PCU_EX}
  LocalVarList.SaveToStream(S);
  S.Write(SId, SizeOf(SId));
  S.Write(N1, SizeOf(N1));
  S.Write(N2, SizeOf(N2));
  S.Write(SelfOffset, SizeOf(SelfOffset));
{$ENDIF}
end;

procedure TSubDesc.LoadFromStream(S: TStream);
begin
  S.Read(OverCount, SizeOf(OverCount));
  S.Read(CallConv, SizeOf(CallConv));
  S.Read(CallMode, SizeOf(CallMode));
  S.Read(ResTypeId, SizeOf(ResTypeId));
  S.Read(MethodIndex, SizeOf(MethodIndex));
  S.Read(RetSize, SizeOf(RetSize));
  S.Read(IsMethod, SizeOf(IsMethod));
  S.Read(IsShared, SizeOf(IsShared));
  ResTypeName := LoadStringFromStream(S);
  ParamList.Clear;
  ParamList.LoadFromStream(S);
  SubName := LoadStringFromStream(S);
  DllName := LoadStringFromStream(S);
  AliasName := LoadStringFromStream(S);
{$IFDEF PCU_EX}
  LocalVarList.Clear;
  LocalVarList.LoadFromStream(S);
  S.Read(SId, SizeOf(SId));
  S.Read(N1, SizeOf(N1));
  S.Read(N2, SizeOf(N2));
  S.Read(SelfOffset, SizeOf(SelfOffset));
{$ENDIF}
end;

// TSubDescList ----------------------------------------------------------------

function TSubDescList.GetRecord(I: Integer): TSubDesc;
begin
  result := TSubDesc(L[I]);
end;

function TSubDescList.AddRecord: TSubDesc;
begin
  result := TSubDesc.Create;
  L.Add(result);
end;

procedure TSubDescList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TSubDescList.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    AddRecord.LoadFromStream(S);
end;

// TTypeMapRec -----------------------------------------------------------------

constructor TTypeMapRec.Create;
begin
  inherited;
  Fields := TIntegerList.Create;
end;

destructor TTypeMapRec.Destroy;
begin
  FreeAndNil(Fields);
  inherited;
end;

// TTypeMap --------------------------------------------------------------------

function TTypeMap.Lookup(TypeId: Integer): TTypeMapRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if Records[I].TypeId = TypeId then
    begin
      result := Records[I];
      Exit;
    end;
end;

function TTypeMap.GetRecord(I: Integer): TTypeMapRec;
begin
  result := TTypeMapRec(L[I]);
end;

function TTypeMap.Add(TypeId: Integer): TTypeMapRec;
begin
  result := Lookup(TypeId);
  if result <> nil then
    Exit;
  result := TTypeMapRec.Create;
  result.TypeId := TypeId;
  L.Add(result);
end;

// TMapFieldRec ----------------------------------------------------------------

procedure TMapFieldRec.SaveToStream(S: TStream);
begin
  SaveStringToStream(FieldName, S);
  S.Write(FieldOffset, SizeOf(FieldOffset));
{$IFDEF PCU_EX}
  SaveStringToStream(FieldTypeName, S);
{$ENDIF}
end;

procedure TMapFieldRec.LoadFromStream(S: TStream);
begin
  FieldName := LoadStringFromStream(S);
  S.Read(FieldOffset, SizeOf(FieldOffset));
{$IFDEF PCU_EX}
  FieldTypeName := LoadStringFromStream(S);
{$ENDIF}
end;

// TMapFieldList ---------------------------------------------------------------

function TMapFieldList.GetRecord(I: Integer): TMapFieldRec;
begin
  result := TMapFieldRec(L[I]);
end;

function TMapFieldList.Lookup(const FieldName: String): TMapFieldRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].FieldName, FieldName) then
    begin
      result := Records[I];
      Exit;
    end;
end;

procedure TMapFieldList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to Count - 1 do
    Records[I].SaveToStream(S);
end;

procedure TMapFieldList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TMapFieldRec;
begin
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
  begin
    R := TMapFieldRec.Create;
    R.LoadFromStream(S);
    L.Add(R);
  end;
end;

function TMapFieldList.Add(const FieldName: String;
                           FieldOffset: Integer;
                           const FieldTypeName: String): TMapFieldRec;
begin
  result := TMapFieldRec.Create;
  result.FieldName := FieldName;
  result.FieldOffset := FieldOffset;
{$IFDEF PCU_EX}
  result.FieldTypeName := FieldTypeName;
{$ENDIF}
  L.Add(result);
end;

// TMapRec ---------------------------------------------------------------------

constructor TMapRec.Create;
begin
  inherited;
  FieldList := nil;
  SubDesc := TSubDesc.Create;
end;

destructor TMapRec.Destroy;
begin
  if Assigned(FieldList) then
    FreeAndNil(FieldList);

  FreeAndNil(SubDesc);

  inherited;
end;

function TMapRec.GetIsMethod: Boolean;
begin
  result := SubDesc.IsMethod;
end;

procedure TMapRec.SaveToStream(S: TStream);
begin
  SaveStringToStream(FullName, S);
  S.Write(Offset, SizeOf(Offset));
  S.Write(ClassIndex, SizeOf(ClassIndex));
  S.Write(Kind, SizeOf(Kind));
  S.Write(Global, SizeOf(Global));
  S.Write(Vis, SizeOf(Vis));
  S.Write(TypedConst, SizeOf(TypedConst));
  S.Write(IsExternal, SizeOf(IsExternal));
  SaveStringToStream(FullTypeName, S);

  SubDesc.SaveToStream(S);

  if ClassIndex > 0 then
    FieldList.SaveToStream(S);
end;

procedure TMapRec.LoadFromStream(S: TStream);
begin
  FullName := LoadStringFromStream(S);
  S.Read(Offset, SizeOf(Offset));
  S.Read(ClassIndex, SizeOf(ClassIndex));
  S.Read(Kind, SizeOf(Kind));
  S.Read(Global, SizeOf(Global));
  S.Read(Vis, SizeOf(Vis));
  S.Read(TypedConst, SizeOf(TypedConst));
  S.Read(IsExternal, SizeOf(IsExternal));
  FullTypeName := LoadStringFromStream(S);

  SubDesc.LoadFromStream(S);

  if ClassIndex > 0 then
  begin
    if Assigned(FieldList) then
      FreeAndNil(FieldList);

    FieldList := TMapFieldList.Create;
    FieldList.LoadFromStream(S);
  end;
end;

// TMapTable ------------------------------------------------------------------------

function TMapTable.GetRecord(I: Integer): TMapRec;
begin
  result := TMapRec(L[I]);
end;

function TMapTable.Add: TMapRec;
begin
  result := TMapRec.Create;
  L.Add(result);
end;

function TMapTable.AddRec(const FullName: String;
                          Shift: Integer; ClassIndex: Integer;
                          Kind: Byte;
                          Global: Boolean;
                          OverCount: Byte;
                          CallMode: Byte): TMapRec;
begin
  result := Add;
  result.FullName := FullName;
  result.Shift := Shift;
  result.ClassIndex := ClassIndex;
  result.Kind := Kind;
  result.Global := Global;

  result.SubDesc.OverCount := OverCount;
  result.SubDesc.CallMode := CallMode;

  if ClassIndex > 0 then
    result.FieldList := TMapFieldList.Create;
end;

procedure TMapTable.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I:=0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TMapTable.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TMapRec;
begin
  Clear;

  S.Read(K, SizeOf(Integer));
  for I:=0 to K - 1 do
  begin
    R := Add;
    R.LoadFromStream(S);
  end;
end;

function TMapTable.Lookup(const FullName: String): TMapRec;
var
  I: Integer;
  S: String;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if StrEql(Records[I].FullName, FullName) then
    begin
      result := Records[I];
      Exit;
    end;

  if ChCount(FullName, '.') <> 1 then
    Exit;

  for I:=0 to Count - 1 do
  begin
    S := Records[I].FullName;
    if ChCount(S, '.') = 2 then
      if StrEql(FullName, ExtractFullName(S)) then
      begin
        result := Records[I];
        Exit;
      end;
  end;
end;

function TMapTable.LookupEx(const FullName: String; OverCount: Integer): TMapRec;
var
  I: Integer;
  S: String;
begin
  result := nil;
  for I:=0 to Count - 1 do
  begin
    if Records[I].Kind in KindSUBS then
    begin
      if Records[I].SubDesc.OverCount = OverCount then
      if StrEql(Records[I].FullName, FullName) then
      begin
        result := Records[I];
        Exit;
      end;
    end
    else
    begin
      if StrEql(Records[I].FullName, FullName) then
      begin
        result := Records[I];
        Exit;
      end;
    end;
  end;

  if ChCount(FullName, '.') <> 1 then
    Exit;
  for I:=0 to Count - 1 do
    if Records[I].Kind in KindSUBS then
    begin
      if Records[I].SubDesc.OverCount = OverCount then
      begin
        S := Records[I].FullName;
        if ChCount(S, '.') = 2 then
          if StrEql(FullName, ExtractFullName(S)) then
          begin
            result := Records[I];
            Exit;
          end;
      end;
    end
    else
    begin
      S := Records[I].FullName;
      if ChCount(S, '.') = 2 then
        if StrEql(FullName, ExtractFullName(S)) then
        begin
          result := Records[I];
          Exit;
        end;
    end;
end;

function TMapTable.LookupByOffset(Offset: Integer): TMapRec;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if Records[I].Offset = Offset then
    begin
      result := Records[I];
      Exit;
    end;
end;

function TMapTable.LookupConstructor(const AClassName: String; NP: Integer): TMapRec;
var
  I: Integer;
  S: String;
  MR: TMapRec;
begin
  result := nil;
  for I:=0 to Count - 1 do
  begin
    MR := Records[I];
    if MR.Kind = kindCONSTRUCTOR then
      if MR.SubDesc.ParamList.Count = NP then
      begin
        S := ExtractClassName(MR.FullName);
        if StrEql(S, AClassName) then
        begin
          result := MR;
          Exit;
        end;
      end;
  end;
end;

function TMapTable.LookupType(const FullName: String): TMapRec;
var
  I: Integer;
  S1, S2: String;
begin
  result := nil;

  S1 := ExtractName(FullName);
  for I:=0 to Count - 1 do
  if Records[I].Kind = kindTYPE then
  begin
    S2 := ExtractName(Records[I].FullName);
    if StrEql(S1, S2) then
    begin
      result := Records[I];
      Exit;
    end;
  end;
end;

function TMapTable.LookupSub(SubId: Integer): TMapRec;
var
  I: Integer;
  MR: TMapRec;
begin
  result := nil;
  for I:=0 to Count - 1 do
  begin
    MR := Records[I];
    if MR.Kind in kindSUBS then
      if MR.SubDesc.SId = SubId then
      begin
        result := MR;
        Exit;
      end;
  end;
end;

procedure TMapTable.CreateOffsets(OffsetList: TOffsetList; Host: Boolean);
var
  I, S, Q: Integer;
  MapRec: TMapRec;
begin
  if OffsetList.Count > 0 then
    for I := 0 to Count - 1 do
    begin
      MapRec := Records[I];

      if (not Host) and (MapRec.Kind in KindSUBS) then
      begin
        MapRec.Offset := MapRec.Shift;
        continue;
      end;

      S := MapRec.Shift;
      if S > 0 then
      begin
        Q := OffsetList.GetOffset(S);
        if Q = -1 then
          raise Exception.Create(errInternalError);
        MapRec.Offset := Q;
      end;
    end
  else
    for I := 0 to Count - 1 do
    begin
      MapRec := Records[I];
      MapRec.Offset := MapRec.Shift;
    end;
end;

function TMapTable.GetSub(N: Integer): TMapRec;
var
  I: Integer;
  MR: TMapRec;
begin
  result := nil;
  for I := 0 to Count - 1 do
  begin
    MR := Records[I];
    if MR.Kind in KindSUBS then
      if (N >= MR.SubDesc.N1) and (N <= MR.SubDesc.N2) then
      begin
        result := MR;
        Exit;
      end;
  end;
end;

end.

