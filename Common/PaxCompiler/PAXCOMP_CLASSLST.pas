////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_CLASSLST.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_CLASSLST;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

type
  TPropRec = class
  public
    PropInfo: Pointer;
    PropOffset: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TPropList = class(TTypedList)
  private
    function GetInfo(I: Integer): PPropInfo;
  public
    procedure Add(P: Pointer; S: Integer);
    function Top: TPropRec;
    property Infos[I: Integer]: PPropInfo read GetInfo; default;
  end;

  TIntfMethodRec = class
  public
    MethodOffset: IntPax;
    InterfaceToObjectOffset: Integer;
    FullMethodName: String;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TIntfMethodList = class(TTypedList)
  private
    function GetRecord(I: Integer): TIntfMethodRec;
    function AddRecord: TIntfMethodRec;
  public
    function AddMethod(const FullMethodName: String;
                       MethodOffset: IntPax;
                       InterfaceToObjectOffset: Integer): TIntfMethodRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TIntfMethodRec read GetRecord; default;
  end;

  TIntfRec = class
  private
    fBuffSize: Integer;
  public
    GUID: TGUID;
    Buff: PPointers;
    IntfMethods: TIntfMethodList;
    constructor Create;
    destructor Destroy; override;
    procedure AllocBuff;
    procedure SetupBuff(CodePtr: Pointer);
    procedure DeallocBuff;
    procedure SaveToStream(P: TStream);
    procedure LoadFromStream(P: TStream);
    property BuffSize: Integer read fBuffSize;
  end;

  TIntfList = class(TTypedList)
  private
    function GetRecord(I: Integer): TIntfRec;
  public
    function Add: TIntfRec;
    procedure Setup(CodePtr: Pointer);
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function Lookup(const GUID: TGUID): TIntfRec;
    function IndexOf(const GUID: TGUID): Integer;
    property Records[I: Integer]: TIntfRec read GetRecord; default;
  end;

  TClassRec = class
  private
    procedure AddPropInfos;
  public
    PClass: TClass;
    PClass_pti: PTypeInfo;
    PropInfos: TPropList;

    Offset: Integer;
    SizeOfScriptClassFields: Integer;
    Host: Boolean;

    DestructorProgOffset: Integer;
    AfterConstructionProgOffset: Integer;
    BeforeDestructionProgOffset: Integer;
    SafeCallExceptionProgOffset: Integer;
    DispatchProgOffset: Integer;
    DefaultHandlerProgOffset: Integer;
    NewInstanceProgOffset: Integer;
    FreeInstanceProgOffset: Integer;
{$IFDEF UNIC}
    ToStringProgOffset: Integer;
    GetHashCodeProgOffset: Integer;
    EqualsProgOffset: Integer;
{$ENDIF}
    InstSize: Integer;

    FullName: String;
    ParentFullName: String;
    IntfList: TIntfList;
    ByteCodeMethodEntryList: TIntegerDynArray;
    VirtualMethodEntryList: array[1..100] of Integer;
    constructor Create(i_PClass: TClass; i_Offset: Integer; i_Host: Boolean);
    destructor Destroy; override;
    procedure SetupInterfaces(CodePtr: Pointer);
    function GetIntfOffset(const GUID: TGUID): Integer;
    function GetIntfTableSize: Integer;
  end;

  TClassList = class
  private
    L: TStringList;
    function GetClassRec(I: Integer): TClassRec;
    function GetName(I: Integer): String;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(const S: String): Integer;
    function FindClass(C: TClass): Integer;
    function Add(const FullName: String; Host: Boolean): TClassRec;
    function AddEx(const FullName: String; ClassIndex: Integer): TClassRec;
    function AddClass(C: TClass; const FullName: String;
                      Host: Boolean; Offset: Integer): TClassRec;
    function AddClassEx(C: TClass;
                        const FullName: String;
                        Host: Boolean;
                        Offset: Integer;
                        ClassIndex: Integer): TClassRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream; Version: Integer);
    function GetSize: Integer;
    procedure SetupInterfaces(CodePtr: Pointer);
    function Lookup(const FullName: String): TClassRec;
    function LookupClassRec(C: TClass): TClassRec;
    function GetByteCodeMethodEntryIndex(N: Integer): Integer;
    property Count: Integer read GetCount;
    property Names[I: Integer]: String read GetName;
    property Records[I: Integer]: TClassRec read GetClassRec; default;
  end;

var
  AddPropInfosDRTTI: procedure(C: TClass; PropInfos: TPropList) = nil;

implementation

// TIntfMethodRec --------------------------------------------------------------

procedure TIntfMethodRec.SaveToStream(S: TStream);
begin
  S.Write(MethodOffset, SizeOf(MethodOffset));
  S.Write(InterfaceToObjectOffset, SizeOf(InterfaceToObjectOffset));
  SaveStringToStream(FullMethodName, S);
end;

procedure TIntfMethodRec.LoadFromStream(S: TStream);
begin
  S.Read(MethodOffset, SizeOf(MethodOffset));
  S.Read(InterfaceToObjectOffset, SizeOf(InterfaceToObjectOffset));
  FullMethodName := LoadStringFromStream(S);
end;

// TIntfMethodList -------------------------------------------------------------

function TIntfMethodList.GetRecord(I: Integer): TIntfMethodRec;
begin
  result := TIntfMethodRec(L[I]);
end;

function TIntfMethodList.AddRecord: TIntfMethodRec;
begin
  result := TIntfMethodRec.Create;
  L.Add(result);
end;

function TIntfMethodList.AddMethod(const FullMethodName: String;
                                   MethodOffset: IntPax;
                                   InterfaceToObjectOffset: Integer): TIntfMethodRec;
begin
  result := AddRecord;
  result.FullMethodName := FullMethodName;
  result.MethodOffset := MethodOffset;
  result.InterfaceToObjectOffset := InterfaceToObjectOffset;
end;

procedure TIntfMethodList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TIntfMethodList.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    AddRecord.LoadFromStream(S);
end;

// -- TIntfRec -----------------------------------------------------------------

constructor TIntfRec.Create;
begin
  inherited;
  IntfMethods := TIntfMethodList.Create;
  fBuffSize := 0;
end;

destructor TIntfRec.Destroy;
begin
  FreeAndNil(IntfMethods);
  if Buff <> nil then
    DeallocBuff;
  inherited;
end;

procedure TIntfRec.SaveToStream(P: TStream);
begin
  P.Write(GUID, SizeOf(GUID));
  IntfMethods.SaveToStream(P);
end;

procedure TIntfRec.LoadFromStream(P: TStream);
begin
  P.Read(GUID, SizeOf(GUID));
  IntfMethods.LoadFromStream(P);
end;

procedure TIntfRec.AllocBuff;
begin
  if Buff <> nil then
    DeallocBuff;
  fBuffSize := MAX_INTERFACE_IMPLEMENT_METHODS * SizeOf(Pointer) * 2;
  Buff := AllocMem(fBuffSize);
end;

procedure TIntfRec.SetupBuff(CodePtr: Pointer);
var
  I, Offset, InterfaceToObjectOffset: Integer;
  Adr: Pointer;
begin
  for I:=0 to IntfMethods.Count - 1 do
  begin
    Offset := IntfMethods[I].MethodOffset;
    InterfaceToObjectOffset := IntfMethods[I].InterfaceToObjectOffset;
    if CodePtr <> nil then
    begin
      Adr := ShiftPointer(CodePtr, Offset);
    end
    else
    begin
      Adr := Pointer(Offset);
    end;
    Buff^[I] := Adr;
    Buff^[MAX_INTERFACE_IMPLEMENT_METHODS + I] := Pointer(InterfaceToObjectOffset);
  end;
end;

procedure TIntfRec.DeallocBuff;
begin
  if Buff <> nil then
    FreeMem(Buff, fBuffSize);
end;

// -- TIntfList ----------------------------------------------------------------

function TIntfList.Lookup(const GUID: TGUID): TIntfRec;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if GuidsAreEqual(Records[I].GUID, GUID) then
    begin
      result := Records[I];
      Exit;
    end;
end;

function TIntfList.IndexOf(const GUID: TGUID): Integer;
var
  I: Integer;
begin
  result := -1;
  for I:=0 to Count - 1 do
    if GuidsAreEqual(Records[I].GUID, GUID) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TIntfList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I:=0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TIntfList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  IntfRec: TIntfRec;
begin
  S.Read(K, SizeOf(Integer));
  for I:=0 to K - 1 do
  begin
    IntfRec := TIntfRec.Create;
    IntfRec.LoadFromStream(S);
    L.Add(IntfRec);
  end;
end;

procedure TIntfList.Setup(CodePtr: Pointer);
var
  I: Integer;
  R: TIntfRec;
begin
  for I:=0 to Count - 1 do
  begin
    R := Records[I];

    R.AllocBuff;
    R.SetupBuff(CodePtr);
  end;
end;

function TIntfList.GetRecord(I: Integer): TIntfRec;
begin
  result := TIntfRec(L[I]);
end;

function TIntfList.Add: TIntfRec;
begin
  result := TIntfRec.Create;
  L.Add(result);
end;

// -- TPropRec -----------------------------------------------------------------

constructor TPropRec.Create;
begin
  inherited;
end;

destructor TPropRec.Destroy;
begin
  inherited;
end;

// -- TPropList ----------------------------------------------------------------


procedure TPropList.Add(P: Pointer; S: Integer);
var
  R: TPropRec;
begin
  R := TPropRec.Create;
  R.PropInfo := P;
  R.PropOffset := S;
  L.Add(R);
end;

function TPropList.GetInfo(I: Integer): PPropInfo;
begin
  result := TPropRec(L[I]).PropInfo;
end;

function TPropList.Top: TPropRec;
begin
  if Count = 0 then
    result := nil
  else
    result := TPropRec(L[Count - 1]);
end;

// -- TClassRec ----------------------------------------------------------------

constructor TClassRec.Create(i_PClass: TClass; i_Offset: Integer; i_Host: Boolean);
begin
  inherited Create;
  PClass := i_PClass;
  Offset := i_Offset;
  PropInfos := TPropList.Create;
  Host := i_Host;
  if PClass <> nil then
  begin
    PClass_pti := PClass.ClassInfo;
    AddPropInfos;
  end;
  IntfList := TIntfList.Create;
end;

destructor TClassRec.Destroy;
begin
  FreeAndNil(PropInfos);
  FreeAndNil(IntfList);
  inherited;
end;

procedure TClassRec.SetupInterfaces(CodePtr: Pointer);
begin
  IntfList.Setup(CodePtr);
end;

procedure TClassRec.AddPropInfos;
var
  pti: PTypeInfo;
  ptd: PTypeData;
  Loop, nProps: Integer;
  pProps: PPropList;
  ppi: PPropInfo;
  PropOffset: Integer;
begin
  PropInfos.Clear;

  pti := PClass.ClassInfo;
  if pti = nil then Exit;
  ptd := GetTypeData(pti);
  nProps := ptd^.PropCount;

  if nProps > 0 then
  begin
    GetMem(pProps, SizeOf(PPropInfo) * nProps);
    try
      GetPropInfos(pti, pProps);
      for Loop:=0 to nProps - 1 do
      begin
    {$ifdef fpc}
        ppi := pProps^[Loop];
    {$else}
        ppi := pProps[Loop];
    {$endif}
        PropOffset := Offset + (Loop + 1) * SizeOf(Pointer);
        PropInfos.Add(ppi, PropOffset);
      end;
    finally
      FreeMem(pProps, SizeOf(PPropInfo) * nProps);
    end;
  end;

  if Assigned(AddPropInfosDRTTI) then
    AddPropInfosDRTTI(PClass, PropInfos);
end;

function TClassRec.GetIntfOffset(const GUID: TGUID): Integer;
var
  I: Integer;
begin
  result := 0;
  I := IntfList.IndexOf(GUID);
  if I = -1 then
    Exit;
  result := InstSize - SizeOf(Pointer) - IntfList.Count * SizeOf(Pointer)
                + I * SizeOf(Pointer);
end;

function TClassRec.GetIntfTableSize: Integer;
begin
  result := SizeOf(Integer) + // EntryCount
            IntfList.Count * SizeOf(TInterfaceEntry);
end;

// -- TClassList ---------------------------------------------------------------

constructor TClassList.Create;
begin
  inherited;
  L := TStringList.Create;
end;

function TClassList.GetCount: Integer;
begin
  result := L.Count;
end;

function TClassList.GetClassRec(I: Integer): TClassRec;
begin
  result := TClassRec(L.Objects[I]);
end;

function TClassList.GetName(I: Integer): String;
begin
  result := L[I];
end;

function TClassList.IndexOf(const S: String): Integer;
begin
  result := L.IndexOf(S);
end;

function TClassList.Add(const FullName: String; Host: Boolean): TClassRec;
begin
  result := TClassRec.Create(nil, 0, false);
  result.FullName := FullName;
  result.Host := Host;
  L.AddObject(ExtractName(FullName), result);
end;

function TClassList.AddEx(const FullName: String; ClassIndex: Integer): TClassRec;
begin
  while L.Count < ClassIndex + 1 do
    L.Add('');

  if Assigned(L.Objects[ClassIndex]) then
{$IFDEF ARC}
    L.Objects[ClassIndex] := nil;
{$ELSE}
    L.Objects[ClassIndex].Free;
{$ENDIF}

  result := TClassRec.Create(nil, 0, false);
  result.FullName := FullName;
  L.Objects[ClassIndex] := result;
end;

function TClassList.AddClass(C: TClass; const FullName: String;
                             Host: Boolean; Offset: Integer): TClassRec;
var
  I: Integer;
  S: String;
begin
  S := C.ClassName;

  I := L.IndexOf(S);
  if I = -1 then
  begin
    result := TClassRec.Create(C, Offset, Host);
    L.AddObject(S, result);
  end
  else
  begin
    result := TClassRec(L.Objects[I]);

    if Assigned(result) then
    begin
      if result.PClass = nil then
      begin
        FreeAndNil(result);
        result := TClassRec.Create(C, Offset, Host);
      end
      else
        result.AddPropInfos;
    end
    else
      result := TClassRec.Create(C, Offset, Host);

    L.Objects[I] := result;
  end;
  result.FullName := FullName;
end;

function TClassList.AddClassEx(C: TClass;
                               const FullName: String;
                               Host: Boolean;
                               Offset: Integer;
                               ClassIndex: Integer): TClassRec;
begin
  while L.Count < ClassIndex + 1 do
    L.AddObject('', nil);

  result := TClassRec(L.Objects[ClassIndex]);

  if Assigned(result) then
  begin
    if result.PClass = nil then
    begin
      FreeAndNil(result);
      result := TClassRec.Create(C, Offset, Host);
    end
    else
      result.AddPropInfos;
  end
  else
    result := TClassRec.Create(C, Offset, Host);

  L.Objects[ClassIndex] := result;
  result.FullName := FullName;
end;

procedure TClassList.SetupInterfaces(CodePtr: Pointer);
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    Records[I].SetupInterfaces(CodePtr);
end;

procedure TClassList.Clear;
var
  I: Integer;
begin
  for I:=0 to L.Count - 1 do
  begin
    if L.Objects[I] <> nil then
{$IFDEF ARC}
       L.Objects[I] := nil;
{$ELSE}
       L.Objects[I].Free;
{$ENDIF}
  end;
  L.Clear;
end;

destructor TClassList.Destroy;
begin
  Clear;
  FreeAndNil(L);
  inherited;
end;

function TClassList.FindClass(C: TClass): Integer;
var
  I: Integer;
begin
  result := -1;
  for I:=0 to L.Count - 1 do
    if Records[I].PClass = C then
    begin
      result := I;
      Exit;
    end;
end;

function TClassList.Lookup(const FullName: String): TClassRec;
var
  I: Integer;
  ClassRec: TClassRec;
begin
  result := nil;
  for I:=0 to Count - 1 do
  begin
    ClassRec := Records[I];
    if StrEql(ClassRec.FullName, FullName) then
    begin
      result := ClassRec;
      Exit;
    end;
  end;
end;

function TClassList.LookupClassRec(C: TClass): TClassRec;
var
  I: Integer;
  ClassRec: TClassRec;
begin
  result := nil;
  for I:=0 to Count - 1 do
  begin
    ClassRec := Records[I];
    if StrEql(ClassRec.PClass.ClassName, C.ClassName) then
    begin
      result := ClassRec;
      Exit;
    end;
  end;
end;

function TClassList.GetSize: Integer;
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveToStream(S);
    result := S.Size;
  finally
    FreeAndNil(S);
  end;
end;

type
  TSaveClassRec = packed record
    Offset: Integer;
    SizeOfScriptClassFields: Integer;
    DestructorProgOffset: Integer;
    AfterConstructionProgOffset: Integer;
    BeforeDestructionProgOffset: Integer;
    SafeCallExceptionProgOffset: Integer;
    DispatchProgOffset: Integer;
    DefaultHandlerProgOffset: Integer;
    NewInstanceProgOffset: Integer;
    FreeInstanceProgOffset: Integer;
    InstSize: Integer;

{$IFDEF UNIC}
    ToStringProgOffset: Integer;
    GetHashCodeProgOffset: Integer;
    EqualsProgOffset: Integer;
{$ENDIF}

    Host: Boolean;
  end;

procedure PackRec(var S: TSaveClassRec; const R: TClassRec);
begin
  S.Offset := R.Offset;
  S.InstSize := R.InstSize;
  S.DestructorProgOffset := R.DestructorProgOffset;
  S.AfterConstructionProgOffset := R.AfterConstructionProgOffset;
  S.BeforeDestructionProgOffset := R.BeforeDestructionProgOffset;
  S.SafeCallExceptionProgOffset := R.SafeCallExceptionProgOffset;
  S.DispatchProgOffset := R.DispatchProgOffset;
  S.DefaultHandlerProgOffset := R.DefaultHandlerProgOffset;
  S.NewInstanceProgOffset := R.NewInstanceProgOffset;
  S.FreeInstanceProgOffset := R.FreeInstanceProgOffset;
  S.SizeOfScriptClassFields := R.SizeOfScriptClassFields;
  S.Host := R.Host;

{$IFDEF UNIC}
  S.ToStringProgOffset := R.ToStringProgOffset;
  S.GetHashCodeProgOffset := R.GetHashCodeProgOffset;
  S.EqualsProgOffset := R.EqualsProgOffset;
{$ENDIF}
end;

procedure UnPackRec(S: TClassRec; const R: TSaveClassRec);
begin
  S.Offset := R.Offset;
  S.InstSize := R.InstSize;
  S.DestructorProgOffset := R.DestructorProgOffset;
  S.AfterConstructionProgOffset := R.AfterConstructionProgOffset;
  S.BeforeDestructionProgOffset := R.BeforeDestructionProgOffset;
  S.SafeCallExceptionProgOffset := R.SafeCallExceptionProgOffset;
  S.DispatchProgOffset := R.DispatchProgOffset;
  S.DefaultHandlerProgOffset := R.DefaultHandlerProgOffset;
  S.NewInstanceProgOffset := R.NewInstanceProgOffset;
  S.FreeInstanceProgOffset := R.FreeInstanceProgOffset;
  S.SizeOfScriptClassFields := R.SizeOfScriptClassFields;
  S.Host := R.Host;

{$IFDEF UNIC}
  S.ToStringProgOffset := R.ToStringProgOffset;
  S.GetHashCodeProgOffset := R.GetHashCodeProgOffset;
  S.EqualsProgOffset := R.EqualsProgOffset;
{$ENDIF}
end;

procedure TClassList.SaveToStream(S: TStream);
var
  I: Integer;
  SR: TSaveClassRec;
begin
  SaveStringListToStream(L, S);
  for I:=0 to L.Count - 1 do
  begin
    PackRec(SR, Records[I]);

    with Records[I] do
    begin
      SaveStringToStream(FullName, S);
      SaveStringToStream(ParentFullName, S);
      S.Write(SR, SizeOf(SR));
      IntfList.SaveToStream(S);
      if not Host then
      begin
        SaveIntDynarrayToStream(BytecodeMethodEntryList, S);
        S.Write(VirtualMethodEntryList, SizeOf(VirtualMethodEntryList));
      end;
    end;
  end;
end;

procedure TClassList.LoadFromStream(S: TStream; Version: Integer);
var
  I: Integer;
  RI: TClassRec;
  SR: TSaveClassRec;
begin
  Clear;

  LoadStringListFromStream(L, S);
  for I:=0 to L.Count - 1 do
  begin
    L.Objects[I] := TClassRec.Create(nil, 0, false);
    RI := Records[I];
    with RI do
    begin
      FullName := LoadStringFromStream(S);
      ParentFullName := LoadStringFromStream(S);
      S.Read(SR, SizeOf(SR));
      UnPackRec(RI, SR);
      IntfList.LoadFromStream(S);
      if not Host then
      begin
        BytecodeMethodEntryList := LoadIntDynarrayFromStream(S);
        S.Read(VirtualMethodEntryList, SizeOf(VirtualMethodEntryList));
      end;
      PClass := nil;
    end;
  end;
end;

function TClassList.GetByteCodeMethodEntryIndex(N: Integer): Integer;
var
  I, J, V, L: Integer;
  R: TClassRec;
begin
  result := -1;
  for I := Count - 1 downto 0 do
  begin
    R := Records[I];
    L := System.Length(R.ByteCodeMethodEntryList);
    for J := 0 to L - 1 do
    begin
      V := R.ByteCodeMethodEntryList[J];
      if V = 0 then
        break;

      if V = N then
      begin
        result := J;
        Exit;
      end;
    end;
  end;
end;

end.
