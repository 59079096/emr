////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_CLASSFACT.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_CLASSFACT;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_MAP,
  PAXCOMP_CLASSLST;
type
  TPaxClassFactoryRec = class
  private
    PaxClassRec: TPaxClassRec;
    fClassName: ShortString;
    fParentClass: TClass;
    Processed: Boolean;
    fMethodTableSize: Integer;
    fIntfTableSize: Integer;
    function GetDelphiClass: TClass;
    function GetVMTPtr: PVMT;
    procedure SetMethodTableSize(value: Integer);
  public
    pti_parent: PTypeInfo;
    FieldTableSize: Integer;
    FieldClassTable: PFieldClassTable;
    DmtTableSize: Integer;
    FullClassName: String;

    constructor Create(const AFullClassName: String);
    destructor Destroy; override;
    procedure RenameClass(const NewFullClassName: String);
    procedure SetInstanceSize(value: Integer);
    procedure SetParentClass(AClass: TClass);
    property DelphiClass: TClass read GetDelphiClass;
    property VMTPtr: PVMT read GetVMTPtr;
    property MethodTableSize: Integer read fMethodTableSize
                                      write SetMethodTableSize;
    property IntfTableSize: Integer read fIntfTableSize write
                                         fIntfTableSize;
  end;

  TPaxClassFactory = class(TTypedList)
  private
    function GetRecord(I: Integer): TPaxClassFactoryRec;
    procedure Reset;
  public
    ForceCreate: Boolean;
    constructor Create;
    function CreatePaxClass(const AFullClassName: String;
                            AnInstanceSize: Integer;
                            ParentClass: TClass;
                            PDestroyObject: Pointer): TClass;
    function CreateCloneClass(const AFullClassName: String;
                              ParentClass: TClass): TClass;
    function RenameClass(const OldFullClassName,
                         NewFullClassName: String): Boolean;
    function FindRecord(AClass: TClass): TPaxClassFactoryRec;
    function FindRecordByFullName(const AFullClassName: String): TPaxClassFactoryRec;
    function LookupFullName(const AFullClassName: String): TClass;
    procedure SetupParents(Prog: Pointer; ClassList: TClassList);
    procedure SetupStdVirtuals(ClassList: TClassList; CodePtr: Pointer);
    procedure AddInheritedMethods; overload;
    procedure AddInheritedMethods(SourceClass: TClass); overload;
    procedure AddOverridenMethods(AProg: Pointer; ScriptMapTable: TMapTable);
    procedure AddVirtualMethod(SourceClass: TClass;
                              SourceMethodAddress: Pointer);
    function AddOverridenMethod(SourceClass: TClass;
                                SourceMethodAddress,
                                InheritedMethodAddress: Pointer): Boolean;
    procedure RaiseError(const Message: string; params: array of Const);
    property Records[I: Integer]: TPaxClassFactoryRec read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_BASERUNNER;

function _NewInstance(Self: TClass): TObject;
var
  S: Integer;
begin
  S := Self.InstanceSize;
  result := Self.InitInstance(AllocMem(S));
end;

{$IFDEF ARC}
const
  objDisposedFlag = Integer($40000000);
type
  TMyObject = class(TObject);

function fake_ObjRelease(Self: Pointer): Integer;
var
  PRefCount: ^Integer;
begin
  PRefCount := ShiftPointer(Pointer(Self), SizeOf(Pointer));
  Result := AtomicDecrement(PRefCount^) and not objDisposedFlag;
  if Result = 0 then
    TMyObject(Self).Destroy;
end;
{$ENDIF}

// TPaxClassFactoryRec ---------------------------------------------------------

constructor TPaxClassFactoryRec.Create(const AFullClassName: String);
begin
  inherited Create;
  FillChar(PaxClassRec, SizeOf(PaxClassRec), 0);
  FieldClassTable := nil;
  FullClassName := AFullClassName;
  PaxClassRec.PaxInfo.ClassFactoryRec := Self;
end;

destructor TPaxClassFactoryRec.Destroy;
begin
  if MethodTableSize > 0 then
    FreeMem(vmtMethodTableSlot(VMTPtr)^, MethodTableSize);
  if FieldTableSize > 0 then
    FreeMem(vmtFieldTableSlot(VMTPtr)^, FieldTableSize);
  if DmtTableSize > 0 then
    FreeMem(vmtDynamicTableSlot(VMTPtr)^, DmtTableSize);
  if FieldClassTable <> nil then
    DestroyFieldClassTable(FieldClassTable);
  if IntfTableSize > 0 then
    FreeMem(vmtIntfTableSlot(VMTPtr)^, IntfTableSize);
  inherited;
end;

{$IFDEF FPC}
function TPaxClassFactoryRec.GetDelphiClass: TClass;
begin
  result := TClass(@PaxClassRec.VMT);
end;
{$ELSE}
function TPaxClassFactoryRec.GetDelphiClass: TClass;
begin
  result := TClass(vmtSelfPtrSlot(@PaxClassRec.VMT)^);
end;
{$ENDIF}


function TPaxClassFactoryRec.GetVMTPtr: PVMT;
begin
  result := @ PaxClassRec.VMT;
end;

procedure TPaxClassFactoryRec.SetMethodTableSize(value: Integer);
begin
  fMethodTableSize := value;
end;

procedure TPaxClassFactoryRec.SetInstanceSize(value: Integer);
begin
  PIntPax(vmtInstanceSizeSlot(@PaxClassRec.VMT))^ := value;
end;

procedure TPaxClassFactoryRec.RenameClass(const NewFullClassName: string);
begin
  PShortStringFromString(@fClassName, ExtractName(NewFullClassName));
  FullClassName := NewFullClassName;
end;

procedure TPaxClassFactoryRec.SetParentClass(AClass: TClass);
var
  ParentVMT: PVMT;
begin
  fParentClass := AClass;

  ParentVMT := GetVmtFromClass(AClass);

//  PaxClassRec.VMT.IntfTable := ParentVMT.IntfTable;

{$IFDEF FPC}
  vmtParentSlot(@PaxClassRec.VMT)^ := AClass;
{$IFNDEF LINUX}
  PaxClassRec.VMT.VToString := ParentVMT^.VToString;
  PaxClassRec.VMT.VGetHashCode := ParentVMT^.VGetHashCode;
  PaxClassRec.VMT.VEquals := ParentVMT^.VEquals;
{$ENDIF}
{$ELSE}
  vmtParentSlot(@PaxClassRec.VMT)^ := @fParentClass;
{$ENDIF}

  vmtAutoTableSlot(@PaxClassRec.VMT)^ := vmtAutoTableSlot(ParentVMT)^;
{$IFNDEF LINUX}
  vmtDispatchSlot(@PaxClassRec.VMT)^ := vmtDispatchSlot(ParentVMT)^;
{$ENDIF}
  vmtInitTableSlot(@PaxClassRec.VMT)^ := vmtInitTableSlot(ParentVMT)^;
  vmtTypeInfoSlot(@PaxClassRec.VMT)^ := vmtTypeInfoSlot(ParentVMT)^;
  vmtFieldTableSlot(@PaxClassRec.VMT)^ := vmtFieldTableSlot(ParentVMT)^;
  vmtMethodTableSlot(@PaxClassRec.VMT)^ := vmtMethodTableSlot(ParentVMT)^;
  vmtDynamicTableSlot(@PaxClassRec.VMT)^ := vmtDynamicTableSlot(ParentVMT)^;

  vmtNewInstanceSlot(@PaxClassRec.VMT)^ := vmtNewInstanceSlot(ParentVMT)^;
  vmtSafeCallExceptionSlot(@PaxClassRec.VMT)^ := vmtSafeCallExceptionSlot(ParentVMT)^;
  vmtAfterConstructionSlot(@PaxClassRec.VMT)^ := vmtAfterConstructionSlot(ParentVMT)^;
  vmtBeforeDestructionSlot(@PaxClassRec.VMT)^ := vmtBeforeDestructionSlot(ParentVMT)^;
  vmtDefaultHandlerSlot(@PaxClassRec.VMT)^ := vmtDefaultHandlerSlot(ParentVMT)^;

  {$IFDEF UNIC}
  vmtToStringSlot(@PaxClassRec.VMT)^ := vmtToStringSlot(ParentVMT)^;
  vmtGetHashCodeSlot(@PaxClassRec.VMT)^ := vmtGetHashCodeSlot(ParentVMT)^;
  vmtEqualsSlot(@PaxClassRec.VMT)^ := vmtEqualsSlot(ParentVMT)^;
  {$ENDIF}

  {$IFDEF ARC}
  vmt__ObjAddRefSlot(@PaxClassRec.VMT)^ := vmt__ObjAddRefSlot(ParentVMT)^;
//  vmt__ObjReleaseSlot(@PaxClassRec.VMT)^ := vmt__ObjReleaseSlot(ParentVMT)^;
  vmt__ObjReleaseSlot(@PaxClassRec.VMT)^ := @ fake_ObjRelease;
  {$ENDIF}
end;

// TPaxClassFactory ------------------------------------------------------------

constructor TPaxClassFactory.Create;
begin
  inherited;
  ForceCreate := false;
end;

procedure TPaxClassFactory.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

procedure _AfterConstruction(Self: TObject);
begin
end;

procedure _BeforeDestruction(Self: TObject);
begin
end;

function TPaxClassFactory.RenameClass(const OldFullClassName,
                                            NewFullClassName: String): Boolean;
var
  R: TPaxClassFactoryRec;
begin
  R := FindRecordByFullName(OldFullClassName);
  result := R <> nil;
  if result then
    R.RenameClass(NewFullClassName);
end;

function TPaxClassFactory.CreatePaxClass(const AFullClassName: String;
                                         AnInstanceSize: Integer;
                                         ParentClass: TClass;
                                         PDestroyObject: Pointer): TClass;
var
  PaxClassObject: TPaxClassFactoryRec;
begin
  PaxClassObject := TPaxClassFactoryRec.Create(AFullClassName);
  PShortStringFromString(@PaxClassObject.fClassName, ExtractName(AFullClassName));
  PaxClassObject.fParentClass := ParentClass;

  {$IFDEF FPC}
  vmtParentSlot(@PaxClassObject.PaxClassRec.VMT)^ := ParentClass;
  {$ELSE}
  vmtSelfPtrSlot(@PaxClassObject.PaxClassRec.VMT)^ := GetClassFromVMT(@PaxClassObject.PaxClassRec.VMT);
  vmtParentSlot(@PaxClassObject.PaxClassRec.VMT)^ := @ PaxClassObject.fParentClass;
  {$ENDIF}
  vmtClassNameSlot(@PaxClassObject.PaxClassRec.VMT)^ := @ PaxClassObject.fClassName;
  PIntPax(vmtInstanceSizeSlot(@PaxClassObject.PaxClassRec.VMT))^ := AnInstanceSize;
  vmtNewInstanceSlot(@PaxClassObject.PaxClassRec.VMT)^ := @ _NewInstance;
  vmtAfterConstructionSlot(@PaxClassObject.PaxClassRec.VMT)^ := @ _AfterConstruction;
  vmtBeforeDestructionSlot(@PaxClassObject.PaxClassRec.VMT)^ := @ _BeforeDestruction;
  vmtDestroySlot(@PaxClassObject.PaxClassRec.VMT)^ := PDestroyObject;

  L.Add(PaxClassObject);

  result := PaxClassObject.DelphiClass;

  PaxClassObject.PaxClassRec.PaxInfo.PaxSignature := strPaxSignature;
end;

function TPaxClassFactory.CreateCloneClass(const AFullClassName: String;
                                           ParentClass: TClass): TClass;
var
  PaxClassObject: TPaxClassFactoryRec;
  ParentVMT: PVMT;
begin
  ParentVMT := GetVmtFromClass(ParentClass);

  PaxClassObject := TPaxClassFactoryRec.Create(AFullClassName);
  PShortStringFromString(@PaxClassObject.fClassName, ExtractName(AFullClassName));
  PaxClassObject.fParentClass := ParentClass;

  {$IFDEF FPC}
  raise Exception.Create(errNotImplementedYet);
  {$ELSE}
  vmtSelfPtrSlot(@PaxClassObject.PaxClassRec.VMT)^ := GetClassFromVMT(@PaxClassObject.PaxClassRec.VMT);
  vmtAutoTableSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtAutoTableSlot(ParentVMT)^;
  {$ENDIF}
  vmtIntfTableSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtIntfTableSlot(ParentVMT)^;

  vmtParentSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtParentSlot(ParentVMT)^;
  vmtClassNameSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtClassNameSlot(ParentVMT)^;
  vmtInstanceSizeSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtInstanceSizeSlot(ParentVMT)^;
  vmtNewInstanceSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtNewInstanceSlot(ParentVMT)^;
  vmtAfterConstructionSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtAfterConstructionSlot(ParentVMT)^;
  vmtBeforeDestructionSlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtBeforeDestructionSlot(ParentVMT)^;
  vmtDestroySlot(@PaxClassObject.PaxClassRec.VMT)^ := vmtDestroySlot(ParentVMT)^;

  L.Add(PaxClassObject);
  result := PaxClassObject.DelphiClass;

  PaxClassObject.PaxClassRec.PaxInfo.PaxSignature := strPaxSignature;
end;

function TPaxClassFactory.GetRecord(I: Integer): TPaxClassFactoryRec;
begin
  result := TPaxClassFactoryRec(L[I]);
end;

function TPaxClassFactory.FindRecord(AClass: TClass): TPaxClassFactoryRec;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if Records[I].DelphiClass = AClass then
    begin
      result := Records[I];
      break;
    end;
end;

function TPaxClassFactory.FindRecordByFullName(const AFullClassName: String): TPaxClassFactoryRec;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if StrEql(Records[I].FullClassName, AFullClassName) then
    begin
      result := Records[I];
      break;
    end;
end;

function TPaxClassFactory.LookupFullName(const AFullClassName: String): TClass;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if StrEql(Records[I].FullClassName, AFullClassName) then
    begin
      result := Records[I].DelphiClass;
      break;
    end;
end;

procedure TPaxClassFactory.Reset;
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    Records[I].Processed := false;
end;

procedure TPaxClassFactory.SetupStdVirtuals(ClassList: TClassList;
                                            CodePtr: Pointer);
var
  I: Integer;
  ClassRec: TClassRec;
  FactoryRec: TPaxClassFactoryRec;
  P: Pointer;
begin
  for I:=0 to ClassList.Count - 1 do
  begin
    ClassRec := ClassList[I];
    if not ClassRec.Host then
    begin
      FactoryRec := FindRecord(ClassRec.PClass);

      if ClassRec.SafeCallExceptionProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.SafeCallExceptionProgOffset);
        vmtSafeCallExceptionSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.AfterConstructionProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.AfterConstructionProgOffset);
        vmtAfterConstructionSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.BeforeDestructionProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.BeforeDestructionProgOffset);
        vmtBeforeDestructionSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.DispatchProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.DispatchProgOffset);
        vmtDispatchSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.DefaultHandlerProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.DefaultHandlerProgOffset);
        vmtDefaultHandlerSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.NewInstanceProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.NewInstanceProgOffset);
        vmtNewInstanceSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.FreeInstanceProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.FreeInstanceProgOffset);
        vmtFreeInstanceSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.DestructorProgOffset > 0 then
      begin
//        P := ShiftPointer(CodePtr, ClassRec.DestructorProgOffset);
//        FactoryRec.GetVMTPtr^.Destroy := P;
      end;
      {$IFDEF UNIC}
      if ClassRec.ToStringProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.ToStringProgOffset);
        vmtToStringSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.GetHashCodeProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.GetHashCodeProgOffset);
        vmtGetHashCodeSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      if ClassRec.EqualsProgOffset > 0 then
      begin
        P := ShiftPointer(CodePtr, ClassRec.EqualsProgOffset);
        vmtEqualsSlot(FactoryRec.GetVMTPtr)^ := P;
      end;
      {$ENDIF}
    end;
  end;
end;

procedure TPaxClassFactory.SetupParents(Prog: Pointer; ClassList: TClassList);
var
  I, J: Integer;
  ClassFactoryRec, ParentFactoryRec: TPaxClassFactoryRec;
  ClassRec, ParentClassRec: TClassRec;
  C: TClass;
  b: Boolean;
  S: String;
begin
  Reset;

  repeat
    b := false;

    for I:=0 to Count - 1 do
    begin
      ClassFactoryRec := Records[I];
      if ClassFactoryRec.Processed then
        continue;

      C := ClassFactoryRec.DelphiClass;
      J := ClassList.FindClass(C);
      if J = -1 then
        raise Exception.Create(errInternalError);

      ClassRec := ClassList[J];
      ParentClassRec := ClassList.Lookup(ClassRec.ParentFullName);

      if ParentClassRec = nil then
      begin
        RaiseError(errInternalError, []);
      end;

      if ParentClassRec.Host then // parent is host
      begin
        if ParentClassRec.PClass = nil then
        begin
          S := ExtractName(ClassRec.ParentFullName);
          ParentClassRec.PClass := Classes.GetClass(S);
          if ParentClassRec.PClass = nil then
          begin
            if Prog <> nil then
              if Assigned(TBaseRunner(Prog).OnMapTableClassRef) then
              begin
                TBaseRunner(Prog).OnMapTableClassRef(TBaseRunner(Prog).Owner,
                       ClassRec.ParentFullName, true, ParentClassRec.PClass);
                if ParentClassRec.PClass = nil then
                  TBaseRunner(Prog).RaiseError(errUnresolvedClassReference,
                    [ClassRec.ParentFullName]);
               end;
            if ParentClassRec.PClass = nil then
              RaiseError(errClassIsNotRegistered, [ClassRec.ParentFullName]);
          end;
        end;

        b := true;
        ClassFactoryRec.SetParentClass(ParentClassRec.PClass);
        ClassFactoryRec.Processed := true;
      end
      else
      begin
        ParentFactoryRec := FindRecord(ParentClassRec.PClass);

        if ParentFactoryRec = nil then
          raise Exception.Create(errInternalError);

        if ParentFactoryRec.Processed then
        begin
          b := true;
          ClassFactoryRec.SetParentClass(ParentClassRec.PClass);
          ClassFactoryRec.Processed := true;
        end;
      end;
    end;

    if b = false then
      break;

  until false;
end;

procedure TPaxClassFactory.AddInheritedMethods;
var
  I: Integer;
  ClassFactoryRec, ParentFactoryRec: TPaxClassFactoryRec;
  C: TClass;
  b: Boolean;
begin
  Reset;

  repeat
    b := false;

    for I:=0 to Count - 1 do
    begin
      ClassFactoryRec := Records[I];
      if ClassFactoryRec.Processed then
        continue;
      C := ClassFactoryRec.DelphiClass;

      ParentFactoryRec := FindRecord(C.ClassParent);
      if ParentFactoryRec = nil then // parent is host
      begin
        b := true;
        AddInheritedMethods(C);
        ClassFactoryRec.Processed := true;
      end
      else if ParentFactoryRec.Processed then
      begin
        b := true;
        AddInheritedMethods(C);
        ClassFactoryRec.Processed := true;
      end;
    end;

    if b = false then
      break;

  until false;
end;

procedure TPaxClassFactory.AddOverridenMethods(AProg: Pointer; ScriptMapTable: TMapTable);
var
  I, J, K: Integer;
  ClassFactoryRec, ParentFactoryRec: TPaxClassFactoryRec;
  C: TClass;
  b: Boolean;
  MapRec, SomeMR: TMapRec;
  P: Pointer;
  PC: PPointerArray;
  Prog: TBaseRunner;
  S, FileName, ProcName: String;
  DestProg: Pointer;
begin
  Reset;

  Prog := TBaseRunner(AProg);

  repeat
    b := false;

    for I:=0 to Count - 1 do
    begin
      ClassFactoryRec := Records[I];
      if ClassFactoryRec.Processed then
        continue;
      C := ClassFactoryRec.DelphiClass;

      ParentFactoryRec := FindRecord(C.ClassParent);

      if (ParentFactoryRec = nil) or ((ParentFactoryRec <> nil) and (ParentFactoryRec.Processed)) then
      begin
        b := true;
        ClassFactoryRec.Processed := true;

        for J:=0 to ScriptMapTable.Count - 1 do
        begin
          MapRec := ScriptMapTable[J];
          if MapRec.SubDesc.MethodIndex > 0 then
          begin
            S := ExtractClassName(MapRec.FullName);
            if not StrEql(S, StringFromPShortString(@ClassFactoryRec.fClassName)) then
              continue;

            if MapRec.Offset = 0 then
            begin
              FileName := ExtractOwner(MapRec.FullName) + '.' + PCU_FILE_EXT;
              ProcName := Copy(MapRec.FullName, PosCh('.', MapRec.FullName) + 1, Length(MapRec.FullName));
              P := Prog.LoadAddressEx(FileName, ProcName, false, 0, SomeMR, DestProg);
            end
            else
              P := ShiftPointer(Prog.CodePtr, MapRec.Offset);

            C := ClassFactoryRec.DelphiClass;
            PC := GetVArray(C);
            PC^[MapRec.SubDesc.MethodIndex - 1] := P;

            for K:=0 to Count - 1 do
              if K <> I then
              if Records[K].DelphiClass.InheritsFrom(C) then
              begin
                PC := GetVArray(Records[K].DelphiClass);
                PC^[MapRec.SubDesc.MethodIndex - 1] := P;
              end;
          end;
        end;
      end;
    end;

    if b = false then
      break;

  until false;
end;

{
procedure TPaxClassFactory.AddOverridenMethods(AProg: Pointer; ScriptMapTable: TMapTable);
var
  I, J, K: Integer;
  ClassFactoryRec, ParentFactoryRec: TPaxClassFactoryRec;
  C: TClass;
  b: Boolean;
  MapRec: TMapRec;
  P: Pointer;
  Prog: TProgram;
  S: AnsiString;
begin
  Reset;

  Prog := TProgram(AProg);

  repeat
    b := false;

    for I:=0 to Count - 1 do
    begin
      ClassFactoryRec := Records[I];
      if ClassFactoryRec.Processed then
        continue;
      C := ClassFactoryRec.DelphiClass;

      ParentFactoryRec := FindRecord(C.ClassParent);
      if ParentFactoryRec = nil then // parent is host
      begin
        b := true;
        ClassFactoryRec.Processed := true;
      end
      else if ParentFactoryRec.Processed then
      begin
        b := true;
        ClassFactoryRec.Processed := true;

        for J:=0 to ScriptMapTable.Count - 1 do
        begin
          MapRec := ScriptMapTable[J];
          if MapRec.MethodIndex > 0 then
          begin
            S := ExtractClassName(MapRec.FullName);
            if not StrEql(S, ClassFactoryRec.fClassName) then
              continue;

            P := ShiftPointer(Prog.CodePtr, MapRec.Offset);
            C := ClassFactoryRec.DelphiClass;
            GetVArray(C)^[MapRec.MethodIndex - 1] := P;

            for K:=0 to Count - 1 do
              if Records[K].DelphiClass.InheritsFrom(C) then
                GetVArray(Records[K].DelphiClass)^[MapRec.MethodIndex - 1] := P;
          end;
        end;
      end;
    end;

    if b = false then
      break;

  until false;
end;
}

procedure TPaxClassFactory.AddInheritedMethods(SourceClass: TClass);
var
  P, Q: PPointerArray;
  I, K: Integer;
begin
  if SourceClass.ClassParent = nil then
    Exit;

  P := GetVArray(SourceClass.ClassParent);

  K := GetVirtualMethodCount(SourceClass.ClassParent);

  Q := GetVArray(SourceClass);

  for I:=0 to K - 1 do
    Q^[I] := P^[I];
end;

procedure TPaxClassFactory.AddVirtualMethod(SourceClass: TClass;
                                            SourceMethodAddress: Pointer);
var
  P: PPointerArray;
  K: Integer;
begin
  P := GetVArray(SourceClass);
  K := GetVirtualMethodCount(SourceClass);
  P^[K] := SourceMethodAddress;
end;

function TPaxClassFactory.AddOverridenMethod(SourceClass: TClass;
                                             SourceMethodAddress,
                                             InheritedMethodAddress: Pointer): Boolean;
var
  P: PPointerArray;
  I: Integer;
begin
  result := false;
  if SourceClass.ClassParent = nil then
    Exit;

  I := VirtualMethodIndex(SourceClass.ClassParent, InheritedMethodAddress);

  if I = -1 then
    Exit;

  P := GetVArray(SourceClass);
  P^[I] := SourceMethodAddress;

  result := true;
end;

end.
