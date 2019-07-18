////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_LOCALSYMBOL_TABLE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_LOCALSYMBOL_TABLE;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_MAP,
  PAXCOMP_STDLIB;
type
  TLocalSymbolTable = class(TBaseSymbolTable)
  private
    SaveClassList: TAssocIntegers;
  protected
    function GetRecord(I: Integer): TSymbolRec; override;
  public
    GlobalST: TBaseSymbolTable;
    GlobalST_LastShiftValue: Integer;
    constructor Create(AGlobalST: TBaseSymbolTable; NeedHash: Boolean = True);
    destructor Destroy; override;
    procedure Reset; override;
    procedure Update; virtual;
    procedure CompressHostClassList(MapTable: TMapTable;
                                    ClassListIds: TIntegerList = nil);
    procedure RestoreClassIndexes;
    procedure SetImportTable(ImportTable: TBaseSymbolTable);
    property Records[I: Integer]: TSymbolRec read GetRecord; default;
  end;

  TProgSymbolTable = class(TLocalSymbolTable)
  public
    constructor Create(AGlobalST: TBaseSymbolTable);
  end;

implementation

//-- TLocalSymbolTable ---------------------------------------------------------

constructor TLocalSymbolTable.Create(AGlobalST: TBaseSymbolTable; NeedHash: Boolean = true);
begin
  inherited Create(NeedHash);
  st_tag := 1;
  GlobalST := AGlobalST;
  SaveClassList := TAssocIntegers.Create;

  SR0 := GlobalST[0];
end;

destructor TLocalSymbolTable.Destroy;
begin
  FreeAndNil(SaveClassList);
  inherited;
end;

procedure TLocalSymbolTable.RestoreClassIndexes;
var
  I, Id, ClassIndex: Integer;
begin
  for I := 0 to SaveClassList.Count - 1 do
  begin
    Id := SaveClassList.Keys[I];
    ClassIndex := SaveClassList.Values[I];
    Records[Id].ClassIndex := ClassIndex;
  end;
end;

procedure TLocalSymbolTable.Reset;
var
  I: Integer;
begin
  SaveClassList.Clear;
  for I:=A.Count - 1 downto 0 do
{$IFDEF ARC}
    A[I] := nil;
{$ELSE}
    TSymbolRec(A[I]).Free;
{$ENDIF}
  A.Clear;

  Card := FirstLocalId;
  SetImportTable(GlobalST);
end;

{$O+}
function TLocalSymbolTable.GetRecord(I: Integer): TSymbolRec;
begin
  if I <= GlobalST.Card then
    result := TSymbolRec(GlobalST.A[I])
  else if I <= FirstLocalId then
    result := SR0
  else
    result := TSymbolRec(A[I - FirstLocalId - 1]);
end;
{$O-}

procedure TLocalSymbolTable.Update;
var
  I, D: Integer;
  R: TSymbolRec;
  S: String;
begin
  D := GlobalST.LastShiftValue - GlobalST_LastShiftValue;
  if D > 0 then
  begin
    for I := FirstLocalId + 1 to Card do
    begin
      R := Records[I];
      if R.Shift >= GlobalST_LastShiftValue then
        if R.Kind in (KindSubs + [KindVAR]) then
          R.Shift := R.Shift + D;
    end;
    GlobalST_LastShiftValue := GlobalST.LastShiftValue;
  end;

  FreeAndNil(HashArray);
  HashArray := GlobalST.HashArray.Clone;

  for I := FirstLocalId + 1 to Card do
  begin
    R := Records[I];
    S := R.Name;
    R.Name := S;
  end;

  LastClassIndex := -1;
  for I := 1 to Card do
  begin
    R := Records[I];
    if R = SR0 then
      continue;

    if R.Kind = kindTYPE then
      if R.TypeId = typeCLASS then
      begin
        Inc(LastClassIndex);
        R.ClassIndex := LastClassIndex;

        if not R.Host then
          continue;

        if I <= Card then
        if R.AncestorId = H_TObject then
          SetAncestorEx(I);
      end;
  end;
end;

procedure TLocalSymbolTable.CompressHostClassList(MapTable: TMapTable;
                                                  ClassListIds: TIntegerList = nil);
var
  I, KK, K1, K2: Integer;
  R: TSymbolRec;
  S: String;
  MapRec: TMapRec;
  L: TStringList;
  UpName: String;
begin
  LastClassIndex := -1;

  L := TStringList.Create;
  try

    for KK := 1 to 2 do
    begin
      if KK = 1 then
      begin
        K1 := 1;
        K2 := GlobalST.Card;
      end
      else
      begin
        K1 := FirstLocalId + 1;
        K2 := Card;
      end;

      for I := K1 to K2 do
      begin
        R := Records[I];

        if R.Kind = kindTYPE then
          if R.TypeId = typeCLASS then
          if R.ClassIndex <> -1 then
          begin
            SaveClassList.Add(I, R.ClassIndex);

            if R.Host then
            begin
              if I <= StdCard then
              begin
                Inc(LastClassIndex);
                if ClassListIds <> nil then
                  ClassListIds.Add(I);
                R.ClassIndex := LastClassIndex;
                continue;
              end;

              UpName := UpperCase(R.Name);
              if L.IndexOf(UpName) >= 0 then
                continue;

              S := R.FullName;

              MapRec := MapTable.Lookup(S);
              if MapRec <> nil then
              begin
                L.Add(UpName);

                Inc(LastClassIndex);
                if ClassListIds <> nil then
                  ClassListIds.Add(I);

                R.ClassIndex := LastClassIndex;
                MapRec.ClassIndex := LastClassIndex;
                continue;
              end;

              MapRec := MapTable.Lookup(ExtractName(S));
              if MapRec <> nil then
              begin
                L.Add(UpName);

                Inc(LastClassIndex);
                if ClassListIds <> nil then
                  ClassListIds.Add(I);

                R.ClassIndex := LastClassIndex;
                MapRec.ClassIndex := LastClassIndex;
              end
              else
                R.ClassIndex := -1;
            end
            else
            begin
              Inc(LastClassIndex);
              R.ClassIndex := LastClassIndex;
            end;
          end;
        end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

procedure TLocalSymbolTable.SetImportTable(ImportTable: TBaseSymbolTable);
begin
  GlobalST := ImportTable;

  SR0 := GlobalST[0];

  ResultId := GlobalST.ResultId;
  TrueId := GlobalST.TrueId;
  FalseId := GlobalST.FalseId;
  NilId := GlobalST.NilId;
  EventNilId := GlobalST.EventNilId;
  CurrExceptionObjectId := GlobalST.CurrExceptionObjectId;
  EmptySetId := GlobalST.EmptySetId;
  EmptyStringId := GlobalST.EmptyStringId;
  LastShiftValue := GlobalST.LastShiftValue;
  LastClassIndex := GlobalST.LastClassIndex;

  GlobalST_LastShiftValue := GlobalST.LastShiftValue;

  if HashArray <> nil then
  begin
    FreeAndNil(HashArray);
    HashArray := GlobalST.HashArray.Clone;
  end;

  if TypeHelpers <> nil then
  begin
    FreeAndNil(TypeHelpers);
    TypeHelpers := GlobalST.TypeHelpers.Clone;
  end;

  FreeAndNil(GuidList);
  GuidList := GlobalST.GuidList.Clone;

  FreeAndNil(SomeTypeList);
  SomeTypeList := GlobalST.SomeTypeList.Clone;

//  ExternList.Free;
//  ExternList := GlobalST.ExternList.Clone;

// CompileCard := Card;
end;

//-- TProgSymbolTable ----------------------------------------------------------

constructor TProgSymbolTable.Create(AGlobalST: TBaseSymbolTable);
begin
  inherited Create(AGlobalST, false);
end;

end.
