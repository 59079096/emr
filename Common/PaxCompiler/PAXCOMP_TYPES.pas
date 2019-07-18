////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_TYPES.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}

unit PAXCOMP_TYPES;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS;
type
  TPauseException = class(EAbort)
  public
    constructor Create;
  end;

  THaltException = class(EAbort)
  public
    ExitCode: Integer;
    constructor Create(i_ExitCode: Integer);
  end;

  TWorkException = class(EAbort)
  end;

  PaxCompilerException = class(Exception)
  end;

  PaxCancelException = class(EAbort)
  end;

  TExitMode = (emExit, emBreak, emContinue);

  PaxExitException = class(EAbort)
  public
    Mode: TExitMode;
  end;

{$IFDEF ARC}
  TPtrList = class(TList<Pointer>);
{$ELSE}
  TPtrList = TList;
{$ENDIF}

  TTypedList = class
  private
    function GetCount: Integer;
    function GetLast: TObject;
  protected
{$IFDEF ARC}
    L: TList<TObject>;
{$ELSE}
    L: TList;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure RemoveAt(I: Integer); virtual;
    procedure InsertAt(I: Integer; X: TObject);
    procedure RemoveTop;
    procedure Remove(X: TObject);
    function IndexOf(P: Pointer): Integer;
    property Count: Integer read GetCount;
    property Last: TObject read GetLast;
  end;

  TIntegerList = class
  private
{$IFDEF ARC}
    fItems: TList<Pointer>;
{$ELSE}
    fItems: TList;
{$ENDIF}
    fDupNo: Boolean;
    function GetItem(I: Integer): Integer;
    procedure SetItem(I: Integer; value: Integer);
    function GetCount: Integer;
  public
    constructor Create(DupNo: Boolean = false);
    destructor Destroy; override;
    procedure Clear;
    function Add(value: Integer): Integer;
    procedure Insert(I: Integer; value: Integer);
    function IndexOf(value: Integer): Integer;
    function LastIndexOf(value: Integer): Integer;
    procedure RemoveAt(I: Integer);
    procedure DeleteValue(value: Integer);
    function Top: Integer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function SaveToPtr(P: Pointer): Integer;
    function LoadFromPtr(P: Pointer): Integer;
    procedure SaveToWriter(S: TWriter);
    procedure LoadFromReader(S: TReader);
    function Clone: TIntegerList;
    function BinSearch(const Key: Integer): Integer;
    procedure QuickSort; overload;
    procedure QuickSort(Start, Stop: Integer); overload;
    property Count: Integer read GetCount;
    property Items[I: Integer]: Integer read GetItem write SetItem; default;
    property Last: Integer read Top;
  end;

  TIntegerStack = class(TIntegerList)
  public
    procedure Push(value: Integer);
    function Pop: Integer;
    function Depth(value: Integer): Integer;
  end;

  TAssocIntegers = class
  private
    function GetCount: Integer;
  public
    Keys, Values: TIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Key, Value: Integer);
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function SaveToPtr(P: Pointer): Integer;
    function LoadFromPtr(P: Pointer): Integer;
    function Clone: TAssocIntegers;
    procedure RemoveAt(I: Integer);
    function IndexOf(K, V: Integer): Integer;
    function Inside(c: Integer): Boolean;
    property Count: Integer read GetCount;
  end;

  TAssocStrings = class
  private
    function GetCount: Integer;
    function GetValue(const Key: String): String;
    procedure SetValue(const Key, Value: String);
  public
    Keys, Values: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key, Value: String);
    procedure RemoveAt(I: Integer);
    property Count: Integer read GetCount;
    property ValueByKey[const S: String]: String
      read GetValue write SetValue; default;
  end;

  TStringObjectList = class(TStringList)
  private
    procedure ClearObjects;
  public
    procedure Clear; override;
    destructor Destroy; override;
  end;

  TAnonymContextRec = class
  public
    SubId: Integer;
    BindList: TIntegerList;
    constructor Create;
    destructor Destroy; override;
  end;

  TAnonymContextStack = class(TTypedList)
  private
    function GetRecord(I: Integer): TAnonymContextRec;
    function GetTop: TAnonymContextRec;
  public
    function Push(SubId: Integer): TAnonymContextRec;
    procedure Pop;
    property Top: TAnonymContextRec read GetTop;
    property Records[I: Integer]: TAnonymContextRec read GetRecord; default;
  end;

  TMessageRec = class
  public
    msg_id: Cardinal;
    FullName: String;
    Class_Name: String;
    Address: Pointer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TMessageList = class(TTypedList)
  private
    function GetRecord(I: Integer): TMessageRec;
  public
    function IndexOf(const FullName: String): Integer;
    function AddRecord: TMessageRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function CreateDmtTable(const AClassName: String; var Size: Integer): Pointer;
    property Records[I: Integer]: TMessageRec read GetRecord; default;
  end;

  TActualParamRec = class
  public
    SubId: Integer;
    Params: TPtrList;
    constructor Create;
    destructor Destroy; override;
  end;

  TActualParamList = class(TTypedList)
  private
    function GetRecord(I: Integer): TActualParamRec;
  public
    function Add(SubId: Integer; Param: Pointer): TActualParamRec;
    function Find(SubId: Integer): TActualParamRec;
    procedure Remove(SubId: Integer);
    property Records[I: Integer]: TActualParamRec read GetRecord; default;
  end;

  TExportRec = class
  public
    Offset: Cardinal; // prog
    Name: String;     // prog

    Address: Cardinal; // pe
    NameAddress: Cardinal; // pe
    Ordinal: Word; // pe
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TExportList = class(TTypedList)
  private
    function GetRecord(I: Integer): TExportRec;
  public
    function Add: TExportRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TExportRec read GetRecord; default;
  end;

  TStreamRec = class
  public
    UnitName: String;
    Stream: TMemoryStream;
    constructor Create;
    destructor Destroy;override;
  end;

  TStreamList = class(TTypedList)
  private
    function GetRecord(I: Integer): TStreamRec;
  public
    function IndexOf(const UnitName: String): Integer;
    procedure AddFromStream(S: TStream; const FileName: String);
    procedure AddFromFile(const FileName: String);
    property Records[I: Integer]: TStreamRec read GetRecord; default;
  end;

  TUpStringList = class(TStringList)
  public
    function IndexOf(const S: String): Integer; override;
  end;

  TUndeclaredTypeRec = class
  public
    Id, ErrorIndex: Integer;
  end;

  TUndeclaredTypeList = class(TStringList)
    procedure Reset;
  end;

  TUndeclaredIdentRec = class
  public
    Id: Integer;
    ErrorIndex: Integer;
  end;

  TUndeclaredIdentList = class(TStringList)
    procedure Reset;
  end;

  THashArray = class
  private
    function GetMaxId: Integer;
  public
    A: array[0..MaxHash] of TIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure AddName(const S: String; Id: Integer);
    procedure DeleteName(const S: String; Id: Integer);
    procedure Clear;
    function Clone: THashArray;
    function GetList(const S: String): TIntegerList;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
    property MaxId: Integer read GetMaxId;
  end;

  TFastStringList = class
  private
    L: TStringList;
    X: THashArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const S: String): Integer;
    function IndexOfEx(const S: String; Upcase: Boolean): Integer;
    property StringList: TStringList read L;
  end;

  TExternRecKind = (erNone, erLevel, erTypeId,
                    erOwnerId, erPatternId,
                    erAncestorId, erReadId, erWriteId,
                    ePropertyInBaseClassId,
                    erGUID);

  TExternRec = class
  public
    Id: Integer;
    FullName: String;
    RecKind: TExternRecKind;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
  end;

  TExternList = class(TTypedList)
  private
    function GetRecord(I: Integer): TExternRec;
  public
    function Add(Id: Integer; const FullName: String;
                 RecKind: TExternRecKind): TExternRec;
    function Clone: TExternList;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
    property Records[I: Integer]: TExternRec read GetRecord; default;
  end;

  TCheckProc = function (const TypeName: String; Data: Pointer;
                         errKind: TExternRecKind): Boolean;

  TSomeTypeRec = class
  public
    Name: String;
    Id: Integer;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
  end;

  TSomeTypeList = class(TTypedList)
  private
    function GetRecord(I: Integer): TSomeTypeRec;
  public
    function Add(const Name: String; Id: Integer): TSomeTypeRec;
    function Clone: TSomeTypeList;
    function IndexOf(const Name: String): Integer;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
    property Records[I: Integer]: TSomeTypeRec read GetRecord; default;
  end;

  TVarPathRec = class
  public
    Id: Integer;
    VarCount: Int64;
  end;

  TVarPath = class(TTypedList)
  private
    function GetItem(I: Integer): TVarPathRec;
  public
    function Add(Id: Integer; VarCount: Int64): TVarPathRec;
    function IndexOf(VarCount: Int64): Integer;
    function LastIndexOf(VarCount: Int64): Integer;
    property Items[I: Integer]: TVarPathRec read GetItem; default;
  end;

  TVarPathList = class(TTypedList)
  private
    function GetPath(I: Integer): TVarPath;
  public
    function AddPath: TVarPath;
    procedure Add(Id: Integer; VarCount: Int64);
    function GetLevel(VarCount: Int64): Integer;
    property Pathes[I: Integer]: TVarPath read GetPath; default;
  end;

  TGuidRec = class
  public
    GUID: TGUID;
    Id: Integer;
    Name: String;
    GuidIndex: Integer;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
  end;

  TGuidList = class(TTypedList)
  private
    function GetRecord(I: Integer): TGuidRec;
  public
    function Add(const GUID: TGUID; Id: Integer;
                 const Name: String): TGuidRec;
    function Clone: TGuidList;
    function IndexOf(const GUID: TGUID): Integer;
    function GetGuidByID(Id: Integer): TGUID;
    function HasId(Id: Integer): Boolean;
    procedure SaveToStream(S: TWriter);
    procedure LoadFromStream(S: TReader);
    property Records[I: Integer]: TGuidRec read GetRecord; default;
  end;

  TStdTypeRec = class
  public
    Name: String;
    TypeID: Integer;
    Size: Integer;
    NativeName: String;
  end;

  TStdTypeList = class(TTypedList)
  private
    fPAX64: Boolean;
    function GetRecord(I: Integer): TStdTypeRec;
  public
    constructor Create;
    function Add(const TypeName: String; Size: Integer): Integer;
    function IndexOf(const S: String): Integer;
    function GetSize(TypeID: Integer): Integer;
    property Records[I: Integer]: TStdTypeRec read GetRecord; default;
    property PAX64: Boolean read fPAX64 write fPAX64;
  end;

  TAssocStringInt = class
  private
    function GetCount: Integer;
  public
    Keys: TStringList;
    Values: TIntegerList;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key: String);
    procedure AddValue(const Key: String; Value: Integer);
    procedure RemoveAt(I: Integer);
    procedure Pack;
    property Count: Integer read GetCount;
  end;

  TMacroRec = class
  public
    S1, S2: String;
    Params: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TMacroList = class(TTypedList)
  private
    function GetRecord(I: Integer): TMacroRec;
  public
    function Add(const S1, S2: String; Params: TStrings): TMacroRec;
    function IndexOf(const S1: String): Integer;
    property Records[I: Integer]: TMacroRec read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_SYS;

  // TAssocStringInt -------------------------------------------------------------

constructor TAssocStringInt.Create;
begin
  inherited;
  Keys := TStringList.Create;
  Values := TIntegerList.Create;
end;

destructor TAssocStringInt.Destroy;
begin
  FreeAndNil(Keys);
  FreeAndNil(Values);
  inherited;
end;

procedure TAssocStringInt.RemoveAt(I: Integer);
begin
  Keys.Delete(I);
  Values.RemoveAt(I);
end;

procedure TAssocStringInt.Clear;
begin
  Keys.Clear;
  Values.Clear;
end;

function TAssocStringInt.GetCount: Integer;
begin
  result := Keys.Count;
end;

procedure TAssocStringInt.Add(const Key: String);
begin
  AddValue(Key, 0);
end;

procedure TAssocStringInt.AddValue(const Key: String; Value: Integer);
begin
  Keys.Add(Key);
  Values.Add(Value);
end;

procedure TAssocStringInt.Pack;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
{$IFDEF ARC}
    begin
    end;
{$ELSE}
    Keys.Objects[I] := Pointer(Values[I]);
{$ENDIF}
end;

// TTypedList ------------------------------------------------------------------

constructor TTypedList.Create;
begin
  inherited;
{$IFDEF ARC}
  L := TList<TObject>.Create;
{$ELSE}
  L := TList.Create;
{$ENDIF}
end;

destructor TTypedList.Destroy;
begin
  Clear;
  L.Free;
  inherited;
end;

function TTypedList.GetCount: Integer;
begin
  result := L.Count;
end;

procedure TTypedList.Clear;
var
  I: Integer;
begin
  for I:=0 to L.Count - 1 do
{$IFDEF ARC}
    L[I] := nil;
{$ELSE}
    TObject(L[I]).Free;
{$ENDIF}
  L.Clear;
end;

function TTypedList.IndexOf(P: Pointer): Integer;
begin
  result := L.IndexOf(P);
end;

function TTypedList.GetLast: TObject;
begin
  result := TObject(L[Count - 1]);
end;

procedure TTypedList.RemoveAt(I: Integer);
begin
{$IFDEF ARC}
  L[I] := nil;
{$ELSE}
  TObject(L[I]).Free;
{$ENDIF}
  L.Delete(I);
end;

procedure TTypedList.InsertAt(I: Integer; X: TObject);
begin
  L.Insert(I, X);
end;

procedure TTypedList.Remove(X: TObject);
var
  I: Integer;
begin
  I := IndexOf(X);
  if I >= 0 then
    RemoveAt(I);
end;

procedure TTypedList.RemoveTop;
begin
{$IFDEF ARC}
  L[Count - 1] := nil;
{$ELSE}
  TObject(L[Count - 1]).Free;
{$ENDIF}
  L.Delete(Count - 1);
end;

//-- TIntegerList --------------------------------------------------------------

constructor TIntegerList.Create(DupNo: Boolean = false);
begin
  inherited Create;
{$IFDEF ARC}
  fItems := TList<Pointer>.Create;
{$ELSE}
  fItems := TList.Create;
{$ENDIF}
  fDupNo := DupNo;
end;

destructor TIntegerList.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TIntegerList.Clone: TIntegerList;
var
  I: Integer;
begin
  result := TIntegerList.Create;
  for I:=0 to Count - 1 do
    result.Add(Items[I]);
end;

procedure TIntegerList.Clear;
begin
  fItems.Clear;
end;

function TIntegerList.GetCount: Integer;
begin
  result := fItems.Count;
end;

function TIntegerList.GetItem(I: Integer): Integer;
begin
  result := Integer(fItems[I]);
end;

procedure TIntegerList.SetItem(I: Integer; value: Integer);
begin
  fItems[I] := Pointer(value);
end;

function TIntegerList.Add(value: Integer): Integer;
begin
  if fDupNo then
  begin
    result := IndexOf(value);
    if result >= 0 then
      Exit;
  end;
  result := fItems.Add(Pointer(value));
end;

procedure TIntegerList.Insert(I: Integer; value: Integer);
begin
  if fDupNo then
    if IndexOf(value) >= 0 then
      Exit;
  fItems.Insert(I, Pointer(value));
end;

procedure TIntegerList.RemoveAt(I: Integer);
begin
  fItems.Delete(I);
end;

procedure TIntegerList.DeleteValue(value: Integer);
var
  I: Integer;
begin
  I := IndexOf(value);
  if I >= 0 then
    fItems.Delete(I);
end;

function TIntegerList.IndexOf(value: Integer): Integer;
begin
  result := fItems.IndexOf(Pointer(value));
end;

function TIntegerList.LastIndexOf(value: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := Count - 1 downto 0 do
    if fItems[I] = Pointer(value) then
    begin
      result := I;
      Exit;
    end;
end;

function TIntegerList.Top: Integer;
begin
  if Count  = 0 then
    RIE;
  result := Items[Count - 1];
end;

function TIntegerList.BinSearch(const Key: Integer): Integer;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
  Found: Boolean;
begin
  First  := 0;
  Last   := Count - 1;
  Found  := False;
  Result := -1;

  while (First <= Last) and (not Found) do
  begin
    Pivot := (First + Last) div 2;
    if Integer(fItems[Pivot]) = Key then
    begin
      Found  := True;
      Result := Pivot;
    end
    else if Integer(fItems[Pivot]) > Key then
      Last := Pivot - 1
    else
      First := Pivot + 1;
  end;
end;

procedure TIntegerList.QuickSort;
begin
  QuickSort(0, Count - 1);
end;

procedure TIntegerList.QuickSort(Start, Stop: Integer);
var
  Left: Integer;
  Right: Integer;
  Mid: Integer;
  Pivot: Integer;
  Temp: Pointer;
begin
  Left  := Start;
  Right := Stop;
  Mid   := (Start + Stop) div 2;

  Pivot := Integer(fItems[mid]);
  repeat
    while Integer(fItems[Left]) < Pivot do Inc(Left);
    while Pivot < Integer(fItems[Right]) do Dec(Right);
    if Left <= Right then
    begin
      Temp        := fItems[Left];
      fItems[Left]  := fItems[Right]; // Swops the two Strings
      fItems[Right] := Temp;
      Inc(Left);
      Dec(Right);
    end;
  until Left > Right;

  if Start < Right then QuickSort(Start, Right); // Uses
  if Left < Stop then QuickSort(Left, Stop);     // Recursion
end;

procedure TIntegerList.SaveToStream(S: TStream);
var
  I, K: Integer;
  A: array of Integer;
  P: Pointer;
begin
  K := Count;
  SaveIntegerToStream(K, S);
  if K = 0 then
    Exit;
  SetLength(A, K);
  for I:=0 to K - 1 do
    A[I] := Integer(fItems[I]);
  P := @A[0];
  S.Write(P^, K * SizeOf(Integer));
end;

procedure TIntegerList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  A: array of Integer;
  P: Pointer;
begin
  Clear;
  K := LoadIntegerFromStream(S);
  if K = 0 then
    Exit;
  SetLength(A, K);
  P := @A[0];
  S.Read(P^, K * SizeOf(Integer));
  for I:=0 to K - 1 do
    Add(A[I]);
end;

function TIntegerList.SaveToPtr(P: Pointer): Integer;
var
  I, K: Integer;
begin
  K := Count;
  Move(K, P^, 4);
  P := ShiftPointer(P, 4);
  for I:=0 to Count - 1 do
  begin
    K := Integer(fItems[I]);
    Move(K, P^, 4);
    P := ShiftPointer(P, 4);
  end;

  result := (Count * 4) + 4;
end;

function TIntegerList.LoadFromPtr(P: Pointer): Integer;
var
  I, K, Z: Integer;
begin
  Move(P^, K, 4);
  P := ShiftPointer(P, 4);
  for I:=0 to K - 1 do
  begin
    Move(P^, Z, 4);
    P := ShiftPointer(P, 4);
    Add(Z);
  end;

  result := (Count * 4) + 4;
end;

procedure TIntegerList.SaveToWriter(S: TWriter);
var
  I: Integer;
begin
  S.WriteInteger(Count);
  for I:=0 to Count - 1 do
    S.WriteInteger(Integer(fItems[I]));
end;

procedure TIntegerList.LoadFromReader(S: TReader);
var
  I, K: Integer;
begin
  Clear;
  K := S.ReadInteger();
  for I:=0 to K - 1 do
    Add(S.ReadInteger());
end;

//-- TIntegerStack -------------------------------------------------------------

procedure TIntegerStack.Push(value: Integer);
begin
  Add(value);
end;

function TIntegerStack.Pop: Integer;
begin
  result := Items[Count - 1];
  fItems.Delete(Count - 1);
end;

function TIntegerStack.Depth(value: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I:=Count - 1 downto 0 do
  begin
    Inc(result);
    if Items[I] = value then
      Exit;
  end;
  raise Exception.Create(errInternalError);
end;

// TAssocIntegers --------------------------------------------------------------

constructor TAssocIntegers.Create;
begin
  inherited;
  Keys := TIntegerList.Create;
  Values := TIntegerList.Create;
end;

destructor TAssocIntegers.Destroy;
begin
  Keys.Free;
  Values.Free;
  inherited;
end;

function TAssocIntegers.Inside(c: Integer): Boolean;
var
  I: Integer;
begin
  result := false;
  for I := 0 to Count - 1 do
    if (c >= Keys[I]) and (c <= Values[I]) then
    begin
      result := true;
      Exit;
    end;
end;

function TAssocIntegers.IndexOf(K, V: Integer): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if (Keys[I] = K) and (Values[I] = V) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TAssocIntegers.RemoveAt(I: Integer);
begin
  Keys.RemoveAt(I);
  Values.RemoveAt(I);
end;

procedure TAssocIntegers.Clear;
begin
  Keys.Clear;
  Values.Clear;
end;

function TAssocIntegers.Clone: TAssocIntegers;
begin
  result := TAssocIntegers.Create;

  result.Keys.Free;
  result.Keys := Keys.Clone;

  result.Values.Free;
  result.Values := Values.Clone;
end;

procedure TAssocIntegers.SaveToStream(S: TStream);
begin
  Keys.SaveToStream(S);
  Values.SaveToStream(S);
end;

procedure TAssocIntegers.LoadFromStream(S: TStream);
begin
  Keys.LoadFromStream(S);
  Values.LoadFromStream(S);
end;

function TAssocIntegers.SaveToPtr(P: Pointer): Integer;
begin
  result := Keys.SaveToPtr(P);
  P := ShiftPointer(P, result);
  Inc(result, Values.SaveToPtr(P));
end;

function TAssocIntegers.LoadFromPtr(P: Pointer): Integer;
begin
  result := Keys.LoadFromPtr(P);
  P := ShiftPointer(P, result);
  Inc(result, Values.LoadFromPtr(P));
end;

function TAssocIntegers.GetCount: Integer;
begin
  result := Keys.Count;
end;

procedure TAssocIntegers.Add(Key, Value: Integer);
begin
  Keys.Add(Key);
  Values.Add(Value);
end;

// TAssocStrings ---------------------------------------------------------------

constructor TAssocStrings.Create;
begin
  inherited;
  Keys := TStringList.Create;
  Values := TStringList.Create;
end;

destructor TAssocStrings.Destroy;
begin
  Keys.Free;
  Values.Free;
  inherited;
end;

function TAssocStrings.GetValue(const Key: String): String;
var
  I: Integer;
begin
  I := Keys.IndexOf(Key);
  if I >= 0 then
    result := Values[I]
  else
    result := '';
end;

procedure TAssocStrings.SetValue(const Key, Value: String);
var
  I: Integer;
begin
  I := Keys.IndexOf(Key);
  if I >= 0 then
    Values[I] := Value
  else
    Add(Key, Value);
end;

procedure TAssocStrings.RemoveAt(I: Integer);
begin
  Keys.Delete(I);
  Values.Delete(I);
end;

procedure TAssocStrings.Clear;
begin
  Keys.Clear;
  Values.Clear;
end;

function TAssocStrings.GetCount: Integer;
begin
  result := Keys.Count;
end;

procedure TAssocStrings.Add(const Key, Value: String);
begin
  Keys.Add(Key);
  Values.Add(Value);
end;

//-- TPauseException -----------------------------------------------------------

constructor TPauseException.Create;
begin
  inherited Create('');
end;

//-- THaltException ------------------------------------------------------------

constructor THaltException.Create(i_ExitCode: Integer);
begin
  inherited Create('');
  ExitCode := i_ExitCode;
end;

procedure TStringObjectList.ClearObjects;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Objects[I] <> nil then
    begin
{$IFDEF ARC}
      Objects[I] := nil;
{$ELSE}
      Objects[I].Free;
{$ENDIF}
      Objects[I] := nil;
    end;
end;

procedure TStringObjectList.Clear;
begin
  ClearObjects;
  inherited;
end;

destructor TStringObjectList.Destroy;
begin
  ClearObjects;
  inherited;
end;

constructor TAnonymContextRec.Create;
begin
  inherited;
  BindList := TIntegerList.Create;
end;

destructor TAnonymContextRec.Destroy;
begin
  BindList.Free;
  inherited;
end;

function TAnonymContextStack.GetRecord(I: Integer): TAnonymContextRec;
begin
  result := TAnonymContextRec(L[I]);
end;

function TAnonymContextStack.GetTop: TAnonymContextRec;
begin
  result := Records[Count - 1];
end;

function TAnonymContextStack.Push(SubId: Integer): TAnonymContextRec;
begin
  result := TAnonymContextRec.Create;
  result.SubId := SubId;
  L.Add(result);
end;

procedure TAnonymContextStack.Pop;
begin
  RemoveAt(Count - 1);
end;

procedure TMessageRec.SaveToStream(S: TStream);
begin
  S.Write(msg_id, SizeOf(msg_id));
  SaveStringToStream(FullName, S);
end;

procedure TMessageRec.LoadFromStream(S: TStream);
begin
  S.Read(msg_id, SizeOf(msg_id));
  FullName := LoadStringFromStream(S);
end;

function TMessageList.IndexOf(const FullName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].FullName = FullName then
    begin
      result := I;
      Exit;
    end;
end;

function TMessageList.AddRecord: TMessageRec;
begin
  result := TMessageRec.Create;
  L.Add(result);
end;

function TMessageList.GetRecord(I: Integer): TMessageRec;
begin
  result := TMessageRec(L[I]);
end;

procedure TMessageList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TMessageList.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
    AddRecord.LoadFromStream(S);
end;

function TMessageList.CreateDmtTable(const AClassName: String; var Size: Integer): Pointer;
var
  I, K: Integer;
  DmtMethodList: PDmtMethodList;
begin
  K := 0;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].Class_Name, AClassName) then
    begin
      Inc(K);
    end;
  if K = 0 then
    result := nil
  else
  begin
    Size := SizeOf(Word) +
                  K * SizeOf(SmallInt) +
                  K * SizeOf(Pointer);
    result := AllocMem(Size);
    PDmtTable(result)^.Count := K;
    DmtMethodList := @PDmtTable(result)^.IndexList[K];
    K := 0;
    for I := 0 to Count - 1 do
      if StrEql(Records[I].Class_Name, AClassName) then
      begin
        PDmtTable(result)^.IndexList[K] := Records[I].msg_id;
        DmtMethodList[K] := Records[I].Address;
        Inc(K);
      end;
  end;
end;

constructor TActualParamRec.Create;
begin
  inherited;
  Params := TPtrList.Create;
end;

destructor TActualParamRec.Destroy;
begin
  Params.Free;
  inherited;
end;

function TActualParamList.GetRecord(I: Integer): TActualParamRec;
begin
  result := TActualParamRec(L[I]);
end;

function TActualParamList.Add(SubId: Integer; Param: Pointer): TActualParamRec;
begin
  result := Find(SubId);
  if result = nil then
  begin
    result := TActualParamRec.Create;
    result.SubId := SubId;
    L.Add(result);
  end;
  result.Params.Add(Param);
end;

function TActualParamList.Find(SubId: Integer): TActualParamRec;
var
  I: Integer;
begin
  result := nil;
  for I := Count - 1 downto 0 do
    if Records[I].SubId = SubId then
    begin
      result := Records[I];
      Exit;
    end;
end;

procedure TActualParamList.Remove(SubId: Integer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Records[I].SubId = SubId then
    begin
      RemoveAt(I);
      Exit;
    end;
end;

procedure TExportRec.SaveToStream(S: TStream);
begin
  SaveStringToStream(Name, S);
  S.Write(Offset, SizeOf(Offset));
end;

procedure TExportRec.LoadFromStream(S: TStream);
begin
  Name := LoadStringFromStream(S);
  S.Read(Offset, SizeOf(Offset));
end;

function TExportList.GetRecord(I: Integer): TExportRec;
begin
  result := TExportRec(L[I]);
end;

function TExportList.Add: TExportRec;
begin
  result := TExportRec.Create;
  L.Add(result);
  result.Ordinal := Count;
end;

procedure TExportList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TExportList.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
    Add.LoadFromStream(S);
end;

constructor TStreamRec.Create;
begin
  inherited;
  Stream := TMemoryStream.Create;
end;

destructor TStreamRec.Destroy;
begin
  inherited;
  Stream.Free;
end;

function TStreamList.GetRecord(I: Integer): TStreamRec;
begin
  result := TStreamRec(L[I]);
end;

function TStreamList.IndexOf(const UnitName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(UnitName, Records[I].UnitName) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TStreamList.AddFromStream(S: TStream; const FileName: String);
var
  UnitName: String;
  R: TStreamRec;
begin
  UnitName := ExtractFullOwner(FileName);
  if IndexOf(UnitName) <> -1 then
    Exit;
  R := TStreamRec.Create;
  R.UnitName := UnitName;
  R.Stream.LoadFromStream(S);
  L.Add(R);
end;

procedure TStreamList.AddFromFile(const FileName: String);
var
  UnitName: String;
  R: TStreamRec;
begin
  UnitName := ExtractFullOwner(FileName);
  if IndexOf(UnitName) <> -1 then
    Exit;
  R := TStreamRec.Create;
  R.UnitName := UnitName;
  R.Stream.LoadFromFile(FileName);
  L.Add(R);
end;

// TUpStringList ---------------------------------------------------------------

function TUpStringList.IndexOf(const S: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Strings[I], S) then
    begin
      result := I;
      Exit;
    end;
end;

// TUndeclaredTypeList ---------------------------------------------------------

procedure TUndeclaredTypeList.Reset;
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
{$IFDEF ARC}
    Objects[I] := nil;
{$ELSE}
    Objects[I].Free;
{$ENDIF}
  Clear;
end;

// TUndeclaredIdentList --------------------------------------------------------

procedure TUndeclaredIdentList.Reset;
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
{$IFDEF ARC}
    Objects[I] := nil;
{$ELSE}
    Objects[I].Free;
{$ENDIF}
  Clear;
end;

constructor THashArray.Create;
var
  I: Integer;
begin
  for I:=0 to MaxHash do
    A[I] := TIntegerList.Create;
end;

procedure THashArray.SaveToStream(S: TWriter);
var
  I: Integer;
begin
  for I:=0 to MaxHash do
    A[I].SaveToWriter(S);
end;

procedure THashArray.LoadFromStream(S: TReader);
var
  I: Integer;
begin
  for I:=0 to MaxHash do
    A[I].LoadFromReader(S);
end;

procedure THashArray.Clear;
var
  I: Integer;
begin
  for I:=0 to MaxHash do
    A[I].Clear;
end;

destructor THashArray.Destroy;
var
  I: Integer;
begin
  for I:=0 to MaxHash do
    A[I].Free;
end;

function THashArray.GetList(const S: String): TIntegerList;
var
  H: Integer;
begin
  H := HashNumber(S);
  result := A[H];
end;

function THashArray.GetMaxId: Integer;
var
  I, J: Integer;
begin
  result := 0;
  for I:=0 to MaxHash do
    for J := 0 to A[I].Count - 1 do
      if A[I][J] > result then
        result := A[I][J];
end;

procedure THashArray.AddName(const S: String; Id: Integer);
var
  H: Integer;
begin
  if S = '' then
    Exit;
  H := HashNumber(S);
  with A[H] do
    Add(Id);
end;

procedure THashArray.DeleteName(const S: String; Id: Integer);
var
  H: Integer;
begin
  if S = '' then
    Exit;
  H := HashNumber(S);
  with A[H] do
    DeleteValue(Id);
end;

function THashArray.Clone: THashArray;
var
  I, J: Integer;
begin
  result := THashArray.Create;
  for I:=0 to MaxHash do
  begin
    for J:=0 to A[I].Count - 1 do
      result.A[I].Add(A[I][J]);
  end;
end;

// TFastStringList -------------------------------------------------------------

constructor TFastStringList.Create;
begin
  inherited;
  L := TStringList.Create;
  X := THashArray.Create;
end;

destructor TFastStringList.Destroy;
begin
  L.Free;
  X.Free;
  inherited;
end;

procedure TFastStringList.Clear;
begin
  L.Clear;
  X.Clear;
end;

function TFastStringList.Add(const S: String): Integer;
var
  H: Integer;
begin
  H := HashNumber(S);
  result := L.Add(S);
  X.A[H].Add(result);
end;

function TFastStringList.IndexOfEx(const S: String; Upcase: Boolean): Integer;
var
  I, J: Integer;
  List: TIntegerList;
begin
  result := -1;

  List := X.GetList(S);

  for I := 0 to List.Count - 1 do
  begin
    J := List[I];
    if Upcase then
    begin
      if StrEql(L[J], S) then
      begin
        result := J;
        Exit;
      end;
    end
    else
    begin
      if L[J] = S then
      begin
        result := J;
        Exit;
      end;
    end;
  end;
end;

// TExternRec ------------------------------------------------------------------

procedure TExternRec.SaveToStream(S: TWriter);
begin
  S.WriteInteger(id);
  S.WriteString(FullName);
  S.Write(RecKind, SizeOf(RecKind));
end;

procedure TExternRec.LoadFromStream(S: TReader);
begin
  Id := S.ReadInteger;
  FullName := S.ReadString;
  S.Read(RecKind, SizeOf(RecKind));
end;

// TExternList -----------------------------------------------------------------

function TExternList.Clone: TExternList;
var
  I: Integer;
begin
  result := TExternList.Create;
  for I:=0 to Count - 1 do
    with Records[I] do
    begin
      result.Add(Id, FullName, RecKind);
    end;
end;

function TExternList.GetRecord(I: Integer): TExternRec;
begin
  result := TExternRec(L[I]);
end;

function TExternList.Add(Id: Integer; const FullName: String;
                         RecKind: TExternRecKind): TExternRec;
begin
  result := TExternRec.Create;
  result.Id := Id;
  result.FullName := FullName;
  result.RecKind := RecKind;
  L.Add(result);
end;

procedure TExternList.SaveToStream(S: TWriter);
var
  I: Integer;
begin
  S.WriteInteger(Count);
  for I := 0 to Count - 1 do
    Records[I].SaveToStream(S);
end;

procedure TExternList.LoadFromStream(S: TReader);
var
  I, K: Integer;
  R: TExternRec;
begin
  K := S.ReadInteger;
  for I := 0 to K - 1 do
  begin
    R := TExternRec.Create;
    R.LoadFromStream(S);
    L.Add(R);
  end;
end;

// TSomeTypeRec ----------------------------------------------------------------

procedure TSomeTypeRec.SaveToStream(S: TWriter);
begin
  S.WriteString(Name);
  S.WriteInteger(Id);
end;

procedure TSomeTypeRec.LoadFromStream(S: TReader);
begin
  Name := S.ReadString();
  Id := S.ReadInteger();
end;

// TSomeTypeList ---------------------------------------------------------------

function TSomeTypeList.GetRecord(I: Integer): TSomeTypeRec;
begin
  result := TSomeTypeRec(L[I]);
end;

function TSomeTypeList.Add(const Name: String; Id: Integer): TSomeTypeRec;
begin
  result := TSomeTypeRec.Create;
  result.Name := Name;
  result.Id := Id;
  L.Add(result);
end;

function TSomeTypeList.Clone: TSomeTypeList;
var
  I: Integer;
begin
  result := TSomeTypeList.Create;
  for I := 0 to Count - 1 do
    result.Add(Records[I].Name, Records[I].Id);
end;

function TSomeTypeList.IndexOf(const Name: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].Name, Name) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TSomeTypeList.SaveToStream(S: TWriter);
var
  I, K: Integer;
begin
  K := Count;
  S.WriteInteger(K);
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TSomeTypeList.LoadFromStream(S: TReader);
var
  I, K: Integer;
  R: TSomeTypeRec;
begin
  K := S.ReadInteger();
  for I := 0 to K - 1 do
  begin
    R := TSomeTypeRec.Create;
    R.LoadFromStream(S);
    L.Add(R);
  end;
end;

// TVarPath --------------------------------------------------------------------

function TVarPath.GetItem(I: Integer): TVarPathRec;
begin
  result := TVarPathRec(L[I]);
end;

function TVarPath.Add(Id: Integer; VarCount: Int64): TVarPathRec;
begin
  result := TVarPathRec.Create;
  result.Id := Id;
  result.VarCount := VarCount;
  L.Add(result);
end;

function TVarPath.IndexOf(VarCount: Int64): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Items[I].VarCount = VarCount then
    begin
      result := I;
      Exit;
    end;
end;

function TVarPath.LastIndexOf(VarCount: Int64): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := Count - 1 downto 0 do
    if Items[I].VarCount = VarCount then
    begin
      result := I;
      Exit;
    end;
end;

// TVarPathList ----------------------------------------------------------------

function TVarPathList.GetPath(I: Integer): TVarPath;
begin
  result := TVarPath(L[I]);
end;

function TVarPathList.AddPath: TVarPath;
begin
  result := TVarPath.Create;
  L.Add(result);
end;

function TVarPathList.GetLevel(VarCount: Int64): Integer;
begin
  if VarCount < 100 then
    result := 1
  else if VarCount < 10000 then
    result := 2
  else if VarCount < 1000000 then
    result := 3
  else if VarCount < 100000000 then
    result := 4
  else if VarCount < 10000000000 then
    result := 5
  else if VarCount < 1000000000000 then
    result := 6
  else if VarCount < 100000000000000 then
    result := 7
  else
    raise PaxCompilerException.Create(errTooManyNestedCaseBlocks);
end;

procedure TVarPathList.Add(Id: Integer; VarCount: Int64);
var
  I, J, Idx: Integer;
  Path: TVarPath;
  Level: Integer;
  ParentVarCount: Integer;
  R: TVarPathRec;
  new_path: Boolean;
begin
  if Count = 0 then
  begin
    Path := AddPath;
    Path.Add(Id, VarCount);
    Exit;
  end;

  for I := 0 to Count - 1 do
  begin
    Path := Pathes[I];
    Idx := Path.IndexOf(VarCount);
    if Idx >= 0 then
    begin
      Path.Add(Id, VarCount);
      Exit;
    end;
  end;

  Level := GetLevel(VarCount);

  if Level = 1 then
  begin
    Path := AddPath;
    Path.Add(Id, VarCount);
    Exit;
  end;

 case Level of
   2:
   begin
     ParentVarCount := VarCount mod 100;
     new_path := VarCount div 100 > 1;
   end;
   3:
   begin
     ParentVarCount := VarCount mod 10000;
     new_path := VarCount div 10000 > 1;
   end;
   4:
   begin
     ParentVarCount := VarCount mod 1000000;
     new_path := VarCount div 1000000 > 1;
   end;
   5:
   begin
     ParentVarCount := VarCount mod 100000000;
     new_path := VarCount div 100000000 > 1;
   end;
   6:
   begin
     ParentVarCount := VarCount mod 10000000000;
     new_path := VarCount div 10000000000 > 1;
   end;
   7:
   begin
     ParentVarCount := VarCount mod 1000000000000;
     new_path := VarCount div 1000000000000 > 1;
   end
 else
   raise PaxCompilerException.Create(errTooManyNestedCaseBlocks);
 end;

  for I := Count - 1 downto 0 do
  begin
    Idx := Pathes[I].LastIndexOf(ParentVarCount);
    if Idx >= 0 then
    begin
      if new_path then
      begin
        Path := AddPath;
        for J := 0 to Idx do
        begin
          R := Pathes[I][J];
          Path.Add(R.Id, R.VarCount);
        end;
        Path.Add(Id, VarCount);
      end
      else
      begin
        Path := Pathes[I];
        Path.Add(Id, VarCount);
      end;

      Exit;
    end;
  end;

  Path := AddPath;
  Path.Add(Id, VarCount);
end;

procedure TGuidRec.SaveToStream(S: TWriter);
begin
  S.Write(Id, SizeOf(Id));
  S.Write(Guid, SizeOf(Guid));
  S.WriteString(Name);
end;

procedure TGuidRec.LoadFromStream(S: TReader);
begin
  S.Read(Id, SizeOf(Id));
  S.Read(Guid, SizeOf(Guid));
  Name := S.ReadString;
end;

function TGuidList.Add(const GUID: TGUID; Id: Integer;
                       const Name: String): TGuidRec;
var
  I: Integer;
begin
  I := IndexOf(GUID);
  if I >= 0 then
  begin
    result := Records[I];
    Exit;
  end;

  result := TGuidRec.Create;
  L.Add(result);
  result.GUID := GUID;
  result.Name := Name;
  result.GuidIndex := L.Count - 1;
  result.Id := Id;
end;

function TGuidList.Clone: TGuidList;
var
  I: Integer;
begin
  result := TGuidList.Create;
  for I := 0 to Count - 1 do
    result.Add(Records[I].GUID, Records[I].Id, Records[I].Name);
end;

function TGuidList.IndexOf(const GUID: TGUID): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if GuidsAreEqual(Records[I].GUID, GUID) then
    begin
      result := I;
      Exit;
    end;
end;

function TGuidList.GetGuidByID(Id: Integer): TGUID;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Records[I].Id = Id then
    begin
      result := Records[I].GUID;
      Exit;
    end;
  result := IUnknown;
end;

function TGuidList.HasId(Id: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Records[I].Id = Id then
    begin
      result := true;
      Exit;
    end;
  result := false;
end;

procedure TGuidList.SaveToStream(S: TWriter);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TGuidList.LoadFromStream(S: TReader);
var
  I, K: Integer;
  R: TGuidRec;
begin
  Clear;
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
  begin
    R := TGuidRec.Create;
    R.LoadFromStream(S);
    Add(R.GUID, R.Id, R.Name);
    R.Free;
  end;
end;

function TGuidList.GetRecord(I: Integer): TGuidRec;
begin
  result := TGuidRec(L[I]);
end;

constructor TStdTypeList.Create;
begin
  inherited;
{$IFDEF PAX64}
  fPAX64 := true;
  H_ExceptionPtr := H_ExceptionPtr_64;
  H_ByteCodePtr := H_ByteCodePtr_64;
  H_Flag := H_Flag_64;
  H_SkipPop := H_SkipPop_64;
  FirstShiftValue := FirstShiftValue_64;
{$ENDIF}
end;

function TStdTypeList.IndexOf(const S: String): Integer;
var
  I: Integer;
  Q: String;
begin
  Q := UpperCase(S);
  for I:=0 to Count - 1 do
    if Records[I].Name = Q then
    begin
      result := I;
      Exit;
    end;
  result := -1;
end;

function TStdTypeList.GetRecord(I: Integer): TStdTypeRec;
begin
{$IFNDEF PAXARM}
  if I = typePANSICHAR then
    I := typePOINTER
  else
{$ENDIF}
  if I = typePWIDECHAR then
    I := typePOINTER
  else if I = typePVOID then
    I := typePOINTER;
  result := TStdTypeRec(L[I]);
end;

function TStdTypeList.Add(const TypeName: String; Size: Integer): Integer;
var
  R: TStdTypeRec;
begin
  R := TStdTypeRec.Create;
  R.TypeID := L.Count;
  R.Size := Size;
  R.Name := UpperCase(TypeName);
  R.NativeName := TypeName;
  result := R.TypeID;
  L.Add(R);
end;

function TStdTypeList.GetSize(TypeID: Integer): Integer;
begin
  result := Records[TypeID].Size;
  if PAX64 then
  begin
    if TypeID in [
{$IFNDEF PAXARM}
                  typeANSISTRING,
                  typeWIDESTRING,
                  typeUNICSTRING,
                  typePANSICHAR,
{$ENDIF}
                  typePOINTER,
                  typeCLASS,
                  typeCLASSREF,
                  typePROC,
                  typeDYNARRAY,
                  typeOPENARRAY,
                  typePWIDECHAR,
                  typePVOID,
                  typeINTERFACE] then
                    result := 8
    else if typeID = typeEVENT then
      result := 16
    else if typeID in [typeVARIANT, typeOLEVARIANT] then
      result := 24;
  end;
end;

constructor TMacroRec.Create;
begin
  inherited;
  Params := TStringList.Create;
end;

destructor TMacroRec.Destroy;
begin
  FreeAndNil(Params);
  inherited;
end;

function TMacroList.GetRecord(I: Integer): TMacroRec;
begin
  result := TMacroRec(L[I]);
end;

function TMacroList.Add(const S1, S2: String; Params: TStrings): TMacroRec;
var
  I: Integer;
begin
  result := TMacroRec.Create;
  result.S1 := S1;
  result.S2 := S2;
  for I := 0 to Params.Count - 1 do
    result.Params.Add(Params[I]);
  L.Add(result);
end;

function TMacroList.IndexOf(const S1: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].S1 = S1 then
    begin
      result := I;
      Exit;
    end;
end;

end.
