////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_SYMBOL_OFFSET.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O+}
unit PAXCOMP_OFFSET;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

type
  TOffsetRec = class
  public
    Id: Integer;
    Shift: Integer;
    Offset: Integer;
    Size: Integer;
    function Clone: TOffsetRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TOffsetList = class(TTypedList)
  private
    Sorted: Boolean;
    function GetRecord(I: Integer): TOffsetRec;
    procedure SetRecord(I: Integer; value: TOffsetRec);
  public
    InitSize: Integer;
    constructor Create;
    function Add(Id, Shift, Offset, Size: Integer): TOffsetRec;
    procedure Clear; override;
    function Clone: TOffsetList;
    function GetSize: Integer;
    function GetOffset(Shift: Integer): Integer;
    function HasId(Id: Integer): Boolean;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function GetOffsetById(Id: Integer): Integer;
    property Records[I: Integer]: TOffsetRec read GetRecord write SetRecord; default;
  end;

procedure TOffsetList_Sort(Self: TOffsetList);

implementation

// TOffsetRec ------------------------------------------------------------------

function TOffsetRec.Clone: TOffsetRec;
begin
  result := TOffsetRec.Create;
  result.Id := Id;
  result.Shift := Shift;
  result.Offset := Offset;
  result.Size := Size;
end;

procedure TOffsetRec.SaveToStream(S: TStream);
begin
  S.Write(Shift, SizeOf(Shift));
  S.Write(Offset, SizeOf(Offset));
end;

procedure TOffsetRec.LoadFromStream(S: TStream);
begin
  S.Read(Shift, SizeOf(Shift));
  S.Read(Offset, SizeOf(Offset));
end;

// TOffsetList -----------------------------------------------------------------

constructor TOffsetList.Create;
begin
  inherited;
  InitSize := 0;
  Sorted := false;
end;

function TOffsetList.Add(Id, Shift, Offset, Size: Integer): TOffsetRec;
begin
  result := TOffsetRec.Create;
  result.Id := Id;
  result.Shift := Shift;
  result.Offset := Offset;
  result.Size := Size;
  L.Add(result);
end;

function TOffsetList.GetSize: Integer;
var
  I, SZ: Integer;
begin
  result := InitSize;
  for I := 0 to Count - 1 do
  begin
    SZ := Records[I].Size;
    Inc(result, SZ);
  end;
end;

function BinSearch(List: TOffsetList; const Key: Integer): Integer;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
  Found: Boolean;
begin
  First  := 0;
  Last   := List.Count - 1;
  Found  := False;
  Result := -1;

  while (First <= Last) and (not Found) do
  begin
    Pivot := (First + Last) div 2;
    if TOffsetRec(List.L[Pivot]).Shift = Key then
    begin
      Found  := True;
      Result := Pivot;
    end
    else if TOffsetRec(List.L[Pivot]).Shift > Key then
      Last := Pivot - 1
    else
      First := Pivot + 1;
  end;
end;

function TOffsetList.GetOffset(Shift: Integer): Integer;
var
  I: Integer;
  R: TOffsetRec;
begin
  result := -1;

  if Shift < StdSize then
  begin
    result := Shift;
    Exit;
  end;

  if Sorted then
  begin
    I := BinSearch(Self, Shift);
    if I = -1 then
      Exit;
    result := Records[I].Offset;
    Exit;
  end;

  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if R.Shift = Shift then
    begin
      result := R.Offset;
      Exit;
    end;
  end;
end;

function TOffsetList.HasId(Id: Integer): Boolean;
var
  I: Integer;
  R: TOffsetRec;
begin
  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if R.Id = Id then
    begin
      result := true;
      Exit;
    end;
  end;
  result := false;
end;

function TOffsetList.GetRecord(I: Integer): TOffsetRec;
begin
  result := TOffsetRec(L[I]);
end;

procedure TOffsetList.SetRecord(I: Integer; value: TOffsetRec);
begin
  L[I] := value;
end;

function TOffsetList.GetOffsetById(Id: Integer): Integer;
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    if Records[I].Id = Id then
    begin
      result := Records[I].Offset;
      Exit;
    end;
  raise Exception.Create(errInternalError);
end;

procedure TOffsetList.Clear;
begin
  inherited;
  InitSize := 0;
  Sorted := false;
end;

function TOffsetList.Clone: TOffsetList;
var
  I: Integer;
  R: TOffsetRec;
begin
  result := TOffsetList.Create;
  for I := 0 to Count - 1 do
  begin
    R := Records[I].Clone;
    result.L.Add(R);
  end;
end;

procedure TOffsetList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TOffsetList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TOffsetRec;
begin
  S.Read(K, SizeOf(Integer));
  for I := 0 to K - 1 do
  begin
    R := TOffsetRec.Create;
    R.LoadFromStream(S);
    L.Add(R);
  end;
end;

procedure QuickSort(var List: TOffsetList; Start, Stop: Integer);
var
  Left: Integer;
  Right: Integer;
  Mid: Integer;
  Pivot: TOffsetRec;
  Temp: TOffsetRec;
begin
  Left  := Start;
  Right := Stop;
  Mid   := (Start + Stop) div 2;

  Pivot := List[mid];
  repeat
    while List[Left].Shift < Pivot.Shift do Inc(Left);
    while Pivot.Shift < List[Right].Shift do Dec(Right);
    if Left <= Right then
    begin
      Temp        := List[Left];
      List[Left]  := List[Right]; // Swops the two Strings
      List[Right] := Temp;
      Inc(Left);
      Dec(Right);
    end;
  until Left > Right;

  if Start < Right then QuickSort(List, Start, Right); // Uses
  if Left < Stop then QuickSort(List, Left, Stop);     // Recursion
end;

procedure TOffsetList_Sort(Self: TOffsetList);
begin
  if Self.Count > 0 then
  begin
    QuickSort(Self, 0, Self.Count - 1);
    Self.Sorted := true;
  end;
end;

end.
