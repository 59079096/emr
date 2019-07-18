////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_GC.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}

unit PAXCOMP_GC;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

const
  MAX_OBJECTS: Integer = 1024;

type
  TGC = class;

  TGC_Object = class(TPersistent)
  private
    fRefCount: Integer;
  public
    constructor Create;
    function AddRef: Integer;
    function __toString: String; virtual;
    function GetGC: TGC; virtual; abstract;
    property RefCount: Integer read fRefCount write fRefCount;
  end;

  PGC_Object = ^TGC_Object;

  TGC = class(TTypedList)
  private
    GC_Ref: TPtrList;
    Bound: Integer;
    function GetRecord(I: Integer): TGC_Object;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ClearRef;
    procedure ClearObjects;
    function AddObject(X: TGC_Object): Integer;
    function AddReference(X: TGC_Object): Integer;
    procedure Remove(X: TGC_Object);
    procedure Collect;
    procedure Mark;
    property Records[I: Integer]: TGC_Object read GetRecord; default;
  end;

procedure GC_Assign(Dest: PGC_Object; Source: TGC_Object);

implementation

procedure GC_Assign(Dest: PGC_Object; Source: TGC_Object);
var
  GC: TGC;
begin
  if Source = nil then
  begin
    if Dest = nil then
      Exit;
    if Dest^ = nil then
      Exit;

    GC := Dest^.GetGC;

    Dec(Dest^.fRefCount);
    if Dest^.fRefCount = 0 then
      GC.Remove(Dest^);
    Exit;
  end;

  GC := Source.GetGC;

  Inc(Source.fRefCount);
  if Dest^ <> nil then
  begin
    Dec(Dest^.fRefCount);
    if Dest^.fRefCount = 0 then
      GC.Remove(Dest^);
  end;
  Dest^ := Source;
end;

// TGC_Object ------------------------------------------------------------------

constructor TGC_Object.Create;
begin
  inherited;
  fRefCount := 1;
end;

function TGC_Object.__toString: String;
begin
  result := '';
end;

function TGC_Object.AddRef: Integer;
begin
  Inc(fRefCount);
  Result := fRefCount;
end;

// TGC -------------------------------------------------------------------------

constructor TGC.Create;
begin
  GC_Ref := TPtrList.Create;
  Bound := 0;
  inherited;
end;

destructor TGC.Destroy;
begin
  Clear;
  inherited;
  GC_Ref.Free;
end;

procedure TGC.ClearRef;
var
  I, K: Integer;
begin
  K := GC_Ref.Count;
  if K = 0 then
    Exit;
  for I := K - 1 downto 0 do
{$IFDEF ARC}
    GC_Ref[I] := nil;
{$ELSE}
    TObject(GC_Ref[I]).Free;
{$ENDIF}
  GC_Ref.Clear;
end;

procedure TGC.Clear;
var
  I: Integer;
begin
  ClearRef;
  for I := Count - 1 downto 0 do
{$IFDEF ARC}
    L[I] := nil;
{$ELSE}
    TObject(L[I]).Free;
{$ENDIF}
  L.Clear;
  Bound := 0;
end;

function TGC.GetRecord(I: Integer): TGC_Object;
begin
  result := TGC_Object(L[I]);
end;

function TGC.AddObject(X: TGC_Object): Integer;
begin
  result := L.IndexOf(X);
  if result >= 0 then
    Exit;
  L.Add(X);
  if L.Count = MAX_OBJECTS then
    Collect;
  result := L.Count;
end;

function TGC.AddReference(X: TGC_Object): Integer;
begin
  result := GC_Ref.Add(X);
end;

procedure TGC.Remove(X: TGC_Object);
begin
  L.Remove(X);
  X.Free;
end;

procedure TGC.Collect;
var
  I: Integer;
  X: TGC_Object;
begin
  for I := Count - 1 downto Bound do
  begin
    X := Records[I];
    if X.RefCount <= 0 then
    begin
      L.Delete(I);
      X.Free;
    end;
  end;
end;

procedure TGC.ClearObjects;
var
  I: Integer;
  X: TGC_Object;
begin
  ClearRef;
  for I := Count - 1 downto Bound do
  begin
    X := Records[I];
    L.Delete(I);
    X.Free;
  end;
end;

procedure TGC.Mark;
begin
  Bound := Count;
end;

end.
