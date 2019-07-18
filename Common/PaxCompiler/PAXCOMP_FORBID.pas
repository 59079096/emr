////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_FORBID.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////
{$I PaxCompiler.def}
unit PAXCOMP_FORBID;
interface
uses {$I uses.def}
  PAXCOMP_SYS,
  PAXCOMP_TYPES,
  SysUtils,
  Classes;

type
  TForbiddenPropRec = class
  private
    C: TClass;
    L: TStringList;
    All: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const PropName: String): Integer;
  end;

  TForbiddenPropList = class(TTypedList)
  private
    function GetRecord(I: Integer): TForbiddenPropRec;
  public
    function FindRecord(C: TClass): TForbiddenPropRec;
    procedure Add(C: TClass; const PropName: String);
    procedure AddAll(C: TClass);
    procedure Delete(C: TClass; const PropName: String);
    procedure DeleteAll(C: TClass);
    function IsForbidden(C: TClass; const PropName: String): Boolean;
    function IsForbiddenAll(C: TClass): Boolean;
    property Records[I: Integer]: TForbiddenPropRec read GetRecord; default;
  end;

var
  ForbiddenPropList: TForbiddenPropList = nil;

implementation

constructor TForbiddenPropRec.Create;
begin
  inherited;
  L := TStringList.Create;
  All := false;
end;

destructor TForbiddenPropRec.Destroy;
begin
  FreeAndNil(L);
  inherited;
end;

function TForbiddenPropRec.IndexOf(const PropName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to L.Count - 1 do
    if StrEql(L[I], PropName) then
    begin
      result := I;
      Exit;
    end;
end;

function TForbiddenPropList.GetRecord(I: Integer): TForbiddenPropRec;
begin
  result := TForbiddenPropRec(L[I]);
end;

function TForbiddenPropList.FindRecord(C: TClass): TForbiddenPropRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to L.Count - 1 do
    if Records[I].C = C then
    begin
      result := Records[I];
      Exit;
    end;
end;

procedure TForbiddenPropList.Add(C: TClass; const PropName: String);
var
  R: TForbiddenPropRec;
begin
  R := FindRecord(C);
  if R = nil then
  begin
    R := TForbiddenPropRec.Create;
    R.C := C;
    L.Add(R);
  end;
  R.L.Add(PropName);
end;

procedure TForbiddenPropList.AddAll(C: TClass);
var
  R: TForbiddenPropRec;
begin
  R := FindRecord(C);
  if R = nil then
  begin
    R := TForbiddenPropRec.Create;
    R.C := C;
    L.Add(R);
  end;
  R.All := true;
end;

procedure TForbiddenPropList.Delete(C: TClass; const PropName: String);
var
  R: TForbiddenPropRec;
  I: Integer;
begin
  R := FindRecord(C);
  if R = nil then
    Exit;
  I := R.IndexOf(PropName);
  if I >= 0 then
    R.L.Delete(I);
end;

procedure TForbiddenPropList.DeleteAll(C: TClass);
var
  R: TForbiddenPropRec;
begin
  R := FindRecord(C);
  if R = nil then
    Exit;
  R.L.Clear;
end;

function TForbiddenPropList.IsForbidden(C: TClass; const PropName: String): Boolean;
var
  R: TForbiddenPropRec;
begin
  R := FindRecord(C);
  if R = nil then
  begin
    result := false;
    Exit;
  end;

  if R.All then
  begin
    result := true;
    Exit;
  end;

  result := R.IndexOf(PropName) >= 0;
end;

function TForbiddenPropList.IsForbiddenAll(C: TClass): Boolean;
var
  R: TForbiddenPropRec;
begin
  R := FindRecord(C);
  if R = nil then
  begin
    result := false;
    Exit;
  end;
  result := R.All;
end;

initialization
finalization

  if Assigned(ForbiddenPropList) then
    ForbiddenPropList.Free;
end.

