////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_TRYLST.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_TRYLST;
interface

uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_PAUSE;

type
  TTryRec = class(TPauseRec)
  private
  public
    TryKind: TTryKind;
    ExceptOnInfo: TAssocIntegers;
    Level: Integer;

    BreakOffset: Integer;
    ContinueOffset: Integer;
    N: Integer; // not saved into stream
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function Clone: TTryRec;
  end;

  TTryList = class(TTypedList)
  private
    function GetRecord(I: Integer): TTryRec;
  public
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    function Add: TTryRec;
    property Records[I: Integer]: TTryRec read GetRecord; default;
  end;

implementation

//----------- TTryRec ----------------------------------------------------------

constructor TTryRec.Create;
begin
  inherited;
  ExceptOnInfo := TAssocIntegers.Create;
end;

destructor TTryRec.Destroy;
begin
  FreeAndNil(ExceptOnInfo);
  inherited;
end;

procedure TTryRec.SaveToStream(S: TStream);
begin
  S.Write(TryKind, SizeOf(TryKind));
  S.Write(ProgOffset, SizeOf(ProgOffset));
  S.Write(Level, SizeOf(Level));
  S.Write(BreakOffset, SizeOf(BreakOffset));
  S.Write(ContinueOffset, SizeOf(ContinueOffset));
  ExceptOnInfo.SaveToStream(S);
end;

procedure TTryRec.LoadFromStream(S: TStream);
begin
  S.Read(TryKind, SizeOf(TryKind));
  S.Read(ProgOffset, SizeOf(ProgOffset));
  S.Read(Level, SizeOf(Level));
  S.Read(BreakOffset, SizeOf(BreakOffset));
  S.Read(ContinueOffset, SizeOf(ContinueOffset));
  ExceptOnInfo.LoadFromStream(S);
end;

function TTryRec.Clone: TTryRec;
begin
  result := TTryRec.Create;

  result.TryKind := TryKind;
  result.Level := Level;
  result.N := N;

  FreeAndNil(result.ExceptOnInfo);
  result.ExceptOnInfo := ExceptOnInfo.Clone;
// TPauseRec

  result._EBP := _EBP;
  result._ESP := _ESP;
  result.ESP0 := ESP0;
  result.ProgOffset := ProgOffset;
  result.StackFrame := StackFrame;
  result.StackFrameSize := StackFrameSize;
end;

//----------- TTryList ---------------------------------------------------------

function TTryList.Add: TTryRec;
begin
  result := TTryRec.Create;
  L.Add(result);
end;

function TTryList.GetRecord(I: Integer): TTryRec;
begin
  result := TTryRec(L[I]);
end;

procedure TTryList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I:=0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TTryList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TTryRec;
begin
  Clear;

  S.Read(K, SizeOf(Integer));
  for I:=0 to K - 1 do
  begin
    R := Add;
    R.LoadFromStream(S);
  end;
end;

end.
