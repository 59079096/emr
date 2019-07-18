////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_EXTRASYMBOL_TABLE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_EXTRASYMBOL_TABLE;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_MAP,
  PAXCOMP_STDLIB;
type
  TExtraSymbolTable = class(TSymbolTable)
  private
    LocalSymbolTable: TSymbolTable;
  protected
    function GetRecord(I: Integer): TSymbolRec; override;
  public
    constructor Create(kernel: Pointer; ALocalSymbolTable: TSymbolTable);
    procedure Reset; override;
    property Records[I: Integer]: TSymbolRec read GetRecord; default;
  end;

implementation

constructor TExtraSymbolTable.Create(kernel: Pointer; ALocalSymbolTable: TSymbolTable);
begin
  inherited Create(kernel);
  LocalSymbolTable := ALocalSymbolTable;
  IsExtraTable := true;
end;

procedure TExtraSymbolTable.Reset;
var
  I: Integer;
begin
  for I:=A.Count - 1 downto 0 do
{$IFDEF ARC}
    A[I] := nil;
{$ELSE}
    TSymbolRec(A[I]).Free;
{$ENDIF}

  A.Clear;

  Card := LocalSymbolTable.Card;

  ResultId := LocalSymbolTable.ResultId;

  TrueId := LocalSymbolTable.TrueId;
  FalseId := LocalSymbolTable.FalseId;
  NilId := LocalSymbolTable.NilId;
  EventNilId := LocalSymbolTable.EventNilId;
  CurrExceptionObjectId := LocalSymbolTable.CurrExceptionObjectId;
  EmptySetId := LocalSymbolTable.EmptySetId;
  EmptyStringId := LocalSymbolTable.EmptyStringId;
  LastShiftValue := LocalSymbolTable.LastShiftValue;
  LastClassIndex := LocalSymbolTable.LastClassIndex;

//  GlobalST_LastShiftValue := GlobalST.LastShiftValue;

  FreeAndNil(HashArray);
  HashArray := LocalSymbolTable.HashArray.Clone;

  FreeAndNil(GuidList);
  GuidList := LocalSymbolTable.GuidList.Clone;

  FreeAndNil(SomeTypeList);
  SomeTypeList := LocalSymbolTable.SomeTypeList.Clone;

//  ExternList.Free;
//  ExternList := GlobalST.ExternList.Clone;

 // CompileCard := Card;
end;

function TExtraSymbolTable.GetRecord(I: Integer): TSymbolRec;
begin
  if I <= LocalSymbolTable.Card then
    result := LocalSymbolTable[I]
  else
    result := TSymbolRec(A[I - LocalSymbolTable.Card - 1]);
end;

end.
