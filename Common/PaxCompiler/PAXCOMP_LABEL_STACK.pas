////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_LABEL_STACK.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_LABEL_STACK;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;
type

  TEntryRec = class
  public
    IntLabel: Integer;
    StringLabel: String;
    CodeN: Integer;
    loopLabel: Integer;
  end;

  TEntryStack = class(TTypedList)
  private
    kernel: Pointer;
    function GetRecord(I: Integer): TEntryRec;
    function GetTop: TEntryRec;
  public
    procedure SetKernel(AKernel: Pointer);
    procedure Push(AIntLabel, ALoopLabel: Integer); overload;
    procedure Push(AIntLabel: Integer;
                   var AStringLabel: String;
                   ALoopLabel: Integer); overload;
    procedure Pop;
    function TopLabel(const AStringLabel: String = ''): Integer;
    property Top: TEntryRec read GetTop;
    property Records[I: Integer]: TEntryRec read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_BYTECODE,
  PAXCOMP_KERNEL;

procedure TEntryStack.SetKernel(AKernel: Pointer);
begin
  kernel := AKernel;
end;

function TEntryStack.GetRecord(I: Integer): TEntryRec;
begin
  result := TEntryRec(L[I]);
end;

procedure TEntryStack.Push(AIntLabel, ALoopLabel: Integer);
var
  R: TEntryRec;
begin
  R := TEntryRec.Create;
  R.IntLabel := AIntLabel;
  R.StringLabel := '';
  R.loopLabel := ALoopLabel;
  if kernel <> nil then
    R.CodeN := TKernel(kernel).Code.Card;
  L.Add(R);
end;

procedure TEntryStack.Push(AIntLabel: Integer; var AStringLabel: String; ALoopLabel: Integer);
var
  R: TEntryRec;
begin
  R := TEntryRec.Create;
  R.IntLabel := AIntLabel;
  R.StringLabel := AStringLabel;
  R.loopLabel := ALoopLabel;
  if kernel <> nil then
    R.CodeN := TKernel(kernel).Code.Card;
  L.Add(R);
  AStringLabel := '';
end;

procedure TEntryStack.Pop;
begin
{$IFDEF ARC}
  L[Count - 1] := nil;
{$ELSE}
  Records[Count - 1].Free;
{$ENDIF}
  L.Delete(Count - 1);
end;

function TEntryStack.TopLabel(const AStringLabel: String = ''): Integer;
var
  I: Integer;
  R: TEntryRec;
begin
  if AStringLabel <> '' then
  begin
    for I:=Count - 1 downto 0 do
    begin
      R := Records[I];
      with R do
        if StringLabel = AStringLabel then
        begin
          result := IntLabel;
          Exit;
        end;
    end;
    raise Exception.Create(errLabelNotFound);
  end
  else
    result := Records[Count - 1].IntLabel;
end;

function TEntryStack.GetTop: TEntryRec;
begin
  if Count = 0 then
    result := nil
  else
    result := Records[Count - 1];
end;

end.
