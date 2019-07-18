////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_ERROR.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
unit PAXCOMP_ERROR;
interface
uses {$I uses.def}
  PAXCOMP_TYPES,
  SysUtils,
  Classes;

type
  TError = class
  private
    kernel: Pointer;
    fMessage: String;
    fPCodeLineNumber: Integer;
    fModuleName: String;
    fSourceLineNumber: Integer;
    fSourceLine: String;
    fLinePos: Integer;
    fSourceFileName: String;
  public
    constructor Create(i_kernel: Pointer; const i_Message: String); overload;
    constructor Create(i_kernel: Pointer); overload;
    function Clone(i_kernel: Pointer): TError;
    property Message: String read fMessage;
    property PCodeLineNumber: Integer read fPCodeLineNumber;
    property ModuleName: String read fModuleName;
    property SourceLine: String read fSourceLine;
    property SourceLineNumber: Integer read fSourceLineNumber;
    property LinePos: Integer read fLinePos;
    property SourceFileName: String read fSourceFileName;
  end;

  TErrorList = class(TTypedList)
  private
    kernel: Pointer;
    function GetRecord(I: Integer): TError;
  public
    constructor Create(i_kernel: Pointer);
    procedure Reset;
    procedure Add(E: TError);
    function IndexOf(E: TError): Integer;
    property Records[I: Integer]: TError read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_KERNEL, PAXCOMP_PARSER, PAXCOMP_MODULE;

constructor TError.Create(i_kernel: Pointer);
begin
  inherited Create;
  kernel := i_kernel;
end;

constructor TError.Create(i_kernel: Pointer; const i_Message: String);
var
  M: TModule;
begin
  inherited Create;
  fMessage := i_Message;
  kernel := i_kernel;
  fPCodeLineNumber := TKernel(kernel).Code.N;
  if (fPCodeLineNumber < 1) or (fPCodeLineNumber > TKernel(kernel).Code.Card) then
    fPCodeLineNumber := TKernel(kernel).Code.Card;
  M := TKernel(kernel).Code.GetModule(fPCodeLineNumber);
  if M <> nil then
  begin
    fModuleName := M.Name;
    fSourceLine := TKernel(kernel).Code.GetSourceLine(fPCodeLineNumber);
    fSourceLineNumber := TKernel(kernel).Code.GetSourceLineNumber(fPCodeLineNumber);
    fLinePos := TKernel(kernel).Code.GetLinePos(fPCodeLineNumber);
    fSourceFileName := TKernel(kernel).Code.GetIncludedFileName(fPCodeLineNumber);
    if fSourceFileName = '' then
      fSourceFileName := M.FileName;
  end
  else
  begin
    fModuleName := '';
    fSourceLine := '';
    fSourceLineNumber := 0;
    fLinePos := 0;
  end;
end;

function TError.Clone(i_kernel: Pointer): TError;
begin
  result := TError.Create(i_Kernel);
  result.fMessage := fMessage;
  result.fPCodeLineNumber := fPCodeLineNumber;
  result.fModuleName := fModuleName;
  result.fSourceLineNumber := fSourceLineNumber;
  result.fSourceLine := fSourceLine;
  result.fLinePos := fLinePos;
  result.fSourceFileName := fSourceFileName;
end;

constructor TErrorList.Create(i_kernel: Pointer);
begin
  inherited Create;
  Self.kernel := i_kernel;
end;

procedure TErrorList.Reset;
begin
  Clear;
end;

function TErrorList.GetRecord(I: Integer): TError;
begin
  result := TError(L[I]);
end;

function TErrorList.IndexOf(E: TError): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if Records[I].fSourceLineNumber = E.fSourceLineNumber then
      if Records[I].fMessage = E.fMessage then
      begin
        result := I;
        Exit;
      end;
end;

procedure TErrorList.Add(E: TError);
begin
  if IndexOf(E) = -1 then
    L.Add(E)
  else
    FreeAndNil(E);
end;

end.
