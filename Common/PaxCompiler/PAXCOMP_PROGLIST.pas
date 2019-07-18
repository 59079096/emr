////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PROGLIST.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}

unit PAXCOMP_PROGLIST;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_MAP;
type
  TProgRec = class
  public
    FullPath: String;
    Prog: Pointer;
    InitProcessed: Boolean;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream; PCUOwner: Pointer);
    procedure RunInitialization;
    procedure RunFinalization;
  end;

  TProgList = class(TTypedList)
  private
    fOwner: Pointer;
    function GetRecord(I: Integer): TProgRec;
    function AddRecord: TProgRec;
  public
    constructor Create(AOwner: Pointer);
    function LoadAddress(const FileName, ProcName: String;
                         RunInit: Boolean;
                         OverCount: Integer;
                         var MR: TMapRec;
                         var DestProg: Pointer): Pointer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream; PCUOwner: Pointer);
    procedure LoadFromStreamList(S: TStreamList; PCUOwner: Pointer);
    procedure SetPCUOwner(PCUOwner: Pointer);
    procedure RunInitialization;
    procedure RunFinalization;
    procedure Add(Rec: TProgRec);
    function IndexOf(const FullPath: String): Integer;
    procedure RemoveProg(const FullPath: String);
    property Records[I: Integer]: TProgRec read GetRecord; default;
  end;

implementation

uses
  PAXCOMP_BASERUNNER;

destructor TProgRec.Destroy;
begin
  if Prog <> nil then
    TBaseRunner(Prog).Destroy;
  inherited;
end;

procedure TProgRec.RunInitialization;
begin
  if not InitProcessed then
  begin
    TBaseRunner(Prog).RunInitialization;
    InitProcessed := true;
  end;
end;

procedure TProgRec.RunFinalization;
begin
  TBaseRunner(Prog).RunFinalization;
end;

procedure TProgRec.SaveToStream(S: TStream);
begin
  SaveStringToStream(FullPath, S);
  TBaseRunner(Prog).SaveToStream(S);
end;

procedure TProgRec.LoadFromStream(S: TStream; PCUOwner: Pointer);
var
  C: TBaseRunnerClass;
begin
  C := TBaseRunnerClass(TBaseRunner(PCUOwner).ClassType);

  FullPath := LoadStringFromStream(S);
  TBaseRunner(Prog) := C.Create;
  TBaseRunner(Prog).PCUOwner := PCUOwner;
  TBaseRunner(Prog).CopyRootEvents;
  TBaseRunner(Prog).LoadFromStream(S);
end;

// TProgList -------------------------------------------------------------------

constructor TProgList.Create(AOwner: Pointer);
begin
  inherited Create;
  fOwner := AOwner;
end;

function TProgList.GetRecord(I: Integer): TProgRec;
begin
  result := TProgRec(L[I]);
end;

function TProgList.AddRecord: TProgRec;
begin
  result := TProgRec.Create;
  L.Add(result);
end;

procedure TProgList.Add(Rec: TProgRec);
begin
  L.Add(Rec);
end;

procedure TProgList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TProgList.LoadFromStream(S: TStream; PCUOwner: Pointer);
var
  I, K: Integer;
  R: TProgRec;
begin
  S.Read(K, SizeOf(Integer));
  for I := 0 to K - 1 do
  begin
    R := AddRecord;
    R.LoadFromStream(S, PCUOwner);
  end;
end;

procedure TProgList.LoadFromStreamList(S: TStreamList; PCUOwner: Pointer);
var
  I: Integer;
  R: TProgRec;
  C: TBaseRunnerClass;
  FullName: String;
begin
  C := TBaseRunnerClass(TBaseRunner(PCUOwner).ClassType);

  for I := 0 to S.Count - 1 do
  begin
    FullName := S[I].UnitName + '.' + PCU_FILE_EXT;
    if IndexOf(FullName) >= 0 then
      continue;

    R := AddRecord;
    R.FullPath := FullName;
    TBaseRunner(R.Prog) := C.Create;
    TBaseRunner(R.Prog).PCUOwner := PCUOwner;
    TBaseRunner(R.Prog).CopyRootEvents;
    TBaseRunner(R.Prog).LoadFromStream(S[I].Stream);
  end;
end;

procedure TProgList.RunInitialization;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Records[I].RunInitialization;
end;

procedure TProgList.RunFinalization;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Records[I].RunFinalization;
end;

procedure TProgList.SetPCUOwner(PCUOwner: Pointer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TBaseRunner(Records[I].Prog).PCUOwner := PCUOwner;
end;

function TProgList.LoadAddress(const FileName, ProcName: String;
                               RunInit: Boolean;
                               OverCount: Integer;
                               var MR: TMapRec;
                               var DestProg: Pointer): Pointer;
var
  I: Integer;
  Owner, P: TBaseRunner;
  UnitName, FullPath, S: String;
  ProgRec: TProgRec;
  InputStream: TStream;
  C: TBaseRunnerClass;
begin
  Owner := TBaseRunner(fOwner);

  DestProg := nil;

  ProgRec := nil;
  FullPath := '';
  UnitName := ExtractFullOwner(FileName);

  InputStream := nil;

//  if not Owner.FileExists(FileName, FullPath) then
//    Owner.RaiseError(errFileNotFound, [FileName]);

  for I := 0 to Count - 1 do
  begin
    S := ExtractFileName(TProgRec(L[I]).FullPath);
    if StrEql(S, FileName) then
    begin
      ProgRec := TProgRec(L[I]);
      FullPath := ProgRec.FullPath;
      break;
    end;
  end;

  if ProgRec = nil then
  begin
    if Assigned(Owner.OnLoadPCU) then
    begin
      Owner.OnLoadPCU(Owner.Owner, UnitName, InputStream);
    end;

    if not Owner.FileExists(FileName, FullPath) then
      if InputStream = nil then
      begin
        result := nil;
        Owner.RaiseError(errFileNotFound, [FileName]);
        Exit;
      end;

    C := TBaseRunnerClass(Owner.ClassType);

    P := C.Create;
    P.PCUOwner := Owner;
    if InputStream <> nil then
      P.LoadFromStream(InputStream)
    else
      P.LoadFromFile(FullPath);
    ProgRec := TProgRec.Create;
    ProgRec.FullPath := FullPath;
    ProgRec.Prog := P;
    L.Add(ProgRec);

    if RunInit then
      ProgRec.RunInitialization;
  end
  else
  begin
    P := ProgRec.Prog;
    if RunInit then
      ProgRec.RunInitialization;
  end;

  P.CopyRootEvents;
  P.CopyRootBreakpoints(UnitName);

  S := ExtractName(UnitName)+ '.' + ProcName;
  result := P.GetAddressEx(S, OverCount, MR);
  DestProg := P;
end;

function TProgList.IndexOf(const FullPath: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Records[I].FullPath, FullPath) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TProgList.RemoveProg(const FullPath: String);
var
  I: Integer;
begin
  I := IndexOf(FullPath);
  if I >= 0 then
    RemoveAt(I);
end;

end.
