////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_MODULE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_MODULE;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

type
  TModuleState = (msNone, msCompiling, msCompiled);

  TModule = class
  private
    kernel: Pointer;
    UsedModules: TIntegerList;
    TempUsedModules: TIntegerList;
  public
    Name: String;
    LanguageName: String;
    FileName: String;
    Lines: TStringList;
    S1, S2, S3: Integer;
    P1, P2, P3: Integer;
    PInitBegin, PInitEnd: Integer;
    PFinBegin, PFinEnd: Integer;
    ModuleNumber: Integer;
    CancelPos: Integer;
    SkipParsing: Boolean;
    State: TModuleState;
    IsExtra: Boolean;
    IsPCU: Boolean;
    IncludedFiles: TStringList;
    constructor Create(i_kernel: Pointer);
    destructor Destroy; override;
    procedure Recalc;
  end;

  TModuleList = class(TTypedList)
  private
    kernel: Pointer;
    function GetModule(I: Integer): TModule;
  public
    LoadOrder: TIntegerList;
    constructor Create(i_kernel: Pointer);
    destructor Destroy; override;
    function AddModule(const ModuleName, LanguageName: String): TModule;
    function IndexOf(const ModuleName: String): Integer;
    function IndexOfModuleById(Id: Integer): Integer;
    function IsDefinedInPCU(Id: Integer): Boolean;
    function GetPos(const ModuleName: String; X, Y: Integer): Integer;
    procedure Recalc;
    procedure CreateLoadOrder;
    procedure SaveScript(const FileName: String);
    procedure CreateError(const Message: string; params: array of Const);
    procedure RaiseError(const Message: string; params: array of Const);
    procedure Delete(M: TModule);
    property Modules[I: Integer]: TModule read GetModule; default;
  end;

implementation

uses
  PAXCOMP_BYTECODE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_KERNEL;

constructor TModule.Create(i_kernel: Pointer);
begin
  inherited Create;
  Self.kernel := i_kernel;
  Lines := TStringList.Create;
  UsedModules := TIntegerList.Create;
  TempUsedModules := TIntegerList.Create;
  IncludedFiles := TStringList.Create;

  S1 := 0;
  S2 := 0;
  S3 := 0;
  P1 := 0;
  P2 := 0;
  P3 := 0;
  PInitBegin := 0;
  PInitEnd := 0;
  PFinBegin := 0;
  PFinEnd := 0;

  CancelPos := -1;
end;

destructor TModule.Destroy;
begin
  FreeAndNil(Lines);
  FreeAndNil(UsedModules);
  FreeAndNil(TempUsedModules);
  FreeAndNil(IncludedFiles);

  inherited;
end;

procedure TModule.Recalc;
var
  I: Integer;
begin
  for I:=1 to TKernel(kernel).Code.Card do
    with TKernel(kernel).Code[I] do
      if (Op = OP_BEGIN_MODULE) and (Arg1 = ModuleNumber) then
        P1 := I
      else if (Op = OP_END_INTERFACE_SECTION) and (Arg1 = ModuleNumber) then
        P2 := I
      else if (Op = OP_END_MODULE) and (Arg1 = ModuleNumber) then
        P3 := I
      else if (Op = OP_BEGIN_INITIALIZATION) and (Arg1 = ModuleNumber) then
        PInitBegin := I
      else if (Op = OP_END_INITIALIZATION) and (Arg1 = ModuleNumber) then
        PInitEnd := I
      else if (Op = OP_BEGIN_FINALIZATION) and (Arg1 = ModuleNumber) then
        PFinBegin := I
      else if (Op = OP_END_FINALIZATION) and (Arg1 = ModuleNumber) then
        PFinEnd := I;
end;

constructor TModuleList.Create(i_kernel: Pointer);
begin
  inherited Create;
  Self.kernel := i_kernel;
  LoadOrder := TIntegerList.Create;
end;

destructor TModuleList.Destroy;
begin
  FreeAndNil(LoadOrder);
  inherited;
end;

function TModuleList.AddModule(const ModuleName, LanguageName: String): TModule;
begin
  result := TModule.Create(kernel);
  result.Name := ModuleName;
  result.LanguageName := LanguageName;
  L.Add(result);
end;

procedure TModuleList.Delete(M: TModule);
var
  I: Integer;
begin
  for I:=Count - 1 downto 0 do
    if Modules[I] = M then
    begin
      L.Delete(I);
      FreeAndNil(M);
    end;
end;

function TModuleList.GetPos(const ModuleName: String; X, Y: Integer): Integer;
var
 S: String;
 I, L, CurrX, CurrY: Integer;
 ch: Char;
begin
 TKernel(kernel).CompletionPrefix := '';
 result := -1;
 I := IndexOf(ModuleName);
 if I = -1 then
   Exit;
 S := Modules[I].Lines.Text + #255;
 CurrX := -1;
 CurrY := 0;

 I := SLow(S) - 1;
 L := SHigh(S);
 while I < L do
 begin
   Inc(I);
   Inc(CurrX);

   if (CurrX = X) and (CurrY = Y) then
   begin
     if TKernel(kernel).FindDeclId < 0 then
     begin
       result := I;
       Exit;
     end;

     if ByteInSet(S[I], IdsSet + WhiteSpaces + [Ord('!'), Ord('.'),
          Ord('('), Ord(')'), Ord(';'), Ord(',')]) then
     begin
       if ByteInSet(S[I], WhiteSpaces + [Ord(';'), Ord(')'), Ord(',')]) and
          (ByteInSet(S[I - 1], IdsSet + [Ord('.')])) then
       begin
         Dec(i);
       end;
       if IsAlpha(S[I]) then
       begin
         while ByteInSet(S[I], IdsSet) do
         begin
           TKernel(kernel).CompletionPrefix := S[I] + TKernel(kernel).CompletionPrefix;

           Dec(I);
           if I = 0 then
             Exit;
         end;
       end;

       result := I;
       Exit;
     end;
   end;

   ch := S[I];
   if ByteInSet(ch, [10, 13]) then
   begin
     Inc(CurrY);
     if S[I + 1] = #10 then
       Inc(I);
     CurrX := -1;
   end;

   if (CurrX = X) and (CurrY = Y) then
   begin
     if ByteInSet(S[I], IdsSet + WhiteSpaces + [Ord('.'), Ord('('), Ord(')'),
               Ord(';'), Ord(',')]) then
     begin
       if ByteInSet(S[I], WhiteSpaces + [Ord(';'), Ord(')'), Ord(',')]) and
          (ByteInSet(S[I - 1], IdsSet + [Ord('.')])) then
       begin
         Dec(i);
       end;
       if IsAlpha(S[I]) then
       begin
         while ByteInSet(S[I], IdsSet) do
         begin
           TKernel(kernel).CompletionPrefix := S[I] + TKernel(kernel).CompletionPrefix;

           Dec(I);
           if I = 0 then
             Exit;
         end;
       end;

       result := I;
       Exit;
     end;
   end;
 end;
end;

function TModuleList.IndexOf(const ModuleName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(Modules[I].Name, ModuleName) then
    begin
      result := I;
      Exit;
    end;
end;

function TModuleList.GetModule(I: Integer): TModule;
begin
  result := TModule(L[I]);
end;

function TModuleList.IndexOfModuleById(Id: Integer): Integer;
var
  I: Integer;
  M: TModule;
begin
  for I:=0 to Count - 1 do
  begin
    M := Modules[I];
    if (Id >= M.S1) and (Id <= M.S3) then
    begin
      result := I;
      Exit;
    end;
  end;
  result := -1;
end;

function TModuleList.IsDefinedInPCU(Id: Integer): Boolean;
var
  I: Integer;
begin
  result := false;
  I := IndexOfModuleById(Id);
  if I = -1 then
    Exit;
  result := Modules[I].IsPCU;
end;

procedure TModuleList.Recalc;
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    Modules[I].Recalc;
end;

procedure TModuleList.CreateLoadOrder;
var
  I, J: Integer;
  M: TModule;
  Code: TCode;
  SymbolTable: TSymbolTable;
  Id: Integer;
  ModuleIndex: Integer;
  ok: Boolean;
  CurrModuleName: String;
begin
  if TKernel(kernel).SignCodeCompletion then
  begin
    LoadOrder.Clear;
    LoadOrder.Add(0);
    Exit;
  end;

  Code := TKernel(kernel).Code;
  SymbolTable := TKernel(kernel).SymbolTable;

  Recalc;

  for I:=0 to Count - 1 do
  begin
    M := Modules[I];
    M.UsedModules.Clear;
    M.TempUsedModules.Clear;

    CurrModuleName := M.Name;

    for J := M.P1 to M.P2 do
    begin
      if Code[J].Op = OP_END_IMPORT then
        break;

      if Code[J].Op = OP_BEGIN_USING then
      begin
        Id := Code[J].Arg1;
        if SymbolTable[Id].Host then
          continue;
        if Id = 0 then
          continue;
        if StrEql(ExtractName(SymbolTable[Id].Name), ExtractName(CurrModuleName)) then
          continue;
        ModuleIndex := IndexOfModuleById(Id);
        if ModuleIndex = -1 then
          RaiseError(errInternalError, []);
        M.UsedModules.Add(ModuleIndex);
        M.TempUsedModules.Add(ModuleIndex);
      end;
    end;
  end;

  LoadOrder.Clear;

  if Count = 0 then
    Exit;

  if Count = 1 then
  begin
    LoadOrder.Add(0);
    Exit;
  end;

  if TKernel(kernel).InterfaceOnly then
  begin
    for I:=0 to Count - 1 do
      LoadOrder.Add(I);
    Exit;
  end;

  repeat
    ok := false;

    for I:=0 to Count - 1 do
    begin
      if LoadOrder.IndexOf(I) >= 0 then
        continue;

      M := Modules[I];
      if M.TempUsedModules.Count = 0 then
      begin
        ok := true;
        LoadOrder.Add(I);
        if LoadOrder.Count = Count then
          Exit;
        for J:=0 to Count - 1 do
          Modules[J].TempUsedModules.DeleteValue(I);
        break;
      end;
    end;

    if not ok then
      for I:=0 to Count - 1 do
        if LoadOrder.IndexOf(I) = -1 then
        begin
          CreateError(errCircularUnitReference, [Modules[I].Name]);
          Exit;
        end;

  until false;
end;

procedure TModuleList.CreateError(const Message: string; params: array of Const);
begin
  TKernel(kernel).CreateError(Message, params);
end;

procedure TModuleList.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).RaiseError(Message, params);
end;

procedure TModuleList.SaveScript(const FileName: String);
var
  I: Integer;
  S: String;
  Lst: TStringList;
begin
  if not IsDump then
    Exit;
  S := '';
  for I:=0 to Count - 1 do
    S := S + Modules[I].Lines.Text;
  Lst := TStringList.Create;
  try
    Lst.Text := S;
    Lst.SaveToFile(FileName);
  finally
    FreeAndNil(Lst);
  end;
end;

end.
