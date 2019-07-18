////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_EVAL.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$R-}
{$O-}
unit PAXCOMP_EVAL;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_BYTECODE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_EXTRASYMBOL_TABLE,
  PAXCOMP_KERNEL,
  PAXCOMP_MODULE,
  PAXCOMP_CLASSFACT,
  PAXCOMP_BASERUNNER;

type
  TSaveRec = class
  public
    Id: Integer;
    Shift: Integer;
    Host: Boolean;
    Address: Pointer;
    OverScript: Boolean;
    ClassIndex: Integer;
  end;

  TSaveList = class(TTypedList)
  private
    symbol_table: TSymbolTable;
    function GetRecord(I: Integer): TSaveRec;
  public
    function AddRec(id: Integer): TSaveRec;
    procedure Save;
    procedure Restore;
    constructor Create(st: TSymbolTable);
    property Records[I: Integer]: TSaveRec read GetRecord; default;
  end;

  TEval = class
  private
    SN: Integer;
    PascalParser: Pointer;
    CurrSubId: Integer;
    ECode_InitCard: Integer;
    HostState: TMemoryStream;
    SaveList: TSaveList;
    ContextStack: TIntegerStack;
    HostState_Position: Integer;
    IsPaused: Boolean;
    function GetPAX64: Boolean;
    procedure RemoveSubs;
    procedure CreateECode;
    procedure CreateESymbolTable;
    procedure SaveHostState;
    procedure RestoreHostState;
    procedure ClearResult;
  public
    ResultId: Integer;
    NamespaceList: TStringList;
    SKernel, EKernel: TKernel;
    SProg, EProg: TBaseRunner;
    RunnerClass: TBaseRunnerClass;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Init(kernel: TKernel; prog: TBaseRunner; N: Integer);
    procedure CompileExpression(const Source: String);
    procedure CompileProgram(const Source: String);
    procedure Run;
    function Valid: Boolean;
    function GetResultAsVariant: Variant;
    function GetResultAsString: String;
    function GetResultAddress: Pointer;
    function GetResultTypeName: String;
    function GetResultTypeId: Integer;
    function HasErrors: Boolean;
    function GetErrorCount: Integer;
    function GetErrorMessage(I: Integer): String;
    procedure MapAddresses;
    property PAX64: Boolean read GetPAX64;
  end;

implementation

uses
  PaxRunner,
  PAXCOMP_STDLIB,
  PAXCOMP_PASCAL_PARSER;

constructor TSaveList.Create(st: TSymbolTable);
begin
  inherited Create;
  symbol_table := st;
end;

function TSaveList.AddRec(id: Integer): TSaveRec;
var
  R: TSymbolRec;
begin
  R := symbol_table[id];
  result := TSaveRec.Create;
  result.Id := R.id;
  result.Shift := R.Shift;
  result.Host := R.Host;
  result.Address := R.Address;
  result.OverScript := R.OverScript;
  result.ClassIndex := R.ClassIndex;
  L.Add(result);
end;

procedure TSaveList.Save;
var
  I: Integer;
begin
  for I := symbol_table.CompileCard + 1 to symbol_table.Card do
    AddRec(I);
end;

procedure TSaveList.Restore;
var
  I: Integer;
  R: TSymbolRec;
begin
  for I := 0 to Count - 1 do
  begin
    R := symbol_table[Records[I].Id];
    with Records[I] do
    begin
      R.Shift := Shift;
      R.Host := Host;
      R.OverScript := OverScript;
      R.ClassIndex := ClassIndex;
    end;
  end;
end;

function TSaveList.GetRecord(I: Integer): TSaveRec;
begin
  result := TSaveRec(L[I]);
end;

constructor TEval.Create;
begin
  inherited;

  SKernel := nil;
  SProg := nil;
  EKernel := nil;
  EProg := nil;

  PascalParser := TPascalParser.Create;
  HostState := TMemoryStream.Create;
  NamespaceList := TStringList.Create;

  SaveList := nil;

  ContextStack := TIntegerStack.Create;
end;

destructor TEval.Destroy;
begin
  ClearResult;

  NamespaceList.Free;

  if ekernel <> nil then
    EKernel.MessageList := nil;

  if eprog <> nil then
    EProg.ProgClassFactory := nil;

  if ekernel <> nil then
    EKernel.Free;

  if eprog <> nil then
    EProg.Free;
  TPascalParser(PascalParser).Free;
  HostState.Free;

  ContextStack.Free;

  if SaveList <> nil then
  begin
    SaveList.Restore;
    SaveList.Free;
    SaveList := nil;
  end;

  inherited;
end;

procedure TEval.Reset;
begin
  ClearResult;

  NamespaceList.Clear;

  ContextStack.Clear;

  if SaveList <> nil then
  begin
    SaveList.Restore;
    SaveList.Free;
    SaveList := nil;
  end;

  SKernel := nil;
  SProg := nil;
  EKernel.Reset;
end;

procedure TEval.Init(kernel: TKernel; prog: TBaseRunner; N: Integer);
begin
  RunnerClass := TBaseRunnerClass(TObject(prog).ClassType);

  IsPaused := prog.IsPaused;

  skernel := kernel;
  sprog := prog;
  EKernel := TKernel.Create(nil);
  EKernel.SignCompression := false;
  EKernel.PAX64 := skernel.PAX64;

  EKernel.MessageList := SProg.MessageList;

  EProg := RunnerClass.Create;
  EProg.PAX64 := skernel.PAX64;

  EProg.ProgClassFactory.Free;
  EProg.ProgClassFactory := SProg.ProgClassFactory;

  SN := N;
  if SN <= 0 then
    SN := skernel.Code.Card;

  CurrSubId := skernel.Code.GetCurrSubId(SN);

  CreateECode;
  CreateESymbolTable;
  TKernel(ekernel).RegisterParser(PascalParser);

  ResultId := ekernel.SymbolTable.Card + 3;
end;

function TEval.GetResultAddress: Pointer;
var
  StackFrameNumber: Integer;
begin
  StackFrameNumber := 0;
  if sprog.GetCallStackCount > 0 then
    StackFrameNumber := sprog.GetCallStackCount - 1;
  result := EKernel.SymbolTable.GetFinalAddress(EProg, StackFrameNumber, ResultId);
end;

function TEval.GetResultTypeId: Integer;
begin
  result := EKernel.SymbolTable[ResultId].TypeId;
end;

function TEval.GetResultTypeName: String;
var
  TypeId: Integer;
begin
  TypeId := EKernel.SymbolTable[ResultId].TypeId;
  result := EKernel.SymbolTable[TypeId].Name;
end;

function TEval.GetResultAsString: String;
var
  Address: Pointer;
  TypeId: Integer;
  StackFrameNumber: Integer;
begin
  StackFrameNumber := 0;
  if sprog.GetCallStackCount > 0 then
    StackFrameNumber := sprog.GetCallStackCount - 1;
  Address := EKernel.SymbolTable.GetFinalAddress(EProg,
    StackFrameNumber, ResultId);
  TypeId := EKernel.SymbolTable[ResultId].TypeId;
  result := EKernel.SymbolTable.GetStrVal(Address, TypeId);
end;

function TEval.GetResultAsVariant: Variant;
var
  Address: Pointer;
  TypeId: Integer;
  StackFrameNumber: Integer;
begin
  StackFrameNumber := 0;
  if sprog.GetCallStackCount > 0 then
    StackFrameNumber := sprog.GetCallStackCount - 1;
  Address := EKernel.SymbolTable.GetFinalAddress(EProg, StackFrameNumber, ResultId);
  TypeId := EKernel.SymbolTable[ResultId].TypeId;
  result := EKernel.SymbolTable.GetVariantVal(Address, TypeId);
end;

function TEval.Valid: Boolean;
begin
  result := (SKernel <> nil) and (SProg <> nil);
end;

procedure TEval.CompileExpression(const Source: String);
var
  M: TModule;
  temp_kernel, temp_runner: Pointer;
begin
  with ekernel do
  begin
    ClassFactory := SProg.ProgClassFactory;

    M := AddModule('$', 'Pascal');
    AddCode('$', Source);
    ParseModule(M, 0, wpEvalExpression, Self);

    if HasError then Exit;
    code.RemoveEvalOp;

    if HasError then Exit;
    modules.CreateLoadOrder;
    if HasError then Exit;

    code.CheckTypes;
    if HasError then Exit;

    SymbolTable.LinkCard := SymbolTable.CompileCard;

    SymbolTable.SetShifts(nil);

    if HasError then Exit;
    code.ProcessSizeOf;
    if HasError then Exit;
    code.ChangeOrderOfActualParams;

    if HasError then Exit;
    code.DestroyExpressionTempVars(ResultId);

    if HasError then Exit;
    code.AssignShifts;

    RemoveSubs;

    if HasError then Exit;
    code.Optimization;

  end;

  MapAddresses;

  temp_kernel := CurrKernel;
  temp_runner := CurrProg;
  CurrKernel := ekernel;
  CurrProg := nil;

//  Dump_all('', ekernel, nil, nil);

  try
    TPaxRunner(sprog.Owner).EmitProc(ekernel, eprog, ContextStack);
  finally
    CurrKernel := temp_kernel;
    CurrProg := temp_runner;
  end;
end;

procedure TEval.CompileProgram(const Source: String);
var
  M: TModule;
begin
  with ekernel do
  begin
    ClassFactory := SProg.ProgClassFactory;

    M := AddModule('$', 'Pascal');
    AddCode('$', Source);
    ParseModule(M, 0, wpProgram, Self);

    if HasError then Exit;
    code.RemoveEvalOp;

    if HasError then Exit;
    modules.CreateLoadOrder;
    if HasError then Exit;

    code.CheckTypes;
    if HasError then Exit;

    SymbolTable.SetShifts(nil);

    if HasError then Exit;
    code.ProcessSizeOf;
    if HasError then Exit;
    code.ChangeOrderOfActualParams;

    if HasError then Exit;
    code.InsertDynamicTypeDestructors;

    if HasError then Exit;
    code.AssignShifts;

    RemoveSubs;

    if HasError then Exit;
    code.Optimization;
  end;
  MapAddresses;

  TPaxRunner(sprog.Owner).EmitProc(ekernel, eprog, ContextStack);
end;

procedure TEval.SaveHostState;
begin
  HostState_Position := HostState.Position;
  SProg.SaveState(HostState);
end;

procedure TEval.RestoreHostState;
begin
  HostState.Position := HostState_Position;
  SProg.LoadState(HostState);
end;

procedure TEval.Run;
begin
  SaveHostState;
  try
    SProg.RemovePause;
    EProg.Run;
    if IsPaused then
      SProg.Pause;
  finally
    RestoreHostState;
  end;
end;

procedure TEval.CreateESymbolTable;

var
  SSymbolTable: TSymbolTable;
  SCode: TCode;

  L1, L2: TIntegerList;

procedure SearchSubstitute(ParamId: Integer);
var
  R: TSymbolRec;
  I: Integer;
  CR: TCodeRec;
begin
  R := SSymbolTable[ParamId];
  if R.ByRef then
    Exit;
  if R.IsConst then
    Exit;
  if not (R.FinalTypeId in StringTypes) then
    Exit;
  for I := 1 to SCode.Card do
  begin
    CR := SCode[I];
    if CR.Op = OP_DECLARE_TEMP_VAR then
      if CR.Res = ParamId then
      begin
        L1.Add(ParamId);
        L2.Add(CR.Arg2);
      end;
  end;
end;

function InSkope(R: TSymbolRec; SubId: Integer): Boolean;
begin
  result := false;

  if SubId = 0 then
    Exit;

  if R.Level = 0 then
    Exit;

  repeat

    result := R.Level = SubId;
    if result then
       Exit;

    SubId := SSymbolTable[SubId].Level;

    if not (SSymbolTable[SubId].Kind in KindSUBS) then
      Exit;

  until false;

end;

var
  I, J, StackFrameNumber: Integer;
  R: TSymbolRec;
  S: Integer;
begin
  ekernel.SymbolTable.Free;
  ekernel.SymbolTable := TExtraSymbolTable.Create(ekernel,
    skernel.SymbolTable);
  ekernel.SymbolTable.Reset;

  StackFrameNumber := 0;
  if sprog.GetCallStackCount > 0 then
    StackFrameNumber := sprog.GetCallStackCount - 1;

  SSymbolTable := skernel.SymbolTable;
  SCode := skernel.Code;

  S := SSymbolTable.GetDataSize(SSymbolTable.CompileCard);

  if SaveList = nil then
  begin
    SaveList := TSaveList.Create(SSymbolTable);
    SaveList.Save;
  end;

  L1 := TIntegerList.Create;
  L2 := TIntegerList.Create;

  try

    for I := SSymbolTable.CompileCard + 1 to SSymbolTable.Card do
    begin
      R := SSymbolTable[I];

       if R.Kind in kindSUBs then
       begin
         if R.Host then
           continue;

         J := R.Value;
         R.Host := true;
         R.OverScript := true;
         R.Address := ShiftPointer(TBaseRunner(sprog).CodePtr, J);
         R.Shift := S;
         Inc(S, 4);

         continue;
       end;

      if R.IsGlobalVarEx then
      begin
        if R.Host then
          continue;

         R.Address := SSymbolTable.GetFinalAddress(SProg, StackFrameNumber, I);
         R.Host := true;
         R.OverScript := true;
         R.Shift := S;
         Inc(S, 4);

         continue;
      end;

      if R.OverScript then
      begin
        R.OverScript := false;
        R.Host := false;
        R.Shift := R.SavedShift;
      end
      else
      begin
        R.SavedShift := R.Shift;
      end;

      if R.Param then
      begin
        if not InSkope(R, CurrSubId) then
          continue;

         R.Address := SSymbolTable.GetFinalAddress(SProg, StackFrameNumber, I);
         R.Host := true;
         R.OverScript := true;
         R.Shift := S;
         Inc(S, 4);

         SearchSubstitute(I);

         continue;
       end;

       if R.IsLocalVarEx or (R.Name = '@') then
       begin
         if R.Level <> CurrSubId then
           continue;

         R.Address := SSymbolTable.GetFinalAddress(SProg, StackFrameNumber, I);
         R.Host := true;
         R.OverScript := true;
         R.Shift := S;
         Inc(S, 4);

         continue;
       end;

       if R.Kind = kindVAR then
       begin
         R.Kind := kindNONE;
         R.Shift := 0;
       end
       else if R.Kind = kindCONST then if R.OwnerId = 0 then
       begin
         R.Kind := kindNONE;
         R.Shift := 0;
       end;
    end;

  finally

    for I := 0 to L1.Count - 1 do
    begin
      SSymbolTable[L1[I]].Address := SSymbolTable[L2[I]].Address;
    end;

    L1.Free;
    L2.Free;
  end;
end;

procedure TEval.CreateECode;
var
  I, Op, Id, L: Integer;
  R: TCodeRec;
  SCode, ECode: TCode;
  SR: TSymbolRec;
  SSymbolTable: TSymbolTable;
begin
  SCode := skernel.Code;
  ECode := ekernel.Code;
  SSymbolTable := skernel.SymbolTable;

  ECode.Reset;
  for I := 1 to SN do
  begin
    R := SCode.Records[I];
    Op := R.Op;
    if (Op = OP_BEGIN_MODULE) or (Op = OP_END_MODULE) or
       (Op = OP_BEGIN_USING) or (Op = OP_END_USING) then
    begin
      ECode.Add(Op, R.Arg1, R.Arg2, R.Res, 0,
                    R.Upcase, R.Language, 0,
                    R.LinePos);
    end
    else if (Op = OP_BEGIN_WITH) or (Op = OP_END_WITH) then
    begin
      Id := R.Arg1;
      SR := SCode.GetSymbolRec(Id);
      L := SR.Level;
      if SSymbolTable[L].Kind in kindSUBS then
        if SSymbolTable.GetSelfId(L) = Id then
          ECode.Add(Op, R.Arg1, R.Arg2, R.Res, 0,
                    R.Upcase, R.Language, 0,
                    R.LinePos);
    end
    else if Op = OP_BEGIN_SUB then
    begin
      ECode.Add(Op, R.Arg1, R.Arg2, R.Res, 0,
                    R.Upcase, R.Language, 0,
                    R.LinePos);
      ContextStack.Push(R.Arg1);
    end
    else if Op = OP_END_SUB then
    begin
      ContextStack.Pop;
      ECode.Add(Op, R.Arg1, R.Arg2, R.Res, 0,
                    R.Upcase, R.Language, 0,
                    R.LinePos);
    end;
  end;

  ECode_InitCard := ECode.Card;
end;

procedure TEval.RemoveSubs;
var
  I: Integer;
  R: TCodeRec;
begin
  for I := 1 to ECode_InitCard do
  begin
    R := ekernel.Code[I];
    if R.Op = OP_BEGIN_SUB then
      R.Op := OP_NOP
    else if R.Op = OP_END_SUB then
      R.Op := OP_NOP
    else if R.Op = OP_BEGIN_MODULE then
      R.Op := OP_NOP
    else if R.Op = OP_END_MODULE then
      R.Op := OP_NOP
    else if R.Op = OP_BEGIN_USING then
      R.Op := OP_NOP
    else if R.Op = OP_END_USING then
      R.Op := OP_NOP
    else if R.Op = OP_BEGIN_WITH then
      R.Op := OP_NOP
    else if R.Op = OP_END_WITH then
      R.Op := OP_NOP;
  end;
end;

function TEval.HasErrors: Boolean;
begin
  result := ekernel.Errors.Count > 0;
end;

function TEval.GetErrorCount: Integer;
begin
  result := ekernel.Errors.Count;
end;

function TEval.GetErrorMessage(I: Integer): String;
begin
  if (I >= 0) and (I < GetErrorCount) then
    result := ekernel.Errors[I].Message
  else
    result := '';
end;

procedure TEval.ClearResult;
var
  T, S: Integer;
  Address: Pointer;
  ArrayTypeId, ElTypeId, ElSize: Integer;
begin
  if ekernel = nil then
    Exit;
  if HasErrors then
     Exit;

  if not ((ResultId > 0) and (ResultId <= ekernel.SymbolTable.Card)) then
    Exit;

  if ekernel.SymbolTable[ResultId].TypeId = 0 then
    Exit;

  T := ekernel.SymbolTable[ResultId].FinalTypeId;
  S := ekernel.SymbolTable[ResultId].Shift;
  Address := eprog.GetAddress(S);
  case T of
{$IFNDEF PAXARM}
    typeANSISTRING: AnsiString(Address^) := '';
    typeWIDESTRING: WideString(Address^) := '';
{$ENDIF}
    typeUNICSTRING: UnicString(Address^) := '';
    typeVARIANT, typeOLEVARIANT: VarClear(Variant(Address^));
    typeINTERFACE: IUnknown(Address^) := nil;
    typeDYNARRAY:
    begin
      ArrayTypeId := ekernel.SymbolTable[ResultId].TerminalTypeId;
      ElTypeId := ekernel.SymbolTable[ArrayTypeId].PatternId;
      ElSize := ekernel.SymbolTable[ElTypeId].Size;
       _DynarrayClr(Address, T, ArrayTypeId, ElSize, 0, 0, 0);
    end;
  end;
end;

procedure TEval.MapAddresses;
var
  SymbolTable: TSymbolTable;
  Code: TCode;
  I, J, Level, K1, K2, Id: Integer;
  R: TSymbolRec;
  S: String;
  b1, b2: Boolean;
  L: TIntegerList;
  CodeRec: TCodeRec;
begin
  b1 := Assigned(SProg.OnMapTableVarAddress);
  b2 := Assigned(SProg.OnMapTableProcAddress);

  if (not b1) and (not b2) then
    Exit;

  SymbolTable := ekernel.SymbolTable;
  Code := ekernel.Code;
  K1 := StdCard + 1;
  K2 := SymbolTable.GlobalST.Card;

  L := TIntegerList.Create;
  try
    for I := 1 to Code.Card do
    begin
      CodeRec := Code[I];
      if CodeRec.OP = OP_SET_CODE_LINE then
        continue;
      if CodeRec.OP = OP_SEPARATOR then
        continue;

      Id := CodeRec.Arg1;
      if (Id >= K1) and (Id <= K2) then
        if L.IndexOf(Id) = -1 then
          L.Add(Id);
      Id := CodeRec.Arg2;
      if (Id >= K1) and (Id <= K2) then
        if L.IndexOf(Id) = -1 then
          L.Add(Id);
      Id := CodeRec.Res;
      if (Id >= K1) and (Id <= K2) then
        if L.IndexOf(Id) = -1 then
          L.Add(Id);
    end;

    for J := 0 to L.Count - 1 do
    begin
      I := L[J];

      R := SymbolTable[I];
      if R.Address = nil then
      if R.PatternId = 0 then
      if R.OwnerId = 0 then
      if (not R.Param) then
      case R.Kind of
        KindVAR:
        begin
          if not b1 then
            continue;

          S := R.Name;
          if S <> '' then
            if S <> '@' then
              SProg.OnMapTableVarAddress(SProg.Owner,
                R.FullName, true, R.Address);
        end;
        KindSUB:
        begin
          if not b2 then
            continue;

          Level := R.Level;
          if Level > 0 then
            if SymbolTable[Level].Kind = KindTYPE then
              if SymbolTable[Level].FinalTypeId = typeINTERFACE then
                continue;

          S := R.Name;
          if S <> '' then
            SProg.OnMapTableProcAddress(SProg.Owner,
              R.FullName, R.OverCount, true, R.Address);
          end;
        end;
      end;
  finally
    L.Free;
  end;
end;

function TEval.GetPAX64: Boolean;
begin
  result := ekernel.PAX64;
end;

end.
