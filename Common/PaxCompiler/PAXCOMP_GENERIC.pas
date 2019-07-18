////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_GENERIC.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_GENERIC;
interface
uses {$I uses.def}
  Classes,
  SysUtils,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS;

type
  TGenericTypeContainer = class;

  TTypeParams = class(TStringObjectList)
  public
    procedure AssTo(L: TTypeParams);
  end;

  TTypeParamsHistory = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeParams;
  public
    function Add(const Value: TTypeParams): TTypeParams;
    property Records[I: Integer]: TTypeParams read GetRecord; default;
  end;

  TTypeRestrictionRec = class
  public
    Id: Integer;
    N: Integer;
    function Clone: TTypeRestrictionRec;
  end;

  TTypeRec = class
  public
    Name: String;
    ParamList: TTypeParams;
    IsExtra: Boolean;
    IsGeneric: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TTypeExpRec = class(TTypeRec);

  TTypeExtRec = class(TTypeRec)
  public
    P1, P2: Integer;
    LangId: Integer;
    Extension: String;
    Valid: Boolean;
    function Substitute(R: TTypeExpRec): String;
  end;

  TTypeExtList = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeExtRec;
    function GetTop: TTypeExtRec;
  public
    function Add: TTypeExtRec;
    property Top: TTypeExtRec read GetTop;
    property Records[I: Integer]: TTypeExtRec read GetRecord; default;
  end;

  TTypeDefRec = class(TTypeRec)
  public
    P1, P2: Integer;
    LangId: Integer;
    TypeId: Integer;
    SubId: Integer;
    Definition: String;
    IsMethodImplementation: Boolean; // Pascal only
    AncestorName: String;
    TypeExtList: TTypeExtList;
    ModuleName: String;
    function Substitute(R: TTypeExpRec): String;
    constructor Create(AModuleName: String; ALangId: Integer);
    destructor Destroy; override;
  end;

  TTypeExpList = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeExpRec;
    function GetTop: TTypeExpRec;
  public
    constructor Create;
    function IndexOf(const TypeName: String;
                     TypeParams: TStrings;
                     Upcase: Boolean): Integer;
    function Add: TTypeExpRec;
    property Top: TTypeExpRec read GetTop;
    property Records[I: Integer]: TTypeExpRec read GetRecord; default;
  end;

  TTypeModuleRec = class
  public
    ModuleName: String;
    LangId: Integer;
    UsingList: TStringList;
    Source: String;
    Success: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TTypeModuleList = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeModuleRec;
  public
    function IndexOf(const ModuleName: String): Integer;
    function AddModule(const ModuleName: String; LangId: Integer): TTypeModuleRec;
    property Records[I: Integer]: TTypeModuleRec read GetRecord; default;
  end;

  TTypeDefList = class(TTypedList)
  private
    procedure RaiseError(const Message: string; params: array of Const);
    function GetRecord(I: Integer): TTypeDefRec;
    function GetTop: TTypeDefRec;
    procedure TryExpansion(const TypeName: String;
                           LangId: Integer;
                           result: TStringList;
                           var Success: Boolean;
                           I, J: Integer);
  public
    TypeExpList: TTypeExpList;
    Expansions: TAssocIntegers;
    RemTypeIds: TIntegerList;
    TypeModuleList: TTypeModuleList;
    CurrTypeModuleRec: TTypeModuleRec;
    constructor Create;
    destructor Destroy; override;
    function IndexOf(const TypeName: String;
                     TypeParams: TStringList;
                     Upcase: Boolean): Integer;
    function Add(const ModuleName: String; LangId: Integer): TTypeDefRec;
    function FindTypeDef(TypeId: Integer): TTypeDefRec;
    function FindMethodDef(SubId: Integer): TTypeDefRec;
    procedure Clear; override;
    procedure ReplaceId(OldId, NewId: Integer);
    procedure GenPascalUnits;
    procedure GenBasicUnits;
    procedure GenJavaUnits;
    procedure CreateConainer(TypeId: Integer; result: TGenericTypeContainer);
    property Top: TTypeDefRec read GetTop;
    property Records[I: Integer]: TTypeDefRec read GetRecord; default;
  end;

  TGenericTypeContainer = class
  public
    Definition: String;
    MethodList: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

implementation

function GetExtraPascalUnitName(const ModuleName: String): String;
begin
  result := strExtraPascalUnit + ExtractName(ModuleName);
end;

function GetExtraBasicUnitName(const ModuleName: String): String;
begin
  result := strExtraBasicUnit + ExtractName(ModuleName);
end;

function GetExtraJavaUnitName(const ModuleName: String): String;
begin
  result := strExtraJavaUnit + ExtractName(ModuleName);
end;

function Substitute(const S: String; P1, P2: TTypeParams): String;
var
  I: Integer;
  Name1, Name2: String;
begin
  if P1.Count <> P2.Count then
    raise Exception.Create(errInternalError);

  result := S;
  for I := 0 to P1.Count - 1 do
  begin
    Name1 := P1[I];
    Name2 := P2[I];
    result := Subst(result, Name1, Name2);
  end;
end;


function TTypeExtRec.Substitute(R: TTypeExpRec): String;
var
  I: Integer;
  Name1, Name2: String;
begin
  if ParamList.Count <> R.ParamList.Count then
    raise Exception.Create(errInternalError);

  result := Extension;
  for I := 0 to ParamList.Count - 1 do
  begin
    Name1 := ParamList[I];
    Name2 := R.ParamList[I];
    result := Subst(result, Name1, Name2);
  end;
end;

// TTypeExtList ----------------------------------------------------------------

function TTypeExtList.GetRecord(I: Integer): TTypeExtRec;
begin
  result := TTypeExtRec(L[I]);
end;

function TTypeExtList.Add: TTypeExtRec;
begin
  result := TTypeExtRec.Create;
  L.Add(result);
end;

function TTypeExtList.GetTop: TTypeExtRec;
begin
  result := Records[Count - 1];
end;

// TTypeRec --------------------------------------------------------------------

constructor TTypeRec.Create;
begin
  inherited;
  ParamList := TTypeParams.Create;
end;

destructor TTypeRec.Destroy;
begin
  ParamList.Free;
  inherited;
end;

function TTypeRestrictionRec.Clone: TTypeRestrictionRec;
begin
  result := TTypeRestrictionRec.Create;
  result.Id := Id;
  result.N := N;
end;

constructor TTypeDefRec.Create(AModuleName: String; ALangId: Integer);
begin
  inherited Create;
  ModuleName := AModuleName;
  LangId := ALangId;
  TypeExtList := TTypeExtList.Create;
end;

destructor TTypeDefRec.Destroy;
begin
  TypeExtList.Free;
  inherited;
end;

function TTypeDefRec.Substitute(R: TTypeExpRec): String;
var
  I: Integer;
  Name1, Name2: String;
begin
  if ParamList.Count <> R.ParamList.Count then
    raise Exception.Create(errInternalError);

  result := Definition;
  for I := 0 to ParamList.Count - 1 do
  begin
    Name1 := ParamList[I];
    Name2 := R.ParamList[I];
    result := Subst(result, Name1, Name2);
  end;
end;

// TTypeExpList ----------------------------------------------------------------

constructor TTypeExpList.Create;
begin
  inherited Create;
end;

function TTypeExpList.GetRecord(I: Integer): TTypeExpRec;
begin
  result := TTypeExpRec(L[I]);
end;

function TTypeExpList.IndexOf(const TypeName: String;
                              TypeParams: TStrings;
                              Upcase: Boolean): Integer;
var
  I, J: Integer;
  R: TTypeExpRec;
  b, b1, b2: Boolean;
  S1, S2: String;
begin
  result := -1;
  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if Upcase then
      b1 := StrEql(R.Name, TypeName)
    else
      b1 := R.Name = TypeName;
    if b1 then
      if R.ParamList.Count = TypeParams.Count then
      begin
        b := true;
        for J := 0 to R.ParamList.Count - 1 do
        begin
          S1 := ExtractName(R.ParamList[J]);
          S2 := ExtractName(TypeParams[J]);
          if Upcase then
            b2 := StrEql(S1, S2)
          else
            b2 := S1 = S2;
          if not b2 then
          begin
            b := false;
            break;
          end;
        end;
        if b then
        begin
          result := I;
          Exit;
        end;
      end;
  end;
end;

function TTypeExpList.Add: TTypeExpRec;
begin
  result := TTypeExpRec.Create;
  L.Add(result);
end;

function TTypeExpList.GetTop: TTypeExpRec;
begin
  result := Records[Count - 1];
end;

// TTypeDefList ----------------------------------------------------------------

constructor TTypeDefList.Create;
begin
  inherited Create;
  TypeExpList := TTypeExpList.Create;
  Expansions := TAssocIntegers.Create;
  RemTypeIds := TIntegerList.Create;
  TypeModuleList := TTypeModuleList.Create;
end;

destructor TTypeDefList.Destroy;
begin
  inherited;
  Expansions.Free;
  TypeExpList.Free;
  RemTypeIds.Free;
  TypeModuleList.Free;
end;

procedure TTypeDefList.Clear;
begin
  Expansions.Clear;
  TypeExpList.Clear;
  RemTypeIds.Clear;
  inherited;
end;

function TTypeDefList.GetRecord(I: Integer): TTypeDefRec;
begin
  result := TTypeDefRec(L[I]);
end;

procedure TTypeDefList.RaiseError(const Message: string; params: array of Const);
begin
  raise PaxCompilerException.Create(Format(Message, params));
end;

function TTypeDefList.IndexOf(const TypeName: String;
                              TypeParams: TStringList;
                              Upcase: Boolean): Integer;
var
  I, J: Integer;
  R: TTypeDefRec;
  b, b1, b2: Boolean;
  S1, S2: String;
begin
  result := -1;
  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if Upcase then
      b1 := StrEql(R.Name, TypeName)
    else
      b1 := R.Name = TypeName;
    if b1 then
      if R.ParamList.Count = TypeParams.Count then
      begin
        b := true;
        for J := 0 to R.ParamList.Count - 1 do
        begin
          S1 := R.ParamList[J];
          S2 := TypeParams[J];
          if Upcase then
            b2 := StrEql(S1, S2)
          else
            b2 := S1 = S2;
          if not b2 then
          begin
            b := false;
            break;
          end;
        end;
        if b then
        begin
          result := I;
          Exit;
        end;
      end;
  end;
end;

function TTypeDefList.Add(const ModuleName: String; LangId: Integer): TTypeDefRec;
var
  I: Integer;
begin
  result := TTypeDefRec.Create(ModuleName, LangId);
  L.Add(result);

  I := TypeModuleList.IndexOf(ModuleName);
  if I = -1 then
    CurrTypeModuleRec := TypeModuleList.AddModule(ModuleName, LangId);
end;

function TTypeDefList.FindTypeDef(TypeId: Integer): TTypeDefRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if Records[I].TypeId = TypeId then
    begin
      result := Records[I];
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TTypeDefList.FindMethodDef(SubId: Integer): TTypeDefRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if Records[I].SubId = SubId then
    begin
      result := Records[I];
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TTypeDefList.GetTop: TTypeDefRec;
begin
  result := Records[Count - 1];
end;

procedure TTypeDefList.ReplaceId(OldId, NewId: Integer);
var
  I, J, K, L: Integer;
  ParamList: TStringObjectList;
  TR: TTypeRestrictionRec;
begin
  for I := 0 to Count - 1 do
  begin
    ParamList := Records[I].ParamList;
    for J := 0 to ParamList.Count - 1 do
    begin
      TR := TTypeRestrictionRec(ParamList.Objects[J]);
      if TR <> nil then
        if TR.Id = OldId then
          TR.Id := NewId;

      for K := 0 to Records[I].TypeExtList.Count - 1 do
      begin
        for L := 0 to Records[I].TypeExtList[K].ParamList.Count - 1 do
        begin
          TR := TTypeRestrictionRec(Records[I].TypeExtList[K].ParamList.Objects[L]);
          if TR <> nil then
            if TR.Id = OldId then
              TR.Id := NewId;
        end;
      end;

    end;
  end;
  for I := 0 to TypeExpList.Count - 1 do
  begin
    ParamList := TypeExpList[I].ParamList;
    for J := 0 to ParamList.Count - 1 do
    begin
      TR := TTypeRestrictionRec(ParamList.Objects[J]);
      if TR <> nil then
        if TR.Id = OldId then
          TR.Id := NewId;
    end;
  end;
end;

procedure TTypeDefList.TryExpansion(const TypeName: String;
                                    LangId: Integer;
                                    result: TStringList;
                                    var Success: Boolean;
                                    I, J: Integer);
var
  RI, RL: TTypeExpRec;
  RJ: TTypeDefRec;
  RK: TTypeExtRec;
  S, Q, S1: String;
  K, L: Integer;
begin
  RI := TypeExpList[I];
  RJ := Records[J];

  if RJ.IsExtra then
    Exit;
  if not RJ.IsGeneric then
    Exit;

  if RJ.LangId = LangId then
  if not RJ.IsMethodImplementation then
  if RI.ParamList.Count = RJ.ParamList.Count then
  if StrEql(TypeName, RJ.Name) then
  begin
    Success := true;

    if RJ.AncestorName <> '' then
    begin
      for K:=0 to Count - 1 do
        if Expansions.IndexOf(I, K) = -1 then
          TryExpansion(RJ.AncestorName,
                       LangId,
                       result,
                       Success,
                       I, K);
    end;

    S := RJ.Substitute(RI);

    if RJ.TypeExtList.Count > 0 then
    begin
      if LangId = PASCAL_LANGUAGE then
      begin
        S1 := Copy(S, 1, Length(S) - 4);

        for K := 0 to RJ.TypeExtList.Count - 1 do
        begin
          RK := RJ.TypeExtList[K];
          for L := 0 to TypeExpList.Count - 1 do
          begin
            RL := TypeExpList[L];
            if RK.ParamList.Count = RL.ParamList.Count then
            if StrEql(RK.Name, RL.Name) then
            begin
              Q := Substitute(RK.Extension, RJ.ParamList, RI.ParamList);
              Q := Substitute(Q, RK.ParamList, RL.ParamList);

              Q := EXTRA_KEYWORD + ' ' + Q;

              S1 := S1 + Q + #13#10;
            end;
          end;
        end;

        S := S1 + 'end;';
      end;
    end;

    result.Add(S);

    Expansions.Add(I, J);
  end;
end;

procedure TTypeDefList.CreateConainer(TypeId: Integer; result: TGenericTypeContainer);
var
  I: Integer;
  R: TTypeDefRec;
begin
  for I := 0 to Count - 1 do
  begin
    R := Records[I];
    if not R.IsGeneric then
      continue;

    if R.TypeId <> TypeId then
      continue;
    if R.IsMethodImplementation then
      result.MethodList.Add(R.Definition)
    else
      result.Definition := R.Definition;
  end;
end;

procedure TTypeDefList.GenPascalUnits;

  function GenUnit(TypeModuleRec: TTypeModuleRec): TStringList;
  var
    I, J, K, L: Integer;
    RI, RL: TTypeExpRec;
    RJ: TTypeDefRec;
    RK: TTypeExtRec;
    S, S1, Q, ModuleName: String;
  begin
    TypeModuleRec.Success := false;
    ModuleName := TypeModuleRec.ModuleName;

    result := TStringList.Create;
    result.Add('unit ' + GetExtraPascalUnitName(ModuleName) + ';');
    result.Add('interface');

    if TypeModuleRec.UsingList.Count > 0 then
    begin
      result.Add('uses ');
      for I := 0 to TypeModuleRec.UsingList.Count - 1 do
      begin
        S := TypeModuleRec.UsingList[I];
        if I = TypeModuleRec.UsingList.Count - 1 then
          S := S + ';'
        else
          S := S + ',';
        result.Add(S);
      end;
    end;

    result.Add('type');

    for I := 0 to TypeExpList.Count - 1 do
    begin
      RI := TypeExpList[I];

      if IndexOf(RI.Name, RI.ParamList, true) >= 0 then
        continue;

      for J:= 0 to Count - 1 do
      begin
        RJ := Records[J];
        if not StrEql(ModuleName, RJ.ModuleName) then
          continue;

        if Expansions.IndexOf(I, J) = -1 then
          TryExpansion(RI.Name, PASCAL_LANGUAGE, result, TypeModuleRec.Success, I, J);
      end;
    end;

    for J := 0 to Count - 1 do
    begin
      RJ := Records[J];
      if RJ.LangId = PASCAL_LANGUAGE then
      if RJ.IsGeneric and (RJ.ParamList.Count = 0) then
      if not RJ.IsMethodImplementation then
      begin
        if RJ.TypeExtList.Count = 0 then
          RaiseError(errInternalError, []);

        S := RJ.Definition;
        S1 := Copy(S, 1, Length(S) - 4);

        for K := 0 to RJ.TypeExtList.Count - 1 do
        begin
          RK := RJ.TypeExtList[K];
          for L := 0 to TypeExpList.Count - 1 do
          begin
            RL := TypeExpList[L];
            if RK.ParamList.Count = RL.ParamList.Count then
            if StrEql(RK.Name, RL.Name) then
            begin
              Q := Substitute(RK.Extension, RK.ParamList, RL.ParamList);

              Q := EXTRA_KEYWORD + ' ' + Q;

              S1 := S1 + Q + #13#10;
            end;
          end;
        end;

        S := S1 + 'end;';

        result.Add(S);

        TypeModuleRec.Success := true;
      end;
    end;

    result.Add('implementation');

    for I := 0 to TypeExpList.Count - 1 do
    begin
      RI := TypeExpList[I];
      if IndexOf(RI.Name, RI.ParamList, true) >= 0 then
        continue;

      for J:= 0 to Count - 1 do
      if Expansions.IndexOf(I, J) = -1 then
      begin
        RJ := Records[J];

        if RJ.IsExtra then
          continue;
        if not RJ.IsGeneric then
          continue;

        if RJ.LangId = PASCAL_LANGUAGE then
        if RJ.IsMethodImplementation then
        if RI.ParamList.Count = RJ.ParamList.Count then
        if StrEql(RI.Name, RJ.Name) then
        begin
          TypeModuleRec.Success := true;
          S := RJ.Substitute(RI);

          result.Add(S);

          for K := 0 to RJ.TypeExtList.Count - 1 do
          begin
            RK := RJ.TypeExtList[K];
            for L := 0 to TypeExpList.Count - 1 do
            begin
              RL := TypeExpList[L];
              if RK.ParamList.Count = RL.ParamList.Count then
              if StrEql(RK.Name, RL.Name) then
              begin
                S := Substitute(RK.Extension, RJ.ParamList, RI.ParamList);
                S := Substitute(S, RK.ParamList, RL.ParamList);
                result.Add(S);
              end;
            end;
          end;

          Expansions.Add(I, J);
        end;
      end;
    end;

    for J := 0 to Count - 1 do
    begin
      RJ := Records[J];
      if RJ.LangId = PASCAL_LANGUAGE then
      if RJ.IsGeneric and (RJ.ParamList.Count = 0) then
      if RJ.IsMethodImplementation then
      begin
        TypeModuleRec.Success := true;

        S := RJ.Definition;
        result.Add(S);
        for K := 0 to RJ.TypeExtList.Count - 1 do
        begin
          RK := RJ.TypeExtList[K];
          for L := 0 to TypeExpList.Count - 1 do
          begin
            RL := TypeExpList[L];
            if RK.ParamList.Count = RL.ParamList.Count then
            if StrEql(RK.Name, RL.Name) then
            begin
              S := RK.Extension;
              S := Substitute(S, RK.ParamList, RL.ParamList);
              result.Add(S);
            end;
          end;
        end;
      end;
    end;

    result.Add('end.');

    if IsDump then
      result.SaveToFile(DUMP_PATH + GetExtraPascalUnitName(ModuleName) + '.txt');
  end; // GenUnit

var
  I: Integer;
  temp: TStringList;
begin
  for I := 0 to TypeModuleList.Count - 1 do
  if TypeModuleList[I].LangId = PASCAL_LANGUAGE then
  begin
    temp := GenUnit(TypeModuleList[I]);
    TypeModuleList[I].Source := temp.Text;
    temp.Free;
  end;
end;

procedure TTypeDefList.GenBasicUnits;
  function GenUnit(TypeModuleRec: TTypeModuleRec): TStringList;
  var
    I, J: Integer;
    RI: TTypeExpRec;
    S, ModuleName: String;
  begin
    TypeModuleRec.Success := false;
    ModuleName := TypeModuleRec.ModuleName;

    result := TStringList.Create;
    result.Add('Module ' + GetExtraBasicUnitName(ModuleName));

    if TypeModuleRec.UsingList.Count > 0 then
    begin
      S := 'Imports ';
      for I := 0 to TypeModuleRec.UsingList.Count - 1 do
      begin
        S := S + TypeModuleRec.UsingList[I];
        if I < TypeModuleRec.UsingList.Count - 1 then
          S := S + ',';
      end;
      result.Add(S);
    end;

    for I := 0 to TypeExpList.Count - 1 do
    begin
      RI := TypeExpList[I];
      if IndexOf(RI.Name, RI.ParamList, true) >= 0 then
        continue;

      for J:= 0 to Count - 1 do
        if Expansions.IndexOf(I, J) = -1 then
          TryExpansion(RI.Name, BASIC_LANGUAGE, result, TypeModuleRec.Success, I, J);
    end;

    result.Add('End Module');

    if IsDump then
      result.SaveToFile(DUMP_PATH + GetExtraBasicUnitName(ModuleName) + '.txt');
  end;
var
  I: Integer;
  temp: TStringList;
begin
  for I := 0 to TypeModuleList.Count - 1 do
  if TypeModuleList[I].LangId = BASIC_LANGUAGE then
  begin
    temp := GenUnit(TypeModuleList[I]);
    TypeModuleList[I].Source := temp.Text;
    temp.Free;
  end;
end;

procedure TTypeDefList.GenJavaUnits;
  function GenUnit(TypeModuleRec: TTypeModuleRec): TStringList;
  var
    I, J: Integer;
    RI: TTypeExpRec;
    ModuleName: String;
  begin
    TypeModuleRec.Success := false;
    ModuleName := TypeModuleRec.ModuleName;

    result := TStringList.Create;
    result.Add('package ' + GetExtraJavaUnitName(ModuleName) + ';');

    for I := 0 to TypeExpList.Count - 1 do
    begin
      RI := TypeExpList[I];
      if IndexOf(RI.Name, RI.ParamList, true) >= 0 then
        continue;

      for J:= 0 to Count - 1 do
        if Expansions.IndexOf(I, J) = -1 then
          TryExpansion(RI.Name, JAVA_LANGUAGE, result, TypeModuleRec.Success, I, J);
    end;

    if IsDump then
      result.SaveToFile(DUMP_PATH + GetExtraJavaUnitName(ModuleName) + '.txt');
  end;
var
  I: Integer;
  temp: TStringList;
begin
  for I := 0 to TypeModuleList.Count - 1 do
  if TypeModuleList[I].LangId = BASIC_LANGUAGE then
  begin
    temp := GenUnit(TypeModuleList[I]);
    TypeModuleList[I].Source := temp.Text;
    temp.Free;
  end;
end;

// TTypeParams -----------------------------------------------------------------

procedure TTypeParams.AssTo(L: TTypeParams);
var
  I: Integer;
  S: String;
  TR: TTypeRestrictionRec;
begin
  L.Clear;
  for I := 0 to Count - 1 do
  begin
    S := Self[I];
    TR := TTypeRestrictionRec(Objects[I]);
    if TR = nil then
      L.Add(S)
    else
      L.AddObject(S, TR.Clone);
  end;
end;

// TTypeParamsHistory ----------------------------------------------------------

function TTypeParamsHistory.GetRecord(I: Integer): TTypeParams;
begin
  result := TTypeParams(L[I]);
end;

function TTypeParamsHistory.Add(const Value: TTypeParams): TTypeParams;
begin
  result := TTypeParams.Create;
  Value.AssTo(result);
  L.Add(result);
end;

//  TTypeModuleRec -------------------------------------------------------------

constructor TTypeModuleRec.Create;
begin
  inherited;
  UsingList := TStringList.Create;
end;

destructor TTypeModuleRec.Destroy;
begin
  UsingList.Free;
  inherited;
end;

// TTypeModuleList -------------------------------------------------------------

function TTypeModuleList.GetRecord(I: Integer): TTypeModuleRec;
begin
  result := TTypeModuleRec(L[I]);
end;

function TTypeModuleList.IndexOf(const ModuleName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(ModuleName, Records[I].ModuleName) then
    begin
      result := I;
      Exit;
    end;
end;

function TTypeModuleList.AddModule(const ModuleName: String; LangId: Integer): TTypeModuleRec;
begin
  result := TTypeModuleRec.Create;
  result.ModuleName := ModuleName;
  result.LangId := LangId;
  L.Add(result);
end;

//  TGenericTypeContainer ------------------------------------------------------

constructor TGenericTypeContainer.Create;
begin
  inherited;
  MethodList := TStringList.Create;
end;

destructor TGenericTypeContainer.Destroy;
begin
  MethodList.Free;
  inherited;
end;

procedure TGenericTypeContainer.SaveToStream(S: TStream);
var
  B: Byte;
begin
  if not GENERICS_ALLOWED then
    Exit;
  if Definition <> '' then
    B := 1
  else
    B := 0;
  S.Write(B, SizeOf(B));
  if B = 1 then
  begin
    SaveStringToStream(Definition, S);
    SaveStringListToStream(MethodList, S);
  end;
end;

procedure TGenericTypeContainer.LoadFromStream(S: TStream);
var
  B: Byte;
begin
  if not GENERICS_ALLOWED then
    Exit;
  S.Read(B, SizeOf(B));
  if B = 1 then
  begin
    Definition := LoadStringFromStream(S);
    LoadStringListFromStream(MethodList, S);
  end;
end;

end.


