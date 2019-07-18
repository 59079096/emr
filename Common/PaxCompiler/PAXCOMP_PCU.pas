///////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_PCU.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_PCU;
interface
uses {$I uses.def}
  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS,
  PAXCOMP_MODULE,
  PAXCOMP_PARSER,
  PAXCOMP_KERNEL;

function CompileUnit(Owner: Pointer;
                     const UnitName, FileName, PCUName: String;
                     Parser: TBaseParser;
                     BuildAll: Boolean;
                     OutputStream: TStream): Boolean;

function PCUToString(Prg: Pointer; const UnitName,
                     FileName: String): String; overload;
function PCUToString(Prg: Pointer; const UnitName: String): String; overload;
function PCUToMainScript(Prg: Pointer; const Expression: String): String;
function PCUToScript(Prg: Pointer; Expression: String): TStringList;

implementation

uses
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_SYMBOL_TABLE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASERUNNER,
  PAXCOMP_RTI,
  PAXCOMP_MAP,
  PAXCOMP_TYPEINFO,
  PaxCompiler,
  PaxRunner;

procedure AddScopeVars(Prog: TBaseRunner; var CodeLines: String); forward;

procedure CopyLocalImport(RootST, ST: TSymbolTable);
var
  I: Integer;
  R1, R2: TSymbolRec;
  S: TMemoryStream;
  Writer: TWriter;
  Reader: TReader;
begin
  for I := FirstLocalId + 1 to RootST.CompileCard do
  begin
    S := TMemoryStream.Create;
    try
      Writer := TWriter.Create(S, 1024);
      try
        R1 := RootST[I];
        R1.SaveToStream(Writer);
      finally
        FreeAndNil(Writer);
      end;

      S.Seek(0, soFromBeginning);
      Reader := TReader.Create(S, 1024);

      try
        R2 := ST.AddRecord;
        R2.LoadFromStream(Reader);
      finally
        FreeAndNil(Reader);
      end;

      R2.Address := R1.Address;
      R2.PClass  := R1.PClass;

    finally
      FreeAndNil(S);
    end;
  end;
end;

function CompileUnit(Owner: Pointer;
                     const UnitName, FileName, PCUName: String;
                     Parser: TBaseParser;
                     BuildAll: Boolean;
                     OutputStream: TStream): Boolean;

var
  PaxCompiler1: TPaxCompiler;
  PaxRunner1: TPaxRunner;
  UnitParser: TBaseParser;
  C: TBaseParserClass;
  RootKernel: TKernel;
  CodeLines: String;
  I: Integer;
  PaxRunnerClass: TPaxRunnerClass;
  TempProg: Pointer;
begin
  RootKernel := TKernel(Owner).RootKernel;
  CodeLines := '';
  PaxRunnerClass := TPaxRunnerClass(RootKernel.Prog.Owner.ClassType);

  if Assigned(RootKernel.OnUsedUnit) then
    if RootKernel.Modules.IndexOf(UnitName) = -1 then
      if not RootKernel.OnUsedUnit(RootKernel.Owner, UnitName, CodeLines) then
        CodeLines := '';

  if CodeLines = '' then
    if not FileExists(FileName) then
    begin
      I := RootKernel.Modules.IndexOf(UnitName);
      if I >= 0 then
      begin
        CodeLines := RootKernel.Modules[I].Lines.Text;
      end
      else
      begin
        result := FileExists(PCUName);
        Exit;
      end;
    end;

  PaxCompiler1 := TPaxCompiler.Create(nil);
  CopyLocalImport(RootKernel.SymbolTable,
    TKernel(PaxCompiler1.GetKernelPtr).SymbolTable);

  PaxRunner1 := PaxRunnerClass.Create(nil);

  C := TBaseParserClass(Parser.ClassType);
  UnitParser := C.Create;

  TKernel(PaxCompiler1.GetKernelPtr).PCUOwner := Owner;

  TKernel(PaxCompiler1.GetKernelPtr).CopyRootEvents;
  PaxCompiler1.DebugMode := RootKernel.DEBUG_MODE;

  TempProg := CurrProg;
  try
    CurrProg := PaxRunner1.GetProgPtr;

    TKernel(PaxCompiler1.GetKernelPtr).RegisterParser(UnitParser);
    PaxCompiler1.AddModule(UnitName, UnitParser.LanguageName);

    if CodeLines = '' then
      PaxCompiler1.AddCodeFromFile(UnitName, FileName)
    else
      PaxCompiler1.AddCode(UnitName, CodeLines);

    if PaxCompiler1.Compile(PaxRunner1, BuildAll) then
    begin
      if RootKernel.BuildWithRuntimePackages then
        PaxRunner1.GetProgPtr.ProgList.Clear;

      PaxRunner1.GetProgPtr.PCULang := UnitParser.LanguageId;

      if Assigned(OutputStream) then
        PaxRunner1.SaveToStream(OutputStream)
      else
        PaxRunner1.SaveToFile(PCUName);
      result := true;

      RootKernel.BuildedUnits.Add(UpperCase(PCUName));
    end
    else
    begin
      result := false;
      RootKernel.Errors.Add(TKernel(PaxCompiler1.GetKernelPtr).Errors[0].Clone(RootKernel));
    end;
  finally
    CurrProg := TempProg;

    FreeAndNil(UnitParser);
    FreeAndNil(PaxCompiler1);
    FreeAndNil(PaxRunner1);
  end;
end;

function PCUToString(Prg: Pointer; const UnitName: String): String;
begin
  result := PCUToString(Prg, UnitName, UnitName + '.PCU');
end;

function PCUToString(Prg: Pointer; const UnitName,
                     FileName: String): String;
var
  Prog: TBaseRunner;
  ModuleRec: TModuleRec;
  CodeLines: String;
  I, J, J1: Integer;
  MapTable: TMapTable;
  TypeInfoList: TPaxTypeInfoList;
  EnumTypeDataContainer: TEnumTypeDataContainer;
  ArrayTypeDataContainer: TArrayTypeDataContainer;
  RecordTypeDataContainer: TRecordTypeDataContainer;
  SetTypeDataContainer: TSetTypeDataContainer;
  AliasTypeDataContainer: TAliasTypeDataContainer;
  PointerTypeDataContainer: TPointerTypeDataContainer;
  ClassRefTypeDataContainer: TClassRefTypeDataContainer;
  DynArrayTypeDataContainer: TDynArrayTypeDataContainer;
  ProceduralTypeDataContainer: TProceduralTypeDataContainer;
  ClassTypeDataContainer: TClassTypeDataContainer;
  MethodTypeDataContainer: TMethodTypeDataContainer;
  InterfaceTypeDataContainer: TInterfaceTypeDataContainer;
  S, S1, S2: String;
  AFullTypeName: String;
  MapRec: TMapRec;
  SubDesc: TSubDesc;
  K: Integer;
  L: TStringList;
  LangId: Integer;
  AClassName: String;
begin
  Prog := TBaseRunner(Prg);

  LangId := Prog.PCULang;
  if LangId <> BASIC_LANGUAGE then
    LangId := PASCAL_LANGUAGE;

  case LangId of
    PASCAL_LANGUAGE:
    begin
      CodeLines := '{$WARNINGS OFF}' + #13#10 +
         'unit ' + ExtractName(UnitName) + ';' + #13#10 +
         'interface' + #13#10;
    end;
    BASIC_LANGUAGE:
    begin
      CodeLines :=
         'Module ' + ExtractName(UnitName) + #13#10;
    end;
  end;

  ModuleRec := Prog.RunTimeModuleList.Modules.GetModuleRec(UnitName);
  if ModuleRec <> nil then
    if ModuleRec.UsedModules.Count > 0 then
    begin
      case LangId of
        PASCAL_LANGUAGE:
        begin
          CodeLines := CodeLines + 'uses ';
          for I := 0 to ModuleRec.UsedModules.Count - 1 do
          begin
            CodeLines := CodeLines + ModuleRec.UsedModules[I];
            if I < ModuleRec.UsedModules.Count - 1 then
              CodeLines := CodeLines + ','
            else
              CodeLines := CodeLines + ';' + #13#10;
          end;
        end;
        BASIC_LANGUAGE:
        begin
          CodeLines := CodeLines + 'Imports ';
          for I := 0 to ModuleRec.UsedModules.Count - 1 do
          begin
            CodeLines := CodeLines + ModuleRec.UsedModules[I];
            if I < ModuleRec.UsedModules.Count - 1 then
              CodeLines := CodeLines + ','
            else
              CodeLines := CodeLines + #13#10;
          end;
        end;
      end;
    end;

  MapTable := Prog.ScriptMapTable;
  TypeInfoList := Prog.ProgTypeInfoList;

  for I := 0 to TypeInfoList.Count - 1 do
  begin
    S := String(TypeInfoList[I].FullName);
    if Pos(UpperCase(UnitName) + '.', UpperCase(S)) = 0 then
      continue;
    case TypeInfoList[I].TypeInfo.Kind of
      tkUnknown:
      begin
        case TypeInfoList[I].FinTypeId of
          typeALIAS:
          begin
            AliasTypeDataContainer :=
              TypeInfoList[I].TypeDataContainer as TAliasTypeDataContainer;
            case LangId of
              PASCAL_LANGUAGE:
              begin
                   CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                   ' = ';
                CodeLines := CodeLines +
                ExtractName(AliasTypeDataContainer.FullSourceTypeName) + ';' +
                #13#10;
              end;
              BASIC_LANGUAGE:
              begin
                   CodeLines := CodeLines + 'TypeDef ' + PTIName(@TypeInfoList[I].TypeInfo) +
                   ' As ';
                CodeLines := CodeLines +
                ExtractName(AliasTypeDataContainer.FullSourceTypeName) +
                #13#10;
              end;
            end;
          end;
          typePOINTER:
          begin
            PointerTypeDataContainer :=
               TypeInfoList[I].TypeDataContainer as TPointerTypeDataContainer;
            case LangId of
              PASCAL_LANGUAGE:
              begin
                CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                ' = ';
                CodeLines := CodeLines + '^' +
                ExtractName(PointerTypeDataContainer.FullOriginTypeName) + ';' +
                #13#10;
              end;
              BASIC_LANGUAGE:
              begin
                CodeLines := CodeLines + 'TypeDef ' + PTIName(@TypeInfoList[I].TypeInfo) +
                ' As ';
                CodeLines := CodeLines +
                ExtractName(PointerTypeDataContainer.FullOriginTypeName) + ' *' +
                #13#10;
              end;
            end;
          end;
          typeCLASSREF:
          begin
            ClassRefTypeDataContainer :=
              TypeInfoList[I].TypeDataContainer as TClassRefTypeDataContainer;
            case LangId of
              PASCAL_LANGUAGE:
              begin
                CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                   ' = ';
                CodeLines := CodeLines + ' class of ' +
                ExtractName(ClassRefTypeDataContainer.FullOriginTypeName) + ';' +
                #13#10;
              end;
              BASIC_LANGUAGE:
              begin
                CodeLines := CodeLines + 'TypeDef ' + PTIName(@TypeInfoList[I].TypeInfo) +
                   ' As ';
                CodeLines := CodeLines + ' Class Of ' +
                ExtractName(ClassRefTypeDataContainer.FullOriginTypeName) +
                #13#10;
              end;
            end;
          end;
          typePROC:
          begin
            ProceduralTypeDataContainer :=
               TypeInfoList[I].TypeDataContainer as TProceduralTypeDataContainer;

            case LangId of
              PASCAL_LANGUAGE:
              begin
                CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                 ' = ';
                with ProceduralTypeDataContainer.SubDesc do
                begin
                  if ResTypeId = typeVOID then
                    CodeLines := CodeLines + 'procedure ('
                  else
                    CodeLines := CodeLines + 'function (';
                  if ParamList.Count = 0 then
                    CodeLines := CodeLines + ')'
                  else
                    for J := 0 to ParamList.Count - 1 do
                    begin
                      if ParamList[J].ParamMod = PM_BYREF then
                        CodeLines := CodeLines + 'var '
                      else if ParamList[J].ParamMod = PM_CONST then
                        CodeLines := CodeLines + 'const ';

                      CodeLines := CodeLines + ParamList[J].ParamName + ':' +
                        ParamList[J].ParamTypeName;

                      if ParamList[J].OptValue <> '' then
                        CodeLines := CodeLines + '=' + ParamList[J].OptValue;

                      if J < ParamList.Count - 1 then
                        CodeLines := CodeLines + ';'
                      else
                        CodeLines := CodeLines + ')';
                    end;

                  if ResTypeId = typeVOID then
                    CodeLines := CodeLines + ';'
                  else
                    CodeLines := CodeLines + ':' + ResTypeName + ';';

                  case CallMode of
                    ccREGISTER: CodeLines := CodeLines + 'register;';
                    ccSTDCALL: CodeLines := CodeLines + 'stdcall;';
                    ccCDECL: CodeLines := CodeLines + 'cdecl;';
                    ccPASCAL: CodeLines := CodeLines + 'pascal;';
                    ccSAFECALL: CodeLines := CodeLines + 'safecall;';
                    ccMSFASTCALL: CodeLines := CodeLines + 'msfastcall;';
                  end;
                end;
              end;
              BASIC_LANGUAGE:
              begin
                Prog.RaiseError(errInternalError, []);
              end;
            end;
            CodeLines := CodeLines + #13#10;
          end;
        end;
      end;
      tkRecord:
      begin
        RecordTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TRecordTypeDataContainer;

        case LangId of
          PASCAL_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + 'type ' + S + #13#10;
              continue;
            end;

            if RecordTypeDataContainer.IsPacked then
              CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                ' = packed record' + #13#10
            else
              CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
                ' = record' + #13#10;

            with RecordTypeDataContainer do
            for J := 0 to FieldListContainer.Count - 1 do
            begin
              CodeLines := CodeLines +
                StringFromPShortString(@FieldListContainer[J].Name) + ':' +
                ExtractName(FieldListContainer[J].FullFieldTypeName) +
                ';'#13#10;
            end;

            AFullTypeName := TypeInfoList[I].FullName;

            // methods
            for J := 0 to MapTable.Count - 1 do
            begin
              MapRec := MapTable[J];
              if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
              begin
                if MapRec.Kind in KindSUBS then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;

                  S := ExtractName(MapRec.FullName);

                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := 'constructor ' + S + '(';
                    KindDESTRUCTOR:
                      S := 'destructor ' + S + '(';
                    KindSUB:
                    begin
                      if SubDesc.ResTypeId = typeVOID then
                        S := 'procedure ' + S + '('
                      else
                        S := 'function ' + S + '(';
                    end;
                  end;

                  if SubDesc.IsShared then
                    S := 'class ' + S;

                  case MapRec.Vis of
                    cvNone: continue;
                    cvPrivate: S := 'private ' + S;
                    cvPublic: S := 'public ' + S;
                    cvProtected: S := 'protected ' + S;
                    cvPublished:
                    begin
                      if MapRec.Kind = KindSUB then
                        S := 'published ' + S;
                    end;
                  end;

                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'var ';
                      PM_CONST: S := S + 'const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ':';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ';';
                  end;

                  S := S + ')';

                  if MapRec.Kind = KindSUB then
                    if SubDesc.ResTypeId <> typeVOID then
                      S := S + ':' + SubDesc.ResTypeName;
                  S := S + ';';
{
                 case SubDesc.CallMode of
                    cmNONE:
                      if MapRec.Kind = KindDESTRUCTOR then
                        S := S + 'override;';
                    cmVIRTUAL:
                    begin
                      S := S + 'virtual;';
                    end;
                    cmDYNAMIC:
                    begin
                      S := S + 'dynamic;';
                    end;
                    cmOVERRIDE:
                      S := S + 'override;';
                  end;
}
                  if not Prog.PAX64 then
                  begin
                    case SubDesc.CallConv of
                      ccREGISTER: S := S + 'register';
                      ccSTDCALL: S := S + 'stdcall';
                      ccCDECL: S := S + 'cdecl';
                      ccPASCAL: S := S + 'pascal';
                      ccSAFECALL: S := S + 'safecall';
                      ccMSFASTCALL: S := S + 'msfastcall';
                    end;
                    S := S + ';';
                  end;

                  S := S + 'external ' + '''' + FileName + '''' + ';';
                  CodeLines := CodeLines + S + #13#10;
                end; // kindSUB
              end;
            end; // methods
            CodeLines := CodeLines + 'end;'#13#10;
          end; // PASCAL_LANGUAGE
          BASIC_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + S + #13#10;
              continue;
            end;

            CodeLines := CodeLines + 'Structure ' + PTIName(@TypeInfoList[I].TypeInfo) +
            #13#10;
            with RecordTypeDataContainer do
            for J := 0 to FieldListContainer.Count - 1 do
            begin
              CodeLines := CodeLines +
                StringFromPShortString(@FieldListContainer[J].Name) + ' As ' +
                ExtractName(FieldListContainer[J].FullFieldTypeName) +
                #13#10;
            end;

            AFullTypeName := TypeInfoList[I].FullName;

            // methods
            for J := 0 to MapTable.Count - 1 do
            begin
              MapRec := MapTable[J];
              if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
              begin
                if MapRec.Kind in KindSUBS then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;

                  AClassName := ExtractName(S);

                  if SubDesc.IsShared then
                    S := 'Shared '
                  else
                    S := '';

                  case MapRec.Vis of
                    cvNone: continue;
                    cvPrivate: S := S + ' Private ';
                    cvPublic: S := S + ' Public ';
                    cvProtected: S := S + ' Protected ';
                    cvPublished:
                    begin
                      if MapRec.Kind = KindSUB then
                        S := S + ' Published ';
                    end;
                  end;
{
                  case SubDesc.CallMode of
                    cmNONE:
                      if MapRec.Kind = KindDESTRUCTOR then
                        S := S + ' Overriden ';
                    cmVIRTUAL:
                    begin
                      S := S + ' Overriadable ';
                    end;
                    cmDYNAMIC:
                    begin
                      S := S + ' Dynamic ';
                    end;
                    cmOVERRIDE:
                      S := S + ' Overriden ';
                  end;
}
                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := S + ' Sub New ';
                    KindDESTRUCTOR:
                      S := S + ' Sub Finalize ';
                    KindSUB:
                    begin
                      if SubDesc.ResTypeId = typeVOID then
                        S := S + ' Sub ' + ExtractName(MapRec.FullName)
                      else
                        S := S + ' Function ' + ExtractName(MapRec.FullName);
                    end;
                  end;
{
                  case SubDesc.CallConv of
                    ccREGISTER: S := S + ' Register ';
                    ccSTDCALL: S := S + ' StdCall ';
                    ccCDECL: S := S + ' Cdecl ';
                    ccPASCAL: S := S + ' Pascal ';
                    ccSAFECALL: S := S + ' Safecall ';
                    ccMSFASTCALL: S := S + ' msfastcall ';
                  end;
}
                  S := S + ' Lib ' + '"' + FileName + '"';
                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := S + ' Alias ' + '"' + AClassName + '.Create' + '"';
                    KindDESTRUCTOR:
                      S := S + ' Alias ' + '"' + AClassName + '.Destroy' + '"';
                    else
                      S := S + ' Alias ' + '"' + AClassName + '.' + ExtractName(MapRec.FullName) + '"';
                  end;

                  S := S + ' (';

                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'ByRef ';
                      PM_CONST: S := S + 'Const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ' As ';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ',';
                  end;

                  S := S + ')';

                  if MapRec.Kind = KindSUB then
                    if SubDesc.ResTypeId <> typeVOID then
                      S := S + ' As ' + SubDesc.ResTypeName;

                  CodeLines := CodeLines + S + #13#10;
                end; // kindSUB
              end;
            end; // methods

            CodeLines := CodeLines + 'End Structure ' + #13#10 + #13#10;

          end; // BASIC_LANGUAGE
        end;
      end; // tkRecord
      tkArray:
      begin
        case LangId of
          PASCAL_LANGUAGE:
          begin
            ArrayTypeDataContainer :=
              TypeInfoList[I].TypeDataContainer as TArrayTypeDataContainer;
            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
              ' = array[';
            case ArrayTypeDataContainer.FinRangeTypeId of
{$IFNDEF PAXARM}
              typeANSICHAR,
{$ENDIF}
              typeWIDECHAR:
                CodeLines := CodeLines +
                  '''' + Chr(ArrayTypeDataContainer.B1) + '''' + '..' +
                  '''' + Chr(ArrayTypeDataContainer.B2) + '''' + ']';
              typeENUM, typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
                CodeLines := CodeLines +
                  ExtractName(ArrayTypeDataContainer.FullRangeTypeName) + ']';
              else
              begin
                CodeLines := CodeLines +
                  IntToStr(ArrayTypeDataContainer.B1) + '..' +
                  IntToStr(ArrayTypeDataContainer.B2) + ']';
              end;
            end; // case
            CodeLines := CodeLines + ' of ' +
              ExtractName(ArrayTypeDataContainer.FullElemTypeName) + ';' +
              #13#10;
          end;
          BASIC_LANGUAGE:
          begin
            Prog.RaiseError(errInternalError, []);
          end;
        end;
      end; //tkArray
      tkDynArray:
      begin
        DynArrayTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TDynArrayTypeDataContainer;
        case LangId of
          PASCAL_LANGUAGE:
          begin
              CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
               ' = ';
            CodeLines := CodeLines + ' array of ' +
            ExtractName(DynArrayTypeDataContainer.FullElementTypeName) + ';' +
            #13#10;
          end;
          BASIC_LANGUAGE:
          begin
            Prog.RaiseError(errInternalError, []);
          end;
        end;
      end;
      tkSet:
      begin
        SetTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TSetTypeDataContainer;
        case LangId of
          PASCAL_LANGUAGE:
          begin
            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
              ' = set of ';
            CodeLines := CodeLines +
              ExtractName(SetTypeDataContainer.FullCompName) + ';' +
              #13#10;
          end;
          BASIC_LANGUAGE:
          begin
            Prog.RaiseError(errInternalError, []);
          end;
        end;
      end; // tkSet
      tkMethod:
      begin
        MethodTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TMethodTypeDataContainer;
        if MethodTypeDataContainer.ResultTypeId = 0 then
          continue;

        case LangId of
          PASCAL_LANGUAGE:
          begin
            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
             ' = ';
            with MethodTypeDataContainer do
            begin
              if ResultTypeId = typeVOID then
                CodeLines := CodeLines + 'procedure ('
              else
                CodeLines := CodeLines + 'function (';
              if ParamCount = 0 then
                CodeLines := CodeLines + ')'
              else
                for J := 0 to ParamCount - 1 do
                with ParamListContainer do
                begin
                  if ParamList[J].Flags = [pfVar] then
                    CodeLines := CodeLines + 'var '
                  else if ParamList[J].Flags = [pfConst] then
                    CodeLines := CodeLines + 'const ';

                  CodeLines := CodeLines + StringFromPShortString(@ParamList[J].ParamName) + ':' +
                    StringFromPShortString(@ParamList[J].TypeName);
                  if J < ParamCount - 1 then
                    CodeLines := CodeLines + ';'
                  else
                    CodeLines := CodeLines + ')';
                end;

              if ResultTypeId = typeVOID then
                CodeLines := CodeLines + ' of object;'
              else
                CodeLines := CodeLines + ':' + StringFromPShortString(@ResultType) + ' of object;';

              case CallConv of
                ccREGISTER: CodeLines := CodeLines + 'register;';
                ccSTDCALL: CodeLines := CodeLines + 'stdcall;';
                ccCDECL: CodeLines := CodeLines + 'cdecl;';
                ccPASCAL: CodeLines := CodeLines + 'pascal;';
                ccSAFECALL: CodeLines := CodeLines + 'safecall;';
                ccMSFASTCALL: CodeLines := CodeLines + 'msfastcall;';
              end;

              CodeLines := CodeLines + #13#10;
            end;
          end;
          BASIC_LANGUAGE:
          begin
            Prog.RaiseError(errInternalError, []);
          end;
        end;
      end; // tkMethod
      tkClass:
      begin
        ClassTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TClassTypeDataContainer;

        if Pos(UpperCase(UnitName), UpperCase(String(TypeInfoList[I].FullName))) <> 1 then
          continue;

        case LangId of
          PASCAL_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + 'type ' + S + #13#10;
              continue;
            end;

            S := ExtractFullName(String(TypeInfoList[I].FullName));
            S := RemoveCh('#', S);
            CodeLines := CodeLines + 'type ' + S;

            S := ExtractName(String(ClassTypeDataContainer.FullParentName));
            S := RemoveCh('#', S);
            CodeLines := CodeLines + ' = class (' + S;

            for J := 0 to ClassTypeDataContainer.SupportedInterfaces.Count - 1 do
            begin
              CodeLines := CodeLines + ',' +
                ClassTypeDataContainer.SupportedInterfaces[J];
            end;

            CodeLines := CodeLines + ')' + #13#10;
            AFullTypeName := String(TypeInfoList[I].FullName);

            // public fields
            for J := 0 to ClassTypeDataContainer.AnotherFieldListContainer.Count - 1 do
            begin
              case ClassTypeDataContainer.AnotherFieldListContainer[J].Vis of
                cvPrivate: CodeLines := CodeLines + 'private ';
                cvProtected: CodeLines := CodeLines + 'protected ';
                cvPublic: CodeLines := CodeLines + 'public ';
              end;

              CodeLines := CodeLines +
                StringFromPShortString(@ClassTypeDataContainer.AnotherFieldListContainer[J].Name)
                + ':' +
                ClassTypeDataContainer.AnotherFieldListContainer[J].FullFieldTypeName +
                ';'#13#10;
            end;

            // published fields
            for J := 0 to ClassTypeDataContainer.FieldListContainer.Count - 1 do
            begin
              CodeLines := CodeLines + 'published ' +
                StringFromPShortString(@ClassTypeDataContainer.FieldListContainer[J].Name) + ':' +
                ExtractName(ClassTypeDataContainer.FieldListContainer[J].FullFieldTypeName) +
                ';'#13#10;
            end;

            // methods
            for J := 0 to MapTable.Count - 1 do
            begin
              MapRec := MapTable[J];
              if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
              begin
                if MapRec.Kind in KindSUBS then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;

                  S := ExtractName(MapRec.FullName);

                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := 'constructor ' + S + '(';
                    KindDESTRUCTOR:
                      S := 'destructor ' + S + '(';
                    KindSUB:
                    begin
                      if SubDesc.ResTypeId = typeVOID then
                        S := 'procedure ' + S + '('
                      else
                        S := 'function ' + S + '(';
                    end;
                  end;

                  if SubDesc.IsShared then
                    S := 'class ' + S;

                  case MapRec.Vis of
                    cvNone: continue;
                    cvPrivate: S := 'private ' + S;
                    cvPublic: S := 'public ' + S;
                    cvProtected: S := 'protected ' + S;
                    cvPublished:
                    begin
                      if MapRec.Kind = KindSUB then
                        S := 'published ' + S;
                    end;
                  end;

                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'var ';
                      PM_CONST: S := S + 'const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ':';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ';';
                  end;

                  S := S + ')';

                  if MapRec.Kind = KindSUB then
                    if SubDesc.ResTypeId <> typeVOID then
                      S := S + ':' + SubDesc.ResTypeName;
                  S := S + ';';

                 case SubDesc.CallMode of
                    cmNONE:
                      if MapRec.Kind = KindDESTRUCTOR then
                        S := S + 'override;';
                    cmVIRTUAL:
                    begin
                      S := S + 'virtual;';
                    end;
                    cmDYNAMIC:
                    begin
                      S := S + 'dynamic;';
                    end;
                    cmOVERRIDE:
                      S := S + 'override;';
                  end;

                  if not Prog.PAX64 then
                  begin
                    case SubDesc.CallConv of
                      ccREGISTER: S := S + 'register';
                      ccSTDCALL: S := S + 'stdcall';
                      ccCDECL: S := S + 'cdecl';
                      ccPASCAL: S := S + 'pascal';
                      ccSAFECALL: S := S + 'safecall';
                      ccMSFASTCALL: S := S + 'msfastcall';
                    end;
                    S := S + ';';
                  end;

                  if MapRec.SubDesc.OverCount > 0 then
                    S := S + 'overload;';

                  if (MapRec.SubDesc.DllName = '') and (MapRec.SubDesc.AliasName = '') then
                    S := S + 'external ' + '''' + FileName + '''' + ';'
                  else
                    S := S + ' external ' + '''' + MapRec.SubDesc.DllName + '''' +
                             ' name ' + '''' + MapRec.SubDesc.AliasName + '''' +
                    ';';

                  CodeLines := CodeLines + S + #13#10;
                end; // kindSUB
              end;
            end; // methods

            // public properties
            with ClassTypeDataContainer do
            for J := 0 to AnotherPropList.Count - 1 do
            with AnotherPropList[J] do
            begin
              case Vis of
                cvPrivate: CodeLines := CodeLines + 'private ';
                cvProtected: CodeLines := CodeLines + 'protected ';
                cvPublic: CodeLines := CodeLines + 'public ';
              end;
              CodeLines := CodeLines + 'property ' + PropName;
              if ParamNames.Count > 0 then
              begin
                CodeLines := CodeLines + '[';
                for K := 0 to ParamNames.Count - 1 do
                begin
                  CodeLines := CodeLines + ParamNames[K] + ':' +
                                           ParamTypes[K];
                  if K < ParamNames.Count - 1 then
                    CodeLines := CodeLines + ','
                  else
                    CodeLines := CodeLines + ']';
                end;
              end;
              CodeLines := CodeLines + ':' + PropType;
              if Length(ReadName) > 0 then
                CodeLines := CodeLines + ' read ' + ReadName;
              if Length(WriteName) > 0 then
                CodeLines := CodeLines + ' write ' + WriteName;
              CodeLines := CodeLines + ';';
              if IsDefault then
                CodeLines := CodeLines + 'default;';
              CodeLines := CodeLines + #13#10;
            end;

            // published properties
            for J := 0 to ClassTypeDataContainer.PropDataContainer.PropTypeNames.Count - 1 do
            begin
              S1 := ExtractName(ClassTypeDataContainer.PropDataContainer.ReadNames[J]);
              if Pos(READ_PREFIX, S1) = 1 then
                S1 := Copy(S1, Length(READ_PREFIX) + 1, Length(S1) - Length(READ_PREFIX));
              S2 := ExtractName(ClassTypeDataContainer.PropDataContainer.WriteNames[J]);
              if Pos(WRITE_PREFIX, S2) = 1 then
                S2 := Copy(S2, Length(WRITE_PREFIX) + 1, Length(S2) - Length(WRITE_PREFIX));

              CodeLines := CodeLines + 'published property ';

              S := StringFromPShortString(@ClassTypeDataContainer.PropDataContainer.PropList[J].Name);
              CodeLines := CodeLines + S;

              for J1 := 0 to MapTable.Count - 1 do
              begin
                MapRec := MapTable[J1];
                if MapRec.Kind <> KindSUB then
                  continue;

                if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;
                  if SubDesc.ParamList.Count = 0 then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;
                  S := ExtractName(MapRec.FullName);
                  if not StrEql(S, S1) then
                     continue;

                  S := '[';
                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'var ';
                      PM_CONST: S := S + 'const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ':';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ';';
                  end;
                  S := S + ']';
                  CodeLines := CodeLines + S;
                  break;
                end;
              end;

              if CodeLines[Length(CodeLines)] <> ']' then
              for J1 := 0 to MapTable.Count - 1 do
              begin
                MapRec := MapTable[J1];
                if MapRec.Kind <> KindSUB then
                  continue;

                if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;
                  if SubDesc.ParamList.Count <= 1 then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;
                  S := ExtractName(MapRec.FullName);
                  if not StrEql(S, S2) then
                     continue;

                  S := '[';
                  for K:=0 to SubDesc.ParamList.Count - 2 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'var ';
                      PM_CONST: S := S + 'const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ':';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 2 then
                      S := S + ';';
                  end;
                  S := S + ']';
                  CodeLines := CodeLines + S;
                  break;
                end;
              end;

              CodeLines := CodeLines +
                ':' +
                ExtractName(ClassTypeDataContainer.PropDataContainer.PropTypeNames[J]);

              if Length(S1) > 0 then
                CodeLines := CodeLines + ' read ' + S1;
              if Length(S2) > 0 then
                CodeLines := CodeLines + ' write ' + S2;
              CodeLines := CodeLines + ';'#13#10;
            end;

            CodeLines := CodeLines + 'end;' + #13#10;
          end;
          BASIC_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + S + #13#10;
              continue;
            end;

            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + S + #13#10;
              continue;
            end;

            CodeLines := CodeLines + 'Class ' + PTIName(@TypeInfoList[I].TypeInfo) +
            #13#10;

            S := ExtractName(ClassTypeDataContainer.FullParentName);
            S := RemoveCh('#', S);
            CodeLines := CodeLines + 'Inherits ' + S;

            for J := 0 to ClassTypeDataContainer.SupportedInterfaces.Count - 1 do
            begin
              CodeLines := CodeLines + ',' +
                ClassTypeDataContainer.SupportedInterfaces[J];
            end;

            CodeLines := CodeLines + #13#10;

            with ClassTypeDataContainer do
            for J := 0 to FieldListContainer.Count - 1 do
            begin
              CodeLines := CodeLines +
                StringFromPShortString(@FieldListContainer[J].Name) + ' As ' +
                ExtractName(FieldListContainer[J].FullFieldTypeName) +
                #13#10;
            end;

            AFullTypeName := TypeInfoList[I].FullName;

            // methods
            for J := 0 to MapTable.Count - 1 do
            begin
              MapRec := MapTable[J];
              if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
              begin
                if MapRec.Kind in KindSUBS then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;

                  AClassName := ExtractName(S);

                  if SubDesc.IsShared then
                    S := 'Shared '
                  else
                    S := '';

                  case MapRec.Vis of
                    cvNone: continue;
                    cvPrivate: S := S + ' Private ';
                    cvPublic: S := S + ' Public ';
                    cvProtected: S := S + ' Protected ';
                    cvPublished:
                    begin
                      if MapRec.Kind = KindSUB then
                        S := S + ' Published ';
                    end;
                  end;
{
                  case SubDesc.CallMode of
                    cmNONE:
                      if MapRec.Kind = KindDESTRUCTOR then
                        S := S + ' Overriden ';
                    cmVIRTUAL:
                    begin
                      S := S + ' Overriadable ';
                    end;
                    cmDYNAMIC:
                    begin
                      S := S + ' Dynamic ';
                    end;
                    cmOVERRIDE:
                      S := S + ' Overriden ';
                  end;
}
                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := S + ' Sub New ';
                    KindDESTRUCTOR:
                      S := S + ' Sub Finalize ';
                    KindSUB:
                    begin
                      S1 := ExtractName(MapRec.FullName);
                      if Pos('__get', S1) = 1 then
                        S1 := '__' + S1
                      else if Pos('__set', S1) = 1 then
                        S1 := '__' + S1;
                      if SubDesc.ResTypeId = typeVOID then
                        S := S + ' Sub ' + S1
                      else
                        S := S + ' Function ' + S1;
                    end;
                  end;
{
                  case SubDesc.CallConv of
                    ccREGISTER: S := S + ' Register ';
                    ccSTDCALL: S := S + ' StdCall ';
                    ccCDECL: S := S + ' Cdecl ';
                    ccPASCAL: S := S + ' Pascal ';
                    ccSAFECALL: S := S + ' Safecall ';
                    ccMSFASTCALL: S := S + ' msfastcall ';
                  end;
}
                  S := S + ' Lib ' + '"' + FileName + '"';
                  case MapRec.Kind of
                    KindCONSTRUCTOR:
                      S := S + ' Alias ' + '"' + AClassName + '.Create' + '"';
                    KindDESTRUCTOR:
                      S := S + ' Alias ' + '"' + AClassName + '.Destroy' + '"';
                    else
                      S := S + ' Alias ' + '"' + AClassName + '.' + ExtractName(MapRec.FullName) + '"';
                  end;

                  S := S + ' (';

                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'ByRef ';
                      PM_CONST: S := S + 'Const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ' As ';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ',';
                  end;

                  S := S + ')';

                  if MapRec.Kind = KindSUB then
                    if SubDesc.ResTypeId <> typeVOID then
                      S := S + ' As ' + SubDesc.ResTypeName;

                  CodeLines := CodeLines + S + #13#10;
                end; // kindSUB
              end;
            end; // methods

            // public properties
            with ClassTypeDataContainer do
            for J := 0 to AnotherPropList.Count - 1 do
            with AnotherPropList[J] do
            begin
              case Vis of
                cvPrivate: CodeLines := CodeLines + 'Private ';
                cvProtected: CodeLines := CodeLines + 'Protected ';
                cvPublic: CodeLines := CodeLines + 'Public ';
              end;
              CodeLines := CodeLines + 'Property ' + PropName;
              if ParamNames.Count > 0 then
              begin
                CodeLines := CodeLines + '(';
                for K := 0 to ParamNames.Count - 1 do
                begin
                  CodeLines := CodeLines + ParamNames[K] + ' As ' +
                                           ParamTypes[K];
                  if K < ParamNames.Count - 1 then
                    CodeLines := CodeLines + ','
                  else
                    CodeLines := CodeLines + ')';
                end;
              end;
              CodeLines := CodeLines + ' As ' +
                PropType + #13#10;
              if Length(ReadName) > 0 then
              begin
                CodeLines := CodeLines + 'Get' + #13#10;
                CodeLines := CodeLines + '  Return ' + '__' + ReadName;
                CodeLines := CodeLines + '(';
                for K := 0 to ParamNames.Count - 1 do
                begin
                  CodeLines := CodeLines + ParamNames[K];
                  if K < ParamNames.Count - 1 then
                    CodeLines := CodeLines + ',';
                end;
                CodeLines := CodeLines + ')';
                CodeLines := CodeLines + #13#10;
                CodeLines := CodeLines + 'End Get' + #13#10;
              end;
              if Length(WriteName) > 0 then
              begin
                CodeLines := CodeLines + 'Set' + #13#10;
                CodeLines := CodeLines + '__' + WriteName;
                CodeLines := CodeLines + '(';
                for K := 0 to ParamNames.Count - 1 do
                begin
                  CodeLines := CodeLines + ParamNames[K];
                  CodeLines := CodeLines + ',';
                end;
                CodeLines := CodeLines + 'value)';
                CodeLines := CodeLines + #13#10;
                CodeLines := CodeLines + 'End Set' + #13#10;
              end;
              CodeLines := CodeLines + 'End Property' + #13#10;
              CodeLines := CodeLines + #13#10;
            end;

            // published properties
            for J := 0 to ClassTypeDataContainer.PropDataContainer.PropTypeNames.Count - 1 do
            begin
              S1 := ExtractName(ClassTypeDataContainer.PropDataContainer.ReadNames[J]);
              if Pos(READ_PREFIX, S1) = 1 then
                S1 := Copy(S1, Length(READ_PREFIX) + 1, Length(S1) - Length(READ_PREFIX));
              S2 := ExtractName(ClassTypeDataContainer.PropDataContainer.WriteNames[J]);
              if Pos(WRITE_PREFIX, S2) = 1 then
                S2 := Copy(S2, Length(WRITE_PREFIX) + 1, Length(S2) - Length(WRITE_PREFIX));

              CodeLines := CodeLines + 'Published Property ';

              S := StringFromPShortString(@ClassTypeDataContainer.PropDataContainer.PropList[J].Name);
              CodeLines := CodeLines + S;

              for J1 := 0 to MapTable.Count - 1 do
              begin
                MapRec := MapTable[J1];
                if MapRec.Kind <> KindSUB then
                  continue;

                if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;
                  if SubDesc.ParamList.Count = 0 then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;
                  S := ExtractName(MapRec.FullName);
                  if not StrEql(S, S1) then
                     continue;

                  S := '(';
                  for K:=0 to SubDesc.ParamList.Count - 1 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'var ';
                      PM_CONST: S := S + 'const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ':';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 1 then
                      S := S + ';';
                  end;
                  S := S + ')';
                  CodeLines := CodeLines + S;
                  break;
                end;
              end;

              if CodeLines[Length(CodeLines)] <> ')' then
              for J1 := 0 to MapTable.Count - 1 do
              begin
                MapRec := MapTable[J1];
                if MapRec.Kind <> KindSUB then
                  continue;

                if Pos(UpperCase(UnitName), UpperCase(MapRec.FullName)) = 1 then
                begin
                  SubDesc := MapRec.SubDesc;
                  if not SubDesc.IsMethod then
                    continue;
                  if SubDesc.ParamList.Count <= 1 then
                    continue;

                  S := ExtractFullOwner(MapRec.FullName);
                  if not StrEql(AFullTypeName, S) then
                    continue;
                  S := ExtractName(MapRec.FullName);
                  if not StrEql(S, S2) then
                     continue;

                  S := '(';
                  for K:=0 to SubDesc.ParamList.Count - 2 do
                  begin
                    case SubDesc.ParamList[K].ParamMod of
                      PM_BYVAL: begin end;
                      PM_BYREF: S := S + 'ByRef ';
                      PM_CONST: S := S + 'Const ';
                    end;
                    S := S + SubDesc.ParamList[K].ParamName;
                    S := S + ' As ';
                    S := S + SubDesc.ParamList[K].ParamTypeName;

                    if SubDesc.ParamList[K].OptValue <> '' then
                      S := S + '=' + SubDesc.ParamList[K].OptValue;

                    if K < SubDesc.ParamList.Count - 2 then
                      S := S + ',';
                  end;
                  S := S + ')';
                  CodeLines := CodeLines + S;
                  break;
                end;
              end;

              CodeLines := CodeLines +
                ' As ' +
                ExtractName(ClassTypeDataContainer.PropDataContainer.PropTypeNames[J]);

              CodeLines := CodeLines + #13#10;
              if Length(S1) > 0 then
              begin
                CodeLines := CodeLines + 'Get' + #13#10;
                CodeLines := CodeLines + '  Return ' + '__' + String(S1);
                CodeLines := CodeLines + '(';
                CodeLines := CodeLines + ')';
                CodeLines := CodeLines + #13#10;
                CodeLines := CodeLines + 'End Get' + #13#10;
              end;
              if Length(S2) > 0 then
              begin
                CodeLines := CodeLines + 'Set' + #13#10;
                CodeLines := CodeLines + '__' + String(S2);
                CodeLines := CodeLines + '(';
                CodeLines := CodeLines + 'value)';
                CodeLines := CodeLines + #13#10;
                CodeLines := CodeLines + 'End Set' + #13#10;
              end;
              CodeLines := CodeLines + 'End Property' + #13#10;
            end;

            CodeLines := CodeLines + 'End Class' + #13#10 + #13#10;

          end; // Basic
        end;
      end;
      tkString:
      begin
        case LangId of
          PASCAL_LANGUAGE:
          begin
{$IFDEF PAXARM}
            Prog.RaiseError(errInternalError, []);
{$ELSE}
            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
              ' = String[' +
              IntToStr(TypeInfoList[I].TypeDataContainer.TypeData.MaxLength) +
              '];'#13#10;
{$ENDIF}
          end;
          BASIC_LANGUAGE:
          begin
            Prog.RaiseError(errInternalError, []);
          end;
        end;
      end;
      tkInterface:
      begin
        InterfaceTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TInterfaceTypeDataContainer;

        case LangId of
          PASCAL_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + ' type ' + S + #13#10;
              continue;
            end;

            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
              ' = interface(' +
              ExtractName(InterfaceTypeDataContainer.FullParentName)
              + ')' + #13#10;
            CodeLines := CodeLines +
              '[''' +
              GuidToString(InterfaceTypeDataContainer.Guid) +
              ''']' +
              #13#10;

            for K := 0 to InterfaceTypeDataContainer.SubDescList.Count - 1 do
            with InterfaceTypeDataContainer.SubDescList[K] do
            begin
              if ResTypeId = typeVOID then
                CodeLines := CodeLines + 'procedure '
              else
                CodeLines := CodeLines + 'function ';

              CodeLines := CodeLines + SubName + '(';

              if ParamList.Count = 0 then
                CodeLines := CodeLines + ')'
              else
                for J := 0 to ParamList.Count - 1 do
                begin
                  if ParamList[J].ParamMod = PM_BYREF then
                    CodeLines := CodeLines + 'var '
                  else if ParamList[J].ParamMod = PM_CONST then
                    CodeLines := CodeLines + 'const ';

                  CodeLines := CodeLines + ParamList[J].ParamName + ':' +
                    ParamList[J].ParamTypeName;

                  if ParamList[J].OptValue <> '' then
                    CodeLines := CodeLines + '=' + ParamList[J].OptValue;

                  if J < ParamList.Count - 1 then
                    CodeLines := CodeLines + ';'
                  else
                    CodeLines := CodeLines + ')';
                end; // for-loop

              if ResTypeId = typeVOID then
                CodeLines := CodeLines + ';'
              else
                CodeLines := CodeLines + ':' + ResTypeName + ';';

              if OverCount > 0 then
                CodeLines := CodeLines + 'overload;';

              case CallConv of
                ccREGISTER: CodeLines := CodeLines + 'register;';
                ccSTDCALL: CodeLines := CodeLines + 'stdcall;';
                ccCDECL: CodeLines := CodeLines + 'cdecl;';
                ccPASCAL: CodeLines := CodeLines + 'pascal;';
                ccSAFECALL: CodeLines := CodeLines + 'safecall;';
                ccMSFASTCALL: CodeLines := CodeLines + 'msfastcall;';
              end;

              CodeLines := CodeLines + #13#10;
            end; // for-looop SubDescList

            for J := 0 to InterfaceTypeDataContainer.PropDataContainer.Count - 1 do
            with InterfaceTypeDataContainer do
            begin
              S1 := ExtractName(PropDataContainer.ReadNames[J]);
              S2 := ExtractName(PropDataContainer.WriteNames[J]);

              CodeLines := CodeLines + 'property ' +
                StringFromPShortString(@PropDataContainer.PropList[J].Name) + ':' +
                PropDataContainer.PropTypeNames[J];
              if S1 <> '' then
                CodeLines := CodeLines + ' read ' + S1;
              if S2 <> '' then
                CodeLines := CodeLines + ' write ' + S2;
              CodeLines := CodeLines + ';'#13#10;
            end;

            CodeLines := CodeLines + 'end;' + #13#10;
          end; // pascal
          BASIC_LANGUAGE:
          begin
            if TypeInfoList[I].IsGeneric then
            begin
              S := TypeInfoList[I].GenericTypeContainer.Definition;
              CodeLines := CodeLines + S + #13#10;
              continue;
            end;

            CodeLines := CodeLines + 'Interface ' + PTIName(@TypeInfoList[I].TypeInfo) +
                         #13#10;
            CodeLines := CodeLines +  'Inherits ' +
              ExtractName(InterfaceTypeDataContainer.FullParentName)
              + #13#10;

            for K := 0 to InterfaceTypeDataContainer.SubDescList.Count - 1 do
            with InterfaceTypeDataContainer.SubDescList[K] do
            begin
              if ResTypeId = typeVOID then
                CodeLines := CodeLines + 'Sub '
              else
                CodeLines := CodeLines + 'Function ';

              CodeLines := CodeLines + SubName + '(';

              if ParamList.Count = 0 then
                CodeLines := CodeLines + ')'
              else
                for J := 0 to ParamList.Count - 1 do
                begin
                  if ParamList[J].ParamMod = PM_BYREF then
                    CodeLines := CodeLines + 'ByRef '
                  else if ParamList[J].ParamMod = PM_CONST then
                    CodeLines := CodeLines + 'Const ';

                  CodeLines := CodeLines + ParamList[J].ParamName + ' As ' +
                    ParamList[J].ParamTypeName;

                  if ParamList[J].OptValue <> '' then
                    CodeLines := CodeLines + '=' + ParamList[J].OptValue;

                  if J < ParamList.Count - 1 then
                    CodeLines := CodeLines + ','
                  else
                    CodeLines := CodeLines + ')';
                end; // for-loop

              if ResTypeId <> typeVOID then
                CodeLines := CodeLines + ' As ' + ResTypeName;

              CodeLines := CodeLines + #13#10;
            end; // for-looop SubDescList

            CodeLines := CodeLines + 'End Interface' + #13#10;
          end;
        end;
      end; // tkInterface
      tkEnumeration:
      begin
        EnumTypeDataContainer :=
          TypeInfoList[I].TypeDataContainer as TEnumTypeDataContainer;

        case LangId of
          PASCAL_LANGUAGE:
          begin
            CodeLines := CodeLines + 'type ' + PTIName(@TypeInfoList[I].TypeInfo) +
              ' = (';
            K := System.Length(EnumTypeDataContainer.NameList);
            for J := 0 to K - 1 do
            begin
              CodeLines := CodeLines +
                StringFromPShortString(@EnumTypeDataContainer.NameList[J]) + '=' +
                IntToStr(EnumTypeDataContainer.ValueList[J]);

              if J < K - 1 then
                CodeLines := CodeLines + ','
              else
                CodeLines := CodeLines + ');' + #13#10;
            end;
          end;
          BASIC_LANGUAGE:
          begin
            CodeLines := CodeLines + 'Enum ' + PTIName(@TypeInfoList[I].TypeInfo) +
              #13#10;
            K := System.Length(EnumTypeDataContainer.NameList);
            for J := 0 to K - 1 do
            begin
              CodeLines := CodeLines +
                StringFromPShortString(@EnumTypeDataContainer.NameList[J]) + '=' +
                IntToStr(EnumTypeDataContainer.ValueList[J]) + #13#10;
            end;
            CodeLines := CodeLines + 'End Enum' + #13#10;
          end;
        end;
      end; //tkEnumeration
    end; // case
  end; // for-loop

  for I := 0 to MapTable.Count - 1 do
  begin
    MapRec := MapTable[I];
    if Pos(UpperCase(ExtractName(UnitName)), UpperCase(MapRec.FullName)) = 1 then
    begin
      S := ExtractName(MapRec.FullName);
      if MapRec.Kind = KindSUB then
      begin
        SubDesc := MapRec.SubDesc;
        if SubDesc.IsMethod then
          continue;

        if S = '' then
          continue;

        if SubDesc.ResTypeId = typeVOID then
          S := 'procedure ' + S + '('
        else
          S := 'function ' + S + '(';

        for J:=0 to SubDesc.ParamList.Count - 1 do
        begin
          case SubDesc.ParamList[J].ParamMod of
            PM_BYVAL: begin end;
            PM_BYREF: S := S + 'var ';
            PM_CONST: S := S + 'const ';
          end;
          S := S + SubDesc.ParamList[J].ParamName;
          S := S + ':';
          S := S + SubDesc.ParamList[J].ParamTypeName;

          if SubDesc.ParamList[J].OptValue <> '' then
            S := S + '=' + SubDesc.ParamList[J].OptValue;

          if J < SubDesc.ParamList.Count - 1 then
            S := S + ';';
        end;

        S := S + ')';
        if SubDesc.ResTypeId <> typeVOID then
          S := S + ':' + SubDesc.ResTypeName;
        S := S + ';';

        if not Prog.PAX64 then
        begin
          case SubDesc.CallConv of
            ccREGISTER: S := S + 'register';
            ccSTDCALL: S := S + 'stdcall';
            ccCDECL: S := S + 'cdecl';
            ccPASCAL: S := S + 'pascal';
            ccSAFECALL: S := S + 'safecall';
            ccMSFASTCALL: S := S + 'msfastcall';
          end;
          S := S + ';';
        end;

        if SubDesc.OverCount > 0 then
          S := S + 'overload;';

        if (MapRec.SubDesc.DllName = '') and (MapRec.SubDesc.AliasName = '') then
          S := S + 'external ' + '''' + FileName + '''' + ';'
        else
          S := S + ' external ' + '''' + MapRec.SubDesc.DllName + '''' +
                   ' name ' + '''' + MapRec.SubDesc.AliasName + '''' +
          ';';

        CodeLines := CodeLines + S + #13#10;
      end // kindSUB
      else if MapRec.Kind = KindVAR then
      begin
        S := 'var ' + S + ':' +
          ExtractName(MapRec.FullTypeName) +
          ';';
        S := S + 'external ' + '''' + FileName + '''' + ';';
        CodeLines := CodeLines + S + #13#10;
      end // kindVAR
      else if MapRec.Kind = KindCONST then
      begin
        S := 'const ' + S + ':' +
          ExtractName(MapRec.FullTypeName) +
          ';';
        S := S + 'external ' + '''' + FileName + '''' + ';';
        CodeLines := CodeLines + S + #13#10;
      end; // kindCONST
      CodeLines := CodeLines + #13#10;
    end;
  end;

  if Prog.CurrExpr <> '' then
    AddScopeVars(Prog, CodeLines);

  case LangId of
    PASCAL_LANGUAGE:
    begin
      CodeLines := CodeLines +
        'implementation' + #13#10;

      for I := 0 to TypeInfoList.Count - 1 do
      begin
        if Pos(UpperCase(ExtractName(UnitName)), UpperCase(String(TypeInfoList[I].FullName))) <> 1 then
          continue;
        if TypeInfoList[I].IsGeneric then
        for J := 0 to TypeInfoList[I].GenericTypeContainer.MethodList.Count - 1 do
        begin
          S := TypeInfoList[I].GenericTypeContainer.MethodList[J];
          CodeLines := CodeLines + S + #13#10;
          continue;
        end;
      end;

      if Prog.CurrExpr <> '' then
      begin
        CodeLines := CodeLines +
          'begin ' + #13#10;
        CodeLines := CodeLines +
          'print ' + Prog.CurrExpr + ';' + #13#10;
      end;

      CodeLines := CodeLines +
        'end.' + #13#10;
    end;
    BASIC_LANGUAGE:
      CodeLines := CodeLines +
        'End Module' + #13#10;
  end;

  if IsDump then
  begin
    L := TStringList.Create;
    try
      L.Text := CodeLines;
      L.SaveToFile(DUMP_PATH + ExtractName(UnitName) + '.dmp');
    finally
      FreeAndNil(L);
    end;
  end;

  result := CodeLines;
end;

procedure AddScopeVars(Prog: TBaseRunner; var CodeLines: String);
var
  MR, MRT: TMapRec;
  I: Integer;
  S: String;
  MapTable: TMapTable;
begin
  MR := Prog.GetCurrentSub;
  if MR <> nil then
  begin
    for I := 0 to MR.SubDesc.ParamList.Count - 1 do
    with MR.SubDesc.ParamList[I] do
    begin
      if ParamMod = PM_BYREF then
        S := PRR_FILE_EXT
      else
        S := PRM_FILE_EXT;

      CodeLines := CodeLines +
        'var ' + ParamName + ':' + ParamTypeName + '; external ' +
        '''' + MR.FullName + '.' + S + '''' + ';' +
        #13#10;
    end;
    for I := 0 to MR.SubDesc.LocalVarList.Count - 1 do
    with MR.SubDesc.LocalVarList[I] do
    begin
      if IsByRef then
        S := LOR_FILE_EXT
      else
        S := LOC_FILE_EXT;

      CodeLines := CodeLines +
        'var ' + LocalVarName + ':' + LocalVarTypeName + '; external ' +
        '''' + MR.FullName + '.' + S + '''' + ';' +
        #13#10;
    end;

    if MR.SubDesc.IsMethod then
    begin
      if MR.SubDesc.ParamList.IndexOf('Self') = -1 then
      if MR.SubDesc.LocalVarList.IndexOf('Self') = -1 then
      begin
        S := ExtractClassName(MR.FullName);
        CodeLines := CodeLines +
          'var Self: ' + S + '; external ' +
          '''' + MR.FullName + '.' + SLF_FILE_EXT + '''' + ';' +
          #13#10;
      end;
      S := ExtractFullOwner(MR.FullName);
      MapTable := Prog.ScriptMapTable;
      MRT := MapTable.LookupType(S);
      if MRT <> nil then
      begin
        for I := 0 to MRT.FieldList.Count - 1 do
        begin
          CodeLines := CodeLines +
            'var ' + MRT.FieldList[I].FieldName + ':' +
            MRT.FieldList[I].FieldTypeName +
            '; external ' +
            '''' + MR.FullName + '.' +
            FLD_FILE_EXT + '''' + ';' +
            #13#10;
        end;
      end;
    end;
  end;
end;

function PCUToMainScript(Prg: Pointer; const Expression: String): String;
var
  Prog: TBaseRunner;
  UnitName: String;
begin
  Prog := TBaseRunner(Prg);
  Prog.CurrExpr := Expression;
  try
    UnitName := Prog.GetModuleName;
    result := PCUToString(Prg, UnitName);
  finally
    Prog.CurrExpr := '';
  end;
end;

function PCUToScript(Prg: Pointer; Expression: String): TStringList;
var
  Prog: TBaseRunner;
  UnitName: String;
  I: Integer;
  S: String;
begin
  Prog := TBaseRunner(Prg);
  UnitName := Prog.GetModuleName;

  S := PCUToMainScript(Prg, Expression);

  result := TStringList.Create;
  result.Add(S);

  for I := 0 to Prog.ProgList.Count - 1 do
  begin
    S := PCUToString(Prog.ProgList[I].Prog,
                     ExtractName(Prog.ProgList[I].FullPath));
    result.Add(S);
  end;

  if not IsDump then
    Exit;
end;

end.


