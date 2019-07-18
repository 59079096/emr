////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_SYMBOL_TABLE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_SYMBOL_TABLE;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_VAROBJECT,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_LOCALSYMBOL_TABLE,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_OFFSET,
  PAXCOMP_MAP,
  PAXCOMP_STDLIB;

type

  TSymbolTable = class(TLocalSymbolTable)
  private
    kernel: Pointer;
  public
    InCode: array of Boolean;
    CompileCard: Integer;
    LinkCard: Integer;
    IsExtraTable: Boolean;
    constructor Create(i_kernel: Pointer);
    procedure Reset; override;
    procedure ResetCompilation;
    procedure SetShifts(prog: Pointer);
    procedure RaiseError(const Message: string; params: array of const);
    procedure CreateOffsets(JS1, JS2: Integer);
    function GetValueAsString(P: Pointer;
                              StackFrameNumber: Integer;
                              Id: Integer;
                              TypeMapRec: TTypeMapRec = nil;
                              BriefCls: Boolean = false): String; override;
    function GetSizeOfParams(SubId: Integer): Integer;
  end;

implementation

uses
  PAXCOMP_BYTECODE,
  PAXCOMP_BASERUNNER,
  PAXCOMP_KERNEL;

// TSymbolTable ----------------------------------------------------------------

constructor TSymbolTable.Create(i_kernel: Pointer);
begin
  inherited Create(TKernel(i_kernel).GT);
  Self.kernel := i_kernel;
  IsExtraTable := false;
end;

procedure TSymbolTable.Reset;
begin
  inherited;
  ExternList.Clear;
  CompileCard := Card;
  LinkCard := Card;
end;

procedure TSymbolTable.ResetCompilation;
var
  I, Id: Integer;
  S: String;
begin
  while Card > CompileCard do
  begin
    S := Records[Card].Name;
    Id := Records[Card].Id;
    HashArray.DeleteName(S, Id);

    I := Card - FirstLocalId - 1;
{$IFDEF ARC}
    A[I] := nil;
{$ELSE}
    TSymbolRec(A[I]).Free;
{$ENDIF}
    A.Delete(I);

    Dec(Card);
  end;

  if TypeHelpers <> nil then
  begin
    FreeAndNil(TypeHelpers);
    TypeHelpers := GlobalST.TypeHelpers.Clone;
  end;
end;

procedure TSymbolTable.SetShifts(prog: Pointer);

  procedure SetInCode;
  var
    Code: TCode;
    I, Id: Integer;
  begin
    InCode := nil;
    SetLength(InCode, Card + Card);
    Code := TKernel(kernel).Code;

    for I := 1 to StdCard do
      InCode[I] := true;

    for I:= 1 to Code.Card do
    begin

      Id := Code[I].Arg1;
      if Id > 0 then
        if Id <= Card then
          InCode[Id] := true;

      Id := Code[I].Arg2;
      if Id > 0 then
        if Id <= Card then
          InCode[Id] := true;

      Id := Code[I].Res;
      if Id > 0 then
        if Id <= Card then
          InCode[Id] := true;

    end;
  end;


  procedure ProcessType(T: Integer); forward;

  procedure ProcessRecordType(T: Integer);
  var
    J, J1, FT, FT1, S, CurrAlign, DefAlign, VJ, VK, VS: Integer;
    VarPathList: TVarPathList;
    Path: TVarPath;
    RJ: TSymbolRec;
  begin
    if Records[T].Host then
      Exit;

    if Records[T].TypeID = typeALIAS then
      T := Records[T].PatternId;

    VarPathList := TVarPathList.Create;

    try

      for J:=T + 1 to Card do
      begin
        RJ := Records[J];

        if RJ.Kind = KindSUB then
        if RJ.Level <> T then
           break;

        if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
          if RJ.VarCount > 0 then
            VarPathList.Add(J, RJ.VarCount);
      end;

      S := 0;
      if Records[T].IsPacked then
      begin
        if VarPathList.Count = 0 then
        begin
          for J:=T + 1 to Card do
          begin
            RJ := Records[J];

            if RJ.Kind = KindSUB then
            if RJ.Level <> T then
              break;

            if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
            begin
              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;
        end // VarCnt = 0
        else
        begin
          for J:=T + 1 to Card do
          begin
            RJ := Records[J];

            if RJ.Kind = KindSUB then
            if RJ.Level <> T then
              break;

            if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
            begin
              if RJ.VarCount > 0 then
                break;

              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;

          // process variant part of record

          VS := S;

          for VK :=0 to VarPathList.Count - 1 do
          begin
            Path := VarPathList[VK];

            S := VS;
            for VJ := 0 to Path.Count - 1 do
            begin
              J := Path[VJ].Id;

              RJ := Records[J];

              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;
        end;
      end
      else
      begin
        DefAlign := Records[T].DefaultAlignment;

        if VarPathList.Count = 0 then
        begin
          J1 := -1;

          for J:=T + 1 to Card do
          begin
            RJ := Records[J];

            if RJ.Kind = KindSUB then
            if RJ.Level <> T then
              break;

            if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
            begin
              CurrAlign := GetAlignmentSize(RJ.TypeId, DefAlign);

              if J1 > 0 then
              begin
                FT1 := Records[J-1].FinalTypeId;
                FT  := RJ.FinalTypeId;
                if FT = FT1 then
                begin
                  CurrAlign := 1;
                  J1 := -1;
                end
                else
                  J1 := J;
              end
              else
                J1 := J;

              if CurrAlign > 1 then
                while S mod CurrAlign <> 0 do
                  Inc(S);

              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;

        end // VarCnt = 0
        else
        begin // VarCnt > 0
          J1 := -1;

          for J:=T + 1 to Card do
          begin
            RJ := Records[J];

            if RJ.Kind = KindSUB then
            if RJ.Level <> T then
              break;

            if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
            begin
              if RJ.VarCount > 0 then
                break;

              CurrAlign := GetAlignmentSize(RJ.TypeId, DefAlign);

              if J1 > 0 then
              begin
                FT1 := Records[J-1].FinalTypeId;
                FT := RJ.FinalTypeId;
                if FT = FT1 then
                begin
                  CurrAlign := 1;
                  J1 := -1;
                end
                else
                  J1 := J;
              end
              else
                J1 := J;

              if CurrAlign > 1 then
                while S mod CurrAlign <> 0 do
                  Inc(S);

              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;

          // process variant part of record

          VS := S;

          for VK :=0 to VarPathList.Count - 1 do
          begin
            S := VS;
            Path := VarPathList[VK];
            for VJ := 0 to Path.Count - 1 do
            begin
              J := Path[VJ].Id;

              RJ := Records[J];

              CurrAlign := GetAlignmentSize(RJ.TypeId, DefAlign);

              if J1 > 0 then
              begin
                FT1 := Records[J-1].FinalTypeId;
                FT := RJ.FinalTypeId;
                if FT = FT1 then
                begin
                  CurrAlign := 1;
                  J1 := -1;
                end
                else
                  J1 := J;
              end
              else
                J1 := J;

              if CurrAlign > 1 then
                while S mod CurrAlign <> 0 do
                  Inc(S);

              RJ.Shift := S;
              ProcessType(RJ.TypeId);
              Inc(S, RJ.Size);
            end;
          end;
        end; // VarCnt > 0
      end;

    finally
      FreeAndNil(VarPathList);
    end;
  end;

  procedure ProcessClassType(T: Integer);
  var
    J, S, AncestorId: Integer;
    RJ: TSymbolRec;
  begin
    if Records[T].Host then
      Exit;

    if Records[T].TypeID = typeALIAS then
      T := Records[T].PatternId;

    AncestorId := Records[T].AncestorId;
    if AncestorId = 0 then
      S := 0
    else
    begin
      if not Records[AncestorId].Completed then
      begin
        ProcessClassType(AncestorId);
        Records[AncestorId].Completed := true;
      end;
      S := Records[AncestorId].GetSizeOfAllClassFields(prog);
    end;

    if Records[T].IsPacked then
    begin
      for J:=T + 1 to Card do
      begin
        RJ := Records[J];
        if (RJ.Kind = KindTYPE_FIELD) and (RJ.Level = T) then
        begin
          RJ.Shift := S;
          ProcessType(RJ.TypeId);
          Inc(S, RJ.Size);
        end;
      end;
    end
    else
      RaiseError(errInternalError, []);
  end;

  procedure ProcessArrayType(T: Integer);
  var
    J, PatternId: Integer;
    RJ: TSymbolRec;
  begin
    for J:=T + 1 to Card do
    begin
      RJ := Records[J];
      if RJ.Level = T then
      begin
        if RJ.Kind in [KindLABEL, KindNONE] then
          continue;

        if RJ.Kind <> KindTYPE then
          RaiseError(errInternalError, []);

        if RJ.TypeID = typeALIAS then
        begin
          PatternId := RJ.PatternId;
          ProcessType(PatternId);
        end
        else
          ProcessType(J);
      end;
    end;
  end;

  procedure ProcessType(T: Integer);
  begin
    if Records[T].Kind <> kindTYPE then
      RaiseError(errInternalError, []);

    T := Records[T].TerminalTypeId;

    if Records[T].Completed then
      Exit;

    Records[T].Completed := true;

    case Records[T].FinalTypeId of
      typeRECORD: ProcessRecordType(T);
      typeCLASS: ProcessClassType(T);

      typeBOOLEAN,
{$IFNDEF PAXARM}
      typeANSICHAR,
      typeANSISTRING,
      typeWIDESTRING,
      typeSHORTSTRING,
{$ENDIF}
      typeBYTE, typeWORD,
      typeINTEGER, typeDOUBLE,
      typeSINGLE, typeEXTENDED, typeCURRENCY,
      typeCLASSREF, typeWIDECHAR,
      typeVARIANT, typeOLEVARIANT,
      typeDYNARRAY, typeOPENARRAY,
      typeINT64, typeUINT64, typeINTERFACE, typeCARDINAL,
      typeSMALLINT, typeSHORTINT, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL,
      typeENUM, typePOINTER, typePROC, typeEVENT,
      typeSET: begin end;

      typeTYPEPARAM: begin end;
      typeHELPER: begin end;

      typeARRAY: ProcessArrayType(T);
      typeALIAS: ProcessType(Records[T].PatternID);
    else
      RaiseError(errInternalError, []);
    end;

  //  Records[T].Completed := true;
  end;

var
  I, J, OwnerId, PatternId, ArrayTypeId, RangeTypeId, ElemTypeId: Integer;
  S, SP, SL, H1, TypeID, SZ, RegCount, NP, K, FT: Integer;
  ExtraParamNeeded, IsMethod: Boolean;
  RI, RJ: TSymbolRec;
  RI_FinalTypeId: Integer;
  K1, K2, KK: Integer;
  RI_Kind: Integer;
  KK1, KK2: Integer;
  LL: TIntegerList;
  RSPOffset, SubRSPSize: Integer;
begin
  KK1 := 1;
  KK2 := 2;
  if IsExtraTable then
  begin
    KK1 := 3;
    KK2 := 3;
  end;
  LL := TIntegerList.Create;

  if LinkCard > 0 then
    LastShiftValue := GetDataSize(LinkCard)
  else
    LastShiftValue := GetDataSize(LinkCard);

  S := LastShiftValue;

  SetInCode;

  for KK := KK1 to KK2 do
  begin

    if KK = 1 then
    begin
      K1 := ResultId + 1;
      K2 := GlobalST.Card;
    end
    else if KK = 2 then
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end
    else // KK = 3
    begin
      K1 := LinkCard;
      K2 := Card;
    end;

    for I := K1 to K2 do
    begin
      RI := Records[I];

      RI_Kind := RI.Kind;

      if RI_Kind = kindPROP then
        continue;

      if KK = 1 then
        if (RI.Shift <> 0) and (not RI.Host) then
        begin
          if RI_Kind = KindCONST then
          begin
{$IFNDEF PAXARM}
            if RI.HasPAnsiCharType then
            begin
              if InCode[I] then
                TKernel(kernel).Code.Add(OP_INIT_PANSICHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);
            end
            else
{$ENDIF}
            if RI.HasPWideCharType then
            begin
              if InCode[I] then
                TKernel(kernel).Code.Add(OP_INIT_PWIDECHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);
            end;
          end;

          continue;
        end;

      if KK = 2 then
        if I <= LinkCard then
        if (RI.Shift <> 0) and (not RI.Host) then
        begin
          if RI_Kind = KindCONST then
          begin
{$IFNDEF PAXARM}
            if RI.HasPAnsiCharType then
            begin
              if InCode[I] then
                TKernel(kernel).Code.Add(OP_INIT_PANSICHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);
            end
            else
{$ENDIF}
            if RI.HasPWideCharType then
            begin
              if InCode[I] then
                TKernel(kernel).Code.Add(OP_INIT_PWIDECHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);
            end;
          end;

          continue;
        end;

      if RI.UnionId <> 0 then
      begin
        RI.Shift := Records[RI.UnionId].Shift;
        if RI.UnionId > I then
          LL.Add(I);
        continue;
      end;

      RI_FinalTypeId := RI.FinalTypeId;

      if RI.Host then
      begin
        case RI_Kind of
          kindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
          begin
            if RI.CallConv = cc64 then
            begin
              NP := RI.Count;

              if NP = 0 then
                continue;

              RegCount := 0;
              RSPOffset := $20;

              if RI.ExtraParamNeeded then
                Inc(RegCount);
              if (RI.IsMethod or RI.IsSubPartOfEventType) and (RI.CallMode <> cmSTATIC) then
                Inc(RegCount);

              if I = JS_AlertId then
                if RI.Name = 'alert' then
                  Inc(RegCount);

              for J:=I + 1 to GetParamId(I, NP - 1) do
              begin
                RJ := Records[J];

                if RJ.Level <> I then
                  break;
                if RJ.Param then
                begin
                  FT := RJ.FinalTypeId;

                  if FT in RealTypes then
                  begin
                    Inc(RegCount);

                    case RegCount of
                      1: RJ.XMMReg := _XMM0;
                      2: RJ.XMMReg := _XMM1;
                      3: RJ.XMMReg := _XMM2;
                      4: RJ.XMMReg := _XMM3;
                      else
                        begin
                          RJ.RSPOffset := RSPOffset;
                          Inc(RSPOffset, 8);
                        end;
                    end;
                    continue;
                  end;

                  if FT in [typeRECORD, typeARRAY] then
                  begin
                    if RJ.Size > 4 then
                      RJ.ByRefEx := true;
                  end;

                  if RJ.ByRef or RJ.ByRefEx
                    or
                    (
                    (RJ.Size <= SizeOfPointer) and (not (FT in RealTypes))
                    )
                    or
                   (FT in [
{$IFNDEF PAXARM}
                      typeSHORTSTRING,
{$ENDIF}
                      typeVARIANT, typeOLEVARIANT, typeEVENT, typeSET])
                   then
                  begin
                    Inc(RegCount);

                    if RI_Kind = KindCONSTRUCTOR then
                    begin
                      if Records[RI.Level].FinalTypeId = typeRECORD then
                        case RegCount of
                          1: RJ.Register := _EDX;
                          2: RJ.Register := _R8;
                          3: RJ.Register := _R9;
                          else
                            begin
                              RJ.RSPOffset := RSPOffset;
                              Inc(RSPOffset, 8);
                            end;
                        end
                      else
                        case RegCount of
                          1: RJ.Register := _R8;
                          2: RJ.Register := _R9;
                          else
                            begin
                              RJ.RSPOffset := RSPOffset;
                              Inc(RSPOffset, 8);
                            end;
                        end;
                    end
                    else
                        case RegCount of
                          1: RJ.Register := _ECX;
                          2: RJ.Register := _EDX;
                          3: RJ.Register := _R8;
                          4: RJ.Register := _R9;
                          else
                            begin
                              RJ.RSPOffset := RSPOffset;
                              Inc(RSPOffset, 8);
                            end;
                        end;

                     if FT in [typeDYNARRAY, typeOPENARRAY] then
                     if RJ.IsOpenArray then
                     begin
                       Inc(RegCount);
                       Inc(RSPOffset, 8);
                     end;
                  end;
                end;
              end;
            end // cc64
            else if RI.CallConv = ccREGISTER then
            begin
              NP := RI.Count;

              if NP = 0 then
                continue;

              RegCount := 0;
              for J:=I + 1 to GetParamId(I, NP - 1) do
              begin
                RJ := Records[J];

                if RJ.Level <> I then
                  break;
                if RJ.Param then
                begin
                  FT := RJ.FinalTypeId;
                  if FT in [typeRECORD, typeARRAY] then
                  begin
                    if RJ.Size > 4 then
                      RJ.ByRefEx := true;
                  end;

                  if RJ.ByRef or RJ.ByRefEx
                    or
                    (
                    (RJ.Size <= 4) and (FT <> typeSINGLE)
                    )
                    or
                   (FT in [
{$IFNDEF PAXARM}
                     typeSHORTSTRING,
{$ENDIF}
                     typeVARIANT, typeOLEVARIANT, typeEVENT, typeSET])
                   then
                  begin
                    Inc(RegCount);

                    if RI_Kind = KindCONSTRUCTOR then
                    begin
                      if Records[RI.Level].FinalTypeId = typeRECORD then
                        case RegCount of
                          1: RJ.Register := _EDX;
                          2: RJ.Register := _ECX;
                        end
                      else
                        case RegCount of
                          1: RJ.Register := _ECX;
                        end;
                    end
                    else
                      if (RI.IsMethod or RI.IsSubPartOfEventType) and
                         (RI.CallMode <> cmSTATIC) then
                        case RegCount of
                          1: RJ.Register := _EDX;
                          2: RJ.Register := _ECX;
                        end
                      else
                        case RegCount of
                          1: RJ.Register := _EAX;
                          2: RJ.Register := _EDX;
                          3: RJ.Register := _ECX;
                        end;

                     if FT in [typeDYNARRAY, typeOPENARRAY] then
                     if RJ.IsOpenArray then
                       Inc(RegCount);
                  end;
                end;
              end;
            end // ccREGISTER
            else
            if RI.CallConv = ccMSFASTCALL then
            begin
              NP := RI.Count;

              if NP = 0 then
                continue;

              RegCount := 1;
              for J:=I + 1 to GetParamId(I, NP - 1) do
              begin
                RJ := Records[J];

                if RJ.Level <> I then
                  break;

                if RJ.Param then
                begin
                  if RJ.ByRef
                    or
                    (
                    (RJ.Size <= 4) and (RJ.FinalTypeId <> typeSINGLE)
                    )
                    or
                   (RJ.FinalTypeId in [
{$IFNDEF PAXARM}
                     typeSHORTSTRING,
{$ENDIF}
                     typeVARIANT, typeOLEVARIANT])
                   then
                  begin
                    Inc(RegCount);

                    if RI_Kind = KindCONSTRUCTOR then
                      case RegCount of
                        1: RJ.Register := _ECX;
                      end
                    else
                      if (RI.IsMethod) and (RI.CallMode <> cmSTATIC) then
                        case RegCount of
                          1: RJ.Register := _ECX;
                        end
                      else
                        case RegCount of
                          2: RJ.Register := _ECX;
                          3: RJ.Register := _EDX;
                        end;

                     if RJ.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
                     if RJ.IsOpenArray then
                       Inc(RegCount);
                  end;
                end;
              end;
            end; // ccMS_FASTCALL
          end; // kindSUB, KindCONSTRUCTOR, KindDESTRUCTOR (HOST)
          KindTYPE:
          begin
            RI.Completed := true;
          end;
        end // case
      end
      else  // not host
      case RI_Kind of
        kindVAR:
        begin
          if RI.Param then
          begin
            // already done
          end
          else if RI.Local then
          begin
            // already done
          end
          else  // global
          begin
            if RI.InternalField then
            begin
              OwnerId := RI.OwnerId;
              TypeId := Records[OwnerId].FinalTypeId;
              case TypeId of
                typeRECORD:
                begin
                  PatternId := RI.PatternId;
                  RI.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                end;
                typeARRAY:
                begin
                  ArrayTypeId := Records[OwnerId].TerminalTypeId;

                  GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                  H1 := GetLowBoundRec(RangeTypeId).Value;

                  RI.Shift := Records[OwnerId].Shift +
                         Records[ElemTypeId].Size * (RI.Value - H1);
                end;
                else
                begin
                  RI.Kind := kindNONE;
                  RI.Name := '';
                  RI.OwnerId := 0;
                  continue;

//                RaiseError(errInternalError, []);
                end;
              end;
            end
            else
            begin
              TypeId := RI.TypeId;

              if TypeId = 0 then
                RI.Kind := kindNONE
              else
              begin
                ProcessType(TypeId);
                RI.Shift := S;
                Inc(S, RI.Size);
              end;
            end;
          end;
        end;
        kindCONST:
        begin
          if RI_FinalTypeId = typeDOUBLE then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Double));
          end
          else if RI_FinalTypeId = typeSINGLE then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Single));
          end
          else if RI_FinalTypeId = typeEXTENDED then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Extended));
          end
          else if RI_FinalTypeId = typeCURRENCY then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Currency));
          end
          else if RI_FinalTypeId in INT64Types then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Int64));
          end
          else if RI_FinalTypeId = typeRECORD then
          begin
            RI.Shift := S;
            Inc(S, RI.Size);
          end
          else if RI_FinalTypeId = typeARRAY then
          begin
            RI.Shift := S;
            Inc(S, RI.Size);
          end
          else if RI_FinalTypeId in VariantTypes then
          begin
            RI.Shift := S;
            Inc(S, SizeOf(Variant));
          end
          else if RI_FinalTypeId = typeSET then
          begin
            RI.Shift := S;
            ProcessType(RI.TypeId);
            Inc(S, RI.Size);
          end
{$IFNDEF PAXARM}
          else if RI.HasPAnsiCharType then
          begin
            RI.Shift := S;

            Inc(S, SizeOfPointer); // pointer to string literal
            Inc(S, SizeOfPointer); // ref counter
            Inc(S, SizeOfPointer); // length

            if InCode[I] then
              TKernel(kernel).Code.Add(OP_INIT_PANSICHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);

            // reserve place for literal
            Inc(S, Length(RI.Value) + 1);
          end
{$ENDIF}
          else if RI.HasPWideCharType then
          begin
            RI.Shift := S;

            Inc(S, SizeOfPointer); // pointer to string literal
            Inc(S, SizeOfPointer); // length

            if InCode[I] then
              TKernel(kernel).Code.Add(OP_INIT_PWIDECHAR_LITERAL, I, 0, 0, 0, true, PASCAL_LANGUAGE, 0, -1);

            // reserve place for literal
            Inc(S, Length(RI.Value) * 2 + 2);
          end
          else
          begin
            if RI.MustBeAllocated then
            begin
              RI.Shift := S;
              Inc(S, RI.Size);
            end;
          end;
        end;

        kindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
        begin
          RI.Shift := S;
          Inc(S, SizeOfPointer);

          SP := 8 + 3 * 4;
          SL := 0;

          ExtraParamNeeded := RI.ExtraParamNeeded;

          IsMethod := (Records[GetSelfId(I)].Name <> '') and
                      (Records[I].CallMode <> cmSTATIC);

          if ExtraParamNeeded then
          if RI.CallConv in [ccSTDCALL, ccSAFECALL, ccCDECL] then
            Inc(SP, 4);

          if RI.CallConv in [ccSTDCALL, ccSAFECALL, ccCDECL] then
          begin
            for J:=I + 1 to Card do
            begin
              if ExtraParamNeeded then
              begin
                if J = GetResultId(I) then
                  continue;
              end;

              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.UnionId <> 0 then
              begin
                RJ.Shift := Records[RJ.UnionId].Shift;
                if RI.UnionId > I then
                  LL.Add(I);
                continue;
              end;

              if RJ.Param then
              begin
                RJ.Shift := SP;

                if RJ.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
                  if RJ.IsOpenArray then
                    Inc(SP, 4);

//                RJ.Shift := SP;

                FT := RJ.FinalTypeId;

                if FT in [typeRECORD, typeARRAY] then
                begin
                  if RJ.IsConst then
                    if RJ.Size > 4 then
                      RJ.ByRefEx := true;
                end;

                case FT of
{$IFNDEF PAXARM}
                  typeSHORTSTRING: RJ.ByRefEx := true;
{$ENDIF}
                  typeVARIANT, typeOLEVARIANT: RJ.ByRefEx := true;
                end;

                if RJ.ByRef or RJ.ByRefEx then
                   Inc(SP, SizeOfPointer)
                else
                begin
                  TypeId := RJ.TypeId;
                  if not Records[TypeId].Completed then
                    ProcessType(TypeId);

                  SZ := RJ.Size;

                  while SZ mod 4 <> 0 do
                    Inc(SZ);

                  Inc(SP, SZ);
                end;
              end
              else if RJ.Local then
              begin
                TypeId := RJ.TypeId;
                if not Records[TypeId].Completed then
                  ProcessType(TypeId);

                if RJ.FinalTypeId = typeSET then
                  Dec(SL, SizeOf(TByteSet))
                else
                  Dec(SL, MPtr(RJ.Size));
                RJ.Shift := SL;
              end
              else if RJ.InternalField then
              begin
                OwnerId := RJ.OwnerId;
                TypeId := Records[OwnerId].FinalTypeId;
                case TypeId of
                  typeRECORD:
                  begin
                    PatternId := RJ.PatternId;
                    RJ.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                  end;
                  typeARRAY:
                  begin
                    ArrayTypeId := Records[OwnerId].TerminalTypeId;
                    GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                    H1 := GetLowBoundRec(RangeTypeId).Value;
                    RJ.Shift := Records[OwnerId].Shift +
                          Records[ElemTypeId].Size * (RJ.Value - H1);
                  end;
                  else
                    RaiseError(errInternalError, []);
                end;
              end
              else if RJ.Kind = KindTYPE then
                ProcessType(J);
            end; // for

            if ExtraParamNeeded then
            begin
              J := GetResultId(I);

              TypeId := Records[J].TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              Records[J].Shift := 8 + 3 * 4;
              Records[J].ByRef := true;
            end; // J-loop

            if RI.CallConv = ccSAFECALL then
            begin
              J := GetResultId(I);

              RJ := Records[J];

              TypeId := RJ.TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              RJ.Shift := GetSizeOfParams(I) + 4;
              RJ.ByRef := true;
            end;

          end // of ccSTDCALL, ccCDECL ccSAFECALL
          else if RI.CallConv = ccPASCAL then
          begin
            if IsMethod then
              Inc(SP, 4);

            for J:=Card downto I + 1 do
            begin
              if ExtraParamNeeded then
              begin
                if J = GetResultId(I) then
                  continue;
              end;

              if IsMethod then
              begin
                if J = GetSelfId(I) then
                  continue;
              end;

              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.UnionId <> 0 then
              begin
                RJ.Shift := Records[RJ.UnionId].Shift;
                if RI.UnionId > I then
                  LL.Add(I);
                continue;
              end;

              if RJ.Param then
              begin
                RJ.Shift := SP;

                FT := RJ.FinalTypeId;
                if FT in [typeRECORD, typeARRAY] then
                begin
                  if RJ.IsConst then
                    if RJ.Size > 4 then
                      RJ.ByRefEx := true;
                end;

                if FT in [
{$IFNDEF PAXARM}
                          typeSHORTSTRING,
{$ENDIF}
                          typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                if RJ.ByRef or RJ.ByRefEx then
                   Inc(SP, SizeOfPointer)
                else
                begin

                  TypeId := RJ.TypeId;

                  if not Records[TypeId].Completed then
                    ProcessType(TypeId);

                  SZ := RJ.Size;

                  if RJ.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
                  if RJ.IsOpenArray then
                    Inc(SP, 4);

                  while SZ mod 4 <> 0 do
                    Inc(SZ);

                  Inc(SP, SZ);
                end;
              end
              else if RJ.Local then
              begin
                TypeId := RJ.TypeId;
                if not Records[TypeId].Completed then
                  ProcessType(TypeId);

                if RJ.FinalTypeId = typeSET then
                  Dec(SL, SizeOf(TByteSet))
                else
                  Dec(SL, MPtr(RJ.Size));
                RJ.Shift := SL;
              end
              else if RJ.InternalField then
              begin
                continue;
              end
              else if RJ.Kind = KindTYPE then
                ProcessType(J);
            end; // for loop - J

            for J:=Card downto I + 1 do
            begin
              RJ := Records[J];
              if RJ.InternalField then
              begin
                OwnerId := RJ.OwnerId;
                TypeId := Records[OwnerId].FinalTypeId;
                case TypeId of
                  typeRECORD:
                  begin
                    PatternId := RJ.PatternId;
                    RJ.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                  end;
                  typeARRAY:
                  begin
                    ArrayTypeId := Records[OwnerId].TerminalTypeId;
                    GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                    H1 := GetLowBoundRec(RangeTypeId).Value;
                    RJ.Shift := Records[OwnerId].Shift +
                        Records[ElemTypeId].Size * (RJ.Value - H1);
                  end;
                  else
                    RaiseError(errInternalError, []);
                end;
              end
            end;

            if IsMethod then
            begin
              J := GetSelfId(I);
              Records[J].Shift := 8;
            end;

            if ExtraParamNeeded then
            begin
              J := GetResultId(I);

              RJ := Records[J];

              TypeId := RJ.TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              if IsMethod then
                RJ.Shift := 12
              else
                RJ.Shift := 8;

              RJ.ByRef := true;
            end;

          end // ccPASCAL
          else if RI.CallConv = ccREGISTER then
          begin
            RegCount := 0;

            if RI.IsMethod or RI.IsSubPartOfEventType or
               RI.IsConstructor or RI.IsDestructor then
            if RI.CallMode <> cmSTATIC then
            begin
              J := GetSelfId(I);
              Records[J].Register := _EAX;
              Inc(RegCount);

              if RI.IsConstructor then //!!
              begin
                if Records[RI.Level].FinalTypeId <> typeRECORD then
                  Inc(RegCount);
              end;

              Dec(SL, 4);
              Records[J].Shift := SL;
            end;

            for NP := 0 to RI.Count - 1 do
            begin
              J := GetParamId(I, NP);

              RJ := Records[J];

              FT := RJ.FinalTypeId;

              if FT in [typeRECORD, typeARRAY] then
                 if RJ.Size > 4 then
                   RJ.ByRefEx := true;

              if RJ.ByRef or RJ.ByRefEx
                or
                (
                 (RJ.Size <= 4) and (FT <> typeSINGLE)
                 )
                or
                (FT in [
{$IFNDEF PAXARM}
                        typeSHORTSTRING,
{$ENDIF}
                        typeVARIANT, typeOLEVARIANT])
               then
               begin

                if FT in [
{$IFNDEF PAXARM}
                          typeSHORTSTRING,
{$ENDIF}
                          typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                Inc(RegCount);

                case RegCount of
                  1: RJ.Register := _EAX;
                  2: RJ.Register := _EDX;
                  3: RJ.Register := _ECX;
                end;

                if RegCount <= 3 then
                begin
                  Dec(SL, 4);
                  RJ.Shift := SL;
                end;

                if FT in [typeDYNARRAY, typeOPENARRAY] then
                if RJ.IsOpenArray then
                begin
                  Inc(RegCount);
                end;

              end;
            end; // set register params

            K := I + 1;

            if ExtraParamNeeded then
            begin
              Inc(K);

              J := GetResultId(I);

              RJ := Records[J];

              TypeId := RJ.TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              if RegCount >= 3 then
              begin
                RJ.Shift := SP;
                RJ.ByRef := true;
                Inc(SP, 4);
              end
              else
              begin
                Inc(RegCount);
                case RegCount of
                  1: RJ.Register := _EAX;
                  2: RJ.Register := _EDX;
                  3: RJ.Register := _ECX;
                end;
                Dec(SL, 4);
                RJ.Shift := SL;
              end;

              RJ.ByRef := true;
            end;

            for J:=Card downto K do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.Register > 0 then
                continue;

              if RJ.UnionId <> 0 then
              begin
                RJ.Shift := Records[RJ.UnionId].Shift;
                if RI.UnionId > I then
                  LL.Add(I);
                continue;
              end;

              if RJ.Param then
              begin
                if RJ.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
                if RJ.IsOpenArray then
                  Inc(SP, 4);

                RJ.Shift := SP;

                if RJ.FinalTypeId in
                  [
{$IFNDEF PAXARM}
                    typeSHORTSTRING,
{$ENDIF}
                    typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                if RJ.ByRef or RJ.ByRefEx then
                   Inc(SP, SizeOfPointer)
                else
                begin
                  TypeId := RJ.TypeId;
                  if not Records[TypeId].Completed then
                    ProcessType(TypeId);

                  SZ := RJ.Size;

                  while SZ mod 4 <> 0 do
                    Inc(SZ);

                  Inc(SP, SZ);
                end;
              end
              else if RJ.Local then
              begin
                TypeId := RJ.TypeId;
                if not Records[TypeId].Completed then
                  ProcessType(TypeId);

                if RJ.FinalTypeId = typeSET then
                  Dec(SL, SizeOf(TByteSet))
                else
                  Dec(SL, MPtr(RJ.Size));
                RJ.Shift := SL;
              end
              else if RJ.InternalField then
              begin
                continue; // later
              end
              else if RJ.Kind = KindTYPE then
                ProcessType(J);
            end; // for


            for J:=Card downto K do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.Register > 0 then
                continue;

              if RJ.InternalField then
              begin
                OwnerId := RJ.OwnerId;
                TypeId := Records[OwnerId].FinalTypeId;
                case TypeId of
                  typeRECORD:
                  begin
                    PatternId := RJ.PatternId;
                    RJ.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                  end;
                  typeARRAY:
                  begin
                    ArrayTypeId := Records[OwnerId].TerminalTypeId;
                    GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                    H1 := GetLowBoundRec(RangeTypeId).Value;
                    Records[J].Shift := Records[OwnerId].Shift +
                          Records[ElemTypeId].Size * (RJ.Value - H1);
                  end;
                  else
                  begin
                    RJ.Kind := kindNONE;
                    RJ.Name := '';
                    RJ.OwnerId := 0;
                    continue;

  //                RaiseError(errInternalError, []);
                  end;
                end;
              end;
            end; // for

          end // ccREGISTER

          else if RI.CallConv = cc64 then
          begin
            RegCount := 0;
            RSPOffset := $20;

            SL := $100;

            if RI.IsMethod or
               RI.IsSubPartOfEventType or
               RI.IsJSFunction or
               RI.IsConstructor or RI.IsDestructor then
            if RI.CallMode <> cmSTATIC then
            begin
              Inc(RegCount);

              J := GetSelfId(I);
              RJ := Records[J];
              case RegCount of
                1: RJ.Register := _ECX;
                2: RJ.Register := _EDX;
              else
                RaiseError(errInternalError, []);
              end;

              if RI.IsConstructor then //!!
              begin
                if Records[RI.Level].FinalTypeId <> typeRECORD then
                begin
                  J := GetDL_Id(I);
                  Records[J].Shift := SL;
                  Inc(SL, 8);

                  Inc(RegCount);
                end;
              end;
            end;

            if ExtraParamNeeded then
            begin
              J := GetResultId(I);
              RJ := Records[J];

              TypeId := RJ.TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              Inc(RegCount);
              case RegCount of
                1: RJ.Register := _ECX;
                2: RJ.Register := _EDX;
              else
                RaiseError(errInternalError, []);
              end;

              RJ.ByRef := true;
            end;

            if RI.IsNestedSub and (not RI.IsJSFunction) then
            begin
              J := GetRBP_Id(I);
              Records[J].Shift := SL;
              Inc(SL, 8);
              Inc(RegCount);
            end;

            // set params

            for NP := 0 to RI.Count - 1 do
            begin
              J := GetParamId(I, NP);

              RJ := Records[J];
              if not Records[RJ.TypeId].Completed then
                ProcessType(RJ.TypeId);

              FT := RJ.FinalTypeId;

              if FT in [typeRECORD, typeARRAY] then
                 if RJ.Size > 4 then
                   RJ.ByRefEx := true;

              if (FT in RealTypes) and (not (RJ.ByRef or RJ.ByRefEx)) then
              begin
                Inc(RegCount);
                case RegCount of
                  1: RJ.XMMReg := _XMM0;
                  2: RJ.XMMReg := _XMM1;
                  3: RJ.XMMReg := _XMM2;
                  4: RJ.XMMReg := _XMM3;
                  else
                    begin
                      RJ.RSPOffset := RSPOffset;
                      Inc(RSPOffset, 8);
                    end;
                end;
              end
              else if RJ.ByRef or RJ.ByRefEx
                or
                (
                (RJ.Size <= SizeOfPointer) and (not (FT in RealTypes))
                )
                or
               (FT in [
{$IFNDEF PAXARM}
                       typeSHORTSTRING,
{$ENDIF}
                       typeVARIANT, typeOLEVARIANT, typeEVENT, typeSET])
              then
              begin
                if FT in [
{$IFNDEF PAXARM}
                          typeSHORTSTRING,
{$ENDIF}
                          typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                Inc(RegCount);

                case RegCount of
                  1: RJ.Register := _ECX;
                  2: RJ.Register := _EDX;
                  3: RJ.Register := _R8;
                  4: RJ.Register := _R9;
                  else
                    begin
                      RJ.RSPOffset := RSPOffset;
                      Inc(RSPOffset, 8);
                    end;
                end;

                if FT in [typeDYNARRAY, typeOPENARRAY] then
                if RJ.IsOpenArray then
                begin
                  Inc(RegCount);
                end;
              end
              else
              begin
                RJ.RSPOffset := RSPOffset;
                Inc(RSPOffset, 8);
              end;
            end; // set register params

            K := I + 1;

            for J:=Card downto K do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;
              if RJ.Param then
                continue;

              if RJ.Local then
              begin
                TypeId := RJ.TypeId;
                if not Records[TypeId].Completed then
                  ProcessType(TypeId);
              end;
            end;

            SubRSPSize := GetSubRSPSize(I);
            SP := SubRSPSize + $10 + 24;

            J := GetResultId(I);
            RJ := Records[J];
            case RJ.Register of
              _ECX: RJ.Shift := SP;
              _EDX: RJ.Shift := SP + 8;
            else
              begin
                RJ.Shift := SL;
                Inc(SL, 8);
              end;
            end;

            J := GetSelfId(I);
            RJ := Records[J];
            case RJ.Register of
              _ECX: RJ.Shift := SP;
              _EDX: RJ.Shift := SP + 8;
            end;

            K := J + 1;

            for NP := 0 to RI.Count - 1 do
            begin
              J := GetParamId(I, NP);
              RJ := Records[J];
              if RJ.Register = _ECX then
                RJ.Shift := SP
              else if RJ.Register = _EDX then
                RJ.Shift := SP + 8
              else if RJ.Register = _R8 then
                RJ.Shift := SP + 16
              else if RJ.Register = _R9 then
                RJ.Shift  := SP + 24
              else if RJ.XMMReg = _XMM0 then
                RJ.Shift := SP
              else if RJ.XMMReg = _XMM1 then
                RJ.Shift := SP + 8
              else if RJ.XMMReg = _XMM2 then
                RJ.Shift := SP + 16
              else if RJ.XMMReg = _XMM3 then
                RJ.Shift  := SP + 24
              else
                RJ.Shift := SP + RJ.RSPOffset;
            end;

            for J:=K to Card do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;
              if RJ.Param then
                continue;

              if RJ.UnionId <> 0 then
              begin
                RJ.Shift := Records[RJ.UnionId].Shift;
                if RI.UnionId > I then
                  LL.Add(I);
                continue;
              end;

              if RJ.Local then
              begin
                RJ.Shift := SL;
                Inc(SL, MPtr(RJ.Size));
              end
              else if RJ.InternalField then
              begin
                OwnerId := RJ.OwnerId;
                TypeId := Records[OwnerId].FinalTypeId;
                case TypeId of
                  typeRECORD:
                  begin
                    PatternId := RJ.PatternId;
                    RJ.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                  end;
                  typeARRAY:
                  begin
                    ArrayTypeId := Records[OwnerId].TerminalTypeId;
                    GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                    H1 := GetLowBoundRec(RangeTypeId).Value;
                    Records[J].Shift := Records[OwnerId].Shift +
                          Records[ElemTypeId].Size * (RJ.Value - H1);
                  end;
                  else
                  begin
                    RJ.Kind := kindNONE;
                    RJ.Name := '';
                    RJ.OwnerId := 0;
                    continue;
                  end;
                end;
              end
              else if RJ.Kind = KindTYPE then
                ProcessType(J);
            end; // for
          end // cc64

          else if RI.CallConv = ccMSFASTCALL then
          begin
            RegCount := 1;

  {
            if Records[I].IsMethod or Records[I].IsConstructor or Records[I].IsDestructor then
            begin
              J := GetSelfId(I);
              Records[J].Register := _EAX;
              Inc(RegCount);

              Dec(SL, 4);
              Records[J].Shift := SL;
            end;
  }

            for NP := 0 to RI.Count - 1 do
            begin
              J := GetParamId(I, NP);

              RJ := Records[J];

              FT := RJ.FinalTypeId;
              if FT in [typeRECORD, typeARRAY] then
              begin
                if RJ.IsConst then
                  if RJ.Size > 4 then
                    RJ.ByRefEx := true;
              end;

              if RJ.ByRef or RJ.ByRefEx
                or
                (
                 (RJ.Size <= 4) and (FT <> typeSINGLE)
                 )
                or
                (FT in [
{$IFNDEF PAXARM}
                         typeSHORTSTRING,
{$ENDIF}
                         typeVARIANT, typeOLEVARIANT])
               then
               begin

                if FT in [
{$IFNDEF PAXARM}
                           typeSHORTSTRING,
{$ENDIF}
                           typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                Inc(RegCount);

                case RegCount of
                  2: RJ.Register := _ECX;
                  3: RJ.Register := _EDX;
                end;

                Dec(SL, 4);
                RJ.Shift := SL;

              end;
            end; // set register params

            K := I + 1;

            if ExtraParamNeeded then
            begin
              Inc(K);

              J := GetResultId(I);

              RJ := Records[J];

              TypeId := RJ.TypeId;
              if not Records[TypeId].Completed then
                ProcessType(TypeId);

              if RegCount >= 3 then
              begin
                RJ.Shift := 8;
                RJ.ByRef := true;
              end
              else
              begin
                Inc(RegCount);
                case RegCount of
                  2: RJ.Register := _ECX;
                  3: RJ.Register := _EDX;
                end;
                Dec(SL, 4);
                RJ.Shift := SL;
              end;

              RJ.ByRef := true;
            end
            else
            begin
              J := GetResultId(I);
              Records[J].ByRef := false;
            end;

            for J:=Card downto K do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.Register > 0 then
                continue;

              if RJ.UnionId <> 0 then
              begin
                RJ.Shift := Records[RJ.UnionId].Shift;
                if RI.UnionId > I then
                  LL.Add(I);
                continue;
              end;

              if RJ.Param then
              begin
                if RJ.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
                  if RJ.IsOpenArray then
                    Inc(SP, 4);

                RJ.Shift := SP;

                if RJ.FinalTypeId in
                  [
{$IFNDEF PAXARM}
                    typeSHORTSTRING,
{$ENDIF}
                    typeVARIANT, typeOLEVARIANT] then
                  RJ.ByRefEx := true;

                if RJ.ByRef or RJ.ByRefEx then
                   Inc(SP, SizeOfPointer)
                else
                begin
                  TypeId := RJ.TypeId;
                  if not Records[TypeId].Completed then
                    ProcessType(TypeId);

                  SZ := RJ.Size;

                  while SZ mod 4 <> 0 do
                    Inc(SZ);

                  Inc(SP, SZ);
                end;
              end
              else if RJ.Local then
              begin
                TypeId := RJ.TypeId;
                if not Records[TypeId].Completed then
                  ProcessType(TypeId);

                if RJ.FinalTypeId = typeSET then
                  Dec(SL, SizeOf(TByteSet))
                else
                  Dec(SL, MPtr(RJ.Size));
                RJ.Shift := SL;
              end
              else if RJ.InternalField then
              begin
                continue;
              end
              else if Records[J].Kind = KindTYPE then
                ProcessType(J);
            end; // for

            for J:=Card downto K do
            begin
              RJ := Records[J];

              if RJ.Level <> I then
                continue;

              if RJ.Register > 0 then
                continue;

              if RJ.InternalField then
              begin
                OwnerId := RJ.OwnerId;
                TypeId := Records[OwnerId].FinalTypeId;
                case TypeId of
                  typeRECORD:
                  begin
                    PatternId := Records[J].PatternId;
                    RJ.Shift := Records[OwnerId].Shift + Records[PatternId].Shift;
                  end;
                  typeARRAY:
                  begin
                    ArrayTypeId := Records[OwnerId].TerminalTypeId;
                    GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
                    H1 := GetLowBoundRec(RangeTypeId).Value;
                    Records[J].Shift := Records[OwnerId].Shift +
                          Records[ElemTypeId].Size * (RJ.Value - H1);
                  end;
                  else
                    RaiseError(errInternalError, []);
                end;
              end;
            end; // for

          end // ccMS_FASTCALL
          else
            RaiseError(errInternalError, []);

        end; // KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR (Script)
        kindTYPE:
          ProcessType(I);
      end;
    end; // loop - I
  end; // loop KK

  for J := 0 to LL.Count - 1 do
  begin
    I := LL[J];
    RI := Records[I];
    RI.Shift := Records[RI.UnionId].Shift;
  end;

  FreeAndNil(LL);
end;

procedure TSymbolTable.RaiseError(const Message: string; params: array of Const);
begin
  TKernel(kernel).RaiseError(Message, params);
end;

procedure TSymbolTable.CreateOffsets(JS1, JS2: Integer);
var
  KK, K1, K2, I, S, SZ, OwnerId, OwnerOffset, D, L: Integer;
  RI: TSymbolRec;
  OffsetList: TOffsetList;
  SignClassHeader: Boolean;
  Id_GetOLEProperty, Id_SetOLEProperty: Integer;
  HostMapTable: TMapTable;
  Id: Integer;
  ClassLst, SubLst: TIntegerList;
  InCodeClass: Boolean;
  MapRec: TMapRec;
begin
  HostMapTable := TKernel(kernel).prog.HostMapTable;
  ClassLst := TIntegerList.Create;
  SubLst := TIntegerList.Create;

  for I := 0 to HostMapTable.Count - 1 do
    case HostMapTable[I].Kind of
      KindTYPE:
      begin
        Id := LookupFullName(HostMapTable[I].FullName, true);
        if Id > 0 then
          if Records[Id].FinalTypeId = typeCLASS then
            ClassLst.Add(Id);
      end;
      KindSUB, KindCONSTRUCTOR, KindDESTRUCTOR:
      begin
        MapRec := HostMapTable[I];
        Id := MapRec.SubDesc.SId;
        if Id > 0 then
          SubLst.Add(Id);
      end;
    end;

  try

    Id_GetOLEProperty := Lookup(_strGetOLEProperty, 0, false);
    Id_SetOLEProperty := Lookup(_strSetOLEProperty, 0, false);

    OffsetList := TKernel(kernel).OffsetList;

    SignClassHeader := false;
    InCodeClass := false;

    S := StdSize;

    OffsetList.Clear;
    OffsetList.InitSize := S;

    for I:= JS1 to JS2 do
      if (I > 0) and (I < System.Length(InCode)) then
        InCode[I] := true;

    I := Id_GetOLEProperty;
    if (I > 0) and (I < System.Length(InCode)) then
      InCode[I] := true;
    SubLst.Add(I);

    I := Id_SetOLEProperty;
    if (I > 0) and (I < System.Length(InCode)) then
      InCode[I] := true;
    SubLst.Add(I);

    for KK := 1 to 2 do
    begin
      if KK = 1 then
      begin
        K1 := StdCard;
        K2 := GlobalST.Card;
      end
      else
      begin
        K1 := FirstLocalId + 1;
        K2 := Card;
      end;

      I := K1 - 1;
      repeat
        Inc(I);
        if I > K2 then
          break;

        RI := Records[I];

        if RI.Host then
        begin
          if RI.Kind = KindTYPE then
          if RI.FinalTypeId = typeCLASS then
          begin
            SignClassHeader := true;
            InCodeClass := ClassLst.IndexOf(I) >= 0;

            if InCodeClass then
              InCode[I] := true;
          end;

          if RI.Kind = kindTYPE then
            if RI.TypeID = typeINTERFACE then
              if InCode[I] then
              begin
                InCode[I + 1] := true;
                InCode[I + 2] := true;
              end;
        end;

        if SignClassHeader and InCodeClass then
          InCode[I] := true;

        if RI.Kind = kindEND_CLASS_HEADER then
        begin
          SignClassHeader := false;
          InCodeClass := false;
          continue;
        end;

        if I > CompileCard then
        begin
          InCode[I] := true;
          if SignClassHeader and (not InCodeClass) then
            InCode[I] := false;
        end;

        if InCode[I] then
        if (RI.Shift > 0) and
           (RI.Kind in (KindSUBS + [KindVAR, KindCONST])) then
        begin
          if RI.Param or RI.Local or RI.LocalInternalField then
            continue;

          if RI.UnionId > 0 then
            continue;

          SZ := 0;

          if RI.Host then
          begin
            L := RI.Level;
            if RI.Kind in KindSUBS then
            if not (RI.IsSubPartOfEventType or RI.IsSubPartOfProcType) then
            if (L = 0) or (Records[L].FinalTypeId <> typeINTERFACE) then
            begin
              if SubLst.IndexOf(I) = -1 then
              begin
                InCode[I] := false;
                InCode[I+1] := false;
                InCode[I+2] := false;
                Inc(I, 2);
                continue;
              end;
            end;
            SZ := SizeOfPointer;
          end
{$IFNDEF PAXARM}
          else if RI.HasPAnsiCharType then
          begin
            SZ := 0;
            Inc(SZ, SizeOfPointer); // pointer to string literal
            Inc(SZ, SizeOfPointer); // ref counter
            Inc(SZ, SizeOfPointer); // length
            // reserve place for literal
            Inc(SZ, Length(RI.Value) + 1);
          end
{$ENDIF}
          else if RI.HasPWideCharType then
          begin
            SZ := 0;
            Inc(SZ, SizeOfPointer); // pointer to string literal
            Inc(SZ, SizeOfPointer); // length
            // reserve place for literal
            Inc(SZ, Length(RI.Value) * 2 + 2);
          end
          else
            SZ := RI.Size;

          if RI.InternalField then
          begin
            SZ := 0;
            OwnerId := RI.OwnerId;
            D := RI.Shift - Records[OwnerId].Shift;
            OwnerOffset := TKernel(kernel).GetOffset(Records[OwnerId]);
            OffsetList.Add(I, RI.Shift, OwnerOffset + D, SZ);
            continue;
          end;

          SZ := MPtr(SZ);

          OffsetList.Add(I, RI.Shift, S, SZ);
          Inc(S, SZ);
        end;
      until false;
    end;

  finally
    FreeAndNil(ClassLst);
    FreeAndNil(SubLst);
  end;
end;

function TSymbolTable.GetValueAsString(P: Pointer;
                                       StackFrameNumber: Integer;
                                       Id: Integer;
                                       TypeMapRec: TTypeMapRec = nil;
                                       BriefCls: Boolean = false): String;
var
  Address: Pointer;
  TypeId: Integer;
  Code: TCode;
  N: Integer;
begin
  result := '???';

  if not Records[Id].Host then
  if Records[Id].Param then
    if Records[Id].ByRef then
    begin
      Code := TKernel(kernel).Code;
      N := TBaseRunner(P).CurrN;
      if Code.ParamHasBeenChanged(N, Id) then
        Exit;
    end;

  Address := GetFinalAddress(P, StackFrameNumber, Id);
  TypeId := Self[Id].TerminalTypeId;

  if Address = nil then
    Exit;

  result := GetStrVal(Address, TypeId, TypeMapRec, BriefCls);
end;

function TSymbolTable.GetSizeOfParams(SubId: Integer): Integer;
var
  I, J, SZ, RegCount: Integer;
  R: TSymbolRec;
  FinTypeId: Integer;
  MaxRegCount: Integer;
begin
  if Records[SubId].CallConv = cc64 then
    MaxRegCount := 4
  else
    MaxRegCount := 3;

  result := 0;
  RegCount := 0;

  if Records[SubId].IsMethod then
  begin
    if Records[SubId].CallConv in [ccREGISTER, ccMSFASTCALL, cc64] then
      Inc(RegCount)
    else
      Inc(result, 4);
  end;

  for J:=0 to Records[SubId].Count - 1 do
  begin
    I := GetParamId(SubId, J);
    R := Records[I];

    if Records[SubId].CallConv in [ccREGISTER, ccMSFASTCALL, cc64] then
    begin
      if R.Register > 0 then
      begin
        Inc(RegCount);

        if R.FinalTypeId in [typeDYNARRAY, typeOPENARRAY] then
        if R.IsOpenArray then
        begin
          Inc(RegCount);

          if RegCount > MaxRegCount then
            Inc(result, 4);
        end;

        continue;
      end;
    end;

    if R.ByRef or R.ByRefEx then
      Inc(result, SizeOfPointer)
    else
    begin
      SZ := R.Size;

      FinTypeId := R.FinalTypeId;

      if FinTypeId = typeEXTENDED then
        if TKernel(kernel).TargetPlatform in [tpOSX32, tpIOSSim] then
          SZ := 16;

      if FinTypeId in [typeDYNARRAY, typeOPENARRAY] then
      if R.IsOpenArray then
        Inc(SZ, 4);

      while SZ mod 4 <> 0 do
        Inc(SZ);

      Inc(result, SZ);
    end;
  end;

  result := Abs(result);

  if Records[SubId].ExtraParamNeeded then
  begin
    if Records[SubId].CallConv in [ccREGISTER, cc64] then
    begin
      if RegCount >= MaxRegCount then
        Inc(result, 4);
    end
    else
      Inc(result, 4);
  end;

  if Records[SubId].CallConv = ccSAFECALL then
    Inc(result, 4);
end;


end.

