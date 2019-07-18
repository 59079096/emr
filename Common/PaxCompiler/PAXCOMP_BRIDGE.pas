////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_BRIDGE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_BRIDGE;
interface
uses {$I uses.def}
  PAXCOMP_CONSTANTS,
  PAXCOMP_SYS;

function GetFakeAddRefAddress(Index: Integer): Pointer;
function GetFakeReleaseAddress(Index: Integer): Pointer;
function GetFakeHandlerAddress(Index: Integer): Pointer;
function GetFakeGlobalAddress(Index: Integer): Pointer;

implementation

uses
  PAXCOMP_BASERUNNER;

var
  FakeHandlerAddressList,
  FakeGlobalAddressList,
  FakeAddRefAddressList,
  FakeReleaseAddressList: array[0..100] of Pointer;

{$IFDEF PAX64}
{$DEFINE PARAM4}
{$ENDIF}

{$IFDEF PAXARM_DEVICE}
{$DEFINE PARAM4}
{$ENDIF}

{$IFDEF PARAM4}

{$IFDEF PAX64}
procedure LoadRAX(R_AX: IntPax); assembler;
asm
  mov RAX, R_AX
end;
{$ELSE}
function LoadRAX(R_AX: IntPax): Integer;
begin
  result := R_AX;
end;
{$ENDIF}

procedure CallGlobal(P1: IntPax;
                     P2: IntPax;
                     P3: IntPax;
                     P4: IntPax;
                     CommonGlobalIndex: Integer;
                     StackPtr: Pointer);
var
  Runner: TBaseRunner;
  N, FT, RetSize: Integer;
  R_AX: IntPax;
  ResBuff: array[0..SizeOf(Variant)-1] of Byte;
begin
  Runner := CurrRunner;

  N := Runner.ByteCodeGlobalEntryList[CommonGlobalIndex];

  FillChar(ResBuff, SizeOf(ResBuff), 0);
{$IFDEF PAX64}
  RetSize := Runner.CallByteCode(N, nil, 0, P1, P2, P3, P4,
{$ELSE}
  RetSize := Runner.CallByteCode(N, nil, P1, P2, P3, P4, 0,
{$ENDIF}
    StackPtr, @ResBuff, FT);
  R_AX := 0;
  if FT in (OrdinalTypes + [
{$IFNDEF PAXARM}
            typeANSISTRING, typeWIDESTRING,
{$ENDIF}
            typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT, typeINT64, typeUINT64,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
  begin
    Move(ResBuff, R_AX, Types.GetSize(FT));
    LoadRAX(R_AX);
  end
  else if FT = typeDOUBLE then
    LoadDouble(@ResBuff)
  else if FT = typeSINGLE then
    LoadSingle(@ResBuff)
  else if FT = typeEXTENDED then
    LoadExtended(@ResBuff)
  else if FT = typeCURRENCY then
    LoadCurrency(@ResBuff);
end;

procedure FakeGlobal_00(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 0, GetRBPPtr);
end;

procedure FakeGlobal_01(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 1, GetRBPPtr);
end;

procedure FakeGlobal_02(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 2, GetRBPPtr);
end;

procedure FakeGlobal_03(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 3, GetRBPPtr);
end;

procedure FakeGlobal_04(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 4, GetRBPPtr);
end;

procedure FakeGlobal_05(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 5, GetRBPPtr);
end;

procedure FakeGlobal_06(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 6, GetRBPPtr);
end;

procedure FakeGlobal_07(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 7, GetRBPPtr);
end;

procedure FakeGlobal_08(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 8, GetRBPPtr);
end;

procedure FakeGlobal_09(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 9, GetRBPPtr);
end;

procedure FakeGlobal_10(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 10, GetRBPPtr);
end;

procedure FakeGlobal_11(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 11, GetRBPPtr);
end;

procedure FakeGlobal_12(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 12, GetRBPPtr);
end;

procedure FakeGlobal_13(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 13, GetRBPPtr);
end;

procedure FakeGlobal_14(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 14, GetRBPPtr);
end;

procedure FakeGlobal_15(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 15, GetRBPPtr);
end;

procedure FakeGlobal_16(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 16, GetRBPPtr);
end;

procedure FakeGlobal_17(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 17, GetRBPPtr);
end;

procedure FakeGlobal_18(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 18, GetRBPPtr);
end;

procedure FakeGlobal_19(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 19, GetRBPPtr);
end;

procedure FakeGlobal_20(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 20, GetRBPPtr);
end;

procedure FakeGlobal_21(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 21, GetRBPPtr);
end;

procedure FakeGlobal_22(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 22, GetRBPPtr);
end;

procedure FakeGlobal_23(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 23, GetRBPPtr);
end;

procedure FakeGlobal_24(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 24, GetRBPPtr);
end;

procedure FakeGlobal_25(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 25, GetRBPPtr);
end;

procedure FakeGlobal_26(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 26, GetRBPPtr);
end;

procedure FakeGlobal_27(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 27, GetRBPPtr);
end;

procedure FakeGlobal_28(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 28, GetRBPPtr);
end;

procedure FakeGlobal_29(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 29, GetRBPPtr);
end;

procedure FakeGlobal_30(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 30, GetRBPPtr);
end;

procedure FakeGlobal_31(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 31, GetRBPPtr);
end;

procedure FakeGlobal_32(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 32, GetRBPPtr);
end;

procedure FakeGlobal_33(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 33, GetRBPPtr);
end;

procedure FakeGlobal_34(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 34, GetRBPPtr);
end;

procedure FakeGlobal_35(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 35, GetRBPPtr);
end;

procedure FakeGlobal_36(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 36, GetRBPPtr);
end;

procedure FakeGlobal_37(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 37, GetRBPPtr);
end;

procedure FakeGlobal_38(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 38, GetRBPPtr);
end;

procedure FakeGlobal_39(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 39, GetRBPPtr);
end;

procedure FakeGlobal_40(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 40, GetRBPPtr);
end;

procedure FakeGlobal_41(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 41, GetRBPPtr);
end;

procedure FakeGlobal_42(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 42, GetRBPPtr);
end;

procedure FakeGlobal_43(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 43, GetRBPPtr);
end;

procedure FakeGlobal_44(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 44, GetRBPPtr);
end;

procedure FakeGlobal_45(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 45, GetRBPPtr);
end;

procedure FakeGlobal_46(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 46, GetRBPPtr);
end;

procedure FakeGlobal_47(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 47, GetRBPPtr);
end;

procedure FakeGlobal_48(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 48, GetRBPPtr);
end;

procedure FakeGlobal_49(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 49, GetRBPPtr);
end;

procedure FakeGlobal_50(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 50, GetRBPPtr);
end;

procedure FakeGlobal_51(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 51, GetRBPPtr);
end;

procedure FakeGlobal_52(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 52, GetRBPPtr);
end;

procedure FakeGlobal_53(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 53, GetRBPPtr);
end;

procedure FakeGlobal_54(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 54, GetRBPPtr);
end;

procedure FakeGlobal_55(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 55, GetRBPPtr);
end;

procedure FakeGlobal_56(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 56, GetRBPPtr);
end;

procedure FakeGlobal_57(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 57, GetRBPPtr);
end;

procedure FakeGlobal_58(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 58, GetRBPPtr);
end;

procedure FakeGlobal_59(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 59, GetRBPPtr);
end;

procedure FakeGlobal_60(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 60, GetRBPPtr);
end;

procedure FakeGlobal_61(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 61, GetRBPPtr);
end;

procedure FakeGlobal_62(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 62, GetRBPPtr);
end;

procedure FakeGlobal_63(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 63, GetRBPPtr);
end;

procedure FakeGlobal_64(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 64, GetRBPPtr);
end;

procedure FakeGlobal_65(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 65, GetRBPPtr);
end;

procedure FakeGlobal_66(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 66, GetRBPPtr);
end;

procedure FakeGlobal_67(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 67, GetRBPPtr);
end;

procedure FakeGlobal_68(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 68, GetRBPPtr);
end;

procedure FakeGlobal_69(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 69, GetRBPPtr);
end;

procedure FakeGlobal_70(P1, P2, P3, P4: IntPax);
begin
  CallGlobal(P1, P2, P3, P4, 70, GetRBPPtr);
end;

procedure CallHandler(P1: IntPax;
                      P2: IntPax;
                      P3: IntPax;
                      P4: IntPax;
                      CommonMethodIndex: Integer;
                      StackPtr: Pointer);
var
  C: TClass;
  PaxInfo: PPaxInfo;
  Runner: TBaseRunner;
  Self: Pointer;
  N, FT, RetSize: Integer;
  R_AX: IntPax;
  ResBuff: array[1..64] of Byte;
  ResAddress: Pointer;
begin
{$IFDEF FPC}
  if P1 = 0 then
    Self := Pointer(P2)
  else
    Self := Pointer(P1);
{$ELSE}
  Self := Pointer(P1);
{$ENDIF}
  if IsDelphiClass(Self) then
    C := TClass(Self)
  else
    C := TObject(Self).ClassType;
  PaxInfo := GetPaxInfo(C);
  Assert(PaxInfo <> nil);
  Runner := TBaseRunner(PaxInfo^.Prog);

  N := Runner.ClassList[PaxInfo^.ClassIndex].
         ByteCodeMethodEntryList[CommonMethodIndex];

//  FT := Runner.GetReturnFinalTypeId(N);

  FillChar(ResBuff, SizeOf(ResBuff), 0);
  ResAddress := @ ResBuff;

{$IFDEF PAX64}
  RetSize := Runner.CallByteCode(N, Self, 0, P1, P2, P3, P4,
{$ELSE}
  RetSize := Runner.CallByteCode(N, Self, P1, P2, P3, P4, 0,
{$ENDIF}
    StackPtr, ResAddress, FT);

  R_AX := 0;
  if FT in (OrdinalTypes + [
{$IFNDEF PAXARM}
            typeANSISTRING, typeWIDESTRING,
{$ENDIF}
            typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT, typeINT64, typeUINT64,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
  begin
    if FT = typeCLASS then
    begin
      LoadRAX(IntPax(ResAddress^));
    end
    else
    begin
      Move(ResAddress^, R_AX, Types.GetSize(FT));
      LoadRAX(R_AX);
    end;
  end
  else if FT = typeDOUBLE then
    LoadDouble(@ResBuff)
  else if FT = typeSINGLE then
    LoadSingle(@ResBuff)
  else if FT = typeEXTENDED then
    LoadExtended(@ResBuff)
  else if FT = typeCURRENCY then
    LoadCurrency(@ResBuff);
end;

procedure FakeHandler_00(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 0, GetRBPPtr);
end;

procedure FakeHandler_01(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 1, GetRBPPtr);
end;

procedure FakeHandler_02(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 2, GetRBPPtr);
end;

procedure FakeHandler_03(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 3, GetRBPPtr);
end;

procedure FakeHandler_04(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 4, GetRBPPtr);
end;

procedure FakeHandler_05(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 5, GetRBPPtr);
end;

procedure FakeHandler_06(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 6, GetRBPPtr);
end;

procedure FakeHandler_07(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 7, GetRBPPtr);
end;

procedure FakeHandler_08(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 8, GetRBPPtr);
end;

procedure FakeHandler_09(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 9, GetRBPPtr);
end;

procedure FakeHandler_10(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 10, GetRBPPtr);
end;

procedure FakeHandler_11(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 11, GetRBPPtr);
end;

procedure FakeHandler_12(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 12, GetRBPPtr);
end;

procedure FakeHandler_13(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 13, GetRBPPtr);
end;

procedure FakeHandler_14(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 14, GetRBPPtr);
end;

procedure FakeHandler_15(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 15, GetRBPPtr);
end;

procedure FakeHandler_16(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 16, GetRBPPtr);
end;

procedure FakeHandler_17(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 17, GetRBPPtr);
end;

procedure FakeHandler_18(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 18, GetRBPPtr);
end;

procedure FakeHandler_19(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 19, GetRBPPtr);
end;

procedure FakeHandler_20(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 20, GetRBPPtr);
end;

procedure FakeHandler_21(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 21, GetRBPPtr);
end;

procedure FakeHandler_22(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 22, GetRBPPtr);
end;

procedure FakeHandler_23(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 23, GetRBPPtr);
end;

procedure FakeHandler_24(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 24, GetRBPPtr);
end;

procedure FakeHandler_25(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 25, GetRBPPtr);
end;

procedure FakeHandler_26(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 26, GetRBPPtr);
end;

procedure FakeHandler_27(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 27, GetRBPPtr);
end;

procedure FakeHandler_28(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 28, GetRBPPtr);
end;

procedure FakeHandler_29(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 29, GetRBPPtr);
end;

procedure FakeHandler_30(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 30, GetRBPPtr);
end;

procedure FakeHandler_31(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 31, GetRBPPtr);
end;

procedure FakeHandler_32(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 32, GetRBPPtr);
end;

procedure FakeHandler_33(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 33, GetRBPPtr);
end;

procedure FakeHandler_34(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 34, GetRBPPtr);
end;

procedure FakeHandler_35(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 35, GetRBPPtr);
end;

procedure FakeHandler_36(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 36, GetRBPPtr);
end;

procedure FakeHandler_37(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 37, GetRBPPtr);
end;

procedure FakeHandler_38(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 38, GetRBPPtr);
end;

procedure FakeHandler_39(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 39, GetRBPPtr);
end;

procedure FakeHandler_40(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 40, GetRBPPtr);
end;

procedure FakeHandler_41(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 41, GetRBPPtr);
end;

procedure FakeHandler_42(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 42, GetRBPPtr);
end;

procedure FakeHandler_43(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 43, GetRBPPtr);
end;

procedure FakeHandler_44(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 44, GetRBPPtr);
end;

procedure FakeHandler_45(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 45, GetRBPPtr);
end;

procedure FakeHandler_46(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 46, GetRBPPtr);
end;

procedure FakeHandler_47(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 47, GetRBPPtr);
end;

procedure FakeHandler_48(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 48, GetRBPPtr);
end;

procedure FakeHandler_49(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 49, GetRBPPtr);
end;

procedure FakeHandler_50(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 50, GetRBPPtr);
end;

procedure FakeHandler_51(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 51, GetRBPPtr);
end;

procedure FakeHandler_52(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 52, GetRBPPtr);
end;

procedure FakeHandler_53(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 53, GetRBPPtr);
end;

procedure FakeHandler_54(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 54, GetRBPPtr);
end;

procedure FakeHandler_55(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 55, GetRBPPtr);
end;

procedure FakeHandler_56(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 56, GetRBPPtr);
end;

procedure FakeHandler_57(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 57, GetRBPPtr);
end;

procedure FakeHandler_58(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 58, GetRBPPtr);
end;

procedure FakeHandler_59(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 59, GetRBPPtr);
end;

procedure FakeHandler_60(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 60, GetRBPPtr);
end;

procedure FakeHandler_61(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 61, GetRBPPtr);
end;

procedure FakeHandler_62(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 62, GetRBPPtr);
end;

procedure FakeHandler_63(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 63, GetRBPPtr);
end;

procedure FakeHandler_64(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 64, GetRBPPtr);
end;

procedure FakeHandler_65(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 65, GetRBPPtr);
end;

procedure FakeHandler_66(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 66, GetRBPPtr);
end;

procedure FakeHandler_67(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 67, GetRBPPtr);
end;

procedure FakeHandler_68(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 68, GetRBPPtr);
end;

procedure FakeHandler_69(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 69, GetRBPPtr);
end;

procedure FakeHandler_70(P1, P2, P3, P4: IntPax);
begin
  CallHandler(P1, P2, P3, P4, 70, GetRBPPtr);
end;

{$ELSE}

procedure CallGlobal(P1: IntPax;
                     P2: IntPax;
                     P3: IntPax;
                     CommonGlobalIndex: Integer;
                     StackPtr: Pointer);
var
  Runner: TBaseRunner;
  N, R_AX, R_DX, RetSize, FT: Integer;
  ResBuff: array[0..SizeOf(Variant)-1] of Byte;
begin
  Runner := CurrRunner;

  N := Runner.ByteCodeGlobalEntryList[CommonGlobalIndex];

  FillChar(ResBuff, SizeOf(ResBuff), 0);
  RetSize := Runner.CallByteCode(N, nil, P1, P3, P2, 0, 0,
    StackPtr, @ResBuff, FT);
  R_AX := 0;
  R_DX := 0;
{$IFDEF PAXARM}
  if FT in (OrdinalTypes + [typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
{$ELSE}
  if FT in (OrdinalTypes + [typeANSISTRING, typeWIDESTRING, typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
{$ENDIF}
  begin
    Move(ResBuff, R_AX, Types.GetSize(FT));
  end
  else if FT in Int64Types then
  begin
    Move(ResBuff, R_AX, SizeOf(Integer));
    Move(ResBuff[SizeOf(Integer)], R_DX, SizeOf(Integer));
  end
  else if FT = typeDOUBLE then
    LoadDouble(@ResBuff)
  else if FT = typeSINGLE then
    LoadSingle(@ResBuff)
  else if FT = typeEXTENDED then
    LoadExtended(@ResBuff)
  else if FT = typeCURRENCY then
    LoadCurrency(@ResBuff);
{$IFNDEF PAXARM_DEVICE}
  ProcessRet32(R_AX, R_DX, RetSize, StackPtr);
{$ENDIF}
end;

procedure FakeGlobal_00(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 0, GetRBPPtr);
end;

procedure FakeGlobal_01(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 1, GetRBPPtr);
end;

procedure FakeGlobal_02(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 2, GetRBPPtr);
end;

procedure FakeGlobal_03(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 3, GetRBPPtr);
end;

procedure FakeGlobal_04(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 4, GetRBPPtr);
end;

procedure FakeGlobal_05(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 5, GetRBPPtr);
end;

procedure FakeGlobal_06(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 6, GetRBPPtr);
end;

procedure FakeGlobal_07(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 7, GetRBPPtr);
end;

procedure FakeGlobal_08(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 8, GetRBPPtr);
end;

procedure FakeGlobal_09(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 9, GetRBPPtr);
end;

procedure FakeGlobal_10(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 10, GetRBPPtr);
end;

procedure FakeGlobal_11(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 11, GetRBPPtr);
end;

procedure FakeGlobal_12(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 12, GetRBPPtr);
end;

procedure FakeGlobal_13(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 13, GetRBPPtr);
end;

procedure FakeGlobal_14(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 14, GetRBPPtr);
end;

procedure FakeGlobal_15(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 15, GetRBPPtr);
end;

procedure FakeGlobal_16(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 16, GetRBPPtr);
end;

procedure FakeGlobal_17(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 17, GetRBPPtr);
end;

procedure FakeGlobal_18(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 18, GetRBPPtr);
end;

procedure FakeGlobal_19(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 19, GetRBPPtr);
end;

procedure FakeGlobal_20(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 20, GetRBPPtr);
end;

procedure FakeGlobal_21(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 21, GetRBPPtr);
end;

procedure FakeGlobal_22(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 22, GetRBPPtr);
end;

procedure FakeGlobal_23(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 23, GetRBPPtr);
end;

procedure FakeGlobal_24(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 24, GetRBPPtr);
end;

procedure FakeGlobal_25(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 25, GetRBPPtr);
end;

procedure FakeGlobal_26(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 26, GetRBPPtr);
end;

procedure FakeGlobal_27(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 27, GetRBPPtr);
end;

procedure FakeGlobal_28(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 28, GetRBPPtr);
end;

procedure FakeGlobal_29(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 29, GetRBPPtr);
end;

procedure FakeGlobal_30(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 30, GetRBPPtr);
end;

procedure FakeGlobal_31(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 31, GetRBPPtr);
end;

procedure FakeGlobal_32(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 32, GetRBPPtr);
end;

procedure FakeGlobal_33(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 33, GetRBPPtr);
end;

procedure FakeGlobal_34(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 34, GetRBPPtr);
end;

procedure FakeGlobal_35(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 35, GetRBPPtr);
end;

procedure FakeGlobal_36(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 36, GetRBPPtr);
end;

procedure FakeGlobal_37(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 37, GetRBPPtr);
end;

procedure FakeGlobal_38(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 38, GetRBPPtr);
end;

procedure FakeGlobal_39(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 39, GetRBPPtr);
end;

procedure FakeGlobal_40(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 40, GetRBPPtr);
end;

procedure FakeGlobal_41(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 41, GetRBPPtr);
end;

procedure FakeGlobal_42(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 42, GetRBPPtr);
end;

procedure FakeGlobal_43(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 43, GetRBPPtr);
end;

procedure FakeGlobal_44(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 44, GetRBPPtr);
end;

procedure FakeGlobal_45(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 45, GetRBPPtr);
end;

procedure FakeGlobal_46(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 46, GetRBPPtr);
end;

procedure FakeGlobal_47(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 47, GetRBPPtr);
end;

procedure FakeGlobal_48(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 48, GetRBPPtr);
end;

procedure FakeGlobal_49(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 49, GetRBPPtr);
end;

procedure FakeGlobal_50(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 50, GetRBPPtr);
end;

procedure FakeGlobal_51(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 51, GetRBPPtr);
end;

procedure FakeGlobal_52(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 52, GetRBPPtr);
end;

procedure FakeGlobal_53(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 53, GetRBPPtr);
end;

procedure FakeGlobal_54(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 54, GetRBPPtr);
end;

procedure FakeGlobal_55(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 55, GetRBPPtr);
end;

procedure FakeGlobal_56(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 56, GetRBPPtr);
end;

procedure FakeGlobal_57(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 57, GetRBPPtr);
end;

procedure FakeGlobal_58(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 58, GetRBPPtr);
end;

procedure FakeGlobal_59(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 59, GetRBPPtr);
end;

procedure FakeGlobal_60(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 60, GetRBPPtr);
end;

procedure FakeGlobal_61(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 61, GetRBPPtr);
end;

procedure FakeGlobal_62(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 62, GetRBPPtr);
end;

procedure FakeGlobal_63(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 63, GetRBPPtr);
end;

procedure FakeGlobal_64(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 64, GetRBPPtr);
end;

procedure FakeGlobal_65(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 65, GetRBPPtr);
end;

procedure FakeGlobal_66(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 66, GetRBPPtr);
end;

procedure FakeGlobal_67(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 67, GetRBPPtr);
end;

procedure FakeGlobal_68(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 68, GetRBPPtr);
end;

procedure FakeGlobal_69(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 69, GetRBPPtr);
end;

procedure FakeGlobal_70(P1, P2, P3: IntPax);
begin
  CallGlobal(P1, P2, P3, 70, GetRBPPtr);
end;

/////////////

procedure CallHandler(P1: IntPax;
                      P2: IntPax;
                      P3: IntPax;
                      CommonMethodIndex: Integer;
                      StackPtr: Pointer);
var
  C: TClass;
  PaxInfo: PPaxInfo;
  Runner: TBaseRunner;
  Self: Pointer;
  N, R_AX, R_DX, RetSize, FT: Integer;
  ResBuff: array[0..SizeOf(Variant)-1] of Byte;
begin
 {$IFDEF FPC}
   if P1 = 0 then
     Self := Pointer(P2)
   else
     Self := Pointer(P1);
  {$ELSE}
    Self := Pointer(P1);
  {$ENDIF}
  if IsDelphiClass(Self) then
    C := TClass(Self)
  else
    C := TObject(Self).ClassType;
  PaxInfo := GetPaxInfo(C);
  Assert(PaxInfo <> nil);
  Runner := TBaseRunner(PaxInfo^.Prog);

  N := Runner.ClassList[PaxInfo^.ClassIndex].
         ByteCodeMethodEntryList[CommonMethodIndex];

  FillChar(ResBuff, SizeOf(ResBuff), 0);
  RetSize := Runner.CallByteCode(N, Self, P1, P3, P2, 0, 0,
    StackPtr, @ResBuff, FT);
  R_AX := 0;
  R_DX := 0;
{$IFDEF PAXARM}
  if FT in (OrdinalTypes + [typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
{$ELSE}
  if FT in (OrdinalTypes + [typeANSISTRING, typeWIDESTRING, typeUNICSTRING, typeVARIANT,
            typeOLEVARIANT,
            typePOINTER, typeCLASS, typeCLASSREF, typeDYNARRAY,
            typeINTERFACE]) then
{$ENDIF}
  begin
    Move(ResBuff, R_AX, Types.GetSize(FT));
{$IFDEF ARC}
{
    if FT = typeCLASS then
      if R_AX <> 0 then
      begin
        if TObject(R_AX).RefCount > 1 then
          TObject(R_AX).__ObjRelease
        else
        begin
          Self := ShiftPointer(Pointer(R_AX), SizeOf(Pointer));
          Pointer(Self^) := nil;
        end;
      end;
}
{$ENDIF}
  end
  else if FT = typeINT64 then
  begin
    Move(ResBuff, R_AX, SizeOf(Integer));
    Move(ResBuff[SizeOf(Integer)], R_DX, SizeOf(Integer));
  end
  else if FT = typeDOUBLE then
    LoadDouble(@ResBuff)
  else if FT = typeSINGLE then
    LoadSingle(@ResBuff)
  else if FT = typeEXTENDED then
    LoadExtended(@ResBuff)
  else if FT = typeCURRENCY then
    LoadCurrency(@ResBuff);
{$IFNDEF PAXARM_DEVICE}
  ProcessRet32(R_AX, R_DX, RetSize, StackPtr);
{$ENDIF}
end;

procedure FakeHandler_00(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 0, GetRBPPtr);
end;

procedure FakeHandler_01(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 1, GetRBPPtr);
end;

procedure FakeHandler_02(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 2, GetRBPPtr);
end;

procedure FakeHandler_03(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 3, GetRBPPtr);
end;

procedure FakeHandler_04(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 4, GetRBPPtr);
end;

procedure FakeHandler_05(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 5, GetRBPPtr);
end;

procedure FakeHandler_06(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 6, GetRBPPtr);
end;

procedure FakeHandler_07(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 7, GetRBPPtr);
end;

procedure FakeHandler_08(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 8, GetRBPPtr);
end;

procedure FakeHandler_09(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 9, GetRBPPtr);
end;

procedure FakeHandler_10(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 10, GetRBPPtr);
end;

procedure FakeHandler_11(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 11, GetRBPPtr);
end;

procedure FakeHandler_12(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 12, GetRBPPtr);
end;

procedure FakeHandler_13(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 13, GetRBPPtr);
end;

procedure FakeHandler_14(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 14, GetRBPPtr);
end;

procedure FakeHandler_15(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 15, GetRBPPtr);
end;

procedure FakeHandler_16(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 16, GetRBPPtr);
end;

procedure FakeHandler_17(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 17, GetRBPPtr);
end;

procedure FakeHandler_18(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 18, GetRBPPtr);
end;

procedure FakeHandler_19(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 19, GetRBPPtr);
end;

procedure FakeHandler_20(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 20, GetRBPPtr);
end;

procedure FakeHandler_21(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 21, GetRBPPtr);
end;

procedure FakeHandler_22(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 22, GetRBPPtr);
end;

procedure FakeHandler_23(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 23, GetRBPPtr);
end;

procedure FakeHandler_24(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 24, GetRBPPtr);
end;

procedure FakeHandler_25(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 25, GetRBPPtr);
end;

procedure FakeHandler_26(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 26, GetRBPPtr);
end;

procedure FakeHandler_27(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 27, GetRBPPtr);
end;

procedure FakeHandler_28(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 28, GetRBPPtr);
end;

procedure FakeHandler_29(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 29, GetRBPPtr);
end;

procedure FakeHandler_30(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 30, GetRBPPtr);
end;

procedure FakeHandler_31(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 31, GetRBPPtr);
end;

procedure FakeHandler_32(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 32, GetRBPPtr);
end;

procedure FakeHandler_33(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 33, GetRBPPtr);
end;

procedure FakeHandler_34(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 34, GetRBPPtr);
end;

procedure FakeHandler_35(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 35, GetRBPPtr);
end;

procedure FakeHandler_36(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 36, GetRBPPtr);
end;

procedure FakeHandler_37(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 37, GetRBPPtr);
end;

procedure FakeHandler_38(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 38, GetRBPPtr);
end;

procedure FakeHandler_39(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 39, GetRBPPtr);
end;

procedure FakeHandler_40(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 40, GetRBPPtr);
end;

procedure FakeHandler_41(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 41, GetRBPPtr);
end;

procedure FakeHandler_42(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 42, GetRBPPtr);
end;

procedure FakeHandler_43(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 43, GetRBPPtr);
end;

procedure FakeHandler_44(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 44, GetRBPPtr);
end;

procedure FakeHandler_45(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 45, GetRBPPtr);
end;

procedure FakeHandler_46(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 46, GetRBPPtr);
end;

procedure FakeHandler_47(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 47, GetRBPPtr);
end;

procedure FakeHandler_48(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 48, GetRBPPtr);
end;

procedure FakeHandler_49(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 49, GetRBPPtr);
end;

procedure FakeHandler_50(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 50, GetRBPPtr);
end;

procedure FakeHandler_51(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 51, GetRBPPtr);
end;

procedure FakeHandler_52(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 52, GetRBPPtr);
end;

procedure FakeHandler_53(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 53, GetRBPPtr);
end;

procedure FakeHandler_54(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 54, GetRBPPtr);
end;

procedure FakeHandler_55(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 55, GetRBPPtr);
end;

procedure FakeHandler_56(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 56, GetRBPPtr);
end;

procedure FakeHandler_57(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 57, GetRBPPtr);
end;

procedure FakeHandler_58(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 58, GetRBPPtr);
end;

procedure FakeHandler_59(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 59, GetRBPPtr);
end;

procedure FakeHandler_60(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 60, GetRBPPtr);
end;

procedure FakeHandler_61(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 61, GetRBPPtr);
end;

procedure FakeHandler_62(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 62, GetRBPPtr);
end;

procedure FakeHandler_63(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 63, GetRBPPtr);
end;

procedure FakeHandler_64(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 64, GetRBPPtr);
end;

procedure FakeHandler_65(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 65, GetRBPPtr);
end;

procedure FakeHandler_66(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 66, GetRBPPtr);
end;

procedure FakeHandler_67(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 67, GetRBPPtr);
end;

procedure FakeHandler_68(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 68, GetRBPPtr);
end;

procedure FakeHandler_69(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 69, GetRBPPtr);
end;

procedure FakeHandler_70(P1, P2, P3: IntPax);
begin
  CallHandler(P1, P2, P3, 70, GetRBPPtr);
end;

{$ENDIF}

type
  TMyInterfacedObject = class(TInterfacedObject);

function fake_AddRefGen(I: Pointer; Index: Integer): Integer; stdcall;
var
  N, Offset: Integer;
begin
  N := CurrRunner.ByteCodeInterfaceSetupList[Index];
  Offset := CurrRunner.GetInterfaceToObjectOffset(N);
  I := ShiftPointer(I, Offset);

  result := TMyInterfacedObject(I)._AddRef;
end;

function fake_ReleaseGen(I: Pointer; Index: Integer): Integer; stdcall;
var
  N, Offset: Integer;
begin
  N := CurrRunner.ByteCodeInterfaceSetupList[Index];
  Offset := CurrRunner.GetInterfaceToObjectOffset(N);
  I := ShiftPointer(I, Offset);

  result := TMyInterfacedObject(I)._Release;
end;

function fake_AddRef00(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 00);
end;

function fake_AddRef01(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 01);
end;

function fake_AddRef02(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 02);
end;

function fake_AddRef03(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 03);
end;

function fake_AddRef04(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 04);
end;

function fake_AddRef05(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 05);
end;

function fake_AddRef06(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 06);
end;

function fake_AddRef07(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 07);
end;

function fake_AddRef08(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 08);
end;

function fake_AddRef09(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 09);
end;

function fake_AddRef10(I: Pointer): Integer; stdcall;
begin
  result := fake_AddRefGen(I, 10);
end;

function fake_Release00(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 00);
end;

function fake_Release01(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 01);
end;

function fake_Release02(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 02);
end;

function fake_Release03(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 03);
end;

function fake_Release04(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 04);
end;

function fake_Release05(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 05);
end;

function fake_Release06(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 06);
end;

function fake_Release07(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 07);
end;

function fake_Release08(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 08);
end;

function fake_Release09(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 09);
end;

function fake_Release10(I: Pointer): Integer; stdcall;
begin
  result := fake_ReleaseGen(I, 10);
end;

function GetFakeAddRefAddress(Index: Integer): Pointer;
begin
  result := FakeAddRefAddressList[Index];
end;

function GetFakeReleaseAddress(Index: Integer): Pointer;
begin
  result := FakeReleaseAddressList[Index];
end;

function GetFakeHandlerAddress(Index: Integer): Pointer;
begin
  result := FakeHandlerAddressList[Index];
end;

function GetFakeGlobalAddress(Index: Integer): Pointer;
begin
  result := FakeGlobalAddressList[Index];
end;

procedure InitFakeAddRefMethods;
begin
  FakeAddRefAddressList[00] := @ fake_AddRef00;
  FakeAddRefAddressList[01] := @ fake_AddRef01;
  FakeAddRefAddressList[02] := @ fake_AddRef02;
  FakeAddRefAddressList[03] := @ fake_AddRef03;
  FakeAddRefAddressList[04] := @ fake_AddRef04;
  FakeAddRefAddressList[05] := @ fake_AddRef05;
  FakeAddRefAddressList[06] := @ fake_AddRef06;
  FakeAddRefAddressList[07] := @ fake_AddRef07;
  FakeAddRefAddressList[08] := @ fake_AddRef08;
  FakeAddRefAddressList[09] := @ fake_AddRef09;
  FakeAddRefAddressList[10] := @ fake_AddRef10;
end;

procedure InitFakeReleaseMethods;
begin
  FakeReleaseAddressList[00] := @ fake_Release00;
  FakeReleaseAddressList[01] := @ fake_Release01;
  FakeReleaseAddressList[02] := @ fake_Release02;
  FakeReleaseAddressList[03] := @ fake_Release03;
  FakeReleaseAddressList[04] := @ fake_Release04;
  FakeReleaseAddressList[05] := @ fake_Release05;
  FakeReleaseAddressList[06] := @ fake_Release06;
  FakeReleaseAddressList[07] := @ fake_Release07;
  FakeReleaseAddressList[08] := @ fake_Release08;
  FakeReleaseAddressList[09] := @ fake_Release09;
  FakeReleaseAddressList[10] := @ fake_Release10;
end;

procedure InitFakeMethods;
begin
  FakeHandlerAddressList[00] := @ FakeHandler_00;
  FakeHandlerAddressList[01] := @ FakeHandler_01;
  FakeHandlerAddressList[02] := @ FakeHandler_02;
  FakeHandlerAddressList[03] := @ FakeHandler_03;
  FakeHandlerAddressList[04] := @ FakeHandler_04;
  FakeHandlerAddressList[05] := @ FakeHandler_05;
  FakeHandlerAddressList[06] := @ FakeHandler_06;
  FakeHandlerAddressList[07] := @ FakeHandler_07;
  FakeHandlerAddressList[08] := @ FakeHandler_08;
  FakeHandlerAddressList[09] := @ FakeHandler_09;
  FakeHandlerAddressList[10] := @ FakeHandler_10;
  FakeHandlerAddressList[11] := @ FakeHandler_11;
  FakeHandlerAddressList[12] := @ FakeHandler_12;
  FakeHandlerAddressList[13] := @ FakeHandler_13;
  FakeHandlerAddressList[14] := @ FakeHandler_14;
  FakeHandlerAddressList[15] := @ FakeHandler_15;
  FakeHandlerAddressList[16] := @ FakeHandler_16;
  FakeHandlerAddressList[17] := @ FakeHandler_17;
  FakeHandlerAddressList[18] := @ FakeHandler_18;
  FakeHandlerAddressList[19] := @ FakeHandler_19;
  FakeHandlerAddressList[20] := @ FakeHandler_20;
  FakeHandlerAddressList[21] := @ FakeHandler_21;
  FakeHandlerAddressList[22] := @ FakeHandler_22;
  FakeHandlerAddressList[23] := @ FakeHandler_23;
  FakeHandlerAddressList[24] := @ FakeHandler_24;
  FakeHandlerAddressList[25] := @ FakeHandler_25;
  FakeHandlerAddressList[26] := @ FakeHandler_26;
  FakeHandlerAddressList[27] := @ FakeHandler_27;
  FakeHandlerAddressList[28] := @ FakeHandler_28;
  FakeHandlerAddressList[29] := @ FakeHandler_29;
  FakeHandlerAddressList[30] := @ FakeHandler_30;
  FakeHandlerAddressList[31] := @ FakeHandler_31;
  FakeHandlerAddressList[32] := @ FakeHandler_32;
  FakeHandlerAddressList[33] := @ FakeHandler_33;
  FakeHandlerAddressList[34] := @ FakeHandler_34;
  FakeHandlerAddressList[35] := @ FakeHandler_35;
  FakeHandlerAddressList[36] := @ FakeHandler_36;
  FakeHandlerAddressList[37] := @ FakeHandler_37;
  FakeHandlerAddressList[38] := @ FakeHandler_38;
  FakeHandlerAddressList[39] := @ FakeHandler_39;
  FakeHandlerAddressList[40] := @ FakeHandler_40;
  FakeHandlerAddressList[41] := @ FakeHandler_41;
  FakeHandlerAddressList[42] := @ FakeHandler_42;
  FakeHandlerAddressList[43] := @ FakeHandler_43;
  FakeHandlerAddressList[44] := @ FakeHandler_44;
  FakeHandlerAddressList[45] := @ FakeHandler_45;
  FakeHandlerAddressList[46] := @ FakeHandler_46;
  FakeHandlerAddressList[47] := @ FakeHandler_47;
  FakeHandlerAddressList[48] := @ FakeHandler_48;
  FakeHandlerAddressList[49] := @ FakeHandler_49;
  FakeHandlerAddressList[50] := @ FakeHandler_50;
  FakeHandlerAddressList[51] := @ FakeHandler_51;
  FakeHandlerAddressList[52] := @ FakeHandler_52;
  FakeHandlerAddressList[53] := @ FakeHandler_53;
  FakeHandlerAddressList[54] := @ FakeHandler_54;
  FakeHandlerAddressList[55] := @ FakeHandler_55;
  FakeHandlerAddressList[56] := @ FakeHandler_56;
  FakeHandlerAddressList[57] := @ FakeHandler_57;
  FakeHandlerAddressList[58] := @ FakeHandler_58;
  FakeHandlerAddressList[59] := @ FakeHandler_59;
  FakeHandlerAddressList[60] := @ FakeHandler_60;
  FakeHandlerAddressList[61] := @ FakeHandler_61;
  FakeHandlerAddressList[62] := @ FakeHandler_62;
  FakeHandlerAddressList[63] := @ FakeHandler_63;
  FakeHandlerAddressList[64] := @ FakeHandler_64;
  FakeHandlerAddressList[65] := @ FakeHandler_65;
  FakeHandlerAddressList[66] := @ FakeHandler_66;
  FakeHandlerAddressList[67] := @ FakeHandler_67;
  FakeHandlerAddressList[68] := @ FakeHandler_68;
  FakeHandlerAddressList[69] := @ FakeHandler_69;
  FakeHandlerAddressList[70] := @ FakeHandler_70;
end;

procedure InitFakeGlobals;
begin
  FakeGlobalAddressList[00] := @ FakeGlobal_00;
  FakeGlobalAddressList[01] := @ FakeGlobal_01;
  FakeGlobalAddressList[02] := @ FakeGlobal_02;
  FakeGlobalAddressList[03] := @ FakeGlobal_03;
  FakeGlobalAddressList[04] := @ FakeGlobal_04;
  FakeGlobalAddressList[05] := @ FakeGlobal_05;
  FakeGlobalAddressList[06] := @ FakeGlobal_06;
  FakeGlobalAddressList[07] := @ FakeGlobal_07;
  FakeGlobalAddressList[08] := @ FakeGlobal_08;
  FakeGlobalAddressList[09] := @ FakeGlobal_09;
  FakeGlobalAddressList[10] := @ FakeGlobal_10;
  FakeGlobalAddressList[11] := @ FakeGlobal_11;
  FakeGlobalAddressList[12] := @ FakeGlobal_12;
  FakeGlobalAddressList[13] := @ FakeGlobal_13;
  FakeGlobalAddressList[14] := @ FakeGlobal_14;
  FakeGlobalAddressList[15] := @ FakeGlobal_15;
  FakeGlobalAddressList[16] := @ FakeGlobal_16;
  FakeGlobalAddressList[17] := @ FakeGlobal_17;
  FakeGlobalAddressList[18] := @ FakeGlobal_18;
  FakeGlobalAddressList[19] := @ FakeGlobal_19;
  FakeGlobalAddressList[20] := @ FakeGlobal_20;
  FakeGlobalAddressList[21] := @ FakeGlobal_21;
  FakeGlobalAddressList[22] := @ FakeGlobal_22;
  FakeGlobalAddressList[23] := @ FakeGlobal_23;
  FakeGlobalAddressList[24] := @ FakeGlobal_24;
  FakeGlobalAddressList[25] := @ FakeGlobal_25;
  FakeGlobalAddressList[26] := @ FakeGlobal_26;
  FakeGlobalAddressList[27] := @ FakeGlobal_27;
  FakeGlobalAddressList[28] := @ FakeGlobal_28;
  FakeGlobalAddressList[29] := @ FakeGlobal_29;
  FakeGlobalAddressList[30] := @ FakeGlobal_30;
  FakeGlobalAddressList[31] := @ FakeGlobal_31;
  FakeGlobalAddressList[32] := @ FakeGlobal_32;
  FakeGlobalAddressList[33] := @ FakeGlobal_33;
  FakeGlobalAddressList[34] := @ FakeGlobal_34;
  FakeGlobalAddressList[35] := @ FakeGlobal_35;
  FakeGlobalAddressList[36] := @ FakeGlobal_36;
  FakeGlobalAddressList[37] := @ FakeGlobal_37;
  FakeGlobalAddressList[38] := @ FakeGlobal_38;
  FakeGlobalAddressList[39] := @ FakeGlobal_39;
  FakeGlobalAddressList[40] := @ FakeGlobal_40;
  FakeGlobalAddressList[41] := @ FakeGlobal_41;
  FakeGlobalAddressList[42] := @ FakeGlobal_42;
  FakeGlobalAddressList[43] := @ FakeGlobal_43;
  FakeGlobalAddressList[44] := @ FakeGlobal_44;
  FakeGlobalAddressList[45] := @ FakeGlobal_45;
  FakeGlobalAddressList[46] := @ FakeGlobal_46;
  FakeGlobalAddressList[47] := @ FakeGlobal_47;
  FakeGlobalAddressList[48] := @ FakeGlobal_48;
  FakeGlobalAddressList[49] := @ FakeGlobal_49;
  FakeGlobalAddressList[50] := @ FakeGlobal_50;
  FakeGlobalAddressList[51] := @ FakeGlobal_51;
  FakeGlobalAddressList[52] := @ FakeGlobal_52;
  FakeGlobalAddressList[53] := @ FakeGlobal_53;
  FakeGlobalAddressList[54] := @ FakeGlobal_54;
  FakeGlobalAddressList[55] := @ FakeGlobal_55;
  FakeGlobalAddressList[56] := @ FakeGlobal_56;
  FakeGlobalAddressList[57] := @ FakeGlobal_57;
  FakeGlobalAddressList[58] := @ FakeGlobal_58;
  FakeGlobalAddressList[59] := @ FakeGlobal_59;
  FakeGlobalAddressList[60] := @ FakeGlobal_60;
  FakeGlobalAddressList[61] := @ FakeGlobal_61;
  FakeGlobalAddressList[62] := @ FakeGlobal_62;
  FakeGlobalAddressList[63] := @ FakeGlobal_63;
  FakeGlobalAddressList[64] := @ FakeGlobal_64;
  FakeGlobalAddressList[65] := @ FakeGlobal_65;
  FakeGlobalAddressList[66] := @ FakeGlobal_66;
  FakeGlobalAddressList[67] := @ FakeGlobal_67;
  FakeGlobalAddressList[68] := @ FakeGlobal_68;
  FakeGlobalAddressList[69] := @ FakeGlobal_69;
  FakeGlobalAddressList[70] := @ FakeGlobal_70;
end;

initialization

  InitFakeAddRefMethods;
  InitFakeReleaseMethods;
  InitFakeMethods;
  InitFakeGlobals;

end.

