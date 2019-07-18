////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_TYPEINFO.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PAXCOMP_TYPEINFO;
interface

uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_MAP,
  PAXCOMP_CLASSFACT,
  PAXCOMP_GENERIC;

type
  TParamData = record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;

  TTypeInfoContainer = class;
  TTypeDataContainer = class;
  TClassTypeDataContainer = class;
  TMethodTypeDataContainer = class;

  TFieldDataContainer = class
  private
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  public
    Id: Integer; // not saved to stream
    Offset: Cardinal;    { Offset of field in the class data. }
    ClassIndex: Word;    { Index in the FieldClassTable. }
    Name: ShortString;
    FullFieldTypeName: String;
    // PCU only
    FinalFieldTypeId: Byte;
    Vis: TClassVisibility;
  end;

  TFieldListContainer = class(TTypedList)
  private
    function GetRecord(I: Integer): TFieldDataContainer;
  public
    function Add: TFieldDataContainer;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TFieldDataContainer read GetRecord; default;
  end;

  TAnotherPropRec = class //PCU only
  public
    Vis: TClassVisibility;
    PropName: String;
    ParamNames: TStringList;
    ParamTypes: TStringList;
    PropType: String;
    ReadName: String;
    WriteName: String;
    IsDefault: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  end;

  TAnotherPropList = class(TTypedList)
  private
    function GetRecord(I: Integer): TAnotherPropRec;
  public
    function Add: TAnotherPropRec;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Records[I: Integer]: TAnotherPropRec read GetRecord; default;
  end;

  TPropDataContainer = class
  private
    Owner: TTypeDataContainer;
    function GetCount: Integer;
    function GetSize: Integer;
  public
    PropData: TPropData;
    PropList: array of TPropInfo;
    PropTypeIds: array of Integer;
    ReadNames: TStringList;
    WriteNames: TStringList;
    PropTypeNames: TStringList;
    constructor Create(AOwner: TTypeDataContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure SaveToBuff(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
  end;

  TParamListContainer = class
  private
    Owner: TMethodTypeDataContainer;
    function GetCount: Integer;
    function GetSize: Integer;
  public
    ParamList: array of TParamData;
    constructor Create(AOwner: TMethodTypeDataContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure SaveToBuff(S: TStream);
    procedure LoadFromStream(S: TStream);
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
  end;

{$ifdef GE_DXETOKYO}   // XILINX, Tokyo mod
  TTypeDataXil = packed record
    function NameListFld: TTypeInfoFieldAccessor; inline;
    function UnitNameFld: TTypeInfoFieldAccessor; inline;
    function IntfUnitFld: TTypeInfoFieldAccessor; inline;
    function DynUnitNameFld: TTypeInfoFieldAccessor; inline;

    function PropData: PPropData; inline;
    function IntfMethods: PIntfMethodTable; inline;

    function DynArrElType: PPTypeInfo; inline;
    function DynArrAttrData: PAttrData; inline;

    case TTypeKind of
      tkUnknown: ();
      tkUString,
{$IFNDEF NEXTGEN}
      tkWString,
{$ENDIF !NEXTGEN}
      tkVariant: (AttrData: TAttrData);
      tkLString: (
        CodePage: Word
       {LStrAttrData: TAttrData});
      tkInteger, tkChar, tkEnumeration, tkSet, tkWChar: (
        OrdType: TOrdType;
        case TTypeKind of
          tkInteger, tkChar, tkEnumeration, tkWChar: (

            MinValue: Integer;
            MaxValue: Integer;
            case TTypeKind of
              tkInteger, tkChar, tkWChar: (
                {OrdAttrData: TAttrData});
              tkEnumeration: (
                BaseType: PPTypeInfo;
                NameList: TSymbolName;
               {EnumUnitName: TSymbolName;
                EnumAttrData: TAttrData}));
          tkSet: (
            CompType: PPTypeInfo
           {SetAttrData: TAttrData}));
      tkFloat: (
        FloatType: TFloatType
       {FloatAttrData: TAttrData});
{$IFNDEF NEXTGEN}
      tkString: (
        MaxLength: Byte
       {StrAttrData: TAttrData});
{$ENDIF !NEXTGEN}
      tkClass: (
        ClassType: TClass; // most data for instance types is in VMT offsets
        ParentInfo: PPTypeInfo;
        PropCount: SmallInt; // total properties inc. ancestors
        UnitName: TSymbolName;
       {PropData: TPropData;
        PropDataEx: TPropDataEx;
        ClassAttrData: TAttrData;
        ArrayPropCount: Word;
        ArrayPropData: array[1..ArrayPropCount] of TArrayPropInfo;});
      tkMethod: (
        MethodKind: TMethodKind; // only mkFunction or mkProcedure
        ParamCount: Byte;
{$IFNDEF NEXTGEN}
        ParamList: array[0..1023] of AnsiChar
{$ELSE NEXTGEN}
        ParamList: array[0..1023] of Byte
{$ENDIF NEXTGEN}
       {ParamList: array[1..ParamCount] of
          record
            Flags: TParamFlags;
            ParamName: ShortString;
            TypeName: ShortString;
          end;
        ResultType: ShortString; // only if MethodKind = mkFunction
        ResultTypeRef: PPTypeInfo; // only if MethodKind = mkFunction
        CC: TCallConv;
        ParamTypeRefs: array[1..ParamCount] of PPTypeInfo;
        MethSig: PProcedureSignature;
        MethAttrData: TAttrData});
      tkProcedure: (
        ProcSig: PProcedureSignature;
        ProcAttrData: TAttrData;);
      tkInterface: (
        IntfParent : PPTypeInfo; { ancestor }
        IntfFlags : TIntfFlagsBase;
        Guid : TGUID;
        IntfUnit : TSymbolName
       {IntfMethods: TIntfMethodTable;
        IntfAttrData: TAttrData;});
      tkInt64: (
        MinInt64Value, MaxInt64Value: Int64;
        Int64AttrData: TAttrData;);
      tkDynArray: (

        elSize: Integer;
        elType: PPTypeInfo;       // nil if type does not require cleanup
        varType: Integer;         // Ole Automation varType equivalent
        elType2: PPTypeInfo;      // independent of cleanup
        DynUnitName: TSymbolName;
       {DynArrElType: PPTypeInfo; // actual element type, even if dynamic array
        DynArrAttrData: TAttrData});
      tkRecord: (
        RecSize: Integer;
        ManagedFldCount: Integer;
       {ManagedFields: array[0..ManagedFldCnt - 1] of TManagedField;
        NumOps: Byte;
        RecOps: array[1..NumOps] of Pointer;
        RecFldCnt: Integer;
        RecFields: array[1..RecFldCnt] of TRecordTypeField;
        RecAttrData: TAttrData;
        RecMethCnt: Word;
        RecMeths: array[1..RecMethCnt] of TRecordTypeMethod});
      tkClassRef: (
        InstanceType: PPTypeInfo;
        ClassRefAttrData: TAttrData;);
      tkPointer: (
        RefType: PPTypeInfo;
        PtrAttrData: TAttrData);
      tkArray: (
        ArrayData: TArrayTypeData;
       {ArrAttrData: TAttrData});
  end;
{$endif}

  TTypeDataContainer = class
  private
    Owner: TTypeInfoContainer;
    function GetTypeDataSize: Integer; virtual; //save to buff
    function GetSize: Integer; virtual; // save to stream
  public
{$ifdef GE_DXETOKYO}   // XILINX, Tokyo mod
    TypeData: TTypeDataXil;
{$else}
    TypeData: TTypeData;
{$endif}
    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); virtual;
    procedure SaveToBuff(S: TStream); virtual;
    procedure LoadFromStream(S: TStream); virtual;
    property TypeDataSize: Integer read GetTypeDataSize;
    property Size: Integer read GetSize;
  end;

  TMethodTypeDataContainer = class(TTypeDataContainer)
  private
    function GetTypeDataSize: Integer; override;
    function GetSize: Integer; override;
  public
    MethodKind: TMethodKind;
    ParamCount: Byte;
    ParamListContainer: TParamListContainer;
    // extra data
    ResultType: ShortString;
    OwnerTypeName: String;
    MethodTableIndex: Integer;
    ResultTypeId: Integer;
    CallConv: Byte;
    OverCount: Byte;
    Address: Pointer; // not saved to stream
    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TClassTypeDataContainer = class(TTypeDataContainer)
  private
    function GetTypeDataSize: Integer; override;
    function GetSize: Integer; override;
  public
    // info about published members
    PropDataContainer: TPropDataContainer;
    MethodTableCount: Integer;
    MethodTableSize: Integer;
    FieldTableCount: Integer;
    FieldTableSize: Integer;
    FullParentName: String;
    FieldListContainer: TFieldListContainer;

    // PCU only
    AnotherFieldListContainer: TFieldListContainer;
    AnotherPropList: TAnotherPropList;
    SupportedInterfaces: TStringList;

    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TInterfaceTypeDataContainer = class(TTypeDataContainer)
  private
    function GetTypeDataSize: Integer; override;
    function GetSize: Integer; override;
  public
    PropDataContainer: TPropDataContainer;
    FullParentName: String;
    GUID: TGUID;
    SubDescList: TSubDescList;
    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TSetTypeDataContainer = class(TTypeDataContainer)
  private
    function GetSize: Integer; override;
  public
    FullCompName: String;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TEnumTypeDataContainer = class(TTypeDataContainer)
  private
    function GetTypeDataSize: Integer; override;
    function GetSize: Integer; override;
  public
    NameList: array of ShortString;
    EnumUnitName: ShortString;

    //pcu only
    ValueList: array of Integer;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TArrayTypeDataContainer = class(TTypeDataContainer)
  public
    FullRangeTypeName: String;
    FullElemTypeName: String;
    B1: Integer;
    B2: Integer;
    FinRangeTypeId: Integer;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TRecordTypeDataContainer = class(TTypeDataContainer)
  private
    function GetSize: Integer; override;
  public
    IsPacked: Boolean;
    FieldListContainer: TFieldListContainer;
    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); override;
    procedure SaveToBuff(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TAliasTypeDataContainer = class(TTypeDataContainer)
  public
    FullSourceTypeName: String;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TPointerTypeDataContainer = class(TTypeDataContainer)
  public
    FullOriginTypeName: String;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TClassRefTypeDataContainer = class(TTypeDataContainer)
  public
    FullOriginTypeName: String;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TDynArrayTypeDataContainer = class(TTypeDataContainer)
  public
    FullElementTypeName: String;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

  TProceduralTypeDataContainer = class(TTypeDataContainer)
  public
    SubDesc: TSubDesc;
    constructor Create(AOwner: TTypeInfoContainer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream); override;
    procedure LoadFromStream(S: TStream); override;
  end;

{$IFDEF PAXARM}
  TTypeInfoBuff = record
    Kind: TTypeKind;
    Name: ShortString;
  end;
{$ELSE}
  TTypeInfoBuff = TTypeInfo;
{$ENDIF}

  TTypeInfoContainer = class
  private
    Buff: Pointer;
    Buff4: Pointer;
    Processed: Boolean;
    function GetSize: Integer;
    function GetPosTypeData: Integer;
    function GetStreamSize: Integer;
    function GetIsGeneric: Boolean;
  public
    TypeInfo: TTypeInfoBuff;
    TypeDataContainer: TTypeDataContainer;
    FullName: String;
    FinTypeId: Byte;
    GenericTypeContainer: TGenericTypeContainer;

    constructor Create(AFinTypeId: Integer);
    destructor Destroy; override;
    procedure SaveToStream(S: TStream);
    procedure SaveToBuff(S: TStream);
    procedure LoadFromStream(S: TStream;
                             FinTypeId: Byte);
    procedure RaiseError(const Message: string; params: array of Const);
    property Size: Integer read GetSize;
    property PosTypeData: Integer read GetPosTypeData;
    property TypeInfoPtr: Pointer read Buff4;
    property IsGeneric: Boolean read GetIsGeneric;
  end;

  TSetTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TEnumTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TClassTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TMethodTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TInterfaceTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TArrayTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TRecordTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TAliasTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TPointerTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TClassRefTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TDynArrayTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TProceduralTypeInfoContainer = class(TTypeInfoContainer)
  public
    constructor Create(const AName: String);
  end;

  TPaxTypeInfoList = class(TTypedList)
  private
    function GetRecord(I: Integer): TTypeInfoContainer;
    procedure RaiseError(const Message: string; params: array of Const);
  public
    destructor Destroy; override;
    procedure Add(Rec: TTypeInfoContainer);
    function IndexOf(const FullName: String): Integer;
    function LookupFullName(const FullName: String): TTypeInfoContainer;
    procedure CopyToBuff;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure AddToProgram(AProg: Pointer);
    function FindMethodFullName(Address: Pointer): String;
    function Processed: Boolean;
    property Records[I: Integer]: TTypeInfoContainer read GetRecord; default;
  end;

function FinTypeToTypeKind(FinTypeId: Integer): TTypeKind;
function GetClassTypeInfoContainer(X: TObject): TClassTypeInfoContainer;
function PtiToFinType(Pti: PTypeInfo): Integer;

implementation

uses
  PAXCOMP_CLASSLST,
  PAXCOMP_STDLIB,
  PAXCOMP_BASERUNNER;

{$ifdef GE_DXETOKYO}   // XILINX, Tokyo mod
{ TTypeDataXil }

function TTypeDataXil.NameListFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@NameList);
end;

function TTypeDataXil.UnitNameFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@UnitName);
end;

function TTypeDataXil.IntfUnitFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@IntfUnit);
end;

function TTypeDataXil.DynUnitNameFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@DynUnitName);
end;

function TTypeDataXil.PropData: PPropData;
begin
  Result := PPropData(UnitNameFld.Tail)
end;

function TTypeDataXil.IntfMethods: PIntfMethodTable;
begin
  Result := PIntfMethodTable(IntfUnitFld.Tail);
end;

function TTypeDataXil.DynArrElType: PPTypeInfo;
type
  PPPTypeInfo = ^PPTypeInfo;
begin
  Result := PPPTypeInfo(DynUnitNameFld.Tail)^;
end;

function TTypeDataXil.DynArrAttrData: PAttrData;
begin
  Result := PAttrData(Self.DynUnitNameFld.Tail + SizeOf(PPTypeInfo));
end;
{$endif}

function FinTypeToTypeKind(FinTypeId: Integer): TTypeKind;
begin
  result := tkUnknown;
  case FinTypeId of

{$IFNDEF PAXARM}
    typeWIDESTRING: result := tkWString;
    typeANSISTRING: result := tkLString;
    typeANSICHAR: result := tkChar;
    typeSHORTSTRING: result := tkString;
{$ENDIF}

{$IFDEF UNIC}
    typeUNICSTRING: result := tkUString;
{$ENDIF}
    typeVARIANT, typeOLEVARIANT: result := tkVariant;
    typeINTEGER, typeBYTE, typeWORD, typeCARDINAL,
    typeSMALLINT, typeSHORTINT: result := tkInteger;
    typeENUM, typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL: result := tkEnumeration;
    typeSET: result := tkSet;
    typeWIDECHAR: result := tkWChar;
    typeSINGLE, typeDOUBLE, typeEXTENDED, typeCURRENCY: result := tkFloat;
    typeEVENT: result := tkMethod;
    typeCLASS: result := tkClass;
    typeINT64: result := tkInt64;
    typeDYNARRAY: result := tkDynArray;
    typeINTERFACE: result := tkInterface;
    typeRECORD: result := tkRecord;
    typeARRAY: result := tkArray;
  end;
end;

function PtiToFinType(Pti: PTypeInfo): Integer;
begin
  result := 0;
  case Pti^.Kind of
    tkInteger:
    case GetTypeData(pti).OrdType of
      otSByte: result := typeSMALLINT;
      otUByte: result := typeBYTE;
      otSWord: result := typeSHORTINT;
      otUWord: result := typeWORD;
      otSLong: result := typeINTEGER;
      otULong: result := typeCARDINAL;
    end;
    tkChar:
      result := typeCHAR;
    tkWChar:
      result := typeWIDECHAR;
{$IFNDEF PAXARM}
    tkString:
      result := typeSHORTSTRING;
    tkLString:
      result := typeANSISTRING;
    tkWString:
      result := typeWIDESTRING;
{$ENDIF}
{$IFDEF UNIC}
    tkUString:
      result := typeUNICSTRING;
{$ENDIF}
    tkFloat:
      case GetTypeData(pti)^.FloatType of
        ftSingle: result := typeSINGLE;
        ftDouble: result := typeDOUBLE;
        ftExtended: result := typeEXTENDED;
        ftComp: result := 0;
        ftCurr: result := typeCURRENCY;
      end;
{$IFDEF UNIC}
    tkPointer:
      result := typePOINTER;
{$ENDIF}
    tkClass:
      result := typeCLASS;
{$IFDEF UNIC}
    tkClassRef:
      result := typeCLASSREF;
    tkProcedure:
      result := typePROC;
{$ENDIF}
    tkMethod:
      result := typeEVENT;
    tkInterface:
      result := typeINTERFACE;
    tkInt64:
      result := typeINT64;
    tkEnumeration:
      result := typeENUM;
    tkVariant:
      result := typeVARIANT;
  end;
end;

// TAnotherPropRec -------------------------------------------------------------

constructor TAnotherPropRec.Create;
begin
  inherited;
  ParamNames := TStringList.Create;
  ParamTypes := TStringList.Create;
end;

destructor TAnotherPropRec.Destroy;
begin
  FreeAndNil(ParamNames);
  FreeAndNil(ParamTypes);
  inherited;
end;

procedure TAnotherPropRec.SaveToStream(S: TStream);
begin
  S.Write(Vis, SizeOf(Vis));
  SaveStringToStream(PropName, S);
  SaveStringListToStream(ParamNames, S);
  SaveStringListToStream(ParamTypes, S);
  SaveStringToStream(PropType, S);
  SaveStringToStream(ReadName, S);
  SaveStringToStream(WriteName, S);
  S.Write(IsDefault, SizeOf(IsDefault));
end;

procedure TAnotherPropRec.LoadFromStream(S: TStream);
begin
  S.Read(Vis, SizeOf(Vis));
  PropName := LoadStringFromStream(S);
  LoadStringListFromStream(ParamNames, S);
  LoadStringListFromStream(ParamTypes, S);
  PropType := LoadStringFromStream(S);
  ReadName := LoadStringFromStream(S);
  WriteName := LoadStringFromStream(S);
  S.Read(IsDefault, SizeOf(IsDefault));
end;

// TAnotherPropList ------------------------------------------------------------

function TAnotherPropList.GetRecord(I: Integer): TAnotherPropRec;
begin
  result := TAnotherPropRec(L[I]);
end;

function TAnotherPropList.Add: TAnotherPropRec;
begin
  result := TAnotherPropRec.Create;
  L.Add(result);
end;

procedure TAnotherPropList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TAnotherPropList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TAnotherPropRec;
begin
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
  begin
    R := Add;
    R.LoadFromStream(S);
  end;
end;

// TFieldDataContainer ---------------------------------------------------------

procedure TFieldDataContainer.SaveToStream(S: TStream);
begin
  S.Write(Offset, SizeOf(Offset));
  S.Write(ClassIndex, SizeOf(ClassIndex));
  SaveShortStringToStream(Name, S);
  SaveStringToStream(FullFieldTypeName, S);
  S.Write(Vis, SizeOf(Vis));
{$IFDEF PCU_EX}
  S.Write(FinalFieldTypeId, SizeOf(FinalFieldTypeId));
{$ENDIF}
end;

procedure TFieldDataContainer.LoadFromStream(S: TStream);
begin
  S.Read(Offset, SizeOf(Offset));
  S.Read(ClassIndex, SizeOf(ClassIndex));
  Name := LoadShortStringFromStream(S);
  FullFieldTypeName := LoadStringFromStream(S);
  S.Read(Vis, SizeOf(Vis));
{$IFDEF PCU_EX}
  S.Read(FinalFieldTypeId, SizeOf(FinalFieldTypeId));
{$ENDIF}
end;

// TFieldListContainer ---------------------------------------------------------

function TFieldListContainer.GetRecord(I: Integer): TFieldDataContainer;
begin
  result := TFieldDataContainer(L[I]);
end;

function TFieldListContainer.Add: TFieldDataContainer;
begin
  result := TFieldDataContainer.Create;
  L.Add(result);
end;

procedure TFieldListContainer.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    Records[I].SaveToStream(S);
end;

procedure TFieldListContainer.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TFieldDataContainer;
begin
  S.Read(K, SizeOf(K));
  for I := 0 to K - 1 do
  begin
    R := Add;
    R.LoadFromStream(S);
  end;
end;

// TParamListContainer ---------------------------------------------------------

constructor TParamListContainer.Create(AOwner: TMethodTypeDataContainer);
begin
  inherited Create;
  Owner := AOwner;
end;

destructor TParamListContainer.Destroy;
begin
  inherited;
end;

function TParamListContainer.GetCount: Integer;
begin
  result := System.Length(ParamList);
end;

function TParamListContainer.GetSize: Integer;
var
  I: Integer;
begin
  result := 0;
  for I := 0 to Count - 1 do
  begin
    Inc(result, SizeOf(ParamList[I].Flags));
    Inc(result, Length(ParamList[I].ParamName) + 1);
    Inc(result, Length(ParamList[I].TypeName) + 1);
  end;
end;

procedure TParamListContainer.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I := 0 to Count - 1 do
  with ParamList[I] do
  begin
    S.Write(Flags, SizeOf(Flags));
    SaveShortStringToStream(ParamName, S);
    SaveShortStringToStream(TypeName, S);
  end;
end;

procedure TParamListContainer.SaveToBuff(S: TStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  with ParamList[I] do
  begin
    S.Write(Flags, SizeOf(Flags));
    SaveShortStringToStream(ParamName, S);
    SaveShortStringToStream(TypeName, S);
  end;
end;

procedure TParamListContainer.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(K, SizeOf(Integer));
  SetLength(ParamList, K);
  for I := 0 to Count - 1 do
  with ParamList[I] do
  begin
    S.Read(Flags, SizeOf(Flags));
    ParamName := LoadShortStringFromStream(S);
    TypeName := LoadShortStringFromStream(S);
  end;
end;

// TPropDataContainer -------------------------------------------------------

constructor TPropDataContainer.Create(AOwner: TTypeDataContainer);
begin
  Owner := AOwner;
  ReadNames := TStringList.Create;
  WriteNames := TStringList.Create;
  PropTypeNames := TStringList.Create;
  inherited Create;
end;

function PropInfoSize(const PropInfo: TPropInfo): Integer;
begin
{$IFDEF PAXARM}
  result := SizeOf(TPropInfo) - SizeOf(ShortString) +
  {$IFDEF ARC}
            PropInfo.Name + 1;
  {$ELSE}
            Length(PropInfo.Name) + 1;
  {$ENDIF}
{$ELSE}
  result := SizeOf(TPropInfo) - SizeOf(ShortString) +
            Length(PropInfo.Name) + 1;
{$ENDIF}
end;

destructor TPropDataContainer.Destroy;
begin
  FreeAndNil(ReadNames);
  FreeAndNil(WriteNames);
  FreeAndNil(PropTypeNames);
  inherited;
end;

function TPropDataContainer.GetCount: Integer;
begin
  result := PropData.PropCount;
end;

function TPropDataContainer.GetSize: Integer;
var
  I: Integer;
begin
  result := SizeOf(TPropData);
  for I := 0 to Count - 1 do
    Inc(result,  PropInfoSize(PropList[I]));
{$IFDEF DRTTI}
  Inc(result, SizeOf(TPropDataEx));
{$ELSE}
  {$IFDEF DPULSAR}
   Inc(result, SizeOf(TPropInfoEx));
  {$ENDIF}
{$ENDIF}
end;

procedure TPropDataContainer.SaveToStream(S: TStream);
var
  I, SZ: Integer;
begin
  S.Write(PropData, SizeOf(TPropData));
  for I := 0 to Count - 1 do
  begin
    SZ := PropInfoSize(PropList[I]);
    S.Write(SZ, SizeOf(Integer));
    S.Write(PropList[I], SZ);
  end;
  SaveStringListToStream(ReadNames, S);
  SaveStringListToStream(WriteNames, S);
  SaveStringListToStream(PropTypeNames, S);
  for I := 0 to Count - 1 do
    S.Write(PropTypeIds[I], SizeOf(PropTypeIds[I]));
end;

procedure TPropDataContainer.SaveToBuff(S: TStream);
var
  I: Integer;
{$IFDEF DRTTI}
  PropEx: TPropDataEx;
{$ELSE}
  {$IFDEF DPULSAR}
  PropEx: TPropDataEx;
  {$ENDIF}
{$ENDIF}
begin
  S.Write(PropData, SizeOf(TPropData));
  for I := 0 to Count - 1 do
    S.Write(PropList[I], PropInfoSize(PropList[I]));

{$IFDEF DRTTI}
  FillChar(PropEx, SizeOf(PropEx), #0);
  S.Write(PropEx, SizeOf(TPropDataEx));
{$ELSE}
  {$IFDEF DPULSAR}
  FillChar(PropEx, SizeOf(PropEx), #0);
  S.Write(PropEx, SizeOf(TPropDataEx));
  {$ENDIF}
{$ENDIF}
end;

procedure TPropDataContainer.LoadFromStream(S: TStream);
var
  I, SZ: Integer;
begin
  S.Read(PropData, SizeOf(TPropData));
  SetLength(PropList, PropData.PropCount);
  for I := 0 to PropData.PropCount - 1 do
  begin
    S.Read(SZ, SizeOf(Integer));
    S.Read(PropList[I], SZ);
  end;
  LoadStringListFromStream(ReadNames, S);
  LoadStringListFromStream(WriteNames, S);
  LoadStringListFromStream(PropTypeNames, S);
  SetLength(PropTypeIds, PropData.PropCount);
  for I := 0 to PropData.PropCount - 1 do
    S.Read(PropTypeIds[I], SizeOf(PropTypeIds));
end;

// TTypeDataContainer -------------------------------------------------------

constructor TTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  Owner := AOwner;
  inherited Create;
end;

destructor TTypeDataContainer.Destroy;
begin
  inherited;
end;

function TTypeDataContainer.GetTypeDataSize: Integer;
begin
  result := SizeOf(TypeData);
end;

function TTypeDataContainer.GetSize: Integer;
begin
  result := SizeOf(TypeData);
end;

procedure TTypeDataContainer.SaveToStream(S: TStream);
begin
  S.Write(TypeData, 16);
end;

procedure TTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, SizeOf(TypeData));
end;

procedure TTypeDataContainer.LoadFromStream(S: TStream);
begin
  FillChar(TypeData, SizeOf(TypeData), 0);
  S.Read(TypeData, 16);
end;

// TMethodTypeDataContainer ----------------------------------------------------

constructor TMethodTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  inherited;
  ParamListContainer := TParamListContainer.Create(Self);
end;

destructor TMethodTypeDataContainer.Destroy;
begin
  FreeAndNil(ParamListContainer);
  inherited;
end;

function TMethodTypeDataContainer.GetTypeDataSize: Integer;
begin
  result := SizeOf(MethodKind) + SizeOf(ParamCount);
end;

function TMethodTypeDataContainer.GetSize: Integer;
begin
  result := TypeDataSize + ParamListContainer.Size +
    Length(ResultType) + 1;
end;

procedure TMethodTypeDataContainer.SaveToStream(S: TStream);
begin
  S.Write(MethodKind, SizeOf(MethodKind));
  S.Write(ParamCount, SizeOf(ParamCount));
  ParamListContainer.SaveToStream(S);
  SaveShortStringToStream(ResultType, S);
  SaveStringToStream(OwnerTypeName, S);
  S.Write(MethodTableIndex, SizeOf(MethodTableIndex));
  S.Write(ResultTypeId, SizeOf(ResultTypeId));
  S.Write(CallConv, SizeOf(CallConv));
  S.Write(OverCount, SizeOf(OverCount));
end;

procedure TMethodTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(MethodKind, SizeOf(MethodKind));
  S.Write(ParamCount, SizeOf(ParamCount));
  ParamListContainer.SaveToBuff(S);
  SaveShortStringToStream(ResultType, S);
end;

procedure TMethodTypeDataContainer.LoadFromStream(S: TStream);
begin
  S.Read(MethodKind, SizeOf(MethodKind));
  S.Read(ParamCount, SizeOf(ParamCount));
  ParamListContainer.LoadFromStream(S);
  ResultType := LoadShortStringFromStream(S);
  OwnerTypeName := LoadStringFromStream(S);
  S.Read(MethodTableIndex, SizeOf(MethodTableIndex));
  S.Read(ResultTypeId, SizeOf(ResultTypeId));
  S.Read(CallConv, SizeOf(CallConv));
  S.Read(OverCount, SizeOf(OverCount));
end;

// TClassTypeDataContainer -----------------------------------------------------

constructor TClassTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  inherited;

  PropDataContainer := TPropDataContainer.Create(Self);
  FieldListContainer := TFieldListContainer.Create;

  AnotherFieldListContainer := TFieldListContainer.Create;
  AnotherPropList := TAnotherPropList.Create;
  SupportedInterfaces := TStringList.Create;

end;

destructor TClassTypeDataContainer.Destroy;
begin
  FreeAndNil(PropDataContainer);
  FreeAndNil(FieldListContainer);

  FreeAndNil(AnotherFieldListContainer);
  FreeAndNil(AnotherPropList);
  FreeAndNil(SupportedInterfaces);

  inherited;
end;

function TClassTypeDataContainer.GetTypeDataSize: Integer;
begin
{$IFDEF PAXARM}
  result := SizeOf(TypeData.ClassType) +
            SizeOf(TypeData.ParentInfo) +
            SizeOf(TypeData.PropCount) +
    {$IFDEF ARC}
            TypeData.UnitName + 1;
    {$ELSE}
            Length(TypeData.UnitName) + 1;
    {$ENDIF}
{$ELSE}
  result := SizeOf(TypeData.ClassType) +
            SizeOf(TypeData.ParentInfo) +
            SizeOf(TypeData.PropCount) +
            Length(TypeData.UnitName) + 1;
{$ENDIF}
end;

function TClassTypeDataContainer.GetSize: Integer;
begin
  result := TypeDataSize +
            PropDataContainer.Size;
end;

procedure TClassTypeDataContainer.SaveToStream(S: TStream);
var
  K: Integer;
begin
  K := TypeDataSize;
  S.Write(K, SizeOf(K));
  S.Write(TypeData, TypeDataSize);
  PropDataContainer.SaveToStream(S);
  S.Write(MethodTableCount, SizeOf(MethodTableCount));
  S.Write(MethodTableSize, SizeOf(MethodTableSize));
  S.Write(FieldTableCount, SizeOf(FieldTableCount));
  S.Write(FieldTableSize, SizeOf(FieldTableSize));
  SaveStringToStream(FullParentName, S);
  FieldListContainer.SaveToStream(S);
  AnotherFieldListContainer.SaveToStream(S);
  AnotherPropList.SaveToStream(S);
  SaveStringListToStream(SupportedInterfaces, S);
end;

procedure TClassTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, TypeDataSize);
  PropDataContainer.SaveToBuff(S);
end;

procedure TClassTypeDataContainer.LoadFromStream(S: TStream);
var
  K: Integer;
begin
  S.Read(K, SizeOf(K));
  S.Read(TypeData, K);
  PropDataContainer.LoadFromStream(S);
  S.Read(MethodTableCount, SizeOf(MethodTableCount));
  S.Read(MethodTableSize, SizeOf(MethodTableSize));
  S.Read(FieldTableCount, SizeOf(FieldTableCount));
  S.Read(FieldTableSize, SizeOf(FieldTableSize));
  FullParentName := LoadStringFromStream(S);
  FieldListContainer.LoadFromStream(S);
  AnotherFieldListContainer.LoadFromStream(S);
  AnotherPropList.LoadFromStream(S);
  LoadStringListFromStream(SupportedInterfaces, S);
end;

// TSetTypeDataContainer -------------------------------------------------

function TSetTypeDataContainer.GetSize: Integer;
begin
  result := SizeOf(TypeData);
end;

procedure TSetTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullCompName, S);
end;

procedure TSetTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, SizeOf(TypeData));
end;

procedure TSetTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullCompName := LoadStringFromStream(S);
end;


// TEnumTypeDataContainer ------------------------------------------------------

function TEnumTypeDataContainer.GetTypeDataSize: Integer;
begin
  result := SizeOf(TOrdType) +
            SizeOf(Longint) + // min value
            SizeOf(Longint) + // max value
            SizeOf(Pointer); // base type
end;

function TEnumTypeDataContainer.GetSize: Integer;
begin
  result := GetTypeDataSize;

{
  for I := 0 to Length(NameList) - 1 do
    Inc(result, Length(NameList[I]) + 1);
}
  Inc(result, 256);

  Inc(result, Length(EnumUnitName) + 1);
end;

procedure TEnumTypeDataContainer.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  S.Write(TypeData, GetTypeDataSize);
  K := System.Length(NameList);
  S.Write(K, SizeOf(K));
  for I := 0 to K - 1 do
    SaveShortStringToStream(NameList[I], S);
  SaveShortStringToStream(EnumUnitName, S);
  for I := 0 to K - 1 do
    S.Write(ValueList[I], SizeOf(ValueList[I]));
end;

procedure TEnumTypeDataContainer.SaveToBuff(S: TStream);
var
  I, K, Z: Integer;
  B: Byte;
begin
  S.Write(TypeData, GetTypeDataSize);
  K := System.Length(NameList);
  Z := 0;
  for I := 0 to K - 1 do
  begin
    SaveShortStringToStream(NameList[I], S);
    Inc(z, Length(NameList[I]) + 1);
  end;
  B := 0;
  while Z < 256 do
  begin
    Inc(Z);
    S.Write(B, 1);
  end;
  SaveShortStringToStream(EnumUnitName, S);
end;

procedure TEnumTypeDataContainer.LoadFromStream(S: TStream);
var
  I, K: Integer;
begin
  S.Read(TypeData, GetTypeDataSize);
  S.Read(K, SizeOf(K));
  SetLength(NameList, K);
  for I := 0 to K - 1 do
    NameList[I] := LoadShortStringFromStream(S);
  EnumUnitName := LoadShortStringFromStream(S);
  SetLength(ValueList, K);
  for I := 0 to K - 1 do
    S.Read(ValueList[I], SizeOf(ValueList[I]));
end;

// TInterfaceTypeDataContainer -------------------------------------------------

constructor TInterfaceTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  inherited;
  PropDataContainer := TPropDataContainer.Create(Self);
  SubDescList := TSubDescList.Create;
end;

destructor TInterfaceTypeDataContainer.Destroy;
begin
  FreeAndNil(PropDataContainer);
  FreeAndNil(SubDescList);
  inherited;
end;

function TInterfaceTypeDataContainer.GetTypeDataSize: Integer;
begin
{$IFDEF PAXARM}
  result := SizeOf(TypeData.IntfParent) +
            SizeOf(TypeData.IntfFlags) +
            SizeOf(TypeData.Guid) +
    {$IFDEF ARC}
            TypeData.IntfUnit + 1;
    {$ELSE}
            Length(TypeData.IntfUnit) + 1;
    {$ENDIF}
{$ELSE}
  result := SizeOf(TypeData.IntfParent) +
            SizeOf(TypeData.IntfFlags) +
            SizeOf(TypeData.Guid) +
            Length(TypeData.IntfUnit) + 1;
{$ENDIF}
end;

function TInterfaceTypeDataContainer.GetSize: Integer;
begin
  result := TypeDataSize +
            PropDataContainer.Size;
end;

procedure TInterfaceTypeDataContainer.SaveToStream(S: TStream);
var
  K: Integer;
begin
  K := TypeDataSize;
  S.Write(K, SizeOf(K));
  S.Write(TypeData, TypeDataSize);
  PropDataContainer.SaveToStream(S);
  SaveStringToStream(FullParentName, S);
  SubDescList.SaveToStream(S);
  S.Write(GUID, SizeOf(GUID));
end;

procedure TInterfaceTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, TypeDataSize);
  PropDataContainer.SaveToBuff(S);
end;

procedure TInterfaceTypeDataContainer.LoadFromStream(S: TStream);
var
  K: Integer;
begin
  S.Read(K, SizeOf(K));
  S.Read(TypeData, K);
  PropDataContainer.LoadFromStream(S);
  FullParentName := LoadStringFromStream(S);
  SubDescList.LoadFromStream(S);
  S.Read(GUID, SizeOf(GUID));
end;

// TArrayTypeDataContainer -----------------------------------------------------

procedure TArrayTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullRangeTypeName, S);
  SaveStringToStream(FullElemTypeName, S);
  S.Write(B1, SizeOf(B1));
  S.Write(B2, SizeOf(B2));
  S.Write(FinRangeTypeId, SizeOf(FinRangeTypeId));
end;

procedure TArrayTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, SizeOf(TypeData));
end;

procedure TArrayTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullRangeTypeName := LoadStringFromStream(S);
  FullElemTypeName := LoadStringFromStream(S);
  S.Read(B1, SizeOf(B1));
  S.Read(B2, SizeOf(B2));
  S.Read(FinRangeTypeId, SizeOf(FinRangeTypeId));
end;

// TRecordTypeDataContainer ----------------------------------------------------

constructor TRecordTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  inherited;
  FieldListContainer := TFieldListContainer.Create;
end;

destructor TRecordTypeDataContainer.Destroy;
begin
  FreeAndNil(FieldListContainer);
  inherited;
end;

function TRecordTypeDataContainer.GetSize: Integer;
begin
  result := SizeOf(TypeData);
end;

procedure TRecordTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  S.Write(IsPacked, SizeOf(IsPacked));
  FieldListContainer.SaveToStream(S);
end;

procedure TRecordTypeDataContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeData, SizeOf(TypeData));
end;

procedure TRecordTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  S.Read(IsPacked, SizeOf(IsPacked));
  FieldListContainer.LoadFromStream(S);
end;

// TAliasTypeDataContainer -----------------------------------------------------

procedure TAliasTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullSourceTypeName, S);
end;

procedure TAliasTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullSourceTypeName := LoadStringFromStream(S);
end;

// TPointerTypeDataContainer ---------------------------------------------------

procedure TPointerTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullOriginTypeName, S);
end;

procedure TPointerTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullOriginTypeName := LoadStringFromStream(S);
end;

// TClassRefTypeDataContainer --------------------------------------------------

procedure TClassRefTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullOriginTypeName, S);
end;

procedure TClassRefTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullOriginTypeName := LoadStringFromStream(S);
end;

// TDynArrayTypeDataContainer --------------------------------------------------

procedure TDynArrayTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SaveStringToStream(FullElementTypeName, S);
end;

procedure TDynArrayTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  FullElementTypeName := LoadStringFromStream(S);
end;

// TProceduralTypeDataContainer ------------------------------------------------

constructor TProceduralTypeDataContainer.Create(AOwner: TTypeInfoContainer);
begin
  inherited;
  SubDesc := TSubDesc.Create;
end;

destructor TProceduralTypeDataContainer.Destroy;
begin
  FreeAndNil(SubDesc);
  inherited;
end;

procedure TProceduralTypeDataContainer.SaveToStream(S: TStream);
begin
  inherited;
  SubDesc.SaveToStream(S);
end;

procedure TProceduralTypeDataContainer.LoadFromStream(S: TStream);
begin
  inherited;
  SubDesc.LoadFromStream(S);
end;

// TEnumTypeInfoContainer ------------------------------------------------------

constructor TEnumTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkEnumeration;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TEnumTypeDataContainer.Create(Self);
  FinTypeId := typeENUM;
end;

// TSetTypeInfoContainer -----------------------------------------------------

constructor TSetTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkSet;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TSetTypeDataContainer.Create(Self);
  FinTypeId := typeSET;
end;

// TClassTypeInfoContainer -----------------------------------------------------

constructor TClassTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkClass;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TClassTypeDataContainer.Create(Self);
  FinTypeId := typeCLASS;
end;

// TInterfaceTypeInfoContainer -----------------------------------------------------

constructor TInterfaceTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkInterface;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TInterfaceTypeDataContainer.Create(Self);
  FinTypeId := typeINTERFACE;
end;

// TMethodTypeInfoContainer -----------------------------------------------------

constructor TMethodTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkMethod;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TMethodTypeDataContainer.Create(Self);
  FinTypeId := typeEVENT;
end;

// TArrayTypeInfoContainer -----------------------------------------------------

constructor TArrayTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkArray;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TArrayTypeDataContainer.Create(Self);
  FinTypeId := typeARRAY;
end;

// TRecordTypeInfoContainer ----------------------------------------------------

constructor TRecordTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkRecord;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TRecordTypeDataContainer.Create(Self);
  FinTypeId := typeRECORD;
end;

// TAliasTypeInfoContainer -----------------------------------------------------

constructor TAliasTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkUnknown;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TAliasTypeDataContainer.Create(Self);
  FinTypeId := typeALIAS;
end;

// TPointerTypeInfoContainer ---------------------------------------------------

constructor TPointerTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkUnknown;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TPointerTypeDataContainer.Create(Self);
  FinTypeId := typePOINTER;
end;

// TClassRefTypeInfoContainer --------------------------------------------------

constructor TClassRefTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkUnknown;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TClassRefTypeDataContainer.Create(Self);
  FinTypeId := typeCLASSREF;
end;

// TDynArrayTypeInfoContainer --------------------------------------------------

constructor TDynArrayTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkDynArray;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TDynArrayTypeDataContainer.Create(Self);
  FinTypeId := typeDYNARRAY;
end;

// TProceduralTypeInfoContainer ------------------------------------------------

constructor TProceduralTypeInfoContainer.Create(const AName: String);
begin
  inherited Create(0);
  TypeInfo.Kind := tkUnknown;
  PShortStringFromString(PShortString(@TypeInfo.Name), AName);
  FreeAndNil(TypeDataContainer);
  TypeDataContainer := TProceduralTypeDataContainer.Create(Self);
  FinTypeId := typePROC;
end;

// TTypeInfoContainer -------------------------------------------------------

constructor TTypeInfoContainer.Create(AFinTypeId: Integer);
begin
  inherited Create;
  TypeDataContainer := TTypeDataContainer.Create(Self);
  FinTypeId := AFinTypeId;
  GenericTypeContainer := TGenericTypeContainer.Create;
end;

destructor TTypeInfoContainer.Destroy;
begin
  if Assigned(Buff) then
    FreeMem(Buff, Size);
  FreeAndNil(TypeDataContainer);
  FreeAndNil(GenericTypeContainer);

  inherited;
end;

function TTypeInfoContainer.GetIsGeneric: Boolean;
begin
  result := GenericTypeContainer.Definition <> '';
end;

function TTypeInfoContainer.GetSize: Integer;
begin
{$IFDEF ARC}
  result := SizeOf(TTypeKind) + TypeInfo.Name[0] + 1 +
    TypeDataContainer.Size;
{$ELSE}
  result := SizeOf(TTypeKind) + Length(TypeInfo.Name) + 1 +
    TypeDataContainer.Size;
{$ENDIF}
end;

function TTypeInfoContainer.GetPosTypeData: Integer;
begin
{$IFDEF ARC}
  result := SizeOf(TTypeKind) + TypeInfo.Name[0] + 1;
{$ELSE}
  result := SizeOf(TTypeKind) + Length(TypeInfo.Name) + 1;
{$ENDIF}
end;

function TTypeInfoContainer.GetStreamSize: Integer;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    SaveToStream(M);
    result := M.Size;
  finally
    FreeAndNil(M);
  end;
end;

procedure TTypeInfoContainer.SaveToStream(S: TStream);
begin
  S.Write(TypeInfo.Kind, SizeOf(TTypeKind));
  SaveShortStringToStream(PShortString(@TypeInfo.Name)^, S);
  TypeDataContainer.SaveToStream(S);
  SaveStringToStream(FullName, S);
  GenericTypeContainer.SaveToStream(S);
end;

procedure TTypeInfoContainer.SaveToBuff(S: TStream);
begin
  S.Write(TypeInfo.Kind, SizeOf(TTypeKind));
  SaveShortStringToStream(PShortString(@TypeInfo.Name)^, S);
  TypeDataContainer.SaveToBuff(S);
end;

procedure TTypeInfoContainer.LoadFromStream(S: TStream;
                                            FinTypeId: Byte);
begin
  S.Read(TypeInfo.Kind, SizeOf(TTypeKind));
  _ShortStringAssign(LoadShortStringFromStream(S), 255, PShortString(@TypeInfo.Name));

  FreeAndNil(TypeDataContainer);

  case FinTypeId of
    typeCLASS: TypeDataContainer := TClassTypeDataContainer.Create(Self);
    typeINTERFACE: TypeDataContainer := TInterfaceTypeDataContainer.Create(Self);
    typeEVENT: TypeDataContainer := TMethodTypeDataContainer.Create(Self);
    typeSET: TypeDataContainer := TSetTypeDataContainer.Create(Self);
    typeENUM: TypeDataContainer := TEnumTypeDataContainer.Create(Self);
    typeARRAY: TypeDataContainer := TArrayTypeDataContainer.Create(Self);
    typeRECORD: TypeDataContainer := TRecordTypeDataContainer.Create(Self);
    typeALIAS: TypeDataContainer := TAliasTypeDataContainer.Create(Self);
    typePOINTER: TypeDataContainer := TPointerTypeDataContainer.Create(Self);
    typeCLASSREF: TypeDataContainer := TClassRefTypeDataContainer.Create(Self);
    typeDYNARRAY: TypeDataContainer := TDynArrayTypeDataContainer.Create(Self);
    typePROC: TypeDataContainer := TProceduralTypeDataContainer.Create(Self);
  else
    TypeDataContainer := TTypeDataContainer.Create(Self);
  end;

  TypeDataContainer.LoadFromStream(S);
  FullName := LoadStringFromStream(S);
  GenericTypeContainer.LoadFromStream(S);
end;

procedure TTypeInfoContainer.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

// TPaxTypeInfoList ------------------------------------------------------------

destructor TPaxTypeInfoList.Destroy;
begin
  inherited;
end;

function TPaxTypeInfoList.GetRecord(I: Integer): TTypeInfoContainer;
begin
  result := TTypeInfoContainer(L[I]);
end;

procedure TPaxTypeInfoList.Add(Rec: TTypeInfoContainer);
begin
  L.Add(Rec);
end;

procedure TPaxTypeInfoList.SaveToStream(S: TStream);
var
  I, K: Integer;
begin
  K := Count;
  S.Write(K, SizeOf(Integer));
  for I:=0 to K - 1 do
  begin
    S.Write(Records[I].FinTypeId, SizeOf(Records[I].FinTypeId));
    Records[I].SaveToStream(S);
  end;
end;

procedure TPaxTypeInfoList.LoadFromStream(S: TStream);
var
  I, K: Integer;
  R: TTypeInfoContainer;
  FinTypeId: Byte;
begin
  Clear;

  S.Read(K, SizeOf(Integer));
  for I:=0 to K - 1 do
  begin
    S.Read(FinTypeId, SizeOf(Byte));

    case FinTypeId of
      typeCLASS: R := TClassTypeInfoContainer.Create('');
      typeINTERFACE: R := TInterfaceTypeInfoContainer.Create('');
      typeEVENT: R := TMethodTypeInfoContainer.Create('');
      typeSET: R := TSetTypeInfoContainer.Create('');
      typeENUM: R := TEnumTypeInfoContainer.Create('');
      typeARRAY: R := TArrayTypeInfoContainer.Create('');
      typeRECORD: R := TRecordTypeInfoContainer.Create('');
      typeALIAS: R := TAliasTypeInfoContainer.Create('');
      typePOINTER: R := TPointerTypeInfoContainer.Create('');
      typeCLASSREF: R := TClassRefTypeInfoContainer.Create('');
      typeDYNARRAY: R := TDynArrayTypeInfoContainer.Create('');
      typePROC: R := TProceduralTypeInfoContainer.Create('');
    else
      R := TTypeInfoContainer.Create(FinTypeId);
    end;

    R.LoadFromStream(S, FinTypeId);

    Add(R);
  end;
end;

function TPaxTypeInfoList.LookupFullName(const FullName: String): TTypeInfoContainer;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to Count - 1 do
    if StrEql(FullName, String(Records[I].FullName)) then
    begin
      result := Records[I];
     Exit;
    end;
end;

function TPaxTypeInfoList.IndexOf(const FullName: String): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to Count - 1 do
    if StrEql(FullName, String(Records[I].FullName)) then
    begin
      result := I;
      Exit;
    end;
end;

procedure TPaxTypeInfoList.CopyToBuff;
var
  S: TMemoryStream;
  I, SZ, StreamSize, K: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    SZ := Records[I].Size;
    StreamSize := Records[I].GetStreamSize;

    K := SizeOf(Integer) + SZ +
         SizeOf(Integer) + StreamSize;

    Records[I].Buff := AllocMem(K);
    Records[I].Buff4 := ShiftPointer(Records[I].Buff, 4);

    S := TMemoryStream.Create;
    try
      S.Write(SZ, SizeOf(Integer));
      Records[I].SaveToBuff(S);

      S.Write(StreamSize, SizeOf(Integer));
      Records[I].SaveToStream(S);

      S.Position := 0;
      S.Read(Records[I].Buff^, K);

    finally
      FreeAndNil(S);
    end;
  end;
end;

procedure TPaxTypeInfoList.AddToProgram(AProg: Pointer);
var
  ClassFactory: TPaxClassFactory;
  I, J, JJ: Integer;
  P: Pointer;
  R: TPaxClassFactoryRec;
  C: TClass;
  ptd: PTypeData;
  pti, pti_parent: PTypeInfo;
  Record_Parent, Record_Temp: TTypeInfoContainer;
  PropDataContainer: TPropDataContainer;
  Prog: TBaseRunner;
  FullName: String;
  ppi: PPropInfo;
  Z, ZZ: Integer;

  ClassTypeInfoContainer: TClassTypeInfoContainer;
  ClassTypeDataContainer: TClassTypeDataContainer;

  MethodTypeInfoContainer: TMethodTypeInfoContainer;
  MethodTypeDataContainer: TMethodTypeDataContainer;

  InterfaceTypeDataContainer: TInterfaceTypeDataContainer;
  SetTypeDataContainer: TSetTypeDataContainer;

  MethodTableIndex: Integer;
  PMethod: PVmtMethod;

  FieldListContainer: TFieldListContainer;
  PField: PVmtField;
  ClassRec: TClassRec;
  RI: TTypeInfoContainer;
  ParentPropCount: Integer;
  MR, SomeMR: TMapRec;
  FileName, ProcName: String;
  DestProg: Pointer;
begin
  Prog := TBaseRunner(AProg);
  ClassFactory := Prog.ProgClassFactory;

  CopyToBuff;

  for I:=0 to Count - 1 do
    Records[I].Processed := false;

  repeat

    for I:=0 to Count - 1 do
    begin
      RI := Records[I];
      if RI.Processed then
        continue;

      pti := RI.Buff4;

      case RI.TypeInfo.Kind of
        tkEnumeration:
        begin
          ptd := ShiftPointer(pti, RI.PosTypeData);
{$IFDEF FPC}
          ptd^.BaseType := RI.Buff4;
{$ELSE}
          ptd^.BaseType := @ RI.Buff4;
{$ENDIF}

          RI.Processed := true;
        end;
        tkSet:
        begin
          RI.Processed := true;

          ptd := ShiftPointer(pti, RI.PosTypeData);

          SetTypeDataContainer := RI.TypeDataContainer as
                                    TSetTypeDataContainer;

          Record_Temp := LookupFullName(SetTypeDataContainer.FullCompName);
          if Record_Temp = nil then
            ptd^.CompType := nil
          else
{$IFDEF FPC}
            ptd^.CompType := Record_Temp.buff4;
{$ELSE}
            ptd^.CompType := @ Record_Temp.buff4;
{$ENDIF}

        end;
        tkMethod:
        begin
          RI.Processed := true;

          MethodTypeInfoContainer := TMethodTypeInfoContainer(RI);
          MethodTypeDataContainer := TMethodTypeDataContainer(MethodTypeInfoContainer.TypeDataContainer);
          if MethodTypeDataContainer.OwnerTypeName = '' then
            continue;

          Record_Temp := LookupFullName(MethodTypeDataContainer.OwnerTypeName);
          if Record_Temp = nil then
            RaiseError(errInternalError, []);
          R := ClassFactory.FindRecordByFullName(MethodTypeDataContainer.OwnerTypeName);
          if R = nil then
            RaiseError(errInternalError, []);

          ClassTypeInfoContainer := Record_Temp as
                                    TClassTypeInfoContainer;
          ClassTypeDataContainer := Record_Temp.TypeDataContainer as
                                    TClassTypeDataContainer;

          if R.MethodTableSize = 0 then
          begin
            R.MethodTableSize := ClassTypeDataContainer.MethodTableSize;
            vmtMethodTableSlot(R.VMTPtr)^ := AllocMem(R.MethodTableSize);
            PVmtMethodTable(vmtMethodTableSlot(R.VMTPtr)^)^.Count :=
               ClassTypeDataContainer.MethodTableCount;
          end;
          PMethod := ShiftPointer(vmtMethodTableSlot(R.VMTPtr)^,
            SizeOf(TVmtMethodCount));

          MethodTableIndex := MethodTypeDataContainer.MethodTableIndex;

          for J := 0 to MethodTableIndex - 1 do
            PMethod := ShiftPointer(PMethod, GetMethodSize(PMethod));

{$IFDEF FPC}
          PMethod^.MethName := @ MethodTypeInfoContainer.TypeInfo.Name;
          FullName := MethodTypeDataContainer.OwnerTypeName + '.' +
                      String(PMethod^.MethName^);
          PMethod^.MethAddr := Prog.GetAddress(FullName, MR);

          DestProg := Prog;
          if PMethod^.MethAddr = nil then
          begin
            FileName := ExtractOwner(FullName) + '.' + PCU_FILE_EXT;
            ProcName := Copy(FullName, PosCh('.', FullName) + 1, Length(FullName));
            PMethod^.MethAddr := Prog.LoadAddressEx(FileName, ProcName, false,
              MethodTypeDataContainer.OverCount, SomeMR, DestProg);
          end;
          TBaseRunner(DestProg).WrapMethodAddress(PMethod^.MethAddr);
          MethodTypeDataContainer.Address := PMethod^.MethAddr;
{$ELSE}
          _ShortStringAssign(PShortString(@MethodTypeInfoContainer.TypeInfo.Name)^,
                             255,
                             PShortString(@PMethod^.Name));
          FullName := MethodTypeDataContainer.OwnerTypeName + '.' +
                      StringFromPShortString(PShortString(@PMethod^.Name));
          PMethod^.Address := Prog.GetAddress(FullName, MR);
          DestProg := Prog;
          if PMethod^.Address = nil then
          begin
            FileName := ExtractOwner(FullName) + '.' + PCU_FILE_EXT;
            ProcName := Copy(FullName, PosCh('.', FullName) + 1, Length(FullName));
            PMethod^.Address := Prog.LoadAddressEx(FileName, ProcName, false,
              MethodTypeDataContainer.OverCount, SomeMR, DestProg);
          end;

          TBaseRunner(DestProg).WrapMethodAddress(PMethod^.Address);

          {$ifdef PAX64}
          PMethod^.Size := SizeOf(Word) +
                           SizeOf(Pointer) +
                           Length(PMethod^.Name) + 1;
          {$endif}

          {$ifdef WIN32}
          PMethod^.Size := SizeOf(Word) +
                           SizeOf(Pointer) +
                           Length(PMethod^.Name) + 1;
          {$endif}

          {$ifdef MACOS}
          PMethod^.Size := SizeOf(Word) +
                           SizeOf(Pointer) +
                           Length(PMethod^.Name) + 1;
          {$endif}

          {$ifdef ARC}
          PMethod^.Size := SizeOf(Word) +
                           SizeOf(Pointer) +
                           PMethod^.Name[0] + 1;
          {$ENDIF}

          MethodTypeDataContainer.Address := PMethod^.Address;
{$ENDIF}
        end;
        tkInterface:
        begin
          RI.Processed := true;

          ptd := ShiftPointer(pti, RI.PosTypeData);

          InterfaceTypeDataContainer := RI.TypeDataContainer as
                                    TInterfaceTypeDataContainer;

          Record_Parent := LookupFullName(InterfaceTypeDataContainer.FullParentName);
          if Record_Parent = nil then
            ptd^.IntfParent := nil
          else
{$IFDEF FPC}
            ptd^.IntfParent := Record_Parent.buff4;
{$ELSE}
            ptd^.IntfParent := @ Record_Parent.buff4;
{$ENDIF}

          Z := RI.TypeDataContainer.TypeDataSize +
               SizeOf(TPropData);

          PropDataContainer := InterfaceTypeDataContainer.PropDataContainer;

          for J := 0 to PropDataContainer.Count - 1 do
          begin
            ZZ := 0;
            if J > 0 then
              for JJ := 0 to J - 1 do
                Inc(ZZ, PropInfoSize(PropDataContainer.PropList[JJ]));

            ppi := ShiftPointer(ptd, Z + ZZ);

            ppi^.NameIndex := J;

            Record_Temp := LookupFullName(PropDataContainer.PropTypeNames[J]);
            if Record_Temp = nil then
            begin
              ppi^.PropType := nil;
            end
            else
              {$IFDEF FPC}
              ppi^.PropType := Record_Temp.Buff4;
              {$ELSE}
              ppi^.PropType := PPTypeInfo(@Record_Temp.Buff4);
              {$ENDIF}
          end;
        end;
        tkClass:
        begin
          R := ClassFactory.FindRecordByFullName(String(RI.FullName));

          if R = nil then
            RaiseError(errInternalError, []);

          ClassTypeInfoContainer := RI as
                                    TClassTypeInfoContainer;
          ClassTypeDataContainer := RI.TypeDataContainer as
                                    TClassTypeDataContainer;

//        R.VMTPtr^.DynamicTable := Prog.MessageList.CreateDmtTable(ExtractName(RI.FullName),
//          R.DmtTableSize);
          ClassRec := Prog.ClassList.Lookup(String(RI.FullName));
          if ClassRec <> nil then
          begin
            if ClassRec.IntfList.Count > 0 then
            begin
              R.IntfTableSize := ClassRec.GetIntfTableSize;
              vmtIntfTableSlot(R.VMTPtr)^ := AllocMem(R.IntfTableSize);
              with PInterfaceTable(vmtIntfTableSlot(R.VMTPtr)^)^ do
              begin
                EntryCount := ClassRec.IntfList.Count;
                for J := 0 to EntryCount - 1 do
                begin
{$IFDEF FPC}
                  Entries[J].IID := @ ClassRec.IntfList[J].GUID;
{$ELSE}
                  Entries[J].IID := ClassRec.IntfList[J].GUID;
{$ENDIF}
                  Entries[J].VTable := ClassRec.IntfList[J].Buff;
                  Entries[J].IOffset := ClassRec.GetIntfOffset(ClassRec.IntfList[J].GUID);
                end;
              end;
            end;
          end;

          if ClassTypeDataContainer.FieldTableCount > 0 then
          begin
            if R.FieldClassTable = nil then
              R.FieldClassTable :=
                CreateFieldClassTable(ClassTypeDataContainer.FieldTableCount);

            if R.FieldTableSize = 0 then
            begin
              R.FieldTableSize := ClassTypeDataContainer.FieldTableSize;
              vmtFieldTableSlot(R.VMTPtr)^ := AllocMem(R.FieldTableSize);
            end;

            FieldListContainer :=
              ClassTypeDataContainer.FieldListContainer;

            PVmtFieldTable(vmtFieldTableSlot(R.VMTPtr)^)^.Count :=
               FieldListContainer.Count;

            {$IFDEF ARC}
              P := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                SizeOf(Word));
              Pointer(P^) := R.FieldClassTable;
              PField := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                SizeOf(Word) + SizeOf(Pointer));
            {$ELSE}

              {$IFDEF PAX64}
                P := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                  SizeOf(Word));
                Pointer(P^) := R.FieldClassTable;
                PField := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                  SizeOf(Word) + SizeOf(Pointer));
              {$ELSE}
                {$ifdef WIN32}
                P := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                  SizeOf(Word));
                Pointer(P^) := R.FieldClassTable;
                PField := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                  SizeOf(Word) + SizeOf(Pointer));
                {$else}
                PField := ShiftPointer(vmtFieldTableSlot(R.VMTPtr)^,
                  SizeOf(Word) + SizeOf(Word));
                {$endif}
              {$ENDIF}

            {$ENDIF}

            for J := 0 to FieldListContainer.Count - 1 do
            begin
              // set up PField

              PField^.Name := FieldListContainer[J].Name;
              PField^.Offset := FieldListContainer[J].Offset;
              PField^.ClassIndex := J;

              ClassRec := Prog.ClassList.Lookup(FieldListContainer[J].FullFieldTypeName);
              if ClassRec <> nil then
              begin
                if ClassRec.Host then
{$IFDEF FPC}
                  R.FieldClassTable^.Classes[J] := ClassRec.PClass;
{$ELSE}
                  R.FieldClassTable^.Classes[J] := @ ClassRec.PClass;
{$ENDIF}
              end;

              PField := ShiftPointer(PField, GetFieldSize(PField));
            end;
          end;

          vmtTypeInfoSlot(R.VMTPtr)^ := pti;
          {$IFDEF FPC}
          C := TClass(R.VMTPtr);
          {$ELSE}
          C := vmtSelfPtrSlot(R.VMTPtr)^;
          {$ENDIF}
          ptd := ShiftPointer(pti, RI.PosTypeData);
          ptd^.ClassType := C;

          pti_parent := nil;

          C := C.ClassParent;
          if IsPaxClass(C) then
          begin
            Record_Parent := LookupFullName(ClassTypeDataContainer.FullParentName);
            if Record_Parent <> nil then
              if not Record_Parent.Processed then
                continue;

            if Record_Parent = nil then
            begin
              ClassRec := Prog.ClassList.Lookup(ClassTypeDataContainer.FullParentName);
              if ClassRec = nil then
                RaiseError(errInternalError, []);
              pti_parent := C.ClassInfo;
            end
            else
              pti_parent := Record_Parent.buff4;
          end
          else
          begin
            if Assigned(C) then
              pti_parent := C.ClassInfo;
          end;

          RI.Processed := true;

          R.pti_parent := pti_parent;
{$IFDEF FPC}
          ptd^.ParentInfo := R.pti_parent;
{$ELSE}
          ptd^.ParentInfo := @ R.pti_parent;
{$ENDIF}
          ptd^.PropCount := RI.TypeDataContainer.TypeData.PropCount;
          ptd^.UnitName := RI.TypeDataContainer.TypeData.UnitName;

          Z := RI.TypeDataContainer.TypeDataSize +
               SizeOf(TPropData);

          PropDataContainer :=
            TClassTypeDataContainer(RI.TypeDataContainer).PropDataContainer;

          if pti_parent <> nil then
            ParentPropCount := GetTypeData(pti_parent)^.PropCount
          else
            ParentPropCount := 0;

          Inc(ptd^.PropCount, ParentPropCount);

          for J := 0 to PropDataContainer.Count - 1 do
          begin
            ZZ := 0;
            if J > 0 then
              for JJ := 0 to J - 1 do
                Inc(ZZ, PropInfoSize(PropDataContainer.PropList[JJ]));

            ppi := ShiftPointer(ptd, Z + ZZ);

            ppi^.NameIndex := J + ParentPropCount;

            Record_Temp := LookupFullName(PropDataContainer.PropTypeNames[J]);
            if Record_Temp = nil then
            begin
//            ClassRec := Prog.ClassList.Lookup(String(ClassTypeDataContainer.FullParentName));
              ClassRec := Prog.ClassList.Lookup(PropDataContainer.PropTypeNames[J]);
              if ClassRec = nil then
                RaiseError(errInternalError, []);
              ClassRec.PClass_pti := ClassRec.PClass.ClassInfo;
{$IFDEF FPC}
              ppi^.PropType := ClassRec.PClass_pti;
{$ELSE}
              ppi^.PropType := @ ClassRec.PClass_pti;
{$ENDIF}
            end
            else
             {$IFDEF FPC}
              ppi^.PropType := Record_Temp.Buff4;
              {$ELSE}
              ppi^.PropType := PPTypeInfo(@Record_Temp.Buff4);
              {$ENDIF}

            FullName := PropDataContainer.ReadNames[J];
            if Length(FullName) > 0 then
            begin
              DestProg := Prog;
              ppi^.GetProc := Prog.GetAddress(FullName, MR);
              if ppi^.GetProc = nil then
              begin
                FileName := ExtractOwner(FullName) + '.' + PCU_FILE_EXT;
                ProcName := Copy(FullName, PosCh('.', FullName) + 1, Length(FullName));
                ppi^.GetProc := Prog.LoadAddressEx(FileName, ProcName, false, 0, SomeMR, DestProg);
              end;
              TBaseRunner(DestProg).WrapMethodAddress(ppi^.GetProc);
            end;
            FullName := PropDataContainer.WriteNames[J];
            if Length(FullName) > 0 then
            begin
              DestProg := Prog;
              ppi^.SetProc := Prog.GetAddress(FullName, MR);
              if ppi^.SetProc = nil then
              begin
                FileName := ExtractOwner(FullName) + '.' + PCU_FILE_EXT;
                ProcName := Copy(FullName, PosCh('.', FullName) + 1, Length(FullName));
                ppi^.SetProc := Prog.LoadAddressEx(FileName, ProcName, false, 0, SomeMR, DestProg);
              end;
              TBaseRunner(DestProg).WrapMethodAddress(ppi^.SetProc);
            end;

             ppi^.Index := Integer($80000000);  // no index
          end;
        end; // tkClass
        else
          begin
            RI.Processed := true;
          end;
      end; // case
    end; // i-loop

  until Processed;
end;

function TPaxTypeInfoList.Processed: Boolean;
var
  I: Integer;
begin
  result := true;
  for I := 0 to Count - 1 do
    if not Records[I].Processed then
    begin
      result := false;
      Exit;
    end;
end;

function TPaxTypeInfoList.FindMethodFullName(Address: Pointer): String;
var
  I: Integer;
  MethodTypeDataContainer: TMethodTypeDataContainer;
begin
  result := '';

  for I := 0 to Count - 1 do
    if Records[I].TypeInfo.Kind = tkMethod then
    begin
      MethodTypeDataContainer := Records[I].TypeDataContainer as
        TMethodTypeDataContainer;
      if MethodTypeDataContainer.Address = Address then
      begin
        result := String(Records[I].FullName);
        Exit;
      end;
    end;
end;

procedure TPaxTypeInfoList.RaiseError(const Message: string; params: array of Const);
begin
  raise Exception.Create(Format(Message, params));
end;

function GetClassTypeInfoContainer(X: TObject): TClassTypeInfoContainer;
var
  pti: PTypeInfo;
  P: Pointer;
  sz, StreamSize: Integer;
  M: TMemoryStream;
begin
  result := nil;
  pti := X.ClassInfo;
  if pti = nil then
    Exit;
  if not IsPaxObject(X) then
    Exit;
  P := ShiftPointer(pti, - SizeOf(Integer));
  sz := Integer(p^);
  P := ShiftPointer(pti, sz);
  StreamSize := Integer(P^);
  P := ShiftPointer(P, SizeOf(Integer)); // p points to stream
  M := TMemoryStream.Create;
  try
    M.Write(P^, StreamSize);
    M.Position := 0;
    result := TClassTypeInfoContainer.Create(X.ClassName);
    result.LoadFromStream(M, typeCLASS);
  finally
    FreeAndNil(M);
  end;
end;

function GetTypeInfoContainer(pti: PTypeInfo): TTypeInfoContainer;
var
  P: Pointer;
  sz, StreamSize: Integer;
  M: TMemoryStream;
begin
  result := nil;
  P := ShiftPointer(pti, - SizeOf(Integer));
  sz := Integer(p^);
  P := ShiftPointer(pti, sz);
  StreamSize := Integer(P^);
  P := ShiftPointer(P, SizeOf(Integer)); // p points to stream
  M := TMemoryStream.Create;
  try
    M.Write(P^, StreamSize);
    M.Position := 0;
//    result := TTypeInfoContainer.Create;
//    result.LoadFromStream(M, typeCLASS);
  finally
    FreeAndNil(M);
  end;
end;


end.
