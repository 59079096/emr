////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PAXCOMP_BASESYMBOL_TABLE.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
{$O-}
{$R-}
unit PAXCOMP_BASESYMBOL_TABLE;
interface
uses {$I uses.def}
  TypInfo,
  SysUtils,
  Classes,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_VAROBJECT,
  PAXCOMP_HEADER_PARSER,
  PAXCOMP_MAP,
  PAXCOMP_CLASSFACT,
  PAXCOMP_SYMBOL_REC;
type
  TSaveStateRec = class
  public
    Id: Integer;
    LastShiftValue: Integer;
    LastClassIndex: Integer;
    LastSubId: Integer;
    LastVarId: Integer;
  end;

  TSaveStateList = class(TTypedList)
  private
    function GetRecord(I: Integer): TSaveStateRec;
  public
    function Add: TSaveStateRec;
    function Find(Id: Integer): TSaveStateRec;
    property Records[I: Integer]: TSaveStateRec read GetRecord;
  end;

  TBaseSymbolTable = class
  private
    LastCard: Integer;
    SaveStateList: TSaveStateList;
    function GetSizeOfPointer: Integer;
    function GetSizeOfTMethod: Integer;
    procedure CheckMemory(p: Pointer; Size: Cardinal);
    procedure UpdateSomeTypeList(const TypeName: String; TypeId: Integer);
    procedure CheckError(B: Boolean);
    procedure BindDummyType(TypeId, OriginTypeId: Integer);
  protected
    function GetRecord(I: Integer): TSymbolRec; virtual;
  public
    st_tag: Integer;
{$IFDEF ARC}
    A: TList<TSymbolRec>;
{$ELSE}
    A: TList;
{$ENDIF}
    Card: Integer;

    ResultId: Integer;
    TrueId: Integer;
    FalseId: Integer;
    NilId: Integer;
    EventNilId: Integer;
    EmptySetId: Integer;
    EmptyStringId: Integer;
    CurrExceptionObjectId: Integer;

    VarObjects: TVarObjectList;
    HashArray: THashArray;
    HeaderParser: THeaderParser;
    GuidList: TGuidList;
    SomeTypeList: TSomeTypeList;
    ExternList: TExternList;

    LastShiftValue: Integer;
    LastClassIndex: Integer;
    LastSubId: Integer;
    LastVarId: Integer;

    SR0: TSymbolRec;

    TypeHelpers: TAssocIntegers;

    constructor Create(NeedHash: Boolean = True);
    destructor Destroy; override;
    function AddRecord: TSymbolRec; virtual;
    procedure RemoveLastRecord;
    procedure Reset; virtual;
    procedure Discard(OldCard: Integer);
    procedure SetPAX64(value: Boolean);
    function LookupAnonymousInterface(ClassId: Integer): Integer;
    function LookupAnonymousMethod(IntfId: Integer): Integer;
{$IFDEF PAXARM}
    function AddPWideCharConst(const Value: String): TSymbolRec;
{$ELSE}
    function AddPWideCharConst(const Value: WideString): TSymbolRec;
    function AddAnsiCharConst(Value: AnsiChar): TSymbolRec;
    function AddPAnsiCharConst(const Value: AnsiString): TSymbolRec;
    function AddShortStringConst(const Value: String): TSymbolRec;
{$ENDIF}
    function AddWideCharConst(Value: Integer): TSymbolRec;
    function AddByteConst(Value: Byte): TSymbolRec;
    function AddWordConst(Value: Word): TSymbolRec;
    function AddIntegerConst(Value: Integer): TSymbolRec;
    function AddInt64Const(Value: Int64): TSymbolRec;
    function AddUInt64Const(Value: UInt64): TSymbolRec;
    function AddCardinalConst(Value: Cardinal): TSymbolRec;
    function AddSmallIntConst(Value: SmallInt): TSymbolRec;
    function AddShortIntConst(Value: ShortInt): TSymbolRec;
    function AddEnumConst(TypeId, Value: Integer): TSymbolRec;
    function AddPointerConst(TypeId: Integer; Value: Pointer): TSymbolRec;
    function AddRecordConst(TypeId: Integer; const Value: Variant): TSymbolRec;
    function AddArrayConst(TypeId: Integer; const Value: Variant): TSymbolRec;
    function AddSetConst(TypeId: Integer; const Value: Variant): TSymbolRec;
    function AddClassConst(TypeId: Integer; Value: TObject): TSymbolRec;
    function AddClassRefConst(TypeId: Integer; Value: TClass): TSymbolRec;
    function AddSetVar(TypeId: Integer; const Value: Variant): TSymbolRec;
    function AddDoubleConst(Value: Double): TSymbolRec;
    function AddCurrencyConst(Value: Double): TSymbolRec;
    function AddSingleConst(Value: Single): TSymbolRec;
    function AddExtendedConst(Value: Extended): TSymbolRec;
    function AddBooleanConst(Value: Boolean): TSymbolRec;
    function AddByteBoolConst(Value: ByteBool): TSymbolRec;
    function AddWordBoolConst(Value: WordBool): TSymbolRec;
    function AddLongBoolConst(Value: LongBool): TSymbolRec;
    function AddVariantConst(const Value: Variant): TSymbolRec;
    function AddOleVariantConst(const Value: OleVariant): TSymbolRec;

    function AddTMethodVar(Level: Integer): TSymbolRec;
    function AddCurrencyVar(Level: Integer): TSymbolRec;
    function AddDoubleVar(Level: Integer): TSymbolRec;
    function AddSingleVar(Level: Integer): TSymbolRec;
    function AddExtendedVar(Level: Integer): TSymbolRec;
    function AddInt64Var(Level: Integer): TSymbolRec;
    function AddUInt64Var(Level: Integer): TSymbolRec;
{$IFNDEF PAXARM}
    function AddStringVar(Level: Integer): TSymbolRec;
    function AddWideStringVar(Level: Integer): TSymbolRec;
    function AddShortStringVar(Level, TypeId: Integer): TSymbolRec;
    function AddAnsiCharVar(Level: Integer): TSymbolRec;
{$ENDIF}
    function AddInterfaceVar(Level: Integer): TSymbolRec;
    function AddClassVar(Level: Integer): TSymbolRec;
    function AddUnicStringVar(Level: Integer): TSymbolRec;
    function AddVariantVar(Level: Integer): TSymbolRec;
    function AddOleVariantVar(Level: Integer): TSymbolRec;
    function AddDynarrayVar(Level, TypeId: Integer): TSymbolRec;
    function AddRecordVar(Level, TypeId: Integer): TSymbolRec;
    function AddBooleanVar(Level: Integer): TSymbolRec;
    function AddByteBoolVar(Level: Integer): TSymbolRec;
    function AddWordBoolVar(Level: Integer): TSymbolRec;
    function AddLongBoolVar(Level: Integer): TSymbolRec;
    function AddIntegerVar(Level: Integer): TSymbolRec;
    function AddCardinalVar(Level: Integer): TSymbolRec;
    function AddSmallIntVar(Level: Integer): TSymbolRec;
    function AddShortIntVar(Level: Integer): TSymbolRec;
    function AddByteVar(Level: Integer): TSymbolRec;
    function AddWordVar(Level: Integer): TSymbolRec;
    function AddPointerVar(Level: Integer): TSymbolRec;
    function AddWideCharVar(Level: Integer): TSymbolRec;
    function AddVoidVar(Level: Integer; SZ: Integer): TSymbolRec;
    function AddClassRefVar(Level: Integer): TSymbolRec;
    function AddLabel: TSymbolRec;
    function AddPointerType(SourceTypeId: Integer): TSymbolRec;

    function AddEndOfClassHeader(ClassId: Integer): TSymbolRec;

    function GetDataSize(UpperId: Integer = MaxInt - 1): Integer;

    function LookUpEnumItem(const S: String; EnumTypeId: Integer;
                            UpCase: Boolean): Integer;

    function LookupNamespace(const S: String;
                             i_Level: Integer; UpCase: Boolean): Integer;
    function LookupFullName(const S: String; UpCase: Boolean): Integer;
    function LookupFullNameEx(const S: String; UpCase: Boolean;
                              OverCount: Integer): Integer;
    function LookUpType(const S: String; i_Level: Integer; UpCase: Boolean): Integer; overload;
    function LookUpType(const S: String; UpCase: Boolean): Integer; overload;
    function LookupParentMethodBase(SubId: Integer;
                                    UpCase: Boolean;
                                    var BestId: Integer): Integer;
    function LookupParentMethod(SubId: Integer;
                      UpCase: Boolean; HasMethodIndex: Boolean = false): Integer;
    function LookupParentMethods(SubId: Integer; Upcase: Boolean): TIntegerList;

    function LookupParentConstructor(SubId: Integer): Integer;
    function LookupParentConstructors(SubId: Integer): TIntegerList;

    function LookUpTypeEx(const S: String;
             i_Level: Integer; UpCase: Boolean; LowBound: Integer): Integer;
    function LookUp(const S: String; Level: Integer; UpCase: Boolean;
        UpperBoundId: Integer = MaxInt; recursive: Boolean = true): Integer;
    function LookUpEx(var HelperTypeId: Integer; const S: String; Level: Integer; UpCase: Boolean;
        UpperBoundId: Integer = MaxInt; recursive: Boolean = true): Integer;
    function LookUps(const S: String; LevelStack: TIntegerStack;
                     UpCase: Boolean;
                     UpperBoundId: Integer = MaxInt;
                     Recursive: Boolean = true): Integer;
    function LookUpsEx(const S: String; LevelStack: TIntegerStack; var LevelId: Integer; UpCase: Boolean): Integer;
    function LookUpsExcept(const S: String; LevelStack: TIntegerStack; LevelId: Integer; UpCase: Boolean): Integer;
    function LookUpAll(const S: String; Level: Integer; UpCase: Boolean): TIntegerList;
    function LookUpSub(const S: String; Level: Integer; UpCase: Boolean): TIntegerList;
    function LookUpSubs(const S: String; Level: Integer; UsingList: TIntegerList; UpCase: Boolean): TIntegerList;
    function LookupAnotherDeclaration(Id: Integer; UpCase: Boolean;
                                      var BestID: Integer): Integer;
    function LookupForwardDeclaration(Id: Integer; UpCase: Boolean;
                            var BestID: Integer): Integer;
    function LookupForwardDeclarations(Id: Integer;
                                       UpCase: Boolean): TIntegerList;
    function RegisterNamespace(LevelId: Integer;
                               const NamespaceName: String): Integer;
    function RegisterArrayType(LevelId: Integer;
                               const TypeName: String;
                               RangeTypeId, ElemTypeId: Integer;
                               Align: Integer): Integer;
    function RegisterDynamicArrayType(LevelId: Integer;
                                      const TypeName: String;
                                      ElemTypeId: Integer): Integer;
    function RegisterOpenArrayType(LevelId: Integer;
                                   const TypeName: String;
                                   ElemTypeId: Integer): Integer;
    function FindInterfaceTypeId(const GUID: TGUID): Integer;
    function RegisterInterfaceType(LevelId: Integer;
                                   const TypeName: String;
                                   const GUID: TGUID): Integer; overload;
    function RegisterInterfaceType(LevelId: Integer;
                                   pti: PTypeInfo): Integer; overload;
    procedure RegisterSupportedInterface(TypeId: Integer;
                                         const SupportedInterfaceName: String;
                                         const i_GUID: TGUID); overload;
    procedure RegisterSupportedInterface(TypeId,
                                         InterfaceTypeId: Integer); overload;

    function RegisterClassType(LevelId: Integer;
                               const TypeName: String; i_AncestorID: Integer): Integer; overload;
    function RegisterClassType(LevelId: Integer;
                               C: TClass;
                               Reserved: Integer = 0): Integer; overload;
    function RegisterClassTypeForImporter(LevelId: Integer;
                                          C: TClass): Integer; overload;
    function RegisterClassTypeForImporter(LevelId: Integer;
                                          const TypeName: String): Integer; overload;
    procedure RegisterClassTypeInfos(ClassId: Integer;
                                     C: TClass);
    function RegisterClassReferenceType(LevelId: Integer;
                               const TypeName: String;
                               OriginClassId: Integer): Integer;
    function RegisterHelperType(LevelId: Integer;
                                const TypeName: String;
                                OriginTypeId: Integer): Integer;
    function RegisterProperty(LevelId: Integer; const PropName: String;
                              PropTypeID, i_ReadId, i_WriteId: Integer;
                              i_IsDefault: Boolean): Integer;
    function RegisterInterfaceProperty(LevelId: Integer;
                                       const PropName: String;
                                       PropTypeID,
                                       ReadIndex,
                                       WriteIndex: Integer): Integer;
    function RegisterRecordType(LevelId: Integer;
                                const TypeName: String;
                                Align: Integer): Integer;
    function RegisterRTTIType(LevelId: Integer;
                              pti: PTypeInfo): Integer;
    function RegisterTypeAlias(LevelId:Integer;
                               const TypeName: String;
                               OriginTypeId: Integer;
                               const OriginTypeName: String = ''): Integer; overload;
    function RegisterTypeAlias(LevelId:Integer;
                               const TypeName, OriginTypeName: String): Integer; overload;
    function RegisterTypeAlias(LevelId:Integer;
                               const Declaration: String): Integer; overload;
    function RegisterTypeField(LevelId: Integer; const FieldName: String;
                               FieldTypeID: Integer;
                               FieldOffset: Integer = -1;
                               ACompIndex: Integer = -1): Integer;
    function RegisterTypeFieldEx(LevelId: Integer;
                               const Declaration: String;
                               FieldOffset: Integer = -1): Integer;
    function RegisterVariantRecordTypeField(LevelId: Integer;
                                const Declaration: String;
                                VarCnt: Int64): Integer; overload;
    function RegisterVariantRecordTypeField(LevelId: Integer;
                                const FieldName: String;
                                FieldTypeID: Integer;
                                VarCnt: Int64): Integer; overload;

    function RegisterSubrangeType(LevelId: Integer;
                                  const TypeName: String;
                                  TypeBaseId: Integer;
                                  B1, B2: Integer): Integer;

    function RegisterEnumType(LevelId: Integer;
                              const TypeName: String;
                              TypeBaseId: Integer): Integer;
    function RegisterEnumValue(EnumTypeId: Integer;
                               const FieldName: String;
                               const i_Value: Integer): Integer;
    function RegisterPointerType(LevelId: Integer;
                                 const TypeName: String;
                                 OriginTypeId: Integer;
                                 const OriginTypeName: String = ''): Integer;
{$IFNDEF PAXARM}
    function RegisterShortStringType(LevelId: Integer;
                                     const TypeName: String;
                                     L: Integer): Integer;
{$ENDIF}
    function RegisterSetType(LevelId: Integer;
                             const TypeName: String;
                             OriginTypeId: Integer): Integer;

    function CreateEmptySet: TSymbolRec;

    function RegisterProceduralType(LevelId: Integer;
                                    const TypeName: String;
                                    HSub: Integer): Integer;
    function RegisterMethodReferenceType(LevelId: Integer;
                                    const TypeName: String;
                                    HSub: Integer): Integer;
    function RegisterEventType(LevelId: Integer;
                               const TypeName: String;
                               HSub: Integer): Integer;
    function RegisterVariable(LevelId: Integer;
                              const Declaration: String; Address: Pointer): Integer; overload;
    function RegisterVariable(LevelId: Integer; const VarName: String;
                              VarTypeID: Integer; i_Address: Pointer): Integer; overload;
    function RegisterObject(LevelId: Integer;
                            const ObjectName: String;
                            TypeId: Integer;
                            i_Address: Pointer): Integer;
    function RegisterVirtualObject(LevelId: Integer;
                                   const ObjectName: String): Integer;
    function RegisterConstant(LevelId: Integer;
                              const Declaration: String): Integer; overload;
    function RegisterConstant(LevelId: Integer; const i_Name: String; i_TypeID: Integer;
                              const i_Value: Variant): Integer; overload;
    function RegisterConstant(LevelId: Integer; const i_Name: String;
                              const i_Value: Variant): Integer; overload;
    function RegisterPointerConstant(LevelId: Integer; const i_Name: String;
                              const i_Value: Pointer): Integer; overload;
    function RegisterExtendedConstant(LevelId: Integer; const i_Name: String;
                              const i_Value: Extended): Integer; overload;
    function RegisterInt64Constant(LevelId: Integer;
                               const i_Name: String; const i_Value: Int64): Integer;
    function RegisterRoutine(LevelId: Integer;
                             const SubName: String; ResultTypeID: Integer;
                             CallConvention: Integer;
                             i_Address: Pointer;
                             i_OverCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterRoutine(LevelId: Integer;
                             const SubName, ResultType: String;
                             CallConvention: Integer;
                             i_Address: Pointer;
                             i_OverCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterMethod(LevelId: Integer;
                            const SubName: String; ResultTypeID: Integer;
                            CallConvention: Integer;
                            i_Address: Pointer;
                            IsShared: Boolean = false;
                            i_CallMode: Integer = cmNONE;
                            i_MethodIndex: Integer = 0;
                            i_OverCount: Integer = 0;
                            i_IsAbstract: Boolean = false;
                            i_AbstractMethodCount: Integer = 0;
                            i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterMethod(LevelId: Integer;
                            const SubName, ResultType: String;
                            CallConvention: Integer;
                            i_Address: Pointer;
                            IsShared: Boolean = false;
                            i_CallMode: Integer = cmNONE;
                            i_MethodIndex: Integer = 0;
                            i_OverCount: Integer = 0;
                            i_IsAbstract: Boolean = false;
                            i_AbstractMethodCount: Integer = 0;
                            i_IsDeprecated: Boolean = false): Integer; overload;
    function RegisterConstructor(LevelId: Integer;
                                 const SubName: String;
                                 i_Address: Pointer;
                                 IsShared: Boolean = false;
                                 i_CallMode: Integer = cmNONE;
                                 i_MethodIndex: Integer = 0;
                                 i_OverCount: Integer = 0;
                                 i_IsAbstract: Boolean = false;
                                 i_AbstractMethodCount: Integer = 0;
                                 i_IsDeprecated: Boolean = false): Integer;
    function RegisterDestructor(LevelId: Integer;
                                const SubName: String; i_Address: Pointer;
                                i_CallMode: Integer = cmVIRTUAL): Integer;
    function RegisterParameter(HSub: Integer;
                               const ParameterName: String;
                               ParamTypeID: Integer;
                               ParamMod: Integer = 0;
                               Optional: Boolean = false;
                               const DefaultValue: String = ''): Integer; overload;
    function RegisterParameter(HSub: Integer;
                               const ParameterName: String;
                               const ParameterType: String;
                               ParamMod: Integer = 0;
                               Optional: Boolean = false;
                               const DefaultValue: String = ''): Integer; overload;
    function RegisterParameter(HSub: Integer; ParamTypeID: Integer;
                               const DefaultValue: Variant;
                               InitByRef: Boolean = false;
                               ParameterName: String = '';
                               Tag: Integer = 0): Integer; overload;
    procedure RegisterRunnerParameter(HSub: Integer);
    function RegisterHeader(LevelId: Integer;
                            const Header: String; Address: Pointer;
                            AMethodIndex: Integer = 0): Integer;
    function RegisterFakeHeader(LevelId: Integer;
              const Header: String; Address: Pointer): Integer;
    procedure RegisterMember(LevelId: Integer; const MemberName: String;
                             i_Address: Pointer);
    function RegisterTypeDeclaration(LevelId: Integer;
      const Declaration: String): Integer;
    function RegisterSpace(K: Integer): Integer;

    function RestorePositiveIndex(L: Integer): Integer;
    function FindMaxMethodIndex(IntfId: Integer): Integer;
    procedure SetAncestorEx(ClassId: Integer);

    function IsResultId(Id: Integer): Boolean;
    function GetResultId(SubId: Integer): Integer;
    function GetSelfId(SubId: Integer): Integer;
    function GetParamId(SubId, ParamNumber: Integer): Integer;
    function GetDL_Id(SubId: Integer): Integer;
    function GetRBP_Id(SubId: Integer): Integer;
    function GetRBX_Id(SubId: Integer): Integer;
    function GetRDI_Id(SubId: Integer): Integer;
    function GetSizeOfLocals(SubId: Integer): Integer;
    function GetSizeOfLocalsEx(SubId: Integer): Integer;
    function GetSubRSPSize(SubId: Integer): Integer;
    function GetSizeOfSetType(SetTypeId: Integer): Integer;
    function CheckSetTypes(T1, T2: Integer): Boolean;
    function GetLowBoundRec(TypeID: Integer): TSymbolRec;
    function GetHighBoundRec(TypeID: Integer): TSymbolRec;
    procedure GetArrayTypeInfo(ArrayTypeId: Integer; var RangeTypeId: Integer; var ElemTypeId: Integer);
{$IFNDEF PAXARM}
    function IsZeroBasedAnsiCharArray(Id: Integer): Boolean;
{$ENDIF}
    function IsZeroBasedWideCharArray(Id: Integer): Boolean;
    function GetTypeBase(TypeId: Integer): Integer;
    function GetPatternSubId(ProcTypeID: Integer): Integer;
    function EqualHeaders(SubId1, SubId2: Integer): Boolean;
    function GetShiftsOfDynamicFields(ATypeId: Integer): TIntegerList;
    function GetTypesOfDynamicFields(ATypeId: Integer): TIntegerList;
    function HasDynamicFields(ATypeId: Integer): Boolean;
    function TerminalTypeOf(TypeID: Integer): Integer;
    function FindDefaultPropertyId(i_TypeId: Integer): Integer;
    function FindConstructorId(i_TypeId: Integer): Integer;
    function FindConstructorIdEx(i_TypeId: Integer): Integer;
    function FindConstructorIds(i_TypeId: Integer): TIntegerList;

    function FindDestructorId(i_TypeId: Integer): Integer;
    function FindDestructorIdEx(i_TypeId: Integer): Integer;
    function Inherits(T1, T2: Integer): Boolean;
    function Supports(T1, T2: Integer): Boolean;

    function GetFinalAddress(P: Pointer; StackFrameNumber: Integer;
                             Id: Integer): Pointer;

    function GetStrVal(Address: Pointer;
                       TypeId: Integer;
                       TypeMapRec: TTypeMapRec = nil;
                       BriefCls: Boolean = false): String;

    function GetVariantVal(Address: Pointer;
                       TypeId: Integer;
                       TypeMapRec: TTypeMapRec = nil): Variant;

    function GetValueAsString(P: Pointer;
                              StackFrameNumber: Integer;
                              Id: Integer;
                              TypeMapRec: TTypeMapRec = nil;
                              BriefCls: Boolean = false): String; virtual;

    procedure CheckVariantData (const V);
    function GetVal(Address: Pointer;
                    TypeId: Integer): Variant;
    function GetValue(P: Pointer; StackFrameNumber: Integer;
                      Id: Integer): Variant;

    procedure PutVal(Address: Pointer;
                     TypeId: Integer; const Value: Variant);
    procedure PutValue(P: Pointer; StackFrameNumber: Integer;
                       Id: Integer; const Value: Variant);

    function GetLocalCount(SubId: Integer): Integer;
    function GetLocalId(SubId, LocalVarNumber: Integer): Integer;
    function IsLocalOf(Id, SubId: Integer): Boolean;

    function GetGlobalCount(NamespaceId: Integer): Integer;
    function GetGlobalId(NamespaceId, GlobalVarNumber: Integer): Integer;

    function GetFieldCount(Id: Integer; TypeMapRec: TTypeMapRec = nil): Integer;

    function GetFieldDescriptorId(Id,
                                  FieldNumber: Integer;
                                  TypeMapRec: TTypeMapRec = nil): Integer;

    function GetFieldDescriptorIdByName(Id: Integer; const FieldName: String): Integer;
    function GetFieldName(Id, FieldNumber: Integer): String;
    function GetFieldAddress(P: Pointer;
                             StackFrameNumber,
                             Id,
                             FieldNumber: Integer;
                             TypeMapRec: TTypeMapRec = nil
                             ): Pointer;
    function GetFieldValueAsString(P: Pointer;
                                   StackFrameNumber: Integer;
                                   Id: Integer;
                                   FieldNumber: Integer;
                                   TypeMapRec: TTypeMapRec = nil;
                                   BriefCls: Boolean = false): String;

    function GetPublishedPropCount(Id: Integer): Integer;
    function GetPublishedPropDescriptorId(Id, PropNumber: Integer): Integer;
    function GetPublishedPropName(Id, PropNumber: Integer): String;
    function GetPublishedPropValueAsString(P: Pointer; StackFrameNumber: Integer;
                    Id, PropNumber: Integer): String;

    function GetArrayItemAddress(P: Pointer; StackFrameNumber, Id,
                                 Index: Integer): Pointer;
    function GetArrayItemValueAsString(P: Pointer; StackFrameNumber: Integer;
                                  Id, Index: Integer): String;
    function GetDynArrayItemAddress(P: Pointer;
                                    StackFrameNumber: Integer;
                                    Id, Index: Integer): Pointer;
    function GetDynArrayItemValueAsString(P: Pointer; StackFrameNumber: Integer;
                                     Id, Index: Integer): String;

    function IsParam(SubId, Id: Integer): Boolean;
    function IsVar(LevelId, Id: Integer): Boolean;
    function IsConst(LevelId, Id: Integer): Boolean;
    function IsType(LevelId, Id: Integer): Boolean;
    function IsProcedure(LevelId, Id: Integer): Boolean;
    function IsFunction(LevelId, Id: Integer): Boolean;
    function IsConstructor(ClassId, Id: Integer): Boolean;
    function IsDestructor(ClassId, Id: Integer): Boolean;
    function IsTypeField(LevelId, Id: Integer): Boolean;
    function IsEnumMember(LevelId, Id: Integer): Boolean;
    function IsProperty(ClassId, Id: Integer): Boolean;
    function IsNamespace(LevelId, Id: Integer): Boolean;

    function HasAbstractAncestor(ClassId: Integer): Boolean;
{$IFNDEF PAXARM}
    function FindPAnsiCharConst(const S: String; LimitId: Integer): Integer;
{$ENDIF}
    function FindPWideCharConst(const S: String; LimitId: Integer): Integer;

    function RegisterDummyType(LevelId: Integer;
                               const TypeName: String): Integer;
    function RegisterSomeType(LevelId: Integer;
                              const TypeName: String): Integer;

    function GetAlignmentSize(TypeId, DefAlign: Integer): Integer;
    procedure SetVisibility(ClassId: integer;
                            const MemberName: String; value: Integer); overload;
    procedure SetVisibility(C: TClass; const MemberName: String; value: Integer); overload;
    function FindClassTypeId(Cls: TClass): Integer;
    function FindClassTypeIdByPti(Pti: PTypeInfo): Integer;

    procedure SaveNamespaceToStream(LevelId: Integer; S: TStream); overload;
    procedure SaveNamespaceToStream(const NamespaceName: String; S: TStream); overload;

    procedure SaveNamespaceToFile(LevelId: Integer; const FileName: String); overload;
    procedure SaveNamespaceToFile(const NamespaceName, FileName: String); overload;

    procedure LoadNamespaceFromStream(S: TStream);
    procedure LoadNamespaceFromFile(const FileName: String);

    procedure ResolveExternList(CheckProc: TCheckProc; Data: Pointer);
    procedure AddScriptFields(ClassId: Integer; FieldList: TMapFieldList);

    procedure ExtractNamespaces(const Level: Integer; L: TStrings);

    procedure ExtractMembers(const Id: Integer; L: TStrings;
                             Lang: TPaxLang = lngPascal;
                             SharedOnly: Boolean = false;
                             VisSet: TMemberVisibilitySet = [cvPublic, cvPublished]);

    procedure ExtractParameters(Id: Integer; L: TStrings;
                               Lang: TPaxLang = lngPascal;
                               SkipParameters: Integer = 0);

    procedure ExtractParametersEx(Id: Integer;
                                  L: TStrings;
                                  Upcase: Boolean;
                                  SkipParameters: Integer = 0);

    function ValueStr(I: Integer): String;
    procedure AddTypes(const TypeName: String; L: TStrings;
                       ErrorIndex: Integer; Upcase: Boolean);
    procedure AddUndeclaredIdent(const IdentName: String; L: TStrings;
                       ErrorIndex: Integer; Upcase: Boolean);

    procedure CreateInterfaceMethodList(IntfId: Integer;
                                        L: TIntegerList); overload;
    procedure CreateInterfaceMethodList(ClassId, IntfId: Integer;
                                        InterfaceMethodIds,
                                        ClassMethodIds: TIntegerList); overload;

    procedure ProcessClassFactory(AClassFactory: Pointer;
                                  AProg: Pointer);
    procedure HideClass(C: TClass);

    function ImportFromTable(st: TBaseSymbolTable;
                             const FullName: String;
                             UpCase: Boolean;
                             DoRaiseError: Boolean = true): Integer;

    procedure LoadGlobalSymbolTableFromStream(Stream: TStream);
    procedure LoadGlobalSymbolTableFromFile(const FileName: String);
    procedure SaveGlobalSymbolTableToStream(Stream: TStream);
    procedure SaveGlobalSymbolTableToFile(const FileName: String);
    procedure SaveState;
    procedure RestoreState(Id: Integer);
    function GetOpenArrayHighId(Id: Integer): Integer;
    function GetOuterThisId(TypeId: Integer): Integer;
    function GetTypeParameters(Id: Integer): TIntegerList;
    function ExtractEnumNames(EnumTypeId: Integer): TStringList;
    function GetTypeHelpers(TypeId: Integer): TIntegerList;

    procedure RaiseError(const Message: string; params: array of const);
    property Records[I: Integer]: TSymbolRec read GetRecord; default;
    property SizeOfPointer: Integer read GetSizeOfPointer;
    property SizeOfTMethod: Integer read GetSizeOfTMethod;
  end;
var
  RegisterDRTTIProperties: procedure (Level: Integer;
                                      c: TClass;
                                      s: TBaseSymbolTable) = nil;
  // Added callback to get namespace of type.
  GetNamespaceOfType: function (aSymbolTable: TBaseSymbolTable; aTypeInfo: PTypeInfo): Integer = nil;

  GlobalAlignment: Integer = 8;

  RaiseE: Boolean = true;
  REG_OK: Boolean = true;
  REG_ERROR: String;
  DllDefined: Boolean = false;

  H_TByteSet: Integer = -1;

  GlobalCurrExceptionObjectId: Integer = -1;

  JS_JavaScriptNamespace: Integer = 1;
  JS_GetPropertyId: Integer = 0;
  JS_PutPropertyId: Integer = 0;
  JS_GetArrPropertyId: Integer = 0;
  JS_PutArrPropertyId: Integer = 0;

  JS_GetPropertyAsObjectId: Integer = 0;

  JS_ObjectClassId: Integer = 0;
  JS_BooleanClassId: Integer = 0;
  JS_FunctionClassId: Integer = 0;
  JS_StringClassId: Integer = 0;
  JS_NumberClassId: Integer = 0;
  JS_DateClassId: Integer = 0;
  JS_ArrayClassId: Integer = 0;
  JS_MathClassId: Integer = 0;
  JS_RegExpClassId: Integer = 0;
  JS_ErrorClassId: Integer = 0;

  JS_FunctionCallId: Integer = 0;
  JS_TempNamespaceId: Integer = 0;

  JS_GetGenericPropertyId: Integer = 0;
  JS_PutGenericPropertyId: Integer = 0;

  JS_ToObjectId: Integer = 0;
  JS_GetNextPropId: Integer = 0;
  JS_TypeOfId: Integer = 0;
  JS_VoidId: Integer = 0;
  JS_FindFuncId: Integer = 0;
  JS_AssignProgId: Integer = 0;
  JS_ClearReferencesId: Integer = 0;
  JS_AlertId: Integer = 0;
  JS_Delete: Integer = 0;

type
  TGetOleProp =
     procedure (const D: Variant; PropName: PChar;
                var Result: Variant;
                ParamCount: Integer); stdcall;

  TPutOleProp =
    procedure (const D: Variant; PropName: PChar;
               const Value: Variant;
               ParamCount: Integer); stdcall;

var
  GetOlePropProc: TGetOleProp = nil;
  PutOlePropProc: TPutOleProp = nil;
function IsFrameworkTypeId(Id: Integer): Boolean;

implementation

uses
  PAXCOMP_BASERUNNER,
  PAXCOMP_LOCALSYMBOL_TABLE,
  PAXCOMP_GC,
  PAXCOMP_STDLIB;

{$IFNDEF UNIX}
{$IFNDEF PAXARM}
{$IFNDEF LINUX}
{$IFNDEF MACOS32}
function GetReadableSize(Address, Size: DWord): DWord;
const
  ReadAttributes = [PAGE_READONLY, PAGE_READWRITE, PAGE_WRITECOPY, PAGE_EXECUTE,
    PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE, PAGE_EXECUTE_WRITECOPY];
var
  MemInfo: TMemoryBasicInformation;
  Tmp: DWord;
begin
  Result := 0;
  if (VirtualQuery(Pointer(Address), MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo)) and
    (MemInfo.State = MEM_COMMIT) and (MemInfo.Protect in ReadAttributes) then
  begin
    Result := (MemInfo.RegionSize - (Address - DWord(MemInfo.BaseAddress)));
    if (Result < Size) then
    begin
      repeat
        Tmp := GetReadableSize((DWord(MemInfo.BaseAddress) + MemInfo.RegionSize), (Size - Result));
        if (Tmp > 0) then Inc(Result, Tmp)
        else Result := 0;
      until (Result >= Size) or (Tmp = 0);
    end;
  end;
end;

function IsValidBlockAddr(Address, Size: DWord): Boolean;
begin
  try
    Result := (GetReadableSize(Address, Size) >= Size);
  except
    Result := false;
  end;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

function TSaveStateList.GetRecord(I: Integer): TSaveStateRec;
begin
  result := TSaveStateRec(L[I]);
end;

function TSaveStateList.Add: TSaveStateRec;
begin
  result := TSaveStateRec.Create;
  L.Add(result);
end;

function TSaveStateList.Find(Id: Integer): TSaveStateRec;
var
  I: Integer;
begin
  result := nil;
  for I := 0 to L.Count - 1 do
    if Records[I].Id = Id then
    begin
      result := Records[I];
      Exit;
    end;
end;

procedure TBaseSymbolTable.CheckMemory(p: Pointer; Size: Cardinal);
begin
{$IFNDEF UNIX}
{$IFNDEF PAXARM}
{$IFNDEF LINUX}
{$IFNDEF MACOS32}
  if not IsValidBlockAddr(Cardinal(p), Size) then
    raise EAbort.Create(errMemoryNotInitialized);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

constructor TBaseSymbolTable.Create(NeedHash: Boolean = True);
begin
  inherited Create;
{$IFDEF ARC}
  A := TList<TSymbolRec>.Create;
{$ELSE}
  A := TList.Create;
{$ENDIF}
  HeaderParser := THeaderParser.Create;
  Card := -1;
  VarObjects := TVarObjectList.Create(Self);
  if NeedHash then
    HashArray := THashArray.Create
  else
    HashArray := nil;
  GuidList := TGuidList.Create;
  SomeTypeList := TSomeTypeList.Create;
  ExternList := TExternList.Create;
  SaveStateList := TSaveStateList.Create;

  TypeHelpers := TAssocIntegers.Create;

  LastCard := 0;
end;

destructor TBaseSymbolTable.Destroy;
var
  I: Integer;
begin
  FreeAndNil(VarObjects);
  for I := A.Count - 1 downto 0 do
{$IFDEF ARC}
    A[I] := nil;
{$ELSE}
    TSymbolRec(A[I]).Free;
{$ENDIF}
  FreeAndNil(A);
  FreeAndNil(HeaderParser);
  if HashArray <> nil then
    FreeAndNil(HashArray);
  FreeAndNil(GuidList);
  FreeAndNil(SomeTypeList);
  FreeAndNil(ExternList);
  FreeAndNil(SaveStateList);
  FreeAndNil(TypeHelpers);
  inherited;
end;

procedure TBaseSymbolTable.SaveState;
var
  R: TSaveStateRec;
begin
  R := SaveStateList.Add;
  R.Id := Card;
  R.LastShiftValue := LastShiftValue;
  R.LastClassIndex := LastClassIndex;
  R.LastSubId := LastSubId;
  R.LastVarId := LastVarId;
end;

procedure TBaseSymbolTable.RestoreState(Id: Integer);
var
  R: TSaveStateRec;
begin
  R := SaveStateList.Find(Id);
  if R <> nil then
  begin
    Card := Id;
    LastShiftValue := R.LastShiftValue;
    LastClassIndex := R.LastClassIndex;
    LastSubId := R.LastSubId;
    LastVarId := R.LastVarId;
  end;
end;

procedure TBaseSymbolTable.Reset;
var
  I: Integer;
  R: TSymbolRec;
begin
  VarObjects.Clear;
  ExternList.Clear;
  SomeTypeList.Clear;
  SaveStateList.Clear;
  TypeHelpers.Clear;

  for I:=0 to A.Count - 1 do
{$IFDEF ARC}
    A[I] := nil;
{$ELSE}
    Records[I].Free;
{$ENDIF}
  A.Clear;
  Card := -1;

  for I:=0 to Types.Count - 1 do
  begin
    R := AddRecord;
    R.Name := Types[I].Name;
    R.Kind := KindTYPE;
    R.Completed := true;
  end;

  SR0 := Records[0];

{$IFDEF PAXARM}
  while Card < typePVOID - 1 do
    AddRecord;
{$ELSE}
  while Card < typePANSICHAR - 1 do
    AddRecord;
{$ENDIF}

{$IFNDEF PAXARM}
  RegisterPointerType(0, 'PCHAR', typeANSICHAR);
{$ENDIF}
  RegisterPointerType(0, 'PVOID', typeVOID);
  RegisterPointerType(0, 'PWIDECHAR', typeWIDECHAR);

  with AddRecord do
  begin
    NilId := Id;
    Kind := KindCONST;
    TypeId := typePOINTER;
    Level := 0;
    Value := 0;
  end;

  with AddBooleanConst(false) do
  begin
    FalseId := Id;
    Name := 'false';
    Level := typeBOOLEAN;
  end;

  with AddBooleanConst(true) do
  begin
    TrueId := Id;
    Name := 'true';
    Level := typeBOOLEAN;
  end;

  with AddByteBoolConst(Low(ByteBool)) do
    Level := typeBYTEBOOL;
  with AddByteBoolConst(High(ByteBool)) do
    Level := typeBYTEBOOL;

  with AddWordBoolConst(Low(WordBool)) do
    Level := typeWORDBOOL;
  with AddWordBoolConst(High(WordBool)) do
    Level := typeWORDBOOL;

  with AddLongBoolConst(Low(LongBool)) do
    Level := typeLONGBOOL;
  with AddLongBoolConst(High(LongBool)) do
    Level := typeLONGBOOL;

{$IFNDEF PAXARM}
  with AddAnsiCharConst(Low(AnsiChar)) do
    Level := typeANSICHAR;
  with AddAnsiCharConst(High(AnsiChar)) do
    Level := typeANSICHAR;
{$ENDIF}

  with AddByteConst(Low(Byte)) do
    Level := typeBYTE;
  with AddByteConst(High(Byte)) do
    Level := typeBYTE;

  with AddWordConst(Low(Word)) do
    Level := typeWORD;
  with AddWordConst(High(Word)) do
    Level := typeWORD;

  with AddIntegerConst(Low(Integer)) do
    Level := typeINTEGER;
  with AddIntegerConst(High(Integer)) do
    Level := typeINTEGER;

  with AddInt64Const(Low(Int64)) do
    Level := typeINT64;
  with AddInt64Const(High(Int64)) do
    Level := typeINT64;

  with AddUInt64Const(Low(UInt64)) do
    Level := typeUINT64;
  with AddUInt64Const(High(Int64)) do
    Level := typeUINT64;

  with AddCardinalConst(Low(Cardinal)) do
    Level := typeCARDINAL;
  with AddCardinalConst(High(Cardinal)) do
    Level := typeCARDINAL;

  with AddSmallIntConst(Low(SmallInt)) do
    Level := typeSMALLINT;
  with AddSmallIntConst(High(SmallInt)) do
    Level := typeSMALLINT;

  with AddShortIntConst(Low(ShortInt)) do
    Level := typeSHORTINT;
  with AddShortIntConst(High(ShortInt)) do
    Level := typeSHORTINT;

  Records[typePOINTER].PatternId := typeVOID;

  R := AddRecord;
  R.Kind := KindVAR;
  ResultID := R.Id;

  R := AddRecord;
  R.Kind := KindNONE;
  R.TypeId := typePOINTER;
  R.Shift := H_SelfPtr;

  R := AddRecord;
  R.Kind := KindVAR;
  R.TypeId := typePOINTER;
  R.Shift := H_ExceptionPtr;
  R.Host := true;
  CurrExceptionObjectId := R.Id;
  GlobalCurrExceptionObjectId := CurrExceptionObjectId;

  R := AddRecord;
  R.Kind := KindNONE;
  R.TypeId := typeINTEGER;
  R.Shift := H_ByteCodePtr;

  R := AddRecord;
  R.Kind := KindNONE;
  R.TypeId := typeINTEGER;
  R.Shift := H_Flag;

  R := AddRecord;
  R.Kind := KindNONE;
  R.TypeId := typeINTEGER;
  R.Shift := H_SkipPop;

  LastShiftValue := H_SkipPop + SizeOf(Integer);

  R := CreateEmptySet;
  EmptySetId := R.Id;

  H_TByteSet := RegisterSetType(0, '$$', typeBYTE);

{$IFDEF PAXARM}
  EmptyStringId := AddPWideCharConst('').Id;
{$ELSE}
  EmptyStringId := AddPAnsiCharConst('').Id;
{$ENDIF}

  with AddRecord do
  begin
    EventNilId := Id;
    Kind := KindVAR;
    TypeId := typeEVENT;
    Level := 0;
    Value := 0;
    Shift := LastShiftValue;
    Inc(LastShiftValue, SizeOfTMethod);
  end;

  if LastShiftValue <> FirstShiftValue then
    RaiseError(errInternalError, []);

  LastClassIndex := -1;
end;

procedure TBaseSymbolTable.Discard(OldCard: Integer);
var
  I, Id: Integer;
  S: String;
begin
  while Card > OldCard do
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
end;

function TBaseSymbolTable.RegisterNamespace(LevelId: Integer;
                                        const NamespaceName: String): Integer;
var
  Q: TStringList;
  S: String;
  I: Integer;
begin
  result := LookupNamespace(NamespaceName, LevelId, true);

  if result > 0 then
  begin
    HeaderParser.NamespaceId := result;
    Exit;
  end;

  if PosCh('.', NamespaceName) > 0 then
  begin
    Q := ExtractNames(NamespaceName);
    try
      for I := 0 to Q.Count - 1 do
      begin
        S := Q[I];
        if StrEql(S, 'System') then
          result := 0
        else
          result := RegisterNamespace(result, S);
      end;
    finally
      FreeAndNil(Q);
    end;
    Exit;
  end
  else
    S := NamespaceName;

  LastCard := Card;

  with AddRecord do
  begin
    Name := S;
    Kind := KindNAMESPACE;
    Host := true;
    Level := LevelId;
    result := Id;
  end;
  HeaderParser.NamespaceId := result;
end;

function TBaseSymbolTable.RegisterTypeDeclaration(LevelId: Integer;
  const Declaration: String): Integer;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  result := HeaderParser.Register_TypeDeclaration;
end;

procedure TBaseSymbolTable.UpdateSomeTypeList(const TypeName: String; TypeId: Integer);
var
  I, Id: Integer;
  R: TSymbolRec;
begin
  I := SomeTypeList.IndexOf(TypeName);
  if I = -1 then
    Exit;

  Id := SomeTypeList[I].Id;
  SomeTypeList.RemoveAt(I);

  for I := Card downto StdCard do
  begin
    R := Records[I];
    if R.PatternId = Id then
       R.PatternId := TypeId;
  end;

  Records[Id].Kind := KindNONE;
  Records[Id].Name := '';
end;

function TBaseSymbolTable.RegisterSubrangeType(LevelId: Integer;
                                           const TypeName: String;
                                           TypeBaseId: Integer;
                                           B1, B2: Integer): Integer;
begin
  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  LastCard := Card;

  if not TypeBaseId in OrdinalTypes then
  begin
    result := 0;
    RaiseError(errInternalError, []);
    Exit;
  end;

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := TypeBaseId;
    Host := true;
    Shift := 0;
    Level := LevelId;
    result := Id;

    Completed := true;
  end;

  with AddRecord do
  begin
    Kind := KindCONST;
    TypeID := TypeBaseId;
    Host := true;
    Shift := 0;
    Level := result;
    Value := B1;
  end;

  with AddRecord do
  begin
    Kind := KindCONST;
    TypeID := TypeBaseId;
    Host := true;
    Shift := 0;
    Level := result;
    Value := B2;
  end;
end;

function TBaseSymbolTable.RegisterEnumType(LevelId: Integer;
                                       const TypeName: String;
                                       TypeBaseId: Integer): Integer;
begin
  LastCard := Card;

  if TypeName <> '' then
  begin
    result := LookUpType(TypeName, LevelId, true);
    if (result > 0) and (Records [Result].Level = LevelID) then
      Exit;
  end;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeENUM;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := TypeBaseId;
    result := Id;
  end;

  BindDummyType(result, TypeBaseId);
end;

function TBaseSymbolTable.RegisterEnumValue(EnumTypeId: Integer;
                                        const FieldName: String;
                                        const i_Value: Integer): Integer;
begin
  with AddRecord do
  begin
    Name := FieldName;
    Kind := KindCONST;
    TypeID := EnumTypeId;
    Host := true;
    Shift := 0;
    Level := Records[EnumTypeId].Level;
    OwnerId := TypeId;

    Value := i_Value;

    result := Id;
  end;

  if EnumTypeId > 0 then
    Records[EnumTypeId].Count := Records[EnumTypeId].Count + 1;
end;

function TBaseSymbolTable.RegisterArrayType(LevelId: Integer;
                                        const TypeName: String;
                                        RangeTypeId, ElemTypeId: Integer;
                                        Align: Integer): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeARRAY;
    Host := true;
    Shift := 0;
    Level := LevelId;

    DefaultAlignment := Align;

    result := Id;

    Completed := true;
  end;

  with AddRecord do
  begin
    Kind := KindTYPE;
    TypeID := typeALIAS;
    Host := true;
    Shift := 0;
    Level := result;
    PatternId := RangeTypeId;
  end;

  with AddRecord do
  begin
    Kind := KindTYPE;
    TypeID := typeALIAS;
    Host := true;
    Shift := 0;
    Level := result;
    PatternId := ElemTypeId;
  end;
end;

function TBaseSymbolTable.RegisterTypeAlias(LevelId:Integer;
                                            const TypeName: String;
                                            OriginTypeId: Integer;
                                            const OriginTypeName: String = ''): Integer;
begin
  LastCard := Card;

  result := LookupType(TypeName, LevelId, true);
  if result > 0 then
  begin
    if Records[result].Level = LevelId then
      if result > OriginTypeId then
        Exit;
  end;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeALIAS;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := OriginTypeId;
    result := Id;
  end;

  if OriginTypeId = 0 then
  if OriginTypeName <> '' then
  begin
    ExternList.Add(Card,
                   OriginTypeName,
                   erPatternId);
  end;

  BindDummyType(result, OriginTypeId);
end;

function TBaseSymbolTable.RegisterTypeAlias(LevelId:Integer;
                                            const TypeName, OriginTypeName: String): Integer;
var
  TypeId: Integer;
begin
  TypeId := LookupType(OriginTypeName, true);
  result := RegisterTypeAlias(LevelId, TypeName, TypeId, OriginTypeName);
end;

function TBaseSymbolTable.RegisterTypeAlias(LevelId:Integer;
                                            const Declaration: String): Integer;
var
  TypeName: String;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  TypeName := HeaderParser.Parse_Ident;
  result := HeaderParser.Register_TypeAlias(TypeName);
end;

function TBaseSymbolTable.RegisterPointerType(LevelId: Integer;
                                          const TypeName: String;
                                          OriginTypeId: Integer;
                                          const OriginTypeName: String = ''): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typePOINTER;
    Host := true;
    Shift := 0;
    Level := LevelId;

    PatternId := OriginTypeId;

    result := Id;

    Completed := true;
  end;

  if OriginTypeId = 0 then
  if OriginTypeName <> '' then
  begin
    ExternList.Add(Card,
                   OriginTypeName,
                   erPatternId);
  end;
end;

function TBaseSymbolTable.RegisterDynamicArrayType(LevelId: Integer;
                                                   const TypeName: String;
                                                   ElemTypeId: Integer): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeDYNARRAY;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := ElemTypeId;
    result := Id;

    Completed := true;
  end;

  BindDummyType(result, ElemTypeId);
end;

function TBaseSymbolTable.RegisterOpenArrayType(LevelId: Integer;
                                                const TypeName: String;
                                                ElemTypeId: Integer): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeOPENARRAY;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := ElemTypeId;
    result := Id;

    Completed := true;
  end;

  BindDummyType(result, ElemTypeId);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.RegisterShortStringType(LevelId: Integer;
                                              const TypeName: String;
                                              L: Integer): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeSHORTSTRING;
    Host := true;
    Shift := 0;
    Level := LevelId;
    Count := L;
    result := Id;

    Completed := true;
  end;
end;
{$ENDIF}

function TBaseSymbolTable.RegisterSetType(LevelId: Integer;
                                      const TypeName: String;
                                      OriginTypeId: Integer): Integer;
begin
  LastCard := Card;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeSET;
    Host := true;
    Shift := 0;
    Level := LevelId;

    PatternId := OriginTypeId;

    result := Id;

    Completed := true;
  end;

  BindDummyType(result, OriginTypeId);
end;

function TBaseSymbolTable.RegisterProceduralType(LevelId: Integer;
                                             const TypeName: String;
                                             HSub: Integer): Integer;
var
  I, SubId: Integer;
begin
  LastCard := Card;

  if HSub < 0 then
    SubId := -HSub
  else
  begin
    SubId := -1;
    for I:=Card downto 1 do
      if Records[I].Shift = HSub then
      begin
        SubId := I;
        Break;
      end;

    if SubId = -1 then
    begin
      result := 0;
      RaiseError(errInternalError, []);
      Exit;
    end;
  end;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typePROC;
    Host := true;
    Shift := 0;
    Level := LevelId;

    PatternId := SubId;

    result := Id;

    Completed := true;
  end;
end;

function TBaseSymbolTable.RegisterMethodReferenceType(LevelId: Integer;
                                                      const TypeName: String;
                                                      HSub: Integer): Integer;
var
  I, SubId: Integer;
begin
  LastCard := Card;

  SubId := -1;
  for I:=Card downto 1 do
    if Records[I].Shift = HSub then
    begin
      SubId := I;
      Break;
    end;

  if SubId = -1 then
  begin
    result := 0;
    RaiseError(errInternalError, []);
    Exit;
  end;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  result := RegisterInterfaceType(LevelId, TypeName, IUnknown);
  Records[SubId].Level := result;
  Records[SubId].Name := ANONYMOUS_METHOD_NAME;
  Records[SubId].MethodIndex := 4;
end;

function TBaseSymbolTable.RegisterEventType(LevelId: Integer;
                                            const TypeName: String;
                                            HSub: Integer): Integer;
var
  I, SubId: Integer;
begin
  LastCard := Card;

  SubId := -1;
  for I:=Card downto 1 do
    if Records[I].Shift = HSub then
    begin
      SubId := I;
      Break;
    end;

  if SubId = -1 then
  begin
    result := 0;
    RaiseError(errInternalError, []);
    Exit;
  end;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeEVENT;
    Host := true;
    Shift := 0;
    Level := LevelId;

    PatternId := SubId;

    result := Id;

    Completed := true;
  end;
end;

function TBaseSymbolTable.FindInterfaceTypeId(const GUID: TGUID): Integer;
var
  I: Integer;
begin
  I := GuidList.IndexOf(GUID);
  if I = -1 then
    result := 0
  else
    result := GuidList[I].Id;
end;

function TBaseSymbolTable.RegisterInterfaceType(LevelId: Integer;
                                                const TypeName: String;
                                                const GUID: TGUID): Integer;
var
  D: packed record
       D1, D2: Double;
     end;
begin
  LastCard := Card;

//  result := FindInterfaceTypeId(GUID);
  result := LookUpType(TypeName, LevelId, true);

  if result > 0 then
    Exit;

  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeINTERFACE;
    Host := true;
    Shift := 0;
    Level := LevelId;
    AncestorId := 0;
    result := Id;

    GuidList.Add(GUID, result, TypeName);
  end;

  Move(GUID, D, SizeOf(TGUID));
  AddDoubleConst(D.D1);
  AddDoubleConst(D.D2);

  if not GuidsAreEqual(IUnknown, GUID) then
{$IFDEF VARIANTS}
    if not GuidsAreEqual(IInterface, GUID) then
{$ENDIF}
      RegisterSupportedInterface(result, 'IUnknown', IUnknown);
end;

function TBaseSymbolTable.RegisterInterfaceType(LevelId: Integer;
                                                pti: PTypeInfo): Integer;
var
  TypeData: PTypeData;
  L: TPtrList;
  p: PTypeInfo;
  d: PTypeData;
  I: Integer;
begin
  LastCard := Card;

  result := 0;
  TypeData := GetTypeData(pti);
  if TypeData <> nil then
    if ifHasGuid in TypeData^.IntfFlags then
    begin
      L := TPtrList.Create;
      try
        p := nil;
{$IFDEF FPC}
        p := TypeData^.IntfParent;
{$ELSE}
        if TypeData^.IntfParent <> nil then
          p := TypeData^.IntfParent^;
{$ENDIF}
        while p <> nil do
        begin
          L.Insert(0, p);
          d := GetTypeData(p);
          if d <> nil then
          begin
{$IFDEF FPC}
            p := d^.IntfParent
{$ELSE}
            if d^.IntfParent <> nil then
              p := d^.IntfParent^
            else
              p := nil;
{$ENDIF}
          end
          else
            p := nil;
        end;
        for I := 0 to L.Count - 1 do
          RegisterInterfaceType(LevelId, L[I]);
      finally
        FreeAndNil(L);
      end;

      result := RegisterInterfaceType(LevelId, PTIName(pti), TypeData^.Guid);
    end;
end;

procedure TBaseSymbolTable.RegisterSupportedInterface(TypeId: Integer;
                             const SupportedInterfaceName: String;
                             const i_GUID: TGUID);
var
  InterfaceTypeId: Integer;
begin
  LastCard := Card;

  InterfaceTypeId := LookupType(SupportedInterfaceName, true);

  if InterfaceTypeId = 0 then
  begin
    ExternList.Add(TypeId, SupportedInterfaceName, erGUID);
    Exit;
  end;

  with Records[TypeId] do
  begin
    if SupportedInterfaces = nil then
      SupportedInterfaces := TGuidList.Create;
    SupportedInterfaces.Add(i_GUID, InterfaceTypeId, SupportedInterfaceName);
  end;
end;

procedure TBaseSymbolTable.RegisterSupportedInterface(TypeId,
                                                      InterfaceTypeId: Integer);
var
  SupportedInterfaceName: String;
  GUID: TGUID;
  D: record
       D1, D2: Double;
     end;
begin
  if Records[InterfaceTypeId].FinalTypeId <> typeINTERFACE then
    Exit;

  LastCard := Card;

  SupportedInterfaceName := Records[InterfaceTypeId].Name;
  D.D1 := Records[InterfaceTypeId + 1].Value;
  D.D2 := Records[InterfaceTypeId + 2].Value;
  Move(D, GUID, SizeOf(TGUID));

  with Records[TypeId] do
  begin
    if SupportedInterfaces = nil then
      SupportedInterfaces := TGuidList.Create;
    SupportedInterfaces.Add(GUID, InterfaceTypeId, SupportedInterfaceName);
  end;
end;

procedure TBaseSymbolTable.RegisterClassTypeInfos(ClassId: Integer;
                                                  C: TClass);
  procedure SetAncestor;
  var
    I: Integer;
    S: String;
    ParentClass: TClass;
  begin
    ParentClass := C.ClassParent;

    if DllDefined then
      S := ParentClass.ClassName
    else
      S := '';

    for I:=ClassId - 1 downto H_TObject do
      with Records[I] do
      if PClass <> nil then
      begin
        if DllDefined then
        begin
          if Kind = KindTYPE then if Name = S then
          begin
            Records[ClassId].AncestorId := I;
            Exit;
          end
        end
        else
        if PClass = ParentClass then
        begin
          Records[ClassId].AncestorId := I;
          Exit;
        end;
      end;

    Records[ClassId].AncestorId := H_TObject;
  end;

var
  UnresolvedPropIds: TIntegerList;
  UnresolvedTypes: TPtrList;

  procedure RegisterPublishedProperty(Index: Integer; ppi: PPropInfo);
  var
    TypeData: PTypeData;
    pti: PTypeInfo;
    T: Integer;
  begin
    if ppi = nil then
      Exit;

  {$ifdef fpc}
      pti := ppi^.PropType;
  {$else}
      pti := ppi.PropType^;
  {$endif}

    T := 0;

    case pti^.Kind of
      tkInteger: T := typeINTEGER;
      tkFloat:
      begin
        TypeData := GetTypeData(pti);
        case TypeData^.FloatType of
          ftSingle: T := typeSINGLE;
          ftDouble: T := typeDOUBLE;
          ftExtended: T := typeEXTENDED;
        end;
      end;
{$IFNDEF PAXARM}
      tkChar: T := typeANSICHAR;
      tkString: T := typeSHORTSTRING;
      tkLString: T := typeANSISTRING;
{$ENDIF}
{$IFDEF UNIC}
      tkUString: T := typeUNICSTRING;
{$ENDIF}
      tkVariant: T := typeVARIANT;
      else
        T := LookupType(PTIName(pti), 0, true);
    end;

    with AddRecord do
    begin
      Name := StringFromPShortString(@ppi^.Name);
      Kind := KindPROP;
      TypeID := T;
      Host := true;
      Shift := 0;
      Level := ClassId;
      IsPublished := true;
      PropIndex := Index;
    end;

    if T = 0 then
    begin
{
      if pti^.Kind = tkClass then
      begin
        ExternList.Add(Card, pti^.Name, erTypeId);
      end
      else   }
      begin
        UnresolvedPropIds.Add(Card);
        UnresolvedTypes.Add(pti);
      end;
    end;
  end;

  function RegisterPublishedProperties: Integer;
  var
    pti: PTypeInfo;
    ptd: PTypeData;
    I, nProps: Integer;
    pProps: PPropList;
    ppi: PPropInfo;
  begin
    result := 0;
    pti := C.ClassInfo;
    if pti = nil then Exit;
    ptd := GetTypeData(pti);
    nProps := ptd^.PropCount;
    if nProps > 0 then begin
      GetMem(pProps, SizeOf(PPropInfo) * nProps);
      GetPropInfos(pti, pProps);
    end
    else
      pProps := nil;
    for I:=0 to nProps - 1 do
    begin
  {$ifdef fpc}
      ppi := pProps^[I];
  {$else}
      ppi := pProps[I];
  {$endif}
      RegisterPublishedProperty(I, ppi);
    end;
    if pProps <> nil then
      FreeMem(pProps, SizeOf(PPropInfo) * nProps);
    result := nProps;
  end;

var
  I, K, T, LevelId: Integer;
  pti: PTypeInfo;
begin
  Records[ClassId].PClass := C;
  Records[ClassId + 1].Value := Integer(C); // classref var

  if C <> TObject then
    SetAncestor;

  UnresolvedPropIds := TIntegerList.Create;
  UnresolvedTypes := TPtrList.Create;

  try
    RegisterPublishedProperties;
    AddEndOfClassHeader(ClassId);

    for I:=0 to UnresolvedTypes.Count - 1 do
    begin
      pti := UnresolvedTypes[I];
      K := UnresolvedPropIds[I];

      // Call event to get namespace of type.
      if Assigned(GetNamespaceOfType) then
        LevelId := GetNamespaceOfType(Self, pti)
      else
        LevelId := 0;

      T := RegisterRTTIType(LevelId, pti);
      if T = 0 then
      begin
        Records[K].Name := '';
        Records[K].Kind := KindNONE;
      end
      else
        Records[K].TypeID := T;
    end;

  finally
    FreeAndNil(UnresolvedPropIds);
    FreeAndNil(UnresolvedTypes);
  end;
end;

function TBaseSymbolTable.RegisterClassTypeForImporter(LevelId: Integer;
                                                       C: TClass): Integer;
begin
  result := RegisterClassType(LevelId, C.ClassName, H_TObject);
  AddVoidVar(0, MaxPublishedProps * SizeOfPointer);
end;

function TBaseSymbolTable.RegisterClassTypeForImporter(LevelId: Integer;
                                                       const TypeName: String): Integer;
begin
  result := RegisterClassType(LevelId, TypeName, H_TObject);
  AddVoidVar(0, MaxPublishedProps * SizeOfPointer);
end;

function TBaseSymbolTable.RegisterClassType(LevelId: Integer;
                                            const TypeName: String;
                                            i_AncestorId: Integer): Integer;
var
  ClassRefTypeId: Integer;
begin
  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  LastCard := Card;

  Inc(LastClassIndex);
  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeCLASS;
    Host := true;
    Shift := 0;
    Level := LevelId;
    ClassIndex := LastClassIndex;
    AncestorId := i_AncestorId;

    result := Id;
  end;

  AddClassRefVar(0);
  ClassRefTypeId := RegisterClassReferenceType(0, '', result);
  Records[result + 1].TypeId := ClassRefTypeId;
end;

procedure TBaseSymbolTable.SetAncestorEx(ClassId: Integer);
var
  I: Integer;
  S: String;
  ParentClass: TClass;
  C: TClass;
begin
  C := Records[ClassId].PClass;
  if C = nil then
    Exit;
{$IFDEF FPC}
//  ParentClass := PVMT(C).Parent;
ParentClass := C.ClassParent;
{$ELSE}
  ParentClass := C.ClassParent;
{$ENDIF}
  S := ParentClass.ClassName;
  I := LookupType(S, true);
  if I > 0 then
    Records[ClassId].AncestorId := I;
end;

function TBaseSymbolTable.RegisterClassType(LevelId: Integer;
                                            C: TClass;
                                            Reserved: Integer = 0): Integer;

var
  UnresolvedPropIds: TIntegerList;
  UnresolvedTypes: TPtrList;

  procedure RegisterPublishedProperty(Index: Integer; ppi: PPropInfo);
  var
    TypeData: PTypeData;
    pti: PTypeInfo;
    T: Integer;
  begin
    if ppi = nil then
      Exit;

  {$ifdef fpc}
      pti := ppi^.PropType;
  {$else}
      pti := ppi.PropType^;
  {$endif}

    T := 0;

    case pti^.Kind of
      tkInteger: T := typeINTEGER;
      tkFloat:
      begin
        TypeData := GetTypeData(pti);
        case TypeData^.FloatType of
          ftSingle: T := typeSINGLE;
          ftDouble: T := typeDOUBLE;
          ftExtended: T := typeEXTENDED;
        end;
      end;
{$IFDEF UNIC}
      tkUString: T := typeUNICSTRING;
{$ENDIF}
{$IFNDEF PAXARM}
      tkChar: T := typeANSICHAR;
      tkString: T := typeSHORTSTRING;
      tkLString: T := typeANSISTRING;
{$ENDIF}
      tkVariant: T := typeVARIANT;
      else
        T := LookupType(PTIName(pti), 0, true);
    end;

    with AddRecord do
    begin
      Name := StringFromPShortString(@ppi^.Name);
      Kind := KindPROP;
      TypeID := T;
      Host := true;
      Shift := 0;
      Level := result;
      IsPublished := true;
      PropIndex := Index;
    end;

    if T = 0 then
    begin
{
      if pti^.Kind = tkClass then
      begin
        ExternList.Add(Card, pti^.Name, erTypeId);
      end
      else       }
      begin
        UnresolvedPropIds.Add(Card);
        UnresolvedTypes.Add(pti);
      end;
    end;
  end;

  function RegisterPublishedProperties: Integer;
  var
    pti: PTypeInfo;
    ptd: PTypeData;
    I, nProps: Integer;
    pProps: PPropList;
    ppi: PPropInfo;
  begin
    result := 0;
    pti := C.ClassInfo;
    if pti = nil then Exit;
    ptd := GetTypeData(pti);
    nProps := ptd^.PropCount;
    if nProps > 0 then begin
      GetMem(pProps, SizeOf(PPropInfo) * nProps);
      GetPropInfos(pti, pProps);
    end
    else
      pProps := nil;
    for I:=0 to nProps - 1 do
    begin
  {$ifdef fpc}
      ppi := pProps^[I];
  {$else}
      ppi := pProps[I];
  {$endif}
      RegisterPublishedProperty(I, ppi);
    end;
    if pProps <> nil then
      FreeMem(pProps, SizeOf(PPropInfo) * nProps);
    result := nProps;
  end;

  procedure SetAncestor;
  var
    I: Integer;
    S: String;
    ParentClass: TClass;
  begin
    ParentClass := C.ClassParent;

    if DllDefined then
      S := ParentClass.ClassName
    else
      S := '';

    for I:=result - 1 downto H_TObject do
      with Records[I] do
      if PClass <> nil then
      begin
        if DllDefined then
        begin
          if Kind = KindTYPE then if Name = S then
          begin
            Records[result].AncestorId := I;
            Exit;
          end
        end
        else if PClass = ParentClass then
        begin
          Records[result].AncestorId := I;
          Exit;
        end;
      end;
{
    if AncestorRequired then
      ExternList.Add(result, ParentClass.ClassName, erAncestorId)
    else
}
    Records[result].AncestorId := H_TObject;
  end;

  procedure RegisterAncestors(Cls: TClass);
  var
    ParentClass: TClass;
    Id: Integer;
  begin
    ParentClass := Cls.ClassParent;
    if ParentClass = nil then
      Exit;
    Id := FindClassTypeId(ParentClass);
    if Id > 0 then
      Exit;
{
    if (LevelId > 0) and (ParentClass.ClassInfo <> nil) then
    begin
      ptd := GetTypeData(ParentClass.ClassInfo);
      if ptd = nil then
        RegisterClassType(LevelId, ParentClass)
      else if StrEql(ptd^.UnitName, Records[LevelId].Name) then
        RegisterClassType(LevelId, ParentClass)
      else
      begin
        Exit;
      end;
    end
    else           }
      RegisterClassType(LevelId, ParentClass);
    RegisterAncestors(ParentClass);
  end;

var
  I, K, T: Integer;
  pti: PTypeInfo;
  S: String;
  R: TSymbolRec;
  AlreadyExists: Boolean;
  K1, K2, KK: Integer;
begin
  HeaderParser.AbstractMethodCount := 0;

  LastCard := Card;

  AlreadyExists := false;
  result := FindClassTypeId(C);
  T := result;

  if result > 0 then
  begin
    if result < FirstLocalId then
      if Card >= FirstLocalId then
      begin
        Records[result].Level := LevelId;
        Exit;
      end;

    AlreadyExists := true;
  end;

  if not AlreadyExists then
  begin
    if not C.InheritsFrom(TGC_Object) then
      RegisterAncestors(C);
  end;

  S := C.ClassName;

  result := RegisterClassType(LevelId, S, 0);

  if AlreadyExists then
  begin
    Records[T].Name := '@@';
    Records[T].PClass := nil;
    Records[T].ClassIndex := -1;
    Records[T + 1].Value := 0;

    for KK := 1 to 2 do
    begin
      if KK = 1 then
      begin
        K1 := 1;
        if Self.st_tag = 0 then
          K2 := Card
        else
          K2 := TLocalSymbolTable(Self).GlobalST.Card;
      end
      else
      begin
        K1 := FirstLocalId + 1;
        K2 := Card;
      end;

      for I := K1 to K2 do
      begin
        R := Records[I];

        if R.TypeID = T then
          R.TypeID := result;
        if R.PatternID = T then
          R.PatternID := result;
        if R.AncestorID = T then
          R.AncestorID := result;
      end;
    end;
  end;

  Records[result].PClass := C;
  if S <> TObject.ClassName then
    SetAncestor;

  Records[Result + 1].Value := Integer(C); // classref var

  UnresolvedPropIds := TIntegerList.Create;
  UnresolvedTypes := TPtrList.Create;

  try

    K := RegisterPublishedProperties;

    for I:=1 to Reserved do // reserve extra space
      AddRecord;

    for I:=1 to K do // reserve place for ppi
      AddPointerVar(0);

    for I:=1 to Reserved do // reserve extra space
      AddPointerVar(0);

    if Assigned(RegisterDRTTIProperties) then
      RegisterDRTTIProperties(result, C, Self);

    AddEndOfClassHeader(result);

    for I:=0 to UnresolvedTypes.Count - 1 do
    begin
      pti := UnresolvedTypes[I];
      K := UnresolvedPropIds[I];

      // Call event to get namespace of type.
      if Assigned(GetNamespaceOfType) then
        LevelId := GetNamespaceOfType(Self, pti)
      else
        LevelId := 0;

      if pti^.Kind = tkClass then
      begin
        ExternList.Add(K, PTIName(pti), erTypeId);
        continue;
      end;

      T := RegisterRTTIType(LevelId, pti);

      if T = 0 then
      begin
        Records[K].Name := '';
        Records[K].Kind := KindNONE;
      end
      else
        Records[K].TypeID := T;
    end;

  finally
    FreeAndNil(UnresolvedPropIds);
    FreeAndNil(UnresolvedTypes);
  end;
end;

function TBaseSymbolTable.RegisterClassReferenceType(LevelId: Integer;
                                                     const TypeName: String;
                                                     OriginClassId: Integer): Integer;
begin
  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  LastCard := Card;

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeCLASSREF;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := OriginClassId;
    result := Id;

    Completed := true;
  end;

  BindDummyType(result, OriginClassId);
end;

function TBaseSymbolTable.RegisterHelperType(LevelId: Integer;
                                             const TypeName: String;
                                             OriginTypeId: Integer): Integer;
begin
  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  LastCard := Card;

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeHELPER;
    Host := true;
    Shift := 0;
    Level := LevelId;
    PatternId := OriginTypeId;
    result := Id;

    Completed := true;
  end;

  BindDummyType(result, OriginTypeId);

  TypeHelpers.Add(result, OriginTypeId);
end;

procedure TBaseSymbolTable.BindDummyType(TypeId, OriginTypeId: Integer);
begin
  if Records[OriginTypeId].IsDummyType then
  begin
    ExternList.Add(TypeId,
                   Records[OriginTypeId].FullName,
                   erPatternId);
    Records[OriginTypeId].Name := '';
    Records[OriginTypeId].Kind := KindNONE;
  end;
end;

function TBaseSymbolTable.RegisterRecordType(LevelId: Integer;
                                             const TypeName: String;
                                             Align: Integer): Integer;
begin
  if SomeTypeList.Count > 0 then
    UpdateSomeTypeList(TypeName, Card + 1);

  LastCard := Card;

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeRECORD;
    Host := true;
    Shift := 0;
    Level := LevelId;
    DefaultAlignment := Align;

    result := Id;
  end;
end;

function TBaseSymbolTable.RegisterRTTIType(LevelId: Integer;
                                           pti: PTypeInfo): Integer;
var
  TypeData, td, ptd: PTypeData;
  S, SParamType: String;
  I, T, K1, K2: Integer;

  pParam: PParamData;
  pNameString, pTypeString: ^ShortString;

  ParamIds: array[0..20] of Integer;
  ParamRef: array[0..20] of Boolean;
  ParamNames: array[0..20] of string;
  H_Sub: Integer;
begin
  LastCard := Card;

  S := PTIName(pti);
  result := LookupType(S, LevelId, true);
  if result > 0 then
    Exit;

  case pti^.Kind of
{$IFDEF VARIANTS}
    tkDynArray:
    begin
      ptd := GetTypeData(pti);
      I := LookupType(StringFromPShortString(@ptd.elType2^.Name), true);
      if I > 0 then
        result := RegisterDynamicArrayType(LevelId, S, I);
    end;
{$ENDIF}
    tkInteger:
    begin
      ptd := GetTypeData(pti);
      T := typeINTEGER;
      case ptd^.OrdType of
        otSByte: T := typeSMALLINT;
        otUByte: T := typeBYTE;
        otSWord: T := typeSHORTINT;
        otUWord: T := typeWORD;
        otSLong: T := typeINTEGER;
        otULong: T := typeCARDINAL;
      end;
      result := RegisterSubrangeType(LevelId, S, T, ptd^.MinValue, ptd^.MaxValue);
    end;
    tkInt64:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeINT64);
    end;
    tkFloat:
    begin
      TypeData := GetTypeData(pti);
      with TypeData^ do
      case FloatType of
        ftSingle: result := RegisterTypeAlias(LevelId, PTIName(pti), typeSINGLE);
        ftDouble: result := RegisterTypeAlias(LevelId, PTIName(pti), typeDOUBLE);
        ftExtended: result := RegisterTypeAlias(LevelId, PTIName(pti), typeEXTENDED);
      end
    end;
    tkEnumeration:
    begin
      S := PTIName(pti);
      K1 := Card;
      T := RegisterEnumType(LevelId, S, typeINTEGER);
      K2 := Card;
      if K1 <> K2 then
      begin
        TypeData := GetTypeData(pti);
        with TypeData^ do
        begin
//        if (MinValue < 256) and (MaxValue < 256) then
          for I:= MinValue to MaxValue do
          begin
            S := GetEnumName(pti, I);
            RegisterEnumValue(T, S, I);
          end;
        end;
      end;
      result := T;
    end;
    tkSet:
    begin
      TypeData := GetTypeData(pti);
      if TypeData = nil then
        Exit;
      if TypeData^.CompType = nil then
        Exit;
      S := StringFromPShortString(@TypeData^.CompType^.Name);
      case TypeData^.CompType^.Kind of
        tkInteger:
        begin
          result := RegisterSetType(LevelId, PTIName(pti), typeINTEGER);
        end;
{$IFNDEF PAXARM}
        tkChar:
        begin
          result := RegisterSetType(LevelId, PTIName(pti), typeANSICHAR);
        end;
{$ENDIF}
        tkWChar:
        begin
          result := RegisterSetType(LevelId, PTIName(pti), typeWIDECHAR);
        end;
        tkEnumeration:
        begin
          K1 := Card;
          if IsValidName(S) then
            T := RegisterEnumType(LevelId, S, typeINTEGER)
          else
            T := RegisterEnumType(LevelId, PTIName(pti) + '_Comp', typeINTEGER);

          K2 := Card;
          if K1 <> K2 then
          begin
{$ifdef fpc}
            td := GetTypeData(TypeData^.CompType);
{$else}
            td := GetTypeData(TypeData^.CompType^);
{$endif}
            with td^ do
              if (MinValue < 256) and (MaxValue < 256) then
              for I:= MinValue to MaxValue do
              begin
{$ifdef fpc}
                S := GetEnumName(TypeData^.CompType, I);
{$else}
                S := GetEnumName(TypeData^.CompType^, I);
{$endif}
                RegisterEnumValue(T, S, I);
              end;
          end;
          result := RegisterSetType(LevelId, PTIName(pti), T);
        end;
      end;
    end;
    tkClass:
    begin
      TypeData := GetTypeData(pti);
      if LevelId > 0 then
      begin
        if StrEql(StringFromPShortString(@TypeData^.UnitName), Records[LevelId].Name) then
          result := RegisterClassType(LevelId, TypeData^.ClassType)
        else
          result := FindClassTypeIdByPti(pti);
      end
      else
      begin
        result := FindClassTypeIdByPti(pti);
        if result = 0 then
          result := RegisterClassType(LevelId, TypeData^.ClassType);
      end;
    end;
    tkInterface:
    begin
      TypeData := GetTypeData(pti);
      if TypeData <> nil then
        if ifHasGuid in TypeData^.IntfFlags then
          result := RegisterInterfaceType(LevelId, pti);
    end;
{$IFNDEF PAXARM}
    tkChar:
    begin
      ptd := GetTypeData(pti);
      T := typeANSICHAR;
      result := RegisterSubrangeType(LevelId, S, T, ptd^.MinValue, ptd^.MaxValue);
    end;
    tkString:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeSHORTSTRING);
    end;
    tkLString:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeANSISTRING);
    end;
    tkWString:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeWIDESTRING);
    end;
{$ENDIF}
    tkWChar:
    begin
      ptd := GetTypeData(pti);
      T := typeWIDECHAR;
      result := RegisterSubrangeType(LevelId, S, T, ptd^.MinValue, ptd^.MaxValue);
    end;
{$IFDEF UNIC}
    tkUString:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeUNICSTRING);
    end;
{$ENDIF}
    tkVariant:
    begin
      result := RegisterTypeAlias(LevelId, PTIName(pti), typeVARIANT);
    end;
    tkMethod:
    begin
      S := PTIName(pti);

      result := LookUpType(S, 0, true);
      if result > 0 then
        Exit;

      ptd := GetTypeData(pti);

      pParam := PParamData(@(ptd^.ParamList));
      I := 0;
      while I <= ptd^.ParamCount - 1 do
      begin
        ParamRef[I] := false;
        if pfVar in pParam^.Flags then
        begin
          ParamRef[I] := true;
        end
        else if pfConst in pParam^.Flags then
        begin
        end
        else if pfOut in pParam^.Flags then
        begin
          ParamRef[I] := true;
        end;

        if pfArray in pParam^.Flags then
        begin
          result := H_TMethod;
          Exit;
        end;

        pNameString := ShiftPointer(pParam, SizeOf(TParamFlags));
        ParamNames[I] := StringFromPShortString(PShortString(pNameString));

        pTypeString := ShiftPointer(pParam, SizeOf(TParamFlags) +
                                    Length(pParam^.ParamName) + 1);

        SParamType := StringFromPShortString(PShortString(pTypeString));
        T := LookUpType(SParamType, 0, true);

        if T = 0 then
        begin
          result := H_TMethod;
          Exit;
        end;
        ParamIds[I] := T;

        if Records[T].FinalTypeId in [typeRECORD, typeARRAY] then
          ParamRef[I] := true;

        pParam := ShiftPointer(pTypeString, Length(pTypeString^) + 1);
        Inc(I);
      end;

      T := typeVOID;
      if ptd^.MethodKind = mkFunction then
      begin
        pTypeString := Pointer(pParam);
        T := LookUpType(StringFromPShortString(PShortString(pTypeString)), 0, true);
        if T = 0 then
        begin
          result := H_TMethod;
          Exit;
        end;
      end;

      H_Sub := RegisterRoutine(LevelId, '', T, ccREGISTER, nil);

      K1 := I;
      for I:=0 to K1 - 1 do
        RegisterParameter(H_Sub, ParamIds[I], Unassigned, ParamRef[I], ParamNames[I]);
      result := RegisterEventType(LevelId, S, H_Sub);
    end;
    else
    begin
      Exit;
    end;
  end;
end;

function TBaseSymbolTable.RegisterProperty(LevelId: Integer; const PropName: String;
                                           PropTypeID, i_ReadId, i_WriteId: Integer;
                                           i_IsDefault: Boolean): Integer;
var
  I: Integer;
begin
  with AddRecord do
  begin
    Name := PropName;
    Count := 0;
    Kind := KindPROP;
    TypeID := PropTypeID;
    Host := true;
    Shift := 0;
    Level := LevelId;

    if i_ReadId <= 0 then
      ReadId := - i_ReadId
    else
    begin
      ReadId := -1;
      for I:=Card downto 1 do
        if Records[I].Shift = i_ReadId then
        begin
          ReadId := I;
          Break;
        end;

      if ReadId = -1 then
      begin
        result := 0;
        RaiseError(errInternalError, []);
        Exit;
      end;
    end;

    if i_WriteId <= 0 then
      WriteId := - i_WriteId
    else
    begin
      WriteId := -1;
      for I:=Card downto 1 do
        if Records[I].Shift = i_WriteId then
        begin
          WriteId := I;
          Break;
        end;

      if WriteId = -1 then
      begin
        result := 0;
        RaiseError(errInternalError, []);
        Exit;
      end;
    end;

    IsDefault := i_IsDefault;

    result := Id;
  end;

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := PropTypeID;
    Host := true;
    Shift := 0;
    Level := result;
  end;
end;

function TBaseSymbolTable.RegisterInterfaceProperty(LevelId: Integer;
                                   const PropName: String;
                                   PropTypeID,
                                   ReadIndex,
                                   WriteIndex: Integer): Integer;
var
  I: Integer;
  R: TSymbolRec;
  exists: Boolean;
begin
  with AddRecord do
  begin
    Name := PropName;
    Count := 0;
    Kind := KindPROP;
    TypeID := PropTypeID;
    Host := true;
    Shift := 0;
    Level := LevelId;

    result := Id;

    if ReadIndex <= 0 then
      ReadId := 0
    else
    begin
      exists := false;
      for I:=Card downto 1 do
      begin
        R := Records[I];
        if R.Level = LevelId then
        if R.MethodIndex = ReadIndex then
        begin
          ReadId := I;
          exists := true;
          Break;
        end;
      end;

      if not exists then
      begin
        result := 0;
        RaiseError(errInternalError, []);
        Exit;
      end;
    end;

    if WriteIndex <= 0 then
      WriteId := 0
    else
    begin
      exists := false;
      for I:=Card downto 1 do
      begin
        R := Records[I];
        if R.Level = LevelId then
        if R.MethodIndex = WriteIndex then
        begin
          WriteId := I;
          exists := true;
          Break;
        end;
      end;

      if not exists then
      begin
        result := 0;
        RaiseError(errInternalError, []);
        Exit;
      end;
    end;
  end;

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := PropTypeID;
    Host := true;
    Shift := 0;
    Level := result;
  end;
end;

function TBaseSymbolTable.RegisterTypeFieldEx(LevelId: Integer;
                                              const Declaration: String;
                                              FieldOffset: Integer = -1): Integer;
var
  FieldName, FieldTypeName: String;
  FieldTypeId: Integer;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  FieldName := HeaderParser.Parse_Ident;
  HeaderParser.Match(':');
  FieldTypeName := HeaderParser.CurrToken;
  FieldTypeId := HeaderParser.LookupId(FieldTypeName);
  result := RegisterTypeField(LevelId, FieldName, FieldTypeId, FieldOffset);
end;

function TBaseSymbolTable.RegisterTypeField(LevelId: Integer;
                                            const FieldName: String;
                                            FieldTypeID: Integer;
                                            FieldOffset: Integer = -1;
                                            ACompIndex: Integer = -1): Integer;
var
  J, S: Integer;
  DefAlign, CurrAlign, J1, FT, FT1: Integer;
  R: TSymbolRec;
begin
  with AddRecord do
  begin
    Name := FieldName;
    Kind := KindTYPE_FIELD;
    TypeID := FieldTypeId;
    Host := true;
    Shift := FieldOffset;
    Level := LevelId;

    result := - Id;

    CompIndex := ACompIndex;
  end;

  Records[FieldTypeId].Completed := true;

  if (FieldOffset = -1) and (ACompIndex = -1) then
  begin
    S := 0;
    if Records[LevelId].IsPacked then
    begin
      for J:=LevelId + 1 to Card do
      begin
        R := Records[J];

        if R = SR0 then
          break;

        if (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId) then
        begin
          R.Shift := S;
          Inc(S, R.Size);
        end;
      end;
    end
    else
    begin
      DefAlign := Records[LevelId].DefaultAlignment;
      J1 := -1;

      for J:=LevelId + 1 to Card do
      begin
        R := Records[J];

        if R = SR0 then
          break;

        if (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId) then
        begin
          CurrAlign := GetAlignmentSize(R.TypeId, DefAlign);

          if J1 > 0 then
          begin
            FT1 := Records[J-1].FinalTypeId;
            FT := R.FinalTypeId;
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

          R.Shift := S;
          Inc(S, R.Size);
        end;
      end; // for-loop
    end;
  end;
end;

function TBaseSymbolTable.RegisterVariantRecordTypeField(LevelId: Integer;
                                const Declaration: String;
                                VarCnt: Int64): Integer;
var
  FieldName: String;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  FieldName := HeaderParser.Parse_Ident;
  result := HeaderParser.Register_VariantRecordTypeField(FieldName, VarCnt);
end;

function TBaseSymbolTable.RegisterVariantRecordTypeField(LevelId: Integer;
                                const FieldName: String;
                                FieldTypeID: Integer;
                                VarCnt: Int64): Integer;
var
  J, S: Integer;
  DefAlign, CurrAlign, J1, FT, FT1, VJ, VK, VS: Integer;
  VarPathList: TVarPathList;
  Path: TVarPath;
  R: TSymbolRec;
begin
  with AddRecord do
  begin
    Name := FieldName;
    Kind := KindTYPE_FIELD;
    TypeID := FieldTypeId;
    Host := true;
    Level := LevelId;
    VarCount := VarCnt;

    result := - Id;
  end;

  VarPathList := TVarPathList.Create;
  try
    for J:=LevelId + 1 to Card do
    begin
      R := Records[J];
      if R = SR0 then
        break;

      if (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId) then
        if R.VarCount > 0 then
          VarPathList.Add(J, R.VarCount);
    end;

    S := 0;
    if Records[LevelId].IsPacked then
    begin
      for J:=LevelId + 1 to Card do
      begin
        R := Records[J];
        if R = SR0 then
          break;

        if (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId) then
        begin
          if R.VarCount > 0 then
            break;

          R.Shift := S;
          Inc(S, R.Size);
        end;
      end;

      // process variant part

      VS := S;
      for VK :=0 to VarPathList.Count - 1 do
      begin
        Path := VarPathList[VK];
        S := VS;

        for VJ := 0 to Path.Count - 1 do
        begin
          J := Path[VJ].Id;
          Records[J].Shift := S;
          Inc(S, Records[J].Size);
        end;
      end;
    end
    else
    begin
      DefAlign := Records[LevelId].DefaultAlignment;
      J1 := -1;

      for J:=LevelId + 1 to Card do
      begin
        R := Records[J];
        if R = SR0 then
          break;

        if (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId) then
        begin
          if R.VarCount > 0 then
            break;

          CurrAlign := GetAlignmentSize(R.TypeId, DefAlign);

          if J1 > 0 then
          begin
            FT1 := Records[J-1].FinalTypeId;
            FT := R.FinalTypeId;
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

          R.Shift := S;
          Inc(S, R.Size);
        end;
      end; // for-loop

      // process variant part

      VS := S;
      for VK :=0 to VarPathList.Count - 1 do
      begin
        S := VS;
        Path := VarPathList[VK];

        for VJ := 0 to Path.Count - 1 do
        begin
          J := Path[VJ].Id;
          CurrAlign := GetAlignmentSize(Records[J].TypeId, DefAlign);

          if J1 > 0 then
          begin
            FT1 := Records[J-1].FinalTypeId;
            FT := Records[J].FinalTypeId;
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

          Records[J].Shift := S;
          Inc(S, Records[J].Size);
        end;
      end;
    end;

  finally
    FreeAndNil(VarPathList);
  end;
end;

function TBaseSymbolTable.RegisterVariable(LevelId: Integer;
                                           const Declaration: String; Address: Pointer): Integer;
var
  VarName: String;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  VarName := HeaderParser.Parse_Ident;
  result := HeaderParser.Register_Variable(VarName, Address);
end;

function TBaseSymbolTable.RegisterVariable(LevelId: Integer; const VarName: String;
                                           VarTypeID: Integer; i_Address: Pointer): Integer;
begin
  LastCard := Card;

  result := LastShiftValue;
  with AddRecord do
  begin
    Name := VarName;
    Kind := KindVAR;
    TypeID := VarTypeID;

    Host := true;
    Shift := LastShiftValue;
    Level := LevelId;

    Address := i_Address;
  end;
  Inc(LastShiftValue, SizeOfPointer);

  LastVarId := Card;
end;

function TBaseSymbolTable.RegisterObject(LevelId: Integer;
                                         const ObjectName: String;
                                         TypeId: Integer;
                                         i_Address: Pointer): Integer;
var
  X: TObject;
  C: TComponent;
  I, Offset: Integer;
  S, FieldTypeName: String;
  FAddress: Pointer;
  FieldTypeId: Integer;
begin
  if ObjectName = '' then
  begin
    result := 0;
    if i_Address <> nil then
    begin
      X := TObject(i_Address^);
      if X is TComponent then
      begin
        C := X as TComponent;
        for I := 0 to C.ComponentCount - 1 do
        begin
          S := C.Components[I].Name;
          FieldTypeName := C.Components[I].ClassName;
          FieldTypeId := LookupType(FieldTypeName, true);
          FAddress := C.FieldAddress(S);
          if FieldTypeId > 0 then
            RegisterObject(LevelId, S, FieldTypeId, FAddress);
        end;
      end;
    end;
    Exit;
  end;

  result := RegisterVariable(LevelId, ObjectName, TypeId, i_Address);
  if i_Address <> nil then
  begin
    X := TObject(i_Address^);
    if X is TComponent then
    begin
      C := X as TComponent;
      for I := 0 to C.ComponentCount - 1 do
      begin
        S := C.Components[I].Name;
        FieldTypeName := C.Components[I].ClassName;
        FieldTypeId := LookupType(FieldTypeName, true);
        if FieldTypeId > 0 then
        begin
          FAddress := C.FieldAddress(S);
          if FAddress <> nil then
          begin
            Offset := Integer(FAddress) - Integer(C);
            RegisterTypeField(TypeId, S, FieldTypeId, Offset);
          end
          else
          begin
            RegisterTypeField(TypeId, S, FieldTypeId, -1, I);
          end;
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.RegisterVirtualObject(LevelId: Integer;
                                                const ObjectName: String): Integer;
begin
  result := RegisterVariable(LevelId, ObjectName, typeVOBJECT, nil);
end;

procedure TBaseSymbolTable.RegisterMember(LevelId: Integer; const MemberName: String;
                                          i_Address: Pointer);
begin
  RegisterVariable(LevelId, MemberName, typeINTEGER, i_Address);
end;

function TBaseSymbolTable.RegisterConstant(LevelId: Integer;
                                           const Declaration: String): Integer;
var
  ConstName: String;
begin
  HeaderParser.Init(Declaration, Self, LevelId);
  HeaderParser.Call_SCANNER;
  ConstName := HeaderParser.Parse_Ident;
  result := HeaderParser.Register_Constant(ConstName);
end;

function TBaseSymbolTable.RegisterConstant(LevelId: Integer;
                                           const i_Name: String; i_TypeID: Integer; const i_Value: Variant): Integer;
var
  R: TSymbolRec;
  FT: Integer;
{$IFDEF PAX64}
  VCardinal: UInt64;
{$ELSE}
  VCardinal: Cardinal;
{$ENDIF}
  VWideChar: WideChar;
{$IFDEF PAXARM}
  VWideString: String;
{$ELSE}
  VWideString: WideString;
{$ENDIF}
begin
  LastCard := Card;
  R := nil;

{$IFNDEF PAXARM}
  if Records[i_TypeID].HasPAnsiCharType then
    FT := typePANSICHAR
  else
{$ENDIF}
  if Records[i_TypeID].HasPWideCharType then
    FT := typePWIDECHAR
  else
    FT := Records[i_TypeID].FinalTypeId;

{$IFNDEF PAXARM}
  if FT = typeANSICHAR then
    if Integer(i_Value) > 255 then
    begin
      FT := -1;
      R := AddWideCharConst(Integer(i_Value));
    end;
{$ENDIF}

  if FT <> - 1 then
  case FT of
    typeENUM: R := AddEnumConst(i_TypeId, Integer(i_Value));
{$IFNDEF PAXARM}
    typeANSICHAR: R := AddAnsiCharConst(AnsiChar(Integer(i_Value)));
    typePANSICHAR: R := AddPAnsiCharConst(AnsiString(i_Value));
    typeANSISTRING: R := AddPAnsiCharConst(AnsiString(i_Value));
    typeSHORTSTRING: R := AddShortStringConst(i_Value);
{$ENDIF}
    typeWIDECHAR:
    begin
      VWideString := i_Value;
      VWideChar := VWideString[1];
      R := AddWideCharConst(Integer(VWideChar));
    end;
    typePWIDECHAR: R := AddPWideCharConst(i_Value);
    typeUNICSTRING: R := AddPWideCharConst(i_Value);
    typeBYTE: R := AddByteConst(i_Value);
    typeWORD: R := AddWordConst(i_Value);
    typeINTEGER:
    begin
      if Abs(i_Value) > MaxInt then
      begin
        R := AddInt64Const({$IfNDef VARIANTS} integer (i_Value) {$Else} i_Value {$EndIf});
        R.TypeID := typeINTEGER;
      end
      else
        R := AddIntegerConst(i_Value);
    end;
{$IFDEF VARIANTS}
    typeINT64: R := AddInt64Const(i_Value);
{$ELSE}
    typeINT64: R := AddInt64Const(Integer(i_Value));
{$ENDIF}

    typeCARDINAL: R := AddCardinalConst(i_Value);
    typeSMALLINT: R := AddSmallIntConst(i_Value);
    typeSHORTINT: R := AddShortIntConst(i_Value);
    typeDOUBLE: R := AddDoubleConst(i_Value);
    typeSINGLE: R := AddSingleConst(i_Value);
    typeEXTENDED: R := AddExtendedConst(i_Value);
    typeCURRENCY: R := AddCurrencyConst(i_Value);
    typeBOOLEAN: R := AddBooleanConst(i_Value);
{$IFDEF UNIX}
    typeBYTEBOOL: R := AddBooleanConst(i_Value);
{$ELSE}
    typeBYTEBOOL: R := AddByteBoolConst(ByteBool(Byte(i_Value)));
{$ENDIF}
    typeWORDBOOL: R := AddWordBoolConst(i_Value);
    typeLONGBOOL: R := AddLongBoolConst(i_Value);
    typeVARIANT: R := AddVariantConst(i_Value);
    typeOLEVARIANT: R := AddOleVariantConst(i_Value);
    typeRECORD: R := AddRecordConst(i_TypeId, i_Value);
    typeARRAY: R := AddArrayConst(i_TypeId, i_Value);
    typeSET: R := AddSetConst(I_TypeId, i_Value);
    typeCLASS:
    begin
      VCardinal := i_Value;
      R := AddClassConst(I_TypeId, TObject(VCardinal));
    end;
    typeCLASSREF:
    begin
      VCardinal := i_Value;
      R := AddClassRefConst(I_TypeId, TClass(VCardinal));
    end;
    typePOINTER, typeVOID:
    begin
      VCardinal := i_Value;
      R := AddPointerConst(i_TypeId, Pointer(VCardinal));
    end;
  else
    begin
      result := 0;
      RaiseError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
  end;

  R.Level := LevelId;
  R.Name := i_Name;
//  R.Host := true;
  result := R.Id;
end;

function TBaseSymbolTable.RegisterConstant(LevelId: Integer;
                                           const i_Name: String; const i_Value: Variant): Integer;
var
  TypeID: Integer;
  VT: Integer;
begin
  LastCard := Card;

  VT := VarType(i_Value);

  case VT of
    varEmpty: typeId := typeVARIANT;
    varBoolean: TypeId := typeBOOLEAN;
    varInteger, varByte, varSmallInt: TypeId := typeINTEGER;
{$IFDEF VARIANTS}
    varShortInt, varWord, varLongWord: TypeId := typeINTEGER;
{$ENDIF}
{$IFDEF UNIC}
    varUString: typeId := typeUNICSTRING;
{$ENDIF}
{$IFNDEF PAXARM}
    varString: TypeId := typeANSISTRING;
{$ENDIF}
    varDouble: TypeId := typeDOUBLE;
    varCurrency: TypeId := typeCURRENCY;
  else
    begin
      result := 0;
      RaiseError(errIncompatibleTypesNoArgs, []);
      Exit;
    end;
  end;
  result := RegisterConstant(LevelId, i_Name, TypeId, i_Value);
end;

function TBaseSymbolTable.RegisterPointerConstant(LevelId: Integer;
                                           const i_Name: String; const i_Value: Pointer): Integer;
var
  I: IntPax;
begin
  I := IntPax(i_Value);
  result := RegisterConstant(LevelId, i_Name, typePOINTER, I);
end;

function TBaseSymbolTable.RegisterExtendedConstant(LevelId: Integer;
                                           const i_Name: String; const i_Value: Extended): Integer;
var
  D: Double;
begin
  if i_Value > MaxDouble then
    D := MaxDouble
  else if (i_Value > 0) and (i_Value < MinDouble) then
    D := MinDouble
  else
    D := i_Value;

  result := RegisterConstant(LevelId, i_Name, typeEXTENDED, D);
end;

function TBaseSymbolTable.RegisterInt64Constant(LevelId: Integer;
                                                const i_Name: String; const i_Value: Int64): Integer;
begin
{$IFDEF VARIANTS}
  if Abs(i_Value) >= Abs(MaxInt) then
    result := RegisterConstant(LevelId, i_Name, typeINT64, i_Value)
  else
    result := RegisterConstant(LevelId, i_Name, typeINTEGER, i_Value);
{$ELSE}
  result := RegisterConstant(LevelId, i_Name, typeINTEGER, Integer(i_Value));
{$ENDIF}
end;

function TBaseSymbolTable.RegisterRoutine(LevelId: Integer;
                                          const SubName: String; ResultTypeID: Integer;
                                          CallConvention: Integer;
                                          i_Address: Pointer;
                                          i_OverCount: Integer = 0;
                                          i_IsDeprecated: Boolean = false): Integer;

var
  SubID: Integer;
begin
{$IFDEF PAX64}
  CallConvention := cc64;
{$ENDIF}
  result := LastShiftValue;
  with AddRecord do
  begin
    Name := SubName;
    Count := 0;
    Kind := KindSUB;
    TypeID := ResultTypeID;
    Host := true;
    Shift := LastShiftValue;
    Level := LevelId;
    CallConv := CallConvention;
    IsDeprecated := i_IsDeprecated;

    if not (CallConvention in [ccSTDCALL, ccREGISTER, cc64,
                               ccCDECL, ccPASCAL,
                               ccSAFECALL, ccMSFASTCALL]) then
    begin
      RaiseError(errInternalError, []);
      Exit;
    end;

    SubId := Id;

    Address := i_Address;
    OverCount := i_OverCount;
  end;
  Inc(LastShiftValue, SizeOfPointer);

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := ResultTypeID;

    Host := true;

    Shift := 0;
    Level := SubId;
  end;

  with AddRecord do
  begin
    Kind := KindNONE;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  LastSubId := SubId;
end;

function TBaseSymbolTable.RegisterRoutine(LevelId: Integer;
    const SubName, ResultType: String;
    CallConvention: Integer;
    i_Address: Pointer;
    i_OverCount: Integer = 0;
    i_IsDeprecated: Boolean = false): Integer;
var
  TypeId: Integer;
begin
  TypeId := LookupType(ResultType, true);
  if TypeId = 0 then
  begin
    ExternList.Add(Card + 1,
                   ResultType,
                   erTypeId);
  end;
  result := RegisterRoutine(LevelId, SubName, TypeId,
     CallConvention, i_Address, i_OverCount, i_IsDeprecated);
end;

function TBaseSymbolTable.RestorePositiveIndex(L: Integer): Integer;
var
  R: TSymbolRec;
  SupportedInterfaces: TGuidList;
  I, IntfId, temp: Integer;
begin
  R := Records[L];
  SupportedInterfaces := R.SupportedInterfaces;

  result := -1;

  if SupportedInterfaces <> nil then
  begin
    for I:=0 to SupportedInterfaces.Count - 1 do
    begin
//      Idx := GuidList.IndexOf(SupportedInterfaces[I].GUID);
//      if Idx >= 0 then
      begin
        IntfId := SupportedInterfaces[I].Id;
        temp := FindMaxMethodIndex(IntfId);
        if temp > result then
          result := temp;
      end;
    end
  end
  else
    result := 3;
end;

function TBaseSymbolTable.FindMaxMethodIndex(IntfId: Integer): Integer;
var
  I, temp: Integer;
  R: TSymbolRec;
begin
  result := -1;
  for I:= IntfId + 1 to Card do
  begin
    R := Records[I];

    if R = SR0 then
      break;
    if R.Kind = KindNAMESPACE then
      break;

    if R.Level = IntfId then
      if R.MethodIndex > 0 then
      begin
        temp := R.MethodIndex;
        if temp > result then
          result := temp
      end;
  end;

  if result = -1 then
    result := RestorePositiveIndex(IntfId);
end;

function TBaseSymbolTable.RegisterMethod(LevelId: Integer;
                                     const SubName: String; ResultTypeID: Integer;
                                     CallConvention: Integer;
                                     i_Address: Pointer;
                                     IsShared: Boolean = false;
                                     i_CallMode: Integer = cmNONE;
                                     i_MethodIndex: Integer = 0;
                                     i_OverCount: Integer = 0;
                                     i_IsAbstract: Boolean = false;
                                     i_AbstractMethodCount: Integer = 0;
                                     i_IsDeprecated: Boolean = false): Integer;
var
  SubID: Integer;
  PositiveIndex: Integer;
  NegativeIndex: Integer;
  MethodIndex: Integer;
  C: TClass;
begin
  NegativeIndex := 0;

  if i_MethodIndex < 0 then
  begin
    NegativeIndex := i_MethodIndex;

    PositiveIndex := RestorePositiveIndex(LevelId);
    if PositiveIndex = -1 then
      RaiseError(errInternalError, []);
    i_MethodIndex := Abs(i_MethodIndex) + PositiveIndex;
  end;

  result := LastShiftValue;
  with AddRecord do
  begin
    Name := SubName;
    Count := 0;
    Kind := KindSUB;
    TypeID := ResultTypeID;
    Host := true;
    Shift := LastShiftValue;
    Level := LevelId;
    CallConv := CallConvention;
{$IFDEF PAX64}
    CallConv := cc64;
{$ENDIF}
    IsSharedMethod := IsShared;

    if not (CallConvention in [ccSTDCALL, ccREGISTER, ccCDECL, cc64,
                               ccPASCAL, ccSAFECALL, ccMSFASTCALL]) then
    begin
      RaiseError(errInternalError, []);
      Exit;
    end;

    SubId := Id;

    Address := i_Address;
    CallMode := i_CallMode;
    MethodIndex := i_MethodIndex;
    NegativeMethodIndex := NegativeIndex;
    OverCount := i_OverCount;
    IsDeprecated := i_IsDeprecated;
  end;

  Inc(LastShiftValue, SizeOfPointer);

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := ResultTypeID;

    Host := true;

    Shift := 0;
    Level := SubId;
  end;

  with AddRecord do
  begin
    Kind := KindNONE;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  LastSubId := SubId;

  if i_CallMode = cmNONE then
    Exit;

  C := Records[LevelId].PClass;
  if C <> nil then
  begin
    MethodIndex := VirtualMethodIndex(C, i_Address) + 1;
    if MethodIndex > 0 then
    begin
      Records[LastSubId].MethodIndex := MethodIndex;
    end;

    if i_IsAbstract then
    begin
      Records[LastSubId].CallMode := cmVIRTUAL;
      if i_MethodIndex > 0 then
        HeaderParser.AbstractMethodCount := i_MethodIndex;
      Records[LastSubId].MethodIndex :=
        GetAbstractMethodIndex(C, i_AbstractMethodCount, i_Address) + 1;
    end;

    if i_CallMode in [cmDYNAMIC, cmOVERRIDE] then
      Records[LastSubId].DynamicMethodIndex :=
        GetDynamicMethodIndexByAddress(C, i_Address);
  end;

end;

function TBaseSymbolTable.RegisterMethod(LevelId: Integer;
                                     const SubName, ResultType: String;
                                     CallConvention: Integer;
                                     i_Address: Pointer;
                                     IsShared: Boolean = false;
                                     i_CallMode: Integer = cmNONE;
                                     i_MethodIndex: Integer = 0;
                                     i_OverCount: Integer = 0;
                                     i_IsAbstract: Boolean = false;
                                     i_AbstractMethodCount: Integer = 0;
                                     i_IsDeprecated: Boolean = false): Integer;
var
  TypeId: Integer;
begin
  TypeId := LookupType(ResultType, true);
  if TypeId = 0 then
  begin
    ExternList.Add(Card + 1,
                   ResultType,
                   erTypeId);
  end;

  result := RegisterMethod(LevelId, SubName, TypeId, CallConvention, i_Address,
    IsShared, i_CallMode, i_MethodIndex, i_OverCount,
    i_IsAbstract, i_AbstractMethodCount, i_IsDeprecated);
end;

function TBaseSymbolTable.RegisterConstructor(LevelId: Integer;
                                              const SubName: String;
                                              i_Address: Pointer;
                                              IsShared: Boolean = false;
                                              i_CallMode: Integer = cmNONE;
                                              i_MethodIndex: Integer = 0;
                                              i_OverCount: Integer = 0;
                                              i_IsAbstract: Boolean = false;
                                              i_AbstractMethodCount: Integer = 0;
                                              i_IsDeprecated: Boolean = false): Integer;
var
  SubID, MethodIndex: Integer;
  C: TClass;
begin
  result := LastShiftValue;
  with AddRecord do
  begin
    Name := SubName;
    Count := 0;
    Kind := KindCONSTRUCTOR;
    TypeID := LevelID;
    Host := true;
    Shift := LastShiftValue;
    Level := LevelId;
    CallConv := ccREGISTER;
    IsSharedMethod := IsShared;

{$IFDEF PAX64}
    CallConv := cc64;
{$ENDIF}

    SubId := Id;

    Address := i_Address;
    CallMode := i_CallMode;
    OverCount := i_OverCount;
    IsDeprecated := i_IsDeprecated;
  end;
  Inc(LastShiftValue, SizeOfPointer);

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := LevelID;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  with AddRecord do
  begin
    Kind := KindNONE;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  LastSubId := SubId;

  if i_CallMode = cmNONE then
    Exit;

  C := Records[LevelId].PClass;
  if C <> nil then
  begin
    MethodIndex := VirtualMethodIndex(C, i_Address) + 1;
    if MethodIndex > 0 then
      Records[LastSubId].MethodIndex := MethodIndex;

    if HeaderParser.IsAbstract then
    begin
      Records[LastSubId].CallMode := cmVIRTUAL;
      if i_MethodIndex > 0 then
        HeaderParser.AbstractMethodCount := i_MethodIndex;
      Records[LastSubId].MethodIndex :=
        GetAbstractMethodIndex(C, i_AbstractMethodCount) + 1;
    end;
  end;
end;

function TBaseSymbolTable.RegisterDestructor(LevelId: Integer;
                                             const SubName: String;
                                             i_Address: Pointer;
                                             i_CallMode: Integer = cmVIRTUAL): Integer;
var
  SubID: Integer;
begin
  result := LastShiftValue;
  with AddRecord do
  begin
    Name := SubName;
    Count := 0;
    Kind := KindDESTRUCTOR;
    TypeID := typeVOID;
    Host := true;
    Shift := LastShiftValue;
    Level := LevelId;
    CallConv := ccREGISTER;
{$IFDEF PAX64}
    CallConv := cc64;
{$ENDIF}

    SubId := Id;

    Address := i_Address;
    CallMode := i_CallMode;
  end;

  Inc(LastShiftValue, SizeOfPointer);

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := LevelID;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  with AddRecord do
  begin
    Kind := KindNONE;
    Host := true;
    Shift := 0;
    Level := SubId;
  end;

  LastSubId := SubId;
end;

function TBaseSymbolTable.RegisterFakeHeader(LevelId: Integer;
              const Header: String; Address: Pointer): Integer;
begin
  result := RegisterHeader(LevelId, Header, Address);
  Records[LastSubId].IsFakeMethod := true;
end;

function TBaseSymbolTable.RegisterHeader(LevelId: Integer;
                               const Header: String; Address: Pointer;
                               AMethodIndex: Integer = 0): Integer;
var
  TypeId, I, J, L, P, ElemTypeId, SubId, ReadId, WriteId, PropId: Integer;
  IsMethod: Boolean;
  ParamMod: TParamMod;
  S: String;
  Tag: Integer;
  OverList: TIntegerList;
  OpenArray: Boolean;
  AncestorId: Integer;
  OverCount: Integer;
label
  label_params;
begin
  LastCard := Card;

  result := 0;

  HeaderParser.Init(Header, Self, LevelId);
  if not HeaderParser.Parse then
  begin
    if RaiseE then
      raise Exception.Create(errSyntaxError);

    REG_OK := false;
    Exit;
  end;
{$IFDEF PAX64}
  HeaderParser.CC := cc64;
{$ENDIF}

  IsMethod := (LevelId > 0) and (Records[LevelId].Kind = KindTYPE);

  if IsMethod then
  begin
    L := Records[LevelId].Level;
  end
  else
    L := LevelId;

  if HeaderParser.IsProperty then
  begin
    if (HeaderParser.ReadIdent = '') and (HeaderParser.ReadIdent = '') then
    if LevelId > 0 then
    if Records[LevelId].FinalTypeId = typeCLASS then
    begin
      AncestorId := Records[LevelId].AncestorId;
      while AncestorId > 0 do
      begin
        PropId := Lookup(HeaderParser.Name, AncestorId, true);
        if PropId = 0 then
          break;
        if Records[PropId].Kind <> KindPROP then
          break;
        if (Records[PropId].ReadId <> 0) or (Records[PropId].WriteId <> 0) then
        begin
          HeaderParser.ReadIdent := Records[Records[PropId].ReadId].Name;
          HeaderParser.WriteIdent := Records[Records[PropId].WriteId].Name;
          break;
        end
        else
          AncestorId := Records[AncestorId].AncestorId;
      end;
    end;

    if HeaderParser.ReadIdent <> '' then
    begin
      ReadId := Lookup(HeaderParser.ReadIdent, LevelId, true);
      if ReadId > 0 then
      begin
        if Records[ReadId].Kind = KindTYPE_FIELD then
          ReadId := - ReadId
        else
          ReadId := Records[ReadId].Shift;
      end
      else
        RaiseError(errUndeclaredIdentifier, [HeaderParser.ReadIdent]);
    end
    else
      ReadId := 0;

    if HeaderParser.WriteIdent <> '' then
    begin
      WriteId := Lookup(HeaderParser.WriteIdent, LevelId, true);
      if WriteId > 0 then
      begin
        if Records[WriteId].Kind = KindTYPE_FIELD then
          WriteId := - WriteId
        else
          WriteId := Records[WriteId].Shift;
      end
      else
        RaiseError(errUndeclaredIdentifier, [HeaderParser.WriteIdent]);
    end
    else
      WriteId := 0;

{
    if HeaderParser.ResType = '' then
    begin
      PropId := Lookup(HeaderParser.Name, LevelId, true);

      if PropId = 0 then
        ExternList.Add(0,
                       Records[LevelId].FullName + '.' + HeaderParser.ResType,
                       ePropertyInBaseClassId);
      Exit;
    end;
}
    TypeId := LookupType(HeaderParser.ResType, L, true);

    if TypeId = 0 then
      ExternList.Add(Card + 1,
                     HeaderParser.ResType,
                     erTypeId);

    if (HeaderParser.ReadIdent <> '') and (ReadId = 0) then
      ExternList.Add(Card + 1,
                     Records[LevelId].FullName + '.' + HeaderParser.ReadIdent,
                     erReadId);

    if (HeaderParser.WriteIdent <> '') and (WriteId = 0) then
      ExternList.Add(Card + 1,
                     Records[LevelId].FullName + '.' + HeaderParser.WriteIdent,
                     erWriteId);

    result := RegisterProperty(LevelId, HeaderParser.Name,
         TypeId, ReadId, WriteId, HeaderParser.IsDefault);
{$IFDEF DRTTI}
   goto label_Params;
{$ENDIF}

    Exit;
  end;

  if HeaderParser.IsOverloaded then
  begin
    OverList := LookupAll(HeaderParser.Name, LevelId, true);
    OverCount := OverList.Count;
    FreeAndNil(OverList);
  end
  else
    OverCount := 0;

  case HeaderParser.KS of
    ksPROCEDURE:
    begin
      if IsMethod then
        result := RegisterMethod(LevelId,
                                 HeaderParser.Name,
                                 TypeVOID,
                                 HeaderParser.CC,
                                 Address,
                                 HeaderParser.IsShared,
                                 HeaderParser.CallMode,
                                 AMethodIndex,
                                 OverCount,
                                 HeaderParser.IsAbstract,
                                 HeaderParser.AbstractMethodCount,
                                 HeaderParser.IsDeprecated)
      else
        result := RegisterRoutine(LevelId, HeaderParser.Name, TypeVOID, HeaderParser.CC,
                        Address, OverCount, HeaderParser.IsDeprecated);
    end;
    ksFUNCTION:
    begin
      TypeId := LookupType(HeaderParser.ResType, L, true);
      if TypeId = 0 then
      begin
        ExternList.Add(Card + 1,
                       HeaderParser.ResType,
                       erTypeId);
      end;
      if IsMethod then
        result := RegisterMethod(LevelId,
                                 HeaderParser.Name,
                                 TypeId,
                                 HeaderParser.CC,
                                 Address,
                                 HeaderParser.IsShared,
                                 HeaderParser.CallMode,
                                 AMethodIndex,
                                 OverCount,
                                 HeaderParser.IsAbstract,
                                 HeaderParser.AbstractMethodCount,
                                 HeaderParser.IsDeprecated)
      else
        result := RegisterRoutine(LevelId, HeaderParser.Name, TypeId, HeaderParser.CC,
                        Address, OverCount, HeaderParser.IsDeprecated);
    end;
    ksCONSTRUCTOR:
    begin
      result := RegisterConstructor(LevelId,
                                    HeaderParser.Name,
                                    Address,
                                    HeaderParser.IsShared,
                                    HeaderParser.CallMode,
                                    AMethodIndex,
                                    OverCount,
                                    HeaderParser.IsAbstract,
                                    HeaderParser.AbstractMethodCount,
                                    HeaderParser.IsDeprecated);
    end;
    ksDESTRUCTOR:
    begin
      result := RegisterMethod(LevelId,
                               HeaderParser.Name,
                               TypeVOID,
                               HeaderParser.CC,
                               Address,
                               false,
                               HeaderParser.CallMode,
                               0);
      Records[LastSubId].Kind := kindDESTRUCTOR;
    end;
  end;

Label_Params:

  for I:=1 to HeaderParser.NP do
  begin
    OpenArray := false;
    S := HeaderParser.Types[I];
    TypeId := LookupType(S, L, true);
    if TypeId = 0 then
    begin
      P := Pos('ARRAY OF ', S);
      if P = 1 then
      begin
        OpenArray := true;

        Delete(S, 1, 9);
        if StrEql(S, 'CONST') then
          ElemTypeId := H_TVarRec
        else
          ElemTypeId := LookupType(S, L, true);

        if ElemTypeId = 0 then
        begin
          ExternList.Add(Card + 1,
                         S,
                         erPatternId);
        end;

        SubId := -1;
        for J:=Card downto 1 do
          if Records[J].Shift = result then
          begin
            SubId := J;
            Break;
          end;

        if SubId = -1 then
        begin
          RaiseError(errInternalError, []);
          Exit;
        end;

        TypeId := RegisterOpenArrayType(SubId, 'T' + S + 'Array', ElemTypeId);
      end
      else
      begin
        ExternList.Add(Card + 1,
                       S,
                       erTypeId);
      end;
    end;

    ParamMod := HeaderParser.Mods[I];

    Tag := 0;
    if HeaderParser.Optionals[I] then
      Tag := 1;

    if ParamMod in [pmByRef, pmOut] then
    begin
      RegisterParameter(result, TypeId, HeaderParser.Values[I], true, HeaderParser.Params[I], Tag);
      if ParamMod = pmOut then
        Records[Card].IsOut := true;
    end
    else if ParamMod = pmConst then
    begin
      RegisterParameter(result, TypeId, HeaderParser.Values[I], false, HeaderParser.Params[I], Tag);
      Records[Card].IsConst := true;
    end
    else
    begin
      RegisterParameter(result, TypeId, HeaderParser.Values[I], false, HeaderParser.Params[I], Tag);
    end;

    if OpenArray then
      Records[Card].IsOpenArray := true;

    if HeaderParser.Optionals[I] then
      Records[Card].DefVal := HeaderParser.DefVals[I];
  end;
end;

procedure TBaseSymbolTable.RegisterRunnerParameter(HSub: Integer);
var
  I, SubId: Integer;
  R: TSymbolRec;
begin
  SubId := -1;
  for I:=Card downto 1 do
  begin
    R := Records[I];

    if R.Kind = KindPROP then
    begin
      SubId := I;
      Break;
    end;

    if R.Kind in KindSUBS then
    if R.Shift = HSub then
    begin
      SubId := I;
      Break;
    end;
  end;

  if SubId = -1 then
  begin
    RaiseError(errInternalError, []);
    Exit;
  end;

  Records[SubId].RunnerParameter := true;
end;

function TBaseSymbolTable.RegisterParameter(HSub: Integer;
                                            const ParameterName: String;
                                            ParamTypeID: Integer;
                                            ParamMod: Integer = 0;
                                            Optional: Boolean = false;
                                            const DefaultValue: String = ''): Integer;
var
  pm: TParamMod;
  Value: Variant;
  Tag: Integer;
  OpenArray: Boolean;
  ParamTypeName: String;
  FT, IntVal, DoubleVal, Code1: Integer;
begin
  Tag := 0;

  pm := TParamMod(ParamMod);
  ParamTypeName := Records[ParamTypeId].Name;
  OpenArray := Pos('DYNARRAY_', ParamTypeName) = 1;

  if Optional then
  begin
    Tag := 1;
    FT := Records[ParamTypeId].FinalTypeId;
    if FT in IntegerTypes then
    begin
      Val(DefaultValue, IntVal, Code1);
      Value := IntVal;
    end
    else if FT in RealTypes then
    begin
      Val(DefaultValue, DoubleVal, Code1);
      Value := DoubleVal;
    end
    else
      Value := DefaultValue;
  end;

  if pm in [pmByRef, pmOut] then
  begin
    result := RegisterParameter(HSub, ParamTypeId, Value, true, ParameterName);
    if pm = pmOut then
      Records[Card].IsOut := true;
  end
  else if pm = pmConst then
  begin
    result := RegisterParameter(HSub, ParamTypeId, Value, false, ParameterName, Tag);
    Records[Card].IsConst := true;
  end
  else
  begin
    result := RegisterParameter(HSub, ParamTypeId, Value, false, ParameterName, Tag);
  end;

  if OpenArray then
    Records[Card].IsOpenArray := true;
end;

function TBaseSymbolTable.RegisterParameter(HSub: Integer;
                                            const ParameterName: String;
                                            const ParameterType: String;
                                            ParamMod: Integer = 0;
                                            Optional: Boolean = false;
                                            const DefaultValue: String = ''): Integer;
var
  ParamTypeId: Integer;
begin
  ParamTypeId := LookupType(ParameterType, true);
  result := RegisterParameter(Hsub, ParameterName, ParamTypeId, ParamMod, Optional,
    DefaultValue);
  if ParamTypeId = 0 then
    ExternList.Add(Card,
                   ParameterType,
                   erTypeId);
end;

function TBaseSymbolTable.RegisterParameter(HSub: Integer; ParamTypeID: Integer;
                                            const DefaultValue: Variant;
                                            InitByRef: Boolean = false;
                                            ParameterName: String = '';
                                            Tag: Integer = 0): Integer;
var
  I, SubId: Integer;
  R: TSymbolRec;
begin
  result := LastShiftValue;

  SubId := -1;
  for I:=Card downto 1 do
  begin
    R := Records[I];

    if R.Kind = KindPROP then
    begin
      SubId := I;
      Break;
    end;

    if R.Kind in KindSUBS then
    if R.Shift = HSub then
    begin
      SubId := I;
      Break;
    end;
  end;

  if SubId = -1 then
  begin
    RaiseError(errInternalError, []);
    Exit;
  end;

  Records[SubId].Count := Records[SubId].Count + 1;

  with AddRecord do
  begin
    Kind := KindVAR;
    TypeID := ParamTypeId;

    Host := true;
    Param := true;
    Shift := 0;
    Level := SubId;
    ByRef := InitByRef;
    Value := DefaultValue;
    Optional := VarType(Value) <> varEmpty;

    Name := ParameterName;

    if Tag = 1 then
      Optional := true;

    if InitByRef and Optional then
      RaiseError(errDefaultParameterMustBeByValueOrConst, [Name]);
  end;
end;

function TBaseSymbolTable.IsResultId(Id: Integer): Boolean;
var
  L: Integer;
begin
  result := false;
  if Id = 0 then
    Exit;
  L := Records[Id].Level;
  if L = 0 then
    Exit;
  if Records[L].Kind in KindSUBS then
    result := GetResultId(L) = Id;
end;

function TBaseSymbolTable.GetResultId(SubId: Integer): Integer;
begin
  result := SubId + 1;
end;

function TBaseSymbolTable.GetSelfId(SubId: Integer): Integer;
begin
  result := SubId + 2;
end;

function TBaseSymbolTable.GetDl_Id(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SubId + 3 to Card do
    if Records[I].Level = SubId then
    if Records[I].Name = '%DL' then
    begin
      result := I;
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TBaseSymbolTable.GetRBP_Id(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SubId + 3 to Card do
    if Records[I].Level = SubId then
    if Records[I].Name = '%RBP' then
    begin
      result := I;
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TBaseSymbolTable.GetRBX_Id(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SubId + 3 to Card do
    if Records[I].Level = SubId then
    if Records[I].Name = '%RBX' then
    begin
      result := I;
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TBaseSymbolTable.GetRDI_Id(SubId: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I:=SubId + 3 to Card do
    if Records[I].Level = SubId then
    if Records[I].Name = '%RDI' then
    begin
      result := I;
      Exit;
    end;
  RaiseError(errInternalError, []);
end;

function TBaseSymbolTable.GetParamId(SubId, ParamNumber: Integer): Integer;
var
  I, K, D: Integer;
  RI: TSymbolRec;
begin
  result := -1;
  K := -1;
  D := 3;
  if Records[SubId].Kind = kindPROP then
    D := 1;
  for I:=SubId + D to Card do
  begin
    RI := Records[I];
    if RI.Param and (RI.Level = SubId) then
    begin
      Inc(K);
      if K = ParamNumber then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
  RaiseError(errInvalidIndex, [ParamNumber]);
end;

function TBaseSymbolTable.GetRecord(I: Integer): TSymbolRec;
begin
  result := TSymbolRec(A[I]);
end;

function TBaseSymbolTable.AddRecord: TSymbolRec;
var
  AK: Integer;
begin
  AK := A.Count;

  if (Card = AK - 1) or (Card >= FirstLocalId) then
  begin
    Inc(Card);
    result := TSymbolRec.Create(Self);
    result.Id := Card;
    A.Add(result);
  end
  else
  begin
    Inc(Card);
    result := Records[Card];
  end;
end;

procedure TBaseSymbolTable.RemoveLastRecord;
var
  AK: Integer;
  R: TSymbolRec;
  S: String;
begin
  R := Records[Card];
  S := R.Name;
  if S <> '' then
    HashArray.DeleteName(S, R.Id);

  AK := A.Count;
  if (Card = AK - 1) or (Card >= FirstLocalId) then
  begin
{$IFDEF ARC}
    A[AK - 1] := nil;
{$ELSE}
    TObject(A[AK - 1]).Free;
{$ENDIF}
    A.Delete(AK - 1);
  end;
  Dec(Card);
end;

function TBaseSymbolTable.CreateEmptySet: TSymbolRec;
var
  T: Integer;
begin
  T := RegisterSetType(0, '$$', typeVOID);

  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := T;
  result.Value := 0;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := 32;
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddIntegerConst(Value: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeINTEGER;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddCardinalConst(Value: Cardinal): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeCARDINAL;
{$IFDEF VARIANTS}
  result.Value := Value;
{$ELSE}
  result.Value := Integer(Value);
{$ENDIF}
  result.Level := 0;
end;

function TBaseSymbolTable.AddSmallIntConst(Value: SmallInt): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeSMALLINT;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddShortIntConst(Value: ShortInt): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeSHORTINT;
  result.Value := Value;
  result.Level := 0;
end;

{$IFDEF VARIANTS}
function TBaseSymbolTable.AddInt64Const(Value: Int64): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeINT64;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Int64);
  Inc(LastShiftValue, result.FinSize);
end;
function TBaseSymbolTable.AddUInt64Const(Value: UInt64): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeUINT64;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(UInt64);
  Inc(LastShiftValue, result.FinSize);
end;
{$ELSE}
function TBaseSymbolTable.AddInt64Const(Value: Int64): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeINT64;
  result.Value := Integer(Value);
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Int64);
  Inc(LastShiftValue, result.FinSize);
end;
function TBaseSymbolTable.AddUInt64Const(Value: UInt64): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeUINT64;
  result.Value := Integer(Value);
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(UInt64);
  Inc(LastShiftValue, result.FinSize);
end;
{$ENDIF}

function TBaseSymbolTable.AddEnumConst(TypeId, Value: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddPointerConst(TypeId: Integer; Value: Pointer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
{$IFDEF VARIANTS}
  result.Value := Cardinal(Value);
{$ELSE}
  result.Value := Integer(Value);
{$ENDIF}
  result.Level := 0;
end;

function TBaseSymbolTable.AddRecordConst(TypeId: Integer; const Value: Variant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddArrayConst(TypeId: Integer; const Value: Variant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddSetConst(TypeId: Integer; const Value: Variant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Value;
  result.Level := 0;

//  result.Shift := LastShiftValue;
//  Records[TypeId].Completed := true;
//  Inc(LastShiftValue, Records[result.Id].Size);
end;

function TBaseSymbolTable.AddClassConst(TypeId: Integer; Value: TObject): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Integer(Value);
  result.Level := 0;
  result.MustBeAllocated := true;
end;

function TBaseSymbolTable.AddClassRefConst(TypeId: Integer; Value: TClass): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := TypeId;
  result.Value := Integer(Value);
  result.Level := 0;
  result.MustBeAllocated := true;
end;

function TBaseSymbolTable.AddSetVar(TypeId: Integer; const Value: Variant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := TypeId;
  result.Value := Value;
  result.Level := 0;

//  result.Shift := LastShiftValue;
//  Records[TypeId].Completed := true;
//  Inc(LastShiftValue, Records[result.Id].Size);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddAnsiCharConst(Value: AnsiChar): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeANSICHAR;
  result.Value := Ord(Value);
  result.Level := 0;
end;
{$ENDIF}

function TBaseSymbolTable.AddWideCharConst(Value: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeWIDECHAR;
  result.Value := Value;
  result.Level := 0;
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddPAnsiCharConst(const Value: AnsiString): TSymbolRec;
var
  SZ: Integer;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typePANSICHAR;
  result.Value := Value;
  result.Level := 0;

  SZ := 0;
  Inc(SZ, SizeOfPointer); // pointer to string literal
  Inc(SZ, SizeOfPointer); // ref counter
  Inc(SZ, SizeOfPointer); // length
  // reserve place for literal
  Inc(SZ, Length(Value) + 1);

  result.Shift := LastShiftValue;
  Inc(LastShiftValue, SZ);
end;

function TBaseSymbolTable.AddShortStringConst(const Value: String): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeSHORTSTRING;
  result.Value := Value;
  result.Level := 0;
end;
{$ENDIF}

{$IFDEF PAXARM}
function TBaseSymbolTable.AddPWideCharConst(const Value: String): TSymbolRec;
{$ELSE}
function TBaseSymbolTable.AddPWideCharConst(const Value: WideString): TSymbolRec;
{$ENDIF}
var
  SZ: Integer;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typePWIDECHAR;
  result.Value := Value;
  result.Level := 0;

  SZ := 0;
  Inc(SZ, SizeOfPointer); // pointer to string literal
  Inc(SZ, SizeOfPointer); // length

  // reserve place for literal
  Inc(SZ, Length(Value) * 2 + 2);

  result.Shift := LastShiftValue;
  Inc(LastShiftValue, SZ);
end;

function TBaseSymbolTable.AddByteConst(Value: Byte): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeBYTE;
  result.Value := Ord(Value);
  result.Level := 0;
end;

function TBaseSymbolTable.AddWordConst(Value: Word): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeWORD;
  result.Value := Ord(Value);
  result.Level := 0;
end;

function TBaseSymbolTable.AddBooleanConst(Value: Boolean): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeBOOLEAN;
  result.Value := Value;
  result.Level := 0;
end;

function TBaseSymbolTable.AddByteBoolConst(Value: ByteBool): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeBYTEBOOL;
  if Value then
    result.Value := $ff
  else
    result.Value := 0;
  result.Level := 0;
end;

function TBaseSymbolTable.AddWordBoolConst(Value: WordBool): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeWORDBOOL;
  if Value then
    result.Value := $ffff
  else
    result.Value := 0;
  result.Level := 0;
end;

function TBaseSymbolTable.AddLongBoolConst(Value: LongBool): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeLONGBOOL;
  if Value then
    result.Value := $ffffffff
  else
    result.Value := 0;
  result.Level := 0;
end;

function TBaseSymbolTable.AddDoubleConst(Value: Double): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeDOUBLE;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Double);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddCurrencyConst(Value: Double): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeCURRENCY;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Currency);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddSingleConst(Value: Single): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeSINGLE;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Single);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddExtendedConst(Value: Extended): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeEXTENDED;
  result.Value := Value;
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Extended);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddVariantConst(const Value: Variant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeVARIANT;
  result.SetVariantValue(Value);
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Variant);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddOleVariantConst(const Value: OleVariant): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindCONST;
  result.TypeId := typeOLEVARIANT;
  result.SetVariantValue(Value);
  result.Level := 0;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(OleVariant);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddTMethodVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := H_TMethod;
  result.Value := 0.0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOfTMethod;
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddDoubleVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeDOUBLE;
  result.Value := 0.0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Double);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddCurrencyVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeCURRENCY;
  result.Value := 0.0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Currency);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddSingleVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeSINGLE;
  result.Value := 0.0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Single);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddExtendedVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeEXTENDED;
  result.Value := 0.0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Extended);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddInt64Var(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeINT64;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Int64);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddUInt64Var(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeUINT64;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(UInt64);
  Inc(LastShiftValue, result.FinSize);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddStringVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeANSISTRING;
  result.Value := '';
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(String);
  Inc(LastShiftValue, result.FinSize);
end;
{$ENDIF}

function TBaseSymbolTable.AddInterfaceVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := H_IUnknown;
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(IUnknown);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddClassVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeCLASS;
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOfPointer;
  Inc(LastShiftValue, result.FinSize);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddWideStringVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeWIDESTRING;
  result.Value := '';
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(WideString);
  Inc(LastShiftValue, result.FinSize);
end;
{$ENDIF}

function TBaseSymbolTable.AddUnicStringVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeUNICSTRING;
  result.Value := '';
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(UnicString);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddVariantVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeVARIANT;
//  result.Value := '';
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Variant);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddOleVariantVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeOLEVARIANT;
//  result.Value := '';
  result.Level := Level;
  result.Name := '@';

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(OleVariant);
  Inc(LastShiftValue, result.FinSize);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddShortStringVar(Level, TypeId: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := TypeId;
  result.Value := '';
  result.Level := Level;
end;
{$ENDIF}

function TBaseSymbolTable.AddDynarrayVar(Level, TypeId: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := TypeId;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOfPointer;
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddRecordVar(Level, TypeId: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := TypeId;
  result.Level := Level;
end;

function TBaseSymbolTable.AddIntegerVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeINTEGER;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(LongInt);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddCardinalVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeCARDINAL;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Cardinal);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddSmallIntVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeSMALLINT;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(SmallInt);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddShortIntVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeSHORTINT;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(ShortInt);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddByteVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeBYTE;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Byte);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddWordVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeWORD;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Word);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddBooleanVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeBOOLEAN;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(Boolean);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddByteBoolVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeBYTEBOOL;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(ByteBool);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddWordBoolVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeWORDBOOL;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(WordBool);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddLongBoolVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeLONGBOOL;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(LongBool);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddPointerVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typePOINTER;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOfPointer;
  Inc(LastShiftValue, result.FinSize);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.AddAnsiCharVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeANSICHAR;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(AnsiChar);
  Inc(LastShiftValue, result.FinSize);
end;
{$ENDIF}

function TBaseSymbolTable.AddWideCharVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeWIDECHAR;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOf(WideChar);
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddVoidVar(Level: Integer; SZ: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeVOID;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SZ;
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddClassRefVar(Level: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindVAR;
  result.TypeId := typeCLASSREF;
  result.Value := 0;
  result.Level := Level;

  result.Shift := LastShiftValue;
  result.FinSize := SizeOfPointer;
  Inc(LastShiftValue, result.FinSize);
end;

function TBaseSymbolTable.AddLabel: TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindLABEL;
  result.Level := 0;
end;

function TBaseSymbolTable.AddPointerType(SourceTypeId: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Kind := kindTYPE;
  result.TypeId := typePOINTER;
  result.Level := Records[SourceTypeId].Level;
  result.PatternId := SourceTypeId;
end;

function TBaseSymbolTable.AddEndOfClassHeader(ClassId: Integer): TSymbolRec;
begin
  result := AddRecord;
  result.Name := '*' + Records[ClassId].Name;
  result.Kind := kindEND_CLASS_HEADER;
  result.Level := ClassId;
end;

function TBaseSymbolTable.LookupNamespace(const S: String;
                                          i_Level: Integer; UpCase: Boolean): Integer;
var
  I, J: Integer;
  ok: Boolean;
  List: TIntegerList;
  Q: TStringList;
  S2: String;
begin
  if PosCh('.', S) > 0 then
  begin
    Q := ExtractNames(S);
    try
      result := i_Level;
      for I := 0 to Q.Count - 1 do
      begin
        S2 := Q[I];
        if StrEql(S, 'System') then
          result := 0
        else
          result := LookupNamespace(S2, result, Upcase);
      end;
    finally
      FreeAndNil(Q);
    end;
    Exit;
  end;

  List := HashArray.GetList(S);

  result := 0;
  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    with Records[I] do
    if (Kind = KindNAMESPACE) and (Level = i_Level) then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupFullName(const S: String; UpCase: Boolean): Integer;
var
  I, J: Integer;
  ok: Boolean;
  RI: TSymbolRec;
  List: TIntegerList;
begin
  result := 0;

  if HashArray = nil then
  begin
    for I := Card downto FirstLocalId + 1 do
    begin
      RI := Records[I];
      begin
        if UpCase then
          ok := StrEql(RI.FullName, S)
        else
          ok := RI.FullName = S;
        if ok then
        begin
          result := I;
          Exit;
        end;
      end;
    end;
    Exit;
  end;

  List := HashArray.GetList(ExtractName(S));

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    RI := Records[I];
    begin
      if UpCase then
        ok := StrEql(RI.FullName, S)
      else
        ok := RI.FullName = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupFullNameEx(const S: String; UpCase: Boolean;
                                           OverCount: Integer): Integer;
var
  I, J: Integer;
  ok: Boolean;
  RI: TSymbolRec;
  List: TIntegerList;
begin
  if OverCount = 0 then
  begin
    result := LookupFullName(S, Upcase);
    Exit;
  end;

  result := 0;

  if HashArray = nil then
  begin
    for I:= Card downto FirstLocalId + 1 do
    begin
      RI := Records[I];
      begin
        if UpCase then
          ok := StrEql(RI.FullName, S)
        else
          ok := RI.FullName = S;
        if ok then
        if RI.OverCount = OverCount then
        begin
          result := I;
          Exit;
        end;
      end;
    end;
    Exit;
  end;

  List := HashArray.GetList(ExtractName(S));

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    RI := Records[I];
    begin
      if UpCase then
        ok := StrEql(RI.FullName, S)
      else
        ok := RI.FullName = S;
      if ok then
      if RI.OverCount = OverCount then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookUpType(const S: String;
    i_Level: Integer; UpCase: Boolean): Integer;
var
  I, J: Integer;
  ok: Boolean;
  List: TIntegerList;
begin
  if S = '' then
  begin
    result := 0;
    Exit;
  end;

{$IFDEF UNIC}
  if StrEql(S, 'Char') then
    result := typeWIDECHAR
  else if StrEql(S, 'String') then
    result := typeUNICSTRING
  else if StrEql(S, 'PChar') then
    result := typePWIDECHAR
  else
    result := Types.IndexOf(S);
{$ELSE}
  result := Types.IndexOf(S);
{$ENDIF}

  if result > 0 then
    Exit;

  List := HashArray.GetList(S);

  result := 0;

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    with Records[I] do
    if (Kind = KindTYPE) and (Level = i_Level) then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    with Records[I] do
    if Kind = KindTYPE then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookUpType(const S: String; UpCase: Boolean): Integer;
var
  I, J: Integer;
  ok: Boolean;
  List: TIntegerList;
begin
  if S = '' then
  begin
    result := 0;
    Exit;
  end;

{$IFDEF UNIC}
  if StrEql(S, 'Char') then
    result := typeWideCHAR
  else if StrEql(S, 'String') then
    result := typeUNICSTRING
  else if StrEql(S, 'PChar') then
    result := typePWIDECHAR
  else
    result := Types.IndexOf(S);
{$ELSE}
  result := Types.IndexOf(S);
{$ENDIF}

  if result > 0 then
    Exit;

  List := HashArray.GetList(S);

  result := 0;

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    with Records[I] do
    if Kind = KindTYPE then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookUpTypeEx(const S: String;
    i_Level: Integer; UpCase: Boolean; LowBound: Integer): Integer;
var
  I, J: Integer;
  ok: Boolean;
  List: TIntegerList;
begin
  if S = '' then
  begin
    result := 0;
    Exit;
  end;

  result := Types.IndexOf(S);
  if result > 0 then
    if result >= LowBound then
    Exit;

  List := HashArray.GetList(S);

  result := 0;

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    if I < LowBound then
      continue;

    with Records[I] do
    if (Kind = KindTYPE) and (Level = i_Level) then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];

    if I < LowBound then
      continue;

    with Records[I] do
    if Kind = KindTYPE then
    begin
      if UpCase then
        ok := StrEql(Name, S)
      else
        ok := Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupParentMethodBase(SubId: Integer;
                                                 UpCase: Boolean;
                                                 var BestId: Integer): Integer;
var
  I, J, Level, TempLevel: Integer;
  ok: Boolean;
  List: TIntegerList;
  R: TSymbolRec;
  Name, Signature: String;
begin
  Name := Records[SubId].Name;
  Signature := Records[SubId].SignatureBrief;
  Level := Records[SubId].Level;

  List := HashArray.GetList(Name);

  result := 0;
  BestId := 0;

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];

    R := Records[I];
    if R.Kind in KindSUBS then
    begin
      TempLevel := R.Level;

      if TempLevel = 0 then
        continue;
      if Level = TempLevel then
        continue;
      if Records[TempLevel].ClassIndex = -1 then
        continue;
      if not Inherits(Level, TempLevel) then
        continue;

      if UpCase then
        ok := StrEql(R.Name, Name)
      else
        ok := R.Name = Name;
      if ok then
      begin
        BestId := I;

        if UpCase then
          ok := StrEql(R.SignatureBrief, Signature)
        else
          ok := R.SignatureBrief = Signature;

        if ok then
        begin
          result := I;
          Exit;
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupParentMethod(SubId: Integer;
                                             UpCase: Boolean;
                                             HasMethodIndex: Boolean = false): Integer;
var
  I, J, Level: Integer;
  ok: Boolean;
  List: TIntegerList;
  R: TSymbolRec;
  Name, Signature: String;
  C, CA: TClass;
label
  Again;
begin
  Name := Records[SubId].Name;
  Signature := Records[SubId].SignatureBrief;

  List := HashArray.GetList(Name);
  C := Records[Records[SubId].Level].PClass;

  result := 0;
  Level := Records[SubId].Level;

Again:

  if C = nil then
    Exit;

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];
    R := Records[I];
    if R.Kind in KindSUBS then
    begin
      CA := Records[Records[I].Level].PClass;

      if CA = nil then
        continue;

      if C = CA then
        continue;

      if not C.InheritsFrom(CA) then
      begin
        if Records[Records[SubId].Level].AncestorId <>
           Records[I].Level then
        continue;
      end;

      if UpCase then
        ok := StrEql(R.Name, Name)
      else
        ok := R.Name = Name;
      if ok then
      begin

        if UpCase then
          ok := StrEql(R.SignatureBrief, Signature)
        else
          ok := R.SignatureBrief = Signature;

        if ok then
        begin
          if HasMethodIndex = false then
          begin
            result := I;
            Exit;
          end
          else
          if R.MethodIndex <> 0 then
          begin
            result := I;
            Exit;
          end;
        end;
      end;
    end;
  end;

  if Level > 0 then
    if Records[Level].AncestorId > 0 then
    begin
      Level := Records[Level].AncestorId;
      C := Records[Level].PClass;
      goto Again;
    end;
end;

function TBaseSymbolTable.LookupParentMethods(SubId: Integer; Upcase: Boolean): TIntegerList;
var
  Name, Signature: String;
  List: TIntegerList;
  I, J, L: Integer;
  R: TSymbolRec;
  b: Boolean;
begin
  Name := Records[SubId].Name;
  Signature := Records[SubId].SignatureBrief;
  L := Records[SubId].Level;

  result := TIntegerList.Create;

  List := HashArray.GetList(Name);
  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];
    R := Records[I];
    if R.Kind in KindSUBS then
    begin
      if I = SubId then
        continue;
      if R.Level > 0 then
      if Records[R.Level].Kind = KindTYPE then
      if Inherits(L, R.Level) then
      begin
        if Upcase then
          b := StrEql(R.Name, Name)
        else
          b := R.Name = Name;

        if not b then
          continue;

        if Upcase then
          b := StrEql(R.SignatureBrief, Signature)
        else
          b := R.SignatureBrief = Signature;

        if b then
        begin
          result.Add(I);
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupParentConstructor(SubId: Integer): Integer;
var
  I, L, KK, K1, K2, AncestorId: Integer;
  R: TSymbolRec;
  Signature: String;
begin
  result := 0;

  Signature := Records[SubId].Signature;

  L := Records[SubId].Level;
  AncestorId := Records[L].AncestorId;

  for KK := 2 downto 1 do
  begin
    if KK = 1 then
    begin
      K1 := 1;
      if Self.st_tag = 0 then
        K2 := Card
      else
        K2 := TLocalSymbolTable(Self).GlobalST.Card;
    end
    else
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end;

    for I := K2 downto K1 do
    begin
      R := Records[I];
      if R.Kind = KindCONSTRUCTOR then
      begin
        if I = SubId then
          continue;

        if R.Level > 0 then
        if Records[R.Level].Kind = KindTYPE then
        if AncestorId = R.Level then
        if StrEql(Records[I].Signature, Signature) then
        begin
          result := I;
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookupParentConstructors(SubId: Integer): TIntegerList;
var
  I, L, KK, K1, K2: Integer;
  R: TSymbolRec;
  Signature: String;
begin
  L := Records[SubId].Level;
  result := TIntegerList.Create;
  Signature := Records[SubId].Signature;

  for KK := 2 downto 1 do
  begin
    if KK = 1 then
    begin
      K1 := 1;
      if Self.st_tag = 0 then
        K2 := Card
      else
        K2 := TLocalSymbolTable(Self).GlobalST.Card;
    end
    else
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end;

    for I := K2 downto K1 do
    begin
      R := Records[I];
      if R.Kind = KindCONSTRUCTOR then
      begin
        if I = SubId then
          continue;

        if R.Level > 0 then
          if Records[R.Level].Kind = KindTYPE then
            if Inherits(L, R.Level) then
              if StrEql(Records[I].Signature, Signature) then
              begin
                result.Add(I);
                if Records[I].Host then
                  break;
               end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookUpEnumItem(const S: String; EnumTypeId: Integer;
                                         UpCase: Boolean): Integer;
var
  I: Integer;
  R: TSymbolRec;
  ok: Boolean;
begin
  result := 0;

  for I:=EnumTypeId + 1 to Card do
  begin
    R := Records[I];

    if R = SR0 then
     Exit;

    if R.Kind = KindCONST then
    if R.OwnerId = EnumTypeId then
    begin
     if UpCase then
        ok := StrEql(R.Name, S)
      else
        ok := R.Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;
end;

function TBaseSymbolTable.LookUp(const S: String; Level: Integer; UpCase: Boolean;
        UpperBoundId: Integer = MaxInt; recursive: Boolean = true): Integer;
var
  I, J: Integer;
  Ok: Boolean;
  InterfaceTypeId: Integer;
  R: TSymbolRec;
  List: TIntegerList;
begin
  List := HashArray.GetList(S);

  result := 0;

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];
    R := Records[I];

    if I < UpperBoundId then
    if (R.Level = Level) and (not R.InternalField) then
    if R.Kind <> KindNONE then
    begin
      if R.OwnerId > 0 then
        if Records[R.OwnerId].Kind <> KindTYPE then
          continue;

     if UpCase then
        ok := StrEql(R.Name, S)
      else
        ok := R.Name = S;
      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;

  if Recursive then
  if Level > 0 then
  begin
    if Records[Level].AncestorId > 0 then
    begin
      result := LookUp(S, Records[Level].AncestorId, Upcase);
      if result > 0 then
        Exit;
    end;
  end;

  if Assigned(Records[Level].SupportedInterfaces) then
  begin
    for I := 0 to Records[Level].SupportedInterfaces.Count - 1 do
    begin
      InterfaceTypeId := Records[Level].SupportedInterfaces[I].Id;
      result := LookUp(S, InterfaceTypeId, Upcase);
      if result > 0 then
        Exit;
    end;
  end;
end;

function TBaseSymbolTable.LookUpEx(var HelperTypeId: Integer; const S: String; Level: Integer; UpCase: Boolean;
        UpperBoundId: Integer = MaxInt; recursive: Boolean = true): Integer;
var
  L: TIntegerList;
  I: Integer;
begin
  HelperTypeId := 0;
  result := LookUp(S, Level, UpCase, UpperBoundId, recursive);
  if result > 0 then
    Exit;
  if Level = 0 then
    Exit;
  if Records[Level].Kind <> KindTYPE then
    Exit;
  if Records[Level].FinalTypeId = typeHELPER then
  begin
    I := Records[Level].PatternId;
    if I = 0 then
      Exit;
    result := LookUp(S, I, UpCase, UpperBoundId, recursive);
    if result > 0 then
      HelperTypeId := Level;
    Exit;
  end;
  L := GetTypeHelpers(Level);
  try
    for I := 0 to L.Count - 1 do
    begin
      result := LookUp(S, L[I], UpCase, UpperBoundId, recursive);
      if result > 0 then
      begin
        HelperTypeId := L[I];
        Exit;
      end;
    end;
  finally
    L.Free;
  end;
end;

function TBaseSymbolTable.LookUpAll(const S: String; Level: Integer; UpCase: Boolean): TIntegerList;
var
  I, J: Integer;
  Ok: Boolean;
  R: TSymbolRec;
  List: TIntegerList;
begin
  List := HashArray.GetList(S);

  result := TIntegerList.Create;
  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];

    R := Records[I];

    if (R.Level = Level) and (not R.InternalField) then
    if R.Kind <> KindNONE then
    if R.OwnerId = 0 then
    begin
      if UpCase then
        ok := StrEql(R.Name, S)
      else
        ok := R.Name = S;
      if ok then
        result.Insert(0, I);
    end;
  end;
end;

function TBaseSymbolTable.LookUpSub(const S: String; Level: Integer; UpCase: Boolean): TIntegerList;

  function HasAtLevel(const S: String; Level: Integer; UpCase: Boolean): Boolean;
  var
    I, J: Integer;
    Ok: Boolean;
    R: TSymbolRec;
    List: TIntegerList;
  begin
    List := HashArray.GetList(S);

    result := false;

    for J:=List.Count - 1 downto 0 do
    begin
      I := List[J];
      R := Records[I];
      if (R.Level = Level) and (R.Kind in kindSUBS) then
      begin
        if UpCase then
          ok := StrEql(R.Name, S)
        else
          ok := R.Name = S;
        if ok then
          begin
            result := true;
            Exit;
          end;
      end;
    end;
  end;

  procedure DoIt(const S: String; Level: Integer; UpCase: Boolean; Fin: Boolean = false);
  var
    I, J, I1, K1: Integer;
    Ok: Boolean;
    R: TSymbolRec;
    List: TIntegerList;
    Sig: String;
  begin
    List := HashArray.GetList(S);

    K1 := result.Count;

    for J:=List.Count - 1 downto 0 do
    begin
      I := List[J];

      R := Records[I];

      if (R.Level = Level) and (R.Kind in KindSUBS) then
      begin
        if UpCase then
          ok := StrEql(R.Name, S)
        else
          ok := R.Name = S;
        if ok then
        begin
          Sig := Records[I].Signature;
          for I1 := 0 to K1 - 1 do
            if Records[result[I1]].Signature = Sig then
            begin
              ok := false;
              break;
            end;
          if not ok then
            continue;

          result.Insert(0, I);
        end;
      end;
    end;

    if Fin then
      Exit;

    Level := Records[Level].AncestorId;
    while not ((Level = 0) or (Level = H_TObject) or (Level = JS_ObjectClassId)) do
    begin
      if HasAtLevel(S, Level, Upcase) then
      begin
        DoIt(S, Level, upcase, true);
        Exit;
      end;
      Level := Records[Level].AncestorId;
    end;
  end;

begin
  result := TIntegerList.Create;
  DoIt(S, Level, Upcase);
end;

function TBaseSymbolTable.LookUpSubs(const S: String; Level: Integer; UsingList: TIntegerList; UpCase: Boolean): TIntegerList;
var
  I, J, LevelId, SubId: Integer;
  temp: TIntegerList;
begin
  result := LookupSub(S, Level, Upcase);

  for I := 0 to UsingList.Count - 1 do
  begin
    LevelId := UsingList[I];
    temp := LookupSub(S, LevelId, upcase);
    for J := 0 to temp.Count - 1 do
    begin
      SubId := temp[J];
      if result.IndexOf(SubId) = -1 then
        result.Add(SubId);
    end;
    FreeAndNil(temp);
  end;
end;

function TBaseSymbolTable.LookUps(const S: String; LevelStack: TIntegerStack;
                                  UpCase: Boolean;
                                  UpperBoundId: Integer = MaxInt;
                                  Recursive: Boolean = true): Integer;
var
  I, R: Integer;
begin
  for I:=LevelStack.Count - 1 downto 0 do
  begin
    R := LookUp(S, LevelStack[I], upcase, UpperBoundId, Recursive);
    if R > 0 then
    begin
      result := R;
      Exit;
    end;
  end;
  result := 0;
end;

function TBaseSymbolTable.LookUpsEx(const S: String; LevelStack: TIntegerStack; var LevelId: Integer; UpCase: Boolean): Integer;
var
  I, R: Integer;
begin
  for I:=LevelStack.Count - 1 downto 0 do
  begin
    R := LookUp(S, LevelStack[I], upcase, MaxInt);
    if R > 0 then
    begin
      result := R;
      LevelId := LevelStack[I];
      Exit;
    end;
  end;
  result := 0;
end;

function TBaseSymbolTable.LookUpsExcept(const S: String; LevelStack: TIntegerStack; LevelId: Integer; UpCase: Boolean): Integer;
var
  I, R: Integer;
begin
  for I:=LevelStack.Count - 1 downto 0 do
  begin
    if LevelId = LevelStack[I] then
      continue;

    R := LookUp(S, LevelStack[I], upcase, MaxInt);
    if R > 0 then
    begin
      result := R;
      Exit;
    end;
  end;
  result := 0;
end;

function TBaseSymbolTable.LookupAnotherDeclaration(Id: Integer; UpCase: Boolean;
                                                   var BestID: Integer): Integer;
var
  I, J, Level: Integer;
  Name, SignatureEx: String;
  Ok: Boolean;
  RI: TSymbolRec;
  List: TIntegerList;
begin
  Level := Records[Id].Level;
  Name := Records[Id].Name;
  SignatureEx := Records[Id].SignatureEx;

  BestId := 0;

  List := HashArray.GetList(Name);

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];

    if I = Id then
      continue;

    RI := Records[I];
    if (RI.Level = Level) and
       (RI.Kind in kindSUBS) then
    begin
      if UpCase then
        ok := StrEql(RI.Name, Name)
      else
        ok := (RI.Name = Name);

      if ok then
      begin
        BestId := I;

        if UpCase then
          ok := StrEql(RI.SignatureEx, SignatureEx)
        else
          ok := RI.SignatureEx = SignatureEx;
      end;

      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;

  result := 0;
end;

function TBaseSymbolTable.LookupForwardDeclaration(Id: Integer; UpCase: Boolean;
                                                   var BestID: Integer): Integer;
var
  I, J, Level: Integer;
  Name, Sig: String;
  Ok: Boolean;
  RI: TSymbolRec;
  List: TIntegerList;
begin
  Level := Records[Id].Level;
  Name := Records[Id].Name;
  Sig := Records[Id].Sig;

  BestId := 0;

  List := HashArray.GetList(Name);

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    RI := Records[I];
    if RI.IsForward and (RI.Level = Level) and
      (RI.Kind in kindSUBS) then
    begin
      if UpCase then
        ok := StrEql(RI.Name, Name)
      else
        ok := (RI.Name = Name);

      if ok then
      begin
        BestId := I;

        if UpCase then
          ok := StrEql(RI.Sig, Sig)
        else
          ok := RI.Sig = Sig;
      end;

      if ok then
      begin
        result := I;
        Exit;
      end;
    end;
  end;

  result := 0;
end;

function TBaseSymbolTable.LookupForwardDeclarations(Id: Integer;
                                          UpCase: Boolean): TIntegerList;
var
  I, J, Level: Integer;
  Ok: Boolean;
  Name: String;
  R: TSymbolRec;
  List: TIntegerList;
begin
  result := nil;

  Level := Records[Id].Level;
  Name := Records[Id].Name;

  List := HashArray.GetList(Name);

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];

    R := Records[I];
    if R.IsForward and (R.Level = Level) and
      (R.Kind in KindSUBS) then
    begin
      if UpCase then
        ok := StrEql(R.Name, Name)
      else
        ok := (R.Name = Name);
      if ok then
      begin
        if result = nil then
          result := TIntegerList.Create;

        result.Add(I);
      end;
    end;
  end;
end;

function TBaseSymbolTable.GetDataSize(UpperId: Integer = MaxInt - 1): Integer;
var
  I: Integer;
  R: TSymbolRec;
  K1, K2, KK: Integer;
begin
  result := FirstShiftValue;

  Inc(UpperId);

  for KK := 1 to 2 do
  begin
    if KK = 1 then
    begin
      K1 := Types.Count;

      if Self.st_tag = 0 then
        K2 := Card
      else
        K2 := TLocalSymbolTable(Self).GlobalST.Card;
    end
    else
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end;

    for I := K1 to K2 do
    begin

      if I = UpperId then
        break;

      R := Records[I];

      if R.UnionId <> 0 then
        Continue;

      if R.Kind in [KindTYPE, KindTYPE_FIELD, KindLABEL] then
        Continue;

      if R.OverScript then
      begin
        Inc(result, SizeOfPointer);
        continue;
      end;

      if (R.Shift > 0) and
         (not R.Param) and (not R.Local) and (not R.InternalField) then
      begin
        if R.Kind = kindSUB then
          Inc(result, SizeOfPointer)
        else if R.Host then
          Inc(result, SizeOfPointer)
{$IFNDEF PAXARM}
        else if (R.Kind = KindCONST) and R.HasPAnsiCharType then // literal
        begin
          Inc(result, SizeOfPointer);
          Inc(result, SizeOfPointer);
          Inc(result, SizeOfPointer);
          Inc(result, Length(R.Value) + 1);
        end
{$ENDIF}
        else if (R.Kind = KindCONST) and R.HasPWideCharType then // literal
        begin
          Inc(result, SizeOfPointer);
          Inc(result, SizeOfPointer);
          Inc(result, Length(R.Value) * 2 + 2);
        end
        else
          Inc(result, R.Size);
      end;
    end;
  end;
end;

function TBaseSymbolTable.GetSizeOfLocals(SubId: Integer): Integer;
var
  I: Integer;
  R: TSymbolRec;
begin
  result := 0;
  for I := SubId + 1 to Card do
  begin
    R := Records[I];

    if R.UnionId <> 0 then
      Continue;

    if (R.Kind = KindVAR) and (R.Level = SubId) and R.Local then
    begin
      if R.FinalTypeId = typeSET then
        Inc(result, SizeOf(TByteSet))
      else
        Inc(result, MPtr(R.Size));
    end;
  end;

  result := Abs(result);

  if Records[SubId].ExtraParamNeeded then
  begin
    if Records[SubId].CallConv in [ccREGISTER, ccMSFASTCALL] then
    begin
      if Records[GetResultId(SubId)].Register = 0 then
        Dec(result, SizeOfPointer);
    end
    else
      Dec(result, SizeOfPointer);
  end;

end;

function TBaseSymbolTable.GetSizeOfLocalsEx(SubId: Integer): Integer;
begin
  result := GetSizeOfLocals(SubId);

  while (result mod 32 <> 0) do
    Inc(result);
end;

function TBaseSymbolTable.GetSubRSPSize(SubId: Integer): Integer;
begin
  result := GetSizeOfLocalsEx(SubId);
  Inc(result, $150);
  while result mod 32 <> 0 do
    Inc(result);
  Inc(result, 8);
end;

function TBaseSymbolTable.GetSizeOfSetType(SetTypeId: Integer): Integer;
var
  FT, OriginTypeId: Integer;
  B2: TSymbolRec;
  I: Cardinal;
begin
  OriginTypeId := Records[SetTypeId].PatternId;
  if OriginTypeId <= 1 then
  begin
    result := 32;
    Exit;
  end;

  FT := Records[OriginTypeId].FinalTypeId;
{
  if FT = typeENUM then
  begin
    result := 1;
    Exit;
  end;
}
  if not (FT in OrdinalTypes) then
  begin
    result := 32;
    Exit;
  end;

  B2 := GetHighBoundRec(OriginTypeId);
  I := B2.Value;
  if I < 8 then
    result := 1
  else if I < 16 then
    result := 2
  else if I < 32 then
    result := 4
  else if I < 64 then
    result := 8
  else if I < 128 then
    result := 16
  else
    result := 32;
end;

function TBaseSymbolTable.CheckSetTypes(T1, T2: Integer): Boolean;
var
  P1, P2, F1, F2: Integer;
begin
  result := true;

  if T2 = H_TByteSet then
    Exit;

  P1 := Records[T1].PatternId;
  P2 := Records[T2].PatternId;

  if (P1 > 1) and (P2 > 1) then
  begin
    F1 := Records[P1].FinalTypeId;
    F2 := Records[P2].FinalTypeId;

    if F1 <> F2 then
    begin
      if (F1 in IntegerTypes) and (F2 in IntegerTypes) then
      begin
        // ok
      end
      else
        result := false;
    end
    else if (F1 = typeENUM) and (F2 = typeENUM)then
    begin
      if P1 <> P2 then
        if Records[T2].Name <> '$$' then
          result := false;
    end;
  end;
end;

function TBaseSymbolTable.GetLowBoundRec(TypeID: Integer): TSymbolRec;
var
  I: Integer;
  RI: TSymbolRec;
begin
  result := nil;

  if Records[TypeID].Kind <> kindTYPE then
  begin
    RaiseError(errInternalError, []);
    Exit;
  end;

  if Records[TypeID].TypeID = typeALIAS then
    TypeID := Records[TypeID].TerminalTypeId;

  case Records[TypeID].FinalTypeId of
    typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
    begin
      result := Records[FalseId];
      Exit;
    end;
    typeINTEGER,
{$IFNDEF PAXARM}
    typeANSICHAR,
{$ENDIF}
    typeBYTE, typeWORD, typeCARDINAL,
    typeSMALLINT, typeSHORTINT, typeWIDECHAR,
    typeINT64:
    begin
      for I:=TypeID + 1 to Card do
      begin
        RI := Records[I];

        if RI = SR0 then
          break;

        if RI.Level = TypeID then
        begin
          result := RI;
          Exit;
        end;
      end;
    end;
    typeENUM:
    begin
      if Records[TypeID].IsSubrangeEnumType then
      begin
        result := Records[TypeID + 1];
        Exit;
      end;

      for I:=TypeID + 1 to Card do
      begin
        RI := Records[I];

        if RI = SR0 then
          break;

        if RI.OwnerId = TypeID then
        begin
          result := RI;
          Exit;
        end;
      end;
    end;
  else
    begin
      RaiseError(errInternalError, []);
      Exit;
    end;
  end;

  RaiseError(errInternalError, []);
end;

function TBaseSymbolTable.GetHighBoundRec(TypeID: Integer): TSymbolRec;
var
  I, J: Integer;
  RI: TSymbolRec;
begin
  result := nil;

  if Records[TypeID].TypeID = typeALIAS then
    TypeID := Records[TypeID].TerminalTypeId;

  case Records[TypeID].FinalTypeId of
    typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
    begin
      result := Records[TrueId];
      Exit;
    end;
    typeINTEGER,
{$IFNDEF PAXARM}
    typeANSICHAR,
{$ENDIF}
    typeBYTE, typeWORD, typeCARDINAL,
    typeSMALLINT, typeSHORTINT, typeWIDECHAR,
    typeINT64:
    begin
      for I:=GetLowBoundRec(TypeID).Id + 1 to Card do
      begin
        RI := Records[I];
        if RI = SR0 then
          break;

        if RI.Level = TypeID then
        begin
          result := RI;
          Exit;
        end;
      end;
    end;
    typeENUM:
    begin
      if Records[TypeID].IsSubrangeEnumType then
      begin
        result := Records[TypeID + 2];
        Exit;
      end;

      J := Records[TypeID].Count;
      for I:=TypeID + 1 to Card do
      begin
        RI := Records[I];

        if RI = SR0 then
          break;

        if RI.Kind = KindCONST then
        if RI.OwnerId = TypeID then
        begin
          result := RI;
          Dec(J);
          if J = 0 then Break;
        end;
      end;
      Exit;
    end;
  else
    begin
      RaiseError(errInternalError, []);
      Exit;
    end;
  end;

  RaiseError(errInternalError, []);
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.IsZeroBasedAnsiCharArray(Id: Integer): Boolean;
var
  ArrayTypeId, RangeTypeId, ElemTypeId: Integer;
begin
  if Records[Id].FinalTypeId <> typeARRAY then
  begin
    result := false;
    Exit;
  end;

  ArrayTypeId := Records[Id].TerminalTypeId;

  GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
  result := (Records[ElemTypeId].FinalTypeId = typeANSICHAR) and
            (GetLowBoundRec(RangeTypeId).Value = 0);
end;
{$ENDIF}

function TBaseSymbolTable.IsZeroBasedWideCharArray(Id: Integer): Boolean;
var
  ArrayTypeId, RangeTypeId, ElemTypeId: Integer;
begin
  if Records[Id].FinalTypeId <> typeARRAY then
  begin
    result := false;
    Exit;
  end;

  ArrayTypeId := Records[Id].TerminalTypeId;

  GetArrayTypeInfo(ArrayTypeId, RangeTypeId, ElemTypeId);
  result := (Records[ElemTypeId].FinalTypeId = typeWIDECHAR) and
            (GetLowBoundRec(RangeTypeId).Value = 0);
end;

procedure TBaseSymbolTable.GetArrayTypeInfo(ArrayTypeId: Integer; var RangeTypeId: Integer;
                                            var ElemTypeId: Integer);
var
  I, K: Integer;
begin
  if Records[ArrayTypeID].TypeID = typeALIAS then
    ArrayTypeID := Records[ArrayTypeID].TerminalTypeId;

  if Records[ArrayTypeID].ReadId <> 0 then
  begin
    RangeTypeId := Records[ArrayTypeID].ReadId;
    ElemTypeId := Records[ArrayTypeID].WriteId;
    Exit;
  end;

  ElemTypeId := 0;
  RangeTypeId := 0;

  K := Card;

  if Self.st_tag > 0 then
  if ArrayTypeId < TLocalSymbolTable(Self).GlobalST.Card then
  begin
    K := TLocalSymbolTable(Self).GlobalST.Card;
  end;

  for I:=ArrayTypeId + 1 to K do
    if Records[I].Level = ArrayTypeId then
    if Records[I].Kind = KindTYPE then
    begin
      RangeTypeId := I;
      break;
    end;

  for I:=K downto ArrayTypeId + 1 do
    if Records[I].Level = ArrayTypeId then
    if Records[I].Kind = KindTYPE then
    begin
      ElemTypeId := I;
      break;
    end;

  if (RangeTypeId = 0) or (ElemTypeId = 0) then
  Begin
    RaiseError(errInternalError, []);
    Exit;
  end;

  if Records[RangeTypeId].TypeID = typeALIAS then
    RangeTypeId := Records[RangeTypeId].TerminalTypeId;

  if Records[ElemTypeId].TypeID = typeALIAS then
    ElemTypeId := Records[ElemTypeId].TerminalTypeId;

  Records[ArrayTypeID].ReadId := RangeTypeId;
  Records[ArrayTypeID].WriteId := ElemTypeId;
end;

function TBaseSymbolTable.GetTypeBase(TypeId: Integer): Integer;
begin
  result := Records[TypeId].PatternId;
end;

function TBaseSymbolTable.GetPatternSubId(ProcTypeID: Integer): Integer;
begin
  result := Records[ProcTypeID].PatternId;
end;

function TBaseSymbolTable.EqualHeaders(SubId1, SubId2: Integer): Boolean;

  function CompareTypes(T1, T2: Integer): Boolean;
  var
    F1, F2: Integer;
  begin
    result := false;
    F1 := Records[T1].FinalTypeId;
    F2 := Records[T2].FinalTypeId;
    if F1 <> F2 then
      Exit;
    if F1 = typeDYNARRAY then
    begin
      T1 := Records[T1].TerminalTypeId;
      T2 := Records[T2].TerminalTypeId;
      T1 := Records[T1].PatternId;
      T2 := Records[T2].PatternId;
      result := CompareTypes(T1, T2);
      Exit;
    end;
    result := true;
  end;

var
  I: Integer;
begin
  result := false;
  if not CompareTypes(Records[SubId1].TypeId, Records[SubId2].TypeId) then
    Exit;
  if Records[SubId1].Count <> Records[SubId2].Count then
    Exit;
  for I:=0 to Records[SubId1].Count - 1 do
    if not CompareTypes(Records[GetParamId(SubId1, I)].TypeID, Records[GetParamId(SubId2, I)].TypeID) then
      Exit;
  result := true;
end;

procedure TBaseSymbolTable.CheckError(B: Boolean);
begin
  if B then
    RaiseError(errInternalError, []);
end;

procedure TBaseSymbolTable.RaiseError(const Message: string; params: array of Const);
var
  I: Integer;
begin
  if RaiseE then
  begin
    raise Exception.Create(Format(Message, params));
  end
  else
  begin
    if LastCard > 0 then
      for I:= LastCard + 1 to Card do
        Records[I].Name := '';

    REG_ERROR := Format(Message, params);
    REG_OK := false;

    if Message = errUndeclaredIdentifier then
      REG_ERROR := '';
  end;
end;

function TBaseSymbolTable.GetShiftsOfDynamicFields(ATypeId: Integer): TIntegerList;

procedure GetArrayShifts(TypeID: Integer; S: Integer); forward;

procedure GetRecordShifts(TypeID: Integer; S: Integer);
var
  I, T, T1: Integer;
  RI: TSymbolRec;
begin
  for I:=TypeId + 1 to Card do
  begin
    RI := Records[I];

    if RI = SR0 then
      break;

    if (RI.Kind = KindTYPE_FIELD) and (RI.Level = TypeId) then
    begin
      T := RI.FinalTypeId;
      case T of
{$IFNDEF PAXARM}
        typeANSISTRING, typeWIDESTRING,
{$ENDIF}
        typeUNICSTRING, typeDYNARRAY, typeVARIANT, typeOLEVARIANT:
          result.Add(RI.Shift + S);
        typeCLASS:
          result.Add(RI.Shift + S);
        typeRECORD:
        begin
          T1 := TerminalTypeOf(RI.TypeID);
          GetRecordShifts(T1, RI.Shift);
        end;
        typeARRAY:
        begin
          T1 := TerminalTypeOf(RI.TypeID);
          GetArrayShifts(T1, RI.Shift);
        end;
      end;
    end;
  end;
end;

procedure GetArrayShifts(TypeID: Integer; S: Integer);
var
  RangeTypeId, ElemTypeId, H1, H2, T, I, ElemSize, P: Integer;
begin
  GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
  H1 := GetLowBoundRec(RangeTypeId).Value;
  H2 := GetHighBoundRec(RangeTypeId).Value;

  ElemSize := Records[ElemTypeId].Size;

  T := Records[ElemTypeId].FinalTypeId;

  case T of
{$IFNDEF PAXARM}
    typeANSISTRING, typeWIDESTRING,
{$ENDIF}
    typeUNICSTRING, typeDYNARRAY, typeVARIANT, typeOLEVARIANT:
    begin
      P := S;
      for I:=0 to H2 - H1 do
      begin
        result.Add(P);
        Inc(P, SizeOfPointer);
      end;
    end;
    typeCLASS:
    begin
      P := S;
      for I:=0 to H2 - H1 do
      begin
        result.Add(P);
        Inc(P, SizeOfPointer);
      end;
    end;
    typeRECORD:
    begin
      P := S;
      for I:=0 to H2 - H1 do
      begin
        TypeID := TerminalTypeOf(ElemTypeId);
        GetRecordShifts(TypeId, P);
        Inc(P, ElemSize);
      end;
    end;
    typeARRAY:
    begin
      P := S;
      for I:=0 to H2 - H1 do
      begin
        TypeID := TerminalTypeOf(ElemTypeId);
        GetArrayShifts(TypeId, P);
        Inc(P, ElemSize);
      end;
    end;
  end;
end;

var
  T: Integer;
begin
  result := TIntegerList.Create;
  T := Records[ATypeId].FinalTypeId;
  case T of
    typeRECORD: GetRecordShifts(ATypeId, 0);
    typeARRAY: GetArrayShifts(ATypeId, 0);
  end;
end;

function TBaseSymbolTable.GetTypesOfDynamicFields(ATypeId: Integer): TIntegerList;

  procedure GetArrayTypes(TypeID: Integer); forward;

  procedure GetRecordTypes(TypeID: Integer);
  var
    I, T, T1: Integer;
    RI: TSymbolRec;
  begin
    for I:=TypeId + 1 to Card do
    begin
      RI := Records[I];

      if RI = SR0 then
        break;

      if (RI.Kind = KindTYPE_FIELD) and (RI.Level = TypeId) then
      begin
        T := RI.FinalTypeId;
        case T of
{$IFNDEF PAXARM}
          typeANSISTRING, typeWIDESTRING,
{$ENDIF}
          typeUNICSTRING, typeDYNARRAY, typeVARIANT, typeOLEVARIANT:
            result.Add(RI.TerminalTypeId);
          typeCLASS:
            result.Add(RI.TerminalTypeId);
          typeRECORD:
          begin
            T1 := TerminalTypeOf(RI.TypeID);
            GetRecordTypes(T1);
          end;
          typeARRAY:
          begin
            T1 := TerminalTypeOf(RI.TypeID);
            GetArrayTypes(T1);
          end;
        end;
      end;
    end;
  end;

  procedure GetArrayTypes(TypeID: Integer);
  var
    RangeTypeId, ElemTypeId, H1, H2, T, I: Integer;
  begin
    GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
    H1 := GetLowBoundRec(RangeTypeId).Value;
    H2 := GetHighBoundRec(RangeTypeId).Value;

    T := Records[ElemTypeId].FinalTypeId;

    case T of
{$IFNDEF PAXARM}
      typeANSISTRING, typeWIDESTRING,
{$ENDIF}
      typeUNICSTRING, typeDYNARRAY, typeVARIANT, typeOLEVARIANT:
        for I:=0 to H2 - H1 do
          result.Add(Records[ElemTypeId].TerminalTypeId);
      typeRECORD:
      begin
        for I:=0 to H2 - H1 do
        begin
          TypeID := TerminalTypeOf(ElemTypeId);
          GetRecordTypes(TypeId);
        end;
      end;
      typeCLASS:
      begin
        for I:=0 to H2 - H1 do
          result.Add(Records[ElemTypeId].TerminalTypeId);
      end;
      typeARRAY:
        for I:=0 to H2 - H1 do
        begin
          TypeID := TerminalTypeOf(ElemTypeId);
          GetArrayTypes(TypeId);
        end;
    end;
  end;
begin
  result := TIntegerList.Create;
  case Records[ATypeId].FinalTypeId of
    typeRECORD: GetRecordTypes(ATypeId);
    typeARRAY: GetArrayTypes(ATypeId);
  end;
end;

function TBaseSymbolTable.HasDynamicFields(ATypeId: Integer): Boolean;
var
  L: TIntegerList;
begin
  L := GetShiftsOfDynamicFields(ATypeID);
  result := L.Count > 0;
  FreeAndNil(L);
end;

function TBaseSymbolTable.TerminalTypeOf(TypeID: Integer): Integer;
begin
  result := TypeID;
  if Records[result].TypeID = typeALIAS then
    result := Records[result].TerminalTypeId;
end;

function TBaseSymbolTable.FindDefaultPropertyId(i_TypeId: Integer): Integer;
var
  I: Integer;
  RI: TSymbolRec;
begin
  for I:=i_TypeId + 1 to Card do
  begin
    RI := Records[I];

    if RI = SR0 then
      break;

    if RI.Kind = KindNAMESPACE then
      break;

    with RI do
    if (Kind = kindPROP) and (Level = i_TypeId) and IsDefault then
    begin
      result := I;
      Exit;
    end;
  end;

  if Records[i_TypeId].AncestorId > 0 then
    result := FindDefaultPropertyId(Records[i_TypeId].AncestorId)
  else
    result := 0;
end;

function TBaseSymbolTable.FindConstructorId(i_TypeId: Integer): Integer;
var
  I, temp: Integer;
  RI: TSymbolRec;
begin
  temp := 0;

  for I:=i_TypeId + 1 to Card do
  begin
    RI := Records[I];

    if RI = SR0 then
      break;
    if RI.Kind = KindNAMESPACE then
      break;

    with RI do
    if (Kind = kindCONSTRUCTOR) and (Level = i_TypeId) then
    begin
      if StrEql(RI.Name, 'Create') then
      begin
        result := I;
        Exit;
      end
      else
        temp := I;
    end;
  end;

  result := temp;

  if result = 0 then
    if Records[i_TypeId].Host then
    begin
      if i_TypeId = H_TObject then
        Exit;

      i_TypeId := Records[i_TypeId].AncestorId;

      if I_typeId = 0 then
        Exit;

      result := FindConstructorId(i_TypeId);
    end;
end;

function TBaseSymbolTable.FindConstructorIdEx(i_TypeId: Integer): Integer;
begin
  result := FindConstructorId(I_TypeId);

  if result = 0 then
    if Records[i_TypeId].AncestorId <> 0 then
      result := FindConstructorIdEx(Records[i_TypeId].AncestorId);
end;

function TBaseSymbolTable.FindConstructorIds(i_TypeId: Integer): TIntegerList;
var
  I: Integer;
  RI: TSymbolRec;
begin
  result := TIntegerList.Create;

  for I:=i_TypeId + 1 to Card do
  begin
    RI := Records[I];

    if RI = SR0 then
      break;
    if RI.Kind = KindNAMESPACE then
      break;

    with RI do
    if (Kind = kindCONSTRUCTOR) and (Level = i_TypeId) then
    begin
      result.Add(I);
    end;
  end;
end;

function TBaseSymbolTable.FindDestructorId(i_TypeId: Integer): Integer;
var
  I: Integer;
  RI: TSymbolRec;
begin
  for I:=i_TypeId + 1 to Card do
  begin
    RI := Records[I];

    if RI = SR0 then
      break;

    with RI do
    if (Kind = kindDESTRUCTOR) and (Level = i_TypeId) then
    begin
      result := I;
      Exit;
    end;
  end;
  result := 0;
end;

function TBaseSymbolTable.FindDestructorIdEx(i_TypeId: Integer): Integer;
begin

  result := FindDestructorId(i_TypeId);
  if result = 0 then
    if Records[i_TypeId].AncestorId <> 0 then
      result := FindDestructorIdEx(Records[i_TypeId].AncestorId);
end;

function TBaseSymbolTable.Inherits(T1, T2: Integer): Boolean;
begin
  T1 := Records[T1].TerminalTypeId;
  T2 := Records[T2].TerminalTypeId;

  result := (T1 = T2);
  if not result then
    result := Records[T1].Inherits(T2);
end;

function TBaseSymbolTable.Supports(T1, T2: Integer): Boolean;
var
  I: Integer;
  GuidList: TGuidList;
begin
  T1 := Records[T1].TerminalTypeId;
  T2 := Records[T2].TerminalTypeId;

  result := (T1 = T2);

  if result then
    Exit;

  if T2 = H_IUnknown then
  begin
    result := true;
    Exit;
  end;

  GuidList := Records[T1].SupportedInterfaces;

  if GuidList = nil then
  begin
    result := false;
    Exit;
  end;

  if GuidList.HasId(T2) then
  begin
    result := true;
    Exit;
  end;

  for I:=0 to GuidList.Count - 1 do
    if Supports(GuidList[I].Id, T2) then
    begin
      result := true;
      Exit;
    end;
end;

function TBaseSymbolTable.RegisterDummyType(LevelId: Integer;
                                            const TypeName: String): Integer;
begin
  result := LookupType(TypeName, LevelId, true);
  if result > 0 then
    Exit;

  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeVOID;
    Host := true;
    Shift := 0;
    Level := LevelId;
    IsDummyType := true;

    result := Id;
  end;
end;

function TBaseSymbolTable.RegisterSomeType(LevelId: Integer;
                                           const TypeName: String): Integer;
begin
  with AddRecord do
  begin
    Name := TypeName;
    Kind := KindTYPE;
    TypeID := typeVOID;
    Host := true;
    Shift := 0;
    Level := LevelId;

    result := Id;
  end;

  SomeTypeList.Add(TypeName, result);
end;

function TBaseSymbolTable.GetLocalCount(SubId: Integer): Integer;
var
  I, SelfId: Integer;
  RI: TSymbolRec;
begin
  result := 0;
  SelfId := GetSelfId(SubId);
  for I:=SubId + 1 to Card do
  begin
    RI := Self[I];
    if RI.Level = SubId then
      if RI.Kind = KindVAR then
        if RI.OwnerId = 0 then
          if RI.PatternId = 0 then
          if RI.Local then
          begin
            if RI.Name <> '' then
              if RI.Name <> '@' then
                Inc(result);
          end
          else if I = SelfId then
          begin
            if RI.Name <> '' then
              Inc(result);
          end;
    if RI.Kind = kindNAMESPACE then
      break;
  end;
end;

function TBaseSymbolTable.IsLocalOf(Id, SubId: Integer): Boolean;
var
  RI: TSymbolRec;
begin
  result := false;
  RI := Records[Id];

  if RI.Param then
    Exit;

  if RI.Level = SubId then
    if RI.Kind = KindVAR then
      if RI.OwnerId = 0 then
        if RI.PatternId = 0 then
        if RI.Local then
        begin
          if RI.Name <> '' then
            if RI.Name <> '@' then
              result := true;
        end
        else if Id = GetSelfId(SubId) then
        begin
          if RI.Name <> '' then
            result := true;
        end;
end;

function TBaseSymbolTable.GetLocalId(SubId, LocalVarNumber: Integer): Integer;
var
  I, K, SelfId: Integer;
  RI: TSymbolRec;
begin
  K := -1;
  SelfId := GetSelfId(SubId);
  for I:=SubId + 1 to Card do
  begin
    RI := Self[I];
    if RI.Level = SubId then
      if RI.Kind = KindVAR then
        if RI.OwnerId = 0 then
          if RI.PatternId = 0 then
          if RI.Local then
          begin
            if RI.Name <> '' then
              if RI.Name <> '@' then
              begin
                Inc(K);
                if K = LocalVarNumber then
                begin
                  result := I;
                  Exit;
                end;
              end;
          end
          else if I = SelfId then
          begin
            if RI.Name <> '' then
            begin
              Inc(K);
              if K = LocalVarNumber then
              begin
                result := I;
                Exit;
              end;
            end;
          end;
  end;

  result := 0;
  RaiseError(errInvalidIndex, [LocalVarNumber]);
end;

function TBaseSymbolTable.IsParam(SubId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Level = SubId) and
             R.Param and
            (GetSelfId(SubId) <> Id);
end;

function TBaseSymbolTable.IsVar(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  if Self[LevelId].Kind = KindSUB then
    if GetSelfId(LevelId) = Id then
    begin
      result := (Self[Id].Name <> '') and (not Self[Id].Param);
      Exit;
    end;

  result := false;

  R := Self[Id];
  if R.Param then
    Exit;
  if R.TypedConst then
    Exit;

  if R.Level = LevelId then
    if R.Kind = KindVAR then
      if R.OwnerId = 0 then
        if R.Name <> '' then
          if R.Name <> '@' then
            result := true;
end;

function TBaseSymbolTable.IsConst(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := ((R.Kind = KindCONST) and (R.Level = LevelId) and
            (R.Name <> ''))
            or
            R.TypedConst;
end;

function TBaseSymbolTable.IsType(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindTYPE) and (R.Level = LevelId) and
            (R.Name <> '');
end;

function TBaseSymbolTable.IsNamespace(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindNAMESPACE) and (R.Level = LevelId);
end;

function TBaseSymbolTable.IsTypeField(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindTYPE_FIELD) and (R.Level = LevelId);
end;

function TBaseSymbolTable.IsEnumMember(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindCONST) and (R.OwnerId = LevelId);
end;

function TBaseSymbolTable.IsProperty(ClassId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindPROP) and (R.Level = ClassId);
end;

function TBaseSymbolTable.IsProcedure(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindSUB) and (R.Level = LevelId) and
            (R.FinalTypeId = typeVOID);
  if result then
    result := Self[Id].Name <> '';
end;

function TBaseSymbolTable.IsFunction(LevelId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindSUB) and (R.Level = LevelId) and
            (R.FinalTypeId <> typeVOID);
  if result then
    result := Self[Id].Name <> '';
end;

function TBaseSymbolTable.IsConstructor(ClassId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindCONSTRUCTOR) and
            (R.Level = ClassId);
end;

function TBaseSymbolTable.IsDestructor(ClassId, Id: Integer): Boolean;
var
  R: TSymbolRec;
begin
  R := Self[Id];
  result := (R.Kind = KindDESTRUCTOR) and
            (R.Level = ClassId);
end;

function TBaseSymbolTable.GetGlobalCount(NamespaceId: Integer): Integer;
var
  I: Integer;
  RI: TSymbolRec;
begin
  result := 0;
  for I:=NamespaceId + 1 to Card do
  begin
    RI := Self[I];
    if RI.Host then
      continue;
    if RI.Level = NamespaceId then
      if RI.OwnerId = 0 then
        if RI.IsGlobalVar then
          if RI.Name <> '' then
            if RI.Name <> '@' then
              Inc(result);
  end;
end;

function TBaseSymbolTable.GetGlobalId(NamespaceId, GlobalVarNumber: Integer): Integer;
var
  I, K: Integer;
  RI: TSymbolRec;
begin
  K := -1;
  for I:=NamespaceId + 1 to Card do
  begin
    RI := Self[I];
    if RI.Host then
      continue;
    if RI.Level = NamespaceId then
      if RI.OwnerId = 0 then
        if RI.IsGlobalVar then
          if RI.Name <> '' then
            if RI.Name <> '@' then
            begin
              Inc(K);
              if K = GlobalVarNumber then
              begin
                result := I;
                Exit;
              end;
            end;
  end;
  result := 0;
  RaiseError(errInvalidIndex, [GlobalVarNumber]);
end;

function TBaseSymbolTable.GetFieldCount(Id: Integer; TypeMapRec: TTypeMapRec = nil): Integer;
var
  T, FinTypeId, I: Integer;
  R: TSymbolRec;
begin
  result := 0;
  if Id = 0 then
    Exit;
  FinTypeId := Self[Id].FinalTypeId;
  if FinTypeId = typeCLASS then
  begin
    if TypeMapRec <> nil then
      if TypeMapRec.Completed then
      begin
        result := TypeMapRec.Fields.Count;
        Exit;
      end;

    T := Self[Id].TerminalTypeId;
    for I:=T + 1 to Card do
    begin
      R := Self[I];
      if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
      begin
        Inc(result);
        if TypeMapRec <> nil then
          TypeMapRec.Fields.Add(I);
      end
      else if R.Kind = kindNAMESPACE then
        break;
    end;
    Inc(result, GetFieldCount(Self[T].AncestorId, TypeMapRec));
  end
  else if FinTypeId = typeRECORD then
  begin
    // Added by Oberon
    if TypeMapRec <> nil then
      if TypeMapRec.Completed then
      begin
        result := TypeMapRec.Fields.Count;
        Exit;
      end;
    //   end oberon

    T := Self[Id].TerminalTypeId;
    for I:=T + 1 to Card do
    begin
      R := Self[I];
      if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
      begin
        Inc(result);
        if TypeMapRec <> nil then
          TypeMapRec.Fields.Add(I);
      end
      else if R.Kind = kindNAMESPACE then
        break;
    end;
  end;
end;

function TBaseSymbolTable.GetPublishedPropCount(Id: Integer): Integer;
var
  T, FinTypeId, I: Integer;
  R: TSymbolRec;
begin
  result := 0;
  if Id = 0 then
    Exit;
  FinTypeId := Self[Id].FinalTypeId;
  if FinTypeId = typeCLASS then
  begin
    T := Self[Id].TerminalTypeId;
    while not Self[T].Host do
      T := Self[T].AncestorId;

    for I:=T + 1 to Card do
    begin
      R := Self[I];
      if (R.Level = T) and (R.Kind = KindPROP) and
         R.IsPublished and
         (not (R.FinalTypeId in [typeEVENT, typeRECORD])) then
        Inc(result)
      else if R.Kind = kindNAMESPACE then
        break;
    end;
  end
  else
    Exit;
end;

function TBaseSymbolTable.GetPublishedPropDescriptorId(Id, PropNumber: Integer): Integer;
var
  T, FinTypeId, I: Integer;
  R: TSymbolRec;
begin
  result := 0;
  if Id = 0 then
    Exit;
  FinTypeId := Self[Id].FinalTypeId;
  if FinTypeId = typeCLASS then
  begin
    T := Self[Id].TerminalTypeId;

    while not Self[T].Host do
      T := Self[T].AncestorId;

    result := -1;
    for I:=T + 1 to Card do
    begin
      R := Self[I];
      if (R.Level = T) and (R.Kind = KindPROP) and
         R.IsPublished and
         (not (R.FinalTypeId in [typeEVENT, typeRECORD])) then
        begin
          Inc(result);
          if result = PropNumber then
          begin
            result := I;
            Exit;
          end;
        end
      else if R.Kind = kindNAMESPACE then
        break;
    end;
  end
  else
    RaiseError(errClassTypeRequired, []);
end;

function TBaseSymbolTable.GetPublishedPropName(Id, PropNumber: Integer): String;
var
  PropDescriptorId: Integer;
begin
  PropDescriptorId := GetPublishedPropDescriptorId(Id, PropNumber);
  result := Self[PropDescriptorId].Name;
end;

function TBaseSymbolTable.GetPublishedPropValueAsString(P: Pointer; StackFrameNumber: Integer;
                                           Id, PropNumber: Integer): String;
var
  OwnerAddress: Pointer;
  TypeId, PropDescriptorId: Integer;
  X: TObject;
  PropName: String;
  V: Variant;
begin
  try
    TypeId := Self[Id].TerminalTypeId;
    PropDescriptorId := GetPublishedPropDescriptorId(TypeId, PropNumber);

    OwnerAddress := GetFinalAddress(P, StackFrameNumber, Id);
    if OwnerAddress = nil then
      result := errError
    else
    begin
      X := TObject(OwnerAddress^);
      if X = nil then
        result := errError
      else
      begin
        if X = nil then
          result := errError
        else
        begin
          PropName := Records[PropDescriptorId].Name;
          if GetPropInfo(X, PropName) = nil then
            result := errError
          else
          begin
            V := GetPropValue(X, PropName, true);
            result := VarToStr(V);
          end;
        end;
      end;
    end;
  except
    result := errError;
  end;
end;

function TBaseSymbolTable.GetFieldDescriptorId(Id,
                                               FieldNumber: Integer;
                                               TypeMapRec: TTypeMapRec = nil
                                               ): Integer;
var
  T, FinTypeId, I, J, K: Integer;
  R: TSymbolRec;
  L: TIntegerList;
begin
  result := 0;
  FinTypeId := Self[Id].FinalTypeId;

  if FinTypeId = typeCLASS then
  begin
    T := Self[Id].TerminalTypeId;

    if TypeMapRec <> nil then
      if TypeMapRec.TypeId = T then
        if TypeMapRec.Completed then
        begin
          result := TypeMapRec.Fields[FieldNumber];
          Exit;
        end;

    L := TIntegerList.Create;

    try
      L.Add(T);

      T := Self[T].AncestorId;
      while T <> 0 do
      begin
        L.Insert(0, T);
        T := Self[T].AncestorId;
      end;

      K := -1;
      for I:=0 to L.Count - 1 do
      begin
        T := L[I];
        for J:=T + 1 to Card do
        begin
          R := Self[J];
          if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
          begin
            Inc(K);
            if K = FieldNumber then
            begin
              result := J;
              Exit;
            end;
          end;
        end;
      end;

    finally
      FreeAndNil(L);
    end;
  end
  else if FinTypeId = typeRECORD then
  begin
    T := Self[Id].TerminalTypeId;

    if TypeMapRec <> nil then
      if TypeMapRec.TypeId = T then
        if TypeMapRec.Completed then
        begin
          result := TypeMapRec.Fields[FieldNumber];
          Exit;
        end;

    K := -1;
    for J:=T + 1 to Card do
    begin
      R := Self[J];
      if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
      begin
        Inc(K);
        if K = FieldNumber then
        begin
          result := J;
          Exit;
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.GetFieldDescriptorIdByName(Id: Integer; const FieldName: String): Integer;
var
  T, FinTypeId, I, J: Integer;
  R: TSymbolRec;
  L: TIntegerList;
begin
  result := 0;
  FinTypeId := Self[Id].FinalTypeId;

  if FinTypeId = typeCLASS then
  begin
    T := Self[Id].TerminalTypeId;
    L := TIntegerList.Create;

    try
      L.Add(T);

      T := Self[T].AncestorId;
      while T <> 0 do
      begin
        L.Insert(0, T);
        T := Self[T].AncestorId;
      end;

      for I:=0 to L.Count - 1 do
      begin
        T := L[I];
        for J:=T + 1 to Card do
        begin
          R := Self[J];
          if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
          begin
            if StrEql(R.Name, FieldName) then
            begin
              result := J;
              Exit;
            end;
          end;
        end;
      end;

    finally
      FreeAndNil(L);
    end;
  end
  else if FinTypeId = typeRECORD then
  begin
    T := Self[Id].TerminalTypeId;
    for J:=T + 1 to Card do
    begin
      R := Self[J];
      if (R.Level = T) and (R.Kind = KindTYPE_FIELD) then
      begin
        if StrEql(R.Name, FieldName) then
        begin
          result := J;
          Exit;
        end;
      end;
    end;
  end;
end;

function TBaseSymbolTable.GetFieldName(Id, FieldNumber: Integer): String;
var
  FieldDescriptorId: Integer;
begin
  FieldDescriptorId := GetFieldDescriptorId(Id, FieldNumber);
  result := Self[FieldDescriptorId].Name;
end;

function TBaseSymbolTable.GetFieldAddress(P: Pointer;
                                          StackFrameNumber,
                                          Id,
                                          FieldNumber: Integer;
                                          TypeMapRec: TTypeMapRec = nil
                                          ): Pointer;
var
  FieldDescriptorId, Shift: Integer;
  X: TObject;
begin
  result := GetFinalAddress(P, StackFrameNumber, Id);

  try
    CheckMemory(Result, sizeof (TObject));
    if Self[Id].FinalTypeId = typeCLASS then
    begin
      X := TObject(result^);
      if X = nil then
      begin
        result := nil;
        Exit;
      end;

{$IFNDEF FPC}
      CheckMemory (pointer (integer (pointer (X)^) + vmtSelfPtr), - vmtSelfPtr);
      if pointer (pointer (integer (pointer (X)^) + vmtSelfPtr)^) <> pointer(pointer (X)^) then
        raise EAbort.Create (errNotValidObject);
{$ENDIF}
      result := Pointer(X);
    end;
  except
    result := nil;
  end;

  if result = nil then
    Exit;

  FieldDescriptorId := GetFieldDescriptorId(Id, FieldNumber, TypeMapRec);
  Shift := Self[FieldDescriptorId].Shift;

  result := ShiftPointer(result, Shift);
end;

function TBaseSymbolTable.GetStrVal(Address: Pointer;
                                    TypeId: Integer;
                                    TypeMapRec: TTypeMapRec = nil;
                                    BriefCls: Boolean = false): String;
var
  B: Byte;
  W: Word;
  X: TObject;
  C: TClass;
  P: Pointer;
  V: Variant;
  FinTypeId: Integer;
  I, K: Integer;
  FieldAddress: Pointer;
  FieldDescriptorId, FieldTypeId, FieldShift: Integer;
  RangeTypeId, ElemTypeId, K1, K2: Integer;
  ByteSet: TByteSet;
  EnumNames: TStringList;
begin
  FinTypeId := Self[TypeId].FinalTypeId;

  if TypeMapRec <> nil then
    if TypeMapRec.TypeId <> TypeId then
      TypeMapRec := nil;

  try

    case FinTypeId of
      typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
      begin
        CheckMemory(Address, sizeof(Byte));
        B := Byte(Address^);
        if B <> 0 then
          result := 'true'
        else
          result := 'false';
      end;
      typeBYTE, typeENUM:
      begin
        CheckMemory(Address, SizeOf(Byte));
        B := Byte(Address^);
        result := IntToStr(B);
      end;
{$IFNDEF PAXARM}
      typeANSICHAR:
      begin
        CheckMemory(Address, SizeOf(Byte));
        B := Byte(Address^);
        result := String(AnsiChar(B));
      end;
     typeWIDESTRING:
       begin
         CheckMemory (Address, sizeof (WideString));
         if pointer (Address^) <> nil then
           begin
             // First check to be able to length if WideString in bytes
             CheckMemory(pointer(integer(Address^) - SizeOf(LongInt)), SizeOf(LongInt));

             // Let's check if contents are accesible
             I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
             // I contains length of string in bytes
             if I = 0 then
             begin
               result := '';
               Exit;
             end;
             CheckMemory (pointer(Address^), I + SizeOf (WideChar)); // One extra WideChar for #0
             result := WideString(Address^);
           end
           else Result := '';
       end;
      typeANSISTRING:
        begin
          // Check pointer to string
          CheckMemory (Address, sizeof (AnsiString));
          if pointer (Address^) <> nil then
          begin
            // First check to be able to access length of string and the ref count integer
            CheckMemory(pointer(integer(Address^) - SizeOf(LongInt) * 2), SizeOf(LongInt) * 2);

            // Let's check if contents are accesible
            I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
            // I contains length of string
            if I = 0 then
            begin
              result := '';
              Exit;
            end;
            CheckMemory (pointer(Address^), I + 1);
            result := String(AnsiString(Address^));
          end
          else
          begin
            result := '';
            Exit;
          end;
        end;
      typeSHORTSTRING:
        begin
          CheckMemory (Address, sizeof (ShortString));
          result := String(ShortString(Address^));
        end;
{$ENDIF}
      typeSET:
      begin
        CheckMemory(Address, SizeOf(TByteSet));
        TypeId := Self[TypeId].PatternId;
        FinTypeId := Self[TypeId].FinalTypeId;
        if FinTypeId = typeENUM then
          EnumNames := ExtractEnumNames(TypeId)
        else
          EnumNames := nil;
        ByteSet := UpdateSet(TByteSet(Address^), Self[TypeId].Size);
        result := ByteSetToString(ByteSet, FinTypeId, EnumNames);
        if EnumNames <> nil then
          FreeAndNil(EnumNames);
      end;
      typeINTEGER:
        begin
          CheckMemory(Address, SizeOf(LongInt));
          result := IntToStr(Integer(Address^));
        end;
      typeCARDINAL:
        begin
          CheckMemory(Address, SizeOf(Cardinal));
          result := IntToStr(Cardinal(Address^));
        end;
      typeSMALLINT:
        begin
          CheckMemory (Address, sizeof(SmallInt));
          result := IntToStr(SmallInt(Address^));
        end;
      typeSHORTINT:
        begin
          CheckMemory (Address, sizeof (ShortInt));
          result := IntToStr(ShortInt(Address^));
        end;
      typeEVENT:
      begin
        FieldTypeId := typePOINTER;
        FieldShift := SizeOfPointer;
        FieldAddress := Address;

        result := Self[TypeId].Name + '(';
        result := result + GetStrVal(FieldAddress, FieldTypeId);

        result := result + ',';

        FieldAddress := ShiftPointer(Address, FieldShift);
        result := result + GetStrVal(FieldAddress, FieldTypeId);

        result := result + ')';
      end;
      typeRECORD:
      begin
        result := Self[TypeId].Name + '(';
        K := GetFieldCount(TypeId, TypeMapRec);
        for I:=0 to K - 1 do
        begin
          FieldDescriptorId := GetFieldDescriptorId(TypeId, I, TypeMapRec);

          FieldTypeId := Self[FieldDescriptorId].TypeId;
          FieldShift := Self[FieldDescriptorId].Shift;

          FieldAddress := ShiftPointer(Address, FieldShift);

          result := result + GetStrVal(FieldAddress, FieldTypeId);
          if I < K - 1 then
            result := result + ',';
        end;
        result := result + ')';
      end;
      typeCLASS:
      begin
        // Check pointer to object
        CheckMemory (Address, sizeof (TObject));
        X := TObject(Address^);
        if Assigned(X) then
        begin
          if X is TGC_Object then
          begin
            result := TGC_Object(X).__toString;
            Exit;
          end;

          if BriefCls then
          begin
            result := Self[TypeId].Name +
              '(' + Format('0x%x', [Cardinal(Address^)]) + ')';
            Exit;
          end;

          result := Self[TypeId].Name + '(';
          K := GetFieldCount(TypeId, TypeMapRec);
          for I:=0 to K - 1 do
          begin
            FieldDescriptorId := GetFieldDescriptorId(TypeId, I, TypeMapRec);

            FieldTypeId := Self[FieldDescriptorId].TypeId;
            FieldShift := Self[FieldDescriptorId].Shift;

{$IFNDEF FPC}
            // Check VMT for readability and to see if it's a true VMT
            CheckMemory (pointer (integer (pointer (X)^) + vmtSelfPtr), - vmtSelfPtr);
            if pointer (pointer (integer (pointer (X)^) + vmtSelfPtr)^) <> pointer(pointer (X)^) then
              raise EAbort.Create (errNotValidObject);
{$ENDIF}

            FieldAddress := ShiftPointer(Pointer(X), FieldShift);

            if FieldTypeId = TypeId then
              result := result + GetStrVal(FieldAddress, FieldTypeId, nil, true)
            else
              result := result + GetStrVal(FieldAddress, FieldTypeId);
            if I < K - 1 then
              result := result + ',';
          end;
          result := result + ')';
        end
        else
          result := 'nil';
      end;
      typeCLASSREF:
      begin
        CheckMemory (Address, sizeof (TClass));
        C := TClass(Address^);
        if Assigned(C) then
          result := Self[TypeId].Name
        else
          result := 'nil';
      end;
      typePOINTER:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        result := Format('0x%x', [Cardinal(Address^)]);
      end;
      typeINTERFACE:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        if Cardinal(Address^) = 0 then
          result := 'nil'
        else
          result := Self[TypeId].Name + '(' +
            Format('0x%x', [Cardinal(Address^)]) + ')';
      end;
      typePROC:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        result := Format('0x%x', [Cardinal(Address^)]);
      end;
      typeARRAY:
      begin
        result := Self[TypeId].Name + '(';

        GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
        FieldShift := Self[ElemTypeId].Size;

        K1 := GetLowBoundRec(RangeTypeId).Value;
        K2 := GetHighBoundRec(RangeTypeId).Value;

        for I:=K1 to K2 do
        begin
          FieldAddress := ShiftPointer(Address, (I - K1) * FieldShift);
          result := result + GetStrVal(FieldAddress, ElemTypeId);
          if I < K2 then
            result := result + ',';
        end;

        result := result + ')';
      end;
      typeDYNARRAY:
      begin
        CheckMemory (Address, sizeof (Pointer));
        Address := Pointer(Address^);

        if Address = nil then
        begin
          result := 'nil';
          Exit;
        end;

        result := Self[TypeId].Name + '(';

        ElemTypeId := Self[TypeId].PatternId;
        FieldShift := Self[ElemTypeId].Size;

        P := ShiftPointer(Address, - SizeOf(LongInt));

        K1 := 0;
        K2 := Integer(P^);

        for I:=K1 to K2 - 1 do
        begin
          FieldAddress := ShiftPointer(Address, (I - K1) * FieldShift);
          result := result + GetStrVal(FieldAddress, ElemTypeId);
          if I < K2 - 1 then
            result := result + ',';
        end;

        result := result + ')';
      end;
     typeUNICSTRING:
       begin
         CheckMemory (Address, sizeof (UnicString));
         if pointer (Address^) <> nil then
           begin
             // First check to be able to length if WideString in bytes
             CheckMemory(pointer(integer(Address^) - SizeOf(LongInt)), SizeOf(LongInt));

             // Let's check if contents are accesible
             I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
             // I contains length of string in bytes
             if I = 0 then
             begin
               result := '';
               Exit;
             end;
             CheckMemory (pointer(Address^), I + SizeOf (WideChar)); // One extra WideChar for #0
             result := UnicString(Address^);
           end
           else Result := '';
       end;
      typeWIDECHAR:
      begin
        CheckMemory(Address, sizeof(WideChar));
        W := Word(Address^);
        result := WideChar(W);
      end;
      typeWORD:
      begin
        CheckMemory(Address, sizeof(Word));
        W := Word(Address^);
        result := IntToStr(W);
      end;
      typeINT64:
        begin
          CheckMemory (Address, sizeof (Int64));
          result := IntToStr(Int64(Address^));
        end;
      typeSINGLE:
        begin
          CheckMemory (Address, sizeof (Single));
          result := FloatToStr(Single(Address^));
        end;
      typeDOUBLE:
        begin
          CheckMemory (Address, sizeof (Double));
          result := FloatToStr(Double(Address^));
        end;
      typeEXTENDED:
        begin
          CheckMemory (Address, sizeof (Extended));
          result := FloatToStr(Extended(Address^));
        end;
      typeCURRENCY:
        begin
          CheckMemory (Address, sizeof (Extended));
          result := CurrToStr(Currency(Address^));
        end;
      typeVARIANT, typeOLEVARIANT:
        begin
          try
            begin
              CheckMemory (Address, sizeof (Variant));
              CheckVariantData (Address^);
              if VarType(Variant(Address^)) = varError then
                result := ''
              else
                result := VarToStr(Variant(Address^));
            end
          finally
            { Variant is residing within the context of script,
              if we don't clean the TVarData before leaving the scope
              Delphi code will add cleanup code that will either free
              memory it shouldn't or even try to cleanup garbage, causing trouble regardless.
              The variant we evaluated was done so for temporary reasons anc copied for memory
              residing on the stack, as such, not cleanup is fine. Script should cleanup
              when the variant leaves its own scope }
            FillChar (V, Sizeof (V), 0);
          end;
        end
      else
        result := '';
    end;
  except
    result := errInvalidValue;
  end;
end;

function TBaseSymbolTable.GetVariantVal(Address: Pointer;
                                    TypeId: Integer;
                                    TypeMapRec: TTypeMapRec = nil): Variant;
var
  B: Byte;
  W: Word;
  X: TObject;
  C: TClass;
  P: Pointer;
  V: Variant;
  FinTypeId: Integer;
  I, K: Integer;
  FieldAddress: Pointer;
  FieldDescriptorId, FieldTypeId, FieldShift: Integer;
  RangeTypeId, ElemTypeId, K1, K2: Integer;
begin
  FinTypeId := Self[TypeId].FinalTypeId;

  if TypeMapRec <> nil then
    if TypeMapRec.TypeId <> TypeId then
      TypeMapRec := nil;

  try

    case FinTypeId of
      typeBOOLEAN, typeBYTEBOOL, typeWORDBOOL, typeLONGBOOL:
      begin
        CheckMemory(Address, sizeof(Byte));
        B := Byte(Address^);
        if B <> 0 then
          result := true
        else
          result := false;
      end;
      typeBYTE, typeENUM:
      begin
        CheckMemory(Address, SizeOf(Byte));
        B := Byte(Address^);
        result := B;
      end;
{$IFNDEF PAXARM}
      typeANSICHAR:
      begin
        CheckMemory(Address, SizeOf(Byte));
        B := Byte(Address^);
        result := String(AnsiChar(B));
      end;
{$ENDIF}
      typeSET:
      begin
        CheckMemory(Address, SizeOf(TByteSet));
        TypeId := Self[TypeId].PatternId;
        FinTypeId := Self[TypeId].FinalTypeId;
        result := ByteSetToString(TByteSet(Address^), FinTypeId);
      end;
      typeINTEGER:
        begin
          CheckMemory(Address, SizeOf(LongInt));
          result := Integer(Address^);
        end;
      typeCARDINAL:
        begin
          CheckMemory(Address, SizeOf(Cardinal));
          result := Cardinal(Address^);
        end;
      typeSMALLINT:
        begin
          CheckMemory (Address, sizeof(SmallInt));
          result := SmallInt(Address^);
        end;
      typeSHORTINT:
        begin
          CheckMemory (Address, sizeof (ShortInt));
          result := ShortInt(Address^);
        end;
      typeEVENT:
      begin
        FieldTypeId := typePOINTER;
        FieldShift := SizeOfPointer;
        FieldAddress := Address;

        result := Self[TypeId].Name + '(';
        result := result + GetStrVal(FieldAddress, FieldTypeId);

        result := result + ',';

        FieldAddress := ShiftPointer(Address, FieldShift);
        result := result + GetStrVal(FieldAddress, FieldTypeId);

        result := result + ')';
      end;
      typeRECORD:
      begin
        result := Self[TypeId].Name + '(';
        K := GetFieldCount(TypeId, TypeMapRec);
        for I:=0 to K - 1 do
        begin
          FieldDescriptorId := GetFieldDescriptorId(TypeId, I, TypeMapRec);

          FieldTypeId := Self[FieldDescriptorId].TypeId;
          FieldShift := Self[FieldDescriptorId].Shift;

          FieldAddress := ShiftPointer(Address, FieldShift);

          result := result + GetStrVal(FieldAddress, FieldTypeId);
          if I < K - 1 then
            result := result + ',';
        end;
        result := result + ')';
      end;
      typeCLASS:
      begin
        // Check pointer to object
        CheckMemory (Address, sizeof (TObject));
        X := TObject(Address^);
        if Assigned(X) then
        begin
          result := Self[TypeId].Name + '(';
          K := GetFieldCount(TypeId, TypeMapRec);
          for I:=0 to K - 1 do
          begin
            FieldDescriptorId := GetFieldDescriptorId(TypeId, I, TypeMapRec);

            FieldTypeId := Self[FieldDescriptorId].TypeId;
            FieldShift := Self[FieldDescriptorId].Shift;

{$IFNDEF FPC}
            // Check VMT for readability and to see if it's a true VMT
            CheckMemory (pointer (integer (pointer (X)^) + vmtSelfPtr), - vmtSelfPtr);
            if pointer (pointer (integer (pointer (X)^) + vmtSelfPtr)^) <> pointer(pointer (X)^) then
              raise EAbort.Create (errNotValidObject);
{$ENDIF}

            FieldAddress := ShiftPointer(Pointer(X), FieldShift);

            result := result + GetStrVal(FieldAddress, FieldTypeId);
            if I < K - 1 then
              result := result + ',';
          end;
          result := result + ')';
        end
        else
          result := 0;
      end;
      typeCLASSREF:
      begin
        CheckMemory (Address, sizeof (TClass));
        C := TClass(Address^);
        if Assigned(C) then
          result := Self[TypeId].Name
        else
          result := 0;
      end;
      typePOINTER:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        result := Integer(Address^);
      end;
      typeINTERFACE:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        if Cardinal(Address^) = 0 then
          result := 0
        else
          result := Self[TypeId].Name + '(' +
            Format('0x%x', [Cardinal(Address^)]) + ')';
      end;
      typePROC:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        result := Integer(Address^);
      end;
      typeARRAY:
      begin
        result := Self[TypeId].Name + '(';

        GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
        FieldShift := Self[ElemTypeId].Size;

        K1 := GetLowBoundRec(RangeTypeId).Value;
        K2 := GetHighBoundRec(RangeTypeId).Value;

        for I:=K1 to K2 do
        begin
          FieldAddress := ShiftPointer(Address, (I - K1) * FieldShift);
          result := result + GetStrVal(FieldAddress, ElemTypeId);
          if I < K2 then
            result := result + ',';
        end;

        result := result + ')';
      end;
      typeDYNARRAY:
      begin
        CheckMemory (Address, sizeof (Pointer));
        Address := Pointer(Address^);

        if Address = nil then
        begin
          result := 0;
          Exit;
        end;

        result := Self[TypeId].Name + '(';

        ElemTypeId := Self[TypeId].PatternId;
        FieldShift := Self[ElemTypeId].Size;

        P := ShiftPointer(Address, - SizeOf(LongInt));

        K1 := 0;
        K2 := Integer(P^);

        for I:=K1 to K2 - 1 do
        begin
          FieldAddress := ShiftPointer(Address, (I - K1) * FieldShift);
          result := result + GetStrVal(FieldAddress, ElemTypeId);
          if I < K2 - 1 then
            result := result + ',';
        end;

        result := result + ')';
      end;
{$IFNDEF PAXARM}
      typeANSISTRING:
        begin
          // Check pointer to string
          CheckMemory (Address, sizeof (AnsiString));
          if pointer (Address^) <> nil then
          begin
            // First check to be able to access length of string and the ref count integer
            CheckMemory(pointer(integer(Address^) - SizeOf(LongInt) * 2), SizeOf(LongInt) * 2);

            // Let's check if contents are accesible
            I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
            // I contains length of string
            if I = 0 then
            begin
              result := '';
              Exit;
            end;
            CheckMemory (pointer(Address^), I + 1);
            result := String(AnsiString(Address^));
          end
          else
          begin
            result := '';
            Exit;
          end;
        end;
     typeWIDESTRING:
       begin
         CheckMemory (Address, sizeof (WideString));
         if pointer (Address^) <> nil then
           begin
             // First check to be able to length if WideString in bytes
             CheckMemory(pointer(integer(Address^) - SizeOf(LongInt)), SizeOf(LongInt));

             // Let's check if contents are accesible
             I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
             // I contains length of string in bytes
             if I = 0 then
             begin
               result := '';
               Exit;
             end;
             CheckMemory (pointer(Address^), I + SizeOf (WideChar)); // One extra WideChar for #0
             result := WideString(Address^);
           end
           else Result := '';
       end;
      typeSHORTSTRING:
        begin
          CheckMemory (Address, sizeof (ShortString));
          result := String(ShortString(Address^));
        end;
{$ENDIF}
     typeUNICSTRING:
       begin
         CheckMemory (Address, sizeof (UnicString));
         if pointer (Address^) <> nil then
           begin
             // First check to be able to length if WideString in bytes
             CheckMemory(pointer(integer(Address^) - SizeOf(LongInt)), SizeOf(LongInt));

             // Let's check if contents are accesible
             I := Integer(pointer(integer(Address^) - SizeOf(LongInt))^);
             // I contains length of string in bytes
             if I = 0 then
             begin
               result := '';
               Exit;
             end;
             CheckMemory (pointer(Address^), I + SizeOf (WideChar)); // One extra WideChar for #0
             result := UnicString(Address^);
           end
           else Result := '';
       end;
      typeWIDECHAR:
      begin
        CheckMemory(Address, sizeof(WideChar));
        W := Word(Address^);
        result := WideChar(W);
      end;
      typeWORD:
      begin
        CheckMemory(Address, sizeof(Word));
        W := Word(Address^);
        result := W;
      end;
      typeINT64:
        begin
          CheckMemory (Address, sizeof (Int64));
{$IFDEF VARIANTS}
          result := Int64(Address^);
{$ELSE}
          result := Integer(Address^);
{$ENDIF}
        end;
      typeSINGLE:
        begin
          CheckMemory (Address, sizeof (Single));
          result := Single(Address^);
        end;
      typeDOUBLE:
        begin
          CheckMemory (Address, sizeof (Double));
          result := Double(Address^);
        end;
      typeEXTENDED:
        begin
          CheckMemory (Address, sizeof (Extended));
          result := Extended(Address^);
        end;
      typeCURRENCY:
        begin
          CheckMemory (Address, sizeof (Extended));
          result := Currency(Address^);
        end;
      typeVARIANT, typeOLEVARIANT:
        begin
          try
            begin
              CheckMemory (Address, sizeof (Variant));
              CheckVariantData (Address^);
              result := Variant(Address^);
            end
          finally
            { Variant is residing within the context of script,
              if we don't clean the TVarData before leaving the scope
              Delphi code will add cleanup code that will either free
              memory it shouldn't or even try to cleanup garbage, causing trouble regardless.
              The variant we evaluated was done so for temporary reasons anc copied for memory
              residing on the stack, as such, not cleanup is fine. Script should cleanup
              when the variant leaves its own scope }
            FillChar (V, Sizeof (V), 0);
          end;
        end
      else
        result := 0;
    end;
  except
    result := errInvalidValue;
  end;
end;

procedure TBaseSymbolTable.CheckVariantData (const V);
var
  I: Integer;
begin
  if (TVarData (V).VType and varByRef <> 0) or
       (TVarData (V).VType and varArray <> 0)
    then raise EAbort.Create('varArray of varByRef not supported in debugger');
  case TVarData (V).VType and varTypeMask of
    varEmpty, varNull,
    varSmallInt, varInteger, varSingle,
    varDouble, varCurrency, varDate,
    varError, varBoolean,
    {$IFDEF VARIANTS}
    varShortInt, varWord, varLongWord, varInt64,
    {$ENDIF}
    varByte : { Everything all right, this types won't cause trouble };
    varOleStr:
      begin
        with TVarData (V) do
          begin
            CheckMemory (pointer (integer (VOleStr) - sizeof (integer)), sizeof (integer));
            I := integer (pointer (integer (VOleStr) - sizeof (integer))^);
            CheckMemory (VOleStr, I + sizeof (WideChar));
          end;
      end;
    varString, varUString:
      begin
        with TVarData (V) do
          begin
            if Assigned(VString) then
            begin
              CheckMemory (pointer (integer (VString) - sizeof (integer) * 2), sizeof (integer) * 2);
              I := integer (pointer (integer (VString) - sizeof (integer))^);
              CheckMemory (VString, I + sizeof (Char));
            end;
          end;
      end;
     else
       RaiseError(errInvalidVariantType, []);
  end;
end;

function TBaseSymbolTable.GetVal(Address: Pointer;
                                 TypeId: Integer): Variant;
var
  FinTypeId, SZ: Integer;
begin
  FinTypeId := Self[TypeId].FinalTypeId;

  try

    SZ := Types.GetSize(TypeId);
    if SZ > 0 then
      CheckMemory(Address, SZ);

    case FinTypeId of
      typeBOOLEAN: result := Boolean(Address^);
      typeBYTEBOOL: result := ByteBool(Address^);
{$IFNDEF PAXARM}
      typeANSISTRING: result := AnsiString(Address^);
      typeWIDESTRING: result := WideString(Address^);
      typeSHORTSTRING: result := ShortString(Address^);
      typeANSICHAR,
{$ENDIF}
      typeBYTE, typeENUM: result := Byte(Address^);
      typeINTEGER, typeLONGBOOL: result := Integer(Address^);
{$IFDEF VARIANTS}
      typeCARDINAL: result := Cardinal(Address^);
{$ELSE}
      typeCARDINAL: result := Integer(Address^);
{$ENDIF}
      typeSMALLINT: result := SmallInt(Address^);
      typeSHORTINT: result := ShortInt(Address^);
{$IFDEF VARIANTS}
      typePOINTER, typeINTERFACE, typeCLASS, typeCLASSREF: result := Cardinal(Address^);
{$ELSE}
      typePOINTER, typeINTERFACE, typeCLASS, typeCLASSREF: result := Integer(Address^);
{$ENDIF}
      typeUNICSTRING: result := UnicString(Address^);
      typeWORD, typeWIDECHAR, typeWORDBOOL: result := Word(Address^);
      {$IFDEF VARIANTS}
      typeINT64: result := Int64(Address^);
      {$ELSE}
      typeINT64: result := Integer(Address^);
      {$ENDIF}
      typeSINGLE: result := Single(Address^);
      typeDOUBLE: result := Double(Address^);
      typeEXTENDED: result := Extended(Address^);
      typeVARIANT, typeOLEVARIANT:
      begin
        CheckVariantData (Address^);
        result := Variant(Address^);
      end;
       else
         RaiseError(errIncompatibleTypesNoArgs, []);
    end;

  except
    result := Unassigned;
  end;
end;

procedure TBaseSymbolTable.PutVal(Address: Pointer;
                                  TypeId: Integer; const Value: Variant);
var
  B: Byte;
  FinTypeId: Integer;
  W: Word;
begin
  FinTypeId := Self[TypeId].FinalTypeId;

  case FinTypeId of
    typeBOOLEAN, typeBYTEBOOL:
    begin
      CheckMemory(Address, SizeOf(Byte));
      if Value then
        B := 1
      else
        B := 0;
      Byte(Address^) := B;
    end;
    typeBYTE, typeENUM:
    begin
      CheckMemory(Address, SizeOf(Byte));
      Byte(Address^) := Value;
    end;
{$IFNDEF PAXARM}
    typeANSICHAR:
    begin
      CheckMemory(Address, SizeOf(Byte));
      Byte(Address^) := Byte(Value);
    end;
    typeANSISTRING:
      begin
        // Check pointer to string
        CheckMemory (Address, sizeof (AnsiString));
        AnsiString(Address^) := AnsiString(Value);
      end;
   typeWIDESTRING:
     begin
       CheckMemory (Address, sizeof (WideString));
       WideString(Address^) := Value;
     end;
    typeSHORTSTRING:
      begin
        CheckMemory (Address, sizeof (ShortString));
        ShortString(Address^) := ShortString(Value);
      end;
{$ENDIF}
    typeINTEGER, typeLONGBOOL:
      begin
        CheckMemory (Address, sizeof (integer));
        Integer(Address^) := Integer(Value);
      end;
    typeCARDINAL:
      begin
        CheckMemory (Address, sizeof (Cardinal));
        Cardinal(Address^) := Value;
      end;
    typeSMALLINT:
      begin
        CheckMemory (Address, sizeof (SmallInt));
        SmallInt(Address^) := Value;
      end;
    typeSHORTINT:
      begin
        CheckMemory (Address, sizeof (ShortInt));
        ShortInt(Address^) := Value;
      end;
    typePOINTER, typeINTERFACE, typeCLASS, typeCLASSREF:
    begin
      CheckMemory (Address, sizeof (Cardinal));
      Cardinal(Address^) := Value;
    end;
   typeUNICSTRING:
     begin
       CheckMemory (Address, sizeof (UnicString));
       UnicString(Address^) := Value;
     end;
    typeWORD, typeWIDECHAR, typeWORDBOOL:
    begin
      CheckMemory(Address, SizeOf(Word));
      W := Word(Value);
      Word(Address^) := W;
    end;
    typeINT64:
      begin
        CheckMemory (Address, sizeof (Int64));
        {$IFDEF VARIANTS}
        Int64(Address^) := Value;
        {$ELSE}
        Int64(Address^) := Integer(Value);
        {$ENDIF}
      end;
    typeSINGLE:
      begin
        CheckMemory (Address, sizeof (Single));
        Single(Address^) := Value;
      end;
    typeDOUBLE:
      begin
        CheckMemory (Address, sizeof (Double));
        Double(Address^) := Value;
      end;
    typeEXTENDED:
      begin
        CheckMemory (Address, sizeof (Extended));
        Extended(Address^) := Value;
      end;
    typeVARIANT, typeOLEVARIANT:
      begin
        CheckMemory (Address, sizeof (Variant));
        CheckVariantData (Address^);
        Variant(Address^) := Value;
      end;
     else
       RaiseError(errIncompatibleTypesNoArgs, []);
  end;
end;

procedure TBaseSymbolTable.PutValue(P: Pointer; StackFrameNumber: Integer;
                                   Id: Integer; const Value: Variant);
var
  Address: Pointer;
  TypeId: Integer;
begin
  Address := GetFinalAddress(P, StackFrameNumber, Id);
  TypeId := Self[Id].TerminalTypeId;

  if Address = nil then
    Exit;

  PutVal(Address, TypeId, Value);
end;

function TBaseSymbolTable.GetValue(P: Pointer; StackFrameNumber: Integer;
                                   Id: Integer): Variant;
var
  Address: Pointer;
  TypeId: Integer;
begin
  Address := GetFinalAddress(P, StackFrameNumber, Id);
  TypeId := Self[Id].TerminalTypeId;

  if Address = nil then
    Exit;

  result := GetVal(Address, TypeId);
end;

function TBaseSymbolTable.GetValueAsString(P: Pointer;
                                           StackFrameNumber: Integer;
                                           Id: Integer;
                                           TypeMapRec: TTypeMapRec = nil;
                                           BriefCls: Boolean = false): String;
var
  Address: Pointer;
  TypeId: Integer;
begin
  result := '???';

  Address := GetFinalAddress(P, StackFrameNumber, Id);
  TypeId := Self[Id].TerminalTypeId;

  if Address = nil then
    Exit;

  result := GetStrVal(Address, TypeId, TypeMapRec);
end;

function TBaseSymbolTable.GetFieldValueAsString(P: Pointer;
                                                StackFrameNumber: Integer;
                                                Id: Integer;
                                                FieldNumber: Integer;
                                                TypeMapRec: TTypeMapRec = nil;
                                                BriefCls: Boolean = false): String;
var
  FieldAddress: Pointer;
  TypeId, FieldDescriptorId, FieldTypeId: Integer;
begin
  TypeId := Self[Id].TerminalTypeId;
  FieldDescriptorId := GetFieldDescriptorId(TypeId, FieldNumber, TypeMapRec);
  FieldTypeId := Self[FieldDescriptorId].TypeID;

  FieldAddress := GetFieldAddress(P, StackFrameNumber, Id, FieldNumber, TypeMapRec);

  if FieldAddress = nil then
    result := errInvalidValue
  else
    result := GetStrVal(FieldAddress, FieldTypeId, TypeMapRec, BriefCls);
end;

function TBaseSymbolTable.GetArrayItemAddress(P: Pointer; StackFrameNumber, Id,
                                              Index: Integer): Pointer;
var
  TypeId, RangeTypeId, ElemTypeId, Shift, K1: Integer;
begin
  result := GetFinalAddress(P, StackFrameNumber, Id);
  TypeId := Self[Id].TerminalTypeId;
  GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
  Shift := Self[ElemTypeId].Size;
  K1 := GetLowBoundRec(RangeTypeId).Value;
  result := ShiftPointer(result, (Index - K1) * Shift);
end;


function TBaseSymbolTable.GetArrayItemValueAsString(P: Pointer; StackFrameNumber: Integer;
                                               Id, Index: Integer): String;
var
  Address: Pointer;
  TypeId, RangeTypeId, ElemTypeId: Integer;
begin
  Address := GetArrayItemAddress(P, StackFrameNumber, Id, Index);
  TypeId := Self[Id].TerminalTypeId;
  GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
  result := GetStrVal(Address, ElemTypeId);
end;


function TBaseSymbolTable.GetDynArrayItemAddress(P: Pointer;
                                    StackFrameNumber: Integer;
                                    Id, Index: Integer): Pointer;
var
  TypeId, ElemTypeId, Shift: Integer;
begin
  result := GetFinalAddress(P, StackFrameNumber, Id);
  result := Pointer(result^);
  if result = nil then
    Exit;

  TypeId := Self[Id].TerminalTypeId;
  ElemTypeId := Self[TypeId].PatternId;
  Shift := Self[ElemTypeId].Size;

  result := ShiftPointer(result, Index * Shift);
end;

function TBaseSymbolTable.GetDynArrayItemValueAsString(P: Pointer; StackFrameNumber: Integer;
                                               Id, Index: Integer): String;
var
  Address: Pointer;
  TypeId, ElemTypeId: Integer;
begin
  Address := GetDynArrayItemAddress(P, StackFrameNumber, Id, Index);
  TypeId := Self[Id].TerminalTypeId;
  ElemTypeId := Self[TypeId].PatternId;
  result := GetStrVal(Address, ElemTypeId);
end;

function TBaseSymbolTable.GetFinalAddress(P: Pointer; StackFrameNumber: Integer;
                                          Id: Integer): Pointer;

var
  Shift: Integer;
begin
  result := nil;

  Shift := Self[Id].Shift;

  if Self[Id].Param then
  begin
    result := TBaseRunner(P).GetParamAddress(StackFrameNumber, Shift);
    if Self[Id].ByRef or Self[Id].ByRefEx then
      result := Pointer(result^);
  end
  else if Self[Id].Local then
  begin
    result := TBaseRunner(P).GetLocalAddress(StackFrameNumber, Shift);
    if Self[Id].ByRef or Self[Id].ByRefEx then
      result := Pointer(result^);
  end
  else if Self[Id].IsGlobalVar then
  begin
    result := TBaseRunner(P).GetAddress(Shift);
    if Self[Id].ByRef or Self[Id].ByRefEx or Self[Id].Host then
      result := Pointer(result^);
  end;
end;

{$IFNDEF PAXARM}
function TBaseSymbolTable.FindPAnsiCharConst(const S: String; LimitId: Integer): Integer;
var
  I: Integer;
  R: TSymbolRec;
begin
  for I:=Types.Count to LimitId do
  begin
    R := Records[I];
    if R.Kind = KindCONST then
      if R.HasPAnsiCharType then
        if R.Value = S then
        begin
          result := I;
          Exit;
        end;
  end;
  result := 0;
end;
{$ENDIF}

function TBaseSymbolTable.FindPWideCharConst(const S: String; LimitId: Integer): Integer;
var
  I: Integer;
  R: TSymbolRec;
begin
  for I:=Types.Count to LimitId do
  begin
    R := Records[I];
    if R.Kind = KindCONST then
      if R.HasPWideCharType then
        if R.Value = S then
        begin
          result := I;
          Exit;
        end;
  end;
  result := 0;
end;

function TBaseSymbolTable.GetAlignmentSize(TypeId, DefAlign: Integer): Integer;
var
  FT, J, temp,
  RangeTypeId, ElemTypeId: Integer;
  R: TSymbolRec;
  K: Integer;
begin
  if DefAlign = 1 then
  begin
    result := DefAlign;
    Exit;
  end;

  FT := Records[TypeId].FinalTypeId;
  if FT in (OrdinalTypes +
    [typeCLASS, typeCLASSREF, typePOINTER,
{$IFNDEF PAXARM}
     typeANSISTRING, typeWIDESTRING,
{$ENDIF}
     typeUNICSTRING, typeDYNARRAY, typeINTERFACE]) then
  begin
    result := Types.GetSize(FT);
    if result > DefAlign then
      result := DefAlign;
  end
  else if FT = typeSET then
  begin
    result := GetSizeOfSetType(TypeId);
    if result > 4 then
       result := 1;
  end
  else if FT = typeSINGLE then
  begin
    result := 4;
    if result > DefAlign then
      result := DefAlign;
  end
  else if FT in [typeDOUBLE, typeCURRENCY, typeEXTENDED] then
  begin
    result := 8;
    if result > DefAlign then
      result := DefAlign;
  end
{$IFNDEF PAXARM}
  else if FT = typeSHORTSTRING then
    result := 1
{$ENDIF}
  else if FT = typeARRAY then
  begin
    TypeId := Records[TypeId].TerminalTypeId;
    if Records[TypeId].IsPacked then
    begin
      result := 1;
      Exit;
    end;

    GetArrayTypeInfo(TypeId, RangeTypeId, ElemTypeId);
    result := GetAlignmentSize(ElemTypeId, DefAlign);
  end
  else if FT = typeRECORD then
  begin
    TypeId := Records[TypeId].TerminalTypeId;
    if Records[TypeId].IsPacked then
    begin
      result := 1;
      Exit;
    end;

    K := Card;

    result := 0;
    for J:= TypeId + 1 to K do
    begin
      R := Records[J];

      if R = SR0 then
        break;

      if (R.Kind = KindTYPE_FIELD) and (R.Level = TypeId) then
      begin
        temp := GetAlignmentSize(R.TypeId, DefAlign);
        if temp > result then
          result := temp;
      end;
    end;
  end
  else
    result := DefAlign;
end;

function TBaseSymbolTable.FindClassTypeId(Cls: TClass): Integer;
var
  I, J: Integer;
  S: String;
  R: TSymbolRec;
  ok: Boolean;
  List: TIntegerList;
begin
  if StdCard = 0 then
  begin
    if DllDefined then
      S := Cls.ClassName
    else
      S := '';

    for I:= Card downto 1 do
      with Records[I] do
        if DllDefined then
        begin
          if Kind = KindTYPE then if Name = S then
          begin
            result := I;
            Exit;
          end;
        end
        else if PClass = Cls then
        begin
          result := I;
          Exit;
        end;

    result := 0;
    Exit;
  end;

  S := Cls.ClassName;

  List := HashArray.GetList(S);

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];
    R := Records[I];
    if DllDefined then
      ok := StrEql(R.Name, S) and (R.Kind = KindTYPE)
    else
      ok := R.PClass = Cls;
    if ok then
    begin
      result := I;
      Exit;
    end;
  end;

  result := 0;
end;

function TBaseSymbolTable.FindClassTypeIdByPti(Pti: PTypeInfo): Integer;
var
  I, J: Integer;
  S: String;
  R: TSymbolRec;
  ok: Boolean;
  List: TIntegerList;
begin
  if StdCard = 0 then
  begin
    if DllDefined then
      S := PTIName(pti)
    else
      S := '';

    for I:= Card downto 1 do
      with Records[I] do
        if DllDefined then
        begin
          if Kind = KindTYPE then if Name = S then
          begin
            result := I;
            Exit;
          end;
        end
        else if PClass.ClassInfo = Pti then
        begin
          result := I;
          Exit;
        end;

    result := 0;
    Exit;
  end;

  S := PTIName(pti);

  List := HashArray.GetList(S);

  for J:=List.Count - 1 downto 0 do
  begin
    I := List[J];
    R := Records[I];
    if DllDefined then
      ok := StrEql(R.Name, S) and (R.Kind = KindTYPE)
    else if R.PClass <> nil then
      ok := R.PClass.ClassInfo = Pti
    else
      ok := false;
    if ok then
    begin
      result := I;
      Exit;
    end;
  end;

  result := 0;
end;

procedure TBaseSymbolTable.SetVisibility(ClassId: integer;
  const MemberName: String; value: Integer);
var
  Id: Integer;
  Vis: TClassVisibility;
begin
  Vis := cvNone;

  if Value = 0 then
    Vis := cvPublic
  else if Value = 1 then
    Vis := cvProtected
  else if Value = 2 then
    Vis := cvPrivate
  else if Value = 3 then
    Vis := cvPublished
  else
    RaiseError(errIncorrectValue, []);

  if ClassId > 0 then
  begin
    id := Lookup(MemberName, ClassId, true);
    if Id > 0 then
      Records[Id].Vis := Vis;
  end;

end;

procedure TBaseSymbolTable.SetVisibility(C: TClass; const MemberName: String; value: Integer);
begin
  SetVisibility(FindClassTypeId(C), MemberName, value);
end;

procedure TBaseSymbolTable.LoadGlobalSymbolTableFromStream(Stream: TStream);
var
  Reader: TReader;
  I, K: Integer;
  R: TSymbolRec;
  ClearNextVal: Boolean;
  C: TClass;
begin
  Reader := TReader.Create(Stream, 4096 * 4);
  try
    K := Reader.ReadInteger();

    ClearNextVal := false;

    C := nil;

    for I := StdCard + 1 to StdCard + K do
    begin
      R := AddRecord;
      R.LoadFromStream(Reader);

      if ClearNextVal then
      begin
        R.Value := Integer(C);
        ClearNextVal := false;
      end;

      if R.ClassIndex <> -1 then
      begin
        Inc(LastClassIndex);
        R.ClassIndex := LastClassIndex;

        C := Classes.GetClass(R.Name);

        R.PClass := C;
        ClearNextVal := true;
      end;
    end;

    LastShiftValue := Reader.ReadInteger();
    LastClassIndex := Reader.ReadInteger();
    LastSubId := Reader.ReadInteger();
    LastVarId := Reader.ReadInteger();

    HashArray.Clear;
    HashArray.LoadFromStream(Reader);

    SomeTypeList.Clear;
    SomeTypeList.LoadFromStream(Reader);

    GuidList.Clear;
    GuidList.LoadFromStream(Reader);

    ExternList.Clear;
    ExternList.LoadFromStream(Reader);

  finally
    FreeAndNil(Reader);
  end;
end;

procedure TBaseSymbolTable.LoadGlobalSymbolTableFromFile(const FileName: String);
var
  F: TFileStream;
begin
  if not FileExists(FileName) then
    RaiseError(errFileNotFound, [FileName]);
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadGlobalSymbolTableFromStream(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TBaseSymbolTable.SaveGlobalSymbolTableToStream(Stream: TStream);
var
  Writer: TWriter;
  I, K: Integer;
begin
  Writer := TWriter.Create(Stream, 4096 * 4);
  try
    K := Card - StdCard;
    Writer.WriteInteger(K);

    for I := StdCard + 1 to StdCard + K do
    begin
      Records[I].SaveToStream(Writer);
    end;

    Writer.WriteInteger(LastShiftValue);
    Writer.WriteInteger(LastClassIndex);
    Writer.WriteInteger(LastSubId);
    Writer.WriteInteger(LastVarId);

    HashArray.SaveToStream(Writer);
    SomeTypeList.SaveToStream(Writer);
    GuidList.SaveToStream(Writer);
    ExternList.SaveToStream(Writer);

  finally
    FreeAndNil(Writer);
  end;
end;

procedure TBaseSymbolTable.SaveGlobalSymbolTableToFile(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveGlobalSymbolTableToStream(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TBaseSymbolTable.SaveNamespaceToStream(LevelId: Integer; S: TStream);

var
  BeginId, EndId: Integer;

function IsExternalId(Id: Integer): Boolean;
begin
  if Id > EndId then
    result := true
  else if Id < BeginId then
  begin
    if Id < StdCard then
      result := false
    else
      result := true;
  end
  else
    result := false;
end;

var
  Writer: TWriter;
  ExternRec: TExternRec;
  R: TSymbolRec;
  I: Integer;
  CurrOffset: Integer;
  GUID: TGUID;
begin
  CurrOffset := GetDataSize(LevelId);

  Writer := TWriter.Create(S, 4096 * 4);
  ExternRec := TExternRec.Create;

  try

    BeginId := LevelId;
    EndId := Card;

    for I := LevelId + 1 to Card do
      if Records[I].Kind = KindNAMESPACE then
      begin
        EndId := I - 1;
        break;
      end;

    Writer.Write(StreamVersion,  SizeOf(StreamVersion));
    Writer.Write(BeginId,     SizeOf(LongInt));
    Writer.Write(EndId,       SizeOf(LongInt));
    Writer.Write(CurrOffset,  SizeOf(LongInt));

    for I := LevelId to EndId do
    begin
      R := Records[I];
      R.SaveToStream(Writer);

      if (R.Kind = KindTYPE) and (R.TypeId = typeINTERFACE) then
      begin
        GUID := GuidList.GetGuidByID(I);
        Writer.Write(GUID, SizeOf(GUID));
      end;

      if IsExternalId(R.TypeId) then
      begin
        ExternRec.Id := R.Id;
        ExternRec.FullName := Records[R.TypeId].FullName;
        ExternRec.RecKind := erTypeId;

        ExternRec.SaveToStream(Writer);
      end;

      if IsExternalId(R.PatternId) then
      begin
        ExternRec.Id := R.Id;
        ExternRec.FullName := Records[R.PatternId].FullName;
        ExternRec.RecKind := erPatternId;

        ExternRec.SaveToStream(Writer);
      end;

      if IsExternalId(R.AncestorId) then
      begin
        ExternRec.Id := R.Id;
        ExternRec.FullName := Records[R.AncestorId].FullName;
        ExternRec.RecKind := erAncestorId;

        ExternRec.SaveToStream(Writer);
      end;

      if IsExternalId(R.ReadId) then
      begin
        ExternRec.Id := R.Id;
        ExternRec.FullName := Records[R.ReadId].FullName;
        ExternRec.RecKind := erReadId;
        ExternRec.SaveToStream(Writer);
      end;

      if IsExternalId(R.WriteId) then
      begin
        ExternRec.Id := R.Id;
        ExternRec.FullName := Records[R.WriteId].FullName;
        ExternRec.RecKind := erWriteId;
        ExternRec.SaveToStream(Writer);
      end;

    end;

  finally

    FreeAndNil(Writer);
    FreeAndNil(ExternRec);

  end;
end;

procedure TBaseSymbolTable.LoadNamespaceFromStream(S: TStream);

var
  OldNamespaceId, OldEndId: Integer;

function IsExternalId(Id: Integer): Boolean;
begin
  if Id > OldEndId then
    result := true
  else if Id < OldNamespaceId then
  begin
    if Id > StdCard then
      result := true
    else
      result := false;
  end
  else
    result := false;
end;

function IsInternalId(Id: Integer): Boolean;
begin
  result := (Id >= OldNamespaceId) and (Id <= OldEndId);
end;

var
  I, J, NamespaceId, K, Delta: Integer;
  R: TSymbolRec;
  ClearNextVal: Boolean;
  Reader: TReader;
  ExternRec: TExternRec;
  CurrOffset, OldOffset, DeltaOffset: Integer;
  First: Boolean;
  CurrStreamVersion: Integer;
  GUID: TGUID;
  GuidRec: TGuidRec;
  C: TClass;
begin
  C := nil;

  First := true;

  CurrOffset := GetDataSize;

  Reader := TReader.Create(S, 4096 * 4);
  ExternRec := TExternRec.Create;

  try
    Reader.Read(CurrStreamVersion,  SizeOf(StreamVersion));
    if CurrStreamVersion <> StreamVersion then
      RaiseError(errIncorrectStreamVersion, []);

    Reader.Read(OldNamespaceId, SizeOf(LongInt));
    Reader.Read(OldEndId,       SizeOf(LongInt));
    Reader.Read(OldOffset,      SizeOf(LongInt));

    K := OldEndId - OldNamespaceId + 1;

    R := AddRecord;
    NamespaceId := R.Id;

    Delta := NamespaceId - OldNamespaceId;
    DeltaOffset := CurrOffset - OldOffset;

    R.LoadFromStream(Reader);
    R.Update;

    ClearNextVal := false;

    for I := 2 to K do
    begin
      R := AddRecord;

      R.LoadFromStream(Reader);
      R.Update;

      if (R.Kind = KindTYPE) and (R.TypeId = typeINTERFACE) then
      begin
        Reader.Read(GUID, SizeOf(GUID));
        GuidList.Add(GUID, R.Id, R.Name);

        if R.SupportedInterfaces <> nil then
          for J := R.SupportedInterfaces.Count - 1 downto 0 do
          begin
            GuidRec := R.SupportedInterfaces[J];

            if IsExternalId(GuidRec.Id) then
            begin
              ExternList.Add(R.Id, GuidRec.Name, erGUID);
              R.SupportedInterfaces.RemoveAt(J);
            end
            else if IsInternalId(GuidRec.Id) then
              GuidRec.Id := GuidRec.Id + Delta;
          end;
      end;

      if IsInternalId(R.Level) then
        R.Level := R.Level + Delta;

      if R.Shift > 0 then
        if R.Kind <> kindTYPE_FIELD then
        begin
          R.Shift := R.Shift + DeltaOffset;

          if First then
          begin
            while R.Shift < CurrOffset do
            begin
              R.Shift := R.Shift + 1;
              Inc(DeltaOffset);
            end;

            First := false;
          end;
        end;

      if IsInternalId(R.OwnerId) then
        R.OwnerId := R.OwnerId + Delta;

      if IsExternalId(R.TypeId) then
      begin
        ExternRec.LoadFromStream(Reader);
        J := LookupFullName(ExternRec.FullName, true);
        if J > 0 then
          R.TypeID := J
        else
        begin
          ExternRec.Id := R.Id;
          ExternList.Add(ExternRec.Id, ExternRec.FullName, ExternRec.RecKind);
        end;
      end
      else if IsInternalId(R.TypeID) then
        R.TypeId := R.TypeId + Delta;

      if IsExternalId(R.PatternId) then
      begin
        ExternRec.LoadFromStream(Reader);
        J := LookupFullName(ExternRec.FullName, true);
        if J > 0 then
          R.PatternID := J
        else
        begin
          ExternRec.Id := R.Id;
          ExternList.Add(ExternRec.Id, ExternRec.FullName, ExternRec.RecKind);
        end;
      end
      else if IsInternalId(R.PatternID) then
        R.PatternId := R.PatternId + Delta;

      if IsExternalId(R.AncestorId) then
      begin
        ExternRec.LoadFromStream(Reader);
        J := LookupFullName(ExternRec.FullName, true);
        if J > 0 then
          R.AncestorID := J
        else
        begin
          ExternRec.Id := R.Id;
          ExternList.Add(ExternRec.Id, ExternRec.FullName, ExternRec.RecKind);
        end;
      end
      else if IsInternalId(R.AncestorID) then
        R.AncestorId := R.AncestorId + Delta;

      if IsExternalId(R.ReadId) then
      begin
        ExternRec.LoadFromStream(Reader);
        J := LookupFullName(ExternRec.FullName, true);
        if J > 0 then
          R.ReadId := J
        else
        begin
          ExternRec.Id := R.Id;
          ExternList.Add(ExternRec.Id, ExternRec.FullName, ExternRec.RecKind);
        end;
      end
      else if IsInternalId(R.ReadId) then
        R.ReadId := R.ReadId + Delta;

      if IsExternalId(R.WriteId) then
      begin
        ExternRec.LoadFromStream(Reader);
        J := LookupFullName(ExternRec.FullName, true);
        if J > 0 then
          R.WriteId := J
        else
        begin
          ExternRec.Id := R.Id;
          ExternList.Add(ExternRec.Id, ExternRec.FullName, ExternRec.RecKind);
        end;
      end
      else if IsInternalId(R.WriteId) then
        R.WriteId := R.WriteId + Delta;

      if ClearNextVal then
      begin
        R.Value := Integer(C);
        ClearNextVal := false;
      end;

      if R.ClassIndex <> -1 then
      begin
        Inc(LastClassIndex);
        R.ClassIndex := LastClassIndex;

        C := Classes.GetClass(R.Name);

        R.PClass := C;
        ClearNextVal := true;
      end;
    end;

  finally

    FreeAndNil(Reader);
    FreeAndNil(ExternRec);

  end;

  LastShiftValue := GetDataSize;

end;

procedure TBaseSymbolTable.ResolveExternList(CheckProc: TCheckProc; Data: Pointer);
var
  I, J, PositiveIndex: Integer;
  GD: TGUID;
  RJ: TSymbolRec;
begin
  for I := 0 to ExternList.Count - 1 do
  with ExternList[I] do
  begin
    if RecKind = erGUID then
    begin
      J := LookupType(FullName, true);

      if J = 0 then
      begin
        if Assigned(CheckProc) then
          if not CheckProc(FullName, Data, erNone) then
            continue;

        RaiseError(errUndeclaredInterface, [FullName]);
      end;

      if Records[Id].SupportedInterfaces = nil then
         Records[Id].SupportedInterfaces := TGuidList.Create;

      GD := GuidList.GetGuidByID(J);

      Records[Id].SupportedInterfaces.Add(GD, J, FullName);

      // recreate positive method indexes

      PositiveIndex := -1;

      for J := Id to Card do
      begin
        RJ := Records[J];

        if RJ = SR0 then
          break;

        if RJ.Kind = kindNAMESPACE then
          break;

        if (RJ.Level = Id) and (RJ.Kind = kindSUB) and
           (RJ.NegativeMethodIndex < 0) then
           begin
             if PositiveIndex = -1 then
               PositiveIndex := RestorePositiveIndex(Id);

             if PositiveIndex = -1 then
               RaiseError(errInternalError, []);
             RJ.MethodIndex := Abs(RJ.NegativeMethodIndex) + PositiveIndex;
           end;
      end;

      continue;
    end;

    if RecKind = erTypeId then
    begin
      if PosCh('.', FullName) > 0 then
      begin
        J := LookupFullName(FullName, true);
        if J = 0 then
          J := LookupType(FullName, 0, true);
      end
      else
        J := LookupType(FullName, 0, true)
    end
    else
    begin
      J := LookupFullName(FullName, true);
      if J = 0 then
        J := LookupType(FullName, 0, true);
    end;

    if J > 0 then
    begin
      case RecKind of
        erTypeId: Records[Id].TypeID := J;
        erPatternId: Records[Id].PatternID := J;
        erAncestorId: Records[Id].AncestorID := J;
        erReadId: Records[Id].ReadID := J;
        erWriteId: Records[Id].WriteID := J;
      end;
    end
    else
    begin
      case RecKind of
        ePropertyInBaseClassId:
        begin
          if Assigned(CheckProc) then
            if not CheckProc(FullName, Data, RecKind) then
              continue;

          RaiseError(errPropertyDoesNotExistsInTheBaseClass, [FullName]);
        end;
        erTypeId:
        begin
          if Assigned(CheckProc) then
            if not CheckProc(FullName, Data, RecKind) then
              continue;

//          RaiseError(errTypeNotFound, [FullName]);
        end
      else
        begin
          if Assigned(CheckProc) then
            if not CheckProc(FullName, Data, RecKind) then
              continue;

          RaiseError(errUndeclaredIdentifier, [FullName]);
        end;
      end;
    end;
  end;

  if Self.St_Tag > 0 then
    TLocalSymbolTable(Self).GlobalST.ResolveExternList(CheckProc, Data);
end;

procedure TBaseSymbolTable.SaveNamespaceToFile(LevelId: Integer; const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveNamespaceToStream(LevelId, F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TBaseSymbolTable.SaveNamespaceToStream(const NamespaceName: String; S: TStream);
var
  Id: Integer;
begin
  Id := LookupNamespace(NamespaceName, 0, true);
  if Id = 0 then
    RaiseError(errUndeclaredIdentifier, [NamespaceName]);
  SaveNamespaceToStream(Id, S);
end;

procedure TBaseSymbolTable.SaveNamespaceToFile(const NamespaceName, FileName: String);
var
  Id: Integer;
begin
  Id := LookupNamespace(NamespaceName, 0, true);
  if Id = 0 then
    RaiseError(errUndeclaredIdentifier, [NamespaceName]);
  SaveNamespaceToFile(Id, FileName);
end;

procedure TBaseSymbolTable.LoadNamespaceFromFile(const FileName: String);
var
  F: TFileStream;
begin
  if not FileExists(FileName) then
    RaiseError(errFileNotFound, [FileName]);
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadNamespaceFromStream(F);
  finally
    FreeAndNil(F);
  end;
end;

procedure TBaseSymbolTable.AddScriptFields(ClassId: Integer; FieldList: TMapFieldList);
var
  I, TypeId: Integer;
  RI: TSymbolRec;
  FieldTypeName: String;
begin
  for I := ClassId + 1 to Card do
  begin
    RI := Records[I];
    if RI.Level = ClassId then
      if RI.Kind = kindTYPE_FIELD then
      begin
        TypeId := RI.TypeID;
        FieldTypeName := Records[TypeId].Name;
        FieldList.Add(RI.Name, RI.Shift, FieldTypeName);
      end;
  end;
  ClassId := Records[ClassId].AncestorId;
  if ClassId > 0 then
    if not Records[ClassId].Host then
      AddScriptFields(ClassId, FieldList);
end;

procedure TBaseSymbolTable.ExtractNamespaces(const Level: Integer; L: TStrings);
var
  I, KK, K1, K2: Integer;
  RI: TSymbolRec;
  S: String;
begin
  for KK := 1 to 2 do
  begin
    if KK = 1 then
    begin
      K1 := 1;
      if Self.st_tag = 0 then
        K2 := Card
      else
        K2 := TLocalSymbolTable(Self).GlobalST.Card;
    end
    else
    begin
      K1 := FirstLocalId + 1;
      K2 := Card;
    end;

    for I := K1 to K2 do
    begin
      RI := Records[I];
      if RI.Kind = KindNAMESPACE then
      begin
        if I = H_PascalNamespace then
          continue;
        if I = H_BasicNamespace then
          continue;
        if I = JS_JavaScriptNamespace then
          continue;
        if I = JS_TempNamespaceId then
          continue;
        if I = H_PaxCompilerFramework then
          continue;
        if I = H_PaxCompilerSEH then
          continue;

        S := RI.Name;
        L.AddObject(S, TObject(I));
      end;
    end;
  end;
end;

procedure TBaseSymbolTable.ExtractMembers(const Id: Integer; L: TStrings;
                                          Lang: TPaxLang = lngPascal;
                                          SharedOnly: Boolean = false;
                                          VisSet: TMemberVisibilitySet = [cvPublic, cvPublished]);

  function ExtractParams(R: TSymbolRec): String;
  var
    LP: TStringList;
    J: Integer;
  begin
    if R.Kind = KindPROP then
      if R.ReadId <> 0 then
        R := Records[R.ReadId];

    result := '';
    if R.Count > 0 then
    begin
      LP := TStringList.Create;
      try
        ExtractParameters(R.Id, LP, Lang);
        for J := 0 to LP.Count - 1 do
        begin
          result := result + LP[J];
          if J <> LP.Count - 1 then
            case Lang of
              lngBasic:
                result := result + ', ';
            else
              result := result + '; ';
            end;
        end;
      finally
        FreeAndNil(LP);
      end;
    end;
  end;

  function IndexOfEx(List: TStrings;
                     S: String;
                     P: Integer): Integer;
  var
    I: Integer;
  begin
    result := -1;
    S := Copy(S, 1, P);
    for I := 0 to List.Count - 1 do
      if StrEql(Copy(L[I], 1, P), S) then
      begin
        result := I;
        Exit;
      end;
  end;

var
  T, I, K, K0: Integer;
  R: TSymbolRec;
  S, P: String;
  IsNamespace: Boolean;
  PP: Integer;
begin
  IsNamespace := false;

  if Id = 0 then
  begin
    T := 0;
    IsNamespace := true;
  end
  else if Records[Id].Kind = kindNAMESPACE then
  begin
    T := Id;
    IsNamespace := true;
  end
  else
  begin
    T := Records[Id].TerminalTypeId;
    if T in [0, typeVOID] then
      Exit;
  end;

  if T > FirstLocalId then
    K := Card
  else
    K := TLocalSymbolTable(Self).GlobalST.Card;

  if Id > 0 then
  if Records[T].FinalTypeId = typeCLASSREF then
  begin
    T := Records[T].PatternId;
    SharedOnly := true;
  end;

  K0 := T;
  if Id = 0 then
  begin
    K0 := FirstLocalId;
    K := Card;
  end;

  for I:= K0 + 1 to K do
  begin
    R := Records[I];

    if R.Kind = KindNAMESPACE then
    if not IsNamespace then
      break;

    if R.Level = T then
    begin
      if IsNamespace then
        if R.InImplementation then
          continue;

      if not (R.Vis in VisSet) then
//        if not (R.Host and (R.Vis = cvNone)) then // backward compatibility only
        if R.Vis <> cvNone then // backward compatibility only
          continue;

      if SharedOnly then
        if not R.IsSharedMethod then
          continue;

      PP := -1;
      S := R.Name;
      if IsValidName(S) then
      case R.Kind of
        kindCONSTRUCTOR:
        begin
          P := ExtractParams(R);
          case Lang of
            lngBasic:
            begin
              S := 'Sub ' + R.Name + '(' + P + ')';
            end
            else
            begin
              if P = '' then
                S := 'constructor ' + R.Name + ';'
              else
                S := 'constructor ' + R.Name + '(' + P + ');';
            end;
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindDESTRUCTOR:
        begin
          case Lang of
            lngBasic:
            begin
              S := 'Sub ' + R.Name + '()';
            end;
            else
            begin
              S := 'destructor ' + R.Name + ';';
            end;
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindSUB:
        begin
          P := ExtractParams(R);

          case Lang of
            lngBasic:
            begin
              if R.TypeId in [0, typeVOID] then
                S := 'Sub ' + R.Name + '(' + P + ')'
              else
                S := 'Function ' + R.Name + '(' + P + ') As ' +
                     Records[R.TypeId].NativeName;
            end;
            else
            begin
              if R.TypeId in [0, typeVOID] then
              begin
                if P = '' then
                  S := 'procedure ' + R.Name + ';'
                else
                  S := 'procedure ' + R.Name + '(' + P + ');';
              end
              else
              begin
                if P = '' then
                  S := 'function ' + R.Name + ': ' +
                     Records[R.TypeId].NativeName + ';'
                else
                  S := 'function ' + R.Name + '(' + P + '): ' +
                     Records[R.TypeId].NativeName + ';';
              end;
            end;
          end;

          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindTYPE_FIELD:
        begin
          case Lang of
            lngBasic:
              S := 'Dim ' + R.Name + ' As ' + Records[R.TypeId].NativeName;
            else
              S := 'field ' + R.Name + ': ' + Records[R.TypeId].NativeName + ';';
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindPROP:
        begin
          P := ExtractParams(R);
          case Lang of
            lngBasic:
            begin
              if P = '' then
                S := 'Property ' + R.Name +
                  ' As ' + Records[R.TypeId].NativeName
              else
              begin
                S := 'Property ' + R.Name +
                  '(' + P + ')' +
                  ' As ' + Records[R.TypeId].NativeName;
                PP := PosCh('(', S);
              end;
            end
            else
            begin
              if P = '' then
                S := 'property ' + R.Name +
                  ': ' + Records[R.TypeId].NativeName + ';'
               else
               begin
                 S := 'property ' + R.Name +
                   '[' + P + ']' +
                   ': ' + Records[R.TypeId].NativeName + ';';
                  PP := PosCh('[', S);
               end;
             end;
          end;

          if PP = -1 then
          begin
            if L.IndexOf(S) = -1 then
              L.AddObject(S, TObject(R.Id));
          end
          else
          begin
            if IndexOfEx(L, S, PP) = -1 then
              L.AddObject(S, TObject(R.Id));
          end;

        end;
        kindVAR:
        begin
          case Lang of
            lngBasic:
              S := 'Dim ' + R.Name + ' As ' + Records[R.TypeId].NativeName;
            else
              S := 'var ' + R.Name + ': ' + Records[R.TypeId].NativeName + ';';
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindCONST:
        begin
          case Lang of
            lngBasic:
              S := 'Const ' + R.Name + ' As ' +
                    Records[R.TypeId].NativeName + '= ' +
                    ValueStr(R.Id);
            else
              S := 'const ' + R.Name + ': ' +
                    Records[R.TypeId].NativeName + '= ' +
                    ValueStr(R.Id) +
                    ';';
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
        kindTYPE:
        begin
          case Lang of
            lngBasic:
              S := 'Type ' + R.Name;
            else
              S := 'type ' + R.Name + ';';
          end;
          if L.IndexOf(S) = -1 then
            L.AddObject(S, TObject(R.Id));
        end;
      end;
    end;
  end;

  if IsFrameworkTypeId(T) then
    Exit;

  if Records[T].AncestorId > 0 then
    ExtractMembers(Records[T].AncestorId, L, lang, SharedOnly, VisSet);

//  if Records[T].FinalTypeId <> typeINTERFACE then
//    Exit;

  if Records[T].SupportedInterfaces = nil then
    Exit;

  for I:=0 to Records[T].SupportedInterfaces.Count - 1 do
    ExtractMembers(Records[T].SupportedInterfaces[I].Id, L, Lang, SharedOnly, VisSet);
end;

function TBaseSymbolTable.ValueStr(I: Integer): String;
var
  VarObject: TVarObject;
  B: Integer;
begin
  if Records[I].DefVal <> '' then
  begin
    result := Records[I].DefVal;
    if Records[I].FinalTypeId in (CharTypes + StringTypes) then
      result := '''' + result + '''';
    Exit;
  end;

  if VarType(Records[I].Value) = varEmpty then
    result := ''
  else if IsVarObject(Records[I].Value) then
  begin
    VarObject := VariantToVarObject(Records[I].Value);
    if VarObject = nil then
      result := 'nil'
    else
      result := VarObject.ToStr;
  end
  else if Records[I].FinalTypeId in CharTypes then
  begin
    B := Records[I].Value;
    result := '''' + chr(B) + '''';
  end
  else
    result := VarToStr(Records[I].Value);
  result := ReplaceCh(#0, '_', result);

  if result = '' then
    result := '''' + ''''
  else if Records[I].FinalTypeId in [typePOINTER,
                                     typeCLASS,
                                     typeCLASSREF,
                                     typeINTERFACE] then
       if result = '0' then
          result := 'nil';
end;

procedure TBaseSymbolTable.ExtractParameters(Id: Integer; L: TStrings;
                                             Lang: TPaxLang = lngPascal;
                                             SkipParameters: Integer = 0);
var
  I, J, K, Skp: Integer;
  R: TSymbolRec;
  S: String;
begin
  K := Records[Id].Count;
  J := 0;
  Skp := 0;
  for I := Id + 1 to Card do
    if IsParam(Id, I) then
    begin
      Inc(Skp);
      if Skp <= SkipParameters then
        continue
      else
      begin
        Inc(J);

        R := Records[I];
        S := R.Name;
        if not IsValidName(S) then
        begin
          S := Copy(Records[R.TypeId].Name, 1, 1);
          if L.IndexOf(S) >= 0 then
            S := S + Chr(J);
        end;

        case Lang of
          lngBasic:
          begin
            S := S + ' As ' +
                 Records[R.TypeId].NativeName;
            if R.ByRef then
              S := 'ByRef ' + S
            else if R.IsConst then
              S := 'Const ' + S;
          end;
          else
          begin
            S := S + ': ' +
                 Records[R.TypeId].NativeName;
            if R.ByRef then
              S := 'var ' + S
            else if R.IsConst then
              S := 'const ' + S;
          end;
        end;

        if R.Optional then
        begin
          S := S + '= ' + ValueStr(I);
        end;

        L.AddObject(S, TObject(R.Id));
        if J = K then
          break;
      end;
    end;
end;

procedure TBaseSymbolTable.ExtractParametersEx(Id: Integer;
                                               L: TStrings;
                                               Upcase: Boolean;
                                               SkipParameters: Integer = 0);
var
  OverList: TIntegerList;
  I: Integer;
begin
  OverList := LookUpAll(Records[Id].Name,
                        Records[Id].Level,
                        Upcase);
  try
    for I := 0 to OverList.Count - 1 do
    begin
      ExtractParameters(OverList[I], L, lngPascal, SkipParameters);
      if I <> OverList.Count - 1 then
        L.Add(PARAMS_DELIMITER);
    end;
  finally
    FreeAndNil(OverList);
  end;
end;

procedure TBaseSymbolTable.AddTypes(const TypeName: String; L: TStrings;
                                    ErrorIndex: Integer; Upcase: Boolean);
var
  I, J, K1, K2: Integer;
  List: TIntegerList;
  RI: TSymbolRec;
  ok: Boolean;
  R: TUndeclaredTypeRec;
begin
  K1 := L.Count;

  List := HashArray.GetList(TypeName);
  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    RI := Records[I];
    if RI.Kind = kindTYPE then
    begin
      if UpCase then
        ok := StrEql(RI.Name, TypeName)
      else
        ok := RI.Name = TypeName;
      if ok then
      begin
        if L.IndexOf(RI.FullName) = -1 then
        begin
          R := TUndeclaredTypeRec.Create;
          R.Id := I;
          R.ErrorIndex := ErrorIndex;
          L.AddObject(RI.FullName, R);
        end;
      end;
    end;
  end;

  K2 := L.Count;
  if K1 = K2 then
    if L.IndexOf(TypeName) = -1 then
    begin
      R := TUndeclaredTypeRec.Create;
      R.Id := 0;
      R.ErrorIndex := ErrorIndex;
      L.AddObject(TypeName, R);
    end;
end;

procedure TBaseSymbolTable.AddUndeclaredIdent(const IdentName: String; L: TStrings;
                                    ErrorIndex: Integer; Upcase: Boolean);
var
  I, J, K1, K2, Level: Integer;
  List: TIntegerList;
  RI: TSymbolRec;
  ok: Boolean;
  R: TUndeclaredIdentRec;
begin
  K1 := L.Count;

  List := HashArray.GetList(IdentName);
  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    RI := Records[I];
    if RI.Kind in (KindSUBS + [KindVAR, KindCONST, KindTYPE, KindNAMESPACE]) then
    if not RI.Param then
    begin
      if UpCase then
        ok := StrEql(RI.Name, IdentName)
      else
        ok := RI.Name = IdentName;
      if ok then
      begin
        Level := RI.Level;
        if Level > 0 then
          ok := Records[Level].Kind = KindNAMESPACE;
      end;
      if ok then
      begin
        if L.IndexOf(RI.FullName) = -1 then
        begin
          R := TUndeclaredIdentRec.Create;
          R.Id := I;
          R.ErrorIndex := ErrorIndex;
          L.AddObject(RI.FullName, R);
        end;
      end;
    end;
  end;

  K2 := L.Count;
  if K1 = K2 then
    if L.IndexOf(IdentName) = -1 then
    begin
      R := TUndeclaredIdentRec.Create;
      R.Id := 0;
      R.ErrorIndex := ErrorIndex;
      L.AddObject(IdentName, R);
    end;
end;

procedure TBaseSymbolTable.CreateInterfaceMethodList(IntfId: Integer;
                                                     L: TIntegerList);
var
  I: Integer;
  R: TSymbolRec;
begin
  repeat
    IntfId := Records[IntfId].TerminalTypeId;
    if IntfId = H_IUnknown then
    begin
      L.Add(H_QueryInterface);
      L.Add(H_AddRef);
      L.Add(H_Release);
      break;
    end;

    for I:= IntfId + 1 to Card do
    begin
      R := Records[I];
      if R.Level = IntfId then
        if R.Kind = kindSUB then
          L.Add(I);
    end;
    if Records[IntfId].SupportedInterfaces = nil then
      break;
    if Records[IntfId].SupportedInterfaces.Count = 0 then
      break;

    IntfId := Records[IntfId].SupportedInterfaces[0].Id;

  until false;
end;

procedure TBaseSymbolTable.CreateInterfaceMethodList(ClassId, IntfId: Integer;
                                                     InterfaceMethodIds,
                                                     ClassMethodIds: TIntegerList);
var
  I, J, Id, MethodIndex: Integer;
  R, RR: TSymbolRec;
  Buff: array[0..1000] of Integer;
begin
  InterfaceMethodIds.Clear;
  ClassMethodIds.Clear;
  CreateInterfaceMethodList(IntfId, InterfaceMethodIds);
  FillChar(Buff, SizeOf(Buff), 0);
  repeat

    I := Card;
    while I > ClassId do
    begin
      R := Records[I];

      if st_tag > 0 then
      if R = SR0 then
      begin
        I := TLocalSymbolTable(Self).GlobalST.Card;
        R := Records[I];
      end;

      if R.Level = ClassId then
        if R.Kind = kindSUB then
        begin
          for J := InterfaceMethodIds.Count - 1 downto 0 do
          begin
            Id := InterfaceMethodIds[J];
            RR := Records[Id];
            if StrEql(R.Name, RR.Name) then
            if StrEql(R.Signature, RR.Signature) then
            begin
              InterfaceMethodIds.RemoveAt(J);

              MethodIndex := RR.MethodIndex;

              Buff[MethodIndex] := I;

              R.PatternId := RR.Id;

              break;
            end;
          end;
        end;
      Dec(I);
    end;

    if Records[ClassId].AncestorId = 0 then
      break;

    if Records[ClassId].AncestorId = H_TObject then
      break;

    ClassId := Records[ClassId].AncestorId;

  until false;

  if InterfaceMethodIds.Count > 0 then
    Exit;

  for I:=1 to 1000 do
  begin
    Id := Buff[I];

    if Id = 0 then
      break;

    ClassMethodIds.Add(Id);
  end;
end;

type
  TScriptClassRec = class
  public
    ClassId: Integer;
    AncestorId: Integer;
    Processed: Boolean;
    PClass: TClass;
    VirtualMethodList: TIntegerList;
    constructor Create;
    destructor Destroy; override;
  end;

  TScriptClassList = class(TTypedList)
  private
    function GetRecord(I: Integer): TScriptClassRec;
  public
    procedure Reset;
    function Add: TScriptClassRec;
    function FindClass(ClassId: Integer): TScriptClassRec;
    property Records[I: Integer]: TScriptClassRec read GetRecord; default;
  end;

//--  TScriptClassRec ----------------------------------------------------------

constructor TScriptClassRec.Create;
begin
  inherited;
  Processed := false;
  VirtualMethodList := TIntegerList.Create;
end;

destructor TScriptClassRec.Destroy;
begin
  FreeAndNil(VirtualMethodList);
  inherited;
end;

//--  TScriptClassList ---------------------------------------------------------

function TScriptClassList.Add: TScriptClassRec;
begin
  result := TScriptClassRec.Create;
  L.Add(result);
end;

procedure TScriptClassList.Reset;
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    Records[I].Processed := false;
end;

function TScriptClassList.FindClass(ClassId: Integer): TScriptClassRec;
var
  I: Integer;
begin
  result := nil;
  for I:=0 to Count - 1 do
    if Records[I].ClassId = ClassId then
    begin
      result := Records[I];
      Exit;
    end;
end;

function TScriptClassList.GetRecord(I: Integer): TScriptClassRec;
begin
  result := TScriptClassRec(L[I]);
end;

procedure TBaseSymbolTable.ProcessClassFactory(AClassFactory: Pointer;
                                               AProg: Pointer);

  var
    ScriptClassList: TScriptClassList;

  procedure BuildScriptClassList;
  var
    I, LevelId: Integer;
    R: TSymbolRec;
    ScriptClassRec: TScriptClassRec;
  begin
    for I:=FirstLocalId + 1 to Card do
    begin
      R := Records[I];

      if R.ClassIndex <> -1 then
      begin
        if R.Host then
          continue;

        if ScriptClassList.FindClass(R.Id) = nil then
        begin
          ScriptClassRec := ScriptClassList.Add;
          ScriptClassRec.ClassId := R.Id;
          ScriptClassRec.PClass := R.PClass;
          ScriptClassRec.AncestorId := R.AncestorId;
          continue;
        end;
      end;

      if R.Kind in [kindSUB, kindCONSTRUCTOR] then
        if (R.CallMode > 0) and (R.DynamicMethodIndex = 0) then
        begin
          LevelId := R.Level;

          if LevelId = 0 then
            continue;

          if Records[LevelId].Host then
            continue;

          if Records[LevelId].FinalTypeId <> typeCLASS then
            continue;

          ScriptClassRec := ScriptClassList.FindClass(LevelId);
          if ScriptClassRec = nil then
          begin
            ScriptClassRec := ScriptClassList.Add;
            ScriptClassRec.ClassId := LevelId;
            ScriptClassRec.PClass := Records[LevelId].PClass;
            ScriptClassRec.AncestorId := Records[LevelId].AncestorId;
          end;
          ScriptClassRec.VirtualMethodList.Add(R.Id);
        end;
    end;
  end;

var
  ClassFactory: TPaxClassFactory;
  ClassFactoryRec: TPaxClassFactoryRec;
  I, J, SubId, CallMode: Integer;
  RR: TSymbolRec;
  C, CA: TClass;
  ScriptClassRec, AncestorScriptClassRec: TScriptClassRec;
  VirtualMethodList: TIntegerList;
  b: Boolean;
  P, Q: PPointerArray;
  MethodIndex: Integer;
  Prog: TBaseRunner;
  MapRec: TMapRec;
  Name, FullName, Signature: String;
  Address, Adr: Pointer;
  OverCount: Integer;
  II, JJ, temp: Integer;
  Found: Boolean;
  MR, SomeMR: TMapRec;
  FileName, ProcName: String;
  DestProg: Pointer;
  T: Integer;
begin
  ClassFactory := TPaxClassFactory(AClassFactory);
  Prog := TBaseRunner(AProg);

  Prog.ForceMappingEvents;

  ClassFactory.SetupParents(Prog, Prog.ClassList);
  ClassFactory.AddInheritedMethods;

  ScriptClassList := TScriptClassList.Create;
  try
    BuildScriptClassList;

    repeat
      b := false;

      for I:=0 to ScriptClassList.Count - 1 do
      begin
        ScriptClassRec := ScriptClassList[I];
        if ScriptClassRec.Processed then
          continue;

        if Records[ScriptClassRec.AncestorId].Host then
        begin
          ScriptClassRec.Processed := true;
          b := true;
        end
        else
        begin
          AncestorScriptClassRec := ScriptClassList.FindClass(ScriptClassRec.AncestorId);
          if AncestorScriptClassRec.Processed then
          begin
            ScriptClassRec.Processed := true;
            b := true;
          end
          else
            continue;
        end;

        C := Records[ScriptClassRec.ClassId].PClass;
        P := GetVArray(C);

        VirtualMethodList := ScriptClassRec.VirtualMethodList;
        for J:=0 to VirtualMethodList.Count - 1 do
        begin
          SubId := VirtualMethodList[J];
          CallMode := Records[SubId].CallMode;
          Name := Records[SubId].Name;
          FullName := Records[SubId].FullName;
          Signature := Records[SubId].Signature;
          OverCount := Records[SubId].OverCount;

          Address := Prog.GetAddressEx(FullName, OverCount, MR);

          DestProg := Prog;
          if Address = nil then
          begin
            FileName := ExtractOwner(FullName) + '.' + PCU_FILE_EXT;
            ProcName := Copy(FullName, PosCh('.', FullName) + 1, Length(FullName));
            Address := Prog.LoadAddressEx(FileName, ProcName, false, OverCount, SomeMR, DestProg);
          end;

          if CallMode = cmVIRTUAL then
          begin
            TBaseRunner(DestProg).WrapMethodAddress(Address);

            MethodIndex := VirtualMethodIndex(C, Address);
            if MethodIndex = -1 then
              MethodIndex := GetVirtualMethodCount(C) + 1
            else
              Inc(MethodIndex); // method index starts from 1, not 0

            Records[SubId].MethodIndex := MethodIndex;
            P^[MethodIndex - 1] := Address;

            MapRec := Prog.ScriptMapTable.LookupEx(FullName, OverCount);
            if MapRec <> nil then
              MapRec.SubDesc.MethodIndex := MethodIndex;

            for II:=0 to ScriptClassList.Count - 1 do
            begin
              CA := ScriptClassList[II].PClass;
              if CA.InheritsFrom(C) and (C <> CA) then
              begin
                Found := false;
                for JJ:=0 to ScriptClassList[II].VirtualMethodList.Count - 1 do
                begin
                  temp := ScriptClassList[II].VirtualMethodList[JJ];
                  RR := Records[temp];
                  if RR.MethodIndex = 0 then
                    if StrEql(Name, RR.Name) then
                      if StrEql(Signature, RR.Signature) then
                      begin
                        Found := true;

                        RR.MethodIndex := MethodIndex;
                        Adr := Prog.GetAddress(RR.FullName, MR);
                        Prog.WrapMethodAddress(Adr);
                        Q := GetVArray(CA);
                        Q^[MethodIndex - 1] := Adr;

                        MapRec := Prog.ScriptMapTable.LookupEx(RR.FullName, RR.OverCount);
                        if MapRec <> nil then
                          MapRec.SubDesc.MethodIndex := MethodIndex;

                        break;
                      end;
                end;

                if not Found then
                begin
                  Q := GetVArray(CA);
                  Q^[MethodIndex - 1] := Address;
                end;
              end;
            end;
          end;
        end;
      end;

      if b = false then
        break;

    until false;

    // process overriden methods

    ScriptClassList.Reset;

    repeat
      b := false;

      for I:=0 to ScriptClassList.Count - 1 do
      begin
        ScriptClassRec := ScriptClassList[I];
        if ScriptClassRec.Processed then
          continue;

        if Records[ScriptClassRec.AncestorId].Host then
        begin
          ScriptClassRec.Processed := true;
          b := true;
        end
        else
        begin
          AncestorScriptClassRec := ScriptClassList.FindClass(ScriptClassRec.AncestorId);
          if AncestorScriptClassRec.Processed then
          begin
            ScriptClassRec.Processed := true;
            b := true;
          end
          else
            continue;
        end;

        C := Records[ScriptClassRec.ClassId].PClass;
        P := GetVArray(C);

        VirtualMethodList := ScriptClassRec.VirtualMethodList;
        for J:=0 to VirtualMethodList.Count - 1 do
        begin
          SubId := VirtualMethodList[J];
          CallMode := Records[SubId].CallMode;
          Name := Records[SubId].Name;
          FullName := Records[SubId].FullName;
          Signature := Records[SubId].Signature;
          OverCount := Records[SubId].OverCount;
          Address := Prog.GetAddressEx(FullName, OverCount, MR);

          Prog.WrapMethodAddress(Address);

          if CallMode = cmOVERRIDE then
          begin
            MethodIndex := Records[SubId].MethodIndex;
            if MethodIndex = 0 then
            begin
              temp := LookupParentMethod(SubId, true, true);
              if temp = 0 then
                if Records[SubId].DynamicMethodIndex = 0 then
                  RaiseError(errInternalError, []);
              if Records[temp].MethodIndex = 0 then
              begin
{$IFDEF UNIC}
                if StrEql(Records[SubId].Name, 'toString') then
                begin
                  T := Records[SubId].Level;
                  ClassFactoryRec := ClassFactory.FindRecordByFullName(Records[T].FullName);
                  if ClassFactoryRec = nil then
                    RaiseError(errInternalError, []);
                  vmtToStringSlot(ClassFactoryRec.VMTPtr)^ := Address;
                  continue;
                end
                else if StrEql(Records[SubId].Name, 'GetHashCode') then
                begin
                  T := Records[SubId].Level;
                  ClassFactoryRec := ClassFactory.FindRecordByFullName(Records[T].FullName);
                  if ClassFactoryRec = nil then
                    RaiseError(errInternalError, []);
                  vmtGetHashCodeSlot(ClassFactoryRec.VMTPtr)^ := Address;
                  continue;
                end
                else if StrEql(Records[SubId].Name, 'Equals') then
                begin
                  T := Records[SubId].Level;
                  ClassFactoryRec := ClassFactory.FindRecordByFullName(Records[T].FullName);
                  if ClassFactoryRec = nil then
                    RaiseError(errInternalError, []);
                  vmtEqualsSlot(ClassFactoryRec.VMTPtr)^ := Address;
                  continue;
                end;
{$ENDIF}
                RaiseError(errInternalError, []);
              end;
              MethodIndex := Records[temp].MethodIndex;
              Records[SubId].MethodIndex := MethodIndex;
            end;

            P^[MethodIndex - 1] := Address;

            MapRec := Prog.ScriptMapTable.LookupEx(FullName, OverCount);
            if MapRec <> nil then
              MapRec.SubDesc.MethodIndex := MethodIndex;

            for II:=0 to ScriptClassList.Count - 1 do
            begin
              CA := ScriptClassList[II].PClass;
              if CA.InheritsFrom(C) and (C <> CA) then
              begin
                Found := false;
                for JJ:=0 to ScriptClassList[II].VirtualMethodList.Count - 1 do
                begin
                  temp := ScriptClassList[II].VirtualMethodList[JJ];
                  RR := Records[temp];
                  if RR.MethodIndex = 0 then
                    if StrEql(Name, RR.Name) then
                      if StrEql(Signature, RR.Signature) then
                      begin
                        Found := true;
                        RR.MethodIndex := MethodIndex;
                        Adr := Prog.GetAddress(RR.FullName, MR);
                        Prog.WrapMethodAddress(Adr);
                        Q := GetVArray(CA);
                        Q^[MethodIndex - 1] := Adr;

                        MapRec := Prog.ScriptMapTable.LookupEx(RR.FullName, RR.OverCount);
                        if MapRec <> nil then
                          MapRec.SubDesc.MethodIndex := MethodIndex;

                        break;
                      end;
                end;
                if not Found then
                begin
                  Q := GetVArray(CA);
                  Q^[MethodIndex - 1] := Address;
                end;
              end;
            end;
          end;
        end;
      end;

      if b = false then
        break;

    until false;

  finally
    FreeAndNil(ScriptClassList);
  end;
end;

function TBaseSymbolTable.RegisterSpace(K: Integer): Integer;
var
  I: Integer;
begin
  result := Card;
  for I:=1 to K do
    AddRecord;
end;

procedure TBaseSymbolTable.HideClass(C: TClass);
var
  I: Integer;
  R: TSymbolRec;
begin
  for I := 1 to Card do
  begin
    R := Records[I];
    if R = SR0 then
      continue;
    if R.PClass <> nil then
      if R.PClass = C then
        if R.Kind = KindTYPE then
        begin
          R.Name := '@' + R.Name;
          Exit;
        end;
  end;
end;

function TBaseSymbolTable.ImportFromTable(st: TBaseSymbolTable;
                                          const FullName: String;
                                          UpCase: Boolean;
                                          DoRaiseError: Boolean = true): Integer;

  function TranslateId(Id: Integer): Integer;
  var
    S: String;
  begin
    S := st[Id].FullName;
    result := LookupFullName(S, UpCase);
    if result = 0 then
      result := ImportFromTable(st, S, UpCase);
  end;

var
  I, J, Id, Id1, Id2, FinTypeId, Kind, LevelId, TypeBaseId,
  OriginTypeId, FieldTypeId, RangeTypeId, ElemTypeId: Integer;
  S, TypeName: String;
  RI: TSymbolRec;
  B1, B2: Integer;
  ResTypeId, ParamId, ParamTypeId, H_Sub, OriginId: Integer;
  PClass: TClass;
  GUID: TGUID;
  D: packed record
       D1, D2: Double;
     end;
  IsGlobalMember: Boolean;
  MethodClass: TClass;
  MethodIndex: Integer;
  OverList: TIntegerList;
begin
  result := 0;
  Id := st.LookupFullName(FullName, UpCase);

  if id = 0 then
  begin
    if DoRaiseError then
      RaiseError(errUndeclaredIdentifier, [FullName]);
    Exit;
  end;

  MethodClass := nil;

  LevelId := st[Id].Level;
  if LevelId > 0 then
  begin
    IsGlobalMember := st[LevelId].Kind = kindNAMESPACE;
    if not IsGlobalMember then
      if st[LevelId].FinalTypeId = typeCLASS then
        MethodClass := st[LevelId].PClass;

    LevelId := TranslateId(LevelId);
  end
  else
    IsGlobalMember := true;

  Kind := st[id].Kind;
  FinTypeId := st[Id].FinalTypeId;
  case Kind of
    kindTYPE:
      if FinTypeId in (StandardTypes - [typeENUM]) then
      begin
        TypeName := ExtractName(FullName);
        if st[id].PatternId = 0 then // this is a subrange type
        begin
          Id1 := st.GetLowBoundRec(id).Id;
          Id2 := st.GetHighBoundRec(id).Id;

          B1 := st[Id1].Value;
          B2 := st[Id2].Value;
          TypeBaseId := TranslateId(st[id].TypeID);

          result := RegisterSubrangeType(LevelId, TypeName,
                               TypeBaseId,
                               B1, B2);
        end
        else // this is an alias type
        begin
          OriginTypeId := TranslateId(st[id].PatternId);
          result := RegisterTypeAlias(LevelId, TypeName, OriginTypeId);
        end;
      end
      else if FinTypeId = typePOINTER then
      begin
        TypeName := ExtractName(FullName);
        OriginTypeId := TranslateId(st[id].PatternId);
        result := RegisterPointerType(LevelId, TypeName, OriginTypeId);
      end
      else if FinTypeId = typeRECORD then
      begin
        TypeName := ExtractName(FullName);
        result := RegisterRecordType(LevelId, TypeName, st[Id].DefaultAlignment);
        for I := Id + 1 to st.Card do
        begin
          RI := st[I];
          if RI = SR0 then
            break;
          if RI.Kind = kindTYPE_FIELD then
            if RI.Level = Id then
             begin
               FieldTypeId := TranslateId(RI.TypeID);
               RegisterTypeField(result, RI.Name, FieldTypeId, RI.Shift);
             end;
        end;
      end // typeRECORD
      else if FinTypeId = typeARRAY then
      begin
        TypeName := ExtractName(FullName);
        st.GetArrayTypeInfo(id, RangeTypeId, ElemTypeId);
        RangeTypeId := TranslateId(RangeTypeId);
        ElemTypeId := TranslateId(ElemTypeId);
        result := RegisterArrayType(LevelId, TypeName,
          RangeTypeId, ElemTypeId, st[Id].DefaultAlignment);
      end // type ARRAY
      else if FinTypeId = typeDYNARRAY then
      begin
        TypeName := ExtractName(FullName);
        ElemTypeId := TranslateId(st[id].PatternId);
        result := RegisterDynamicArrayType(LevelId, TypeName, ElemTypeId);
      end
      else if FinTypeId = typeENUM then
      begin
        TypeName := ExtractName(FullName);
        TypeBaseId := TranslateId(st[id].PatternId);
        result := RegisterEnumType(LevelId, TypeName, TypeBaseId);
        for I := Id + 1 to st.Card do
        begin
          RI := st[I];
          if RI = SR0 then
            break;
          if RI.Kind = KindTYPE then
            break;
          if RI.Kind = kindCONST then
            if RI.TypeId = Id then
               RegisterEnumValue(result, RI.Name, RI.Value);
        end;
      end // typeENUM
      else if FinTypeId = typePROC then
      begin
        TypeName := ExtractName(FullName);
        OriginId := TranslateId(st[id].PatternId);
        result := RegisterProceduralType(LevelId, TypeName, Records[OriginId].Shift);
      end
      else if FinTypeId = typeEVENT then
      begin
        TypeName := ExtractName(FullName);
        OriginId := TranslateId(st[id].PatternId);
        result := RegisterEventType(LevelId, TypeName, Records[OriginId].Shift);
      end
      else if FinTypeId = typeSET then
      begin
        TypeName := ExtractName(FullName);
        OriginTypeId := TranslateId(st[id].PatternId);
        result := RegisterSetType(LevelId, TypeName, OriginTypeId);
      end
      else if FinTypeId = typeCLASSREF then
      begin
        TypeName := ExtractName(FullName);
        OriginTypeId := TranslateId(st[id].PatternId);
        result := RegisterClassReferenceType(LevelId, TypeName, OriginTypeId);
      end
      else if FinTypeId = typeCLASS then
      begin
        PClass := st[id].PClass;
        result := RegisterClassType(LevelId, PClass);
        for I := Id + 1 to st.Card do
        begin
          RI := st[I];
          if RI = SR0 then
            break;
          if RI.Kind = kindTYPE_FIELD then
            if RI.Level = Id then
             begin
               FieldTypeId := TranslateId(RI.TypeID);
               RegisterTypeField(result, RI.Name, FieldTypeId, RI.Shift);
             end;
        end;
      end
      else if FinTypeId = typeINTERFACE then
      begin
        TypeName := ExtractName(FullName);
        D.D1 := st[Id + 1].Value;
        D.D2 := st[Id + 2].Value;
        Move(D, GUID, SizeOf(GUID));
        result := RegisterInterfaceType(LevelId, TypeName, GUID);
      end
      else
        RaiseError(errInternalError, []);
       // kindTYPE
    kindSUB:
    begin
      S := ExtractName(FullName);
      ResTypeId := TranslateId(st[Id].TypeID);

      OverList := TIntegerList.Create;
      try
        OverList := st.LookUpSub(S,
                                 LevelId,
                                 UpCase);
        for J := 0 to OverList.Count - 1 do
        begin
          Id := OverList[J];

          if IsGlobalMember then
          begin
            H_Sub := RegisterRoutine(LevelId, S, ResTypeId,
              st[Id].CallConv, st[id].Address);
            result := LastSubId;
          end
          else
          begin
            H_Sub := RegisterMethod(LevelId, S, ResTypeId, st[Id].CallConv,
                           st[Id].Address, st[Id].IsSharedMethod,
                           st[Id].CallMode,
                           st[Id].MethodIndex);
            result := LastSubId;
            Self[LastSubId].IsFakeMethod := st[id].IsFakeMethod;
            if MethodClass <> nil then
            begin
              MethodIndex := VirtualMethodIndex(MethodClass,
                st[Id].Address) + 1;
              if MethodIndex > 0 then
                Records[LastSubId].MethodIndex := MethodIndex;
            end;
            Records[LastSubId].CallMode := st[id].CallMode;
          end;
          Records[result].OverCount := st[id].OverCount;
          for I := 0 to st[id].Count - 1 do
          begin
            ParamId := st.GetParamId(id, I);
            ParamTypeId := TranslateId(st[ParamId].TypeId);
            RegisterParameter(H_Sub, ParamTypeId, st[ParamId].Value,
              st[ParamId].ByRef, st[ParamId].Name);
            Records[Card].IsConst := st[ParamId].IsConst;
            Records[Card].IsOpenArray := st[ParamId].IsOpenArray;
            Records[Card].Optional := st[ParamId].Optional;
          end;
        end;
      finally
        FreeAndNil(OverList);
      end;
    end; // KindSUB
    KindCONSTRUCTOR:
    begin
      S := ExtractName(FullName);

      OverList := TIntegerList.Create;
      try
        OverList := st.LookUpSub(S,
                                 LevelId,
                                 UpCase);
        for J := 0 to OverList.Count - 1 do
        begin
          Id := OverList[J];
          H_Sub := RegisterConstructor(LevelId, S, st[id].Address,
                                                   st[id].IsSharedMethod,
                                                   st[Id].CallConv);
          result := LastSubId;
          if MethodClass <> nil then
          begin
            MethodIndex := VirtualMethodIndex(MethodClass, st[id].Address) + 1;
            if MethodIndex > 0 then
              Records[result].MethodIndex := MethodIndex;
          end;
          Records[result].OverCount := st[id].OverCount;
          for I := 0 to st[id].Count - 1 do
          begin
            ParamId := st.GetParamId(id, I);
            ParamTypeId := TranslateId(st[ParamId].TypeId);
            RegisterParameter(H_Sub, ParamTypeId, st[ParamId].Value,
              st[ParamId].ByRef, st[ParamId].Name);
            Records[Card].IsConst := st[ParamId].IsConst;
            Records[Card].IsOpenArray := st[ParamId].IsOpenArray;
            Records[Card].Optional := st[ParamId].Optional;
          end;
        end;
      finally
        FreeAndNil(OverList);
      end;
    end;
    KindDESTRUCTOR:
    begin
      S := ExtractName(FullName);
      RegisterMethod(LevelId, S, TypeVOID,
                st[id].CallConv, st[id].Address, false,
                cmOVERRIDE,
                0);
      result := LastSubId;
      Records[result].Kind := kindDESTRUCTOR;
    end;
    KindVAR:
    begin
      S := ExtractName(FullName);
      result := RegisterVariable(LevelId, S,
        TranslateId(st[Id].TypeID), st[id].Address);
    end;
    KindCONST:
    begin
      S := ExtractName(FullName);
      result := RegisterConstant(LevelId, S,
        TranslateId(st[Id].TypeID), st[id].Value);
    end;
  end;
end;

function TBaseSymbolTable.GetOpenArrayHighId(Id: Integer): Integer;
begin
  if Records[Id].Kind = KindVAR then
    Id := Records[Id].TypeId;
  result := Id;
  while Records[result].Kind <> KindVAR do
    Inc(result);
end;

function TBaseSymbolTable.GetOuterThisId(TypeId: Integer): Integer;
var
  I: Integer;
  R: TSymbolRec;
begin
  result := 0;
  CheckError(TypeId = 0);
  if Records[TypeId].Kind <> KindTYPE then
    Exit;
  if Records[TypeId].FinalTypeId <> typeCLASS then
    Exit;

  for I := TypeId + 1 to Card do
  begin
    R := Records[I];
    if R.Level = TypeId then
      if R.Kind = KindTYPE_FIELD then
        if R.Name = StrOuterThis then
        begin
          result := I;
          Exit;
        end;
  end;
end;

function TBaseSymbolTable.HasAbstractAncestor(ClassId: Integer): Boolean;
begin
  result := false;
  repeat
    ClassId := Records[ClassId].AncestorId;
    if ClassId = 0 then
      Exit;
    if Records[ClassId].IsAbstract then
    begin
      result := true;
      break;
    end;
  until false;
end;

function TBaseSymbolTable.GetTypeParameters(Id: Integer): TIntegerList;
var
  I: Integer;
  R: TSymbolRec;
  b: Boolean;
begin
  result := TIntegerList.Create;
  R := Records[Id];
  if R.Kind = KindTYPE then
  begin
    if not R.IsGeneric then
      Exit;
    b := false;
    for I := Id + 1 to Card do
    begin
      R := Records[I];
      if (R.Kind = KindTYPE) and (R.TypeId = typeTYPEPARAM) then
      begin
        b := true;
        result.Add(I);
      end
      else if b then
        Exit;
    end;
  end
  else if R.Kind in KindSUBS then
  begin
    if not Records[R.Level].IsGeneric then
      Exit;
    b := false;
    for I := Id + 1 to Card do
    begin
      R := Records[I];
      if (R.Kind = KindTYPE) and (R.TypeId = typeTYPEPARAM) then
      begin
        b := true;
        result.Add(I);
      end
      else if b then
        Exit;
    end;
  end;
end;

function TBaseSymbolTable.ExtractEnumNames(EnumTypeId: Integer): TStringList;
var
  I: Integer;
  R: TSymbolRec;
begin
  result := TStringList.Create;
  for I:=EnumTypeId + 1 to Card do
  begin
    R := Records[I];
    if R = SR0 then
      break;
    if R.Kind in [KindTYPE, KindSUB, KindNAMESPACE] then
      break;
    if R.TypeId = EnumTypeId then
      if R.Name <> '' then
        result.Add(R.Name);
  end;
end;

function IsFrameworkTypeId(Id: Integer): Boolean;
begin
  result := (Id = H_TFW_Boolean) or
            (Id = H_TFW_ByteBool) or
            (Id = H_TFW_WordBool) or
            (Id = H_TFW_LongBool) or
            (Id = H_TFW_Byte) or
            (Id = H_TFW_SmallInt) or
            (Id = H_TFW_ShortInt) or
            (Id = H_TFW_Word) or
            (Id = H_TFW_Cardinal) or
            (Id = H_TFW_Double) or
            (Id = H_TFW_Single) or
            (Id = H_TFW_Extended) or
            (Id = H_TFW_Currency) or
            (Id = H_TFW_AnsiChar) or
            (Id = H_TFW_WideChar) or
            (Id = H_TFW_Integer) or
            (Id = H_TFW_Int64) or
            (Id = H_TFW_Variant) or
            (Id = H_TFW_DateTime) or
            (Id = H_TFW_AnsiString) or
            (Id = H_TFW_UnicString) or
            (Id = H_TFW_Array);
end;

function TBaseSymbolTable.GetSizeOfPointer: Integer;
begin
  result := SizeOf(Pointer);
  if Types.PAX64 then
    result := 8;
end;

function TBaseSymbolTable.GetSizeOfTMethod: Integer;
begin
  result := SizeOfPointer * 2;
end;

procedure TBaseSymbolTable.SetPAX64(value: Boolean);
var
  I: Integer;
  R: TSymbolRec;
begin
  Types.PAX64 := value;
  if value then
  begin
    H_ExceptionPtr := H_ExceptionPtr_64;
    H_ByteCodePtr := H_ByteCodePtr_64;
    H_Flag := H_Flag_64;
    H_SkipPop := H_SkipPop_64;
    FirstShiftValue := FirstShiftValue_64;
  end
  else
  begin
    H_ExceptionPtr := H_ExceptionPtr_32;
    H_ByteCodePtr := H_ByteCodePtr_32;
    H_Flag := H_Flag_32;
    H_SkipPop := H_SkipPop_32;
    FirstShiftValue := FirstShiftValue_32;
  end;
  for I := Types.Count + 1 to Card do
  begin
    R := Records[I];
    if R <> SR0 then
    if R.Shift <> 0 then
    begin
      R.Completed := false;
      R.FinSize := -1;
    end;
  end;
end;

function TBaseSymbolTable.LookupAnonymousInterface(ClassId: Integer): Integer;
var
  I, J, SubId: Integer;
  List: TIntegerList;
  S: String;
begin
  result := 0;
  SubId := 0;
  for I := ClassId + 1 to Card do
  begin
    if Records[I].Kind = KindSUB then
       if Records[I].Level = ClassId then
       begin
         SubId := I;
         break;
       end;
  end;
  if SubId = 0 then
    Exit;
  if Records[SubId].Name <> ANONYMOUS_METHOD_NAME then
    RaiseError(errInternalError, []);

  S := UpperCase(Records[SubId].SignatureEx);

  List := HashArray.GetList(ANONYMOUS_METHOD_NAME);

  for J := List.Count - 1 downto 0 do
  begin
    I := List[J];
    if I = SubId then
      continue;
    with Records[I] do
    begin
      if Kind <> KindSUB then
        continue;
      if Level = 0 then
        continue;
      if Name <> ANONYMOUS_METHOD_NAME then
        continue;
      if Records[Level].FinalTypeId <> typeINTERFACE then
        continue;
      if UpperCase(Records[I].SignatureEx) <> S then
        continue;

      result := Level;
      Exit;
    end;
  end;
end;

function TBaseSymbolTable.LookupAnonymousMethod(IntfId: Integer): Integer;
var
  I: Integer;
begin
  result := 0;
  for I := IntfId + 1 to Card do
  begin
    if Records[I].Kind = KindSUB then
       if Records[I].Level = IntfId then
       begin
         if Records[I].Name = ANONYMOUS_METHOD_NAME then
           result := I;
         break;
       end;
  end;
end;

function TBaseSymbolTable.GetTypeHelpers(TypeId: Integer): TIntegerList;

  procedure Collect(T: Integer);
  var
    I: Integer;
  begin
    for I := TypeHelpers.Count - 1 downto 0 do
      if TypeHelpers.Values[I] = T then
        result.Add(TypeHelpers.Keys[I]);
  end;

begin
  result := TIntegerList.Create(true);
  Collect(TypeId);
  if Records[TypeId].HasPAnsiCharType or Records[TypeId].HasPWideCharType then
  begin
{$IFNDEF PAXARM}
    Collect(typeANSISTRING);
    Collect(typeWIDESTRING);
    Collect(typeSHORTSTRING);
{$ENDIF}
    Collect(typeUNICSTRING);
  end;
end;

end.
