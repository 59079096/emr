////////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxRegister.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxRegister;
interface
uses {$I uses.def}
{$ifdef DRTTI}
  RTTI,
  PAXCOMP_2010,
  PAXCOMP_2010REG,
{$ENDIF}
  SysUtils,
  Classes,
  TypInfo,
  PAXCOMP_CONSTANTS,
  PAXCOMP_TYPES,
  PAXCOMP_SYS,
  PAXCOMP_FORBID,
  PAXCOMP_SYMBOL_REC,
  PAXCOMP_BASESYMBOL_TABLE,
  PAXCOMP_LOCALSYMBOL_TABLE,
  PAXCOMP_STDLIB;

const
  _ccSTDCALL = 1;
  _ccREGISTER = 2;
  _ccCDECL = 3;
  _ccPASCAL = 4;
  _ccSAFECALL = 5;
  _ccMSFASTCALL = 6;

  _visPUBLIC = 0;
  _visPROTECTED = 1;
  _visPRIVATE = 2;

  _cmNONE = 0;
  _cmVIRTUAL = 1;
  _cmOVERRIDE = 2;
  _cmDYNAMIC = 3;
  _cmSTATIC = 4;

  _parVal = 0;
  _parVar = 1;
  _parConst = 2;
  _parOut = 3;

  _typeVOID = 1;
  _typeBOOLEAN = 2;
  _typeBYTE = 3;
  _typeANSICHAR = 4;
  _typeANSISTRING = 5;
  _typeWORD = 6;
  _typeINTEGER = 7;
  _typeDOUBLE = 8;
  _typePOINTER = 9;
  _typeRECORD = 10;
  _typeARRAY = 11;
  _typeALIAS = 12;
  _typeENUM = 13;
  _typePROC = 14;
  _typeSET = 15;
  _typeSHORTSTRING = 16;
  _typeSINGLE = 17;
  _typeEXTENDED = 18;
  _typeCLASS = 19;
  _typeWIDECHAR = 21;
  _typeWIDESTRING = 22;
  _typeVARIANT = 23;
  _typeDYNARRAY = 24;
  _typeINT64 = 25;
  _typeCARDINAL = 27;
  _typeCURRENCY = 29;
  _typeSMALLINT = 30;
  _typeSHORTINT = 31;
  _typeWORDBOOL = 32;
  _typeLONGBOOL = 33;
  _typeBYTEBOOL = 34;
  _typeOLEVARIANT = 35;
  _typeUNICSTRING = 36;
  _typeUINT64 = 39;

{$IFDEF UNIC}
  _typeSTRING = _typeUNICSTRING;
  _typeCHAR = _typeWIDECHAR;
{$ELSE}
  _typeSTRING = _typeANSISTRING;
  _typeCHAR = _typeANSICHAR;
{$ENDIF}

  _typePCHAR = 49;
  _typePVOID = 50;
  _typePWIDECHAR = 51;
var
  _Unassigned: Variant;

function RegisterNamespace(LevelId: Integer; const Name: String): Integer;

procedure RegisterUsingNamespace(const aNamespaceName: String); overload;
procedure RegisterUsingNamespace(aNamespaceID: Integer); overload;

procedure UnregisterUsingNamespace(aNamespaceID: Integer); overload;
procedure UnregisterUsingNamespace(const aNamespaceName: String); overload;
procedure UnregisterUsingNamespaces;

function RegisterConstant(LevelId: Integer; const Name: String; TypeId: Integer;
                          const Value: Variant): Integer; overload;
function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Variant): Integer; overload;
function RegisterPointerConstant(LevelId: Integer; const Name: String;
                          const Value: Pointer): Integer; overload;
function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Extended): Integer; overload;
function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Int64): Integer; overload;
function RegisterConstant(LevelId: Integer; const Declaration: String): Integer; overload;
function RegisterVariable(LevelId: Integer;
                          const Name: String; TypeId: Integer;
                          Address: Pointer): Integer; overload;
function RegisterVariable(LevelId: Integer;
                          const Declaration: String; Address: Pointer): Integer; overload;
function RegisterObject(LevelId: Integer;
                        const ObjectName: String;
                        TypeId: Integer;
                        Address: Pointer = nil): Integer;
function RegisterVirtualObject(LevelId: Integer;
                               const ObjectName: String): Integer;
function RegisterRoutine(LevelId: Integer; const Name: String; ResultId: Integer;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer; overload;
function RegisterRoutine(LevelId: Integer; const Name, ResultType: String;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer; overload;
procedure RegisterMember(LevelId: Integer; const Name: String;
                         Address: Pointer);
function RegisterConstructor(ClassId: Integer;
                             const Name: String;
                             Address: Pointer;
                             CallConvention: Integer = _ccREGISTER;
                             IsShared: Boolean = false;
                             CallMode: Integer = cmNONE;
                             i_MethodIndex: Integer = 0;
                             OverCount: Integer = 0;
                             i_IsAbstract: Boolean = false;
                             i_AbstractMethodCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer; overload;
function RegisterDestructor(ClassId: Integer; const Name: String;
                             Address: Pointer): Integer;
function RegisterMethod(ClassId: Integer;
                        const Name: String;
                        ResultId: Integer;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer; overload;
function RegisterMethod(ClassId: Integer;
                        const Name, ResultType: String;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer; overload;
function RegisterHeader(LevelId: Integer;
                        const Header: String; Address: Pointer;
                        MethodIndex: Integer = 0;
                        Visibility: Integer = 0): Integer;
function RegisterFakeHeader(LevelId: Integer;
                        const Header: String; Address: Pointer): Integer;
function RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           ParamTypeID: Integer;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer; overload;
function RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           const ParameterType: String;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer; overload;
function RegisterParameter(LevelId: Integer; TypeId: Integer;
                           ByRef: Boolean = false): Integer; overload;
function RegisterParameter(LevelId: Integer; TypeId: Integer;
                           const DefaultValue: Variant;
                           ByRef: Boolean = false): Integer; overload;
function RegisterParameter(LevelId: Integer;
                           ParameterName: String;
                           TypeID: Integer;
                           const DefaultValue: Variant;
                           ByRef: Boolean): Integer; overload;
function RegisterParameterEx(LevelId: Integer;
                             ParameterName: String;
                             TypeID: Integer;
                             const DefaultValue: Variant;
                             ByRef: Boolean;
                             IsConst: Boolean): Integer;
function RegisterParameterEx2(LevelId: Integer; ParameterName: String;
    TypeID: Integer; const DefaultValue: Variant;
    ByRef, IsConst, IsOpenArray: Boolean): Integer;
procedure RegisterRunnerParameter(HSub: Integer);
function RegisterInterfaceType(LevelId: Integer;
                               const TypeName: String;
                               const GUID: TGUID): Integer; overload;
function RegisterInterfaceType(LevelId: Integer;
                               const TypeName: String;
                               const GUID: TGUID;
                               const ParentName: String;
                               const ParentGUID: TGUID): Integer; overload;

procedure RegisterSupportedInterface(TypeId: Integer;
                                     const SupportedInterfaceName: String;
                                     const GUID: TGUID);
function RegisterClassType(LevelId: Integer; C: TClass;
                           DoRegisterClass: Boolean = false): Integer; overload;
function RegisterClassType(LevelId: Integer; C: TClass;
                           DoRegisterClass: Boolean;
                           Reserved: Integer): Integer; overload;
function RegisterClassType(LevelId: Integer;
                           const TypeName: String; AncestorId: Integer): Integer; overload;
function RegisterClassType(LevelId: Integer;
                           const TypeName: String): Integer; overload;
function RegisterClassTypeForImporter(LevelId: Integer;
                                      C: TClass): Integer;
procedure RegisterClassTypeInfos(ClassId: Integer;
                                     C: TClass);
function RegisterClassReferenceType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer; overload;
function RegisterClassReferenceType(LevelId: Integer;
                           const TypeName: String): Integer; overload;
function RegisterClassReferenceType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
function RegisterClassHelperType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer; overload;
function RegisterClassHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
function RegisterRecordHelperType(LevelId: Integer;
                           const TypeName: String; OriginRecordId: Integer): Integer; overload;
function RegisterRecordHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
function RegisterClassTypeField(ClassTypeId: Integer; const Name: String;
                                TypeID: Integer; Offset: Integer = -1): Integer; overload;
function RegisterClassTypeField(TypeId: Integer; const Declaration: String
                                 ): Integer; overload;
function RegisterProperty(LevelId: Integer; const PropName: String;
                          PropTypeID, ReadId, WriteId: Integer;
                          IsDefault: Boolean): Integer; overload;
function RegisterProperty(LevelId: Integer; const Header: String): Integer; overload;
function RegisterInterfaceProperty(LevelId: Integer;
                                   const PropName: String;
                                   PropTypeID,
                                   ReadIndex,
                                   WriteIndex: Integer): Integer;
function RegisterRecordType(LevelId: Integer;
                            const TypeName: String;
                            IsPacked: Boolean = false): Integer;
function RegisterRecordTypeField(TypeId: Integer; const FieldName: String;
                                FieldTypeID: Integer;
                                FieldOffset: Integer = -1): Integer; overload;
function RegisterRecordTypeField(TypeId: Integer; const Declaration: String
                                 ): Integer; overload;

function RegisterVariantRecordTypeField(LevelId: Integer; const FieldName: String;
                                FieldTypeID: Integer;
                                VarCount: Int64): Integer; overload;
function RegisterVariantRecordTypeField(LevelId: Integer; const Declaration: String;
                                VarCount: Int64): Integer; overload;
function RegisterSubrangeType(LevelId: Integer;
                              const TypeName: String;
                              TypeBaseId: Integer;
                              B1, B2: Integer): Integer;

function RegisterEnumType(LevelId: Integer;
                          const TypeName: String;
                          TypeBaseId: Integer = _typeINTEGER): Integer;
function RegisterEnumValue(EnumTypeId: Integer;
                           const FieldName: String;
                           const Value: Integer): Integer;

function RegisterArrayType(LevelId: Integer;
                           const TypeName: String;
                           RangeTypeId, ElemTypeId: Integer;
                           IsPacked: Boolean = false): Integer;
function RegisterDynamicArrayType(LevelId: Integer;
                           const TypeName: String;
                           ElemTypeId: Integer): Integer;
function RegisterPointerType(LevelId: Integer;
                             const TypeName: String;
                             OriginTypeId: Integer;
                             const OriginTypeName: String = ''): Integer;
function RegisterSetType(LevelId: Integer;
                         const TypeName: String;
                         OriginTypeId: Integer): Integer;

function RegisterProceduralType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;

function RegisterMethodReferenceType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;
{$IFNDEF PAXARM}
function RegisterShortStringType(LevelId: Integer;
                                 const TypeName: String;
                                 L: Integer): Integer;
{$ENDIF}
function RegisterEventType(LevelId: Integer;
                           const TypeName: String;
                           SubId: Integer): Integer;
function RegisterRTTIType(LevelId: Integer;
                          pti: PTypeInfo): Integer;
function RegisterTypeAlias(LevelId:Integer;
                           const TypeName: String;
                           OriginTypeId: Integer): Integer; overload;
function RegisterTypeAlias(LevelId:Integer;
                           const TypeName, OriginTypeName: String): Integer; overload;
function RegisterTypeAlias(LevelId:Integer;
                           const Declaration: String): Integer; overload;

function RegisterTypeDeclaration(LevelId: Integer;
                        const Declaration: String): Integer;

function RegisterDummyType(LevelId: Integer;
                           const TypeName: String): Integer;
function RegisterSomeType(LevelId: Integer;
                          const TypeName: String): Integer;
function RegisterSpace(K: Integer): Integer;

procedure ForbidClass(C: TClass);
procedure ForbidPublishedProperty(C: TClass; const PropName: String);
procedure ForbidAllPublishedProperties(C: TClass);

function LookupTypeId(const TypeName: String): Integer;
function LookupTypeNamespaceId(const TypeName: String): Integer;
function LookupNamespace(LevelId: Integer; const NamespaceName: String;
                         CaseSensitive: Boolean): Integer; overload;
function LookupNamespace(const NamespaceName: String): Integer; overload;

procedure SaveNamespaceToStream(const NamespaceName: String;
                                S: TStream);
procedure SaveNamespaceToFile(const NamespaceName: String;
                              const FileName: String);

procedure LoadNamespaceFromStream(S: TStream);
procedure LoadNamespaceFromFile(const FileName: String);

procedure RegisterAlignment(value: Integer);
procedure SetVisibility(C: TClass; const MemberName: String; value: Integer); overload;
procedure SetVisibility(ClassId: integer;
                        const MemberName: String; value: Integer); overload;

procedure EndOfRegistration(CheckProc: TCheckProc; Data: Pointer); overload;
procedure EndOfRegistration; overload;

function CreateNewImportTable: Pointer;
procedure SetImportTable(ImportTable: Pointer);
procedure DestroyImportTable(var ImportTable: Pointer);

function LoadImportLibrary(const DllName: String): Cardinal;
function FreeImportLibrary(H: Cardinal): LongBool;

function SetImportEntry(K: Integer): Integer;
function GetImportEntry: Integer;

procedure LoadGlobalSymbolTableFromStream(Stream: TStream);
procedure LoadGlobalSymbolTableFromFile(const FileName: String);
procedure SaveGlobalSymbolTableToStream(Stream: TStream);
procedure SaveGlobalSymbolTableToFile(const FileName: String);

function FindNextVirtualMethodAddress(C: TClass; PrevAddress: Pointer): Pointer;
function RenameClassType(C: TClass; const NewName: String): Boolean;
procedure EndOfStructuredType(Id: Integer);

{$ifdef DRTTI}
function RegisterRTTIRecordType(Level: Integer; t: TRTTIRecordType): Integer;
function RegisterRTTIClassType(Level: Integer; t: TRTTIInstanceType): Integer;
function RegisterRTTIInterfaceType(Level: Integer; t: TRTTIInterfaceType): Integer;
{$endif}

var
  MustRegisterClass: Boolean = false;

implementation

uses
  PaxDllImport;

procedure ForbidPublishedProperty(C: TClass; const PropName: String);
begin
  if ForbiddenPropList = nil then
    ForbiddenPropList := TForbiddenPropList.Create;

  ForbiddenPropList.Add(C, PropName);
end;

procedure ForbidAllPublishedProperties(C: TClass);
begin
  if ForbiddenPropList = nil then
    ForbiddenPropList := TForbiddenPropList.Create;

  ForbiddenPropList.AddAll(C);
end;

procedure RaiseError(const Message: string; params: array of Const);
begin
  if RaiseE then
    raise Exception.Create(Format(Message, params))
  else
  begin
    REG_ERROR := Format(Message, params);
    REG_OK := false;

    if Message = errUndeclaredIdentifier then
      REG_ERROR := '';
  end;
end;

function RegisterNamespace(LevelId: Integer; const Name: String): Integer;
begin
  result := GlobalImportTable.RegisterNamespace(LevelId, Name);
end;

procedure RegisterUsingNamespace(const aNamespaceName: String); overload;
Var
  H: integer;
begin
  H := GlobalImportTable.LookupNamespace(aNamespaceName, 0, True);
  if H > 0 then
    RegisterUsingNamespace (H);
end;

procedure RegisterUsingNamespace(aNamespaceID: Integer);
begin
  GlobalImportTable.HeaderParser.UsedNamespaceList.Add(aNamespaceID);
end;

procedure UnregisterUsingNamespace(aNamespaceID: Integer); overload;
begin
  GlobalImportTable.HeaderParser.UsedNamespaceList.DeleteValue(aNamespaceID);
end;

procedure UnregisterUsingNamespaces;
begin
  GlobalImportTable.HeaderParser.UsedNamespaceList.Clear;
end;

procedure UnregisterUsingNamespace(const aNamespaceName: String); overload;
Var
  H: integer;
begin
  H := GlobalImportTable.LookupNamespace(aNamespaceName, 0, True);
  if H > 0 then
    UnRegisterUsingNamespace(H);
end;

function RegisterConstant(LevelId: Integer; const Name: String; TypeId: Integer;
                          const Value: Variant): Integer;
begin
  result := GlobalImportTable.RegisterConstant(LevelId, Name, TypeId, Value);
end;

function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Variant): Integer;
begin
  result := GlobalImportTable.RegisterConstant(LevelId, Name, Value);
end;

function RegisterPointerConstant(LevelId: Integer; const Name: String;
                          const Value: Pointer): Integer;
begin
  result := GlobalImportTable.RegisterPointerConstant(LevelId, Name, Value);
end;

function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Extended): Integer;
begin
  result := GlobalImportTable.RegisterExtendedConstant(LevelId, Name, Value);
end;

function RegisterConstant(LevelId: Integer; const Name: String;
                          const Value: Int64): Integer;
begin
  result := GlobalImportTable.RegisterInt64Constant(LevelId, Name, Value);
end;

function RegisterConstant(LevelId: Integer; const Declaration: String): Integer;
begin
  result := GlobalImportTable.RegisterConstant(LevelId, Declaration);
end;

function RegisterObject(LevelId: Integer;
                        const ObjectName: String;
                        TypeId: Integer;
                        Address: Pointer = nil): Integer;
begin
  result := GlobalImportTable.RegisterObject(LevelId, ObjectName, TypeId, Address);
end;

function RegisterVirtualObject(LevelId: Integer;
                               const ObjectName: String): Integer;
begin
  result := GlobalImportTable.RegisterVirtualObject(LevelId, ObjectName);
end;

function RegisterVariable(LevelId: Integer;
                          const Name: String; TypeId: Integer;
                          Address: Pointer): Integer;
begin
  result := GlobalImportTable.RegisterVariable(LevelId, Name, TypeId, Address);
end;

function RegisterVariable(LevelId: Integer;
                          const Declaration: String; Address: Pointer): Integer;
begin
  result := GlobalImportTable.RegisterVariable(LevelId, Declaration, Address);
end;

function RegisterRoutine(LevelId: Integer; const Name: String;
                         ResultId: Integer;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterRoutine(LevelId, Name,
       ResultId, CallConvention, Address, OverCount, i_IsDeprecated);
end;

function RegisterRoutine(LevelId: Integer; const Name, ResultType: String;
                         Address: Pointer;
                         CallConvention: Integer = _ccREGISTER;
                         OverCount: Integer = 0;
                         i_IsDeprecated: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterRoutine(LevelId, Name, ResultType,
               CallConvention, Address, OverCount, i_IsDeprecated);
end;

procedure RegisterMember(LevelId: Integer; const Name: String;
                        Address: Pointer);
begin
  GlobalImportTable.RegisterMember(LevelId, Name, Address);
end;

function RegisterConstructor(ClassId: Integer;
                             const Name: String;
                             Address: Pointer;
                             CallConvention: Integer = _ccREGISTER;
                             IsShared: Boolean = false;
                             CallMode: Integer = 0;
                             i_MethodIndex: Integer = 0;
                             OverCount: Integer = 0;
                             i_IsAbstract: Boolean = false;
                             i_AbstractMethodCount: Integer = 0;
                             i_IsDeprecated: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterConstructor(ClassId,
     Name, Address, IsShared, CallMode, i_MethodIndex, OverCount,
     i_IsAbstract, i_AbstractMethodCount, i_IsDeprecated);
end;

function RegisterDestructor(ClassId: Integer; const Name: String;
                             Address: Pointer): Integer;
begin
  result := GlobalImportTable.RegisterDestructor(ClassId, Name, Address);
end;

function RegisterMethod(ClassId: Integer;
                        const Name: String;
                        ResultId: Integer;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterMethod(ClassId,
                                             Name,
                                             ResultId,
                                             CallConvention,
                                             Address,
                                             IsShared,
                                             CallMode,
                                             MethodIndex,
                                             OverCount,
                                             i_IsAbstract,
                                             i_AbstractMethodCount,
                                             i_IsDeprecated);
end;

function RegisterMethod(ClassId: Integer;
                        const Name, ResultType: String;
                        Address: Pointer;
                        CallConvention: Integer = _ccREGISTER;
                        IsShared: Boolean = false;
                        CallMode: Integer = _cmNONE;
                        MethodIndex: Integer = 0;
                        OverCount: Integer = 0;
                        i_IsAbstract: Boolean = false;
                        i_AbstractMethodCount: Integer = 0;
                        i_IsDeprecated: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterMethod(ClassId,
                                             Name,
                                             ResultType,
                                             CallConvention,
                                             Address,
                                             IsShared,
                                             CallMode,
                                             MethodIndex,
                                             OverCount,
                                             i_IsAbstract,
                                             i_AbstractMethodCount,
                                             i_IsDeprecated);
end;

function RegisterHeader(LevelId: Integer;
                        const Header: String; Address: Pointer;
                        MethodIndex: Integer = 0; Visibility: Integer = 0): Integer;
var
  Id: Integer;
  Vis: TClassVisibility;
begin
  result := GlobalImportTable.RegisterHeader(LevelId, Header, Address, MethodIndex);
  if (Visibility > 0) and (LevelId > 0) then
    if GlobalImportTable[LevelId].Kind = KindTYPE then
    begin
      Id := GlobalImportTable.LastSubId;
      Vis := cvNone;
      if Visibility = 0 then
        Vis := cvPublic
      else if Visibility = 1 then
        Vis := cvProtected
      else if Visibility = 2 then
        Vis := cvPrivate
      else if Visibility = 3 then
        Vis := cvPublished;
      GlobalImportTable[Id].Vis := Vis;
    end;
end;

function RegisterFakeHeader(LevelId: Integer;
              const Header: String; Address: Pointer): Integer;
begin
  result := GlobalImportTable.RegisterFakeHeader(LevelId, Header, Address);
end;

procedure RegisterRunnerParameter(HSub: Integer);
begin
  GlobalImportTable.RegisterRunnerParameter(HSub);
end;

function RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           ParamTypeID: Integer;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId,
                                                ParameterName,
                                                ParamTypeId,
                                                ParamMod,
                                                Optional,
                                                DefaultValue);
end;

function RegisterParameter(LevelId: Integer;
                           const ParameterName: String;
                           const ParameterType: String;
                           ParamMod: Integer = 0;
                           Optional: Boolean = false;
                           const DefaultValue: String = ''): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId,
                                                ParameterName,
                                                ParameterType,
                                                ParamMod,
                                                Optional,
                                                DefaultValue);
end;

function RegisterParameter(LevelId: Integer; TypeId: Integer; ByRef: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId, TypeId, _Unassigned, ByRef);
end;

function RegisterParameter(LevelId: Integer; TypeId: Integer;
                           const DefaultValue: Variant; ByRef: Boolean = false): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId, TypeId, DefaultValue, ByRef);
end;

function RegisterParameter(LevelId: Integer;
                           ParameterName: String;
                           TypeID: Integer;
                           const DefaultValue: Variant;
                           ByRef: Boolean): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId, TypeId, DefaultValue, ByRef, ParameterName);
end;

function RegisterParameterEx(LevelId: Integer;
                             ParameterName: String;
                             TypeID: Integer;
                             const DefaultValue: Variant;
                             ByRef: Boolean;
                             IsConst: Boolean): Integer;
begin
  result := GlobalImportTable.RegisterParameter(LevelId, TypeId, DefaultValue, ByRef, ParameterName);
  if IsConst then
    GlobalImportTable[GlobalImportTable.Card].IsConst := true;
end;

function RegisterParameterEx2(LevelId: Integer; ParameterName: String; TypeID: Integer; const DefaultValue: Variant;
    ByRef, IsConst, IsOpenArray: Boolean): Integer;
Begin
  Result := RegisterParameterEx(LevelId, ParameterName, TypeId, DefaultValue, ByRef, IsConst);
  if IsOpenArray then
    GlobalImportTable[GlobalImportTable.Card].IsOpenArray := True;
end;

function RegisterClassType(LevelId: Integer; C: TClass;
                           DoRegisterClass: Boolean = false
                           ): Integer;
begin
  if DoRegisterClass or MustRegisterClass then
    if C.InheritsFrom(TPersistent) then
      if Classes.GetClass(C.ClassName) = nil then
        Classes.RegisterClass(TPersistentClass(C));

  result := GlobalImportTable.RegisterClassType(LevelId, C);
end;


function RegisterClassType(LevelId: Integer; C: TClass;
                           DoRegisterClass: Boolean;
                           Reserved: Integer): Integer;
begin
  if DoRegisterClass or MustRegisterClass then
    if C.InheritsFrom(TPersistent) then
      if Classes.GetClass(C.ClassName) = nil then
        Classes.RegisterClass(TPersistentClass(C));

  result := GlobalImportTable.RegisterClassType(LevelId, C, Reserved);
end;

function RegisterClassTypeForImporter(LevelId: Integer;
                                      C: TClass): Integer;
begin
  result := GlobalImportTable.RegisterClassTypeForImporter(LevelId, C);
end;

function RegisterInterfaceType(LevelId: Integer;
                               const TypeName: String;
                               const GUID: TGUID;
                               const ParentName: String;
                               const ParentGUID: TGUID): Integer;
begin
  result := GlobalImportTable.RegisterInterfaceType(LevelId, TypeName, GUID);
  GlobalImportTable.RegisterSupportedInterface(result, ParentName, ParentGUID);
end;

function RegisterInterfaceType(LevelId: Integer;
                               const TypeName: String;
                               const GUID: TGUID): Integer;
begin
  result := GlobalImportTable.RegisterInterfaceType(LevelId, TypeName, GUID);
end;

procedure RegisterSupportedInterface(TypeId: Integer;
                                     const SupportedInterfaceName: String;
                                     const GUID: TGUID);
begin
  GlobalImportTable.RegisterSupportedInterface(TypeId, SupportedInterfaceName, GUID);
end;

function RegisterClassType(LevelId: Integer;
                           const TypeName: String; AncestorId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterClassType(LevelId, TypeName, AncestorId);
end;

function RegisterClassType(LevelId: Integer;
                           const TypeName: String): Integer;
begin
  result := GlobalImportTable.RegisterClassType(LevelId, TypeName, H_TObject);
end;

procedure RegisterClassTypeInfos(ClassId: Integer;
                                     C: TClass);
begin
  GlobalImportTable.RegisterClassTypeInfos(ClassId, C);
end;

function RegisterClassReferenceType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterClassReferenceType(LevelId, TypeName, OriginClassId);
end;

function RegisterClassReferenceType(LevelId: Integer;
                           const TypeName: String): Integer;
var
  OriginClassId: Integer;
  OriginTypeName: String;
begin
  OriginTypeName := Copy(TypeName, 1, Length(TypeName) - 5);
  OriginClassId := GlobalImportTable.LookUpType(OriginTypeName, 0, true);
  if OriginClassId = 0 then
  begin
    result := 0;
    RaiseError(errUndeclaredIdentifier, [OriginTypeName]);
    Exit;
  end;
  result := GlobalImportTable.RegisterClassReferenceType(LevelId, TypeName, OriginClassId);
end;

function RegisterClassReferenceType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
var
  OriginClassId: Integer;
begin
  OriginClassId := GlobalImportTable.LookUpType(OriginalTypeName, 0, true);
  if OriginClassId = 0 then
  begin
    result := 0;
    RaiseError(errUndeclaredIdentifier, [OriginalTypeName]);
    Exit;
  end;
  result := GlobalImportTable.RegisterClassReferenceType(LevelId, TypeName, OriginClassId);
end;

function RegisterClassHelperType(LevelId: Integer;
                           const TypeName: String; OriginClassId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterHelperType(LevelId, TypeName, OriginClassId);
end;

function RegisterClassHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
var
  OriginClassId: Integer;
begin
  OriginClassId := GlobalImportTable.LookUpType(OriginalTypeName, 0, true);
  result := GlobalImportTable.RegisterHelperType(LevelId, TypeName, OriginClassId);
end;

function RegisterRecordHelperType(LevelId: Integer;
                           const TypeName: String; OriginRecordId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterHelperType(LevelId, TypeName, OriginRecordId);
end;

function RegisterRecordHelperType(LevelID: Integer; const TypeName, OriginalTypeName: String): Integer; overload;
var
  OriginRecordId: Integer;
begin
  OriginRecordId := GlobalImportTable.LookUpType(OriginalTypeName, 0, true);
  result := GlobalImportTable.RegisterHelperType(LevelId, TypeName, OriginRecordId);
end;

function RegisterClassTypeField(ClassTypeId: Integer; const Name: String;
                                TypeID: Integer; Offset: Integer = -1): Integer;
begin
  result := GlobalImportTable.RegisterTypeField(ClassTypeId, Name, TypeId, Offset);
end;

function RegisterClassTypeField(TypeId: Integer; const Declaration: String
                                 ): Integer;
begin
  result := GlobalImportTable.RegisterTypeFieldEx(TypeId, Declaration);
end;

function RegisterProperty(LevelId: Integer; const PropName: String;
                          PropTypeID, ReadId, WriteId: Integer;
                          IsDefault: Boolean): Integer;
begin
  result := GlobalImportTable.RegisterProperty(LevelId, PropName,
                                        PropTypeId, ReadId, WriteId, IsDefault);
end;

function RegisterInterfaceProperty(LevelId: Integer;
                                   const PropName: String;
                                   PropTypeID,
                                   ReadIndex,
                                   WriteIndex: Integer): Integer;
begin
  result := GlobalImportTable.RegisterInterfaceProperty(LevelId, PropName, PropTypeId,
                                    ReadIndex, WriteIndex);
end;

function RegisterProperty(LevelId: Integer; const Header: String): Integer;
begin
  result := GlobalImportTable.RegisterHeader(LevelId, Header, nil);
end;

function RegisterRecordType(LevelId: Integer;
                            const TypeName: String;
                            IsPacked: Boolean = false): Integer;
begin
  if IsPacked then
    result := GlobalImportTable.RegisterRecordType(LevelId, TypeName, 1)
  else
    result := GlobalImportTable.RegisterRecordType(LevelId, TypeName, GlobalAlignment);
end;

function RegisterDummyType(LevelId: Integer;
                           const TypeName: String): Integer;
begin
  result := GlobalImportTable.RegisterDummyType(LevelId, TypeName);
end;

function RegisterSomeType(LevelId: Integer;
                           const TypeName: String): Integer;
begin
  result := GlobalImportTable.RegisterSomeType(LevelId, TypeName);
end;

function LookupNamespace(LevelId: Integer; const NamespaceName: String;
                         CaseSensitive: Boolean): Integer;
begin
  result := GlobalImportTable.LookupNamespace(NamespaceName, LevelId, not CaseSensitive);
end;

function LookupNamespace(const NamespaceName: String): Integer; overload;
begin
  result := LookupNamespace(0, NamespaceName, true);
end;

function RegisterRecordTypeField(TypeId: Integer; const FieldName: String;
                                 FieldTypeID: Integer; FieldOffset: Integer = -1): Integer;
begin
  result := GlobalImportTable.RegisterTypeField(TypeId, FieldName,
                                                FieldTypeId, FieldOffset);
end;

function RegisterRecordTypeField(TypeId: Integer; const Declaration: String
                                 ): Integer;
begin
  result := GlobalImportTable.RegisterTypeFieldEx(TypeId, Declaration);
end;

function RegisterVariantRecordTypeField(LevelId: Integer; const FieldName: String;
                                FieldTypeID: Integer;
                                VarCount: Int64): Integer;
begin
  result := GlobalImportTable.RegisterVariantRecordTypeField(LevelId,
                                                           FieldName,
                                                           FieldTypeId,
                                                           VarCount);
end;

function RegisterVariantRecordTypeField(LevelId: Integer; const Declaration: String;
                                VarCount: Int64): Integer;
begin
  result := GlobalImportTable.RegisterVariantRecordTypeField(LevelId,
      Declaration, VarCount);
end;

function RegisterSubrangeType(LevelId: Integer;
                              const TypeName: String;
                              TypeBaseId: Integer;
                              B1, B2: Integer): Integer;
begin
  result := GlobalImportTable.RegisterSubrangeType(LevelId, TypeName, TypeBaseId, B1, B2);
end;

function RegisterTypeDeclaration(LevelId: Integer;
                                 const Declaration: String): Integer;
begin
  result := GlobalImportTable.RegisterTypeDeclaration(LevelId, Declaration);
end;

function LookupTypeId(const TypeName: String): Integer;
begin
//  result := GlobalImportTable.HeaderParser.LookupId(TypeName);
  result := GlobalImportTable.LookupType(TypeName, true);
end;

function LookupTypeNamespaceId(const TypeName: String): Integer;
var
  R: TSymbolRec;
  L, Id: Integer;
begin
  result := 0;
  Id := LookupTypeId(TypeName);
  if Id = 0 then
    Exit;

  L := GlobalImportTable[Id].Level;

  repeat
    if L = 0 then
    begin
      result := 0;
      Exit;
    end;

    R := GlobalImportTable[L];

    if R.Kind = kindNAMESPACE then
    begin
      result := R.Id;
      Exit;
    end;

    L := R.Level;

  until false;
end;


function RegisterEnumType(LevelId: Integer;
                          const TypeName: String;
                          TypeBaseId: Integer = _typeINTEGER): Integer;
begin
  result := GlobalImportTable.RegisterEnumType(LevelId, TypeName, TypeBaseId);
end;

function RegisterEnumValue(EnumTypeId: Integer;
                           const FieldName: String;
                           const Value: Integer): Integer;
begin
  result := GlobalImportTable.RegisterEnumValue(EnumTypeId, FieldName, Value);
end;

function RegisterArrayType(LevelId: Integer;
                           const TypeName: String;
                           RangeTypeId, ElemTypeId: Integer;
                           IsPacked: Boolean = false): Integer;
begin
  if IsPacked then
    result := GlobalImportTable.RegisterArrayType(LevelId, TypeName, RangeTypeId, ElemTypeId, 1)
  else
    result := GlobalImportTable.RegisterArrayType(LevelId, TypeName, RangeTypeId, ElemTypeId, GlobalAlignment);
end;

function RegisterDynamicArrayType(LevelId: Integer;
                           const TypeName: String;
                           ElemTypeId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterDynamicArrayType(LevelId, TypeName, ElemTypeId);
end;

function RegisterPointerType(LevelId: Integer;
                             const TypeName: String;
                             OriginTypeId: Integer;
                             const OriginTypeName: String = ''): Integer;
begin
  result := GlobalImportTable.RegisterPointerType(LevelId,
     TypeName, OriginTypeId, OriginTypeName);
end;

function RegisterSetType(LevelId: Integer;
                         const TypeName: String;
                         OriginTypeId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterSetType(LevelId, TypeName, OriginTypeId);
end;

function RegisterProceduralType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterProceduralType(LevelId, TypeName, SubId);
end;

function RegisterMethodReferenceType(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterMethodReferenceType(LevelId, TypeName, SubId);
end;

{$IFNDEF PAXARM}
function RegisterShortStringType(LevelId: Integer;
                                 const TypeName: String;
                                 L: Integer): Integer;
begin
  result := GlobalImportTable.RegisterShortStringType(LevelId, TypeName, L);
end;
{$ENDIF}

function RegisterEventType(LevelId: Integer;
                           const TypeName: String;
                           SubId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterEventType(LevelId, TypeName, SubId);
end;

function RegisterRTTIType(LevelId: Integer; pti: PTypeInfo): Integer;
begin
  result := GlobalImportTable.RegisterRTTIType(LevelId, pti);
end;

function RegisterTypeAlias(LevelId:Integer; const TypeName: String;
                           OriginTypeId: Integer): Integer;
begin
  result := GlobalImportTable.RegisterTypeAlias(LevelId, TypeName, OriginTypeId);
end;

function RegisterTypeAlias(LevelId:Integer; const TypeName, OriginTypeName: String): Integer;
begin
  result := GlobalImportTable.RegisterTypeAlias(LevelId, TypeName, OriginTypeName);
end;

function RegisterTypeAlias(LevelId:Integer;
                           const Declaration: String): Integer;
begin
  result := GlobalImportTable.RegisterTypeAlias(LevelId, Declaration);
end;

function RegisterSpace(K: Integer): Integer;
begin
  result := GlobalImportTable.RegisterSpace(K);
end;

procedure RegisterAlignment(value: Integer);
begin
  GlobalAlignment := value;
end;

procedure SetVisibility(C: TClass; const MemberName: String; value: Integer); overload;
begin
  GlobalImportTable.SetVisibility(C, MemberName, value);
end;

procedure SetVisibility(ClassId: integer;
                        const MemberName: String; value: Integer); overload;
begin
  GlobalImportTable.SetVisibility(ClassId, MemberName, value);
end;

procedure SaveNamespaceToStream(const NamespaceName: String;
                                S: TStream);
begin
  GlobalImportTable.SaveNamespaceToStream(NamespaceName, S);
end;

procedure SaveNamespaceToFile(const NamespaceName: String;
                              const FileName: String);
begin
  GlobalImportTable.SaveNamespaceToFile(NamespaceName, FileName);
end;

procedure LoadNamespaceFromStream(S: TStream);
begin
  GlobalImportTable.LoadNamespaceFromStream(S);
end;

procedure LoadNamespaceFromFile(const FileName: String);
begin
  GlobalImportTable.LoadNamespaceFromFile(FileName);
end;

{$O-}
procedure EndOfRegistration(CheckProc: TCheckProc; Data: Pointer);
var
  I, J, L: Integer;
  RI: TSymbolRec;
  pti: PTypeInfo;
  ptd: PTypeData;
  C: TClass;
  S, err: String;
  LS: TStringList;
  found: Boolean;
begin
  GlobalImportTable.ResolveExternList(CheckProc, Data);
{$IFDEF DRTTI}
  Exit;
{$ENDIF}
  err := '';
  LS := TStringList.Create;
  try
    for I:=1 to GlobalImportTable.Card do
    begin
      RI := GlobalImportTable[I];
      if RI.Kind = kindTYPE then
        RI.Completed := true
      else
        RI.Size;
      if RI.FinalTypeId = typeCLASS then
      begin
        if I <= StdCard then
          continue;

        C := RI.PClass;
        if C = nil then
          continue;

        pti := C.ClassInfo;
        if pti = nil then
          continue;

        ptd := GetTypeData(pti);
        if ptd = nil then
          continue;
        L := RI.Level;
        if L > 0 then
          S := GlobalImportTable[L].Name
        else
          S := '';
        if not StrEql(S, StringFromPShortString(PShortString(@ptd^.UnitName))) then
        begin
          err := RI.Name + '(' + StringFromPShortString(PShortString(@ptd^.UnitName)) + ')' +
                 errWrongRegistration + '-' + S;
          found := false;
          for J := 0 to LS.Count - 1 do
            if StrEql(LS[J], err) then
            begin
              found := true;
              break;
            end;
          if not found then
            LS.Add(err);
        end;
      end;
    end;

    err := '';
    for J := 0 to LS.Count - 1 do
      err := err + LS[J] + #13#10;

  finally
    FreeAndNil(LS);
  end;

  if err <> '' then
    RaiseError(errWrongRegistration + ':' + err, []);
end;

procedure EndOfRegistration;
begin
  EndOfRegistration(nil, nil);
end;

procedure ForbidClass(C: TClass);
begin
  GlobalImportTable.HideClass(C);
end;

function CreateNewImportTable: Pointer;
var
  st: TBaseSymbolTable;
begin
  st := TBaseSymbolTable.Create;
  AddStdRoutines(st);
  result := st;
  GlobalExtraImportTableList.Add(st);
end;

procedure SetImportTable(ImportTable: Pointer);
begin
  if ImportTable = nil then
    GlobalImportTable := GlobalSymbolTable
  else
    GlobalImportTable := ImportTable;
end;

procedure DestroyImportTable(var ImportTable: Pointer);
begin
  GlobalExtraImportTableList.Remove(ImportTable);
  ImportTable := nil;
end;

function LoadImportLibrary(const DllName: String): Cardinal;
var
  P: TRegisterDllProc;
  R: TRegisterProcRec;
begin
  R.RegisterNamespace := RegisterNamespace;
  R.RegisterConstant := RegisterConstant;
  R.RegisterVariable := RegisterVariable;
  R.RegisterHeader := RegisterHeader;
  R.RegisterProperty := RegisterProperty;

  R.RegisterClassType := RegisterClassType;
  R.RegisterClassTypeField := RegisterClassTypeField;

  R.RegisterClassReferenceType := RegisterClassReferenceType;

  R.RegisterRecordType := RegisterRecordType;
  R.RegisterRecordTypeField := RegisterRecordTypeField;
  R.RegisterVariantRecordTypeField := RegisterVariantRecordTypeField;

  R.RegisterEnumType := RegisterEnumType;
  R.RegisterEnumValue:= RegisterEnumValue;

  R.RegisterSubrangeType := RegisterSubrangeType;
  R.RegisterArrayType := RegisterArrayType;
  R.RegisterDynamicArrayType := RegisterDynamicArrayType;
  R.RegisterPointerType := RegisterPointerType;
  R.RegisterSetType := RegisterSetType;
  R.RegisterProceduralType := RegisterProceduralType;
  R.RegisterEventType := RegisterEventType;
{$IFNDEF PAXARM}
  R.RegisterShortStringType := RegisterShortStringType;
{$ENDIF}
  R.RegisterRTTIType := RegisterRTTIType;
  R.RegisterTypeAlias := RegisterTypeAlias;

{$IFDEF FPC}
   result := HMODULE(dynlibs.LoadLibrary(DLLName));
{$ELSE}
   result := LoadLibrary(PChar(DllName));
{$ENDIF}
  if result > 0 then
  begin
    P := GetProcAddress(result, 'RegisterDllProcs');
    if not Assigned(P) then
    begin
      result := 0;
      Exit;
    end;

    P(R);
  end;
end;

function FreeImportLibrary(H: Cardinal): LongBool;
begin
  result := FreeLibrary(H);
end;

function SetImportEntry(K: Integer): Integer;
begin
  result := GlobalImportTable.Card;
  GlobalImportTable.RestoreState(K);
end;

function GetImportEntry: Integer;
begin
  result := GlobalImportTable.Card;
  GlobalImportTable.SaveState;
end;

procedure LoadGlobalSymbolTableFromStream(Stream: TStream);
begin
  GlobalImportTable.LoadGlobalSymbolTableFromStream(Stream);
end;

procedure LoadGlobalSymbolTableFromFile(const FileName: String);
begin
  GlobalImportTable.LoadGlobalSymbolTableFromFile(FileName);
end;

procedure SaveGlobalSymbolTableToStream(Stream: TStream);
begin
  GlobalImportTable.SaveGlobalSymbolTableToStream(Stream);
end;

procedure SaveGlobalSymbolTableToFile(const FileName: String);
begin
  GlobalImportTable.SaveGlobalSymbolTableToFile(FileName);
end;

function FindNextVirtualMethodAddress(C: TClass; PrevAddress: Pointer): Pointer;
begin
  result := paxcomp_sys.FindNextVirtualMethodAddress(C, PrevAddress);
end;

function RenameClassType(C: TClass; const NewName: String): Boolean;
var
  Id: Integer;
begin
  result := false;
  Id := LookupTypeId(C.ClassName);
  if Id > 0 then
  begin
    result := true;
    ForbidClass(C);
    GlobalImportTable[Id].Name := NewName;
  end;
end;

procedure EndOfStructuredType(Id: Integer);
begin
  GlobalImportTable[Id].Completed := true;
end;

{$ifdef DRTTI}
function RegisterRTTIRecordType(Level: Integer; t: TRTTIRecordType): Integer;
begin
  result := PAXCOMP_2010Reg.RegisterRecordType(Level, t, GlobalImportTable);
end;

function RegisterRTTIClassType(Level: Integer; t: TRTTIInstanceType): Integer;
begin
  result := PAXCOMP_2010Reg.RegisterClassType(Level, t, GlobalImportTable);
end;

function RegisterRTTIInterfaceType(Level: Integer; t: TRTTIInterfaceType): Integer;
begin
  result := PAXCOMP_2010Reg.RegisterInterfaceType(Level, t, GlobalImportTable);
end;

{$endif}


end.
