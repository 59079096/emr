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

unit PaxDllImport;

interface

uses {$I uses.def}
  TypInfo;

type
  TRegisterNamespace = function (LevelId: Integer; const Name: String): Integer;
  TRegisterConstant = function(LevelId: Integer; const Name: String;
                          const Value: Variant): Integer;
  TRegisterVariable = function(LevelId: Integer;
                          const Name: String; TypeId: Integer;
                          Address: Pointer): Integer;
  TRegisterHeader = function(LevelId: Integer;
                        const Header: String; Address: Pointer;
                        MethodIndex: Integer = 0; Visibility: Integer = 0): Integer;
  TRegisterProperty = function(LevelId: Integer; const Header: String): Integer;

  TRegisterClassType = function(LevelId: Integer; C: TClass;
    DoRegisterClass: Boolean = false): Integer;

  TRegisterClassReferenceType = function(LevelID: Integer;
    const TypeName, OriginalTypeName: String): Integer;
  TRegisterClassTypeField = function(TypeId: Integer; const Declaration: String): Integer;

  TRegisterRecordType = function(LevelId: Integer;
                          const TypeName: String;
                          IsPacked: Boolean = false): Integer;
  TRegisterRecordTypeField = function(TypeId: Integer; const Declaration: String): Integer;
  TRegisterVariantRecordTypeField = function(LevelId: Integer; const Declaration: String;
                                VarCount: Int64): Integer;

  TRegisterEnumType = function(LevelId: Integer;
                          const TypeName: String;
                          TypeBaseId: Integer = 7): Integer;
  TRegisterEnumValue = function(EnumTypeId: Integer;
                           const FieldName: String;
                           const Value: Integer): Integer;

  TRegisterSubrangeType = function(LevelId: Integer;
                              const TypeName: String;
                              TypeBaseId: Integer;
                              B1, B2: Integer): Integer;

  TRegisterArrayType = function(LevelId: Integer;
                           const TypeName: String;
                           RangeTypeId, ElemTypeId: Integer;
                           IsPacked: Boolean = false): Integer;
  TRegisterDynamicArrayType = function(LevelId: Integer;
                           const TypeName: String;
                           ElemTypeId: Integer): Integer;

  TRegisterPointerType = function(LevelId: Integer;
                             const TypeName: String;
                             OriginTypeId: Integer;
                             const OrginTypeName: String = ''): Integer;
  TRegisterSetType = function(LevelId: Integer;
                         const TypeName: String;
                         OriginTypeId: Integer): Integer;

  TRegisterProceduralType = function(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;

  TRegisterEventType = function(LevelId: Integer;
                                const TypeName: String;
                                SubId: Integer): Integer;

  TRegisterShortStringType = function(LevelId: Integer;
                                 const TypeName: String;
                                 L: Integer): Integer;

  TRegisterInterfaceType = function(LevelId: Integer;
                               const TypeName: String;
                               const GUID: TGUID;
                               const ParentName: String;
                               const ParentGUID: TGUID): Integer;

  TRegisterRTTIType = function(LevelId: Integer;
                          pti: PTypeInfo): Integer;

  TRegisterTypeAlias = function(LevelId:Integer;
                           const TypeName: String;
                           OriginTypeId: Integer): Integer;

  TRegisterProcRec = record
    RegisterNamespace: TRegisterNamespace;
    RegisterConstant: TRegisterConstant;
    RegisterVariable: TRegisterVariable;
    RegisterHeader: TRegisterHeader;
    RegisterProperty: TRegisterProperty;
    RegisterClassType: TRegisterClassType;
    RegisterClassTypeField: TRegisterClassTypeField;
    RegisterClassReferenceType: TRegisterClassReferenceType;
    RegisterRecordType: TRegisterRecordType;
    RegisterRecordTypeField: TRegisterRecordTypeField;
    RegisterVariantRecordTypeField: TRegisterVariantRecordTypeField;
    RegisterEnumType: TRegisterEnumType;
    RegisterEnumValue: TRegisterEnumValue;
    RegisterSubrangeType: TRegisterSubrangeType;
    RegisterArrayType: TRegisterArrayType;
    RegisterDynamicArrayType: TRegisterDynamicArrayType;
    RegisterPointerType: TRegisterPointerType;
    RegisterSetType: TRegisterSetType;
    RegisterProceduralType: TRegisterProceduralType;
    RegisterEventType: TRegisterEventType;
    RegisterShortStringType: TRegisterShortStringType;
    RegisterInterfaceType: TRegisterInterfaceType;
    RegisterRTTIType: TRegisterRTTIType;
    RegisterTypeAlias: TRegisterTypeAlias;
  end;

  TRegisterDllProc = procedure (R: TRegisterProcRec);

implementation

end.

