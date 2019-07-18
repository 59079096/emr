//////////////////////////////////////////////////////////////////////////
// PaxCompiler
// Site: http://www.paxcompiler.com
// Author: Alexander Baranovsky (paxscript@gmail.com)
// ========================================================================
// Copyright (c) Alexander Baranovsky, 2006-2014. All rights reserved.
// Code Version: 4.2
// ========================================================================
// Unit: PaxInfos.pas
// ========================================================================
////////////////////////////////////////////////////////////////////////////

{$I PaxCompiler.def}
unit PaxInfos;
interface
uses {$I uses.def}
  TypInfo;
type
{$IFNDEF UNIC}
  TMemberVisibility = (mvPrivate, mvProtected, mvPublic, mvPublished);
{$ENDIF}

  TPrintClassTypeFieldInfo = record
    Owner: TObject;
    FieldIndex: Integer;
    FieldCount: Integer;
    Address: Pointer;
    FieldName: String;
    TypeId: Integer;
    FieldTypeName: String;
    Started: Boolean;
    Finished: Boolean;
    Visibility: TMemberVisibility;
    Host: Boolean;
  end;

  TPrintClassTypePropInfo = record
    Owner: TObject;
    PropIndex: Integer;
    PropCount: Integer;
    StrValue: String;
    PropName: String;
    PropTypeName: String;
    Started: Boolean;
    Finished: Boolean;
    Visibility: TMemberVisibility;
    Host: Boolean;
  end;

implementation
end.
