{$I PaxCompiler.def}
unit IMPORT_Variants;
interface
uses
{$IFDEF VARIANTS}
  Variants,
{$ENDIF}
  SysUtils,
  PaxRegister,
  PaxCompiler;

procedure Register_Variants;

implementation

procedure _VarArrayRedim(var A: Variant; HighBound: Integer);
begin
  VarArrayRedim(A, HighBound);
end;

procedure _VarCast(var Dest: Variant; const Source: Variant; VarType: Integer);
begin
  VarCast(Dest, Source, VarType);
end;

procedure _VarClear(var V : Variant);
begin
  VarClear(V);
end;

procedure _VarCopy(var Dest: Variant; const Source: Variant);
begin
  VarCopy(Dest, Source);
end;

procedure Register_Variants;
var
  H: Integer;
begin
  H := RegisterNamespace(0, 'Variants');

  RegisterConstant(0, 'varEmpty', _typeWORD, varEmpty);
  RegisterConstant(0, 'varNull', _typeWORD, varNull);
  RegisterConstant(0, 'varSmallint', _typeWORD, varSmallInt);
  RegisterConstant(0, 'varInteger', _typeWORD, varInteger);
  RegisterConstant(0, 'varSingle', _typeWORD, varSingle);
  RegisterConstant(0, 'varDouble', _typeWORD, varDouble);
  RegisterConstant(0, 'varCurrency', _typeWORD, varCurrency);
  RegisterConstant(0, 'varDate', _typeWORD, varDate);
  RegisterConstant(0, 'varOleStr', _typeWORD, varOleStr);
  RegisterConstant(0, 'varDispatch', _typeWORD, varDispatch);
  RegisterConstant(0, 'varError', _typeWORD, varError);
  RegisterConstant(0, 'varBoolean', _typeWORD, varBoolean);
  RegisterConstant(0, 'varVariant', _typeWORD, varVariant);
  RegisterConstant(0, 'varUnknown', _typeWORD, varUnknown);
{$IFDEF VARIANTS}
  RegisterConstant(0, 'varShortInt', _typeWORD, varShortInt);
{$ENDIF}
  RegisterConstant(0, 'varByte', _typeWORD, varByte);
{$IFDEF VARIANTS}
  RegisterConstant(0, 'varWord', _typeWORD, varWord);
  RegisterConstant(0, 'varLongWord', _typeWORD, varLongWord);
  RegisterConstant(0, 'varInt64', _typeWORD, varInt64);
{$ENDIF}
  RegisterConstant(0, 'varStrArg', _typeWORD, varStrArg);
  RegisterConstant(0, 'varString', _typeWORD, varString);
  RegisterConstant(0, 'varAny', _typeWORD, varAny);

  RegisterHeader(H, 'function VarArrayCreate(const Bounds: array of Integer; VarType: Integer): Variant;',
                 @VarArrayCreate);
  RegisterHeader(H, 'function VarArrayDimCount(const A: Variant): Integer;',
                 @VarArrayDimCount);
  RegisterHeader(H, 'function VarArrayHighBound(const A: Variant; Dim: Integer): Integer;',
                 @VarArrayHighBound);
  RegisterHeader(H, 'function VarArrayLock(var A: Variant): Pointer;',
                 @VarArrayLock);
  RegisterHeader(H, 'function VarArrayLowBound(const A: Variant; Dim: Integer): Integer;',
                 @VarArrayLowBound);
  RegisterHeader(H, 'function VarArrayOf(const Values: array of Variant): Variant;',
                 @VarArrayOf);
  RegisterHeader(H, 'procedure VarArrayRedim(var A: Variant; HighBound: Integer);',
                 @_VarArrayRedim);
  RegisterHeader(H, 'function VarArrayRef(const A: Variant): Variant;',
                 @VarArrayRef);
  RegisterHeader(H, 'procedure VarArrayUnlock(var A: Variant);',
                 @VarArrayUnlock);
  RegisterHeader(H, 'function VarAsType(const V: Variant; VarType: Integer): Variant;',
                 @VarAsType);
  RegisterHeader(H, 'procedure VarCast(var Dest: Variant; const Source: Variant; VarType: Integer);',
                 @_VarCast);
  RegisterHeader(H, 'procedure VarClear(var V : Variant);',
                 @_VarClear);
  RegisterHeader(H, 'procedure VarCopy(var Dest: Variant; const Source: Variant);',
                 @_VarCopy);
  RegisterHeader(H, 'function VarFromDateTime(DateTime: TDateTime): Variant;',
                 @VarFromDateTime);
  RegisterHeader(H, 'function VarIsArray(const V: Variant): Boolean;',
                 @VarIsArray);
  RegisterHeader(H, 'function VarIsEmpty(const V: Variant): Boolean;',
                 @VarIsEmpty);
  RegisterHeader(H, 'function VarIsNull(const V: Variant): Boolean;',
                 @VarIsNull);
  RegisterHeader(H, 'function VarToDateTime(const V: Variant): TDateTime;',
                 @VarToDateTime);
  RegisterHeader(H, 'function VarToStr(const V: Variant): string;',
                 @VarToStr);
  RegisterHeader(H, 'function VarType(const V: Variant): Integer;', @VarType);

  RegisterVariable(0, 'EmptyParam', _typeOLEVARIANT, @ EmptyParam);
end;

initialization

Register_Variants;

end.
